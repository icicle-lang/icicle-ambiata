#include "51-chord.h"

#if !ICICLE_NO_PSV

/* forward declarations for types, implemented by generated code */
typedef struct ifleet ifleet_t;

/* psv types */
typedef struct {
    /* inputs */
    /* these are 32-bit file handles, but storing them as 64-bit in the struct makes it easier to poke from Haskell */
    iint_t input_fd;
    iint_t output_fd;
    iint_t chord_fd;

    /* outputs */
    psv_error_t error;
    iint_t      fact_count;
    iint_t      entity_count;
} psv_config_t;

typedef struct {
    /* input buffer */
    const char *buffer_ptr;
    size_t      buffer_size;
    size_t      buffer_remaining;

    /* input chords */
    const ichord_t *chord_cur;

    /* stats */
    iint_t fact_count;
    iint_t entity_count;

    /* current entity */
    char   *entity_cur;      /* invariant: this must point to a block of memory at least */
    size_t  entity_cur_size; /*            as large as the input buffer or we'll overflow */

    /* fleet state */
    ifleet_t *fleet;

    /* output file descriptor */
    int output_fd;
} psv_state_t;


/* forward declarations for functions, implemented by generated code */
static ifleet_t * INLINE psv_alloc_fleet (iint_t max_chord_count);

static void INLINE psv_collect_fleet (ifleet_t *fleet);

static psv_error_t INLINE psv_configure_fleet (const char *entity, size_t entity_size, const ichord_t **chord, ifleet_t *fleet);

static void INLINE psv_write_outputs (int fd, const char *entity, ifleet_t *fleet);

static psv_error_t INLINE psv_read_fact
  ( const char   *attrib
  , const size_t  attrib_size
  , const char   *value
  , const size_t  value_size
  , idate_t       date
  , ifleet_t     *fleet );

/* psv driver */
static const size_t psv_max_row_count   = 128;
static const size_t psv_buffer_size     = 16*1024;
static const size_t psv_output_buf_size = 128;

static psv_error_t INLINE psv_read_date (const char *time_ptr, const size_t time_size, idate_t *output_ptr)
{
    const size_t time0_size = time_size + 1;

                        /* time_ptr + 0123456789 */
    const size_t date_only = sizeof ("yyyy-mm-dd");

    if (date_only == time0_size &&
        *(time_ptr + 4) == '-'  &&
        *(time_ptr + 7) == '-') {

        char *year_end, *month_end, *day_end;
        const iint_t year  = strtol (time_ptr + 0, &year_end,  10);
        const iint_t month = strtol (time_ptr + 5, &month_end, 10);
        const iint_t day   = strtol (time_ptr + 8, &day_end,   10);

        if (year_end  != time_ptr + 4 ||
            month_end != time_ptr + 7 ||
            day_end   != time_ptr + 10)
            return psv_alloc_error ("expected 'yyyy-mm-dd'", time_ptr, time_size);

        *output_ptr = idate_from_gregorian (year, month, day, 0, 0, 0);
        return 0;
    }

                        /* time_ptr + 01234567890123456789 */
    const size_t date_time = sizeof ("yyyy-mm-ddThh:mm:ssZ");

    if (date_time == time0_size &&
        *(time_ptr +  4) == '-' &&
        *(time_ptr +  7) == '-' &&
        *(time_ptr + 10) == 'T' &&
        *(time_ptr + 13) == ':' &&
        *(time_ptr + 16) == ':' &&
        *(time_ptr + 19) == 'Z') {

        char *year_end, *month_end, *day_end, *hour_end, *minute_end, *second_end;
        const iint_t year   = strtol (time_ptr +  0, &year_end,   10);
        const iint_t month  = strtol (time_ptr +  5, &month_end,  10);
        const iint_t day    = strtol (time_ptr +  8, &day_end,    10);
        const iint_t hour   = strtol (time_ptr + 11, &hour_end,   10);
        const iint_t minute = strtol (time_ptr + 14, &minute_end, 10);
        const iint_t second = strtol (time_ptr + 17, &second_end, 10);

        if (year_end   != time_ptr +  4 ||
            month_end  != time_ptr +  7 ||
            day_end    != time_ptr + 10 ||
            hour_end   != time_ptr + 13 ||
            minute_end != time_ptr + 16 ||
            second_end != time_ptr + 19)
            return psv_alloc_error ("expected 'yyyy-mm-ddThh:mm:ssZ'", time_ptr, time_size);

        *output_ptr = idate_from_gregorian (year, month, day, hour, minute, second);
        return 0;
    }

    return psv_alloc_error ("expected 'yyyy-mm-dd' or 'yyyy-mm-ddThh:mm:ssZ' but was", time_ptr, time_size);
}

static psv_error_t INLINE psv_read_json_date (char **pp, char *pe, idate_t *output_ptr)
{
    char *p = *pp;

    if (*p++ != '"')
        return psv_alloc_error ("missing opening quote '\"'",  p, pe - p);

    char *quote_ptr = memchr (p, '"', pe - p);

    if (!quote_ptr)
        return psv_alloc_error ("missing closing quote '\"'",  p, pe - p);

    size_t date_size = quote_ptr - p;
    psv_error_t error = psv_read_date (p, date_size, output_ptr);

    if (error) return error;

    *pp = quote_ptr + 1;

    return 0;
}

static psv_error_t INLINE psv_read_string (imempool_t *pool, char **pp, char *pe, istring_t *output_ptr)
{
    char *p = *pp;

    size_t output_size = pe - p + 1;
    char  *output      = imempool_alloc (pool, output_size);

    output[output_size] = 0;
    memcpy (output, p, output_size - 1);

    *output_ptr = output;
    *pp         = p + output_size - 1;

    return 0;
}

static psv_error_t INLINE psv_read_json_string (imempool_t *pool, char **pp, char *pe, istring_t *output_ptr)
{
    char *p = *pp;

    if (*p++ != '"')
        return psv_alloc_error ("missing '\"'",  p, pe - p);

    char *quote_ptr = memchr (p, '"', pe - p);

    if (!quote_ptr)
        return psv_alloc_error ("missing closing quote '\"'",  p, pe - p);

    size_t output_size = quote_ptr - p + 1;
    char  *output      = imempool_alloc (pool, output_size);

    output[output_size] = 0;
    memcpy (output, p, output_size - 1);

    *output_ptr = output;
    *pp         = quote_ptr + 1;

    return 0;
}

static psv_error_t INLINE psv_try_read_json_null (char **pp, char *pe, ibool_t *was_null_ptr)
{
    static const uint32_t null_bits = 0x000000006c6c756e; /* "null" */

    char *p = *pp;

    uint32_t next4 = *(uint32_t *)p;

    if (next4 != null_bits) {
        *was_null_ptr = ifalse;
        return 0;
    }

    *was_null_ptr = itrue;
    *pp           = p + sizeof ("null") - 1;

    return 0;
}

static psv_error_t INLINE psv_read_mask_bool (uint64_t mask, char **pp, char *pe, ibool_t *output_ptr)
{
    static const uint64_t true_mask  = 0x00000000ffffffff;
    static const uint64_t true_bits  = 0x0000000065757274; /* "true" */
    static const uint64_t false_mask = 0x000000ffffffffff;
    static const uint64_t false_bits = 0x00000065736c6166; /* "false" */

    char *p = *pp;

    uint64_t next8 = *(uint64_t *)p | mask;

    int is_true  = (next8 & true_mask)  == true_bits;
    int is_false = (next8 & false_mask) == false_bits;

    if (is_true) {
        *output_ptr = itrue;
        *pp         = p + sizeof ("true") - 1;
    } else if (is_false) {
        *output_ptr = ifalse;
        *pp         = p + sizeof ("false") - 1;
    } else {
        return psv_alloc_error ("was not a boolean", p, pe - p);
    }

    return 0;
}
static psv_error_t INLINE psv_read_json_bool (char **pp, char *pe, ibool_t *output_ptr)
{
    return psv_read_mask_bool (0x0, pp, pe, output_ptr);
}

static psv_error_t INLINE psv_read_bool (char **pp, char *pe, ibool_t *output_ptr)
{
    static const uint64_t to_lower = 0x2020202020202020;
    return psv_read_mask_bool (to_lower, pp, pe, output_ptr);
}

static psv_error_t INLINE psv_read_int (char **pp, char *pe, iint_t *output_ptr)
{
    *output_ptr = strtol (*pp, pp, 10);
    return 0;
}

static psv_error_t INLINE psv_read_double (char **pp, char *pe, idouble_t *output_ptr)
{
    *output_ptr = strtod (*pp, pp);
    return 0;
}

static psv_error_t psv_read_buffer (psv_state_t *s)
{
    psv_error_t error;

    const char  *buffer_ptr  = s->buffer_ptr;
    const size_t buffer_size = s->buffer_size;
    const char  *end_ptr     = buffer_ptr + buffer_size;
    const char  *line_ptr    = buffer_ptr;

    iint_t  fact_count      = s->fact_count;
    iint_t  entity_count    = s->entity_count;
    char   *entity_cur      = s->entity_cur;
    size_t  entity_cur_size = s->entity_cur_size;

    for (;;) {
        const size_t bytes_remaining = end_ptr - line_ptr;
        const char  *n_ptr           = memchr (line_ptr, '\n', bytes_remaining);

        if (n_ptr == 0) {
            s->fact_count       = fact_count;
            s->entity_count     = entity_count;
            s->entity_cur       = entity_cur;
            s->entity_cur_size  = entity_cur_size;
            s->buffer_remaining = bytes_remaining;
            return 0;
        }

        fact_count++;

        const char  *entity_ptr  = line_ptr;
        const char  *entity_end  = memchr (entity_ptr, '|', n_ptr - entity_ptr);
        const size_t entity_size = entity_end - entity_ptr;

        if (entity_end == 0) {
            error = psv_alloc_error ("missing |", entity_ptr, n_ptr - entity_ptr);
            goto on_error;
        }

        const char  *attrib_ptr  = entity_end + 1;
        const char  *attrib_end  = memchr (attrib_ptr, '|', n_ptr - attrib_ptr);
        const size_t attrib_size = attrib_end - attrib_ptr;

        if (attrib_end == 0) {
            error = psv_alloc_error ("missing |", attrib_ptr, n_ptr - attrib_ptr);
            goto on_error;
        }

        const char *time_ptr;
        const char *n11_ptr = n_ptr - 11;
        const char *n21_ptr = n_ptr - 21;

        if (*n11_ptr == '|') {
            time_ptr = n11_ptr + 1;
        } else if (*n21_ptr == '|') {
            time_ptr = n21_ptr + 1;
        } else {
            error = psv_alloc_error ("expected |", n21_ptr, n_ptr - n21_ptr);
            goto on_error;
        }

        const char  *time_end   = n_ptr;
        const size_t time_size  = time_end - time_ptr;

        const char  *value_ptr  = attrib_end + 1;
        const char  *value_end  = time_ptr - 1;
        const size_t value_size = value_end - value_ptr;

        const bool new_entity = entity_cur_size != entity_size
                             || memcmp (entity_cur, entity_ptr, entity_size) != 0;

        if (new_entity) {
            if (entity_cur_size != 0) {
                psv_write_outputs (s->output_fd, entity_cur, s->fleet);
            }

            memcpy (entity_cur, entity_ptr, entity_size);
            entity_cur[entity_size] = 0;
            entity_cur_size = entity_size;

            error = psv_configure_fleet (entity_cur, entity_cur_size, &s->chord_cur, s->fleet);
            if (error) goto on_error;

            entity_count++;
        }

        idate_t date;
        error = psv_read_date (time_ptr, time_size, &date);
        if (error) goto on_error;

        error = psv_read_fact (attrib_ptr, attrib_size, value_ptr, value_size, date, s->fleet);
        if (error) goto on_error;

        line_ptr = n_ptr + 1;
    }

on_error:
    s->entity_count = entity_count;
    s->fact_count   = fact_count;
    return error;
}

void psv_snapshot (psv_config_t *cfg)
{
    static const size_t psv_read_error = (size_t) -1;

    int input_fd  = (int)cfg->input_fd;
    int output_fd = (int)cfg->output_fd;
    int chord_fd  = (int)cfg->chord_fd;

    ichord_file_t chord_file;
    psv_error_t chord_mmap_error = ichord_file_mmap (chord_fd, &chord_file);
    if (chord_mmap_error) {
        cfg->error        = chord_mmap_error;
        cfg->fact_count   = 0;
        cfg->entity_count = 0;
        return;
    }

    ifleet_t *fleet = psv_alloc_fleet (chord_file.max_chord_count);

    char buffer_ptr[psv_buffer_size + 1];
    char entity_cur[psv_buffer_size + 1];
    memset (buffer_ptr, 0, psv_buffer_size + 1);
    memset (entity_cur, 0, psv_buffer_size + 1);

    static const psv_state_t empty_state;
    psv_state_t state = empty_state;
    state.chord_cur   = chord_file.chords;
    state.buffer_ptr  = buffer_ptr;
    state.entity_cur  = entity_cur;
    state.fleet       = fleet;
    state.output_fd   = output_fd;

    size_t buffer_offset = 0;

    for (;;) {
        psv_collect_fleet (fleet);

        size_t bytes_read = read ( input_fd
                                 , buffer_ptr  + buffer_offset
                                 , psv_buffer_size - buffer_offset );

        if (bytes_read == psv_read_error) {
            cfg->error = psv_alloc_error ("error reading input", 0, 0);
            break;
        }

        if (bytes_read == 0) {
            if (state.entity_cur_size != 0) {
                psv_write_outputs (state.output_fd, state.entity_cur, state.fleet);
            }
            break;
        }

        size_t bytes_avail = buffer_offset + bytes_read;
        state.buffer_size  = bytes_avail;

        psv_error_t error = psv_read_buffer (&state);

        if (error) {
            cfg->error = psv_error_add_line (state.fact_count, error);
            break;
        }

        size_t bytes_remaining = state.buffer_remaining;

        memcpy ( buffer_ptr
               , buffer_ptr + bytes_avail - bytes_remaining
               , bytes_remaining );

        buffer_offset = bytes_remaining;
    }

    psv_error_t chord_unmap_error = ichord_file_unmap (&chord_file);
    if (cfg->error == 0)
        cfg->error = chord_unmap_error;

    cfg->fact_count   = state.fact_count;
    cfg->entity_count = state.entity_count;
}

// todo handle error when printing
void psv_iprintf(int fd, char* buf, size_t size, const char* restrict fmt, ...)
{
    va_list ap;

    if (size > 0) {
      snprintf (buf, size, fmt, ap);
    } else {
      write (fd, buf, psv_output_buf_size);
    }

}
#endif
