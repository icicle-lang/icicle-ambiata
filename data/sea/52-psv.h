#include "51-chord.h"

#if !ICICLE_NO_INPUT

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
    const char  *error;
    iint_t       fact_count;
    iint_t       entity_count;
} psv_config_t;

typedef struct {
    /* input buffer */
    const char *input_ptr;
    size_t      input_size;
    size_t      input_remaining;

    /* input chords */
    const ichord_t *chord_cur;

    /* output buffer */
    char *output_start;
    char *output_end;
    char *output_ptr;

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

static ierror_loc_t INLINE psv_configure_fleet (const char *entity, size_t entity_size, const ichord_t **chord, ifleet_t *fleet);

static ierror_msg_t INLINE psv_write_outputs
  ( int fd
  , char  *output_start
  , char  *output_end
  , char **output_ptr
  , const char *entity
  , size_t entity_size
  , ifleet_t *fleet );

#if ICICLE_PSV_INPUT_SPARSE
static ierror_loc_t INLINE psv_read_fact
  ( const char   *entity_ptr
  , const size_t  entity_size
  , const char   *attrib_ptr
  , const size_t  attrib_size
  , const char   *value_ptr
  , const size_t  value_size
  , const char   *time_ptr
  , const size_t  time_size
  , ifleet_t     *fleet );
#else
static ierror_loc_t INLINE psv_read_fact
  ( const char   *entity_ptr
  , const size_t  entity_size
  , const char   *value_ptr
  , const size_t  value_size
  , const char   *time_ptr
  , const size_t  time_size
  , ifleet_t     *fleet );
#endif

/*
Constants
*/

static const size_t psv_max_row_count      = 128;
static const size_t psv_max_ent_attr_count = 1;
static const size_t psv_input_buffer_size  = 256*1024;
static const size_t psv_output_buffer_size = 256*1024;

/*
Input
*/

static int INLINE psv_compare (const char *xs, size_t xs_size, const char *ys, size_t ys_size)
{
    size_t min = MIN (xs_size, ys_size);
    int    cmp = memcmp (xs, ys, min);

    if (cmp != 0)
        return cmp;

    return xs_size - ys_size;
}

static ierror_loc_t psv_read_buffer (psv_state_t *s)
{
    ierror_loc_t error;

    const char  *buffer_ptr  = s->input_ptr;
    const size_t buffer_size = s->input_size;
    const char  *end_ptr     = buffer_ptr + buffer_size;
    const char  *line_ptr    = buffer_ptr;
    const char  *n_ptr       = 0;

    iint_t  fact_count      = s->fact_count;
    iint_t  entity_count    = s->entity_count;
    char   *entity_cur      = s->entity_cur;
    size_t  entity_cur_size = s->entity_cur_size;

    for (;;) {
        const size_t bytes_remaining = end_ptr - line_ptr;

        n_ptr = memchr (line_ptr, '\n', bytes_remaining);

        if (n_ptr == 0) {
            s->fact_count      = fact_count;
            s->entity_count    = entity_count;
            s->entity_cur      = entity_cur;
            s->entity_cur_size = entity_cur_size;
            s->input_remaining = bytes_remaining;
            return 0;
        }

        fact_count++;

        const char  *entity_ptr  = line_ptr;
        const char  *entity_end  = memchr (entity_ptr, '|', n_ptr - entity_ptr);
        const size_t entity_size = entity_end - entity_ptr;

        if (entity_end == 0) {
            error = ierror_loc_format (entity_ptr, n_ptr, "missing '|' after entity");
            goto on_error;
        }

        if (entity_size == 0) {
            error = ierror_loc_format (entity_ptr, entity_ptr, "entity was empty");
            goto on_error;
        }

#if ICICLE_PSV_INPUT_SPARSE
        const char  *attrib_ptr  = entity_end + 1;
        const char  *attrib_end  = memchr (attrib_ptr, '|', n_ptr - attrib_ptr);
        const size_t attrib_size = attrib_end - attrib_ptr;

        if (attrib_end == 0) {
            error = ierror_loc_format (attrib_ptr, n_ptr, "missing '|' after attribute");
            goto on_error;
        }
#endif

        const char *time_ptr;
        const char *n11_ptr = n_ptr - 11;
        const char *n21_ptr = n_ptr - 21;

        if (*n11_ptr == '|') {
            time_ptr = n11_ptr + 1;
        } else if (*n21_ptr == '|') {
            time_ptr = n21_ptr + 1;
        } else {
            error = ierror_loc_format (0, 0, "time must be in the format 'yyyy-mm-dd' or 'yyyy-mm-ddThh:mm:ssZ'");
            goto on_error;
        }

        const char  *time_end   = n_ptr;
        const size_t time_size  = time_end - time_ptr;

#if ICICLE_PSV_INPUT_SPARSE
        const char  *value_ptr  = attrib_end + 1;
#else
        const char  *value_ptr  = entity_end + 1;
#endif

        const char  *value_end  = time_ptr - 1;
        const size_t value_size = value_end - value_ptr;

        const int new_entity = psv_compare (entity_cur, entity_cur_size, entity_ptr, entity_size);

        if (new_entity < 0) {
            if (entity_cur_size != 0) {
                ierror_msg_t msg = psv_write_outputs
                    ( s->output_fd
                    , s->output_start
                    , s->output_end
                    , &s->output_ptr
                    , entity_cur
                    , entity_cur_size
                    , s->fleet );

                if (error) {
                    error = ierror_loc_format (0, 0, "%s", msg);
                    goto on_error;
                }
            }

            memcpy (entity_cur, entity_ptr, entity_size);
            entity_cur[entity_size] = 0;
            entity_cur_size = entity_size;

            error = psv_configure_fleet (entity_cur, entity_cur_size, &s->chord_cur, s->fleet);
            if (error) goto on_error;

            entity_count++;
        } else if (new_entity > 0 && entity_cur_size != 0) {
            error = ierror_loc_format
                ( entity_end
                , entity_ptr
                , "entity out of order: <%.*s> should be before <%.*s>"
                , entity_size
                , entity_ptr
                , entity_cur_size
                , entity_cur );
            goto on_error;
        }

#if ICICLE_PSV_INPUT_SPARSE
        error = psv_read_fact (entity_ptr, entity_size, attrib_ptr, attrib_size, value_ptr, value_size, time_ptr, time_size, s->fleet);
        if (error)
            goto on_error;
#else
        error = psv_read_fact (entity_ptr, entity_size, value_ptr, value_size, time_ptr, time_size, s->fleet);
        if (error)
          goto on_error;
#endif

        line_ptr = n_ptr + 1;
    }

on_error:
    s->entity_count = entity_count;
    s->fact_count   = fact_count;

    error->line_start = line_ptr;
    error->line_end   = n_ptr;

    return error;
}


/*
Output
*/

static ierror_msg_t INLINE psv_output_flush (int fd, char *ps, char **pp)
{
    size_t bytes_avail   = *pp - ps;
    size_t bytes_written = write (fd, ps, bytes_avail);

    if (bytes_written < bytes_avail) {
        return ierror_msg_alloc ("cannot write psv output to file", ps, bytes_avail);
    }

    *pp = ps;

    return 0;
}

#define ENSURE_SIZE(bytes_required)                                   \
    size_t bytes_remaining = pe - *pp;                                \
    if (bytes_remaining < bytes_required) {                           \
        ierror_msg_t error = psv_output_flush (fd, ps, pp);           \
        if (error) return error;                                      \
    }

static ierror_msg_t INLINE psv_output_char
    (int fd, char *ps, char *pe, char **pp, char value)
{
    ENSURE_SIZE (1);

    **pp = value;
    *pp += 1;

    return 0;
}

static ierror_msg_t INLINE psv_output_string
    (int fd, char *ps, char *pe, char **pp, const char *value_ptr, size_t value_size)
{
    ENSURE_SIZE (value_size);

    memcpy (*pp, value_ptr, value_size);
    *pp += value_size;

    return 0;
}

static ierror_msg_t INLINE psv_output_time
    (int fd, char *ps, char *pe, char **pp, itime_t value)
{
    ENSURE_SIZE (text_itime_max_size);

    size_t value_size = text_write_itime (value, *pp);
    *pp += value_size;

    return 0;
}

static ierror_msg_t INLINE psv_output_double
    (int fd, char *ps, char *pe, char **pp, idouble_t value)
{
    ENSURE_SIZE (text_idouble_max_size);

    size_t value_size = text_write_idouble (value, *pp);
    *pp += value_size;

    return 0;
}

static ierror_msg_t INLINE psv_output_int
    (int fd, char *ps, char *pe, char **pp, iint_t value)
{
    ENSURE_SIZE (text_iint_max_size);

    size_t value_size = text_write_iint (value, *pp);
    *pp += value_size;

    return 0;
}


/*
Main Loop
*/

static void psv_set_blocking_mode (int fd)
{
    const int flags = fcntl (fd, F_GETFL, 0);
    fcntl (fd, F_SETFL, flags & ~O_NONBLOCK);
}

void psv_snapshot (psv_config_t *cfg)
{
    static const size_t psv_read_error = (size_t) -1;
    ierror_msg_t psv_write_error = 0;

    int input_fd  = (int)cfg->input_fd;
    int output_fd = (int)cfg->output_fd;
    int chord_fd  = (int)cfg->chord_fd;

    /* System.IO.Handles are in non-blocking mode by default */
    psv_set_blocking_mode (input_fd);
    psv_set_blocking_mode (output_fd);
    psv_set_blocking_mode (chord_fd);

    ichord_file_t chord_file;
    ierror_msg_t chord_mmap_error = ichord_file_mmap (chord_fd, &chord_file);
    if (chord_mmap_error) {
        cfg->error        = chord_mmap_error;
        cfg->fact_count   = 0;
        cfg->entity_count = 0;
        return;
    }

    ifleet_t *fleet = psv_alloc_fleet (chord_file.max_chord_count);

    char *input_ptr  = calloc (psv_input_buffer_size + 1, 1);
    char *entity_cur = calloc (psv_input_buffer_size + 1, 1);
    char *output_ptr = calloc (psv_output_buffer_size + 1, 1);

    static const psv_state_t empty_state;
    psv_state_t state = empty_state;

    state.chord_cur    = chord_file.chords;
    state.input_ptr    = input_ptr;
    state.entity_cur   = entity_cur;
    state.output_start = output_ptr;
    state.output_end   = output_ptr + psv_output_buffer_size - 1;
    state.output_ptr   = output_ptr;
    state.fleet        = fleet;
    state.output_fd    = output_fd;

    size_t input_offset = 0;

    for (;;) {
        psv_collect_fleet (fleet);

        size_t bytes_read = read ( input_fd
                                 , input_ptr  + input_offset
                                 , psv_input_buffer_size - input_offset );

        if (bytes_read == psv_read_error) {
            cfg->error = ierror_msg_alloc ("error reading input", 0, 0);
            break;
        }

        if (bytes_read == 0) {
            if (state.entity_cur_size != 0) {
                psv_write_error = psv_write_outputs
                    ( state.output_fd
                    , state.output_start
                    , state.output_end
                    , &state.output_ptr
                    , state.entity_cur
                    , state.entity_cur_size
                    , state.fleet );

                if (psv_write_error != 0) {
                    cfg->error = psv_write_error;
                    break;
                }
            }
            break;
        }

        size_t input_avail = input_offset + bytes_read;
        state.input_size = input_avail;

        ierror_loc_t error = psv_read_buffer (&state);

        if (error) {
            cfg->error = ierror_loc_pretty (error, state.fact_count);
            break;
        }

        size_t input_remaining = state.input_remaining;

        memmove ( input_ptr
                , input_ptr + input_avail - input_remaining
                , input_remaining );

        input_offset = input_remaining;
    }

    if (!cfg->error) {
        cfg->error = psv_output_flush (state.output_fd, state.output_start, &state.output_ptr);
    }

    ierror_msg_t chord_unmap_error = ichord_file_unmap (&chord_file);
    if (cfg->error == 0)
        cfg->error = chord_unmap_error;

    cfg->fact_count   = state.fact_count;
    cfg->entity_count = state.entity_count;

    free (input_ptr);
    free (entity_cur);
    free (output_ptr);
}

#endif
