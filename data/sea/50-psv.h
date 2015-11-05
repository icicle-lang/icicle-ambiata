#include "21-date.h"

/* forward declarations, implemented by generated code */
typedef struct istate istate_t;

static void INLINE psv_write_values (int fd, istate_t *state);

static void INLINE psv_read_value
  ( istate_t     *state
  , idate_t       date
  , const char   *attrib
  , const size_t  attrib_size
  , const char   *value
  , const size_t  value_size );


typedef struct {
    /* inputs */
    istring_t input;
    istring_t output;

    /* outputs */
    istring_t error;
} psv_snapshot_t;

typedef struct {
    /* input buffer */
    const char *buffer_ptr;
    size_t      buffer_size;

    /* current entity/attribute */
    char       *entity_cur; /* invariant: these must point to a block of memory at least */
    char       *attrib_cur; /*            as large as the input buffer or we'll overflow */

    /* icicle state */
    istate_t   *icicle_state;

    /* output file descriptor */
    int         output_fd;
} psv_state_t;

static const size_t psv_buffer_size = 16*1024;

static void psv_debug (const char *msg, const char *value_ptr, const size_t value_size)
{
    char value_text[4*1024] = {0};
    memcpy (value_text, value_ptr, MIN (value_size, sizeof (value_text) - 1));

    fprintf (stderr, "psv_debug: %s: %s\n", msg, value_text);
}

static void psv_die_parse_error (const char *msg, const char *value_ptr, const size_t value_size)
{
    psv_debug (msg, value_ptr, value_size);
    exit (1);
}

static idate_t psv_read_date (const char *time_ptr, const size_t time_size)
{
                       /* time_ptr + 0123456789 */
    const size_t yyyymmdd = sizeof ("YYYY-MM-DD");

    if (yyyymmdd != time_size + 1 || *(time_ptr + 4) != '-' || *(time_ptr + 7) != '-')
      psv_die_parse_error ("expected YYYY-MM-DD", time_ptr, time_size);

    char *year_end, *month_end, *day_end;
    const iint_t year  = strtol (time_ptr + 0, &year_end,  10);
    const iint_t month = strtol (time_ptr + 5, &month_end, 10);
    const iint_t day   = strtol (time_ptr + 8, &day_end,   10);

    if (year_end != time_ptr + 4 || month_end != time_ptr + 7 || day_end != time_ptr + 10)
      psv_die_parse_error ("expected YYYY-MM-DD", time_ptr, time_size);

    return idate_from_gregorian (year, month, day);
}

static size_t psv_read_buffer (psv_state_t *s)
{
    char *entity_cur = s->entity_cur;
    char *attrib_cur = s->attrib_cur;

    const char  *buffer_ptr  = s->buffer_ptr;
    const size_t buffer_size = s->buffer_size;
    const char  *end_ptr     = buffer_ptr + buffer_size;
    const char  *line_ptr    = buffer_ptr;

    for (;;) {
        const size_t bytes_remaining = end_ptr - line_ptr;
        const char  *n_ptr           = memchr (line_ptr, '\n', bytes_remaining);

        if (n_ptr == 0) {
            s->entity_cur = entity_cur;
            s->attrib_cur = attrib_cur;
            return bytes_remaining;
        }

        const char  *entity_ptr  = line_ptr;
        const char  *entity_end  = memchr (entity_ptr, '|', n_ptr - entity_ptr);
        const size_t entity_size = entity_end - entity_ptr;

        if (entity_end == 0)
            psv_die_parse_error ("missing |", entity_ptr, n_ptr - entity_ptr);

        const char  *attrib_ptr  = entity_end + 1;
        const char  *attrib_end  = memchr (attrib_ptr, '|', n_ptr - attrib_ptr);
        const size_t attrib_size = attrib_end - attrib_ptr;

        if (attrib_end == 0)
            psv_die_parse_error ("missing |", attrib_ptr, n_ptr - attrib_ptr);

        const char  *time_ptr   = n_ptr - 10;
        const char  *time_end   = n_ptr;
        const size_t time_size  = time_end - time_ptr;

        if (*(time_ptr - 1) != '|')
            psv_die_parse_error ("expected |", time_ptr - 1, time_size + 1);

        const char  *value_ptr  = attrib_end + 1;
        const char  *value_end  = time_ptr - 1;
        const size_t value_size = value_end - value_ptr;

        const bool new_entity = memcmp (entity_cur, entity_ptr, entity_size) != 0;
        const bool new_attrib = memcmp (attrib_cur, attrib_ptr, attrib_size) != 0;

        if (new_entity) {
            psv_debug ("entity", entity_ptr, entity_size);
            psv_debug ("attrib", attrib_ptr, attrib_size);
            psv_debug ("time", time_ptr, time_size);
            psv_debug ("value", value_ptr, value_size);

            /* write output */
            psv_write_values (s->output_fd, s->icicle_state);

            memcpy (entity_cur, entity_ptr, entity_size);
            entity_cur[entity_size] = 0;

            memcpy (attrib_cur, attrib_ptr, attrib_size);
            attrib_cur[attrib_size] = 0;
        }
        else if (new_attrib) {
            memcpy (attrib_cur, attrib_ptr, attrib_size);
            attrib_cur[attrib_size] = 0;
        }

        const idate_t date = psv_read_date (time_ptr, time_size);
        psv_read_value (s->icicle_state, date, attrib_ptr, attrib_size, value_ptr, value_size);

        line_ptr = n_ptr + 1;
    }
}

void psv_snapshot (psv_snapshot_t *s)
{
    fprintf (stderr, "psv_snapshot: input = %s\n", s->input);
    int ifd = open (s->input,  O_RDONLY);

    if (ifd == -1) {
        s->error = "error loading input";
        return;
    }

    fprintf (stderr, "psv_snapshot: output = %s\n", s->output);
    int ofd = open (s->output, O_WRONLY | O_CREAT | O_TRUNC, 0644);

    if (ofd == -1) {
        s->error = "error loading output";
        return;
    }

    static const size_t psv_read_error = (size_t) -1;

    char buffer_ptr[psv_buffer_size+1];
    char entity_cur[psv_buffer_size+1];
    char attrib_cur[psv_buffer_size+1];
    buffer_ptr[psv_buffer_size] = '\0';
    entity_cur[psv_buffer_size] = '\0';
    attrib_cur[psv_buffer_size] = '\0';

    psv_state_t state = { 0, 0, 0, 0, 0, 0 };
    state.buffer_ptr = buffer_ptr;
    state.entity_cur = entity_cur;
    state.attrib_cur = attrib_cur;
    state.output_fd  = ofd;

    size_t buffer_offset = 0;

    int blocks = 0;

    for (;;) {
        fprintf (stderr, "psv_snapshot: read %zd bytes\n", psv_buffer_size - buffer_offset);

        size_t bytes_read = read ( ifd
                                 , buffer_ptr  + buffer_offset
                                 , psv_buffer_size - buffer_offset );

        if (bytes_read == psv_read_error) {
            s->error = "error reading input";
            return;
        }

        if (bytes_read == 0) {
            break;
        }

        blocks++;

        size_t bytes_avail = buffer_offset + bytes_read;
        state.buffer_size  = bytes_avail;

        size_t bytes_remaining = psv_read_buffer (&state);

        memcpy ( buffer_ptr
               , buffer_ptr + bytes_avail - bytes_remaining
               , bytes_remaining );

        buffer_offset = bytes_remaining;

        //char rest[psv_buffer_size] = {0};
        //memcpy (rest, buffer_ptr, bytes_remaining);
        //printf ("asx: remaining = %s (%zd bytes)\n", rest, bytes_remaining);
    }

    fprintf (stderr, "psv_snapshot: blocks = %d\n", blocks);
}



/* generated code */
typedef struct istate {
    iint_t   new_count;
    iint_t  *new_fact;
    idate_t *new_date;
} istate_t;

static void psv_read_value
  ( istate_t     *state
  , idate_t       date
  , const char   *attrib
  , const size_t  attrib_size
  , const char   *value
  , const size_t  value_size )
{
    fprintf (stderr, "%0llx\n", date);
}

static void INLINE psv_write_values (int fd, istate_t *state)
{
}
