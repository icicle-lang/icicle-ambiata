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
    iint_t drop_fd;
    iint_t chord_fd;

    /* outputs */
    const char  *error;
    iint_t       fact_count;
    iint_t       entity_count;

    /* stuff */
    const size_t  facts_limit;   /* maximum number of facts per entity-attribute */
    const ibool_t has_drop; /* whether to write discarded facts to output or drop log */

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

    /* dropped facts file descriptor */
    int drop_fd;

    /* entities that exceed the entity-attribute count limit and were partially dropped */
    char     *entity_dropped;      /* save the previously dropped entity, because if we are */
    size_t    entity_dropped_size; /* dropping a new entity, we need to record that. */
    iint_t    entity_dropped_count;
    size_t    facts_limit;
    ibool_t   has_drop;

} psv_state_t;


/* forward declarations for functions, implemented by generated code */
static ifleet_t * INLINE psv_alloc_fleet (iint_t max_chord_count);

static void INLINE psv_collect_fleet (ifleet_t *fleet);

static ierror_loc_t INLINE psv_configure_fleet (const char *entity, size_t entity_size, const ichord_t **chord, ifleet_t *fleet);

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
  , ifleet_t     *fleet
  , const size_t  max_ent_attr_count);
#else
static ierror_loc_t INLINE psv_read_fact
  ( const char   *entity_ptr
  , const size_t  entity_size
  , const char   *value_ptr
  , const size_t  value_size
  , const char   *time_ptr
  , const size_t  time_size
  , ifleet_t     *fleet
  , const size_t  max_ent_attr_count);
#endif

static ierror_msg_t INLINE psv_write_outputs
  ( int fd
  , char  *output_start
  , char  *output_end
  , char **output_ptr
  , const char *entity
  , size_t entity_size
  , ifleet_t *fleet );

static ierror_msg_t INLINE psv_output_flush
  ( int fd
  , char *ps
  , char **pp );

static ierror_msg_t INLINE psv_flush_to_output
  ( psv_state_t *s );

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

static ibool_t INLINE psv_is_dropping_new (const psv_state_t *s, const char *drop, const size_t drop_size)
{
    return s->entity_dropped == 0
        || psv_compare (drop, drop_size, s->entity_dropped, s->entity_dropped_size);
}

static ibool_t INLINE psv_is_dropping_this (const psv_state_t *s, const char *entity, const size_t entity_size)
{
    return s->entity_dropped != 0
        && !psv_compare (entity, entity_size, s->entity_dropped, s->entity_dropped_size);
}

/* Seek to a line where the entity changes, or EOF. Returns true if we found a new entity. */
static ibool_t INLINE psv_skip_to_next_entity (psv_state_t *s)
{
    const char  *end_ptr    = s->input_ptr + s->input_size;
    const char  *start_ptr  = end_ptr - s->input_remaining;
    const char  *line_ptr   = start_ptr;
    const char  *n_ptr      = 0;
    const char  *drop_me    = s->entity_dropped;
    const size_t drop_size  = s->entity_dropped_size;

    for (;;) {
        const size_t bytes_remaining = end_ptr - line_ptr;

        /* mark some of the input buffer as skipped, this needs to be reset on the next read_whole_buffer */
        s->input_ptr = line_ptr;
        s->input_size = bytes_remaining;
        s->input_remaining = bytes_remaining;

        n_ptr = memchr (line_ptr, '\n', bytes_remaining);

        if (n_ptr == 0) {
            return ifalse;
        }

        const char  *entity_ptr  = line_ptr;
        const char  *entity_end  = memchr (entity_ptr, '|', n_ptr - entity_ptr);
        const size_t entity_size = entity_end - entity_ptr;
        const int    new_entity  =  (drop_size != entity_size)
                                 || (psv_compare ( drop_me, drop_size
                                                 , entity_ptr, entity_size ) != 0);

        if (new_entity) {
            /* start the input buffer here */
            return itrue;
        } else {
            /* just skip this fact */
            s->entity_dropped_count++;
            line_ptr = n_ptr + 1;
        }
    }
}

/* Record entity-attributes that were dropped */
static int psv_write_dropped_count (psv_state_t *s) {
  if (s->entity_dropped) {
    char *msg = calloc (error_msg_size, 1);

    const size_t msg_size = snprintf ( msg, error_msg_size
                                     , "total: %" PRId64 " facts for %.*s were dropped.\n\n"
                                     , s->entity_dropped_count, (int) s->entity_dropped_size, s->entity_dropped);

    write (s->drop_fd, msg, msg_size);
    free (msg);

  }

  return 0;
}

static ierror_loc_t psv_write_dropped_entity_cur (psv_state_t *s, const ierror_loc_t error)
{
    /* save the previously dropped entity, because if we are
       dropping a new entity, we need to record that. */
    const int    new_dropped = psv_is_dropping_new (s, s->entity_cur, s->entity_cur_size);
    ierror_msg_t msg         = ierror_loc_pretty (error, s->fact_count);
    const size_t msg_size    = strnlen (msg, error_msg_size);

    if (new_dropped) {
        psv_write_dropped_count (s);

        /* write the error message */
        size_t bytes_written = write (s->drop_fd, msg, msg_size);
        if (bytes_written == 0)
            return error;

        /* write the partial result to drop or output */
        int fd = s->has_drop ? s->drop_fd : s->output_fd;

        msg = psv_write_outputs (fd, s->output_start, s->output_end, &s->output_ptr, s->entity_cur, s->entity_cur_size, s->fleet);

        if (msg)
            return error;

        msg = psv_output_flush (fd, s->output_start, &s->output_ptr);

        if (msg)
            return error;

        if (s->entity_dropped)
            free(s->entity_dropped);

        /* this is the latest entity that was dropped */
        s->entity_dropped_count = 0;
        s->entity_dropped_size  = s->entity_cur_size;
        s->entity_dropped       = calloc (s->entity_cur_size, 1);
        memcpy (s->entity_dropped, s->entity_cur, s->entity_cur_size);
    }

    return 0;
}

static ierror_loc_t psv_read_buffer (psv_state_t *s, const size_t facts_limit)
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
            ibool_t      dropping = psv_is_dropping_this (s, s->entity_cur, s->entity_cur_size);

#if ICICLE_PSV_OUTPUT_SPARSE
            int          fd       = s->has_drop && dropping ? s->drop_fd : s->output_fd;
            ierror_msg_t msg      = psv_write_outputs (fd, s->output_start, s->output_end, &s->output_ptr, entity_cur, entity_cur_size, s->fleet);

            if (msg) {
              error = ierror_loc_format (0, 0, "%s", msg);
              goto on_error;
            }

            msg = psv_output_flush (fd, s->output_start, &s->output_ptr);

            if (msg) {
              error = ierror_loc_format (0, 0, "%s", msg);
              goto on_error;
            }
#else
            if (!dropping) {
                int          fd  = s->output_fd;
                ierror_msg_t msg = psv_write_outputs (fd, s->output_start, s->output_end, &s->output_ptr, entity_cur, entity_cur_size, s->fleet);

                if (msg) {
                  error = ierror_loc_format (0, 0, "%s", msg);
                  goto on_error;
                }

                msg = psv_output_flush (fd, s->output_start, &s->output_ptr);

                if (msg) {
                  error = ierror_loc_format (0, 0, "%s", msg);
                  goto on_error;
                }
            }
#endif

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
        error = psv_read_fact (entity_ptr, entity_size, attrib_ptr, attrib_size, value_ptr, value_size, time_ptr, time_size, s->fleet, facts_limit);
        if (error)
            goto on_error;
#else
        error = psv_read_fact (entity_ptr, entity_size, value_ptr, value_size, time_ptr, time_size, s->fleet, facts_limit);
        if (error)
          goto on_error;
#endif

        line_ptr = n_ptr + 1;
    }

on_error:
    s->input_remaining = end_ptr - line_ptr;
    s->entity_count    = entity_count;
    s->fact_count      = fact_count;
    s->entity_cur_size = entity_cur_size;

    error->line_start = line_ptr;
    error->line_end   = n_ptr;

    return error;
}


static ierror_loc_t psv_read_whole_buffer (psv_config_t *cfg, psv_state_t *s)
{
    /* we need to read all whole lines in the input buffer
       even if we need to drop some of them */
    ibool_t      keep_going = itrue;
    ierror_loc_t error      = 0;

    while (keep_going) {
        error = psv_read_buffer (s, cfg->facts_limit);

        if (error) {
            /* try to skip to the next entity, if there is no new entity
               in the buffer, this error will be triggered again. */
            if (error->tag == IERROR_LIMIT_EXCEEDED) {
                const int line = s->fact_count;

                error = psv_write_dropped_entity_cur (s, error);
                if (error) {
                    const char *error_pretty = ierror_loc_pretty (error, line);
                    cfg->error = ierror_msg_format ("failed to drop entity:\n%s\n", error_pretty);
                }

                /* stop if either we have skipped all rows for this entity,
                   or we have exhausted the buffer */
                keep_going = psv_skip_to_next_entity (s);

            } else {
                goto on_error;
            }
        } else {
            /* we finished reading the buffer, it needs to be refilled */
            keep_going = ifalse;
        }
    }

on_error:
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

static ierror_msg_t INLINE psv_flush_to_output (psv_state_t *s)
{
    return psv_output_flush (s->output_fd, s->output_start, &s->output_ptr);
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

    int fd;
    int input_fd  = (int)cfg->input_fd;
    int output_fd = (int)cfg->output_fd;
    int drop_fd   = (int)cfg->drop_fd;
    int chord_fd  = (int)cfg->chord_fd;

    /* System.IO.Handles are in non-blocking mode by default */
    psv_set_blocking_mode (input_fd);
    psv_set_blocking_mode (output_fd);
    psv_set_blocking_mode (drop_fd);
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
    state.drop_fd      = drop_fd;
    state.facts_limit  = cfg->facts_limit;
    state.has_drop     = cfg->has_drop;

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
            ibool_t dropping = psv_is_dropping_this (&state, state.entity_cur, state.entity_cur_size);
#if ICICLE_PSV_OUTPUT_SPARSE
            int     fd       = state.has_drop && dropping ? state.drop_fd : state.output_fd;

            cfg->error = psv_write_outputs (fd, state.output_start, state.output_end, &state.output_ptr, state.entity_cur, state.entity_cur_size, state.fleet);

            if (cfg->error != 0) {
                cfg->error = psv_output_flush (fd, state.output_start, &state.output_ptr);
            }
#else
            if (!dropping) {
                int fd = state.output_fd;

                cfg->error = psv_write_outputs (fd, state.output_start, state.output_end, &state.output_ptr, state.entity_cur, state.entity_cur_size, state.fleet);

                if (cfg->error != 0) {
                  cfg->error = psv_output_flush (fd, state.output_start, &state.output_ptr);
                }
            }
#endif

            break;
        }

        size_t input_avail = input_offset + bytes_read;
        state.input_size   = input_avail;
        state.input_ptr    = input_ptr;

        ierror_loc_t error = psv_read_whole_buffer (cfg, &state);

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
        cfg->error = psv_flush_to_output (&state);
    }

    ierror_msg_t chord_unmap_error = ichord_file_unmap (&chord_file);
    if (cfg->error == 0)
        cfg->error = chord_unmap_error;

    cfg->fact_count   = state.fact_count;
    cfg->entity_count = state.entity_count;

    free (input_ptr);
    free (entity_cur);
    free (output_ptr);

    if (state.entity_dropped) {
        psv_write_dropped_count (&state);
        free (state.entity_dropped);
    }
}

#endif
