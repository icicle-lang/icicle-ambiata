#include "53-zebra-data.h"

#if !ICICLE_NO_INPUT
#if ICICLE_ZEBRA

typedef struct {
    /* inputs */
    const char *input_path;
    iint_t output_fd;
    iint_t chord_fd;

    /* outputs */
    const char  *error;
    iint_t fact_count;
    iint_t entity_count;
    const size_t output_buffer_size;

} zebra_config_t;

typedef struct zebra_state {
    /* output buffer */
    char *output_start;
    char *output_end;
    char *output_ptr;

    /* stats */
    iint_t fact_count;
    iint_t entity_count;

    /* fleet state */
    ifleet_t *fleet;

    /* output file descriptor */
    int output_fd;

    /* read state */
    int64_t  entity_alloc_limit;
    int64_t  attribute_count;
    int64_t *entity_fact_offset;
    int64_t *entity_fact_count;
    ibool_t *entity_attribute_leftover;

} zebra_state_t;


static int64_t zebra_translate_table
  ( anemone_mempool_t *mempool
  , iint_t elem_start
  , iint_t count
  , void **dst
  , const zebra_table_t *table );

static int64_t zebra_translate_column
  ( anemone_mempool_t *mempool
  , iint_t elem_start
  , iint_t count
  , void **dst
  , const zebra_column_t *src );

static ierror_msg_t zebra_read_entity
  ( piano_t *piano
  , zebra_state_t *state
  , zebra_entity_t *entity );

ierror_msg_t zebra_snapshot_step
  ( piano_t *piano
  , zebra_state_t *state
  , zebra_entity_t *entity );

/* map a zebra entity to sea fleet inputs */
static ierror_msg_t zebra_translate
    ( piano_t              *piano
    , zebra_state_t        *state
    , zebra_entity_t       *entity
    , anemone_mempool_t    *mempool
    , int                   attribute_ix
    , itime_t               chord_time
    , iint_t               *new_fact_count
    , ierror_t            **new_tombstone
    , itime_t             **new_fact_time
    , iint_t                columns_to_fill
    , void                **dst
    , const zebra_entity_t *src )
{
    zebra_attribute_t *attribute = &src->attributes[attribute_ix];
    iint_t columns_count = attribute->table.column_count;

    /* zebra columns must match the number of input variables */
    if (columns_count != columns_to_fill) {
        return ierror_msg_format
          ( "zebra_translate: attribute_ix=%d, column_count=%d, columns_to_fill=%d\n"
          , attribute_ix
          , columns_count
          , columns_to_fill );
     }

    zebra_table_t *table = &attribute->table;
    int64_t row_count = table->row_count;
    int64_t fact_count = 0;

    /* number of total facts for this entity */
    if (state->entity_fact_count[attribute_ix] == 0) {
        /* temporary fix until icicle uses the same time as zebra */
        itime_t zebra_chord_time = itime_to_epoch_seconds (chord_time);

        /* the number of facts before chord time for this entity */
        for (iint_t i = 0; i != row_count; ++i) {
            itime_t time = attribute->times[i];
            if (zebra_chord_time <= time) {
                break;
            }
            fact_count++;
        }
        state->entity_fact_count[attribute_ix] = fact_count;
    } else {
        fact_count = state->entity_fact_count[attribute_ix];
    }

    /* number of unread facts for this entity */
    iint_t fact_offset    = state->entity_fact_offset[attribute_ix];
    iint_t fact_remaining = fact_count - fact_offset;
    iint_t fact_to_read   = 0;

    if (fact_remaining > zebra_chunk_fact_count) {
        fact_to_read = zebra_chunk_fact_count;
    } else {
        fact_to_read = fact_remaining;
    }

    state->fact_count                       += fact_to_read;
    state->entity_fact_offset[attribute_ix] += fact_to_read;

    /* temporary fix until icicle uses the same time as zebra */
    itime_t *sadface_times = anemone_mempool_alloc (mempool, fact_to_read * sizeof (itime_t));

    for (iint_t i = 0; i != fact_to_read; ++i) {
        sadface_times[i] = itime_from_epoch_seconds (attribute->times[i + fact_offset]);
    }

    *new_fact_count = fact_to_read;
    *new_tombstone  = (ierror_t*) attribute->tombstones + fact_offset;
    *new_fact_time  = sadface_times;

    zebra_translate_table (mempool, fact_offset, fact_to_read, dst, &attribute->table);

    return 0;
}

static int64_t zebra_translate_table
    ( anemone_mempool_t *mempool
    , iint_t elem_start
    , iint_t count
    , void **dst
    , const zebra_table_t *table )
{
    int64_t         offset = 0;
    int64_t         cols_count  = table->column_count;
    zebra_column_t *cols = table->columns;

    for (int64_t i = 0; i != cols_count; ++i) {
        offset += zebra_translate_column (mempool, elem_start, count, dst + offset, cols + i);
    }

    return offset;
}

static int64_t zebra_translate_array
    ( anemone_mempool_t *mempool
    , iint_t elem_start
    , iint_t count
    , void **dst
    , const zebra_data_t *data )
{
    return zebra_translate_table
       ( mempool
       , elem_start
       , count
       , (void**)dst
       , &data->a.table );
}

static int64_t zebra_translate_column
    ( anemone_mempool_t *mempool
    , iint_t elem_start
    , iint_t count
    , void **dst
    , const zebra_column_t *src )
{
    const zebra_type_t type = src->type;
    const zebra_data_t data = src->data;

    switch (type) {
        case ZEBRA_BYTE:
            {
                /* assume bytecolumn is a string */
                uint8_t* bytes = anemone_mempool_alloc(mempool, count + 1);
                memcpy(bytes, data.b + elem_start, count);
                bytes[count] = '\0';
                *dst = bytes;
                return 1;
            }

        case ZEBRA_INT:
            *dst = data.i + elem_start;
            return 1;

        case ZEBRA_DOUBLE:
            *dst = data.d + elem_start;
            return 1;

        case ZEBRA_ARRAY:
            {
                istring_t *strings = anemone_mempool_alloc (mempool, count * sizeof (istring_t));
                int offset = 1;
                for (int i = 0; i != count; ++i) {
                    int64_t index = i + elem_start;
                    int64_t start = data.a.n[index]   - data.a.n[0];
                    int64_t len   = data.a.n[index+1] - data.a.n[index];
                    offset = zebra_translate_array (mempool, start, len, (void**)(strings + i), &data);
                }
                *dst = strings;
                return offset;
            }
    }

    return 0;
}

zebra_state_t *zebra_alloc_state (piano_t *piano, zebra_config_t *cfg, int64_t attribute_count)
{
    zebra_state_t *state = malloc(sizeof(zebra_state_t));

    int fd;
    int output_fd = (int)cfg->output_fd;
    int chord_fd  = (int)cfg->chord_fd;

    /* System.IO.Handles are in non-blocking mode by default */
    psv_set_blocking_mode (output_fd);
    psv_set_blocking_mode (chord_fd);

    /* If we have a piano, we know we are playing a chord */
    int64_t max_chord_count;
    if (piano) {
        max_chord_count = piano_max_count(piano);
    } else {
        max_chord_count = 1;
    }

    /* max_row_count is unused */
    ifleet_t *fleet = psv_alloc_fleet (max_chord_count, 0);
    char *output_ptr = calloc (cfg->output_buffer_size + 1, 1);

    state->fleet = fleet;

    state->output_start = output_ptr;
    state->output_end   = output_ptr + cfg->output_buffer_size - 1;
    state->output_ptr   = output_ptr;
    state->output_fd    = output_fd;

    state->attribute_count    = attribute_count;
    state->entity_alloc_limit = 0;
    state->entity_fact_offset = calloc(attribute_count, sizeof (int64_t));
    state->entity_fact_count  = calloc(attribute_count, sizeof (int64_t));

    return state;
}

void zebra_collect_state (zebra_config_t *cfg, zebra_state_t *state)
{
    cfg->entity_count = state->entity_count;
    cfg->fact_count = state->fact_count;
    free (state);
}

ierror_msg_t zebra_read_step (piano_t *piano, zebra_state_t *state, zebra_entity_t *entity)
{
    ifleet_t *fleet = state->fleet;
    psv_collect_fleet (fleet);

    ierror_loc_t  error_loc = psv_configure_fleet ((char*) entity->id_bytes, entity->id_length, piano, state->fleet);
    if (error_loc) return ierror_loc_pretty (error_loc, 0);

    ierror_msg_t error = zebra_read_entity (piano, state, entity);
    if (error) return error;

    /* keep going if we have not finished reading all facts for this entity */
    for (int64_t i = 0; i != state->attribute_count; ++i) {
        if (state->entity_fact_offset[i] != state->entity_fact_count[i]) {
            return zebra_read_step (piano, state, entity);
        }
        state->entity_fact_count [i] = 0;
        state->entity_fact_offset[i] = 0;
    }

    return 0;
}

ierror_msg_t zebra_snapshot_step (piano_t *piano, zebra_state_t *state, zebra_entity_t *entity)
{
    ierror_msg_t  error;
    ierror_loc_t  error_loc;
    ifleet_t     *fleet = state->fleet;
    int           fd = state->output_fd;
    uint8_t      *entity_id = entity->id_bytes;
    int64_t       entity_id_size = entity->id_length;

    state->entity_count++;

    error = zebra_read_step (piano, state, entity);
    if (error) return error;

    error = psv_write_output
        ( fd
        , state->output_start
        , state->output_end
        , &state->output_ptr
        , (char*) entity_id
        , entity_id_size
        , state->fleet );
    if (error) return error;

    error = psv_flush_output
        ( fd
        , state->output_start
        , &state->output_ptr );
    if (error) return error;

    return 0;
}

#endif
#endif
