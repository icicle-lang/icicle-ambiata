#include "53-zebra-data.h"

#if !ICICLE_NO_INPUT
#if ICICLE_ZEBRA

typedef struct {
    /* inputs */
    const char *input_path;
    int64_t output_fd;
    int64_t chord_fd;
    int64_t drop_fd;
    ibool_t zebra_output;

    /* outputs */
    const char *error;
    int64_t fact_count;
    int64_t entity_count;

    /* configurable */
    const size_t output_buffer_size;
    const size_t chunk_fact_count;
    const size_t alloc_limit_bytes;

} zebra_config_t;

typedef struct zebra_state {
    /* file descriptors */
    int fd_output;
    int fd_dropped_entities;

    /* icicle fleet state */
    ifleet_t *fleet;

    /* zebra input stats */
    int64_t fact_count;
    int64_t entity_count;

    /* zebra input state */
    int64_t  attribute_count;
    int64_t *entity_fact_offset;
    int64_t *entity_fact_count;
    int64_t  chunk_fact_count;
    int64_t  alloc_limit_bytes;
    int64_t  entity_alloc_count;

    /* output mode */
    ibool_t zebra_output;

    /* psv output buffer */
    char *psv_output_start;
    char *psv_output_end;
    char *psv_output_ptr;

} zebra_state_t;

static int64_t zebra_attribute_count ();

static ierror_msg_t zebra_read_entity
    ( piano_t *piano
    , zebra_state_t *state
    , zebra_entity_t *entity );

ierror_msg_t zebra_snapshot_step
    ( piano_t *piano
    , zebra_state_t *state
    , zebra_entity_t *entity );

static int64_t zebra_translate_table
    ( anemone_mempool_t *mempool
    , int64_t elem_start
    , int64_t elem_count
    , void **dst
    , const zebra_table_t *src );

static ierror_msg_t zebra_output_fleet
    ( zebra_state_t *state
    , zebra_table_t **dst );

int64_t zebra_output_column
    ( void **src
    , zebra_column_t *empty );

anemone_mempool_t *fleet_get_mempool (ifleet_t *fleet);


/* map a zebra entity to sea fleet inputs */
static ierror_msg_t zebra_translate
    ( piano_t              *piano
    , zebra_state_t        *state
    , zebra_entity_t       *entity
    , anemone_mempool_t    *mempool
    , int64_t               attribute_ix
    , itime_t               chord_time
    , int64_t               *new_fact_count
    , ierror_t            **new_tombstone
    , itime_t             **new_fact_time
    , int64_t                columns_to_fill
    , void                **dst
    , const zebra_entity_t *src )
{
    const int64_t attribute_count = src->attribute_count;
    if (attribute_ix >= attribute_count) {
        fprintf (stderr, "Fatal error: attribute index (%" PRId64 ") is out of bounds for attribute count (%" PRId64 ")\n", attribute_ix, attribute_count);
    }

    zebra_attribute_t *attribute = &src->attributes[attribute_ix];
    zebra_table_t *table = &attribute->table;
    int64_t row_count = table->row_count;
    int64_t fact_count = 0;

    /* number of total facts for this entity */
    if (state->entity_fact_count[attribute_ix] == 0) {
        /* temporary fix until icicle uses the same time as zebra */
        itime_t zebra_chord_time = itime_to_epoch_seconds (chord_time);

        /* the number of facts before chord time for this entity */
        for (int64_t i = 0; i != row_count; ++i) {
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
    int64_t fact_offset    = state->entity_fact_offset[attribute_ix];
    int64_t fact_remaining = fact_count - fact_offset;
    int64_t fact_to_read   = 0;

    if (fact_remaining > state->chunk_fact_count) {
        fact_to_read = state->chunk_fact_count;
    } else {
        fact_to_read = fact_remaining;
    }

    state->fact_count                       += fact_to_read;
    state->entity_fact_offset[attribute_ix] += fact_to_read;

    /* temporary fix until icicle uses the same time as zebra */
    itime_t *sadface_times = anemone_mempool_alloc (mempool, fact_to_read * sizeof (itime_t));

    for (int64_t i = 0; i != fact_to_read; ++i) {
        sadface_times[i] = itime_from_epoch_seconds (attribute->times[i + fact_offset]);
    }

    *new_fact_count = fact_to_read;
    *new_tombstone  = (ierror_t*) attribute->tombstones + fact_offset;
    *new_fact_time  = sadface_times;

    zebra_translate_table (mempool, fact_offset, fact_to_read, dst, &attribute->table);

    return 0;
}

/* Translate Zebra enum tags to Icicle bool flags, e.g. is_some, is_right, etc.
   We only allow at most two variants since Icicle's runtime representation
   of enums is unsuitable for more.
 */
static int64_t zebra_translate_column_tags
    ( const int64_t variant_count
    , void **dst
    , const int64_t *src )
{
    if (variant_count > 2) {
        fprintf (stderr, "Fatal error: found an enum with %" PRId64 " variants. Icicle does not support Zebra enums with more than two variants.\n", variant_count);
        exit(1);
    }

    *dst = src;
    return 1;
}

static int64_t zebra_translate_column_nested
    ( anemone_mempool_t *mempool
    , int64_t elem_start
    , int64_t elem_count
    , void **dst
    , const int64_t *indices
    , const zebra_table_t *src )
{
    /* Only read nested tables of type binary now.
       FIXME: slice out nested tables, when we need to support them. */
    if (src->tag != ZEBRA_TABLE_BINARY) {
        fprintf (stderr, "Fatal error: found a nested table with tag %ud. Icicle only supports nested binary table for now.\n", src->tag);
        exit(1);
    }

    int64_t offset = 1;
    void **target = anemone_mempool_alloc (mempool, elem_count * 8);

    for (int i = 0; i != elem_count; ++i) {
        const int64_t elem_index = elem_start + i;
        const int64_t table_start = indices[elem_index] - indices[0];
        const int64_t table_row_count = indices[elem_index + 1] - indices[elem_index];

        /* read the inner tables */
        zebra_translate_table (mempool, table_start, table_row_count, target + i, src);
    }

    *dst = target;
    return offset;
}

static int64_t zebra_translate_column
    ( anemone_mempool_t *mempool
    , int64_t elem_start
    , int64_t elem_count
    , void **dst
    , const zebra_column_t *src )
{
    const zebra_column_tag_t tag = src->tag;
    const zebra_column_variant_t data = src->of;

    switch (tag) {
        case ZEBRA_COLUMN_UNIT: {
            return 0;
        }

        case ZEBRA_COLUMN_INT: {
            *dst = data._int.values + elem_start;
            return 1;
        }

        case ZEBRA_COLUMN_DOUBLE: {
            *dst = data._double.values + elem_start;
            return 1;
        }

        case ZEBRA_COLUMN_ENUM: {
            int64_t column_count = data._enum.columns.count;
            int64_t offset = zebra_translate_column_tags
                ( column_count
                , dst
                , data._enum.tags );
            for (int64_t i = 0; i != column_count; ++i) {
                offset += zebra_translate_column
                    ( mempool
                    , elem_start
                    , elem_count
                    , dst + offset
                    , data._enum.columns.columns + i );
            }
            return offset;
        }

        case ZEBRA_COLUMN_STRUCT: {
            int64_t offset = 0;
            int64_t column_count = data._struct.columns.count;
            for (int64_t i = 0; i != column_count; ++i) {
                offset += zebra_translate_column
                    ( mempool
                    , elem_start
                    , elem_count
                    , dst + offset
                    , data._struct.columns.columns + i );
            }
            return offset;
        }

        case ZEBRA_COLUMN_NESTED: {
            return zebra_translate_column_nested
                ( mempool
                , elem_start
                , elem_count
                , dst
                , data._nested.indices
                , &data._nested.table );
        }

        case ZEBRA_COLUMN_REVERSED: {
            return zebra_translate_column
                ( mempool
                , elem_start
                , elem_count
                , dst
                , (const zebra_column_t *) &data._reversed );
        }

    }

    fprintf (stderr, "Fatal error: unknown Zebra column tag: %ud\n", tag);
    exit (1);
}

static int64_t zebra_translate_table
    ( anemone_mempool_t *mempool
    , int64_t elem_start
    , int64_t elem_count
    , void **dst
    , const zebra_table_t *src )
{
    const int64_t row_count = src->row_count;
    const zebra_table_tag_t tag = src->tag;
    const zebra_table_variant_t data = src->of;

    IASSERT (elem_start + elem_count <= row_count);

    switch (tag) {
    case ZEBRA_TABLE_BINARY: {
            char *bytes = anemone_mempool_alloc(mempool, elem_count + 1);
            memcpy(bytes, data._binary.bytes + elem_start, elem_count);
            bytes[elem_count] = '\0';
            *dst = bytes;
            return 1;
        }

        case ZEBRA_TABLE_ARRAY: {
            return zebra_translate_column
                ( mempool
                , elem_start
                , elem_count
                , dst
                , data._array.values );
        }

        case ZEBRA_TABLE_MAP: {
            int64_t offset = 0;
            offset += zebra_translate_column
                ( mempool
                , elem_start
                , elem_count
                , dst
                , data._map.keys );
            offset += zebra_translate_column
                ( mempool
                , elem_start
                , elem_count
                , dst + offset
                , data._map.values );
            return offset;
        }
    }
    fprintf (stderr, "Fatal error: unknown Zebra table tag: %ud\n", tag);
    exit (1);
}

static int64_t zebra_translate_table
    ( anemone_mempool_t *mempool
    , int64_t elem_start
    , int64_t elem_count
    , void **dst
    , const zebra_table_t *src )
{
    const int64_t row_count = src->row_count;
    const zebra_table_tag_t tag = src->tag;
    const zebra_table_variant_t data = src->of;

    IASSERT (elem_start + elem_count <= row_count);

    switch (tag) {
    case ZEBRA_TABLE_BINARY: {
            char *bytes = anemone_mempool_alloc(mempool, elem_count + 1);
            memcpy(bytes, data._binary.bytes + elem_start, elem_count);
            bytes[elem_count] = '\0';
            *dst = bytes;
            return 1;
        }

        case ZEBRA_TABLE_ARRAY: {
            return zebra_translate_column
                ( mempool
                , elem_start
                , elem_count
                , dst
                , data._array.values );
        }

        case ZEBRA_TABLE_MAP: {
            int64_t offset = 0;
            offset += zebra_translate_column
                ( mempool
                , elem_start
                , elem_count
                , dst
                , data._map.keys );
            offset += zebra_translate_column
                ( mempool
                , elem_start
                , elem_count
                , dst + offset
                , data._map.values );
            return offset;
        }
    }
}

zebra_state_t *zebra_alloc_state (piano_t *piano, zebra_config_t *cfg)
{
    zebra_state_t *state = malloc(sizeof(zebra_state_t));

    int fd;
    int output_fd = (int)cfg->output_fd;
    int chord_fd  = (int)cfg->chord_fd;
    int drop_fd   = (int)cfg->drop_fd;

    /* System.IO.Handles are in non-blocking mode by default */
    psv_set_blocking_mode (output_fd);
    psv_set_blocking_mode (chord_fd);
    psv_set_blocking_mode (drop_fd);

    /* If we have a piano, we know we are playing a chord */
    int64_t max_chord_count;
    if (piano) {
        max_chord_count = piano_max_count(piano);
    } else {
        max_chord_count = 1;
    }

    ifleet_t *fleet = psv_alloc_fleet (max_chord_count, 0);

    state->fleet = fleet;

    int64_t attribute_count = zebra_attribute_count ();

    state->fd_output           = output_fd;
    state->fd_dropped_entities = drop_fd;

    state->attribute_count    = attribute_count;
    state->entity_fact_offset = calloc(attribute_count, sizeof (int64_t));
    state->entity_fact_count  = calloc(attribute_count, sizeof (int64_t));

    state->chunk_fact_count   = cfg->chunk_fact_count;
    state->alloc_limit_bytes  = cfg->alloc_limit_bytes;
    state->entity_alloc_count = 0;

    state->zebra_output = cfg->zebra_output;

    if (state->zebra_output) {
        state->psv_output_start = NULL;
        state->psv_output_end   = NULL;
        state->psv_output_ptr   = NULL;
    } else {
        char *output_ptr = calloc (cfg->output_buffer_size + 1, 1);
        state->psv_output_start = output_ptr;
        state->psv_output_end   = output_ptr + cfg->output_buffer_size - 1;
        state->psv_output_ptr   = output_ptr;
    }

    return state;
}

void zebra_collect_state (zebra_config_t *cfg, zebra_state_t *state)
{
    cfg->entity_count = state->entity_count;
    cfg->fact_count = state->fact_count;
    free (state->entity_fact_offset);
    free (state->entity_fact_count);
    free (state);
}

void zebra_write_dropped_entity (zebra_state_t *state, zebra_entity_t *entity) {
    char *msg = calloc (error_msg_size, 1);
    const size_t msg_size = snprintf (msg, error_msg_size, "%.*s\n", (int)entity->id_length, entity->id_bytes);
    write (state->fd_dropped_entities, msg, msg_size);
    free (msg);
}

ibool_t zebra_limit_exceeded (zebra_state_t *state) {
    return state->entity_alloc_count > state->alloc_limit_bytes;
}

/* A read and compute step */
ierror_msg_t zebra_read_step (piano_t *piano, zebra_state_t *state, zebra_entity_t *entity)
{
    ifleet_t *fleet = state->fleet;
    psv_collect_fleet (fleet);

    ierror_loc_t  error_loc = psv_configure_fleet ((char*) entity->id_bytes, entity->id_length, piano, state->fleet);
    if (error_loc) return ierror_loc_pretty (error_loc, 0);

    ibool_t read_all = ifalse;

    while (!read_all) {
        ierror_msg_t error = zebra_read_entity (piano, state, entity);
        if (error) return error;

        if (zebra_limit_exceeded (state)) {
            zebra_write_dropped_entity (state, entity);
            break;
        }

        read_all = itrue;
        for (int64_t i = 0; i != state->attribute_count; ++i) {
            if (state->entity_fact_offset[i] != state->entity_fact_count[i]) {
                read_all = ifalse;
                break;
            }
        }
    }

    for (int64_t i = 0; i != state->attribute_count; ++i) {
        state->entity_fact_count [i] = 0;
        state->entity_fact_offset[i] = 0;
    }

    return 0;
}

/*
 * Output
 */

int64_t zebra_output_named_column
    ( void **src
    , zebra_named_columns_t *dst )
{
    int64_t offset = 0;

    for (int64_t i = 0; i < dst->count; ++i) {
        offset += zebra_output_column (src, &dst->columns[i]);
    }

    return offset;
}

int64_t zebra_output_column_nested
    ( void **src
    , int64_t *dst_indices
    , int64_t *dst_table )
{
}

int64_t zebra_output_column
    ( void **src
    , zebra_column_t *empty )
{
    const zebra_column_tag_t tag = empty->tag;
    zebra_column_variant_t dst = empty->of;

    switch (tag) {
        case ZEBRA_COLUMN_UNIT: {
            return 1;
        }

        case ZEBRA_COLUMN_INT: {
            iint_t a = *((iint_t*)src);
            *dst._int.values = a;
            return 1;
        }

        case ZEBRA_COLUMN_DOUBLE: {
            idouble_t a = *((idouble_t*)src);
            *dst._double.values = a;
            return 1;
        }

        case ZEBRA_COLUMN_ENUM: {
            int64_t enum_tag = *((int64_t*)src);

            switch (enum_tag) {
                case 0: {
                    *dst._enum.tags = 0;
                    return 2;
                }
                case 1: {
                    *dst._enum.tags = 1;
                    int64_t offset = 1;
                    offset += zebra_output_named_column (src, &dst._enum.columns);
                    return offset;
                }
                fprintf (stderr, "Fatal error: encountered enum type with more than two variants. Icicle should never produce such a type.\n");
                exit (1);
            }
        }

        case ZEBRA_COLUMN_STRUCT: {
            return zebra_output_named_column (src, &dst._enum.columns);
        }

        case ZEBRA_COLUMN_NESTED: {
            return zebra_output_column_nested (src, dst._nested.indices, &dst._nested.table);
            return 0;
        }

        case ZEBRA_COLUMN_REVERSED: {
            fprintf (stderr, "Fatal error: encountered a reversed Zebra column. Icicle should never produce this.\n");
            exit (1);
        }
    }
}

static int64_t zebra_output_table
    ( void **src
    , zebra_table_t *empty )
{
    const zebra_table_tag_t tag = empty->tag;
    zebra_table_variant_t data = empty->of;

    switch (tag) {
        case ZEBRA_TABLE_BINARY: {
            const zebra_binary_encoding_t encoding = data._binary.encoding;

            switch (encoding) {
                case ZEBRA_BINARY_NONE: {
                    fprintf (stderr, "Fatal error: encountered raw bytes encoding. Icicle should never output this.\n");
                    exit (1);
                }
                case ZEBRA_BINARY_UTF8: {
                    istring_t str = *((istring_t*)src);
                    data._binary.bytes = str;
                    return 1;
                }
            }
        }

        case ZEBRA_TABLE_ARRAY: {
            return zebra_output_column (src, empty);
        }
    }
}

ierror_msg_t zebra_snapshot_step_output_zebra
    ( piano_t *piano
    , zebra_state_t *state
    , zebra_entity_t *entity
    , zebra_table_t *outputs )
{
    ierror_msg_t  error;
    ierror_loc_t  error_loc;

    uint8_t *entity_id     = entity->id_bytes;
    int64_t entity_id_size = entity->id_length;

    int       fd              = state->fd_dropped_entities;
    int64_t   attribute_count = state->attribute_count;

    state->entity_count++;

    error = zebra_read_step (piano, state, entity);
    if (error) return error;

    if (!zebra_limit_exceeded (state)) {
        zebra_output_fleet (state, &outputs);
    }

    return 0;
}

ierror_msg_t zebra_snapshot_step_output_psv
    ( piano_t *piano
    , zebra_state_t *state
    , zebra_entity_t *entity )
{
    ierror_msg_t  error;
    ierror_loc_t  error_loc;

    uint8_t *entity_id     = entity->id_bytes;
    int64_t entity_id_size = entity->id_length;

    ifleet_t *fleet = state->fleet;
    int       fd    = state->fd_output;

    state->entity_count++;

    error = zebra_read_step (piano, state, entity);
    if (error) return error;

    if (!zebra_limit_exceeded (state)) {
        error = psv_write_output
            ( fd
            , state->psv_output_start
            , state->psv_output_end
            , &state->psv_output_ptr
            , (char*) entity_id
            , entity_id_size
            , state->fleet );
        if (error) return error;

        error = psv_flush_output
            ( fd
            , state->psv_output_start
            , &state->psv_output_ptr );
        if (error) return error;
    }

    return 0;
}

anemone_mempool_t *zebra_get_mempool (zebra_state_t *state)
{
    return fleet_get_mempool (state->fleet); /* roundabout dereference because fleet is generated */
}

#endif
#endif
