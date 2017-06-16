#include "53-zebra-data.h"

#if !ICICLE_NO_INPUT
#if ICICLE_ZEBRA

typedef struct {
    /* inputs */
    const char *input_path;
    iint_t output_fd;
    iint_t chord_fd;
    iint_t drop_fd;

    /* outputs */
    const char  *error;
    iint_t fact_count;
    iint_t entity_count;

    /* stuff */
    const size_t output_buffer_size;
    const iint_t max_map_size;

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
    int drop_fd;

    /* read state */
    int64_t  attribute_count;

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
    , const zebra_table_t *src
    , iarray_iany_t *dst );

static int64_t zebra_compute_offset_column(const zebra_column_t *src);
static int64_t zebra_compute_offset_table(const zebra_table_t *src);


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
        fprintf (stderr, "Fatal error: Icicle dictionary does not match Zebra schema.\n");
        fprintf (stderr, "Fatal error: attribute index (%" PRId64 ") is out of bounds for attribute count (%" PRId64 ")\n", attribute_ix, attribute_count);
        exit(1);
    }

    zebra_attribute_t *attribute = &src->attributes[attribute_ix];
    zebra_table_t *table = &attribute->table;
    int64_t row_count = table->row_count;

    int64_t compute_offset   = zebra_compute_offset_table (table);

    if (compute_offset != columns_to_fill) {
        fprintf (stderr, "Fatal error: Icicle dictionary does not match Zebra schema.\n");
        fprintf (stderr, "Total column count for Zebra: %" PRId64 "\nTotal column count for Icicle: %"PRId64"\n", compute_offset, columns_to_fill);
        exit(1);
    }

    /* temporary fix until icicle uses the same time as zebra */
    itime_t zebra_chord_time = itime_to_epoch_seconds (chord_time);

    /* the number of facts before chord time for this entity */
    int64_t fact_count = 0;
    for (int64_t i = 0; i != row_count; ++i) {
        itime_t time = attribute->times[i];
        if (zebra_chord_time <= time) {
            break;
        }
        fact_count++;
    }

    /* temporary fix until icicle uses the same time as zebra */
    itime_t *sadface_times = anemone_mempool_alloc (mempool, fact_count * sizeof (itime_t));

    for (int64_t i = 0; i != fact_count; ++i) {
        sadface_times[i] = itime_from_epoch_seconds (attribute->times[i]);
    }

    *new_fact_count = fact_count;
    *new_tombstone  = (ierror_t*) attribute->tombstones;
    *new_fact_time  = sadface_times;

    iarray_iany_t *dst_arrays = anemone_mempool_alloc (mempool, columns_to_fill * sizeof(iarray_iany_t));

    int64_t translate_offset = zebra_translate_table (mempool, 0, fact_count, table, dst_arrays);
    for (int i = 0; i != columns_to_fill; ++i) {
        dst[i] = ARRAY_PAYLOAD(iany, dst_arrays[i]);
    }

    if (compute_offset != translate_offset) {
        fprintf (stderr, "Fatal internal error: computed offset does not match translated offset.\n");
        fprintf (stderr, "Expected offset: %" PRId64 "\nTranslate offset: %"PRId64"\n", compute_offset, translate_offset);
        exit(1);
    }

    return 0;
}

static iarray_iany_t zebra_iarray_copy
    ( anemone_mempool_t *mempool
    , int64_t elem_start
    , int64_t elem_count
    , iany_t *src)
{
    iarray_iany_t arr = iarray_iany_create (mempool, elem_count);
    memcpy (ARRAY_PAYLOAD (iany, arr), src + elem_start, elem_count * sizeof(iany_t));
    return arr;
}

/* Translate Zebra enum tags to Icicle bool flags, e.g. is_some, is_right, etc.
   We only allow at most two variants since Icicle's runtime representation
   of enums is unsuitable for more.
 */
static int64_t zebra_translate_column_tags
    ( anemone_mempool_t *mempool
    , const int64_t variant_count
    , int64_t elem_start
    , int64_t elem_count
    , int64_t *src
    , iarray_iany_t *dst )
{
    if (variant_count > 2) {
        fprintf (stderr, "Fatal error: found an enum with %" PRId64 " variants. Icicle does not support Zebra enums with more than two variants.\n", variant_count);
        exit(1);
    }

    *dst = zebra_iarray_copy (mempool, elem_start, elem_count, (iany_t*)src);
    return 1;
}

static int64_t zebra_compute_offset_table(const zebra_table_t *src)
{
    const zebra_table_tag_t tag = src->tag;
    const zebra_table_variant_t data = src->of;

    switch (tag) {
        case ZEBRA_TABLE_BINARY: {
            return 1;
        }

        case ZEBRA_TABLE_ARRAY: {
            return zebra_compute_offset_column
                ( data._array.values );
        }

        case ZEBRA_TABLE_MAP: {
            int64_t offset = 0;
            offset += zebra_compute_offset_column
                ( data._map.keys );
            offset += zebra_compute_offset_column
                ( data._map.values );
            return offset;
        }
    }
    fprintf (stderr, "Fatal error: unknown Zebra table tag: %u\n", tag);
    exit (1);
}

static int64_t zebra_compute_offset_column(const zebra_column_t *src)
{
    const zebra_column_tag_t tag = src->tag;
    const zebra_column_variant_t data = src->of;

    switch (tag) {
        case ZEBRA_COLUMN_UNIT: {
            return 0;
        }

        case ZEBRA_COLUMN_INT: {
            return 1;
        }

        case ZEBRA_COLUMN_DOUBLE: {
            return 1;
        }

        case ZEBRA_COLUMN_ENUM: {
            int64_t column_count = data._enum.columns.count;
            int64_t offset = 1; // tags
            for (int64_t i = 0; i != column_count; ++i) {
                offset += zebra_compute_offset_column
                    ( data._enum.columns.columns + i );
            }
            return offset;
        }

        case ZEBRA_COLUMN_STRUCT: {
            int64_t offset = 0;
            int64_t column_count = data._struct.columns.count;
            for (int64_t i = 0; i != column_count; ++i) {
                offset += zebra_compute_offset_column
                    ( data._struct.columns.columns + i );
            }
            return offset;
        }

        case ZEBRA_COLUMN_NESTED: {
            return zebra_compute_offset_table
                ( &data._nested.table );
        }

        case ZEBRA_COLUMN_REVERSED: {
            return zebra_compute_offset_column
                ( data._reversed.column );
        }

    }

    fprintf (stderr, "Fatal error: unknown Zebra column tag: %u\n", tag);
    exit (1);
}


static int64_t zebra_translate_column_nested
    ( anemone_mempool_t *mempool
    , int64_t elem_start
    , int64_t elem_count
    , const int64_t *indices
    , const zebra_table_t *src
    , iarray_iany_t *dst )
{
    switch (src->tag) {
        case ZEBRA_TABLE_BINARY: {
            iarray_iany_t strings = iarray_iany_create (mempool, elem_count);
            const int64_t index_0 = indices[0];

            for (int i = 0; i != elem_count; ++i) {
                const int64_t elem_index     = elem_start + i;
                const int64_t index_i        = indices[elem_index];
                const int64_t string_start   = index_i - index_0;
                const int64_t string_length  = indices[elem_index + 1] - index_i;

                char *bytes = anemone_mempool_alloc (mempool, string_length + 1);
                memcpy(bytes, src->of._binary.bytes + string_start, string_length);
                bytes[string_length] = '\0';

                ARRAY_PAYLOAD(iany, strings)[i] = bytes;
            }

            *dst = strings;
            return 1;
        }

        case ZEBRA_TABLE_ARRAY:
        case ZEBRA_TABLE_MAP: {
            int64_t offset = zebra_compute_offset_table ( src );

            for (int i = 0; i != offset; ++i) {
                dst[i] = iarray_iany_create (mempool, elem_count);
            }

            iarray_iany_t *tmp_into = anemone_mempool_alloc (mempool, offset * sizeof(iarray_iany_t));
            const int64_t index_0 = indices[0];

            for (int nested_ix = 0; nested_ix != elem_count; ++nested_ix) {
                const int64_t elem_index     = elem_start + nested_ix;
                const int64_t index_i        = indices[elem_index];
                const int64_t array_start    = index_i - index_0;
                const int64_t array_length   = indices[elem_index + 1] - index_i;

                int64_t translate_offset = zebra_translate_table (mempool, array_start, array_length, src, tmp_into);
                IASSERT (offset == translate_offset);

                for (int offset_ix = 0; offset_ix != offset; ++offset_ix) {
                    ARRAY_PAYLOAD(iany, dst[offset_ix])[nested_ix] = tmp_into[offset_ix];
                }
            }

            return offset;
        }
    }
    fprintf (stderr, "Fatal error: unknown Zebra table tag: %u\n", src->tag);
    exit (1);

}

static int64_t zebra_translate_column
    ( anemone_mempool_t *mempool
    , int64_t elem_start
    , int64_t elem_count
    , const zebra_column_t *src
    , iarray_iany_t *dst )
{
    const zebra_column_tag_t tag = src->tag;
    const zebra_column_variant_t data = src->of;

    switch (tag) {
        case ZEBRA_COLUMN_UNIT: {
            return 0;
        }

        case ZEBRA_COLUMN_INT: {
            // FIXME We should support decoding dates/times if the encoding is set.
            // FIXME Hopefully we remove all this code before that happens!
            *dst = zebra_iarray_copy (mempool, elem_start, elem_count, (iany_t*)data._int.values);
            return 1;
        }

        case ZEBRA_COLUMN_DOUBLE: {
            *dst = zebra_iarray_copy (mempool, elem_start, elem_count, (iany_t*)data._double.values);
            return 1;
        }

        case ZEBRA_COLUMN_ENUM: {
            int64_t column_count = data._enum.columns.count;
            int64_t offset = zebra_translate_column_tags
                ( mempool
                , column_count
                , elem_start
                , elem_count
                , data._enum.tags
                , dst );
            for (int64_t i = 0; i != column_count; ++i) {
                offset += zebra_translate_column
                    ( mempool
                    , elem_start
                    , elem_count
                    , data._enum.columns.columns + i
                    , dst + offset );
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
                    , data._struct.columns.columns + i
                    , dst + offset );
            }
            return offset;
        }

        case ZEBRA_COLUMN_NESTED: {
            return zebra_translate_column_nested
                ( mempool
                , elem_start
                , elem_count
                , data._nested.indices
                , &data._nested.table
                , dst );
        }

        case ZEBRA_COLUMN_REVERSED: {
            return zebra_translate_column
                ( mempool
                , elem_start
                , elem_count
                , data._reversed.column
                , dst );
        }

    }

    fprintf (stderr, "Fatal error: unknown Zebra column tag: %u\n", tag);
    exit (1);
}

static int64_t zebra_translate_table
    ( anemone_mempool_t *mempool
    , int64_t elem_start
    , int64_t elem_count
    , const zebra_table_t *src
    , iarray_iany_t *dst )
{
    const int64_t row_count = src->row_count;
    const zebra_table_tag_t tag = src->tag;
    const zebra_table_variant_t data = src->of;

    IASSERT (elem_start + elem_count <= row_count);

    switch (tag) {
        case ZEBRA_TABLE_BINARY: {
            fprintf (stderr, "Fatal error: ZEBRA_TABLE_BINARY disallowed at top-level\n");
            exit (1);
        }

        case ZEBRA_TABLE_ARRAY: {
            return zebra_translate_column
                ( mempool
                , elem_start
                , elem_count
                , data._array.values
                , dst );
        }

        case ZEBRA_TABLE_MAP: {
            int64_t offset = 0;
            offset += zebra_translate_column
                ( mempool
                , elem_start
                , elem_count
                , data._map.keys
                , dst );
            offset += zebra_translate_column
                ( mempool
                , elem_start
                , elem_count
                , data._map.values
                , dst + offset );
            return offset;
        }
    }
    fprintf (stderr, "Fatal error: unknown Zebra table tag: %u\n", tag);
    exit (1);
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

    ifleet_t *fleet = psv_alloc_fleet (max_chord_count, 0, cfg->max_map_size);

    state->fleet = fleet;

    state->output_fd = output_fd;
    state->drop_fd   = drop_fd;

    state->entity_count = cfg->entity_count;
    state->fact_count = cfg->fact_count;

    int64_t attribute_count = zebra_attribute_count ();
    state->attribute_count = attribute_count;

    char *output_ptr = calloc (cfg->output_buffer_size + 1, 1);
    state->output_start = output_ptr;
    state->output_end   = output_ptr + cfg->output_buffer_size - 1;
    state->output_ptr   = output_ptr;

    return state;
}

void zebra_collect_state (zebra_config_t *cfg, zebra_state_t *state)
{
    cfg->entity_count = state->entity_count;
    cfg->fact_count = state->fact_count;

    //
    // FIXME currently there is no way to free state->fleet
    //

    free (state);
}

/* A read and compute step */
ierror_msg_t zebra_read_step (piano_t *piano, zebra_state_t *state, zebra_entity_t *entity)
{
     int64_t attribute_count = zebra_attribute_count ();
     if (attribute_count != entity->attribute_count) {
        fprintf (stderr, "Fatal error: Icicle dictionary does not match Zebra schema.\n");
        fprintf (stderr, "Total attribute count for Zebra: %" PRId64 "\n", entity->attribute_count);
        fprintf (stderr, "Total attribute count for Icicle: %" PRId64 "\n", attribute_count);
        exit(1);
     }

    ifleet_t *fleet = state->fleet;
    psv_collect_fleet (fleet);

    ierror_loc_t  error_loc = psv_configure_fleet ((char*) entity->id_bytes, entity->id_length, piano, state->fleet);
    if (error_loc) return ierror_loc_pretty (error_loc, 0);

    ierror_msg_t error = zebra_read_entity (piano, state, entity);
    if (error) return error;

    return 0;
}

ierror_msg_t zebra_snapshot_step
    ( piano_t *piano
    , zebra_state_t *state
    , zebra_entity_t *entity )
{
    ierror_msg_t  error;
    ierror_loc_t  error_loc;

    uint8_t *entity_id     = entity->id_bytes;
    int64_t entity_id_size = entity->id_length;

    ifleet_t *fleet = state->fleet;

    state->entity_count++;

    error = zebra_read_step (piano, state, entity);
    if (error) return error;

    int fd = state->output_fd;

    /* launch the missiles */
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
