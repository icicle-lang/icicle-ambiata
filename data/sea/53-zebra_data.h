#include "52-psv.h"

#if !ICICLE_NO_INPUT
#if ICICLE_ZEBRA

typedef int64_t bool64_t;

typedef enum zebra_type {
    ZEBRA_BYTE,
    ZEBRA_INT,
    ZEBRA_DOUBLE,
    ZEBRA_ARRAY,
} zebra_type_t;

struct zebra_column;
typedef struct zebra_column zebra_column_t;

typedef struct zebra_table {
    int64_t row_count;
    int64_t row_capacity;
    int64_t column_count;
    zebra_column_t *columns;
} zebra_table_t;

typedef union zebra_data {
    uint8_t *b;
    int64_t *i;
    double *d;
    struct {
        int64_t *n;
        zebra_table_t table;
    } a;
} zebra_data_t;

struct zebra_column {
    zebra_type_t type;
    zebra_data_t data;
}; // zebra_column_t

typedef struct zebra_attribute {
    int64_t *times;
    int64_t *priorities;
    bool64_t *tombstones;
    zebra_table_t table;
} zebra_attribute_t;

typedef struct zebra_entity {
    uint32_t hash;
    int64_t id_length;
    uint8_t *id_bytes;

    int64_t attribute_count;
    zebra_attribute_t *attributes;
} zebra_entity_t;

#endif
#endif
