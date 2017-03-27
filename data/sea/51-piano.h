#include "42-text-conversion.h"

typedef struct piano piano_t;

int64_t piano_max_count (
    piano_t *piano
  );

error_t piano_lookup (
    piano_t *piano
  , const uint8_t *needle_id
  , size_t needle_id_size
  , int64_t *out_count
  , const int64_t **out_label_times
  , const int64_t **out_label_name_offsets
  , const int64_t **out_label_name_lengths
  , const uint8_t **out_label_name_data
  );
