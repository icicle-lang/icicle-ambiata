#include "50-chord.h"

typedef struct piano piano_t;

int64_t piano_max_count (
    piano_t *piano
  );

error_t piano_lookup (
    piano_t *piano
  , const uint8_t *needle_id
  , size_t needle_id_size
  , int64_t *out_count
  , const int64_t **out_times
  );

