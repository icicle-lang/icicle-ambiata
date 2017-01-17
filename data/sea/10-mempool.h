#include "06-segv.h"
#include "../../lib/anemone/csrc/anemone_mempool.h"

#define TRY_ALLOC(fn_name) ANEMONE_MEMPOOL_TRY_ALLOC(fn_name)

void anemone_mempool_debug_block_usage (anemone_mempool_t *pool)
{
    const anemone_block_t *block = pool->last;
    uint64_t size = 0;
    uint64_t blocks = 0;
    while (block != 0) {
      size += anemone_block_size;
      blocks++;
      block = block->prev;
    }
    fprintf (stderr, "anemone_mempool_debug_block_usage: %" PRIu64 " blocks, %" PRIu64 " bytes\n", blocks, size);
}
