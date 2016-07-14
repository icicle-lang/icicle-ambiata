#include "06-segv.h"

const size_t iblock_size = 1 * 1024 * 1024;

typedef struct iblock {
    void                * const ptr;
    const struct iblock * const prev;
} iblock_t;

typedef struct {
    iblock_t *last;
    void     *current_ptr;
    void     *maximum_ptr;
} imempool_t;

static iblock_t * iblock_create (iblock_t *prev)
{
    void     *ptr = malloc (iblock_size);
    iblock_t  src = { ptr, prev };

    iblock_t *dst = malloc (sizeof (iblock_t));
    memcpy (dst, &src, sizeof (iblock_t));

#if ICICLE_DEBUG
    fprintf (stderr, "iblock_create: %p\n", dst->ptr);
#endif

    return dst;
}

static void iblock_free (const iblock_t *block)
{
    if (block == 0) return;

#if ICICLE_DEBUG
    fprintf (stderr, "iblock_free: %p\n", block->ptr);
#endif

    free (block->ptr);
    const iblock_t *prev = block->prev;
    free (block);
    iblock_free (prev);
}

static void imempool_add_block (imempool_t *pool)
{
    iblock_t *next = iblock_create (pool->last);

    pool->last        = next;
    pool->current_ptr = next->ptr;
    pool->maximum_ptr = next->ptr + iblock_size;
}

/* This has to be a macro as when it's a function we end up with an extra
 * conditional because we need to return something to indicate whether we
 * succeeded or not. */
#define TRY_ALLOC(fn_name)                                                                             \
    void *ptr  = pool->current_ptr;                                                                    \
    void *next = ptr + num_bytes;                                                                      \
                                                                                                       \
    if (next <= pool->maximum_ptr) {                                                                   \
        ICICLE_WHEN_DEBUG (fprintf (stderr, #fn_name ": %p (allocated %zu bytes)\n", ptr, num_bytes)); \
        pool->current_ptr = next;                                                                      \
        return ptr;                                                                                    \
    }

static void * NOINLINE imempool_alloc_block (imempool_t *pool, size_t num_bytes)
{
    imempool_add_block (pool);

    TRY_ALLOC (imempool_alloc_block);

    /* Couldn't allocate even after adding a new block to the pool, we will
     * never be able to service this request. */
    fprintf (stderr, "oh my, someone's a bit greedy\n");
    fprintf (stderr, "did you really need %zu bytes\n", num_bytes);
    exit (1);
}

static void * INLINE imempool_alloc (imempool_t *pool, size_t num_bytes)
{
    TRY_ALLOC (imempool_alloc);

    return imempool_alloc_block (pool, num_bytes);
}

/* Non-static functions cannot be inlined because of an old version of gcc
 * However we also need to be able to call this from Haskell */
void * imempool_alloc_extern (imempool_t *pool, size_t num_bytes)
{
    return imempool_alloc(pool, num_bytes);
}

imempool_t * imempool_create ()
{
    imempool_t *pool = calloc(1, sizeof(imempool_t));

#if ICICLE_DEBUG
    fprintf (stderr, "imempool_create: %p\n", pool);
#endif

    imempool_add_block (pool);

    return pool;
}

void imempool_debug_block_usage (imempool_t *pool)
{
    iblock_t *block = pool->last;
    uint64_t size = 0;
    uint64_t blocks = 0;
    while (block != 0) {
      size += iblock_size;
      blocks++;
      block = block->prev;
    }
    fprintf (stderr, "imempool_debug_block_usage: %llu blocks, %llu bytes\n", blocks, size);
}

void imempool_free (imempool_t *pool)
{
#if ICICLE_DEBUG
    fprintf (stderr, "imempool_free: %p\n", pool);
    imempool_debug_block_usage (pool);
#endif

    iblock_free (pool->last);
    free(pool);
}
