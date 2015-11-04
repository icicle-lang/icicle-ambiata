#include "00-includes.h"

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

// If the size of imempool_t changes, be sure to update the `seaEval` and
// `stateWordsOfProgram` functions.
ASSERT_SIZE (imempool_t, 3)

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
    iblock_free (block->prev);
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
    exit (1);
}

void * INLINE imempool_alloc (imempool_t *pool, size_t num_bytes)
{
    TRY_ALLOC (imempool_alloc);

    return imempool_alloc_block (pool, num_bytes);
}

void imempool_create (imempool_t *pool)
{
    pool->last        = 0;
    pool->current_ptr = 0;
    pool->maximum_ptr = 0;

    imempool_add_block (pool);
}

void imempool_free (imempool_t *pool)
{
    iblock_free (pool->last);
}
