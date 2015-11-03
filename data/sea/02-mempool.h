#include "01-includes.h"

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
    iblock_free (block->prev);
}

static void imempool_add_block (imempool_t *pool)
{
    iblock_t *next = iblock_create (pool->last);

    pool->last        = next;
    pool->current_ptr = next->ptr;
    pool->maximum_ptr = next->ptr + iblock_size;
}

static void * INLINE imempool_try_alloc (imempool_t *pool, size_t num_bytes)
{
    void *ptr  = pool->current_ptr;
    void *next = ptr + num_bytes;

    if (next <= pool->maximum_ptr) {
#if ICICLE_DEBUG
        fprintf (stderr, "imempool_try_alloc: %p (allocated %zu bytes)\n", ptr, num_bytes);
#endif
        pool->current_ptr = next;
        return ptr;
    }

    return 0;
}

static void * NOINLINE imempool_alloc_block (imempool_t *pool, size_t num_bytes)
{
    imempool_add_block (pool);

    void *ptr = imempool_try_alloc (pool, num_bytes);

    if (ptr == 0) {
        /* Couldn't allocate even after adding a new block to the pool, we will
         * never be able to service this request. */
        fprintf (stderr, "oh my, someone's a bit greedy\n");
        exit (1);
    }

    return ptr;
}

void * INLINE imempool_alloc (imempool_t *pool, size_t num_bytes)
{
    void *ptr = imempool_try_alloc (pool, num_bytes);

    if (ptr == 0) {
        return imempool_alloc_block (pool, num_bytes);
    }

    return ptr;
}

imempool_t imempool_create ()
{
    imempool_t pool = { 0, 0, 0 };
    imempool_add_block (&pool);
    return pool;
}

void imempool_free (imempool_t pool)
{
    iblock_free (pool.last);
}
