#include "21-time.h"

static iint_t INLINE iarray_size(iint_t count)
{
    if (count < 4) return 4;

    iint_t bits = 64 - __builtin_clzll (count - 1);
    iint_t next = 1L << bits;

    return next;
}

typedef struct
{
    iint_t count;
} iarray_struct;
// payload goes straight after

#define ARRAY(t)         iarray_ ## t
#define ARRAY_FUN(t,fun) CONCAT(ARRAY(t), _ ## fun)
#define ARRAY_T(t)       CONCAT(ARRAY(t), _t)

// I'm not certain there's a point having a different one for each type.
// It makes it look a little better, but I don't think it's any safer.
#define MK_ARRAY_STRUCT(t) typedef iarray_struct* ARRAY_T(t);

// get payload by advancing pointer by size of struct
// (which should be equivalent to straight after struct fields)
// then casting to t*
#define ARRAY_PAYLOAD(t,p) ((t##_t *)(p+1))

#define ARRAY_ITEM_SIZE(t,n)  (sizeof(t##_t) * (n))
#define ARRAY_SIZE(t,n)       (ARRAY_ITEM_SIZE(t,n) + sizeof(iarray_struct))

#ifndef ICICLE_ASSERT_MAXIMUM_ARRAY_COUNT
#define ICICLE_ASSERT_MAXIMUM_ARRAY_COUNT 1000000000
#endif

#define VALID_ARRAY(x)                                                          \
    IASSERT(x->count <= ICICLE_ASSERT_MAXIMUM_ARRAY_COUNT);                     \
    IASSERT(x->count >= 0);                                                     \

#define VALID_INDEX(ix, x)                                                      \
    IASSERT(ix < x->count);                                                     \
    IASSERT(ix >= 0);                                                           \



/*
Length(arr)
*/

#define MK_ARRAY_LENGTH(t)                                                      \
                                                                                \
static iint_t INLINE ARRAY_FUN(t,length) (ARRAY_T(t) arr)                       \
{                                                                               \
    VALID_ARRAY(arr)                                                            \
    return arr->count;                                                          \
}


/*
Eq(arr_x, arr_y)
Ne(arr_x, arr_y)
*/

#define MK_ARRAY_EQ(t)                                                          \
                                                                                \
static ibool_t INLINE ARRAY_FUN(t,eq) (ARRAY_T(t) x, ARRAY_T(t) y)              \
{                                                                               \
    VALID_ARRAY(x)                                                              \
    VALID_ARRAY(y)                                                              \
    if (x->count != y->count) return ifalse;                                    \
                                                                                \
    for (iint_t ix = 0; ix != x->count; ++ix) {                                 \
        if (!t##_eq(ARRAY_PAYLOAD(t,x)[ix], ARRAY_PAYLOAD(t,y)[ix]))            \
            return ifalse;                                                      \
    }                                                                           \
                                                                                \
    return itrue;                                                               \
}                                                                               \
                                                                                \
static ibool_t INLINE ARRAY_FUN(t,ne) (ARRAY_T(t) x, ARRAY_T(t) y)              \
{                                                                               \
    return !ARRAY_FUN(t,eq)(x, y);                                              \
}


/*
Lt(arr_x, arr_y)
Le(arr_x, arr_y)
Ge(arr_x, arr_y)
Gt(arr_x, arr_y)
*/

#define MK_ARRAY_CMP(t,op)                                                      \
                                                                                \
static ibool_t INLINE ARRAY_FUN(t,op) (ARRAY_T(t) x, ARRAY_T(t) y)              \
{                                                                               \
    VALID_ARRAY(x)                                                              \
    VALID_ARRAY(y)                                                              \
    iint_t x_count = x->count;                                                  \
    iint_t y_count = y->count;                                                  \
    iint_t min     = x_count < y_count ? x_count : y_count;                     \
                                                                                \
    for (iint_t ix = 0; ix != min; ++ix) {                                      \
        if (!t##_##op(ARRAY_PAYLOAD(t,x)[ix], ARRAY_PAYLOAD(t,y)[ix]))          \
            return ifalse;                                                      \
    }                                                                           \
                                                                                \
    return iint_##op(x_count, y_count);                                         \
}


#define MK_ARRAY_CMPS(t)                                                        \
    MK_ARRAY_EQ(t)                                                              \
    MK_ARRAY_CMP(t,lt)                                                          \
    MK_ARRAY_CMP(t,le)                                                          \
    MK_ARRAY_CMP(t,gt)                                                          \
    MK_ARRAY_CMP(t,ge)                        

/*
Index(arr, ix)
*/

#define MK_ARRAY_INDEX(t)                                                       \
                                                                                \
static t##_t INLINE ARRAY_FUN(t,index) (ARRAY_T(t) x, iint_t ix)                \
{                                                                               \
    VALID_INDEX(ix, x)                                                          \
    VALID_ARRAY(x)                                                              \
    return ARRAY_PAYLOAD(t,x)[ix];                                              \
}


/*
Create(size)
*/

#define MK_ARRAY_CREATE(t)                                                      \
                                                                                \
static ARRAY_T(t) INLINE ARRAY_FUN(t,create)(anemone_mempool_t *pool, iint_t sz)\
{                                                                               \
    iint_t sz_alloc = iarray_size(sz);                                          \
    size_t bytes    = ARRAY_SIZE(t,sz_alloc);                                   \
                                                                                \
    ARRAY_T(t) ret  = (ARRAY_T(t))anemone_mempool_calloc(pool, bytes, 1);       \
    ret->count      = sz;                                                       \
                                                                                \
    VALID_ARRAY(ret)                                                            \
    return ret;                                                                 \
}


/*
Copy(arr)
*/

#define MK_ARRAY_COPY(t)                                                        \
                                                                                \
static ARRAY_T(t) INLINE ARRAY_FUN(t,copy) (anemone_mempool_t *into, ARRAY_T(t) x)\
{                                                                               \
    VALID_ARRAY(x)                                                              \
    if (x == 0) return 0;                                                       \
                                                                                \
    ARRAY_T(t) arr = ARRAY_FUN(t,create)(into, x->count);                       \
                                                                                \
    for (iint_t ix = 0; ix != x->count; ++ix) {                                 \
        t##_t cp = ARRAY_PAYLOAD(t,x)[ix];                                      \
        ARRAY_PAYLOAD(t,arr)[ix] = t##_copy(into, cp);                          \
    }                                                                           \
                                                                                \
    VALID_ARRAY(arr)                                                            \
    return arr;                                                                 \
}


/*
Put(arr, ix, val)
*/

#define MK_ARRAY_PUT_MUTABLE(t)                                                 \
                                                                                \
static ARRAY_T(t) INLINE ARRAY_FUN(t,put_mutable)                               \
                  (anemone_mempool_t *pool, ARRAY_T(t) x, iint_t ix, t##_t v)   \
{                                                                               \
    VALID_ARRAY(x)                                                              \
    iint_t count  = x->count;                                                   \
    iint_t sz_old = iarray_size(count);                                         \
                                                                                \
    IASSERT(ix < count + 1);                                                    \
                                                                                \
    if (ix >= sz_old) {                                                         \
        iint_t sz_new    = iarray_size(ix+1);                                   \
        size_t bytes_new = ARRAY_SIZE(t, sz_new);                               \
        size_t bytes_old = ARRAY_SIZE(t, sz_old);                               \
                                                                                \
        ARRAY_T(t) arr = (ARRAY_T(t))anemone_mempool_alloc(pool, bytes_new);    \
        memcpy(arr, x, bytes_old);                                              \
        x = arr;                                                                \
                                                                                \
        x->count = ix + 1;                                                      \
    } else if (ix >= count) {                                                   \
        x->count = ix + 1;                                                      \
    }                                                                           \
                                                                                \
    ARRAY_PAYLOAD(t,x)[ix] = v;                                                 \
    VALID_ARRAY(x)                                                              \
    VALID_INDEX(ix, x)                                                          \
    return x;                                                                   \
}


/*
Immutable_put(arr, ix, val)
*/

#define MK_ARRAY_PUT_IMMUTABLE(t)                                               \
                                                                                \
static ARRAY_T(t) INLINE ARRAY_FUN(t,put_immutable)                             \
                  (anemone_mempool_t *pool, ARRAY_T(t) x, iint_t ix, t##_t v)   \
{                                                                               \
    ARRAY_T(t) arr = ARRAY_FUN(t,copy)(pool, x);                                \
    arr = ARRAY_FUN(t,put_mutable)(pool, arr, ix, v);                           \
    return arr;                                                                 \
}


/*
Swap(arr, ix1, ix2)
*/

#define MK_ARRAY_SWAP(t)                                                        \
                                                                                \
static ARRAY_T(t) INLINE ARRAY_FUN(t,swap)                                      \
                  (ARRAY_T(t) x, iint_t ix1, iint_t ix2)                        \
{                                                                               \
    VALID_ARRAY(x)                                                              \
    VALID_INDEX(ix1, x)                                                         \
    VALID_INDEX(ix2, x)                                                         \
                                                                                \
    t##_t tmp = ARRAY_PAYLOAD(t,x)[ix1];                                        \
    ARRAY_PAYLOAD(t,x)[ix1] = ARRAY_PAYLOAD(t,x)[ix2];                          \
    ARRAY_PAYLOAD(t,x)[ix2] = tmp;                                              \
    return x;                                                                   \
}


/*
Immutable Delete (arr, ix)
*/

#define MK_ARRAY_DELETE(t)                                                      \
                                                                                \
static ARRAY_T(t) INLINE ARRAY_FUN(t,delete)                                    \
                  (anemone_mempool_t *pool, ARRAY_T(t) x, iint_t ix_delete)     \
{                                                                               \
    VALID_ARRAY(x)                                                              \
    VALID_INDEX(ix_delete, x)                                                   \
                                                                                \
    iint_t count      = x->count;                                               \
    iint_t capacity   = iarray_size(count - 1);                                 \
    size_t bytes      = ARRAY_SIZE(t, capacity);                                \
                                                                                \
    ARRAY_T(t) arr = (ARRAY_T(t))anemone_mempool_alloc(pool, bytes);            \
                                                                                \
    for (iint_t ix = 0; ix != ix_delete; ++ix) {                                \
        t##_t val = ARRAY_PAYLOAD(t,x)[ix];                                     \
        ARRAY_PAYLOAD(t,arr)[ix] = val;                                         \
    }                                                                           \
    for (iint_t ix = ix_delete + 1; ix != count; ++ix) {                        \
        t##_t val = ARRAY_PAYLOAD(t,x)[ix];                                     \
        ARRAY_PAYLOAD(t,arr)[ix - 1] = val;                                     \
    }                                                                           \
                                                                                \
    arr->count = count - 1;                                                     \
                                                                                \
    VALID_ARRAY(arr)                                                            \
    return arr;                                                                 \
}


/*
Define an array
*/

#define MAKE_ARRAY(t)                                                           \
    MK_ARRAY_STRUCT         (t)                                                 \
    MK_ARRAY_LENGTH         (t)                                                 \
    MK_ARRAY_CMPS           (t)                                                 \
    MK_ARRAY_INDEX          (t)                                                 \
    MK_ARRAY_CREATE         (t)                                                 \
    MK_ARRAY_COPY           (t)                                                 \
    MK_ARRAY_PUT_MUTABLE    (t)                                                 \
    MK_ARRAY_PUT_IMMUTABLE  (t)                                                 \
    MK_ARRAY_SWAP           (t)                                                 \
    MK_ARRAY_DELETE         (t)                                                 \


// enable if you need to resolve compiler errors in the macros above
#if 0
MAKE_ARRAY(idouble)
MAKE_ARRAY(iint)
MAKE_ARRAY(ierror)
MAKE_ARRAY(ibool)
MAKE_ARRAY(itime)
MAKE_ARRAY(istring)
MAKE_ARRAY(iunit)

MAKE_ARRAY(ARRAY(idouble))
MAKE_ARRAY(ARRAY(iint))
MAKE_ARRAY(ARRAY(ierror))
MAKE_ARRAY(ARRAY(ibool))
MAKE_ARRAY(ARRAY(itime))
MAKE_ARRAY(ARRAY(istring))
MAKE_ARRAY(ARRAY(iunit))
#endif
