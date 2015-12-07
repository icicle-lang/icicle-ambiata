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

#define ARRAY_SIZE(t,n)    ((sizeof(t##_t) * (n)) + sizeof(iarray_struct))


/*
Length(arr)
*/

#define MK_ARRAY_LENGTH(t)                                                      \
                                                                                \
static iint_t INLINE ARRAY_FUN(t,length) (ARRAY_T(t) arr)                       \
{                                                                               \
    return arr->count;                                                          \
}


/*
Eq(arr_x, arr_y)
*/

#define MK_ARRAY_EQ(t)                                                          \
                                                                                \
static ibool_t INLINE ARRAY_FUN(t,eq) (ARRAY_T(t) x, ARRAY_T(t) y)              \
{                                                                               \
    if (x->count != y->count) return ifalse;                                    \
                                                                                \
    for (iint_t ix = 0; ix != x->count; ++ix) {                                 \
        if (!t##_eq(ARRAY_PAYLOAD(t,x)[ix], ARRAY_PAYLOAD(t,y)[ix]))            \
            return ifalse;                                                      \
    }                                                                           \
                                                                                \
    return itrue;                                                               \
}


/*
Lt(arr_x, arr_y)
*/

#define MK_ARRAY_LT(t)                                                          \
                                                                                \
static ibool_t INLINE ARRAY_FUN(t,lt) (ARRAY_T(t) x, ARRAY_T(t) y)              \
{                                                                               \
    iint_t x_count = x->count;                                                  \
    iint_t y_count = y->count;                                                  \
    iint_t min     = x_count < y_count ? x_count : y_count;                     \
                                                                                \
    for (iint_t ix = 0; ix != min; ++ix) {                                      \
        if (!t##_lt(ARRAY_PAYLOAD(t,x)[ix], ARRAY_PAYLOAD(t,y)[ix]))            \
            return ifalse;                                                      \
    }                                                                           \
                                                                                \
    return x_count < y_count;                                                   \
}


/*
Ne(arr_x, arr_y)
Le(arr_x, arr_y)
Ge(arr_x, arr_y)
Gt(arr_x, arr_y)
*/

#define MK_ARRAY_CMP(t,op,ret)                                                  \
                                                                                \
static ibool_t INLINE ARRAY_FUN(t,op) (ARRAY_T(t) x, ARRAY_T(t) y)              \
{                                                                               \
    return ret;                                                                 \
}

#define MK_ARRAY_CMPS(t)                                                        \
    MK_ARRAY_EQ(t)                                                              \
    MK_ARRAY_LT(t)                                                              \
    MK_ARRAY_CMP(t,ne, !ARRAY_FUN(t,eq) (x,y))                                  \
    MK_ARRAY_CMP(t,le,  ARRAY_FUN(t,lt) (x,y) || ARRAY_FUN(t,eq) (x,y))         \
    MK_ARRAY_CMP(t,ge, !ARRAY_FUN(t,lt) (x,y))                                  \
    MK_ARRAY_CMP(t,gt, !ARRAY_FUN(t,le) (x,y))


/*
Index(arr, ix)
*/

#define MK_ARRAY_INDEX(t)                                                       \
                                                                                \
static t##_t INLINE ARRAY_FUN(t,index) (ARRAY_T(t) x, iint_t ix)                \
{                                                                               \
    return ARRAY_PAYLOAD(t,x)[ix];                                              \
}


/*
Put(size)
*/

#define MK_ARRAY_CREATE(t)                                                      \
                                                                                \
static ARRAY_T(t) INLINE ARRAY_FUN(t,create) (imempool_t *pool, iint_t sz)      \
{                                                                               \
    iint_t sz_alloc = iarray_size(sz);                                          \
    size_t bytes    = ARRAY_SIZE(t,sz_alloc);                                   \
                                                                                \
    ARRAY_T(t) ret  = (ARRAY_T(t))imempool_alloc(pool, bytes);                  \
    ret->count      = sz;                                                       \
                                                                                \
    return ret;                                                                 \
}


/*
Put(arr, ix, val)
*/

#define MK_ARRAY_PUT(t)                                                         \
                                                                                \
static ARRAY_T(t) INLINE ARRAY_FUN(t,put)                                       \
                  (imempool_t *pool, ARRAY_T(t) x, iint_t ix, t##_t v)          \
{                                                                               \
    iint_t count  = x->count;                                                   \
    iint_t sz_old = iarray_size(count);                                         \
                                                                                \
    if (ix >= sz_old) {                                                         \
        iint_t sz_new    = iarray_size(ix+1);                                   \
        size_t bytes_new = ARRAY_SIZE(t, sz_new);                               \
        size_t bytes_old = ARRAY_SIZE(t, sz_old);                               \
                                                                                \
        ARRAY_T(t) arr = (ARRAY_T(t))imempool_alloc(pool, bytes_new);           \
        memcpy(arr, x, bytes_old);                                              \
        x = arr;                                                                \
                                                                                \
        x->count = ix + 1;                                                      \
    } else if (ix >= count) {                                                   \
        x->count = ix + 1;                                                      \
    }                                                                           \
                                                                                \
    ARRAY_PAYLOAD(t,x)[ix] = v;                                                 \
    return x;                                                                   \
}


/*
Copy(arr)
*/

#define MK_ARRAY_COPY(t)                                                        \
                                                                                \
static ARRAY_T(t) INLINE ARRAY_FUN(t,copy) (imempool_t *into, ARRAY_T(t) x)     \
{                                                                               \
    if (x == 0) return 0;                                                       \
                                                                                \
    ARRAY_T(t) arr = ARRAY_FUN(t,create)(into, x->count);                       \
                                                                                \
    for (iint_t ix = 0; ix != x->count; ++ix) {                                 \
        t##_t cp = ARRAY_PAYLOAD(t,x)[ix];                                      \
        ARRAY_PAYLOAD(t,arr)[ix] = t##_copy(into, cp);                          \
    }                                                                           \
                                                                                \
    return arr;                                                                 \
}


/*
Define an array
*/

#define MAKE_ARRAY(t)                                                           \
    MK_ARRAY_STRUCT (t)                                                         \
    MK_ARRAY_LENGTH (t)                                                         \
    MK_ARRAY_CMPS   (t)                                                         \
    MK_ARRAY_INDEX  (t)                                                         \
    MK_ARRAY_CREATE (t)                                                         \
    MK_ARRAY_PUT    (t)                                                         \
    MK_ARRAY_COPY   (t)                                                         \


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
