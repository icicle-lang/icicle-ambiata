#include "21-date.h"

typedef struct
{
    iint_t count;
} iarray_struct;
// payload goes straight after

#define ARRAY(t)         iarray_t__##t
#define ARRAY_FUN(f,pre) iarray__##pre##f

// I'm not certain there's a point having a different one for each type.
// It makes it look a little better, but I don't think it's any safer.
#define MK_ARRAY_STRUCT(t) typedef iarray_struct* ARRAY(t);

// get payload by advancing pointer by size of struct
// (which should be equivalent to straight after struct fields)
// then casting to t*
#define ARRAY_PAYLOAD(x,t) ((t*)(x+1))


#define MK_ARRAY_LENGTH(t,pre)                                                  \
    static iint_t INLINE ARRAY_FUN(length,pre) (ARRAY(t) arr)                   \
    { return arr->count; }

#define MK_ARRAY_EQ(t,pre)                                                      \
    static ibool_t INLINE ARRAY_FUN(eq,pre) (ARRAY(t) x, ARRAY(t) y)            \
    {                                                                           \
        if (x->count != y->count) return ifalse;                                \
        for (iint_t ix = 0; ix != x->count; ++ix) {                             \
            if (!pre##eq(ARRAY_PAYLOAD(x,t)[ix], ARRAY_PAYLOAD(y,t)[ix]))       \
                return ifalse;                                                  \
        }                                                                       \
        return itrue;                                                           \
    }

#define MK_ARRAY_LT(t,pre)                                                      \
    static ibool_t INLINE ARRAY_FUN(lt,pre) (ARRAY(t) x, ARRAY(t) y)            \
    {                                                                           \
        iint_t min = (x->count < y->count) ? x->count : y->count;               \
        for (iint_t ix = 0; ix != min; ++ix) {                                  \
            if (!pre##lt(ARRAY_PAYLOAD(x,t)[ix], ARRAY_PAYLOAD(y,t)[ix]))       \
                return ifalse;                                                  \
        }                                                                       \
        if (x->count < y->count)                                                \
            return itrue;                                                       \
        else                                                                    \
            return ifalse;                                                      \
    }

#define MK_ARRAY_CMP(t,pre,op,ret)                                              \
    static ibool_t INLINE ARRAY_FUN(op,pre) (ARRAY(t) x, ARRAY(t) y)            \
    { return ret ; }

#define MK_ARRAY_CMPS(t,pre)                                                    \
    MK_ARRAY_EQ(t,pre)                                                          \
    MK_ARRAY_LT(t,pre)                                                          \
    MK_ARRAY_CMP(t,pre,ne, !ARRAY_FUN(eq,pre) (x,y))                            \
    MK_ARRAY_CMP(t,pre,le,  ARRAY_FUN(lt,pre) (x,y) || ARRAY_FUN(eq,pre) (x,y)) \
    MK_ARRAY_CMP(t,pre,ge, !ARRAY_FUN(lt,pre) (x,y))                            \
    MK_ARRAY_CMP(t,pre,gt, !ARRAY_FUN(le,pre) (x,y))



#define MK_ARRAY_INDEX(t,pre)                                                   \
    static t       INLINE ARRAY_FUN(index,pre) (ARRAY(t) x, iint_t ix)          \
    { return ARRAY_PAYLOAD(x,t)[ix]; }


#define ARRAY_SIZE(t,x) ((sizeof(t) * (x)) + sizeof(iarray_struct))

#define MK_ARRAY_CREATE(t,pre)                                                  \
    static ARRAY(t)  INLINE ARRAY_FUN(create,pre)                               \
                                     (imempool_t *pool, iint_t sz)              \
    {                                                                           \
        iint_t sz_alloc = next_power_of_two(sz);                                \
        size_t bytes = ARRAY_SIZE(t,sz_alloc);                                  \
        ARRAY(t) ret = (ARRAY(t))imempool_alloc(pool, bytes);                   \
        ret->count   = sz;                                                      \
        return ret;                                                             \
    }

#define MK_ARRAY_PUT(t,pre)                                                     \
    static ARRAY(t) INLINE ARRAY_FUN(put,pre)                                   \
            (imempool_t *pool, ARRAY(t) x, iint_t ix, t v)                      \
    {                                                                           \
        iint_t count  = x->count;                                               \
        iint_t sz_old = next_power_of_two(count);                               \
        if (ix >= sz_old) {                                                     \
            iint_t sz_new    = next_power_of_two(ix);                           \
            size_t bytes_new = ARRAY_SIZE(t, sz_new);                           \
            size_t bytes_old = ARRAY_SIZE(t, sz_old);                           \
                                                                                \
            ARRAY(t) arr = (ARRAY(t))imempool_alloc(pool, bytes_new);           \
            memcpy(arr, x, bytes_old);                                          \
            arr->count = ix;                                                    \
            x = arr;                                                            \
                                                                                \
        } else if (ix >= count) {                                               \
            x->count = ix;                                                      \
        }                                                                       \
        ARRAY_PAYLOAD(x,t)[ix] = v;                                             \
        return x;                                                               \
    }

#define MK_ARRAY_COPY(t,pre)                                                    \
    static ARRAY(t) INLINE ARRAY_FUN(copy,pre)   (imempool_t *into, ARRAY(t) x) \
    {                                                                           \
        ARRAY(t) arr = ARRAY_FUN(create,pre)(into, x->count);                   \
        for (iint_t ix = 0; ix != x->count; ++ix) {                             \
            t cp = ARRAY_PAYLOAD(x,t)[ix];                                      \
            ARRAY_PAYLOAD(arr,t)[ix] = pre##copy(into, cp);                     \
        }                                                                       \
        return arr;                                                             \
    }




#define MAKE_ARRAY(t,pre)                                                       \
    MK_ARRAY_STRUCT (t)                                                         \
    MK_ARRAY_LENGTH (t,pre)                                                     \
    MK_ARRAY_CMPS   (t,pre)                                                     \
    MK_ARRAY_INDEX  (t,pre)                                                     \
    MK_ARRAY_CREATE (t,pre)                                                     \
    MK_ARRAY_PUT    (t,pre)                                                     \
    MK_ARRAY_COPY   (t,pre)                                                     \

MAKE_ARRAY(idouble_t,   idouble_)
MAKE_ARRAY(iint_t,      iint_)
MAKE_ARRAY(ierror_t,    ierror_)
MAKE_ARRAY(ibool_t,     ibool_)
MAKE_ARRAY(idate_t,     idate_)
MAKE_ARRAY(istring_t,   istring_)
MAKE_ARRAY(iunit_t,     iunit_)

MAKE_ARRAY(iarray_t__idouble_t,   iarray__idouble_)
MAKE_ARRAY(iarray_t__iint_t,      iarray__iint_)
MAKE_ARRAY(iarray_t__ierror_t,    iarray__ierror_)
MAKE_ARRAY(iarray_t__ibool_t,     iarray__ibool_)
MAKE_ARRAY(iarray_t__idate_t,     iarray__idate_)
MAKE_ARRAY(iarray_t__istring_t,   iarray__istring_)
MAKE_ARRAY(iarray_t__iunit_t,     iarray__iunit_)



// Specialise array of unit to a single int
/*
typedef iint_t ARRAY(iunit_t);
MK_SIMPLE_CMPS(iarray_t__iunit_t, iarray__iunit_)

static iint_t INLINE ARRAY_FUN(length,iunit_) (ARRAY(iunit_t) arr)
{ return arr; }
static iint_t INLINE ARRAY_FUN(index,iunit_) (ARRAY(iunit_t) arr, iint_t ix)
{ return iunit; }
static iint_t INLINE ARRAY_FUN(create,iunit_) (imempool_t *pool, iint_t sz)
{ return sz; }
static iint_t INLINE ARRAY_FUN(put,iunit_) (ARRAY(iunit_t) arr, iint_t ix, iunit_t v)
{ return arr; }
static iint_t INLINE ARRAY_FUN(copy,iunit_) (imempool_t *pool, imempool_t *into, ARRAY(iunit_t) arr)
{ return arr; }

*/
