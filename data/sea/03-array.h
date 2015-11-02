
#define ARRAY_OF(t) iarray_t__##t
#define ARRAY_PREFIX(pre) iarray__##pre
#define ARRAY_FUN(f,pre) iarray__##pre##f

#define MK_ARRAY_STRUCT(t)                                                      \
    typedef struct                                                              \
    {                                                                           \
        t* elements;                                                            \
        iint_t count;                                                           \
    } *ARRAY_OF(t);

#define MK_ARRAY_LENGTH(t,pre)                                                  \
    static iint_t INLINE ARRAY_FUN(length,t) (ARRAY_OF(t) arr)                  \
    { return arr->count; }

#define MK_ARRAY_EQ(t,pre)                                                      \
    static ibool_t INLINE ARRAY_FUN(eq,t) (ARRAY_OF(t) x, ARRAY_OF(t) y)        \
    {                                                                           \
        if (x->count != y->count) return ifalse;                                \
        for (iint_t ix = 0; ix != x->count; ++ix) {                             \
            if (!pre##eq(x->elements[ix], y->elements[ix])) return ifalse;      \
        }                                                                       \
        return itrue;                                                           \
    }

#define MK_ARRAY_LT(t,pre)                                                      \
    static ibool_t INLINE ARRAY_FUN(lt,t) (ARRAY_OF(t) x, ARRAY_OF(t) y)        \
    {                                                                           \
        iint_t min = (x->count < y->count) ? x->count : y->count;               \
        for (iint_t ix = 0; ix != min; ++ix) {                                  \
            if (!pre##lt(x->elements[ix], y->elements[ix])) return ifalse;      \
        }                                                                       \
        if (x->count < y->count)                                                \
            return itrue;                                                       \
        else                                                                    \
            return ifalse;                                                      \
    }

#define MK_ARRAY_CMP(t,op,ret)                                                  \
    static ibool_t INLINE ARRAY_FUN(op,t) (ARRAY_OF(t) x, ARRAY_OF(t) y)        \
    { return ret ; }                                                            \

#define MK_ARRAY_CMPS(t,pre)                                                    \
    MK_ARRAY_EQ(t,pre)                                                          \
    MK_ARRAY_LT(t,pre)                                                          \
    MK_ARRAY_CMP(t,ne, !ARRAY_FUN(eq,t) (x,y))                                  \
    MK_ARRAY_CMP(t,le,  ARRAY_FUN(lt,t) (x,y) || ARRAY_FUN(eq,t) (x,y))         \
    MK_ARRAY_CMP(t,ge, !ARRAY_FUN(lt,t) (x,y))                                  \
    MK_ARRAY_CMP(t,gt, !ARRAY_FUN(le,t) (x,y))                                  \



#define MK_ARRAY_INDEX(t,pre)                                                   \
    static t       INLINE ARRAY_FUN(index,t) (ARRAY_OF(t) x, iint_t ix)         \
    { return x->elements[ix]; }                                                 \


#define MK_ARRAY_CREATE(t,pre)                                                  \
    static ARRAY_OF(t)  INLINE ARRAY_FUN(create,t)                              \
                                        (iallocate_t alloc, iint_t sz)          \
    {                                                                           \
        iint_t bytes     = sizeof(t) * sz + sizeof(iint_t);                     \
        ARRAY_OF(t)  ret = (ARRAY_OF(t))allocate(alloc, bytes);                 \
        ret->count = sz;                                                        \
        return ret;                                                             \
    }                                                                           \

#define MK_ARRAY_PUT(t,pre)                                                     \
    static iunit_t INLINE ARRAY_FUN(put,t)   (ARRAY_OF(t) x, iint_t ix, t v)    \
    {                                                                           \
        x->elements[ix] = v;                                                    \
        return iunit;                                                           \
    }                                                                           \

                                                                                \
                                                                                \
                                                                                \
                                                                                \
                                                                                \



#define MAKE_ARRAY(t,pre)                                                       \
    MK_ARRAY_STRUCT (t)                                                         \
    MK_ARRAY_LENGTH (t,pre)                                                     \
    MK_ARRAY_CMPS   (t,pre)                                                     \
    MK_ARRAY_INDEX  (t,pre)                                                     \
    MK_ARRAY_CREATE (t,pre)                                                     \
    MK_ARRAY_PUT    (t,pre)                                                     \
    // MK_ARRAY_ZIP    (t,pre)                                                     \

// TEMPORARY
typedef void* iallocate_t;
void* allocate(iallocate_t t, iint_t sz);

MAKE_ARRAY(idouble_t,   idouble_)
MAKE_ARRAY(iint_t,      iint_)
MAKE_ARRAY(ierror_t,    ierror_)
MAKE_ARRAY(ibool_t,     ibool_)
MAKE_ARRAY(idate_t,     idate_)
MAKE_ARRAY(iunit_t,     iunit_)
