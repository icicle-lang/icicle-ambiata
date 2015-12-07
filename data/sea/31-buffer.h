#include "30-array.h"

#define BUF(n,t)         ibuf_ ## n ## _ ## t
#define BUF_FUN(n,t,fun) CONCAT(BUF(n,t), _ ## fun)
#define BUF_T(n,t)       CONCAT(BUF(n,t), _t)

/*
 Invariants
  0 <= size <= n
  0 <= head <  n

  read = vals[ head..(head+size) % n )

*/
#define MK_BUF_STRUCT(n,t)                                                      \
                                                                                \
typedef struct {                                                                \
    iint_t size;                                                                \
    iint_t head;                                                                \
    t##_t  vals[n];                                                             \
} BUF_T(n,t);


/*
Make
 Pre
  0 <= sz

 Post
  size' = 0
  head' = 0
  vals' = [??...]

  read' = vals'[head'..head'+size')
        = vals'[0..0)
        = []

  (invariants hold)

*/
#define MK_BUF_MAKE(n,t)                                                        \
                                                                                \
static BUF_T(n,t) INLINE BUF_FUN(n,t,make) ()                                   \
{                                                                               \
    const static BUF_T(n,t) empty;                                              \
    return empty;                                                               \
}

/*
Push(buf, val)
  {buf invariants hold}
  ...
  {
  head'  = if size < n
           then head
           else (head+1) % size

  size'  = min n (size+1)

  vals'  = vals[update]:=val
  read'  = vals[head'..(head'+size') % n)
  update = (head + size) % n
  }
  ==>
  {
  size'  = min n (size+1)
  read'  = (take (n - 1) read) ++ [val]
  }

*/
#define MK_BUF_PUSH(n,t)                                                        \
                                                                                \
static BUF_T(n,t) INLINE BUF_FUN(n,t,push) (BUF_T(n,t) *buf, t##_t val)         \
{                                                                               \
    iint_t size       = buf->size;                                              \
    iint_t head       = buf->head;                                              \
                                                                                \
    iint_t head_new   = size < n ? head : (head+1) % n;                         \
    iint_t size_new   = size < n ? size + 1 : n;                                \
                                                                                \
    iint_t update     = (head + size) % n;                                      \
                                                                                \
    buf->head         = head_new;                                               \
    buf->size         = size_new;                                               \
    buf->vals[update] = val;                                                    \
                                                                                \
    return *buf;                                                                \
}


/*
Read(buf)
 Pre
  (buf invariants hold)
 Post
  out = read
*/

#define MK_BUF_READ(n,t)                                                        \
                                                                                \
static ARRAY(t##_t) INLINE BUF_FUN(n,t,read) (imempool_t *pool, BUF_T(n,t) *buf)\
{                                                                               \
    iint_t size = buf->size;                                                    \
    iint_t head = buf->head;                                                    \
    t##_t *vals = buf->vals;                                                    \
                                                                                \
    ARRAY_T(t) out = ARRAY_FUN(t,create)(pool, size);                           \
                                                                                \
    for (iint_t ix = 0; ix != size; ++ix)                                       \
    {                                                                           \
        iint_t in = (head + ix) % n;                                            \
        ARRAY_PAYLOAD(t,out)[ix] = vals[in];                                    \
    }                                                                           \
                                                                                \
    return out;                                                                 \
}


/*
Copy(buf)
*/

#define MK_BUF_COPY(n,t)                                                        \
                                                                                \
static BUF_T(n,t) INLINE BUF_FUN(n,t,copy) (imempool_t *into, BUF_T(n,t) x)     \
{                                                                               \
    for (iint_t ix = 0; ix != n; ++ix) {                                        \
        t##_t cp   = x.vals[ix];                                                \
        x.vals[ix] = t##_copy(into, cp);                                        \
    }                                                                           \
                                                                                \
    return x;                                                                   \
}


/*
Eq(buf_x, buf_y)
Ne(buf_x, buf_y)
*/

#define MK_BUF_EQ(n,t)                                                          \
                                                                                \
static ibool_t INLINE BUF_FUN(n,t,eq) (BUF_T(n,t) x, BUF_T(n,t) y)              \
{                                                                               \
    if (x.size != y.size) return ifalse;                                        \
                                                                                \
    for (iint_t ix = 0; ix != x.size; ++ix) {                                   \
        iint_t x_ix = (ix + x.head) % n;                                        \
        iint_t y_ix = (ix + y.head) % n;                                        \
                                                                                \
        if (!t##_eq(x.vals[x_ix], y.vals[y_ix]))                                \
            return ifalse;                                                      \
    }                                                                           \
                                                                                \
    return itrue;                                                               \
}                                                                               \
                                                                                \
static ibool_t INLINE BUF_FUN(n,t,ne) (BUF_T(n,t) x, BUF_T(n,t) y)              \
{                                                                               \
    return !BUF_FUN(n,t,eq)(x, y);                                              \
}


/*
Lt(buf_x, buf_y)
Le(buf_x, buf_y)
Ge(buf_x, buf_y)
Gt(buf_x, buf_y)
*/

#define MK_BUF_CMP(n,t,op)                                                      \
                                                                                \
static ibool_t INLINE BUF_FUN(n,t,op) (BUF_T(n,t) x, BUF_T(n,t) y)              \
{                                                                               \
    iint_t min = (x.size < y.size) ? x.size : y.size;                           \
                                                                                \
    for (iint_t ix = 0; ix != min; ++ix) {                                      \
        iint_t x_ix = (ix + x.head) % n;                                        \
        iint_t y_ix = (ix + y.head) % n;                                        \
                                                                                \
        if (!t##_##op(x.vals[x_ix], y.vals[y_ix]))                              \
            return ifalse;                                                      \
    }                                                                           \
                                                                                \
    return iint_##op(x.size, y.size);                                           \
}


#define MK_BUF_CMPS(n,t)                                                        \
    MK_BUF_EQ(n,t)                                                              \
    MK_BUF_CMP(n,t,lt)                                                          \
    MK_BUF_CMP(n,t,le)                                                          \
    MK_BUF_CMP(n,t,gt)                                                          \
    MK_BUF_CMP(n,t,ge)


/*
Define a buffer
*/

#define MAKE_BUF(n,t)                                                           \
    MK_BUF_STRUCT (n,t)                                                         \
    MK_BUF_MAKE   (n,t)                                                         \
    MK_BUF_CMPS   (n,t)                                                         \
    MK_BUF_PUSH   (n,t)                                                         \
    MK_BUF_READ   (n,t)                                                         \
    MK_BUF_COPY   (n,t)


// enable if you need to resolve compiler errors in the macros above
#if 0
MAKE_BUF(5, idouble)
MAKE_BUF(5, iint)
MAKE_BUF(5, ierror)
MAKE_BUF(5, ibool)
MAKE_BUF(5, itime)
MAKE_BUF(5, iunit)
MAKE_BUF(5, istring)

MAKE_ARRAY(BUF(5, idouble))
MAKE_ARRAY(BUF(5, iint))
MAKE_ARRAY(BUF(5, ierror))
MAKE_ARRAY(BUF(5, ibool))
MAKE_ARRAY(BUF(5, itime))
MAKE_ARRAY(BUF(5, iunit))
MAKE_ARRAY(BUF(5, istring))
#endif
