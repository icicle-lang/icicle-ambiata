#include "30-array.h"

typedef struct
{
    iint_t max_size;
    iint_t cur_size;
    iint_t head;
} ibuf_struct;
/*
 Invariants
  0 <= cur_size <= max_size
  0 <= head     <  max_size

  read = ARRAY[ head..(head+cur_size) % max_size )

*/

#define BUF(t)         ibuf_t__##t
#define BUF_FUN(f,pre) ibuf__##pre##f

#define MK_BUF_STRUCT(t) typedef ibuf_struct* BUF(t);

#define BUF_PAYLOAD(x,t) ((t*)(x+1))

/*
Make
 Pre
  0 <= sz

 Post
  cur_size' = 0
  head'     = 0
  max_size' = sz
  ARRAY'    = [??...]

  read'     = ARRAY'[head'..head'+cur_size')
            = ARRAY'[0..0)
            = []

  (invariants hold)

*/
#define MK_BUF_MAKE(t,pre)                                                      \
    static BUF(t) INLINE BUF_FUN(make,pre)                                      \
                     (imempool_t *pool, iint_t sz)                              \
    {                                                                           \
        size_t bytes  = sizeof(t) * sz + sizeof(ibuf_struct);                   \
        BUF(t) ret    = (BUF(t))imempool_alloc(pool, bytes);                    \
        ret->max_size = sz;                                                     \
        ret->cur_size = 0;                                                      \
        ret->head     = 0;                                                      \
        return ret;                                                             \
    }

/*
Push(buf, val)
 Pre
  (buf invariants hold)
 Post
  cur_size' = min max_size (cur_size+1)
  read'     = (take (max_size - 1) read) ++ [val]

*/

/*
Push(buf, val)
  {buf invariants hold}
  ...
  {
  head'     = if cur_size < max_size
              then head
              else (head+1) % max_size

  cur_size' = min max_size (cur_size+1)

  ARRAY'    = ARRAY[update]:=val
  read'     = ARRAY[head'..(head'+cur_size') % max_size)
  update    = (head + cur_size) % max_size
  }
  ==>
  {
  cur_size' = min max_size (cur_size+1)
  read'     = (take (max_size - 1) read) ++ [val]
  }

*/

#define MK_BUF_PUSH(t,pre)                                                      \
    static BUF(t) INLINE BUF_FUN(push,pre) (BUF(t) buf, t val)                  \
    {                                                                           \
        iint_t head_new = (buf->cur_size < buf->max_size)                       \
                        ?  buf->head                                            \
                        : (buf->head+1) % buf->max_size;                        \
                                                                                \
        iint_t size_new = (buf->cur_size < buf->max_size)                       \
                        ?  buf->cur_size + 1                                    \
                        :  buf->max_size;                                       \
                                                                                \
        iint_t update   = (buf->head + buf->cur_size) % buf->max_size;          \
                                                                                \
        BUF_PAYLOAD(buf,t)[update] = val;                                       \
                                                                                \
        buf->head     = head_new;                                               \
        buf->cur_size = size_new;                                               \
        return buf;                                                             \
    }


/*
Read(buf)
 Pre
  (buf invariants hold)
 Post
  out       = read
*/

#define MK_BUF_READ(t,pre)                                                      \
    static ARRAY(t) INLINE BUF_FUN(read,pre)                                    \
                    (imempool_t *pool, BUF(t) buf)                              \
    {                                                                           \
        ARRAY(t) out = ARRAY_FUN(create,pre)(pool, buf->cur_size);              \
                                                                                \
        for (iint_t ix = 0; ix != buf->cur_size; ++ix)                          \
        {                                                                       \
            iint_t in = (buf->head + ix) % buf->max_size;                       \
            ARRAY_PAYLOAD(out,t)[ix] = BUF_PAYLOAD(buf,t)[in];                  \
        }                                                                       \
                                                                                \
        return out;                                                             \
    }



#define MAKE_BUF(t,pre)                                                         \
    MK_BUF_STRUCT (t)                                                           \
    MK_BUF_MAKE   (t,pre)                                                       \
    MK_BUF_PUSH   (t,pre)                                                       \
    MK_BUF_READ   (t,pre)

MAKE_BUF(idouble_t,   idouble_)
MAKE_BUF(iint_t,      iint_)
MAKE_BUF(ierror_t,    ierror_)
MAKE_BUF(ibool_t,     ibool_)
MAKE_BUF(idate_t,     idate_)
MAKE_BUF(iunit_t,     iunit_)
MAKE_BUF(istring_t,   istring_)
