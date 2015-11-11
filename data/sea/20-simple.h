#include "10-mempool.h"

#define ibool_and(x, y) (x) && (y)
#define ibool_or(x, y)  (x) || (y)
#define ibool_not(x)    !(x)

#define MK_SIMPLE_CMPS(t,pre)                                                   \
    static ibool_t   INLINE pre##eq   (t x, t y) { return x == y; }             \
    static ibool_t   INLINE pre##ne   (t x, t y) { return x != y; }             \
    static ibool_t   INLINE pre##gt   (t x, t y) { return x >  y; }             \
    static ibool_t   INLINE pre##ge   (t x, t y) { return x >= y; }             \
    static ibool_t   INLINE pre##lt   (t x, t y) { return x <  y; }             \
    static ibool_t   INLINE pre##le   (t x, t y) { return x <= y; }             \

MK_SIMPLE_CMPS(ierror_t, ierror_)
MK_SIMPLE_CMPS(ibool_t,  ibool_)
MK_SIMPLE_CMPS(idate_t,  idate_)
MK_SIMPLE_CMPS(iunit_t,  iunit_)

static idouble_t INLINE iint_extend   (iint_t    x)              { return x; }
static iint_t    INLINE iint_neg      (iint_t    x)              { return -x; }
static iint_t    INLINE iint_add      (iint_t    x, iint_t    y) { return x + y; }
static iint_t    INLINE iint_sub      (iint_t    x, iint_t    y) { return x - y; }
static iint_t    INLINE iint_mul      (iint_t    x, iint_t    y) { return x * y; }

MK_SIMPLE_CMPS(iint_t, iint_)

static iint_t    INLINE idouble_trunc (idouble_t x)              { return (iint_t)x; }
static idouble_t INLINE idouble_neg   (idouble_t x)              { return -x; }
static idouble_t INLINE idouble_add   (idouble_t x, idouble_t y) { return x + y; }
static idouble_t INLINE idouble_sub   (idouble_t x, idouble_t y) { return x - y; }
static idouble_t INLINE idouble_mul   (idouble_t x, idouble_t y) { return x * y; }
static idouble_t INLINE idouble_pow   (idouble_t x, idouble_t y) { return pow(x, y); }
static idouble_t INLINE idouble_div   (idouble_t x, idouble_t y) { return x / y; }
static idouble_t INLINE idouble_log   (idouble_t x)              { return log(x); }
static idouble_t INLINE idouble_exp   (idouble_t x)              { return exp(x); }

MK_SIMPLE_CMPS(idouble_t, idouble_)

static ibool_t   INLINE istring_gt    (istring_t x, istring_t y) { return strcmp(x, y) >  0; }
static ibool_t   INLINE istring_ge    (istring_t x, istring_t y) { return strcmp(x, y) >= 0; }
static ibool_t   INLINE istring_lt    (istring_t x, istring_t y) { return strcmp(x, y) <  0; }
static ibool_t   INLINE istring_le    (istring_t x, istring_t y) { return strcmp(x, y) <= 0; }
static ibool_t   INLINE istring_eq    (istring_t x, istring_t y) { return strcmp(x, y) == 0; }
static ibool_t   INLINE istring_ne    (istring_t x, istring_t y) { return strcmp(x, y) != 0; }
