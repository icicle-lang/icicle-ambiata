#include "10-mempool.h"

static idouble_t INLINE iint_extend   (iint_t    x)              { return x; }
static iint_t    INLINE iint_neg      (iint_t    x)              { return -x; }
static iint_t    INLINE iint_add      (iint_t    x, iint_t    y) { return x + y; }
static iint_t    INLINE iint_sub      (iint_t    x, iint_t    y) { return x - y; }
static iint_t    INLINE iint_mul      (iint_t    x, iint_t    y) { return x * y; }

static iint_t    INLINE idouble_trunc (idouble_t x)              { return (iint_t)x; }
static idouble_t INLINE idouble_neg   (idouble_t x)              { return -x; }
static idouble_t INLINE idouble_add   (idouble_t x, idouble_t y) { return x + y; }
static idouble_t INLINE idouble_sub   (idouble_t x, idouble_t y) { return x - y; }
static idouble_t INLINE idouble_mul   (idouble_t x, idouble_t y) { return x * y; }
static idouble_t INLINE idouble_pow   (idouble_t x, idouble_t y) { return pow(x, y); }
static idouble_t INLINE idouble_div   (idouble_t x, idouble_t y) { return x / y; }
static idouble_t INLINE idouble_log   (idouble_t x)              { return log(x); }
static idouble_t INLINE idouble_exp   (idouble_t x)              { return exp(x); }


template<typename T>
ibool_t ieq(const T& t, const T& u)
{ return t == u; }

template<typename T>
ibool_t ine(const T& t, const T& u)
{ return t != u; }

template<typename T>
ibool_t ilt(const T& t, const T& u)
{ return t <  u; }

template<typename T>
ibool_t ile(const T& t, const T& u)
{ return t <= u; }

template<typename T>
ibool_t igt(const T& t, const T& u)
{ return t >  u; }

template<typename T>
ibool_t ige(const T& t, const T& u)
{ return t >= u; }

