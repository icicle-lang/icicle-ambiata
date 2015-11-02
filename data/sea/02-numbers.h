
static ibool_t   INLINE ierror_eq     (ierror_t  x, ierror_t  y) { return x == y; }

static idouble_t INLINE iint_extend   (iint_t    x)              { return x; }
static iint_t    INLINE iint_add      (iint_t    x, iint_t    y) { return x +  y; }
static iint_t    INLINE iint_sub      (iint_t    x, iint_t    y) { return x -  y; }
static iint_t    INLINE iint_mul      (iint_t    x, iint_t    y) { return x *  y; }
static ibool_t   INLINE iint_gt       (iint_t    x, iint_t    y) { return x >  y; }
static ibool_t   INLINE iint_ge       (iint_t    x, iint_t    y) { return x >= y; }
static ibool_t   INLINE iint_lt       (iint_t    x, iint_t    y) { return x <  y; }
static ibool_t   INLINE iint_le       (iint_t    x, iint_t    y) { return x <= y; }
static ibool_t   INLINE iint_eq       (iint_t    x, iint_t    y) { return x == y; }
static ibool_t   INLINE iint_ne       (iint_t    x, iint_t    y) { return x != y; }

static iint_t    INLINE idouble_trunc (idouble_t x)              { return (iint_t)x; }
static idouble_t INLINE idouble_add   (idouble_t x, idouble_t y) { return x + y; }
static idouble_t INLINE idouble_sub   (idouble_t x, idouble_t y) { return x - y; }
static idouble_t INLINE idouble_mul   (idouble_t x, idouble_t y) { return x * y; }
static idouble_t INLINE idouble_pow   (idouble_t x, idouble_t y) { return pow(x, y); }
static idouble_t INLINE idouble_div   (idouble_t x, idouble_t y) { return x / y; }
static idouble_t INLINE idouble_log   (idouble_t x)              { return log(x); }
static idouble_t INLINE idouble_exp   (idouble_t x)              { return exp(x); }
static ibool_t   INLINE idouble_gt    (idouble_t x, idouble_t y) { return x >  y; }
static ibool_t   INLINE idouble_ge    (idouble_t x, idouble_t y) { return x >= y; }
static ibool_t   INLINE idouble_lt    (idouble_t x, idouble_t y) { return x <  y; }
static ibool_t   INLINE idouble_le    (idouble_t x, idouble_t y) { return x <= y; }
static ibool_t   INLINE idouble_eq    (idouble_t x, idouble_t y) { return x == y; }
static ibool_t   INLINE idouble_ne    (idouble_t x, idouble_t y) { return x != y; }

