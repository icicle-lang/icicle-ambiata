#include <stdbool.h>
#include <stdint.h>
#include <math.h>

typedef uint64_t ierror_t;
typedef uint64_t iunit_t;
typedef uint64_t ibool_t;
typedef  int64_t iint_t;
typedef   double idouble_t;
typedef  int64_t idate_t;

typedef const char *istring_t;

static const ierror_t ierror_tombstone              = 0;
static const ierror_t ierror_fold1_no_value         = 1;
static const ierror_t ierror_variable_not_available = 2;

static const iunit_t iunit  = 0x13013;
static const ibool_t ifalse = 0;
static const ibool_t itrue  = 1;

#define INLINE __attribute__((always_inline))
