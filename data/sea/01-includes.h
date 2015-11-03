#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

#define INLINE   __attribute__((always_inline))
#define NOINLINE __attribute__((noinline))

#define ASSERT_SIZE(t, num_words) \
  _Static_assert(sizeof (t) == (num_words) * 8, #t " must be " #num_words " words in size");

_Static_assert(sizeof (void *) == sizeof (uint64_t), "Icicle only supports systems with a 64-bit word size");

/* #if-style conditionals cannot be used inside macros so we define a macro
 * which only outputs its code when we're in debug mode. */
#if ICICLE_DEBUG
#   define ICICLE_WHEN_DEBUG(code) code
#else
#   define ICICLE_WHEN_DEBUG(code)
#endif
