#define _GNU_SOURCE 1

#include <assert.h>
#include <ctype.h>
#include <execinfo.h>
#include <fcntl.h>
#include <math.h>
#include <signal.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <unistd.h>

#pragma GCC diagnostic ignored "-Wunused-function"
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wundefined-internal"

typedef uint64_t ierror_t;
typedef uint64_t iunit_t;
typedef uint64_t ibool_t;
typedef  int64_t iint_t;
typedef   double idouble_t;
typedef  int64_t itime_t;

typedef const char *istring_t;
typedef const char *ierror_msg_t;

static const ierror_t ierror_not_an_error           = 0;
static const ierror_t ierror_tombstone              = 1;
static const ierror_t ierror_fold1_no_value         = 2;
static const ierror_t ierror_variable_not_available = 3;

static const iunit_t iunit  = 0x13013;
static const ibool_t ifalse = 0;
static const ibool_t itrue  = 1;

#if ICICLE_NOINLINE || ICICLE_DEBUG
#   define INLINE __attribute__((noinline))
#else
#   define INLINE __attribute__((always_inline))
#endif

#define NOINLINE __attribute__((noinline))

#define STATIC_ASSERT(cond, msg) typedef char static_assert_##msg[(!!(cond))*2-1];

#define ASSERT_SIZE(t, num_words) \
  STATIC_ASSERT (sizeof (t) == (num_words) * 8, type_has_unexpected_size);

STATIC_ASSERT(sizeof (void *) == sizeof (uint64_t), icicle_only_supports_systems_with_a_64_bit_word_size);

/* #if-style conditionals cannot be used inside macros so we define a macro
 * which only outputs its code when we're in debug mode. */
#if ICICLE_DEBUG
#   define ICICLE_WHEN_DEBUG(code) code
#else
#   define ICICLE_WHEN_DEBUG(code)
#endif

#define CONCAT0(x, y) x ## y
#define CONCAT(x, y)  CONCAT0(x, y)
