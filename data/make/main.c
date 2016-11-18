#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

typedef uint64_t ibool_t;
typedef int64_t iint_t;

typedef struct {
    /* inputs */
    /* these are 32-bit file handles, but storing them as 64-bit in the struct makes it easier to poke from Haskell */
    iint_t input_fd;
    iint_t output_fd;
    iint_t drop_fd;
    iint_t chord_fd;

    /* outputs */
    const char *error;
    iint_t fact_count;
    iint_t entity_count;

    /* stuff */
    size_t facts_limit;   /* maximum number of facts per entity-attribute */
    ibool_t has_drop; /* whether to write discarded facts to output or drop log */
} psv_config_t;

void psv_snapshot (psv_config_t *config);

int main ()
{
    psv_config_t config = { 0 };

    config.input_fd = STDIN_FILENO;
    config.output_fd = STDOUT_FILENO;
    config.drop_fd = STDERR_FILENO;

    config.facts_limit = 128 * 1024;
    config.has_drop = 1;

    psv_snapshot (&config);

    printf ("facts = %" PRId64 "\n", config.fact_count);
    printf ("entities = %" PRId64 "\n", config.entity_count);

    if (config.error) {
        printf ("error: %s\n", config.error);
        return 1;
    }

    return 0;
}
