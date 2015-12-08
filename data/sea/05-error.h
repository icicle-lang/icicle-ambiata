#include "00-includes.h"

static ierror_msg_t ierror_msg_alloc (const char *msg, const char *value_ptr, const size_t value_size)
{
    size_t  error_size = 4 * 1024;
    char   *error_text = calloc (error_size, 1);

    if (value_ptr) {
        char value_text[4*1024] = {0};
        memcpy (value_text, value_ptr, MIN (value_size, sizeof (value_text) - 1));

        snprintf (error_text, error_size, "%s: %s", msg, value_text);
    } else {
        snprintf (error_text, error_size, "%s", msg);
    }

    return error_text;
}

static ierror_msg_t ierror_msg_add_line (iint_t line, ierror_msg_t original)
{
    size_t  error_size = 4 * 1024;
    char   *error_text = calloc (error_size, 1);

    snprintf (error_text, error_size, "line %lld: %s", line, original);
    free ((char *) original);

    return error_text;
}

static void idebug (const char *msg, const char *value_ptr, const size_t value_size)
{
    char value_text[4*1024] = {0};
    memcpy (value_text, value_ptr, MIN (value_size, sizeof (value_text) - 1));

    fprintf (stderr, "icicle-debug: %s: %s\n", msg, value_text);
}
