#include "00-includes.h"

/*
Error messages
*/

typedef const char *ierror_msg_t;

static const size_t error_msg_size = 8 * 1024;

static ierror_msg_t NOINLINE ierror_msg_format (const char *fmt, ...)
{
    va_list args;
    va_start (args, fmt);

    size_t  error_size = 4 * 1024;
    char   *error_text = calloc (error_size, 1);

    vsnprintf (error_text, error_size, fmt, args);

    va_end (args);

    return error_text;
}

static ierror_msg_t NOINLINE ierror_msg_alloc (const char *msg, const char *value_ptr, const size_t value_size)
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

static ierror_msg_t NOINLINE ierror_msg_add_line (iint_t line, ierror_msg_t original)
{
    size_t  error_size = 4 * 1024;
    char   *error_text = calloc (error_size, 1);

    snprintf (error_text, error_size, "line %lld: %s", line, original);
    free ((char *) original);

    return error_text;
}

/*
Location-based errors
*/

typedef enum ierror_tag {
  IERROR_LIMIT_EXCEEDED,
  IERROR_DISASTER
} ierror_tag_t;

typedef struct ierror_loc {
    const char   *line_start;
    const char   *line_end;
    const char   *span_start;
    const char   *span_end;
    iint_t        fd;
    ierror_tag_t  tag;
    char          message[0];
} *ierror_loc_t;

static ierror_loc_t NOINLINE v_ierror_loc_tag_format (const ierror_tag_t tag, const iint_t fd, const char *start, const char *end, const char *fmt, va_list args)
{
    const  size_t message_size = 4 * 1024;
    struct ierror_loc *error  = calloc (sizeof (struct ierror_loc) + message_size, 1);

    vsnprintf (error->message, message_size, fmt, args);

    error->fd         = fd;
    error->span_start = start;
    error->span_end   = end;
    error->tag        = tag;

    return error;
}

static ierror_loc_t NOINLINE ierror_loc_tag_format (const ierror_tag_t tag, const iint_t fd, const char *start, const char *end, const char *fmt, ...)
{
    va_list args;
    va_start (args, fmt);

    struct ierror_loc *error = v_ierror_loc_tag_format (tag, fd, start, end, fmt, args);

    va_end (args);

    return error;
}

static ierror_loc_t NOINLINE v_ierror_loc_format (const iint_t fd, const char *start, const char *end, const char *fmt, va_list args)
{
    return v_ierror_loc_tag_format (IERROR_DISASTER, fd, start, end, fmt, args);
}

static ierror_loc_t NOINLINE ierror_loc_format (const iint_t fd, const char *start, const char *end, const char *fmt, ...)
{
    va_list args;
    va_start (args, fmt);

    struct ierror_loc *error = v_ierror_loc_format (fd, start, end, fmt, args);

    va_end (args);

    return error;
}

static ierror_msg_t NOINLINE ierror_loc_pretty (ierror_loc_t loc, iint_t line)
{
    size_t  msg_size = error_msg_size;
    char   *msg_text = calloc (msg_size, 1);
    bzero(msg_text, msg_size);

    char *p  = msg_text;
    char *pe = msg_text + msg_size;

    char *filename = calloc (255, 1);
    char *proc     = calloc (269, 1);
    const iint_t fd = loc->fd;

    snprintf (proc, 269, "/proc/self/fd/%lld", fd);
    ssize_t ret = readlink (proc, filename, 255);

    if (ret < 0) {
        p += snprintf (p, pe - p, "from file descriptor %lld (cannot get filepath)\n", fd);
    } else {
        p += snprintf (p, pe - p, "from file: %s\n", filename);
    }

    int prefix_size = snprintf (p, pe - p, "line %lld: ", line);
    p += prefix_size;
    p += snprintf (p, pe - p, "%.*s\n", (int)(loc->line_end - loc->line_start), loc->line_start);

    if (loc->span_start == 0 || loc->span_end == 0)
        goto done;

    int ix_start = prefix_size + (loc->span_start - loc->line_start);
    int ix_end   = prefix_size + (loc->span_end   - loc->line_start);

    static const char *span_chars = "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";

    if (ix_start <= ix_end) {
        int span_size = ix_end - ix_start;
        p += snprintf (p, pe - p, "%*s",    ix_start, "");
        p += snprintf (p, pe - p, "%.*s^ ", MAX (0, span_size), span_chars);
        goto done;
    }

    if (ix_start > ix_end) {
        int span_size = ix_start - ix_end;
        p += snprintf (p, pe - p, "%*s",    ix_end, "");
        p += snprintf (p, pe - p, "^%.*s ", MAX (0, span_size), span_chars);
        goto done;
    }

done:
    snprintf (p, pe - p, "%s", loc->message);
    free ((struct ierror_loc *) loc);

    return msg_text;
}

/*
Debugging
*/

static void NOINLINE idebug (const char *msg, const char *value_ptr, const size_t value_size)
{
    char value_text[4*1024] = {0};
    memcpy (value_text, value_ptr, MIN (value_size, sizeof (value_text) - 1));

    fprintf (stderr, "icicle-debug: %s: %s\n", msg, value_text);
}
