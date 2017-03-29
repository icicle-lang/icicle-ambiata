#include "05-error.h"

/* this global is pretty nasty, don't know how else we can get
 * information in a signal handler though */
#define SEGV_USER_DATA_SIZE (1024*1024)
static char segv_user_data[SEGV_USER_DATA_SIZE];

void segv_remove_handler ()
{
    struct sigaction act;

    sigemptyset (&act.sa_mask);
    act.sa_flags   = 0;
    act.sa_handler = SIG_DFL;

    sigaction (SIGSEGV, &act, NULL);
}

static void segv_handler (int sig)
{
    segv_remove_handler ();

    void *frames[50];
    size_t n_frames = backtrace (frames, sizeof(frames));

    fprintf (stderr, "icicle: segmentation fault:\n");
    backtrace_symbols_fd (frames, n_frames, STDERR_FILENO);
    fprintf (stderr, "\n%s\n", segv_user_data);

    exit (1);
}

void segv_install_handler (const char *user_data, size_t user_data_size)
{
    size_t size = MIN (user_data_size, SEGV_USER_DATA_SIZE-1);
    memcpy (segv_user_data, user_data, size);
    segv_user_data[size] = '\0';

    struct sigaction act;

    sigemptyset (&act.sa_mask);
    act.sa_flags   = 0;
    act.sa_handler = segv_handler;

    sigaction (SIGSEGV, &act, NULL);
}

static void INLINE iassert(const char *file, const int line, const char *func, const char *msg, ibool_t p)
{
    // #if ICICLE_DEBUG
    if (!p) {
        fprintf(stderr, "assertion failed");
        fprintf (stderr, "SEGV_USER_DATA:\n");
        fprintf (stderr, "%s\n", segv_user_data);
        fprintf(stderr, "assertion failed:\n%s:%d %s\n\t%s\n", file, line, func, msg);

        void *frames[50];
        size_t n_frames = backtrace (frames, sizeof(frames));

        backtrace_symbols_fd (frames, n_frames, STDERR_FILENO);

        exit(1);
    }
}

#define STRINGIFY(x) #x

#if ICICLE_ASSERT
#   define IASSERT(p) iassert(__FILE__, __LINE__, __func__, #p , p)
#else
#   define IASSERT(p) 
#endif

