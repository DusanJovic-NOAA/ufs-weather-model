#include <stdint.h>
#include <time.h>

// Returns time since epoch (1970-01-01 UTC) in nanoseconds

int64_t get_time_ns(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);

    int64_t ns = (int64_t)ts.tv_sec * 1000000000LL + ts.tv_nsec;

    return ns;
}
