#include <sys/resource.h>
#include <stdio.h>

void get_maxrss(long *maxrss_kb) {
  struct rusage usage;
  getrusage(RUSAGE_SELF, &usage);

  #ifdef __APPLE__
    /* macOS returns MaxRSS in bytes */
    *maxrss_kb = usage.ru_maxrss / 1024;
  #else
    /* Linux returns MaxRSS in KiB (kibibytes) 2^10 bytes (or 1024) */
    *maxrss_kb = usage.ru_maxrss;
  #endif
}
