#include <pthread.h>
#include <unistd.h>
#include <stdbool.h>
#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>
// #include <stdio.h>

#ifdef __WIN32__

#include <sysinfoapi.h>

#endif

// This one is for looking up error IDs.
// #include <errno.h>
// Go through to <errno-base.h>

// You can ctrl - click this to get there.
// int test = ENOENT;

//! TODO: Needs to be tested on FreeBSD.

/**
 * Notes about mutexes:
 *
 * Why are we running C code for this?
 *
 * We are running C code for this due to the data width of
 * pthread_mutex_t being 40 bytes wide on a 64 bit system.
 * I can make this work in Fortran using: integer(1), dimension(40),
 * but, this is the cleanest and most streamline example.
 * You only need the type(c_ptr) with this style.
 */

/**
 * Create a mutex pointer.
 */
pthread_mutex_t *for_p_thread_create_mutex()
{
  pthread_mutex_t *mutex = malloc(sizeof(pthread_mutex_t));

  // If this returns anything but 0, bail out.
  if (pthread_mutex_init(mutex, NULL) != 0)
  {
    assert(false);
  }

  return mutex;
}

/**
 * Destroy a mutex pointer.
 */
void for_p_thread_destroy_mutex(pthread_mutex_t *mutex)
{
  if (pthread_mutex_destroy(mutex) != 0)
  {
    assert(false);
  }

  free(mutex);
}

/**
 * Get the number of available threads.
 *
 * If leave_room_for_main is true, this will give you N-1 threads.
 *
 * You can thank tavianator: https://www.reddit.com/r/C_Programming/comments/6zxnr1/comment/dmzuwt6
 */
size_t for_p_thread_get_cpu_thread_count()
{
  // Windows is special.
#ifdef __WIN32__

  SYSTEM_INFO sysinfo;
  GetSystemInfo(&sysinfo);

  // If this ever runs into problems, here's a quick testing tool. :)
  // printf("CPU THREADS: %i\n", sysinfo.dwNumberOfProcessors);

  return (size_t)sysinfo.dwNumberOfProcessors;

#else

  return (size_t)sysconf(_SC_NPROCESSORS_ONLN);

#endif
}