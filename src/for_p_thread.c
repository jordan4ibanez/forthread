#include <pthread.h>
#include <unistd.h>
#include <stdbool.h>
#include <assert.h>
#include <inttypes.h>
// This one is for looking up error IDs.
// #include <errno.h>
// Go through to <errno-base.h>

// You can ctrl - click this to get there.
// int test = ENOENT;

//! TODO: Needs to be tested on FreeBSD.

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
}

//* Getting the number of available threads.
//* You can thank tavianator: https://www.reddit.com/r/C_Programming/comments/6zxnr1/comment/dmzuwt6
int32_t for_p_thread_get_cpu_threads(bool leave_room_for_main)
{
  int thread_count = sysconf(_SC_NPROCESSORS_ONLN);
  if (leave_room_for_main)
  {
    thread_count = thread_count - 1;
    if (thread_count == 0)
    {
      thread_count = 1;
    }
  }
  return thread_count;
}