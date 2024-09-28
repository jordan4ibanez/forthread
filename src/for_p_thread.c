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

// Forward declaration.
pthread_mutex_t *for_p_thread_create_mutex();
void for_p_thread_destroy_mutex(pthread_mutex_t *mutex);
uint64_t for_p_thread_create_thread(void *(*start_routine)(void *), void *restrict arg);
size_t for_p_thread_get_cpu_threads(bool leave_room_for_main);

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

  free(mutex);
}

/**
 * Create thread TID pointer.
 */
uint64_t for_p_thread_create_thread(void *(*start_routine)(void *), void *restrict arg)
{
  // In POSIX, pthread_t IS the thread ID.
  // This is of size_t. Since I only target 64 bit, it's 8 bytes wide.
  uint64_t tid = 0;

  // If it fails to create, bail out.
  if (pthread_create(&tid, NULL, start_routine, arg) != 0)
  {
    assert(false);
  }

  return tid;
}

/**
 * Get the number of available threads.
 *
 * If leave_room_for_main is true, this will give you N-1 threads.
 *
 * You can thank tavianator: https://www.reddit.com/r/C_Programming/comments/6zxnr1/comment/dmzuwt6
 */
size_t for_p_thread_get_cpu_threads(bool leave_room_for_main)
{
  size_t thread_count = (size_t)sysconf(_SC_NPROCESSORS_ONLN);
  if (leave_room_for_main)
  {
    // In case you're running this on your AMD Thunderbird.
    thread_count = thread_count - 1;
    if (thread_count == 0)
    {
      thread_count = 1;
    }
  }
  return thread_count;
}