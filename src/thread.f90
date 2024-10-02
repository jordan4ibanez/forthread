

module thread
  use :: thread_types
  use :: thread_mutex
  use :: thread_bindings
  use :: thread_fifo_queue_linked
  use, intrinsic :: iso_c_binding
  implicit none


  ! https://www.cs.cmu.edu/afs/cs/academic/class/15492-f07/www/pthreads.html
  ! https://hpc-tutorials.llnl.gov/posix/what_is_a_thread/
  ! https://docs.oracle.com/cd/E19455-01/806-5257/6je9h032u/index.html
  ! https://pubs.opengroup.org/onlinepubs/007908799/xsh/pthread_rwlock_unlock.html
  ! Also: All of the Linux man pages LOL.
  !
  !* Implementation note:
  !* This has been HEAVILY modified to be easy to work with in Fortran.
  !
  ! Detached thread runthrough:
  !
  ! 1.) thread_create_detached()
  ! Goes into queue.
  !
  ! 2.) thread_process_detached_thread_queue()
  ! Calls pop_thread_queue()
  !
  ! 3.) thread_process_detached_thread()
  ! Will delete old thread attributes.
  ! Passes required data pointers into slots.


  private

  public :: new_concurrent_fifo_queue
  public :: concurrent_fifo_queue

  public :: thread_argument
  public :: thread_queue_element
  public :: concurrent_fifo_queue

  public :: thread_create_mutex
  public :: thread_destroy_mutex
  public :: thread_lock_mutex
  public :: thread_unlock_mutex

  public :: thread_initialize
  public :: thread_destroy
  public :: thread_create_mutex
  public :: thread_destroy_mutex
  public :: thread_create
  public :: thread_process_thread_queue
  public :: thread_queue_is_empty
  public :: thread_await_all_thread_completion

  integer(c_int), parameter :: THREAD_OK = 0
  integer(c_int), parameter :: THREAD_DOES_NOT_EXIST = 3

  integer(c_int) :: CPU_THREADS = 0

  type(c_ptr) :: module_mutex

  type(thread_argument), dimension(:), pointer :: thread_arguments
  logical(c_bool), dimension(:), pointer :: thread_active

  type(concurrent_fifo_queue) :: master_thread_queue


contains


  !* Fire up the module.
  !* If [leave_room_for_main] is .true. it will allocate [CPU_THREADS - 1] worker threads
  !* into the thread pool state machine.
  subroutine thread_initialize(leave_room_for_main)
    implicit none

    logical, intent(in), value :: leave_room_for_main
    integer(c_int) :: i

    CPU_THREADS = thread_get_cpu_thread_count(logical(leave_room_for_main, kind = c_bool))

    module_mutex = thread_create_mutex()

    allocate(thread_arguments(CPU_THREADS))
    allocate(thread_active(CPU_THREADS))

    ! Very important: Initialize memory.

    do i = 1,CPU_THREADS
      thread_active(i) = .false.
    end do

    master_thread_queue = new_concurrent_fifo_queue(sizeof(thread_queue_element()))
  end subroutine thread_initialize


  !* Clean up the module.
  subroutine thread_destroy()
    implicit none

    do while (thread_await_all_thread_completion())
    end do

    deallocate(thread_arguments)
    deallocate(thread_active)

    call thread_destroy_mutex(module_mutex)
  end subroutine thread_destroy


  !* Create a new joinable thread.
  function thread_create_joinable(subroutine_c_funptr, argument_ptr) result(tid) bind(c)
    use :: internal_temp_string
    implicit none

    type(c_funptr), intent(in), value :: subroutine_c_funptr
    type(c_ptr), intent(in), value :: argument_ptr
    integer(c_int64_t) :: tid
    integer(c_int) :: status

    status = pthread_create(tid, c_null_ptr, subroutine_c_funptr, argument_ptr)

    if (status /= 0) then
      error stop "[Thread] Error: Failed to create a thread. Error status: ["//int_to_string(status)//"]"
    end if
  end function thread_create_joinable


  !* Join a thread back into the main thread.
  subroutine thread_join(tid, return_val_c_ptr) bind(c)
    use :: internal_temp_string
    implicit none

    integer(c_int64_t), intent(in), value :: tid
    type(c_ptr), intent(in), value :: return_val_c_ptr
    integer(c_int) :: status

    status = pthread_join(tid, return_val_c_ptr)

    if (status /= THREAD_OK) then
      error stop "[Forthread] Error: Failed to join thread. Error status: ["//int_to_string(status)//"]"
    end if
  end subroutine thread_join


  !* Create a detached thread.
  function create_detached(subroutine_c_funptr, argument_ptr) result(tid) bind(c)
    use :: internal_temp_string
    implicit none

    type(c_funptr), intent(in), value :: subroutine_c_funptr
    type(c_ptr), intent(in), value :: argument_ptr
    integer(c_int64_t) :: tid
    integer(c_int) :: status

    status = pthread_create(tid, c_null_ptr, subroutine_c_funptr, argument_ptr)

    if (status /= 0) then
      error stop "[Thread] Error: Failed to create a thread. Error status: ["//int_to_string(status)//"]"
    end if

    status = pthread_detach(tid)

    if (status /= 0) then
      error stop "[Thread] Error: Failed to detach a thread. Error status: ["//int_to_string(status)//"]"
    end if
  end function create_detached


  !* Queue up a thread to be run.
  subroutine thread_create(subroutine_to_use, argument_c_ptr)
    implicit none

    procedure(thread_function_c_interface) :: subroutine_to_use
    type(c_ptr), intent(in), value :: argument_c_ptr
    type(thread_queue_element) :: new_element

    new_element%function_ptr = c_funloc(subroutine_to_use)
    new_element%data_to_send_c_ptr = argument_c_ptr

    call master_thread_queue%push(new_element)
  end subroutine thread_create


  !* Process all the queued threads limited by cpu threads available.
  subroutine thread_process_thread_queue()
    implicit none

    integer(c_size_t) :: queue_size, i, discard
    integer(c_int) :: thread_to_use, status
    type(c_ptr) :: raw_c_ptr
    type(thread_queue_element), pointer :: new_element
    type(c_funptr) :: function_ptr

    if (master_thread_queue%is_empty()) then
      return
    end if

    queue_size = master_thread_queue%count()

    ! Don't attempt to go past available threads.
    if (queue_size > CPU_THREADS) then
      queue_size = CPU_THREADS
    end if

    do i = 1,queue_size

      thread_to_use = find_free_thread()

      ! If there's no available threads, stop.
      if (thread_to_use == 0) then
        ! print*,"FAILURE! iter", i
        exit
      end if

      if (master_thread_queue%pop(raw_c_ptr)) then

        call c_f_pointer(raw_c_ptr, new_element)

        ! Set the completion flag.
        status = thread_lock_mutex(module_mutex)

        thread_active(thread_to_use) = .true.

        thread_arguments(thread_to_use)%mutex_ptr = module_mutex

        status = thread_unlock_mutex(module_mutex)

        ! Set the raw data to send.
        thread_arguments(thread_to_use)%active_flag => thread_active(thread_to_use)
        thread_arguments(thread_to_use)%data = new_element%data_to_send_c_ptr

        function_ptr = new_element%function_ptr

        ! Now clean up the shell.
        deallocate(new_element)

        ! Fire off the thread.
        discard = create_detached(function_ptr, c_loc(thread_arguments(thread_to_use)))
      else
        ! Nothing left to get.
        exit
      end if
    end do
  end subroutine thread_process_thread_queue


  !* Simply searches for a free thread to dispatch.
  !* This is a very naive implementation.
  function find_free_thread() result(thread_index)
    implicit none

    integer(c_int) :: thread_index, i, discard

    thread_index = 0

    discard = thread_lock_mutex(module_mutex)

    do i = 1,CPU_THREADS
      if (.not. thread_active(i)) then
        thread_index = i
        exit
      end if
    end do

    discard = thread_unlock_mutex(module_mutex)
  end function find_free_thread


  !* Check if the thread queue is empty.
  !* This is primarily used for debugging.
  function thread_queue_is_empty() result(is_empty)
    implicit none

    logical(c_bool) :: is_empty

    is_empty = master_thread_queue%is_empty()
  end function thread_queue_is_empty


  !* And the end of the program, wait for all threads to complete until continuing.
  function thread_await_all_thread_completion() result(keep_going)
    implicit none

    logical(c_bool) :: keep_going
    integer(c_int) :: i, status

    keep_going = .true.

    status = thread_lock_mutex(module_mutex)

    do i = 1,CPU_THREADS
      if (thread_active(i)) then
        status = thread_unlock_mutex(module_mutex)
        return
      end if
    end do

    status = thread_unlock_mutex(module_mutex)

    keep_going = .false.
  end function thread_await_all_thread_completion


end module thread
