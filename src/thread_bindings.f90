module thread_bindings
  use, intrinsic :: iso_c_binding
  implicit none

  interface

    !* The following tid/status docs are reflected upon every pthread binding.

    !* Create a thread.
    function pthread_create(tid, attr, start_routine, arg) result(status) bind(c, name = "pthread_create")
      use, intrinsic :: iso_c_binding
      implicit none

      !* pthread_t is of width 8, uint64_t.
      !* We are utilizing the fact that the memory layout of the assigned
      !* TID will not change over the lifetime of the thread.
      integer(c_int64_t), intent(inout) :: tid
      type(c_ptr), intent(in), value :: attr, arg
      type(c_funptr), intent(in), value :: start_routine
      !* If the status is anything but 0, the thread creation failed.
      integer(c_int) :: status
    end function pthread_create


    !* Detach a thread. (Auto clean up pthread [only] C memory upon completion.)
    function pthread_detach(tid) result(status) bind(c, name = "pthread_detach")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int64_t), intent(inout) :: tid
      integer(c_int) :: status
    end function pthread_detach


    !* Join a thread. (Only needed if thread_detach() is not run.)
    function pthread_join(tid, retval) result(status) bind(c, name = "pthread_join")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int64_t), intent(in), value :: tid
      type(c_ptr), intent(in), value :: retval
      integer(c_int) :: status
    end function pthread_join


    !* Get the number of CPU threads available on the system.
    !* If [leave_room_for_main] is .true., this will give you [X - 1] threads.
    !* (To leave room for the main thread in the OS scheduler.)
    function for_p_thread_get_cpu_threads(leave_room_for_main) result(thread_count) bind(c, name = "for_p_thread_get_cpu_threads")
      use, intrinsic :: iso_c_binding
      implicit none

      logical(c_bool), intent(in), value :: leave_room_for_main
      integer(c_int) :: thread_count
    end function for_p_thread_get_cpu_threads


    !* BEGIN FUNCTION BLUEPRINTS.


    recursive function thread_function_c_interface(raw_c_arg_ptr) result(void_ptr) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: raw_c_arg_ptr
      type(c_ptr) :: void_ptr
    end function thread_function_c_interface


    subroutine thread_garbage_collector_c_interface(old_data_c_ptr) bind(c)
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: old_data_c_ptr
    end subroutine thread_garbage_collector_c_interface

  end interface

end module thread_bindings
