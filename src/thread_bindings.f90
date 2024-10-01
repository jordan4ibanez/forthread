module thread_bindings
  use, intrinsic :: iso_c_binding
  implicit none

  interface

    function internal_for_p_thread_create_thread(tid, start_routine, arg) result(status) bind(c, name = "for_p_thread_create_thread")
      use, intrinsic :: iso_c_binding
      implicit none

      !* Keep in mind: this type is simply a size_t (8 bytes) in POSIX.
      !* Restricting to 64 bit systems.
      integer(c_int64_t), intent(inout) :: tid
      type(c_funptr), intent(in), value :: start_routine
      type(c_ptr), intent(in), value :: arg
      integer(c_int) :: status
    end function internal_for_p_thread_create_thread


    function internal_pthread_join(tid, retval) result(status) bind(c, name = "pthread_join")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int64_t), intent(in), value :: tid
      type(c_ptr), intent(in), value :: retval
      integer(c_int) :: status
    end function internal_pthread_join


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
