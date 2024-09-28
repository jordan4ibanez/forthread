module thread_types
  use, intrinsic :: iso_c_binding
  implicit none


  ! Raw thread struct.
  ! https://ffmpeg.org/doxygen/3.1/os2threads_8h_source.html
  !! THIS THING IS A MESS! REMOVE IT!
  type, bind(c) :: pthread_t
    integer(c_int64_t) :: tid = 0_8
    type(c_funptr) :: start_routine
    type(c_ptr) :: arg
    type(c_ptr) :: result
  end type pthread_t


  ! A raw thread queue element.
  type :: thread_queue_element
    type(c_funptr) :: function_ptr = c_null_funptr
    type(c_ptr) :: data_to_send_c_ptr = c_null_ptr
  end type thread_queue_element


  ! What gets passed into the thread.
  type :: thread_argument
    logical(c_bool), pointer :: active_flag => null()
    type(c_ptr) :: data = c_null_ptr
    type(c_ptr) :: mutex_pointer = c_null_ptr
  end type thread_argument


!* for_p_thread.

  interface


    function for_p_thread_get_cpu_threads(leave_room_for_main) result(thread_count) bind(c, name = "for_p_thread_get_cpu_threads")
      use, intrinsic :: iso_c_binding
      implicit none

      logical(c_bool), intent(in), value :: leave_room_for_main
      integer(c_int) :: thread_count
    end function for_p_thread_get_cpu_threads


    function for_p_thread_get_pthread_mutex_t_width() result(data_width) bind(c, name = "for_p_thread_get_pthread_mutex_t_width")
      use, intrinsic :: iso_c_binding
      implicit none

      integer(c_int) :: data_width
    end function for_p_thread_get_pthread_mutex_t_width


  end interface


end module thread_types
