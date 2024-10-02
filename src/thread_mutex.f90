module thread_mutex
  use :: thread_types
  implicit none


  interface


    function thread_create_mutex() result(c_mutex) bind(c, name = "for_p_thread_create_mutex")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr) :: c_mutex
    end function thread_create_mutex


    subroutine thread_destroy_mutex(c_mutex) bind(c, name = "for_p_thread_destroy_mutex")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: c_mutex
    end subroutine thread_destroy_mutex


    function thread_lock_mutex(c_mutex) result(status) bind(c, name = "pthread_mutex_lock")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: c_mutex
      integer(c_int) :: status
    end function thread_lock_mutex


    function thread_unlock_mutex(c_mutex) result(status) bind(c, name = "pthread_mutex_unlock")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: c_mutex
      integer(c_int) :: status
    end function thread_unlock_mutex


  end interface


end module thread_mutex
