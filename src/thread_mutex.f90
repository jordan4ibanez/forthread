module thread_mutex
  use :: thread_types
  implicit none

  public :: thread_write_lock
  public :: thread_read_lock
  public :: thread_unlock_lock


  interface


    function internal_for_p_thread_create_mutex() result(c_mutex_pointer) bind(c, name = "for_p_thread_create_mutex")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr) :: c_mutex_pointer
    end function internal_for_p_thread_create_mutex


    subroutine internal_for_p_thread_destroy_mutex(rwlock) bind(c, name = "for_p_thread_destroy_mutex")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: rwlock
    end subroutine internal_for_p_thread_destroy_mutex


    function thread_write_lock(rwlock) result(status) bind(c, name = "pthread_rwlock_wrlock")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: rwlock
      integer(c_int) :: status
    end function thread_write_lock


    function thread_read_lock(rwlock) result(status) bind(c, name = "pthread_rwlock_rdlock")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: rwlock
      integer(c_int) :: status
    end function thread_read_lock


    function thread_unlock_lock(rwlock) result(status) bind(c, name = "pthread_rwlock_unlock")
      use, intrinsic :: iso_c_binding
      implicit none

      type(c_ptr), intent(in), value :: rwlock
      integer(c_int) :: status
    end function thread_unlock_lock

  end interface


end module thread_mutex
