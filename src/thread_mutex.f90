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


contains

  !* Create a new mutex pointer.
  function thread_create_mutex_pointer() result(new_mutex_pointer)
    implicit none

    type(mutex_rwlock), pointer :: new_mutex_pointer
    integer(c_int) :: status

    allocate(new_mutex_pointer)
    allocate(new_mutex_pointer%raw_data_pointer(for_p_thread_get_pthread_mutex_t_width()))

    status = internal_pthread_rwlock_init(c_loc(new_mutex_pointer), c_null_ptr)
  end function thread_create_mutex_pointer


  !* Destroy a mutex pointer.
  subroutine thread_destroy_mutex_pointer(input_mutex_pointer)
    implicit none

    type(mutex_rwlock), intent(inout), pointer :: input_mutex_pointer
    integer(c_int) :: status

    status = internal_pthread_rwlock_destroy(c_loc(input_mutex_pointer), c_null_ptr)

    deallocate(input_mutex_pointer%raw_data_pointer)
    deallocate(input_mutex_pointer)
  end subroutine thread_destroy_mutex_pointer


end module thread_mutex
