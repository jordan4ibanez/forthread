module thread_filo_queue_array
  use :: thread_types
  use :: thread_mutex
  use, intrinsic :: iso_c_binding
  implicit none


  private


  type :: concurrent_array_filo_queue
    private
    class(*), dimension(:), allocatable :: data
    type(mutex_rwlock), pointer :: mutex => null()
    type(c_ptr) :: c_mutex_pointer = c_null_ptr
    integer(c_int) :: items = 0
  contains
    procedure :: push => concurrent_array_filo_queue_push
    procedure :: pop => concurrent_array_filo_queue_pop
    procedure :: destroy => concurrent_array_filo_queue_destroy
    procedure :: is_empty => concurrent_array_filo_queue_is_empty
    procedure :: get_size => concurrent_array_filo_queue_get_size
  end type concurrent_array_filo_queue

  interface concurrent_array_filo_queue
    module procedure :: constructor_concurrent_array_filo_queue
  end interface concurrent_array_filo_queue


contains


  function constructor_concurrent_array_filo_queue() result(new_queue)
    implicit none

    type(concurrent_array_filo_queue) :: new_queue

    new_queue%mutex => thread_create_mutex_pointer()
    new_queue%c_mutex_pointer = c_loc(new_queue%mutex)
  end function constructor_concurrent_array_filo_queue


  !* Push an element into the end of a queue.
  subroutine concurrent_array_filo_queue_push(this, generic_pointer)
    implicit none

    class(concurrent_array_filo_queue), intent(inout) :: this
    class(*), intent(in), target :: generic_pointer
    integer(c_int) :: status
    class(*), dimension(:), allocatable :: new_data

    status = thread_write_lock(this%c_mutex_pointer)
    !! BEGIN SAFE OPERATION.

    allocate(new_data(23))

    this%items = this%items + 1



    !! END SAFE OPERATION.
    status = thread_unlock_lock(this%c_mutex_pointer)
  end subroutine concurrent_array_filo_queue_push


  !* Pop the first element off the queue.
  function concurrent_array_filo_queue_pop(this, generic_pointer_option) result(some)
    implicit none

    class(concurrent_array_filo_queue), intent(inout) :: this
    class(*), intent(inout), pointer :: generic_pointer_option
    logical(c_bool) :: some
    integer(c_int) :: status
    type(queue_node), pointer :: next_pointer

    status = thread_write_lock(this%c_mutex_pointer)
    !! BEGIN SAFE OPERATION.

    some = .false.

    generic_pointer_option => null()

    ! If we have a head, the output will become the head data.
    ! The head will now be shifted forward, and the old head will be cleaned up.
    if (associated(this%head)) then

      some = .true.

      next_pointer => this%head%next

      ! First we unshell the data.
      generic_pointer_option => this%head%data

      ! Then we deallocate.
      deallocate(this%head)

      this%head => next_pointer

      this%items = this%items - 1
    end if

    !* If the head was pointed to null, we must nullify the tail.
    if (.not. associated(this%head)) then
      this%tail => null()
    end if

    !! END SAFE OPERATION.
    status = thread_unlock_lock(this%c_mutex_pointer)
  end function concurrent_array_filo_queue_pop


  !* Destroy all data in a queue.
  !! This will not destroy the mutex. You are still required to do that.
  subroutine concurrent_array_filo_queue_destroy(this)
    implicit none

    class(concurrent_array_filo_queue), intent(inout) :: this
    type(queue_node), pointer :: current, next
    integer(c_int) :: status

    status = thread_write_lock(this%c_mutex_pointer)
    !! BEGIN SAFE OPERATION.

    if (associated(this%head)) then

      current => this%head

      do
        next => current%next

        deallocate(current)

        ! Pointing at nothing.
        if (.not. associated(next)) then
          exit
        end if

        current => next
      end do

    end if

    this%head => null()
    this%tail => null()

    this%items = 0

    !! END SAFE OPERATION.
    status = thread_unlock_lock(this%c_mutex_pointer)
  end subroutine concurrent_array_filo_queue_destroy


  !* Check if the queue is empty.
  function concurrent_array_filo_queue_is_empty(this) result(empty)
    implicit none

    class(concurrent_array_filo_queue), intent(inout) :: this
    logical(c_bool) :: empty
    integer(c_int) :: status

    status = thread_write_lock(this%c_mutex_pointer)
    !! BEGIN SAFE OPERATION.

    empty = this%items == 0

    !! END SAFE OPERATION.
    status = thread_unlock_lock(this%c_mutex_pointer)
  end function concurrent_array_filo_queue_is_empty


  !* Check number of items in the queue.
  function concurrent_array_filo_queue_get_size(this) result(item_count)
    implicit none

    class(concurrent_array_filo_queue), intent(inout) :: this
    integer(c_int) :: item_count
    integer(c_int) :: status

    status = thread_write_lock(this%c_mutex_pointer)
    !! BEGIN SAFE OPERATION.

    item_count = this%items

    !! END SAFE OPERATION.
    status = thread_unlock_lock(this%c_mutex_pointer)
  end function concurrent_array_filo_queue_get_size

end module thread_filo_queue_array
