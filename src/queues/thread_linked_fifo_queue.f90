module thread_fifo_queue_linked
  use :: thread_mutex
  use :: fifo_queue
  use, intrinsic :: iso_c_binding
  implicit none


  private


  public :: concurrent_fifo_queue


  !* An concurrent shell for fifo_queue.
  type :: concurrent_fifo_queue
    private
    type(fifo) :: i_queue
    type(c_ptr) :: mutex_pointer = c_null_ptr
  contains
    procedure :: push => concurrent_fifo_queue_push
    procedure :: pop => concurrent_fifo_queue_pop
    procedure :: destroy => concurrent_fifo_queue_destroy
    procedure :: is_empty => concurrent_fifo_queue_is_empty
    procedure :: size => concurrent_fifo_queue_get_size
  end type concurrent_fifo_queue


contains


  function new_concurrent_fifo_queue(data_size) result(new_queue)
    implicit none

    integer(c_size_t), intent(in), value :: data_size
    type(concurrent_fifo_queue) :: new_queue

    new_queue%i_queue = new_fifo_queue(data_size)
    new_queue%mutex_pointer = internal_for_p_thread_create_mutex()
  end function new_concurrent_fifo_queue


  !* Push an element into the end of a queue.
  subroutine concurrent_fifo_queue_push(this, generic_pointer)
    implicit none

    class(concurrent_fifo_queue), intent(inout) :: this
    class(*), intent(in), target :: generic_pointer
    integer(c_int) :: discard

    discard = thread_write_lock(this%mutex_pointer)
    !! BEGIN SAFE OPERATION.




    !! END SAFE OPERATION.
    discard = thread_unlock_lock(this%mutex_pointer)
  end subroutine concurrent_fifo_queue_push


  !* Pop the first element off the queue.
  function concurrent_fifo_queue_pop(this, generic_pointer_option) result(some)
    implicit none

    class(concurrent_fifo_queue), intent(inout) :: this
    class(*), intent(inout), pointer :: generic_pointer_option
    logical(c_bool) :: some
    integer(c_int) :: discard

    discard = thread_write_lock(this%mutex_pointer)
    !! BEGIN SAFE OPERATION.


    !! END SAFE OPERATION.
    discard = thread_unlock_lock(this%mutex_pointer)
  end function concurrent_fifo_queue_pop


  !* Destroy all data in a queue.
  !! This will not destroy the mutex. You are still required to do that.
  subroutine concurrent_fifo_queue_destroy(this)
    implicit none

    class(concurrent_fifo_queue), intent(inout) :: this
    integer(c_int) :: discard

    discard = thread_write_lock(this%mutex_pointer)
    !! BEGIN SAFE OPERATION.


    !! END SAFE OPERATION.
    discard = thread_unlock_lock(this%mutex_pointer)
  end subroutine concurrent_fifo_queue_destroy


  !* Check if the queue is empty.
  function concurrent_fifo_queue_is_empty(this) result(empty)
    implicit none

    class(concurrent_fifo_queue), intent(inout) :: this
    logical(c_bool) :: empty
    integer(c_int) :: discard

    discard = thread_write_lock(this%mutex_pointer)
    !! BEGIN SAFE OPERATION.



    !! END SAFE OPERATION.
    discard = thread_unlock_lock(this%mutex_pointer)
  end function concurrent_fifo_queue_is_empty


  !* Check number of items in the queue.
  function concurrent_fifo_queue_get_size(this) result(item_count)
    implicit none

    class(concurrent_fifo_queue), intent(inout) :: this
    integer(c_int) :: item_count
    integer(c_int) :: discard

    discard = thread_write_lock(this%mutex_pointer)
    !! BEGIN SAFE OPERATION.



    !! END SAFE OPERATION.
    discard = thread_unlock_lock(this%mutex_pointer)
  end function concurrent_fifo_queue_get_size


end module thread_fifo_queue_linked
