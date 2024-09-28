module thread_types
  use, intrinsic :: iso_c_binding
  implicit none


  !* A raw thread queue element.
  type :: thread_queue_element
    type(c_funptr) :: function_ptr = c_null_funptr
    type(c_ptr) :: data_to_send_c_ptr = c_null_ptr
  end type thread_queue_element


  !* What gets passed into the thread.
  type :: thread_argument
    logical(c_bool), pointer :: active_flag => null()
    type(c_ptr) :: data = c_null_ptr
    type(c_ptr) :: mutex_ptr = c_null_ptr
  end type thread_argument


end module thread_types
