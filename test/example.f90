module a_thread_module
  use, intrinsic :: iso_c_binding
  use :: thread

  type :: thread_data_example
    integer(c_int) :: a_number
  end type thread_data_example


end module a_thread_module

program thread_example
  use, intrinsic :: iso_c_binding
  implicit none

end program thread_example
