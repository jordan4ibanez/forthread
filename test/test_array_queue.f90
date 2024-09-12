program test_array_queue
  use, intrinsic :: iso_c_binding
  use :: internal_temp_string
  use :: thread_filo_queue_array
  implicit none

  type :: hi
    character(len = :, kind = c_char), allocatable :: data
  end type hi

  type(concurrent_array_filo_queue) :: test
  type(hi), pointer :: cool
  integer(c_int) :: i
  class(*), pointer :: generic_pointer

  test = concurrent_array_filo_queue()

  do
    do i = 1,10000
      print*,i
      allocate(cool)
      cool%data = "hi"//int_to_string(i)
      call test%push(cool)
    end do

    do while(test%pop(generic_pointer))
      select type(generic_pointer)
       type is(hi)
        print*, generic_pointer%data
        deallocate(generic_pointer)
       class default
        error stop
      end select
    end do
  end do

  call test%destroy()

  call sleep(100)

end program test_array_queue
