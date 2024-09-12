module internal_temp_string
  implicit none

  !* Note: Since this is no-std,
  !* I have to make this into a module or else
  !* error stop will not view the results as a string.

contains


  ! Convert an integer into an allocated string.
  function int_to_string(i) result(output)
    use, intrinsic :: iso_c_binding
    implicit none

    integer(c_int) :: i
    character(len = :, kind = c_char), allocatable :: output

    ! If the number is any bigger than this, wat.
    allocate(character(11) :: output)
    write(output, "(i11)") i

    ! Now we shift the whole thing left and trim it to fit.
    output = trim(adjustl(output))
  end function int_to_string


end module internal_temp_string
