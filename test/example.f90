!*
!* A very simple example which shows off a pool dumping data into
!* a concurrent FIFO queue.
!*
module an_example_thread_module
  use, intrinsic :: iso_c_binding
  use :: thread
  implicit none


  !* This is our data that will be sent into the thread from main.
  type :: thread_data_in_example
    integer(c_int) :: a_number
    character(len = :, kind = c_char), pointer :: a_string
    !* This one is a bit special, it'll make sense as you read.
    type(concurrent_fifo_queue), pointer :: output
  end type thread_data_in_example


  !* This is our data that will be sent from threads into main.
  type :: thread_data_out_example
    integer(c_int) :: value
  end type thread_data_out_example


contains

  !* You must format your function like this or else it might blow up.
  recursive function thread_worker_thing(c_arg_pointer) result(void_pointer) bind(c)
    implicit none

    !* The thread library will pass in a very specific portion of memory in a simple layout.
    !* It is binding to C, so we must shuffle some data around first.
    type(c_ptr), intent(in), value :: c_arg_pointer

    !* The void pointer is C's way of dynamic types. In this library, it has no use.
    !* Just make it a c_null_ptr basically.
    type(c_ptr) :: void_pointer

    !* This is our baseline to how this is laid out.
    !* We have the thread library's mutex, and the actual data. (if any was sent)
    type(thread_argument), pointer :: argument_pointer

    !* This is the message we will be sending back.
    type(thread_data_out_example), pointer :: output

    !* Status is basically if we want to look at the result of mutex locking.
    !* (You'll see this later.)
    integer(c_int) :: status

    !* This is the data we expect to get in this thread.
    type(thread_data_in_example), pointer :: some_cool_data


    !* I suppose we will get into this now.

    ! Check association. If this is null, something has gone horribly wrong.
    if (.not. c_associated(c_arg_pointer)) then
      error stop "[Thread worker thing] Error: c_arg_pointer was null."
    end if

    ! Transfer the data into Fortran.
    call c_f_pointer(c_arg_pointer, argument_pointer)


    !* Now we have our thread module mutex and the pointer to the data!
    !* Let's grab that data from the c_ptr.

    ! We really need that data, it cannot be null.
    if (.not. c_associated(argument_pointer%data)) then
      error stop "[Thread worker thing] Error: The sent data was null!"
    end if

    call c_f_pointer(argument_pointer%data, some_cool_data)


    !* And now we have it. 8)
    !* So let's print it out.

    print*,"hello from thread", some_cool_data%a_number, "the string is: "//some_cool_data%a_string

    !* The string is a pointer, free it!
    deallocate(some_cool_data%a_string)


    !* Well, we also got sent in that pointer, let's output some data to it.
    !? Also, very important note:
    !* This is the concurrent FILO queue. You must ensure that your data you push into it
    !* is a pointer or else it will be extremely undefined behavior.

    ! allocate(output)
    ! output%value = some_cool_data%a_number
    ! call some_cool_data%output%push(output)


    !* You must remember: We are working in manual memory management.
    !* In this specific scenario, all we have to do is free the cool data.
    !* But it might get much more complex depending on what you're doing.

    deallocate(some_cool_data)


    !* Remember that void pointer?

    void_pointer = c_null_ptr


    !* Now, for the finale.
    !* This part is extremely important, do it in this order.
    !* You will be talking straight to the library during this.

    !* Lock the master mutex.
    status = thread_write_lock(argument_pointer%mutex_ptr)

    !* This is a pointer into the thread library to say: "this thread finished"
    argument_pointer%active_flag = .false.

    !* Finally, unlock the mutex.
    status = thread_unlock_lock(argument_pointer%mutex_ptr)

    !* This thread has completed. :)
  end function thread_worker_thing


end module an_example_thread_module

program thread_example
  use, intrinsic :: iso_c_binding
  use :: an_example_thread_module
  use :: thread
  implicit none

  integer(c_int) :: i
  !* Remember to make this a pointer!
  !* If you make it allocatable, I literally have no idea what the
  !* Fortran runtime will do to it.
  type(thread_data_in_example), pointer :: sending_data

  !* We will create a concurrent FIFO queue for the threads to output to.
  type(concurrent_fifo_queue), target :: output_queue

  !* You'll see this used later. 8)
  type(c_ptr) :: raw_c_ptr
  type(thread_data_out_example), pointer :: pointer_data


  !* The first step is very simple, you must initialize the library.
  !* [leave_room_for_main], if .true. will have [CPU_THREADS - 1] workers.
  !* This is specifically designed for games where you don't want the main thread
  !* to become overburdened in the OS scheduler.
  !* Since we are using pure calculations, use all available threads.
  call thread_initialize(.false.)

  !* Initialize the output queue.
  output_queue = new_concurrent_fifo_queue(sizeof(thread_data_out_example(0)))

  !* We'll make this an infinite loop because that's cool.
  do

    !* Don't have 1024 cpu cores? No problem! That's why we have RAM. 8)
    !! An extremely important note:
    !* If you create too many threads in one shot, the Linux arena heap will try to hold onto memory.
    do i = 1,1024

      !* Reallocate the pointer every loop.
      allocate(sending_data)
      sending_data%a_number = i
      allocate(character(len = 3) :: sending_data%a_string)
      sending_data%a_string = "hi!"
      ! Yes, this is quite pointy.
      sending_data%output => output_queue

      !* Remember, we are binding to a C library. We must abide by C's rules.
      call thread_create(thread_worker_thing, c_loc(sending_data))
    end do

    !* I've included a method for you to wait for a huge job to finish. :)


    ! Churn through the queue.
    do while(.not. thread_queue_is_empty())
      call thread_process_thread_queue()
    end do


    ! Spin while we await all the threads to finish.
    ! do while (thread_await_all_thread_completion())
    ! end do


    !* Let's grab that data that the threads output!
    ! do while(output_queue%pop(raw_c_ptr))
    !   call c_f_pointer(raw_c_ptr, pointer_data)

    !   print*,pointer_data%value

    !   ! Don't forget to deallocate. 8)
    !   deallocate(pointer_data)
    ! end do

  end do


end program thread_example
