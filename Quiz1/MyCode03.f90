program Quiz3
    character(len = 25) :: name
    character :: hall
    integer :: std_number, last_2_digits
    logical :: proceed

    proceed = .true.

    print*, "Please enter your full name (maximum 25 leters) using qoutations, and your student"
    print*, "number like the below example"
    print*, '"John Smith", 123456'

    do while (proceed)
        read *, name, std_number
        if (std_number < 222000 .and. std_number > 215000) then
            proceed = .false.
        else
            print *, "Your student number is not valid. Please enter it again."
            print *, "If you are not sure, press control+C on your keyboard and try again later."
        end if
    end do

    last_2_digits = mod(std_number, 100)

    if (last_2_digits <= 33) then
        print*, "Dear ", name, ", your exam will be held in Hall-A"
    else if (last_2_digits <= 66 .and. last_2_digits >= 34) then
        print*, "Dear ", name, ", your exam will be held in Hall-B"
    else
        print*, "Dear ", name, ", your exam will be held in Hall-C"
    end if

end program
