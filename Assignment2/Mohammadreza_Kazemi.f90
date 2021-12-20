program Assignment2

    implicit none
    !    Declaring variables
    character(len = 65) :: name
    integer :: a_number, i, sum

    i = 1
    sum = 0

    print *, "This program prints all odd numbers between 0 and your desired integer number (less than 1000) on"
    print *,"the screen and represents the sum of the odd numbers at the end."
    print *, "Please enter your name (maximum 65 letters)"
    read(*,'(A)') name

    print *, "Please enter your desire integers number (your number should be between 0 to 1000)"
    read *, a_number

    print *, "The odd numbers between 0 to ", a_number, " are:"

    do while (i <= a_number .and. i < 1000)
        print *, i
        sum = sum + i
        i = i + 2
    end do

    print *, "The sum of these odd numbers is: ", sum
    print *, "Thank you ", name, " for using this code."

end program Assignment2
