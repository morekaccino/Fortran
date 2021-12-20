program Assignment1

    ! Declaring the variables
    implicit none
    real::pi, area
    integer::XX
    character(len=19)::me
    character(len=9)::student_number

    ! Assigning values
    pi = 3.1415
    me = 'Mohammadreza Kazemi'
    student_number = '215888985'
    XX = 85

    ! Calculations
    area = pi * XX * XX

    ! Printing the result
    print *, 'Hello, My name is ', me, ' and my student number is ', student_number ,'.'
    print *, 'The area of a ', student_number(8:9) ,'m-radius circle is:', area
end program Assignment1
