program Quiz2
    implicit none
    integer :: n, i, j, temp
    integer, dimension(3, 2) :: m

    print*, "Enter n (an integer between 0 to 6)"
    read*, n
    if (n >= 6 .or. n <=0) then
        stop
    end if

    print*, "The below 3x2 matrix is created based on your entry"

    do i = 1, 3
        do j = 1, 2
            temp = ((2 * i) + j) ** n
            if (temp > 2500) then
                temp = 2500
            end if
            m(i, j) = temp
            print*, "i=", i, " j=", j, "Mij=", m(i, j)
        end do
    end do

end program
