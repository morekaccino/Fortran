Program Problem2
    implicit none
    integer :: n;
    Print*, 'Enter an integer number >1'
    read*, n
    print '(a,i3,a,i4)', 'hanoi number of ', n, ' is:', TOH(n)

contains
    recursive integer function TOH (n) result (hanoi)
        implicit none
        integer :: n;

        if (n == 1) then
            hanoi = 1;
        else
            hanoi = (2 * TOH(n - 1)) + 1;
        end if

    end function TOH
end program Problem2
