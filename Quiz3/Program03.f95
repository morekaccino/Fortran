module VectorStat
    implicit none

contains
    subroutine RD(FileName, N)
        !======================================================================!
        ! Description: This subroutine counts the number of lines in the       !
        !   given file name, FileName.                                         !
        ! Arguments:                                                           !
        !   FileName, character: The name of the of file to be counted         !   !
        !   N, integer: The number of lines in the filename provided.          !
        !======================================================================!
        implicit none
        character (len = *), intent(in) :: FileName
        character(len = 100) :: r
        integer, intent(out) :: N
        integer :: stat
        open(10, file = FileName)
        N = 0
        stat = 0
        do while(stat == 0)
            N = N + 1
            read(10, *, iostat = stat)r
        enddo
        N = N - 1
        rewind(10)
    end subroutine RD

    subroutine StatCal(A, M, SD, VR, N)
        !======================================================================!
        ! Subroutine StatCal receives an unknown-size 1D array and calculates  !
        ! the mean, standard deviation, and variance of its first n elements.  !
        ! Arguments:                                                           !
        !   A:  Input, 1D real array.                                          !
        !   M:  output, real, the average of the first n element of the array. !
        !   SD: output, real, the standard deviation of the first n element of !
        !       the array.                                                     !
        !   VR: output, real, the variance of the first n element of the array.!
        !   N:  optional, integer, first n element of the array. Its default   !
        !       value is the full length of the array.                         !
        !======================================================================!
        implicit none
        integer, intent(in), optional :: N
        real, dimension(:), intent(in) :: A
        real, intent(out) :: M, SD, VR
        real :: sum2, sum_
        integer :: i, n_;

        if (present(N)) then
            n_ = N;
        else
            n_ = size(A);
        end if

        M = sum(A(1:n_)) / size(A(1:n_));

        sum2 = 0;
        do i = 1, n_
            sum2 = sum2 + A(i) ** 2;
        end do

        sum_ = sum(A(1:n_));
        VR = (sum2 - sum_ * sum_ / n_) / (n_ - 1);
        SD = sqrt(VR);

    end subroutine StatCal

end module VectorStat

program Program3
    use VectorStat
    implicit none
    real, dimension (:), allocatable :: x
    integer :: i, n, elm
    real :: mean, stdDev, var
    call RD('data.txt', n)
    allocate(x(n))
    do i = 1, n
        read(10, *) x(i)
    enddo
    elm = 53
    call StatCal(x, mean, stdDev, var, elm)
    print*, 'The array has ', n, ' elements.'
    print 100, 'The average of the first ', elm, ' element of the array is ', mean, '.'
    print 100, 'The standard deviation of the first ', elm, ' element of the array is ', stdDev, '.'
    print 100, 'The variance of the first ', elm, ' element of the array is ', var, '.'
    100 format(a, i3, a, es10.3, a)
end program Program3