program Problem02
    implicit none
    integer :: i
    real, dimension(15, 2) :: Table
    real :: x_value
    ! The problem table:
    Table(:, 1) = (/65.0, 70.0, 75.0, 80.0, 85.0, 160.0, 170.0, 180.0, 190.0, 200.0, 120.0, 130.0, 140.0, 150.0, 220.0/)
    Table(:, 2) = (/0.025, 0.031, 0.038, 0.047, 0.058, 0.618, 0.792, 1.002, 1.255, 1.55, 0.20, 0.27, 0.36, 0.476, 2.32/)
    ! Interpolation task.
    print 100, 'Please input a real value for X between ', minval(Table(:, 1)), ' and ', maxval(Table(:, 1)), '.'
    read(*, *) x_value
    print'(a,es8.2,a)', 'The estimated Y value for your entry is ', LnIntPl(Table, x_value), '.'
    100 format (a, f4.1, a, f5.1, a)

contains

    function LnIntPl(XY, X) result(Y)
        !---------------------------------------------------------------------------!
        ! This function performs linear interpolation from a two-column table.      !
        !                                                                           !
        ! Input Arguments:                                                          !
        ! XY: an n-by-2 (two-column) real array, assuming the first column is X     !
        !     data, and the second column is Y data.                                !
        ! X:  must be a real value that we want to find a corresponding Y value for.!
        !                                                                           !
        ! Output Result: the interpolated value (real) we want to find as Y.        !
        !                                                                           !
        ! Note: when X is out of the XY range, the function returns NaN.            !
        !---------------------------------------------------------------------------!
        real, dimension(:, :), intent(in) :: XY
        real, intent(in) :: X
        real :: Y, x_smaller, x_greater, y_smaller, y_greater, index_greater, index_smaller
        integer, dimension(1) :: index


        !        index_smaller = maxloc(XY(:, 1), mask=XY(:, 1) .lt. X);
        x_smaller = maxval(XY(:, 1), mask = XY(:, 1).lt.X);
        x_greater = minval(XY(:, 1), mask = XY(:, 1).gt.X);
        index = findloc(XY(:, 1), x_smaller);
        y_smaller = XY(index(1), 2);
        index = findloc(XY(:, 1), x_greater);
        y_greater = XY(index(1), 2);

        if (X < minval(XY(:, 1)) .or. X > maxval(XY(:, 1))) then
            Y = -1.0;
            Y = sqrt(Y);
        else
            y = y_smaller + (((X - x_smaller) / (x_greater - x_smaller)) * (y_greater - y_smaller));
        end if

    end function

end program Problem02

