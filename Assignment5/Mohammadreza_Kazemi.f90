program Assignment5
    implicit none
    integer :: dim, index;
    real :: start, stop, step, temp, c, f, k;
    real, dimension (:, :), allocatable :: table;
    logical :: failed;

    failed = .true.;

    do while (failed)
        print *, "enter the starting and ending temperatures of your table (separate with a comma)";
        read *, start, stop;
        if (start <= 210 .and. start >= -10 .and. stop <= 210 .and. stop >= -10) then
            failed = .false.;
        end if
    end do
    failed = .true.;

    if (start > stop) then
        temp = start;
        start = stop;
        stop = temp;
    end if

    do while (failed)
        print *, "enter the increment of your table, a value between 1 and 185.000000";
        read *, step;
        if (step >= 1 .and. step <= 185 .and. (step < abs(stop - start))) then
            failed = .false.
        end if
    end do

    dim = 1;
    temp = start;
    do while (temp < stop)
        temp = temp + step;
        dim = dim + 1;
    end do
    allocate(table(dim, 3));

    print *, "Centigrade         Fahrenheit        Kelvin";

    do index = 1, dim-1
        c = start + ((index - 1) * step);
        f = 32 + (1.8 * c);
        k = c + 273.15;
        table(index, 1) = c;
        table(index, 2) = f;
        table(index, 3) = k;
    end do
    c = stop;
    f = 32 + (1.8 * c);
    k = c + 273.15;
    table(dim, 1) = c;
    table(dim, 2) = f;
    table(dim, 3) = k;

    do index = 1, dim
        print *, table(index, 1), table(index, 2), table(index, 3);
    end do

end program Assignment5
