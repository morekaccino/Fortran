program Assignment6
    implicit none
    integer :: i, year, month, day, hour, min, sec;
    character(len = 128) :: s, fmt;
    character(len = 11) :: make
    character(len = 9) :: color
    character(len = 8) :: plate
    logical :: is_before_2_15

    open(2, file = "output_plates.txt", status = "new", ACCESS = 'Sequential', form = 'formatted');
    write(2, '(A)') "The below-listed license plate(s) should be contacted:";
    write(2, '(A, 8X, A, 2X, A)') "Plate", "hr", "min";
    open(1, file = "logfile.txt", status = "old", ACCESS = 'Sequential', form = 'formatted');
    read(1, '(A)') s;
    read(1, '(A)') s;

    write(*, '(A)') "The below-listed license plate(s) should be contacted:";
    write(*, '(A, 8X, A, 2X, A)') "Plate", "hr", "min";

    fmt = '(A10, A9, A9, i6, i2, i3, i3, i3, i3)';
    do i = 1, 2520
        read(1, fmt) make, color, plate, year, month, day, hour, min, sec;

        if (hour < 14) then
            is_before_2_15 = .true.;
        elseif (hour == 14 .and. min < 15) then
            is_before_2_15 = .true.;
        elseif (hour == 14 .and. min == 15 .and. sec == 0) then
            is_before_2_15 = .true.;
        else
            is_before_2_15 = .false.;
        end if

        make = adjustl(make);
        color = adjustl(color);

        if (make == "Nissan" .and. color == "White" .and. is_before_2_15) then
            write(2, '(A, 5X, i2.2, 2X, i2.2)') plate, hour, min;
            write(*, '(A, 5X, i2.2, 2X, i2.2)') plate, hour, min;
        end if
    end do
    close(1);
    close(2);
end program
