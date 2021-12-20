program Quiz2
    implicit none
    real :: all_nums(36), largest, smallest, dot, sum;
    real, dimension (:), allocatable :: V, U;
    integer :: i, n, positive = 1, negative = 1, po = 0, ne = 0;
    logical :: not_got_n = .true.;

    open(1, file = "Problem3_input.txt");
    do i = 1, 36
        read(1, *) all_nums(i);
    end do

    do while(not_got_n)
        write(*, "(A)") "This program creates two n-element vectors from question3_input.txt.";
        write(*, "(A)") "Enter n (an integer between 6 and 16)";
        read(*, *) n;
        if (n >= 6 .and. n <= 16) then
            not_got_n = .false.;
        end if
    end do

    allocate(V(n));
    allocate(U(n));

    do i = 1, 36
        if (positive <= n .and. all_nums(i) > 0) then
            V(positive) = all_nums(i);
            positive = positive + 1;
        elseif (negative <= n .and. all_nums(i) < 0) then
            U(negative) = all_nums(i);
            negative = negative + 1;
        end if
    end do

    open(2, file = "problem3_output.txt");
    write(2, "(A)") "Vector V:     Vector U:";

    largest = -1.0;
    smallest = 1.0;
    dot = 0;
    sum = 0;
    do i = 1, n
        write(2, "(E8.2, 6x ,E9.2)") V(i), U(i);

        if (V(i) > largest) then
            largest = V(i);
        end if
        if (U(i) < smallest) then
            smallest = U(i);
        end if

        if (V(i) > 4) then
            po = po + 1;
        end if
        if (U(i) < -4) then
            ne = ne + 1;
        end if

        dot = dot + (V(i) * U(i));
        sum = sum + V(i) + U(i);

    end do
    write(2, "(A,1x,E10.3)") "The multiplication of largest element of V and smallest element of U is:", largest * smallest;
    write(2, "(A,I0,A,I0,A)") "V has ", po, " elements greater than 4, and U has ", ne, " elements smaller than -4.";
    write(2, "(A, E10.3)") "The dot product of V and U is ", dot;
    write(2, "(A, E10.3)") "The sum of all the V elements plus the sum of all the U elements is ", sum;

    close(1);
    close(2);
end program
