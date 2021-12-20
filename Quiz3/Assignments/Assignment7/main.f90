program Assignment7
    implicit none
    integer :: n = -1, i;
    integer, dimension (:, :), allocatable :: matrix
    character(len = 200) :: file_name;
    logical :: singular = .false.;

    write(*, '(a)') "This program receives a n-square integer matrix and swaps rows whose pivots (diagonal elements) are zero.";

    do while (n > 20 .or. n < 0)
        write(*, '(a)') " Enter n (an integer between 0 and 20)";
        read(*, *) n;
    end do

    allocate(matrix(n, n));
    file_name = "New_" // trim(num2str(n)) // "_by_" // trim(num2str(n)) // "_Matrix.txt";
    open(1, file = file_name, status = 'new', ACCESS = 'Sequential', form = 'formatted');
    write(1, "(A)") "The entered matrix is:";

    do i = 1, n
        write(*, '(A,I0,A)') " Enter the elements of the row# ", i, ", comma or space separated.";
        write(*, '(A,I0,A,I0,A)') " Note: if you enter more than ", n, " elements, only the first ", n, " elements will be taken.";
        write(*, '(A)') " Note: you should eneter integer values only. Otherwise execution will be stopped.";
        read(*, *) matrix(i, :);

        write(1, *) matrix(i, :);
    end do

    ! do determinant calculations
    call bareiss(matrix, n, singular);

    write(1, "(A)") "To calculate the entered matrix determinant by LU or Bareiss method, use the following matrix:";
    do i = 1, n
        write(1, *) matrix(i, :);
    end do

    close(1);

contains
    subroutine bareiss(matrix, n, singular)
        implicit none
        integer, intent(in) :: n;
        logical, intent(inout) :: singular;
        integer, dimension (:, :), allocatable, intent(inout) :: matrix;
        integer, dimension (:), allocatable :: row;
        integer :: i = 1, l = 1, k = 1;
        singular = .false.;
        allocate(row(n));

        print *, "this is isi is s ", n;

        10 continue;
        if (i <= n .and. singular .eqv. .false.) then
            print *, "------------------", i;
            if (matrix(i, i) == 0) then
                20 continue;
                if (l < n .and. matrix(l, i) == 0) then
                    l = l + 1;
                    if (matrix(l, i) /= 0 .and. matrix(i, l) /= 0) then
                        row(:) = matrix(l, :);
                        matrix(i, :) = matrix(l, :);
                        do k = 1, n
                            matrix(l, k) = -1 * row(k);
                        end do
                        i = i + 1;
                        go to 10;
                    else
                        go to 20;
                    end if
                else
                    singular = .true.;
                    i = i + 1;
                    go to 10;
                end if
            else
                i = i + 1;
                go to 10;
            end if
        else
            go to 30;
        end if
        30 continue;

    end subroutine bareiss

    character(len = 2) function num2str(num)
        integer :: num;
        write(num2str, "(I0)") num;
    endfunction
end program