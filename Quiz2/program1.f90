program Quiz2
    implicit none
    character(len = 10) :: name;
    integer :: i, j, k, l;
    logical :: repeated;
    open(1, file = 'Problem1_input.txt');

    do i = 1, 20
        read(1, "(A)") name;
        l = LEN_TRIM(name);
        repeated = .false.;
        do j = 1, l - 1
            do k = j + 1, l
                if (name(j:j) == name(k:k)) then
                    repeated = .true.;
                    exit;
                end if
            end do
        end do
        if (repeated) then
            write(*, "(A)") name;
        end if
    end do

    close(1);
end program
