program Assignment08
    implicit none
    real :: Rec_time(3)
    Rec_time = (/1.0, 2.0, 3.0/)
    call RodCov1DFEM(TIME = Rec_time, L = 1.5, N = 1500, K = 0.1, BC_L = 150.0, BC_R = 30.0, OUTPUT = 'MyOutputFile')
contains
    subroutine RodCov1DFEM(TIME, L, N, k, BC_L, BC_R, IC, EPS, OUTPUT)
        !-------------------------------------------------------------------------------!
        ! This subroutine solves the 1D convective heat equation to find temperature !
        ! distribution along a rod using explicit finite difference approximation. !
        ! !
        ! INPUT ARGUMENTS: !
        ! TIME is a vector which elements specifies the times (in seconds) at which !
        ! solution (temperature distribution) is recorded into the output file. !
        ! L is the length of the rod in meters. !
        ! N is number of grid points along the rod. it should be an integer !
        ! between 100 and 3000. !
        ! out of the range inputs will be set to the closest limit. !
        ! k is thermal diffusively in squared meter per second. !
        ! BC_L temperature at the left end of the rod in Celsius. !
        ! BC_R temperature at the right end of the rod in Celsius. !
        ! IC (Optional) is the initial temperature of the rod. The default value !
        ! is zero degree Celsius. !
        ! EPS (Optional) is the steady-state condition criterion. !
        ! if sum of |Temp_new - Temp_old| (over all grid points)is less than. !
        ! EPS, steady-state condition is reached. default vale is 1.0E-3 . !
        ! OUTPUT (Optional) is the output file name. It should be a string. !
        ! The default name is output. The output file has a .dat format. !
        ! !
        ! NOTE: 1- Since the subroutine has more than one optional argument, it is !
        ! recommended to specify the input argument values by using their !
        ! dummy names to avoid mixing the order of arguments. For example: !
        ! call RodCov1DFEM(TIME=(/1.0,5.0/),L=1.5,N=1500,...,OUTPUT='Result') !
        ! 2- Computation is terminated when reaches steady-state condition. The !
        ! steady-state temperature distribution will be written into the !
        ! output file. !
        !
        !   Note: The subroutine was heavily inspired by the solution provided
        !   in the additional_examples pdf file.
        !-------------------------------------------------------------------------------!
        implicit none
        real, dimension(:), intent(in) :: TIME
        integer, intent(in) :: N
        real, intent(in) :: k, BC_L, BC_R, L
        real, intent(in), optional :: IC, EPS
        character(len = *), intent(in), optional :: OUTPUT

        real, allocatable :: T(:), Tnew(:)
        integer :: i, j, n_
        real :: delta_t, delta_x, eps_, time_, thresh_

        if (N > 3000) then
            n_ = 3000;
        elseif (N < 100) then
            n_ = 100;
        else
            n_ = N;
        end if

        allocate(T(n_), Tnew(n_));

        if (present(IC)) then
            T = IC;
        else
            T = 0.0;
        end if
        Tnew = 0
        T(1) = BC_L; T(n_) = BC_R;
        eps_ = huge(eps_)

        if (present(OUTPUT)) then
            open(10, file = OUTPUT // '.dat')
        else
            open(10, file = 'output.dat')
        end if

        if (present(EPS)) then
            thresh_ = EPS;
        else
            thresh_ = 0.001;
        end if

        delta_x = L / n_
        delta_t = (0.5 * delta_x**2) / k
        time_ = 0
        do while (eps_>thresh_)
            time_ = time_ + delta_t
            Tnew(1) = T(1);Tnew(n_) = T(n_)
            eps_ = 0
            do i = 2, n_ - 1
                Tnew(i) = T(i) + (k * delta_t / delta_x**2) * (T(i + 1) - 2.0 * T(i) + T(i - 1))
                eps_ = eps_ + abs(Tnew(i) - T(i))
            enddo
            T = Tnew
            print*, time_, eps_
            do j = 1, size(TIME)
                if (TIME(j)<=time_ .and. time_<=TIME(j) + delta_t) then
                    Write(10, '(/,a,f6.3)') 'Time:', time_
                    write(10, *) Tnew
                end if
            end do
        enddo
        Write(10, '(/,a,f6.3,a)') 'Simulation reached to a steady-state condition around ', time_, ' second.'
        write(10, *) Tnew
        close(10)

    end subroutine RodCov1DFEM
end program Assignment08
