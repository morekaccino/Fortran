module Set3LnEqn
contains
    subroutine TakeLnEq(A, B)
        !===========================================================================================================!
        ! @brief read 3 equation and make the System of linear equations.                                           !
        !													                                                        !
        ! this subroutin reads three equations from the input, each with three variables X, Y, and Z (aX+bY+cZ=d),	!
        ! and make a system of linear equations for them. Coefficients for each equation are stored in a row of	    !
        ! a 3x3 matrix (A) and constants are stored in a vector (B).						                        !
        ! 													                                                        !
        ! @param[out]	A    	a real double precision matrix, size: 3x3					                        !
        ! @param[out]	B	a real double precision array, size: 3						                            !
        !===========================================================================================================!
        implicit none
        integer :: i, ix, iy, iz, ieq
        real (kind = 8), dimension(3, 3), intent(out) :: A
        real (kind = 8), dimension(3), intent(out) :: B
        character (len = 80) :: str
        character (len = 3), dimension(3) :: eqn_num
        eqn_num(1) = '1st'; eqn_num(2) = '2nd'; eqn_num(3) = '3rd'
        A = 0.0; B = 0.0
        Do i = 1, 3
            print*, 'Enter your ', eqn_num(i), ' 3-variable linear equation.'
            print*, 'NOTE:'
            print*, '1- use real numbers for the variable coefficients (i.e. 1.0X+0.234Y+158.98Z=22.1),'
            print*, '2- the left side of the equation must have 3 terms (one term for each variable),'
            print*, '3- use capital letters for variables, and do not use any comma or space in your entry,'
            print*, '4- right side of the equation must be a real number only.'
            read(*, *) str
            ix = index(str, 'X'); iy = index(str, 'Y'); iz = index(str, 'Z'); ieq = index(str, '=')
            if (ix<iy .and. ix<iz) read(str(1:ix - 1), *) A(i, 1)
            if (ix>iy .and. (ix<iz.or.iy>iz)) read(str(iy + 1:ix - 1), *) A(i, 1)
            if (ix>iz .and. (ix<iy.or.iz>iy)) read(str(iz + 1:ix - 1), *) A(i, 1)
            if (iy<ix .and. iy<iz) read(str(1:iy - 1), *) A(i, 2)
            if (iy>ix .and. (iy<iz.or.ix>iz)) read(str(ix + 1:iy - 1), *) A(i, 2)
            if (iy>iz .and. (iy<ix.or.iz>ix)) read(str(iz + 1:iy - 1), *) A(i, 2)
            if (iz<iy .and. iz<ix) read(str(1:iz - 1), *) A(i, 3)
            if (iz>iy .and. (iz<ix.or.iy>ix)) read(str(iy + 1:iz - 1), *) A(i, 3)
            if (iz>ix .and. (iz<iy.or.ix>iy)) read(str(ix + 1:iz - 1), *) A(i, 3)
            read(str(ieq + 1:len_trim(str)), *) B(i)
        enddo
    end subroutine TakeLnEq


    function Jacobi(A, B) result(X)
        !-----------------------------------------------------------------------------------------------------!
        ! This function solves a system of linear equations (with maximum 3 variables) using Jacobi's Method. !
        ! If a system linear equations presented in matrix form as: [A]X=[B], this function will output X. The!
        ! output of the function, X, is a 3x1 vector which elements are the solutions of the linear system.   !
        !                                                                                                     !
        ! Note: This function was inspired by the example provided in Additional_examples.pdf > Problem9	  !
        !                                                                                                     !
        !-----------------------------------------------------------------------------------------------------!
        implicit none
        real (kind = 8), dimension(3, 3), intent(in) :: A
        real (kind = 8), dimension(3), intent(in) :: B
        real (kind = 8), dimension(3) :: X

        real (kind = 8) :: x_, y, z, x_old, y_old, z_old, eps, eps_old
        integer :: it, div_count, i
        eps_old = abs(huge(eps))
        eps = 1.0e-8
        x_old = 0.01; y_old = 0.01; z_old = 0.01 ! initial guesses
        it = 0
        div_count = 0
        do it = 1, 1000 ! limiting the iterations to 1000
            ! constructing the updated formula
            x_ = (B(1) - (A(1, 2) * y_old) - (A(1, 3) * z_old)) / A(1, 1)
            y = (B(2) - (A(2, 1) * x_old) - (A(2, 3) * z_old)) / A(2, 2)
            z = (B(3) - (A(3, 1) * x_old) - (A(3, 2) * y_old)) / A(3, 3)
            eps = abs(x_ - x_old) + abs(y - y_old) + abs(z - z_old)
            if (eps<=1.0e-8) exit
            if (eps>=eps_old) then
                div_count = div_count + 1
                if (div_count>=100) then
                    print '(a,i3,a)', 'No solution was found. Solution diverges after&
                            &', it, ' iterations.'
                    stop
                end if
            else
                div_count = 0
            end if
            x_old = x_; y_old = y; z_old = z
            eps_old = eps
        end do
        X(1) = x_;
        X(2) = y;
        X(3) = z;
    end function Jacobi

end module Set3LnEqn


program Assignment07
    use Set3LnEqn
    implicit none
    real(kind = 8), dimension(3, 3) :: A
    real(kind = 8), dimension(3) :: B, X
    call TakeLnEq(A, B)
    X = Jacobi(A, B)
    print 100, 'x= ', X(1), ', y=', X(2), ', and z= ', X(3), '.'
    100 format(a, es11.4, a, es11.4, a, es11.4, a)
end program Assignment07
