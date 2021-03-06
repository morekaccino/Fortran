subroutine jacobi_eigenvalue ( n, a, it_max, v, d, it_num, rot_num )

    !*****************************************************************************************
    !
    !! JACOBI_EIGENVALUE carries out the Jacobi eigenvalue iteration.
    !
    !  Discussion:
    !
    !    This function computes the eigenvalues and eigenvectors of a
    !    real symmetric matrix, using Rutishauser's modfications of the classical
    !    Jacobi rotation method with threshold pivoting.
    !
    !  Parameters:
    !
    !    Input, integer ( kind = 4 ) N, the order of the matrix.
    !
    !    Input, real ( kind = 4 ) A(N,N), the matrix, which must be square, real,
    !    and symmetric.
    !
    !    Input, integer ( kind = 4 ) IT_MAX, the maximum number of iterations (100 should be ok)
    !
    !    Output, real ( kind = 4 ) V(N,N), the matrix of eigenvectors.
    !
    !    Output, real ( kind = 4 ) D(N), the eigenvalues, in descending order.
    !
    !    Output, integer ( kind = 4 ) IT_NUM, the total number of iterations.
    !
    !    Output, integer ( kind = 4 ) ROT_NUM, the total number of rotations.
    !
    !
    !
    !  Licensing:
    !
    !    This code is distributed under the GNU LGPL license.
    !
    !  Modified:
    !
    !    17 September 2013
    !
    !  Author:
    !
    !    FORTRAN90 version by John Burkardt
    !
    !*****************************************************************************************
    implicit none

    integer ( kind = 4 ) n

    real ( kind = 4 ) a(n,n)
    real ( kind = 4 ) bw(n)
    real ( kind = 4 ) c
    real ( kind = 4 ) d(n)
    real ( kind = 4 ) g
    real ( kind = 4 ) gapq
    real ( kind = 4 ) h
    integer ( kind = 4 ) i
    integer ( kind = 4 ) it_max
    integer ( kind = 4 ) it_num
    integer ( kind = 4 ) j
    integer ( kind = 4 ) k
    integer ( kind = 4 ) l
    integer ( kind = 4 ) m
    integer ( kind = 4 ) p
    integer ( kind = 4 ) q
    integer ( kind = 4 ) rot_num
    real ( kind = 4 ) s
    real ( kind = 4 ) t
    real ( kind = 4 ) tau
    real ( kind = 4 ) term
    real ( kind = 4 ) termp
    real ( kind = 4 ) termq
    real ( kind = 4 ) theta
    real ( kind = 4 ) thresh
    real ( kind = 4 ) v(n,n)
    real ( kind = 4 ) w(n)
    real ( kind = 4 ) zw(n)

    do j = 1, n
        do i = 1, n
            v(i,j) = 0.0
        end do
        v(j,j) = 1.0
    end do

    do i = 1, n
        d(i) = a(i,i)
    end do

    bw(1:n) = d(1:n)
    zw(1:n) = 0.0
    it_num = 0
    rot_num = 0

    do while ( it_num < it_max )

        it_num = it_num + 1
        !
        !  The convergence threshold is based on the size of the elements in
        !  the strict upper triangle of the matrix.
        !
        thresh = 0.0
        do j = 1, n
            do i = 1, j - 1
                thresh = thresh + a(i,j) ** 2
            end do
        end do

        thresh = sqrt ( thresh ) / real ( 4 * n, kind = 4 )

        if ( thresh == 0.0 ) then
            exit
        end if

        do p = 1, n
            do q = p + 1, n

                gapq = 10.0 * abs ( a(p,q) )
                termp = gapq + abs ( d(p) )
                termq = gapq + abs ( d(q) )
                !
                !  Annihilate tiny offdiagonal elements.
                !
                if ( 4 < it_num .and. &
                        termp == abs ( d(p) ) .and. &
                        termq == abs ( d(q) ) ) then

                    a(p,q) = 0.0
                    !
                    !  Otherwise, apply a rotation.
                    !
                else if ( thresh <= abs ( a(p,q) ) ) then

                    h = d(q) - d(p)
                    term = abs ( h ) + gapq

                    if ( term == abs ( h ) ) then
                        t = a(p,q) / h
                    else
                        theta = 0.5 * h / a(p,q)
                        t = 1.0 / ( abs ( theta ) + sqrt ( 1.0 + theta * theta ) )
                        if ( theta < 0.0 ) then
                            t = - t
                        end if
                    end if

                    c = 1.0 / sqrt ( 1.0 + t * t )
                    s = t * c
                    tau = s / ( 1.0 + c )
                    h = t * a(p,q)
                    !
                    !  Accumulate corrections to diagonal elements.
                    !
                    zw(p) = zw(p) - h
                    zw(q) = zw(q) + h
                    d(p) = d(p) - h
                    d(q) = d(q) + h

                    a(p,q) = 0.0
                    !
                    !  Rotate, using information from the upper triangle of A only.
                    !
                    do j = 1, p - 1
                        g = a(j,p)
                        h = a(j,q)
                        a(j,p) = g - s * ( h + g * tau )
                        a(j,q) = h + s * ( g - h * tau )
                    end do

                    do j = p + 1, q - 1
                        g = a(p,j)
                        h = a(j,q)
                        a(p,j) = g - s * ( h + g * tau )
                        a(j,q) = h + s * ( g - h * tau )
                    end do

                    do j = q + 1, n
                        g = a(p,j)
                        h = a(q,j)
                        a(p,j) = g - s * ( h + g * tau )
                        a(q,j) = h + s * ( g - h * tau )
                    end do
                    !
                    !  Accumulate information in the eigenvector matrix.
                    !
                    do j = 1, n
                        g = v(j,p)
                        h = v(j,q)
                        v(j,p) = g - s * ( h + g * tau )
                        v(j,q) = h + s * ( g - h * tau )
                    end do

                    rot_num = rot_num + 1

                end if

            end do
        end do

        bw(1:n) = bw(1:n) + zw(1:n)
        d(1:n) = bw(1:n)
        zw(1:n) = 0.0

    end do
    !
    !  Restore upper triangle of input matrix.
    !
    do j = 1, n
        do i = 1, j - 1
            a(i,j) = a(j,i)
        end do
    end do
    !
    !  Ascending sort the eigenvalues and eigenvectors.
    !
    do k = 1, n - 1

        m = k

        do l = k + 1, n
            if ( d(l) < d(m) ) then
                m = l
            end if
        end do

        if ( m /= k ) then

            t    = d(m)
            d(m) = d(k)
            d(k) = t

            w(1:n)   = v(1:n,m)
            v(1:n,m) = v(1:n,k)
            v(1:n,k) = w(1:n)

        end if

    end do

    return
end