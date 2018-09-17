
PROGRAM fdmplay
IMPLICIT NONE

    INTEGER, PARAMETER :: N = 1000
    DOUBLE PRECISION, PARAMETER :: L = 1.0,    &
&                                  C = 2.0,    &
&                                  RHO = 5.0,  &
&                                  TN = 0.5,        &
&                                  DELTA_T = 0.05,  & 
&                                  LEFT_BC = 100.0,   &
&                                  RIGHT_BC= 0.0, &
&                                  INITIAL_VALUE = 100.0

    PRINT *, '-------------------------'
    PRINT *, 'Naive GE test for identity matrix'
    CALL naive_ge_test_ident

    PRINT *, '-------------------------'
    PRINT *, 'Naive GE test'
    CALL naive_ge_test

    PRINT *, '-------------------------'
    PRINT *, 'Thomas test'
    CALL thomastest

    PRINT *, '-------------------------'
    PRINT *, 'DGESV test'
    CALL dgesv_test

    PRINT *, '-------------------------'
    PRINT *, 'DGTSV test'
    CALL dgtsv_test

    PRINT *, '-------------------------'
    PRINT *, 'Full matrix, naive GE'
    CALL fullmat_fdm1d(N, L, INITIAL_VALUE, LEFT_BC, RIGHT_BC, &
&                      C, RHO, DELTA_T, TN)

    PRINT *, '-------------------------'
    PRINT *, 'Full matrix, DGESV'
    CALL dgesv_fdm1d(N, L, INITIAL_VALUE, LEFT_BC, RIGHT_BC, &
&                    C, RHO, DELTA_T, TN)

    PRINT *, '-------------------------'
    PRINT *, 'Tridiagonal, custom thomas'
    CALL thomas_fdm1d(N, L, INITIAL_VALUE, LEFT_BC, RIGHT_BC, &
&                      C, RHO, DELTA_T, TN)

    PRINT *, '-------------------------'
    PRINT *, 'Tridiagonal, DGTSV'
    CALL dgtsv_fdm1d(N, L, INITIAL_VALUE, LEFT_BC, RIGHT_BC, &
&                      C, RHO, DELTA_T, TN)


END PROGRAM fdmplay


SUBROUTINE naive_ge(N, A, b)
IMPLICIT NONE

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! 
    ! Performns naive GE (no pivoting) on Ax=b.   
    ! 
    ! The result, x, is returned in b.  A is
    ! modified during this routine. 
    ! 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    INTEGER, INTENT(IN) :: N
    DOUBLE PRECISION, DIMENSION(N,N) :: A
    DOUBLE PRECISION, DIMENSION(N) :: b

    ! Local variables
    INTEGER :: i, j, k
    DOUBLE PRECISION :: factor, sum

    ! Forward elimination
    DO k = 1, N-1
        ! Reducing column k
        DO i = k+1, N
            factor = A(i,k)/A(k,k)
            DO j = k+1, N
                A(i,j) = A(i,j) - factor*A(k,j)
            ENDDO
            b(i) = b(i) - factor*b(k) 
        ENDDO
    ENDDO

    ! Back substitution
    b(N) = b(N) / A(N,N)
    DO i = N-1, 1, -1
        sum = b(i)
        DO j = i+1, N
            sum = sum - A(i,j)*b(j)
        ENDDO
        b(i) = sum / A(i,i)
    ENDDO

END SUBROUTINE naive_ge


SUBROUTINE thomas(N, e, f, g, r)
IMPLICIT NONE

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Custom implementation of Thomas algorithm
    ! 1D arrays as input
    !
    ! e : lower subdiagonal, length N-1
    ! f : diagonal, length N
    ! g : upper subdiagonal, length N-1
    ! r : right-hand side, length N
    !
    ! Solution is returned in r
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    INTEGER  :: N
    DOUBLE PRECISION, DIMENSION(N)  :: f
    DOUBLE PRECISION, DIMENSION(N-1)  :: e, g
    DOUBLE PRECISION, DIMENSION(N)  :: r

    INTEGER k

    ! Decomposition
    DO k=2,N
        e(k-1) = e(k-1)/f(k-1)
        f(k) = f(k) - e(k-1)*g(k-1)
    ENDDO

    ! Forward substitution
    DO k=2,N
        r(k) = r(k) - e(k-1)*r(k-1)
    ENDDO
    
    ! Back substitution
    r(n) = r(n)/f(n)
    DO k=N-1, 1, -1
        r(k) = (r(k) - g(k)*r(k+1))/f(k)
    ENDDO

END SUBROUTINE thomas


SUBROUTINE naive_ge_test_ident
IMPLICIT NONE

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! Test of Naive GE algorithm on an identity matrix
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    INTEGER, PARAMETER :: N = 4

    DOUBLE PRECISION, DIMENSION(N,N) :: A
    DOUBLE PRECISION, DIMENSION(N) :: b


    A = RESHAPE( (/ 1.0, 0.0, 0.0, 0.0,    &
&                   0.0, 1.0, 0.0, 0.0,   &
&                   0.0, 0.0, 1.0, 0.0,   &
&                   0.0, 0.0, 0.0, 1.0 /), &
&                   (/4, 4/) )

    b = (/ 1.0, 2.0, 3.0, 4.0 /)

    PRINT *, 'A: ', A
    PRINT *, 'b: ', b
    CALL naive_ge(N, A, b)

    PRINT *, 'b: ', b

END SUBROUTINE naive_ge_test_ident

SUBROUTINE naive_ge_test
IMPLICIT NONE

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! Test of Naive GE algorithm.  Specifics of this
    ! problem are available in Example 11.1 from
    ! Chapra textbook
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    INTEGER, PARAMETER :: N = 4

    DOUBLE PRECISION, DIMENSION(N,N) :: A
    DOUBLE PRECISION, DIMENSION(N) :: b


    A = RESHAPE( (/ 2.04, -1.0, 0.0, 0.0,    &
&                   -1.0, 2.04, -1.0, 0.0,   &
&                   0.0, -1.0, 2.04, -1.0,   &
&                   0.0, 0.0, -1.0, 2.04 /), &
&                   (/4, 4/) )

    b = (/ 40.8, 0.8, 0.8, 200.8 /)

    PRINT *, 'A: ', A
    PRINT *, 'b: ', b
    CALL naive_ge(N, A, b)

    PRINT *, 'b: ', b

END SUBROUTINE naive_ge_test


SUBROUTINE thomastest
IMPLICIT NONE

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! Test of Thomas algorithm.  Specifics of this
    ! problem are available in Example 11.1 from
    ! Chapra textbook
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    INTEGER, PARAMETER :: N = 4

    DOUBLE PRECISION, DIMENSION(N) :: f,  r
    DOUBLE PRECISION, DIMENSION(N-1) :: e, g

    f = (/ 2.04, 2.04, 2.04, 2.04 /)
    e = (/  -1.0, -1.0, -1.0 /)
    g = (/  -1.0, -1.0, -1.0 /)
    r = (/ 40.8, 0.8, 0.8, 200.8 /)

    PRINT *, 'f: ', f
    PRINT *, 'e: ', e
    PRINT *, 'g: ', g
    PRINT *, 'r: ', r

    CALL thomas(N, e, f, g, r)

    PRINT *, 'r: ', r
END SUBROUTINE thomastest


SUBROUTINE dgesv_test
IMPLICIT NONE

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! Test of DGESV LAPACK routine.  Specifics of this
    ! problem are available in Example 11.1 from
    ! Chapra textbook
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    INTEGER, PARAMETER :: N = 4

    DOUBLE PRECISION, DIMENSION(N,N) :: A
    DOUBLE PRECISION, DIMENSION(N) :: b
    INTEGER, DIMENSION(N) :: ipiv

    INTEGER :: nrhs, lda, ldb, info

    nrhs = 1;  lda = N;  ldb = N

    A = RESHAPE( (/ 2.04, -1.0, 0.0, 0.0,    &
&                   -1.0, 2.04, -1.0, 0.0,   &
&                   0.0, -1.0, 2.04, -1.0,   &
&                   0.0, 0.0, -1.0, 2.04 /), &
&                   (/4, 4/) )

    b = (/ 40.8, 0.8, 0.8, 200.8 /)

    PRINT *, 'A: ', A
    PRINT *, 'b: ', b

    CALL DGESV(N, nrhs, A, lda, ipiv, b, ldb, info)

    PRINT *, 'b: ', b

END SUBROUTINE dgesv_test



SUBROUTINE dgtsv_test
IMPLICIT NONE

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! Test of Thomas algorithm.  Specifics of this
    ! problem are available in Example 11.1 from
    ! Chapra textbook
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    INTEGER, PARAMETER :: N = 4

    DOUBLE PRECISION, DIMENSION(N) :: d,  b
    DOUBLE PRECISION, DIMENSION(N-1) :: dl, du

    INTEGER :: nrhs, ldb, info

    nrhs = 1;  ldb = N

    dl = (/  -1.0, -1.0, -1.0 /)
    d = (/ 2.04, 2.04, 2.04, 2.04 /)
    du = (/  -1.0, -1.0, -1.0 /)

    b = (/ 40.8, 0.8, 0.8, 200.8 /)

    PRINT *, 'd: ', d
    PRINT *, 'dl: ', dl
    PRINT *, 'du: ', du
    PRINT *, 'b: ', b
    CALL DGTSV(N, nrhs, dl, d, du, b, ldb, info)

    PRINT *, 'b: ', b
END SUBROUTINE dgtsv_test


DOUBLE PRECISION FUNCTION k(u)
IMPLICIT NONE

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! Implementation of a nonlinear thermal conductivity,
    ! making the value of k dependent on the temperature, u.
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    DOUBLE PRECISION, INTENT(IN) :: u

    k = 0.01*u + 0.01

    RETURN 
END FUNCTION k


SUBROUTINE fullmat_fdm1d(N, L, initial_value, &
&                        left_bc, right_bc,   &
&                        c, rho,              &
&                        delta_t, t_n)
IMPLICIT NONE

    ! Subroutine parameters
    INTEGER, INTENT(IN) :: N
    DOUBLE PRECISION, INTENT(IN) :: L, initial_value, left_bc, &
&                                   right_bc, c, rho, delta_t, t_n
    
    DOUBLE PRECISION, DIMENSION(N,N) :: A
    DOUBLE PRECISION, DIMENSION(N) :: f, u_old, u_new

    ! External functions
    DOUBLE PRECISION :: k

    ! Local variables
    DOUBLE PRECISION :: h, t, r
    INTEGER :: i

    ! For timers
    REAL, DIMENSION(2) :: timearray
    REAL :: clock_start, clock_stop

    ! Distance between nodes
    h = L/(N-1)

    ! Set initial values
    u_new = initial_value

    t = 0.0D0
    CALL ETIME(timearray, clock_start)
    DO WHILE (t < t_n) 

        ! Copy new solution to old
        u_old = u_new

        ! Reset system Au=f
        A = 0.0;  f = 0.0

        ! Coefficient for equations of interior nodes 
        ! (all but boundary nodes)
        DO i = 2, N-1
            ! Compute the nonlinear r(u) for the node
            r = ( k(u_old(i))*delta_t ) / (c*rho*rho)
            A(i, i-1) = r
            A(i, i) = -(2.0*r + 1)
            A(i, i+1) = r
            f(i) = -u_old(i)
        ENDDO

        ! Modify for boundary conditions
        A(1, 1) = 1.0;    f(1) = left_bc 
        A(N, N) = 1.0;    f(N) = right_bc 

        !PRINT *, 'A: ', A
        CALL naive_ge(N, A, f)
        u_new = f

        IF (N < 10) THEN
            PRINT '(F5.2, 1X, 10(E8.3, 1X))', t, u_new
        ENDIF

        t = t + delta_t
    ENDDO
    CALL ETIME(timearray, clock_stop)
    
    PRINT '("Wall time: ", F5.1, " seconds")', clock_stop-clock_start
    PRINT '("Sum: ", E10.5)', SUM(u_new) 
    PRINT '("Mean: ", E10.5)', SUM(u_new) / SIZE(u_new)
    PRINT '("Max: ", E10.5)', MAXVAL(u_new)
    PRINT '("Min: ", E10.5)', MINVAL(u_new)

END SUBROUTINE fullmat_fdm1d


SUBROUTINE thomas_fdm1d(N, L, initial_value, &
&                        left_bc, right_bc,   &
&                        c, rho,              &
&                        delta_t, t_n)
IMPLICIT NONE

    INTEGER, INTENT(IN) :: N
    DOUBLE PRECISION, INTENT(IN) :: L, initial_value, left_bc, &
&                                   right_bc, c, rho, delta_t, t_n
    
    PRINT *, 'You get to implement thomas_fdm1d!!'

END SUBROUTINE thomas_fdm1d


SUBROUTINE dgesv_fdm1d(N, L, initial_value, &
&                        left_bc, right_bc,   &
&                        c, rho,              &
&                        delta_t, t_n)
IMPLICIT NONE

    INTEGER, INTENT(IN) :: N
    DOUBLE PRECISION, INTENT(IN) :: L, initial_value, left_bc, &
&                                   right_bc, c, rho, delta_t, t_n
    

    PRINT *, 'You get to implement dgesv_fdm1d!!'

END SUBROUTINE dgesv_fdm1d


SUBROUTINE dgtsv_fdm1d(N, L, initial_value, &
&                      left_bc, right_bc,   &
&                        c, rho,              &
&                        delta_t, t_n)
IMPLICIT NONE

    INTEGER, INTENT(IN) :: N
    DOUBLE PRECISION, INTENT(IN) :: L, initial_value, left_bc, &
&                                   right_bc, c, rho, delta_t, t_n
    

    PRINT *, 'You get to implement dgtsv_fdm1d!!'


END SUBROUTINE dgtsv_fdm1d

