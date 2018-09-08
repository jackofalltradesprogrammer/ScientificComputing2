PROGRAM innerproduct
IMPLICIT NONE

	INTEGER, PARAMETER :: N = 3

	DOUBLE PRECISION, DIMENSION(N) :: x, y
	DOUBLE PRECISION :: ip
	INTEGER :: i

	! Assign values to x and y
	x = (/ 1.0, 2.0, 3.0 /)
	y = (/ 4.0, 5.0, 6.0 /)

	! Compute the inner product x and y
	ip = 0.0
	DO i=1,N
		ip = ip + x(i)*y(i)
	ENDDO

	PRINT *, ip

	ip = DOT_PRODUCT(x, y)
	PRINT *, ip
END PROGRAM innerproduct
