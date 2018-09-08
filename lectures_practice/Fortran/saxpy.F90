PROGRAM saxpy
IMPLICIT NONE

	INTEGER, PARAMETER :: N = 3
	DOUBLE PRECISION, PARAMETER :: ALPHA = 2.41

	DOUBLE PRECISION, DIMENSION(N) :: x, y, z
	INTEGER :: i

	! Initialize x,y with random numbers from 0 to 100
	CALL RANDOM_NUMBER(x)
	CALL RANDOM_NUMBER(y)
	x = x*100.0
	y = y*100.0

	! Compute the Saxpy
	DO i=1, N
		z(i) = ALPHA*x(i) + y(i)
	ENDDO
	PRINT *, Z

	z = ALPHA*x + y
	PRINT *,Z

	PRINT *, ALPHA*x + y
END PROGRAM saxpy
