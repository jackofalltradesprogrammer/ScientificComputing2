PROGRAM vectoradd
IMPLICIT NONE

	INTEGER, PARAMETER :: N = 5
    DOUBLE PRECISION, DIMENSION(N) :: x, y, z
    INTEGER :: i

    ! Initialize x,y with random numbers from 0 to 100
    DO i=1, N
      	x(i) = RAND()*100
        y(i) = RAND()*100
    ENDDO

    !Compute the elementwise sum of x and y
    DO i=1,N
      	z(i) = x(i) + y(i)
    ENDDO
    PRINT *, SUM(z)

    z = x + y
    PRINT *, SUM(z)

    z = x + y
    PRINT *, SUM(z)

    PRINT *, SUM(x+y)
END PROGRAM vectoradd