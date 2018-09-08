PROGRAM matvecproduct
IMPLICIT NONE

	INTEGER, PARAMETER :: N = 3

	DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: x, b
	DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: A
	INTEGER :: i, j

	! Dynamically allocate arrays and vectors
	ALLOCATE( x(N) )
	ALLOCATE( b(N) )
	ALLOCATE( A(N,N) )

	! Give A and x some values
	DO j=1,N
		DO i=1,N
			A(i,j) = RAND()
		END DO
		x(j) = RAND()
	END DO

	! Compute the matrix-vector Ax
	DO i=1,N
		b(i) = 0.0
		DO j=1,N
			b(i) = b(i) + A(i,j)*x(j)
		ENDDO
	END DO
	PRINT *, b


	! Compute the matrix-vector AX using matmul()
	DO i=1,N
		b(i) = 0.0
		b(i) = DOT_PRODUCT(A(i,:), x)
	END DO
	PRINT *, b

	! Compute the matrix-vector Ax using matmul()
	b = MATMUL(A, x)
	PRINT *, b

	! Once more assigning random numbers differently
	CALL RANDOM_NUMBER(A)
	CALL RANDOM_NUMBER(x)
	b = MATMUL(A, x)
	PRINT *, b
END PROGRAM matvecproduct
