PROGRAM matmatproduct
IMPLICIT NONE

	INTEGER, PARAMETER :: N =1000
	DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: A, B, C
	INTEGER :: i, j, k

	!For timers
	REAL, DIMENSION(2) :: timearray
	REAL :: clock_start, clock_stop

	!Dynamically allocate arrays and vechtors
	ALLOCATE( A(N,N) )
	ALLOCATE( B(N,N) )
	ALLOCATE( C(N,N) )

	!Initialize A and B
	DO j=1,N
	 Do i=1,N
	  A(i,j) = DBLE(i+j)
	  B(i,j) = DBLE(i+j)
	 ENDDO
	ENDDO

	!Compute the matrix-matric AB
	!--------------------------------------------
	CALL ETIME(timearray, clock_start)
	C = 0.0
	DO i=1, N
	 DO j=1, N
	  DO k=1, N
	   c(i,j)=c(i,j) + A(i,k)*B(k,j)
	  ENDDO
	 ENDDO
	ENDDO

	PRINT *, SUM(C)
	CALL ETIME(timearray, clock_stop)
	PRINT *, 'Elapsed i,j,k: ', (clock_stop-clock_start)

! Continue with the other five loop permutations
! followed by individual dot products, mat-vec products,
! and matmult()

END PROGRAM matmatproduct

