PROGRAM matmatproduct
IMPLICIT NONE

	INTEGER, PARAMETER :: N = 3

	DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE::A,B,C
	INTEGER :: i,j,k

	!Dynamically allocate arrays and vectors
	ALLOCATE(A(N,N))
	ALLOCATE(B(N,N))
	ALLOCATE(C(N,N))

	! Initialize A and B
	DO j=1,N
	 DO i=1,N
	  A(i,j) = DBLE(i+j)
	  B(i,j) = DBLE(i+j)
	 ENDDO
	ENDDO

	! Compute the matrix-matrix AB
	!------------------------------
	C = 0.0
	DO i=1,N
	 DO j=1,N
	  DO k=1,N
	   C(i,j)=C(i,j)+A(i,k)*B(k,j)
	  ENDDO
	 ENDDO
	ENDDO
	PRINT *, SUM(C)

	!Compute the matrix-matrix using individual dot products
	C = 0.0
	DO i=1,N
	 DO j=1,N
	  C(i,j) = DOT_PRODUCT(A(i,:),B(:,j))
	 ENDDO
	ENDDO
	PRINT *, SUM(C)

	!Compute the matrix-matrix using individual matrix-vector products
	C=0.0
	DO j=1,N
	 C(:,j)=MATMUL(A,B(:,j))
	ENDDO
	PRINT *, SUM(C)

	!Or, let's just be a wimp
	C = MATMUL(A,B)
	PRINT *, SUM(C)
END PROGRAM matmatproduct
