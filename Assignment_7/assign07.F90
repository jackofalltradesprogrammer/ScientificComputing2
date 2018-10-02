
! Define the function
DOUBLE PRECISION FUNCTION myfunc(x)
IMPLICIT NONE

	DOUBLE PRECISION :: x
	myfunc = x*x
	RETURN

END FUNCTION myfunc
 
PROGRAM assign07
IMPLICIT NONE


	INCLUDE "mpif.h"
	
	CHARACTER(LEN=*), PARAMETER :: INPUT_FILEPATH = "input1.txt"
	INTEGER :: my_pe, num_pe, ierr
	DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: NXoXnArray
	DOUBLE PRECISION :: myfunc, DeltaX, MyX, localSum, globalSum, Xo, Xn, N, MyN
	INTEGER :: i
	REAL  :: start_clock, stop_clock

!	start_clock = 0
!	stop_clock = 0

	start_clock = MPI_WTIME()

#ifdef DEBUG
	PRINT *, "START TIME IS ", start_clock
#endif

	! Enroll this task in the MPI environment
	CALL MPI_INIT(ierr)

	! Find out what my logical pe number is
	CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_pe, ierr)

	! Find out how many of us there are
	CALL MPI_COMM_SIZE(MPI_COMM_WORLD, num_pe, ierr)

#ifdef DEBUG
	PRINT *, "Started PE", my_pe, " of ", num_pe
#endif

	! Assume that the master process has access to INPUT_FILEPATH
	! Do NOT assume that other processes can safely read this

!	ALLOCATE(NXoXnArray(2))

	IF (my_pe == 0) THEN
		OPEN(10, FILE=INPUT_FILEPATH, STATUS = 'OLD')
		READ(10, *) N
		READ(10, *) Xo, Xn
		CLOSE(23)
	ENDIF

	! Add the Synchronization Point
	!CALL MPI_BCAST(NXoXnArray, 2, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr) 

	CALL MPI_BCAST(N, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr) 
	CALL MPI_BCAST(Xo, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr) 
	CALL MPI_BCAST(Xn, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr) 


#ifdef DEBUG
	PRINT *, "PE = ", my_pe, ": Xo, Xn, N: ", Xo, Xn,N
#endif

	! Integration Algorithm as per teh slides
	DeltaX = (Xn - Xo)/N
	MyN   = N/num_pe
	MyX   = Xo + my_pe*DeltaX*MyN
	localSum = 0

	DO i=1, INT(MyN)
		localSum = localSum + myfunc(MyX)*DeltaX
		MyX = MyX + DeltaX
	END DO

#ifdef DEBUG
	PRINT *, "PE = ", my_pe, ": DeltaX, MyN, MyX: ", DeltaX, MyN, Myx
#endif

	CALL MPI_REDUCE(localSum,globalSum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, &
	                     0, MPI_COMM_WORLD, ierr)

#ifdef DEBUG
	PRINT *, "PE", my_pe, ": ", "worker_sum: ", localSum

#endif

	IF (my_pe == 0) THEN
		PRINT *, "Global Sum is ", globalSum
	stop_clock = MPI_WTIME()
	PRINT *, "Stop time: ", stop_clock
	PRINT *, "Wall time:  ", stop_clock-start_clock
	ENDIF

	! Get this tast out of the MPI environment
	CALL MPI_FINALIZE(ierr)

END PROGRAM assign07

