PROGRAM factortest
IMPLICIT NONE

	INCLUDE "mpif.h"
	
	INTEGER :: my_pe, num_pe, ierr
	CHARACTER(LEN=*), PARAMETER :: INPUT_FILEPATH = "input1.txt"
	DOUBLE PRECISION :: factor

	
	! Enroll this task in the MPI environment
	CALL MPI_INIT(ierr)

	! Find out what my logical pe number is
	CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_pe, ierr)

	! Find out how many of us there are
	CALL MPI_COMM_SIZE(MPI_COMM_WORLD, num_pe, ierr)

	! Assume that the master process has access to INPUT_FILEPATH
	! Do NOT assume that other processes can safely read this
	IF (my_pe == 0) THEN
		OPEN(23, FILE=INPUT_FILEPATH, STATUS = 'OLD')
		READ(23, *) factor
		CLOSE(23)
	ENDIF

	PRINT *, "PE", my_pe, ": ", "Howdy, MPI!!"
	PRINT *, "PE", my_pe, ": ", factor

	! Get this tast out of the MPI environment
	CALL MPI_FINALIZE(ierr)

END PROGRAM factortest

