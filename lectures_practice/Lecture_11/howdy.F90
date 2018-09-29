PROGRAM howdy
IMPLICIT NONE

	INCLUDE "mpif.h"
	
	INTEGER :: my_pe, num_pe, ierr

	! Enroll this task in the MPI environment
	CALL MPI_INIT(ierr)

	! Find out what my logical pe number is
	CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_pe, ierr)

	! Find out how many of us there are
	CALL MPI_COMM_SIZE(MPI_COMM_WORLD, num_pe, ierr)

	PRINT *, "Howdy, MPI!! I am task ", my_pe, " of ", num_pe, " tasks."

	! Get this task out of the MPI environment
	CALL MPI_FINALIZE(ierr)

END PROGRAM howdy
