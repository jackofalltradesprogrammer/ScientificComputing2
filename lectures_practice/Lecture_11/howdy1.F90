PROGRAM howdy
IMPLICIT NONE

	INCLUDE "mpif.h"
	
	INTEGER :: ierr  ! Store status code from MPI calss

	! Enroll this task in the MPI environment
	CALL MPI_INIT(ierr)

	PRINT *, "Howdy, MPI !!!"

	! Get this task out of the MPI environment
	CALL MPI_FINALIZE(ierr)

END PROGRAM howdy	
