PROGRAM assign06
IMPLICIT NONE

    ! Three arrays of size N used in the main program
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: x, y, z

    ! Will store the input file name/path that comes from command line
    CHARACTER(LEN=256) :: inputfile

    ! Miscellaneous variables used in the main program
    DOUBLE PRECISION :: ssum
    INTEGER :: N, i

    ! Need to make sure we declare any functions we use
    ! within this program unit
    DOUBLE PRECISION scalar_sum

    ! Get the first command line argument and store it in variable inputfile
    CALL GETARG(1, inputfile)
    ! TRY PRINTING THIS WITHOUT THE TRIM() as an example
#ifdef DEBUG
    PRINT *, 'Getting input from file: ', TRIM(inputfile)
#endif

    ! Open "inputfile" and assign to logical unit 10
    OPEN(UNIT=10, FILE=TRIM(inputfile), STATUS='OLD')

    ! We assume that the first line contains the number of entries,
    ! and the next N lines each contain a pair of numbers.  So, read
    ! the first line and store the number in N
    READ(10, *) N

    ! Now that we know the size of N, allocate any arrays we need
    ALLOCATE( x(N), y(N) )
    ALLOCATE( z(N) )
    
    ! Read 'em in, one line at a time!
    DO i=1,N
        READ(10,*) x(i), y(i)
    END DO 

    CLOSE(10)

#ifdef DEBUG
    PRINT *, 'Main program - x: ', x
    PRINT *, 'Main program - y: ', y
    PRINT *, '-----------------------'
#endif


    ! Calculate pairwise sums of x and y
    CALL pairwise_vector_sum(N, x, y, z)
#ifdef DEBUG
    PRINT *, 'pairwise_vector_sum: ', z
#endif
    
    ! Test of scalar_sum() function
    ssum = scalar_sum(x(1), y(1))
#ifdef DEBUG
    PRINT *, 'ssum: ', ssum
#endif

END PROGRAM assign06

!---------------------------------------------------

DOUBLE PRECISION FUNCTION scalar_sum(e, f)
IMPLICIT NONE    

    ! Returns the sum of scalar variables e and f.
    ! It's obviously a stupid function, since it would be
    ! easier to just do "e + f" rather than "scalar_sum(ef, f), 
    ! but this demonstrates how to create and use a simple function

    ! Declaration of subroutine arguments 
    DOUBLE PRECISION :: e, f

#ifdef DEBUG
    PRINT *, 'scalar_sum() - e: ', e
    PRINT *, 'scalar_sum() - f: ', f
#endif

    ! Need to place the result in a variable that has the name of the function
    scalar_sum = e + f

    ! This returns value of "scalar_sum" to calling routine
    RETURN

END FUNCTION scalar_sum

!---------------------------------------------------

SUBROUTINE pairwise_vector_sum(N, a, b, c)
IMPLICIT NONE

    ! Performs pairwise addition of elements in a and b, 
    ! placing result in c.  

    ! Declaration of subroutine arguments 
    INTEGER :: N
    DOUBLE PRECISION, DIMENSION(N) :: a, b, c

    ! Declaration of local variables
    INTEGER :: i

    ! Need to make sure we declare any functions we use
    ! within this program unit
    DOUBLE PRECISION scalar_sum
    
#ifdef DEBUG
    PRINT *, 'pairwise_vector_sum() - a: ', a
    PRINT *, 'pairwise_vector_sum() - b: ', b
#endif

    DO i=1,N
        ! There are a couple of ways to do this
        ! c(i) = a(i) + b(i)
        c(i) = scalar_sum( a(i), b(i) )
    END DO

END SUBROUTINE pairwise_vector_sum

