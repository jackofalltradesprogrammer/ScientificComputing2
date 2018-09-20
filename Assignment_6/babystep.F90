PROGRAM assign06
IMPLICIT NONE

    ! Three arrays of size N used in the main program
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: x, y, z, x_square

    ! Will store the input file name/path that comes from command line
    CHARACTER(LEN=256) :: inputfile

    ! Miscellaneous variables used in the main program
    DOUBLE PRECISION :: ssum, esum
    INTEGER :: N, i

    ! Need to make sure we declare any functions we use
    ! within this program unit
    DOUBLE PRECISION scalar_sum, sum_of_elements

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
    ALLOCATE( z(N), x_square(N) )
    
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

     ! Calculate pairwise difference of x and y
     CALL pairwise_vector_differ(N, x, y, z)
#ifdef DEBUG
     PRINT *, 'pairwise_vector_differ: ', z
#endif

     ! Calculate vector_square()
     CALL vector_square(N,x,x_square)
#ifdef DEBUG
     PRINT *, 'Square: ', x_square
#endif

     ! Calculate sum_of elements() function
     esum = sum_of_elements(N, x)
#ifdef DEBUG
     PRINT *, 'Sum of x elements: ', esum
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

DOUBLE PRECISION FUNCTION scalar_differ(e,f)
IMPLICIT NONE 

        ! Returns the difference of vector variable e and f

        ! Declarations of subroutine arguments
        DOUBLE PRECISION :: e, f
#ifdef DEBUG
        PRINT *, 'scalar_differ() - e: ', e
        PRINT *, 'scalar_differ() - f: ', f
#endif

        scalar_differ = e - f
        RETURN

END FUNCTION scalar_differ

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

!---------------_------------------------------------

SUBROUTINE pairwise_vector_differ(N, a, b, c)
IMPLICIT NONE

    ! Performs the pairwise substraction of elements in a and b
    ! placing the results in  c.
        
    ! Declaration of Subroutie arguments
    INTEGER :: N
    DOUBLE PRECISION, DIMENSION(N) :: a, b, c

    ! Declaration of local variable
    INTEGER :: i

    ! Declare any functions that we are going to use
    DOUBLE PRECISION scalar_differ

#ifdef DEBUG
    PRINT *, 'pairwise_vector_diff() - a: ', a
    PRINT *, 'pairwise_vector_sum() - b: ', b
#endif

    DO i=1,N
        !There are a couple of ways to do this
        c(i) = scalar_differ( a(i), b(i))
    ENDDO

END SUBROUTINE pairwise_vector_differ

!-----------------------------------------------------

SUBROUTINE vector_square(N, x, x_square)
IMPLICIT NONE
     ! Perform the square of elements in x 
     ! placing the resuts in x_square

     ! Declaration of subroutine arguments
     INTEGER :: N
     DOUBLE PRECISION, DIMENSION(N) :: x, x_square

     ! Declaration of local variable
     INTEGER :: i

#ifdef DEBUG
     PRINT *, 'vector_square() - x: ', x
#endif

     DO i=1, N
        x_square(i)  = x(i)*x(i)
     ENDDO
     

END SUBROUTINE vector_square

!-----------------------------------------------------

DOUBLE PRECISION FUNCTION sum_of_elements(N, x)
IMPLICIT NONE
     ! Perform the sum of of elements in x
     ! Return the sum

     INTEGER :: N, i
     DOUBLE PRECISION, DIMENSION(N) :: x

     DO i=1, N
        sum_of_elements = sum_of_elements + x(i)
     ENDDO

     RETURN
END FUNCTION sum_of_elements

DOUBLE PRECISION FUNCTION rmse(N, x, y)
!IMPLICIT NONE
        ! Write code here
END FUNCTION rmse
