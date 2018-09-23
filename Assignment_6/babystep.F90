PROGRAM assign06
IMPLICIT NONE

    ! Three arrays of size N used in the main program
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: x, y, z, x_square

    ! Will store the input file name/path that comes from command line
    CHARACTER(LEN=256) :: inputfile

    ! Miscellaneous variables used in the main program
    DOUBLE PRECISION :: esum, rmse_result
    INTEGER :: N, i

    ! Need to make sure we declare any functions we use
    ! within this program unit
    DOUBLE PRECISION sum_of_elements, rmse

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
    PRINT *, 'Main program - x:           ', x
    PRINT *, 'Main program - y:           ', y
    PRINT *, '-------------------------------------------------------------------------------------------------------------------'
#endif

     ! Calculate pairwise difference of x and y
     CALL pairwise_vector_diff(N, x, y, z)
#ifdef DEBUG
     PRINT *, 'pairwise_vector_diff:       ', z
     PRINT *, '------------------------------------------------------------------------------------------------------------------'
#endif

     ! Calculate vector_square()
     CALL vector_square(N, x, z)
#ifdef DEBUG
     PRINT *, 'Square:                      ', z
     PRINT *, '-----------------------------------------------------------------------------------------------------------------'
#endif

     ! Calculate sum_of elements() function
     esum = sum_of_elements(N, x)
#ifdef DEBUG
     PRINT *, 'Sum of x elements:           ', esum
     PRINT *, '-----------------------------------------------------------------------------------------------------------------'
#endif

     ! Run  rmse()
#ifdef DEBUG    
     PRINT *, '                                              -                                            '
     PRINT *, '                                            rmse()                   '
     PRINT *, '                                              -                                            '
     rmse_result = rmse(N, x, y)
     PRINT *, 'rmse_result:                 ', rmse_result
#endif

END PROGRAM assign06

!---------------------------------------------------

DOUBLE PRECISION FUNCTION scalar_differ(e,f)
IMPLICIT NONE 

        ! Returns the difference of vector variable e and f

        ! Declarations of subroutine arguments
        DOUBLE PRECISION :: e, f
#ifdef DEBUG
        PRINT *, 'scalar_differ()-e:          ', e
        PRINT *, 'scalar_differ()-f:          ', f
#endif

        scalar_differ = e - f
        RETURN

END FUNCTION scalar_differ

!---------------------------------------------------



SUBROUTINE pairwise_vector_diff(N, a, b, c)
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
    PRINT *, 'pairwise_vector_diff()-a:   ', a
    PRINT *, 'pairwise_vector_diff()-b:   ', b
#endif

    DO i=1,N
        !There are a couple of ways to do this
        c(i) = scalar_differ( a(i), b(i))
    ENDDO

END SUBROUTINE pairwise_vector_diff

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
     PRINT *, 'vector_square() - x:         ', x
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
#ifdef DEBUG
     PRINT *, 'Inside sum_of_elements, sum: ', sum_of_elements
#endif
     RETURN
END FUNCTION sum_of_elements
!-----------------------------------------------------
DOUBLE PRECISION FUNCTION rmse(N, x, y)
IMPLICIT NONE
     ! Returns the Root Mean Square Eror of
     ! Forecast(x) and Observations(y)

     INTEGER :: N
     DOUBLE PRECISION, DIMENSION(N) :: x,y, xydiff, xysquare
     
     DOUBLE PRECISION sum_of_elements
     
     ! Call the SUBROUTINE pairwise_vector_diff
     CALL pairwise_vector_diff(N, x, y, xydiff)

     ! Call the SUBROUTINE vector_square
     CALL vector_square(N, xydiff, xysquare)

#ifdef DEBUG
     PRINT *, 'Square:                      ', xysquare
#endif

     ! get the sum of element
     rmse = SQRT(sum_of_elements(N, xysquare)/N)

     RETURN
END FUNCTION rmse
