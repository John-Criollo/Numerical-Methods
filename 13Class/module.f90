MODULE numet
    IMPLICIT NONE
    CONTAINS
    REAL(8) FUNCTION f(x) RESULT(res)
        IMPLICIT NONE
        REAL(8), INTENT(IN):: x 
        res = (16*x-16)/(x**4-2*x**3+4*x-4)
    END FUNCTION f
    REAL(8) FUNCTION isum(a,b,err) RESULT(res)
        IMPLICIT NONE
        REAL(8), INTENT(IN) :: a,b,err
        REAL(8) :: iold,inew
        INTEGER :: N,i,j,maxit = 100, count
        count = 0
        N = 20
        res = 0
        iold = trapezoid(a,b,N)
        DO
            N = 2*N
            inew = trapezoid(a,b,N)
            PRINT*, N, inew
            count = count + 1
            IF(ABS(inew-iold)<err .AND. count<maxit) EXIT
            iold = inew
        END DO
        res = inew
    END FUNCTION isum
    REAL(8) FUNCTION lrieman(a,b,N) RESULT(res)
        IMPLICIT NONE
        REAL(8), INTENT(IN) :: a,b
        INTEGER, INTENT(IN) :: N 
        REAL(8) :: h
        INTEGER :: i 
        res = 0
        h = (b-a)/N  
        DO i = 1,N-1
            res = res + (f(a + i*h))*h
        END DO
    END FUNCTION lrieman
    REAL(8) FUNCTION midpoint(a,b,N) RESULT(res)
        IMPLICIT NONE
        REAL(8), INTENT(IN) :: a,b
        INTEGER, INTENT(IN) :: n
        REAL(8) :: h
        INTEGER :: i 
        h = (b-a)/N 
        res = 0
        DO i = 0,N-1
            res = res + f(a + i*h + h/2)
        END DO
        res = res*h
    END FUNCTION midpoint
    REAL(8) FUNCTION simpson(a,b,N) RESULT(res)
        IMPLICIT NONE
        REAL(8), INTENT(IN) :: a,b
        INTEGER, INTENT(IN) :: n
        REAL(8) :: h
        INTEGER :: i 
        h = (b-a)/N
        res = f(a) + f(b) + 4*f(a+h/2)
        DO i = 1,N-1
            res = res + 2*f(a + i*h) + 4*f(a + i*h + h/2)
        END DO
        res = res*h/6
    END FUNCTION simpson
    REAL(8) FUNCTION trapezoid(a,b,N) RESULT(res)
        IMPLICIT NONE
        REAL(8), INTENT(IN) :: a,b
        INTEGER, INTENT(IN) :: N 
        REAL(8) :: h
        INTEGER :: i 
        h = (b-a)/N
        res = f(a) + f(b)
        DO i = 1, N-1
            res = res + 2*f(a+i*h)
        END DO
        res = res*h/2
    END FUNCTION trapezoid
    SUBROUTINE comparing(a,b,err)
        REAL(8), INTENT(IN) :: a,b,err
        REAL(8) :: iold,inew
        INTEGER :: N, i,j,maxit = 100, faster
        INTEGER, DIMENSION(4) :: count
        PRINT*, 'Left Rieman Sum:'
        count = (/0,0,0,0/)
        N = 20
        iold = lrieman(a,b,N)
        DO
            N = 2*N
            inew = lrieman(a,b,N)
            PRINT*, N, inew
            count(1) = count(1) + 1
            IF(ABS(inew-iold)<err .AND. count(1)<maxit) EXIT
            iold = inew
        END DO
        PRINT*,'Midpoint Method:'
        count = 0
        N = 20
        iold = midpoint(a,b,N)
        DO
            N = 2*N
            inew = midpoint(a,b,N)
            PRINT*, N, inew
            count(2) = count(2) + 1
            IF(ABS(inew-iold)<err .AND. count(2)<maxit) EXIT
            iold = inew
        END DO
        PRINT*, 'Trapezoidal Method:'
        count = 0
        N = 20
        iold = trapezoid(a,b,N)
        DO
            N = 2*N
            inew = trapezoid(a,b,N)
            PRINT*, N, inew
            count(3) = count(3) + 1
            IF(ABS(inew-iold)<err .AND. count(3)<maxit) EXIT
            iold = inew
        END DO
        PRINT*, 'Simpson method:'
        count = 0
        N = 20
        iold = simpson(a,b,N)
        DO
            N = 2*N
            inew = simpson(a,b,N)
            PRINT*, N, inew
            count(4) = count(4) + 1
            IF(ABS(inew-iold)<err .AND. count(4)<maxit) EXIT
            iold = inew
        END DO
        
    END SUBROUTINE comparing
END MODULE numet