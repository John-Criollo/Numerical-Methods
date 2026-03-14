REAL(8) FUNCTION Pol(x) RESULT(res)
        IMPLICIT NONE
        REAL(8), INTENT(IN) :: x
        res = (x**5) + (3*x**4) - (2*x**2) - LOG(x**2) - 2
END FUNCTION Pol

SUBROUTINE Bisection(a,b,c,n,err)
    REAL(8), INTENT(INOUT) :: a,b,c
    REAL(8), EXTERNAL :: Pol
    INTEGER, INTENT(INOUT) :: n 
    REAL(8), INTENT(IN) :: err
    DO
        IF ((b-a) <= err) EXIT
        c = (a+b)/2
        IF (Pol(b) * Pol(c) <= 0) THEN
            a = c 
        ELSE 
            b = c
        END IF
        n = n+1
    END DO
END SUBROUTINE Bisection

SUBROUTINE Falsi(a,b,c,n,err)
    REAL(8), INTENT(INOUT) :: a,b,c
    INTEGER, INTENT(INOUT) :: n 
    REAL(8), INTENT(IN) :: err
    REAL(8), EXTERNAL :: Pol
    DO
        IF (ABS(b-a) <= err) EXIT
        c = (Pol(b) * a - Pol(a) * b)/(Pol(b) - Pol(a))
        IF (Pol(b) * Pol(c) <= 0) THEN
            a = c 
        ELSE 
            b = c
        END IF
        n = n+1
    END DO
END SUBROUTINE Falsi

PROGRAM main
    IMPLICIT NONE
    REAL(8) :: a,b
    REAL(8) :: x , y
    REAL(8), EXTERNAL :: Pol
    INTEGER :: n,m
    REAL(8) :: err = 1.0E-7
    a = -2.0
    b = -1.0
    n = 0
    m = 0
    x = 0.0
    y = 0.0
    CALL Bisection(a,b,x,n,err)
    PRINT*, 'Bisection method:'
    PRINT*, 'Last value: ', x, "Number of iterations: ", n
    a = -2.0
    b = -1.0
    CALL Falsi(a,b,y,m,err)
    PRINT*, 'Regula Falsi method: '
    PRINT*, 'Last value: ', y, "Number of iterations: ", m
    IF (n > m) THEN
        PRINT*, 'Bisection method is slower than Falsi'
    ELSE IF (n == m) THEN
        PRINT*, 'Bisection and Regula Falsi has the same velocity'
    ELSE
        PRINT*, 'Regula Falsi method is slower than Bisection'
    END IF
END PROGRAM main