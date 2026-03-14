MODULE method
    IMPLICIT NONE
    Contains

    SUBROUTINE Bisection(a1,b1,sol,n,err)
        REAL(8), INTENT(IN) :: a1,b1,err
        REAL(8), INTENT(INOUT) :: sol
        INTEGER, INTENT(INOUT) :: n 
        REAL(8) :: a,b
        a = a1
        b = b1
        DO
            IF ((b-a) <= err) EXIT
            sol = (a+b)/2
            IF (Pol(b) * Pol(sol) <= 0) THEN
                a = sol 
            ELSE 
                b = sol
            END IF
            n = n+1
            if (n == 100) EXIT
        END DO
        PRINT*, '(1)Bisection method:'
        PRINT*, 'Last value: ', sol, "Number of iterations: ", n
    END SUBROUTINE Bisection

    SUBROUTINE Falsi(a1,b1,sol,n,err)
        REAL(8), INTENT(IN) :: a1,b1,err
        INTEGER, INTENT(INOUT) :: n 
        REAL(8), INTENT(INOUT) :: sol
        REAL(8) :: a,b
        a = a1
        b = b1
        DO  
            IF ((b-a) <= err) EXIT !I guess it never reaches this
            !condition so it keeps iterating
            sol = (Pol(b) * a - Pol(a) * b)/(Pol(b) - Pol(a))
            IF (Pol(b) * Pol(sol) <= 0) THEN
                a = sol 
            ELSE 
                b = sol
            END IF
            n = n+1
            if (n == 100) EXIT 
        END DO
        PRINT*, '(2)Regula Falsi method:'
        PRINT*, 'Last value: ', sol, "Number of iterations: ", n
    END SUBROUTINE Falsi

    SUBROUTINE Newton(a,sol,n,err)
        REAL(8), INTENT(INOUT) :: sol
        REAL(8), INTENT(IN) :: a,err
        INTEGER, INTENT(INOUT) :: n
        REAL(8) :: x,x1
        x = a
        IF(ABS(dpol(x))<TINY(x)) STOP
        DO
            x1 = x - pol(x)/dpol(x)
            IF(abs(x1 - x)<err) THEN
                sol = x1
                EXIT
            ELSE
                x = x1
            END IF
            n = n+1
            if (n == 100) EXIT
        END DO
        PRINT*, '(3)Newton-Raphson method:'
        PRINT*, 'Last value: ', sol, "Number of iterations: ", n
    END SUBROUTINE Newton

    SUBROUTINE Secant(a,b,sol,n,err)
        REAL(8), INTENT(INOUT) :: sol
        REAL(8), INTENT(IN) :: a,b,err
        INTEGER, INTENT(INOUT) :: n 
        REAL(8) :: x,x1,x2
        x = a
        x1 = b
        IF(ABS(pol(x1)-pol(x))<TINY(x)) STOP
        DO
            x2 = x1 - (pol(x1)*(x1-x))/(pol(x1) - pol(x))
            IF (ABS(x2-x1)<err) THEN
                sol = x2
                EXIT
            ELSE
                x = x1
                x1 = x2
            END IF
            n = n+1
            if (n == 100) EXIT
        END DO
        PRINT*, '(4)Secant method:'
        PRINT*, 'Last value: ', sol, "Number of iterations: ", n
    END SUBROUTINE Secant

    SUBROUTINE Root_computing(a,b,solutions,counters,err)
        REAL(8), INTENT(IN) :: a,b,err
        REAL(8), DIMENSION(:), INTENT(INOUT) :: solutions
        INTEGER, DIMENSION(:), INTENT(INOUT) :: counters
        INTEGER :: i
        DO i = 1, SIZE(solutions) !it is supposed to be 4 because its 4 methods
            solutions(i) = 0.0
            counters(i) = 0
        END DO
        CALL Bisection(a,b,solutions(1),counters(1),err)

        CALL Falsi(a,b,solutions(2),counters(2),err)
        
        CALL Newton(b,solutions(3),counters(3),err)
        
        CALL Secant(a,b,solutions(4),counters(4),err)
        
        CALL Faster(counters)
    END SUBROUTINE Root_computing

    SUBROUTINE Root_finding(a,b,err)
        REAL(8), INTENT(IN) :: a,b,err
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: counters
        REAL(8), DIMENSION(:,:), ALLOCATABLE :: solutions
        REAL(8), DIMENSION(:), ALLOCATABLE :: lower, upper
        INTEGER :: nroots,i
        REAL(8) :: grid = 0.1
        nroots = Root_counting(a,b)
        ALLOCATE(counters(nroots,4))
        ALLOCATE(solutions(nroots,4))
        ALLOCATE(lower(nroots))
        ALLOCATE(upper(nroots))
        CALL Root_boundaries(a,b,lower,upper,grid)
        DO i = 1, nroots
            PRINT*, 'Solution for the root number', i
            CALL Root_computing(lower(i), upper(i),solutions(i,:),counters(i,:),err)
        END DO
        PRINT*, 'So the roots are found in this interval are:'
        PRINT*, solutions(:,3)
    END SUBROUTINE Root_finding

    INTEGER FUNCTION Root_counting(a,b) RESULT(res)
        REAL(8), INTENT(IN):: a,b
        REAL(8) :: x,y, grid = 0.1 
        x = a
        y = x + grid
        res = 0
        DO
            IF (y < b) THEN
                IF (pol(x)*pol(y) <= 0) res = res + 1 
            ELSE
                EXIT
            END IF
            x = y
            y = x + grid
        END DO
    END FUNCTION Root_counting
    SUBROUTINE Root_boundaries(a,b,lower,upper,grid)
        REAL(8),DIMENSION(:), INTENT(INOUT) :: upper,lower
        REAL(8), INTENT(IN) :: a,b,grid
        INTEGER :: i
        REAL(8) :: x1,x2
        x1 = a
        x2 = a + grid
        i = 1
        DO 
            IF (x2 > b) EXIT
            IF (pol(x1) * pol(x2) < 0) THEN
                lower(i) = x1
                upper(i) = x2
                i = i + 1
                IF (i >= (SIZE(upper) + 1)) EXIT
                x1 = x2
                x2 = x1 + grid
            ELSE
                x1 = x2
                x2 = x1 + grid
            END IF
        END DO    
    END SUBROUTINE Root_boundaries
    SUBROUTINE Faster(counters)
        INTEGER, DIMENSION(:), INTENT(IN) :: counters
        INTEGER :: i, select
        INTEGER :: mincount
        CHARACTER(LEN = 10), DIMENSION(4) :: name
        name(1) = 'Bisection '
        name(2) = 'Falsi     '
        name(3) = 'Newton    '
        name(4) = 'Secant    '
        mincount = counters(1)
        select = 1
        DO i = 1, SIZE(counters)
            IF (counters(i) < mincount) THEN
                mincount = counters(i)
                select = i
            ELSE 
                CYCLE
            END IF
        END DO
        PRINT*, TRIM(name(select)), ' is the fastest method.'
    END SUBROUTINE Faster

    REAL(8) FUNCTION pol(x) RESULT(res)
        IMPLICIT NONE
        REAL(8), INTENT(IN) :: x 
        res = COS(x) - LOG(x) + 1.0
    END FUNCTION pol

    REAL(8) FUNCTION Dpol(x) RESULT(res)
        IMPLICIT NONE
        real(8), intent(in) :: x
        res = -SIN(x) - 1/x
    END FUNCTION dpol

END MODULE  method
