MODULE numet
    IMPLICIT NONE
    CONTAINS
    REAL(8) FUNCTION f(x) RESULT(res)
        REAL(8), INTENT(in) :: x
        res = 3*x**2
    END FUNCTION f
    REAL(8) FUNCTION g(x) RESULT(res)
        REAL(8), INTENT(in) :: x
        res = EXP(-x**2)
    END FUNCTION g
    REAL(8) FUNCTION f_pi(x,y) RESULT(res)
        REAL(8), INTENT(in) :: x,y
        REAL(8) ::r
        r = x**2 + y**2
        IF (r <= 1) THEN
            res = 1
        else
            res = 0
        END IF
    END FUNCTION f_pi
    SUBROUTINE pi_est()
        REAL(8),DIMENSION(2) :: r
        INTEGER :: N = 10000, i
        INTEGER, DIMENSION(2) :: counter
        REAL(8) :: radius
        counter = (/0,0/)
        DO i = 1, N
            CALL random_number(r)
            r = 2*r - 1
            radius = (r(1))**2 + (r(2))**2
            IF (radius <= 1) THEN
                counter(1) = counter(1) + 1
            else
                counter(2) = counter(2) + 1
            END IF
        END DO
        PRINT('(A, f6.3)'), "Estimate value of pi:", 4*REAL(counter(1),8)/sum(counter)
    END SUBROUTINE pi_est
    SUBROUTINE reject(x)
        INTEGER :: i, N = 10000
        REAL(8) :: xmax,r,d
        REAL(8), INTENT(INOUT) :: x
        xmax = 1
        Do 
            CALL random_number(x)
            CALL random_number(r)
            d = f(x)/f(xmax)
            IF (r<d) exit
        END DO
    END SUBROUTINE reject
    SUBROUTINE int_est()
        INTEGER :: i, N = 10000
        INTEGER :: counter
        REAL(8) :: x,y, y_max, Abox
        y_max = 27
        Abox = y_max*(3-0)
        DO i = 1, N
            CALL random_number(x)
            CALL random_number(y)
            x = 0 + (3-0) * x
            y = y_max * y
            IF (y<f(x)) THEN
                counter = counter + 1
            END IF
        END DO
        PRINT('(A, f10.3)'),"Integral estimate", Abox * REAL(counter,8)/N
    END SUBROUTINE int_est
    SUBROUTINE crudemon()
        INTEGER :: i,N = 10000
        REAL(8) :: x, a
        a = 0
        DO i = 1,N
            CALL random_number(x)
            !x = a + (b-a)*x
            a = a + g(x)
        END DO
        a = a/N
        PRINT('(A,f6.3)'), "Integral estimate", a
    END SUBROUTINE crudemon
    SUBROUTINE pi_cmc()
        REAL(8),DIMENSION(2) :: r
        INTEGER :: N = 10000000, i
        REAL(8) :: a
        a = 0
        DO i = 1, N
            CALL random_number(r)
            r = 2*r - 1
            a = a + f_pi(r(1),r(2))
        END DO
        a = 4*a/N
        PRINT('(A,f10.5)'), "Pi value estimate", a
    END SUBROUTINE pi_cmc
END MODULE numet
