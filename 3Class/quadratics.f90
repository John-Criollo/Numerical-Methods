PROGRAM quadratic
    IMPLICIT NONE
    REAL :: b,c,d,x1,x2
    PRINT*, 'We are solving the equation ax**2 + bx + c = 0'
    PRINT*, 'Key in the parameters b and c'
    READ*, b
    READ*, c
    d = b*b -4.0*c
    IF (d < 0) THEN
        PRINT*, 'No real solution exist'
    ELSE
        x1 = -0.5 * (b + SQRT(d))
        x2 = -0.5 * (b - SQRT(d))
        PRINT*, 'The solutions are',x1,'and',x2
    END IF
END PROGRAM quadratic
    
