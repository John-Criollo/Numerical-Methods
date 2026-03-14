INTEGER(8) RECURSIVE FUNCTION myfac(n) RESULT(res)
    !la forma de definir el programa es <type> RECURSIVE FUNCTION fname(vars) RESULT(var)
    INTEGER(8), INTENT(IN) :: n
    !Intent especifica que las variables asignadas son las variables que usa el programa
    IF (n<2) THEN
        res = 1
    ELSE
        res = n * myfac(n-1)
    END IF
END FUNCTION myfac

PROGRAM factorial
    IMPLICIT NONE
    INTEGER(8) :: m, n
    INTEGER(8), EXTERNAL :: myfac
    PRINT*, 'Which factorial would you like to get?:'
    READ*, n
    m = myfac(n)
    PRINT*, n, 'Factorial is:', m
END PROGRAM factorial
