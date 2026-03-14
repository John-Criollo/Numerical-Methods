SUBROUTINE pairsort(x, y)
    INTEGER, INTENT(INOUT) :: x, y
    INTEGER :: n
    IF (x > y) THEN
        n = x
        x = y
        y = n
    END IF
END SUBROUTINE pairsort

PROGRAM sort
    IMPLICIT NONE
    INTEGER :: a, b
    READ*, a,b
    PRINT*, 'a = ', a, 'b = ', b
    CALL pairsort(a, b)
    PRINT*, 'a = ',a , 'b = ', b
END PROGRAM sort
