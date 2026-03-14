MODULE nmmod
    IMPLICIT NONE
    REAL FUNCTION Pol(x) RESULT(res)
        IMPLICIT NONE
        res = (x**5) + (3*x**4) - (2*x**2) - LOG(x**2) - 2
    REAL FUNCTION Bisection(a,b,F) RESULT(res)
        IMPLICIT NONE
        REAL, INTENT(IN) :: a,b
    END FUNCTION Bisection
    REAL FUNCTION False_position RESULT(res)
        IMPLICIT NONE
    END FUNCTION False_position
END MODULE nmmod