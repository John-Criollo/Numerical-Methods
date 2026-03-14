PROGRAM array
    IMPLICIT NONE
    REAL(8), DIMENSION(5) :: v1
    REAL, DIMENSION(4) :: v2
    INTEGER :: i
    v1 = (/ 1.1,1.2,1.3,1.4,1.5 /)
    !v2 = (/ 1.1,1.2,1.3,1.4 /)
    PRINT*, v1
    PRINT*, SIZE(v2)
    PRINT*, 2**2,3**2,SQRT(9.0),SQRT(REAL(SIZE(v2)))
    DO i = 1, 5
        v1(i) = 1.0 + i* 0.1_8
    END DO
    PRINT*, v1
END PROGRAM array
