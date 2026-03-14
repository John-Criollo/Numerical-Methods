Program main
    USE numet
    IMPLICIT NONE
    REAL(8) :: a = 0, b = 1,err = 1e-5, x
    !x = isum(a,b,err)
    !print*,'The value of the integral is:',x
    PRINT*, 'Assignment 12:'
    CALL comparing(a,b,err)
    PRINT*, 'Simpson has the best performance!' 
END PROGRAM main