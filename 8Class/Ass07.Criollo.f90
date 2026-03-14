PROGRAM main
    USE method
    IMPLICIT NONE
    REAL(8) :: a,b,err = 1.0E-7!,grid = 0.1
    a = 1.0
    b = 10.0
    CALL Root_finding(a,b,err)
    !DO i = 1, 10
    !    counter = 0
    !    a = 1.6 - 0.1*i 
    !    b = 2.2 + 0.1*i
    !    CALL Falsi(a,b,sol,counter,err)
    !END DO
END PROGRAM main
