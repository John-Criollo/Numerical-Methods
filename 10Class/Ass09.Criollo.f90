PROGRAM main
    USE Practice
    USE lup_solver
    IMPLICIT NONE
    REAL(8), DIMENSION(:,:), ALLOCATABLE :: A 
    REAL(8), DIMENSION(:), ALLOCATABLE :: x,b,x1
    INTEGER :: n,m,i
    INTEGER :: u1 = 10, ios1
    OPEN(UNIT = u1, IOSTAT = ios1, FILE = 'matrix.dat', &
        STATUS = 'old', ACTION = 'read')
    n = Nrows('matrix.dat')
    m = Ncols('matrix.dat')
    !PRINT*, n,m
    ALLOCATE(A(m,m))
    ALLOCATE(x(m))
    ALLOCATE(b(m))
    ALLOCATE(x1(m))
    DO i = 1, m 
        READ(u1,*) A(i,:)
    END DO
    
    READ(u1,*) b
    !PRINT*, A 
    !PRINT*, b
    CLOSE(u1)
    !PRINT*, det(A)
    CALL cramer(A,b,x)
    PRINT*, 'Result using Cramer:'
    PRINT*,x
    CALL solve_lup(A,b,x1)
    PRINT*, 'Result using LUP:'
    PRINT*, x1

END PROGRAM main