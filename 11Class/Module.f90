MODULE Practice
    IMPLICIT NONE
    CONTAINS

    REAL(8) RECURSIVE FUNCTION det(A) RESULT(res)
        IMPLICIT NONE
        REAL(8), DIMENSION(:,:), INTENT(IN) :: A
        REAL(8), DIMENSION(:,:), ALLOCATABLE :: A1 
        INTEGER :: i,j, n
        n = SIZE(A,1)
        ALLOCATE(A1(n-1,n-1))
        res = 0
        IF (SIZE(A,1) == 1) THEN
            res = A(1,1)
        ELSE
            DO i = 1, n
                CALL SmallMatrix(A,A1,i)
                res = res + ((-1.0)**i)*A(1,i)*det(A1)
            END DO
        END IF
    END FUNCTION det
    SUBROUTINE SmallMatrix(A,A1,i)
        REAL(8), DIMENSION(:,:), INTENT(IN) :: A 
        REAL(8), DIMENSION(:,:), INTENT(INOUT) :: A1
        INTEGER, INTENT(IN) :: i 
        INTEGER :: n 
        n = SIZE(A,1)
        IF(i > n .OR. i<0) STOP
        IF(i == 1) THEN
            A1 = A(2:n,2:n)
        ELSE IF (i == n) THEN
            A1 = A(2:n,1:n-1)
        ELSE
            A1(:,1:i-1) = A(2:n,1:i-1)
            A1(:,i:n-1) = A(2:n,i+1:n)
        END IF
    END SUBROUTINE SmallMatrix
    SUBROUTINE cramer(A,b,x)
        REAL(8), DIMENSION(:,:), INTENT(IN) :: A
        REAL(8), DIMENSION(:), INTENT(IN) :: b
        REAL(8), DIMENSION(:), INTENT(INOUT) :: x
        REAL(8) :: detA
        REAL(8), DIMENSION(:,:), ALLOCATABLE :: Ai
        INTEGER :: n,i 
        n = SIZE(A,1)
        detA = det(A)
        ALLOCATE(Ai(n,n))
        DO i = 1 , n
            Ai = A
            Ai(:,i) = B
            x(i) = det(Ai)/detA
        END DO
        !PRINT*, x
    END SUBROUTINE cramer
    REAL(8) FUNCTION mean(X) RESULT(res)
        IMPLICIT NONE
        REAL(8), DIMENSION(:), INTENT(IN) :: X
        res = SUM(X)/SIZE(X)
    END FUNCTION mean
    REAL(8) FUNCTION sdev(X) RESULT(res)
        IMPLICIT NONE
        REAL(8), DIMENSION(:), INTENT(IN) :: X
        INTEGER :: n 
        res = sqrt(SUM((X - mean(X))**2)/SIZE(X))
    END FUNCTION sdev
    INTEGER FUNCTION Nrows(file_name) RESULT(res)
        IMPLICIT NONE
        CHARACTER(*), INTENT(IN) :: file_name
        CHARACTER :: a
        INTEGER :: u, ios, ios1
        OPEN(NEWUNIT = u, IOSTAT = ios, FILE = file_name, &
            STATUS = 'old', ACTION = 'read' )
        IF (ios /= 0) STOP 'Error: file not opened'
        res = 0
        DO 
            READ(u,'(A1)', IOSTAT = ios1) a
            IF (a == '') CYCLE
            IF (ios1 /= 0) EXIT
            res = res + 1
        END DO
        CLOSE(u)
    END FUNCTION Nrows
    INTEGER FUNCTION Ncols(file_name) RESULT(res)
        IMPLICIT NONE
        CHARACTER(*), INTENT(IN)  :: file_name
        CHARACTER(LEN = 80) :: line
        INTEGER :: u, ios, ios1, n
        OPEN(NEWUNIT = u, IOSTAT = ios, FILE = file_name, &
            STATUS = 'old', ACTION = 'read')
        IF (ios /= 0) STOP 'Error: file not opened'
        res = 0
        READ(u, '(A80)') line
        DO n = 1, 80
            IF (line(n:n) == '.') res = res + 1
        END DO
        CLOSE(u)
    END FUNCTION Ncols
END MODULE Practice
