MODULE Practice
    IMPLICIT NONE
    CONTAINS
    REAL FUNCTION mean(X) RESULT(res)
        IMPLICIT NONE
        REAL, DIMENSION(:), INTENT(IN) :: X
        res = SUM(X)/SIZE(X)
    END FUNCTION mean
    REAL FUNCTION Var(X) RESULT(res)
        IMPLICIT NONE
        REAL, DIMENSION(:), INTENT(IN) :: X
        INTEGER :: n 
        res = SQRT(SUM((X - mean(X))**2)/SIZE(X))
    END FUNCTION Var
    INTEGER(8) FUNCTION Nrows(file_name) RESULT(res)
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
            IF (ios1 /= 0) EXIT
            res = res + 1
        END DO
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
    END FUNCTION Ncols
    INTEGER Function Nskip(file_name) RESULT(res)
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
            IF (ios1 /= 0) EXIT
            IF (a(1:1) == '#') res = res + 1
        END DO
    END FUNCTION Nskip
END MODULE Practice