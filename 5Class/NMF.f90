MODULE NMF
    IMPLICIT NONE
CONTAINS
    REAL FUNCTION Average(vec) RESULT(res)
        IMPLICIT NONE
        REAL,DIMENSION(:), INTENT(IN) :: vec
        res = SUM(vec)/SIZE(vec)
    END FUNCTION Average
    REAL FUNCTION SDev(vec) RESULT(res)
        IMPLICIT NONE
        REAL, DIMENSION(:), INTENT(IN) :: vec
        REAL :: avr
        avr = SUM(vec)/SIZE(vec)
        res = SQRT(SUM((vec - avr)**2)/SIZE(vec))
    END FUNCTION SDev
    REAL FUNCTION Trace(mtrx) RESULT(res)
        IMPLICIT NONE
        REAL, DIMENSION(:,:), INTENT(IN) :: mtrx
        IF (SIZE(mtrx,1) == 2) THEN
            res = mtrx(1,1) + mtrx(2,2)
        ELSE IF (SIZE(mtrx,1) == 3) THEN
            res = mtrx(1,1) + mtrx(2,2) + mtrx(3,3)
        ELSE 
            STOP
        END IF
    END FUNCTION Trace
    REAL FUNCTION Determinant(mtrx) RESULT(res)
        IMPLICIT NONE
        REAL, DIMENSION(:,:), INTENT(IN) :: mtrx
        IF (SIZE(mtrx,1) == 2) THEN
            res = mtrx(1,1)*mtrx(2,2)-mtrx(1,2)*mtrx(2,1)
        ELSE IF (SIZE(mtrx,1) == 3) THEN
            res = mtrx(1,1)*mtrx(2,2)*mtrx(3,3) + &
                   mtrx(1,2)*mtrx(2,3)*mtrx(3,1) + &
                   mtrx(1,3)*mtrx(2,1)*mtrx(3,2) - &
                   mtrx(3,1)*mtrx(2,2)*mtrx(1,3) - &
                   mtrx(3,2)*mtrx(2,3)*mtrx(1,1) - &
                   mtrx(3,3)*mtrx(2,1)*mtrx(1,2) 
        ELSE 
            STOP
        END IF
    END FUNCTION Determinant
    INTEGER FUNCTION Nrows(file_name) RESULT(res)
        !IMPLICIT NONE
        CHARACTER(*),INTENT(IN) :: file_name
        INTEGER :: u, ios,state,n
        CHARACTER(LEN = 80) :: row
        OPEN(NEWUNIT = u, IOSTAT = ios, FILE = file_name, & 
            STATUS = 'old', ACTION = 'read') !NUNIT???
        res = 0
        IF (ios /= 0) THEN
            STOP
        END IF
        DO  
            !iostat changes its value when it doesn find more numbers
            READ(u,'(A)', IOSTAT = state) row
            IF (state /= 0) EXIT !another way of using if without end if
            IF (row(1:1) /= '#') res = res + 1
        END DO
        CLOSE(u)
    END FUNCTION Nrows
    INTEGER FUNCTION Ncols(file_name) RESULT(res)
        !IMPLICIT NONE
        CHARACTER(*),INTENT(IN) :: file_name
        INTEGER :: u, ios,state,n
        CHARACTER(LEN = 80) :: row
        OPEN(NEWUNIT = u, IOSTAT = ios, FILE = file_name, &
            STATUS = 'old', ACTION = 'read')
        res = 0
        IF (ios /= 0) THEN
            STOP
        END IF
        READ(u,'(A)', IOSTAT = state) row
        CLOSE(u)
        DO n = 1, 80 !LEN_TRIM function give me the non empty part
            IF (row(n:n) == '.') THEN
                res = res + 1
            END IF
        END DO
    END FUNCTION Ncols
END MODULE NMF