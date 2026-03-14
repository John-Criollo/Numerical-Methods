MODULE functions
    IMPLICIT NONE
CONTAINS
    FUNCTION funa(vec) RESULT(res)
        !IMPLICIT NONE
        REAL, DIMENSION(:), INTENT(IN) :: vec
        REAL, DIMENSION(2) :: res
        INTEGER :: n, m
        REAL :: avr, sdev, sumval
        m = SIZE(vec)
        avr = SUM(vec)/m
        sdev = SQRT(SUM((vec-avr)**2)/m)
        res = (/ avr , sdev /)
    END FUNCTION funa
    FUNCTION funb(mtrx) RESULT(res)
        !IMPLICIT NONE
        REAL, DIMENSION(:,:), INTENT(IN) :: mtrx
        REAL, DIMENSION(2) :: res
        !INTEGER :: i, j
        REAL :: Tr, det, sumval
        sumval = 0.0
        IF (SIZE(mtrx, 1) == 2) THEN
            det = mtrx(1,1)*mtrx(2,2)-mtrx(1,2)*mtrx(2,1)
            Tr = mtrx(1,1) + mtrx(2,2)
        ELSE IF (SIZE(mtrx,1) == 3) THEN
            Tr = mtrx(1,1) + mtrx(2,2) + mtrx(3,3)
            !We can use a loops but this way its simpler
            det =  mtrx(1,1)*mtrx(2,2)*mtrx(3,3) + &
                   mtrx(1,2)*mtrx(2,3)*mtrx(3,1) + &
                   mtrx(1,3)*mtrx(2,1)*mtrx(3,2) - &
                   mtrx(3,1)*mtrx(2,2)*mtrx(1,3) - &
                   mtrx(3,2)*mtrx(2,3)*mtrx(1,1) - &
                   mtrx(3,3)*mtrx(2,1)*mtrx(1,2) 
        ELSE 
            STOP
        END IF
        res = (/ Tr, det /)
    END FUNCTION funb
    INTEGER,DIMENSION(2) FUNCTION matdim(name) RESULT(res)
        !IMPLICIT NONE
        CHARACTER(LEN = *), INTENT(IN) :: name
        INTEGER, DIMENSION(2)  :: res
        INTEGER :: n,nrow,ncol
        INTEGER :: u = 10, ios, ios1
        CHARACTER(LEN = 80) :: sample_row
        OPEN(UNIT = u, IOSTAT = ios, FILE = name, &
            STATUS = 'old', ACTION = 'read')
        nrow = 0
        ncol = 0
        IF (ios /= 0) THEN
            STOP
        END IF
        DO 
            READ(u, '(A)', IOSTAT = ios1) sample_row
            if (ios1 /= 0) EXIT
            nrow = nrow + 1
        END DO
        DO n = 1, 80
            IF (sample_row(n:n) == '.') THEN
                ncol = ncol + 1
            END IF
        END DO
        CLOSE(u)
        res = (/nrow,ncol/)
    END FUNCTION matdim
END MODULE functions

