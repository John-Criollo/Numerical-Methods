MODULE Practice
    IMPLICIT NONE
    CONTAINS
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
    REAL(8) FUNCTION Interpol(x,data_array) RESULT(res)
        IMPLICIT NONE
        REAL(8), DIMENSION(:,:), INTENT(IN) :: data_array
        REAL(8), INTENT(IN) :: x
        REAL(8) :: f1,f2,a,b
        INTEGER :: i
        IF(x < data_array(1,1)) THEN
            res = data_array(1,2)
        ELSE IF (x > data_array(SIZE(data_array,1),1)) THEN
            res = data_array(SIZE(data_array,1),2)
        ELSE
            DO i = 1, SIZE(data_array,1)
                IF (x >= data_array(i,1) .AND. x<= data_array(i+1,1)) THEN
                    a = data_array(i,1)
                    b = data_array(i+1,1)
                    f1 = data_array(i,2)
                    f2 = data_array(i+1,2)
                    res =  f1 + (f2-f1) * (x-a)/(b-a)
                    EXIT
                END IF
            END DO
        END IF 
    END FUNCTION Interpol
    REAL(8) FUNCTION Linterpol(x,data_array) RESULT(res)
        IMPLICIT NONE
        REAL(8), DIMENSION(:,:), INTENT(IN) :: data_array
        REAL(8), INTENT(IN) :: x
        REAL(8) :: f1,f2,a,b
        INTEGER :: i
        IF(x < data_array(1,1)) THEN
            f1 = data_array(1,2)
            f2 = data_array(2,2)
            a = data_array(1,1)
            b = data_array(2,1)
            res = f1 - (a-x)*(f2-f1)/(b-a)
        ELSE IF (x > data_array(SIZE(data_array,1),1)) THEN
            f1 = data_array(SIZE(data_array,1)-1,2)
            f2 = data_array(SIZE(data_array,1),2)
            a = data_array(SIZE(data_array,1)-1,1)
            b = data_array(SIZE(data_array,1),1)
            res = f2 +  (x-b)*(f2-f1)/(b-a)
        ELSE
            DO i = 1, SIZE(data_array,1)
                IF (x >= data_array(i,1) .AND. x<= data_array(i+1,1)) THEN
                    a = data_array(i,1)
                    b = data_array(i+1,1)
                    f1 = data_array(i,2)
                    f2 = data_array(i+1,2)
                    res =  f1 + (f2-f1) * (x-a)/(b-a)
                    EXIT
                END IF
            END DO
        END IF 
    END FUNCTION Linterpol
END MODULE Practice