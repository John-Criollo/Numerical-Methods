PROGRAM test5
    IMPLICIT NONE
    CHARACTER(LEN = 80) :: row
    REAL :: f
    INTEGER :: n, u = 0, ios,ios1, c, c1,i
    OPEN(UNIT = u, IOSTAT = ios, FILE = 'data.txt', &
        STATUS = 'old', ACTION = 'read')
    c = 0
    c1 = 0
    IF (ios == 0) THEN
      2 READ(u,'(A)' , IOSTAT = ios1) row
        IF (ios1 == 0) THEN
            c = c + 1
            GO TO 2
        ELSE
            DO i = 1, 80
                IF (row(i:i) == '.') THEN 
                   c1 = c1 + 1
                END IF
            END DO
        END IF
    CLOSE(u)
    PRINT*, c,c1
    ELSE
        PRINT*,'It didnt work'
        STOP
    END IF
END PROGRAM test5