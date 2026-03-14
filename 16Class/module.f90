module numet
    IMPLICIT NONE
    contains
    REAL(8) function invg1(x) RESULT(res)
        IMPLICIT NONE
        REAL(8), INTENT(IN) :: x
        res = -1+2/(2-x)
    end function invg1
    REAL(8) function invg2(x) RESULT(res)
        IMPLICIT NONE
        REAL(8), INTENT(IN) :: x
        res = x**(0.334)
    end function invg2
    SUBROUTINE tmethod1
        REAL(8) :: x,avg,m = 1.0, s = 4.0
        REAL(8),DIMENSION(10) :: bins
        INTEGER :: u, ios
        INTEGER :: N = 1000000,i
        INTEGER, DIMENSION(10):: counter
        OPEN(NEWUNIT = u, IOSTAT = ios, FILE = 'list1.txt',&
        STATUS = 'replace', ACTION = 'write')
        IF (ios /= 0) THEN
            PRINT*, '(a25)','Error: file not opened.'
        END IF
        counter = (/0,0,0,0,0,0,0,0,0,0/)
        bins = (/0.05,0.15,0.25,0.35,0.45,0.55,0.65,0.75,0.85,0.95/)
        DO i = 1, N
            CALL random_number(x)
            x = -1+2/(2-x)
            WRITE(u,*) x
            IF (x>0 .and. x<0.1) THEN
                counter(1) = counter(1) + 1
            ELSE IF (x>0.1 .and. x<0.2) THEN
                counter(2) = counter(2) + 1
            ELSE IF (x>0.2 .and. x<0.3) THEN
                counter(3) = counter(3) + 1
            ELSE IF (x>0.3 .and. x<0.4) THEN
                counter(4) = counter(4) + 1
            ELSE IF (x>0.4 .and. x<0.5) THEN
                counter(5) = counter(5) + 1
            ELSE IF (x>0.5 .and. x<0.6) THEN
                counter(6) = counter(6) + 1
            ELSE IF (x>0.6 .and. x<0.7) THEN
                counter(7) = counter(7) + 1
            ELSE IF (x>0.7 .and. x<0.8) THEN
                counter(8) = counter(8) + 1
            ELSE IF (x>0.8 .and. x<0.9) THEN
                counter(9) = counter(9) + 1
            ELSE IF (x>0.9 .and. x<1) THEN
                counter(10) = counter(10) + 1
            END IF
        END DO
        CLOSE(u)
        avg = sum(counter*bins)/N
        PRINT('(A)'), "First Part"
        PRINT('(A,f5.3)'), "The average of the distribution:", avg
    END SUBROUTINE tmethod1
    SUBROUTINE tmethod2
        REAL(8) :: x,avg,m = 1.0, s = 4.0
        REAL(8),DIMENSION(10) :: bins
        INTEGER :: u, ios
        INTEGER :: N = 1000000,i
        INTEGER, DIMENSION(10):: counter
        OPEN(NEWUNIT = u, IOSTAT = ios, FILE = 'list2.txt',&
        STATUS = 'replace', ACTION = 'write')
        IF (ios /= 0) THEN
            PRINT*, '(a25)','Error: file not opened.'
        END IF
        counter = (/0,0,0,0,0,0,0,0,0,0/)
        bins = (/0.05,0.15,0.25,0.35,0.45,0.55,0.65,0.75,0.85,0.95/)
        DO i = 1, N
            CALL random_number(x)
            x = x**(0.334)
            WRITE(u,*) x
            IF (x>0 .and. x<0.1) THEN
                counter(1) = counter(1) + 1
            ELSE IF (x>0.1 .and. x<0.2) THEN
                counter(2) = counter(2) + 1
            ELSE IF (x>0.2 .and. x<0.3) THEN
                counter(3) = counter(3) + 1
            ELSE IF (x>0.3 .and. x<0.4) THEN
                counter(4) = counter(4) + 1
            ELSE IF (x>0.4 .and. x<0.5) THEN
                counter(5) = counter(5) + 1
            ELSE IF (x>0.5 .and. x<0.6) THEN
                counter(6) = counter(6) + 1
            ELSE IF (x>0.6 .and. x<0.7) THEN
                counter(7) = counter(7) + 1
            ELSE IF (x>0.7 .and. x<0.8) THEN
                counter(8) = counter(8) + 1
            ELSE IF (x>0.8 .and. x<0.9) THEN
                counter(9) = counter(9) + 1
            ELSE IF (x>0.9 .and. x<1) THEN
                counter(10) = counter(10) + 1
            END IF
        END DO
        CLOSE(u)
        avg = sum(counter*bins)/N
        PRINT('(A)'), "Second Part"
        PRINT('(A,f5.3)'), "The average of the distribution:", avg
    END SUBROUTINE tmethod2
    SUBROUTINE BoxMuller(m,s2)
        REAL(8), INTENT(IN) :: m,s2
        REAL(8) :: u1,u2,pi
        REAL(8), DIMENSION(2) :: z,x
        INTEGER :: u, ios
        INTEGER :: N = 10000,i
        INTEGER, DIMENSION(10):: counter
        OPEN(NEWUNIT = u, IOSTAT = ios, FILE = 'list3.txt',&
            STATUS = 'replace', ACTION = 'write')
        IF (ios /= 0) THEN
            PRINT*, '(a25)','Error: file not opened.'
        END IF
        pi = DACOS(-1.D0)
        DO i = 1,N
            CALL random_number(u1)
            CALL random_number(u2)
            z(1) = SQRT(-2*LOG(u1)) * COS(2*pi*u2)
            z(2) = SQRT(-2*LOG(u1)) * SIN(2*pi*u2)
            !Generalize for a mean m and variance s^2, standard deviation s
            x = SQRT(s2)*z + m
            WRITE(u,*) x(1)
            WRITE(u,*) x(2)
        END DO
        CLOSE(u)
    END SUBROUTINE BoxMuller
end module numet
