PROGRAM main
    IMPLICIT NONE
    INTEGER :: i,j,k,fintraj
    INTEGER :: u1 = 10, ios1
    INTEGER :: u2 = 20, ios2
    INTEGER :: u3 = 30, ios3
    INTEGER :: u4 = 40, ios4
    REAL(8) :: x, mean,std
    REAL :: walk
    REAL(8), DIMENSION(10) :: walks
    INTEGER,DIMENSION(10) :: trajectories
    !OPEN(UNIT = u1, IOSTAT = ios1, FILE = 'histo1.txt',&
    !STATUS = 'replace', ACTION = 'write')
    !OPEN(UNIT = u2, IOSTAT = ios2, FILE = 'histo2.txt',&
    !STATUS = 'replace', ACTION = 'write')
    OPEN(UNIT = u3, IOSTAT = ios3, FILE = 'histo3.txt',&
    STATUS = 'replace', ACTION = 'write')
    !OPEN(UNIT = u4, IOSTAT = ios4, FILE = 'histo4.txt',&
    !STATUS = 'replace', ACTION = 'write')
    !IF (ios1 /= 0 .OR. ios2 /= 0 .OR. ios3 /= 0 .OR. ios4 /= 0) THEN
    !    PRINT*, '(a25)','Error: file not opened.'
    !END IF
    !#First part of the assignment
    !DO i = 1, 1000
    !    mean = 0
    !    std = 0
    !    DO j = 1, 10000
    !        CALL random_number(x)
    !        mean = mean + x
    !        std = std + (x - 0.5)**2
    !    END DO
    !    mean = mean/10000
    !    std = SQRT(std/10000)
    !    WRITE(u1,*) mean, std
    !END DO
    !#Second part of the assignment
    !DO i = 1, 10
    !    trajectories(i) = 0
    !END DO
    !DO j = 1,1000
    !call random_number(walks)
    !DO K = 1, 10
    !    IF (walks(k)>= 0.5) THEN
    !        trajectories(k) = trajectories(k) + 1
    !    ELSE
    !        trajectories(k) = trajectories(k) - 1
    !    END IF
    !END DO
    !WRITE(u2,*) j, trajectories
    !END DO
    !Third part of the assignment
    DO i = 1, 100000
        fintraj = 0
        DO j = 2,1000
            call random_number(walk)
            IF (walk > 0.5) THEN
                fintraj = fintraj + 1
            ELSE
                fintraj = fintraj - 1
            END IF
        END DO
        WRITE(u3,*) fintraj
    END DO
    !CLOSE(u1)
    !CLOSE(u2)
    CLOSE(u3)
    !CLOSE(u4)
END PROGRAM main