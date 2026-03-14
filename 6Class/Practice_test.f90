PROGRAM main
    USE module_sorter
    USE Practice
    IMPLICIT NONE
    REAL, DIMENSION(:), ALLOCATABLE :: data_array
    !REAL :: p98 
    INTEGER :: u = 10, ios
    INTEGER :: u1 = 20, ios1
    INTEGER(8) :: i
    OPEN(UNIT = u, IOSTAT = ios, FILE = 'sample.dat.txt', &
        STATUS = 'old', ACTION = 'read')
    OPEN(UNIT = u1, IOSTAT = ios1, FILE = 'result.dat.txt', &
        STATUS = 'old', ACTION = 'write')
    IF (ios /= 0 .OR. ios1 /= 0) STOP 'Error: file not opened'
    i = Nrows('sample.dat.txt')
    ALLOCATE(data_array(i))
    DO n = 1, i 
        READ(u,*) data_array(n)
    END DO
    CALL quicksort(data_array)
    !p98 = 0.98*i
    !PRINT*, 98*i/100
    WRITE(u1, '(A)') '# THIS IS COMPUTED FORM SAMPLE/DAT'
    WRITE(u1, '(A)') '#      MEAN       STD     98PCT'
    WRITE(u1, '(A)') '#aaaaaa.bbbcccccc.dddeeeeee.fff'
    !DO n = 1,i 
    !    WRITE(u1, '(F8.3)') data_array(n)
    !END DO
    WRITE(u1,'(F10.3,F10.3,F10.3)') mean(data_array),Var(data_array),data_array(98*i/100)
    !WRITE(u1,'(F10.3)') Var(data_array)
    !WRITE(u1, '(F10.3)') data_array(98*i/100)
    CLOSE(u)
    CLOSE(u1)
END PROGRAM main