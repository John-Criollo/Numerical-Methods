PROGRAM main
    USE Practice
    IMPLICIT NONE
    REAL(8), DIMENSION(:,:), ALLOCATABLE :: data_array, data_array1
    !REAL :: p98 
    INTEGER :: u1 = 10, ios1
    INTEGER :: u2 = 20, ios2
    INTEGER :: u3 = 30, ios3
    INTEGER :: u4 = 40, ios4
    INTEGER :: n,i,j,i1,j1
    REAL(8), DIMENSION(10) :: tval = (/1.0,12.0,23.0,34.0,45.0,56.0,67.0,78.0,89.0,100.0/)
    OPEN(UNIT = u1, IOSTAT = ios1, FILE = 'input_data.dat', &
        STATUS = 'old', ACTION = 'read')
    OPEN(UNIT = u2, IOSTAT = ios2, FILE = 'output_data.dat', &
        STATUS = 'old', ACTION = 'write')
    IF (ios1 /= 0 .OR. ios2 /= 0) STOP 'Error: file not opened'
    OPEN(UNIT = u3, IOSTAT = ios3, FILE = 'input_data1.dat', &
        STATUS = 'old', ACTION = 'read')
    OPEN(UNIT = u4, IOSTAT = ios4, FILE = 'output_data1.dat', &
        STATUS = 'old', ACTION = 'write')
    IF (ios3 /= 0 .OR. ios4 /= 0) STOP 'Error: file not opened'
    i = Nrows('input_data.dat')
    j = Ncols('input_data.dat')
    ALLOCATE(data_array(i,j))
    i1 = Nrows('input_data1.dat')
    j1 = Ncols('input_data1.dat')
    ALLOCATE(data_array1(i1,j1))
    DO n = 1, i 
        READ(u1,*) data_array(n,:)
    END DO
    DO n = 1, i1 
        READ(u3,*) data_array1(n,:)
    END DO
    DO n = 1, 10
        WRITE(u2, '(F4.0,1x,F6.2)') tval(n), Interpol(tval(n),data_array)
        WRITE(u4, '(F4.0,1x,F6.2)') tval(n), Linterpol(tval(n),data_array1)
    END DO
    CLOSE(u1)
    CLOSE(u2)
    CLOSE(u3)
    CLOSE(u4)
END PROGRAM main