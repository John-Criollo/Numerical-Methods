PROGRAM prueba
    USE NMF
    IMPLICIT NONE
    CHARACTER(LEN = 80) :: row
    REAL, DIMENSION(:,:), ALLOCATABLE :: mtrx
    INTEGER, DIMENSION(:), ALLOCATABLE :: ind
    REAL :: f
    CHARACTER :: ch
    INTEGER :: n,m, u = 10, ios, state
    !I was using the same u for both, module and program
    OPEN(UNIT = u, IOSTAT = ios, FILE = 'data1.txt', &
        STATUS = 'old', ACTION = 'read')
    PRINT*, Ncols('data1.txt')
    PRINT*, Nrows('data1.txt')
    ALLOCATE(mtrx(Nrows('data1.txt'),Ncols('data1.txt')))
    ALLOCATE(ind(Nrows('data1.txt')))
    n = 1
    m = 1
    DO
        READ(u,'(A1)',IOSTAT = state) ch
        IF (state /= 0) EXIT
        IF (ch /= '#') THEN
            ind(m) = n
            m = m + 1
        END IF
        n = n+1
    END DO
    PRINT*, ind
    REWIND(u)
    n = 1
    m = 1
    DO !Cycle is a command for skipping 
        IF (ind(n) == m) THEN
             READ(u,*,IOSTAT = state) mtrx(n,:)
             IF (state /= 0) EXIT
             n = n + 1
        ELSE
            READ(u,'(A1)',IOSTAT = state) ch 
            IF (state /= 0) EXIT
        END IF
        m = m + 1
    END DO
    PRINT*, mtrx
    !READ(u,*) row
    !IF (row(1:1) == '#')
    !PRINT*, mtrx(1,:)
    !PRINT*, row, LEN(row)
    !PRINT*,TRIM(row), LEN(TRIM(row))
    !READ(u,'(A)') row
    !PRINT*, row, LEN(row)
    !PRINT*,TRIM(row), LEN(TRIM(row))
    CLOSE(u)
END PROGRAM prueba
! gfortran -o test.exe NMF.f90 test.f90