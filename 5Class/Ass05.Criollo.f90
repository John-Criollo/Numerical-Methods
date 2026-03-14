PROGRAM num5
    USE NMF
    IMPLICIT NONE
    REAL, DIMENSION(:,:), ALLOCATABLE :: mtrx
    !REAL, DIMENSION(:), ALLOCATABLE :: arr_res
    CHARACTER(LEN = 80) :: ch
    INTEGER :: i,j
    INTEGER :: n,m, u=10,u1=20 , ios,ios1, state
    !INTEGER, DIMENSION(:), ALLOCATABLE :: ind
    i = Nrows('data.txt')
    j = Ncols('data.txt')
    ALLOCATE(mtrx(i,j))
    !ALLOCATE(ind(i))
    !ALLOCATE(arr_res(j))
    !PRINT*,i,j
    OPEN(UNIT = u, IOSTAT = ios, FILE = 'data.txt', &
         STATUS = 'old', ACTION = 'read')
    OPEN(UNIT = u1, IOSTAT = ios1, FILE = 'result.txt',&
        STATUS = 'replace', ACTION = 'write')
    IF (ios /= 0 .OR. ios1 /= 0) THEN
        PRINT*, '(a25)','Error: file not opened.'
    END IF
    n = 1
    m = 1
    DO m = 1, i
        READ(u,'(a80)', IOSTAT = state) ch
        IF (state /= 0) EXIT
        IF ( ch(1:1) /= '#' ) THEN
            READ(ch,*,IOSTAT=state) mtrx(m,1:j)
            IF (state /= 0) EXIT
        ELSE
            CONTINUE
        END IF
    END DO
    REWIND(u)
    n = 1
    m = 1
    !DO n = 1, j
    !    arr_res(n) = Average(mtrx(:,n))
    !END DO
    !WRITE(u1,'(F8.2,1X)') (arr_res(n), n = 1, j)
    WRITE(u1,*) (Average(mtrx(:,n)), n=1,j) !Found on the internet
    close(u)
    close(u1)
END PROGRAM num5
!gfortran -o Ass05.exe NMF.f90 Ass05.Criollo.f90 
!   289.735626    
!   289.735596    
!   289.917358    