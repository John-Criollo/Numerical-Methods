PROGRAM main
    USE practice
    IMPLICIT NONE
    REAL(8), DIMENSION(2,2) :: Jprod,var
    REAL(8), DIMENSION(2) :: beta,dbeta
    REAL(8), DIMENSION(:,:), ALLOCATABLE :: data,J,Jt
    REAL(8), DIMENSION(:), ALLOCATABLE :: r,error
    REAL(8) :: modbeta, alpha, err = 1.0e-6,hkb = 14387.77,verror
    INTEGER :: n,m,i,k,maxIter = 1000
    INTEGER :: u1 = 10, ios1,u2 = 20, ios2 ,ios
    OPEN(UNIT = u1, IOSTAT = ios1, FILE = 'sun_data.txt', &
        STATUS = 'old', ACTION = 'read')
    OPEN(UNIT = u2, IOSTAT = ios2, FILE = 'fit.txt',&
        STATUS = 'replace', ACTION = 'write')
    IF (ios2 /= 0 .OR. ios1 /= 0) THEN
        PRINT*, '(a25)','Error: file not opened.'
    END IF
    n = Nrows('sun_data.txt')
    m = Ncols('sun_data.txt')
    ALLOCATE(data(n,m))
    ALLOCATE(J(n,m))
    ALLOCATE(r(n))
    ALLOCATE(error(n))
    DO i = 1,n
        READ(u1,*) data(i,:)
    END DO
    beta(1) = 1.0
    beta(2) = 1.0
    alpha = 0.1
    DO k = 1,maxIter
        DO i = 1,n 
            r(i) = data(i,2) - fitfun(data(i,1),beta(1),beta(2))
            J(i,1) = dfun1(data(i,1),beta(1),beta(2))
            J(i,2) = dfun2(data(i,1),beta(1),beta(2))
        END DO
        Jprod = MATMUL(TRANSPOSE(J),J)
        call inv2x2(Jprod)
        dbeta = MATMUL(Jprod, MATMUL(TRANSPOSE(J), r))
        modbeta = SUM(dbeta**2)
        beta = beta + alpha*dbeta
        IF (modbeta <= err) THEN
            !PRINT*, 'Number of iterations:',k
            write(u2,'(A10,1x,i10)') 'N.Iteration:', k
            EXIT
        END IF
    END DO
    DO i = 1,n
        error(i) = data(i,2) - fitfun(data(i,1),beta(1),beta(2))
    END DO
    verror = sum(error**2)/(n-2)
    DO i = 1,n 
            r(i) = data(i,2) - fitfun(data(i,1),beta(1),beta(2))
            J(i,1) = dfun1(data(i,1),beta(1),beta(2))
            J(i,2) = dfun2(data(i,1),beta(1),beta(2))
    END DO
    Jprod = MATMUL(TRANSPOSE(J),J)
    call inv2x2(Jprod)
    var = verror * Jprod
    !print*, beta
    !print*, verror
    !print*,sqrt(var(1,1)), sqrt(var(2,2))
    write(u2,'(A16)') 'Computed values:'
    write(u2,'(A6,1x,f10.2)') 'Beta1:', beta(1)
    write(u2,'(A6,1x,f10.2)') 'Beta2:', beta(2)
    write(u2,'(A10,1x,f10.5)') 'Var(error):', verror
    write(u2,'(A10,1x,f10.5)') 'Var(beta1):', sqrt(var(1,1))
    write(u2,'(A10,1x,f10.5)') 'Var(beta2):', sqrt(var(2,2))
    write(u2,'(A13,1x,f10.5)') 'Temperature:', hkb/beta(2)
    close(u1)
    close(u2)
end program main
