program main
    use practice
    implicit none
    REAL(8), DIMENSION(2,2) :: A 
    REAL(8), dimension(2) :: b,x
    REAL(8), DIMENSION(:,:), ALLOCATABLE :: q !speed,distance
    real(8) :: sres,stot, r2,var,se1,se2,xmean,betamean
    INTEGER :: n,m,i
    INTEGER :: u1 = 10, ios1,u2 = 20, ios2
    OPEN(UNIT = u1, IOSTAT = ios1, FILE = 'hubble_data.txt', &
        STATUS = 'old', ACTION = 'read')
    OPEN(UNIT = u2, IOSTAT = ios2, FILE = 'fit.txt',&
        STATUS = 'replace', ACTION = 'write')
    IF (ios2 /= 0 .OR. ios1 /= 0) THEN
        PRINT*, '(a25)','Error: file not opened.'
    END IF
    n = Nrows('hubble_data.txt')
    m = Ncols('hubble_data.txt')
    allocate(q(n,m))
    DO i = 1, n 
        READ(u1,*) q(i,:)
    END DO
    A(1,1) = size(q,1)
    a(1,2) = sum(q(:,2))
    a(2,1) = sum(q(:,2))
    a(2,2) = sum(q(:,2)**2)
    b(1) = sum(q(:,1))
    b(2) = sum(q(:,1)*q(:,2))
    call cramer(a,b,x)
    sres = sum((q(:,1) - (x(1) + x(2)*q(:,2)))**2)
    stot = sum((q(:,1) - mean(q(:,1)))**2)
    xmean = mean(q(:,2))
    r2 = 1 - sres/stot
    var = sum((q(:,1) - (x(1) + x(2) * q(:,2)))**2/(n-2))
    se1 = sqrt(var)*sqrt( 1/n + xmean**2/sum( ( q(:,2) - xmean )**2 ))
    se2 = sqrt(var)/sqrt(sum( ( q(:,2) - xmean )**2 ))
    
    write(u2,*) 'Computed values:'
    write(u2,'(A10,1x,f10.2)') 'Beta1:', x(1)
    write(u2,'(A10,1x,f10.2)') 'Beta2:', x(2)
    write(u2,'(A10,1x,f10.2)') 'R2:', r2
    write(u2,'(A10,1x,f10.2)') 'SD1:', se1
    write(u2,'(A10,1x,f10.2)') 'SD2:', se2
    write(u2,*) ' In the case we have Vr(d) = beta*d'
    betamean = mean(q(:,1)/q(:,2))
    write(u2,'(A10,1x,f10.2)') 'Beta:', betamean
    close(u1)
    close(u2)
    
end program main
