program main
    use numet
    IMPLICIT NONE
    REAL(8) :: t0,x0,v0, dt,tf
    REAL(8), DIMENSION(2) :: xi,vi
    REAL(8), DIMENSION(301):: x,vx,t
    REAL(8), DIMENSION(301)::x1,vx1
    REAL(8),DIMENSION(301,2) :: x2,vx2,x3,vx3
    INTEGER :: u1 = 10, ios1
    INTEGER :: u2 = 20, ios2
    INTEGER :: u3 = 30, ios3
    INTEGER :: u4 = 40, ios4
    INTEGER :: ios, N = 301,i
    OPEN(UNIT = u1, IOSTAT = ios1, FILE = 'euler.txt',&
        STATUS = 'replace', ACTION = 'write')
    OPEN(UNIT = u2, IOSTAT = ios2, FILE = 'verlet.txt',&
        STATUS = 'replace', ACTION = 'write')
    OPEN(UNIT = u3, IOSTAT = ios3, FILE = 'bonus.txt',&
        STATUS = 'replace', ACTION = 'write')
    OPEN(UNIT = u4, IOSTAT = ios4, FILE = 'bonus1.txt',&
        STATUS = 'replace', ACTION = 'write')
    IF (ios2 /= 0 .OR. ios1 /= 0 .OR. ios3 /= 0 .OR. ios4 /= 0) THEN
        PRINT*, '(a25)','Error: file not opened.'
    END IF
    t0 = 0
    x0 = 1
    v0 = 0
    dt = 0.1
    tf = 30
    call euler(t0,x0,v0,dt,tf,x,vx,t)
    WRITE(u1, '(A)') '#      TIME      X(T)      V(T)'
    WRITE(u1, '(A)') '#aaaaaa.bbbcccccc.dddeeeeee.fff'
    DO i = 1,N
        WRITE(u1,'(F10.3,F10.3,F10.3)') t(i),x(i),vx(i)
    end do
    call verlet(t0,x0,v0,dt,tf,x1,vx1,t)
    WRITE(u2, '(A)') '#      TIME      X(T)      V(T)'
    WRITE(u2, '(A)') '#aaaaaa.bbbcccccc.dddeeeeee.fff'
    DO i = 1,N
        WRITE(u2,'(F10.3,F10.3,F10.3)') t(i),x1(i),vx1(i)
    END DO
    xi = (/2 , 0/)
    vi = (/0.0 , 0.5/)
    call euler1(t0,xi,vi,dt,tf,x2,vx2,t)
    WRITE(u3, '(A)') '#     X1(T)     X2(T)'
    WRITE(u3, '(A)') '#cccccc.dddeeeeee.fff'
    DO i = 1,N
        WRITE(u3,'(F10.3,F10.3)') x2(i,:)
    END DO
    call verlet1(t0,xi,vi,dt,tf,x3,vx3,t)
    WRITE(u4, '(A)') '#     X1(T)     X2(T)'
    WRITE(u4, '(A)') '#cccccc.dddeeeeee.fff'
    DO i = 1,N
        WRITE(u4,'(F10.3,F10.3)') x3(i,:)
    END DO
    close(u1)
    close(u2)
    close(u3)
    close(u4)
end program main
