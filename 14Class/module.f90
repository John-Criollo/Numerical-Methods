MODULE numet
    IMPLICIT NONE
    CONTAINS
    REAL(8) FUNCTION f(x) RESULT(res)
        IMPLICIT NONE
        REAL(8), INTENT(IN):: x 
        res = -SIN(x)
    END FUNCTION f
    FUNCTION f1(x) RESULT(res)
        IMPLICIT NONE
        REAL(8),DIMENSION(2) :: res
        REAL(8),DIMENSION(2), INTENT(IN):: x 
        res(:) = -x(:)/(SQRT(x(1)**2 + x(2)**2))**3
    END FUNCTION f1
    subroutine euler(t0,x0,v0,dt,tf,x,vx,t)
    real(8), intent(in) :: t0,tf,dt,x0,v0
    real(8),dimension(:),intent(inout) :: x,vx,t
    integer :: N = 301,i
    x(1) = x0
    vx(1) = v0
    t(1) = t0
    t = t0
    do i = 2, N
        x(i) = x(i-1) + vx(i-1) * dt
        vx(i) = vx(i-1) + f(x(i-1))*dt
        t(i) = t(i-1) + dt
    end do
    end subroutine euler
    subroutine verlet(t0,x0,v0,dt,tf,x,vx,t)
    real(8), intent(in) :: t0,tf,dt,x0,v0
    real(8),dimension(:),intent(inout) :: x,vx,t
    integer :: N = 301,i! N = tf/dt +1
    x(1) = x0
    vx(1) = v0
    t(1) = t0
    t = t0
    do i = 2, N
        vx(i) = vx(i-1) + f(x(i-1))*dt/2
        x(i) = x(i-1) + vx(i) * dt
        vx(i) = vx(i) + f(x(i))*dt/2
        t(i) = t(i-1) + dt
    end do
    end subroutine verlet
    subroutine euler1(t0,xi,vi,dt,tf,x,vx,t)
    real(8), intent(in) :: t0,tf,dt
    REAL(8), DIMENSION(:), INTENT(IN) :: xi,vi
    real(8),dimension(:),intent(inout) :: t
    real(8),dimension(:,:), intent(inout) :: x,vx
    integer :: N = 301,i
    x(1,:) = xi
    vx(1,:) = vi
    t(1) = t0
    t = t0
    do i = 2, N
        x(i,:) = x(i-1,:) + vx(i-1,:) * dt
        vx(i,:) = vx(i-1,:) + f1(x(i-1,:))*dt
        t(i) = t(i-1) + dt
    end do
    end subroutine euler1
    subroutine verlet1(t0,xi,vi,dt,tf,x,vx,t)
    real(8), intent(in) :: t0,tf,dt
    REAL(8), DIMENSION(:), INTENT(IN) :: xi,vi
    real(8),dimension(:),intent(inout) :: t
    real(8),dimension(:,:), intent(inout) :: x,vx
    integer :: N = 301,i
    x(1,:) = xi
    vx(1,:) = vi
    t(1) = t0
    t = t0
    do i = 2, N
        vx(i,:) = vx(i-1,:) + f1(x(i-1,:))*dt/2
        x(i,:) = x(i-1,:) + vx(i,:) * dt
        vx(i,:) = vx(i,:) + f1(x(i,:))*dt/2
        t(i) = t(i-1) + dt
    end do
    end subroutine verlet1
END MODULE numet
