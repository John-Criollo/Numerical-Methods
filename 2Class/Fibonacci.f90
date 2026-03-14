Program fibprog1
    implicit none
    integer :: x,y,z
    real :: phi_est
    integer :: n=3
    z = 1
    y = 1
  2 x = y + z
    phi_est = x/(1.0*y)
    print*,n, x, phi_est
    z = y
    y = x
    n = n + 1
    if (n<50) go to 2
!    DO n = 3, 50
!        x = y + z
!        phi_est = x/(1.0*y)
!        print*,'n:',n,'x:', x,'phi:', phi_est
!        z = y
!        y = x
!    END DO 
    end program fibprog1
