module numet
    implicit none
    contains
    subroutine statprint(select)
        integer, intent(in) :: select
        real(8),dimension(4):: x
        integer, dimension(5) :: counter
        real,dimension(5) :: frac
        real(8) :: tau, delta,cond
        integer(8) :: seed = 798277
        integer:: i, N = 1000000
        counter = (/0,0,0,0,0/)
        do i = 1, N
            IF (select == 1) THEN
                call random_number(x)
                x = 2*(x-0.5)
            ELSE IF (select == 2) THEN
                CALL msquare(x,seed)
            ELSE IF (select == 3) THEN
                CALL linconmet(x,seed)
            ELSE
                STOP 'Method not found'
            END IF
            tau = x(1)+x(4)
            delta = x(1)*x(4) -x(2)*x(3)
            cond = tau**2 - 4*delta
            if (cond>=0 .and. tau<0 .and. delta>0) then
                counter(1) = counter(1) + 1
            else if (cond >0 .and. tau>0 .and. delta>0) then
                counter(2) = counter(2) + 1
            else if (delta<0) then
                counter(3) = counter(3) + 1
            else if (cond<0 .and. tau<0) then
                counter(4) = counter(4) + 1
            else if (cond <0 .and. tau>0) then
                counter(5) = counter(5) + 1
            end if
        end do
        frac = 100*real(counter,4)/real(sum(counter),4)
        IF (select == 1) THEN
                print('(A)'), 'Using Built-in Function'
            ELSE IF (select == 2) THEN
                print('(A)'), 'Using Middle Square Method'
            ELSE IF (select == 3) THEN
                print('(A)'), 'Using Linear Congruential Method'
            ELSE
                STOP 'Method not found'
            END IF
        print('(A)'), 'Final result:'
        print ('(A,1x,f5.2,A)'), "Number of stable nodes:",frac(1),"%"
        print('(A,1x,f5.2,A)'), "Number of unstable nodes:",frac(2),"%"
        print('(A,1x,f5.2,A)'), "Number of saddle points:",frac(3),"%"
        print('(A,1x,f5.2,A)'), "Number of stable spirals:",frac(4),"%"
        print('(A,1x,f5.2,A)'), "Number of unstable spirals:",frac(5),"%"
        print('(A)'), "################"
    end subroutine
    subroutine msquare(x,seed)
        real(8),dimension(:),intent(inout) :: x
        integer :: i,N
        integer(8),intent(inout) :: seed
        N = size(x)
        do i = 1, N
            seed = seed**2
            seed = mod(seed/1000,1000000)
            !print*,seed
            x(i) = real(seed,8)/100000 - 5
        end do
    end subroutine msquare
    subroutine linconmet(x,seed)
        real(8),dimension(:),intent(inout) :: x
        integer(8),intent(inout) :: seed
        integer(8), dimension(:), allocatable :: inival
        integer :: i,N
        N = size(x)
        allocate(inival(N))
        inival(1) = seed
        x(1) = real(inival(1),8)/100000
        do i = 2, N
            inival(i) = mod(1664525*inival(i-1) + 1,1000000)
            x(i) = real(inival(i),8)/100000 - 5
        end do
        seed = inival(N)
        deallocate(inival)
    end subroutine linconmet
end module numet
