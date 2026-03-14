program main
    use numet
    implicit none
    real(8) :: x
    integer::i, N = 10000
    INTEGER :: u1 = 10, ios1
    OPEN(UNIT = u1, IOSTAT = ios1, FILE = 'histo.txt',&
    STATUS = 'replace', ACTION = 'write')
    IF (ios1 /= 0) THEN
        PRINT*, '(a25)','Error: file not opened.'
    END IF
    print('(A)'), "Assignment 16"
    print('(A)'), "1) Estimate the value of pi using 'Hit-or-Miss'" 
    call pi_est
    print('(A)'), "2)Generate and plot a histogram of the distribuition 3x^2"
    Do i = 1, N
       call reject(x)
        write(u1,*) x
    end do
    close(u1)
    print('(A)'), "3) Estimate the integral of 3x^2 over [0,3] using 'H&M'"
    call int_est()
    print('(A)'), "4)Estimate the integral of e^-x^2 using crude monte-carlo"
    call crudemon()
    print('(A)'), "Bonus: Estimate pi using 'Crude Monte-Carlo'"
    call pi_cmc()
end program main 
