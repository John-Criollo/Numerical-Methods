REAL FUNCTION pi_madhava(m) RESULT(pi)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: m
    INTEGER :: n
    pi = 4.0
    DO n = 1, m
        pi = pi + 4*((-1.0)**n)/(2.0*n + 1.0)
    END DO
END FUNCTION

PROGRAM pinum
    IMPLICIT NONE
    REAL :: pi_val, c
    INTEGER :: m, n
    REAL, EXTERNAL :: pi_madhava
    PRINT*, 'Assigment 3!'
    PRINT*, '1.Compute the value of pi using the Madhava aproximation?'
  2 PRINT*, 'Please insert m:'
    READ*, m
    IF (m<0 .OR. m == 0) THEN
        PRINT*, 'Insert a valid number!'
        GO TO 2
    ELSE 
        !DO n = 1, m
            !pi_val = pi_madhava(n)
            !PRINT*,pi_val
        !END DO
        pi_val = pi_madhava(m)
    END IF
    PRINT*, 'Your pi is:', pi_val
    PRINT*, '2. Increase the value of m and repeat until the condition is fullfiled'
    n = 1
  3 c = ABS(ATAN(1.0) - 0.25*pi_madhava(n))
    IF (c > 1.0e-3) THEN
        n = n+1
        GO TO 3
    ELSE
        PRINT*, 'The final values are m =', n, 'pi=', pi_madhava(n)
    END IF
    PRINT*, 'Bonus question!'
    PRINT*, 'We try changing the restriction to 1.0e-6'
    !n = 1 
  !4 c = ABS(ATAN(1.0) - 0.25*pi_madhava(n))
    !IF (c > 1.0e-6) THEN
    !    n = n+1
    !    GO TO 4
    !ELSE
    !    PRINT*, 'The final values are m=',n, 'pi=', pi_madhava(n)
    !END IF
    PRINT*, 'We will see that the problem get stuck because it cannot reach the condition so it will keep iterating'
    PRINT*, 'This problem is related to the bits assigned to the numbers so it is posible to reach if we change the'
    PRINT*, 'number of bits assigned to the real numbers.'         
END PROGRAM pinum
 
