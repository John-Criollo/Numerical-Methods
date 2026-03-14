PROGRAM Lucas
    IMPLICIT NONE
    INTEGER :: nmax,n
    INTEGER :: L0,L1,Ln
    L0 = 2
    L1 = 1
   2 PRINT*, 'Which Luca number would you like to find?:'
    READ*, nmax
    IF (nmax>0) THEN
        IF(nmax == 1) THEN 
            PRINT*,'Your Lucas number is:', L1
        ELSE
            DO n = 2,nmax
                Ln = L1 + L0
                L0 = L1
                L1 = Ln
                !We print a large value to find at wich number we have a problem
                !PRINT*,n,L0,L1,Ln 
            END DO
            PRINT*,'Your Lucas number is:', Ln
        END IF
    ELSE
        PRINT*, 'Error: Your number is out of range'
        PRINT*, 'Please insert a valid number'
        GO TO 2
        !STOP
    END IF
END PROGRAM Lucas 
!In the case we work with integers, the code breaks on number nmax = 45, L46 turns negative.
!But in the case we work with real numbers, we can reach higher values but they change to scientific notation
!and can get the same problem when we reach larger values.
!This happens because one bit of the number we are using represents if the number is positive or negative,
!and when we reach a high value this bit is changed so the number turns negative.
!To avoid this we can expand the number of bits assigned to a number.
