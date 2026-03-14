SUBROUTINE mtrxp(mat,row,col)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: row,col
    REAL, DIMENSION(row,col), INTENT(IN) :: mat
    INTEGER :: in1, in2
    DO in1 = 1, row
        WRITE(*,'(100F8.2)') (mat(in1, in2), in2=1, col) !Found on the internet
    END DO
END SUBROUTINE mtrxp
PROGRAM test
    USE functions
    IMPLICIT NONE
    REAL, DIMENSION(8) :: v1
    REAL, DIMENSION(2) :: res,res1 , res2 , res3
    REAL, DIMENSION(2,2) :: mtrx1
    REAL, DIMENSION(3,3) :: mtrx2
    REAL, DIMENSION(:), ALLOCATABLE :: temp(:), mtrx(:,:)
    INTEGER :: i,j,option 
    PRINT*, 'Assigment 4'
    PRINT*, 'Testing the code!'
    v1 = (/ 2 , 4 , 4 , 4 , 5 , 5 , 7 , 9/)
    mtrx1 = RESHAPE((/3 ,  7 , 1 , -4/),(/2,2/))
    mtrx2 = RESHAPE((/-2 ,-1 , 2 , 2 , 1 , 4 , -3 , 3 ,-1/),(/3,3/),ORDER = (/2,1/))
    res1 = funa(v1)
    res2 = funb(mtrx1)
    res3 = funb(mtrx2)
    PRINT*, 'Array:',v1
    PRINT*, 'Average =',res1(1),'Standard deviation =',res1(2)
    PRINT*, 'Matrix 2x2:'!, mtrx1
    CALL mtrxp(mtrx1,2,2)
    PRINT*, 'Trace =',res2(1) ,'Determinant =',res2(2)
    PRINT*, 'Matrix 3x3:'!, mtrx2
    CALL mtrxp(mtrx2,3,3)
    PRINT*, 'Trace =',res3(1), 'Determinant =',res3(2)
    !PRINT*, 'Results:'
    !PRINT*, 'Array: Average =',res1(1),'Standard deviation =',res1(2)
    !PRINT*, 'Matrix 2x2: Trace =',res2(1) ,'Determinant =',res2(2)
    !PRINT*, 'Matrix 3x3: Trace =',res3(1), 'Determinant =',res3(2)
    PRINT*, 'Now you can try yourself!'
  2 PRINT*, 'What do you want to compute?'
    PRINT*, '[1] Average and standard deviation of a real value array'
    PRINT*, '[2] Trace and determinant of any 2x2 and 3x3 real value matrix'
    READ*, option
    IF (option == 1) THEN
        PRINT*, 'Insert the length of the array:'
        READ*, i
        ALLOCATE(temp(i))
        PRINT*, 'Insert the array:'
        READ(*,*) temp
        res = funa(temp)
        PRINT*, 'Average =',res(1), 'Standard deviation =', res(2)
    ELSE IF (option == 2) THEN
        PRINT*, "Introduce the order of the square matrix:"
        !PRINT*, "Insert number of rows:"
  3     READ*, i
        j = i
        !PRINT*, "Insert number of columns:"
        !READ*, j
        IF (i == j .AND. (i == 2 .OR. i == 3)) THEN
            ALLOCATE(temp(i*j))
            ALLOCATE(mtrx(i,j))
            PRINT*, 'Now insert the matrix:'
            READ(*,*) temp
            mtrx = RESHAPE(temp,(/i,j/),ORDER = (/2,1/))
            PRINT*, 'Your matrix:'
            CALL mtrxp(mtrx,i,j)
            res = funb(mtrx)
            PRINT*, 'Trace = ', res(1) ,'Determinant =', res(2)
        ELSE
            PRINT*, 'This program is just for 2x2 or 3x3 matrices!' !'Your matrix is not valid, you must insert a 2x2 or 3x3 matrix!'
            PRINT*, 'Please introduce the order of the square matrix again:'
            GO TO 3
        END IF
        ELSE
            PRINT*, 'You must insert a valid option!'
            GO TO 2
    END IF
END PROGRAM test
