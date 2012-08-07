 SUBROUTINE GUESS3(X1,Y1,X2,Y2,SUBA,DX,TOL,IERROR)
      implicit none

      INTEGER IERR, ICOUNT, IERROR
      REAL SIGN, SLOPE
      REAL X1, X2, Y1, Y2, DX, TOL
      REAL YMIN, DDX
      REAL SUBA
        EXTERNAL SUBA
      CHARACTER(LEN=111),PARAMETER :: FMT_1001 = "(' GUESS3: ** FAILED TO BRACKET A SOLUTION **',/,9X,'F(',1PE12.4,') = ',1PE13.5,5X,'F(',1PE12.4,') = ',1PE13.5)"

      ICOUNT = 0
      IERROR = 0
      SIGN = 1.0
      SLOPE = 0.0

      DO WHILE (.TRUE.)
          Y1 = SUBA(X1,IERR)
          X2 = X1                                !Initilize ISI - 03/26/04
          ICOUNT = ICOUNT + 1
          IF (IERR .EQ. 0) THEN
              EXIT
          END IF
          IF (IERR .EQ. 1) THEN                  !ISI - 03/26/04
              X1 = X1 - DX                         !ISI - 03/26/04
          ELSE                                   !ISI - 03/26/04
              X1 = X1 + DX                         !ISI - 03/26/04
          END IF                                 !ISI - 03/26/04
          
          IF (ICOUNT .GT. 30) THEN
              IERROR = 4
              RETURN
          END IF

      END DO

      YMIN = ABS(Y1)
      IF (YMIN .LE. TOL) THEN
          RETURN              !ISI - 02/12/06
      END IF
      IF(Y1.GT.0.0) THEN
          SIGN = -1.0
      END IF
      DDX = ABS(DX)
      X2 = X2 + 2.0**(ICOUNT-1)*DX*SIGN      !To bracket root ISI - 03/26/04
      
      DO WHILE (.TRUE.)

          Y2 = SUBA(X2,IERR)          !RS Comment: The compressor saturation temperature seems to be iterating the wrong way with the MC case
          ICOUNT = ICOUNT + 1
          SLOPE = (Y2 - Y1)/(X2 - X1)
          IF (IERR .NE. 0) THEN 
              X2 = (X1 + X2)/2.
              IF (ICOUNT .GT. 15) THEN
                  IERROR = 4
                  RETURN
              END IF
              
              CYCLE
          END IF

          YMIN = AMIN1(YMIN,ABS(Y2))
          IF (YMIN .LE. TOL) THEN
              RETURN              !ISI - 02/12/06
          END IF
          DDX = ABS(DX)
          DDX=2.0**(ICOUNT-1)*ABS(DX)				!ISI - 05/10/04
!
          IF (Y1*Y2 .LE. 0.) THEN
              RETURN
          END IF
          IF (ICOUNT .GT. 15) THEN
              IERROR = 4
              RETURN
          END IF
!
!	IF SLOPE IS POSITIVE AND "Y" IS GREATER THAN 0. OR
!	IF SLOPE IS NEGATIVE AND "Y" IS LESS THAN 0. MOVE THE LOWER
!	POINT TO THE LEFT
!
          SLOPE = (Y2 - Y1)/(X2 - X1)
          IF (SLOPE*Y1 .GE. 0.) THEN
!
!	MAKE "X1" THE LOWER OR LEFT-HAND POINT
!
              IF (X1 .GE. X2) THEN
                  X1 = X2
                  Y1 = Y2
              END IF
              
              X2 = X1 - DDX
              CYCLE

          END IF

!	IF THERE IS A NEGATIVE SLOPE AND "Y" IS GREATER THAN 0. OR
!	THERE IS A POSITIVE SLOPE AND "Y" IS LESS THAN 0. THEN
!	MOVE THE RIGHT-HAND POINT FURTHER TO THE RIGHT
!
          IF (X1 .LE. X2) THEN 
!
!	MAKE X1 THE UPPER POINT
!
              X1 = X2
              Y1 = Y2
          END IF

          X2 = X1 + DDX
          CYCLE   !VL: Not needed ...

      END DO

 END SUBROUTINE
