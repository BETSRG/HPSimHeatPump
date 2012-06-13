      SUBROUTINE GUESS1(X1,Y1,X2,Y2,SUBA,DX,TOL,IERROR)
C
C	COMMON / EXPAND / IPRINT, JPRINT
C
      INTEGER IERR

      ICOUNT = 0
      IERROR = 0
      SIGN = 1.0
      SLOPE = 0.0
C
!VL: Previously : 50    Y1 = SUBA(X1,IERR)\
      DO WHILE (.TRUE.)
          Y1 = SUBA(X1,IERR)
          X2 = X1                                !Initilize ISI - 03/26/04
          ICOUNT = ICOUNT + 1
          !VL: Previously: IF (IERR .EQ. 0) GO TO 75
          IF (IERR .EQ. 0) EXIT
          IF (IERR .EQ. 1) THEN                  !ISI - 03/26/04
              !X1 = X1 - 5.0                       !ISI - 03/26/04
              X1 = X1 - DX                         !ISI - 03/26/04
          ELSE                                   !ISI - 03/26/04
              !X1 = X1 + 5.0                       !ISI - 03/26/04
              X1 = X1 + DX                         !ISI - 03/26/04
          END IF                                 !ISI - 03/26/04
          IF (ICOUNT .GT. 30) THEN 
              !VL: Previously : GO TO 999
              IERROR = 4
              RETURN
          END IF
          !VL: Previously: GO TO 50
      END DO
C
!VL: PreviouslyL: 75    YMIN = ABS(Y1)
      YMIN = ABS(Y1)
      !IF (YMIN .LE. TOL/2.) GO TO 500       !ISI - 03/26/04
!      IF (YMIN .LE. TOL) GO TO 500           !ISI - 03/26/04
      IF (YMIN .LE. TOL) RETURN              !ISI - 02/12/06

      IF(Y1.GT.0.0) SIGN = -1.0
      DDX = ABS(DX)
!	IF (YMIN .LT.  1.0) DDX = ABS(DX)/2.   !ISI - 03/26/04
!	IF (YMIN .GT. 2.5) DDX = 1.5*ABS(DX)   !ISI - 03/26/04
!	IF (YMIN .GT. 5.0) DDX = 3.0*ABS(DX)   !ISI - 03/26/04
!	IF (YMIN .GT. 10.0) DDX = 6.0*ABS(DX)  !ISI - 03/26/04
!	IF (YMIN .GT. 20.0) DDX = 12.0*ABS(DX) !ISI - 03/26/04
!	IF (YMIN .GT. 40.0) DDX = 15.*ABS(DX)  !ISI - 03/26/04
!	X2 = X1 + DDX*SIGN                     !ISI - 03/26/04
      X2 = X2 + 2.0**(ICOUNT-1)*DX*SIGN      !To bracket root ISI - 03/26/04
C
!VL: Previously: 100   Y2 = SUBA(X2,IERR)
      DO WHILE (.TRUE.)

          Y2 = SUBA(X2,IERR)
          ICOUNT = ICOUNT + 1
          SLOPE = (Y2 - Y1)/(X2 - X1)

          !VL: Previously: IF (IERR .EQ. 0) GO TO 125
          IF (IERR .NE. 0) THEN
              X2 = (X1 + X2)/2.
              IF (ICOUNT .GT. 30) THEN 
                  !VL: Previously : GO TO 999
                  IERROR = 4
                  RETURN
              END IF
              !VL: Previously: GO TO 100
              CYCLE
          END IF

C
!VL: Previously: 125       YMIN = AMIN1(YMIN,ABS(Y2))
          YMIN = AMIN1(YMIN,ABS(Y2))
!      IF (YMIN .LE. TOL) GO TO 500           !ISI - 03/26/04
          IF (YMIN .LE. TOL) RETURN              !ISI - 02/12/06

          DDX = ABS(DX)
!	IF (YMIN .LT.  1.0) DDX = DX/2.			!ISI - 05/10/04
!	IF (YMIN .GT. 2.5) DDX = 1.5*ABS(DX)	!ISI - 05/10/04
!	IF (YMIN .GT. 5.0) DDX = 3.0*ABS(DX)	!ISI - 05/10/04
!	IF (YMIN .GT. 10.0) DDX = 6.0*ABS(DX)	!ISI - 05/10/04
!	IF (YMIN .GT. 20.0) DDX = 12.0*ABS(DX)	!ISI - 05/10/04
!	IF (YMIN .GT. 40.0) DDX = 15.*ABS(DX)	!ISI - 05/10/04
          DDX=2.0**(ICOUNT-1)*ABS(DX)				!ISI - 05/10/04
C
          IF (Y1*Y2 .LE. 0.) RETURN
          IF (ICOUNT .GT. 30) THEN 
              !VL: Previously : GO TO 999
              IERROR = 4
              RETURN
          END IF
C
C	IF SLOPE IS POSITIVE AND "Y" IS GREATER THAN 0. OR
C	IF SLOPE IS NEGATIVE AND "Y" IS LESS THAN 0. MOVE THE LOWER
C	POINT TO THE LEFT
C
          SLOPE = (Y2 - Y1)/(X2 - X1)
          !VL: Previously: IF (SLOPE*Y1 .LT. 0.) GO TO 300
          IF (SLOPE*Y1 .GE. 0.) THEN
C
C	MAKE "X1" THE LOWER OR LEFT-HAND POINT
C
              !VL: Previously: IF (X1 .LT. X2) GO TO 200
              IF (X1 .GE. X2) THEN
                  X1 = X2
                  Y1 = Y2
              END IF

C
              !VL Previously: 200       X2 = X1 - DDX
              X2 = X1 - DDX
              !VL: Previously: GO TO 100
              CYCLE

          END IF

C
C	IF THERE IS A NEGATIVE SLOPE AND "Y" IS GREATER THAN 0. OR
C	THERE IS A POSITIVE SLOPE AND "Y" IS LESS THAN 0. THEN
C	MOVE THE RIGHT-HAND POINT FURTHER TO THE RIGHT
C

!VL: Previously: 300       IF (X1 .GT. X2) GO TO 400
          IF (X1 .LE. X2) THEN
C
C	MAKE X1 THE UPPER POINT
C
              X1 = X2
              Y1 = Y2
          END IF

!VL: Previously: 400       X2 = X1 + DDX
          X2 = X1 + DDX
          !VL: Previously: GO TO 100
          CYCLE   !VL: Not needed ...

      END DO


C
C	FIRST POINT TRIED WAS WITHIN THE DESIRED TOLERANCE
C

!VL:  All instances of GO TO 500 have been commented out ...
!500   X2 = X1
!      Y2 = Y1
!      RETURN

!VL: All instances of GO TO 999 have been replaced with equivalent return statements
!999   IERROR = 4
C	IF (JPRINT .NE. 0) WRITE(6,1001) X1,Y1,X2,Y2
      !WRITE(6,1001) X1,Y1,X2,Y2                                 !ISI - 03/26/04
      !RETURN

 1001 FORMAT(' GUESS1: ** FAILED TO BRACKET A SOLUTION **',/,
     &9X,'F(',1PE12.4,') = ',1PE13.5,5X,'F(',1PE12.4,') = ',
     &1PE13.5)
      END
