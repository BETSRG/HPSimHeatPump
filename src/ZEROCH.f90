    REAL FUNCTION ZEROCH(AX,F,TOL1,TOL2,DX,FB,IERROR)
    !
    LOGICAL FIRST
    REAL AX,FAX,BX,FBX,F,TOL1,TOL2
    REAL A,B,C,D,E,EPS,FA,FB,FC,TOLX,TOLF,XM,P,Q,R,S
    EXTERNAL F
    DATA FIRST / .TRUE. /
    !
    CALL GUESS1(AX,FAX,BX,FBX,F,DX,TOL2,IERROR)
    IF (IERROR .NE. 0) RETURN
    !VL: Previously: IF (FIRST) GO TO 100
    IF (FIRST) THEN
        !VL: Previously: 100     EPS = 1.0
        EPS = 1.0
        ONE = 1.0   !VL
        DO WHILE (ONE .GT. 1.0)
            !VL: Previously:110     EPS = EPS/2.0
            EPS = EPS/2.0
            ONE = 1.0 + EPS
            !VL: Previously:IF (ONE .GT. 1.0) GO TO 110
        END DO 

        FIRST = .FALSE.
        !VL: Previously: GO TO 10
    END IF
    !

    !VL: Previously: 10  A = AX ! all GOTO 10 statements eliminated ....
    A = AX
    B = BX
    FA = FAX
    FB = FBX
    !

    !VL: Previously: 20  C = A ! all GOTO 20 statements eliminated ....
    C = A
    FC = FA
    D = B-A
    E = D

    DO WHILE (.TRUE.)

        !VL: Previously: 30      IF (ABS(FC) .GE. ABS(FB)) GO TO 40

        !VL: Previously:
        !IF (ABS(FC) .GE. ABS(FB)) GO TO 40
        !A = B
        !B = C
        !C = A
        !FA = FB
        !FB = FC
        !FC = FA

        IF (ABS(FC) .LT. ABS(FB)) THEN
            A = B
            B = C
            C = A
            FA = FB
            FB = FC
            FC = FA        
        END IF

        !40	TOLX = 2.0*EPS*ABS(B) + 0.5*TOL1	!ISI - 05/31/05
        !	TOLF = 2.0*EPS*ABS(FB) + 0.5*TOL2	!ISI - 05/31/05
        !	XM = 0.5*(C - B)				    !ISI - 05/31/05

        !VL: Previously: 40      TOLX = TOL1		 !ISI - 05/31/05 ! all GOTO 40 statements eliminated ....
        TOLX = TOL1		 !ISI - 05/31/05
        TOLF = TOL2		 !ISI - 05/31/05
        XM = 0.5*(C - B) !ISI - 05/31/05 

        !VL: Previously: 
        !IF (ABS(XM) .LE. TOLX) GO TO 90
        !IF (ABS(FB) .LE. TOLF) GO TO 90
        IF (ABS(XM) .LE. TOLX) EXIT
        IF (ABS(FB) .LE. TOLF) EXIT

        !VL: Previously: 
        !IF (ABS(E) .LT. TOLX) GO TO 70
        !IF (ABS(FA) .LE. ABS(FB)) GO TO 70
        !IF ((ABS(E) .LT. TOLX) .OR. (ABS(FA) .LE. ABS(FB))) GO TO 70
        IF ((ABS(E) .LT. TOLX) .OR. (ABS(FA) .LE. ABS(FB))) THEN
            !VL: Previously: 70          D = XM
            D = XM
            E = D
        ELSE


            !VL: Previously: 
            !        IF (A .NE. C) GO TO 50
            !        S = FB/FA
            !        P = 2.0*XM*S
            !        Q = 1.0 - S
            !        GO TO 60
            !        !
            !50      Q = FA/FC
            !        R = FB/FC
            !        S = FB/FA
            !        P = S*(2.0*XM*Q*(Q - R) - (B - A)*(R - 1.0))
            !        Q = (Q - 1.0)*(R - 1.0)*(S - 1.0)

            IF (A .NE. C) THEN

                Q = FA/FC
                R = FB/FC
                S = FB/FA
                P = S*(2.0*XM*Q*(Q - R) - (B - A)*(R - 1.0))
                Q = (Q - 1.0)*(R - 1.0)*(S - 1.0)            

            ELSE

                S = FB/FA
                P = 2.0*XM*S
                Q = 1.0 - S

            END IF
            !


            !
            !VL: Previously: 60      IF (P .GT. 0.0) Q = -Q ! all GOTO 60 statements eliminated ....
            IF (P .GT. 0.0) Q = -Q
            P = ABS(P)

            !VL: Previously:
            !IF (((2.0*P) .GE. (3.0*XM*Q - ABS(TOLX*Q))) .OR. (P .GE. ABS(0.5*E*Q))) GO TO 70
            !E = D
            !D = P/Q
            !GO TO 80

            IF (((2.0*P) .GE. (3.0*XM*Q - ABS(TOLX*Q))) .OR. (P .GE. ABS(0.5*E*Q))) THEN
                !VL: Previously:70          D = XM
                D = XM
                E = D                
            ELSE
                E = D
                D = P/Q
                !VL: Previously:GO TO 80            
            END IF

        END IF

        !VL: Previously:80      A = B   ! all GOTO 80 statements eliminated ....
        A = B
        FA = FB
        IF (ABS(D) .GT. TOLX) B = B + D
        IF (ABS(D) .LE. TOLX) B = B + SIGN(TOLX,XM)
        FB = F(B,IERR)
        !VL: Previously: IF ((FB*(FC/ABS(FC))) .GT. 0.) GO TO 20
        IF ((FB*(FC/ABS(FC))) .GT. 0.) THEN
            C = A
            FC = FA
            D = B-A
            E = D
        END IF

        !VL: Previously: GO TO 30
    END DO

    !
    !	SET ERROR CODES
    !	  IERROR = 0, NORMAL RETURN
    !	  IERROR = 1, TOLERANCE ON INDEPENDENT VARIABLE EXCEEDED
    !	  IERROR = 2, TOLERANCE ON FUNCTION VALUE EXCEEDED
    !	  IERROR = 3, TOLERANCES ON INDEPENDENT VARIABLE AND FUNCTION VALUE
    !		      EXCEEDED
    !

    !VL: Previously: 90  ZERO3 = B ! all GOTO 90 statements eliminated ....
    ZEROCH = B
    IERROR = 0
    IF (ABS(XM) .GT. TOLX) IERROR = 1
    IF (ABS(FB).GT.TOLF) IERROR = IERROR + 2
    RETURN
    !
    !	COMPUTE MACHINE PRECISION "EPS"
    !

    !VL: Code chunk moved to GOTO call ...
    ! -----------------------------------
    !100	EPS = 1.0
    !110	EPS = EPS/2.0
    !	ONE = 1.0 + EPS
    !	IF (ONE .GT. 1.0) GO TO 110
    !	FIRST = .FALSE.
    !	GO TO 10
    ! -----------------------------------

    END
