
! ************************************** !
! ** HEAT PUMP SIMULATION CODE HEADER ** !
! ************************************** !

! ************************************** !
! -- HIGH LEVEL OVERVIEW/DESCRIPTION --- !
! -------------------------------------- !
! This function looks like it checks each iteration for convergence; it's a root solver.
! It's looking for the zero value. We believe this module is adapted from the ZBRENT function
! in Numerical Recipes (1996); thus it uses the Van Wijngaarden-Dekker-Brent method (Section 9.3). 

! ************************************** !
! -- PHYSICAL DESCRIPTION -------------- !
! -------------------------------------- !
! This component does not represent any physical item.

! ************************************** !
! -- SIMULATION DATA RESPONSIBILITIES -- !
! -------------------------------------- !
! This function looks like it checks each iteration for convergence, and loops until the calculation converges.

! ************************************** !
! -- INPUT FILES/OUTPUT FILES (none) --- !
! -------------------------------------- !
! There are no input or output files directly connected to this function.

! ************************************** !
! -- MODULE LEVEL VARIABLES/STRUCTURES - !
! -------------------------------------- !
! There is no module level; this is a single function.

! ************************************** !
! -- SUMMARY OF METHODS, CALL TREE ----- !
! -------------------------------------- !
! This module contains 1 method:
!    PUBLIC ZeroConvergence -- Calls GUESS3 and iterates until convergence.
!      Called by HPdesignMod.f90

! ************************************** !
! -- ISSUES/BUGS/TICKETS --------------- !
! -------------------------------------- !
! NA

! ************************************** !
! -- CHANGELOG ------------------------- !
! -------------------------------------- !
! 2012-12-11 | ESL | Initial header
! 2012-12-12 | RAS | Updated header 

! ************************************** !
! -- TODO/NOTES/RECOMMENDATIONS -------- !
! -------------------------------------- !
! Some documentation would be really useful.

    REAL FUNCTION ZeroConvergence(AX,F,TOL1,TOL2,DX,FB,IERROR)
    USE DataSimulation !, ONLY: CoilMode  !RS: Debugging: This is for a test below (11/8/13)
    implicit none
    !
    LOGICAL FIRST
    REAL AX,FAX,BX,FBX,F,TOL1,TOL2
    REAL A,B,C,D,E,EPS,FA,FB,FC,TOLX,TOLF,XM,P,Q,R,S
    REAL DX, ONE, EPS0
    INTEGER IERROR, IERR
    EXTERNAL F
    DATA FIRST / .TRUE. /
    !
    CALL GUESS3(AX,FAX,BX,FBX,F,DX,TOL2,IERROR) !RS: Initial guesses and bounding of region
    IF (IERROR .NE. 0) THEN
        RETURN
    END IF
    IF (FIRST) THEN
        EPS = 1.0
        ONE = 1.0   !VL
        DO WHILE (ONE .GT. 1.0)
            EPS = EPS/2.0
            ONE = 1.0 + EPS0
        END DO 

        FIRST = .FALSE.
    END IF
    !

    A = AX
    B = BX
    FA = FAX
    FB = FBX
    !

    C = A
    FC = FA
    D = B-A
    E = D

    DO WHILE (.TRUE.)

        IF (ABS(FC) .LT. ABS(FB)) THEN
            A = B
            B = C
            C = A
            FA = FB
            FB = FC
            FC = FA        
        END IF

        TOLX = TOL1      !ISI - 05/31/05
        TOLF = TOL2      !ISI - 05/31/05
        XM = 0.5*(C - B) !ISI - 05/31/05 

        !RS: Debugging: According to the original ORNL flowchart, for the evaporator case, it should
        ! only exit out if both terms are converged. Therefore, the following commented out lines are
        ! a test of this. It didn't seem to make a huge difference, so it's been commented back out
        ! for now.
        !RS: Debugging: Testing (11/8/13)
        !IF (CoilMode .EQ. 1) THEN    !RS: If this is the low-side (evaporator) loop, then
        !    IF (ABS(XM) .LE. TOLX .AND. ABS(FB) .LE. TOLF) THEN
        !        EXIT   !RS: Both convergence criteria have to be met
        !    END IF
        !ELSE   !RS: Otherwise, if it's the high-side loop, then only one of the two
                ! convergence criteria has to be met.
        !    IF (ABS(XM) .LE. TOLX) THEN
        !        EXIT
        !    END IF
        !    IF (ABS(FB) .LE. TOLF) THEN
        !        EXIT
        !    END IF
        !END IF

        IF (ABS(XM) .LE. TOLX) THEN    !RS: Debugging: Comment out when testing the above (11/8/13)
            EXIT
        END IF
        IF (ABS(FB) .LE. TOLF) THEN
            EXIT
        END IF
        
        IF ((ABS(E) .LT. TOLX) .OR. (ABS(FA) .LE. ABS(FB))) THEN
            D = XM
            E = D
        ELSE

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

            IF (P .GT. 0.0) THEN
                Q = -Q
            END IF
            P = ABS(P)

            IF (((2.0*P) .GE. (3.0*XM*Q - ABS(TOLX*Q))) .OR. (P .GE. ABS(0.5*E*Q))) THEN
                D = XM
                E = D                
            ELSE
                E = D
                D = P/Q          
            END IF

        END IF

        A = B
        FA = FB
        IF (ABS(D) .GT. TOLX) THEN
            B = B + D
        END IF
        IF (ABS(D) .LE. TOLX) THEN
            B = B + SIGN(TOLX,XM)
        END IF
        FB = F(B,IERR)
        IF ((FB*(FC/ABS(FC))) .GT. 0.) THEN
            C = A
            FC = FA
            D = B-A
            E = D
        END IF

    END DO

    !   SET ERROR CODES
    !     IERROR = 0, NORMAL RETURN
    !     IERROR = 1, TOLERANCE ON INDEPENDENT VARIABLE EXCEEDED
    !     IERROR = 2, TOLERANCE ON FUNCTION VALUE EXCEEDED
    !     IERROR = 3, TOLERANCES ON INDEPENDENT VARIABLE AND FUNCTION VALUE EXCEEDED

    ZeroConvergence = B
    IERROR = 0
    IF (ABS(XM) .GT. TOLX) THEN
        IERROR = 1
    END IF
    IF (ABS(FB).GT.TOLF) THEN
        IERROR = IERROR + 2
    END IF
    RETURN
    !
    !   COMPUTE MACHINE PRECISION "EPS"
    !
    END FUNCTION
