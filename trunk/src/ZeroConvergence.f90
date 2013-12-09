! ************************************** !
! ** HEAT PUMP SIMULATION CODE HEADER ** !
! ************************************** !

! ************************************** !
! -- HIGH LEVEL OVERVIEW/DESCRIPTION --- !
! -------------------------------------- !
!This module contains a function to find a zero value.
!
! ************************************** !
! -- PHYSICAL DESCRIPTION -------------- !
! -------------------------------------- !
! THis does not represent a phusical component of a heat pump system.

! ************************************** !
! -- SIMULATION DATA RESPONSIBILITIES -- !
! -------------------------------------- !
! Here's a one line summary of what this does for the simulation itself.
! This module takes inputs such as...and modifies them like so...and outputs these things

! ************************************** !
! -- INPUT FILES/OUTPUT FILES (none) --- !
! -------------------------------------- !
! Check for any OPEN statements in the code
! Check for any WRITE statements in the code
!  Note that writing to unit "*" or "6" means just write to the terminal, not to a file

! ************************************** !
! -- MODULE LEVEL VARIABLES/STRUCTURES - !
! -------------------------------------- !
! What vars and structures are defined at the *module* level...are units defined?  Any other notes?

! ************************************** !
! -- SUMMARY OF METHODS, CALL TREE ----- !
! -------------------------------------- !
! This module contains X methods:
!    PUBLIC InitSomething -- What does this do (in one line)?
!      Called by what other modules?

! ************************************** !
! -- ISSUES/BUGS/TICKETS --------------- !
! -------------------------------------- !
! Are there any interesting issues with this, unfuddle ticket numbers?

! ************************************** !
! -- CHANGELOG ------------------------- !
! -------------------------------------- !
! 2012-12-11 | ESL | Initial header
! YEAR-MM-DD | ABC | Some other log message? 

! ************************************** !
! -- TODO/NOTES/RECOMMENDATIONS -------- !
! -------------------------------------- !
! Put some notes for what needs to happen here
! Silly things are fine
! Somethings these small silly things are great to grab on to when starting up with the code after being off for a while

! ************************************** !
! ** HEAT PUMP SIMULATION CODE HEADER ** !
! ************************************** !

! ************************************** !
! -- HIGH LEVEL OVERVIEW/DESCRIPTION --- !
! -------------------------------------- !
! This function looks like it checks each iteration for convergence; it's a root solver.

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
    CALL GUESS3(AX,FAX,BX,FBX,F,DX,TOL2,IERROR)
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
        ! only exit out if both terms are converged.
        !IF (CoilMode .EQ. 1) THEN    !RS: Debugging: Testing (11/8/13)
        !    IF (ABS(XM) .LE. TOLX .AND. ABS(FB) .LE. TOLF) THEN
        !        EXIT
        !    END IF
        !ELSE
        !    IF (ABS(XM) .LE. TOLX) THEN
        !        EXIT
        !    END IF
        !    IF (ABS(FB) .LE. TOLF) THEN
        !        EXIT
        !    END IF
        !END IF

        IF (ABS(XM) .LE. TOLX) THEN    !RS: Debugging: Testing above (11/8/13)
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

    !
    !   SET ERROR CODES
    !     IERROR = 0, NORMAL RETURN
    !     IERROR = 1, TOLERANCE ON INDEPENDENT VARIABLE EXCEEDED
    !     IERROR = 2, TOLERANCE ON FUNCTION VALUE EXCEEDED
    !     IERROR = 3, TOLERANCES ON INDEPENDENT VARIABLE AND FUNCTION VALUE
    !             EXCEEDED
    !
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
