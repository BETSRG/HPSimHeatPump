! ************************************** !
! ** HEAT PUMP SIMULATION CODE HEADER ** !
! ************************************** !

! ************************************** !
! -- HIGH LEVEL OVERVIEW/DESCRIPTION --- !
! -------------------------------------- !
! Provide a 1 or 2 sentence overview of this module.  
! In most cases, it is probably not a useful entry and can be inferred from the name of the module anyway.
!
! ************************************** !
! -- PHYSICAL DESCRIPTION -------------- !
! -------------------------------------- !
! This component represents something...or nothing...in a heat pump system.
! A description of the component is found at:
! some website
! From that website: 
!  - It does something

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

REAL FUNCTION CHARGM(DTVALU,IERR)

    USE DataGlobals_HPSim, ONLY: MaxNameLength  !RS Comment: Needs to be used for implementation with Energy+ currently (7/23/12)
    USE DataSimulation

    IMPLICIT NONE

    REAL DTVALU
    INTEGER ICHRGE
    INTEGER IERR

    REAL DTVAL

    CHARACTER(LEN=13),PARAMETER :: FMT_1000 = "(A32,F7.2,A9)"
    CHARACTER(LEN=119),PARAMETER :: FMT_1003 = "('0CHARGM: TEST FOR CONVERGENCE ON REFRIGERANT CHARGE',/, 11  X,'ESTIMATED  CONDENSER SUBCOOLING      ',F10.3,' F DEG')"
    CHARACTER(LEN=110),PARAMETER :: FMT_1004 = "('0CHARGM: TEST FOR CONVERGENCE ON REFRIGERANT CHARGE',/, 11  X,'ESTIMATED  CONDENSER EXIT QUALITY    ',F10.4)"
    CHARACTER(LEN=MaxNameLength) :: sPrint

    IERR = 0
    ICHRGE=1

    IF (MODE .EQ. TXVSIMULATION .AND. DTVALU .GT. 80) THEN
        DTVALU=(DTVALU+DTVAL)/2 !If it is higher then 80 F subcooling, take the average of the current and previous guesses ISI - 01/02/09
    END IF

    CALL HPDM(DTVALU)

    IF (FirstTimeChargeLoop) THEN
        FirstTimeChargeLoop=.FALSE.
    END IF

    IF (MODE .EQ. 4) THEN
        CHARGM = ( CALCHG - REFCHG ) !More subcooling, more charge
    ELSE
        CHARGM = ( REFCHG - CALCHG ) !Less superheat, more charge
    END IF
    IF(ICHRGE.EQ.2) THEN
        CHARGM = -CHARGM
    END IF

    DTVAL = DTVALU

    IF(ICHRGE.EQ.2) THEN
        IF(DTVALU.LT.0.0) THEN
            DTVAL = -DTVALU/200.
            WRITE(sPrint,FMT_1004) DTVAL
        ELSE
            WRITE(sPrint,FMT_1003) DTVAL
        END IF
        
    ELSE
        IF(DTVALU.LT.0.0) THEN
            Xunit=' (%)'
            IF (MODE .EQ. 1) THEN
                DTVAL = 1.0 + DTVALU/500.
                WRITE(sPrint,FMT_1000)'Compressor suction quality: ',DTVAL*100,Xunit
            ELSE
                DTVAL = 1.0 + DTVALU/500.
                DTVAL = -DTVALU/200.
                WRITE(sPrint,FMT_1000)'Condenser quality: ',DTVAL*100,Xunit
            END IF
        ELSE
            IF (MODE .EQ. 1) THEN
                IF (Unit .EQ. 1) THEN
                    DTunit=' (K)'
                    WRITE(sPrint,FMT_1000)'Compressor suction supreheat: ',DTVAL/1.8,DTunit
                ELSE
                    DTunit=' (R)'
                    WRITE(sPrint,FMT_1000)'Compressor suction supreheat: ',DTVAL,DTunit
                END IF
            ELSE
                IF (Unit .EQ. 1) THEN
                    DTunit=' (K)'
                    WRITE(sPrint,FMT_1000)'Condenser subcooling: ',DTVAL/1.8,DTunit
                ELSE
                    DTunit=' (R)'
                    WRITE(sPrint,FMT_1000)'Condenser subcooling: ',DTVAL,DTunit
                END IF
            END IF

        END IF

    END IF
    CALL IssueOutputMessage( '')
    CALL IssueOutputMessage( TRIM(sPrint))

    IF (Unit .EQ. 1) THEN
        MassUnit = ' (kg)'
        WRITE(sPrint,FMT_1000)'           Desired charge = ',REFCHG*0.4536,MassUnit
        CALL IssueOutputMessage( TRIM(sPrint))
        WRITE(sPrint,FMT_1000)'        Calculated charge = ',CALCHG*0.4536,MassUnit
        CALL IssueOutputMessage( TRIM(sPrint))
    ELSE
        MassUnit = ' (lbm)'
        WRITE(sPrint,FMT_1000)'           Desired charge = ',REFCHG,MassUnit
        CALL IssueOutputMessage( TRIM(sPrint))
        WRITE(sPrint,FMT_1000)'        Calculated charge = ',CALCHG,MassUnit
        CALL IssueOutputMessage( TRIM(sPrint))
    END IF

    RETURN 

END FUNCTION
