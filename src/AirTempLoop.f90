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

REAL FUNCTION EVPTR(TINPUT,IERR)
    !
    !       EVPTR(TEMPERATURE) = SUPCAL(TEMPERATURE) - SUPERE
    !       EVPTR IS USED WITH THE ROOT SOLVER 'ZERO' TO FIND TAIIE SO
    !       THAT ABS(SUPCAL(TAIIE) - SUPERE) < EVPCON.
    !       'ZERO' CONTAINS ALL OF THE LOGIC NECESSARY TO ITERATE TO
    !       A ROOT, TAIIE.
    !
    USE FluidProperties_HPSim !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)
    USE EvaporatorMod
    USE AccumulatorMod
    USE DataSimulation
    USE DataGlobals_HPSim, ONLY: MaxNameLength, RefrigIndex  !RS Comment: Needs to be used for implementation with Energy+ currently (7/23/12)
    USE UnitConvertMod, ONLY: Temperature_F2C

    IMPLICIT NONE

    CHARACTER (len=15) :: Property           
    !INTEGER            :: RefrigIndex =0
    REAL Temperature,Quality,Pressure,Enthalpy

    REAL TINPUT
    INTEGER IERR

    INTEGER(2) RefPropOpt			!Ref prop calc. option
    INTEGER(2) RefPropErr			!Error flag:1-error; 0-no error
    REAL RefProp(28)	!Refrigerant properties

    INTEGER(2) AirPropOpt			!Air prop calc. option
    INTEGER(2) AirPropErr			!Error flag:1-error; 0-no error
    REAL AirProp(8)		!Air properties

    REAL,PARAMETER :: StandardDensity=1.2 !kg/m3

    REAL TAIIE,XMR,TSATEI,TROE,TSATEO,SUPCAL,SUPCL,SXOE,TSATCI
    REAL SUPR,DIFFER,TSAVG,TRIC,SXIC,Hsuc
    REAL ID,L,Elevation,mdot,xi,xo,mu,muVap,muLiq,rhoi,rhoo,rhoiVap
    REAL rhoiLiq,rhooVap,rhooLiq,DPfric,DPmom,DPgrav,DPtot,DPvalve
    REAL TsatEvp,TsatCnd,Subcooling,Superheat,AccumDP,Xliq,Xvap
    INTEGER I
    
    CHARACTER(LEN=13),PARAMETER :: FMT_800 = "(A41,F7.2,A5)"
    CHARACTER(LEN=13),PARAMETER :: FMT_804 = "(A32,F7.2,A5)"

    CHARACTER(LEN=MaxNameLength) :: PrintString ! placeholder for formatted output strings

    ! initialize some parameters
    EVPTR = 1.0E+10
    IERR = 0
    TAIIE = TINPUT

    IF (TAIIE .LT. TSICMP) THEN
        IERR=2
        RETURN
    END IF

    IF (Unit .EQ. 1) THEN
        WRITE(PrintString, FMT_800) '>> Evaporator entering air temperature: ',Temperature_F2C(TAIIE),Tunit
    ELSE
        WRITE(PrintString, FMT_800) '>> Evaporator entering air temperature: ',TAIIE,Tunit
    END IF
    CALL IssueOutputMessage( '')
    CALL IssueOutputMessage( TRIM(PrintString))    

    XMR=CompOUT(2)*3600/UnitM   !RS Comment: Unit Conversion, lbm/s???

    HiEvp=EvapIN(3)

    IF (FirstTimeAirTempLoop) THEN
        EvapIN(2)=CompIN(1)+(EvapIN(2)-EvapOUT(6))
        FirstTimeAirTempLoop=.FALSE.
    END IF

    PiEvp=EvapIN(2)

    Pressure=PiEvp*1000 !RS Comment: Unit Conversion
    Quality=0
    TSATEI=PQ(Ref$,Pressure,Quality,'temperature',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        CALL IssueOutputMessage( '-- WARNING -- LowSide: Refprop error.')
        IERR=1
        RETURN
    END IF
    TSATEI=TSATEI*1.8+32    !RS Comment: Unit Conversion, from C to F

    EvapIN(1)=MdotR			!Refrigerant side mass flow rate, kg/s
    EvapIN(2)=PiEvp			!Evap. inlet pressure, kPa
    EvapIN(3)=HiEvp			!Refrigerant side inlet enthalpy, kJ/kg
    EvapIN(4)=XMaE            !Air side mass flow rate, kg/s
    EvapIN(5)=(TAIIE-32)/1.8  !Air side inlet temp. C
    EvapIN(6)=RHiE            !Air side inlet relative humidity
    EvapIN(9)=CompOUT(5)      !Discharge temperature, C

    !Take compressor shell loss into account
    IF (CompPAR(21) .NE. 0) THEN !Shell loss in fraction
        EvapPAR(32)=CompPAR(21)*CompOUT(1)
    ELSE !Shell loss in W
        EvapPAR(32)=CompPAR(22)/1000    !RS Comment: Unit Conversion
    END IF

    CALL Evaporator(Ref$,PureRef,EvapIN,EvapPAR,EvapOUT)
    CALL PrintEvaporatorResult 
    EvapPAR(54)=0 !No longer first time
    IF (EvapOUT(20) .NE. 0) THEN
        SELECT CASE (INT(EvapOUT(20)))
        CASE (2)
            IERR=1
            RETURN
        CASE (3,4,5)
            CALL IssueOutputMessage('Press return to terminate program.')
            STOP
        END SELECT
    END IF

    PoEvp=EvapOUT(1)
    HoEvp=EvapOUT(2)
    ToEvp=EvapOUT(3)
    XoEvp=EvapOUT(4)
    PiCmp=EvapOUT(6)
    HiCmp=EvapOUT(7)
    TiCmp=EvapOUT(8)
    XiCmp=EvapOUT(9)

    IF (AccumPAR(2) .GT. 0) THEN !Accumulator exists
        TsatEvp=(TSICMP-32)*5/9     !RS Comment: Unit Conversion, from F to C
        TsatCnd=(TSOCMP-32)*5/9     !RS Comment: Unit Conversion, from F to C
        Subcooling=CondOUT(14)
        Superheat=EvapOUT(10)
        Xliq=CondOUT(13)
        Xvap=XiCmp
        CALL CalcAccumulatorDP(MdotR,TsatEvp,TsatCnd,Subcooling,Superheat, &
        Xliq,Xvap,AccumDP)

        PiCmp=PiCmp-AccumDP
        EvapOUT(6)=PiCmp

        Pressure=PiCmp*1000 !RS Comment: Unit Conversion
        Enthalpy=HiCmp*1000 !RS Comment: Unit Conversion
        XiCmp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)   !Compressor Inlet Quality
        IF (RefPropErr .GT. 0) THEN
            CALL IssueOutputMessage( '-- WARNING -- LowSide: Refprop error.')
            IERR=1
            RETURN
        END IF
    END IF	

    TRIC=TiCmp*1.8+32   !RS Comment: Unit Conversion, from C to F

    Pressure=PiCmp*1000 !RS Comment: Unit Conversion
    Quality=1
    TSATCI=PQ(Ref$,Pressure,Quality,'temperature',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        CALL IssueOutputMessage( '-- WARNING -- LowSide: Refprop error.')
        IERR=1
        RETURN
    END IF
    TSATCI=TSATCI*1.8+32    !RS Comment: Unit Conversion, from C to F

    TSAVG=(TSATEI+TSATCI)/2
    IF(TSAVG.GT.TAIIE) THEN
        IERR=2
    END IF

    IF (IERR .GE. 1) THEN
        RETURN
    END IF
    SUPCAL=EvapOUT(10)*1.8 !ISI - 10/07/06
    SUPCL = SUPCAL

    IF (XICMP .LT. 0.) THEN !Edited for refprop table 01-15-04 - ISI
        SUPCL = SUPCAL*10 !-SUPCAL !-500.0*SUPCAL
    ELSEIF (XICMP .LT. 1. .AND. XICMP .GT. 0) THEN
        SUPCL = -500.0*(1. - XICMP)
    END IF

    SUPR = SUPER
    IF (SUPER .LT. 0.0) THEN
        SUPR = -500.*(1.0 + SUPER)
    END IF
    EVPTR = SUPCL - SUPR

    IF(SUPER.LT.0.0) THEN
        SXIC = -SUPER
        WRITE(PrintString,FMT_804) '           Desired quality = ',SXIC*100,Xunit
    ELSE
        IF (Unit .EQ. 1) THEN
            WRITE(PrintString,FMT_804) '           Desired superheat = ',SUPER/1.8,DTunit
        ELSE
            WRITE(PrintString,FMT_804) '           Desired superheat = ',SUPER,DTunit
        END IF
    END IF
    CALL IssueOutputMessage( PrintString)

    !This IF block will always report one and only one message based on calculated "quality"
    IF (XICMP .LT. 0.0) THEN
        IF (Unit .EQ. 1) THEN
            WRITE(PrintString,FMT_804) '       Calculated subcooling = ',-SUPCAL/1.8,DTunit
        ELSE
            WRITE(PrintString,FMT_804) '       Calculated subcooling = ',-SUPCAL,DTunit
        END IF
    ELSEIF (XICMP.LT.1.0) THEN
        WRITE(PrintString,FMT_804)'        Calculated quality = ',XICMP*100,Xunit
    ELSE
        IF (Unit .EQ. 1) THEN
            WRITE(PrintString,FMT_804)'        Calculated superheat = ',SUPCAL/1.8,DTunit
        ELSE  
            WRITE(PrintString,FMT_804)'        Calculated superheat = ',SUPCAL,DTunit
        END IF
    END IF
    CALL IssueOutputMessage( PrintString)

    RETURN

END FUNCTION
