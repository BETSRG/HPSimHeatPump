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
    USE DataGlobals, ONLY: MaxNameLength, RefrigIndex   !RS: Debugging: Removal of plethora of RefrigIndex definitions in the code
    USE UnitConvertMod, ONLY: Temperature_F2C

    IMPLICIT NONE

    !CHARACTER (len=15) :: Property !RS: Debugging: Extraneous
    REAL Quality,Pressure,Enthalpy !Temperature,!RS: Debugging: Extraneous

    REAL TINPUT
    INTEGER IERR
    INTEGER(2) RefPropErr			!Error flag:1-error; 0-no error

    REAL,PARAMETER :: StandardDensity=1.2 !kg/m3

    REAL TAIIE,TSATEI,SUPCAL,SUPCL,TSATCI !XMR, !TROE,TSATEO,SXOE    !RS: Debugging: Set once but never used & Extraneous
    REAL SUPR,TSAVG,SXIC !TRIC,!DIFFER,Hsuc  !RS: Debugging: Set once but never used & Extraneous
    REAL TsatEvp,TsatCnd,Subcooling,Superheat,AccumDP,Xliq,Xvap
    
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
       ! WRITE (PrintString, FMT_800) '>> Evaporator entering air temperature: ',Temperature_F2C(TAIIE),Tunit   !RS: Debugging: File Check
    ELSE
        !WRITE(*,FMT_800)'>> Evaporator entering air temperature: ',TAIIE,Tunit !RS: Debugging: File Check
    END IF
    CALL IssueOutputMessage(PrnLog, PrnCon, '')
    CALL IssueOutputMessage(PrnLog, PrnCon, TRIM(PrintString))    

    !XMR=CompOUT(2)*3600/UnitM   !RS Comment: Unit Conversion, lbm/s??? !RS: Debugging: Extraneous

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
        CALL IssueOutputMessage(PrnLog, PrnCon, '-- WARNING -- LowSide: Refprop error.')
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
            WRITE(*,*)'Press return to terminate program.'
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
            CALL IssueOutputMessage(PrnLog, PrnCon, '-- WARNING -- LowSide: Refprop error.')
            IERR=1
            RETURN
        END IF
    END IF	

    !TRIC=TiCmp*1.8+32   !RS Comment: Unit Conversion, from C to F  !RS: Debugging: Extraneous

    Pressure=PiCmp*1000 !RS Comment: Unit Conversion
    Quality=1
    TSATCI=PQ(Ref$,Pressure,Quality,'temperature',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        CALL IssueOutputMessage(PrnLog, PrnCon, '-- WARNING -- LowSide: Refprop error.')
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
        !WRITE(PrintString,FMT_804) '           Desired quality = ',SXIC*100,Xunit  !RS: Debugging: File Check
    ELSE
        IF (Unit .EQ. 1) THEN
            !WRITE(PrintString,FMT_804) '           Desired superheat = ',SUPER/1.8,DTunit  !RS: Debugging: File Check
        ELSE
            !WRITE(PrintString,FMT_804) '           Desired superheat = ',SUPER,DTunit  !RS: Debugging: File Check
        END IF
    END IF
    CALL IssueOutputMessage(PrnLog, PrnCon, PrintString)

    !This IF block will always report one and only one message based on calculated "quality"
    IF (XICMP .LT. 0.0) THEN
        IF (Unit .EQ. 1) THEN
            !WRITE(PrintString,FMT_804) '       Calculated subcooling = ',-SUPCAL/1.8,DTunit    !RS: Debugging: File Check
        ELSE
            !WRITE(PrintString,FMT_804) '       Calculated subcooling = ',-SUPCAL,DTunit    !RS: Debugging: File Check
        END IF
    ELSEIF (XICMP.LT.1.0) THEN
        !WRITE(PrintString,FMT_804)'        Calculated quality = ',XICMP*100,Xunit  !RS: Debugging: File Check
    ELSE
        IF (Unit .EQ. 1) THEN
            !WRITE(PrintString,FMT_804)'        Calculated superheat = ',SUPCAL/1.8,DTunit  !RS: Debugging: File Check
        ELSE  
            !WRITE(PrintString,FMT_804)'        Calculated superheat = ',SUPCAL,DTunit  !RS: Debugging: File Check
        END IF
    END IF
    CALL IssueOutputMessage(PrnLog, PrnCon, PrintString)

    RETURN

END FUNCTION
