    REAL FUNCTION EVPTR(TINPUT,IERR)
    !
    !       EVPTR(TEMPERATURE) = SUPCAL(TEMPERATURE) - SUPERE
    !       EVPTR IS USED WITH THE ROOT SOLVER 'ZERO' TO FIND TAIIE SO
    !       THAT ABS(SUPCAL(TAIIE) - SUPERE) < EVPCON.
    !       'ZERO' CONTAINS ALL OF THE LOGIC NECESSARY TO ITERATE TO
    !       A ROOT, TAIIE.
    !
    USE FluidProperties
    USE EvaporatorMod
    USE AccumulatorMod
    USE DataSimulation

    IMPLICIT NONE

    CHARACTER (len=15) :: Property           
    INTEGER            :: RefrigIndex =0
    REAL Temperature,Quality,Pressure,Enthalpy

    REAL TINPUT
    INTEGER IERR

    LOGICAL PRINT

    INTEGER(2) RefPropOpt			!Ref prop calc. option
    INTEGER(2) RefPropErr			!Error flag:1-error; 0-no error
    REAL RefProp(28)	!Refrigerant properties

    INTEGER(2) AirPropOpt			!Air prop calc. option
    INTEGER(2) AirPropErr			!Error flag:1-error; 0-no error
    REAL AirProp(8)		!Air properties

    REAL,PARAMETER :: StandardDensity=1.2 !kg/m3

    REAL TAIIE,XMR,TSATEI,TROE,TSATEO,SUPCAL,SUPCL,SXOE,TSATCI
    REAL SUPR,DIFFER,TSAVG,TRIC,SXIC,Hsuc
    REAL ID,L,Elevation,mdot,xi,xo,mu,muVap,muLiq,rhoi,rhoo,rhoiVap,rhoiLiq, &
    rhooVap,rhooLiq,DPfric,DPmom,DPgrav,DPtot,DPvalve
    REAL TsatEvp,TsatCnd,Subcooling,Superheat,AccumDP,Xliq,Xvap
    INTEGER I
    
    CHARACTER(LEN=13),PARAMETER :: FMT_800 = "(A41,F7.2,A5)"
    CHARACTER(LEN=13),PARAMETER :: FMT_804 = "(A32,F7.2,A5)"


    !REAL, PARAMETER :: Tboiling=212 !Boiling temperature of air, 212 F

    PRINT=.TRUE.

    EVPTR = 1.0E+10
    IERR = 0
    TAIIE = TINPUT

    IF (TAIIE .LT. TSICMP) THEN
        IERR=2
        RETURN
    END IF

    IF (Unit .EQ. 1) THEN

        IF (PrnLog .EQ. 1) WRITE(6,*)
        IF (PrnLog .EQ. 1) WRITE(6,FMT_800)'>> Evaporator entering air temperature: ',(TAIIE-32)*5/9,Tunit		
        IF (PrnCon .EQ. 1) WRITE(*,*)
        IF (PrnCon .EQ. 1) WRITE(*,FMT_800)'>> Evaporator entering air temperature: ',(TAIIE-32)*5/9,Tunit

    ELSE

        IF (PrnLog .EQ. 1) WRITE(6,*)
        IF (PrnLog .EQ. 1) WRITE(6,FMT_800)'>> Evaporator entering air temperature: ',TAIIE,Tunit		
        IF (PrnCon .EQ. 1) WRITE(*,*)
        IF (PrnCon .EQ. 1) WRITE(*,FMT_800)'>> Evaporator entering air temperature: ',TAIIE,Tunit

    END IF      

    !IF (TAIIE .GT. TSOCMP) THEN
    !	  IERR=2
    !	  RETURN
    !END IF

    XMR=CompOUT(2)*3600/UnitM

    HiEvp=EvapIN(3)

    IF (FirstTimeAirTempLoop) THEN
        EvapIN(2)=CompIN(1)+(EvapIN(2)-EvapOUT(6))
        FirstTimeAirTempLoop=.FALSE.
    END IF

    PiEvp=EvapIN(2)

    Pressure=PiEvp*1000
    Quality=0
    TSATEI=PQ(Ref$,Pressure,Quality,'temperature',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        IF (PrnCon .EQ. 1) WRITE(*,*)'-- WARNING -- LowSide: Refprop error.'
        IF (PrnLog .EQ. 1) WRITE(6,*)'-- WARNING -- LowSide: Refprop error.'
        IERR=1
        RETURN !GO TO 200
    END IF
    TSATEI=TSATEI*1.8+32

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
        EvapPAR(32)=CompPAR(22)/1000
    END IF

    CALL Evaporator(Ref$,PureRef,EvapIN,EvapPAR,EvapOUT)
    CALL PrintEvaporatorResult 
    EvapPAR(54)=0 !No longer first time
    IF (EvapOUT(20) .NE. 0) THEN
        SELECT CASE (INT(EvapOUT(20)))
        CASE (2)
            IERR=1
            RETURN !GO TO 200
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
        TsatEvp=(TSICMP-32)*5/9
        TsatCnd=(TSOCMP-32)*5/9
        Subcooling=CondOUT(14)
        Superheat=EvapOUT(10)
        Xliq=CondOUT(13)
        Xvap=XiCmp
        !CALL CalcAccumulatorDP(DBLE(MdotR),TsatEvp,TsatCnd,Subcooling,Superheat, &
        !                       Xliq,Xvap,AccumDP)
        CALL CalcAccumulatorDP(MdotR,TsatEvp,TsatCnd,Subcooling,Superheat, &
        Xliq,Xvap,AccumDP)

        PiCmp=PiCmp-AccumDP
        EvapOUT(6)=PiCmp

        Pressure=PiCmp*1000
        Enthalpy=HiCmp*1000
        XiCmp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
        IF (RefPropErr .GT. 0) THEN
            WRITE(*,*)'-- WARNING -- LowSide: Refprop error.'
            IERR=1
            RETURN
        END IF
    END IF	

    !TROE=ToEvp*1.8+32
    TRIC=TiCmp*1.8+32

    !Pressure=PoEvp*1000
    Pressure=PiCmp*1000
    Quality=1
    !TSATEO=PQ(Ref$,Pressure,Quality,'temperature',RefrigIndex,RefPropErr)
    TSATCI=PQ(Ref$,Pressure,Quality,'temperature',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        IF (PrnCon .EQ. 1) WRITE(*,*)'-- WARNING -- LowSide: Refprop error.'
        IF (PrnLog .EQ. 1) WRITE(6,*)'-- WARNING -- LowSide: Refprop error.'
        IERR=1
        RETURN !GO TO 200
    END IF
    !TSATEO=TSATEO*1.8+32
    TSATCI=TSATCI*1.8+32

    !TSAVG=(TSATEI+TSATEO)/2
    TSAVG=(TSATEI+TSATCI)/2
    IF(TSAVG.GT.TAIIE) THEN
        IERR=2
    END IF

    IF (IERR .GE. 1) RETURN !GO TO 200
    !SUPCAL = TRIC - TSATCI
    SUPCAL=EvapOUT(10)*1.8 !ISI - 10/07/06
    !SUPCAL=EvapOUT(5)*1.8 !ISI - 05/28/08	  
    SUPCL = SUPCAL

    IF (XICMP .LT. 0.) THEN !Edited for refprop table 01-15-04 - ISI
        SUPCL = SUPCAL*10 !-SUPCAL !-500.0*SUPCAL
    ELSEIF (XICMP .LT. 1. .AND. XICMP .GT. 0) THEN
        SUPCL = -500.0*(1. - XICMP)
    END IF

    SUPR = SUPER
    IF (SUPER .LT. 0.0) SUPR = -500.*(1.0 + SUPER)
    EVPTR = SUPCL - SUPR
    IF (.NOT. PRINT) RETURN !GO TO 200

    ! VL: Previously :
    !    IF(SUPER.LT.0.0) GO TO 1
    !    IF (Unit .EQ. 1) THEN
    !        IF (PrnLog .EQ. 1) WRITE(6,FMT_804)'           Desired superheat = ',SUPER/1.8,DTunit
    !        IF (PrnCon .EQ. 1) WRITE(*,FMT_804)'           Desired superheat = ',SUPER/1.8,DTunit
    !    ELSE
    !        IF (PrnLog .EQ. 1) WRITE(6,FMT_804)'           Desired superheat = ',SUPER,DTunit
    !        IF (PrnCon .EQ. 1) WRITE(*,FMT_804)'           Desired superheat = ',SUPER,DTunit
    !    END IF
    !    GO TO 2
    !1   CONTINUE
    !    SXIC = -SUPER
    !    IF (PrnLog .EQ. 1) WRITE(6,FMT_804)'           Desired quality = ',SXIC*100,Xunit
    !    IF (PrnCon .EQ. 1) WRITE(*,FMT_804)'           Desired quality = ',SXIC*100,Xunit
    !2   CONTINUE

    IF(SUPER.LT.0.0) THEN

        SXIC = -SUPER
        IF (PrnLog .EQ. 1) WRITE(6,FMT_804)'           Desired quality = ',SXIC*100,Xunit
        IF (PrnCon .EQ. 1) WRITE(*,FMT_804)'           Desired quality = ',SXIC*100,Xunit

    ELSE
        IF (Unit .EQ. 1) THEN
            IF (PrnLog .EQ. 1) WRITE(6,FMT_804)'           Desired superheat = ',SUPER/1.8,DTunit
            IF (PrnCon .EQ. 1) WRITE(*,FMT_804)'           Desired superheat = ',SUPER/1.8,DTunit
        ELSE
            IF (PrnLog .EQ. 1) WRITE(6,FMT_804)'           Desired superheat = ',SUPER,DTunit
            IF (PrnCon .EQ. 1) WRITE(*,FMT_804)'           Desired superheat = ',SUPER,DTunit
        END IF

    END IF


    ! VL: Previously:
    !    IF(XICMP.LT.1.0) GO TO 3
    !    IF (Unit .EQ. 1) THEN
    !        IF (PrnLog .EQ. 1) WRITE(6,FMT_804)'        Calculated superheat = ',SUPCAL/1.8,DTunit
    !        IF (PrnCon .EQ. 1) WRITE(*,FMT_804)'        Calculated superheat = ',SUPCAL/1.8,DTunit
    !    ELSE  
    !        IF (PrnLog .EQ. 1) WRITE(6,FMT_804)'        Calculated superheat = ',SUPCAL,DTunit
    !        IF (PrnCon .EQ. 1) WRITE(*,FMT_804)'        Calculated superheat = ',SUPCAL,DTunit
    !    END IF
    !    GO TO 4
    !3   CONTINUE
    !    IF (XICMP .LT. 0.0) GO TO 33
    !    IF (PrnLog .EQ. 1) WRITE(6,FMT_804)'        Calculated quality = ',XICMP*100,Xunit
    !    IF (PrnCon .EQ. 1) WRITE(*,FMT_804)'        Calculated quality = ',XICMP*100,Xunit
    !    GO TO 4
    !33  CONTINUE
    !    IF (Unit .EQ. 1) THEN
    !        IF (PrnLog .EQ. 1) WRITE(6,FMT_804)'       Calculated subcooling = ',-SUPCAL/1.8,DTunit
    !        IF (PrnCon .EQ. 1) WRITE(*,FMT_804)'       Calculated subcooling = ',-SUPCAL/1.8,DTunit
    !    ELSE  
    !        IF (PrnLog .EQ. 1) WRITE(6,FMT_804)'       Calculated subcooling = ',-SUPCAL,DTunit
    !        IF (PrnCon .EQ. 1) WRITE(*,FMT_804)'       Calculated subcooling = ',-SUPCAL,DTunit
    !    END IF
    !4   CONTINUE

    IF (XICMP.LT.1.0) THEN

        IF (XICMP .LT. 0.0) THEN

            IF (Unit .EQ. 1) THEN
                IF (PrnLog .EQ. 1) WRITE(6,FMT_804)'       Calculated subcooling = ',-SUPCAL/1.8,DTunit
                IF (PrnCon .EQ. 1) WRITE(*,FMT_804)'       Calculated subcooling = ',-SUPCAL/1.8,DTunit
            ELSE  
                IF (PrnLog .EQ. 1) WRITE(6,FMT_804)'       Calculated subcooling = ',-SUPCAL,DTunit
                IF (PrnCon .EQ. 1) WRITE(*,FMT_804)'       Calculated subcooling = ',-SUPCAL,DTunit
            END IF            

        ELSE
            
            IF (PrnLog .EQ. 1) WRITE(6,FMT_804)'        Calculated quality = ',XICMP*100,Xunit
            IF (PrnCon .EQ. 1) WRITE(*,FMT_804)'        Calculated quality = ',XICMP*100,Xunit
           
        END IF


    ELSE

        IF (Unit .EQ. 1) THEN
            IF (PrnLog .EQ. 1) WRITE(6,FMT_804)'        Calculated superheat = ',SUPCAL/1.8,DTunit
            IF (PrnCon .EQ. 1) WRITE(*,FMT_804)'        Calculated superheat = ',SUPCAL/1.8,DTunit
        ELSE  
            IF (PrnLog .EQ. 1) WRITE(6,FMT_804)'        Calculated superheat = ',SUPCAL,DTunit
            IF (PrnCon .EQ. 1) WRITE(*,FMT_804)'        Calculated superheat = ',SUPCAL,DTunit
        END IF

    END IF


!VL: Previously: 200 CONTINUE
    !DIFFER = EVPTR

    RETURN

!!VL: Previously: 800 FORMAT(A41,F7.2,A5)
!!VL: Previously: 804 FORMAT(A32,F7.2,A5)

    END FUNCTION

