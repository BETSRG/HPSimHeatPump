    REAL FUNCTION CNDNSR(TINPUT,IERR)
    !
    !       CNDNSR(TEMPERATURE) = CDTROC(TEMPERATURE) - DTROC OR
    !                           = XMRFLD(TEMPERATURE) - XMR(TEMPERATURE)
    !
    !       CNDNSR IS USED WITH THE ROOT SOLVER 'ZERO' TO FIND THE
    !              SATURATION TEMPERATURE OUT OF THE COMPRESSOR, TSOCMP,
    !              SO THAT EITHER
    !              THE CALCULATED SUBCOOLING OUT OF THE CONDENSER IS NEARLY
    !              EQUAL TO THE SPECIFIED SUBCOOLING:
    !              I.E. ABS(CDTROC(TSATCI) - DTROC) < CNDCON.
    !              OR
    !              THE FLOW CONTROL  MASS FLOW RATE IS NEARLY
    !              EQUAL TO THAT OF THE COMPRESSOR
    !              I.E. ABS[XMRFLD(TSATCI) - XMR(TSATCI)] < FLOCON.
    !
    !       'ZERO' CONTAINS ALL OF THE LOGIC NECESSARY TO ITERATE
    !              TO A ROOT, TSATCI.
    !
    ! IERR = 0:No error
    !      = 1:TINPUT is too high
    !      = 2:TINPUT is too low 

    USE FluidProperties_HPSim
    USE CondenserMod
    USE CompressorMod
    USE ShortTubeMod
    USE CapillaryTubeMod
    USE DataSimulation
    USE IFPORT

    IMPLICIT NONE

    CHARACTER (len=15) :: Property           
    INTEGER            :: RefrigIndex =0
    REAL Temperature,Quality,Pressure,Enthalpy

    REAL TINPUT
    INTEGER IERR

    LOGICAL PRINT
    REAL NTE,NSECTE

    INTEGER(2) RefPropOpt			!Ref prop calc. option
    INTEGER(2) RefPropErr			!Error flag:1-error; 0-no error
    REAL RefProp(28)	!Refrigerant properties

    INTEGER(2) AirPropOpt			!Air prop calc. option
    INTEGER(2) AirPropErr			!Error flag:1-error; 0-no error
    REAL AirProp(8)		!Air properties

    REAL,PARAMETER :: StandardDensity=1.2 !kg/m3

    INTEGER IREFC
    REAL XMR,TSATCI,TROC,TSATCO,CDTROC,TSATEI
    REAL CDTRC,DTRC,SXOC,XMRFLD,TSAVG,TRIE,CDTRIE,DTRE,CDTRE,DTRIE,SXIE
    REAL ID,L,Elevation,mdot,xi,xo,mu,muVap,muLiq,rhoi,rhoo,rhoiVap,rhoiLiq, &
    rhooVap,rhooLiq,DPfric,DPmom,DPgrav,DPtot
    REAL FilterDP
    REAL MassCoil,MassLiqCoil,MassVapCoil
    REAL SimpleCondOUT(29),DetailedCondOUT(29)
    REAL DetailedQcnd,DetailedDPcnd
    REAL SimpleQcnd,SimpleDPcnd
    LOGICAL,SAVE :: IsFirstTimeCondenser = .TRUE. !First time to call condenser flag
    INTEGER IsCoolingMode !Cooling mode flag: 1=yes, otherwise=no
    LOGICAL,SAVE :: IsCondenserAllocated = .FALSE. !Flag to check if the arrays in the
    !condenser model are allocated
    REAL, SAVE:: PrevTime = 0.0                                               

    !VL: Previously: 300 CONTINUE <-- SEE GOTO 300 in if statement at end of function ...
    IsCondenserAllocated = .FALSE.  !VL: the "SAVE" in the declaration causes a "TRUE" to persist causing a failure on a second call.
    DO WHILE (.NOT. IsCondenserAllocated)

        PRINT=.TRUE.
        IF (MODE .EQ. 2 .OR. MODE .EQ. 4 .OR. MODE .EQ. 5) THEN
            IREFC=0 !for specified subcooiling, set to zero
            !for specifed flow control, set to 3 
        ELSE
            IREFC=3
        END IF

        TSOCMP = TINPUT
        CNDNSR = 1.0E+10
        IERR = 0

        IF (.NOT. PRINT) THEN
            !VL: Previously: GO TO 200
            CYCLE
        END IF

        IF (Unit .EQ. 1) THEN

            IF (PrnLog .EQ. 1) WRITE(6,*)
            IF (PrnLog .EQ. 1) WRITE(6,900)'>> Compressor discharge saturation temperature: ',(TSOCMP-32)*5/9,Tunit
            WRITE(*,*)
            WRITE(*,900)'>> Compressor discharge saturation temperature: ',(TSOCMP-32)*5/9,Tunit

        ELSE

            IF (PrnLog .EQ. 1) WRITE(6,*)
            IF (PrnLog .EQ. 1) WRITE(6,900)'>> Compressor discharge saturation temperature: ',TSOCMP,Tunit
            WRITE(*,*)
            WRITE(*,900)'>> Compressor discharge saturation temperature: ',TSOCMP,Tunit

        END IF

        !     CALL SUBROUTINE COMP TO DETERMINE THE COMPRESSOR
        !     PERFORMANCE AND REFRIGERANT FLOW RATE 'XMR'

        Temperature=(TSOCMP-32)/1.8
        Quality=1
        PoCmp=TQ(Ref$,Temperature,Quality,'pressure',RefrigIndex,RefPropErr)
        IF (RefPropErr .GT. 0) THEN
            WRITE(*,*)'Trying another iterating value....'
            IERR=1
            !VL: Previously: GO TO 200
            CYCLE
        END IF
        PoCmp=PoCmp/1000

        Temperature=(TSICMP-32)/1.8
        Quality=1
        PiCmp=TQ(Ref$,Temperature,Quality,'pressure',RefrigIndex,RefPropErr)
        IF (RefPropErr .GT. 0) THEN
            WRITE(*,*)'Trying another iterating value....'
            IERR=1
            !VL: Previously: GO TO 200
            CYCLE
        END IF
        PiCmp=PiCmp/1000

        IF (SUPER .GT. 0) THEN
            Temperature=(TSICMP+SUPER-32)/1.8
            Pressure=PiCmp*1000
            HiCmp=TP(Ref$,Temperature,Pressure,'enthalpy',RefrigIndex,RefPropErr)
            IF (RefPropErr .GT. 0) THEN
                WRITE(*,*)'Trying another iterating value....'
                IERR=1
                !VL: Previously: GO TO 200
                CYCLE
            END IF
            HiCmp=HiCmp/1000
        ELSE

            Pressure=PiCmp*1000
            Quality=-SUPER
            HiCmp=PQ(Ref$,Pressure,Quality,'enthalpy',RefrigIndex,RefPropErr)
            IF (RefPropErr .GT. 0) THEN
                WRITE(*,*)'Trying another iterating value....'
                IERR=1
                !VL: Previously: GO TO 200
                CYCLE
            END IF
            HiCmp=HiCmp/1000

        END IF

        CompIN(1)=PiCmp
        CompIN(2)=PoCmp
        CompIN(3)=HiCmp
        CALL Compressor(Ref$,PureRef,CompIN,CompPAR,CompOUT)
        IF (CompOUT(7) .NE. 0) THEN
            SELECT CASE (INT(CompOUT(7)))
            CASE (1,2)
                WRITE(*,*)'Trying another iterating value....'
                IERR=1
                !VL: Previously: GO TO 200
                CYCLE
            END SELECT
        END IF

        XMR=CompOUT(2)*3600/UnitM
        HoCmp=CompOUT(3)
        ToCmp=CompOUT(5)

        CondIN(1)=XMR*UnitM/3600
        CondIN(2)=PoCmp         
        CondIN(3)=HoCmp         
        CondIN(4)=XMaC           
        CondIN(5)=(TAIC-32)/1.8  
        CondIN(6)=RHIC           
        CondIN(8)=EvapOUT(3)
        CondIN(9)=(TAIE-32)/1.8

        IF (SystemType .EQ. 4) THEN !Reheat system
            IF (FirstTimeFlowRateLoop) THEN
                CondIN(4)=XMaE
                CondIN(5)=(TAIE-32)/1.8
                CondIN(6)=RHIE
            ELSE
                CondIN(4)=XMaE
                CondIN(5)=EvapOUT(17)
                CondIN(6)=EvapOUT(18)

            END IF

        END IF

        !Take compressor shell loss into account
        IF (CompPAR(21) .NE. 0) THEN !Shell loss in fraction
            CondPAR(39)=CompPAR(21)*CompOUT(1)
        ELSE !Shell loss in W
            CondPAR(39)=CompPAR(22)/1000
        END IF

        IsCoolingMode=CondPAR(27)
        !Change the logic to reset IsFirstTimeCondenser
        !IF(PrevTime .NE. CurSimTime)THEN
        ! IsFirstTimeCondenser = .TRUE.
        ! PrevTime=CurSimTime
        !END IF 
        IF ((IsCoolingMode .GT. 0 .AND. ODCcoilType .EQ. MCCONDENSER) .OR. &
        (IsCoolingMode .LT. 1 .AND. IDCcoilType .EQ. MCCONDENSER)) THEN
            !Microchannel coil
            IF (IsFirstTimeCondenser) THEN 
                CondPAR(62)=1 !First time
                CondPAR(61)=0 !Detailed version
                IsFirstTimeCondenser=.FALSE.
            END IF
            CALL Condenser(Ref$,PureRef,CondIN,CondPAR,CondOUT)
            CondPAR(62)=0 !No longer first time
            IsCondenserAllocated=.TRUE.
        ELSE
            !Plate-fin coil
            !Run both simple and detailed version to determine which one to use
            IF (IsFirstTimeCondenser) THEN 
                CondPAR(62)=1 !First time

                CondPAR(61)=0 !Detailed version
                CALL Condenser(Ref$,PureRef,CondIN,CondPAR,DetailedCondOUT)
                !CALL CalcCondenserInventory(MassCoil,MassLiqCoil,MassVapCoil,CondLiqTubeLength,CondVapTubeLength,CondTwoPhaseTubeLength,CondNumLiqTubes)
                DetailedQcnd=DetailedCondOUT(15)
                DetailedDPcnd=CondIN(2)-DetailedCondOUT(10)
                CALL EndCondenserCoil

                CondPAR(61)=1 !Simple version
                CALL Condenser(Ref$,PureRef,CondIN,CondPAR,SimpleCondOUT)
                !CALL CalcCondenserInventory(MassCoil,MassLiqCoil,MassVapCoil,CondLiqTubeLength,CondVapTubeLength,CondTwoPhaseTubeLength,CondNumLiqTubes)
                SimpleQcnd=SimpleCondOUT(15)
                SimpleDPcnd=CondIN(2)-SimpleCondOUT(10)
                CALL EndCondenserCoil

                IF (ABS((SimpleQcnd-DetailedQcnd)/DetailedQcnd) .LT. 0.1 .AND. &
                ABS((SimpleDPcnd-DetailedDPcnd)/DetailedDPcnd) .LT. 0.1) THEN
                    CondPAR(61)=1
                    CondOUT=SimpleCondOUT
                ELSE
                    CondPAR(61)=0
                    CondOUT=DetailedCondOUT
                END IF 
                IsFirstTimeCondenser=.FALSE.

                !Always detailed
                CondPAR(61)=0
                CondOUT=DetailedCondOUT

            ELSE
                CALL Condenser(Ref$,PureRef,CondIN,CondPAR,CondOUT)
                CondPAR(62)=0 !No longer first time
                IsCondenserAllocated=.TRUE.
            END IF

        END IF
        !CALL PrintCondenserResult 

        IF (CondOUT(24) .NE. 0) THEN
            SELECT CASE (INT(CondOUT(24))) 
            CASE (2) !Refprop error
                WRITE(*,*)'Trying another iterating value....'
                IERR=1
                !VL: Previously: GO TO 200
                CYCLE
            CASE (3)
                !WRITE(*,*)'Press return to terminate program'
                !READ(*,*)
                !RS Comment: Previously: CALL SLEEP(300) !Wait for 5 minutes and stop
                STOP
            CASE (4,5)
                WRITE(*,*)'## ERROR ## Highside: Coil geometry misdefined.'
                !WRITE(*,*)'Press return to terminate program'
                !READ(*,*)
                !RS Comment: Previously: CALL SLEEP(300) !Wait for 5 minutes and stop
                STOP
            CASE (8) !Too much pressure drop
                WRITE(*,*)'Trying another iterating value....'
                IERR=2
                !VL: Previously: GO TO 200
                CYCLE
            END SELECT
        END IF

        PiCnd=CondOUT(1)
        HiCnd=CondOUT(2)
        TiCnd=CondOUT(3)
        XiCnd=CondOUT(4)
        PoCnd=CondOUT(5)
        HoCnd=CondOUT(6)
        ToCnd=CondOUT(7)
        XoCnd=CondOUT(8)
        PiExp=CondOUT(10)
        HiExp=CondOUT(11)
        TiExp=CondOUT(12)
        XiExp=CondOUT(13)

        IF (XiExp .GT. 1) THEN !Condenser outlet is still in superheated region, ISI - 06/06/07
            WRITE(*,*)'Trying another iterating value....'
            IERR=1
            !VL: Previously: GO TO 200
            CYCLE
        END IF

        Pressure=PiCnd*1000
        Quality=1
        TSATCI=PQ(Ref$,Pressure,Quality,'temperature',RefrigIndex,RefPropErr)
        IF (RefPropErr .GT. 0) THEN
            IF (PrnCon .EQ. 1) WRITE(*,*)'-- WARNING -- Highside: Refprop error.'
            IF (PrnLog .EQ. 1) WRITE(6,*)'-- WARNING -- Highside: Refprop error.'
            WRITE(*,*)'Trying another iterating value....'
            IERR=1
            !VL: Previously: GO TO 200
            CYCLE
        END IF
        TSATCI=TSATCI*1.8+32

        IF (FilterPAR(1) .GT. 0) THEN !Filter drier exits
            FilterIN(1)=CondIN(1) !Mass flow rate, kg/s
            CALL CalcFilterDrierDP(FilterIN(1),FilterPAR,FilterOUT,Ref$)
            FilterDP=FilterOUT(1)

            PiExp=PiExp-FilterDP
            CondOUT(10)=PiExp

            Pressure=PiExp*1000
            Enthalpy=HiExp*1000
            XiExp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
            IF (RefPropErr .GT. 0) THEN
                WRITE(*,*)'-- WARNING -- Highside: Refprop error.'
                IERR=1
                !VL: Previously: GO TO 200
                CYCLE
            END IF
        END IF 

        TRIE=TiExp*1.8+32

        Pressure=PiExp*1000
        Quality=0
        TSATEI=PQ(Ref$,Pressure,Quality,'temperature',RefrigIndex,RefPropErr)
        IF (RefPropErr .GT. 0) THEN
            IF (PrnCon .EQ. 1) WRITE(*,*)'-- WARNING -- Highside: Refprop error.'
            IF (PrnLog .EQ. 1) WRITE(6,*)'-- WARNING -- Highside: Refprop error.'
            WRITE(*,*)'Trying another iterating value....'
            IERR=1
            !VL: Previously: GO TO 200
            CYCLE
        END IF

        TSATEI=TSATEI*1.8+32

        TSAVG=(TSATCI+TSATEI)/2
        IF(TSAVG.LT.TAIC) THEN
            IF (PrnCon .EQ. 1) WRITE(*,*)'-- WARNING -- Highside: Ref. temperature lower than inlet air temperature.'
            IF (PrnLog .EQ. 1) WRITE(6,*)'-- WARNING -- Highside: Ref. temperature lower than inlet air temperature.'
            WRITE(*,*)'Trying another iterating value....'
            IF (TSOCMP .LE. TSICMP) THEN
                IF (PrnCon .EQ. 1) WRITE(*,*)'## ERROR ## Highside: No solution for this configuration.'
                IF (PrnCon .EQ. 1) WRITE(*,*)'Try another condenser or compressor.'
                IF (PrnLog .EQ. 1) WRITE(6,*)'## ERROR ## Highside: No solution for this configuration.'
                IF (PrnLog .EQ. 1) WRITE(6,*)'Try another condenser or compressor.'
                !WRITE(*,*)'Press return to terminate program'
                !READ(*,*)
                !RS Comment: Previously: CALL SLEEP(300) !Wait for 5 minutes and stop
                STOP
            END IF
            IERR=2
        END IF

        IF (IERR .GE. 1) THEN
            !VL: Previously: GO TO 200
            CYCLE
        END IF


        CDTRIE = TSATEI - TRIE
        CDTRIE=CondOUT(14)*1.8 !ISI - 10/07/06
        !CDTRIE=CondOUT(9)*1.8 !ISI - 05/28/08

        !VL: Previously: IF(IREFC.NE.0) GO TO 100
        IF(IREFC.EQ.0) THEN

            CDTRE = CDTRIE
            IF (XIEXP .GT. 1.) THEN !Edited for refprop table 01-15-04 - ISI
                CDTRE = CDTRIE !-CDTRIE !ISI - 08/06/06
            ELSEIF (XIEXP .GT. 0. .AND. XIEXP .LT. 1) THEN
                CDTRE = -200.0*XIEXP
            END IF

            DTRIE=DTROC
            DTRE = DTRIE
            IF (DTRIE .LT. 0.) DTRE = 200.*DTRIE

            CNDNSR = CDTRE - DTRE

            MdotR=XMR*UnitM/3600

            ! VL: Previously: 
            !      IF(DTRIE.LT.0.0) GO TO 1
            !      IF (Unit .EQ. 1) THEN
            !	      IF (PrnLog .EQ. 1) WRITE(6,904)'           Desired subcooling = ',DTRIE/1.8,DTunit
            !          IF (PrnCon .EQ. 1) WRITE(*,904)'           Desired subcooling = ',DTRIE/1.8,DTunit
            !	  ELSE
            !	      IF (PrnLog .EQ. 1) WRITE(6,904)'           Desired subcooling = ',DTRIE,DTunit
            !          IF (PrnCon .EQ. 1) WRITE(*,904)'           Desired subcooling = ',DTRIE,DTunit
            !	  END IF
            !      GO TO 2
            !1 CONTINUE
            !      SXIE = -DTRIE
            !      IF (PrnLog .EQ. 1) WRITE(6,904)'           Desired quality = ',SXIE*100,Xunit
            !      IF (PrnCon .EQ. 1) WRITE(*,904)'           Desired quality = ',SXIE*100,Xunit
            !2     CONTINUE
            !      IF(XIEXP.GT.0.0) GO TO 3
            !      IF (Unit .EQ. 1) THEN
            !	      IF (PrnLog .EQ. 1) WRITE(6,904)'        Calculated subcooling = ',CDTRIE/1.8,DTunit
            !          IF (PrnCon .EQ. 1) WRITE(*,904)'        Calculated subcooling = ',CDTRIE/1.8,DTunit
            !	  ELSE  
            !		  IF (PrnLog .EQ. 1) WRITE(6,904)'        Calculated subcooling = ',CDTRIE,DTunit
            !          IF (PrnCon .EQ. 1) WRITE(*,904)'        Calculated subcooling = ',CDTRIE,DTunit
            !	  END IF
            !      GO TO 4
            !3 CONTINUE
            !	  IF (XIEXP .LT. 1) THEN
            !	      IF (PrnLog .EQ. 1) WRITE(6,904)'        Calculated quality = ',XIEXP*100,Xunit
            !          IF (PrnCon .EQ. 1) WRITE(*,904)'        Calculated quality = ',XIEXP*100,Xunit
            !	  ELSE
            !          IF (PrnLog .EQ. 1) WRITE(6,904)'      Calculated superheat = ',-CDTRIE,DTunit
            !          IF (PrnCon .EQ. 1) WRITE(*,904)'      Calculated superheat = ',-CDTRIE,DTunit
            !	  END IF
            !4     CONTINUE

            IF(DTRIE.LT.0.0) THEN
                SXIE = -DTRIE
                IF (PrnLog .EQ. 1) WRITE(6,904)'           Desired quality = ',SXIE*100,Xunit
                IF (PrnCon .EQ. 1) WRITE(*,904)'           Desired quality = ',SXIE*100,Xunit
            ELSE
                IF (Unit .EQ. 1) THEN
                    IF (PrnLog .EQ. 1) WRITE(6,904)'           Desired subcooling = ',DTRIE/1.8,DTunit
                    IF (PrnCon .EQ. 1) WRITE(*,904)'           Desired subcooling = ',DTRIE/1.8,DTunit
                ELSE
                    IF (PrnLog .EQ. 1) WRITE(6,904)'           Desired subcooling = ',DTRIE,DTunit
                    IF (PrnCon .EQ. 1) WRITE(*,904)'           Desired subcooling = ',DTRIE,DTunit
                END IF
            END IF


            IF(XIEXP.GT.0.0) THEN
                IF (XIEXP .LT. 1) THEN
                    IF (PrnLog .EQ. 1) WRITE(6,904)'        Calculated quality = ',XIEXP*100,Xunit
                    IF (PrnCon .EQ. 1) WRITE(*,904)'        Calculated quality = ',XIEXP*100,Xunit
                ELSE
                    IF (PrnLog .EQ. 1) WRITE(6,904)'      Calculated superheat = ',-CDTRIE,DTunit
                    IF (PrnCon .EQ. 1) WRITE(*,904)'      Calculated superheat = ',-CDTRIE,DTunit
                END IF
            ELSE
                IF (Unit .EQ. 1) THEN
                    IF (PrnLog .EQ. 1) WRITE(6,904)'        Calculated subcooling = ',CDTRIE/1.8,DTunit
                    IF (PrnCon .EQ. 1) WRITE(*,904)'        Calculated subcooling = ',CDTRIE/1.8,DTunit
                ELSE  
                    IF (PrnLog .EQ. 1) WRITE(6,904)'        Calculated subcooling = ',CDTRIE,DTunit
                    IF (PrnCon .EQ. 1) WRITE(*,904)'        Calculated subcooling = ',CDTRIE,DTunit
                END IF
            END IF


            !VL: Previously: GO TO 200
            CYCLE            
        END IF
        !VL: Previously: 100     CONTINUE


        PiEvp=EvapIN(2)
        PoExp=PiEvp

        IF (ExpDevice .EQ. 3) THEN

            CapTubeIN(1)=CompOUT(2)  !Compressor mass flow rate
            CapTubeIN(2)=PiExp       !Inlet pressure
            CapTubeIN(3)=HiExp       !Inlet enthalpy
            CapTubeIN(4)=PiEvp       !Evaporator inlet pressure
            CapTubeIN(5)=EvapOUT(1)  !Evaporator outlet pressure

            !CALL CapillaryTubeChoi(Ref$,PureRef,CapTubeIN,CapTubePAR,CapTubeOUT)  
            CALL CapillaryTubeORNL(Ref$,PureRef,CapTubeIN,CapTubePAR,CapTubeOUT)  

            XMRFLD=CapTubeOUT(1)*3600/UnitM
            ToExp=CapTubeOUT(3)
            XoExp=CapTubeOUT(4)

        ELSE
            ShTbIN(1)=CompOUT(2) !Compressor mass flow rate, kg/s
            ShTbIN(2)=PiExp
            ShTbIN(3)=HiExp
            ShTbIN(4)=PiEvp
            ShTbIN(5)=EvapOUT(1)

            !CALL ShortTube(Ref$,PureRef,ShTbIN,ShTbPAR,ShTbOUT)
            CALL ShortTubePayne(Ref$,PureRef,ShTbIN,ShTbPAR,ShTbOUT)
            IF (ShTbOUT(7) .NE. 0) THEN
                SELECT CASE (INT(ShTbOUT(7)))
                CASE (1)
                    IF (PrnCon .EQ. 1) WRITE(*,*)
                    IF (PrnLog .EQ. 1) WRITE(6,*)
                    IF (PrnCon .EQ. 1) WRITE(*,*)'## ERROR ## Highside: Short tube solution error.'
                    IF (PrnLog .EQ. 1) WRITE(6,*)'## ERROR ## Highside: Short tube solution error.'
                    !WRITE(*,*)'Press return to terminate program'
                    !READ(*,*)
                    !RS Comment: Previously: CALL SLEEP(300) !Wait for 5 minutes and stop
                    STOP
                CASE (2)
                    WRITE(*,*)'Trying another iterating value....'
                    IERR=1
                    !VL: Previously: GO TO 200
                    CYCLE
                END SELECT
            END IF

            XMRFLD=ShTbOUT(1)*3600/UnitM
            ToExp=ShTbOUT(3)
            XoExp=ShTbOUT(4)
        END IF

        HoExp=HiExp
        EvapIN(3)=HoExp

        !CNDNSR = ( XMRFLD - XMR ) / 20
        CNDNSR = ( XMRFLD - XMR )

        MdotR=XMR*UnitM/3600

        IF(.NOT. PRINT) THEN
            !VL: Previously: GO TO 200
            CYCLE
        END IF

        IF (Unit .EQ. 1) THEN
            IF (PrnCon .EQ. 1) WRITE(*,904)'     Compressor flow rate = ',XMR*UnitM,MdotUnit
            IF (PrnLog .EQ. 1) WRITE(6,904)'     Compressor flow rate = ',XMR*UnitM,MdotUnit
            IF (PrnCon .EQ. 1) WRITE(*,904)'    Exp. device flow rate = ',XMRFLD*UnitM,MdotUnit
            IF (PrnLog .EQ. 1) WRITE(6,904)'    Exp. device flow rate = ',XMRFLD*UnitM,MdotUnit
        ELSE
            IF (PrnCon .EQ. 1) WRITE(*,904)'     Compressor flow rate = ',XMR,MdotUnit
            IF (PrnLog .EQ. 1) WRITE(6,904)'     Compressor flow rate = ',XMR,MdotUnit
            IF (PrnCon .EQ. 1) WRITE(*,904)'    Exp. device flow rate = ',XMRFLD,MdotUnit
            IF (PrnLog .EQ. 1) WRITE(6,904)'    Exp. device flow rate = ',XMRFLD,MdotUnit
        END IF

        !VL: Previously: 200     CONTINUE

        !VL: Previously: IF (.NOT. IsCondenserAllocated) GO TO 300 !ISI - 12/27/06 <-- conditional moved to while at beginning of function definition
    END DO


    RETURN

900 FORMAT (A50,F7.2,A5)
904 FORMAT (A32,F7.2,A9)

    END FUNCTION
