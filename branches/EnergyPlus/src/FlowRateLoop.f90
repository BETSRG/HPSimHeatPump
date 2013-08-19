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

    USE FluidProperties_HPSim !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)
    USE CondenserMod
    USE CompressorMod
    USE ShortTubeMod
    USE CapillaryTubeMod
    USE DataSimulation
    USE DataGlobals, ONLY: RefrigIndex  !RS: Debugging: Removal of plethora of RefrigIndex definitions in the code

    IMPLICIT NONE

    REAL Temperature,Quality,Pressure,Enthalpy

    REAL TINPUT
    INTEGER IERR

    LOGICAL PRINT

    INTEGER(2) RefPropErr			!Error flag:1-error; 0-no error
    REAL,PARAMETER :: StandardDensity=1.2 !kg/m3

    INTEGER IREFC
    REAL XMR,TSATCI,TSATEI
    REAL XMRFLD,TSAVG,TRIE,CDTRIE,DTRE,CDTRE,DTRIE,SXIE 
    REAL FilterDP
    REAL SimpleCondOUT(29),DetailedCondOUT(29)
    REAL DetailedQcnd,DetailedDPcnd
    REAL SimpleQcnd,SimpleDPcnd
    LOGICAL,SAVE :: IsFirstTimeCondenser = .TRUE. !First time to call condenser flag
    INTEGER IsCoolingMode !Cooling mode flag: 1=yes, otherwise=no
    LOGICAL :: IsCondenserAllocated = .FALSE. !Flag to check if the arrays in the condenser model are allocated !RS: See VL's note 6 lines below
    INTEGER, SAVE :: ErrorCount = 0  !RS: Debugging
    INTEGER, SAVE :: LoopCount = 0   !RS: Debugging
    INTEGER, SAVE :: LoopCountSmall = 0 !RS: Debugging
    
    CHARACTER(LEN=13),PARAMETER :: FMT_900 = "(A50,F7.2,A5)"
    CHARACTER(LEN=13),PARAMETER :: FMT_904 = "(A32,F7.2,A9)"
    !RS: Debugging: Check if the following line is really necessary
    IsCondenserAllocated = .FALSE.  !VL: the "SAVE" in the declaration causes a "TRUE" to persist causing a failure on a second call.
    DO WHILE (.NOT. IsCondenserAllocated)

        PRINT=.TRUE.
        !IF (MODE .EQ. 2 .OR. MODE .EQ. 4 .OR. MODE .EQ. 5) THEN    !RS: Debugging: This is a relic of the MODE mismatch between sim and design
            !IREFC=0 !for specified subcooling, set to zero
            !for specifed flow control, set to 3 
        !ELSE
            IREFC=3
        !END IF

        TSOCMP = TINPUT
        CNDNSR = 1.0E+10
        IERR = 0
        
        IF (ErrorCount .EQ. 1) THEN !RS: Debugging: Getting the next iteration to actually try a different value
            LoopCount = LoopCount + 1
            IF (TSOCMP .GE. (2*LoopCount)) THEN
                TSOCMP=TSOCMP-(2*LoopCount) !RS: Debugging: Just trying to actually get the temp to change value
            !ELSE    !RS: Debugging: Dealing with the case where the temperature needs to be raised to iterate properly                TSOCMP=TSOCMP+(2*LoopCount)

            ELSEIF (TSOCMP .LE. 0 .AND. ABS(TSOCMP) .GE. 50) THEN   !RS: Debugging
                !IF (TSOCMP .GE. -50) THEN   !RS: Debugging, trying to deal with case where TSOCMP is a large negative number
                IF ((TSOCMP+(50*LoopCount)) .GE. 0) THEN    !RS: Debugging
                    LoopCountSmall=1+LoopCountSmall
                    TSOCMP=2*LoopCountSmall
                ELSEIF ((TSOCMP+(1000*(LoopCount-1))) .LE. 0 .AND. ABS(TSOCMP+(1000*(LoopCount-1))) .GE. 1000) THEN
                    TSOCMP=TSOCMP +(1000*LoopCount)
                ELSE
                    TSOCMP=TSOCMP+(50*LoopCount)
                END IF
            ELSE    !RS: Debugging
                TSOCMP=TSOCMP + (2*LoopCount)
            END IF
            
        ELSEIF (ErrorCount .EQ. 2) THEN
            IF (TSICMP .GE. (2*LoopCount)) THEN !RS: Debugging: In case of TSICMP being the problem
                TSICMP=TSICMP-(2*LoopCount) !RS: Debugging: Just trying to actually get the temp to change value
            ELSEIF (TSICMP .LE. 0 .AND. ABS(TSICMP) .GE. 50) THEN   !RS: Debugging
                !IF (TSOCMP .GE. -50) THEN   !RS: Debugging, trying to deal with case where TSOCMP is a large negative number
                IF ((TSICMP+(50*LoopCount)) .GE. 0) THEN    !RS: Debugging
                    LoopCountSmall=1+LoopCountSmall
                    TSICMP=2*LoopCountSmall
                ELSEIF ((TSICMP+(1000*(LoopCount-1))) .LE. 0 .AND. ABS(TSICMP+(1000*(LoopCount-1))) .GE. 1000) THEN
                    TSICMP=TSICMP +(1000*LoopCount)
                ELSE
                    TSICMP=TSICMP+(50*LoopCount)
                END IF
            ELSE    !RS: Debugging
                TSICMP=TSICMP + (2*LoopCount)
            END IF
            
        END IF

        IF (.NOT. PRINT) THEN
            CYCLE
        END IF

        IF (Unit .EQ. 1) THEN

            IF (PrnLog .EQ. 1) THEN
                WRITE(6,*)
            END IF
            IF (PrnLog .EQ. 1) THEN
                WRITE(6,FMT_900)'>> Compressor discharge saturation temperature: ',(TSOCMP-32)*5/9,Tunit 
            END IF
            WRITE(*,*)
            WRITE(*,FMT_900)'>> Compressor discharge saturation temperature: ',(TSOCMP-32)*5/9,Tunit

        ELSE

            IF (PrnLog .EQ. 1) THEN
                WRITE(6,*)
            END IF
            IF (PrnLog .EQ. 1) THEN
                WRITE(6,FMT_900)'>> Compressor discharge saturation temperature: ',TSOCMP,Tunit
            END IF
            WRITE(*,*)
            WRITE(*,FMT_900)'>> Compressor discharge saturation temperature: ',TSOCMP,Tunit

        END IF

        !     CALL SUBROUTINE COMP TO DETERMINE THE COMPRESSOR
        !     PERFORMANCE AND REFRIGERANT FLOW RATE 'XMR'

        Temperature=(TSOCMP-32)/1.8 !RS Comment: Unit Conversion, from F to C
        Quality=1
        PoCmp=TQ(Ref$,Temperature,Quality,'pressure',RefrigIndex,RefPropErr)    !Compressor Outlet Pressure
        IF (RefPropErr .GT. 0) THEN
            WRITE(*,*)'Trying another iterating value....'
            ErrorCount = 1  !RS: Debugging: Flag set to force it to iterate
            CYCLE
        END IF
        PoCmp=PoCmp/1000    !RS Comment: Unit Conversion

        Temperature=(TSICMP-32)/1.8 !RS Comment: Unit Conversion, from F to C
        Quality=1
        PiCmp=TQ(Ref$,Temperature,Quality,'pressure',RefrigIndex,RefPropErr)    !Compressor Inlet Pressure
        IF (RefPropErr .GT. 0) THEN
            WRITE(*,*)'Trying another iterating value....'
            IERR=1
            ErrorCount = 2  !RS: Debugging: Flag set to force it to iterate
            CYCLE
        END IF
        PiCmp=PiCmp/1000    !RS Comment: Unit Conversion

        IF (SUPER .GT. 0) THEN
            Temperature=(TSICMP+SUPER-32)/1.8   !RS Comment: Unit Conversion, from F to C
            Pressure=PiCmp*1000 !RS Comment: Unit Conversion
            HiCmp=TP(Ref$,Temperature,Pressure,'enthalpy',RefrigIndex,RefPropErr)   !Compressor Inlet Enthalpy
            IF (RefPropErr .GT. 0) THEN
                WRITE(*,*)'Trying another iterating value....'
                IERR=1
                !ErrorCount = 2  !RS: Debugging: Flag set to force it to iterate
                CYCLE
            END IF
            HiCmp=HiCmp/1000    !RS Comment: Unit Conversion
        ELSE
            Pressure=PiCmp*1000 !RS Comment: Unit Conversion
            Quality=-SUPER
            HiCmp=PQ(Ref$,Pressure,Quality,'enthalpy',RefrigIndex,RefPropErr)   !Compressor Inlet Enthalpy
            IF (RefPropErr .GT. 0) THEN
                WRITE(*,*)'Trying another iterating value....'
                IERR=1
                !ErrorCount = 1  !RS: Debugging: Flag set to force it to iterate
                CYCLE
            END IF
            HiCmp=HiCmp/1000    !RS Comment: Unit Conversion
        END IF
        
        IF (TSOCMP .LT. 0) THEN !RS: Debugging: Trying to account for it not working when the temp is too low
            WRITE(*,*)'Trying another iterating value....'
            IERR=1
            !ErrorCount = 1  !RS: Debugging: Flag set to force it to iterate
            CYCLE
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
                !ErrorCount=1    !RS: Trying to actually use a different value for iteration
                CYCLE
            END SELECT
        END IF
                
        IF (ErrorCount .NE. 0) THEN !RS: Debugging: Resetting the ErrorCount to 0 after the cycle ends
            ErrorCount=0
            LoopCount=0 !RS: Debugging: Also resetting the LoopCount for 0 so the temperature doesn't go negative
            LoopCountSmall=0
        END IF

        XMR=CompOUT(2)*3600/UnitM   !RS Comment: Unit Conversion, lbm/s??
        HoCmp=CompOUT(3)
        ToCmp=CompOUT(5)

        CondIN(1)=XMR*UnitM/3600    !RS Comment: Unit Conversion, kg/hr???
        CondIN(2)=PoCmp         
        CondIN(3)=HoCmp         
        CondIN(4)=XMaC           
        CondIN(5)=(TAIC-32)/1.8 !RS Comment: Unit Conversion, from F to C
        CondIN(6)=RHIC           
        CondIN(8)=EvapOUT(3)
        CondIN(9)=(TAIE-32)/1.8 !RS Comment: Unit Conversion, from F to C

        IF (SystemType .EQ. 4) THEN !Reheat system
            IF (FirstTimeFlowRateLoop) THEN
                CondIN(4)=XMaE
                CondIN(5)=(TAIE-32)/1.8 !RS Comment: Unit Conversion, from F to C
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
            CondPAR(39)=CompPAR(22)/1000    !RS Comment: Unit Conversion, from kW to W?
        END IF

        IsCoolingMode=CondPAR(27)
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
                DetailedQcnd=DetailedCondOUT(15)
                DetailedDPcnd=CondIN(2)-DetailedCondOUT(10)
                !CALL EndCondenserCoil  !RS: Debugging

                CondPAR(61)=1 !Simple version
                CALL Condenser(Ref$,PureRef,CondIN,CondPAR,SimpleCondOUT)
                SimpleQcnd=SimpleCondOUT(15)
                SimpleDPcnd=CondIN(2)-SimpleCondOUT(10)
                !CALL EndCondenserCoil  !RS: Debugging

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

        IF (CondOUT(24) .NE. 0) THEN
            SELECT CASE (INT(CondOUT(24))) 
            CASE (2) !Refprop error
                WRITE(*,*)'Trying another iterating value....'
                IERR=1
                CYCLE
            CASE (3)    !RS: Debugging: This is an useless stop---doesn't tell us what's wrong!
                !STOP   !RS: This is for a Circuit File Error
                WRITE(*,*) 'Circuit File Error' !RS: Debugging: Because I may as well...
            CASE (4,5)
                WRITE(*,*)'## ERROR ## Highside: Coil geometry misdefined.'
                STOP
            CASE (8) !Too much pressure drop
                WRITE(*,*)'Trying another iterating value....'
                IERR=2
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
            CYCLE
        END IF

        Pressure=PiCnd*1000 !RS Comment: Unit Conversion
        Quality=1
        TSATCI=PQ(Ref$,Pressure,Quality,'temperature',RefrigIndex,RefPropErr)
        IF (RefPropErr .GT. 0) THEN
            IF (PrnCon .EQ. 1) THEN
                WRITE(*,*)'-- WARNING -- Highside: Refprop error.'
            END IF
            IF (PrnLog .EQ. 1) THEN
                !WRITE(6,*)'-- WARNING -- Highside: Refprop error.' !RS: Debugging: File Check
            END IF
            WRITE(*,*)'Trying another iterating value....'
            IERR=1
            CYCLE
        END IF
        TSATCI=TSATCI*1.8+32    !RS Comment: Unit Conversion, from C to F

        IF (FilterPAR(1) .GT. 0) THEN !Filter drier exits
            FilterIN(1)=CondIN(1) !Mass flow rate, kg/s
            !CALL CalcFilterDrierDP(FilterIN(1),FilterPAR,FilterOUT,Ref$)
            CALL CalcFilterDrierDP(FilterIN(1),FilterPAR,FilterOUT)
            FilterDP=FilterOUT(1)

            PiExp=PiExp-FilterDP
            CondOUT(10)=PiExp

            Pressure=PiExp*1000 !RS Comment: Unit Conversion
            Enthalpy=HiExp*1000 !RS Comment: Unit Conversion
            XiExp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)   !Expansion Device Inlet Quality
            IF (RefPropErr .GT. 0) THEN
                WRITE(*,*)'-- WARNING -- Highside: Refprop error.'
                IERR=1
                CYCLE
            END IF
        END IF 

        TRIE=TiExp*1.8+32   !RS Comment: Unit Conversion, from C to F

        Pressure=PiExp*1000 !RS Comment: Unit Conversion
        Quality=0
        TSATEI=PQ(Ref$,Pressure,Quality,'temperature',RefrigIndex,RefPropErr)
        IF (RefPropErr .GT. 0) THEN
            IF (PrnCon .EQ. 1) THEN 
                WRITE(*,*)'-- WARNING -- Highside: Refprop error.'
            END IF
            IF (PrnLog .EQ. 1) THEN
                WRITE(6,*)'-- WARNING -- Highside: Refprop error.' 
            END IF
            WRITE(*,*)'Trying another iterating value....'
            IERR=1
            CYCLE
        END IF

        TSATEI=TSATEI*1.8+32    !RS Comment: Unit Conversion, from C to F

        TSAVG=(TSATCI+TSATEI)/2
        IF(TSAVG.LT.TAIC) THEN
            IF (PrnCon .EQ. 1) THEN
                WRITE(*,*)'-- WARNING -- Highside: Ref. temperature lower than inlet air temperature.'
            END IF
            IF (PrnLog .EQ. 1) THEN
                WRITE(6,*)'-- WARNING -- Highside: Ref. temperature lower than inlet air temperature.' 
            END IF
            WRITE(*,*)'Trying another iterating value....'
            IF (TSOCMP .LE. TSICMP) THEN
                IF (PrnCon .EQ. 1) THEN
                    WRITE(*,*)'## ERROR ## Highside: No solution for this configuration.'
                    WRITE(*,*)'Try another condenser or compressor.'
                END IF
                IF (PrnLog .EQ. 1) THEN
                    WRITE(6,*)'## ERROR ## Highside: No solution for this configuration.' 
                    WRITE(6,*)'Try another condenser or compressor.'  
                END IF
                STOP
            END IF
            IERR=2
        END IF

        IF (IERR .GE. 1) THEN
            CYCLE
        END IF

        CDTRIE = TSATEI - TRIE
        CDTRIE=CondOUT(14)*1.8 !ISI - 10/07/06

        IF(IREFC.EQ.0) THEN

            CDTRE = CDTRIE
            IF (XIEXP .GT. 1.) THEN !Edited for refprop table 01-15-04 - ISI
                CDTRE = CDTRIE !-CDTRIE !ISI - 08/06/06
            ELSEIF (XIEXP .GT. 0. .AND. XIEXP .LT. 1) THEN
                CDTRE = -200.0*XIEXP
            END IF

            DTRIE=DTROC
            DTRE = DTRIE
            IF (DTRIE .LT. 0.) THEN
                DTRE = 200.*DTRIE
            END IF

            CNDNSR = CDTRE - DTRE

            MdotR=XMR*UnitM/3600    !RS Comment: Unit Conversion, kg/hr??

            IF(DTRIE.LT.0.0) THEN
                SXIE = -DTRIE
                IF (PrnLog .EQ. 1) THEN
                    WRITE(6,FMT_904)'           Desired quality = ',SXIE*100,Xunit 
                END IF
                IF (PrnCon .EQ. 1) THEN
                    WRITE(*,FMT_904)'           Desired quality = ',SXIE*100,Xunit
                END IF
            ELSE
                IF (Unit .EQ. 1) THEN
                    IF (PrnLog .EQ. 1) THEN
                        WRITE(6,FMT_904)'           Desired subcooling = ',DTRIE/1.8,DTunit    
                    END IF
                    IF (PrnCon .EQ. 1) THEN
                        WRITE(*,FMT_904)'           Desired subcooling = ',DTRIE/1.8,DTunit
                    END IF
                ELSE
                    IF (PrnLog .EQ. 1) THEN
                        WRITE(6,FMT_904)'           Desired subcooling = ',DTRIE,DTunit    
                    END IF
                    IF (PrnCon .EQ. 1) THEN
                        WRITE(*,FMT_904)'           Desired subcooling = ',DTRIE,DTunit
                    END IF
                END IF
            END IF

            IF(XIEXP.GT.0.0) THEN
                IF (XIEXP .LT. 1) THEN
                    IF (PrnLog .EQ. 1) THEN
                        WRITE(6,FMT_904)'        Calculated quality = ',XIEXP*100,Xunit
                    END IF
                    IF (PrnCon .EQ. 1) THEN
                        WRITE(*,FMT_904)'        Calculated quality = ',XIEXP*100,Xunit
                    END IF
                ELSE
                    IF (PrnLog .EQ. 1) THEN
                        WRITE(6,FMT_904)'      Calculated superheat = ',-CDTRIE,DTunit
                    END IF
                    IF (PrnCon .EQ. 1) THEN
                        WRITE(*,FMT_904)'      Calculated superheat = ',-CDTRIE,DTunit
                    END IF
                END IF
            ELSE
                IF (Unit .EQ. 1) THEN
                    IF (PrnLog .EQ. 1) THEN
                        WRITE(6,FMT_904)'        Calculated subcooling = ',CDTRIE/1.8,DTunit
                    END IF
                    IF (PrnCon .EQ. 1) THEN
                        WRITE(*,FMT_904)'        Calculated subcooling = ',CDTRIE/1.8,DTunit
                    END IF
                ELSE  
                    IF (PrnLog .EQ. 1) THEN
                        WRITE(6,FMT_904)'        Calculated subcooling = ',CDTRIE,DTunit
                    END IF
                    IF (PrnCon .EQ. 1) THEN
                        WRITE(*,FMT_904)'        Calculated subcooling = ',CDTRIE,DTunit
                    END IF
                END IF
            END IF

            CYCLE            
        END IF

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

            XMRFLD=CapTubeOUT(1)*3600/UnitM !RS Comment: Unit Conversion, lbm/s???
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
                    IF (PrnCon .EQ. 1) THEN
                        WRITE(*,*)
                        WRITE(*,*)'## ERROR ## Highside: Short tube solution error.'
                    END IF
                    IF (PrnLog .EQ. 1) THEN
                        WRITE(6,*)
                        WRITE(6,*)'## ERROR ## Highside: Short tube solution error.'
                    END IF
                    ShTbPAR(2)=ShTbPAR(2) !*1.2   !RS: Debugging: Pulled from HPDM 761
                            CYCLE
                    !STOP   !RS: Debugging: Can't have it just crash
                CASE (2)
                    WRITE(*,*)'Trying another iterating value....'
                    IERR=1
                    CYCLE
                END SELECT
            END IF

            XMRFLD=ShTbOUT(1)*3600/UnitM    !RS Comment: Unit Conversion, lbm/s?
            ToExp=ShTbOUT(3)
            XoExp=ShTbOUT(4)
        END IF

        HoExp=HiExp
        EvapIN(3)=HoExp

        CNDNSR = ( XMRFLD - XMR )

        MdotR=XMR*UnitM/3600    !RS Comment: Unit Conversion, kg/hr?

        IF(.NOT. PRINT) THEN
            CYCLE
        END IF

        IF (Unit .EQ. 1) THEN
            IF (PrnCon .EQ. 1) THEN
                WRITE(*,FMT_904)'     Compressor flow rate = ',XMR*UnitM,MdotUnit
            END IF
            IF (PrnLog .EQ. 1) THEN
                WRITE(6,FMT_904)'     Compressor flow rate = ',XMR*UnitM,MdotUnit
            END IF
            IF (PrnCon .EQ. 1) THEN
                WRITE(*,FMT_904)'    Exp. device flow rate = ',XMRFLD*UnitM,MdotUnit
            END IF
            IF (PrnLog .EQ. 1) THEN
                WRITE(6,FMT_904)'    Exp. device flow rate = ',XMRFLD*UnitM,MdotUnit 
            END IF
        ELSE
            IF (PrnCon .EQ. 1) THEN
                WRITE(*,FMT_904)'     Compressor flow rate = ',XMR,MdotUnit
            END IF
            IF (PrnLog .EQ. 1) THEN
                WRITE(6,FMT_904)'     Compressor flow rate = ',XMR,MdotUnit
            END IF
            IF (PrnCon .EQ. 1) THEN
                WRITE(*,FMT_904)'    Exp. device flow rate = ',XMRFLD,MdotUnit
            END IF
            IF (PrnLog .EQ. 1) THEN
                WRITE(6,FMT_904)'    Exp. device flow rate = ',XMRFLD,MdotUnit
            END IF
        END IF

        !VL: Previously: IF (.NOT. IsCondenserAllocated) GO TO 300 !ISI - 12/27/06 <-- conditional moved to while at beginning of function definition
    END DO

    RETURN

    END FUNCTION
