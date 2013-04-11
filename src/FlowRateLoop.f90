! ************************************** !
! ** HEAT PUMP SIMULATION CODE HEADER ** !
! ************************************** !

! ************************************** !
! -- HIGH LEVEL OVERVIEW/DESCRIPTION --- !
! -------------------------------------- !
! This module calculates the saturation temperature of the compressor, such that the condenser subcooling is about
! equal to the specified subcooling, or the condenser mass flow rate is about equal to the compressor mass flow rate.

! ************************************** !
! -- PHYSICAL DESCRIPTION -------------- !
! -------------------------------------- !
! This component handles the overarching structure for the condenser, but does not actually represent the condenser or any physical item.

! ************************************** !
! -- SIMULATION DATA RESPONSIBILITIES -- !
! -------------------------------------- !
! This function brings in the compressor temperature, calculates, and returns either condensor subcooling or mass flow rate.

! ************************************** !
! -- INPUT FILES/OUTPUT FILES (none) --- !
! -------------------------------------- !
! There are no input or output files directly connected to this module.

! ************************************** !
! -- MODULE LEVEL VARIABLES/STRUCTURES - !
! -------------------------------------- !
! There is nothing defined on the module level; there is only one function here.

! ************************************** !
! -- SUMMARY OF METHODS, CALL TREE ----- !
! -------------------------------------- !
! This module contains 1 method:
!    PUBLIC CDNSR -- Determines the compressor saturation temperature
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
! The documentation might benefit from some clean-up and possible expansion.

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
    USE DataGlobals_HPSim, ONLY: RefrigIndex   !RS: Debugging: Removal of plethora of RefrigIndex definitions in the code
    USE InputProcessor_HPSim    !RS: Debugging: Brought over from GetHPSimInputs
    USE DataGlobals_HPSim, ONLY: MaxNameLength, RefName    !RS Comment: Needs to be used for implementation with Energy+ currently (7/23/12)

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
    REAL SimpleCondOUT(20),DetailedCondOUT(20)  !RS: Debugging: Formerly (29), (24)
    REAL DetailedQcnd,DetailedDPcnd
    REAL SimpleQcnd,SimpleDPcnd
    LOGICAL,SAVE :: IsFirstTimeCondenser = .TRUE. !First time to call condenser flag
    LOGICAL :: IsCondenserAllocated = .FALSE. !Flag to check if the arrays in the condenser model are allocated !RS: See VL's note 6 lines below
    
    CHARACTER(LEN=14) :: tmpString

    LOGICAL, EXTERNAL :: IssueRefPropError
        !RS: Debugging: Bringing this over from GetHPSimInputs
    CHARACTER(len=MaxNameLength),DIMENSION(200) :: Alphas ! Reads string value from input file
    INTEGER :: NumAlphas               ! States which alpha value to read from a "Number" line
    REAL, DIMENSION(200) :: Numbers    ! brings in data from IP
    INTEGER :: NumNumbers              ! States which number value to read from a "Numbers" line
    INTEGER :: Status                  ! Either 1 "object found" or -1 "not found"
    REAL, DIMENSION(200) :: TmpNumbers !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)
    
    !RS: Debugging: Moving here from GetHPSimInputs
      !*************** Filter Drier ****************    !RS: Debugging: Moving: FlowRateLoop

  CALL GetObjectItem('FilterDrierData',1,Alphas,NumAlphas, &
                      TmpNumbers,NumNumbers,Status)
  Numbers = DBLE(TmpNumbers) !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)
  
  FilterPAR(1) = Numbers(1) !Flow capacity
  FilterPAR(2) = Numbers(2) !Rating DP
  
  FilterPAR(2)=FilterPAR(2)*UnitP   !RS: Debugging: Bringing in unit conversion
  !-------

    IsCondenserAllocated = .FALSE.  !VL: the "SAVE" in the declaration causes a "TRUE" to persist causing a failure on a second call.
    DO WHILE (.NOT. IsCondenserAllocated)

        PRINT=.TRUE.
        !IF (MODE .EQ. 2 .OR. MODE .EQ. 4 .OR. MODE .EQ. 5) THEN    !RS: Debugging: Due to Mode Mismatch
            !IREFC=0 !for specified subcooling, set to zero
            !for specifed flow control, set to 3 
        !ELSE
            IREFC=3
        !END IF

        TSOCMP = TINPUT
        CNDNSR = 1.0E+10
        IERR = 0

        IF (.NOT. PRINT) THEN
            CYCLE
        END IF

        CALL IssueOutputMessage( '')
        IF (Unit .EQ. 1) THEN
            WRITE(tmpString,'(F10.4)') (TSOCMP-32)*5/9
        ELSE
            WRITE(tmpString,'(F10.4)') TSOCMP
        END IF
        CALL IssueOutputMessage( '>> Compressor discharge saturation temperature: '//TRIM(tmpString)//Tunit)

        !     CALL SUBROUTINE COMP TO DETERMINE THE COMPRESSOR
        !     PERFORMANCE AND REFRIGERANT FLOW RATE 'XMR'

        Temperature=(TSOCMP-32)/1.8 !RS Comment: Unit Conversion, from F to C
        Quality=1
        PoCmp=TQ(Ref$,Temperature,Quality,'pressure',RefrigIndex,RefPropErr)    !Compressor Outlet Pressure
        IF (IssueRefPropError(RefPropErr, 'FlowRateLoop')) THEN
            CALL IssueOutputMessage('Trying another iterating value....')
            IERR=1
            CYCLE
        END IF

        PoCmp=PoCmp/1000    !RS Comment: Unit Conversion

        Temperature=(TSICMP-32)/1.8 !RS Comment: Unit Conversion, from F to C
        Quality=1
        PiCmp=TQ(Ref$,Temperature,Quality,'pressure',RefrigIndex,RefPropErr)    !Compressor Inlet Pressure
        IF (IssueRefPropError(RefPropErr, 'FlowRateLoop')) THEN
            CALL IssueOutputMessage('Trying another iterating value....')
            IERR=1
            CYCLE
        END IF
        PiCmp=PiCmp/1000    !RS Comment: Unit Conversion

        IF (SUPER .GT. 0) THEN
            Temperature=(TSICMP+SUPER-32)/1.8   !RS Comment: Unit Conversion, from F to C
            Pressure=PiCmp*1000 !RS Comment: Unit Conversion
            HiCmp=TP(Ref$,Temperature,Pressure,'enthalpy',RefrigIndex,RefPropErr)   !Compressor Inlet Enthalpy
            IF (IssueRefPropError(RefPropErr, 'FlowRateLoop')) THEN
                CALL IssueOutputMessage('Trying another iterating value....')
                IERR=1
                CYCLE
            END IF
            HiCmp=HiCmp/1000    !RS Comment: Unit Conversion
        ELSE
            Pressure=PiCmp*1000 !RS Comment: Unit Conversion
            Quality=-SUPER
            HiCmp=PQ(Ref$,Pressure,Quality,'enthalpy',RefrigIndex,RefPropErr)   !Compressor Inlet Enthalpy
            IF (IssueRefPropError(RefPropErr, 'FlowRateLoop')) THEN
                CALL IssueOutputMessage('Trying another iterating value....')
                IERR=1
                CYCLE
            END IF
            HiCmp=HiCmp/1000    !RS Comment: Unit Conversion
        END IF

        CompIN(1)=PiCmp
        CompIN(2)=PoCmp
        CompIN(3)=HiCmp
        CALL Compressor(Ref$,CompIN,CompPAR,CompOUT) !(Ref$,PureRef,CompIN,CompPAR,CompOUT) !RS: Debugging: Extraneous PureRef
        IF (CompOUT(7) .NE. 0) THEN
            SELECT CASE (INT(CompOUT(7)))
            CASE (1,2)
                CALL IssueOutputMessage('Trying another iterating value....')
                IERR=1
                CYCLE
            END SELECT
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

        IF (SystemType .EQ. 4) THEN !Reheat system
            IF (FirstTimeFlowRateLoop) THEN
                CondIN(4)=XMaE
                CondIN(5)=(TAIE-32)/1.8 !RS Comment: Unit Conversion, from F to C
                CondIN(6)=RHIE
            ELSE
                CondIN(4)=XMaE
                CondIN(5)=EvapOUT(3)   !RS: Debugging: Formerly EvapOUT(17)
                CondIN(6)=EvapOUT(4)   !RS: Debugging: Formerly EvapOUT(18)
            END IF
        END IF

        !Take compressor shell loss into account
        IF (CompPAR(CompQLossFrac) .NE. 0) THEN !Shell loss in fraction    !RS: Debugging: Formerly CompPAR(21)
            CondPAR(CondCompQLoss)=CompPAR(CompQLossFrac)*CompOUT(1)  !RS: Debugging: Formerly CondPAR(39) & CompPAR(21)
        ELSE !Shell loss in W
            CondPAR(CondCompQLoss)=CompPAR(CompQLoss)/1000    !RS Comment: Unit Conversion, from kW to W? !RS: Debugging: Formerly CondPAR(39) & CompPAR(22)
        END IF

        IF ((IsCoolingMode .GT. 0 .AND. ODCcoilType .EQ. MCCONDENSER) .OR. &
        (IsCoolingMode .LT. 1 .AND. IDCcoilType .EQ. MCCONDENSER)) THEN
            !Microchannel coil
            IF (IsFirstTimeCondenser) THEN 
                CondPAR(CondFirstTime)=1 !First time   !RS: Debugging: Formerly CONDPAR(45)
                CondPAR(CondSimpCoil)=0 !Detailed version !RS: Debugging: Formerly CONDPAR(44)
                IsFirstTimeCondenser=.FALSE.
            END IF
            CALL Condenser(Ref$,CondIN,CondPAR,CondOUT) !(Ref$,PureRef,CondIN,CondPAR,CondOUT)  !RS: Debugging: Extraneous PureRef
            CondPAR(CondFirstTime)=0 !No longer first time !RS: Debugging: Formerly CONDPAR(45)
            IsCondenserAllocated=.TRUE.
        ELSE
            !Plate-fin coil
            !Run both simple and detailed version to determine which one to use
            IF (IsFirstTimeCondenser) THEN 
                CondPAR(CondFirstTime)=1 !First time   !RS: Debugging: Formerly CONDPAR(45)

                CondPAR(CondSimpCoil)=0 !Detailed version !RS: Debugging: Formerly CONDPAR(44)
                CALL Condenser(Ref$,CondIN,CondPAR,DetailedCondOUT) !(Ref$,PureRef,CondIN,CondPAR,DetailedCondOUT)  !RS: Debugging: Extraneous PureRef
                DetailedQcnd=DetailedCondOUT(15)
                DetailedDPcnd=CondIN(2)-DetailedCondOUT(10)

                CondPAR(CondSimpCoil)=1 !Simple version   !RS: Debugging: Formerly CONDPAR(44)
                CALL Condenser(Ref$,CondIN,CondPAR,SimpleCondOUT) !(Ref$,PureRef,CondIN,CondPAR,SimpleCondOUT)   !RS: Debugging: Extraneous PureRef
                SimpleQcnd=SimpleCondOUT(15)
                SimpleDPcnd=CondIN(2)-SimpleCondOUT(10)

                IF (ABS((SimpleQcnd-DetailedQcnd)/DetailedQcnd) .LT. 0.1 .AND. &
                ABS((SimpleDPcnd-DetailedDPcnd)/DetailedDPcnd) .LT. 0.1) THEN
                    CondPAR(CondSimpCoil)=1   !RS: Debugging: Formerly CONDPAR(44)
                    CondOUT=SimpleCondOUT
                ELSE
                    CondPAR(CondSimpCoil)=0   !RS: Debugging: Formerly CONDPAR(44)
                    CondOUT=DetailedCondOUT
                END IF 
                IsFirstTimeCondenser=.FALSE.

                !Always detailed
                CondPAR(CondSimpCoil)=0   !RS: Debugging: Formerly CONDPAR(44)
                CondOUT=DetailedCondOUT

            ELSE
                CALL Condenser(Ref$,CondIN,CondPAR,CondOUT) !(Ref$,PureRef,CondIN,CondPAR,CondOUT)  !RS: Debugging: Extraneous PureRef
                CondPAR(CondFirstTime)=0 !No longer first time !RS: Debugging: Formerly CONDPAR(45)
                IsCondenserAllocated=.TRUE.
            END IF

        END IF

        IF (CondOUT(20) .NE. 0) THEN   !RS: Debugging: Formerly CondOUT(24)
            SELECT CASE (INT(CondOUT(20)))    !RS: Debugging: Formerly CondOUT(24)
            CASE (2) !Refprop error
                CALL IssueOutputMessage('Trying another iterating value....')
                IERR=1
                CYCLE
            CASE (3)
                STOP
            CASE (4,5)
                CALL IssueOutputMessage('## ERROR ## Highside: Coil geometry misdefined.')
                STOP
            CASE (8) !Too much pressure drop
                CALL IssueOutputMessage('Trying another iterating value....')
                IERR=2
                CYCLE
            END SELECT
        END IF

        PiCnd=CondOUT(1)
        PiExp=CondOUT(10)
        HiExp=CondOUT(11)
        TiExp=CondOUT(12)
        XiExp=CondOUT(13)

        IF (XiExp .GT. 1) THEN !Condenser outlet is still in superheated region, ISI - 06/06/07
            CALL IssueOutputMessage('Trying another iterating value....')
            IERR=1
            CYCLE
        END IF

        Pressure=PiCnd*1000 !RS Comment: Unit Conversion
        Quality=1
        TSATCI=PQ(Ref$,Pressure,Quality,'temperature',RefrigIndex,RefPropErr)
        IF (RefPropErr .GT. 0) THEN
            CALL IssueOutputMessage( '-- WARNING -- Highside: Refprop error.')
            CALL IssueOutputMessage( 'Trying another iterating value....')
            IERR=1
            CYCLE
        END IF
        TSATCI=TSATCI*1.8+32    !RS Comment: Unit Conversion, from C to F

        IF (FilterPAR(1) .GT. 0) THEN !Filter drier exits
            FilterIN(1)=CondIN(1) !Mass flow rate, kg/s
            CALL CalcFilterDrierDP(FilterIN(1),FilterPAR,FilterOUT,Ref$)
            FilterDP=FilterOUT(1)

            PiExp=PiExp-FilterDP
            CondOUT(10)=PiExp

            Pressure=PiExp*1000 !RS Comment: Unit Conversion
            Enthalpy=HiExp*1000 !RS Comment: Unit Conversion
            XiExp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)   !Expansion Device Inlet Quality
            IF (RefPropErr .GT. 0) THEN
                CALL IssueOutputMessage('-- WARNING -- Highside: Refprop error.')
                IERR=1
                CYCLE
            END IF
        END IF 

        TRIE=TiExp*1.8+32   !RS Comment: Unit Conversion, from C to F

        Pressure=PiExp*1000 !RS Comment: Unit Conversion
        Quality=0
        TSATEI=PQ(Ref$,Pressure,Quality,'temperature',RefrigIndex,RefPropErr)
        IF (RefPropErr .GT. 0) THEN
            CALL IssueOutputMessage('-- WARNING -- Highside: Refprop error.')
            CALL IssueOutputMessage( 'Trying another iterating value....')
            IERR=1
            CYCLE
        END IF

        TSATEI=TSATEI*1.8+32    !RS Comment: Unit Conversion, from C to F

        TSAVG=(TSATCI+TSATEI)/2
        IF(TSAVG.LT.TAIC) THEN
            CALL IssueOutputMessage('-- WARNING -- Highside: Ref. temperature lower than inlet air temperature.')
            CALL IssueOutputMessage('Trying another iterating value....')
            IF (TSOCMP .LE. TSICMP) THEN
                CALL IssueOutputMessage('## ERROR ## Highside: No solution for this configuration.')
                CALL IssueOutputMessage('Try another condenser or compressor.')
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
                WRITE(tmpString, '(F10.4)') SXIE*100
                CALL IssueOutputMessage('           Desired quality = '//TRIM(tmpString)//Xunit)
            ELSE
                IF (Unit .EQ. 1) THEN
                    WRITE(tmpString, '(F10.4)') DTRIE/1.8
                    CALL IssueOutputMessage('           Desired subcooling = '//TRIM(tmpString)//DTunit)
                ELSE
                    WRITE(tmpString, '(F10.4)') DTRIE
                    CALL IssueOutputMessage('           Desired subcooling = '//TRIM(tmpString)//DTunit)
                END IF
            END IF

            IF(XIEXP.GT.0.0) THEN
                IF (XIEXP .LT. 1) THEN
                    WRITE(tmpString, '(F10.4)')XIEXP*100
                    CALL IssueOutputMessage('        Calculated quality = '//TRIM(tmpString)//Xunit)
                ELSE
                    WRITE(tmpString, '(F10.4)')-CDTRIE
                    CALL IssueOutputMessage('      Calculated superheat = '//TRIM(tmpString)//DTunit)
                END IF
            ELSE
                IF (Unit .EQ. 1) THEN
                    WRITE(tmpString, '(F10.4)')CDTRIE/1.8
                    CALL IssueOutputMessage('        Calculated subcooling = '//TRIM(tmpString)//DTunit)
                ELSE  
                    WRITE(tmpString, '(F10.4)')CDTRIE
                    CALL IssueOutputMessage('        Calculated subcooling = '//TRIM(tmpString)//DTunit)
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
            !CALL CapillaryTubeORNL(Ref$,PureRef,CapTubeIN,CapTubePAR,CapTubeOUT)  !RS: Debugging: Extraneous PureRef
            CALL CapillaryTubeORNL(Ref$,CapTubeIN,CapTubePAR,CapTubeOUT)

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
            !CALL ShortTubePayne(Ref$,PureRef,ShTbIN,ShTbPAR,ShTbOUT)
            CALL ShortTubePayne(Ref$,ShTbIN,ShTbPAR,ShTbOUT)
            IF (ShTbOUT(7) .NE. 0) THEN
                SELECT CASE (INT(ShTbOUT(7)))
                CASE (1)
                    CALL IssueOutputMessage('')
                    CALL IssueOutputMessage('## ERROR ## Highside: Short tube solution error.')
                    !ShTbPAR(2)=ShTbPAR(2)*1.2   !RS: Debugging: Pulled from HPDM 641
                            CYCLE   !RS: Debugging: Try again to converge
                    !STOP   !RS: Debugging: Can't just let it stop; try to force it to continue through
                CASE (2)
                    CALL IssueOutputMessage('Trying another iterating value....')
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
            WRITE(tmpString, '(F10.4)')XMR*UnitM
            CALL IssueOutputMessage('     Compressor flow rate = '//TRIM(tmpString)//MdotUnit)
            WRITE(tmpString, '(F10.4)')XMRFLD*UnitM
            CALL IssueOutputMessage('    Exp. device flow rate = '//TRIM(tmpString)//MdotUnit)
        ELSE
            WRITE(tmpString, '(F10.4)')XMR
            CALL IssueOutputMessage('     Compressor flow rate = '//TRIM(tmpString)//MdotUnit)
            WRITE(tmpString, '(F10.4)')XMRFLD
            CALL IssueOutputMessage('    Exp. device flow rate = '//TRIM(tmpString)//MdotUnit)
        END IF

    END DO

    RETURN

    END FUNCTION
