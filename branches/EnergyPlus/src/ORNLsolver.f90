!MODULE ORNLsolver
    
    !SUBROUTINE GETVAR(Variable)
    !    
    !    LOGICAL Variable
    !
    !    Variable=ONCECALL
    !
    !END SUBROUTINE GETVAR
        
    SUBROUTINE SimulationCycle(QUnitOut,LatOutputProvided, ONCECALL) !RS: Testing: Attempting to pass variables out

    !
    !
    !
    !
    !          
    !                   ___________________
    !                  |                   |
    !        3  -------|     Condenser     |-------  2 
    !          |       |___________________|       |
    !          |                                   |/|
    !          |                                  /  |
    !        |\ /| Expansion                     |   | Compressor!
    !        |/ \| Device                         \  |
    !          |                                   |\|
    !          |        ___________________        |
    !          |       |                   |       |
    !        4  -------|     Evaporator    |-------  1
    !                  |___________________|
    !
    !
    !
    !
    !
    !
    !
    !
    !
    USE InputPreProcessor
    USE FluidProperties_HPSim
    USE HeatPumpInput
    USE CompressorMod
    USE CondenserMod
    USE EvaporatorMod
    USE AccumulatorMod
    USE UnitConvertMod
    USE DataSimulation
    USE FrostModel
    USE InputProcessor_HPSim
    USE DataGlobals_HPSim, ONLY: RefName    !RS Comment: Needs to be used for implementation with Energy+ currently (7/23/12)

    IMPLICIT NONE

    !Subroutine parameters

    CHARACTER(len=80)   :: Refrigerant
    CHARACTER (len=15) :: Property           
    INTEGER            :: RefrigIndex =0
    REAL Temperature,Quality,Pressure,Enthalpy

    INTEGER(2) RefPropOpt			!Ref prop calc. option
    INTEGER(2) RefPropErr			!Error flag:1-error; 0-no error 
    REAL RefProp(28)	!Refrigerant properties ! VL Comment: Array Size Explanation??

    INTEGER(2) AirPropOpt			!Air prop calc. option
    INTEGER(2) AirPropErr			!Error flag:1-error; 0-no error 
    REAL AirProp(8)		!Air properties ! VL Comment: Array Size Explanation??

    REAL TimeStart, TimeStop, TimeSpent

    INTEGER ICHRGE,I
    REAL DTVALU
    REAL DTCONV
    REAL CHRGECONV
    INTEGER IERROR
    REAL DTVAL
    REAL STEP
    REAL CHGDIF

    REAL TsubExp,TsubCnd,TsupEvp,TsupCmp,Qcnd,Qevp,QevpSens
    REAL PwrCmp,mdot,TsiExp,TsoCnd,TsoEvp
    REAL Dshtb,WinTrans,Qloss
    REAL mdotRmax,mdotRmin,mdotRprev !mass flow rate iteration parameter
    REAL DetailedQevp,SimpleQevp !Evaporator capacity from detailed and simple models
    REAL DetailedQcnd,SimpleQcnd !Condenser capacity from detailed and simple models
    REAL MassCoil,MassLiqCoil,MassVapCoil
    INTEGER(2) IsCoolingMode !1=yes; 0=no   
    REAL, EXTERNAL :: ZEROCH
    REAL, EXTERNAL :: CHARGM
    !INTEGER :: TimeStep !Added Sankar transient
    LOGICAL:: Trues
    REAL :: SUPERAct
    REAL :: TsiCmpAct
    REAL :: TsoCmpAct
    REAL :: RHiCAct
    REAL :: RHiEAct
    REAL, SAVE :: IDCFlowConst
    REAL, SAVE :: ODCFlowConst
    INTEGER   :: Flag
    INTEGER :: LastTime !Aids in the event of a microchannel device
    LOGICAL, SAVE :: ONETIME = .TRUE.    !RS: Debugging: Keeps the program from calling the unit conversion subroutine over again.
    
    REAL, INTENT (OUT) :: QUnitOut            ! sensible capacity delivered to zone !RS: Testing: Trying to pass variables out
    REAL, INTENT (OUT) :: LatOutputProvided   ! Latent add/removal by packaged terminal unit (kg/s), dehumid = negative !RS: Testing: Trying to pass variables out
    !LOGICAL, SAVE :: ONCECALL = .TRUE. !RS: Debugging: Keeps the program from allocating certain arrays when they're already allocated (Condenser sub)
    LOGICAL :: ONCECALL
    
    ! VL : Flags to assist with dismantling of GOTO-based control structures ....
    ! Change names of the flags to reflect the intention of the GOTO statements ...
    ! GOTO 30 means "skip refined simulation" according to previous comments ....
    INTEGER   :: FLAG_GOTO_20, FLAG_GOTO_30     

    CHARACTER(LEN=11),PARAMETER :: FMT_103 = "(A20,F30.2)"
    CHARACTER(LEN=15),PARAMETER :: FMT_1005 = "(I20,10(F20.2))"
    CHARACTER(LEN=9),PARAMETER :: FMT_1006 = "(10(A20))"
    CHARACTER(LEN=9),PARAMETER :: FMT_1007 = "(A17,A42)"
    CHARACTER(LEN=33),PARAMETER :: FMT_1008 = "(A13,F10.3,A10,A10,A13,F10.3,A10)"
    CHARACTER(LEN=15),PARAMETER :: FMT_2001 = "(A13,F10.3,A10)"
    CHARACTER(LEN=15),PARAMETER :: FMT_2004 = "(A56,F10.3,A10)"
    CHARACTER(LEN=14),PARAMETER :: FMT_2007 = "(A16,F10.3,A9)"
    
    !Flow**:
    !CALL PreProcessInput   !RS: Commenting out since it'll already have been called in E+ module
    !CALL ProcessInput   !Moved up to avoid errors with "CALL GetInputs"    !RS: Commenting out since it'll already have been called in E+ module

    CoarseConvergenceCriteriaMet=.FALSE. !.TRUE. !.FALSE.     ! VL Comment: default initialization for program or user setting?
    FirstTimeAirTempLoop=.TRUE.                               ! VL Comment: default initialization for program or user setting?
    FirstTimeFlowRateLoop=.TRUE.                              ! VL Comment: default initialization for program or user setting?
    FirstTimeChargeLoop=.TRUE.                                ! VL Comment: default initialization for program or user setting?
    PrnLog=1                                                  ! VL_User_Setting
    PrnCon=1                                                  ! VL_User_Setting

    WinTrans=0.9  ! VL_Magic_Number
    CondIN(7) = 0 !VL Comment: CondIN(7)=0*WinTrans !stillwater 0.83 kW/m2 !Harbin 0.82 kW/m2 !Singapore 1.03 kW/m2   ! VL_Index_Replace	! VL_User_Setting
    CondPAR(36)=0.8   ! VL_Magic_Number    ! VL_Index_Replace

    EvapIN(8)=0   !VL Comment: EvapIN(8)=0*WinTrans !stillwater 0.63 kW/m2 !Harbin 0.52 kW/m2 !Singapore 0.88 kW/m2   ! VL_Index_Replace	! VL_User_Setting
    EvapPAR(29)=0.8   ! VL_Magic_Number    ! VL_Index_Replace

    OPEN(5,FILE='YorkHP.out')     ! VL_User_Setting -- file name
    OPEN(6,FILE='YorkHP.log')     ! VL_User_Setting -- file name

    !CALL GetInputs                ! VL Comment: Reads file "HPdata.ydd"; input and error file names should be sent in as parameters to file ...
    !RS: Debugging: Commenting out above call to see if we can just call it once at the beginning of the entire program
    !Oil fraction
    CondPAR(59)=0.007             ! VL_Magic_Number    ! VL_Index_Replace
    EvapPAR(51)=0.007             ! VL_Magic_Number    ! VL_Index_Replace
    
    !CondPAR(62)=1   !RS: Debugging: This will hopefully reset the "FirstTime" every run
    EvapPAR(54)=1   !RS: Debugging: This will hopefully reset the "FirstTime" every run

    IF (TaiE-TsiCmp .LT. 10) THEN     ! VL_Magic_Number number 10 ....
        TsiCmp = TaiE - 10 !Correct initial guess
    END IF

    IF (TsoCmp-TaiC .LT. 10) THEN     ! VL_Magic_Number number 10 ....
        TsoCmp = TaiC + 10 !Correct initial guess	! VL_Magic_Number
    END IF

    IF (TsoCmp .LE. TsiCmp) THEN
        WRITE(*,*)'Compressor suction temperature is greater than discharge temperature.'
        WRITE(*,*)'## ERROR ## Main: Wrong initial guess!'
        STOP
    END IF

    Punit = ' (kPa)'
    Hunit = ' (kJ/kg)'
    Tunit = ' (C)'
    DTunit = ' (K)'
    MdotUnit = ' (kg/hr)'
    MassUnit = ' (kg)'
    PwrUnit = ' (W)'
    CapUnit = ' (W)'
    EERunit = ' (Btu/W-hr)'
    SysUnit = ' (ton)'
    NoUnit = ' (-)'
    Xunit = ' (%)'
    Lunit = ' (m)'
    MiniLunit = ' (mm)'
    IF (Unit .EQ. 2) THEN
        Punit = ' (psi)'
        Hunit = ' (Btu/lbm)'
        Tunit = ' (F)'
        DTunit = ' (R)'
        MdotUnit = ' (lbm/h)'
        MassUnit = ' (lbm)'
        CapUnit = ' (Btu/h)'
        Lunit = ' (ft)'
        MiniLunit = ' (in)'
    END IF

    IF (ONETIME) THEN   !RS: Debugging: Only called once
        CALL UnitConvert(Unit,CompPAR,CondPAR,EvapPAR,ShTbPAR,CapTubePAR,TxvPAR,  &
        AccumPAR,FilterPAR,CFMcnd,CFMevp,TaiC,TaiE,RHiC,RHiE, &
        Refchg,TSOCMP,TSICMP,SUPER,SUBCOOL,BaroPressure, &
        ChargeCurveSlope,ChargeCurveIntercept,RefLiquidLength,Tdis,Tliq)
        
        ONETIME = .FALSE.
    END IF

    CALL InitAccumulator(AccumPAR)

    !set up Refrigerant variable...why?
    Refrigerant = RefName

    SUPERAct=SUPER
    TsiCmpAct=TsiCmp
    TsoCmpAct=TsoCmp
    RHiCAct=RHiC
    RHiEAct=RHiE
    IsCoolingMode=CondPAR(27)       ! VL_Index_Replace

    !Get simulation starting time
    TimeStart=SECNDS(0.0)
    CoolHeatModeFlag = IsCoolingMode
    TimeInterval = 25.0                 ! VL_Magic_Number number
    PrevSimTime = 0.0
    Timestep=0
    LastDefrostInitTime = 0.0

    !DO WHILE(SimRunning)
    DO WHILE(FrostingPeriod) !Added by Sankar
        !FirstTimeAirTempLoop=.TRUE.
        !FirstTimeFlowRateLoop=.TRUE.
        !FirstTimeChargeLoop=.TRUE.
        TimeStep = TimeStep+1

        CurSimTime=(TimeStep-1)*TimeInterval  !PrevSimTime+
        !PrevSimTime=CurSimTime
        IF(FrostingPeriod) THEN
            CALL EvaluateFrostModel
        ELSE IF(DefrostingPeriod) THEN
            CALL EvaluateDefrostModel
        END IF  

        TsiCmp=TsiCmpAct
        TsoCmp=TsoCmpAct
        RHiC=RHiCAct
        RHiE=RHiEAct
        SUPER=SUPERAct
        EvapIn=0.0
        EvapOut=0.0
        CondIn=0.0
        CondOut=0.0

        IF (RHiC .GT. TaiC) THEN
            WRITE(*,*)'## ERROR ## Main: Condenser wet bulb temperature is greater than dry bulb temperature.'
            STOP
        END IF
        AirPropOpt=3                  ! VL_Magic_Number number	! VL_User_Setting
        AirProp(1)=Temperature_F2C(TaiC)  ! VL_Index_Replace
        AirProp(5)=RHiC                   ! VL_Index_Replace
        CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
        RHiC=AirProp(3)               ! VL_Index_Replace
        RhoAiC=AirProp(7)             ! VL_Index_Replace

        CondIN(5)=Temperature_F2C(TaiC)   ! VL_Index_Replace
        CondIN(6)=RHiC                    ! VL_Index_Replace

        IF (RHiE .GT. TaiE) THEN !ISI - 11/04/07
            WRITE(*,*)'## ERROR ## Main: Evaporator wet bulb temperature is greater than dry bulb temperature.'
            STOP
        END IF
        AirPropOpt=3                  ! VL_Magic_Number number	! VL_User_Setting
        AirProp(1)=Temperature_F2C(TaiE)  ! VL_Index_Replace
        AirProp(5)=RHiE                   ! VL_Index_Replace
        CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
        RHiE=AirProp(3)               ! VL_Index_Replace
        RhoAiE=AirProp(7)             ! VL_Index_Replace

        EvapIN(5)=Temperature_F2C(TaiE)  !Air side inlet temp. C      ! temp F to C   ! VL_Index_Replace
        EvapIN(6)=RHiE            !Air side inlet relative humidity                   ! VL_Index_Replace

        !Initialize
        Temperature=Temperature_F2C(TSICMP)
        Quality=1	! VL_User_Setting
        PiCmp=TQ(Ref$,Temperature,Quality,'pressure',RefrigIndex,RefPropErr)    !Compressor Inlet Pressure
        IF (RefPropErr .GT. 0) THEN
            WRITE(*,*)'## ERROR ## Main: Refrigerant property is out of bound!'
            STOP
        END IF
        PiCmp=PiCmp/1000.0    ! VL : conversion ?

        PiEvp=PiCmp !Evaporator inlet pressure
        EvapIN(2)=PiEvp   ! VL_Index_Replace
        EvapOUT(1)=PiEvp  ! VL_Index_Replace
        EvapOUT(6)=PiEvp  ! VL_Index_Replace

        Temperature=Temperature_F2C(TSOCMP)
        Quality=1	! VL_User_Setting
        PoCmp=TQ(Ref$,Temperature,Quality,'pressure',RefrigIndex,RefPropErr)    !Compressor Outlet Pressure
        IF (RefPropErr .GT. 0) THEN
            WRITE(*,*)'## ERROR ## Main: Refrigerant property is out of bound!'
            STOP
        END IF  
        PoCmp=PoCmp/1000.0  !RS Comment: Unit Conversion

        IF (SUPER .GE. 0) THEN !ISI - 11/16/07

            Temperature=Temperature_F2C(TSICMP+SUPER)
            Pressure=PiCmp*1000
            HiCmp=TP(Ref$,Temperature,Pressure,'enthalpy',RefrigIndex,RefPropErr)   !Compressor Inlet Enthalpy
            IF (RefPropErr .GT. 0) THEN
                WRITE(*,*)'## ERROR ## Main: Refrigerant property is out of bound!'
                STOP
            END IF
            HiCmp=HiCmp/1000    !RS Comment: Unit Conversion

        ELSE
            Pressure=PiCmp*1000 !RS Comment: Unit Conversion
            Quality=-SUPER
            HiCmp=PQ(Ref$,Pressure,Quality,'enthalpy',RefrigIndex,RefPropErr)   !Compressor Inlet Enthalpy
            IF (RefPropErr .GT. 0) THEN
                WRITE(*,*)'## ERROR ## Main: Refrigerant property is out of bound!'
                STOP
            END IF
            HiCmp=HiCmp/1000    !RS Comment: Unit Conversion

        END IF

        CompIN(1)=PiCmp   ! VL_Index_Replace
        CompIN(2)=PoCmp	! VL_Index_Replace
        CompIN(3)=HiCmp	! VL_Index_Replace
        IF (SystemType .NE. EVAPORATORONLY) THEN
            CALL Compressor(Ref$,PureRef,CompIN,CompPAR,CompOUT)
            IF (CompOUT(7) .NE. 0) THEN	! VL_Index_Replace
                SELECT CASE (INT(CompOUT(7)))	! VL_Index_Replace
                CASE (1)
                    WRITE(*,*)'## ERROR ## Highside: Compressor solution error!'
                    STOP
                CASE (2)
                    WRITE(*,*)'-- WARNING -- Highside: Refprop out of range in compressor model.'
                END SELECT
            END IF 
        END IF
        WRITE(*,*) 

        EvapOUT(3)=Temperature_F2C(TSICMP) !Initialize for reversing valve calculation        

        IsCoolingMode=CondPAR(27)	! VL_Index_Replace
        WRITE(6,*)'Heat Pump Design Tool (ver. 2.0 12/17/09)' 
        WRITE(*,*)'Heat Pump Design Tool (ver. 2.0 12/17/09)'
        IF (IsCoolingMode .EQ. 1) THEN
            IF (PrnLog .EQ. 1) THEN
                WRITE(6,*)'***** Cooling Mode *****'
            END IF
            IF (PrnCon .EQ. 1) THEN
                WRITE(*,*)'***** Cooling Mode *****'
            END IF
        ELSE
            IF (PrnLog .EQ. 1) THEN
                WRITE(6,*)'***** Heating Mode *****'
            END IF
            IF (PrnCon .EQ. 1) THEN
                WRITE(*,*)'***** Heating Mode *****'
            END IF
        END IF
        
        ! VL: No GOTO statements before this line in this file ..... so this is a nice place to set default values for the flags
        FLAG_GOTO_20 = .FALSE.
        FLAG_GOTO_30 = .FALSE.

        SELECT CASE(MODE)

        CASE(FIXEDORIFICESIM)
            IF (PrnLog .EQ. 1) THEN
                WRITE(6,*)'***** System Simulation (Fixed Orifice) *****'
            END IF
            IF (PrnCon .EQ. 1) THEN
                WRITE(*,*)'***** System Simulation (Fixed Orifice) *****'
            END IF
            ICHRGE=2	! VL_User_Setting

            !ISI - 08/07/06
            AMBCON=1E-3 !1 !air temperature, F	! VL_Magic_Number
            CNDCON=1 !subcooling, F	! VL_Magic_Number	! VL_User_Setting
            CHRGECONV=.5 !charge, lbm	! VL_Magic_Number

            EVPCON=1 !superheat, F	! VL_User_Setting

            FLOCON=5 !mass flow rate, lbm/hr	! VL_Magic_Number	
            EVAPPAR(50)=7 !Pressure, kPa	! VL_Index_Replace
            CONDPAR(56)=7 !.05 !Pressure, kPa	! VL_Index_Replace

        CASE(ORIFICEANDTXVDESIGN)
            IF (PrnLog .EQ. 1) THEN
                WRITE(6,*)'***** Design Calculation (Orifice and TXV) *****'
            END IF
            IF (PrnCon .EQ. 1) THEN
                WRITE(*,*)'***** Design Calculation (Orifice and TXV) *****'
            END IF
            ICHRGE=0	! VL_User_Setting

            AMBCON=1E-3 !1 !air temperature, F
            CNDCON=1 !subcooling, F	! VL_User_Setting
            CHRGECONV=.5 !charge, lbm

            EVPCON=1 !superheat, F	! VL_User_Setting

            FLOCON=5 !mass flow rate, lbm/hr
            EVAPPAR(50)=7 !Pressure, kPa	! VL_Index_Replace	! VL_User_Setting
            CONDPAR(56)=7 !.05 !Pressure, kPa	! VL_Index_Replace	! VL_User_Setting

        CASE(FIXEDSUPERHEATSIM)
            IF (PrnLog .EQ. 1) THEN
                WRITE(6,*)'***** Design Calculation (Fixed Orifice) *****'
            END IF
            IF (PrnCon .EQ. 1) THEN
                WRITE(*,*)'***** Design Calculation (Fixed Orifice) *****'
            END IF
            ICHRGE=0	! VL_User_Setting

            AMBCON=1E-3 !1 !air temperature, F
            CNDCON=1 !subcooling, F	! VL_User_Setting
            CHRGECONV=.5 !charge, lbm

            EVPCON=1 !superheat, F	! VL_User_Setting

            FLOCON=5 !mass flow rate, lbm/hr
            EVAPPAR(50)=7 !Pressure, kPa	! VL_Index_Replace	! VL_User_Setting
            CONDPAR(56)=7 !.05 !Pressure, kPa	! VL_Index_Replace	! VL_User_Setting

        CASE(TXVSIMULATION)
            IF (PrnLog .EQ. 1) THEN
                WRITE(6,*)'***** System Simulation (TXV) *****'
            END IF
            IF (PrnCon .EQ. 1) THEN
                WRITE(*,*)'***** System Simulation (TXV) *****'
            END IF
            ICHRGE=2	! VL_User_Setting

            !ISI - 08/07/06
            AMBCON=1E-3 !1 !air temperature, F
            CNDCON=0.5 !subcooling, F
            CHRGECONV=.5 !charge, lbm

            EVPCON=1 !superheat, F	! VL_User_Setting

            FLOCON=5 !mass flow rate, lbm/hr
            EVAPPAR(50)=7 !Pressure, kPa	! VL_Index_Replace	! VL_User_Setting
            CONDPAR(56)=7 !.05 !Pressure, kPa	! VL_Index_Replace	! VL_User_Setting

        CASE(CONDENSERUNITSIM)
            IF (PrnLog .EQ. 1) THEN
                WRITE(6,*)'***** Condenser Unit Simulation *****'
            END IF
            IF (PrnCon .EQ. 1) THEN
                WRITE(*,*)'***** Condenser Unit Simulation *****'
            END IF
            ICHRGE=0	! VL_User_Setting

            !ISI - 08/07/06
            AMBCON=1E-3 !1 !air temperature, F
            CNDCON=1 !subcooling, F	! VL_User_Setting
            CHRGECONV=.5 !charge, lbm

            EVPCON=1 !superheat, F

            FLOCON=5 !mass flow rate, lbm/hr
            EVAPPAR(50)=7 !Pressure, kPa	! VL_Index_Replace	! VL_User_Setting
            CONDPAR(56)=7 !.05 !Pressure, kPa	! VL_Index_Replace	! VL_User_Setting

        CASE(COILONLYSIM) !Added for coil only simulation - ISI - 10/23/07
            IF (PrnLog .EQ. 1) THEN
                WRITE(6,*)'***** Coil Only Simulation *****'
            END IF
            IF (PrnCon .EQ. 1) THEN
                WRITE(*,*)'***** Coil Only Simulation *****'
            END IF

            IF (PrnLog .EQ. 1) THEN
                WRITE(6,*)
            END IF
            IF (PrnCon .EQ. 1) THEN
                WRITE(*,*)
            END IF

            IF (Unit .EQ. 1) THEN !SI Unit
                IF (PrnLog .EQ. 1) THEN
                    WRITE(6,FMT_1006)'Iteration','mdot(kg/hr)','Capacity(kW)'
                END IF
                IF (PrnCon .EQ. 1) THEN
                    WRITE(*,FMT_1006)'Iteration','mdot(kg/hr)','Capacity(kW)'
                END IF
            ELSE !IP Unit
                IF (PrnLog .EQ. 1) THEN
                    WRITE(6,FMT_1006)'Iteration','mdot(lbm/hr)','Capacity(MBtu/hr)'
                END IF
                IF (PrnCon .EQ. 1) THEN
                    WRITE(*,FMT_1006)'Iteration','mdot(lbm/hr)','Capacity(MBtu/hr)'
                END IF
            END IF

            IF (IsCoolingMode .GT. 0) THEN
                CondOUT(7)=Tliq
                Temperature=Tliq

                Temperature=Tliq+SUBCOOL*5/9
                Quality=0	! VL_User_Setting
                PiExp=TQ(Ref$,Temperature,Quality,'pressure',RefrigIndex,RefPropErr)    !Expansion Device Inlet Pressure
                PiExp=PiExp/1000    !RS Comment: Unit Conversion

                Pressure=PiExp*1000 !RS Comment: Unit Conversion
                Temperature=Tliq
                HiExp=TP(Ref$,Temperature,Pressure,'enthalpy',RefrigIndex,RefPropErr)   !Expansion Device Inlet Enthalpy
                HiExp=HiExp/1000    !RS Comment: Unit Conversion
                HiEvp=HiExp

                Temperature=Temperature_F2C(TSICMP)
                Quality=1	! VL_User_Setting
                PiEvp=TQ(Ref$,Temperature,Quality,'pressure',RefrigIndex,RefPropErr)    !Evaporator Inlet Pressure
                PiEvp=PiEvp/1000    !RS Comment: Unit Conversion

                Pressure=PiEvp*1000 !RS Comment: Unit Conversion
                Temperature=Temperature_F2C(TSICMP+SUPER)
                HoEvp=TP(Ref$,Temperature,Pressure,'enthalpy',RefrigIndex,RefPropErr)   !Evaporator Outlet Enthalpy
                HoEvp=HoEvp/1000    !RS Comment: Unit Conversion

            ELSE

                Temperature=Temperature_F2C(TSOCMP)
                Quality=1	! VL_User_Setting
                PoCmp=TQ(Ref$,Temperature,Quality,'pressure',RefrigIndex,RefPropErr)    !Compressor Outlet Pressure
                PoCmp=PoCmp/1000    !RS Comment: Unit Conversion

                CompOUT(5)=Tdis
                Tocmp=Tdis

                Pressure=PoCmp*1000 !RS Comment: Unit Conversion
                Temperature=ToCmp
                HoCmp=TP(Ref$,Temperature,Pressure,'enthalpy',RefrigIndex,RefPropErr)   !Compressor Outelt Enthalpy
                HoCmp=HoCmp/1000    !RS Comment: Unit Conversion

                Temperature=Temperature_F2C(TSOCMP)
                Quality=0	! VL_User_Setting
                PiExp=TQ(Ref$,Temperature,Quality,'pressure',RefrigIndex,RefPropErr)    !Expansion Device Inlet Pressure
                PiExp=PiExp/1000    !RS Comment: Unit Conversion

                Pressure=PiExp*1000 !RS Comment: Unit Conversion
                Temperature=Temperature_F2C(TSOCMP-SUBCOOL)
                HiExp=TP(Ref$,Temperature,Pressure,'enthalpy',RefrigIndex,RefPropErr)   !Expansion Device Inlet Enthalpy
                HiExp=HiExp/1000    !RS Comment: Unit Conversion

            END IF

            !Initial guess
            MdotRmax=999	! VL_Magic_Number
            MdotRmin=0
            MdotR=0.01 !0.1 is too big, not easy to converge, ISI - 02/11/08

            IF (IsCoolingMode .GT. 0) THEN !Cooling mode, indoor coil is evaporator

                XMaE=RhoAiE*CFMevp

                EvapPAR(1)=0 !Suction line length	! VL_Index_Replace
                EVAPPAR(50) =0.5 !0.1 ! Mass flow rate convergence criterion	! VL_Index_Replace	! VL_Magic_Number
                EvapPAR(54)=1 !First time	! VL_Index_Replace

                !Determine if detailed model is needed, ISI - 02/07/08

                EvapIN(1)=MdotR			!Refrigerant side mass flow rate, kg/s	! VL_Index_Replace
                EvapIN(2)=PiEvp			!Evap. inlet pressure, kPa	! VL_Index_Replace
                EvapIN(3)=HiEvp			!Refrigerant side inlet enthalpy, kJ/kg	! VL_Index_Replace
                EvapIN(4)=XMaE            !Air side mass flow rate, kg/s	! VL_Index_Replace
                EvapIN(5)=Temperature_F2C(TaiE)   !Air side inlet temp. C     	! VL_Index_Replace
                EvapIN(6)=RHiE            !Air side inlet relative humidity	! VL_Index_Replace
                EvapIN(9)=0.0             !Discharge temperature, C, not used for this	! VL_Index_Replace

                EvapPAR(53)=0 !Detailed model	! VL_Index_Replace
                CALL Evaporator(Ref$,PureRef,EvapIN,EvapPAR,EvapOUT)	
                DetailedQevp=-EvapOUT(11)	! VL_Index_Replace
                CALL EndEvaporatorCoil

                EvapPAR(53)=1 !Simple model	! VL_Index_Replace
                CALL Evaporator(Ref$,PureRef,EvapIN,EvapPAR,EvapOUT)	
                SimpleQevp=-EvapOUT(11)	! VL_Index_Replace
                CALL EndEvaporatorCoil

                IF (ABS((SimpleQevp-DetailedQevp)/DetailedQevp) .LT. 0.005) THEN	! VL_Magic_Number
                    EvapPAR(53)=1 !Simple version	! VL_Index_Replace	! VL_User_Setting
                ELSE
                    EvapPAR(53)=0 !Detailed version	! VL_Index_Replace	! VL_User_Setting
                END IF

                !Iterate mass flow rate to match outlet enthalpy
                DO I=1,MaxIter

                    EvapIN(1)=MdotR			!Refrigerant side mass flow rate, kg/s	! VL_Index_Replace
                    EvapIN(2)=PiEvp			!Evap. inlet pressure, kPa	! VL_Index_Replace
                    EvapIN(3)=HiEvp			!Refrigerant side inlet enthalpy, kJ/kg	! VL_Index_Replace
                    EvapIN(4)=XMaE            !Air side mass flow rate, kg/s	! VL_Index_Replace
                    EvapIN(5)=Temperature_F2C(TaiE)   !Air side inlet temp. C     	! VL_Index_Replace
                    EvapIN(6)=RHiE            !Air side inlet relative humidity	! VL_Index_Replace
                    EvapIN(9)=0.0             !Discharge temperature, C, not used for this	! VL_Index_Replace

                    CALL Evaporator(Ref$,PureRef,EvapIN,EvapPAR,EvapOUT)			
                    EvapPAR(54)=0 !First time	! VL_Index_Replace	! VL_User_Setting

                    Qevp=-EvapOUT(11) 	! VL_Index_Replace

                    IF (Unit .EQ. 1) THEN !SI Unit
                        IF (PrnLog .EQ. 1) THEN
                            WRITE(6,FMT_1005)I,MdotR*3600,Qevp
                        END IF
                        IF (PrnCon .EQ. 1) THEN
                            WRITE(*,FMT_1005)I,MdotR*3600,Qevp
                        END IF
                    ELSE
                        IF (PrnLog .EQ. 1) THEN
                            WRITE(6,FMT_1005)I,MdotR/Umass*3600,Qevp/UnitPwr
                        END IF
                        IF (PrnCon .EQ. 1) THEN
                            WRITE(*,FMT_1005)I,MdotR/Umass*3600,Qevp/UnitPwr
                        END IF
                    END IF

                    IF (ABS(EvapOUT(2)-HoEvp)>0.1 .AND. (mdotRmax-mdotRmin)/mdotR > 0.001) THEN	! VL_Magic_Number

                        !Take half time step if not converged - ISI 12/09/2009
                        IF (EvapOUT(20) .GT. 0) THEN	! VL_Index_Replace
                            mdotR=mdotRprev+(mdotR-mdotRprev)/2
                            CYCLE
                        END IF

                        !EvapOUT(2) is enthalpy, EvapOUT(20) is error flag
                        IF (EvapOUT(2) .LT. HoEvp .OR. EvapOUT(20) .GT. 0) THEN 	! VL_Index_Replace
                            mdotRmax=mdotR
                        ELSE
                            mdotRmin=mdotR
                        END IF

                        mdotRprev=mdotR !Stored prevous value - ISI 12/09/2009

                        IF (mdotRmax .NE. 999 .AND. mdotRmin .NE. 0) THEN
                            mdotR=(mdotRmax+mdotRmin)/2                        
                        ELSE IF (mdotRmax .EQ. 999) THEN
                            mdotR=mdotR*1.5**I
                        ELSE IF (mdotRmin .EQ. 0) THEN
                            mdotR=mdotR*1.5**(-I)
                        END IF
                    ELSE
                        EXIT
                    END IF

                END DO

                CALL CalcEvaporatorInventory(MassCoil,MassLiqCoil,MassVapCoil,EvapLiqTubeLength,EvapVapTubeLength,EvapTwoPhaseTubeLength,EvapNumLiqTubes)
                EvapOUT(14)=MassCoil	      	! VL_Index_Replace
            ELSE !Heating mode, indoor coil is condenser
                XMaC=RhoAiC*CFMcnd

                CondPAR(1)=0 !Discharge line length	! VL_Index_Replace
                CondPAR(8)=0 !Liquid line length	! VL_Index_Replace
                CondPAR(56)=0.5 !0.1 ! Mass flow rate convergence criterion	! VL_Index_Replace
                CondPAR(62)=1 !First time	! VL_Index_Replace

                CondIN(1)=MdotR	! VL_Index_Replace
                CondIN(2)=PoCmp	! VL_Index_Replace         
                CondIN(3)=HoCmp	! VL_Index_Replace         
                CondIN(4)=XMaC	! VL_Index_Replace           
                CondIN(5)=Temperature_F2C(TAIC)	! VL_Index_Replace
                CondIN(6)=RHIC	! VL_Index_Replace           
                CondIN(8)=0 !Evaporator outlet temperature, C, not used for this	! VL_Index_Replace
                CondIN(9)=Temperature_F2C(TAIE)	! VL_Index_Replace

                !Determine if detailed model is needed, ISI - 02/07/08
                CondPAR(61)=1 !Simple version	! VL_Index_Replace	! VL_User_Setting
                CALL Condenser(Ref$,PureRef,CondIN,CondPAR,CondOUT)
                SimpleQcnd=CondOUT(15)	! VL_Index_Replace
                CALL EndCondenserCoil

                CondPAR(61)=0 !Detailed version	! VL_Index_Replace	! VL_User_Setting
                CALL Condenser(Ref$,PureRef,CondIN,CondPAR,CondOUT)
                DetailedQcnd=CondOUT(15)	! VL_Index_Replace
                CALL EndCondenserCoil

                IF (ABS((SimpleQcnd-DetailedQcnd)/DetailedQcnd) .LT. 0.1) THEN	! VL_Magic_Number
                    CondPAR(61)=1 !Simple version	! VL_Index_Replace	! VL_User_Setting
                ELSE
                    CondPAR(61)=0 !Detailed version	! VL_Index_Replace	! VL_User_Setting
                END IF 

                !Iterate mass flow rate to match outlet enthalpy
                DO I=1,MaxIter

                    CondIN(1)=MdotR	! VL_Index_Replace
                    CondIN(2)=PoCmp	! VL_Index_Replace         
                    CondIN(3)=HoCmp	! VL_Index_Replace         
                    CondIN(4)=XMaC 	! VL_Index_Replace          
                    CondIN(5)=Temperature_F2C(TAIC)	! VL_Index_Replace
                    CondIN(6)=RHIC 	! VL_Index_Replace          
                    CondIN(8)=0 !Evaporator outlet temperature, C, not used for this	! VL_Index_Replace
                    CondIN(9)=Temperature_F2C(TAIE)	! VL_Index_Replace

                    CALL Condenser(Ref$,PureRef,CondIN,CondPAR,CondOUT)			
                    CondPAR(62)=0 !First time	! VL_Index_Replace

                    Qcnd=CondOUT(15) 	! VL_Index_Replace

                    IF (Unit .EQ. 1) THEN !SI Unit
                        IF (PrnLog .EQ. 1) THEN
                            WRITE(6,FMT_1005)I,MdotR*3600,Qcnd
                        END IF
                        IF (PrnCon .EQ. 1) THEN
                            WRITE(*,FMT_1005)I,MdotR*3600,Qcnd
                        END IF
                    ELSE
                        IF (PrnLog .EQ. 1) THEN
                            WRITE(6,FMT_1005)I,MdotR/Umass*3600,Qcnd/UnitPwr
                        END IF
                        IF (PrnCon .EQ. 1) THEN
                            WRITE(*,FMT_1005)I,MdotR/Umass*3600,Qcnd/UnitPwr
                        END IF
                    END IF

                    IF (ABS(CondOUT(6)-HiExp)>0.1 .AND. (mdotRmax-mdotRmin)/mdotR > 0.001) THEN	! VL_Index_Replace	! VL_Magic_Number

                        !Take half time step if not converged - ISI 12/09/2009
                        !                  IF (CondOUT(24) .GT. 0) THEN
                        !                      mdotR=mdotRprev-(mdotR-mdotRprev)/2
                        !                      CYCLE
                        !                  END IF

                        !CondOUT(6) is enthalpy, CondOUT(24) is error flag
                        IF (CondOUT(6) .LT. HiExp .OR. CondOUT(24) .GT. 0) THEN 	! VL_Index_Replace
                            mdotRmin=mdotR
                        ELSE
                            mdotRmax=mdotR
                        END IF

                        mdotRprev=mdotR !Stored prevous value - ISI 12/09/2009

                        IF (mdotRmax .NE. 999 .AND. mdotRmin .NE. 0) THEN
                            mdotR=(mdotRmax+mdotRmin)/2                        
                        ELSE IF (mdotRmax .EQ. 999) THEN
                            mdotR=mdotR*1.5**I
                        ELSE IF (mdotRmin .EQ. 0) THEN
                            mdotR=mdotR*1.5**(-I)
                        END IF
                    ELSE
                        EXIT
                    END IF

                END DO

                CALL CalcCondenserInventory(MassCoil,MassLiqCoil,MassVapCoil,CondLiqTubeLength,CondVapTubeLength,CondTwoPhaseTubeLength,CondNumLiqTubes)
                CondOUT(18)=MassCoil      	! VL_Index_Replace
            END IF

            FLAG_GOTO_30 = .TRUE.

        CASE DEFAULT
            IF (PrnLog .EQ. 1) THEN
                WRITE(6,*)'***** Design Calculation (Orifice and TXV) *****'
            END IF
            IF (PrnCon .EQ. 1) THEN
                WRITE(*,*)'***** Design Calculation (Orifice and TXV) *****'
            END IF
            ICHRGE=0

            !ISI - 08/07/06
            AMBCON=1E-3 !1 !air temperature, F	! VL_Magic_Number
            CNDCON=1 !subcooling, F
            CHRGECONV=.5 !charge, lbm	! VL_Magic_Number

            EVPCON=1 !superheat, F

            FLOCON=5 !mass flow rate, lbm/hr	! VL_Index_Replace	! VL_Magic_Number
            EVAPPAR(50)=7 !Pressure, kPa	! VL_Index_Replace
            CONDPAR(56)=7 !.05 !Pressure, kPa	! VL_Index_Replace

        END SELECT
        
        ! VL : Check if a GOTO 30 was intended previously ... and skip code block accordingly ....        
        IF (FLAG_GOTO_30 .EQ. .FALSE.) THEN 
        
            !comment block starts here
            FirstTimeHPdesignMode=.TRUE. !Moved from HPdesignMod - ISI 02/06/2009
            FirstTimeFlowRateLoop=.TRUE. !Moved from HPdesignMod - ISI 02/06/2009

            AMBCON=1E-3 !0.01 !0.05 !0.3 !0.01 !1E-3 !Air temp. F	! VL_Magic_Number
            CNDCON=0.1 !0.3 !Subcooling, F	! VL_Magic_Number
            CHRGECONV=0.05 !0.1 !Charge, lbm	! VL_Magic_Number

            EVPCON=1 !0.1 !0.2 !SUPERHEAT

            FLOCON=5 !Mass flow rate, lbm/hr	! VL_Magic_Number
            EVAPPAR(50) =0.1 ! 7	! VL_Index_Replace	! VL_Magic_Number
            CONDPAR(56)=0.1 !7 !.05	! VL_Index_Replace	! VL_Magic_Number

            DTVALU = SUPER
            IF (MODE .EQ. TXVSIMULATION) THEN
                DTVALU=SUBCOOL
            END IF
            DTCONV = EVPCON

            IF(ICHRGE.EQ.0) THEN
                FLAG_GOTO_20 = .TRUE.
            END IF
            IF (Refchg .EQ. 0) THEN
                FLAG_GOTO_20 = .TRUE.
            END IF
        
            IF (FLAG_GOTO_20 .EQ. .FALSE.) THEN
        
                ! Block of Code before "going" to "20"
                ! ----------

                DTVAL = DTVALU 
                STEP = 30.0	! VL_Magic_Number
                IF(ICHRGE.EQ.2) THEN
                    STEP = 5 !10	! VL_Magic_Number
                END IF

                !1st run is for coarse convergence criteria
                DTVALU = ZEROCH(DTVAL,CHARGM,CHRGECONV,CHRGECONV,STEP,CHGDIF,IERROR)
                !CALL SolveRegulaFalsi(CHRGECONV, MaxIter, Flag, DTVALU, CHARGM, DTVAL, STEP, IError)
                !      SolveRegulaFalsi(Eps, MaxIte, Flag, XRes, f, X_0, X_1, Par)
            
                FLAG_GOTO_30 = .TRUE.
                IF (FLAG_GOTO_30 .EQ. .FALSE.) THEN

                    !~Refine convergence criteria and run simulation again
                    CoarseConvergenceCriteriaMet=.TRUE.

                    IF (MODE .EQ. CONDENSERUNITSIM) THEN
                        DTVAL=DTROC !Use previous iterated value, ISI - 07/26/07
                    END IF
                    AMBCON=1E-3 !0.01 !0.05 !0.3 !0.01 !1E-3 !Air temp. F	! VL_Magic_Number
                    CNDCON=0.1 !0.3 !Subcooling, F	! VL_Magic_Number
                    CHRGECONV=0.05 !0.1 !Charge, lbm	! VL_Magic_Number

                    EVPCON=1 !0.1 !0.2 !SUPERHEAT

                    FLOCON=5 !Mass flow rate, lbm/hr	! VL_Magic_Number
                    EVAPPAR(50) =0.1 ! 7	! VL_Index_Replace	! VL_Magic_Number
                    CONDPAR(56)=0.1 !7 !.05	! VL_Index_Replace	! VL_Magic_Number

                    DTVALU = ZEROCH(DTVAL,CHARGM,CHRGECONV,CHRGECONV,STEP,CHGDIF,IERROR)
                    !CALL SolveRegulaFalsi(CHRGECONV, MaxIter, Flag, DTVALU, CHARGM, DTVAL, STEP,IError)

                    FLAG_GOTO_30 = .TRUE.   ! VL : This will not get executed either !!
                
                ENDIF
        
            ENDIF

            ! VL : Check if a GOTO 30 was intended previously ... and skip code block accordingly ....
        
            IF (FLAG_GOTO_30 .EQ. .FALSE.) THEN
        
                DTROC=SUBCOOL !Specified subcooling
                !1st run is for coarse convergence criteria
                CALL HPDM(DTVALU)

                FLAG_GOTO_30 = .TRUE.
                IF (FLAG_GOTO_30 .EQ. .FALSE.) THEN

                    !~Refine convergence criteria and run simulation again
                    CoarseConvergenceCriteriaMet=.TRUE.

                    AMBCON=1E-3 !0.01 !0.05 !0.3 !0.01 !1E-3 !Air temp. F	! VL_Magic_Number
                    CNDCON=0.1 !0.3 !Subcooling, F	! VL_Magic_Number
                    CHRGECONV=0.1 !0.1 !Charge, lbm	! VL_Magic_Number

                    EVPCON=1 !0.1 !0.2 !SUPERHEAT

                    FLOCON=5 !Mass flow rate, lbm/hr	! VL_Magic_Number
                    EVAPPAR(50) =0.1 !0.01 !7	! VL_Index_Replace	! VL_Magic_Number
                    CONDPAR(56)=0.1 !0.01 !7 !.05	! VL_Index_Replace	! VL_Magic_Number

                    !2nd run is for refined convergence criteria
                    CALL HPDM(DTVALU)
            
                ENDIF
            
            ENDIF
            
        ENDIF

        CALL DumpOutputs

        TimeSpent=SECNDS(TimeStart)
        WRITE(*,*)
        WRITE(*,*)'Calculation completed successfully.'
        WRITE(*,FMT_103)'Time Spent (Min):',TimeSpent/60
        WRITE(5,*) 
        WRITE(5,FMT_103)'Time Spent (Min):',TimeSpent/60  
        WRITE(*,*)'Press return to end program.'      
        !READ(*,*)                                     !For parametric run, comment this line

        IF (TaiE .GT. 32) THEN !only update time step above freezing temp.  - ISI 12/22/2009
            IF (MODE .NE. FIXEDORIFICESIM .OR. MODE .NE. TXVSIMULATION) THEN
                EXIT !only update time step for simulation mode - ISI 12/22/2009
            END IF
            IF (IsCoolingMode .EQ. 2) THEN
                EXIT !only update time step for heating mode - ISI 12/22/2009
            END IF
            IF (RHIE .LT. 0.8 ) THEN
                EXIT !only update time step for wet condition - ISI 12/22/2009  	! VL_Magic_Number
            END IF
        END IF

        IF(TimeStep == 1) THEN
            IDCFlowConst = CoilParams(1)%AirFlowRate*SQRT(CoilParams(1)%DPair)
            !ODCFlowConst = CoilParams(2)%AirFlowRate*SQRT(CoilParams(2)%DPair)
            ODCFlowConst=926.8     !497.17	! VL_Magic_Number
        END IF

        CFMEvp= ODCFlowConst*(CoilParams(2)%DPair*1000/UairPres)**(-0.3973)*UnitArFlw    !CoilParams(2)%AirFlowRate*(1-0.008*TimeStep)*UnitArFlw	! VL_Magic_Number

        CALL DetermineDefrostInitiate
        DefrostInitiate = .FALSE.
        IF(DefrostInitiate==.TRUE.) THEN
            FrostingPeriod=.FALSE.
            DefrostingPeriod = .TRUE.
        END IF 
    END DO

    CLOSE(5)
    CLOSE(6)
    CLOSE(9)

    CALL PrintCondenserResult
    CALL EndCondenserCoil
    
    CALL GetQOut(QUnitOut,LatOutputProvided)    !RS: Testing: Trying to read variables from PrintEvaporator File

    IF (MODE .NE. CONDENSERUNITSIM) THEN
        CALL PrintEvaporatorResult 
        CALL EndEvaporatorCoil
    END IF

    !ONCECALL = .FALSE.  !RS: Debugging
    !CALL EndEnergyPlus !RS: This will be called later by the E+ main routine

    CLOSE(666)

    !STOP   !RS: This also will be called by the E+ main routine later
    
    RETURN  !RS: Returning back to the previous location in E+

    END SUBROUTINE

    ! Comment Index
    ! --------------------------------------------------------
    ! VL_Magic_Number : magic number -- explain or resolve
    ! VL_Index_Replace : Can/should the index number be replaced by a field name?
    ! VL_User_Setting : flag or user setting?
    
!END MODULE ORNLsolver