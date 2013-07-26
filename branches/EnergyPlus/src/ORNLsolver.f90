        
    SUBROUTINE SimulationCycle(QSensUnitOut1,LatOutputProvided1,QUnitOut1,TempOut1) !RS: Attempting to pass variables out

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
    USE InputProcessor 
    USE DataGlobals, ONLY: RefName, RefrigIndex, DOASFlag
    USE DataHeatBalFanSys, ONLY: MAT, ZoneAirHumRat, TempControlType  !RS: Debugging
    USE WeatherManager !RS: Debugging: OutWetBulbTemp, OutDryBulbTemp
    USE Psychrometrics !RS: Debugging: Solving for TWiE
    USE DataZoneEnergyDemands   !RS: Debugging: Determining if the zone requires heating or cooling
    USE DataHVACGlobals !RS: Debugging: Small Load and SingleHeatingSetPoint, SingleCoolingSetPoint

    IMPLICIT NONE

    !Subroutine parameters

    CHARACTER(len=80)   :: Refrigerant
    REAL Temperature,Quality,Pressure
    
    INTEGER(2) RefPropErr			!Error flag:1-error; 0-no error 

    INTEGER(2) AirPropOpt			!Air prop calc. option
    INTEGER(2) AirPropErr			!Error flag:1-error; 0-no error 
    REAL AirProp(8)		!Air properties ! VL Comment: Array Size Explanation??

    REAL TimeStart, TimeSpent !TimeStop,    !RS: Debugging: Extraneous

    INTEGER ICHRGE !,I  !RS: Debugging: Extraneous
    REAL DTVALU
    REAL DTCONV
    REAL CHRGECONV
    INTEGER IERROR
    REAL DTVAL
    REAL STEP
    REAL CHGDIF

    REAL WinTrans
    INTEGER(2) IsCoolingMode !1=yes; 0=no   
    REAL, EXTERNAL :: ZEROCH
    REAL, EXTERNAL :: CHARGM
    INTEGER :: FTimeStep !Added Sankar transient    !RS: Debugging: Changed TimeStep to FTimeStep to make sure it wasn't being confused with the E+ TimeStep
    REAL :: SUPERAct
    REAL :: TsiCmpAct
    REAL :: TsoCmpAct
    REAL :: RHiCAct
    REAL :: RHiEAct
    REAL, SAVE :: IDCFlowConst
    REAL, SAVE :: ODCFlowConst
    LOGICAL, SAVE :: ONETIME = .TRUE.    !RS: Debugging: Keeps the program from calling the unit conversion subroutine over again.
    
    REAL(r64), INTENT (OUT) :: QUnitOut1            ! sensible capacity delivered to zone !RS: Testing: Trying to pass variables out
    REAL(r64), INTENT (OUT) :: LatOutputProvided1   ! Latent add/removal by packaged terminal unit (kg/s), dehumid = negative !RS: Testing: Trying to pass variables out
    REAL(r64), INTENT (OUT) :: QSensUnitOut1            ! sensible capacity delivered to zone !RS: Testing: Trying to pass variables out
    REAL(r64), INTENT (OUT) :: TempOut1            ! sensible capacity delivered to zone !RS: Testing: Trying to pass variables out
    REAL :: QSensUnitOut            ! sensible capacity delivered to zone !RS: Testing: Trying to pass variables out
    REAL :: QUnitOut    !Total capacity delivered to zone !RS: Testing: Passing variables out
    REAL :: LatOutputProvided   ! Latent add/removal by packaged terminal unit (kg/s), dehumid = negative !RS: Testing: Trying to pass variables out
    
    REAL TWiC   !RS: Wetbulb Temperature, Outdoor Entering (C)
    REAL TWiE   !RS: Wetbulb Temperature, Indoor Entering (C)
    REAL OutDryBulbTemp !RS: Drybulb temperature, Outdoor (C)
    REAL OutWetBulBTemp !RS: Wetbulb temperature, Outdoor (C)
    REAL OutBaroPress   !RS: Debugging
    REAL DummyHR !RS: Debugging
    !REAL QZnReq !RS: Debugging: The heating or cooling required by the zone    !RS: Debugging: Extraneous
    REAL QRemain    !RS: Debugging: The difference between qtotalout and the qrequired.
    
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
    INTEGER :: LogFile       =153 !RS: Debugging file denotion, hopefully this works.
    INTEGER(2),SAVE :: LastCoolingMode !RS: Debugging: Trying to only allocate/reallocate if cooling mode changes
    INTEGER:: ErrorFlag !RS: Debugging: Trying to clean out ErrorFlag at the start of every run; don't think it'll actually work, though
    
  OPEN(unit=LogFile,file='logfile.txt')    !RS: Debugging
    
    !Flow**:

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
    !OPEN(154,FILE='YorkHP.out')     ! VL_User_Setting -- file name
    !OPEN(155,FILE='YorkHP.log')     ! VL_User_Setting -- file name

    !Oil fraction
    CondPAR(59)=0.007             ! VL_Magic_Number    ! VL_Index_Replace
    EvapPAR(51)=0.007             ! VL_Magic_Number    ! VL_Index_Replace
    
    CondPAR(62)=1   !RS: Debugging: This will hopefully reset the "FirstTime" every run
    EvapPAR(54)=1   !RS: Debugging: This will hopefully reset the "FirstTime" every run
    ErrorFlag=0   !RS: Debugging: Resetting this at the beginning of each run so that errors don't carry over
    
    !RS: Debugging: Commenting out this section since we're only running cooling-only right now
    !IF (ZoneSysEnergyDemand(1)%RemainingOutputRequired .EQ. 0.0) THEN   !RS: Debugging: Checking the remaining
    IF (ZoneSysEnergyDemand(1)%TotalOutputRequired .EQ. 0) THEN
        QUnitOut=0
        LatOutputProvided=0
        RETURN
    !ELSEIF (ZoneSysEnergyDemand(1)%RemainingOutputRequired .GT. 0.0) THEN   !RS: Debugging: Checking the remaining
    ELSEIF (ZoneSysEnergyDemand(1)%TotalOutputRequired .GT. 0.0) THEN ! .OR. ZoneSysEnergyDemand(1)%RemainingOutputRequired .GT. 0.0) THEN !RS: Debugging: Is it needing a positive heat gain from HPSim?
        IsCoolingMode=0 !RS: Debugging: Heat pump operating in heating mode
        CondPAR(27)=IsCoolingMode
        EvapPar(20)=IsCoolingMode
    ELSE
        IsCoolingMode=1 !RS: Debugging: The heat pump is operating in cooling mode
        CondPAR(27)=IsCoolingMode
        EvapPar(20)=IsCoolingMode
    END IF
    
    IF (IsCoolingMode .NE. LastCoolingMode) THEN   !RS: Debugging: Only deallocating and reallocating if cooling mode changed between iterations
        CALL EndCondenserCoil
        CALL EndEvaporatorCoil
        !CondPAR(62)=1   !RS: Debugging: This will hopefully reset the "FirstTime" only when needed
        !EvapPAR(54)=1   !RS: Debugging: This will hopefully reset the "FirstTime" only when needed
    END IF

    CALL GetTempsOut(OutDryBulbTemp, OutWetBulbTemp, OutBaroPress, RHiC)    !RS: Debugging: RHiC = outdoor relative humidity
    TWiC=OutWetBulbTemp !RS: Debugging: Updating outdoor entering wet bulb temperature
    TaiC=OutDryBulbTemp !RS: Debugging: Updating outdoor entering dry bulb temperature
    DummyHR=ZoneAirHumRat(1)    !RS: Debugging
    IF (DOASFlag .EQ. 1) THEN   !RS: Checking to see if the system has return air or not
        TaiE=TaiC   !RS: DOAS system
        TWiE=TWiC   !RS: Debugging: Since it's 100% Outside Air here, this is true.
    ELSE    !RS: System with return air
        TaiE=MAT(1) !RS: Debugging: Updating indoor entering temperature with the mean air temperature for zone 1 every run
        CALL PsyTwbFnTdbWPb2(TaiE,DummyHR,OutBaroPress,TWiE)    !RS: Debugging: Converting from humidity ratio to wet bulb temp
    END IF
    CALL PsyRhFnTdbWPb2(TaiE,DummyHR,OutBaroPress,RHiE)  !RS: Debugging: Converting from humidity ratio to relative humidity
    RHiE=RHiE*100   !RS: Debugging: Conversion from decimal to fraction form
    
    IF ((TaiE*1.8+32)-TsiCmp .LT. 10) THEN     ! VL_Magic_Number number 10 ....
        TsiCmp = (TaiE*1.8+32) - 10 !Correct initial guess
    END IF

    IF (TsoCmp-(TaiC*1.8+32) .LT. 10) THEN     ! VL_Magic_Number number 10 ....
        TsoCmp = (TaiC*1.8+32) + 10 !Correct initial guess	! VL_Magic_Number
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
        
        CALL UnitConvert(Unit,CompPAR,CondPAR,EvapPAR,ShTbPAR,CapTubePAR, & !TxvPAR,  & !RS: Debugging: Extraneous
        AccumPAR,FilterPAR,CFMcnd,CFMevp,TaiC,TaiE,TWiC,TWiE, &
        Refchg,TSOCMP,TSICMP,SUPER,SUBCOOL,BaroPressure, &
        ChargeCurveSlope,ChargeCurveIntercept,RefLiquidLength,Tdis,Tliq)
        
        ONETIME = .FALSE.

    ELSE
        CFMcnd=CFMcnd*UnitArFlw   !RS: Debugging: This needs to be called every time for some reason
        CFMevp=CFMevp*UnitArFlw
        TaiC= TaiC*1.8+32   !RS: Debugging: E+ brings it in in C, and HPSim solves it in F
        TaiE= TaiE*1.8+32
    END IF
    
    BaroPressure=(OutBaroPress/1000)    !RS: Debugging: Setting the HPSim baro pressure equal to the E+ (kPa from Pa)
    EvapPAR(31)=BaroPressure
    CondPAR(38)=BaroPressure
    
    CALL InitAccumulator(AccumPAR)

    !set up Refrigerant variable...why?
    Refrigerant = RefName

    SUPERAct=SUPER
    TsiCmpAct=TsiCmp
    TsoCmpAct=TsoCmp
    RHiCAct=RHiC
    RHiEAct=RHiE
    
    !Get simulation starting time
    TimeStart=SECNDS(0.0)
    CoolHeatModeFlag = IsCoolingMode
    TimeInterval = 25.0                 ! VL_Magic_Number number    !RS: Debugging: Was 25.0, resetting to 15.0 (sec?)
    PrevSimTime = 0.0
    FTimestep=0 !RS: Debugging: Changed TimeStep to FTimeStep to make sure it wasn't being confused with the E+ TimeStep
    LastDefrostInitTime = 0.0

    !DO WHILE(SimRunning)
    DO WHILE(FrostingPeriod) !Added by Sankar
        !FirstTimeAirTempLoop=.TRUE.
        !FirstTimeFlowRateLoop=.TRUE.
        !FirstTimeChargeLoop=.TRUE.
        FTimeStep = FTimeStep+1   !RS: Debugging: Changed TimeStep to FTimeStep to make sure it wasn't being confused with the E+ TimeStep

        CurSimTime=(FTimeStep-1)*TimeInterval  !PrevSimTime+    !RS: Debugging: Changed TimeStep to FTimeStep to make sure it wasn't being confused with the E+ TimeStep
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

        IF (TWiC .GT. TaiC) THEN
            WRITE(*,*)'## ERROR ## Main: Condenser wet bulb temperature is greater than dry bulb temperature.'
            STOP
        END IF
        AirPropOpt=3                  ! VL_Magic_Number number	! VL_User_Setting
        AirProp(1)=Temperature_F2C(TaiC)  ! VL_Index_Replace
        AirProp(5)=TWiC                   ! VL_Index_Replace
        CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
        RHiC=AirProp(3)               ! VL_Index_Replace
        RhoAiC=AirProp(7)             ! VL_Index_Replace

        CondIN(5)=Temperature_F2C(TaiC)   ! VL_Index_Replace
        CondIN(6)=RHiC                    ! VL_Index_Replace

        IF (TWiE .GT. TaiE) THEN !ISI - 11/04/07
            WRITE(*,*)'## ERROR ## Main: Evaporator wet bulb temperature is greater than dry bulb temperature.'
            STOP
        END IF
        AirPropOpt=3                  ! VL_Magic_Number number	! VL_User_Setting
        AirProp(1)=Temperature_F2C(TaiE)  ! VL_Index_Replace
        AirProp(5)=TWiE                   ! VL_Index_Replace
        CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
        RHiE=AirProp(3)               ! VL_Index_Replace
        RhoAiE=AirProp(7)             ! VL_Index_Replace
        IF (RHiE .LT. 0) THEN   !RS: Debugging: Trying to avoid negative relative humidities
            RHiE=0
        END IF

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

        !IsCoolingMode=CondPAR(27)	! VL_Index_Replace  !RS: Debugging: Removing as IsCoolingMode is set above
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

        CASE DEFAULT
            !FAIL
        END SELECT
        
        ! VL : Check if a GOTO 30 was intended previously ... and skip code block accordingly ....        
        !IF (FLAG_GOTO_30 .EQ. .FALSE.) THEN !RS: Debugging: Commented out because there's no way it could equal TRUE
        
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
                DTVALU = ZEROCH(DTVAL,CHARGM,CHRGECONV,CHRGECONV,STEP,CHGDIF,IERROR)    !RS: Debugging: Temporarily setting in an Epsilon-NTU method
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
                CALL HPDM(DTVALU)  !RS: Debugging: Testing 123 !RS: Debugging: Temporarily setting in an Epsilon-NTU method

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
            
        !ENDIF

        CALL DumpOutputs

        TimeSpent=SECNDS(TimeStart)
        WRITE(*,*)
        WRITE(*,*)'HPSim Calculation completed successfully.'
        WRITE(*,FMT_103)'Time Spent (Min):',TimeSpent/60
        WRITE(5,*)
        WRITE(5,FMT_103)'Time Spent (Min):',TimeSpent/60
        !WRITE(*,*)'Press return to end program.'      
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

        IF(FTimeStep == 1) THEN !RS: Debugging: Changed TimeStep to FTimeStep to make sure it wasn't being confused with the E+ TimeStep
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
    !CLOSE(9)   !RS: Debugging: Not sure what this is attached to...

    CALL PrintCondenserResult
    !CALL EndCondenserCoil  !RS: Debugging: Removing for the moment
    
    CALL GetQOut(QSensUnitOut,LatOutputProvided,QUnitOut)    !RS: Testing: Trying to read variables from PrintEvaporator File   !RS: Debugging: Temporarily setting in an Epsilon-NTU method
    CALL GetNodeProp    !RS: Debugging: Trying to update the nodal properties
    
    QRemain=ZoneSysEnergyDemand(1)%RemainingOutputRequired-QUnitOut !-LatOutputProvided    !RS: Debugging: Qouts are load into zone

    WRITE(LogFile,*) 'Cooling Mode: ',IsCoolingMode !RS: Debugging: Is it cooling or heating?
    WRITE(LogFile,*) 'QSensOut: ',QSensUnitOut  !RS: Debugging: Printing out some data
    WRITE(LogFile,*) 'LatentOutput: ',LatOutputProvided
    WRITE(LogFile,*) 'QZoneRequired: ',ZoneSysEnergyDemand(1)%TotalOutputRequired
    WRITE(LogFile,*) 'Q left to meet required Q: ',QRemain
    
    IF (MODE .NE. CONDENSERUNITSIM) THEN
        CALL PrintEvaporatorResult 
        !CALL EndEvaporatorCoil !RS: Debugging: Removing for the moment
    END IF

    LastCoolingMode=ISCoolingMode   !RS: Debugging: Seeing if cooling mode changes between iterations
    
    CALL FlushHPOutput()
    !RS: Debugging: Testing: Seeing if the outputs should really be negative.
    QUnitOut1=QUnitOut
    TempOut1=TaiC
    QSensUnitOut1=QSensUnitOut    !RS: Debugging: Temporarily setting in an Epsilon-NTU method
    LatOutputProvided1=LatOutputProvided    !RS: Debugging: Temporarily setting in an Epsilon-NTU method

    CLOSE(666)

    RETURN  !RS: Returning back to the previous location in E+

    END SUBROUTINE

    ! Comment Index
    ! --------------------------------------------------------
    ! VL_Magic_Number : magic number -- explain or resolve
    ! VL_Index_Replace : Can/should the index number be replaced by a field name?
    ! VL_User_Setting : flag or user setting?
