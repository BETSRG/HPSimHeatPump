MODULE DataGenerators

          ! MODULE INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   March 2005
          !       MODIFIED
          !       RE-ENGINEERED  July 2006 BG, generalized and added data for ICE/SE model micro CHP

          ! PURPOSE OF THIS MODULE:
          ! This data-only module is a repository for the variables that relate specifically
          ! to the Fuel cell and Micro CHP modeling in EnergyPlus
          !  the data for the older BLAST generators are in those component's modules

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
    USE DataPrecisionGlobals
    USE DataGlobals, ONLY: MaxNameLength

    IMPLICIT NONE   ! Enforce explicit typing of all variables

    PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.

          ! MODULE PARAMETER DEFINITIONS:
    INTEGER, PARAMETER :: NormalizedCurveMode = 1 ! mode where efficiency curves are modifier curves
    INTEGER, PARAMETER :: DirectCurveMode     = 2 ! mode where efficiency curves are direct

    INTEGER, PARAMETER :: ConstantRateSkinLoss = 1 ! fixed rate mode for skin losses
    INTEGER, PARAMETER :: UADTSkinLoss         = 2 ! UAdelta T mode for skin losses
    INTEGER, PARAMETER :: QuadraticFuelNdotSkin= 3 ! Quadratic function of fuel flow for skin losses

    INTEGER, PARAMETER :: QuadraticFuncofNdot  = 1 ! function of fuel rate mode for air flow
    INTEGER, PARAMETER :: ConstantStoicsAirRat = 2 ! Constant air ratio in stoics with fuel constituents
    INTEGER, PARAMETER :: QuadraticFuncofPel   = 3 ! function of electric power mode

    INTEGER, PARAMETER :: NoRecoveryOnAirIntake = 101 ! mode for controlling intake air heat recovery
    INTEGER, PARAMETER :: RecoverBurnInvertBatt = 102 ! mode for controlling intake air heat recovery
    INTEGER, PARAMETER :: RecoverAuxiliaryBurner= 103 ! mode for controlling intake air heat recovery
    INTEGER, PARAMETER :: RecoverInverterBatt   = 104 ! mode for controlling intake air heat recovery
    INTEGER, PARAMETER :: RecoverInverter       = 105 ! mode for controlling intake air heat recovery
    INTEGER, PARAMETER :: RecoverBattery        = 106 ! mode for controlling intake air heat recovery

    INTEGER, PARAMETER :: RegularAir = 1 !
    INTEGER, PARAMETER :: UserDefinedConstituents = 2 !

    INTEGER, PARAMETER :: FuelInTempFromNode = 1
    INTEGER, PARAMETER :: FuelInTempSchedule = 2

    INTEGER, PARAMETER :: WaterInReformMains    = 21
    INTEGER, PARAMETER :: WaterInReformAirNode  = 22
    INTEGER, PARAMETER :: WaterInReformWaterNode = 23
    INTEGER, PARAMETER :: WaterInReformSchedule  = 24

    INTEGER, PARAMETER :: InverterEffConstant  = 1
    INTEGER, PARAMETER :: InverterEffQuadratic = 2

    INTEGER, PARAMETER :: FixedEffectiveness = 11 !exhaust gas HX modeling mode
    INTEGER, PARAMETER :: LMTDempiricalUAeff = 12 !exhaust gas HX modeling mode
    INTEGER, PARAMETER :: LMTDfundementalUAeff = 13 !exhaust gas HX modeling mode
    INTEGER, PARAMETER :: Condensing = 14           !exhaust gas HX modeling mode

    INTEGER, PARAMETER :: SimpleEffConstraints = 21 !electrical storage modeling mode
    INTEGER, PARAMETER :: LeadAcidBatterySaupe = 22 !electrical storage modeling mode
    INTEGER, PARAMETER :: LeadAcidBatterManwellMcGowan = 23 ! electrical storage modeling mode

    INTEGER, PARAMETER :: SurroundingZone = 31
    INTEGER, PARAMETER :: AirInletForFC   = 32

    INTEGER, PARAMETER :: OpModeOFF      = 1 ! CHP operating mode OFF
    INTEGER, PARAMETER :: OpModeStandby  = 2 ! CHP operating mode Stand By
    INTEGER, PARAMETER :: OpModeWarmUp   = 3 ! CHP operating mode Warm Up or start up
    INTEGER, PARAMETER :: OpModeNormal   = 4 ! CHP operating mode Normal
    INTEGER, PARAMETER :: opModeCoolDown = 5 ! CHP operating mode Cool down or shut down

    INTEGER, PARAMETER :: fuelModeGaseousConstituents = 301
    INTEGER, PARAMETER :: fuelModeGenericLiquid       = 302

    REAL,    PARAMETER :: MinProductGasTemp = 100.0 ! Minimum bound on search for product gas temps
    REAL,    PARAMETER :: MaxProductGasTemp = 2000.0 ! Maximum bound on search for product gas temps

    INTEGER, PARAMETER :: NISTShomate    = 41
    INTEGER, PARAMETER :: NASAPolynomial = 42

    REAL,    PARAMETER :: RinKJperMolpK  = 0.0083145 !R is ideal gas constant (kJ/mol-K)
    REAL,    PARAMETER :: InitHRTemp     = 50.0 !Initialization temperature for heat recovery water

    REAL,    PARAMETER :: ImBalanceTol  = 0.00001 ! used as fraction of electrical power at power module

          ! DERIVED TYPE DEFINITIONS

TYPE FCPowerModuleStruct
   ! user input data
    CHARACTER(len=MaxNameLength) :: Name     = ' '  !name of this PowerModule data
    INTEGER                      :: EffMode  = 0 ! mode for efficiency curves
    INTEGER                      :: EffCurveID = 0 ! pointer to curve for efficiency
    REAL                    :: NomEff   = 0.0 ! nominal efficiency
    REAL                    :: NomPel  = 0.0 ! nominal power rate at rating point
    INTEGER                      :: NumCycles = 0  ! number of start stop cycles
    REAL                    :: CyclingDegradRat = 0.0 ! rate of degradation from cycles
    REAL                    :: NumRunHours = 0.0 ! number of hours of operation
    REAL                    :: OperateDegradRat = 0.0 ! rate of degradation from run time (per hour)
    REAL                    :: ThreshRunHours  = 0.0 ! number of hours before degradation starts
    REAL                    :: UpTranLimit = 0.0 !power up transient limit
    REAL                    :: DownTranLimit = 0.0 !power down tran limit
    REAL                    :: StartUpTime   = 0.0 !time for start up [hours]
    REAL                    :: StartUpFuel   = 0.0 !fuel use during start up
    REAL                    :: StartUpElectConsum  = 0.0 !electricity used during start up
    REAL                    :: StartUpElectProd  = 0.0 !electricity produced during start up
    REAL                    :: ShutDownTime  = 0.0 ! time to shut down [hours]
    REAL                    :: ShutDownFuel  = 0.0 ! fuel consumed during shut down
    REAL                    :: ShutDownElectConsum = 0.0 !Elect consumed during shut down
    REAL                    :: ANC0 = 0.0 ! Ancilliary Loads constant term
    REAL                    :: ANC1 = 0.0 ! Ancilliary Loads linear term
    INTEGER                      :: SkinLossMode = 0 ! how are skin losses determined
    CHARACTER(len=MaxNameLength) :: ZoneName = ' ' !
    Integer                      :: ZoneID = 0 ! "pointer" to zone with component in it
    REAL                    :: RadiativeFract = 0.0 !
    REAL                    :: QdotSkin = 0.0 !
    REAL                    :: UAskin = 0.0 !
    Integer                      :: SkinLossCurveID = 0 !
    INTEGER                      :: WaterSupplyCurveID = 0 ! pointer to curve for water use in reforming
    REAL                    :: NdotDilutionAir = 0.0 ! user defined constant flow of dilution air (kmol/sec)
    REAL                    :: StackHeatLossToDilution = 0.0 ! (watts)
    CHARACTER(len=MaxNameLength) :: DilutionInletNodeName = ' ' !dilution -> AirHR ?? added air heat recovery path
    INTEGER                      :: DilutionInletNode = 0 ! pointer to node for inlet
    CHARACTER(len=MaxNameLength) :: DilutionExhaustNodeName = ' '
    INTEGER                      :: DilutionExhaustNode = 0 ! pointer to node getting exhaust
    REAL                    :: PelMin = 0.0 ! minimum operating point for FCPM electrical power Pel
    REAL                    :: PelMax = 0.0 ! maximum operating point for FCPM electrical power Pel
   !Calculated values and input from elsewhere

    REAL                    :: Pel             = 0.0 !current DC electrical power produced
    REAL                    :: PelLastTimeStep = 0.0
    REAL                    :: Eel             = 0.0 ! power module efficiency
    REAL                    :: QdotStackCool   = 0.0 ! Heat removed by stack cooler
    REAL                    :: FractionalDayofLastStartUp = 0.0 !fractional days into simulation
    REAL                    :: FractionalDayofLastShutDown = 0.0 !fractional Days into simulations
    LOGICAL                      :: HasBeenOn =.true.
    LOGICAL                      :: DuringShutDown = .false.
    LOGICAL                      :: DuringStartUp = .false.
    REAL                    :: NdotFuel = 0.0 ! molar fuel use rate.  (kmol/sec)
    REAL                    :: TotFuelInEnthalphy = 0.0 ! Enthalpy of fuel coming into FCPM (watts)
    REAL                    :: NdotProdGas       = 0.0  !(kmol/sec)
    REAL, DIMENSION(14)          :: ConstitMolalFract = 0.0
    INTEGER, DIMENSION(14)       :: GasLibID  = 0 ! lookup ID in Gas Phase ThermoChemistry Structure Array
    REAL                    :: TprodGasLeavingFCPM = 0.0
    REAL                    :: NdotAir  = 0.0 ! molar air use rate    (kmol/sec)
    REAL                    :: TotAirInEnthalphy  = 0.0 ! Enthalpy of air coming nto FCPM energy balance (watts)
    REAL                    :: NdotLiqwater= 0.0    ! molar water use rate (kmol/sec)
    REAL                    :: TwaterInlet = 0.0    !
    REAL                    :: WaterInEnthalpy    = 0.0 ! Enthalpy of liquid water used for reforming (watts)
    REAL                    :: DilutionAirInEnthalpy = 0.0  ! Enthalpy of Dilution air coming into FCPM (watts)
    REAL                    :: DilutionAirOutEnthalpy = 0.0 !
    REAL                    :: PelancillariesAC    = 0.0  !ancillary power (watts)
    REAL                    :: TotProdGasEnthalphy = 0.0 !Enthalphy of product gases leaving FCPM   (watts)
    REAL                    :: WaterOutEnthalpy    = 0.0 ! enthalpy of vapor from water used for reforming
    INTEGER                      :: SeqSubstitIter  = 0 !
    INTEGER                      :: RegulaFalsiIter = 0 !
END TYPE FCPowerModuleStruct

TYPE FCAirSupplyDataStruct
   ! user input data
    CHARACTER(len=MaxNameLength) :: Name = ' ' !name of this
    CHARACTER(len=MaxNameLength) :: NodeName = ' ' ! Air supply node name
    INTEGER                      :: SupNodeNum = 0 ! Air supply node ID
    INTEGER                      :: BlowerPowerCurveID = 0 ! "pointer" to blower power quadratic
    REAL                    :: BlowerHeatLossFactor = 0.0 ! alpha for blower heat loss fraction
    INTEGER                      :: AirSupRateMode = 0 ! control for modeling method used to deterime supply air flow rate
    REAL                    :: Stoics = 0.0 !excess air ratio
    INTEGER                      :: AirFuncPelCurveID = 0 !"pointer" to curve for air as function of power
    REAL                    :: AirTempCoeff   = 0.0 ! coeff a3 in equ 16.
    INTEGER                      :: AirFuncNdotCurveID = 0 !"pointer" to curve for air as function of fuel flow rate
    INTEGER                      :: IntakeRecoveryMode = 0 !
    INTEGER                      :: ConstituentMode = 0 ! how are air data input
    INTEGER                      :: NumConstituents = 0 !
    CHARACTER(len=MaxNameLength), DIMENSION(14) :: ConstitName = ' '
    REAL, DIMENSION(14) :: ConstitMolalFract = 0.0
   !Calculated values and input from elsewhere
    INTEGER, DIMENSION(14)        :: GasLibID  = 0 ! lookup ID in Gas Phase ThermoChemistry Structure Array

    REAL                    :: O2fraction = 0.0
    REAL                    :: TairIntoBlower = 0.0 ! temperature entering blower
    REAL                    :: TairIntoFCPM   = 0.0 ! temperature leaving blower and entering FCPM
    REAL                    :: PairCompEl     = 0.0 ! power drawn by compressor
    REAL                    :: QskinLoss      = 0.0 ! pumping losses for zone
    REAL                    :: QintakeRecovery = 0.0 !heat recovered on intake air by accessories

END TYPE FCAirSupplyDataStruct

TYPE FCStackCoolerDataStruct
    ! user input data
    CHARACTER(len=MaxNameLength) :: Name = ' ' !name of this stack cooler module
    CHARACTER(len=MaxNameLength) :: WaterInNodeName  = ' ' !HR Water Inlet Node
    INTEGER                      :: WaterInNode      = 0  ! HR Water Outlet Node ID
    CHARACTER(len=MaxNameLength) :: WaterOutNodeName = ' ' !HR water outlet Node name
    INTEGER                      :: WaterOutNode     = 0 ! HR Water outlet Node ID
    REAL                    :: TstackNom        = 0.0 ! nominal fuel cell stack temperature
    REAL                    :: TstackActual     = 0.0 ! actual fuel cell stack temperature
    REAL                    :: r0               = 0.0 ! stack cooling power coefficient r0
    REAL                    :: r1               = 0.0 ! stack cooling power coefficient r1
    REAL                    :: r2               = 0.0 ! stack cooling power coefficient r2
    REAL                    :: r3               = 0.0 ! stack cooling power coefficient r3
    REAL                    :: MdotStackCoolant = 0.0 ! stack coolant flow rate kg/s
    REAL                    :: UAs_cool         = 0.0 ! stack heat transfer coef
    REAL                    :: Fs_cogen         = 0.0 !
    REAL                    :: As_cogen         = 0.0 !
    REAL                    :: MdotCogenNom     = 0.0 !
    REAL                    :: hCogenNom        = 0.0
    REAL                    :: ns               = 0.0 !
    REAL                    :: PstackPumpEl     = 0.0
    REAL                    :: PmpPowerLossFactor = 0.0
    REAL                    :: f0 = 0.0
    REAL                    :: f1 = 0.0
    REAL                    :: f2 = 0.0

    ! calculated and from elsewhere
    LOGICAL                      :: StackCoolerPresent = .false. ! control modeling
    REAL                    :: qs_cool  = 0.0 !
    REAL                    :: qs_air   = 0.0 !

END Type FCStackCoolerDataStruct

TYPE FCWaterSupplyDataStruct
    CHARACTER(len=MaxNameLength) :: Name = ' ' !name of this water supply module
    INTEGER                      :: WaterTempMode =0 ! temperature of water inlet determination
    CHARACTER(len=MaxNameLength) :: NodeName = ' ' !node name for temperature at input
    INTEGER                      :: NodeNum = 0 ! node number for temperature at input
    INTEGER                      :: SchedNum = 0 ! water temperature at input
    INTEGER                      :: WaterSupRateCurveID = 0 ! "pointer" to water flow rate curve as a function of fuel rate
    INTEGER                      :: PmpPowerCurveID = 0 !"pointer to Pump power curve as a function of water flow Rate
    REAL                    :: PmpPowerLossFactor = 0.0 !Pump heat loss factor
    !calculated data
    LOGICAL                      :: IsModeled  = .TRUE.
    REAL                    :: TwaterIntoCompress = 0.0 ! inlet Water Temperature
    REAL                    :: TwaterIntoFCPM     = 0.0 ! pumped water temp
    REAL                    :: PwaterCompEl       = 0.0 ! water pump power
    REAL                    :: QskinLoss          = 0.0 ! pumping losses for zone


END TYPE FCWaterSupplyDataStruct

TYPE FCAuxilHeatDataStruct
    CHARACTER(len=MaxNameLength) :: Name      = ' ' !name of this auxiliary heating module
    CHARACTER(len=MaxNameLength) :: ZoneName  = ' '
    INTEGER                      :: ZoneID    = 0
    REAL                    :: UASkin    = 0.0 !for skin losses to zone
    REAL                    :: ExcessAirRAT = 0.0 !
    REAL                    :: ANC0      = 0.0
    REAL                    :: ANC1      = 0.0
    INTEGER                      :: SkinLossDestination = 0 !control mode for where lost heat goes
    REAL                    :: MaxPowerW = 0.0
    REAL                    :: MinPowerW = 0.0
    REAL                    :: MaxPowerkmolperSec = 0.0
    REAL                    :: MinPowerkmolperSec = 0.0
    ! calculated and from elsewhere
    INTEGER                      :: NumConstituents = 0
    REAL                    :: TauxMix           = 0.0
    REAL                    :: NdotAuxMix        = 0.0
    REAL, DIMENSION(14)           :: ConstitMolalFract = 0.0
    INTEGER, DIMENSION(14)        :: GasLibID          = 0 ! lookup ID in Gas Phase ThermoChemistry Structure Array
    REAL                    :: QskinLoss         = 0.0 !Heat lost to room
    REAL                    :: QairIntake        = 0.0 ! heat into intake air


END TYPE FCAuxilHeatDataStruct

TYPE FCExhaustHXDataStruct
    ! user defined variables
    CHARACTER(len=MaxNameLength) :: Name             = ' ' !name of this exhaust gas heat recovery
    CHARACTER(len=MaxNameLength) :: WaterInNodeName  = ' ' !HR Water Inlet Node
    INTEGER                      :: WaterInNode      = 0  ! HR Water Outlet Node ID
    CHARACTER(len=MaxNameLength) :: WaterOutNodeName = ' ' !HR water outlet Node name
    INTEGER                      :: WaterOutNode     = 0 ! HR Water outlet Node ID
    REAL                    :: WaterVolumeFlowMax = 0.0 ! HR water flow rate max avail
    CHARACTER(len=MaxNameLength) :: ExhaustOutNodeName = ' ' ! air node for exhaust flow
    INTEGER                      :: ExhaustOutNode   = 0 ! Exhaust Air node ID
    INTEGER                      :: HXmodelMode      = 0  !Heat Exchanger Calculation Method
    REAL                    :: HXEffect         = 0 ! Heat Exchanger Effectiveness (method 1)
    REAL                    :: hxs0             = 0.0 !(method 2)
    REAL                    :: hxs1             = 0.0 ! (method 2)
    REAL                    :: hxs2             = 0.0 ! (method 2)
    REAL                    :: hxs3             = 0.0 ! (method 2)
    REAL                    :: hxs4             = 0.0 ! (method 2)
    REAL                    :: h0gas            = 0.0 ! (method 3)
    REAL                    :: NdotGasRef       = 0.0 ! (method 3)
    REAL                    :: nCoeff           = 0.0 ! (method 3)
    REAL                    :: AreaGas          = 0.0 ! (method 3)
    REAL                    :: h0Water          = 0.0  ! (method 3)
    REAL                    :: NdotWaterRef     = 0.0 !(method 3)
    REAL                    :: mCoeff           = 0.0 ! (method 3)
    REAL                    :: AreaWater        = 0.0 !(method 3)
    REAL                    :: Fadjust          = 0.0     ! (method 3)
    REAL                    :: l1Coeff          = 0.0 ! (method 4)
    REAL                    :: l2Coeff          = 0.0 ! (method 4)
    REAL                    :: CondensationThresholdTemp = 0.0 ! (method 4) [degrees C]
   !calculated
    REAL                    :: qHX = 0.0 ! heat flow from gas stream to water
    REAL                    :: THXexh = 0.0 ! temperature of exhaust gases leaving heat exchanger.
    REAL                    :: WaterMassFlowRateDesign = 0.0 !Design level of water flow rate
    REAL                    :: WaterMassFlowRate  = 0.0 ! water flow rate in plant loop
    REAL                    :: WaterInletTemp  = 0.0 !
    REAL                    :: WaterVaporFractExh = 0.0 ! water vapor fraction in exhaust gas stream.
    REAL                    :: CondensateRate     = 0.0 ! water condensation rate.
    REAL, DIMENSION(14)     :: ConstitMolalFract = 0.0
    INTEGER, DIMENSION(14)       :: GasLibID          = 0 ! lookup ID in Gas Phase ThermoChemistry Structure Array
    REAL                    :: NdotHXleaving       = 0.0
    REAL                    :: WaterOutletTemp     = 0.0
    REAL                    :: WaterOutletEnthalpy = 0.0

END TYPE FCExhaustHXDataStruct

TYPE BatteryDichargeDataStruct
    ! user defined variables
    CHARACTER(len=MaxNameLength) :: Name = ' ' !name of this battery data set
    REAL                    :: NumInSeries = 0.0
    REAL                    :: NumInParallel = 0.0
    REAL                    :: NominalVoltage = 0.0
    REAL                    :: LowVoltsDischarged = 0.0 !not used
    INTEGER                      :: NumTablePairs = 0
    REAL, Allocatable, Dimension(:) :: DischargeCurrent  ! amps
    REAL, Allocatable, Dimension(:) :: DischargeTime     ! hours
    ! calculated variables
    REAL                    :: k =0.0 !parameter in Manwell McGowan model
    REAL                    :: c =0.0 !parameter in Manwell McGowan model
    REAL                    :: qmax =0.0 !parameter in Manwell McGowan model

END TYPE BatteryDichargeDataStruct


TYPE FCElecStorageDataStruct
    !user defined variables
    CHARACTER(len=MaxNameLength) :: Name = ' ' !name of this electrical storage module
    INTEGER                      :: StorageModelMode = 0
    REAL                    :: StartingEnergyStored = 0.0 !joules inside
    REAL                    :: EnergeticEfficCharge = 0.0 ! for
    REAL                    :: EnergeticEfficDischarge = 0.0
    REAL                    :: MaxPowerDraw  = 0.0 ! for simple bucket method 0
    REAL                    :: MaxPowerStore  = 0.0 ! for simple bucket method 0
    REAL                    :: NominalVoltage = 0.0 !
    REAL                    :: NominalEnergyCapacity = 0.0 ! [J]
    !calculated and from elsewhere vars
    REAL                    :: ThisTimeStepStateOfCharge =0.0 ! [J]
    REAL                    :: LastTimeStepStateOfCharge =0.0! [J]
    REAL                    :: PelNeedFromStorage =0.0
    REAL                    :: IdesiredDischargeCurrent = 0.0 !
    REAL                    :: PelFromStorage  = 0.0 ! power
    REAL                    :: IfromStorage = 0.0 ! current this timestepm
    REAL                    :: PelIntoStorage = 0.0 !
    REAL                    :: QairIntake =0.0! heat into intake air
    !nested structures
    TYPE(BatteryDichargeDataStruct) :: Battery

END TYPE FCElecStorageDataStruct


TYPE FCInverterDataStruct
    ! user-defined data
    CHARACTER(len=MaxNameLength) :: Name = ' ' !name of this inverter
    INTEGER                      :: EffMode = 0 !efficiency calculation mode
    REAL                    :: ConstEff = 0.0 !
    INTEGER                      :: EffQuadraticCurveID = 0
    ! calculated and from elsewhere
    REAL                    :: PCUlosses  = 0.0
    REAL                    :: QairIntake =0.0

END TYPE FCInverterDataStruct


TYPE FCReportDataStruct  !these are all for reporting only!
       REAL    :: ACPowerGen                 = 0.0 ! reporting: power (W)
       REAL    :: ACEnergyGen                = 0.0 ! reporting: energy (J)

       REAL    :: QdotExhaust                = 0.0 ! reporting: exhaust gas heat recovered (W)
       REAL    :: TotalHeatEnergyRec         = 0.0 ! reporting: total heat recovered (J)
       REAL    :: ExhaustEnergyRec           = 0.0 ! reporting: exhaust gas heat recovered (J)
       REAL    :: FuelEnergyLHV              = 0.0 ! reporting: Fuel Energy used in Lower Heating Value(J)
       REAL    :: FuelEnergyUseRateLHV       = 0.0 ! reporting: Fuel Energy used in Lower Heating Value(W)
       REAL    :: FuelEnergyHHV              = 0.0 ! reporting: Fuel Energy used in Lower Heating Value(J)
       REAL    :: FuelEnergyUseRateHHV       = 0.0 ! reporting: Fuel Energy used in Lower Heating Value(W)
       REAL    :: FuelRateMdot               = 0.0 ! (Kg/s)
       REAL    :: HeatRecInletTemp           = 0.0 ! reporting: Heat Recovery Loop Inlet Temperature (C)
       REAL    :: HeatRecOutletTemp          = 0.0 ! reporting: Heat Recovery Loop Outlet Temperature (C)
       REAL    :: HeatRecMdot                = 0.0 ! reporting: Heat Recovery Loop Mass flow rate (kg/s)
        ! air supply and blower
       REAL    :: TairInlet         = 0.0 ! State point 1
       REAL    :: TairIntoFCPM      = 0.0 ! Temperature at State point 4
       REAL    :: NdotAir           = 0.0 ! air flow in kmol/sec
       REAL    :: TotAirInEnthalphy = 0.0 ! Enthalpy at State point 4
       REAL    :: BlowerPower       = 0.0 ! electrical power used by air supply blower
       REAL    :: BlowerEnergy      = 0.0 ! electrical energy used by air supply blower
       REAL    :: BlowerSkinLoss    = 0.0 ! heat rate of losses by blower
        !fuel supply and compressor
       REAL    :: TfuelInlet     = 0.0 ! State point 2 [C]
       REAL    :: TfuelIntoFCPM  = 0.0 ! state point 5 [C]
       REAL    :: NdotFuel       = 0.0 ! fuel flow in [kmol/sec]
       REAL    :: TotFuelInEnthalpy = 0.0 ! state point 5 [W]
       REAL    :: FuelCompressPower = 0.0 ! electrical power used by fuel supply compressor [W]
       REAL    :: FuelCompressEnergy= 0.0 ! electrical energy used by fuel supply compressor [J]
       REAL    :: FuelCompressSkinLoss = 0.0 !heat rate of losses.by fuel supply compressor [W]
        !reformer water supply
       REAL    :: TwaterInlet    = 0.0 ! State point 3
       REAL    :: TwaterIntoFCPM = 0.0 ! State point 6
       REAL    :: NdotWater      = 0.0 ! water flow in kmol/sec (reformer water)
       REAL    :: WaterPumpPower = 0.0 ! electrical power used by water pump [W]
       REAL    :: WaterPumpEnergy = 0.0 ! electrical energy used by water pump [J]
       REAL    :: WaterIntoFCPMEnthalpy = 0.0  ! state point 6
        !product (exhaust) gas leaving power module
       REAL    :: TprodGas       = 0.0 ! State point 7 Product Gas temperature
       REAL    :: EnthalProdGas  = 0.0 ! state point 7 product gas enthalpy
       REAL    :: NdotProdGas    = 0.0 ! point 7 flow rate [kmol/sec]
       REAL    :: NdotProdAr     = 0.0 ! argon flow rate at point 7
       REAL    :: NdotProdCO2    = 0.0 ! carbon dioxide flow rate at point 7
       REAL    :: NdotProdH2O    = 0.0 ! water vapor flow rate at point 7
       REAL    :: NdotProdN2     = 0.0 ! nitrogen flow rate at point 7
       REAL    :: NdotProdO2     = 0.0 ! oxygen flow rate at point 7

       !heat exchanger for water to exhaust heat recovery
       REAL    :: qHX = 0.0 ! heat flow from gas stream to water [W]
       REAL    :: HXenergy = 0.0 !energy from gas stream to water [J]
       REAL    :: THXexh = 0.0 ! temperature of exhaust gases leaving heat exchanger.
       REAL    :: WaterVaporFractExh = 0.0 ! water vapor fraction in exhaust gas stream
                                           ! relative to water vapor entering HX  (NdotH20/Ndoaux-mix)
       REAL    :: CondensateRate     = 0.0 ! water condensation rate [kmol/s]

       INTEGER :: SeqSubstIterations = 0 ! number of iterations in SOFC loop
       INTEGER :: RegulaFalsiIterations = 0 ! number of iterations in Tproduct gas solving

       REAL    :: ACancillariesPower  = 0.0 !
       REAL    :: ACancillariesEnergy = 0.0 !
       REAL    :: PCUlosses = 0.0  ! power conditioning Unit losses
       REAL    :: DCPowerGen = 0.0 ! Pel, Power module power level [W]
       REAL    :: DCPowerEff = 0.0 ! Eel, power module efficiency []
       REAL    :: ElectEnergyinStorage = 0.0 ! State of charge in Electrical Storage [J]
       REAL    :: StoredPower = 0.0 ! Power added to Electrical Storage [W]
       REAL    :: StoredEnergy = 0.0 ! energy added to Electrical STorage [J]
       REAL    :: DrawnPower = 0.0 ! Power drawn from Electrical STorage [W]
       REAL    :: DrawnEnergy = 0.0 ! Energy drawn from Electrical STorage [J]

       REAL    :: SkinLossPower   = 0.0 ! heat loss to surrounding zone [W]
       REAL    :: SkinLossEnergy  = 0.0 ! heat loss to surround zone [J]
       REAL    :: SkinLossConvect = 0.0 ! convective heat loss to zone [W]
       REAL    :: SkinLossRadiat  = 0.0 ! radiative heat loss to zone [W}

       REAL    :: ElectEfficiency  = 0.0
       REAL    :: ThermalEfficiency = 0.0
       REAL    :: OverallEfficiency = 0.0
       REAL    :: ExergyEfficiency  = 0.0

END TYPE FCReportDataStruct
TYPE FCDataStruct
  ! from input data and nested types for subsystems
    CHARACTER(len=MaxNameLength) :: Name           = ' ' ! user identifier
    CHARACTER(len=MaxNameLength) :: NameFCPM       = ' ' ! name of FC Power Module
    TYPE(FCPowerModuleStruct)    :: FCPM                 ! data for Power Module
    CHARACTER(len=MaxNameLength) :: NameFCAirSup   = ' ' ! name of air supply module for fuel cell
    TYPE(FCAirSupplyDataStruct)  :: AirSup               ! data for air supply module
    CHARACTER(len=MaxNameLength) :: NameFCFuelSup  = ' ' ! name of fuel supply module
    INTEGER                      :: FuelSupNum     = 0   ! indes for fuel supply module structure
    CHARACTER(len=MaxNameLength) :: NameFCWaterSup = ' ' ! name of water supply module
    TYPE(FCWaterSupplyDataStruct):: WaterSup             ! data for water supply module
    CHARACTER(len=MaxNameLength) :: NameFCAuxilHeat= ' ' ! name of auxiliary heating module
    TYPE(FCAuxilHeatDataStruct)  :: AuxilHeat            ! data for auxiliary heating module
    CHARACTER(len=MaxNameLength) :: NameExhaustHX  = ' ' ! name of Exhaust HX module
    TYPE(FCExhaustHXDataStruct)  :: ExhaustHX            ! data for Exhaust heat exchanger module
    CHARACTER(len=MaxNameLength) :: NameElecStorage= ' ' ! name of Battery module
    TYPE(FCElecStorageDataStruct):: ElecStorage          ! data for Battery module
    CHARACTER(len=MaxNameLength) :: NameInverter   = ' ' ! name of Inverter Module
    TYPE(FCInverterDataStruct)   :: Inverter             ! data for INverter module
    CHARACTER(len=MaxNameLength) :: NameStackCooler   = ' ' ! name of Inverter Module
    TYPE(FCStackCoolerDataStruct) :: StackCooler             ! data for INverter module
    INTEGER                      :: CWLoopNum     = 0  ! cooling water plant loop index number
    INTEGER                      :: CWLoopSideNum = 0  ! cooling water plant loop side index
    INTEGER                      :: CWBranchNum   = 0  ! cooling water plant loop branch index
    INTEGER                      :: CWCompNum     = 0  ! cooling water plant loop component index
    TYPE(FCReportDataStruct)     :: Report               ! data for reporting as E+ output variables

   ! calculated whole-system level variables
    REAL                    :: ACPowerGen   = 0.0 ! Net output from SOFC unit
    REAL                    :: QconvZone    = 0.0  ! convective heat lost to surrounding zone
    REAL                    :: QradZone     = 0.0  ! radiative heat lost to surrounding zone
    INTEGER                      :: DynamicsControlID = 0 !
    REAL                    :: TimeElapsed  = 0.0 ! used to track when timestep has changed
END TYPE

TYPE GeneratorFuelSupplyDataStruct
   ! user input data
    CHARACTER(len=MaxNameLength) :: Name = ' ' !name of this fuel supply module
    INTEGER                      :: FuelTempMode = 0 ! temperature of fuel node
    INTEGER                      :: FuelTypeMode = 0 ! type of fuel, gasous or liquid
    CHARACTER(len=MaxNameLength) :: NodeName = ' ' !node name for temperature at input
    INTEGER                      :: NodeNum = 0 ! node number for temperature at input
    INTEGER                      :: SchedNum = 0 ! fuel temperature at input
    INTEGER                      :: CompPowerCurveID = 0 ! "pointer" to compressor power cubic curve
    REAL                    :: CompPowerLossFactor = 0.0
    INTEGER                      :: NumConstituents  !number of constituents in fue supply
    CHARACTER(len=MaxNameLength), DIMENSION(14) :: ConstitName = ' '
    REAL,   DIMENSION(14)        :: ConstitMolalFract = 0.0

    !calculated data (except some for generic liquid)
    INTEGER, DIMENSION(14)       :: GasLibID  = 0 ! lookup ID in Gas Phase ThermoChemistry Structure Array
    REAL                    :: LHV  = 0.0     ! lower heating value of gaseous fuel (kJ/mol)
    REAL                    :: LHVJperkg = 0.0  ! lower heating value of gaseous fuel (J/kg)
    REAL                    :: LHVliquid = 0.0 ! userdefined lhv for generic liquid (J/kg)
    REAL                    :: HHV  = 0.0     ! higher heating value of fuel (J/kg)
    REAL                    :: MW   = 0.0     ! molecular weight g/mol
    REAL                    :: eCO2  = 0.0     ! mass flow based CO2 emmissions factor for complete combustion (-)
    REAL                    :: KmolPerSecToKgPerSec = 0.0 ! conversion from moles to kilograms for this fuel. (
    REAL                    :: StoicOxygenRate = 0.0
    REAL                    :: TfuelIntoCompress = 0.0  ! inlet fuel temperature
    REAL                    :: TfuelIntoFCPM     = 0.0  ! compressed fuel temp
    REAL                    :: PfuelCompEl       = 0.0  ! fuel compressor power
    REAL                    :: QskinLoss         = 0.0  ! pumping losses for zone
    REAL                    :: CO2ProductGasCoef = 0.0  ! molar multiplier for stoic products of this fuel
    REAL                    :: H20ProductGasCoef = 0.0  ! molar multiplier for stoic products of this fuel

END TYPE GeneratorFuelSupplyDataStruct

TYPE GasPropertyDataStruct
    CHARACTER(len=MaxNameLength) :: ConstituentName           = ' ' !
    CHARACTER(len=MaxNameLength) :: ConstituentFormula        = ' '
    REAL                    :: StdRefMolarEnthOfForm     = 0.0 !
    INTEGER                      :: ThermoMode = 0 ! method of calculation for thermodynamics
    REAL                    :: ShomateA = 0.0
    REAL                    :: ShomateB = 0.0
    REAL                    :: ShomateC = 0.0
    REAL                    :: ShomateD = 0.0
    REAL                    :: ShomateE = 0.0
    REAL                    :: ShomateF = 0.0
    REAL                    :: ShomateG = 0.0
    REAL                    :: ShomateH = 0.0
    REAL                    :: NumCarbons = 0.0
    REAL                    :: NumHydrogens = 0.0
    REAL                    :: NumOxygens   = 0.0
    REAL                    :: MolecularWeight = 0.0
    REAL                    :: NASA_A1 = 0.0
    REAL                    :: NASA_A2 = 0.0
    REAL                    :: NASA_A3 = 0.0
    REAL                    :: NASA_A4 = 0.0
    REAL                    :: NASA_A5 = 0.0
    REAL                    :: NASA_A6 = 0.0
    REAL                    :: NASA_A7 = 0.0
END TYPE

TYPE GeneratorDynamicsManagerStruct
   ! user input data
    CHARACTER(len=MaxNameLength) :: Name = ' '
    REAL                    :: PelMin = 0.0 ! minimum operating point for electrical power Pel
    REAL                    :: PelMax = 0.0 ! maximum operating point for electrical power Pel
    REAL                    :: UpTranLimit = 0.0 !power up transient limit W/s
    REAL                    :: DownTranLimit = 0.0 !power down tran limit  W/s
    REAL                    :: UpTranLimitFuel = 0.0 ! fuel up transient limit kg/s
    REAL                    :: DownTranLimitFuel = 0.0 ! fuel down transient limit kg/s

    LOGICAL                      :: WarmUpByTimeDelay = .false. ! Warm up mode control
    LOGICAL                      :: WarmUpByEngineTemp = .true. ! Warm up mode control
    REAL                    :: StartUpTimeDelay   = 0.0 !time for start up [hours]
    REAL                    :: WarmUpDelay   = 0.0 ! time for warm up delay [s]

    REAL                    :: StartUpFuel   = 0.0 !fuel use during start up
    REAL                    :: StartUpElectConsum  = 0.0 !electricity used during start up
    REAL                    :: StartUpElectProd  = 0.0 !electricity produced during start up

    REAL                    :: ShutDownFuel  = 0.0 ! fuel consumed during shut down
    REAL                    :: ShutDownElectConsum = 0.0 !Elect consumed during shut down
    REAL                    :: PcoolDown = 0.0 !power during cool down
    REAL                    :: CoolDownDelay = 0.0  ! time for cool down delay [hours]

    INTEGER                      :: NumCyclesInit = 0  ! number of start stop cycles at beginning
    REAL                    :: NumRunHoursInit = 0.0 ! number of hours of operation beginning

    REAL                    :: Pstandby = 0.0  ! standby power [w]
    REAL                    :: MCeng = 0.0 ! aggregated thermal mass of engine [  ]
    REAL                    :: MCcw  = 0.0 ! aggregated thermal mass of heat recovery [   ]
    REAL                    :: kf = 0.0 ! coefficient k_f for warmup fuel flow rate
    REAL                    :: TnomEngOp = 0.0 ! nominal engine operating temperature [C]
    REAL                    :: kp = 0.0 ! coefficient k_p for warmup power

    LOGICAL                      :: MandatoryFullCoolDown = .false. !
    LOGICAL                      :: WarmRestartOkay       = .true.
    INTEGER                      :: AvailabilitySchedID   = 0

   !Calculated values and input from elsewhere
    INTEGER                       :: CurrentOpMode  = OpModeOFF ! current operating mode, uses params like OpModeNormal
    INTEGER                       :: LastOpMode     = OpModeOFF !
    REAL                     :: FractionalDayofLastShutDown = 0.0
    REAL                     :: FractionalDayofLastStartUp = 0.0
    LOGICAL                       :: HasBeenOn = .false.
    LOGICAL                       :: DuringStartUp = .false.
    LOGICAL                       :: DuringShutDown = .false.
    REAL                     :: FuelMdotLastTimestep  = 0.0
    REAL                     :: PelLastTimeStep = 0.0
    INTEGER                       :: NumCycles =0
    REAL                     :: PLRforSubtimestepStartUp = 0.0
    REAL                     :: PLRforSubtimestepShutDown = 0.0
    REAL                     :: ElectEffNom = 0.0 ! efficiency to use for control decisions
    REAL                     :: ThermEffNom = 0.0 ! thermal efficiency to use fo control decisions
    REAL                     :: QdotHXMax = 0.0 ! Thermal power max
    REAL                     :: QdotHXMin = 0.0 ! thermal power min
    REAL                     :: QdotHXOpt = 0.0 ! thermal power nominal/optimal
END TYPE !GeneratorDynamicsManagerStruct

Type MicroCHPParamsNonNormalized
    !user parameters
    CHARACTER(len=MaxNameLength) :: Name     = ' '  !name of this PowerModule data
    REAL                    :: MaxElecPower = 0.0 ! net electric power [W]
    REAL                    :: MinElecPower = 0.0 ! net electric power [W]
    REAL                    :: MinWaterMdot = 0.0 ! minimum cooling water flow [kg/s]
    REAL                    :: MaxWaterTemp = 0.0 ! limit temp for inlet cooling water [C]
    INTEGER                      :: ElecEffCurveID = 0 ! index for TriQuadratic for electrical efficiency
    INTEGER                      :: ThermalEffCurveID = 0 !index for TriQuadric for thermal efficiency
    LOGICAL                      :: InternalFlowControl = .false. ! Plant or Internal Flow rate control?
    LOGICAL                      :: PlantFlowControl = .true.  ! default is plant control
    INTEGER                      :: WaterFlowCurveID =0 ! index for BiQuadratic for water flow rate internal control
    INTEGER                      :: AirFlowCurveID =0 ! index for Quadratic for generator air flow
    REAL                    :: DeltaPelMax =0.0 ! max rate of change in net electric power [W/s}
    REAL                    :: DeltaFuelMdotMax = 0.0 !Maximum Rate of change in fuel flow rate [kmol/s2]
    REAL                    :: UAhx = 0.0 ! heat exchanger UA [W/K]
    REAL                    :: UAskin = 0.0 ! skin loss UA [W/K]
    REAL                    :: RadiativeFraction =0.0 ! skin loss fraction to radiant energy []
    REAL                    :: MCeng = 0.0 ! aggregated thermal mass of engine [J/K]
    REAL                    :: MCcw  = 0.0 ! aggregated thermal mass of heat recovery [J/k]
    REAL                    :: Pstandby = 0.0  ! standby power [w]
    LOGICAL                      :: WarmUpByTimeDelay = .false. ! Warm up mode control
    LOGICAL                      :: WarmUpByEngineTemp = .true. ! Warm up mode control
    REAL                    :: kf = 0.0 ! coefficient k_f for warmup fuel flow rate
    REAL                    :: TnomEngOp = 0.0 ! nominal engine operating temperature [C]
    REAL                    :: kp = 0.0 ! coefficient k_p for warmup power
    REAL                    :: Rfuelwarmup = 0.0 ! Warm Up Fuel Flow Rate Limit Ratio
    REAL                    :: WarmUpDelay = 0.0 ! time for warm up delay [s]
    REAL                    :: PcoolDown = 0.0 !power during cool down
    REAL                    :: CoolDownDelay = 0.0  ! time for cool down delay [s]
    LOGICAL                      :: MandatoryFullCoolDown = .false. !
    LOGICAL                      :: WarmRestartOkay       = .true.
    ! calculated and from elsewhere
    REAL                    :: TimeElapsed = 0.0     ! Fraction of the current hour that has elapsed (h)
                                                          ! Saved in order to identify the beginning of a new system time
    INTEGER                      :: opMode = 0
    REAL    :: OffModeTime      = 0.0 ! amount of time generator spent in Off mode
    REAL    :: StandyByModeTime = 0.0 ! amount of time generator spent in standby mode
    REAL    :: WarmUpModeTime   = 0.0 ! amount of time generator spent in warm up mode
    REAL    :: NormalModeTime   = 0.0 ! amount of time generator spent in normal mode
    REAL    :: CoolDownModeTime = 0.0 ! amount of time generator spent in Cool down mode
    REAL                    :: TengLast = 20.0  ! last timestep's value for engine temperature
    REAL                    :: TempCWOutLast = 20.0  !  last timestep's value for cooling water outlet temperature
    REAL                    :: Pnet = 0.0
    REAL                    :: ElecEff = 0.0
    REAL                    :: Qgross = 0.0
    REAL                    :: ThermEff = 0.0
    REAL                    :: Qgenss = 0.0
    REAL                    :: NdotFuel = 0.0
    REAL                    :: MdotFuel = 0.0
    REAL                    :: Teng = 20.0
    REAL                    :: Tcwin = 20.0
    REAL                    :: Tcwout = 20.0
    REAL                    :: MdotAir = 0.0
    REAL                    :: QdotSkin = 0.0 ! rate of heat loss to zone
    REAL                    :: QdotConvZone = 0.0
    REAL                    :: QdotRadZone= 0.0
END TYPE

TYPE MicroCHPReportDataStruct  !these are all for reporting only!
       INTEGER :: Mode                       = 0 ! report operating mode (dev only, remove at end)
       REAL    :: OffModeTime                = 0.0 ! amount of time generator spent in Off mode
       REAL    :: StandyByModeTime           = 0.0 ! amount of time generator spent in standby mode
       REAL    :: WarmUpModeTime             = 0.0 ! amount of time generator spent in warm up mode
       REAL    :: NormalModeTime             = 0.0 ! amount of time generator spent in normal mode
       REAL    :: CoolDownModeTime           = 0.0 ! amount of time generator spent in Cool down mode

       REAL    :: ACPowerGen                 = 0.0 ! reporting: power (W)
       REAL    :: ACEnergyGen                = 0.0 ! reporting: energy (J)
       REAL    :: Qdotgross                  = 0.0 ! reporting: interim gross power (W)
       REAL    :: Qgenss                     = 0.0 ! reporting: net recovered heat rate steadystate(0)
       REAL    :: QdotHX                     = 0.0 ! reporting: rate of heat exchange from engine to coolant (W)
       REAL    :: QdotHR                     = 0.0 ! reporting: rate of heat recovered (W)
       REAL    :: Tengine                    = 0.0 ! reporting: engine mass temperature (C)

       REAL    :: TotalHeatEnergyRec         = 0.0 ! reporting: total heat recovered (J)
       REAL    :: ExhaustEnergyRec           = 0.0 ! reporting: exhaust gas heat recovered (J)
       REAL    :: FuelEnergyLHV              = 0.0 ! reporting: Fuel Energy used in Lower Heating Value(J)
       REAL    :: FuelEnergyUseRateLHV       = 0.0 ! reporting: Fuel Energy used in Lower Heating Value(W)
       REAL    :: FuelEnergyHHV              = 0.0 ! reporting: Fuel Energy used in Higher Heating Value(J)
       REAL    :: FuelEnergyUseRateHHV       = 0.0 ! reporting: Fuel Energy used in Higher Heating Value(W)
       REAL    :: HeatRecInletTemp           = 0.0 ! reporting: Heat Recovery Loop Inlet Temperature (C)
       REAL    :: HeatRecOutletTemp          = 0.0 ! reporting: Heat Recovery Loop Outlet Temperature (C)
       REAL    :: HeatRecMdot                = 0.0 ! reporting: Heat Recovery Loop Mass flow rate (kg/s)


       ! air supply and blower
       REAL    :: TairInlet         = 0.0 ! State point 1
       REAL    :: MdotAir           = 0.0 ! air flow in kmol/sec
        !fuel supply and compressor
       REAL    :: TfuelInlet     = 0.0 ! State point 2 [C]
       REAL    :: NdotFuel       = 0.0 ! fuel flow in [kmol/sec]
       REAL    :: MdotFuel       = 0.0 ! fuel flow in [kg/s]
       REAL    :: FuelCompressPower = 0.0 ! electrical power used by fuel supply compressor [W]
       REAL    :: FuelCompressEnergy= 0.0 ! electrical energy used by fuel supply compressor [J]
       REAL    :: FuelCompressSkinLoss = 0.0 !heat rate of losses.by fuel supply compressor [W]

       !heat exchanger for water to exhaust heat recovery
    !   REAL    :: qHX = 0.0 ! heat flow from gas stream to water [W]
    !   REAL    :: HXenergy = 0.0 !energy from gas stream to water [J]
    !   REAL    :: THXexh = 0.0 ! temperature of exhaust gases leaving heat exchanger.
    !   REAL    :: WaterVaporFractExh = 0.0 ! water vapor fraction in exhaust gas stream
                                           ! relative to water vapor entering HX  (NdotH20/Ndoaux-mix)


   !    INTEGER :: SeqSubstIterations = 0 ! number of iterations in SOFC loop
   !    INTEGER :: RegulaFalsiIterations = 0 ! number of iterations in Tproduct gas solving


       REAL    :: SkinLossPower   = 0.0 ! heat loss to surrounding zone [W]
       REAL    :: SkinLossEnergy  = 0.0 ! heat loss to surround zone [J]
       REAL    :: SkinLossConvect = 0.0 ! convective heat loss to zone [W]
       REAL    :: SkinLossRadiat  = 0.0 ! radiative heat loss to zone [W}

       REAL    :: ElectEfficiency  = 0.0
       REAL    :: ThermalEfficiency = 0.0
       REAL    :: OverallEfficiency = 0.0

END TYPE MicroCHPReportDataStruct

TYPE MicroCHPDataStruct
   ! user input data
    CHARACTER(len=MaxNameLength) :: Name                = ' '  !name of this Micro CHP Generator
    CHARACTER(len=MaxNameLength) :: ParamObjName        = '' !name of parameter object
    Type(MicroCHPParamsNonNormalized):: A42Model ! Nested parameter data structure
    LOGICAL                      :: ModelTypeAnnex42  = .true. ! normalized =  non-normalized?
    REAL                    :: NomEff   = 0.0 ! nominal efficiency
    CHARACTER(len=MaxNameLength) :: ZoneName = '' !
    INTEGER                      :: ZoneID = 0 !
    CHARACTER(len=MaxNameLength) :: PlantInletNodeName  = ''
    INTEGER                      :: PlantInletNodeID    = 0
    CHARACTER(len=MaxNameLength) :: PlantOutletNodeName = ''
    INTEGER                      :: PlantOutletNodeID   = 0
    REAL        :: PlantMassFlowRate   = 0.0  ! only if internal control
    REAL        :: PlantMassFlowRateMax = 0.0 ! hardware limit for node%massflowrateMax
    CHARACTER(len=MaxNameLength) :: AirInletNodeName    = ''
    INTEGER                      :: AirInletNodeID      = 0
    CHARACTER(len=MaxNameLength) :: AirOutletNodeName   = ''
    INTEGER                      :: AirOutletNodeID     = 0
    TYPE(MicroCHPReportDataStruct)::Report ! structure of report variables
    INTEGER                      :: FuelSupplyID        = 0 ! index for fuel supply data structure
    INTEGER                      :: DynamicsControlID   = 0 ! index in GeneratorDynamics data where control issues are handled
    INTEGER                      :: AvailabilitySchedID = 0 !index for availability schedule
    INTEGER                      :: CWLoopNum     = 0  ! cooling water plant loop index number
    INTEGER                      :: CWLoopSideNum = 0  ! cooling water plant loop side index
    INTEGER                      :: CWBranchNum   = 0  ! cooling water plant loop branch index
    INTEGER                      :: CWCompNum     = 0  ! cooling water plant loop component index
END TYPE !MicroCHP


          ! MODULE VARIABLE DECLARATIONS:
TYPE (FCDataStruct),    ALLOCATABLE, DIMENSION(:)  :: FuelCell  !dimension to number of machines
TYPE (GasPropertyDataStruct), ALLOCATABLE, DIMENSION(:) :: GasPhaseThermoChemistryData
TYPE (GeneratorFuelSupplyDataStruct), ALLOCATABLE, DIMENSION(:) :: FuelSupply !fuel supply (reused across various)
TYPE (MicroCHPDataStruct), ALLOCATABLE, DIMENSION(:)            :: MicroCHP
TYPE (MicroCHPParamsNonNormalized), Allocatable, Dimension(:)   :: MicroCHPParamInput !  Used during get input then put into nested
TYPE (GeneratorDynamicsManagerStruct) , Allocatable, Dimension(:)  :: GeneratorDynamics

INTEGER  :: NumFuelConstit=0
INTEGER  :: NumGeneratorFuelSups=0
INTEGER  :: NumFuelCellGenerators=0 ! number of SOFC Generators specified in input
INTEGER  :: NumMicroCHPs=0 !
INTEGER  :: NumMicroCHPParams  =0  ! number of parameter sets for micro chp
INTEGER  :: NumGensWDynamics =0 ! number of dynamics controls for generators

!     NOTICE
!
!     Copyright © 1996-2012 The Board of Trustees of the University of Illinois
!     and The Regents of the University of California through Ernest Orlando Lawrence
!     Berkeley National Laboratory.  All rights reserved.
!
!     Portions of the EnergyPlus software package have been developed and copyrighted
!     by other individuals, companies and institutions.  These portions have been
!     incorporated into the EnergyPlus software package under license.   For a complete
!     list of contributors, see "Notice" located in EnergyPlus.f90.
!
!     NOTICE: The U.S. Government is granted for itself and others acting on its
!     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
!     reproduce, prepare derivative works, and perform publicly and display publicly.
!     Beginning five (5) years after permission to assert copyright is granted,
!     subject to two possible five year renewals, the U.S. Government is granted for
!     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
!     worldwide license in this data to reproduce, prepare derivative works,
!     distribute copies to the public, perform publicly and display publicly, and to
!     permit others to do so.
!
!     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

END MODULE DataGenerators
