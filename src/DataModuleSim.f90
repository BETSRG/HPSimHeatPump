MODULE DataSimulation

!Conversion factors
REAL, PARAMETER :: UnitPwr = 0.2927     !(Btu/hr X Upower = KW)
REAL, PARAMETER :: UrefFlow = 0.4536    !(Lbm/hr X UrefFlow = kg/hr)
REAL, PARAMETER :: UairFlow = 0.472E-3  !(CFM X UairFlow = kg/s)
REAL, PARAMETER :: UnitArFlw = 0.0004719474 !(CFM X UnitArFlw = m^3/s)
REAL, PARAMETER :: Upressure = 6.895    !(psi X Upressure = kPa)
REAL, PARAMETER :: UairPres = 249.1   !(in-H2o X UairPres = Pa)
REAL, PARAMETER :: Umass = 0.4536       !(lbm X Umass = kg)
REAL, PARAMETER :: Ulength = 0.3048     !(ft X Ulength = m)
REAL, PARAMETER :: Uenthalpy = 2.326     !(Btu/lbm X Uenthalpy = kJ/kg)
REAL, PARAMETER :: Ucndct = 0.1442E-3   !(Btu-in/hr-ft2-F X Ucndct = kW/m-C)
REAL, PARAMETER :: UnitP=6.895 !Pressure unit conversion (psi * UnitP = kPa)
REAL, PARAMETER :: UnitH=2.326 !Enthalpy unit conversion (Btu/lbm * UnitH = kJ/kg)
REAL, PARAMETER :: UnitM=0.4536 !Mass unit conversion (lbm * UnitM = kg)
REAL, PARAMETER :: UnitL=0.3048 !Length unit conversion (ft * UnitL = m) 

!Coil types
INTEGER,PARAMETER :: CONDENSERCOIL  = 1
INTEGER,PARAMETER :: EVAPORATORCOIL = 2
INTEGER,PARAMETER :: HIGHSIDETUBE   = 3
INTEGER,PARAMETER :: LOWSIDETUBE    = 4
INTEGER,PARAMETER :: MCCONDENSER    = 5
INTEGER,PARAMETER :: MCEVAPORATOR   = 6

!System types
INTEGER,PARAMETER :: AIRCONDITIONER = 1
INTEGER,PARAMETER :: HEATPUMP       = 2
INTEGER,PARAMETER :: CONDENSERUNIT  = 3
INTEGER,PARAMETER :: REHEAT         = 4
INTEGER,PARAMETER :: EVAPORATORONLY = 5

!Calculation Mode
INTEGER,PARAMETER :: FIXEDORIFICESIM     = 1
INTEGER,PARAMETER :: ORIFICEANDTXVDESIGN = 2
INTEGER,PARAMETER :: FIXEDSUPERHEATSIM   = 3
INTEGER,PARAMETER :: TXVSIMULATION       = 4
INTEGER,PARAMETER :: CONDENSERUNITSIM    = 5
INTEGER,PARAMETER :: COILONLYSIM       = 6

INTEGER,PARAMETER :: MaxIter = 30 !Maximum number of iterations
!INTEGER,PARAMETER :: NumTimeSteps = 5 !Sankar added transient
!Frosting Simulation Parameters
LOGICAL :: FrostingPeriod=.TRUE.
LOGICAL :: DeFrostingPeriod=.FALSE.
LOGICAL :: DefrostInitiate
REAL :: CurSimTime=0.0
REAL :: PrevSimTime=0.0
REAL :: TimeInterval=0.0
!Defrost Parameters
REAL :: DefrostControlTemp
REAL :: DefrostSetPoint
REAL :: TimeInhibit
REAL :: DefrostInhibitTime = 2500
REAL :: LastDefrostInitTime
REAL,DIMENSION(2) :: AmbTemp
REAL,DIMENSION(2) :: InitiateTemp

INTEGER :: TimeStep=0

REAL CompIN(3)
REAL CompPAR(26)
REAL CompOUT(7)

!Condenser model passing parameters
REAL CondIN(9)
REAL CondPAR(62)
REAL CondOUT(29)

!Evaporator model passing parameters
REAL EvapIN(9)
REAL EvapPAR(54)
REAL EvapOUT(25) 

!Expansion device
INTEGER(2) ExpDevice !1=Orifice; 2=TXV; 3=Cap. Tube

!Short tube model passing parameters
REAL ShTbIN(5)
REAL ShTbPAR(5)
REAL ShTbOUT(7)

!Capillary tube passing parameters
REAL CapTubeIN(5)
REAL CapTubePAR(5)
REAL CapTubeOUT(7)

!TXV model passing parameters
REAL TxvIN(6)
REAL TxvPAR(7)
REAL TxvOUT(7)

!Accumulator passing parameters
REAL AccumIN(3)
REAL AccumPAR(10)
REAL AccumOUT(6)

!Filter Drier passing parameters
REAL FilterIN(1)
REAL FilterPAR(2)
REAL FilterOUT(1)

INTEGER IDCcoilType !Indoor coil coil type
INTEGER ODCcoilType !Outdoor coil coil type

REAL REFCHG								!Specified refrigerant charge, lbm
REAL CALCHG								!Calculated refrigerant charge, lbm
REAL CondLiqTubeLength		!Condenser liquid tube length, m
REAL CondVapTubeLength		!Condenser vapor tube length, m
REAL CondTwoPhaseTubeLength !Condenser two-phase tube length, m
REAL CondNumLiqTubes		!Number of liquid tubes in condenser
REAL EvapLiqTubeLength		!Evaporator liquid tube length, m
REAL EvapVapTubeLength		!Evaporaotr vapor tube length, m
REAL EvapTwoPhaseTubeLength !Evaporator two-phase tube length, m
REAL EvapNumLiqTubes		!Number of liquid tube length in evaporator
INTEGER IsChargeTuning    !Flag to indicate if charge tuning is performed
REAL ChargeCurveSlope     !Charge curve slope, kg/m
REAL ChargeCurveIntercept !Charge curve intercept, kg
REAL RefLiquidLength      !Liquid length at reference point, m

REAL BaroPressure   !Barometric pressure, kPa

REAL PwrODfan					!Outdoor fan power, W
REAL PwrIDfan					!Indoor fan power, W

REAL DPEVP						!Pressure drop in evaporator and suction line, psi

REAL XMaC						!Condenser inlet air flow rate, kg/s
REAL TAIC						!Condenser inlet DB temp. F
REAL RHIC						!Condenser inlet relative humidity
REAL XMaE						!Evaporator inlet air flow rate, kg/s
REAL TAIE						!Evaporator inlet DB temp. F
REAL RHIE						!Evaporator inlet relative humidity
REAL DTROC
REAL CFMcnd                  !Standard condenser CFM, m3/s
REAL CFMevp                  !Standard evaperator CFM, m3/s

CHARACTER*80 Ref$	!Refrigerant name 
CHARACTER*80 Rref   !Referance Refrigerant name

INTEGER(2) PureRef	!Pure refrigerant flag: 1=Pure; 0=mixture
INTEGER(2) PureRref !Referance refrigerant flag: 1-pure; 0-mixture

!Compressor model passing parameters
REAL TSICMP								!Compressor inlet saturation temperature, F
REAL TSOCMP								!Compressor outlet saturation temperature, F 
REAL SUPER								!Superheat (F) or quality
REAL SUPERE
REAL SUBCOOL							!Subcooling, F

REAL AMBCON						!Convergence criterion for ambient temp.
REAL CNDCON						!Convergence criterion for condenser subcooling
REAL EVPCON						!Convergence criterion for evaporator superheat
REAL FLOCON						!Convergence criterion for flow rate
Logical CoarseConvergenceCriteriaMet  !Flag to refine convergence criteria for final solution

CHARACTER*15  Punit
CHARACTER*15  Hunit
CHARACTER*15  Tunit
CHARACTER*15  DTunit
CHARACTER*15  MdotUnit
CHARACTER*15  MassUnit
CHARACTER*15  PwrUnit
CHARACTER*15  CapUnit
CHARACTER*15  EERunit
CHARACTER*15  SysUnit
CHARACTER*15  NoUnit
CHARACTER*15  Xunit
CHARACTER*15  Lunit
CHARACTER*15  MiniLunit

REAL TICMP,PICMP,HICMP,XICMP	!Compressor inlet temp. (F); pressure (psi); enthalpy (Btu/lbm); Quality
REAL TOCMP,POCMP,HOCMP,XOCMP	!Compressor outlet temp. (F); pressure (psi); enthalpy (Btu/lbm); Quality
REAL TICND,PICND,HICND,XICND	!Condenser inlet temp. (F); pressure (psi); enthalpy (Btu/lbm); Quality
REAL TOCND,POCND,HOCND,XOCND	!Condenser outlet temp. (F); pressure (psi); enthalpy (Btu/lbm); Quality
REAL TIEXP,PIEXP,HIEXP,XIEXP	!Exp.device inlet temp. (F); pressure (psi); enthalpy (Btu/lbm); Quality
REAL TOEXP,POEXP,HOEXP,XOEXP	!Exp.device outlet temp. (F); pressure (psi); enthalpy (Btu/lbm); Quality
REAL TIEVP,PIEVP,HIEVP,XIEVP	!Evaporator inlet temp. (F); pressure (psi); enthalpy (Btu/lbm); Quality
REAL TOEVP,POEVP,HOEVP,XOEVP	!Evaporator outlet temp. (F); pressure (psi); enthalpy (Btu/lbm); Quality

INTEGER(2)	:: Unit !Unit flag: 1=SI; 2=IP

INTEGER		:: MODE !1-Design mode: Given superheat, subcooling; Compute charge, orifice size   
					!2-Simulation mode: Given charge, orifice size; Compute superheat, subcooling
					!3-Design mode: Given expansion device, superheat; Compute charge, subcooling
					!4-Simulation mode: Given superheat, charge; Compute subcooling, expansion deivce

LOGICAL FirstTimeAirTempLoop	!First time to run AirTempLoop?
LOGICAL FirstTimeFlowRateLoop   !First time to run FlowRateLoop?
LOGICAL FirstTimeHPdesignMode	!First time to run HPdesignMode?
LOGICAL FirstTimeChargeLoop     !First time to run ChargeLoop?
INTEGER(2) :: SystemType !1=A/C; 2=Heat Pump; 3=Condenser Unit; 4=Reheat; 5=Evaporator Only

REAL RhoAiE  
REAL RhoAiC
REAL RhoAoC
REAL RhoAoE

REAL MdotR

REAL :: WeightSucLn      !Weight of suction line, kg or lbm
REAL :: WeightDisLn      !Weight of discharge line, kg or lbm  
REAL :: WeightLiqLn      !Weight of liquid line, kg or lbm
REAL :: WeightValveIDCLn !Weight of Valve to IDC line, kg or lbm
REAL :: WeightValveODCLn !Weight of Valve to ODC line, kg or lbm

REAL TimeStepCorrection         ! Added Sankar Transient Code

REAL Tdis !Discharge temperature, C
REAL Tliq !Liquid temperature, C

REAL DTDISLN					!Discharge line temperature change, F
REAL DTSUCLN					!Suction line temperature change, F
REAL DTLIQLN					!Liquid line temperature change, F
REAL QDISLN						!Discharge line heat loss, Btu/hr
REAL QSUCLN						!Suction line heat gain, Btu/hr
REAL QLIQLN						!Liquid line heat loss, Btu/hr

INTEGER(2) :: CoolHeatModeFlag = -1

TYPE FrostParameters
 REAL :: Density = 0.0
 REAL :: Conductivity = 0.0
 REAL :: Thickness = 0.0
END TYPE FrostParameters

TYPE CoilParameters
 REAL CoilFaceArea
 REAL CoilFinArea
 REAL CoilTubeArea
 REAL CoilFreeFlowArea
 REAL CoilAirSideArea
 REAL CoilInletRefTemp
 REAL CoilOutletRefTemp
 REAL CoilInletAirTemp
 REAL CoilOutletAirTemp
 REAL DPair
 REAL DPRef
 REAL AirFlowRate
 REAL FinPitch
 REAL FinThickness
 REAL TSurfCoil
END TYPE CoilParameters

TYPE(FrostParameters) :: FrostParam
TYPE(CoilParameters), DIMENSION(2):: CoilParams

REAL EvapTubeArea
REAL EvapFinArea
REAL EvapTotArea
REAL EvapBareArea
REAL EvapMinArea
REAL CondTubeArea
REAL CondFinArea

END MODULE DataSimulation
