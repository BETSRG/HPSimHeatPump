MODULE CondenserMod

USE CoilCalcMod

IMPLICIT NONE

PRIVATE 

!Parameters
INTEGER,PARAMETER  :: MdotMaxIter=10    !Max. number of iterations for mass flow rate calc.
INTEGER,PARAMETER  :: RefBCmaxIter=20   !Max. number of iterations for refrigerant boundary condition calc.
INTEGER,PARAMETER  :: AirBCmaxIter=20   !Max. number of iterations for air boundary condition calc.
!INTEGER,PARAMETER  :: MaxIter=20        !Max. number of iterations
INTEGER,PARAMETER  :: PressureMaxIter=20        !Max. number of iterations
INTEGER,PARAMETER  :: DPmaxIter=20      !Max. number of iterations for distributor pressure drop calc.
!REAL,PARAMETER :: Ptol=0.05 !0.05 !0.01 !Pressure tolerance, kPa
REAL,PARAMETER :: SMALL=1.0E-4  !Small number 
REAL,PARAMETER :: BIG=1.0E20    !Big number
REAL, PARAMETER :: Hout = 0.009937536 !Bare tube outside film heat transfer coefficient, kW/(m2-K)

!Error Flags
INTEGER,PARAMETER :: NOERROR       = 0
INTEGER,PARAMETER :: CONVERGEERROR = 1
INTEGER,PARAMETER :: REFPROPERROR  = 2
INTEGER,PARAMETER :: CKTFILEERROR  = 3
INTEGER,PARAMETER :: COILTUBEERROR = 4
INTEGER,PARAMETER :: COILFINERROR  = 5
INTEGER,PARAMETER :: AIRSIDEERROR  = 6
INTEGER,PARAMETER :: ZEROLENCOILERROR  = 7
INTEGER,PARAMETER :: DPERROR       = 8

!System Types ISI - 07/13/06
!INTEGER,PARAMETER :: ACUNIT        = 1
!INTEGER,PARAMETER :: HEATPUMPUNIT  = 2
!INTEGER,PARAMETER :: CONDENSERUNIT = 3
!INTEGER,PARAMETER :: REHEATUNIT    = 4

!Fan locations
INTEGER,PARAMETER :: DRAWTHROUGH = 1
INTEGER,PARAMETER :: BLOWTHROUGH = 2

!Coil orientations
INTEGER,PARAMETER :: HORIZONTAL = 1
INTEGER,PARAMETER :: VERTICAL   = 2

REAL,ALLOCATABLE,DIMENSION(:),SAVE :: mRefIter !Circuit flow rate for iteration check, kg/s
INTEGER,ALLOCATABLE,DIMENSION(:),SAVE :: JoinTubes         !Joined tube numbers

!Subcooling cirucits variables, ISI - 06/05/07
INTEGER :: SubcoolingTube !Subcooling tube number
INTEGER :: NumOfSCckts !Number of subcooling circuits

!Circuitry variables
INTEGER Ickt,I,J,K,II,III,IV !Loop control
INTEGER NumOfTubes !Total number of tubes
INTEGER TubeNum    !Tube number

INTEGER ErrorFlag          !0-No error
                           !1-Condenser solution not converge
						   !2-Refprop error
                           !3-Circuit file error
						   !4,5-Coil size misdefined


INTEGER FirstTube           !First simulation tube
INTEGER LastTube            !Last simulation tube
INTEGER EqCircuits          !1=Equivalent circuits; otherwise=no
LOGICAL,SAVE :: IsUniformVelProfile !Is velocity profile uniform?


!Refprop Table variable
!CHARACTER (len=15) :: Property           
INTEGER            :: RefrigIndex =0
CHARACTER*80 :: RefName
INTEGER :: RefID !1-R22; 2-R410A; 3-R407C; 4-R134a; 5-Propane; 6-R417A; 7-R509A
REAL Temperature,Quality,Pressure,Enthalpy

!Nomenclature:
!R-Refrigerant;          A-Air;
!i-Inlet;                o-Outlet;
!f-Liquid phase;         g-Vapor phase;
!t-Temperature(C);       p-Pressure(kPa);      x-Quaility; h-Enthalpy(kJ/kg);
!m-Mass flow rate(kg/s); rh-Relative humidity; v-Specific volume, m^3/kg;
!sat-saturation;		 prev-previous iteration value

!Variables for module
REAL tRiMod,tRoMod,tRmod
REAL pRiMod,pRoMod,pRoModprev
REAL xRmod,xRiMod,xRoMod
REAL hRiMod,hRoMod
REAL hfRiMod,hfRoMod
REAL hgRiMod,hgRoMod
REAL hfgRmod,hfgRiMod,hfgRoMod
REAL vgRmod,vgRiMod,vgRoMod,vgRsat
REAL vfRmod,vfRiMod,vfRoMod,vfRsat 
REAL vRiMod,vRoMod
REAL muRmod,muRiMod,muRoMod
REAL mugRmod,mugRiMod,mugRoMod
REAL mufRmod,mufRiMod,mufRoMod
REAL kRmod,kRiMod,kRoMod
REAL kfRmod,kfRiMod,kfRoMod
REAL kgRmod,kgRiMod,kgRoMod
REAL cpRmod,cpRiMod,cpRoMod
REAL cpfRmod,cpfRiMod,cpfRoMod
REAL cpgRmod,cpgRiMod,cpgRoMod
REAL mAiMod
REAL tAiMod,tAoMod,tAmod
REAL rhAiMod,rhAoMod
REAL wbAiMod,wbAoMod
REAL hAiMod,hAoMod

REAL DPmod !Pressure drop in module, kPa
REAL SigmaMod !Surface tension, N/m
REAL DTmod   !Temperature difference between saturated vapor and wall, C
REAL tRoCkt  !Circuit outlet temp. C
REAL pRiCkt  !Circuit Inlet pressure, kPa
REAL pRoCkt  !Circuit outlet pressure, kPa
REAL SumpRoCkt !Sum of outlet circuit outlet pressure, kPa
REAL hRoCkt  !Circuit outlet enthalpy, kJ/kg
REAL xRoCkt  !Circuit outlet quality
REAL tRoAVG  !Average of ref outlet temperature, C
REAL tSiSUM  !Sum of inlet surface temperature, C
REAL tSoSUM  !Sum of outlet surface temperature, C

!Compressor variables
REAL tRoCmp
REAL pRoCmp
REAL hRoCmp
REAL xRoCmp
REAL vRoCmp
REAL muRoCmp
REAL mufRoCmp
REAL mugRoCmp
REAL CpgRoCmp

!Expansion device variables
REAL tRiExp
REAL pRiExp
REAL hRiExp
REAL xRiExp
REAL vRiExp
REAL vfRiExp
REAL vgRiExp
REAL muRiExp
REAL mufRiExp
REAL mugRiExp

!Heat transfer calc. variables
REAL mRefTot  !Refrigerant mass flow rate, kg/s
REAL,SAVE :: mRefTotPrev !Refrigerant mass flow rate, kg/s from previous iteration
REAL mRefMod  !Module refrigerant mass flow rate, kg/s
REAL mRefJoin !Join tube total mass flow rate, kg/s
REAL Cmin     !Min. capacity rate, kW/C
REAL DT       !Temperature difference, C
REAL NTU      !Number of transfer unit
REAL Cratio   !Ratio of min to max capacity rate
REAL EPS      !Heat exchanger effectiveness
REAL Qcoil    !Total coil heat transfer, kW
REAL PrevQcoil !Previous value of total coil heat transfer, kW
REAL DiffQcoil !Difference of Qcoil in iteration, kW
REAL Qckt     !Circuit heat transfer, kW
REAL Qmod     !Module heat transfer, kW
REAL QmodPrev !Previous module heat transfer, kW
REAL QmodTP   !Heat transfer in two-phase region, kW 
REAL QmodSH   !Heat transfer in superheated region, kW 
REAL TsurfMod !Module surface temperature, C
REAL cAir     !Capcity rate of air, kW/C
REAL cRef     !Capcity rate of refrigerant, kW/C
REAL UA       !Overall heat transfer coefficient, kW/C
REAL hcRef    !Refrigernat film coefficent, W/m^2-C
REAL Rtube    !Thermal resistance of tube, K/W
REAL Rair     !Module air film resistance, K/W
REAL Rrefrig  !Module refrigerant film resistance, K/W
REAL hco      !Air side heat tranfer coefficient, kW/m2-K
REAL hci      !Refrigerant side heat tranfer coefficient, kW/m2-K
REAL hcoMod   !Air side heat tranfer coefficient, kW/m2-K
REAL hciMod   !Refrigerant side heat tranfer coefficient, kW/m2-K
REAL EFref    !Refrigerant side heat tranfer enhancement factor
REAL Velavg   !Average face velocity, m/s
REAL ReVap    !Module Reynolds number vapor
REAL ReLiq    !Module Reynolds number liquid
REAL Const    !A constant
REAL MolWeight !Molecular weight, kg/kmol
REAL sigmaf    !Surface tension, kg/kmol
REAL tSat      !Saturation temp., C
REAL hciMultiplier   !Multiplier for hci
REAL hcoMultiplier   !Multiplier for hco
REAL DPrefMultiplier !Multiplier for DPref
REAL DPairMultiplier !Multiplier for DPair 
REAL DPfric     !Frictional pressure drop, kPa
REAL DPgrav     !Gravitational pressure drop, kPa
REAL DPmom      !Momentum pressure drop, kPa
REAL FaceVel    !Face velocity, m/s
REAL DPair      !Air side pressure drop, kPa
REAL SurfAbsorptivity !Surface absorptivity
REAL SolarFlux  !Solar heat flux, kW/m2
!REAL BaroPressure !Barometric pressure, kPa
REAL QlossCmp   !Compressor heat loss, kW
REAL IsCmpInAirStream !Is compressor in air stream, 1=yes, 0=no
!INTEGER(2) SystemType !1=A/C, 2=Heat Pump, 3=Condenser Unit, 4=Reheat !ISI - 07/14/06
INTEGER,SAVE :: CompManufacturer !Compressor manufacturer: 1=Copeland
                                                          !2=Bristol
														  !3=Danfoss
														  !4=Panasonic

!REAL QdisLn   !Discharge line heat loss, kW
!REAL DTdisLn  !Temperature change in discharge line, C
REAL AddDPDisLn !Discharge line additional pressure drop, kPa

REAL QrevCoilLn   !Reversing valve to coil line heat loss, kW
REAL DTrevCoilLn  !Temperature change in Reversing valve to coil line, C
REAL AddDPrevCoilLn !Reversing valve to coil line additional pressure drop, kPa

!REAL QliqLn   !Liquid line heat loss, kW
!REAL DTliqLn  !Temperature change in liquid line, C
REAL AddDPLiqLn !Liquid line additional pressure drop, kPa

!Properties
REAL mu       !Bulk viscosity, Pa-s
REAL muf      !Liquid viscosity, Pa-s
REAL mug      !Vapor viscosity, Pa-s
REAL kRef     !Refrigerant bulk conductivity, kW/m-K
REAL cpRef    !Ref. specific heat, kJ/(kg-K)
REAL rhoRef   !Ref. density, kg/m3
REAL CPAir                !Specific heat of air, kJ/kg-K
REAL DensityIn   !Inlet air density, kg/m3
REAL DensityOut  !Outlet air density, kg/m3
REAL Wabsolute   !Asolute oil mass fraction  
INTEGER(2) AirPropOpt     !Air prop calc. option
INTEGER(2) AirPropErr     !Error flag:1-error; 0-no error
REAL AirProp(8)

!Variables for coil
REAL mRiCoil
REAL mAiCoil
REAL tRiCoil,tRoCoil
REAL pRiCoil,pRoCoil
REAL pRoCoilTemp !temporary value of pRoCoil !ISI - 07/14/06
REAL pRoCoilPrev !previous iteration value
REAL hRiCoil,hRoCoil
REAL xRiCoil,xRoCoil
REAL tAiCoil !,tAoCoil
REAL,SAVE :: tAoCoil
REAL tSiCoil,tSoCoil
REAL rhAiCoil,rhAoCoil
REAL wbAiCoil,wbAoCoil
REAL hAiCoil,hAoCoil
REAL vRiCoil,vRoCoil
REAL vfRiCoil,vfRoCoil
REAL vgRiCoil,vgRoCoil
REAL muRiCoil,muRoCoil
REAL mufRiCoil,mufRoCoil
REAL mugRiCoil,mugRoCoil
REAL tSCoCoil     !Coil outlet subcooling, C 
REAL tSCiExp      !Exp.device inlet subcooling, C 
REAL tRoEvp !Evaporator outlet temperature, C

!Geometry variables
REAL Aface       !Coil face area
REAL,SAVE :: AiCoil !Inside coil surface area, m^2
REAL,SAVE :: AoCoil !Outside coil surface area, m^2
REAL,SAVE :: AfCoil !Coil fin surface area, m^2
REAL,SAVE :: AmCoil !Coil tube mean surface area, m^2
REAL AoMod       !Module outside surface area
REAL AoDry       !Module dry outside surface area
REAL AoWet       !Module wet outside surface area
REAL AfMod       !Module fin surface area
REAL AiMod       !Module inside surface area
REAL AiModLiq    !Module inside surface area for liquid line
REAL AiModDis    !Module inside surface area for discharge line
REAL AbrMod      !Module bare tube outside surface area
REAL AmMod       !Module tube mean surface area    
REAL Lcoil       !Total tube length, m
REAL LmodTube    !Module length of tube, m
REAL,SAVE :: LmodTP !Two-phase module length, m
REAL LmodTPmin   !Minimum two-phase module length, m
REAL LmodTPmax   !Maximum two-phase module length, m
REAL LmodTPratio !Ratio of two-phase module length to total module length
REAL,SAVE :: LmodSH !Superheated module length, m
REAL LmodSHmin   !Minimum Superheated module length, m
REAL LmodSHmax   !Maximum Superheated module length, m
REAL LmodSHratio !Ratio of Superheated module length to total module length

REAL LmodDis     !Module length of discharge line, m
REAL LdisLn      !Reversing valve to coil line length, m
REAL ElevDisLn   !Discharge line elevation, m
REAL IDdisLn     !Inside diameter of discharge line, m
REAL ODdisLn     !Outside diameter of discharge line, m 
REAL DisLnThk    !Discharge line tube wall thickness, m

REAL LmodRevCoil !Module length of reversing valve to coil, m
REAL LrevCoil    !Reversing valve to coil line length, m
REAL ElevRevCoilLn   !Reversing valve to coil line elevation, m
REAL IDRevCoilLn     !Inside diameter of Reversing valve to coil line, m
REAL ODRevCoilLn     !Outside diameter of Reversing valve to coil line, m 
REAL RevCoilLnThk    !Reversing valve to coil line tube wall thickness, m

REAL LmodLiq     !Module length of liquid line, m
REAL LliqLn      !Liquid line length, m
REAL ElevLiqLn   !Liquid line elevation, m
REAL IDliqLn     !Inside diameter of liquid line, m
REAL ODliqLn     !Outside diameter of liquid line, m 
REAL LiqLnThk    !Liquid line tube wall thickness, m

REAL DreturnBend !Return bend diameter, m
REAL LreturnBend !Return bend length, m

REAL HtCoil      !Coil height, m
REAL FinSpg      !Fin spaing, m
REAL phi         !Parameter for fin efficiency calc.
REAL SurfEff     !Surface effecitiveness
REAL FinEff      !Fin effecitiveness
REAL,SAVE :: DisTubeLength !Distributor tube length !ISI - 07/14/06

INTEGER,SAVE :: FinType       !1=Plain; 2=Wavy; 3=Louver; 4-11-element
REAL,SAVE    :: FinPitch      !Fin pitch, fins/m
REAL,SAVE    :: Kfin          !Fin thermal conductivity, kW/m-K
REAL,SAVE    :: FinThk        !Fin thickness, m
REAL,SAVE    :: FinHeight     !Fin height, m
INTEGER,SAVE :: TubeType      !1=Plain; 2=General Micro Fin; 3=Herringbone; 4=Crosshatch; 5=Herringbone w/crosshatch; 6=Turbo-A
REAL,SAVE    :: TubeHeight    !Tube height, m
REAL,SAVE    :: TubeDepth     !Tube depth, m
REAL,SAVE    :: TubeThk       !Coil tube wall thickness, m
REAL,SAVE    :: Ktube         !Tube thermal conductivity, kW/m-K
REAL,SAVE    :: Pl            !Tube spacing in longitudinal direction, m
REAL,SAVE    :: Pt            !Tube spacing in lateral direction, m
INTEGER,SAVE :: Nl            !Number of tubes in longitudinal direction 
INTEGER,SAVE :: Nt            !Number of tubes in traverse direction
REAL,SAVE    :: ODtube        !Outside diameter of coil tube, m 
REAL,SAVE    :: IDtube        !Inside diameter of coil tube, m
REAL,SAVE    :: Ltube         !Tube length, m
INTEGER,SAVE :: TubeOrientation !Tube orientation, 1=Horizontal; 2=Vertical
INTEGER,SAVE :: NumOfMods	  !Number of modules per tube 
INTEGER,SAVE :: NumOfChannels !Number cf channels
REAL,SAVE    :: Dchannel      !Channel diameter, m
INTEGER,SAVE :: NumOfCkts     !Number of circuits
INTEGER,SAVE :: ShiftTube     !1= last row lower than 2nd last row
                              !0= last row higher than 2nd last row
INTEGER NmodLast      !Total number of modules in the last row
INTEGER NumOfSimCkts  !Number of simulation circuits
INTEGER IsCoolingMode !Cooling mode flag (1=yes; 0=no)
INTEGER IsParallelSlabs !Parallel microchannel slabs (1=yes; 0=no)
INTEGER RowNum        !Coil row number
INTEGER Ntube         !Tube number !Loop counter
INTEGER Nckt          !Circuit number !Loop counter
INTEGER Nmod          !Module number !Loop counter
INTEGER NcktLast      !Total number of outlet circuits 
INTEGER NcktFirst     !Total number of inlet circuits 
INTEGER Nnode         !Number of split and joint nodes
LOGICAL,SAVE :: IsSameNumOfTubes !Flag to check if same number of tubes
                                 !in all circuit branches
REAL DrawBlow  !Fan location, 1=draw through; 2=blow through
REAL PwrFan	   !Fan power, kW
INTEGER WetFlag            !1=Wet; 0=dry
INTEGER tAiEvp !Evaporator entering air temp., C

INTEGER(2)       :: RefPropErr  !Error flag:1-error; 0-no error
REAL Psat,Pcr,Dcrit,Tcr

!Mass inventory
REAL MassDisLn   !Total refrigerant inventory in discharge line, kg
REAL MassLiqLn   !Total refrigerant inventory in liquid line, kg
REAL MassMod     !Refrigerant inventory in a module, kg
REAL MassLiqMod  !Mass in liquid phase, kg
REAL MassVapMod  !Mass in vapor phase, kg

REAL, SAVE :: WeightAluminum !Weight of aluminum, kg
REAL, SAVE :: WeightCopper   !Weight of copper, kg

!Custom air side curve
INTEGER CurveUnit          !Unit convention of the custom air side curve, 1=SI; 2=IP
INTEGER CurveTypeHTC       !Curve fit type of the air side heat transfer coefficient
                           !1-Power fit; 2-Polynomial fit
REAL PowerAHTC !Power fit coefficient for air heat transfer coefficient
REAL PowerBHTC !Power fit coefficient for air heat transfer coefficient
REAL Poly1HTC  !Polynomial fit coefficient for air heat transfer coefficient
REAL Poly2HTC  !Polynomial fit coefficient for air heat transfer coefficient
REAL Poly3HTC  !Polynomial fit coefficient for air heat transfer coefficient
REAL Poly4HTC  !Polynomial fit coefficient for air heat transfer coefficient
INTEGER CurveTypeDP        !Curve fit type of the air side pressure drop
                           !1-Power fit; 2-Polynomial fit
REAL PowerADP  !Power fit coefficient for air pressure drop
REAL PowerBDP  !Power fit coefficient for air pressure drop
REAL Poly1DP   !Polynomial fit coefficient for air pressure drop
REAL Poly2DP   !Polynomial fit coefficient for air pressure drop
REAL Poly3DP   !Polynomial fit coefficient for air pressure drop
REAL Poly4Dp   !Polynomial fit coefficient for air pressure drop

!LOGICAL, SAVE :: FirstTime=.TRUE. !Flag to indicate the first time of execution
INTEGER FirstTime !Flag to indicate the first time of execution
                  !1=yes, otherwise=no
INTEGER Counter                   !Iteration loop counter

!LOGICAL,PARAMETER :: IsSimpleCoil !=.FALSE. !Flag to indicate if it is simple coil, i.e. ignoring circuiting
INTEGER IsSimpleCoil !Flag to indicate if it is simple coil, i.e. ignoring circuiting
                     !1=Simple coil
				     !otherwise=detailed
INTEGER NumOfSections !Number of sections, ISI - 09/10/07

TYPE (SlabInfo),ALLOCATABLE,DIMENSION(:),SAVE :: Slab      !Coil slab pointer
TYPE (CktInfo),ALLOCATABLE,DIMENSION(:),SAVE :: Ckt        !Circuit pointer
TYPE (TubeInfo),ALLOCATABLE,DIMENSION(:),SAVE :: Tube      !Tube pointer
TYPE (TubeInfo),ALLOCATABLE,DIMENSION(:,:),SAVE :: Tube2D  !2-dimensional Tube pointer
TYPE (ModInfo),ALLOCATABLE,DIMENSION(:),SAVE :: DisLnSeg   !Discharge line pointer
TYPE (ModInfo),ALLOCATABLE,DIMENSION(:),SAVE :: LiqLnSeg   !Liquid line pointer
TYPE (NodeInfo),ALLOCATABLE,DIMENSION(:),SAVE :: Node      !Split or joint node
TYPE (SectionInfo),ALLOCATABLE,DIMENSION(:) :: CoilSection !Coil section, ISI - 09/10/07

PUBLIC  Condenser
PUBLIC  MicrochannelCondenser
PUBLIC  CalcCondenserInventory
PUBLIC  PrintCondenserResult
PUBLIC  EndCondenserCoil
PRIVATE InitCondenserCoil
PRIVATE RefrigerantParameters
PRIVATE LoadMicrochannelInputs
PRIVATE LoadMicrochannelOutputs
PRIVATE DischargeLine
PRIVATE LiquidLine
PRIVATE InitBoundaryConditions
PRIVATE CalcCircuitRefInletConditions
PRIVATE CalcSegmentRefInletConditions
PRIVATE CalcSegmentAirInletConditions
PRIVATE CalcCoilSegment
PRIVATE CalcSegmentOutletConditions
PRIVATE CalcTransitionSegment
PRIVATE FindTransitionBoundary
PRIVATE CalcRefProperty
PRIVATE CalcSegmentRefOutletPressure
PRIVATE UpdateTubeDataFromCircuitData

CONTAINS

!***********************************************************************************

    SUBROUTINE Condenser(Ref$,PureRef,XIN,PAR,OUT)

    !-----------------------------------------------------------------------------------
    !
    !  Description:	
    !  Ragazzi's modular coil model (Fixed length version)
    !  To predict coil air side and refrigerant side properties, heat transfer, 
    !  and prssure drop
    !
    !  Inputs:
    !  Ref$=Refrigerant name
    !  PureRef=Refrigerant flag: 1=pure refrigerant
    !                            0=refrigerant mixture
    !  XIN(1)=Refrigerant side mass flow rate, kg/s
    !  XIN(2)=Refrigerant side inlet (compressor outlet) pressure, kPa
    !  XIN(3)=Refrigerant side inlet (compressor outlet) enthalpy, kJ/kg
    !  XIN(4)=Air side mass flow rate, kg/s
    !  XIN(5)=Air side inlet temp. C
    !  XIN(6)=Air side inlet relative humidity
    !  XIN(7)=Solar heat flux, kW/m^2
    !  XIN(8)=Evaporator outlet temperature, C
    !  XIN(9)=Evaporator entering air temperature, C
    !
    !  Parameters:
    !  PAR(1)=Discharge line length, m
    !  PAR(2)=Discharge line outside diameter, m
    !  PAR(3)=Discharge line tube wall thickness, m
    !  PAR(4)=Discharge line elevation, m
    !  PAR(5)=Discharge line heat loss, kW
    !  PAR(6)=Discharge line temperature change, C
    !  PAR(7)=Discharge line additional pressure drop, kPa
    !  PAR(8)=Liquid line length, m
    !  PAR(9)=Liquid line outside diameter, m
    !  PAR(10)=Liquid line tube wall thickness, m 
    !  PAR(11)=Liquid line elevation, m
    !  PAR(12)=Liquid line heat loss, kW
    !  PAR(13)=Liquid line temperature change, C
    !  PAR(14)=Liquid line additional pressure drop, kPa
    !  PAR(15)=Coil tube outside diameter, m
    !  PAR(16)=Coil tube wall thickness, m
    !  PAR(17)=Coil single tube length, m
    !  PAR(18)=Coil tube thermal conductivity, kW/m-C
    !  PAR(19)=Tube spacing in transverse direction, m (normal to air flow)
    !  PAR(20)=Row spacing in longitudinal direction, m (parallel to air flow)
    !  PAR(21)=Fin thickness, m
    !  PAR(22)=Fin pitch, fin/m
    !  PAR(23)=Fin thermal conductivity, kW/m-C
    !  PAR(24)=Number of tubes in transverse direction (normal to air flow)
    !  PAR(25)=Number of rows in longitudinal direction (parallel to air flow)
    !  PAR(26)=Number of circuits
    !  PAR(27)=Cooling mode? 1=yes; 0=no
    !  PAR(28)=Number of modules per tube
    !  PAR(29)=Fin type: 1=Plain; 2=Wavy; 3=Louver
    !  PAR(30)=Multiplier for ref. side heat transfer correlation
    !  PAR(31)=Multiplier for ref. side pressure drop correlation
    !  PAR(32)=Multiplier for air side heat transfer correlation
    !  PAR(33)=Multiplier for air side pressure drop correlation
    !  PAR(34)=Fan power, kW
    !  PAR(35)=Fan location, 1=draw through; 2=blow through
    !  PAR(36)=Surface absorptivity
    !  PAR(37)=Tube tube: 1-Smooth; 2-Microfin; 3=Herringbone; 4=Crosshatch; 
    !                     5=Herringbone w/crosshatch; 6=Rifle; 7=Helical;
    !                     8=42F HXH
    !  PAR(38)=Barometric pressure, kPa
    !  PAR(39)=Compressor heat loss, kW
    !  PAR(40)=Is compressor in air stream, 1=yes, 0=no
    !  PAR(41)=Custom air side data unit, 1=SI; 2=IP
    !  PAR(42)=Custom air heat transfer curve type, 1=Power; 2=Polynomial
    !  PAR(43)=Power coefficient for air heat transfer curve
    !  PAR(44)=Power coefficient for air heat transfer curve
    !  PAR(45)=Polynomial coefficient for air heat transfer curve
    !  PAR(46)=Polynomial coefficient for air heat transfer curve
    !  PAR(47)=Polynomial coefficient for air heat transfer curve
    !  PAR(48)=Polynomial coefficient for air heat transfer curve
    !  PAR(49)=Custom air heat transfer curve type, 1=Power; 2=Polynomial
    !  PAR(50)=Power coefficient for air heat transfer curve
    !  PAR(51)=Power coefficient for air heat transfer curve
    !  PAR(52)=Polynomial coefficient for air heat transfer curve
    !  PAR(53)=Polynomial coefficient for air heat transfer curve
    !  PAR(54)=Polynomial coefficient for air heat transfer curve
    !  PAR(55)=Polynomial coefficient for air heat transfer curve
    !  PAR(56)=Pressure tolerance convergence Criteria, kPa
    !  PAR(57)=System type !1=A/C, 2=Heat Pump, 3=Condenser Unit, 4=Reheat
    !  PAR(58)=Distributor tube length, m
    !  PAR(59)=Oil mass fraction
    !  PAR(60)=Compressor manufacturer: 1=Copeland; 2=Bristol; 
    !                                   3=Danfoss;  4=Panasonic
    !  PAR(61)=Simple coil flag: 1=Simple coil; otherwise=Detailed coil
    !  PAR(62)=First time to run this model flag: 1=yes, otherwise=no
    !          for component validation, set it to 1
    !          for system validation, set it to 1 first, then zero
    !
    !  Outputs:
    !  OUT(1)=Coil inlet pressure, kPa
    !  OUT(2)=Coil inlet enthalpy, kJ/kg
    !  OUT(3)=Coil inlet temperature, C
    !  OUT(4)=Coil inlet quality
    !  OUT(5)=Coil outlet pressure, kPa
    !  OUT(6)=Coil outlet enthalpy, kJ/kg
    !  OUT(7)=Coil outlet temperature, C
    !  OUT(8)=Coil outlet quality
    !  OUT(9)=Coil outlet subcooling, C
    !  OUT(10)=Liquid line outlet pressure, kPa
    !  OUT(11)=Liquid line outlet enthalpy, kJ/kg
    !  OUT(12)=Liquid line outlet temperature, C
    !  OUT(13)=Liquid line outlet quality
    !  OUT(14)=Liquid line outlet subcooling, C
    !  OUT(15)=Coil capacity, kW
    !  OUT(16)=Mass in discharge line, kg
    !  OUT(17)=Mass in liquid line, kg
    !  OUT(18)=Mass in coil, kg
    !  OUT(19)=Liquid mass in coil, kg
    !  OUT(20)=Vapor mass in coil, kg
    !  OUT(21)=Air side outlet temperature, C
    !  OUT(22)=Air side outlet relative humidity
    !  OUT(23)=Air side pressure drop, kPa
    !  OUT(24)=Error flag: 0-No error
    !                      1-Condenser solution not converge
    !                      2-Refprop error
    !                      3-Circuit file error
    !  OUT(25)=Air side heat transfer coefficients, kW/m^2-K
    !  OUT(26)=Inlet coil surface temperature, C
    !  OUT(27)=Outlet coil surface temperature, C
    !  OUT(28)=Aluminum weight, kg 
    !  OUT(29)=Copper weight, kg
    !
    !
    !  Reference: 
    !  Ragazzi, F. and Pedersen, C.O. (1991). Modular-based computer simulation
    !    of an air-cooled condenser. ACRC technical report 07.
    !    University of Illinois, Urbana,IL.
    !
    !  Author:
    !  Ipseng Iu
    !  Mechanical and Aerospace Engineering
    !  Oklahoma State University, Stillwater	
    !
    !  Date: June 2002
    !
    !-----------------------------------------------------------------------------------

    USE FluidProperties
    USE CoilCalcMod
    USE AirPropMod
    USE OilMixtureMod

    IMPLICIT NONE

    !Subroutine argument declarations
    CHARACTER*80,     INTENT(IN)  :: Ref$
    INTEGER(2),       INTENT(IN)  :: PureRef
    REAL, INTENT(IN)  :: XIN(9)
    !REAL, INTENT(IN)  :: PAR(56)
    !REAL, INTENT(IN)  :: PAR(60) !ISI - 07/14/06
    REAL, INTENT(IN)  :: PAR(62) !ISI - 12/21/06
    REAL, INTENT(OUT) :: OUT(29)

    !Subroutine lcoal variables
    REAL :: MCXIN(7)  !Microchannel coil input data
    REAL :: MCPAR(39) !Microchannel coil input parameters
    REAL :: MCOUT(22) !Microchannel coil output data

    INTEGER,SAVE :: CoilType  !1=Condenser; 2=Evaporator; 
    !3=High side interconnecting pipes; 
    !4=Low side interconnecting pipes
    !5=Microchannel condenser
    !6=Microchannel evaporator

    INTEGER Iter                  !Iteration loop counter
    INTEGER AirBCiter             !Iteration loop counter
    LOGICAL Converged             !Solution convergence flag
    REAL MaxResidual !Maximum residual in iteration
    REAL PTol !Condenser Outlet Pressure Convergence criteria, kPa

    !ISI - 07/14/06
    REAL pRiExpPrev  !Previous iteration value of pRiExp
    REAL hRiExpPrev  !Previous iteration value of hRiExp
    REAL Qdistube    !Distributor capacity, kW, not being used 
    REAL DPdisTube   !Distributor pressure drop, kPa
    REAL DPdisTubePrev !Previous value of DPdisTube, kPa
    REAL hRiCoilRtd  !hRiCoil at rating condition
    REAL Wlocal !Local oil mass fraction

    !Flow:

    mRefTot =XIN(1)
    pRoCmp  =XIN(2)
    hRoCmp  =XIN(3)
    mAiCoil =XIN(4)
    tAiCoil =XIN(5)
    rhAiCoil=XIN(6)
    SolarFlux=XIN(7)
    tRoEvp=XIN(8)
    tAiEvp=XIN(9)

    LdisLn    = PAR(1)
    ODdisLn   = PAR(2)
    DisLnThk  = PAR(3)
    ElevDisLn = PAR(4)
    QdisLn    = PAR(5)
    DTdisLn   = PAR(6)
    AddDPdisLn = PAR(7)

    LliqLn    = PAR(8)
    ODliqLn   = PAR(9)
    LiqLnThk  = PAR(10)
    ElevLiqLn = PAR(11)
    QliqLn    = PAR(12)
    DTliqLn   = PAR(13)
    AddDPLiqLn = PAR(14)

    IsCoolingMode   = PAR(27)

    IsSimpleCoil=PAR(61) !ISI - 12/22/06
    FirstTime=PAR(62)    !ISI - 12/22/06

    !Initialize circuiting and refrigerant parameters
    IF (FirstTime .EQ. 1) THEN
        ODtube      = PAR(15)
        TubeThk     = PAR(16)
        Ltube       = PAR(17)
        Ktube       = PAR(18)
        Pt          = PAR(19)
        Pl          = PAR(20)
        FinThk      = PAR(21)
        FinPitch    = PAR(22)
        Kfin        = PAR(23)
        Nt          = PAR(24)
        Nl          = PAR(25)
        NumOfCkts   = PAR(26)
        NumOfMods   = PAR(28)
        FinType     = PAR(29)
        TubeType    = PAR(37)
        CALL InitCondenserCoil(CoilType)
        CALL CalcMaterialWeight(CoilType,Ltube,IDtube,ODtube,TubeHeight,TubeDepth, &
        Dchannel,NumOfChannels,Pt,Pl,Nt,Nl,NumOfCkts, &
        FinThk,FinPitch,WeightAluminum,WeightCopper)
        IF (ErrorFlag .NE. NOERROR) THEN
            !VL: Previously: GOTO 200
            OUT(24)=ErrorFlag
            CALL Condenser_Helper_1
            RETURN
        END IF
        CALL RefrigerantParameters(Ref$)
        CALL GetRefID(Ref$,RefID)
        !FirstTime=.FALSE. !ISI - 12/22/06
        tAoCoil=tAiCoil !ISI - 05/27/2008
    END IF

    hciMultiplier   = PAR(30)
    DPrefMultiplier = PAR(31)
    hcoMultiplier   = PAR(32)
    DPairMultiplier = PAR(33)

    PwrFan           = PAR(34)
    DrawBlow         = PAR(35)
    SurfAbsorptivity = PAR(36)

    BaroPressure     = PAR(38)
    QlossCmp         = PAR(39)
    IsCmpInAirStream = PAR(40)

    CurveUnit        = PAR(41)
    CurveTypeHTC     = PAR(42)
    PowerAHTC        = PAR(43)
    PowerBHTC        = PAR(44)
    Poly1HTC         = PAR(45)
    Poly2HTC         = PAR(46)
    Poly3HTC         = PAR(47)
    Poly4HTC         = PAR(48)
    CurveTypeDP      = PAR(49)
    PowerADP         = PAR(50)
    PowerBDP         = PAR(51)
    Poly1DP          = PAR(52)
    Poly2DP          = PAR(53)
    Poly3DP          = PAR(54)
    Poly4DP          = PAR(55)
    PTol			   = PAR(56)
    SystemType       = PAR(57) !ISI - 07/14/06
    DisTubeLength    = PAR(58) !ISI - 07/14/06
    Wabsolute        = PAR(59)
    CompManufacturer = PAR(60)

    IF (CoilType .EQ. MCCONDENSER) THEN
        CALL LoadMicrochannelInputs(XIN,PAR,MCXIN,MCPAR)
        CALL MicrochannelCondenser(Ref$,MCXIN,MCPAR,MCOUT)
        CALL LoadMicrochannelOutputs(MCOUT,OUT)
        RETURN
    END IF

    EqCircuits=0 !Equivalent circuit flag

    ErrorFlag=NOERROR !Initilaize

    !Tube inside diameter
    IDdisLn=ODdisLn-DisLnThk*2
    IDliqLn=ODliqLn-LiqLnThk*2

    !Return bend length
    Dreturnbend=Pt
    Lreturnbend=Dreturnbend*PI/2 

    !Total number of modules in the last row
    NmodLast=Nt*NumOfMods

    !Coil height
    HtCoil=Nt*Pt

    !Coil length
    Lcoil=Nl*Nt*Ltube

    !Face area
    Aface=Ltube*Nt*Pt

    !Tube information
    LmodTube=Ltube/NumOfMods

    !Discharge line info
    LmodDis=Ldisln
    !LmodDis=Ldisln/NumOfMods !ISI - 08/25/06
    AiModDis=PI*IDdisLn*LmodDis

    !liquid line info
    LmodLiq=Lliqln
    !LmodLiq=Lliqln/NumOfMods !ISI - 08/25/06
    AiModLiq=PI*IDliqLn*LmodLiq

    CALL InitBoundaryConditions(CoilType)
    IF (ErrorFlag .NE. NOERROR) THEN
        !VL: Previously: GOTO 200
        OUT(24)=ErrorFlag
        CALL Condenser_Helper_1
        RETURN
    END IF

    !****** Start coil calculation ******
    Converged=.TRUE.  
    pRoCoilPrev=pRiCoil
    MaxResidual=0

    DO Iter=1, MdotMaxIter

        !Initialize
        mRefJoin=0
        PrevQcoil=BIG
        QmodPrev=0

        DO AirBCiter=1, AirBCmaxIter

            Qcoil=0.0;  tSiSUM=0.0; tSoSUM=0.0; 

            DO I=1,NumOfCkts

                Qckt=0.0

                CALL CalcCircuitRefInletConditions(I,I,CoilType)

                !Find first and last simulation tubes
                IF (IsSimpleCoil .EQ. 1) THEN
                    FirstTube=1
                    LastTube=1
                ELSE
                    FirstTube=1
                    LastTube=Ckt(I)%Ntube
                    IF (Ckt(I)%InSplit .GT. 1) THEN
                        FirstTube=2 !Skip first tube
                    END IF 
                    IF (Ckt(I)%OutJoin .GT. 1) THEN
                        LastTube=Ckt(I)%Ntube-1 !Ignore last tube
                    END IF
                END IF

                mRefMod=Ckt(I)%mRef

                DO J=FirstTube,LastTube

                    IF (IsSimpleCoil .EQ. 1) THEN
                        TubeNum=1
                    ELSE
                        TubeNum=Ckt(I)%TubeSequence(J)
                    END IF

                    DO K=1,NumOfMods

                        IF (IsSimpleCoil .EQ. 1) THEN
                            SELECT CASE(K)
                            CASE (1)
                                LmodTube=Lcoil/NumOfCkts !Start with guessing the whole length
                            CASE (2)
                                LmodTube=Lcoil/NumOfCkts-Ckt(I)%Tube(J)%Seg(1)%Len
                            CASE (3)
                                LmodTube=Lcoil/NumOfCkts-(Ckt(I)%Tube(J)%Seg(1)%Len+Ckt(I)%Tube(J)%Seg(2)%Len)
                            END SELECT
                        END IF
                        CALL CalcCoilSegment(I,I,J,K,CoilType)
                        IF (ErrorFlag .GT. CONVERGEERROR) THEN
                            !VL: Previously: GOTO 200
                            OUT(24)=ErrorFlag
                            CALL Condenser_Helper_1
                            RETURN
                        END IF


                        !Calc. circuit heat transfer
                        Qckt=Qckt+Ckt(I)%Tube(J)%Seg(K)%Qmod

                        !Calc. sum of surface air temperature
                        IF (Ckt(I)%Tube(J)%Back .EQ. 0) THEN
                            tSoSUM=tSoSUM+Ckt(I)%Tube(J)%Seg(K)%tSo
                        END IF

                        IF (Ckt(I)%Tube(J)%Fup .EQ. 0 .AND. Ckt(I)%Tube(J)%Fdown .EQ. 0) THEN
                            tSiSUM=tSiSUM+Ckt(I)%Tube(J)%Seg(K)%tSi
                        END IF

                    END DO !End mod

                END DO !End tube

                pRoCkt=Ckt(I)%Tube(LastTube)%Seg(NumOfMods)%pRo !Circuit outlet pressure
                hRoCkt=Ckt(I)%Tube(LastTube)%Seg(NumOfMods)%hRo !Circuit outlet enthalpy

                Pressure=pRoCkt*1000
                Enthalpy=hRoCkt*1000
                tRoCkt=PH(RefName, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
                IF (RefPropErr .GT. 0) THEN
                    WRITE(*,*)'-- WARNING -- Condenser: Refprop error.'
                    ErrorFlag=REFPROPERROR
                    !VL: Previously: GOTO 200
                    OUT(24)=ErrorFlag
                    CALL Condenser_Helper_1
                    RETURN
                END IF
                xRoCkt=PH(RefName, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
                IF (RefPropErr .GT. 0) THEN
                    WRITE(*,*)'-- WARNING -- Condenser: Refprop error.'
                    ErrorFlag=REFPROPERROR
                    !VL: Previously: GOTO 200
                    OUT(24)=ErrorFlag
                    CALL Condenser_Helper_1
                    RETURN
                END IF

                Pressure=pRoCkt*1000
                Quality=0
                tSat=PQ(RefName, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)
                IF (RefPropErr .GT. 0) THEN
                    WRITE(*,*)'-- WARNING -- Condenser: Refprop error.'
                    ErrorFlag=REFPROPERROR
                    !VL: Previously: GOTO 200
                    OUT(24)=ErrorFlag
                    CALL Condenser_Helper_1
                    RETURN
                END IF

                IF (xRoCkt .LE. 0.0) THEN 
                    Ckt(I)%tSC=tSat-tRoCkt !Subcooling
                ELSE
                    Ckt(I)%tSC=0.0
                END IF

                Ckt(I)%tRo=tRoCkt
                Ckt(I)%pRo=pRoCkt
                Ckt(I)%hRo=hRoCkt
                Ckt(I)%Qckt=Qckt        !Circuit capcity
                Qcoil=Qcoil+Ckt(I)%Qckt !Total coil capacity

                IF (EqCircuits .EQ. 1 .AND. IsUniformVelProfile .OR. IsSimpleCoil .EQ. 1) THEN  !Equivalent circuit and Uniform velocity profile
                    Qcoil=Qcoil*NumOfCkts
                    pRoCoil=pRoCkt
                    hRoCoil=hRoCkt
                    EXIT
                END IF

                IF (Ckt(I)%OutSplit .LE. 1 .AND. Ckt(I)%OutJoin .LE. 1) THEN
                    mRefJoin=mRefJoin+Ckt(I)%mRef 
                END IF

            END DO !End circuit

            IF (IsSimpleCoil .EQ. 1) EXIT

            CALL CalcMeanProp(tAiCoil,tAoCoil,tAmod)

            CPair=CPA(REAL(tAmod))
            Cair=mAicoil*CPAir

            tAoCoil=tAiCoil+Qcoil/Cair

            DiffQcoil=ABS((Qcoil-PrevQcoil)/PrevQcoil)
            IF (DiffQcoil .GT. 1E-3) THEN
                PrevQcoil=Qcoil
            ELSE 
                EXIT
            END IF

        END DO !end AirBCiter

        IF (IsSimpleCoil .EQ. 1) EXIT

        IF (AirBCiter .GT. AirBCmaxIter) THEN
            !WRITE(*,*)'-- WARNING -- Condenser: AirBCiter not converged.'
            ErrorFlag=CONVERGEERROR
        END IF

        IF (EqCircuits .EQ. 1 .AND. IsUniformVelProfile) EXIT !for equivalent circuit and uniform velocity profile no need to update mdot ref.

        !Synchronize from circuit array to tube array
        DO I=1, NumOfCkts

            FirstTube=1
            LastTube=Ckt(I)%Ntube
            IF (Ckt(I)%InSplit .GT. 1) THEN
                FirstTube=2 !Skip first tube
            END IF 
            IF (Ckt(I)%OutJoin .GT. 1) THEN
                LastTube=Ckt(I)%Ntube-1 !Ignore last tube
            END IF

            DO J=FirstTube, LastTube
                TubeNum=Ckt(I)%TubeSequence(J)
                Tube(TubeNum)=Ckt(I)%Tube(J)
            END DO

            IF (Ckt(I)%InSplit .GT. 1) THEN
                Ckt(I)%Tube(1)=Tube(Ckt(I)%TubeSequence(1))
            END IF 
            IF (Ckt(I)%OutJoin .GT. 1) THEN
                Ckt(I)%Tube(Ckt(I)%Ntube)=Tube(Ckt(I)%TubeSequence(Ckt(I)%Ntube))
            END IF

        END DO !End circuit

        !Calculate maximum residual
        pRoCkt=0
        SumpRoCkt=0
        DO I=1, NumOfCkts
            !IF (Ckt(I)%OutJoin .LE. 1 .AND. Ckt(I)%OutSplit .LE. 1) THEN
            IF (Ckt(I)%OutSplit .EQ. 0) THEN !outlet circuit
                SumpRoCkt=SumpRoCkt+Ckt(I)%pRo
                !	  IF (pRoCkt .EQ. 0) pRoCkt=Ckt(I)%pRo
                !	  IF (ABS(pRoCkt-Ckt(I)%pRo) .GT. Ptol) THEN
                !	      MaxResidual=ABS(pRoCkt-Ckt(I)%pRo)
                !	      Converged=.FALSE.
                !	  END IF
            END IF
        END DO !End Circuit
        IF (SumpRoCkt .EQ. 0) SumpRoCkt=Ckt(1)%pRo !At least 1 circuit, ISI - 07/28/06
        pRoCoil=SumpRoCkt/NcktLast

        IF (ABS(pRoCoil-pRoCoilPrev) .GT. Ptol) THEN
            MaxResidual=ABS(pRoCoilPrev-pRoCoil)
            pRoCoilPrev=pRoCoil
            Converged=.FALSE.
        END IF

        IF (IsSameNumOfTubes .AND. IsUniformVelProfile) EXIT	

        IF (NOT(Converged) .OR. Iter .LE. 2) THEN 
            Converged=.TRUE. ! Reinitialize

            !Moved this subroutine to CoilCalc and share with evaporator ISI - 06/05/07
            CALL UpdateRefMassFlowRate(Iter,Ckt,NumOfCkts,pRiCoil,mRefTot,Nnode,Node)

        ELSE
            EXIT
        END IF 

    END DO !End iter

    IF (Iter .GT. MdotMaxIter) THEN
        WRITE(*,107)'-- WARNING -- Condenser: Solution not converged. Max. Residual = ',MaxResidual
        ErrorFlag=CONVERGEERROR
    END IF

    !Surface temperature
    tSiCoil=tSiSUM/NmodLast
    tSoCoil=tSoSUM/NmodLast

    !Coil air side outlet conditions
    !CPair=CPA(REAL(tAiCoil))
    CPair=CPA(REAL(tAmod))
    Cair=mAicoil*CPAir

    tAoCoil=tAiCoil+Qcoil/Cair
    hAoCoil=hAiCoil+Qcoil/mAiCoil

    !Fan air side inlet conditions
    CPair=CPA(REAL(tAoCoil))
    Cair=mAiCoil*CPAir

    !IF (IsCoolingMode .LT. 1 .AND. DrawBlow .EQ. DRAWTHROUGH) THEN !Draw through
    IF (DrawBlow .EQ. DRAWTHROUGH) THEN !Draw through
        tAoCoil=tAoCoil+PwrFan/Cair
        hAoCoil=hAoCoil+PwrFan/mAiCoil
    END IF

    AirPropOpt=1
    AirProp(1)=tAiCoil
    AirProp(4)=hAiCoil
    CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
    rhAiCoil=AirProp(3)
    DensityIn=AirProp(7)

    AirPropOpt=1
    AirProp(1)=tAoCoil
    AirProp(4)=hAoCoil
    CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
    rhAoCoil=AirProp(3)
    DensityOut=AirProp(7)  

    WetFlag=0
    RowNum=0   
    CALL AirSideCalc(CoilType,FinType,WetFlag,Nl,Nt,RowNum,tAiCoil,mAiCoil,DensityIn,DensityOut,Pt,Pl,Ltube,HtCoil, &
    IDtube,ODtube,NumOfChannels,Dchannel,TubeHeight,TubeDepth,FinThk,FinSpg,CurveUnit,CurveTypeHTC,PowerAHTC,PowerBHTC, &
    Poly1HTC,Poly2HTC,Poly3HTC,Poly4HTC,CurveTypeDP,PowerADP,PowerBDP, &
    Poly1DP,Poly2DP,Poly3DP,Poly4DP,Lcoil,AfCoil,AoCoil,AiCoil,FaceVel,hco,DPair)

    DPair=DPair*DPairMultiplier


    hRoCoil=hRiCoil-Qcoil/mRefTot

    Pressure=pRoCoil*1000
    Enthalpy=hRoCoil*1000
    tRoCoil=PH(RefName, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- Condenser: Refprop error.'
        ErrorFlag=REFPROPERROR
        !VL: Previously: GOTO 200
        OUT(24)=ErrorFlag
        CALL Condenser_Helper_1
        RETURN
    END IF

    xRoCoil=PH(RefName, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- Condenser: Refprop error.'
        ErrorFlag=REFPROPERROR
        !VL: Previously: GOTO 200
        OUT(24)=ErrorFlag
        CALL Condenser_Helper_1
        RETURN
    END IF

    Pressure=pRoCoil*1000
    Quality=0
    tSat=PQ(RefName, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- Condenser: Refprop error.'
        ErrorFlag=REFPROPERROR
        !VL: Previously: GOTO 200
        OUT(24)=ErrorFlag
        CALL Condenser_Helper_1
        RETURN
    END IF

    IF (xRoCoil .LE. 0.0) THEN 
        tSCoCoil=tSat-tRoCoil !Subcooling
    ELSE
        tSCoCoil=0.0
    END IF


    !Distributor pressure drop - ISI - 07/14/06
    IF (SystemType .EQ. HEATPUMP) THEN !Heat Pump

        !IF (DisTubeLength .NE. 0) THEN

        !	Pressure=pRoCoil*1000
        !	Enthalpy=hRoCoil*1000
        !	tRoCoil=PH(RefName, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
        !	IF (RefPropErr .GT. 0) THEN
        !		WRITE(*,*)'-- WARNING -- Condenser: Refprop error.'
        !		ErrorFlag=REFPROPERROR
        !		GOTOx 200
        !	END IF

        !	CALL Distributor(Ref$,DisTubeLength/0.0254,NumOfCkts,mRefTot,tRoCoil,hRoCoil,pRiCoil, &
        !					 hRiCoilRtd,QdisTube,DPdisTube,ErrorFlag)
        !	pRoCoil=pRoCoil-DPdisTube
        !END IF

        !****** Liquid line calculation ******
        IF (LliqLn .GT. 0) THEN 
            CALL LiquidLine
            IF (ErrorFlag .GT. CONVERGEERROR) THEN
                WRITE(*,*)'LiquidLine: Refprop error.'
                !VL: Previously: GOTO 200
                OUT(24)=ErrorFlag
                CALL Condenser_Helper_1
                RETURN
            END IF
        ELSE
            pRiExp=pRoCoil !pRoCoilTemp
            hRiExp=hRoCoil
        END IF

    ELSE

        !****** Liquid line calculation ******
        IF (LliqLn .GT. 0) THEN 
            CALL LiquidLine
            IF (ErrorFlag .GT. CONVERGEERROR) THEN
                WRITE(*,*)'LiquidLine: Refprop error.'
                !VL: Previously: GOTO 200
                OUT(24)=ErrorFlag
                CALL Condenser_Helper_1
                RETURN
            END IF
        ELSE
            pRiExp=pRoCoil
            hRiExp=hRoCoil
        END IF

    END IF

    Pressure=pRiExp*1000
    Enthalpy=hRiExp*1000
    tRiExp=PH(RefName, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- Condenser: Refprop error.'
        ErrorFlag=REFPROPERROR
        !VL: Previously: GOTO 200
        OUT(24)=ErrorFlag
        CALL Condenser_Helper_1
        RETURN
    END IF
    xRiExp=PH(RefName, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- Condenser: Refprop error.'
        ErrorFlag=REFPROPERROR
        !VL: Previously: GOTO 200
        OUT(24)=ErrorFlag
        CALL Condenser_Helper_1
        RETURN
    END IF

    Pressure=pRiExp*1000
    Quality=0
    tSat=PQ(RefName, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- Condenser: Refprop error.'
        ErrorFlag=REFPROPERROR
        !VL: Previously: GOTO 200
        OUT(24)=ErrorFlag
        CALL Condenser_Helper_1
        RETURN
    END IF

    !IF (Wabsolute .GT. 0) THEN
    !  Wlocal=LocalOilMassFraction(Wabsolute,xRiExp)
    !  tSat=OilMixtureTsat(RefName,Wlocal,Psat/1000)
    !END IF

    IF (xRiExp .LE. 0) THEN
        tSCiExp=tSat-tRiExp
    ELSE
        tSCiExp=0
    END IF

    OUT(1)=pRiCoil
    OUT(2)=hRiCoil
    OUT(3)=tRiCoil
    OUT(4)=xRiCoil
    OUT(5)=pRoCoil
    OUT(6)=hRoCoil
    OUT(7)=tRoCoil
    OUT(8)=xRoCoil
    OUT(9)=tSCoCoil
    OUT(10)=pRiExp
    OUT(11)=hRiExp
    OUT(12)=tRiExp
    OUT(13)=xRiExp
    OUT(14)=tSCiExp
    OUT(15)=Qcoil
    OUT(16)=MassDisLn
    OUT(17)=MassLiqLn
    OUT(18)=0
    OUT(19)=0
    OUT(20)=0
    OUT(21)=tAoCoil
    OUT(22)=rhAoCoil
    OUT(23)=DPair

    OUT(25)=hco
    OUT(26)=tSiCoil
    OUT(27)=tSoCoil
    OUT(28)=WeightAluminum
    OUT(29)=WeightCopper

!VL: Previously: 200 CONTINUE

    OUT(24)=ErrorFlag

10  FORMAT(3(I4),5(F10.3))
11  FORMAT(6(I))
12  FORMAT(11(I))

101 FORMAT(14(F10.3,','))
102 FORMAT(I3,A,50(F10.3,','))
103 FORMAT(I10,50(',',F10.3))
105 FORMAT(A4,50(',',F10.3))
106 FORMAT(I4,F18.9)
107 FORMAT(A66,F10.3)


    CALL Condenser_Helper_1

    RETURN

    END SUBROUTINE Condenser

    SUBROUTINE Condenser_Helper_1


    IF(CoolHeatModeFlag == 1) THEN
        CoilParams(2)%CoilInletRefTemp=tRiCoil
        CoilParams(2)%CoilOutletRefTemp=tRoCoil
        CoilParams(2)%CoilInletAirTemp=tAiCoil
        CoilParams(2)%CoilOutletAirTemp=tAoCoil
        CoilParams(2)%DPAir=DPair
        CoilParams(2)%DPRef=pRicoil-pRocoil
    ELSE IF(CoolHeatModeFlag == 0) THEN
        CoilParams(1)%CoilInletRefTemp=tRiCoil
        CoilParams(1)%CoilOutletRefTemp=tRoCoil
        CoilParams(1)%CoilInletAirTemp=tAiCoil
        CoilParams(1)%CoilOutletAirTemp=tAoCoil
        CoilParams(1)%DPAir=DPair
        CoilParams(1)%DPRef=pRicoil-pRocoil
    END IF


    END SUBROUTINE Condenser_Helper_1

!************************************************************************

SUBROUTINE CalcCondenserInventory(MassCoil,MassLiqCoil,MassVapCoil, &
                                  LiqTubeLength,VapTubeLength,TwoPhaseTubeLength,NumLiqTubes)

!------------------------------------------------------------------------
!Purpose:
!To get calculate refrigerant inventory in condenser
!
!Author
!Ipseng Iu
!Oklahoma State Univerity, Stillwater
!
!Date
!March 2005
!
!Reference:
!none
!
!------------------------------------------------------------------------

USE FluidProperties
USE CoilCalcMod

IMPLICIT NONE

REAL,INTENT(OUT) :: MassCoil           !Total refrigerant inventory in coil, kg
REAL,INTENT(OUT) :: MassLiqCoil        !Total liquid refrigerant inventory in coil, kg
REAL,INTENT(OUT) :: MassVapCoil        !Total vapor refrigerant inventory in coil, kg
REAL,INTENT(OUT) :: NumLiqTubes        !Number of Liquid tubes
REAL,INTENT(OUT) :: LiqTubeLength      !Liquid tube length, m
REAL,INTENT(OUT) :: VapTubeLength      !Vapor tube length, m
REAL,INTENT(OUT) :: TwoPhaseTubeLength !Two-phase tube length, m

INTEGER :: CoilType !1=Condenser; 2=Evaporator; 
                    !3=High side interconnecting pipes; 
					!4=Low side interconnecting pipes
					!5=Microchannel condenser
					!6=Microchannel evaporator

INTEGER I,J,K,II,III,IV !Loop Counter
REAL Lregion !Region length, m

  LiqTubeLength=0.0
  VapTubeLength=0.0
  TwoPhaseTubeLength=0.0
  
  MassCoil=0
  MassLiqCoil=0
  MassVapCoil=0

  IF (NumOfChannels .GT. 1) THEN
	  CoilType = MCCONDENSER !Microchannel coil
  ELSE
	  CoilType = CONDENSERCOIL !Fin-tube coil
  END IF

  IF (CoilType .NE. MCCONDENSER) THEN 

	  DO I=1, NumOfCkts
		  Ckt(I)%Qckt=0.0
		  Ckt(I)%tSat=0.0
    
		  !Find first and last simulation tubes
		  IF (IsSimpleCoil .EQ. 1) THEN
			  FirstTube=1
			  LastTube=1
		  ELSE
			  FirstTube=1
			  LastTube=Ckt(I)%Ntube
			  IF (Ckt(I)%InSplit .GT. 1) THEN
				  FirstTube=2 
			  END IF 

			  IF (Ckt(I)%OutJoin .GT. 1) THEN
				  LastTube=Ckt(I)%Ntube-1 
			  END IF
		  END IF

		  DO J=1,LastTube !Ckt(I)%Ntube !ISI - 10/30/06
			  DO K=1,NumOfMods
        
				  pRiMod=Ckt(I)%Tube(J)%Seg(K)%pRi
				  hRiMod=Ckt(I)%Tube(J)%Seg(K)%hRi
				  pRoMod=Ckt(I)%Tube(J)%Seg(K)%pRo
				  hRoMod=Ckt(I)%Tube(J)%Seg(K)%hRo

				  LmodTube=Ckt(I)%Tube(J)%Seg(K)%Len
				  AiMod=AiCoil*LmodTube/Lcoil

				  Pressure=pRiMod*1000
				  Enthalpy=hRiMod*1000
				  tRiMod=PH(RefName, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
				  xRiMod=PH(RefName, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
				  vRiMod=PH(RefName, Pressure, Enthalpy, 'density', RefrigIndex,RefPropErr)
				  vRiMod=1/vRiMod
				  cpRiMod=PH(RefName, Pressure, Enthalpy, 'specificheat', RefrigIndex,RefPropErr)
				  cpRiMod=cpRiMod/1000
				  muRiMod=PH(RefName, Pressure, Enthalpy, 'viscosity', RefrigIndex,RefPropErr)
				  kRiMod=PH(RefName, Pressure, Enthalpy, 'conductivity', RefrigIndex,RefPropErr)
				  kRiMod=kRiMod/1000

				  Quality=1
				  vgRiMod=PQ(RefName, Pressure, Quality, 'density', RefrigIndex,RefPropErr)
				  vgRiMod=1/vgRiMod

				  Quality=0
				  vfRiMod=PQ(RefName, Pressure, Quality, 'density', RefrigIndex,RefPropErr)
				  vfRiMod=1/vfRiMod

				  IF (xRiMod .LT. 1 .AND. xRiMod .GT. 0) THEN
					  cpRiMod=0
					  muRiMod=0
					  kRiMod=0
				  END IF

				  Pressure=pRoMod*1000
				  Enthalpy=hRoMod*1000
				  tRoMod=PH(RefName, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
				  xRoMod=PH(RefName, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
				  vRoMod=PH(RefName, Pressure, Enthalpy, 'density', RefrigIndex,RefPropErr)
				  vRoMod=1/vRoMod
				  cpRoMod=PH(RefName, Pressure, Enthalpy, 'specificheat', RefrigIndex,RefPropErr)
				  cpRoMod=cpRoMod/1000
				  muRoMod=PH(RefName, Pressure, Enthalpy, 'viscosity', RefrigIndex,RefPropErr)
				  kRoMod=PH(RefName, Pressure, Enthalpy, 'conductivity', RefrigIndex,RefPropErr)
				  kRoMod=kRoMod/1000

				  Quality=1
				  vgRoMod=PQ(RefName, Pressure, Quality, 'density', RefrigIndex,RefPropErr)
				  vgRoMod=1/vgRoMod

				  Quality=0
				  vfRoMod=PQ(RefName, Pressure, Quality, 'density', RefrigIndex,RefPropErr)
				  vfRoMod=1/vfRoMod

				  Temperature=tRoMod
				  Quality=1
				  IF (tRoMod+273.15 .GT. Tcr) THEN
					  Psat=pRoMod
				  ELSE 
					  Psat=TQ(RefName, Temperature, Quality, 'pressure', RefrigIndex,RefPropErr)
					  Psat=Psat/1000
				  END IF
		  
				  Pressure=pRoMod*1000
				  Quality=0
				  hfRoMod=PQ(RefName, Pressure, Quality, 'enthalpy', RefrigIndex,RefPropErr)
				  hfRoMod=hfRoMod/1000
				  CpfRoMod=PQ(RefName, Pressure, Quality, 'specificheat', RefrigIndex,RefPropErr)
				  CpfRoMod=CpfRoMod/1000
				  mufRoMod=PQ(RefName, Pressure, Quality, 'viscosity', RefrigIndex,RefPropErr)
				  kfRoMod=PQ(RefName, Pressure, Quality, 'conductivity', RefrigIndex,RefPropErr)
				  kfRoMod=kfRoMod/1000

				  Pressure=pRoMod*1000
				  Quality=1
				  hgRoMod=PQ(RefName, Pressure, Quality, 'enthalpy', RefrigIndex,RefPropErr)
				  hgRoMod=hgRoMod/1000
				  CpgRoMod=PQ(RefName, Pressure, Quality, 'specificheat', RefrigIndex,RefPropErr)
				  CpgRoMod=CpgRoMod/1000
				  mugRoMod=PQ(RefName, Pressure, Quality, 'viscosity', RefrigIndex,RefPropErr)
				  kgRoMod=PQ(RefName, Pressure, Quality, 'conductivity', RefrigIndex,RefPropErr)
				  kgRoMod=kgRoMod/1000

				  IF (xRoMod .LT. 1 .AND. xRoMod .GT. 0) THEN
					  cpRoMod=0
					  muRoMod=0
					  kRoMod=0
				  END IF

				  mu=(muRiMod+muRoMod)/2
				  kRef=(kRiMod+kRoMod)/2
				  cpRef=(cpRiMod+cpRoMod)/2
				  rhoRef=(1/vRiMod+1/vRoMod)/2

				  mRefMod=Ckt(I)%mRef

				  tAiMod=Ckt(I)%Tube(J)%Seg(K)%tAi
				  rhAiMod=Ckt(I)%Tube(J)%Seg(K)%rhAi

				  tAoMod=Ckt(I)%Tube(J)%Seg(K)%tAo
				  rhAoMod=Ckt(I)%Tube(J)%Seg(K)%rhAo

				  Qmod=Ckt(I)%Tube(J)%Seg(K)%Qmod
				  hciMod=Ckt(I)%Tube(J)%Seg(K)%hci
				  hcoMod=Ckt(I)%Tube(J)%Seg(K)%hco
				  ReVap=Ckt(I)%Tube(J)%Seg(K)%ReVap
				  ReLiq=Ckt(I)%Tube(J)%Seg(K)%ReLiq
				  Cair=Ckt(I)%Tube(J)%Seg(K)%cAir
				  Rair=Ckt(I)%Tube(J)%Seg(K)%Rair
				  Rtube=Ckt(I)%Tube(J)%Seg(K)%Rtube

				  IF (K .EQ. NumOfMods .OR. (J .EQ. LastTube .AND. (Ckt(I)%OutSplit .GT. 1 .OR. Ckt(I)%OutJoin .GT. 1))) THEN
					  !Include return bend length
					  CALL Inventory(RefName,CoilType,TubeType,IDtube,ktube,mRefMod,Qmod,hgRoMod,hfRoMod,hRiMod,hRoMod, &
									 xRiMod,xRoMod,vRiMod,vRoMod,vgRimod,vfRimod,vgRomod,vfRomod, &
									 muRoMod,mugRoMod,mufRoMod,kRoMod,kfRoMod,kgRoMod,CpRoMod,CpfRoMod,CpgRoMod, &
									 MolWeight,pRoMod,Psat,Pcr,Tsat,cAir,Const,Rair,Rtube,AiMod, &
									 LmodTube+Lreturnbend,LmodTP,LmodSH,MassLiqMod,MassVapMod,MassMod)

				  ELSE
					  CALL Inventory(RefName,CoilType,TubeType,IDtube,ktube,mRefMod,Qmod,hgRoMod,hfRoMod,hRiMod,hRoMod, &
									 xRiMod,xRoMod,vRiMod,vRoMod,vgRimod,vfRimod,vgRomod,vfRomod, &
									 muRoMod,mugRoMod,mufRoMod,kRoMod,kfRoMod,kgRoMod,CpRoMod,CpfRoMod,CpgRoMod, &
									 MolWeight,pRoMod,Psat,Pcr,Tsat,cAir,Const,Rair,Rtube,AiMod, &
									 LmodTube,LmodTP,LmodSH,MassLiqMod,MassVapMod,MassMod)

				  END IF
			  
				  Ckt(I)%Tube(J)%Seg(K)%Mass=MassMod

				  !IF (K .EQ. NumOfMods .OR. (J .EQ. LastTube .AND. (Ckt(I)%OutSplit .GT. 1 .OR. Ckt(I)%OutJoin .GT. 1))) THEN
				  IF ((K .EQ. NumOfMods .AND. J .NE. LastTube) .OR. (J .EQ. LastTube .AND. (Ckt(I)%OutSplit .GT. 1 .OR. Ckt(I)%OutJoin .GT. 1))) THEN !ISI - 02/05/07
					  Lregion=LmodTube+Lreturnbend
				  ELSE
					  Lregion=LmodTube
				  END IF

				  IF (IsCoolingMode .GT. 0) THEN !Condenser
					  IF (xRoMod .GE. 1) THEN 
						  VapTubeLength=VapTubeLength+Lregion !Superheated region
					  ELSEIF (xRiMod .LE. 0) THEN
						  LiqTubeLength=LiqTubeLength+Lregion !Subcooled region
					  ELSEIF (xRiMod .LT. 1 .AND. xRoMod .GT. 0) THEN
						  TwoPhaseTubeLength=TwoPhaseTubeLength+Lregion !Two-phase region
					  ELSEIF (xRiMod .GT. 0 .AND. xRoMod .LT. 0) THEN !Condenser outlet
						  LiqTubeLength=LiqTubeLength+(hfRoMod-hRoMod)/(hRiMod-hRoMod)*Lregion 
						  TwoPhaseTubeLength=TwoPhaseTubeLength+(1-(hfRoMod-hRoMod)/(hRiMod-hRoMod))*Lregion 
					  ELSEIF (xRiMod .GT. 1 .AND. xRoMod .LT. 1) THEN !Condenser inlet
						  VapTubeLength=VapTubeLength+(hRiMod-hgRoMod)/(hRiMod-hRoMod)*Lregion
						  TwoPhaseTubeLength=TwoPhaseTubeLength+(1-(hRiMod-hgRoMod)/(hRiMod-hRoMod))*Lregion
					  END IF
				  ELSE !Evaporator
					  IF (xRiMod .GE. 1) THEN
						  VapTubeLength=VapTubeLength+Lregion !Superheated region
					  ELSEIF (xRoMod .LE. 0) THEN
						  LiqTubeLength=LiqTubeLength+Lregion !Subcooled region
					  ELSEIF (xRiMod .GT. 0 .AND. xRoMod .LT. 1) THEN
						  TwoPhaseTubeLength=TwoPhaseTubeLength+Lregion !Two-phase region
					  ELSEIF (xRiMod .LT. 1 .AND. xRoMod .GT. 1) THEN !Evaporator outlet
						  VapTubeLength=VapTubeLength+(hRoMod-hgRoMod)/(hRoMod-hRiMod)*Lregion
						  TwoPhaseTubeLength=TwoPhaseTubeLength+(1-(hRoMod-hgRoMod)/(hRoMod-hRiMod))*Lregion 
					  ELSEIF (xRiMod .LT. 0 .AND. xRoMod .GT. 0) THEN !Evaporator inlet
						  LiqTubeLength=LiqTubeLength+(hfRoMod-hRiMod)/(hRoMod-hRiMod)*Lregion
						  TwoPhaseTubeLength=TwoPhaseTubeLength+(1-(hfRoMod-hRiMod)/(hRoMod-hRiMod))*Lregion
					  END IF
				  END IF

				  !Total mass inventory
				  IF (J .GE. FirstTube .AND. J .LE. LastTube) THEN
					  MassCoil=MassCoil+MassMod 
					  MassLiqCoil=MassLiqCoil+MassLiqMod
					  MassVapCoil=MassVapCoil+MassVapMod
				  END IF
			
			  END DO !end Nmod

		  END DO !end Ntube

		  IF (EqCircuits .EQ. 1 .AND. IsUniformVelProfile .OR. IsSimpleCoil .EQ. 1) THEN 
			  MassCoil=MassCoil*NumOfCkts
			  MassLiqCoil=MassLiqCoil*NumOfCkts
			  MassVapCoil=MassVapCoil*NumOfCkts
			  LiqTubeLength=LiqTubeLength*NumOfCkts 
			  TwoPhaseTubeLength=TwoPhaseTubeLength*NumOfCkts !ISI - 02/05/07
			  VapTubeLength=VapTubeLength*NumOfCkts !ISI - 02/05/07
			  EXIT
		  END IF
 
	  END DO !end circuit

	  NumLiqTubes=LiqTubeLength/(LiqTubeLength+TwoPhaseTubeLength+VapTubeLength) !ISI - 02/05/07

  ELSE !Microchannel coil

	  DO I=1, Nl

		  DO II=1,Slab(I)%Npass

			  DO III=1,1 !NumOfTubes
				  
				  DO IV=1, NumOfMods
        
					  pRiMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%pRi
					  hRiMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%hRi
					  pRoMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%pRo
					  hRoMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%hRo

					  LmodTube=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%Len
					  AiMod=AiCoil*LmodTube/Lcoil

					  Pressure=pRiMod*1000
					  Enthalpy=hRiMod*1000
					  tRiMod=PH(RefName, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
					  xRiMod=PH(RefName, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
					  vRiMod=PH(RefName, Pressure, Enthalpy, 'density', RefrigIndex,RefPropErr)
					  vRiMod=1/vRiMod
					  cpRiMod=PH(RefName, Pressure, Enthalpy, 'specificheat', RefrigIndex,RefPropErr)
					  cpRiMod=cpRiMod/1000
					  muRiMod=PH(RefName, Pressure, Enthalpy, 'viscosity', RefrigIndex,RefPropErr)
					  kRiMod=PH(RefName, Pressure, Enthalpy, 'conductivity', RefrigIndex,RefPropErr)
					  kRiMod=kRiMod/1000

					  Quality=1
					  vgRiMod=PQ(RefName, Pressure, Quality, 'density', RefrigIndex,RefPropErr)
					  vgRiMod=1/vgRiMod

					  Quality=0
					  vfRiMod=PQ(RefName, Pressure, Quality, 'density', RefrigIndex,RefPropErr)
					  vfRiMod=1/vfRiMod

					  IF (xRiMod .LT. 1 .AND. xRiMod .GT. 0) THEN
						  cpRiMod=0
						  muRiMod=0
						  kRiMod=0
					  END IF

					  Pressure=pRoMod*1000
					  Enthalpy=hRoMod*1000
					  tRoMod=PH(RefName, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
					  xRoMod=PH(RefName, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
					  vRoMod=PH(RefName, Pressure, Enthalpy, 'density', RefrigIndex,RefPropErr)
					  vRoMod=1/vRoMod
					  cpRoMod=PH(RefName, Pressure, Enthalpy, 'specificheat', RefrigIndex,RefPropErr)
					  cpRoMod=cpRoMod/1000
					  muRoMod=PH(RefName, Pressure, Enthalpy, 'viscosity', RefrigIndex,RefPropErr)
					  kRoMod=PH(RefName, Pressure, Enthalpy, 'conductivity', RefrigIndex,RefPropErr)
					  kRoMod=kRoMod/1000

					  Quality=1
					  vgRoMod=PQ(RefName, Pressure, Quality, 'density', RefrigIndex,RefPropErr)
					  vgRoMod=1/vgRoMod

					  Quality=0
					  vfRoMod=PQ(RefName, Pressure, Quality, 'density', RefrigIndex,RefPropErr)
					  vfRoMod=1/vfRoMod

					  Temperature=tRoMod
					  Quality=1
					  IF (tRoMod+273.15 .GT. Tcr) THEN
						  Psat=pRoMod
					  ELSE 
						  Psat=TQ(RefName, Temperature, Quality, 'pressure', RefrigIndex,RefPropErr)
						  Psat=Psat/1000
					  END IF
			  
					  Pressure=pRoMod*1000
					  Quality=0
					  hfRoMod=PQ(RefName, Pressure, Quality, 'enthalpy', RefrigIndex,RefPropErr)
					  hfRoMod=hfRoMod/1000
					  CpfRoMod=PQ(RefName, Pressure, Quality, 'specificheat', RefrigIndex,RefPropErr)
					  CpfRoMod=CpfRoMod/1000
					  mufRoMod=PQ(RefName, Pressure, Quality, 'viscosity', RefrigIndex,RefPropErr)
					  kfRoMod=PQ(RefName, Pressure, Quality, 'conductivity', RefrigIndex,RefPropErr)
					  kfRoMod=kfRoMod/1000

					  Pressure=pRoMod*1000
					  Quality=1
					  hgRoMod=PQ(RefName, Pressure, Quality, 'enthalpy', RefrigIndex,RefPropErr)
					  hgRoMod=hgRoMod/1000
					  CpgRoMod=PQ(RefName, Pressure, Quality, 'specificheat', RefrigIndex,RefPropErr)
					  CpgRoMod=CpgRoMod/1000
					  mugRoMod=PQ(RefName, Pressure, Quality, 'viscosity', RefrigIndex,RefPropErr)
					  kgRoMod=PQ(RefName, Pressure, Quality, 'conductivity', RefrigIndex,RefPropErr)
					  kgRoMod=kgRoMod/1000

					  IF (xRoMod .LT. 1 .AND. xRoMod .GT. 0) THEN
						  cpRoMod=0
						  muRoMod=0
						  kRoMod=0
					  END IF

					  mu=(muRiMod+muRoMod)/2
					  kRef=(kRiMod+kRoMod)/2
					  cpRef=(cpRiMod+cpRoMod)/2
					  rhoRef=(1/vRiMod+1/vRoMod)/2

					  mRefMod=mRefTot/Slab(I)%Pass(II)%Ntube !Ckt(I)%mRef

					  tAiMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%tAi
					  rhAiMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%rhAi

					  tAoMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%tAo
					  rhAoMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%rhAo

					  Qmod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%Qmod
					  hciMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%hci
					  hcoMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%hco
					  ReVap=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%ReVap
					  ReLiq=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%ReLiq
					  Cair=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%cAir
					  Rair=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%Rair
					  Rtube=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%Rtube

					  CALL Inventory(RefName,CoilType,TubeType,Dchannel,ktube,mRefMod/NumOfChannels,Qmod,hgRoMod,hfRoMod,hRiMod,hRoMod, &
									 xRiMod,xRoMod,vRiMod,vRoMod,vgRimod,vfRimod,vgRomod,vfRomod, &
									 muRoMod,mugRoMod,mufRoMod,kRoMod,kfRoMod,kgRoMod,CpRoMod,CpfRoMod,CpgRoMod, &
									 MolWeight,pRoMod,Psat,Pcr,Tsat,cAir,Const,Rair,Rtube,AiMod, &
									 LmodTube,LmodTP,LmodSH,MassLiqMod,MassVapMod,MassMod)
			  
					  MassMod=MassMod*NumOfChannels
					  Slab(I)%Pass(II)%Tube(III)%Seg(IV)%Mass=MassMod

					  Lregion=LmodTube

					  IF (IsCoolingMode .GT. 0) THEN !Condenser
						  IF (xRoMod .GE. 1) THEN 
							  VapTubeLength=VapTubeLength+Lregion !Superheated region
						  ELSEIF (xRiMod .LE. 0) THEN
							  LiqTubeLength=LiqTubeLength+Lregion !Subcooled region
						  ELSEIF (xRiMod .LT. 1 .AND. xRoMod .GT. 0) THEN
							  TwoPhaseTubeLength=TwoPhaseTubeLength+Lregion !Two-phase region
						  ELSEIF (xRiMod .GT. 0 .AND. xRoMod .LT. 0) THEN !Condenser outlet
							  LiqTubeLength=LiqTubeLength+(hfRoMod-hRoMod)/(hRiMod-hRoMod)*Lregion 
							  TwoPhaseTubeLength=TwoPhaseTubeLength+(1-(hfRoMod-hRoMod)/(hRiMod-hRoMod))*Lregion 
						  ELSEIF (xRiMod .GT. 1 .AND. xRoMod .LT. 1) THEN !Condenser inlet
							  VapTubeLength=VapTubeLength+(hRiMod-hgRoMod)/(hRiMod-hRoMod)*Lregion
							  TwoPhaseTubeLength=TwoPhaseTubeLength+(1-(hRiMod-hgRoMod)/(hRiMod-hRoMod))*Lregion
						  END IF
					  ELSE !Evaporator
						  IF (xRiMod .GE. 1) THEN
							  VapTubeLength=VapTubeLength+Lregion !Superheated region
						  ELSEIF (xRoMod .LE. 0) THEN
							  LiqTubeLength=LiqTubeLength+Lregion !Subcooled region
						  ELSEIF (xRiMod .GT. 0 .AND. xRoMod .LT. 1) THEN
							  TwoPhaseTubeLength=TwoPhaseTubeLength+Lregion !Two-phase region
						  ELSEIF (xRiMod .LT. 1 .AND. xRoMod .GT. 1) THEN !Evaporator outlet
							  VapTubeLength=VapTubeLength+(hRoMod-hgRoMod)/(hRoMod-hRiMod)*Lregion
							  TwoPhaseTubeLength=TwoPhaseTubeLength+(1-(hRoMod-hgRoMod)/(hRoMod-hRiMod))*Lregion 
						  ELSEIF (xRiMod .LT. 0 .AND. xRoMod .GT. 0) THEN !Evaporator inlet
							  LiqTubeLength=LiqTubeLength+(hfRoMod-hRiMod)/(hRoMod-hRiMod)*Lregion
							  TwoPhaseTubeLength=TwoPhaseTubeLength+(1-(hfRoMod-hRiMod)/(hRoMod-hRiMod))*Lregion
						  END IF
					  END IF

					  !Total mass inventory

					  MassCoil=MassCoil+MassMod*Slab(I)%Pass(II)%Ntube
					  MassLiqCoil=MassLiqCoil+MassLiqMod*Slab(I)%Pass(II)%Ntube
					  MassVapCoil=MassVapCoil+MassVapMod*Slab(I)%Pass(II)%Ntube

				  END DO !end Nmod
				
			  END DO !end Ntube 

		  END DO !end pass
  
	  END DO !end slab

	  !NumLiqTubes=LiqTubeLength/Lcoil
	  NumLiqTubes=LiqTubeLength/(LiqTubeLength+TwoPhaseTubeLength+VapTubeLength) !ISI - 02/05/07

  END IF

  RETURN

END SUBROUTINE CalcCondenserInventory

!************************************************************************

SUBROUTINE PrintCondenserResult

!------------------------------------------------------------------------
!Purpose:
!To print simulation result to output file "condenser.csv"
!
!Author
!Ipseng Iu
!Oklahoma State Univerity, Stillwater
!
!Date
!March 2005
!
!Reference:
!none
!
!------------------------------------------------------------------------

USE FluidProperties
USE CoilCalcMod

IMPLICIT NONE

INTEGER CoilType !1=Condenser; 2=Evaporator; 
                 !3=High side interconnecting pipes; 
				 !4=Low side interconnecting pipes
				 !5=Microchannel condenser
				 !6=Microchannel evaporator

REAL :: MassCoil    !Total refrigerant inventory in coil, kg
REAL :: MassLiqCoil !Total liquid refrigerant inventory in coil, kg
REAL :: MassVapCoil !Total vapor refrigerant inventory in coil, kg

INTEGER I,J,K,II,III,IV !Loop Counter

  OPEN (16,FILE='Condenser.csv')

  MassCoil=0
  MassLiqCoil=0
  MassVapCoil=0

  IF (NumOfChannels .GT. 1) THEN
	  CoilType = MCCONDENSER
  ELSE
	  CoilType = CONDENSERCOIL
  END IF

  IF (CoilType .NE. MCCONDENSER) THEN

	  WRITE(16,100)'Nckt','Ntube','Nmod','tRi(C)','tRo(C)','pRi(kPa)','pRo(kPa)', &
				   'hRi(kJ/kg)','hRo(kJ/kg)','xRi','xRo','tAi(C)','tAo(C)', &
  				   'rhAi','rhAo','hci(W/m2K)','EF','hco(W/m2K)', &
				   'mu(uPa-s)','k(W/mK)','cp(kJ/kgK)','rho(kg/m3)','ReVap','ReLiq', &
				   'Qmod(W)','MassLiq(g)','MassVap(g)','MassTot(g)','mdot(kg/h)' 

	  DO I=1, NumOfCkts
		  Ckt(I)%Qckt=0.0
		  Ckt(I)%tSat=0.0
    
		  !Find first and last simulation tubes
		  FirstTube=1
		  LastTube=Ckt(I)%Ntube
		  IF (Ckt(I)%InSplit .GT. 1) THEN
			  FirstTube=2 
		  END IF 
		  IF (Ckt(I)%OutJoin .GT. 1) THEN
			  LastTube=Ckt(I)%Ntube-1 
		  END IF

		  DO J=1,Ckt(I)%Ntube
			  DO K=1,NumOfMods
        
				  pRiMod=Ckt(I)%Tube(J)%Seg(K)%pRi
				  hRiMod=Ckt(I)%Tube(J)%Seg(K)%hRi
				  pRoMod=Ckt(I)%Tube(J)%Seg(K)%pRo
				  hRoMod=Ckt(I)%Tube(J)%Seg(K)%hRo

				  LmodTube=Ckt(I)%Tube(J)%Seg(K)%Len
				  AiMod=AiCoil*LmodTube/Lcoil

				  Pressure=pRiMod*1000
				  Enthalpy=hRiMod*1000
				  tRiMod=PH(RefName, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
				  xRiMod=PH(RefName, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
				  vRiMod=PH(RefName, Pressure, Enthalpy, 'density', RefrigIndex,RefPropErr)
				  vRiMod=1/vRiMod
				  cpRiMod=PH(RefName, Pressure, Enthalpy, 'specificheat', RefrigIndex,RefPropErr)
				  cpRiMod=cpRiMod/1000
				  muRiMod=PH(RefName, Pressure, Enthalpy, 'viscosity', RefrigIndex,RefPropErr)
				  kRiMod=PH(RefName, Pressure, Enthalpy, 'conductivity', RefrigIndex,RefPropErr)
				  kRiMod=kRiMod/1000

				  Quality=1
				  vgRiMod=PQ(RefName, Pressure, Quality, 'density', RefrigIndex,RefPropErr)
				  vgRiMod=1/vgRiMod

				  Quality=0
				  vfRiMod=PQ(RefName, Pressure, Quality, 'density', RefrigIndex,RefPropErr)
				  vfRiMod=1/vfRiMod

				  IF (xRiMod .LT. 1 .AND. xRiMod .GT. 0) THEN
					  cpRiMod=0
					  muRiMod=0
					  kRiMod=0
				  END IF

				  Pressure=pRoMod*1000
				  Enthalpy=hRoMod*1000
				  tRoMod=PH(RefName, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
				  xRoMod=PH(RefName, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
				  vRoMod=PH(RefName, Pressure, Enthalpy, 'density', RefrigIndex,RefPropErr)
				  vRoMod=1/vRoMod
				  cpRoMod=PH(RefName, Pressure, Enthalpy, 'specificheat', RefrigIndex,RefPropErr)
				  cpRoMod=cpRoMod/1000
				  muRoMod=PH(RefName, Pressure, Enthalpy, 'viscosity', RefrigIndex,RefPropErr)
				  kRoMod=PH(RefName, Pressure, Enthalpy, 'conductivity', RefrigIndex,RefPropErr)
				  kRoMod=kRoMod/1000

				  Quality=1
				  vgRoMod=PQ(RefName, Pressure, Quality, 'density', RefrigIndex,RefPropErr)
				  vgRoMod=1/vgRoMod

				  Quality=0
				  vfRoMod=PQ(RefName, Pressure, Quality, 'density', RefrigIndex,RefPropErr)
				  vfRoMod=1/vfRoMod

				  Temperature=tRoMod
				  Quality=1
				  IF (tRoMod+273.15 .GT. Tcr) THEN
					  Psat=pRoMod
				  ELSE 
					  Psat=TQ(RefName, Temperature, Quality, 'pressure', RefrigIndex,RefPropErr)
					  Psat=Psat/1000
				  END IF
		  
				  Pressure=pRoMod*1000
				  Quality=0
				  hfRoMod=PQ(RefName, Pressure, Quality, 'enthalpy', RefrigIndex,RefPropErr)
				  hfRoMod=hfRoMod/1000
				  CpfRoMod=PQ(RefName, Pressure, Quality, 'specificheat', RefrigIndex,RefPropErr)
				  CpfRoMod=CpfRoMod/1000
				  mufRoMod=PQ(RefName, Pressure, Quality, 'viscosity', RefrigIndex,RefPropErr)
				  kfRoMod=PQ(RefName, Pressure, Quality, 'conductivity', RefrigIndex,RefPropErr)
				  kfRoMod=kfRoMod/1000

				  Pressure=pRoMod*1000
				  Quality=1
				  hgRoMod=PQ(RefName, Pressure, Quality, 'enthalpy', RefrigIndex,RefPropErr)
				  hgRoMod=hgRoMod/1000
				  CpgRoMod=PQ(RefName, Pressure, Quality, 'specificheat', RefrigIndex,RefPropErr)
				  CpgRoMod=CpgRoMod/1000
				  mugRoMod=PQ(RefName, Pressure, Quality, 'viscosity', RefrigIndex,RefPropErr)
				  kgRoMod=PQ(RefName, Pressure, Quality, 'conductivity', RefrigIndex,RefPropErr)
				  kgRoMod=kgRoMod/1000

				  IF (xRoMod .LT. 1 .AND. xRoMod .GT. 0) THEN
					  cpRoMod=0
					  muRoMod=0
					  kRoMod=0
				  END IF

				  mu=(muRiMod+muRoMod)/2
				  kRef=(kRiMod+kRoMod)/2
				  cpRef=(cpRiMod+cpRoMod)/2
				  rhoRef=(1/vRiMod+1/vRoMod)/2

				  mRefMod=Ckt(I)%mRef

				  tAiMod=Ckt(I)%Tube(J)%Seg(K)%tAi
				  rhAiMod=Ckt(I)%Tube(J)%Seg(K)%rhAi

				  tAoMod=Ckt(I)%Tube(J)%Seg(K)%tAo
				  rhAoMod=Ckt(I)%Tube(J)%Seg(K)%rhAo

				  Qmod=Ckt(I)%Tube(J)%Seg(K)%Qmod
				  hciMod=Ckt(I)%Tube(J)%Seg(K)%hci
				  EFref=Ckt(I)%Tube(J)%Seg(K)%EFref
				  hcoMod=Ckt(I)%Tube(J)%Seg(K)%hco
				  ReVap=Ckt(I)%Tube(J)%Seg(K)%ReVap
				  ReLiq=Ckt(I)%Tube(J)%Seg(K)%ReLiq
				  Cair=Ckt(I)%Tube(J)%Seg(K)%cAir
				  Rair=Ckt(I)%Tube(J)%Seg(K)%Rair
				  Rtube=Ckt(I)%Tube(J)%Seg(K)%Rtube

				  IF (K .EQ. NumOfMods .OR. (J .EQ. LastTube .AND. (Ckt(I)%OutSplit .GT. 1 .OR. Ckt(I)%OutJoin .GT. 1))) THEN
					  !Include return bend length
					  CALL Inventory(RefName,CoilType,TubeType,IDtube,ktube,mRefMod,Qmod,hgRoMod,hfRoMod,hRiMod,hRoMod, &
									 xRiMod,xRoMod,vRiMod,vRoMod,vgRimod,vfRimod,vgRomod,vfRomod, &
									 muRoMod,mugRoMod,mufRoMod,kRoMod,kfRoMod,kgRoMod,CpRoMod,CpfRoMod,CpgRoMod, &
									 MolWeight,pRoMod,Psat,Pcr,Tsat,cAir,Const,Rair,Rtube,AiMod, &
									 LmodTube+Lreturnbend,LmodTP,LmodSH,MassLiqMod,MassVapMod,MassMod)
				  ELSE
					  CALL Inventory(RefName,CoilType,TubeType,IDtube,ktube,mRefMod,Qmod,hgRoMod,hfRoMod,hRiMod,hRoMod, &
									 xRiMod,xRoMod,vRiMod,vRoMod,vgRimod,vfRimod,vgRomod,vfRomod, &
									 muRoMod,mugRoMod,mufRoMod,kRoMod,kfRoMod,kgRoMod,CpRoMod,CpfRoMod,CpgRoMod, &
									 MolWeight,pRoMod,Psat,Pcr,Tsat,cAir,Const,Rair,Rtube,AiMod, &
									 LmodTube,LmodTP,LmodSH,MassLiqMod,MassVapMod,MassMod)
				  END IF
			  
				  Ckt(I)%Tube(J)%Seg(K)%Mass=MassMod

				  !Total mass inventory
				  IF (J .GE. FirstTube .AND. J .LE. LastTube) THEN
					  MassCoil=MassCoil+MassMod 
					  MassLiqCoil=MassLiqCoil+MassLiqMod
					  MassVapCoil=MassVapCoil+MassVapMod
				  END IF
			
				  IF (xRiMod .EQ. -100) xRiMod=0.0
				  IF (xRiMod .EQ. 100) xRiMod=1.0
				  IF (xRoMod .EQ. -100) xRoMod=0.0
				  IF (xRoMod .EQ. 100) xRoMod=1.0

				  MassMod=Ckt(I)%Tube(J)%Seg(K)%Mass
				  WRITE(16,104)I,J,K,tRiMod,tRoMod,pRiMod,pRoMod,hRiMod,hRoMod, &
							   xRiMod,xRoMod,tAiMod,tAoMod,rhAiMod,rhAoMod, &
							   hciMod*1000,EFref,hcoMod*1000,mu*1e6,kRef*1e3,cpRef,rhoRef,ReVap,ReLiq, &
							   Qmod*1000,MassLiqMod*1000,MassVapMod*1000,MassMod*1000, &
							   mRefMod*3600

			  END DO !end Nmod

		  END DO !end Ntube
  
	  END DO !end circuit

  ELSE

	  WRITE(16,100)'Nslab','Npass','Nmod','tRi(C)','tRo(C)','pRi(kPa)','pRo(kPa)', &
				   'hRi(kJ/kg)','hRo(kJ/kg)','xRi','xRo','tAi(C)','tAo(C)', &
  				   'rhAi','rhAo','hci(W/m2K)','hco(W/m2K)', &
				   'mu(uPa-s)','k(W/mK)','cp(kJ/kgK)','rho(kg/m3)','ReVap','ReLiq', &
				   'Qmod(W)','MassLiq(g)','MassVap(g)','MassTot(g)','mdot(kg/h)' 

	  DO I=1, Nl

		  DO II=1,Slab(I)%Npass

			  DO III=1,1 !Num Of tubes
				
				  DO IV=1, NumOfMods
        
					  pRiMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%pRi
					  hRiMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%hRi
					  pRoMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%pRo
					  hRoMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%hRo

					  LmodTube=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%Len
					  AiMod=AiCoil*LmodTube/Lcoil

					  Pressure=pRiMod*1000
					  Enthalpy=hRiMod*1000
					  tRiMod=PH(RefName, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
					  xRiMod=PH(RefName, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
					  vRiMod=PH(RefName, Pressure, Enthalpy, 'density', RefrigIndex,RefPropErr)
					  vRiMod=1/vRiMod
					  cpRiMod=PH(RefName, Pressure, Enthalpy, 'specificheat', RefrigIndex,RefPropErr)
					  cpRiMod=cpRiMod/1000
					  muRiMod=PH(RefName, Pressure, Enthalpy, 'viscosity', RefrigIndex,RefPropErr)
					  kRiMod=PH(RefName, Pressure, Enthalpy, 'conductivity', RefrigIndex,RefPropErr)
					  kRiMod=kRiMod/1000

					  Quality=1
					  vgRiMod=PQ(RefName, Pressure, Quality, 'density', RefrigIndex,RefPropErr)
					  vgRiMod=1/vgRiMod

					  Quality=0
					  vfRiMod=PQ(RefName, Pressure, Quality, 'density', RefrigIndex,RefPropErr)
					  vfRiMod=1/vfRiMod

					  IF (xRiMod .LT. 1 .AND. xRiMod .GT. 0) THEN
						  cpRiMod=0
						  muRiMod=0
						  kRiMod=0
					  END IF

					  Pressure=pRoMod*1000
					  Enthalpy=hRoMod*1000
					  tRoMod=PH(RefName, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
					  xRoMod=PH(RefName, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
					  vRoMod=PH(RefName, Pressure, Enthalpy, 'density', RefrigIndex,RefPropErr)
					  vRoMod=1/vRoMod
					  cpRoMod=PH(RefName, Pressure, Enthalpy, 'specificheat', RefrigIndex,RefPropErr)
					  cpRoMod=cpRoMod/1000
					  muRoMod=PH(RefName, Pressure, Enthalpy, 'viscosity', RefrigIndex,RefPropErr)
					  kRoMod=PH(RefName, Pressure, Enthalpy, 'conductivity', RefrigIndex,RefPropErr)
					  kRoMod=kRoMod/1000

					  Quality=1
					  vgRoMod=PQ(RefName, Pressure, Quality, 'density', RefrigIndex,RefPropErr)
					  vgRoMod=1/vgRoMod

					  Quality=0
					  vfRoMod=PQ(RefName, Pressure, Quality, 'density', RefrigIndex,RefPropErr)
					  vfRoMod=1/vfRoMod

					  Temperature=tRoMod
					  Quality=1
					  IF (tRoMod+273.15 .GT. Tcr) THEN
						  Psat=pRoMod
					  ELSE 
						  Psat=TQ(RefName, Temperature, Quality, 'pressure', RefrigIndex,RefPropErr)
						  Psat=Psat/1000
					  END IF
			  
					  Pressure=pRoMod*1000
					  Quality=0
					  hfRoMod=PQ(RefName, Pressure, Quality, 'enthalpy', RefrigIndex,RefPropErr)
					  hfRoMod=hfRoMod/1000
					  CpfRoMod=PQ(RefName, Pressure, Quality, 'specificheat', RefrigIndex,RefPropErr)
					  CpfRoMod=CpfRoMod/1000
					  mufRoMod=PQ(RefName, Pressure, Quality, 'viscosity', RefrigIndex,RefPropErr)
					  kfRoMod=PQ(RefName, Pressure, Quality, 'conductivity', RefrigIndex,RefPropErr)
					  kfRoMod=kfRoMod/1000

					  Pressure=pRoMod*1000
					  Quality=1
					  hgRoMod=PQ(RefName, Pressure, Quality, 'enthalpy', RefrigIndex,RefPropErr)
					  hgRoMod=hgRoMod/1000
					  CpgRoMod=PQ(RefName, Pressure, Quality, 'specificheat', RefrigIndex,RefPropErr)
					  CpgRoMod=CpgRoMod/1000
					  mugRoMod=PQ(RefName, Pressure, Quality, 'viscosity', RefrigIndex,RefPropErr)
					  kgRoMod=PQ(RefName, Pressure, Quality, 'conductivity', RefrigIndex,RefPropErr)
					  kgRoMod=kgRoMod/1000

					  IF (xRoMod .LT. 1 .AND. xRoMod .GT. 0) THEN
						  cpRoMod=0
						  muRoMod=0
						  kRoMod=0
					  END IF

					  mu=(muRiMod+muRoMod)/2
					  kRef=(kRiMod+kRoMod)/2
					  cpRef=(cpRiMod+cpRoMod)/2
					  rhoRef=(1/vRiMod+1/vRoMod)/2

					  mRefMod=mRefTot/Slab(I)%Pass(II)%Ntube !Ckt(I)%mRef

					  tAiMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%tAi
					  rhAiMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%rhAi

					  tAoMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%tAo
					  rhAoMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%rhAo

					  Qmod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%Qmod
					  hciMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%hci
					  hcoMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%hco
					  ReVap=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%ReVap
					  ReLiq=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%ReLiq
					  Cair=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%cAir
					  Rair=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%Rair
					  Rtube=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%Rtube

					  CALL Inventory(RefName,CoilType,TubeType,Dchannel,ktube,mRefMod/NumOfChannels,Qmod,hgRoMod,hfRoMod,hRiMod,hRoMod, &
									 xRiMod,xRoMod,vRiMod,vRoMod,vgRimod,vfRimod,vgRomod,vfRomod, &
									 muRoMod,mugRoMod,mufRoMod,kRoMod,kfRoMod,kgRoMod,CpRoMod,CpfRoMod,CpgRoMod, &
									 MolWeight,pRoMod,Psat,Pcr,Tsat,cAir,Const,Rair,Rtube,AiMod, &
									 LmodTube,LmodTP,LmodSH,MassLiqMod,MassVapMod,MassMod)
				  
					  MassMod=MassMod*NumOfChannels
					  Slab(I)%Pass(II)%Tube(III)%Seg(IV)%Mass=MassMod

					  !Total mass inventory
					  IF (J .GE. FirstTube .AND. J .LE. LastTube) THEN
						  MassCoil=MassCoil+MassMod*Slab(I)%Pass(II)%Ntube
						  MassLiqCoil=MassLiqCoil+MassLiqMod*Slab(I)%Pass(II)%Ntube
						  MassVapCoil=MassVapCoil+MassVapMod*Slab(I)%Pass(II)%Ntube
					  END IF
				
					  IF (xRiMod .EQ. -100) xRiMod=0.0
					  IF (xRiMod .EQ. 100) xRiMod=1.0
					  IF (xRoMod .EQ. -100) xRoMod=0.0
					  IF (xRoMod .EQ. 100) xRoMod=1.0

					  MassMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%Mass
					  WRITE(16,104)I,II,IV,tRiMod,tRoMod,pRiMod,pRoMod,hRiMod,hRoMod, &
								   xRiMod,xRoMod,tAiMod,tAoMod,rhAiMod,rhAoMod, &
								   hciMod*1000,hcoMod*1000,mu*1e6,kRef*1e3,cpRef,rhoRef,ReVap,ReLiq, &
								   Qmod*1000,MassLiqMod*1000,MassVapMod*1000,MassMod*1000, &
								   mRefMod*3600

				  END DO !end Nmod

			  END DO !end Ntube
  
		  END DO !end circuit

	  END DO !end Slab

  END IF

  CLOSE(16)

  100   FORMAT(50(A12,','))
  104   FORMAT(3(I3,','),50(F10.3,','))

  RETURN

END SUBROUTINE PrintCondenserResult

!************************************************************************

SUBROUTINE InitCondenserCoil(CoilType)

    !------------------------------------------------------------------------
    !Purpose:
    !To initialize condenser geometry and circuiting
    !
    !Author
    !Ipseng Iu
    !Oklahoma State Univerity, Stillwater
    !
    !Date
    !March 2005
    !
    !Reference:
    !none
    !
    !------------------------------------------------------------------------

    USE UnitConvertMod
    USE DataSimulation
    USE RefNameMod
    USE DataStopCodes
    USE InputProcessor

    IMPLICIT NONE

    INTEGER,INTENT(OUT) :: CoilType !1=Condenser; 2=Evaporator; 
    !3=High side interconnecting pipes; 
    !4=Low side interconnecting pipes
    !5=Microchannel condenser
    !6=Microchannel Evaporator

    INTEGER I,J,II,III,IV !Loop counter
    CHARACTER*150 LineData !One-line data in circuit file
    INTEGER NumRow !Row number
    INTEGER NumOfPasses !Number of passes
    INTEGER NumPass !Pass number
    INTEGER Ntubes !Number of tubes
    INTEGER NumOfInlets !Number of inlets
    LOGICAL IsSIunit !SI unit input flag
    LOGICAL IsNewFormat !New format input flag
    LOGICAL IsShift !Is shift tube flag (for staggered tubes)
    
    INTEGER, PARAMETER :: MaxNameLength = 200
    CHARACTER(len=MaxNameLength),DIMENSION(200) :: Alphas ! Reads string value from input file
        INTEGER :: NumAlphas               ! States which alpha value to read from a "Number" line
        REAL, DIMENSION(200) :: Numbers    ! brings in data from IP
        INTEGER :: NumNumbers              ! States which number value to read from a "Numbers" line
        INTEGER :: Status                  ! Either 1 "object found" or -1 "not found"
        CHARACTER(len=MaxNameLength)FinName
        CHARACTER(len=MaxNameLength)FinMaterial
        CHARACTER(len=MaxNameLength)TubeName
        
    !FLOW:

    NumOfSections=1 !ISI - 09/10/07

    !***** Get circuit info *****
    IF (IsCoolingMode .GT. 0) THEN
        OPEN (11,FILE='ODCckt.idf',IOSTAT=ErrorFlag,STATUS='OLD')
    ELSE
        OPEN (11,FILE='IDCckt.idf',IOSTAT=ErrorFlag,STATUS='OLD')
    END IF
    IF (ErrorFlag .NE. NOERROR) THEN 
        ErrorFlag=CKTFILEERROR
        !VL: Previously: GOTO 100
        CALL InitCondenserCoil_Helper_1
        RETURN
    END IF
    READ (11,202,IOSTAT=ErrorFlag)LineData
    IF (ErrorFlag .NE. NOERROR) THEN 
        ErrorFlag=CKTFILEERROR
        !VL: Previously: GOTO 100
        CALL InitCondenserCoil_Helper_1
        RETURN
    END IF

    IF (LineData(1:17) .EQ. 'Microchannel Coil') THEN
        CoilType = MCCONDENSER
    ELSE
        CoilType = CONDENSERCOIL
        IF (LineData(1:13) .EQ. 'Fin-Tube Coil') THEN
            IsNewFormat=.TRUE.
        ELSE
            IsNewFormat=.FALSE.
        END IF
    END IF

    IF (CoilType .EQ. CONDENSERCOIL) THEN !Fin-tube coil

        !IF (IsNewFormat) THEN

            !READ(11,*) !**************************** Geometry *************************************

              CALL GetObjectItem('Geometry',1,Alphas,NumAlphas, &
                        Numbers,NumNumbers,Status)   
            
            SELECT CASE (Alphas(1)(1:1))
            CASE ('F','f')
                IsSIunit=.FALSE.
            CASE ('T','t')
                IsSIunit=.TRUE.
            CASE DEFAULT
                !FAIL
            END SELECT
            
            !READ(12,202)LineData
            !I=SCAN(LineData,',')
            !LineData=ADJUSTL(LineData(I+1:150))
            !CASE ('F','f')
            !    IsSIunit=.FALSE.
            !CASE DEFAULT
            !    IsSIunit=.TRUE.
            !END SELECT

            !READ(11,202)LineData
            !I=SCAN(LineData,',')
            !LineData=ADJUSTL(LineData(I+1:150))
            !READ(LineData,*)FinType
            FinType = Numbers(1)

            !READ(11,*) !Fin name
            FinName = Alphas(2)

            !READ(11,202)LineData
            !I=SCAN(LineData,',')
            !LineData=ADJUSTL(LineData(I+1:150))
            !READ(LineData,*)FinPitch
            FinPitch = Numbers(2)
            Kfin = Numbers(3)
            FinThk = Numbers(4)
            
            FinMaterial = Alphas(3)
            
            TubeType = Numbers(5)
            
            TubeName = Alphas(4)
            
            ODTube = Numbers(6)
            IDTube = Numbers(7)
            KTube = Numbers(8)
            Pt = Numbers(9)
            Pl = Numbers(10)
            Nl = Numbers(11)
            Nt = Numbers(12)
            Ltube = Numbers(13)

            !READ(11,202)LineData
            !I=SCAN(LineData,',')
            !LineData=ADJUSTL(LineData(I+1:150))
            !READ(LineData,*)Kfin
            !
            !READ(11,202)LineData
            !I=SCAN(LineData,',')
            !LineData=ADJUSTL(LineData(I+1:150))
            !READ(LineData,*)FinThk
            !
            !READ(11,*) !Fin material
            !
            !READ(11,202)LineData
            !I=SCAN(LineData,',')
            !LineData=ADJUSTL(LineData(I+1:150))
            !READ(LineData,*)TubeType
            !
            !READ(11,*) !Tube name
            !
            !READ(11,202)LineData
            !I=SCAN(LineData,',')
            !LineData=ADJUSTL(LineData(I+1:150))
            !READ(LineData,*)ODtube
            !
            !READ(11,202)LineData
            !I=SCAN(LineData,',')
            !LineData=ADJUSTL(LineData(I+1:150))
            !READ(LineData,*)IDtube
            !
            !READ(11,202)LineData
            !I=SCAN(LineData,',')
            !LineData=ADJUSTL(LineData(I+1:150))
            !READ(LineData,*)Ktube
            !
            !READ(11,202)LineData
            !I=SCAN(LineData,',')
            !LineData=ADJUSTL(LineData(I+1:150))
            !READ(LineData,*)Pt
            !
            !READ(11,202)LineData
            !I=SCAN(LineData,',')
            !LineData=ADJUSTL(LineData(I+1:150))
            !READ(LineData,*)Pl
            !
            !READ(11,202)LineData
            !I=SCAN(LineData,',')
            !LineData=ADJUSTL(LineData(I+1:150))
            !READ(LineData,*)Nl
            !
            !READ(11,202)LineData
            !I=SCAN(LineData,',')
            !LineData=ADJUSTL(LineData(I+1:150))
            !READ(LineData,*)Nt
            !
            !READ(11,202)LineData
            !I=SCAN(LineData,',')
            !LineData=ADJUSTL(LineData(I+1:150))
            !READ(LineData,*)Ltube

            IF (Ltube .LE. 1e-3) THEN
                ErrorFlag=ZEROLENCOILERROR
                !VL: Previously: GOTO 100
                CALL InitCondenserCoil_Helper_1
                RETURN
            END IF

            !READ(11,202)LineData
            !I=SCAN(LineData,',')
            !LineData=ADJUSTL(LineData(I+1:150))
            !READ(LineData,*)NumOfMods
            !
            !READ(11,202)LineData
            !I=SCAN(LineData,',')
            !LineData=ADJUSTL(LineData(I+1:150))
            !READ(LineData,*)NumOfCkts
            !
            !READ(11,202)LineData
            NumOfMods = Numbers(14)
            NumOfCkts = Numbers(15)

            !IF (LineData(1:1) .EQ. 'N' .OR. LineData(1:1) .EQ. 'n') THEN !New format 06/06/07
            !    I=SCAN(LineData,',')
            !    LineData=ADJUSTL(LineData(I+1:150))
            !    READ(LineData,*)NumOfSCckts
            !
            !    READ(11,202)LineData
            !    I=SCAN(LineData,',')
            !    LineData=ADJUSTL(LineData(I+1:150))
            !    READ(LineData,*)IsShift
            !ELSE
            !    I=SCAN(LineData,',')
            !    LineData=ADJUSTL(LineData(I+1:150))
            !    READ(LineData,*)IsShift
            !END IF
            SELECT CASE (Alphas(5)(1:1))
            CASE ('F','f')
                IsShift=.FALSE.
            CASE ('T','t')
                IsShift=.TRUE.
            END SELECT
            
            !READ(11,202)LineData
            !IF (LineData(1:1) .EQ. 'N') THEN !Number of Sections, ISI - 09/10/07
            !    I=SCAN(LineData,',')
            !    LineData=ADJUSTL(LineData(I+1:150))
            !    READ(LineData,*)NumOfSections	    
            !    READ(11,202)LineData !*************************** Circuiting ************************************
            !END IF
            !READ(11,*) !*************************** Circuiting ************************************

            CALL FinTubeCoilUnitConvert(IsSIUnit,FinPitch,Kfin,FinThk, &
            ODtube,IDtube,Ktube,Pt,Pl,Ltube)

            TubeThk=(ODtube-IDtube)/2

            IF (IsShift) THEN
                ShiftTube = 1
            ELSE
                ShiftTube = 0
            END IF

        !ELSE !Old format
        !
        !    I=SCAN(LineData,',')
        !    DO J=1, 8
        !        LineData=ADJUSTL(LineData(I+1:150))
        !        I=SCAN(LineData,',')
        !    END DO
        !    READ(LineData,*)ShiftTube
        !    IF (ShiftTube .GT. 0) ShiftTube = 1
        !
        !    IDtube = ODtube-TubeThk*2
        !
        !END IF

        IF (Pl .EQ. 0) Pl = Pt

        READ (11,*,IOSTAT=ErrorFlag) !Branch#,#Tubes
        IF (ErrorFlag .NE. NOERROR) THEN 
            ErrorFlag=CKTFILEERROR
            !VL: Previously: GOTO 100
            CALL InitCondenserCoil_Helper_1
            RETURN
        END IF

        NumOfTubes=Nl*Nt

        !Fin spacing
        FinSpg = 1/FinPitch-FinThk

        !For plate finned tube
        FinHeight=0 !No use for plate finned tube
        TubeDepth=ODtube
        TubeHeight=ODtube
        NumOfChannels=1
        Dchannel=IDtube

        IF (FinSpg .LT. FinThk) THEN
            ErrorFlag=COILFINERROR
            !VL: Previously: GOTO 100
            CALL InitCondenserCoil_Helper_1
            RETURN
        END IF

        IF (Pt .LT. ODtube+2*FinThk) THEN
            ErrorFlag=COILTUBEERROR
            !VL: Previously: GOTO 100
            CALL InitCondenserCoil_Helper_1
            RETURN
        END IF

        IF (IsSimpleCoil .EQ. 1) THEN
            NumOfMods=3
            ALLOCATE(Ckt(NumOfCkts))	  
            DO I=1, NumOfCkts
                Ckt(I)%Ntube=1 !Initialize ISI - 12/03/06
                ALLOCATE(Ckt(I)%Tube(1))
                ALLOCATE(Ckt(I)%Tube(1)%Seg(NumOfMods))
            END DO
        ELSE

            !ISI - 09/10/07
            ALLOCATE(CoilSection(NumOfSections)) 

            ALLOCATE(Tube(NumOfTubes))
            ALLOCATE(Tube2D(Nl,Nt))
            ALLOCATE(JoinTubes(NumOfTubes))

            DO I=1, NumOfTubes
                ALLOCATE(Tube(I)%Seg(NumOfMods))
            END DO

            DO I=1, Nl
                DO J=1, Nt
                    ALLOCATE(Tube2D(I,J)%Seg(NumOfMods))
                END DO
            END DO

              CALL GetObjectItem('Circuiting_TubeNumbers',1,Alphas,NumAlphas, &
                        Numbers,NumNumbers,Status)  
            ALLOCATE(Ckt(NumOfCkts))
            ALLOCATE(mRefIter(NumOfCkts))
            DO I=1, NumOfCkts
                !READ(11,*,IOSTAT=ErrorFlag)Nckt,Ckt(I)%Ntube
                Ckt(I)%Ntube =Numbers(I)
                IF (ErrorFlag .NE. NOERROR) THEN 
                    ErrorFlag=CKTFILEERROR
                    !VL: Previously: GOTO 100
                    CALL InitCondenserCoil_Helper_1
                    RETURN
                END IF
                ALLOCATE(Ckt(I)%Tube(Ckt(I)%Ntube))
                ALLOCATE(Ckt(I)%TubeSequence(Ckt(I)%Ntube))
            END DO

            !Check if all circuit have the same number of tubes !ISI - 09/12/06
            IsSameNumOfTubes=.TRUE.	
            Ntubes=Ckt(1)%Ntube
            DO I=2, NumOfCkts
                IF (Ntubes .NE. Ckt(I)%Ntube) THEN
                    IsSameNumOfTubes=.FALSE.
                    EXIT	
                END IF
            END DO

            !READ(11,*,IOSTAT=ErrorFlag) !Branch#,Tube sequence...
            IF (ErrorFlag .NE. NOERROR) THEN 
                ErrorFlag=CKTFILEERROR
                !VL: Previously: GOTO 100
                CALL InitCondenserCoil_Helper_1
                RETURN
            END IF
              
            !DO I=1, NumOfCkts
                IF (IsCoolingMode .EQ. 1) THEN 
                !    READ(11,*,IOSTAT=ErrorFlag)Nckt,(Ckt(I)%TubeSequence(J),J=1,Ckt(I)%Ntube)
                DO J=1, Ckt(1)%Ntube
                    CALL GetObjectItem('Circuit1_TubeSequence',1,Alphas,NumAlphas, &
                            Numbers,NumNumbers,Status)
                
                        Ckt(1)%TubeSequence(J) = Numbers(J)
                END DO
                
                DO J=1, Ckt(2)%Ntube
                    CALL GetObjectItem('Circuit2_TubeSequence',1,Alphas,NumAlphas, &
                            Numbers,NumNumbers,Status)
                
                        Ckt(2)%TubeSequence(J) = Numbers(J)
                END DO
                
                DO J= 1, Ckt(3)%Ntube
                    CALL GetObjectItem('Circuit3_TubeSequence',1,Alphas,NumAlphas, &
                            Numbers,NumNumbers,Status)
                
                        Ckt(3)%TubeSequence(J) = Numbers(J)
                END DO
                
                DO J=1, Ckt(4)%Ntube
                    CALL GetObjectItem('Circuit4_TubeSequence',1,Alphas,NumAlphas, &
                            Numbers,NumNumbers,Status)
                
                        Ckt(4)%TubeSequence(J) = Numbers(J)
                END DO
                
                IF (NumofCkts .EQ. 6) THEN
                    
                DO J= 1, Ckt(5)%Ntube
                    CALL GetObjectItem('Circuit5_TubeSequence',1,Alphas,NumAlphas, &
                            Numbers,NumNumbers,Status)
                
                        Ckt(5)%TubeSequence(J) = Numbers(J)
                END DO
                
                DO J=1, Ckt(6)%Ntube
                    CALL GetObjectItem('Circuit6_TubeSequence',1,Alphas,NumAlphas, &
                            Numbers,NumNumbers,Status)
                
                        Ckt(6)%TubeSequence(J) = Numbers(J)
                END DO  
                END IF    
                        !ELSE
                !    READ(11,*,IOSTAT=ErrorFlag)Nckt,(Ckt(I)%TubeSequence(J),J=Ckt(I)%Ntube,1,-1)
                END IF
                IF (ErrorFlag .NE. NOERROR) THEN 
                    ErrorFlag=CKTFILEERROR
                    !VL: Previously: GOTO 100
                    CALL InitCondenserCoil_Helper_1
                    RETURN
                END IF
            !END DO

            DO I=1,2
                !READ(11,202)LineData !For section data, ISI - 09/10/07
                !READ(11,*,IOSTAT=ErrorFlag) !************************* Velocity Profile ********************************
                IF (ErrorFlag .NE. NOERROR) THEN  !Tube# ,velocity Deviation from mean value
                    ErrorFlag=CKTFILEERROR
                    !VL: Previously: GOTO 100
                    CALL InitCondenserCoil_Helper_1
                    RETURN
                END IF
            END DO

            !Section data, ISI - 09/10/07
            !IF (LineData(1:1) .EQ. 'S') THEN
                DO I=1, NumOfSections
                    !READ(11,*,IOSTAT=ErrorFlag)Nckt,CoilSection(I)%NumOfCkts,CoilSection(I)%IsInlet
                    
                    ALLOCATE(CoilSection(I)%CktNum(CoilSection(I)%NumOfCkts))
                END DO
                !READ(11,*) !Section#,Branch#
                CALL GetObjectItem('VelocityProfile',1,Alphas,NumAlphas, &
                            Numbers,NumNumbers,Status)

                DO I=1, NumOfSections
                    DO J=1, CoilSection(I)%NumofCkts
                        CoilSection(I)%CktNum(J)=Numbers(J)
                    END DO
                    !READ(11,*,IOSTAT=ErrorFlag)Nckt,(CoilSection(I)%CktNum(J),J=1,CoilSection(I)%NumOfCkts)
                END DO		  
                !READ(11,*,IOSTAT=ErrorFlag) !************************* Velocity Profile ********************************
                !READ(11,*,IOSTAT=ErrorFlag) !Tube# ,velocity Deviation from mean value
            !END IF

            IsUniformVelProfile=.TRUE.
            DO I=Nt*(Nl-1)+1,Nt*Nl !last row faces air inlet (Cross flow HX)
                !READ(11,*,IOSTAT=ErrorFlag)Ntube,(Tube(I)%Seg(J)%VelDev,J=1,NumOfMods)
                    DO J=1, NumOfMods
                        Tube(I)%Seg(J)%VelDev=Numbers(J)
                    END DO
                IF (ErrorFlag .NE. NOERROR) THEN 
                    ErrorFlag=CKTFILEERROR
                    !VL: Previously: GOTO 100
                    CALL InitCondenserCoil_Helper_1
                    RETURN
                END IF
                IF (IsUniformVelProfile) THEN
                    DO J=1,NumOfMods
                        IF (Tube(I)%Seg(J)%VelDev .NE. 1) THEN
                            IsUniformVelProfile=.FALSE.
                            EXIT
                        END IF
                    END DO
                END IF
            END DO

            !Synchronize 1-D and 2-D arrays
            DO I=1, Nl
                DO J=1, Nt
                    Tube2D(I,J)=Tube((I-1)*Nt+J)
                END DO
            END DO

            !Propagate velocity profile to suceeding rows
            DO I=Nl-1,1,-1
                DO J=1, Nt
                    DO k=1, NumOfMods
                        Tube2D(I,J)%Seg(k)%VelDev=Tube2D(I+1,J)%Seg(k)%VelDev
                    END DO
                END DO
            END DO

            IF (ErrorFlag .NE. NOERROR) THEN 
                ErrorFlag=CKTFILEERROR
                !VL: Previously: GOTO 100
                CALL InitCondenserCoil_Helper_1
                RETURN
            END IF

            !Determine inlet and outlet flags, split, joint or nothing
            !SubcoolingTube=0 !Subcooling node number
            Nnode=1
            DO I=1, NumOfCkts
                !Initialize
                Ckt(I)%InJoin=0
                Ckt(I)%InSplit=0
                Ckt(I)%OutJoin=0
                Ckt(I)%OutSplit=0
                DO J=1, NumOfCkts
                    IF (Ckt(I)%TubeSequence(1) .EQ. Ckt(J)%TubeSequence(Ckt(J)%Ntube)) THEN
                        Ckt(I)%InJoin=Ckt(I)%InJoin+1
                    END IF
                    IF (Ckt(I)%TubeSequence(1) .EQ. Ckt(J)%TubeSequence(1)) THEN
                        Ckt(I)%InSplit=Ckt(I)%InSplit+1
                    END IF
                    IF (Ckt(I)%TubeSequence(Ckt(I)%Ntube) .EQ. Ckt(J)%TubeSequence(Ckt(J)%Ntube)) THEN
                        Ckt(I)%OutJoin=Ckt(I)%OutJoin+1
                    END IF
                    IF (Ckt(I)%TubeSequence(Ckt(I)%Ntube) .EQ. Ckt(J)%TubeSequence(1)) THEN
                        Ckt(I)%OutSplit=Ckt(I)%OutSplit+1
                    END IF
                END DO
                !IF (Ckt(I)%InJoin .GT. 1 .OR. Ckt(I)%OutSplit .GT. 1) Nnode=Nnode+1 !To accout for split subcooling circuit, ISI - 06//05/07

                IF (Ckt(I)%InJoin .GT. 1 .OR. Ckt(I)%OutSplit .GT. 1) THEN
                    !			  	IF ((Ckt(I)%InJoin .GT. 1 .AND. Ckt(I)%InSplit .EQ. 1) .OR. &
                    !			  	    (Ckt(I)%InJoin .EQ. 1 .AND. Ckt(I)%InSplit .GT. 1)) THEN

                    Nnode=Nnode+1 

                    !			    END IF
                END IF
            END DO

            !NnodeSC=1
            !DO I=1, NumOfSCckts
            !Initialize
            !	  SCckt(I)%InJoin=0
            !	  SCckt(I)%InSplit=0
            !	  SCckt(I)%OutJoin=0
            !	  SCckt(I)%OutSplit=0
            !	  DO J=1, NumOfSCckts
            !		  IF (SCckt(I)%TubeSequence(1) .EQ. SCckt(J)%TubeSequence(SCckt(J)%Ntube)) THEN
            ! 				  SCckt(I)%InJoin=SCckt(I)%InJoin+1
            !		  END IF
            !		  IF (SCckt(I)%TubeSequence(1) .EQ. SCckt(J)%TubeSequence(1)) THEN
            !				  SCckt(I)%InSplit=SCckt(I)%InSplit+1
            !		  END IF
            !		  IF (SCckt(I)%TubeSequence(Ckt(I)%Ntube) .EQ. SCckt(J)%TubeSequence(SCckt(J)%Ntube)) THEN
            !				  SCckt(I)%OutJoin=Ckt(I)%OutJoin+1
            !		  END IF
            !		  IF (SCckt(I)%TubeSequence(SCckt(I)%Ntube) .EQ. SCckt(J)%TubeSequence(1)) THEN
            !				  SCckt(I)%OutSplit=SCckt(I)%OutSplit+1
            !		  END IF
            !	  END DO

            !	  IF (SCckt(I)%InJoin .GT. 1 .OR. Ckt(I)%OutSplit .GT. 1) THEN
            !	  	IF ((SCckt(I)%InJoin .GT. 1 .AND. SCckt(I)%InSplit .EQ. 1) .OR. &
            !	  	    (SCckt(I)%InJoin .EQ. 1 .AND. SCckt(I)%InSplit .GT. 1)) THEN

            !	  		NnodeSC=NnodeSC+1 

            !		    END IF
            !	  END IF
            !END DO


            ALLOCATE(Node(Nnode))
            !ALLOCATE(NodeSC(NnodeSC))

            !Find split and joint tube numbers
            J=0
            DO I=1, NumOfCkts
                !IF (Ckt(I)%InJoin .GT. 1 ) THEN !Not including subcooling part, ISI - 06//05/07
                IF (Ckt(I)%InJoin .GT. 1 .AND. Ckt(I)%TubeSequence(1) .NE. SubcoolingTube) THEN 
                    J=J+1
                    Node(J)%Num=Ckt(I)%TubeSequence(1)
                    !ELSEIF (Ckt(I)%OutSplit .GT. 1) THEN !Not including subcooling part, ISI - 06//05/07
                ELSEIF (Ckt(I)%OutSplit .GT. 1 .AND. Ckt(I)%TubeSequence(Ckt(I)%Ntube) .NE. SubcoolingTube) THEN 
                    J=J+1
                    Node(J)%Num=Ckt(I)%TubeSequence(Ckt(I)%Ntube)
                END IF
                IF (J .GT. Nnode) EXIT
            END DO

            Node(Nnode)%Num=0 !coil outlet
            !Node(Nnode)%Num=0=SubcoolingTube !ISI - 06//05/07

            !Find surrounding tubes
            DO I=1, Nl
                DO J=1, Nt
                    IF (FinType .EQ. 4 .OR. FinType .EQ. 7 .OR. FinType .EQ. 6) THEN
                        !Tube2D(I,J)%RowNum=I
                        Tube2D(I,J)%RowNum=Nl+1-I !Corrected by ISI 07/11/06
                    ELSE
                        Tube2D(I,J)%RowNum=0
                    END IF
                    IF (ShiftTube .EQ. 0) THEN
                        IF (MOD(I,2) .EQ. 1) THEN
                            Tube2D(I,J)%Fup=I*Nt+(J-1)
                            Tube2D(I,J)%Fdown=I*Nt+J
                            Tube2D(I,J)%Back=1
                            IF (MOD(I,2) .EQ. 1 .AND. J .EQ. 1) Tube2D(I,J)%Fup=0 !Odd row, first tube
                        ELSE
                            Tube2D(I,J)%Fup=I*Nt+J
                            Tube2D(I,J)%Fdown=I*Nt+(J+1)
                            Tube2D(I,J)%Back=1
                            IF (MOD(I,2) .EQ. 0 .AND. J .EQ. Nt) Tube2D(I,J)%Fdown=0 !even row, first tube
                        END IF
                    ELSE
                        IF (MOD(I,2) .EQ. 1) THEN
                            Tube2D(I,J)%Fup=I*Nt+J
                            Tube2D(I,J)%Fdown=I*Nt+(J+1)
                            Tube2D(I,J)%Back=1
                            IF (MOD(I,2) .EQ. 1 .AND. J .EQ. Nt) Tube2D(I,J)%Fdown=0 !Odd row, first tube
                        ELSE
                            Tube2D(I,J)%Fup=I*Nt+(J-1)
                            Tube2D(I,J)%Fdown=I*Nt+J
                            Tube2D(I,J)%Back=1
                            IF (MOD(I,2) .EQ. 0 .AND. J .EQ. 1) Tube2D(I,J)%Fup=0 !even row, first tube
                        END IF
                    END IF

                    IF (I .EQ. Nl) THEN
                        Tube2D(I,J)%Fup=0
                        Tube2D(I,J)%Fdown=0
                    END IF
                    IF (I .EQ. 1) Tube2D(I,J)%Back=0
                END DO !End of J
            END DO !End of I

            !Synchronize 1-D and 2-D arrays
            DO I=1, Nl
                DO J=1, Nt
                    Tube((I-1)*Nt+J)=Tube2D(I,J)
                END DO
            END DO

            !Find even tubes
            DO I=1, NumOfCkts

                !Find first and last simulation tubes
                FirstTube=1
                LastTube=Ckt(I)%Ntube
                IF (Ckt(I)%InSplit .GT. 1) THEN
                    FirstTube=2 !Skip first tube
                END IF 
                IF (Ckt(I)%OutJoin .GT. 1) THEN
                    LastTube=Ckt(I)%Ntube-1 !Ignore last tube
                END IF

                DO J=FirstTube, LastTube
                    TubeNum=Ckt(I)%TubeSequence(J)
                    Tube(TubeNum)%Even=0
                    IF (FirstTube .EQ. 2 ) THEN
                        IF (MOD(J,2) .EQ. 1) Tube(TubeNum)%Even=1
                    ELSE
                        IF (MOD(J,2) .EQ. 0) Tube(TubeNum)%Even=1
                    END IF
                END DO !End of J
            END DO !End of I

            !Find empty tubes
            Tube%Empty = 1 !Initialize 
            DO I=1, NumOfCkts
                DO J=1, Ckt(I)%Ntube
                    TubeNum=Ckt(I)%TubeSequence(J)
                    Tube(TubeNum)%Empty=0
                END DO
            END DO

            !Number of inlet circuits
            NcktFirst=0
            DO I=1, NumOfCkts
                IF (Ckt(I)%InJoin .LT. 1) NcktFirst=NcktFirst+1
            END DO
            IF (NcktFirst .EQ. 0) NcktFirst = 1 !At least one circuit, ISI - 07/28/06	  

            !Number of outlet circuits
            NcktLast=0
            DO I=1, NumOfCkts
                IF (Ckt(I)%OutSplit .EQ. 0) NcktLast=NcktLast+1
            END DO
            IF (NcktLast .EQ. 0) NcktLast = 1 !At least one circuit, ISI - 07/28/06

        END IF

    ELSE !Microchannel coil

        READ(11,*) !**************************** Geometry *************************************

        READ(11,202)LineData
        I=SCAN(LineData,',')
        LineData=ADJUSTL(LineData(I+1:150))
        SELECT CASE (LineData(1:1))
        CASE ('F','f')
            IsSIunit=.FALSE.
        CASE DEFAULT
            IsSIunit=.TRUE.
        END SELECT

        READ(11,202)LineData
        I=SCAN(LineData,',')
        LineData=ADJUSTL(LineData(I+1:150))
        READ(LineData,*)FinType

        READ(11,*) !Fin name

        READ(11,202)LineData
        I=SCAN(LineData,',')
        LineData=ADJUSTL(LineData(I+1:150))
        READ(LineData,*)FinPitch

        READ(11,202)LineData
        I=SCAN(LineData,',')
        LineData=ADJUSTL(LineData(I+1:150))
        READ(LineData,*)Kfin

        READ(11,202)LineData
        I=SCAN(LineData,',')
        LineData=ADJUSTL(LineData(I+1:150))
        READ(LineData,*)FinThk

        READ(11,*) !Fin material

        READ(11,202)LineData
        I=SCAN(LineData,',')
        LineData=ADJUSTL(LineData(I+1:150))
        READ(LineData,*)TubeType

        READ(11,*) !Tube name

        READ(11,202)LineData
        I=SCAN(LineData,',')
        LineData=ADJUSTL(LineData(I+1:150))
        READ(LineData,*)TubeHeight

        READ(11,202)LineData
        I=SCAN(LineData,',')
        LineData=ADJUSTL(LineData(I+1:150))
        READ(LineData,*)TubeDepth

        READ(11,202)LineData
        I=SCAN(LineData,',')
        LineData=ADJUSTL(LineData(I+1:150))
        READ(LineData,*)TubeThk

        READ(11,202)LineData
        I=SCAN(LineData,',')
        LineData=ADJUSTL(LineData(I+1:150))
        READ(LineData,*)Ktube

        READ(11,202)LineData
        I=SCAN(LineData,',')
        LineData=ADJUSTL(LineData(I+1:150))
        READ(LineData,*)Pt

        READ(11,202)LineData
        I=SCAN(LineData,',')
        LineData=ADJUSTL(LineData(I+1:150))
        READ(LineData,*)Nl

        READ(11,202)LineData
        I=SCAN(LineData,',')
        LineData=ADJUSTL(LineData(I+1:150))
        READ(LineData,*)Nt

        READ(11,202)LineData
        I=SCAN(LineData,',')
        LineData=ADJUSTL(LineData(I+1:150))
        READ(LineData,*)Ltube

        IF (Ltube .LE. 1e-3) THEN
            ErrorFlag=ZEROLENCOILERROR
            !VL: Previously: GOTO 100
            CALL InitCondenserCoil_Helper_1
            RETURN
        END IF

        TubeOrientation=HORIZONTAL !Default
        READ(11,202)LineData
        IF (LineData(1:1) .EQ. 'T' .OR. LineData(1:1) .EQ. 't') THEN !Tube Orientation, new format - 06/06/06

            I=SCAN(LineData,',')
            LineData=ADJUSTL(LineData(I+1:150))
            SELECT CASE (LineData(1:1))
            CASE ('V','v')
                TubeOrientation=VERTICAL
            CASE DEFAULT
                TubeOrientation=HORIZONTAL
            END SELECT

            READ(11,202)LineData
        END IF

        !READ(12,202)LineData
        I=SCAN(LineData,',') !Number of segments or modules
        LineData=ADJUSTL(LineData(I+1:150))
        READ(LineData,*)NumOfMods

        READ(11,202)LineData
        I=SCAN(LineData,',')
        LineData=ADJUSTL(LineData(I+1:150))
        READ(LineData,*)NumOfChannels

        READ(11,202)LineData
        I=SCAN(LineData,',')
        LineData=ADJUSTL(LineData(I+1:150))
        READ(LineData,*)Dchannel

        READ(11,*) !*************************** Circuiting ************************************

        ALLOCATE(Slab(Nl))
        READ(11,*) !Slab#,#Passes,Tubes per Pass
        DO I=1, Nl

            READ(11,202)LineData
            J=SCAN(LineData,',')
            LineData=ADJUSTL(LineData(J+1:150))
            READ(LineData,*)NumOfPasses
            ALLOCATE(Slab(I)%Pass(NumOfPasses))
            Slab(I)%Npass=NumOfPasses

            DO II=1, NumOfPasses

                J=SCAN(LineData,',')
                LineData=ADJUSTL(LineData(J+1:150))
                READ(LineData,*)Ntubes

                !cooling and heating are different flow direction - ISI 02/06/2009
                IF (IsCoolingMode .GT. 0) THEN
                    ALLOCATE(Slab(I)%Pass(II)%Tube(Ntubes))
                    Slab(I)%Pass(II)%Ntube=Ntubes

                    DO III=1, Ntubes
                        ALLOCATE(Slab(I)%Pass(II)%Tube(III)%Seg(NumOfMods))
                    END DO

                ELSE
                    ALLOCATE(Slab(I)%Pass(NumOfPasses-II+1)%Tube(Ntubes))
                    Slab(I)%Pass(NumOfPasses-II+1)%Ntube=Ntubes

                    DO III=1, Ntubes
                        ALLOCATE(Slab(I)%Pass(NumOfPasses-II+1)%Tube(III)%Seg(NumOfMods))
                    END DO

                END IF

            END DO

        END DO

        !Inlet pass
        READ(11,202)LineData !Slab#,#Inlets,Tubes per Inlet
        IF (LineData(1:1) .EQ. 'S' .OR. LineData(1:1) .EQ. 's') THEN !Inlet pass info

            DO I=1, Nl

                READ(11,202)LineData
                J=SCAN(LineData,',')
                LineData=ADJUSTL(LineData(J+1:150))
                READ(LineData,*)NumOfInlets
                ALLOCATE(Slab(I)%InletPass(NumOfInlets))
                Slab(I)%Ninlet=NumOfInlets

                DO II=1, NumOfInlets

                    J=SCAN(LineData,',')
                    LineData=ADJUSTL(LineData(J+1:150))
                    READ(LineData,*)Ntubes

                    IF (IsCoolingMode .GT. 0) THEN
                        Slab(I)%InletPass(II)%Ntube=Ntubes
                    ELSE !For heating, set it to equal number of inlet tubes, at least for now - ISI - 02/06/2009
                        Slab(I)%InletPass(II)%Ntube=Slab(I)%Pass(1)%Ntube
                    END IF

                END DO

            END DO

            READ(11,*) !************************* Velocity Profile ********************************

        ELSE

            NumOfInlets=1
            DO I=1, Nl

                ALLOCATE(Slab(I)%InletPass(NumOfInlets))
                Slab(I)%Ninlet=NumOfInlets

                DO II=1, NumOfInlets

                    Slab(I)%InletPass(II)%Ntube=Slab(I)%Pass(II)%Ntube

                END DO

            END DO
        END IF

        READ(11,*) !Tube# ,velocity Deviation from mean value

        IsUniformVelProfile=.TRUE.
        DO II=1,Slab(Nl)%Npass
            DO III=1,Slab(Nl)%Pass(II)%Ntube
                READ(11,*,IOSTAT=ErrorFlag)Ntube,(Slab(Nl)%Pass(II)%Tube(III)%Seg(IV)%VelDev,IV=1,NumOfMods)
                IF (IsUniformVelProfile) THEN
                    DO IV=1,NumOfMods
                        IF (Slab(Nl)%Pass(II)%Tube(III)%Seg(IV)%VelDev .NE. 1) THEN
                            IsUniformVelProfile=.FALSE.
                            EXIT
                        END IF
                    END DO
                END IF
            END DO
        END DO

        CALL MicroChannelCoilUnitConvert(IsSIUnit,FinPitch,Kfin,FinThk, &
        TubeHeight,TubeDepth,TubeThk,Ktube, &
        Pt,Ltube,Dchannel)

        ODtube=TubeHeight
        IDtube=Dchannel

        FinHeight=Pt-TubeHeight
        FinSpg=1/FinPitch-FinThk

    END IF

    CLOSE(11) !Circuit file

    !Discharge line info
    IDdisLn=ODdisLn-DisLnThk*2
    LmodDis=Ldisln !/NumOfMods
    AiModDis=PI*IDdisLn*LmodDis

    !liquid line info
    IDliqLn=ODliqLn-LiqLnThk*2
    LmodLiq=Lliqln !/NumOfMods
    AiModLiq=PI*IDliqLn*LmodLiq

    !***** Allocate pointer for discharge lines *****
    ALLOCATE(DisLnSeg(NumOfMods))

    !***** Allocate pointer for liquid lines *****
    ALLOCATE(LiqLnSeg(NumOfMods))

!VL: Previously: 100 CONTINUE


    ! VL: Code Chunck moved to subroutine InitCondenserCoil_Helper_1
    ! -----------------------------------------------------
    !IF (ErrorFlag .NE. NOERROR) THEN
    ! IF (ErrorFlag .EQ. CKTFILEERROR) THEN
    !  WRITE(*,*)'## ERROR ## Condenser: Circuit file error.'
    ! ELSEIF (ErrorFlag .EQ. COILTUBEERROR) THEN
    !  WRITE(*,*)'## ERROR ## Condenser: Coil geometry misdefined.'
    !  WRITE(*,*)'Tube spacing is less than tube diameter.'
    ! ELSEIF (ErrorFlag .EQ. COILFINERROR) THEN
    !  WRITE(*,*)'## ERROR ## Condenser: Coil geometry misdefined.'
    !  WRITE(*,*)'Fin spacing is less than fin thickness.'
    ! END IF
    !END IF
    ! -----------------------------------------------------

    CALL InitCondenserCoil_Helper_1

    RETURN

110 FORMAT(A150)  
202 FORMAT(A150)

    END SUBROUTINE InitCondenserCoil

SUBROUTINE InitCondenserCoil_Helper_1

    IMPLICIT NONE

  IF (ErrorFlag .NE. NOERROR) THEN
	  IF (ErrorFlag .EQ. CKTFILEERROR) THEN
		  WRITE(*,*)'## ERROR ## Condenser: Circuit file error.'
	  ELSEIF (ErrorFlag .EQ. COILTUBEERROR) THEN
		  WRITE(*,*)'## ERROR ## Condenser: Coil geometry misdefined.'
		  WRITE(*,*)'Tube spacing is less than tube diameter.'
	  ELSEIF (ErrorFlag .EQ. COILFINERROR) THEN
		  WRITE(*,*)'## ERROR ## Condenser: Coil geometry misdefined.'
		  WRITE(*,*)'Fin spacing is less than fin thickness.'
	  END IF
  END IF

END SUBROUTINE InitCondenserCoil_Helper_1

!************************************************************************

SUBROUTINE EndCondenserCoil

!------------------------------------------------------------------------
!Purpose:
!To free allocated arrays
!
!Author
!Ipseng Iu
!Oklahoma State Univerity, Stillwater
!
!Date
!March 2005
!
!Reference:
!none
!
!------------------------------------------------------------------------

IMPLICIT NONE

INTEGER I,II,III,IV,J,K !Loop counters

  IF (IsSimpleCoil .EQ. 1) THEN
	  DO I=1, NumOfCkts
		DEALLOCATE(Ckt(I)%Tube(1)%Seg)
		DEALLOCATE(Ckt(I)%Tube)
	  END DO
	  DEALLOCATE(Ckt)
	  IF (ALLOCATED(DisLnSeg)) DEALLOCATE(DisLnSeg) !Discharge line
	  IF (ALLOCATED(LiqLnSeg)) DEALLOCATE(LiqLnSeg) !Line line

	  RETURN
  END IF

  IF (NumOfChannels .GT. 1) THEN

	  IF (ALLOCATED(Slab)) THEN
		  DO I=1, Nl
			  IF (ErrorFlag .NE. CKTFILEERROR) THEN
				  DO II=1,Slab(I)%Npass
					  DO III=1,Slab(I)%Pass(II)%Ntube
						  DEALLOCATE(Slab(I)%Pass(II)%Tube(III)%Seg)
					  END DO !end tube
					  DEALLOCATE(Slab(I)%Pass(II)%Tube)
				  END DO !end pass
				  DEALLOCATE(Slab(I)%Pass) 
				  DEALLOCATE(Slab(I)%InletPass) 
			  END IF
		  END DO !end slab
		  DEALLOCATE(Slab)
	  END IF

  ELSE

	  IF (ErrorFlag .EQ. CKTFILEERROR) THEN
		  DO I=1, NumOfTubes
			  !IF (ALLOCATED(Tube(I)%Seg)) DEALLOCATE(Tube(I)%Seg)
			  DEALLOCATE(Tube(I)%Seg)
		  END DO
	  END IF
    
	  IF (ALLOCATED(Ckt)) THEN
		  DO I=1, NumOfCkts
			  IF (ErrorFlag .NE. CKTFILEERROR) THEN
				  DO J=1,Ckt(I)%Ntube
					  IF (Ckt(I)%OutJoin .LE. 1) THEN 
						  DEALLOCATE(Ckt(I)%Tube(J)%Seg)
					  END IF
				  END DO 
			  END IF
			  DEALLOCATE(Ckt(I)%Tube)
			  DEALLOCATE(Ckt(I)%TubeSequence)
		  END DO
		  DEALLOCATE(Ckt)
	  END IF
	  
	  !ISI - 09/10/07
	  IF (ALLOCATED(CoilSection)) THEN
	      DO I=1, NumOfSections
	          IF (ALLOCATED(CoilSection(I)%CktNum)) DEALLOCATE(CoilSection(I)%CktNum)
	      END DO
	      DEALLOCATE(CoilSection)
	  END IF

	  IF (ALLOCATED(Tube)) DEALLOCATE(Tube)
	  IF (ALLOCATED(Tube2D)) DEALLOCATE(Tube2D)
	  IF (ALLOCATED(JoinTubes)) DEALLOCATE(JoinTubes)
	  IF (ALLOCATED(mRefIter)) DEALLOCATE(mRefIter)

	  IF (ALLOCATED(Node)) DEALLOCATE(Node)

  END IF

  IF (ALLOCATED(DisLnSeg)) DEALLOCATE(DisLnSeg) !Discharge line
  IF (ALLOCATED(LiqLnSeg)) DEALLOCATE(LiqLnSeg) !Line line

  !FirstTime=.TRUE.

  RETURN

END SUBROUTINE EndCondenserCoil

!************************************************************************

SUBROUTINE DischargeLine

!------------------------------------------------------------------------
!Purpose:
!To calculation discharge line outlet conditions
!
!Author
!Ipseng Iu
!Oklahoma State Univerity, Stillwater
!
!Date
!March 2005
!
!Reference:
!Andrews, J.W. 2001. �Impact of refrigerant line length on system 
!efficiency in residential heating and cooling system using refrigerant 
!distributor.� BNL-68550, Brookhaven National Laboratory, Upton, NY. 
!
!------------------------------------------------------------------------

USE FluidProperties
USE CoilCalcMod

IMPLICIT NONE

!INTEGER,PARAMETER :: CoilType = 3  !1=Condenser; 2=Evaporator; 
!                                   !3=High side interconnecting pipes; 
!								   !4=Low side interconnecting pipes
!								   !5=Microchannel condenser

INTEGER CoilType !1=Condenser; 2=Evaporator; 
                 !3=High side interconnecting pipes; 4=Low side interconnecting pipes
INTEGER TubeType !1=Plain; 2=General Micro Fin; 3=Herringbone; 4=Crosshatch; 5=Herringbone w/crosshatch; 6=Turbo-A

REAL DPman !Manifold pressure drop, kPa

REAL Qloss !Discharge line heat loss, kW
REAL Tloss !Discharge line temperature loss, C

!FLOW:

	CoilType=HIGHSIDETUBE
	TubeType=SMOOTH

	MassDisLn=0

    IF (DTdisLn .NE. 0) THEN !Given discharge line temperature change
	    Pressure=pRoCmp*1000
	    Enthalpy=hRoCmp*1000
	    CpgRoCmp=PH(RefName, Pressure, Enthalpy, 'specificheat', RefrigIndex,RefPropErr)
        CpgRoCmp=CpgRoCmp/1000
        tRoCmp=PH(RefName, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
        IF (RefPropErr .GT. 0) THEN
		    WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 1869'
		    ErrorFlag=REFPROPERROR
	        RETURN
        END IF

		QdisLn=mRefTot*CpgRoCmp*(-DTdisLn)
	END IF

    !DO K=1,NumOfMods !ISI - 08/25/06
    DO K=1,1

        IF (K .EQ. 1) THEN
			!Equal to compressor outlet condition
			DisLnSeg(K)%pRi=pRoCmp
			DisLnSeg(K)%hRi=hRoCmp
        ELSE !Equal to outlet of previous module(section)
			DisLnSeg(K)%pRi=DisLnSeg(K-1)%pRo
			DisLnSeg(K)%hRi=DisLnSeg(K-1)%hRo
        END IF

		pRiMod=DisLnSeg(K)%pRi
		hRiMod=DisLnSeg(K)%hRi

		CALL CalcRefProperty(pRiMod,hRiMod,hfRiMod,hgRiMod,hfgRiMod,Psat,Tsat,tRiMod,xRiMod, &
			                 vRiMod,vfRiMod,vgRiMod,cpRiMod,cpfRiMod,cpgRiMod, &
							 muRiMod,mufRiMod,mugRiMod,kRiMod,kfRiMod,kgRiMod,SigmaMod)
		IF (ErrorFlag .GT. CONVERGEERROR) RETURN

        !Discharge line heat loss
!        Tloss = PI*LmodDis*ODdisLn*Hout*(tRiMod-tAoCoil)/(mRefTot*cpgRiMod)
!        Qloss = mRefTot*cpgRiMod*Tloss
!        QdisLn = QdisLn+Qloss

		hRoMod=-QdisLn/mRefTot+hRiMod 
		!hRoMod=-(QdisLn/NumOfMods)/mRefTot+hRiMod  !ISI - 08/25/06

		IF (xRiMod .GT. 1) THEN
			LmodTPratio=0
		ELSE
			LmodTPratio=1
		END IF

		!Find outlet ref. pressure
		CALL CalcSegmentRefOutletPressure(CoilType,TubeType,tRiMod,pRiMod,hgRiMod,hfRiMod, &
			  	                          hRiMod,hRoMod,xRiMod,vRiMod,vgRiMod,vfRiMod,mRefTot, &
										  muRiMod,mugRiMod,mufRiMod,SigmaMod,LmodDis,LmodTPratio, &
										  IDdisLn,ElevDisLn,LmodDis,DPrefMultiplier,pRoMod)
		IF (ErrorFlag .GT. CONVERGEERROR) RETURN

		pRoMod=pRoMod-AddDPdisLn
		!pRoMod=pRoMod-AddDPdisLn/NumOfMods !ISI - 08/25/06

		CALL CalcRefProperty(pRoMod,hRoMod,hfRoMod,hgRoMod,hfgRoMod,Psat,Tsat,tRoMod,xRoMod, &
							 vRoMod,vfRoMod,vgRoMod,cpRoMod,cpfRoMod,cpgRoMod, &
							 muRoMod,mufRoMod,mugRoMod,kRoMod,kfRoMod,kgRoMod,SigmaMod)
		IF (ErrorFlag .GT. CONVERGEERROR) RETURN

		mu=(muRiMod+muRoMod)/2

		CALL Inventory(RefName,CoilType,TubeType,IDdisLn,ktube,mRefTot,QdisLn,hgRoMod,hfRoMod,hRiMod,hRoMod, &
					   xRiMod,xRoMod,vRiMod,vRoMod,vgRimod,vfRimod,vgRomod,vfRomod, &
	                   muRoMod,mugRoMod,mufRoMod,kRoMod,kfRoMod,kgRoMod,CpRoMod,CpfRoMod,CpgRoMod, &
		  		       MolWeight,pRoMod,Psat,Pcr,Tsat,Cair,Const,Rair,Rtube,AiModDis, &
					   LmodDis,LmodTP,LmodSH,MassLiqMod,MassVapMod,MassMod)

		DisLnSeg(K)%Mass=MassMod
		DisLnSeg(K)%pRo=pRoMod
		DisLnSeg(K)%hRo=hRoMod

		MassDisLn=MassDisLn+MassMod

    END DO !End Nmod

	CALL manifold(CoilType,IDdisLn,mRefTot,xRiMod,vRiMod,vgRimod,vfRimod,mugRiMod,mufRiMod,dPman)
    !pRiCoil=DisLnSeg(NumOfMods)%pRo-DPman  !ISI - 08/25/06 
    !hRiCoil=DisLnSeg(NumOfMods)%hRo  !ISI - 08/25/06

    pRiCoil=DisLnSeg(1)%pRo-DPman
    hRiCoil=DisLnSeg(1)%hRo

	RETURN

END SUBROUTINE DischargeLine

!************************************************************************

SUBROUTINE LiquidLine

!------------------------------------------------------------------------
!Purpose:
!To calculation liquid line outlet conditions
!
!Author
!Ipseng Iu
!Oklahoma State Univerity, Stillwater
!
!Date
!March 2005
!
!Reference:
!Andrews, J.W. 2001. �Impact of refrigerant line length on system 
!efficiency in residential heating and cooling system using refrigerant 
!distributor.� BNL-68550, Brookhaven National Laboratory, Upton, NY. 
!
!------------------------------------------------------------------------

USE FluidProperties
USE CoilCalcMod

IMPLICIT NONE

!INTEGER,PARAMETER :: CoilType = 3  !1=Condenser; 2=Evaporator; 
!                                   !3=High side interconnecting pipes; 
!								   !4=Low side interconnecting pipes
!								   !5=Microchannel condenser

INTEGER CoilType !1=Condenser; 2=Evaporator; 
                 !3=High side interconnecting pipes; 4=Low side interconnecting pipes
INTEGER TubeType !1=Plain; 2=General Micro Fin; 3=Herringbone; 4=Crosshatch; 5=Herringbone w/crosshatch; 6=Turbo-A
INTEGER K !Loop counter !ISI - 08/25/06

REAL Qloss !Heat loss due to line length, kW
REAL Tloss !Temperature loss in liquid line, C
REAL Tambient !Ambient temperature, C

	CoilType=HIGHSIDETUBE
	TubeType=SMOOTH

	MassLiqLn=0
    IF (DTliqLn .NE. 0) THEN !Given liquid line temperature change
		QliqLn=mRefTot*CpfRoMod*(-DTliqLn)
	END IF

    !DO K=1,NumOfMods !ISI - 08/25/06 
    DO K=1,1 !ISI - 12/17/2009
 
		IF (K .EQ. 1) THEN
			!Equal to coil outlet condition
			LiqLnSeg(K)%pRi=pRoCoil
		    LiqLnSeg(K)%hRi=hRoCoil
		ELSE !Equal to outlet of previous module(section)
			LiqLnSeg(K)%pRi=LiqLnSeg(K-1)%pRo
			LiqLnSeg(K)%hRi=LiqLnSeg(K-1)%hRo
		END IF

		pRiMod=LiqLnSeg(K)%pRi
		hRiMod=LiqLnSeg(K)%hRi

		!Calculate inlet ref. property
		CALL CalcRefProperty(pRiMod,hRiMod,hfRiMod,hgRiMod,hfgRiMod,Psat,Tsat,tRiMod,xRiMod, &
			                 vRiMod,vfRiMod,vgRiMod,cpRiMod,cpfRiMod,cpgRiMod, &
							 muRiMod,mufRiMod,mugRiMod,kRiMod,kfRiMod,kgRiMod,SigmaMod)
		IF (ErrorFlag .GT. CONVERGEERROR) RETURN

        !Liquid line heat loss
!        IF (IsCoolingMode .GE. 1) THEN
!            Tambient=tAiEvp
!        ELSE
!            Tambient=tAiCoil
!        END IF
!        Tloss = PI*LmodLiq*ODliqLn*Hout*(tRiMod-Tambient)/(mRefTot*cpgRiMod)
!        Qloss = mRefTot*cpgRiMod*Tloss 
!        QliqLn = QliqLn+Qloss

		hRoMod=-QliqLn/mRefTot+hRiMod
		!hRoMod=-(QliqLn/NumOfMods)/mRefTot+hRiMod !ISI - 08/25/06 

		IF (xRiMod .GT. 0) THEN
			LmodTPratio=1
		ELSE
			LmodTPratio=0
		END IF

		!Find outlet ref. pressure
		CALL CalcSegmentRefOutletPressure(CoilType,TubeType,tRiMod,pRiMod,hgRiMod,hfRiMod, &
			  	                          hRiMod,hRoMod,xRiMod,vRiMod,vgRiMod,vfRiMod,mRefTot, &
										  muRiMod,mugRiMod,mufRiMod,SigmaMod,LmodLiq,LmodTPratio, &
										  IDliqLn,ElevLiqLn,LmodLiq,DPrefMultiplier,pRoMod)
		IF (ErrorFlag .GT. CONVERGEERROR) RETURN
		
		pRoMod=pRoMod-AddDPLiqLn
		!pRoMod=pRoMod-AddDPLiqLn/NumOfMods !ISI - 08/25/06

		!Calculate outlet ref. property
		CALL CalcRefProperty(pRoMod,hRoMod,hfRoMod,hgRoMod,hfgRoMod,Psat,Tsat,tRoMod,xRoMod, &
							 vRoMod,vfRoMod,vgRoMod,cpRoMod,cpfRoMod,cpgRoMod, &
							 muRoMod,mufRoMod,mugRoMod,kRoMod,kfRoMod,kgRoMod,SigmaMod)
		IF (ErrorFlag .GT. CONVERGEERROR) RETURN

		mu=(muRiMod+muRoMod)/2
		muf=(mufRiMod+mufRoMod)/2
		mug=(mugRiMod+mugRoMod)/2

		CALL Inventory(RefName,CoilType,TubeType,IDliqLn,ktube,mRefTot,QliqLn,hgRoMod,hfRoMod,hRiMod,hRoMod, &
	                   xRiMod,xRoMod,vRiMod,vRoMod,vgRimod,vfRimod,vgRomod,vfRomod, &
                       mu,mug,muf,kRoMod,kfRoMod,kgRoMod,CpRoMod,CpfRoMod,CpgRoMod, &
					   MolWeight,pRoMod,Psat,Pcr,Tsat,cAir,Const,Rair,Rtube,AiModLiq, &
					   Lmodliq,LmodTP,LmodSH,MassLiqMod,MassVapMod,MassMod)

		LiqLnSeg(K)%Mass=MassMod
		LiqLnSeg(K)%pRo=pRoMod
		LiqLnSeg(K)%hRo=hRoMod
		
		MassLiqLn=MassLiqLn+MassMod

    END DO !End Nmod

    !pRiExp=LiqLnSeg(NumOfMods)%pRo !ISI - 12/17/2009
    !hRiExp=LiqLnSeg(NumOfMods)%hRo !ISI - 12/17/2009

    pRiExp=LiqLnSeg(1)%pRo !ISI - 08/25/06 
    hRiExp=LiqLnSeg(1)%hRo !ISI - 08/25/06 

	RETURN
	
END SUBROUTINE LiquidLine

!************************************************************************

SUBROUTINE RefrigerantParameters(Ref$)

!------------------------------------------------------------------------
!Purpose:
!To set refrigerant parameters
!
!Author
!Ipseng Iu
!Oklahoma State Univerity, Stillwater
!
!Date
!March 2005
!
!Reference:
!none
!
!------------------------------------------------------------------------

USE FluidProperties

IMPLICIT NONE

  CHARACTER*80, INTENT(IN)  :: Ref$

  RefName=Ref$

  MolWeight=MW(RefName)*1000

  Tcr=Tcrit(RefName)+273.15
  Pcr=Pcrit(RefName)/1000

END SUBROUTINE RefrigerantParameters

!************************************************************************

SUBROUTINE InitBoundaryConditions(CoilType)

!------------------------------------------------------------------------
!Purpose:
!To initialize segment boundary conditions
!
!Author
!Ipseng Iu
!Oklahoma State Univerity, Stillwater
!
!Date
!March 2005
!
!Reference:
!none
!
!------------------------------------------------------------------------

USE FluidProperties
USE AirPropMod
USE CoilCalcMod
!USE ReversingValveMod

IMPLICIT NONE


INTEGER,INTENT(IN) :: CoilType     !1=Condenser; 2=Evaporator; 
                                   !3=High side interconnecting pipes; 
								   !4=Low side interconnecting pipes
								   !5=Microchannel condenser


!FLOW:

  !tAoCoil=tAiCoil

  !air side inlet conditions
  CPair=CPA(REAL(tAiCoil))
  Cair=mAiCoil*CPAir

  AirPropOpt=2
  AirProp(1)=tAiCoil
  AirProp(3)=rhAiCoil
  CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
  hAiCoil=AirProp(4)
  wbAiCoil=AirProp(5)

!  IF (IsCoolingMode .LT. 1) THEN
!      IF (DrawBlow .EQ. BLOWTHROUGH) THEN !Blow through
!		  tAiCoil=tAiCoil+PwrFan/Cair
!		  hAiCoil=hAiCoil+PwrFan/mAiCoil
!	  END IF
!	  IF (IsCmpInAirStream .NE. 0) THEN !Compressor in air stream
!		  tAiCoil=tAiCoil+QlossCmp/Cair
!		  hAiCoil=hAiCoil+QlossCmp/mAiCoil
!	  END IF
!  END IF

   IF (DrawBlow .EQ. BLOWTHROUGH) THEN !Blow through
	  tAiCoil=tAiCoil+PwrFan/Cair
	  hAiCoil=hAiCoil+PwrFan/mAiCoil
   END IF
   
   IF (IsCmpInAirStream .NE. 0) THEN !Compressor in air stream
	  tAiCoil=tAiCoil+QlossCmp/Cair
	  hAiCoil=hAiCoil+QlossCmp/mAiCoil
   END IF

  AirPropOpt=1
  AirProp(1)=tAiCoil
  AirProp(4)=hAiCoil
  CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
  DensityIn=AirProp(7)

  !Area calculations
  CALL CalcCoilarea(CoilType,Nl,Nt,Pt,Pl,TubeDepth, &
					Ltube,IDtube,TubeHeight,Dchannel,NumOfChannels, &
					FinThk,FinSpg,Lcoil,AfCoil, &
					AoCoil,AiCoil,AmCoil)

  AoMod=AoCoil*LmodTube/Lcoil
  AfMod=AfCoil*LmodTube/Lcoil
  AiMod=AiCoil*LmodTube/Lcoil
  AmMod=AmCoil*LmodTube/Lcoil

  !****** Discharge line calculation ******
  IF (LdisLn .GT. 0) THEN 
	  CALL DischargeLine
	  IF (ErrorFlag .GT. CONVERGEERROR) THEN
		  WRITE(*,*)'-- WARNING -- DischargeLine: Refprop error. Line 2216'
		  RETURN
	  END IF
  ELSE
	  pRiCoil=pRoCmp
	  hRiCoil=hRoCmp
  END IF

!  IF (SystemType .EQ. HEATPUMPUNIT) THEN !Heat pump
!    !Calculate reversing valve heat transfer and pressure drop
!    Pressure=pRoCmp*1000
!    Enthalpy=hRoCmp*1000
!    tRoCmp=PH(RefName, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
!    IF (RefPropErr .GT. 0) THEN
!        WRITE(*,*)'-- WARNING -- Evaporator: Refprop error. Line 2646'
!        ErrorFlag=REFPROPERROR
!	    RETURN
!    END IF
!
!	CALL DischargeHeatTransfer(mRefTot,tRoCmp,tRoEvp,hRoCmp,hRiCoil)
!  END IF

  Pressure=pRiCoil*1000
  Enthalpy=hRiCoil*1000
  tRiCoil=PH(RefName, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
  IF (RefPropErr .GT. 0) THEN
	  WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 3904'
	  ErrorFlag=REFPROPERROR
	  RETURN
  END IF
  xRiCoil=PH(RefName, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
  IF (RefPropErr .GT. 0) THEN
	  WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 3910'
	  ErrorFlag=REFPROPERROR
	  RETURN
  END IF

  IF (IsSimpleCoil .EQ. 1) THEN
	  !Initialize
	  DO I=1, NumOfCkts
		Ckt(I)%Tube(1)%Seg%Qmod=0
		Ckt(I)%Tube(1)%Seg%tAi=tAiCoil
		Ckt(I)%Tube(1)%Seg%wbAi=wbAiCoil
		Ckt(I)%Tube(1)%Seg%rhAi=rhAiCoil
		Ckt(I)%Tube(1)%Seg%tAo=tAiCoil
		Ckt(I)%Tube(1)%Seg%wbAo=wbAiCoil
		Ckt(I)%Tube(1)%Seg%rhAo=rhAiCoil
		Ckt(I)%Tube(1)%Seg%pRo=pRiCoil
		Ckt(I)%Tube(1)%Seg%hRo=hRiCoil
		Ckt(I)%Tube(1)%Seg%pRi=pRiCoil
		Ckt(I)%Tube(1)%Seg%hRi=hRiCoil

	    Ckt(I)%pRi=pRiCoil
	    Ckt(I)%pRo=pRiCoil
	    Ckt(I)%hRi=hRiCoil
	    Ckt(I)%hRo=hRiCoil
	    Ckt(I)%mRef=mRefTot/NumOfCkts
	  END DO

  ELSE
	  !Calc. air side mass flow rate at the front row
	  VelAvg=(mAiCoil/DensityIn)/Aface
	  !DO I=Nt*(Nl-1)+1,Nt*Nl !last row faces air inlet (Cross flow HX)
	  DO I=1,Nt*Nl 
		  DO J=1,NumOfMods
			  Tube(I)%Seg(J)%Aface=LmodTube/(Ltube*Nt)*Aface
			  Tube(I)%Seg(J)%mAi=mAiCoil*LmodTube/(Ltube*Nt)*Tube(I)%Seg(J)%VelDev 
  		  END DO
	  END DO

	  !Initialize boundary conditions
	  DO I=1, NumOfTubes
		  Tube(I)%Seg%Qmod=0
   		  Tube(I)%Seg%tAi=tAiCoil
		  Tube(I)%Seg%wbAi=wbAiCoil
		  Tube(I)%Seg%rhAi=rhAiCoil
		  Tube(I)%Seg%tAo=tAiCoil
		  Tube(I)%Seg%wbAo=wbAiCoil
		  Tube(I)%Seg%rhAo=rhAiCoil
		  Tube(I)%Seg%pRo=pRiCoil
		  Tube(I)%Seg%hRo=hRiCoil
		  Tube(I)%Seg%pRi=pRiCoil
		  Tube(I)%Seg%hRi=hRiCoil
		  Tube(I)%ID=IDtube !ISI - 06/05/07
		  Tube(I)%NumOfMods=NumOfMods !ISI - 06/05/07
	  END DO

	  !Synchronize boundary conditions from tube to circuits
	  DO I=1, NumOfCkts
		  DO J=1, Ckt(I)%Ntube
			  TubeNum=Ckt(I)%TubeSequence(J)
			  Ckt(I)%Tube(J)=Tube(TubeNum)
		  END DO
		  !Initialize
		  Ckt(I)%pRi=pRiCoil
		  Ckt(I)%pRo=pRiCoil
		  Ckt(I)%hRi=hRiCoil
		  Ckt(I)%hRo=hRiCoil
	  END DO

	  mRefIter=Ckt%mRef !Ref. flow rate iteration value


	  !if (mRefTotPrev.eq.0) then
	  !IF (FirstTime .EQ. 1) THEN !ISI - 12/22/06
			

		  DO I=1, NumOfCkts
  			
			IF (Ckt(I)%OutSplit .EQ. 0) THEN !Outlet circuit
				Ckt(I)%mRef=mRefTot/NcktLast
			ELSEIF (Ckt(I)%InJoin .EQ. 0) THEN !Inlet circuit
			   Ckt(I)%mRef=mRefTot/NcktFirst
			ELSEIF (Ckt(I)%InSplit .GT. 1) THEN !Split inlet
  				DO J=1, NumOfCkts
  					IF (Ckt(I)%TubeSequence(1) .EQ. Ckt(J)%TubeSequence(Ckt(J)%Ntube)) THEN
  						Ckt(I)%mRef=Ckt(J)%mRef/Ckt(I)%InSplit
  						EXIT !Found the split tube
  					END IF
				END DO
  			ELSE IF (Ckt(I)%InJoin .GT. 1) THEN !Joint inlet
  				DO J=1, NumOfCkts
  					IF (Ckt(J)%TubeSequence(Ckt(J)%Ntube) .EQ. Ckt(I)%TubeSequence(1)) THEN
  						Ckt(I)%mRef=Ckt(J)%mRef*Ckt(I)%InJoin
  						EXIT !Found the joined tube
  					END IF
  				END DO
  			ELSE
				Ckt(I)%mRef=mRefTot/NumOfCkts !to take care of one tube case, ISI - 07/28/06
			END IF

		  END DO
		 
	  !END IF !ISI - 03/13/07
	  
	  !else
	  !	  DO I=1, NumOfCkts
  			
	  !		IF (Ckt(I)%OutSplit .EQ. 0) THEN !Outlet circuit
	  !			Ckt(I)%mRef=Ckt(I)%mRef*mRefTot/mRefTotPrev
	  !		ELSEIF (Ckt(I)%InJoin .EQ. 0) THEN !Inlet circuit
	  !		   Ckt(I)%mRef=Ckt(I)%mRef*mRefTot/mRefTotPrev
	  !		ELSEIF (Ckt(I)%InSplit .GT. 1) THEN !Split inlet
  	  !			DO J=1, NumOfCkts
  	  !				IF (Ckt(I)%TubeSequence(1) .EQ. Ckt(J)%TubeSequence(Ckt(J)%Ntube)) THEN
  	  !					Ckt(I)%mRef=Ckt(J)%mRef/Ckt(I)%InSplit
  	  !					EXIT !Found the split tube
  	  !				END IF
	  !			END DO
  	  !		ELSE IF (Ckt(I)%InJoin .GT. 1) THEN !Joint inlet
  	  !			DO J=1, NumOfCkts
  	  !				IF (Ckt(J)%TubeSequence(Ckt(J)%Ntube) .EQ. Ckt(I)%TubeSequence(1)) THEN
  	  !					Ckt(I)%mRef=Ckt(J)%mRef*Ckt(I)%InJoin
  	  !					EXIT !Found the joined tube
  	  !				END IF
  	  !			END DO
  	  !		ELSE
	  !			Ckt(I)%mRef=mRefTot/NumOfCkts !to take care of one tube case, ISI - 07/28/06
	  !		END IF

	  !	  END DO
	  !endif
	  mRefTotPrev = MRefTot

  END IF
  
  RETURN

END SUBROUTINE InitBoundaryConditions

!************************************************************************

SUBROUTINE CalcCircuitRefInletConditions(I,II,CoilType)

!------------------------------------------------------------------------
!Purpose:
!To calculate circuit refrigerant inlet condition according to circuitry 
!
!Author
!Ipseng Iu
!Oklahoma State Univerity, Stillwater
!
!Date
!March 2005
!
!Reference:
!none
!
!------------------------------------------------------------------------

IMPLICIT NONE

INTEGER,INTENT(IN) :: I   !Slab number
INTEGER,INTENT(IN) :: II  !Circuit,pass number
!INTEGER,INTENT(IN) :: III !Tube number
!INTEGER,INTENT(IN) :: IV  !Segment number

INTEGER,INTENT(IN) :: CoilType   !1=Condenser; 2=Evaporator; 
                                 !3=High side interconnecting pipes; 
								 !4=Low side interconnecting pipes
								 !5=Microchannel condenser
								 !6=Microchannel Evaporator

REAL SumMref    !Sum of mdot ref.
REAL SumMrefHri !Sum of mdot ref x hri
INTEGER J,K !Loop counters

!FLOW:

	IF (IsSimpleCoil .EQ. 1) THEN
		Ckt(II)%pRi=pRiCoil
		Ckt(II)%hRi=hRiCoil
		RETURN
	END IF

	IF (CoilType .NE. MCCONDENSER) THEN

		IF (Ckt(II)%InSplit .GT. 1) THEN !Split inlet
			DO J=1, NumOfCkts
				IF (Ckt(J)%TubeSequence(Ckt(J)%Ntube) .EQ. Ckt(II)%TubeSequence(1)) THEN
					IF (Ckt(J)%OutJoin .LE. 1) THEN !No joint at outlet
						!Inlet conditions
						Ckt(II)%pRi=Ckt(J)%pRo
						Ckt(II)%hRi=Ckt(J)%hRo
					ELSE !Outlet has joint
						!Inlet conditions
						Ckt(II)%pRi=Ckt(J)%Tube(Ckt(J)%Ntube-1)%Seg(NumOfMods)%pRo
						Ckt(II)%hRi=Ckt(J)%Tube(Ckt(J)%Ntube-1)%Seg(NumOfMods)%hRo
					END IF
					EXIT !Found the split tube
				END IF
			END DO
		ELSE IF (Ckt(II)%InJoin .GT. 1) THEN !Joint inlet
			K=0
			Ckt(II)%pRi=0
			Ckt(II)%hRi=0
			SumMref=0
			SumMrefHri=0
			DO J=1, NumOfCkts
				IF (Ckt(J)%TubeSequence(Ckt(J)%Ntube) .EQ. Ckt(II)%TubeSequence(1)) THEN
					K=K+1
					!Inlet conditions
					Ckt(II)%pRi=Ckt(II)%pRi+Ckt(J)%pRo
					Ckt(II)%hRi=Ckt(II)%hRi+Ckt(J)%hRo
					JoinTubes(K)=J
					SumMref=SumMref+Ckt(J)%mRef
					SumMrefHri=SumMrefHri+Ckt(J)%mRef*Ckt(J)%hRo
					IF (K .EQ. Ckt(II)%InJoin) EXIT !Found all joined tubes
				END IF
			END DO
			!Calculate according to energy balance
			Ckt(II)%pRi=Ckt(II)%pRi/Ckt(II)%InJoin !Calc inlet pressure
			Ckt(II)%hRi=SumMrefHri/SumMref       !Calc inlet enthalpy
			!Ckt(I)%hRi=Ckt(I)%hRi/Ckt(I)%InJoin       !Calc inlet enthalpy

		ELSE !Coil inlet
			Ckt(II)%pRi=pRiCoil
			Ckt(II)%hRi=hRiCoil
		END IF

	ELSE

        IF (IsParallelSlabs) THEN
		    !ISI - 07/13/07
		    IF (II .EQ. 1) THEN !1st pass
			    Slab(I)%Pass(II)%pRi=pRiCoil
			    Slab(I)%Pass(II)%hRi=hRiCoil
		    ELSE
			    Slab(I)%Pass(II)%pRi=Slab(I)%Pass(II-1)%pRo
			    Slab(I)%Pass(II)%hRi=Slab(I)%Pass(II-1)%hRo
		    END IF
        
        ELSE !Series

		    IF (I .EQ. 1) THEN !1st slab
		      IF (II .EQ. 1) THEN !1st pass
			      Slab(I)%Pass(II)%pRi=pRiCoil
			      Slab(I)%Pass(II)%hRi=hRiCoil
		      ELSE
			      Slab(I)%Pass(II)%pRi=Slab(I)%Pass(II-1)%pRo
			      Slab(I)%Pass(II)%hRi=Slab(I)%Pass(II-1)%hRo
		      END IF
	        ELSE
		      IF (II .EQ. 1) THEN !1st pass
			      Slab(I)%Pass(II)%pRi=Slab(I-1)%Pass(Slab(I)%Npass)%pRo
			      Slab(I)%Pass(II)%hRi=Slab(I-1)%Pass(Slab(I)%Npass)%hRo
		      ELSE
			      Slab(I)%Pass(II)%pRi=Slab(I)%Pass(II-1)%pRo
			      Slab(I)%Pass(II)%hRi=Slab(I)%Pass(II-1)%hRo
		      END IF
	        END IF

        END IF
        
	END IF

RETURN

END SUBROUTINE CalcCircuitRefInletConditions

!************************************************************************

SUBROUTINE CalcCoilSegment(I,II,III,IV,CoilType)

!------------------------------------------------------------------------
!Purpose:
!To perform heat exchanger calculation for a segment
!
!Author
!Ipseng Iu
!Oklahoma State Univerity, Stillwater
!
!Date
!March 2005
!
!Reference:
!none
!
!------------------------------------------------------------------------

USE FluidProperties
USE CoilCalcMod
USE AirPropMod

IMPLICIT NONE

INTEGER,INTENT(IN) :: I   !Slab number
INTEGER,INTENT(IN) :: II  !Circuit,pass number
INTEGER,INTENT(IN) :: III !Tube number
INTEGER,INTENT(IN) :: IV  !Segment number

INTEGER,INTENT(IN) :: CoilType   !1=Condenser; 2=Evaporator; 
                                 !3=High side interconnecting pipes; 
								 !4=Low side interconnecting pipes
								 !5=Microchannel condenser
								 !6=Microchannel Evaporator

!FLOW:

	IF (CoilType .NE. MCCONDENSER) THEN

		Ckt(II)%Tube(III)%Seg(IV)%Len=LmodTube

		IF (IsSimpleCoil .EQ. 1) THEN
!		    IF ((LmodTube .LT. SMALL) .OR. & !For zero length
!			    (IV .EQ. 1 .AND. xRoMod .LT. 1)) THEN !For two-phase inlet condition

		    IF (LmodTube .LT. SMALL) THEN !For zero length, ISI - 02/08/08

				Ckt(II)%Tube(III)%Seg(IV)%Len=0
				Ckt(II)%Tube(III)%Seg(IV)%Qmod=0
				Ckt(II)%Tube(III)%Seg(IV)%pRo=pRoMod
				Ckt(II)%Tube(III)%Seg(IV)%hRo=hRoMod
				Ckt(II)%Tube(III)%Seg(IV)%tAo=tAoMod
				Ckt(II)%Tube(III)%Seg(IV)%rhAo=rhAoMod
				Ckt(II)%Tube(III)%Seg(IV)%wbAo=wbAoMod

				Ckt(II)%Tube(III)%Seg(IV)%hci=0
				Ckt(II)%Tube(III)%Seg(IV)%EFref=0
				Ckt(II)%Tube(III)%Seg(IV)%hco=0

				Ckt(II)%Tube(III)%Seg(IV)%ReVap=0
				Ckt(II)%Tube(III)%Seg(IV)%ReLiq=0

				Ckt(II)%Tube(III)%Seg(IV)%cAir=0
				Ckt(II)%Tube(III)%Seg(IV)%Rair=0
				Ckt(II)%Tube(III)%Seg(IV)%Rtube=0
				
				!Surface temperature
				Ckt(II)%Tube(III)%Seg(IV)%tSi=0
				Ckt(II)%Tube(III)%Seg(IV)%tSo=0

				RETURN
			END IF
		END IF

		CALL CalcSegmentRefInletConditions(II,II,III,IV,CoilType)
							  
		CALL CalcSegmentAirInletConditions(II,II,III,IV,CoilType)
		IF (ErrorFlag .GT. CONVERGEERROR) RETURN

		mAiMod=Ckt(II)%Tube(III)%Seg(IV)%mAi
		tAiMod=Ckt(II)%Tube(III)%Seg(IV)%tAi
		rhAiMod=Ckt(II)%Tube(III)%Seg(IV)%rhAi

		WetFlag=0
		IF (IsSimpleCoil .EQ. 1) THEN
		    RowNum=0
			IF (FinType .EQ. 4) FinType=3 !Use regular louver fin correlation, ISI - 02/12/08
		ELSE
			RowNum=Ckt(II)%Tube(III)%RowNum
		END IF
		IF (RowNum .EQ. 0) THEN
			CALL AirSideCalc(CoilType,FinType,WetFlag,Nl,Nt,RowNum,tAiCoil,mAiCoil,DensityIn,DensityIn,Pt,Pl,Ltube,HtCoil, &
							 IDtube,ODtube,NumOfChannels,Dchannel,TubeHeight,TubeDepth,FinThk,FinSpg,CurveUnit,CurveTypeHTC,PowerAHTC,PowerBHTC, &
							 Poly1HTC,Poly2HTC,Poly3HTC,Poly4HTC,CurveTypeDP,PowerADP,PowerBDP, &
							 Poly1DP,Poly2DP,Poly3DP,Poly4DP,Lcoil,AfCoil,AoCoil,AiCoil,FaceVel,hco,DPair)  
		ELSE
			CALL AirSideCalc(CoilType,FinType,WetFlag,Nl,Nt,RowNum,tAiMod,mAiCoil,DensityIn,DensityIn,Pt,Pl,Ltube,HtCoil, &
							 IDtube,ODtube,NumOfChannels,Dchannel,TubeHeight,TubeDepth,FinThk,FinSpg,CurveUnit,CurveTypeHTC,PowerAHTC,PowerBHTC, &
							 Poly1HTC,Poly2HTC,Poly3HTC,Poly4HTC,CurveTypeDP,PowerADP,PowerBDP, &
							 Poly1DP,Poly2DP,Poly3DP,Poly4DP,Lcoil,AfCoil,AoCoil,AiCoil,FaceVel,hco,DPair)
		END IF
	    AoMod=AoCoil*LmodTube/Lcoil
		AfMod=AfCoil*LmodTube/Lcoil
		AiMod=AiCoil*LmodTube/Lcoil
		AmMod=AmCoil*LmodTube/Lcoil

		hco=hco*hcoMultiplier
		DPair=DPair*DPairMultiplier

		hcoMod=Ckt(II)%Tube(III)%Seg(IV)%VelDev*hco !*LmodTube/Lcoil
        !hcoMod=hco

		Ckt(II)%Tube(III)%Seg(IV)%hco=hcoMod

		AirPropOpt=2
		AirProp(1)=Ckt(II)%Tube(III)%Seg(IV)%tAi
		AirProp(3)=Ckt(II)%Tube(III)%Seg(IV)%rhAi
		CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
		hAiMod=AirProp(4)

		mRefMod=Ckt(II)%mRef
		pRiMod=Ckt(II)%Tube(III)%Seg(IV)%pRi
		hRiMod=Ckt(II)%Tube(III)%Seg(IV)%hRi

		CALL CalcRefProperty(pRiMod,hRiMod,hfRiMod,hgRiMod,hfgRiMod,Psat,Tsat,tRiMod,xRiMod, &
							 vRiMod,vfRiMod,vgRiMod,cpRiMod,cpfRiMod,cpgRiMod, &
							 muRiMod,mufRiMod,mugRiMod,kRiMod,kfRiMod,kgRiMod,SigmaMod)
		IF (ErrorFlag .GT. CONVERGEERROR) RETURN

		IF (IsSimpleCoil .EQ. 1) THEN
			IF (IV .EQ. 2) THEN
				hRiMod=hgRiMod*0.999 !Perturb a little to make sure it is in single phase region
			ELSEIF (IV .EQ. 3) THEN
				hRiMod=hfRiMod*0.999 !Perturb a little to make sure it is in single phase region
			END IF

			IF (IV .EQ. 2 .OR. IV .EQ. 3) THEN
				CALL CalcRefProperty(pRiMod,hRiMod,hfRiMod,hgRiMod,hfgRiMod,Psat,Tsat,tRiMod,xRiMod, &
									 vRiMod,vfRiMod,vgRiMod,cpRiMod,cpfRiMod,cpgRiMod, &
									 muRiMod,mufRiMod,mugRiMod,kRiMod,kfRiMod,kgRiMod,SigmaMod)
				IF (ErrorFlag .GT. CONVERGEERROR) RETURN
			END IF
		END IF

		tAoMod=Ckt(II)%Tube(III)%Seg(IV)%tAo
		CALL CalcMeanProp(tAiMod,tAoMod,tAmod)

		CALL CalcSegmentOutletConditions(II,II,III,IV,CoilType)
		IF (ErrorFlag .GT. CONVERGEERROR) RETURN

		QmodPrev=Qmod
		Ckt(II)%Tube(III)%Seg(IV)%mAi=mAiMod !ISI - 12/05/06
		Ckt(II)%Tube(III)%Seg(IV)%Len=LmodTube
		Ckt(II)%Tube(III)%Seg(IV)%Qmod=Qmod
		Ckt(II)%Tube(III)%Seg(IV)%pRo=pRoMod
		Ckt(II)%Tube(III)%Seg(IV)%hRo=hRoMod
		Ckt(II)%Tube(III)%Seg(IV)%tAo=tAoMod
		Ckt(II)%Tube(III)%Seg(IV)%rhAo=rhAoMod
		Ckt(II)%Tube(III)%Seg(IV)%wbAo=wbAoMod

		Ckt(II)%Tube(III)%Seg(IV)%hci=hciMod
		Ckt(II)%Tube(III)%Seg(IV)%EFref=EFref
		Ckt(II)%Tube(III)%Seg(IV)%hco=hcoMod

		IF (xRmod .GE. 1) THEN
			Ckt(II)%Tube(III)%Seg(IV)%ReVap=ReVap
			Ckt(II)%Tube(III)%Seg(IV)%ReLiq=0
		ELSE IF (xRmod .LE. 0) THEN
			Ckt(II)%Tube(III)%Seg(IV)%ReVap=0
			Ckt(II)%Tube(III)%Seg(IV)%ReLiq=ReLiq
		ELSE
			Ckt(II)%Tube(III)%Seg(IV)%ReVap=ReVap
			Ckt(II)%Tube(III)%Seg(IV)%ReLiq=ReLiq
		END IF

		Ckt(II)%Tube(III)%Seg(IV)%cAir=cAir
		Ckt(II)%Tube(III)%Seg(IV)%Rair=Rair
		Ckt(II)%Tube(III)%Seg(IV)%Rtube=Rtube
		
		!Surface temperature
		Ckt(II)%Tube(III)%Seg(IV)%tSi=tAiMod+ABS(Qmod)*Rair
		Ckt(II)%Tube(III)%Seg(IV)%tSo=tAoMod+ABS(Qmod)*Rair

		IF (IsSimpleCoil .NE. 1) CALL UpdateTubeDataFromCircuitData(II,III)

	ELSE

		Slab(I)%Pass(II)%Tube(III)%Seg(IV)%Len=LmodTube
					  		              
		CALL CalcSegmentRefInletConditions(I,II,III,IV,CoilType)
					  
		CALL CalcSegmentAirInletConditions(I,II,III,IV,CoilType)
		IF (ErrorFlag .GT. CONVERGEERROR) RETURN

		mAiMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%mAi !*Slab(I)%Pass(II)%Ntube
		tAiMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%tAi
		rhAiMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%rhAi

		WetFlag=0
		RowNum=0 !Ckt(I)%Tube(J)%RowNum
		CALL AirSideCalc(CoilType,FinType,WetFlag,Nl,Nt,RowNum,tAiMod,mAiCoil,DensityIn,DensityIn,Pt,Pl,Ltube,HtCoil, &
		 				 IDtube,ODtube,NumOfChannels,Dchannel,TubeHeight,TubeDepth,FinThk,FinSpg,CurveUnit,CurveTypeHTC,PowerAHTC,PowerBHTC, &
					  	 Poly1HTC,Poly2HTC,Poly3HTC,Poly4HTC,CurveTypeDP,PowerADP,PowerBDP, &
					  	 Poly1DP,Poly2DP,Poly3DP,Poly4DP,Lcoil,AfCoil,AoCoil,AiCoil,FaceVel,hco,DPair)
					   					  
		hco=hco*hcoMultiplier
	    DPair=DPair*DPairMultiplier

		hcoMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%VelDev*hco
        !hcoMod=hco

		Slab(I)%Pass(II)%Tube(III)%Seg(IV)%hco=hcoMod

		AirPropOpt=2
		AirProp(1)=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%tAi
		AirProp(3)=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%rhAi
		CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
		hAiMod=AirProp(4)

		pRiMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%pRi
		hRiMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%hRi

		CALL CalcRefProperty(pRiMod,hRiMod,hfRiMod,hgRiMod,hfgRiMod,Psat,Tsat,tRiMod,xRiMod, &
						   vRiMod,vfRiMod,vgRiMod,cpRiMod,cpfRiMod,cpgRiMod, &
						   muRiMod,mufRiMod,mugRiMod,kRiMod,kfRiMod,kgRiMod,SigmaMod)
		IF (ErrorFlag .GT. CONVERGEERROR) RETURN

		tAoMod=Slab(I)%Pass(II)%Tube(III)%Seg(IV)%tAo
		CALL CalcMeanProp(tAiMod,tAoMod,tAmod)

		CALL CalcSegmentOutletConditions(I,II,III,IV,CoilType)
		IF (ErrorFlag .GT. CONVERGEERROR) RETURN
	
		QmodPrev=Qmod
		Slab(I)%Pass(II)%Tube(III)%Seg(IV)%Qmod=Qmod
		Slab(I)%Pass(II)%Tube(III)%Seg(IV)%pRo=pRoMod
		Slab(I)%Pass(II)%Tube(III)%Seg(IV)%hRo=hRoMod
		Slab(I)%Pass(II)%Tube(III)%Seg(IV)%tAo=tAoMod
		Slab(I)%Pass(II)%Tube(III)%Seg(IV)%rhAo=rhAoMod
		Slab(I)%Pass(II)%Tube(III)%Seg(IV)%wbAo=wbAoMod
		Slab(I)%Pass(II)%Tube(III)%Seg(IV)%cAir=cAir
		Slab(I)%Pass(II)%Tube(III)%Seg(IV)%Rair=Rair
		Slab(I)%Pass(II)%Tube(III)%Seg(IV)%Rtube=Rtube
		IF (xRmod .GE. 1) THEN
		    Slab(I)%Pass(II)%Tube(III)%Seg(IV)%ReVap=ReVap
		    Slab(I)%Pass(II)%Tube(III)%Seg(IV)%ReLiq=0
		ELSE IF (xRmod .LE. 0) THEN
		    Slab(I)%Pass(II)%Tube(III)%Seg(IV)%ReVap=0
		    Slab(I)%Pass(II)%Tube(III)%Seg(IV)%ReLiq=ReLiq
		ELSE
		    Slab(I)%Pass(II)%Tube(III)%Seg(IV)%ReVap=ReVap
		    Slab(I)%Pass(II)%Tube(III)%Seg(IV)%ReLiq=ReLiq
		END IF
		Slab(I)%Pass(II)%Tube(III)%Seg(IV)%hci=hciMod
		Slab(I)%Pass(II)%Tube(III)%Seg(IV)%EFref=EFref
		Slab(I)%Pass(II)%Tube(III)%Seg(IV)%hco=hcoMod

	END IF

RETURN

END SUBROUTINE CalcCoilSegment

!************************************************************************

SUBROUTINE CalcSegmentAirInletConditions(I,II,III,IV,CoilType)

!------------------------------------------------------------------------
!Purpose:
!To calculate inlet air temp and relative humidity
!
!Author
!Ipseng Iu
!Oklahoma State Univerity, Stillwater
!
!Date
!March 2005
!
!Reference:
!none
!
!------------------------------------------------------------------------

IMPLICIT NONE

INTEGER,INTENT(IN) :: I   !Slab number
INTEGER,INTENT(IN) :: II  !Circuit,pass number
INTEGER,INTENT(IN) :: III !Tube number
INTEGER,INTENT(IN) :: IV  !Segment number

INTEGER,INTENT(IN) :: CoilType   !1=Condenser; 2=Evaporator; 
                                 !3=High side interconnecting pipes; 
								 !4=Low side interconnecting pipes
								 !5=Microchannel condenser
								 !6=Microchannel evaporator

REAL tAiFavg   !Average front tube inlet air temp. C
REAL tAoFavg   !Average front tube outlet air temp. C
REAL wbAiFavg  !Average front tube inlet wet bulb temp. C
REAL wbAoFavg  !Average front tube outlet wet bulb temp. C
REAL rhAiFavg  !Average front tube inlet RH
REAL rhAoFavg  !Average front tube outlet RH
REAL tAiFup    !Upper front tube inlet air temp. C
REAL tAiFdown  !Lower front tube inlet air temp. C
REAL wbAiFup   !Upper front tube inlet air wet bulb temp. C
REAL wbAiFdown !Lower front tube inlet air wet bulb temp. C
REAL rhAiFup   !Upper front tube inlet air humidity
REAL rhAiFdown !Lower front tube inlet air humidity
REAL tAoFup    !Upper front tube outlet air temp. C
REAL tAoFdown  !Lower front tube outlet air temp. C
REAL wbAoFup   !Upper front tube outlet air wet bulb temp. C
REAL wbAoFdown !Lower front tube outlet air wet bulb temp. C
REAL rhAoFup   !Upper front tube outlet air humidity
REAL rhAoFdown !Lower front tube outlet air humidity
REAL mAiFup    !Upper front tube inlet air mass flow rate, kg/s
REAL mAiFdown  !Lower front tube inlet air mass flow rate, kg/s
REAL VelDevFup    !Upper front tube Velocity deviation
REAL VelDevFdown  !Lower front tube Velocity deviation

!FLOW:

    IF (IsSimpleCoil .EQ. 1) THEN
		Ckt(II)%Tube(III)%Seg(IV)%tAi=tAiCoil
		Ckt(II)%Tube(III)%Seg(IV)%rhAi=rhAiCoil
		Ckt(II)%Tube(III)%Seg(IV)%VelDev=1
		!Ckt(II)%Tube(III)%Seg(IV)%mAi=mAiCoil/NumOfCkts !ISI - 12/05/06
		Ckt(II)%Tube(III)%Seg(IV)%mAi=mAiCoil*Lmodtube/Lcoil !ISI - 12/05/06
		mAiMod=Ckt(II)%Tube(III)%Seg(IV)%mAi

		RETURN
	END IF

	IF (CoilType .NE. MCCONDENSER) THEN
		IF (Tube(TubeNum)%Fup .NE. 0) THEN !Upper front tubes
			IF (Tube(TubeNum)%Even .EQ. 0) THEN !Odd tubes
				IF (Tube(Tube(TubeNum)%Fup)%Empty .NE. 0) THEN !Empty tubes

					IF (Tube(TubeNum)%Fdown .EQ. 0) THEN !Coil bottom, ISI - 07/29/07

                        IF (Tube(Tube(TubeNum)%Fup)%Fdown .EQ. 0) THEN
						    tAoFdown=tAiCoil
						    rhAoFdown=rhAiCoil
						    tAiFdown=tAiCoil
						    rhAiFdown=rhAiCoil
						    mAiFdown=Ckt(II)%Tube(III)%Seg(IV)%mAi
						    VelDevFdown=Ckt(II)%Tube(III)%Seg(IV)%VelDev						
                        ELSE
						    tAoFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(IV)%tAo
						    rhAoFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(IV)%rhAo
						    tAiFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(IV)%tAi
						    rhAiFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(IV)%rhAi
						    mAiFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(IV)%mAi
						    VelDevFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(IV)%VelDev !Velocity deviation
                        END IF
                        
					ELSE IF (Tube(Tube(TubeNum)%Fdown)%Fup .NE. 0) THEN
						tAoFup=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(IV)%tAo   !Temperature 
						rhAoFup=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(IV)%rhAo !Relative humidity
						!wbAoFup=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(K)%wbAo !Wet bulb temp.
						tAiFup=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(IV)%tAi   !Temperature 
						rhAiFup=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(IV)%rhAi !Relative humidity
						!wbAiFup=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(K)%wbAi !Wet bulb temp.
						mAiFup=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(IV)%mAi   !mass flow rate
						VelDevFup=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(IV)%VelDev !Velocity deviation
					ELSE !Frontal tubes
						tAoFup=tAiCoil
 						rhAoFup=rhAiCoil
						!wbAoFup=wbAiCoil
						tAiFup=tAiCoil
						rhAiFup=rhAiCoil
						!wbAiFup=wbAiCoil
						mAiFup=Ckt(II)%Tube(III)%Seg(IV)%mAi
						VelDevFup=Ckt(II)%Tube(III)%Seg(IV)%VelDev
					END IF
				ELSE
					tAoFup=Tube(Tube(TubeNum)%Fup)%Seg(IV)%tAo   !Temperature 
					rhAoFup=Tube(Tube(TubeNum)%Fup)%Seg(IV)%rhAo !Relative humidity
					!wbAoFup=Tube(Tube(TubeNum)%Fup)%Seg(K)%wbAo !wet bulb temp.
					tAiFup=Tube(Tube(TubeNum)%Fup)%Seg(IV)%tAi   !Temperature 
					rhAiFup=Tube(Tube(TubeNum)%Fup)%Seg(IV)%rhAi !Relative humidity
					!wbAiFup=Tube(Tube(TubeNum)%Fup)%Seg(K)%wbAi !wet bulb temp.
					mAiFup=Tube(Tube(TubeNum)%Fup)%Seg(IV)%mAi   !mass flow rate
					VelDevFup=Tube(Tube(TubeNum)%Fup)%Seg(IV)%VelDev !Velocity deviation
				END IF
			ELSE !Even tubes
				IF (Tube(Tube(TubeNum)%Fup)%Empty .NE. 0) THEN !Empty tubes

					IF (Tube(TubeNum)%Fdown .EQ. 0) THEN !Coil bottom, ISI - 07/29/07

                        IF (Tube(Tube(TubeNum)%Fup)%Fdown .EQ. 0) THEN
						    tAoFdown=tAiCoil
						    rhAoFdown=rhAiCoil
						    tAiFdown=tAiCoil
						    rhAiFdown=rhAiCoil
						    mAiFdown=Ckt(II)%Tube(III)%Seg(IV)%mAi
						    VelDevFdown=Ckt(II)%Tube(III)%Seg(IV)%VelDev						
                        ELSE
						    tAoFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(IV)%tAo
						    rhAoFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(IV)%rhAo
						    tAiFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(IV)%tAi
						    rhAiFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(IV)%rhAi
						    mAiFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(IV)%mAi
						    VelDevFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(IV)%VelDev !Velocity deviation
                        END IF
                        
					ELSE IF (Tube(Tube(TubeNum)%Fdown)%Fup .NE. 0) THEN
						tAoFup=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(NumOfMods+1-IV)%tAo   !Temperature 
						rhAoFup=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(NumOfMods+1-IV)%rhAo !Relative humidity
						!wbAoFup=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(NumOfMods+1-K)%wbAo !wet bulb temp.
						tAiFup=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(NumOfMods+1-IV)%tAi   !Temperature 
						rhAiFup=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(NumOfMods+1-IV)%rhAi !Relative humidity
						!wbAiFup=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(NumOfMods+1-K)%wbAi !wet bulb temp.
						mAiFup=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(NumOfMods+1-IV)%mAi   !mass flow rate
						VelDevFup=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(NumOfMods+1-IV)%VelDev !Velocity deviation
					ELSE !Frontal tubes
						tAoFup=tAiCoil
 						rhAoFup=rhAiCoil
						!wbAoFup=wbAiCoil
						tAiFup=tAiCoil
						rhAiFup=rhAiCoil
						!wbAiFup=wbAiCoil
						mAiFup=Ckt(II)%Tube(III)%Seg(IV)%mAi
						VelDevFup=Ckt(II)%Tube(III)%Seg(IV)%VelDev
					END IF
				ELSE
					tAoFup=Tube(Tube(TubeNum)%Fup)%Seg(NumOfMods+1-IV)%tAo   !Temperature 
					rhAoFup=Tube(Tube(TubeNum)%Fup)%Seg(NumOfMods+1-IV)%rhAo !Relative humidity
					!wbAoFup=Tube(Tube(TubeNum)%Fup)%Seg(NumOfMods+1-K)%wbAo !wet bulb temp.
					tAiFup=Tube(Tube(TubeNum)%Fup)%Seg(NumOfMods+1-IV)%tAi   !Temperature 
					rhAiFup=Tube(Tube(TubeNum)%Fup)%Seg(NumOfMods+1-IV)%rhAi !Relative humidity
					!wbAiFup=Tube(Tube(TubeNum)%Fup)%Seg(NumOfMods+1-K)%wbAi !Relative humidity
					mAiFup=Tube(Tube(TubeNum)%Fup)%Seg(NumOfMods+1-IV)%mAi   !mass flow rate
					VelDevFup=Tube(Tube(TubeNum)%Fup)%Seg(NumOfMods+1-IV)%VelDev !Velocity deviation
				END IF
			END IF
		ELSE !Front row tubes
			tAoFup=tAiCoil
			rhAoFup=rhAiCoil
			!wbAoFup=wbAiCoil
			tAiFup=tAiCoil
			rhAiFup=rhAiCoil
			!wbAiFup=wbAiCoil
			mAiFup=Ckt(II)%Tube(III)%Seg(IV)%mAi
			VelDevFup=Ckt(II)%Tube(III)%Seg(IV)%VelDev
		END IF

		IF (Tube(TubeNum)%Fdown .NE. 0) THEN !Lower front tube
			IF (Tube(TubeNum)%Even .EQ. 0) THEN !Odd tubes
				IF (Tube(Tube(TubeNum)%Fdown)%Empty .NE. 0) THEN !Empty tubes

					IF (Tube(TubeNum)%Fup .EQ. 0) THEN !Coil top, ISI - 07/29/07

						tAoFdown=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(IV)%tAo
						rhAoFdown=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(IV)%rhAo
						tAiFdown=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(IV)%tAi
						rhAiFdown=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(IV)%rhAi
						mAiFdown=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(IV)%mAi
						VelDevFdown=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(IV)%VelDev !Velocity deviation

					ELSE IF (Tube(Tube(TubeNum)%Fup)%Fdown .NE. 0) THEN
						tAoFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(IV)%tAo
						rhAoFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(IV)%rhAo
						!wbAoFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(K)%wbAo
						tAiFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(IV)%tAi
						rhAiFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(IV)%rhAi
						!wbAiFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(K)%wbAi
						mAiFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(IV)%mAi
						VelDevFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(IV)%VelDev !Velocity deviation
					ELSE !Frontal tubes
						tAoFdown=tAiCoil
 						rhAoFdown=rhAiCoil
						!wbAoFdown=wbAiCoil
						tAiFdown=tAiCoil
						rhAiFdown=rhAiCoil
						!wbAiFdown=wbAiCoil
						mAiFdown=Ckt(II)%Tube(III)%Seg(IV)%mAi
						VelDevFdown=Ckt(II)%Tube(III)%Seg(IV)%VelDev
					END IF
				ELSE
					tAoFdown=Tube(Tube(TubeNum)%Fdown)%Seg(IV)%tAo
					rhAoFdown=Tube(Tube(TubeNum)%Fdown)%Seg(IV)%rhAo
					!wbAoFdown=Tube(Tube(TubeNum)%Fdown)%Seg(K)%wbAo
					tAiFdown=Tube(Tube(TubeNum)%Fdown)%Seg(IV)%tAi
					rhAiFdown=Tube(Tube(TubeNum)%Fdown)%Seg(IV)%rhAi
					!wbAiFdown=Tube(Tube(TubeNum)%Fdown)%Seg(K)%wbAi
					mAiFdown=Tube(Tube(TubeNum)%Fdown)%Seg(IV)%mAi
					VelDevFdown=Tube(Tube(TubeNum)%Fdown)%Seg(IV)%VelDev !Velocity deviation
				END IF
			ELSE !Even tubes
				IF (Tube(Tube(TubeNum)%Fdown)%Empty .NE. 0) THEN !Empty tubes

					IF (Tube(TubeNum)%Fup .EQ. 0) THEN !Coil top, ISI - 07/29/07

						tAoFdown=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(IV)%tAo
						rhAoFdown=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(IV)%rhAo
						tAiFdown=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(IV)%tAi
						rhAiFdown=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(IV)%rhAi
						mAiFdown=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(IV)%mAi
						VelDevFdown=Tube(Tube(Tube(TubeNum)%Fdown)%Fup)%Seg(IV)%VelDev !Velocity deviation

					ELSE IF (Tube(Tube(TubeNum)%Fup)%Fdown .NE. 0) THEN
						tAoFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(NumOfMods+1-IV)%tAo
 						rhAoFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(NumOfMods+1-IV)%rhAo
						!wbAoFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(NumOfMods+1-K)%wbAo
						tAiFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(NumOfMods+1-IV)%tAi
						rhAiFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(NumOfMods+1-IV)%rhAi
						!wbAiFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(NumOfMods+1-K)%wbAi
						mAiFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(NumOfMods+1-IV)%mAi
						VelDevFdown=Tube(Tube(Tube(TubeNum)%Fup)%Fdown)%Seg(NumOfMods+1-IV)%VelDev !Velocity deviation
					ELSE !Frontal tubes
						tAoFdown=tAiCoil
 						rhAoFdown=rhAiCoil
						!wbAoFdown=wbAiCoil
						tAiFdown=tAiCoil
						rhAiFdown=rhAiCoil
						!wbAiFdown=wbAiCoil
						mAiFdown=Ckt(II)%Tube(III)%Seg(IV)%mAi
						VelDevFdown=Ckt(II)%Tube(III)%Seg(IV)%VelDev
					END IF
				ELSE
					tAoFdown=Tube(Tube(TubeNum)%Fdown)%Seg(NumOfMods+1-IV)%tAo
					rhAoFdown=Tube(Tube(TubeNum)%Fdown)%Seg(NumOfMods+1-IV)%rhAo
					!wbAoFdown=Tube(Tube(TubeNum)%Fdown)%Seg(NumOfMods+1-K)%wbAo
					tAiFdown=Tube(Tube(TubeNum)%Fdown)%Seg(NumOfMods+1-IV)%tAi
					rhAiFdown=Tube(Tube(TubeNum)%Fdown)%Seg(NumOfMods+1-IV)%rhAi
					!wbAiFdown=Tube(Tube(TubeNum)%Fdown)%Seg(NumOfMods+1-K)%wbAi
					mAiFdown=Tube(Tube(TubeNum)%Fdown)%Seg(NumOfMods+1-IV)%mAi
					VelDevFdown=Tube(Tube(TubeNum)%Fdown)%Seg(NumOfMods+1-IV)%VelDev !Velocity deviation
				END IF
			END IF
		ELSE !Front row tubes
			tAoFdown=tAiCoil
			rhAoFdown=rhAiCoil
			!wbAoFdown=wbAiCoil
			tAiFdown=tAiCoil
			rhAiFdown=rhAiCoil
			!wbAiFdown=wbAiCoil
			mAiFdown=Ckt(II)%Tube(III)%Seg(IV)%mAi
			VelDevFdown=Ckt(II)%Tube(III)%Seg(IV)%VelDev
		END IF

		tAiFavg=(tAiFup+tAiFdown)/2
		tAoFavg=(tAoFup+tAoFdown)/2

 		rhAiFavg=(rhAiFup+rhAiFdown)/2
		rhAoFavg=(rhAoFup+rhAoFdown)/2

		Ckt(II)%Tube(III)%Seg(IV)%tAi=tAiFavg-1*(tAiFavg-tAoFavg)
		Ckt(II)%Tube(III)%Seg(IV)%rhAi=rhAiFavg-1*(rhAiFavg-rhAoFavg)

		IF (Ckt(II)%Tube(III)%Seg(IV)%VelDev .LE. 0) Ckt(II)%Tube(III)%Seg(IV)%VelDev=1
		
		Ckt(II)%Tube(III)%Seg(IV)%mAi=(mAiFup+mAiFdown)/2
		mAiMod=Ckt(II)%Tube(III)%Seg(IV)%mAi
		Ckt(II)%Tube(III)%Seg(IV)%mAi=mAiCoil*LmodTube/(Ltube*Nt)*Ckt(II)%Tube(III)%Seg(IV)%VelDev
		mAiMod=Ckt(II)%Tube(III)%Seg(IV)%mAi

	ELSE !Microchannel coil
		IF (I .EQ. 1) THEN !1st slab
			Slab(I)%Pass(II)%Tube(1)%Seg(IV)%tAi=tAiCoil
			Slab(I)%Pass(II)%Tube(1)%Seg(IV)%rhAi=rhAiCoil
	    ELSE
		    Slab(I)%Pass(II)%Tube(1)%Seg(IV)%tAi=Slab(I-1)%tAo
		    Slab(I)%Pass(II)%Tube(1)%Seg(IV)%rhAi=Slab(I-1)%rhAo
		    !Slab(I)%Pass(II)%Tube(1)%Seg(IV)%tAi=Slab(I-1)%Pass(II)%Tube(1)%Seg(IV)%tAo !ISI - 01/06/08
		    !Slab(I)%Pass(II)%Tube(1)%Seg(IV)%rhAi=Slab(I-1)%Pass(II)%Tube(1)%Seg(IV)%rhAo
	    END IF
	END IF

RETURN

END SUBROUTINE CalcSegmentAirInletConditions

!************************************************************************

SUBROUTINE CalcSegmentRefInletConditions(I,II,III,IV,CoilType)

!------------------------------------------------------------------------
!Purpose:
!To calculate inlet refrigerant pressure and enthalpy
!
!Author
!Ipseng Iu
!Oklahoma State Univerity, Stillwater
!
!Date
!November 2005
!
!Reference:
!none
!
!------------------------------------------------------------------------

IMPLICIT NONE

INTEGER,INTENT(IN) :: I   !Slab number
INTEGER,INTENT(IN) :: II  !Circuit,pass number
INTEGER,INTENT(IN) :: III !Tube number
INTEGER,INTENT(IN) :: IV  !Segment number

INTEGER,INTENT(IN) :: CoilType   !1=Condenser; 2=Evaporator; 
                                 !3=High side interconnecting pipes; 
								 !4=Low side interconnecting pipes
								 !5=Microchannel condenser
								 !6=Microchannel Evaporator

!FLOW:

	IF (CoilType .NE. MCCONDENSER) THEN 

		IF (III .EQ. 1 .AND. IV .EQ. 1) THEN !Equal to circuit inlet
			Ckt(II)%Tube(III)%Seg(IV)%pRi=Ckt(II)%pRi
			Ckt(II)%Tube(III)%Seg(IV)%hRi=Ckt(II)%hRi

		ELSE IF (K .EQ. 1) THEN !Equal to outlet of previous tube
			Ckt(II)%Tube(III)%Seg(IV)%pRi=Ckt(II)%Tube(III-1)%Seg(NumOfMods)%pRo
			Ckt(II)%Tube(III)%Seg(IV)%hRi=Ckt(II)%Tube(III-1)%Seg(NumOfMods)%hRo

		ELSE !Equal to outlet of previous module(section)
			Ckt(II)%Tube(III)%Seg(IV)%pRi=Ckt(II)%Tube(III)%Seg(IV-1)%pRo
			Ckt(II)%Tube(III)%Seg(IV)%hRi=Ckt(II)%Tube(III)%Seg(IV-1)%hRo
		END IF

	ELSE

        IF (IsParallelSlabs .GT. 0) THEN
		      !ISI - 07/13/07
		      IF (II .EQ. 1) THEN !1st pass
			      IF (IV .EQ. 1) THEN !1st segment
			          Slab(I)%Pass(II)%Tube(1)%Seg(IV)%pRi=pRiCoil
			          Slab(I)%Pass(II)%Tube(1)%Seg(IV)%hRi=hRiCoil
				  ELSE
			          Slab(I)%Pass(II)%Tube(1)%Seg(IV)%pRi=Slab(I)%Pass(II)%Tube(1)%Seg(IV-1)%pRo
			          Slab(I)%Pass(II)%Tube(1)%Seg(IV)%hRi=Slab(I)%Pass(II)%Tube(1)%Seg(IV-1)%hRo
				  END IF
			  ELSE
			      IF (IV .EQ. 1) THEN !1st segment
					  Slab(I)%Pass(II)%Tube(1)%Seg(IV)%pRi=Slab(I)%Pass(II-1)%Tube(1)%Seg(NumOfMods)%pRo
				      Slab(I)%Pass(II)%Tube(1)%Seg(IV)%hRi=Slab(I)%Pass(II-1)%Tube(1)%Seg(NumOfMods)%hRo
				  ELSE
					  Slab(I)%Pass(II)%Tube(1)%Seg(IV)%pRi=Slab(I)%Pass(II)%Tube(1)%Seg(IV-1)%pRo
				      Slab(I)%Pass(II)%Tube(1)%Seg(IV)%hRi=Slab(I)%Pass(II)%Tube(1)%Seg(IV-1)%hRo
				  END IF
			  END IF
	    ELSE !Series
	    
		    IF (I .EQ. 1) THEN !1st slab
		          IF (II .EQ. 1) THEN !1st pass
			          IF (IV .EQ. 1) THEN !1st segment
			              Slab(I)%Pass(II)%Tube(1)%Seg(IV)%pRi=pRiCoil
			              Slab(I)%Pass(II)%Tube(1)%Seg(IV)%hRi=hRiCoil
				      ELSE
			              Slab(I)%Pass(II)%Tube(1)%Seg(IV)%pRi=Slab(I)%Pass(II)%Tube(1)%Seg(IV-1)%pRo
			              Slab(I)%Pass(II)%Tube(1)%Seg(IV)%hRi=Slab(I)%Pass(II)%Tube(1)%Seg(IV-1)%hRo
				      END IF
			      ELSE
			          IF (IV .EQ. 1) THEN !1st segment
					      Slab(I)%Pass(II)%Tube(1)%Seg(IV)%pRi=Slab(I)%Pass(II-1)%Tube(1)%Seg(NumOfMods)%pRo
				          Slab(I)%Pass(II)%Tube(1)%Seg(IV)%hRi=Slab(I)%Pass(II-1)%Tube(1)%Seg(NumOfMods)%hRo
				      ELSE
					      Slab(I)%Pass(II)%Tube(1)%Seg(IV)%pRi=Slab(I)%Pass(II)%Tube(1)%Seg(IV-1)%pRo
				          Slab(I)%Pass(II)%Tube(1)%Seg(IV)%hRi=Slab(I)%Pass(II)%Tube(1)%Seg(IV-1)%hRo
				      END IF
			      END IF
	        ELSE
		          IF (II .EQ. 1) THEN !1st pass
			          IF (IV .EQ. 1) THEN !1st segment
			              Slab(I)%Pass(II)%Tube(1)%Seg(IV)%pRi=Slab(I-1)%Pass(Slab(I-1)%Npass)%Tube(1)%Seg(NumOfMods)%pRo
			              Slab(I)%Pass(II)%Tube(1)%Seg(IV)%hRi=Slab(I-1)%Pass(Slab(I-1)%Npass)%Tube(1)%Seg(NumOfMods)%hRo
				      ELSE
			              Slab(I)%Pass(II)%Tube(1)%Seg(IV)%pRi=Slab(I)%Pass(II)%Tube(1)%Seg(IV-1)%pRo
			              Slab(I)%Pass(II)%Tube(1)%Seg(IV)%hRi=Slab(I)%Pass(II)%Tube(1)%Seg(IV-1)%hRo
				      END IF
			      ELSE
			          IF (IV .EQ. 1) THEN !1st segment
					      Slab(I)%Pass(II)%Tube(1)%Seg(IV)%pRi=Slab(I)%Pass(II-1)%Tube(1)%Seg(NumOfMods)%pRo
				          Slab(I)%Pass(II)%Tube(1)%Seg(IV)%hRi=Slab(I)%Pass(II-1)%Tube(1)%Seg(NumOfMods)%hRo
				      ELSE
					      Slab(I)%Pass(II)%Tube(1)%Seg(IV)%pRi=Slab(I)%Pass(II)%Tube(1)%Seg(IV-1)%pRo
				          Slab(I)%Pass(II)%Tube(1)%Seg(IV)%hRi=Slab(I)%Pass(II)%Tube(1)%Seg(IV-1)%hRo
				      END IF
			      END IF
		    END IF
	    
	    END IF
			  
	END IF

RETURN

END SUBROUTINE CalcSegmentRefInletConditions

!************************************************************************

SUBROUTINE CalcSegmentOutletConditions(I,II,III,IV,CoilType)

!------------------------------------------------------------------------
!Purpose:
!To calculate segment outlet conditions
!
!Author
!Ipseng Iu
!Oklahoma State Univerity, Stillwater
!
!Date
!March 2005
!
!Reference:
!none
!
!------------------------------------------------------------------------

USE FluidProperties
USE CoilCalcMod
USE AirPropMod
USE OilMixtureMod

IMPLICIT NONE

INTEGER,INTENT(IN) :: I   !Slab number
INTEGER,INTENT(IN) :: II  !Circuit,pass number
INTEGER,INTENT(IN) :: III !Tube number
INTEGER,INTENT(IN) :: IV  !Segment number

INTEGER,INTENT(IN) :: CoilType   !1=Condenser; 2=Evaporator; 
                                 !3=High side interconnecting pipes; 
								 !4=Low side interconnecting pipes
								 !5=Microchannel condenser
								 !6=Microchannel evaporator

REAL Rtot         !Total resistance, K-m^2/W
REAL Qsolar       !Solar radiation, kW
REAL DPreturnbend !Pressure drop at return bend, kPa
REAL DiffpRoMod   !Difference in pRoMod
REAL DiffhRoMod   !Difference in hRoMod
REAL PrevpRoMod   !Previous value of pRoMod
REAL PrevhRoMod   !Previous value of hRoMod
INTEGER RefBCiter             !Iteration loop counter
LOGICAL IsTransitionSegment !Flag to indicate if it is transtion segment

!FLOW:

	!Initialize for property iteration, to find the mean property
	hfgRoMod=0;  xRoMod=0;  vgRoMod=0;  vfRoMod=0
	muRoMod=0;  mugRoMod=0;  mufRoMod=0
	kRoMod=0;	  kfRoMod=0;  kgRoMod=0
	cpRoMod=0;  cpfRoMod=0;  cpgRoMod=0
	DTmod=0;

	PrevpRoMod=BIG
	PrevhRoMod=BIG

	IsTransitionSegment=.FALSE.

	DO RefBCiter=1, RefBCmaxIter

		!Correct quality
		IF (xRoMod .GT. 1) THEN
			xRoMod=1
		ELSEIF (xRoMod .LT. 0) THEN
			xRoMod=0 
		ENDIF
		IF (xRiMod .GT. 1) THEN
			xRiMod=1
		ELSEIF (xRiMod .LT. 0) THEN
			xRiMod=0 
		ENDIF

		!Calculate mean properties
		CALL CalcMeanProp(hfgRiMod,hfgRoMod,hfgRmod)
		CALL CalcMeanProp(xRiMod,xRoMod,xRmod)
		CALL CalcMeanProp(vgRiMod,vgRoMod,vgRmod)
		CALL CalcMeanProp(vfRiMod,vfRoMod,vfRmod)
		CALL CalcMeanProp(muRiMod,muRoMod,muRmod)
		CALL CalcMeanProp(mugRiMod,mugRoMod,mugRmod)
		CALL CalcMeanProp(mufRiMod,mufRoMod,mufRmod)
		CALL CalcMeanProp(kRiMod,kRoMod,kRmod)
		CALL CalcMeanProp(kfRiMod,kfRoMod,kfRmod)
		CALL CalcMeanProp(kgRiMod,kgRoMod,kgRmod)
		CALL CalcMeanProp(cpRiMod,cpRoMod,cpRmod)
		CALL CalcMeanProp(cpfRiMod,cpfRoMod,cpfRmod)
		CALL CalcMeanProp(cpgRiMod,cpgRoMod,cpgRmod)

		!Correct specific heat
		!IF (cpRmod .LT. 0) THEN
		IF (cpRmod .LE. 0) THEN !ISI - 08/03/06
			IF (xRmod .LE. 0) cpRmod = cpfRmod
			IF (xRmod .GE. 1) cpRmod = cpgRmod 
		END IF

		!Correct thermal conductivity
		!IF (kRmod .LT. 0) THEN 
		IF (kRmod .LE. 0) THEN !ISI - 08/03/06
			IF (xRmod .LE. 0) kRmod = kfRmod
			IF (xRmod .GE. 1) kRmod = kgRmod 
		END IF

		IF (muRmod .LE. 0) THEN !ISI - 08/03/06
		    IF (xRmod .LE. 0) muRmod = mufRmod
			IF (xRmod .GE. 1) muRmod = mugRmod 
		END IF

		LmodTPratio=0 
		QmodTP=0 
		LmodSHratio=0
		QmodSH=0

		!For segment covers both two phase and single phase region
		IF (RefBCiter .GT. 1 .AND. &
			((xRiMod .GT. 0 .AND. xRiMod .LT. 1 .AND. xRoMod .LE. 0) .OR. & !Condenser outlet
			(xRiMod .GE. 1 .AND. xRoMod .LT. 1 .AND. xRoMod .GT. 0))) THEN  !Condenser inlet

			CALL CalcTransitionSegment(CoilType) 
			IF (ErrorFlag .GT. CONVERGEERROR) RETURN
			IF (IsSimpleCoil .EQ. 1) IsTransitionSegment=.TRUE.

			!Update properties ISI - 08/03/06 
			IF (cpRmod .LE. 0) THEN 
				IF (xRmod .LE. 0) cpRmod = cpfRmod
				IF (xRmod .GE. 1) cpRmod = cpgRmod 
			END IF

			IF (kRmod .LE. 0) THEN !ISI - 08/03/06
				IF (xRmod .LE. 0) kRmod = kfRmod
				IF (xRmod .GE. 1) kRmod = kgRmod 
			END IF

			IF (muRmod .LE. 0) THEN !ISI - 08/03/06
				IF (xRmod .LE. 0) muRmod = mufRmod
				IF (xRmod .GE. 1) muRmod = mugRmod 
			END IF

		END IF 

		!Condenser inlet
		IF (DTmod .EQ. 0) DTmod=(tAiMod+tRiMod)/2 !First estimate
		CALL hcRefside(RefName,CoilType,TubeType,IDtube,ktube,mRefMod,Qmod,AoMod,AiMod,hfgRmod, &
					   xRmod,xRmod,vgRmod,vfRmod,muRmod,mugRmod,mufRmod,kRmod,kfRmod,kgRmod,cpRmod,cpfRmod,cpgRmod, &
					   MolWeight,Psat,Pcr,Tsat,SigmaMod,DTmod,Wabsolute,EFref,hciMod)
		
		hciMod=hciMod*hciMultiplier

		CALL Reynolds(IDtube,mRefMod,xRmod,muRmod,mugRmod,mufRmod,ReVap,ReLiq)
		  
		WetFlag=0
		TsurfMod=0

		!Calc. UA
		CALL CalcUA(CoilType,WetFlag,Kfin,FinThk,FinHeight,Ktube,Pt,Pl,ODtube,TubeThk,TubeDepth,RowNum,tAiMod,hAiMod, &
					hcoMod,hciMod,AfMod,AoMod,AiMod,AmMod,UA,Rair,Rrefrig,Rtube,FinEff,SurfEff)
		IF (xRiMod .GT. 0 .AND. xRoMod .LE. 0 .AND. LmodTPratio .LT. 1) THEN !Condenser outlet
            UA=UA*(1-LmodTPratio)
		ELSEIF (xRiMod .GE. 1 .AND. xRoMod .LT. 1 .AND. LmodSHratio .LT. 1) THEN !Condenser inlet
			UA=UA*(1-LmodSHratio)
		END IF
		
		!Calc. Cref
		IF (CoilType .NE. MCCONDENSER) THEN
		    cRef=mRefMod*cpRmod
		ELSE
			cRef=mRefMod*cpRmod*NumOfChannels !mRefTot*cpRmod
		END IF
		IF (xRmod .LT. 1. .AND. xRmod .GT. 0.) cRef=BIG !Phase change
		!IF (xRiMod .LT. 1. .AND. xRiMod .GT. 0.) cRef=BIG !Phase change

		!Calc. Cair
		!CPair=CPA(REAL(tAiMod))
		CPair=CPA(REAL(tAmod))
		cAir=mAiMod*cpAir

		!Calc. Cmin
		Cmin=MIN(cAir,cRef)

		!Calc. Epsilon
		CALL EPScalc(cAir,cRef,UA,Cratio,NTU,EPS)

		!Calc. DT
		IF (LmodTPratio .GT. 0 .OR. LmodSHratio .GT. 0) THEN !ISI - 07/21/06
			DT=(tRmod-tAiMod) 
		ELSE
			DT=(tRiMod-tAiMod) 
		END IF

		!Calc. Q module
		Qmod=EPS*Cmin*DT
		
		!Condenser outlet
		IF (xRiMod .GT. 0 .AND. xRoMod .LE. 0) THEN
			IF (IsSimpleCoil .EQ. 1) THEN
				IF (QmodTP .NE. 0) Qmod = QmodTP
			ELSE
				IF (LmodTP .EQ. LmodTube) THEN
					IF (Qmod .GT. QmodTP) Qmod = QmodTP
				ELSE
					Qmod=Qmod+QmodTP
				END IF
			END IF
		END IF

		!Condenser inlet
		IF (xRiMod .GE. 1 .AND. xRoMod .LT. 1) THEN
			IF (IsSimpleCoil .EQ. 1) THEN
				IF (QmodSH .NE. 0) Qmod = QmodSH
			ELSE
				IF (LmodSH .EQ. LmodTube) THEN
					IF (Qmod .GT. QmodSH) Qmod = QmodSH
				ELSE
					Qmod=Qmod+QmodSH
				END IF
			END IF
		END IF

		IF (CoilType .NE. MCCONDENSER) THEN
			!Include solar radiation
			IF (IsCoolingMode .GT. 1 .AND. &
				Ckt(II)%Tube(III)%Fup .EQ. 0 .AND. Ckt(II)%Tube(III)%Fdown .EQ. 0) THEN
				Rtot=Rair*AoMod+Rrefrig*AiMod+Rtube*AmMod
				Qsolar=Rair*AoMod/Rtot*Ckt(II)%Tube(III)%Seg(IV)%Aface*SurfAbsorptivity*SolarFlux
				Qmod=Qmod-Qsolar
			END IF
		END IF

		!Calc. Outside air enthalpy
		IF (CoilType .NE. MCCONDENSER) THEN
			hRoMod=-Qmod/mRefMod+hRiMod
			!IF (Wabsolute .GT. 0) THEN
			!  CALL OilMixtureOutletEnthalpy(CompManufacturer,CoilType,RefName,Psat*1e-3,mRefMod,Qmod*1e3, &
            !                                xRiMod,hfgRmod*1e3,cpfRmod*1e3,cpgRmod*1e3,Wabsolute,hRiMod*1e3,hRoMod,xRoMod) 
			!  hRoMod=hRoMod/1000
			!END IF
		ELSE
			hRoMod=-(Qmod/NumOfChannels)/mRefMod+hRiMod  !-Qmod/mRefTot+hRiMod 
			!IF (Wabsolute .GT. 0) THEN
			!  CALL OilMixtureOutletEnthalpy(CompManufacturer,CoilType,RefName,Psat*1e-3,mRefMod,Qmod/NumOfChannels*1e3, &
            !                                xRiMod,hfgRmod*1e3,cpfRmod*1e3,cpgRmod*1e3,Wabsolute,hRiMod*1e3,hRoMod,xRoMod) 
			!  hRoMod=hRoMod/1000
			!END IF
		END IF

		CALL CalcRefProperty(pRiMod,hRiMod,hfRiMod,hgRiMod,hfgRiMod,Psat,Tsat,tRiMod,xRiMod, &
			                 vRiMod,vfRiMod,vgRiMod,cpRiMod,cpfRiMod,cpgRiMod, &
							 muRiMod,mufRiMod,mugRiMod,kRiMod,kfRiMod,kgRiMod,SigmaMod)

		CALL CalcSegmentRefOutletPressure(CoilType,TubeType,tRiMod,pRiMod,hgRiMod,hfRiMod, &
				  	                      hRiMod,hRoMod,xRiMod,vRiMod,vgRiMod,vfRiMod,mRefMod, &
										  muRiMod,mugRiMod,mufRiMod,SigmaMod,LmodTube,LmodTPratio, &
										  Dchannel,HtCoil,Lcoil,DPrefMultiplier,pRoMod)

		IF (ErrorFlag .GT. CONVERGEERROR) RETURN

		CALL CalcRefProperty(pRoMod,hRoMod,hfRoMod,hgRoMod,hfgRoMod,Psat,Tsat,tRoMod,xRoMod, &
							 vRoMod,vfRoMod,vgRoMod,cpRoMod,cpfRoMod,cpgRoMod, &
							 muRoMod,mufRoMod,mugRoMod,kRoMod,kfRoMod,kgRoMod,SigmaMod)
		IF (ErrorFlag .GT. CONVERGEERROR) RETURN

		IF (CoilType .NE. MCCONDENSER .AND. IsSimpleCoil .NE. 1) THEN
			!Return bend pressure drop
			IF (K .EQ. NumOfMods) THEN
				IF (J .EQ. LastTube) THEN
		  			IF (Ckt(II)%OutSplit .GT. 1 .OR. Ckt(II)%OutJoin .GT. 1) THEN
						CALL returnbend(CoilType,TubeType,IDtube,ODtube,Pt,mRefMod,xRoMod,vRoMod,vgRoMod,vfRoMod,muRoMod,mugRoMod,mufRoMod,DPreturnbend)
						pRoMod=pRoMod-DPreturnbend
		  			END IF
		  		ELSE
					CALL returnbend(CoilType,TubeType,IDtube,ODtube,Pt,mRefMod,xRoMod,vRoMod,vgRoMod,vfRoMod,muRoMod,mugRoMod,mufRoMod,DPreturnbend)
					pRoMod=pRoMod-DPreturnbend
		  		END IF

				CALL CalcRefProperty(pRoMod,hRoMod,hfRoMod,hgRoMod,hfgRoMod,Psat,Tsat,tRoMod,xRoMod, &
									 vRoMod,vfRoMod,vgRoMod,cpRoMod,cpfRoMod,cpgRoMod, &
									 muRoMod,mufRoMod,mugRoMod,kRoMod,kfRoMod,kgRoMod,SigmaMod)
				IF (ErrorFlag .GT. CONVERGEERROR) RETURN

			END IF
		END IF

		IF (IsSimpleCoil .EQ. 1) THEN
		    IF (IsTransitionSegment) EXIT
		END IF

        !Correct the equation, Sankar 2/19/2009 - 9:30pm
		!DTmod=Qmod*(1/(hciMod*AiMod)+LOG(ODtube/IDtube/(2*PI*Ktube*LmodTube)))
		DTmod=Qmod*(1/(hciMod*AiMod)+LOG(ODtube/IDtube)/(2*PI*Ktube*LmodTube))
		!WRITE(*,*)Qmod,DTmod
		DiffpRoMod=ABS((pRoMod-PrevpRoMod)/PrevpRoMod)
		DiffhRoMod=ABS((hRoMod-PrevhRoMod)/PrevhRoMod)
		IF (DiffpRoMod .GT. SMALL .OR. DiffhRoMod .GT. SMALL) THEN 
			PrevpRoMod=pRoMod
			PrevhRoMod=hRoMod
		ELSE 
			EXIT
		END IF
			
	END DO !end of RefBCiter

	IF (RefBCiter .GT. RefBCmaxIter) THEN
		ErrorFlag=CONVERGEERROR
	END IF
	!WRITE(*,*)xRiMod,hciMod
	!pause

	!Outside air temp
	tAoMod=Qmod/cAir+tAiMod

	!Calc. Outside air enthalpy
	hAoMod=Qmod/mAiMod+hAiMod

	!WRITE(*,*)tAiMod,tAoMod,hAiMod,hAoMod,mAiMod,cAir,Qmod
	AirPropOpt=1
	AirProp(1)=tAoMod
	AirProp(4)=hAoMod
	CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)
	rhAoMod=AirProp(3)
	wbAoMod=AirProp(5)  

RETURN

END SUBROUTINE CalcSegmentOutletConditions

!************************************************************************

SUBROUTINE CalcTransitionSegment(CoilType) !,NumOfChannels)

!------------------------------------------------------------------------
!Purpose:
!To calculate transition segment (both single and two phase refrigerant
!in segment) heat transfer
!
!Author
!Ipseng Iu
!Oklahoma State Univerity, Stillwater
!
!Date
!March 2005
!
!Reference:
!none
!
!------------------------------------------------------------------------

USE FluidProperties
USE CoilCalcMod
USE AirPropMod
USE OilMixtureMod

IMPLICIT NONE

INTEGER,INTENT(IN) :: CoilType   !1=Condenser; 2=Evaporator; 
                                 !3=High side interconnecting pipes; 
								 !4=Low side interconnecting pipes
								 !5=Microchannel condenser
								 !6=Microchannel evaporator

!INTEGER,INTENT(IN) :: NumOfChannels !Number of channels

!FLOW:


	!Condenser inlet
	IF (xRiMod .GE. 1 .AND. xRoMod .LT. 1)  THEN	
		xRmod=xRiMod
	END IF
    
	!Calc. ref side resistance
	!ISI - 09/11/06
	!CALL hcRefside(RefName,CoilType,TubeType,IDtube,ktube,mRefMod,Qmod,AoMod,AiMod,hfgRmod,xRmod, &
	!			   vgRmod,vfRmod,muRmod,mugRmod,mufRmod, &
	!	  		   kRmod,kfRmod,kgRmod,cpRmod,cpfRmod,cpgRmod, &
	!			   MolWeight,Psat,Pcr,Tsat,Const,EFref,hciMod)

	hciMod=hciMod*hciMultiplier

	!Condenser outlet
	IF (xRiMod .GT. 0 .AND. xRoMod .LE. 0)  THEN
		!IF (CoilType .NE. 5) THEN
		IF (CoilType .NE. MCCONDENSER) THEN
			QmodTP=mRefMod*(hRiMod-hfRiMod)
		ELSE
			QmodTP=mRefMod*NumOfChannels*(hRiMod-hfRiMod)
		END IF
		cRef=BIG !Phase change 
	END IF
	
	!Condenser inlet
	IF (xRiMod .GE. 1 .AND. xRoMod .LT. 1)  THEN
		!IF (CoilType .NE. 5) THEN
		IF (CoilType .NE. MCCONDENSER) THEN
			QmodSH=mRefMod*(hRiMod-hgRiMod)
			cRef=mRefMod*cpRmod
		ELSE
			QmodSH=mRefMod*NumOfChannels*(hRiMod-hgRiMod) 
			cRef=mRefMod*NumOfChannels*cpRmod 
		END IF

	END IF

	!Calc. DT
	DT=(tRiMod-tAiMod)

	!Find transition boundary
	CALL FindTransitionBoundary(CoilType)

	!Condenser outlet
	IF (xRiMod .GT. 0 .AND. xRoMod .LE. 0)  THEN
		IF (LmodTP .NE. LmodTube) THEN
			xRmod=0
		ELSE
			!IF (CoilType .NE. 5) THEN
			IF (CoilType .NE. MCCONDENSER) THEN
				hRoMod=-QmodTP/mRefMod+hRiMod !ISI - 06/18/05
				!IF (Wabsolute .GT. 0) THEN
				!  CALL OilMixtureOutletEnthalpy(CompManufacturer,CoilType,RefName,Psat*1e-3,mRefMod,QmodTP*1e3, &
				!							    xRiMod,hfgRmod*1e3,cpfRmod*1e3,cpgRmod*1e3,Wabsolute,hRiMod*1e3,hRoMod,xRoMod) 
				!  hRoMod=hRoMod/1000
				!END IF
			ELSE
				hRoMod=-(QmodTP/NumOfChannels)/mRefMod+hRiMod 
				!IF (Wabsolute .GT. 0) THEN
				!  CALL OilMixtureOutletEnthalpy(CompManufacturer,CoilType,RefName,Psat*1e-3,mRefMod,QmodTP/NumOfChannels*1e3, &
				!							    xRiMod,hfgRmod*1e3,cpfRmod*1e3,cpgRmod*1e3,Wabsolute,hRiMod*1e3,hRoMod,xRoMod) 
				!  hRoMod=hRoMod/1000
				!END IF
			END IF
			Pressure=pRiMod*1000
			Enthalpy=hRoMod*1000
			xRmod=PH(RefName, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
			IF (RefPropErr .GT. 0) THEN
				WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 1328'
				ErrorFlag=REFPROPERROR
				RETURN
			END IF
			IF (xRmod .GT. 1) xRmod=1
			IF (xRmod .LT. 0) xRmod=0
		END IF

		!ISI - 07/21/06 to update the refrigerant temperature at the transition boundary
		Pressure=pRiMod*1000
		Quality=xRmod
		tRmod=PQ(RefName, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)

	END IF
		
	!Condenser inlet
	IF (xRiMod .GE. 1 .AND. xRoMod .LT. 1)  THEN
		IF (LmodSH .NE. LmodTube) THEN
			xRmod=(xRoMod+1)/2

			!ISI - 07/21/06 to update the refrigerant temperature at the transition boundary
			Pressure=pRiMod*1000
			Quality=xRmod
			tRmod=PQ(RefName, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)
		END IF
	END IF

RETURN

END SUBROUTINE CalcTransitionSegment

!************************************************************************

SUBROUTINE FindTransitionBoundary(CoilType)

!------------------------------------------------------------------------
!Purpose:
!To find the transition boundary in transition segment
!
!Author
!Ipseng Iu
!Oklahoma State Univerity, Stillwater
!
!Date
!March 2005
!
!Reference:
!none
!
!------------------------------------------------------------------------

USE FluidProperties
USE CoilCalcMod
USE AirPropMod

IMPLICIT NONE

INTEGER,INTENT(IN) :: CoilType   !1=Condenser; 2=Evaporator; 
                                 !3=High side interconnecting pipes; 
								 !4=Low side interconnecting pipes
								 !5=Microchannel condenser
								 !6=Microchannel evaporator

!FLOW:

	!Initialize
	LmodTPmin=0
	LmodTPmax=LmodTube
	LmodTP=LmodTube/2 
	LmodSHmin=0
	LmodSHmax=LmodTube
	LmodSH=LmodTube/2 
	DTmod=0
	DO

		!Condenser outlet
		IF (xRiMod .GT. 0 .AND. xRoMod .LE. 0)  THEN
			LmodTPratio=LmodTP/LmodTube
			LmodSHratio=1-LmodTPratio
			WetFlag=0
			TsurfMod=0

			!ISI - 09/11/06
			IF (DTmod .EQ. 0) DTmod=(tAiMod+tRiMod)/2 !First estimate
			CALL hcRefside(RefName,CoilType,TubeType,IDtube,ktube,mRefMod,Qmod,AoMod,AiMod,hfgRmod,xRiMod,0.0, &
						   vgRmod,vfRmod,muRmod,mugRmod,mufRmod, &
		  				   kRmod,kfRmod,kgRmod,cpRmod,cpfRmod,cpgRmod, &
						   MolWeight,Psat,Pcr,Tsat,SigmaMod,DTmod,Wabsolute,EFref,hciMod)

			!Calc. UA
			CALL CalcUA(CoilType,WetFlag,Kfin,FinThk,FinHeight,Ktube,Pt,Pl,ODtube,TubeThk,TubeDepth,RowNum,tAiMod,hAiMod, &
		 				hcoMod,hciMod,AfMod*LmodTPratio,AoMod*LmodTPratio,AiMod*LmodTPratio,AmMod*LmodTPratio, &
						UA,Rair,Rrefrig,Rtube,FinEff,SurfEff)

			IF (IsSimpleCoil .EQ. 1) mAiMod=mAiCoil*LmodTP/Lcoil !ISI - 12/05/06

			!Calc. Cair !ISI - 12/05/06
			!CPair=CPA(REAL(tAiMod))
			CPair=CPA(REAL(tAmod))
			Cair=mAiMod*CPair

			!Calc. Cmin
			Cmin=MIN(cAir,cRef)

			!Calc. Epsilon
			CALL EPScalc(cAir,cRef,UA,Cratio,NTU,EPS)

			!Calc. dry module heat transfer
			Qmod=EPS*Cmin*DT
              
		    DTmod=Qmod*(1/(hciMod*AiMod*LmodTP/LmodTube)+LOG(ODtube/IDtube)/(2*PI*Ktube*LmodTP))
			IF (ABS(LmodTPratio-1) .LT. SMALL) THEN
				LmodTP=LmodTube !ISI - 06/18/05
				LmodTPratio=1 !ISI - 06/18/05
				QmodTP=Qmod
				EXIT
			END IF
			IF (ABS((Qmod-QmodTP)/QmodTP) .GT. SMALL .AND. ABS(LmodTPmax-LmodTPmin)/LmodTP .GT. SMALL) THEN
				IF (ABS(Qmod) .GT. ABS(QmodTP)) THEN
					LmodTPmax=LmodTP
				ELSE
					LmodTPmin=LmodTP
				END IF
				LmodTP=(LmodTPmax+LmodTPmin)/2
			ELSE
				IF (IsSimpleCoil .EQ. 1) THEN
					LmodTube=LmodTP
				END IF
				EXIT
			END IF
		END IF
			
		!Condenser inlet					  
		IF (xRiMod .GE. 1 .AND. xRoMod .LT. 1)  THEN
			LmodSHratio=LmodSH/LmodTube
			LmodTPratio=1-LmodSHratio
			WetFlag=0
			TsurfMod=0

			!ISI - 09/11/06
			IF (DTmod .EQ. 0) DTmod=(tAiMod+tRiMod)/2 !First estimate
			CALL hcRefside(RefName,CoilType,TubeType,IDtube,ktube,mRefMod,Qmod,AoMod,AiMod,hfgRmod,xRiMod,1.0, &
						   vgRmod,vfRmod,muRmod,mugRmod,mufRmod, &
		  				   kRmod,kfRmod,kgRmod,cpRmod,cpfRmod,cpgRmod, &
						   MolWeight,Psat,Pcr,Tsat,SigmaMod,DTmod,Wabsolute,EFref,hciMod)

			!Calc. UA
			CALL CalcUA(CoilType,WetFlag,Kfin,FinThk,FinHeight,Ktube,Pt,Pl,ODtube,TubeThk,TubeDepth,RowNum,tAiMod,hAiMod, &
						hcoMod,hciMod,AfMod*LmodSHratio,AoMod*LmodSHratio,AiMod*LmodSHratio,AmMod*LmodSHratio, &
						UA,Rair,Rrefrig,Rtube,FinEff,SurfEff)

			IF (IsSimpleCoil .EQ. 1) mAiMod=mAiCoil*LmodSH/Lcoil !ISI - 12/05/06

			!Calc. Cair !ISI - 12/05/06
			!CPair=CPA(REAL(tAiMod))
			CPair=CPA(REAL(tAmod))
			Cair=mAiMod*CPair

			!Calc. Cmin
			Cmin=MIN(cAir,cRef)

			!Calc. Epsilon
			CALL EPScalc(cAir,cRef,UA,Cratio,NTU,EPS)

			!Calc. dry module heat transfer
			Qmod=EPS*Cmin*DT
              
			DTmod=Qmod*(1/(hciMod*AiMod*LmodSH/LmodTube)+LOG(ODtube/IDtube)/(2*PI*Ktube*LmodSH))
			IF (ABS(LmodSHratio-1) .LT. SMALL) THEN
				LmodSH=LmodTube !ISI - 05/18/05
				LmodSHratio=1 !ISI - 05/18/05
				QmodSH=Qmod
				EXIT
			END IF
			IF (ABS((Qmod-QmodSH)/QmodSH) .GT. SMALL .AND. ABS(LmodSHmax-LmodSHmin)/LmodSH .GT. SMALL) THEN
				IF (ABS(Qmod) .GT. ABS(QmodSH)) THEN
					LmodSHmax=LmodSH
				ELSE
					LmodSHmin=LmodSH
				END IF
				LmodSH=(LmodSHmax+LmodSHmin)/2
			ELSE
				IF (IsSimpleCoil .EQ. 1) THEN
					LmodTube=LmodSH
				END IF
				EXIT
			END IF
		END IF
	END DO !End of LmodTPratio, LmodSHratio

RETURN

END SUBROUTINE FindTransitionBoundary

!************************************************************************

SUBROUTINE CalcRefProperty(pRef,hRef,hfRef,hgRef,hfgRef,Psat,Tsat,tRef,xRef, &
	                       vRef,vfRef,vgRef,cpRef,cpfRef,cpgRef, &
						   muRef,mufRef,mugRef,kRef,kfRef,kgRef,SigmaRef)

!------------------------------------------------------------------------
!Purpose:
!To calculate refrigerant properties given pressure and enthalpy
!
!Author
!Ipseng Iu
!Oklahoma State Univerity, Stillwater
!
!Date
!March 2005
!
!Reference:
!none
!
!------------------------------------------------------------------------

USE FluidProperties
USE OilMixtureMod

IMPLICIT NONE

REAL, INTENT(IN)  :: pRef   !Pressure, kPa
REAL, INTENT(IN)  :: hRef   !Enthalpy, kJ/kg
REAL, INTENT(OUT) :: hfRef  !Liquid enthalpy, kJ/kg
REAL, INTENT(OUT) :: hgRef  !Vapor enthalpy, kJ/kg
REAL, INTENT(OUT) :: hfgRef !hg - hf
REAL, INTENT(OUT) :: Psat   !Saturation pressure, kPa
REAL, INTENT(OUT) :: Tsat   !Saturation temperature, C
REAL, INTENT(OUT) :: tRef   !Temperature, C
REAL, INTENT(OUT) :: xRef   !Quality
REAL, INTENT(OUT) :: vRef   !Specific volume, m^3/kg
REAL, INTENT(OUT) :: vfRef  !Liquid specific volume, m^3/kg
REAL, INTENT(OUT) :: vgRef  !Vapor specific volume, m^3/kg
REAL, INTENT(OUT) :: cpRef  !Specific heat, kJ/kg-K
REAL, INTENT(OUT) :: cpfRef !Liquid specific heat, kJ/kg-K
REAL, INTENT(OUT) :: cpgRef !Vapor specific heat, kJ/kg-K
REAL, INTENT(OUT) :: muRef  !Dynamic viscoity, Pa-s
REAL, INTENT(OUT) :: mufRef !Liquid dynamic viscoity, Pa-s
REAL, INTENT(OUT) :: mugRef !Vapor dynamic viscoity, Pa-s
REAL, INTENT(OUT) :: kRef   !Thermal conductivity, kW/m-K
REAL, INTENT(OUT) :: kfRef  !Liquid thermal conductivity, kW/m-K 
REAL, INTENT(OUT) :: kgRef  !Vapor thermal conductivity, kW/m-K
REAL, INTENT(OUT) :: SigmaRef  !Surface tension, N/m

!LOCAL VARIABLES:
REAL Wlocal !Local oil mass fraction

!FLOW:

	Pressure=pRef*1000
	Enthalpy=hRef*1000

	tRef=PH(RefName, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
		WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 3144'
	    ErrorFlag=REFPROPERROR
		RETURN
    END IF
	
	xRef=PH(RefName, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
		WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 3151'
	    ErrorFlag=REFPROPERROR
		RETURN
    END IF
	
	vRef=PH(RefName, Pressure, Enthalpy, 'density', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
		WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 3158'
	    ErrorFlag=REFPROPERROR
		RETURN
    END IF	
	vRef=1/vRef

	cpRef=PH(RefName, Pressure, Enthalpy, 'specificheat', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
		WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 3166'
	    ErrorFlag=REFPROPERROR
		RETURN
    END IF
	cpRef=cpRef/1000
	
	muRef=PH(RefName, Pressure, Enthalpy, 'viscosity', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
		WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 3174'
	    ErrorFlag=REFPROPERROR
		RETURN
    END IF
	
	kRef=PH(RefName, Pressure, Enthalpy, 'conductivity', RefrigIndex,RefPropErr)
	IF (RefPropErr .GT. 0) THEN
		WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 3181'
		ErrorFlag=REFPROPERROR
		RETURN
	END IF
	kRef=kRef/1000

	Temperature=tRef
	Quality=1
	IF (tRef+273.15 .GT. Tcr .OR. tRef+273.15 .LT. 0) THEN
		Psat=pRef
	ELSE 
		Psat=TQ(RefName, Temperature, Quality, 'pressure', RefrigIndex,RefPropErr)
		IF (RefPropErr .GT. 0) THEN
			WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 3194'
			ErrorFlag=REFPROPERROR
			RETURN
		END IF
		Psat=Psat/1000
	END IF

	SigmaRef=PQ(RefName, Pressure, Quality, 'surfacetension', RefrigIndex,RefPropErr)
	IF (RefPropErr .GT. 0) THEN
		WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 4585'
		ErrorFlag=REFPROPERROR
		RETURN
	END IF

	Pressure=pRef*1000
	Quality=0
	hfRef=PQ(RefName, Pressure, Quality, 'enthalpy', RefrigIndex,RefPropErr)
	IF (RefPropErr .GT. 0) THEN
		WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 3205'
		ErrorFlag=REFPROPERROR
		RETURN
	END IF
	hfRef=hfRef/1000
	
	cpfRef=PQ(RefName, Pressure, Quality, 'specificheat', RefrigIndex,RefPropErr)
	IF (RefPropErr .GT. 0) THEN
		WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 3213'
		ErrorFlag=REFPROPERROR
		RETURN
	END IF
	cpfRef=cpfRef/1000

	mufRef=PQ(RefName, Pressure, Quality, 'viscosity', RefrigIndex,RefPropErr)
	IF (RefPropErr .GT. 0) THEN
		WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 3221'
		ErrorFlag=REFPROPERROR
		RETURN
	END IF
	
	kfRef=PQ(RefName, Pressure, Quality, 'conductivity', RefrigIndex,RefPropErr)
	IF (RefPropErr .GT. 0) THEN
		WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 3228'
		ErrorFlag=REFPROPERROR
		RETURN
	END IF
	kfRef=kfRef/1000

	vfRef=PQ(RefName, Pressure, Quality, 'density', RefrigIndex,RefPropErr)
	IF (RefPropErr .GT. 0) THEN
		WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 3236'
		ErrorFlag=REFPROPERROR
		RETURN
	END IF
	vfRef=1/vfRef

	Pressure=pRef*1000
	Quality=1
	hgRef=PQ(RefName, Pressure, Quality, 'enthalpy', RefrigIndex,RefPropErr)
	IF (RefPropErr .GT. 0) THEN
		WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 3246'
		ErrorFlag=REFPROPERROR
		RETURN
	END IF
	hgRef=hgRef/1000
	
	cpgRef=PQ(RefName, Pressure, Quality, 'specificheat', RefrigIndex,RefPropErr)
	IF (RefPropErr .GT. 0) THEN
		WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 3254'
		ErrorFlag=REFPROPERROR
		RETURN
	END IF
	cpgRef=cpgRef/1000
	
	mugRef=PQ(RefName, Pressure, Quality, 'viscosity', RefrigIndex,RefPropErr)
	IF (RefPropErr .GT. 0) THEN
		WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 3262'
		ErrorFlag=REFPROPERROR
		RETURN
	END IF
	
	kgRef=PQ(RefName, Pressure, Quality, 'conductivity', RefrigIndex,RefPropErr)
	IF (RefPropErr .GT. 0) THEN
		WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 3269'
		ErrorFlag=REFPROPERROR
		RETURN
	END IF
	kgRef=kgRef/1000

	vgRef=PQ(RefName, Pressure, Quality, 'density', RefrigIndex,RefPropErr)
	IF (RefPropErr .GT. 0) THEN
		WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 3277'
		ErrorFlag=REFPROPERROR
		RETURN
	END IF
	vgRef=1/vgRef

	Tsat=PQ(RefName, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)
	IF (RefPropErr .GT. 0) THEN
		WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 4566'
		ErrorFlag=REFPROPERROR
		RETURN
	END IF
	IF ((Tsat*1.8+32) .LT. 0) THEN !ISI - 10/11/06
		WRITE(*,*)'-- WARNING -- Condenser: Pressure drop error. Line 4933'
		ErrorFlag=DPERROR
		RETURN
	END IF

	hfgRef=hgRef-hfRef

	!Account for oil effect !ISI - 09/27/06
	IF (xRef .LT. 1 .AND. xRef .GT. 0 .AND. Wabsolute .GT. 0 .AND. LmodTPratio .EQ. 0) THEN
		Wlocal=LocalOilMassFraction(Wabsolute,xRef)
		Tsat=OilMixtureTsat(RefName,Wlocal,Pref*1e-3)
		cpfRef=OilMixtureSpecificHeat(CompManufacturer,RefName,Wlocal,cpfRef*1000,Tsat)*1e-3
		vfRef=1/OilMixtureDensity(CompManufacturer,RefName,Wlocal,1/vfRef,tRef)
		mufRef=OilMixtureViscosity(CompManufacturer,RefName,Wlocal,mufRef,MolWeight,tRef)
		SigmaRef=OilMixtureSurfaceTension(CompManufacturer,RefName,Wlocal,SigmaRef)
		kfRef=OilMixtureThermalConductivity(CompManufacturer,RefName,Wlocal,kfRef*1000)*1e-3
	END IF

RETURN

END SUBROUTINE CalcRefProperty

!************************************************************************

SUBROUTINE CalcSegmentRefOutletPressure(CoilType,TubeType,tRi,pRi,hgRi,hfRi, &
			  	                        hRi,hRo,xRi,vRi,vgRi,vfRi,mRef, &
										muRi,mugRi,mufRi,Sigma,Lsegment,LmodTPratio, &
										IDtube,Elevation,Ltotal,DPrefMultiplier,pRo)

!------------------------------------------------------------------------
!Purpose:
!To calculate segment refrigerant outlet pressure
!
!Author
!Ipseng Iu
!Oklahoma State Univerity, Stillwater
!
!Date
!March 2005
!
!Reference:
!none
!
!------------------------------------------------------------------------

USE FluidProperties
USE CoilCalcMod

IMPLICIT NONE

INTEGER,INTENT(IN) :: CoilType   !1=Condenser; 2=Evaporator; 
                                 !3=High side interconnecting pipes; 
								 !4=Low side interconnecting pipes
								 !5=Microchannel condenser
								 !6=Microchannel evaporator

INTEGER, INTENT(IN) :: TubeType  !1=Plain; 2=General Micro Fin; 3=Herringbone; 
                                 !4=Crosshatch; 5=Herringbone w/crosshatch; 6=Turbo-A
REAL, INTENT(IN) ::  tRi       !Inlet temperature, C
REAL, INTENT(IN) ::  pRi       !Inlet pressure, kPa
REAL, INTENT(IN) ::  hgRi      !Inlet vapor enthalpy, kJ/kg
REAL, INTENT(IN) ::  hfRi      !Inlet liquid enthalpy, kJ/kg
REAL, INTENT(IN) ::  hRi       !Inlet enthalpy, kJ/kg
REAL, INTENT(IN) ::  hRo       !Outlet enthalpy, kJ/kg
REAL, INTENT(IN) ::  xRi       !Inlet quality
REAL, INTENT(IN) ::  vRi       !Inlet specific volume, m^3/kg
REAL, INTENT(IN) ::  vgRi      !Inlet vapor specific volume, m^3/kg
REAL, INTENT(IN) ::  vfRi      !Inlet liquid specific volume, m^3/kg
REAL, INTENT(IN) ::  mRef      !Ref. mass flow rate, kg/s
REAL, INTENT(IN) ::  muRi      !Inlet dynamic viscosity, Pa-s
REAL, INTENT(IN) ::  mugRi     !Inlet vapor dynamic viscosity, Pa-s
REAL, INTENT(IN) ::  mufRi     !Inlet liquid dynamic viscosity, Pa-s
REAL, INTENT(IN) ::  Sigma     !Surface tension, N/m
REAL, INTENT(IN) ::  Lsegment  !Segment length, m
REAL, INTENT(IN) ::  LmodTPratio !Two-phase ratio
REAL, INTENT(IN) ::  IDtube    !Tube inside diameter, m
REAL, INTENT(IN) ::  Elevation !Elevation, m
REAL, INTENT(IN) ::  Ltotal    !Total tube length, m
REAL, INTENT(IN) ::  DPrefMultiplier !Pressure drop multiplier 
REAL, INTENT(OUT) :: pRo       !Outlet pressure, kPa

REAL tRo       !Outlet temperature, C
REAL vgRo      !Inlet vapor specific volume, m^3/kg
REAL vfRo      !Inlet liquid specific volume, m^3/kg
REAL vRo !Outlet specific volume, m^3/kg
REAL xRo !Outlet quality
REAL pRoPrev !Previous value of pRo, for iteration

!FLOW:

	!Find outlet ref. pressure
	pRo=pRi !Initialize
	pRoPrev=BIG !Initialize
	Counter=0
	DO

		Pressure=pRo*1000
		Enthalpy=hRo*1000
		xRo=PH(RefName, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
		IF (RefPropErr .GT. 0) THEN
			WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 1377'
			ErrorFlag=REFPROPERROR
			RETURN
		END IF

	    tRo=PH(RefName, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
		IF (RefPropErr .GT. 0) THEN
			WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 3377'
			ErrorFlag=REFPROPERROR
			RETURN
		END IF

		vRo=PH(RefName, Pressure, Enthalpy, 'density', RefrigIndex,RefPropErr)
		IF (RefPropErr .GT. 0) THEN
			WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 1386'
			ErrorFlag=REFPROPERROR
			RETURN
		END IF
		vRo=1/vRo

		Pressure=pRo*1000
		Quality=1
		vgRo=PQ(RefName, Pressure, Quality, 'density', RefrigIndex,RefPropErr)
		vgRo=1/vgRo
		IF (RefPropErr .GT. 0) THEN
			WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 3394'
			ErrorFlag=REFPROPERROR
			RETURN
		END IF

		Pressure=pRo*1000
		Quality=0
		vfRo=PQ(RefName, Pressure, Quality, 'density', RefrigIndex,RefPropErr)
		vfRo=1/vfRo
		IF (RefPropErr .GT. 0) THEN
			WRITE(*,*)'-- WARNING -- Condenser: Refprop error. Line 3403'
			ErrorFlag=REFPROPERROR
			RETURN
		END IF

		!CALL MODdP(RefName,CoilType,TubeType,tRi,tRo,pRi,hgRi,hfRi, &
		!	  	   hRi,hRo,xRi,xRo,vRi,vRo,vgRi,vfRi,vgRo,vfRo,mRef,muRi,mugRi,mufRi, &
		!		   SigmaMod,Lsegment,LmodTPratio,IDtube,ODtube,Elevation,Ltotal,dPfric,dPmom,dPgrav)
  
		CALL MODdP(RefName,CoilType,TubeType,tRi,tRo,pRi,hgRi,hfRi, &
			  	   hRi,hRo,xRi,xRo,vRi,vRo,vgRi,vfRi,vgRo,vfRo,mRef,muRi,mugRi,mufRi, &
				   SigmaMod,Lsegment,LmodTPratio,IDtube,ODtube,Elevation,Ltotal,dPfric,dPmom,dPgrav)

		IF (ABS(dPfric) .LT. ABS(dPmom)) dPmom=0
  
		dPmod=ABS(dPfric)+ABS(dPmom)+dPgrav
		dPmod=dPmod*DPrefMultiplier
  
		IF (pRi-dPmod .LT. 0) dPmod=0
  
		pRo=pRi-dPmod
		IF (ABS(pRo-pRoPrev)/pRoPrev .LT. SMALL .OR. Counter .GT. PressureMaxIter) THEN
			EXIT
		ELSE
			pRoPrev=pRo
			Counter=Counter+1
		END IF

	END DO !end of pRo

RETURN

END SUBROUTINE CalcSegmentRefOutletPressure

!************************************************************************

SUBROUTINE MicrochannelCondenser(Ref$,XIN,PAR,OUT)

    !-----------------------------------------------------------------------------------
    !
    !  Description:	
    !  A segment-by-segment microchannel condenser model
    !  To predict coil air side and refrigerant side properties, heat transfer, 
    !  and pressure drop
    !
    !  Inputs:
    !  Ref$=Refrigerant name
    !  XIN(1)=Refrigerant side mass flow rate, kg/s
    !  XIN(2)=Refrigerant side inlet (compressor outlet) pressure, kPa
    !  XIN(3)=Refrigerant side inlet (compressor outlet) enthalpy, kJ/kg
    !  XIN(4)=Air side mass flow rate, kg/s
    !  XIN(5)=Air side inlet temp. C
    !  XIN(6)=Air side inlet relative humidity
    !  XIN(7)=Evaporator outlet temperature, C
    !
    !  Parameters:
    !  PAR(1)=Barometric pressure, kPa
    !  PAR(2)=Cooling mode? 1=yes; 0=no  
    !  PAR(3)=Discharge line length, m
    !  PAR(4)=Discharge line outside diameter, m
    !  PAR(5)=Discharge line tube wall thickness, m
    !  PAR(6)=Discharge line elevation, m
    !  PAR(7)=Discharge line heat loss, kW
    !  PAR(8)=Discharge line temperature change, C
    !  PAR(9)=Discharge line additional pressure drop, kPa
    !  PAR(10)=Liquid line length, m
    !  PAR(11)=Liquid line outside diameter, m
    !  PAR(12)=Liquid line tube wall thickness, m 
    !  PAR(13)=Liquid line elevation, m
    !  PAR(14)=Liquid line heat loss, kW
    !  PAR(15)=Liquid line temperature change, C
    !  PAR(16)=Liquid line additional pressure drop, kPa
    !  PAR(17)=Multiplier for ref. side heat transfer correlation
    !  PAR(18)=Multiplier for ref. side pressure drop correlation
    !  PAR(19)=Multiplier for air side heat transfer correlation
    !  PAR(20)=Multiplier for air side pressure drop correlation
    !  PAR(21)=Fan power, kW
    !  PAR(22)=Fan location, 1=draw through; 2=blow through
    !  PAR(23)=Compressor heat loss, kW
    !  PAR(24)=Is compressor in air stream, 1=yes, 0=no
    !  PAR(25)=Custom air side data unit, 1=SI; 2=IP
    !  PAR(26)=Custom air heat transfer curve type, 1=Power; 2=Polynomial
    !  PAR(27)=Power coefficient for air heat transfer curve
    !  PAR(28)=Power coefficient for air heat transfer curve
    !  PAR(29)=Polynomial coefficient for air heat transfer curve
    !  PAR(30)=Polynomial coefficient for air heat transfer curve
    !  PAR(31)=Polynomial coefficient for air heat transfer curve
    !  PAR(32)=Polynomial coefficient for air heat transfer curve
    !  PAR(33)=Custom air heat transfer curve type, 1=Power; 2=Polynomial
    !  PAR(34)=Power coefficient for air heat transfer curve
    !  PAR(35)=Power coefficient for air heat transfer curve
    !  PAR(36)=Polynomial coefficient for air heat transfer curve
    !  PAR(37)=Polynomial coefficient for air heat transfer curve
    !  PAR(38)=Polynomial coefficient for air heat transfer curve
    !  PAR(39)=Polynomial coefficient for air heat transfer curve
    !
    !  Outputs:
    !  OUT(1)=Coil capacity, kW
    !  OUT(2)=Coil inlet pressure, kPa
    !  OUT(3)=Coil inlet enthalpy, kJ/kg
    !  OUT(4)=Coil inlet temperature, C
    !  OUT(5)=Coil inlet quality
    !  OUT(6)=Coil outlet pressure, kPa
    !  OUT(7)=Coil outlet enthalpy, kJ/kg
    !  OUT(8)=Coil outlet temperature, C
    !  OUT(9)=Coil outlet quality
    !  OUT(10)=Coil outlet subcooling, C
    !  OUT(11)=Liquid line outlet pressure, kPa
    !  OUT(12)=Liquid line outlet enthalpy, kJ/kg
    !  OUT(13)=Liquid line outlet temperature, C
    !  OUT(14)=Liquid line outlet quality
    !  OUT(15)=Liquid line outlet subcooling, C
    !  OUT(16)=Air side outlet temperature, C
    !  OUT(17)=Air side outlet relative humidity
    !  OUT(18)=Air side pressure drop, kPa
    !  OUT(19)=Aluminum weight, kg 
    !  OUT(20)=Error flag: 0-No error
    !                      1-Condenser solution not converge
    !                      2-Refprop error
    !					   3-Circuit file error
    !  OUT(21)=Mass in discharge line, kg
    !  OUT(22)=Mass in liquid line, kg
    !  OUT(23)=Mass in coil, kg
    !  OUT(24)=Liquid mass in coil, kg
    !  OUT(25)=Vapor mass in coil, kg
    !
    !  Reference: 
    !  none
    !
    !  Author:
    !  Ipseng Iu
    !  Mechanical and Aerospace Engineering
    !  Oklahoma State University, Stillwater	
    !
    !  Date: November 2005
    !
    !-----------------------------------------------------------------------------------

    USE FluidProperties
    USE CoilCalcMod
    USE AirPropMod
    USE OilMixtureMod
    !USE ReversingValveMod

    IMPLICIT NONE

    !Subroutine argument declarations
    CHARACTER*80, INTENT(IN)  :: Ref$
    REAL,         INTENT(IN)  :: XIN(7)
    REAL,         INTENT(IN)  :: PAR(39)
    REAL,         INTENT(OUT) :: OUT(22)

    !Subroutine local variables
    INTEGER,PARAMETER :: CoilType = MCCONDENSER

    INTEGER I,II,III,IV,V !Loop counters
    LOGICAL Converged     !Convergence flag
    INTEGER AirBCiter     !Air bounadary condition iteration counter
    INTEGER RefBCiter     !Refrigerant bounadary condition iteration counter
    REAL Qtube            !Coil tube capacity, kW
    REAL Qpass            !Coil pass capacity, kW
    REAL QinletPass       !Inlet pass capacity, kW
    REAL PrevpRoMod    !Previous refrigerant outlet pressure, kPa
    REAL DiffpRoMod    !Difference in outlet pressure, kPa
    REAL pRoSlab       !Outlet Refrigerant pressure for a coil slab, kPa
    REAL hRoSlab       !Outlet Refrigerant enthalpy for a coil slab, kJ/kg
    REAL tRoSlab       !Outlet Refrigerant temperature for a coil slab, C  
    REAL xRoSlab       !Outlet Refrigerant quality for a coil slab  
    REAL Aface         !Coil face area, m^2
    REAL tAoSlab       !Outlet air temperature for a coil slab, C
    REAL rhAoSlab      !Outlet air relative humidity for a coil slab
    REAL hAoSlab       !Outlet air enthalpy for a coil slab, kJ/kg
    REAL SumPro        !Sum of outlet pressures, kPa
    REAL SumMrefHro    !Sum of mdot*H (mass flow rate * enthalpy)
    REAL mRefInletPass !Inlet pass mass flow rate, kg/s
    REAL Wlocal !Local oil mass fraction
    REAL tRoEvp !Evaporator outlet temperature
    REAL DPcoil, DPcoilPrev !Coil pressure drop, kPa
    REAL mdothRo !mdot x outlet enthalpy

    !FLOW:

    mRefTot          =XIN(1)
    pRoCmp           =XIN(2)
    hRoCmp           =XIN(3)
    mAiCoil          =XIN(4)
    tAiCoil          =XIN(5)
    rhAiCoil         =XIN(6)
    tRoEvp           =XIN(7)

    BaroPressure     =PAR(1)
    IsCoolingMode    =PAR(2)
    LdisLn           =PAR(3)
    ODdisLn          =PAR(4)
    DisLnThk         =PAR(5)
    ElevDisLn        =PAR(6)
    QdisLn           =PAR(7)
    DTdisLn          =PAR(8)
    AddDPdisLn       =PAR(9)
    LliqLn           =PAR(10)
    ODliqLn          =PAR(11)
    LiqLnThk         =PAR(12)
    ElevLiqLn        =PAR(13)
    QliqLn           =PAR(14)
    DTliqLn          =PAR(15)
    AddDPLiqLn       =PAR(16)	
    hciMultiplier    =PAR(17)
    DPrefMultiplier  =PAR(18)
    hcoMultiplier    =PAR(19)
    DPairMultiplier  =PAR(20)
    PwrFan           =PAR(21)
    DrawBlow         =PAR(22)
    QlossCmp         =PAR(23)
    IsCmpInAirStream =PAR(24)
    CurveUnit        =PAR(25)
    CurveTypeHTC     =PAR(26)
    PowerAHTC        =PAR(27)
    PowerBHTC        =PAR(28)
    Poly1HTC         =PAR(29)
    Poly2HTC         =PAR(30)
    Poly3HTC         =PAR(31)
    Poly4HTC         =PAR(32)
    CurveTypeDP      =PAR(33)
    PowerADP         =PAR(34)
    PowerBDP         =PAR(35)
    Poly1DP          =PAR(36)
    Poly2DP          =PAR(37)
    Poly3DP          =PAR(38)
    Poly4DP          =PAR(39)

    IsParallelSlabs = 1

    ErrorFlag=NOERROR !Initilaize

    !Coil height What is this? Is this right for a microchannel? Yes - ISI - 12/24/2009
    HtCoil=Nt*Pt

    !Coil length
    Lcoil=Nl*Nt*Ltube

    !Face area
    Aface=Ltube*Nt*Pt

    !Tube information
    LmodTube=Ltube/NumOfMods

    CPair=CPA(REAL(tAiCoil))
    Cair=mAiCoil*CPAir

    AirPropOpt=2
    AirProp(1)=tAiCoil
    AirProp(3)=rhAiCoil
    CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
    hAiCoil=AirProp(4)
    wbAiCoil=AirProp(5)

    !IF (IsCoolingMode .LT. 1) THEN
    IF (DrawBlow .EQ. BLOWTHROUGH) THEN !Blow through
        tAiCoil=tAiCoil+PwrFan/Cair
        hAiCoil=hAiCoil+PwrFan/mAiCoil
    END IF
    IF (IsCmpInAirStream .NE. 0) THEN !Compressor in air stream
        tAiCoil=tAiCoil+QlossCmp/Cair
        hAiCoil=hAiCoil+QlossCmp/mAiCoil
    END IF
    !END IF

    AirPropOpt=1
    AirProp(1)=tAiCoil
    AirProp(4)=hAiCoil
    CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
    DensityIn=AirProp(7)

    !Area calculations
    CALL CalcCoilArea(CoilType,Nl,Nt,Pt,Pl,TubeDepth, &
    Ltube,IDtube,TubeHeight,Dchannel,NumOfChannels, &
    FinThk,FinSpg,Lcoil,AfCoil, &
    AoCoil,AiCoil,AmCoil)

    !****** Discharge line calculation ******
    IF (LdisLn .GT. 0) THEN 
        CALL DischargeLine
        IF (ErrorFlag .GT. CONVERGEERROR) THEN
            WRITE(*,*)'-- WARNING -- DischargeLine: Refprop error. Line 4017'
            RETURN
        END IF
    ELSE
        pRiCoil=pRoCmp
        hRiCoil=hRoCmp
    END IF

    !* This needs some work probably - Sankar!!
    !  IF (SystemType .EQ. HEATPUMP) THEN !Heat pump
    !    !Calculate reversing valve heat transfer and pressure drop
    !    Pressure=pRoCmp*1000
    !    Enthalpy=hRoCmp*1000
    !    tRoCmp=PH(RefName, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
    !    IF (RefPropErr .GT. 0) THEN
    !        WRITE(*,*)'-- WARNING -- Evaporator: Refprop error. Line 2646'
    !        ErrorFlag=REFPROPERROR
    !        GOTO 200x
    !    END IF
    !
    !	CALL DischargeHeatTransfer(mRefTot,tRoCmp,tRoEvp,hRoCmp,hRiCoil)
    !  END IF

    Pressure=pRiCoil*1000
    Enthalpy=hRiCoil*1000
    tRiCoil=PH(RefName, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- MCCondenser: Refprop error. Line 3904'
        ErrorFlag=REFPROPERROR
        RETURN
    END IF
    xRiCoil=PH(RefName, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- MCCondenser: Refprop error. Line 3910'
        ErrorFlag=REFPROPERROR
        RETURN
    END IF

    !Initialize boundary conditions
    DO I=1, Nl
        IF (IsParallelSlabs .GT. 0) THEN
            Slab(I)%mdot=mRefTot/Nl !ISI - 07/14/07
        ELSE
            Slab(I)%mdot=mRefTot
        END IF
        DO II=1, Slab(I)%Npass
            DO III=1, Slab(I)%Pass(II)%Ntube
                Slab(I)%Pass(II)%Tube(III)%Seg%mAi=mAiCoil*LmodTube/(Ltube*Nt)
                !Slab(I)%Pass(II)%Tube(III)%Seg%VelDev=1
                Slab(I)%Pass(II)%Tube(III)%Seg%tAi=tAiCoil
                Slab(I)%Pass(II)%Tube(III)%Seg%wbAi=wbAiCoil
                Slab(I)%Pass(II)%Tube(III)%Seg%rhAi=rhAiCoil
                Slab(I)%Pass(II)%Tube(III)%Seg%tAo=tAiCoil
                Slab(I)%Pass(II)%Tube(III)%Seg%wbAo=wbAiCoil
                Slab(I)%Pass(II)%Tube(III)%Seg%rhAo=rhAiCoil
                Slab(I)%Pass(II)%Tube(III)%Seg%pRi=pRiCoil
                Slab(I)%Pass(II)%Tube(III)%Seg%hRi=hRiCoil
                Slab(I)%Pass(II)%Tube(III)%Seg%pRo=pRiCoil
                Slab(I)%Pass(II)%Tube(III)%Seg%hRo=hRiCoil

                !Slab(I)%Pass(II)%mRef=mRefTotal/Slab(I)%Npass
            END DO
        END DO
    END DO

    !Calc. air side mass flow rate at the front row
    VelAvg=(mAiCoil/DensityIn)/Aface
    DO I=1, Nl
        DO II=1, Slab(I)%Npass
            DO III=1, Slab(I)%Pass(II)%Ntube
                DO IV=1, NumOfMods
                    Slab(I)%Pass(II)%Tube(III)%Seg(IV)%Aface=LmodTube/(Ltube*Nt)*Aface
                    IF (Slab(I)%Pass(II)%Tube(III)%Seg(IV)%VelDev .LE. 0) Slab(I)%Pass(II)%Tube(III)%Seg(IV)%VelDev=1
                    Slab(I)%Pass(II)%Tube(III)%Seg(IV)%mAi=mAiCoil*LmodTube/(Ltube*Nt)* &
                    Slab(I)%Pass(II)%Tube(III)%Seg(IV)%VelDev 
                END DO
            END DO
        END DO
    END DO


    !****** Start coil calculation ******
    Converged=.TRUE.  

    !Initialize
    QmodPrev=QdisLn
    DPcoilPrev=1e20

    Qcoil=0.0

    DO RefBCiter=1, MdotMaxIter

        DO I=1, Nl !Number of slabs

            DO II=1,Slab(I)%Npass !Number of passes

                Qpass=0.0

                CALL CalcCircuitRefInletConditions(I,II,CoilType)

                IF (II .EQ. 1 .AND. Slab(I)%Ninlet .GT. 1) THEN !Multi-inlet

                    SumPro=0; SumMrefHro=0;

                    DO V=1, Slab(I)%Ninlet

                        QinletPass=0 

                        Slab(I)%InletPass(V)%pRi=pRiCoil
                        Slab(I)%InletPass(V)%hRi=hRiCoil

                        DO III=1,1 !Slab(I)%Pass(II)%NumOfTubes !Number of tubes
                            DO IV=1,NumOfMods !Number of segments

                                !ref. mass flow rate for each channel
                                !mRefMod=mRefTot/Slab(I)%Pass(II)%Ntube/NumOfChannels
                                mRefMod=Slab(I)%mdot/Slab(I)%Pass(II)%Ntube/NumOfChannels !ISI - 07/13/07

                                AoMod=AoCoil*LmodTube/Lcoil 
                                AfMod=AfCoil*LmodTube/Lcoil 
                                AiMod=AiCoil*LmodTube/Lcoil 
                                AmMod=AmCoil*LmodTube/Lcoil 

                                CALL CalcCoilSegment(I,II,III,IV,CoilType)
                                IF (ErrorFlag .GT. CONVERGEERROR) THEN
                                    !VL: Previously: GOTO 200
                                    OUT(20)=ErrorFlag
                                    RETURN
                                END IF


                                QinletPass=QinletPass+Slab(I)%Pass(II)%Tube(III)%Seg(IV)%Qmod

                            END DO !End mod
                        END DO !End tube

                        Slab(I)%InletPass(V)%Qpass=QinletPass*Slab(I)%InletPass(V)%Ntube

                        Slab(I)%InletPass(V)%pRo=Slab(I)%Pass(II)%Tube(1)%Seg(NumOfMods)%pRo
                        Slab(I)%InletPass(V)%hRo=Slab(I)%Pass(II)%Tube(1)%Seg(NumOfMods)%hRo
                        Slab(I)%InletPass(V)%mRef=mRefTot*Slab(I)%InletPass(V)%Ntube/Slab(I)%Pass(II)%Ntube

                        Qpass=Qpass+Slab(I)%InletPass(V)%Qpass

                        SumPro=SumPro+Slab(I)%InletPass(V)%pRo !Sum of outlet pressures
                        SumMrefHro=SumMrefHro+Slab(I)%InletPass(V)%mRef*Slab(I)%InletPass(V)%hRo !Sum of outlet mdot*hro

                    END DO !End inlet pass

                    pRoCkt=SumPro/Slab(I)%Ninlet
                    hRoCkt=SumMrefHro/mRefTot

                ELSE !Single inlet

                    DO III=1,1 !Slab(I)%Pass(II)%NumOfTubes !Number of tubes

                        DO IV=1,NumOfMods !Number of segments

                            !ref. mass flow rate for each channel
                            !mRefMod=mRefTot/Slab(I)%Pass(II)%Ntube/NumOfChannels
                            mRefMod=Slab(I)%mdot/Slab(I)%Pass(II)%Ntube/NumOfChannels !ISI - 07/13/07

                            AoMod=AoCoil*LmodTube/Lcoil
                            AfMod=AfCoil*LmodTube/Lcoil
                            AiMod=AiCoil*LmodTube/Lcoil
                            AmMod=AmCoil*LmodTube/Lcoil

                            CALL CalcCoilSegment(I,II,III,IV,CoilType)
                            IF (ErrorFlag .GT. CONVERGEERROR) THEN
                                !VL: Previously: GOTO 200
                                OUT(20)=ErrorFlag
                                RETURN
                            END IF


                            !Calc. circuit heat transfer
                            !Qtube=Qtube+Slab(I)%Pass(II)%Tube(1)%Seg(IV)%Qmod

                        END DO !End mod

                    END DO !End tube

                    pRoCkt=Slab(I)%Pass(II)%Tube(1)%Seg(NumOfMods)%pRo !Circuit outlet pressure
                    hRoCkt=Slab(I)%Pass(II)%Tube(1)%Seg(NumOfMods)%hRo !Circuit outlet enthalpy

                END IF

                Slab(I)%Pass(II)%pRo=pRoCkt
                Slab(I)%Pass(II)%hRo=hRoCkt

            END DO !End pass

            Slab(I)%pRi=Slab(I)%Pass(1)%Tube(1)%Seg(1)%pRi
            Slab(I)%hRi=Slab(I)%Pass(1)%Tube(1)%Seg(1)%hRi

            Slab(I)%tAi=Slab(I)%Pass(1)%Tube(1)%Seg(1)%tAi
            Slab(I)%rhAi=Slab(I)%Pass(1)%Tube(1)%Seg(1)%rhAi

            AirPropOpt=2
            AirProp(1)=Slab(I)%tAi
            AirProp(3)=Slab(I)%rhAi
            CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
            Slab(I)%hAi=AirProp(4)

            IF (Slab(1)%Npass .EQ. 1 .AND. Slab(1)%Ninlet .GT. 1) THEN !1-pass, multi-inlet
                pRoSlab=pRoCkt
                hRoSlab=hRoCkt
            ELSE !Single inlet
                pRoSlab=Slab(I)%Pass(Slab(I)%Npass)%pRo
                hRoSlab=Slab(I)%Pass(Slab(I)%Npass)%hRo
            END IF

            Pressure=pRoSlab*1000
            Enthalpy=hRoSlab*1000
            tRoSlab=PH(RefName, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
            IF (RefPropErr .GT. 0) THEN
                WRITE(*,*)'-- WARNING -- MCCondenser: Refprop error.'
                ErrorFlag=REFPROPERROR
                !VL: Previously: GOTO 200
                OUT(20)=ErrorFlag
                RETURN
            END IF
            xRoSlab=PH(RefName, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
            IF (RefPropErr .GT. 0) THEN
                WRITE(*,*)'-- WARNING -- MCCondenser: Refprop error.'
                ErrorFlag=REFPROPERROR
                !VL: Previously: GOTO 200
                OUT(20)=ErrorFlag
                RETURN
            END IF

            Slab(I)%pRo=pRoSlab
            Slab(I)%hRo=hRoSlab
            Slab(I)%tRo=tRoSlab
            Slab(I)%xRo=xRoSlab

            !Slab(I)%Qslab=mRefTot*(Slab(I)%hRi-Slab(I)%hRo)
            Slab(I)%Qslab=Slab(I)%mdot*(Slab(I)%hRi-Slab(I)%hRo) !ISI - 01/06/08

            CALL CalcMeanProp(Slab(I)%tAi,Slab(I)%tAo,tAmod)

            !Coil air side outlet conditions
            !CPair=CPA(REAL(Slab(I)%tAi))
            CPair=CPA(REAL(tAmod))
            Cair=mAicoil*CPAir

            Slab(I)%tAo=Slab(I)%tAi+Slab(I)%Qslab/Cair
            Slab(I)%hAo=Slab(I)%hAi+Slab(I)%Qslab/mAiCoil

            AirPropOpt=1
            AirProp(1)=Slab(I)%tAo
            AirProp(4)=Slab(I)%hAo
            CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
            Slab(I)%rhAo=AirProp(3)

        END DO !End Slabs

        !ISI - 07/14/07
        IF (Nl .GT. 1) THEN

            IF (IsParallelSlabs .GT. 0) THEN
                CALL UpdateMCrefMassFlowRate(Slab,Nl,mRefTot,DPcoil)

                IF (ABS(DPcoil-DPcoilPrev)/DPcoilPrev .GT. SMALL) THEN
                    DPcoilPrev=DPcoil
                ELSE

                    !Calculate outlet pressure and enthalpy
                    pRoCoil=pRiCoil-DPcoil
                    mdothRo=0
                    DO V=1, Nl
                        mdothRo=mdothRo+Slab(V)%mdot*Slab(V)%hRo
                    END DO
                    hRoCoil=mdothRo/mRefTot

                    EXIT
                END IF

            ELSE !Series

                !Calculate outlet pressure and enthalpy
                DPcoil=Slab(1)%pRi-Slab(Nl)%pRo
                pRoCoil=Slab(Nl)%pRo
                hRoCoil=Slab(Nl)%hRo
                EXIT

            END IF
        ELSE
            !Calculate outlet pressure and enthalpy
            DPcoil=Slab(1)%pRi-Slab(1)%pRo
            pRoCoil=Slab(1)%pRo
            hRoCoil=Slab(1)%hRo
            EXIT
        END IF        

    END DO !End iter

    !pRoCoil=Slab(Nl)%Pass(Slab(Nl)%Npass)%pRo
    !hRoCoil=Slab(Nl)%Pass(Slab(Nl)%Npass)%hRo

    Qcoil=mRefTot*(hRiCoil-hRoCoil)

    !Coil air side outlet conditions
    !CPair=CPA(REAL(tAiCoil))
    CPair=CPA(REAL(tAmod))
    Cair=mAicoil*CPAir

    tAoCoil=tAiCoil+Qcoil/Cair
    hAoCoil=hAiCoil+Qcoil/mAiCoil

    !Fan air side inlet conditions
    CPair=CPA(REAL(tAoCoil))
    Cair=mAiCoil*CPAir

    !IF (IsCoolingMode .LT. 1 .AND. DrawBlow .EQ. DRAWTHROUGH) THEN !Draw through
    IF (DrawBlow .EQ. DRAWTHROUGH) THEN !Draw through
        tAoCoil=tAoCoil+PwrFan/Cair
        hAoCoil=hAoCoil+PwrFan/mAiCoil
    END IF

    AirPropOpt=1
    AirProp(1)=tAiCoil
    AirProp(4)=hAiCoil
    CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
    rhAiCoil=AirProp(3)
    DensityIn=AirProp(7)

    AirPropOpt=1
    AirProp(1)=tAoCoil
    AirProp(4)=hAoCoil
    CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
    rhAoCoil=AirProp(3)
    DensityOut=AirProp(7)  

    WetFlag=0
    RowNum=0   
    CALL AirSideCalc(CoilType,FinType,WetFlag,Nl,Nt,RowNum,tAiCoil,mAiCoil,DensityIn,DensityOut,Pt,Pl,Ltube,HtCoil, &
    IDtube,ODtube,NumOfChannels,Dchannel,TubeHeight,TubeDepth,FinThk,FinSpg,CurveUnit,CurveTypeHTC,PowerAHTC,PowerBHTC, &
    Poly1HTC,Poly2HTC,Poly3HTC,Poly4HTC,CurveTypeDP,PowerADP,PowerBDP, &
    Poly1DP,Poly2DP,Poly3DP,Poly4DP,Lcoil,AfCoil,AoCoil,AiCoil,FaceVel,hco,DPair)

    DPair=DPair*DPairMultiplier

    Pressure=pRoCoil*1000
    Enthalpy=hRoCoil*1000
    tRoCoil=PH(RefName, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- MCCondenser: Refprop error.'
        ErrorFlag=REFPROPERROR
        !VL: Previously: GOTO 200
        OUT(20)=ErrorFlag
        RETURN
    END IF
    xRoCoil=PH(RefName, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- MCCondenser: Refprop error.'
        ErrorFlag=REFPROPERROR
        !VL: Previously: GOTO 200
        OUT(20)=ErrorFlag
        RETURN
    END IF

    Pressure=pRoCoil*1000
    Quality=0
    tSat=PQ(RefName, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- MCCondenser: Refprop error.'
        ErrorFlag=REFPROPERROR
        !VL: Previously: GOTO 200
        OUT(20)=ErrorFlag
        RETURN
    END IF

    IF (xRoCoil .LE. 0.0) THEN 
        tSCoCoil=tSat-tRoCoil !Subcooling
    ELSE
        tSCoCoil=0.0
    END IF

    !****** Liquid line calculation ******
    IF (LliqLn .GT. 0) THEN 
        CALL LiquidLine
        IF (ErrorFlag .GT. CONVERGEERROR) THEN
            WRITE(*,*)'LiquidLine: Refprop error.'
            !VL: Previously: GOTO 200
            OUT(20)=ErrorFlag
            RETURN
        END IF
    ELSE
        pRiExp=pRoCoil
        hRiExp=hRoCoil
    END IF

    Pressure=pRiExp*1000
    Enthalpy=hRiExp*1000
    tRiExp=PH(RefName, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- MCCondenser: Refprop error.'
        ErrorFlag=REFPROPERROR
        !VL: Previously: GOTO 200
        OUT(20)=ErrorFlag
        RETURN
    END IF
    xRiExp=PH(RefName, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- MCCondenser: Refprop error.'
        ErrorFlag=REFPROPERROR
        !VL: Previously: GOTO 200
        OUT(20)=ErrorFlag
        RETURN
    END IF

    Pressure=pRiExp*1000
    Quality=0
    tSat=PQ(RefName, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- MCCondenser: Refprop error.'
        ErrorFlag=REFPROPERROR
        !VL: Previously: GOTO 200
        OUT(20)=ErrorFlag
        RETURN
    END IF

    !IF (Wabsolute .GT. 0) THEN
    !  Wlocal=LocalOilMassFraction(Wabsolute,xRiExp)
    !  tSat=OilMixtureTsat(RefName,Wlocal,Psat/1000)
    !END IF

    IF (xRiExp .LE. 0) THEN
        tSCiExp=tSat-tRiExp
    ELSE
        tSCiExp=0
    END IF

    OUT(1)=Qcoil
    OUT(2)=pRiCoil
    OUT(3)=hRiCoil
    OUT(4)=tRiCoil
    OUT(5)=xRiCoil
    OUT(6)=pRoCoil
    OUT(7)=hRoCoil
    OUT(8)=tRoCoil
    OUT(9)=xRoCoil
    OUT(10)=tSCoCoil
    OUT(11)=pRiExp
    OUT(12)=hRiExp
    OUT(13)=tRiExp
    OUT(14)=xRiExp
    OUT(15)=tSCiExp
    OUT(16)=tAoCoil
    OUT(17)=rhAoCoil
    OUT(18)=DPair
    OUT(19)=WeightAluminum

    OUT(21)=MassDisLn
    OUT(22)=MassLiqLn

!VL: Previously: 200 CONTINUE

    OUT(20)=ErrorFlag

    RETURN

    END SUBROUTINE MicrochannelCondenser

!************************************************************************

SUBROUTINE LoadMicrochannelInputs(FTXIN,FTPAR,MCXIN,MCPAR)

!-----------------------------------------------------------------------------------
!
!  Description:	
!  Transfer input data from subroutine "Condenser" to "MircochannelCondenser"
!
!  Author:
!  Ipseng Iu
!  Mechanical and Aerospace Engineering
!  Oklahoma State University, Stillwater	
!
!  Date: November 2005
!
!-----------------------------------------------------------------------------------

IMPLICIT NONE

REAL, INTENT(IN)  :: FTXIN(8)  !Fin-tube coil input data
REAL, INTENT(IN)  :: FTPAR(55) !Fin-tube coil input parameters
REAL, INTENT(OUT) :: MCXIN(7)  !Microchannel coil input data
REAL, INTENT(OUT) :: MCPAR(39) !Microchannel coil input parameters


!FLOW:

  MCXIN(1)=FTXIN(1) !Refrigerant side mass flow rate, kg/s
  MCXIN(2)=FTXIN(2) !Refrigerant side inlet (compressor outlet) pressure, kPa
  MCXIN(3)=FTXIN(3) !Refrigerant side inlet (compressor outlet) enthalpy, kJ/kg
  MCXIN(4)=FTXIN(4) !Air side mass flow rate, kg/s
  MCXIN(5)=FTXIN(5) !Air side inlet temp. C
  MCXIN(6)=FTXIN(6) !Air side inlet relative humidity
  MCXIN(7)=FTXIN(8) !Evaporator outlet temperature, C

  MCPAR(1)=FTPAR(38) !Barometric pressure, kPa
  MCPAR(2)=FTPAR(27) !Cooling mode? 1=yes; 0=no  
  MCPAR(3)=FTPAR(1)  !Discharge line length, m
  MCPAR(4)=FTPAR(2)  !Discharge line outside diameter, m
  MCPAR(5)=FTPAR(3)  !Discharge line tube wall thickness, m
  MCPAR(6)=FTPAR(4)  !Discharge line elevation, m
  MCPAR(7)=FTPAR(5)  !Discharge line heat loss, kW
  MCPAR(8)=FTPAR(6)  !Discharge line temperature change, C
  MCPAR(9)=FTPAR(7)  !Discharge line additional pressure drop, kPa
  MCPAR(10)=FTPAR(8) !Liquid line length, m
  MCPAR(11)=FTPAR(9) !Liquid line outside diameter, m
  MCPAR(12)=FTPAR(10) !Liquid line tube wall thickness, m 
  MCPAR(13)=FTPAR(11) !Liquid line elevation, m
  MCPAR(14)=FTPAR(12) !Liquid line heat loss, kW
  MCPAR(15)=FTPAR(13) !Liquid line temperature change, C
  MCPAR(16)=FTPAR(14) !Liquid line additional pressure drop, kPa
  MCPAR(17)=FTPAR(30) !Multiplier for ref. side heat transfer correlation
  MCPAR(18)=FTPAR(31) !Multiplier for ref. side pressure drop correlation
  MCPAR(19)=FTPAR(32) !Multiplier for air side heat transfer correlation
  MCPAR(20)=FTPAR(33) !Multiplier for air side pressure drop correlation
  MCPAR(21)=FTPAR(34) !Fan power, kW
  MCPAR(22)=FTPAR(35) !Fan location, 1=draw through; 2=blow through
  MCPAR(23)=FTPAR(39) !Compressor heat loss, kW
  MCPAR(24)=FTPAR(40) !Is compressor in air stream, 1=yes, 0=no
  MCPAR(25)=FTPAR(41) !Custom air side data unit, 1=SI; 2=IP
  MCPAR(26)=FTPAR(42) !Custom air heat transfer curve type, 1=Power; 2=Polynomial
  MCPAR(27)=FTPAR(43) !Power coefficient for air heat transfer curve
  MCPAR(28)=FTPAR(44) !Power coefficient for air heat transfer curve
  MCPAR(29)=FTPAR(45) !Polynomial coefficient for air heat transfer curve
  MCPAR(30)=FTPAR(46) !Polynomial coefficient for air heat transfer curve
  MCPAR(31)=FTPAR(47) !Polynomial coefficient for air heat transfer curve
  MCPAR(32)=FTPAR(48) !Polynomial coefficient for air heat transfer curve
  MCPAR(33)=FTPAR(49) !Custom air heat transfer curve type, 1=Power; 2=Polynomial
  MCPAR(34)=FTPAR(50) !Power coefficient for air heat transfer curve
  MCPAR(35)=FTPAR(51) !Power coefficient for air heat transfer curve
  MCPAR(36)=FTPAR(52) !Polynomial coefficient for air heat transfer curve
  MCPAR(37)=FTPAR(53) !Polynomial coefficient for air heat transfer curve
  MCPAR(38)=FTPAR(54) !Polynomial coefficient for air heat transfer curve
  MCPAR(39)=FTPAR(55) !Polynomial coefficient for air heat transfer curve

  RETURN

END SUBROUTINE LoadMicrochannelInputs

!************************************************************************

SUBROUTINE LoadMicrochannelOutputs(MCOUT,FTOUT)

!-----------------------------------------------------------------------------------
!
!  Description:	
!  Transfer output data from subroutine "MircochannelCondenser" to "Condenser"
!
!  Author:
!  Ipseng Iu
!  Mechanical and Aerospace Engineering
!  Oklahoma State University, Stillwater	
!
!  Date: November 2005
!
!-----------------------------------------------------------------------------------

IMPLICIT NONE

REAL, INTENT(IN)  :: MCOUT(22)  !Microchannel coil output data
REAL, INTENT(OUT) :: FTOUT(29)  !Fin-tube coil output data

!FLOW:

  FTOUT(1)=MCOUT(2)   !Coil inlet pressure, kPa
  FTOUT(2)=MCOUT(3)   !Coil inlet enthalpy, kJ/kg
  FTOUT(3)=MCOUT(4)   !Coil inlet temperature, C
  FTOUT(4)=MCOUT(5)   !Coil inlet quality
  FTOUT(5)=MCOUT(6)   !Coil outlet pressure, kPa
  FTOUT(6)=MCOUT(7)   !Coil outlet enthalpy, kJ/kg
  FTOUT(7)=MCOUT(8)   !Coil outlet temperature, C
  FTOUT(8)=MCOUT(9)   !Coil outlet quality
  FTOUT(9)=MCOUT(10)  !Coil outlet subcooling, C
  FTOUT(10)=MCOUT(11) !Liquid line outlet pressure, kPa
  FTOUT(11)=MCOUT(12) !Liquid line outlet enthalpy, kJ/kg
  FTOUT(12)=MCOUT(13) !Liquid line outlet temperature, C
  FTOUT(13)=MCOUT(14) !Liquid line outlet quality
  FTOUT(14)=MCOUT(15) !Liquid line outlet subcooling, C
  FTOUT(15)=MCOUT(1)  !Coil capacity, kW
  FTOUT(16)=MCOUT(21) !Mass in discharge line, kg
  FTOUT(17)=MCOUT(22) !Mass in liquid line, kg
  FTOUT(18)=0         !Mass in coil, kg
  FTOUT(19)=0         !Liquid mass in coil, kg
  FTOUT(20)=0         !Vapor mass in coil, kg
  FTOUT(21)=MCOUT(16) !Air side outlet temperature, C
  FTOUT(22)=MCOUT(17) !Air side outlet relative humidity
  FTOUT(23)=MCOUT(18) !Air side pressure drop, kPa
  FTOUT(24)=MCOUT(20) !Error flag
  FTOUT(25)=0         !Air side heat transfer coefficients, kW/m^2-K
  FTOUT(26)=0         !Inlet coil surface temperature, C
  FTOUT(27)=0         !Outlet coil surface temperature, C
  FTOUT(28)=MCOUT(19) !Aluminum weight, kg 
  FTOUT(29)=0         !Copper weight, kg

  RETURN

END SUBROUTINE LoadMicrochannelOutputs

!************************************************************************

SUBROUTINE UpdateTubeDataFromCircuitData(I,J)

!-----------------------------------------------------------------------------------
!
!  Description:	
!  Update the Tube data from Circuit "Ckt" data. 
!
!  Author:
!  Ipseng Iu
!  Mechanical and Aerospace Engineering
!  Oklahoma State University, Stillwater	
!
!  Date: May 2006
!
!-----------------------------------------------------------------------------------

IMPLICIT NONE

!Subroutine passing variables
INTEGER, INTENT(IN) :: I !Circuit number
INTEGER, INTENT(IN) :: J !Tube number in calculation sequence

!Subroutine local variables
INTEGER TubeNum !Tube number in circuit diagram

!FLOW:

  TubeNum=Ckt(I)%TubeSequence(J)
  Tube(TubeNum)=Ckt(I)%Tube(J)

  RETURN

END SUBROUTINE UpdateTubeDataFromCircuitData

!************************************************************************

END MODULE CondenserMod

