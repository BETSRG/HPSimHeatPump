!***********************************************************************************

MODULE HeatPumpInput

USE DataSimulation
USE DataGlobals_HPSim, ONLY: RefName    !RS Comment: Needs to be used for implementation with Energy+ currently (7/23/12)

PRIVATE

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

!Compressor Manufacturer
INTEGER,PARAMETER :: COPELAND  = 1 !ISI - 10/05/06
INTEGER,PARAMETER :: BRISTOL   = 2
INTEGER,PARAMETER :: DANFOSS   = 3
INTEGER,PARAMETER :: PANASONIC = 4

!Unit flags !ISI - 07/14/06
INTEGER,PARAMETER :: SI=1
INTEGER,PARAMETER :: IP=2

PUBLIC GetInputs

CONTAINS

SUBROUTINE GetInputs

! ----------------------------------------------------------------------
!
!   Description: To collect all the input data for 
!                steady-state heat pump simulation
!
!   Author:
!   Cavlin Iu
!   Mechanical and Aerospace Engineering
!   Oklahoma State University, Stillwater
!
!   Date:
!   November, 2002
!
! ----------------------------------------------------------------------

USE DataStopCodes
USE InputProcessor_HPSim
!USE InputProcessor
USE DataGlobals_HPSim, ONLY: MaxNameLength, RefName !RS Comment: Needs to be used for implementation with Energy+ currently (7/23/12)

IMPLICIT NONE

!Local variables
REAL RefSimulatedCharge     !Simulated charge at reference point, kg or lbm
REAL SimulatedCharge2       !Simulated charge at 2nd reference point, kg or lbm
REAL LiquidLength2          !Liquid length at 2nd reference point, m or ft
REAL VdotODfan				!Outdoor fan volumetric flow rate
REAL VdotIDfan				!Indoor fan volumetric flow rate
REAL CoolingShTbPAR(5)		!Cooling mode short tube model input data
REAL HeatingShTbPAR(5)		!Heating mode short tube model input data
REAL CoolingCapTubePAR(5)	!Cooling mode cap. tube model input data
REAL HeatingCapTubePAR(5)	!Heating mode cap. tube model input data
REAL CoolingDistubeLength	!Distributor tube length for cooling, mm or ft
REAL HeatingDistubeLength	!Distributor tube length for heating, mm or ft
REAL SucLnPAR(7)			!Suction line parameter
REAL DisLnPAR(7)			!Discharge line parameter
REAL LiqLnPAR(7)			!Liquid line parameter
REAL ValveIDCLnPAR(7)		!Valve to indoor coil line parameter
REAL ValveODCLnPAR(7)		!Valve to outdoor coil line parameter
REAL :: EqLength=0.0				!Equivalent length, m or ft
REAL :: EqDiameter=0.0				!Equivalent diameter, mm or in
REAL EqThickness			!Equivalent thickenss, mm or mil 
REAL TotElevation			!Total elevation, m or ft
REAL TotHeatGain			!Total heat gain, w or Btu/hr
REAL TotTempChange			!Total temperature change, C or F
REAL TotAddDP				!Total additional pressure drop, kPa or psia
REAL VolSucLn				!Suction line volume, m^3 or ft^3
REAL VolDisLn				!Discharge line volume, m^3 or ft^3
REAL VolValveIDCLn			!Valve to IDC line volume, m^3 or ft^3
REAL VolValveODCLn			!Valve to ODC line volume, m^3 or ft^3
REAL TotVolume				!Total line volume, m^3 or ft^3
INTEGER IDdrawBlow			!Indoor Fan location, 1=draw through, 2=blow through
INTEGER ODdrawBlow			!Outdoor Fan location, 1=draw through, 2=blow through
REAL,PARAMETER :: PI=3.14159265 !Pi
REAL,PARAMETER :: CopperDensity=8920 !Density of copper, kg/m3

INTEGER ODC_Nt                        !Number of tubes in transverse direction (normal to air flow)
INTEGER ODC_Nl                        !Number of rows in longitudinal direction (parrallel to air flow)
INTEGER ODC_Nckt                      !Number of circuit
INTEGER ODC_Nmod                      !Number of modules per tube
INTEGER ODC_FinType                   !Fin type: 1=Plain; 2=Wavy; 3=Louver
INTEGER ODC_TubeType                  !1=Plain; 2=General Micro Fin; 3=Herringbone; 4=Crosshatch; 5=Herringbone w/crosshatch; 6=Turbo-A
REAL ODC_TubeOD           !Coil tube outside diameter, m
REAL ODC_TubeThk          !Coil tube wall thickness, m
REAL ODC_Ltube            !Coil single tube length, m
REAL ODC_Ktube            !Coil tube thermal conductivity, kW/m-C
REAL ODC_Pt               !Tube spacing in transverse direction, m (normal to air flow)
REAL ODC_Pl               !Row spacing in longitudinal direction, m (parrallel to air flow)
REAL ODC_FinThk           !Fin thickness, m
REAL ODC_FinPitch         !Fin pitch, fin/m
REAL ODC_Kfin             !Fin thermal conductivity, kW/m-C
REAL ODC_hciMultiplier    !Multiplier for ref. side heat transfer correlation
REAL ODC_DPrefMultiplier  !Multiplier for ref. side pressure drop correlation
REAL ODC_hcoMultiplier    !Multiplier for air side heat transfer correlation
REAL ODC_DPairMultiplier  !Multiplier for air side pressure drop correlation
REAL ODC_SurfAbsorptivity !Surface absorptivity

INTEGER IDC_Nt                        !Number of tubes in transverse direction (normal to air flow)
INTEGER IDC_Nl                        !Number of rows in longitudinal direction (parrallel to air flow)
INTEGER IDC_Nckt                      !Number of circuit
INTEGER IDC_Nmod                      !Number of modules per tube
INTEGER IDC_FinType                   !Fin type: 1=Plain; 2=Wavy; 3=Louver
INTEGER IDC_TubeType                  !1=Plain; 2=General Micro Fin; 3=Herringbone; 4=Crosshatch; 5=Herringbone w/crosshatch; 6=Turbo-A
REAL IDC_TubeOD           !Coil tube outside diameter, m
REAL IDC_TubeThk          !Coil tube wall thickness, m
REAL IDC_Ltube            !Coil single tube length, m
REAL IDC_Ktube            !Coil tube thermal conductivity, kW/m-C
REAL IDC_Pt               !Tube spacing in transverse direction, m (normal to air flow)
REAL IDC_Pl               !Row spacing in longitudinal direction, m (parrallel to air flow)
REAL IDC_FinThk           !Fin thickness, m
REAL IDC_FinPitch         !Fin pitch, fin/m
REAL IDC_Kfin             !Fin thermal conductivity, kW/m-C
REAL IDC_hciMultiplier    !Multiplier for ref. side heat transfer correlation
REAL IDC_DPrefMultiplier  !Multiplier for ref. side pressure drop correlation
REAL IDC_hcoMultiplier    !Multiplier for air side heat transfer correlation
REAL IDC_DPairMultiplier  !Multiplier for air side pressure drop correlation
REAL IDC_SurfAbsorptivity !Surface absorptivity

!Custom air side curve
INTEGER IDC_CurveUnit          !Unit convention of the custom air side curve, 1=SI; 2=IP
INTEGER IDC_CurveTypeHTC       !Curve fit type of the air side heat transfer coefficient
							   !1-Power fit; 2-Polynomial fit
REAL IDC_PowerAHTC !Power fit coefficient for air heat transfer coefficient
REAL IDC_PowerBHTC !Power fit coefficient for air heat transfer coefficient
REAL IDC_Poly1HTC  !Polynomial fit coefficient for air heat transfer coefficient
REAL IDC_Poly2HTC  !Polynomial fit coefficient for air heat transfer coefficient
REAL IDC_Poly3HTC  !Polynomial fit coefficient for air heat transfer coefficient
REAL IDC_Poly4HTC  !Polynomial fit coefficient for air heat transfer coefficient
INTEGER IDC_CurveTypeDP        !Curve fit type of the air side pressure drop
						       !1-Power fit; 2-Polynomial fit
REAL IDC_PowerADP  !Power fit coefficient for air pressure drop
REAL IDC_PowerBDP  !Power fit coefficient for air pressure drop
REAL IDC_Poly1DP   !Polynomial fit coefficient for air pressure drop
REAL IDC_Poly2DP   !Polynomial fit coefficient for air pressure drop
REAL IDC_Poly3DP   !Polynomial fit coefficient for air pressure drop
REAL IDC_Poly4DP   !Polynomial fit coefficient for air pressure drop

INTEGER ODC_CurveUnit          !Unit convention of the custom air side curve, 1=SI; 2=IP
INTEGER ODC_CurveTypeHTC       !Curve fit type of the air side heat transfer coefficient
							   !1-Power fit; 2-Polynomial fit
REAL ODC_PowerAHTC !Power fit coefficient for air heat transfer coefficient
REAL ODC_PowerBHTC !Power fit coefficient for air heat transfer coefficient
REAL ODC_Poly1HTC  !Polynomial fit coefficient for air heat transfer coefficient
REAL ODC_Poly2HTC  !Polynomial fit coefficient for air heat transfer coefficient
REAL ODC_Poly3HTC  !Polynomial fit coefficient for air heat transfer coefficient
REAL ODC_Poly4HTC  !Polynomial fit coefficient for air heat transfer coefficient
INTEGER ODC_CurveTypeDP        !Curve fit type of the air side pressure drop
						       !1-Power fit; 2-Polynomial fit
REAL ODC_PowerADP  !Power fit coefficient for air pressure drop
REAL ODC_PowerBDP  !Power fit coefficient for air pressure drop
REAL ODC_Poly1DP   !Polynomial fit coefficient for air pressure drop
REAL ODC_Poly2DP   !Polynomial fit coefficient for air pressure drop
REAL ODC_Poly3DP   !Polynomial fit coefficient for air pressure drop
REAL ODC_Poly4DP   !Polynomial fit coefficient for air pressure drop

INTEGER I !Loop counter
INTEGER J
CHARACTER*150 LineData
CHARACTER*150 BufferString

INTEGER(2) :: IsHeatPump				!Is heat pump operation flag: 1=yes; 0=no
INTEGER(2) :: IsCoolingMode				!Is cooling mode flag: 1=yes; 0=no
INTEGER(2) :: IsCmpInAirStream          !Is compressor in air stream: 1=yes, 0=no
INTEGER(2) :: CoolingDesignCondition
INTEGER(2) :: HeatingDesignCondition
REAL :: InsideDiameter
REAL :: Temp
REAL :: SuperStc !TXV static superheat, C
REAL :: SuperRtd !TXV rated superheat, C
REAL :: CopperVol !Copper volume, m3
INTEGER CompressorManufacturer
INTEGER ErrorFlag !Error flag
INTEGER(2) :: CoolingExpDevice !Cooling Expansion device: 1=short tube; 2=TXV; 3=Cap. tube
INTEGER(2) :: HeatingExpDevice !Heating Expansion device: 1=short tube; 2=TXV; 3=Cap. tube
REAL :: CoolingTXVcapacity !Cooling TXV capacity, ton
REAL :: HeatingTXVcapacity !Heating TXV capacity, ton
LOGICAL :: HPDataFileExists
CHARACTER(len=MaxNameLength),DIMENSION(200) :: Alphas ! Reads string value from input file
  INTEGER :: NumAlphas               ! States which alpha value to read from a "Number" line
  REAL, DIMENSION(200) :: Numbers    ! brings in data from IP
  INTEGER :: NumNumbers              ! States which number value to read from a "Numbers" line
  INTEGER :: Status                  ! Either 1 "object found" or -1 "not found"

REAL Subcooling   !Design Subcooling
REAL Superheat    !Design Superheat
CHARACTER(len=MaxNameLength)RefrigerantName
REAL NumofRefrigerants    !Number of Refrigerants in Blend
REAL NominalCoolingCapacity
REAL NominalHeatingCapacity
REAL ElectricHeating
CHARACTER(len=MaxNameLength)DesignConditionDescription
!REAL OutdoorEnteringDrybulbTemperature
!REAL OutdoorEnteringWetbulbTemperature
!REAL IndoorEnteringDrybulbTemperature
!REAL IndoorEnteringWetbulbTemperature
REAL RefChg    !Design Refrigerant Charge Mass

!Compressor variables
CHARACTER(len=MaxNameLength)CompressorModel
CHARACTER(len=MaxNameLength)CompressorType
CHARACTER(len=MaxNameLength)Rref    !Compressor Refrigerant
REAL CompressorPower
REAL CompressorHeatLossFraction
REAL CompressorHeatLoss
REAL CompressorVolume
REAL CompressorMassCoefficient1
REAL CompressorMassCoefficient2
REAL CompressorMassCoefficient3
REAL CompressorMassCoefficient4
REAL CompressorMassCoefficient5
REAL CompressorMassCoefficient6
REAL CompressorMassCoefficient7
REAL CompressorMassCoefficient8
REAL CompressorMassCoefficient9
REAL CompressorMassCoefficient10
REAL CompressorPowerCoefficient1
REAL CompressorPowerCoefficient2
REAL CompressorPowerCoefficient3
REAL CompressorPowerCoefficient4
REAL CompressorPowerCoefficient5
REAL CompressorPowerCoefficient6
REAL CompressorPowerCoefficient7
REAL CompressorPowerCoefficient8
REAL CompressorPowerCoefficient9
REAL CompressorPowerCoefficient10
CHARACTER(len=MaxNameLength)CompressorCoefficientsUnitFlag
REAL PowerMultiplier
REAL MassFlowRateMultiplier
REAL UserSpecifiedRatingEvapTemperature
REAL UserSpecifiedRatingCondTemperature
REAL UserSpecifiedRatingSubcooling
REAL UserSpecifiedRatingSuperheat

REAL PwrODfan !Outdoor Fan Power
REAL PwrIDfan !Fan Power

CHARACTER(len=MaxNameLength)SucLn_RefrigerantLine
CHARACTER(len=MaxNameLength)SucLn_TubeType
REAL SucLn_KTube    !Conductivity of Suction Line Tube
REAL SucLn_TubeID   !Inner Diameter of Suction Line Tube
REAL SucLn_Charge   !Suction Line Charge
CHARACTER(len=MaxNameLength)DisLn_RefrigerantLine
CHARACTER(len=MaxNameLength)DisLn_TubeType
REAL DisLn_KTube    !Conductivity of Discharge Line Tube
REAL DisLn_TubeID   !Inner Diameter of Discharge Line Tube
REAL DisLn_Charge   !Discharge Line Charge
CHARACTER(len=MaxNameLength)LiqLn_RefrigerantLine
CHARACTER(len=MaxNameLength)LiqLn_TubeType
REAL LiqLn_KTube    !Conductivity of Liquid Line Tube
REAL LiqLn_TubeID   !Inner Diameter of Liquid Line Tube
REAL LiqLn_Charge   !Liquide Line Charge
CHARACTER(len=MaxNameLength)ValveIDCLn_RefrigerantLine
CHARACTER(len=MaxNameLength)ValveIDCLn_TubeType
REAL ValveIDCLn_KTube    !Conductivity of Valve to IDC Line Tube
REAL ValveIDCLn_TubeID   !Inner Diameter of Valve to IDC Line Tube
REAL ValveIDCLn_Charge   !Charge of Valve to IDC Line Tube
CHARACTER(len=MaxNameLength)ValveODCLn_RefrigerantLine
CHARACTER(len=MaxNameLength)ValveODCLn_TubeType
REAL ValveODCLn_KTube    !Conductivity of Valve to ODC Line Tube
REAL ValveODCLn_TubeID   !Inner Diameter of Valve to ODC Line Tube
REAL ValveODCLn_Charge   !Charge of Valve to ODC Line Tube

CHARACTER(len=MaxNameLength)Acc_Manufacturer
CHARACTER(len=MaxNameLength)Acc_Model
REAL Acc_ChgCap  !Accumulator Charge Capacity
REAL Acc_SysCap  !Accumulator Max. Recommended System Capacity
REAL Acc_Chg  !Accumulator Charge
REAL Acc_DP   !Accumulator Pressure Drop

CHARACTER(len=MaxNameLength)Filter_Manufacturer
CHARACTER(len=MaxNameLength)Filter_Model
REAL Filter_DP    !Filter Pressure Drop

INTEGER(2) :: UseAirHandlerData    !0 is FALSE, 1 is TRUE
REAL AHD_Ton !Tonnage
CHARACTER(len=MaxNameLength)AHD_CM    !Coil Model
CHARACTER(len=MaxNameLength)AHD_AHM   !Air Handler Model

REAL CCD_AC !A-Capacity
REAL CCD_AP   !A-Power
REAL CCD_ASP  !A-Suction Pressure
REAL CCD_BC   !B-Capacity
REAL CCD_BP   !B-Power
REAL CCD_BSP  !B-Suction Pressure
REAL CCD_ACS  !A-Capacity Slope
REAL CCD_APS   !A-Power Slope
REAL CCD_BCS  !B-Capacity Slope
REAL CCD_BPS !B-Power Slope
REAL CCD_ACI !A-Capacity Intercept
REAL CCD_API !A-Power Intercept
REAL CCD_BCI !B-Capacity Intercept
REAL CCD_BPI !B-Power Intercept
REAL CCD_ALT !A-Liquid Temperature

CHARACTER(len=MaxNameLength)ODC_FinName
CHARACTER(len=MaxNameLength)ODC_FinMaterial
CHARACTER(len=MaxNameLength)ODC_TubeName
REAL :: ODC_TubeID
CHARACTER(len=MaxNameLength)IDC_FinName
CHARACTER(len=MaxNameLength)IDC_FinMaterial
CHARACTER(len=MaxNameLength)IDC_TubeName
REAL :: IDC_TubeID
REAL :: TubeNumber
REAL :: SystemCost
REAL TWiC   !RS: Outdoor Entering or Condenser Inlet Wetbulb Temperature
REAL TWiE   !RS: Indoor Entering or Evaporator Inlet Wetbulb Temperature

!Flow:

!INTEGER, PARAMETER :: r64=KIND(1.0D0)  !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12) 
!REAL(r64), DIMENSION(200) :: TmpNumbers !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)
REAL, DIMENSION(200) :: TmpNumbers !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)

CHARACTER(LEN=7),PARAMETER :: FMT_201 = "(10(E))"
CHARACTER(LEN=6),PARAMETER :: FMT_202 = "(A150)"
CHARACTER(LEN=4),PARAMETER :: FMT_203 = "(I1)"

  ODC_SurfAbsorptivity=1
  IDC_SurfAbsorptivity=1
  
  !***************** System data *****************

  CALL GetObjectItem('MainDesignData',1,Alphas,NumAlphas, &
                        TmpNumbers,NumNumbers,Status)
  Numbers = DBLE(TmpNumbers) !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)

  SELECT CASE (Alphas(1)(1:1))
  CASE ('F','f')
      Unit = IP
  CASE ('T','t')
      Unit = SI
  CASE DEFAULT
      !FAIL
  END SELECT
    
  !Calculation mode
  MODE = Numbers(1)
  SystemType = Numbers(2)
    
  SELECT CASE (Alphas(2)(1:1))
  CASE ('F','f')
    IsCoolingMode=0
  CASE DEFAULT
    IsCoolingMode=1
  END SELECT

  Subcooling = Numbers(3)   !Design Subcooling
  Superheat = Numbers(4)    !Design Superheat
  
  RefrigerantName = Alphas(3)
  Ref$=RefrigerantName
  
  NumofRefrigerants = Numbers(5)    !Number of Refrigerants in Blend
  
  DesignConditionDescription = Alphas(4)
  
  TAic = Numbers(6) !OutdoorEnteringDrybulbTemperature
  !RHiC = Numbers(7) !OutdoorEnteringWetbulbTemperature
  TWiC = Numbers(7) !OutdoorEnteringWetbulbTemperature
  TAie = Numbers(8) !IndoorEnteringDrybulbTemperature
  !RHiE = Numbers(9) !IndoorEnteringWetbulbTemperature
  TWiE = Numbers(9) !IndoorEnteringWetbulbTemperature
  RefChg = Numbers(10)    !Design Refrigerant Charge Mass


  !***************** Compressor data *****************

  CALL GetObjectItem('CompressorData',1,Alphas,NumAlphas, &
                      TmpNumbers,NumNumbers,Status)
  Numbers = DBLE(TmpNumbers) !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)
  
  CompressorModel = Alphas(1)
  
  SELECT CASE (Alphas(2)(1:1))
  CASE ('C','c')
	CompressorManufacturer=COPELAND
  CASE ('B','b')
	CompressorManufacturer=BRISTOL
  CASE ('D','d')
	CompressorManufacturer=DANFOSS
  CASE ('P','p')
	CompressorManufacturer=PANASONIC
  CASE DEFAULT
	CompressorManufacturer=BRISTOL
  END SELECT

  CompressorType = Alphas(3)
  Rref=Alphas(4)    !Compressor Refrigerant
  
  CompressorPower = Numbers(1)
  CompPAR(21) = Numbers(2) !CompressorHeatLossFraction
  CompPAR(22) = Numbers(3) !CompressorHeatLoss
  CompPAR(23) = Numbers(4) !CompressorVolume
  CompPAR(11) = Numbers(5) !CompressorMassCoefficient1
  CompPAR(12) = Numbers(6) !CompressorMassCoefficient2
  CompPAR(13) = Numbers(7) !CompressorMassCoefficient3
  CompPAR(14) = Numbers(8) !CompressorMassCoefficient4
  CompPAR(15) = Numbers(9) !CompressorMassCoefficient5
  CompPAR(16) = Numbers(10) !CompressorMassCoefficient6
  CompPAR(17) = Numbers(11) !CompressorMassCoefficient7
  CompPAR(18) = Numbers(12) !CompressorMassCoefficient8
  CompPAR(19) = Numbers(13) !CompressorMassCoefficient9
  CompPAR(20) = Numbers(14) !CompressorMassCoefficient10
  CompPAR(1) = Numbers(15) !CompressorPowerCoefficient1
  CompPAR(2) = Numbers(16) !CompressorPowerCoefficient2
  CompPAR(3) = Numbers(17) !CompressorPowerCoefficient3
  CompPAR(4) = Numbers(18) !CompressorPowerCoefficient4
  CompPAR(5) = Numbers(19) !CompressorPowerCoefficient5
  CompPAR(6) = Numbers(20) !CompressorPowerCoefficient6
  CompPAR(7) = Numbers(21) !CompressorPowerCoefficient7
  CompPAR(8) = Numbers(22) !CompressorPowerCoefficient8
  CompPAR(9) = Numbers(23) !CompressorPowerCoefficient9
  CompPAR(10) = Numbers(24) !CompressorPowerCoefficient10
  
  CompressorCoefficientsUnitFlag = Alphas(5)
  
  CompPAR(25) = Numbers(25) !PowerMultiplier
  CompPAR(26) = Numbers(26) !MassFlowRateMultiplier
  TsiCmp = Numbers(27) !UserSpecifiedRatingEvapTemperature
  TsoCmp = Numbers(28) !UserSpecifiedRatingCondTemperature
  Subcool = Numbers(29) !UserSpecifiedRatingSubcooling
  Super = Numbers(30) !UserSpecifiedRatingSuperheat
  
  !***************** Outdoor coil data *****************

  CALL GetObjectItem('OutdoorCoilData',1,Alphas,NumAlphas, &
                       TmpNumbers,NumNumbers,Status)
  Numbers = DBLE(TmpNumbers) !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)
  
  !Fin type (1-smooth; 2-Wavy; 3-louvered)

  ODC_FinType = Numbers(1)
  
  ODC_FinName = Alphas(1)
  
  ODC_FinPitch = Numbers(2)
  ODC_Kfin = Numbers(3) !Conductivity of Fin
  ODC_FinThk = Numbers(4)   !Fin Thickness
  
  ODC_FinMaterial = Alphas(2)
  ODC_TubeName = Alphas(3)

  ODC_TubeType = Numbers(5) !Numerical Denotion of Tube Type
  ODC_TubeID = Numbers(6)   !Tube Inner Diameter
  ODC_TubeOD = Numbers(7)   !Tube Outer Diameter
  ODC_Ktube = Numbers(8)    !Tube Conductivity
  ODC_Pl = Numbers(9)   !Tube Lateral Spacing
  ODC_Pt = Numbers(10)   !Tube Vertical Spacing
  ODC_Nl = Numbers(11)  !Number of Rows
  ODC_Nt = Numbers(12)  !Number of Tubes per Row
  ODC_Nckt = Numbers(13)    !Number of Circuits
  ODC_Nmod = Numbers(14)    !Number of Segments
  ODC_Ltube = Numbers(15)   !Single Tube Length
  ODC_hciMultiplier = Numbers(16)   !Ref Side Heat Transfer Multiplier
  ODC_DPrefMultiplier = Numbers(17) !Ref Side Pressure Drop Multiplier
  ODC_hcoMultiplier = Numbers(18)   !Air Side Heat Transfer Multiplier
  ODC_DPairMultiplier = Numbers(19) !Air Side Pressure Drop Multiplier

    !Tube wall thickness, mm or mil
  ODC_TubeThk=(ODC_TubeOD-ODC_TubeID)/2
  IF (Unit .EQ. IP) THEN
      ODC_TubeThk=ODC_TubeThk*1000
  END IF

  !***************** Outdoor fan data *****************

  CALL GetObjectItem('OutdoorFanData',1,Alphas,NumAlphas, &
                      TmpNumbers,NumNumbers,Status)
  Numbers = DBLE(TmpNumbers) !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)
  
  PwrODfan = Numbers(1) !Fan Power
  VdotODfan = Numbers(2)    !Fan Air Flow Rate
  ODdrawBlow = Numbers(3)   !Draw Through (1) or Blow Through (2)


  !***************** Indoor coil data *****************

  CALL GetObjectItem('IndoorCoilData',1,Alphas,NumAlphas, &
                      TmpNumbers,NumNumbers,Status)
  Numbers = DBLE(TmpNumbers) !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)
  
  IDC_FinType = Numbers(1)
  
  IDC_FinName = Alphas(1)
  
  IDC_FinPitch = Numbers(2)
  IDC_Kfin = Numbers(3) !Fin Conductivity
  IDC_FinThk = Numbers(4)   !Fin Thickness
  
  IDC_FinMaterial = Alphas(2)
  IDC_TubeName = Alphas(3)
  
  IDC_TubeType = Numbers(5) !Numerical Denotion of the tube type
  IDC_TubeID = Numbers(6)   !Tube Inner Diameter
  IDC_TubeOD = Numbers(7)   !Tube Outer Diameter
  IDC_Ktube = Numbers(8)    !Tube Conductivity
  IDC_Pl = Numbers(9)   !Tube Lateral Spacing
  IDC_Pt = Numbers(10)   !Tube Vertical Spacing
  IDC_Nl = Numbers(11)  !Number of Rows
  IDC_Nt = Numbers(12)  !Number of Tubes Per Row
  IDC_Nckt = Numbers(13)    !Number of Circuits
  IDC_Nmod = Numbers(14)    !Number of Segments
  IDC_Ltube = Numbers(15)   !Length of Tube
  IDC_hciMultiplier = Numbers(16)   !Ref Side Heat Transfer Multiplier
  IDC_DPrefMultiplier = Numbers(17) !Ref Side Pressure Drop Multiplier
  IDC_hcoMultiplier = Numbers(18)   !Air Side Heat Transfer Multiplier
  IDC_DPairMultiplier = Numbers(19) !Air Side Pressure Drop Multiplier

  !Tube wall thickness, mm or mil
  IDC_TubeThk=(IDC_TubeOD-IDC_TubeID)/2
  IF (Unit .EQ. IP) THEN
      IDC_TubeThk=IDC_TubeThk*1000
  END IF

  !***************** Indoor fan data *****************
  
  CALL GetObjectItem('IndoorFanData',1,Alphas,NumAlphas, &
                      TmpNumbers,NumNumbers,Status)
  Numbers = DBLE(TmpNumbers) !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)
  
  PwrIDfan = Numbers(1) !Fan Power
  VdotIDfan = Numbers(2)    !Fan Air Flow Rate
  IDdrawBlow = Numbers(3)   !Draw Through or Blow Through

  !***************** Expansion device data *****************

  CALL GetObjectItem('ExpansionDeviceData',1,Alphas,NumAlphas, &
                      TmpNumbers,NumNumbers,Status)
  Numbers = DBLE(TmpNumbers) !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)
  
  SELECT CASE (Alphas(1)(1:1))
  CASE ('C','c')
    CoolingExpDevice=3 !Cap. tube
  CASE ('T','t')
    CoolingExpDevice=2 !TXV
  CASE DEFAULT
    CoolingExpDevice=1 !Short tube orifice
  END SELECT
  
      SELECT CASE (Alphas(2)(1:1))
      CASE ('C','c')
        HeatingExpDevice=3 !Cap. tube
      CASE ('T','t')
        HeatingExpDevice=2 !TXV
      CASE DEFAULT
        HeatingExpDevice=1 !Short tube orifice
      END SELECT
  
  !Short tube orifice

  !Cooling mode

  CoolingShTbPAR(1) = Numbers(1)    !Length
  CoolingShTbPAR(2) = Numbers(2)    !Diameter
  CoolingShTbPAR(3) = Numbers(3)    !Chamfer Depth

  !Heating mode

  HeatingShTbPAR(1) = Numbers(4)    !Length
  HeatingShTbPAR(2) = Numbers(5)    !Diameter
  HeatingShTbPAR(3) = Numbers(6)    !Chamfer Depth

  !Capillary Tube
  
  !Cooling Mode
  
  CoolingCapTubePAR(2) = Numbers(7) !Length
  CoolingCapTubePAR(1) = Numbers(8) !Diameter
  CoolingCapTubePAR(3) = Numbers(9) !Coil Diameter
  
  !Heating Mode
  
  HeatingCapTubePAR(2) = Numbers(10)    !Length
  HeatingCapTubePAR(1) = Numbers(11)    !Diameter
  HeatingCapTubePAR(3) = Numbers(12)    !Coil Diameter

  !TXV data

  !Rated TXV capacity, ton

  CoolingTXVcapacity = Numbers(13)
  HeatingTXVcapacity = Numbers(14)

  !Distributor tubes

  CoolingDistubeLength = Numbers(15)
  
  IF (Unit .EQ. SI) THEN
    CoolingDistubeLength=CoolingDistubeLength/1000  !RS Comment: Unit Conversion
  ELSEIF (Unit .EQ. IP) THEN
    CoolingDistubeLength=CoolingDistubeLength/12    !RS Comment: Unit Conversion, from in to ft?
  END IF

  HeatingDistubeLength = Numbers(16)
  
  IF (Unit .EQ. SI) THEN
    HeatingDistubeLength=HeatingDistubeLength/1000  !RS Comment: Unit Conversion
  ELSEIF (Unit .EQ. IP) THEN
    HeatingDistubeLength=HeatingDistubeLength/12    !RS Comment: Unit Conversion, from in to ft?
  END IF

  !*****************Refrigerant line data******************

  CALL GetObjectItem('RefrigerantLineData',1,Alphas,NumAlphas, &
                      TmpNumbers,NumNumbers,Status)
  Numbers = DBLE(TmpNumbers) !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)
  
  !Suction Line
  
  SucLn_RefrigerantLine = Alphas(1)
  SucLn_TubeType = Alphas(2)
  
  SucLnPAR(1) = Numbers(1)  !Refrigerant Line Length
  SucLnPAR(4) = Numbers(2)  !Refrigerant Line Elevation
  SucLnPAR(5) = Numbers(3)  !Refrigerant Line Heat Loss
  SucLnPAR(6) = Numbers(4)  !Refrigerant Line Temperature Change
  SucLn_KTube = Numbers(5)
  SucLn_TubeID = Numbers(6)
  SucLnPAR(2) = Numbers(7)  !Tube Outside Diameter
  SucLnPAR(7) = Numbers(8)  !Additional Pressure Drop
  SucLn_Charge = Numbers(9) !Charge in Line

  !Suction line tube wall thickness, mm or mil
  SucLnPAR(3)=(SucLnPAR(2)-SucLn_TubeID)/2
  IF (Unit .EQ. IP) THEN
      SucLnPAR(3)=SucLnPAR(3)*1000  !RS Comment: Unit Conversion
  END IF

  !Discharge Line
  
  DisLn_RefrigerantLine = Alphas(3)
  DisLn_TubeType = Alphas(4)
  
  DisLnPAR(1) = Numbers(10) !Refrigerant Line Length
  DisLnPAR(4) = Numbers(11) !Refrigerant Line Elevation
  DisLnPAR(5) = Numbers(12) !Refrigerant Line Heat Loss
  DisLnPAR(6) = Numbers(13) !Refrigerant Line Temperature Change
  DisLn_Ktube = Numbers(14)
  DisLn_TubeID = Numbers(15)
  DisLnPAR(2) = Numbers(16) !Tube Outside Diameter
  DisLnPAR(7) = Numbers(17) !Additional Pressure Drop
  DisLn_Charge = Numbers(18)    !Charge in Line

  !Discharge line tube wall thickness, mm or mil
  DisLnPAR(3)=(DisLnPAR(2)-DisLn_TubeID)/2
  IF (Unit .EQ. IP) THEN
      DisLnPAR(3)=DisLnPAR(3)*1000  !RS Comment: Unit Conversion
  END IF

   !Liquid Line
  
  LiqLn_RefrigerantLine = Alphas(5)
  LiqLn_TubeType = Alphas(6)
  
  LiqLnPAR(1) = Numbers(19) !Refrigerant Line Length
  LiqLnPAR(4) = Numbers(20) !Refrigerant Line Elevation
  LiqLnPAR(5) = Numbers(21) !Refrigerant Line Heat Loss
  LiqLnPAR(6) = Numbers(22) !Refrigerant Line Temperature Change
  LiqLn_Ktube = Numbers(23) !Tube Conductivity
  LiqLn_TubeID = Numbers(24)
  LiqLnPAR(2) = Numbers(25) !Tube Outside Diameter
  LiqLnPAR(7) = Numbers(26) !Additional Pressure Drop
  LiqLn_Charge = Numbers(27)    !Charge in Line

  !Liquid line tube wall thickness, mm or mil
  LiqLnPAR(3)=(LiqLnPAR(2)-LiqLn_TubeID)/2
  IF (Unit .EQ. IP) THEN
      LiqLnPAR(3)=LiqLnPAR(3)*1000  !RS Comment: Unit Conversion
  END IF

  !Reversing Valve to IDC
  
  ValveIDCLn_RefrigerantLine = Alphas(7)
  ValveIDCLn_TubeType = Alphas(8)
  
  ValveIDCLnPAR(1) = Numbers(28)    !Refrigerant Line Length
  ValveIDCLnPAR(4) = Numbers(29)    !Refrigerant Line Elevation
  ValveIDCLnPAR(5) = Numbers(30)    !Refrigerant Line Heat Loss
  ValveIDCLnPAR(6) = Numbers(31)    !Refrigerant Line Temperature Change
  ValveIDCLn_Ktube = Numbers(32)
  ValveIDCLn_TubeID = Numbers(33)
  ValveIDCLnPAR(2) = Numbers(34)    !Tube Outside Diameter
  ValveIDCLnPAR(7) = Numbers(35)    !Additional Pressure Drop
  ValveIDCLn_Charge = Numbers(36)   !Charge in Line

  !Valve to IDC line tube wall thickness, mm or mil
  ValveIDCLnPAR(3)=(ValveIDCLnPAR(2)-ValveIDCLn_TubeID)/2
  IF (Unit .EQ. IP) THEN
      ValveIDCLnPAR(3)=ValveIDCLnPAR(3)*1000    !RS Comment: Unit Conversion
  END IF

    !Valve to ODC Line
  
  ValveODCLn_RefrigerantLine = Alphas(9)
  ValveODCLn_TubeType = Alphas(10)
  
  ValveODCLnPAR(1) = Numbers(37)    !Refrigerant Line Length
  ValveODCLnPAR(4) = Numbers(38)    !Refrigerant Line Elevation
  ValveODCLnPAR(5) = Numbers(39)    !Refrigerant Line Heat Loss
  ValveODCLnPAR(6) = Numbers(40)    !Refrigerant Line Temperature Change
  ValveODCLn_Ktube = Numbers(41)
  ValveODCLn_TubeID = Numbers(42)
  ValveODCLnPAR(2) = Numbers(43)    !Tube Outside Diameter
  ValveODCLnPAR(7) = Numbers(44)    !Additional Pressure Drop

  !Valve to ODC line tube wall thickness, mm or mil
  ValveODCLnPAR(3)=(ValveODCLnPAR(2)-ValveODCLn_TubeID)/2
  IF (Unit .EQ. IP) THEN
      ValveODCLnPAR(3)=ValveODCLnPAR(3)*1000    !RS Comment: Unit Conversion
  END IF

  !********************Refrigerant Cycle Data (Cooling)***********************

  CALL GetObjectItem('RefrigerantCycleData(Cooling)',1,Alphas,NumAlphas, &
                      TmpNumbers,NumNumbers,Status) 
  Numbers = DBLE(TmpNumbers) !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)
  
  !Expansion Device Inlet
  
  Tliq = Numbers(1)    !Inlet Temperature
  
  !********************Refrigerant Cycle Data (Heating)***********************

  CALL GetObjectItem('RefrigerantCycleData(Heating)',1,Alphas,NumAlphas, &
                      TmpNumbers,NumNumbers,Status)
  Numbers = DBLE(TmpNumbers) !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)

  !Compressor Discharge
  
  Tdis = Numbers(1)  !Discharge Temperature

  !Indoor Coil Outlet
  
  BaroPressure = Numbers(2)  !Barometric Pressure
  IsCmpInAirStream = Numbers(3) !Is Compressor in Air Stream

  !*************** Accumulator ****************

  CALL GetObjectItem('AccumulatorData',1,Alphas,NumAlphas, &
                      TmpNumbers,NumNumbers,Status)
  Numbers = DBLE(TmpNumbers) !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)
  
  Acc_Manufacturer = Alphas(1)
  Acc_Model = Alphas(2)
  
  AccumPAR(2) = Numbers(1)  !Height
  AccumPAR(1) = Numbers(2)  !Diameter
  AccumPAR(4) = Numbers(3)  !Upper hole diameter
  AccumPAR(3) = Numbers(4)  !Lower hole diameter
  AccumPAR(7) = Numbers(5)  !Rating Pressure Drop
  AccumPAR(5) = Numbers(6) !Hole distance
  AccumPAR(8) = Numbers(7) !Rating Temperature Drop
  AccumPAR(9) = Numbers(8) !Coefficient M
  AccumPAR(10) = Numbers(9)    !Coefficient B

  AccumPAR(6)=(SucLnPAR(2)-SucLnPAR(3)/1000*2) !J-tube diameter, mm or in

  !*************** Filter Drier ****************

  CALL GetObjectItem('FilterDrierData',1,Alphas,NumAlphas, &
                      TmpNumbers,NumNumbers,Status)
  Numbers = DBLE(TmpNumbers) !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)
  
  Filter_Manufacturer = Alphas(1)
  Filter_Model = Alphas(2)
  
  FilterPAR(1) = Numbers(1) !Flow capacity
  FilterPAR(2) = Numbers(2) !Rating DP
  
  !*************** Custom Air Side Heat Transfer Data **************

  CALL GetObjectItem('CustomAirSideHeatTransferData',1,Alphas,NumAlphas, &
                      TmpNumbers,NumNumbers,Status) 
  Numbers = DBLE(TmpNumbers) !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)
  
  !---Indoor Coil---
  
  IDC_CurveUnit = Numbers(1)
  
  !Heat Transfer data

  IDC_CurveTypeHTC = Numbers(2) !Curve Type
  IDC_PowerAHTC = Numbers(3)    !Power Fit Coefficient A
  IDC_PowerBHTC = Numbers(4)    !Power Fit Coefficient B
  IDC_Poly1HTC = Numbers(5) !Polynomial Fit Coefficient C1
  IDC_Poly2HTC = Numbers(6) !Polynomial Fit Coefficient C2
  IDC_Poly3HTC = Numbers(7) !Polynomial Fit Coefficient C3
  IDC_Poly4HTC = Numbers(8) !Polynomial Fit Coefficent C4

  !Pressure drop data

  IDC_CurveTypeDP = Numbers(9) !Curve Type
  IDC_PowerADP = Numbers(10)    !Power Fit Coefficient A
  IDC_PowerBDP = Numbers(11)    !Power Fit Coefficient B
  IDC_Poly1DP = Numbers(12) !Polynomial Fit Coefficient C1
  IDC_Poly2DP = Numbers(13) !Polynomial Fit Coefficient C2
  IDC_Poly3DP = Numbers(14) !Polynomial Fit Coefficient C3
  IDC_Poly4DP = Numbers(15) !Polynomial Fit Coefficent C4
  
  
  !---Outdoor Coil---
  
  ODC_CurveUnit = Numbers(16)

  !Heat Transfer data
  
  ODC_CurveTypeHTC = Numbers(17) !Curve Type
  ODC_PowerAHTC = Numbers(18)    !Power Fit Coefficient A
  ODC_PowerBHTC = Numbers(19)    !Power Fit Coefficient B
  ODC_Poly1HTC = Numbers(20) !Polynomial Fit Coefficient C1
  ODC_Poly2HTC = Numbers(21) !Polynomial Fit Coefficient C2
  ODC_Poly3HTC = Numbers(22) !Polynomial Fit Coefficient C3
  ODC_Poly4HTC = Numbers(23) !Polynomial Fit Coefficent C4

  !Pressure drop data
  
  ODC_CurveTypeDP = Numbers(9) !Curve Type
  ODC_PowerADP = Numbers(10)    !Power Fit Coefficient A
  ODC_PowerBDP = Numbers(11)    !Power Fit Coefficient B
  ODC_Poly1DP = Numbers(12) !Polynomial Fit Coefficient C1
  ODC_Poly2DP = Numbers(13) !Polynomial Fit Coefficient C2
  ODC_Poly3DP = Numbers(14) !Polynomial Fit Coefficient C3
  ODC_Poly4DP = Numbers(15) !Polynomial Fit Coefficent C4

 
  !*************** Charge Tuning Curve ***************

  CALL GetObjectItem('ChargeTuningCurve',1,Alphas,NumAlphas, &
                        TmpNumbers,NumNumbers,Status) !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)     

  Numbers = DBLE(TmpNumbers) !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)
  
  SELECT CASE (Alphas(1)(1:1))  !Is Charge Tuning?
  CASE ('F','f')
      IsChargeTuning=0
  CASE ('T','t')
      IsChargeTuning=1
  END SELECT
  
  RefSimulatedCharge = Numbers(1)   !Tuning Point #1 Simulated Charge
  RefLiquidLength = Numbers(2)  !Tuning Point #1 Liquid Length
  SimulatedCharge2 = Numbers(3) !Tuning Point #2 Simulated Charge
  LiquidLength2 = Numbers(4)    !Tuning Points #2 Liquid Length
  
  !store the refrigerant name in data globals
  RefName = Ref$
  
  !Calculate charge tuning curve
  IF (MODE .NE. 2 .AND. (RefLiquidLength-LiquidLength2) .NE. 0) THEN
	  IF (RefChg .GT. 0) THEN
		  ChargeCurveSlope=(SimulatedCharge2-RefSimulatedCharge)/ &
						   (LiquidLength2-RefLiquidLength)
		  ChargeCurveIntercept=RefChg-RefSimulatedCharge
	  ELSE
		  ChargeCurveSlope=0
		  ChargeCurveIntercept=0
	  END IF
  END IF

  !***** Calculate weight of interconnecting pipes ****

  IF (Unit .EQ. SI) THEN
	  CopperVol=PI*(DisLnPAR(2)**2-(DisLnPAR(2)-2*DisLnPAR(3))**2)/4/1000*DisLnPAR(1)
	  WeightDisLn=CopperVol*CopperDensity
	  
	  CopperVol=PI*(SucLnPAR(2)**2-(SucLnPAR(2)-2*SucLnPAR(3))**2)/4/1000*SucLnPAR(1)
	  WeightSucLn=CopperVol*CopperDensity
	  
	  CopperVol=PI*(LiqLnPAR(2)**2-(LiqLnPAR(2)-2*LiqLnPAR(3))**2)/4/1000*LiqLnPAR(1)
	  WeightLiqLn=CopperVol*CopperDensity
	  
	  CopperVol=PI*(ValveIDCLnPAR(2)**2-(ValveIDCLnPAR(2)-2*ValveIDCLnPAR(3))**2)/4/1000*ValveIDCLnPAR(1)
	  WeightValveIDCLn=CopperVol*CopperDensity
	  
	  CopperVol=PI*(ValveODCLnPAR(2)**2-(ValveODCLnPAR(2)-2*ValveODCLnPAR(3))**2)/4/1000*ValveODCLnPAR(1)
	  WeightValveODCLn=CopperVol*CopperDensity
  ELSE
	  CopperVol=PI*(DisLnPAR(2)**2-(DisLnPAR(2)-2*DisLnPAR(3)/1000)**2)/4/144*DisLnPAR(1)
	  CopperVol=CopperVol/35.31467 !Convert from ft3 to m3
	  WeightDisLn=CopperVol*CopperDensity

	  CopperVol=PI*(SucLnPAR(2)**2-(SucLnPAR(2)-2*SucLnPAR(3)/1000)**2)/4/144*SucLnPAR(1)
	  CopperVol=CopperVol/35.31467 !Convert from ft3 to m3
	  WeightSucLn=CopperVol*CopperDensity

	  CopperVol=PI*(LiqLnPAR(2)**2-(LiqLnPAR(2)-2*LiqLnPAR(3)/1000)**2)/4/144*LiqLnPAR(1)
	  CopperVol=CopperVol/35.31467 !Convert from ft3 to m3
	  WeightLiqLn=CopperVol*CopperDensity

	  CopperVol=PI*(ValveIDCLnPAR(2)**2-(ValveIDCLnPAR(2)-2*ValveIDCLnPAR(3)/1000)**2)/4/144*ValveIDCLnPAR(1)
	  CopperVol=CopperVol/35.31467 !Convert from ft3 to m3
	  WeightValveIDCLn=CopperVol*CopperDensity

	  CopperVol=PI*(ValveODCLnPAR(2)**2-(ValveODCLnPAR(2)-2*ValveODCLnPAR(3)/1000)**2)/4/144*ValveODCLnPAR(1)
	  CopperVol=CopperVol/35.31467 !Convert from ft3 to m3
	  WeightValveODCLn=CopperVol*CopperDensity

  END IF

  IF (SystemType .EQ. HEATPUMP) THEN

	CondPAR(58)=HeatingDistubeLength

    IF (IsCoolingMode .GT. 0) THEN

	  IF (Unit .EQ. SI) THEN !SI unit

	    !Equilibruim Discharge line, combines compressor discharge line and valve to ODC line
	    VolDisLn=PI*((DisLnPAR(2)-2*DisLnPAR(3))*1e-3)**2/4*DisLnPAR(1)
	    VolValveODCLn=PI*((ValveODCLnPAR(2)-2*ValveODCLnPAR(3))*1e-3)**2/4*ValveODCLnPAR(1)
	    TotVolume=VolDisLn+VolValveODCLn

	    IF (VolValveODCLn .LE. 0) THEN
			EqDiameter=DisLnPAR(2)
			EqThickness=DisLnPAR(3)
			TotElevation=DisLnPAR(4)
			TotHeatGain=DisLnPAR(5)
			TotTempChange=DisLnPAR(6)
			TotAddDP=DisLnPAR(7)
		ELSE
			EqLength=DisLnPAR(1)+ValveODCLnPAR(1) !ISI - 08/03/06
			EqThickness=(DisLnPAR(3)+ValveODCLnPAR(3))/2
			TotElevation=DisLnPAR(4)+ValveODCLnPAR(4)
			TotHeatGain=DisLnPAR(5)+ValveODCLnPAR(5)
			TotTempChange=DisLnPAR(6)+ValveODCLnPAR(6)
			TotAddDP=DisLnPAR(7)+ValveODCLnPAR(7)
		END IF
		EqDiameter=SQRT(4*TotVolume/(PI*EqLength))*1000+2*EqThickness !ISI - 08/03/06

        DisLnPAR(1)=EqLength
	    DisLnPAR(2)=EqDiameter
	    DisLnPAR(3)=EqThickness
	    DisLnPAR(4)=TotElevation
	    DisLnPAR(5)=TotHeatGain
	    DisLnPAR(6)=TotTempChange
	    DisLnPAR(7)=TotAddDP

        !Equilibruim suction line, combines compressor suction line and valve to IDC line
	    VolSucLn=PI*((SucLnPAR(2)-2*SucLnPAR(3))*1e-3)**2/4*SucLnPAR(1)
	    VolValveIDCLn=PI*((ValveIDCLnPAR(2)-2*ValveIDCLnPAR(3))*1e-3)**2/4*ValveIDCLnPAR(1)
	    TotVolume=VolSucLn+VolValveIDCLn

	    IF (VolValveIDCLn .LE. 0) THEN
			EqDiameter=SucLnPAR(2)
			EqThickness=SucLnPAR(3)
			TotElevation=SucLnPAR(4)
			TotHeatGain=SucLnPAR(5)
			TotTempChange=SucLnPAR(6)
			TotAddDP=SucLnPAR(7)
		ELSE
			EqLength=SucLnPAR(1)+ValveIDCLnPAR(1) !ISI - 08/03/06
			EqThickness=(SucLnPAR(3)+ValveIDCLnPAR(3))/2
			TotElevation=SucLnPAR(4)+ValveIDCLnPAR(4)
			TotHeatGain=SucLnPAR(5)+ValveIDCLnPAR(5)
			TotTempChange=SucLnPAR(6)+ValveIDCLnPAR(6)
			TotAddDP=SucLnPAR(7)+ValveIDCLnPAR(7)
		END IF
		EqDiameter=SQRT(4*TotVolume/(PI*EqLength))*1000+2*EqThickness !ISI - 08/03/06
	
        SucLnPAR(1)=EqLength
	    SucLnPAR(2)=EqDiameter
	    SucLnPAR(3)=EqThickness
	    SucLnPAR(4)=TotElevation
	    SucLnPAR(5)=TotHeatGain
	    SucLnPAR(6)=TotTempChange
	    SucLnPAR(7)=TotAddDP

	  ELSE !IP unit

	    !Equilibruim Discharge line, combines compressor discharge line and valve to ODC line
	    VolDisLn=PI*((DisLnPAR(2)-2*DisLnPAR(3)*1e-3)/12)**2/4*DisLnPAR(1)
	    VolValveODCLn=PI*((ValveODCLnPAR(2)-2*ValveODCLnPAR(3)*1e-3)/12)**2/4*ValveODCLnPAR(1)
	    TotVolume=VolDisLn+VolValveODCLn

	    IF (VolValveODCLn .LE. 0) THEN
			EqDiameter=DisLnPAR(2)
			EqThickness=DisLnPAR(3)
			TotElevation=DisLnPAR(4)
			TotHeatGain=DisLnPAR(5)
			TotTempChange=DisLnPAR(6)
			TotAddDP=DisLnPAR(7)
		ELSE
			EqLength=DisLnPAR(1)+ValveODCLnPAR(1) !ISI - 08/03/06
			EqThickness=(DisLnPAR(3)+ValveODCLnPAR(3))/2
			TotElevation=DisLnPAR(4)+ValveODCLnPAR(4)
			TotHeatGain=DisLnPAR(5)+ValveODCLnPAR(5)
			TotTempChange=DisLnPAR(6)+ValveODCLnPAR(6)
			TotAddDP=DisLnPAR(7)+ValveODCLnPAR(7)
		END IF
		EqDiameter=SQRT(4*TotVolume/(PI*EqLength))*12+2*EqThickness*1e-3 !ISI - 08/03/06

        DisLnPAR(1)=EqLength
	    DisLnPAR(2)=EqDiameter
	    DisLnPAR(3)=EqThickness
	    DisLnPAR(4)=TotElevation
	    DisLnPAR(5)=TotHeatGain
	    DisLnPAR(6)=TotTempChange
	    DisLnPAR(7)=TotAddDP

        !Equilibrium suction line, combines compressor suction line and valve to IDC line
	    VolSucLn=PI*((SucLnPAR(2)-2*SucLnPAR(3)*1e-3)/12)**2/4*SucLnPAR(1)
	    VolValveIDCLn=PI*((ValveIDCLnPAR(2)-2*ValveIDCLnPAR(3)*1e-3)/12)**2/4*ValveIDCLnPAR(1)
	    TotVolume=VolSucLn+VolValveIDCLn

	    IF (VolValveIDCLn .LE. 0) THEN
			EqDiameter=SucLnPAR(2)
			EqThickness=SucLnPAR(3)
			TotElevation=SucLnPAR(4)
			TotHeatGain=SucLnPAR(5)
			TotTempChange=SucLnPAR(6)
			TotAddDP=SucLnPAR(7)
		ELSE
			EqLength=SucLnPAR(1)+ValveIDCLnPAR(1) !ISI - 08/03/06
			EqThickness=(SucLnPAR(3)+ValveIDCLnPAR(3))/2
			TotElevation=SucLnPAR(4)+ValveIDCLnPAR(4)
			TotHeatGain=SucLnPAR(5)+ValveIDCLnPAR(5)
			TotTempChange=SucLnPAR(6)+ValveIDCLnPAR(6)
			TotAddDP=SucLnPAR(7)+ValveIDCLnPAR(7)
		END IF
		EqDiameter=SQRT(4*TotVolume/(PI*EqLength))*12+2*EqThickness*1e-3 !ISI - 08/03/06

        SucLnPAR(1)=EqLength
	    SucLnPAR(2)=EqDiameter
	    SucLnPAR(3)=EqThickness
	    SucLnPAR(4)=TotElevation
	    SucLnPAR(5)=TotHeatGain
	    SucLnPAR(6)=TotTempChange
	    SucLnPAR(7)=TotAddDP

	  END IF

    ELSE !Heating mode

	  IF (Unit .EQ. SI) THEN !SI unit

	    !Equilibruim Discharge line, combines compressor discharge line and valve to IDC line
	    VolDisLn=PI*((DisLnPAR(2)-2*DisLnPAR(3))*1e-3)**2/4*DisLnPAR(1)
	    VolValveIDCLn=PI*((ValveIDCLnPAR(2)-2*ValveIDCLnPAR(3))*1e-3)**2/4*ValveIDCLnPAR(1)
	    TotVolume=VolDisLn+VolValveIDCLn

		IF (VolValveIDCLn .LE. 0) THEN
			EqDiameter=DisLnPAR(2)
			EqThickness=DisLnPAR(3)
			TotElevation=DisLnPAR(4)
			TotHeatGain=DisLnPAR(5)
			TotTempChange=DisLnPAR(6)
			TotAddDP=DisLnPAR(7)
		ELSE
			EqLength=DisLnPAR(1)+ValveIDCLnPAR(1) !ISI - 08/03/06
			EqThickness=(DisLnPAR(3)+ValveIDCLnPAR(3))/2
			TotElevation=DisLnPAR(4)+ValveIDCLnPAR(4)
			TotHeatGain=DisLnPAR(5)+ValveIDCLnPAR(5)
			TotTempChange=DisLnPAR(6)+ValveIDCLnPAR(6)
			TotAddDP=DisLnPAR(7)+ValveIDCLnPAR(7)
		END IF
		EqDiameter=SQRT(4*TotVolume/(PI*EqLength))*1000+2*EqThickness !ISI - 08/03/06

        DisLnPAR(1)=EqLength
	    DisLnPAR(2)=EqDiameter
	    DisLnPAR(3)=EqThickness
	    DisLnPAR(4)=TotElevation
	    DisLnPAR(5)=TotHeatGain
	    DisLnPAR(6)=TotTempChange
	    DisLnPAR(7)=TotAddDP

        !Equilibruim suction line, combines compressor suction line and valve to ODC line
	    VolSucLn=PI*((SucLnPAR(2)-2*SucLnPAR(3))*1e-3)**2/4*SucLnPAR(1)
	    VolValveODCLn=PI*((ValveODCLnPAR(2)-2*ValveODCLnPAR(3))*1e-3)**2/4*ValveODCLnPAR(1)
	    TotVolume=VolSucLn+VolValveODCLn

	    IF (VolValveODCLn .LE. 0) THEN
			EqDiameter=SucLnPAR(2)
			EqThickness=SucLnPAR(3)
			TotElevation=SucLnPAR(4)
			TotHeatGain=SucLnPAR(5)
			TotTempChange=SucLnPAR(6)
			TotAddDP=SucLnPAR(7)
		ELSE
			EqLength=SucLnPAR(1)+ValveODCLnPAR(1) !ISI - 08/03/06
			EqThickness=(SucLnPAR(3)+ValveODCLnPAR(3))/2
			TotElevation=SucLnPAR(4)+ValveODCLnPAR(4)
			TotHeatGain=SucLnPAR(5)+ValveODCLnPAR(5)
			TotTempChange=SucLnPAR(6)+ValveODCLnPAR(6)
			TotAddDP=SucLnPAR(7)+ValveODCLnPAR(7)
		END IF
		EqDiameter=SQRT(4*TotVolume/(PI*EqLength))*1000+2*EqThickness !ISI - 08/03/06

        SucLnPAR(1)=EqLength
	    SucLnPAR(2)=EqDiameter
	    SucLnPAR(3)=EqThickness
	    SucLnPAR(4)=TotElevation
	    SucLnPAR(5)=TotHeatGain
	    SucLnPAR(6)=TotTempChange
	    SucLnPAR(7)=TotAddDP

	  ELSE !IP unit

	    !Equilibruim Discharge line, combines compressor discharge line and valve to IDC line
	    VolDisLn=PI*((DisLnPAR(2)-2*DisLnPAR(3)*1e-3)/12)**2/4*DisLnPAR(1)
	    VolValveIDCLn=PI*((ValveIDCLnPAR(2)-2*ValveIDCLnPAR(3)*1e-3)/12)**2/4*ValveIDCLnPAR(1)
	    TotVolume=VolDisLn+VolValveIDCLn

	    IF (VolValveIDCLn .LE. 0) THEN
			EqDiameter=DisLnPAR(2)
			EqThickness=DisLnPAR(3)
			TotElevation=DisLnPAR(4)
			TotHeatGain=DisLnPAR(5)
			TotTempChange=DisLnPAR(6)
			TotAddDP=DisLnPAR(7)
		ELSE
			EqLength=DisLnPAR(1)+ValveIDCLnPAR(1) !ISI - 08/03/06
			EqThickness=(DisLnPAR(3)+ValveIDCLnPAR(3))/2
			TotElevation=DisLnPAR(4)+ValveIDCLnPAR(4)
			TotHeatGain=DisLnPAR(5)+ValveIDCLnPAR(5)
			TotTempChange=DisLnPAR(6)+ValveIDCLnPAR(6)
			TotAddDP=DisLnPAR(7)+ValveIDCLnPAR(7)
		END IF
		EqDiameter=SQRT(4*TotVolume/(PI*EqLength))*12+2*EqThickness*1e-3 !ISI - 08/03/06

        DisLnPAR(1)=EqLength
	    DisLnPAR(2)=EqDiameter
	    DisLnPAR(3)=EqThickness
	    DisLnPAR(4)=TotElevation
	    DisLnPAR(5)=TotHeatGain
	    DisLnPAR(6)=TotTempChange
	    DisLnPAR(7)=TotAddDP

        !Equilibruim suction line, combines compressor suction line and valve to ODC line
	    VolSucLn=PI*((SucLnPAR(2)-2*SucLnPAR(3)*1e-3)/12)**2/4*SucLnPAR(1)
	    VolValveODCLn=PI*((ValveODCLnPAR(2)-2*ValveODCLnPAR(3)*1e-3)/12)**2/4*ValveODCLnPAR(1)
	    TotVolume=VolSucLn+VolValveODCLn

	    IF (VolValveODCLn .LE. 0) THEN
			EqDiameter=SucLnPAR(2)
			EqThickness=SucLnPAR(3)
			TotElevation=SucLnPAR(4)
			TotHeatGain=SucLnPAR(5)
			TotTempChange=SucLnPAR(6)
			TotAddDP=SucLnPAR(7)
		ELSE
			EqLength=SucLnPAR(1)+ValveODCLnPAR(1) !ISI - 08/03/06
			EqThickness=(SucLnPAR(3)+ValveODCLnPAR(3))/2
			TotElevation=SucLnPAR(4)+ValveODCLnPAR(4)
			TotHeatGain=SucLnPAR(5)+ValveODCLnPAR(5)
			TotTempChange=SucLnPAR(6)+ValveODCLnPAR(6)
			TotAddDP=SucLnPAR(7)+ValveODCLnPAR(7)
		END IF
		EqDiameter=SQRT(4*TotVolume/(PI*EqLength))*12+2*EqThickness*1e-3 !ISI - 08/03/06

        SucLnPAR(1)=EqLength
	    SucLnPAR(2)=EqDiameter
	    SucLnPAR(3)=EqThickness
	    SucLnPAR(4)=TotElevation
	    SucLnPAR(5)=TotHeatGain
	    SucLnPAR(6)=TotTempChange
	    SucLnPAR(7)=TotAddDP

	  END IF

    END IF

  END IF

  CondPAR(1)=DisLnPAR(1) !Discharge line length, m or ft
  CondPAR(2)=DisLnPAR(2) !Discharge line outside diameter, mm or in
  CondPAR(3)=DisLnPAR(3) !Discharge line tube wall thickness, mm or mil
  CondPAR(4)=DisLnPAR(4) !Discharge line elevation, m or ft
  CondPAR(5)=DisLnPAR(5) !Discharge line heat loss, W or Btu/hr  
  CondPAR(6)=DisLnPAR(6) !Discharge line temperature change, C or F
  CondPAR(7)=DisLnPAR(7) !Discharge line additional pressure drop

  CondPAR(8)=LiqLnPAR(1)  !Liquid line length, m or ft
  CondPAR(9)=LiqLnPAR(2)  !Liquid line outside diameter, mm or in
  CondPAR(10)=LiqLnPAR(3) !Liquid line tube wall thickness, mm or mil
  CondPAR(11)=LiqLnPAR(4) !Liquid line elevation, m or ft
  CondPAR(12)=LiqLnPAR(5) !Liquid line heat loss, W or Btu/hr  
  CondPAR(13)=LiqLnPAR(6) !Liquid line temperature change, C or F
  CondPAR(14)=LiqLnPAR(7) !Liquid line additional pressure drop

  EvapPAR(1)=SucLnPAR(1) !Suction line length, m or ft
  EvapPAR(2)=SucLnPAR(2) !Suction line outside diameter, mm or in
  EvapPAR(3)=SucLnPAR(3) !Suction line tube wall thickness, mm or mil
  EvapPAR(4)=SucLnPAR(4) !Suction line elevation, m or ft
  EvapPAR(5)=SucLnPAR(5) !Suction line heat loss, W or Btu/hr  
  EvapPAR(6)=SucLnPAR(6) !Suction line temperature change, C or F
  EvapPAR(7)=SucLnPAR(7) !Suction line additional pressure drop

  IF (IsCoolingMode .GT. 0) THEN    !Populating arrays
    
	CFMcnd=VdotODfan
    CFMevp=VdotIDfan
    CoilParams(1)%AirFlowRate=CFMevp
    CoilParams(2)%AirFlowRate=CFMcnd

	CondPAR(15)=ODC_TubeOD
	CondPAR(16)=ODC_TubeThk
	CondPAR(17)=ODC_Ltube
	CondPAR(18)=ODC_Ktube
	CondPAR(19)=ODC_Pt
	CondPAR(20)=ODC_Pl
	CondPAR(21)=ODC_FinThk
	CondPAR(22)=ODC_FinPitch
	CondPAR(23)=ODC_Kfin

	CondPAR(24)=ODC_Nt
	CondPAR(25)=ODC_Nl
	CondPAR(26)=ODC_Nckt
	CondPAR(27)=IsCoolingMode
	CondPAR(28)=ODC_Nmod
	CondPAR(29)=ODC_FinType

	CondPAR(30)=ODC_hciMultiplier
	CondPAR(31)=ODC_DPrefMultiplier
	CondPAR(32)=ODC_hcoMultiplier
	CondPAR(33)=ODC_DPairMultiplier
    CondPAR(34)=PwrODfan
    CondPAR(35)=ODdrawblow

	CondPAR(36)=ODC_SurfAbsorptivity
    CondPAR(37)=ODC_TubeType

	CondPAR(41)=ODC_CurveUnit
    CondPAR(42)=ODC_CurveTypeHTC
    CondPAR(43)=ODC_PowerAHTC
    CondPAR(44)=ODC_PowerBHTC
    CondPAR(45)=ODC_Poly1HTC
    CondPAR(46)=ODC_Poly2HTC
    CondPAR(47)=ODC_Poly3HTC
    CondPAR(48)=ODC_Poly4HTC
    CondPAR(49)=ODC_CurveTypeDP
    CondPAR(50)=ODC_PowerADP
    CondPAR(51)=ODC_PowerBDP
    CondPAR(52)=ODC_Poly1DP
    CondPAR(53)=ODC_Poly2DP
    CondPAR(54)=ODC_Poly3DP
    CondPAR(55)=ODC_Poly4DP

	EvapPAR(8)=IDC_TubeOD
	EvapPAR(9)=IDC_TubeThk
	EvapPAR(10)=IDC_Ltube
	EvapPAR(11)=IDC_Ktube
	EvapPAR(12)=IDC_Pt
	EvapPAR(13)=IDC_Pl
	EvapPAR(14)=IDC_FinThk
	EvapPAR(15)=IDC_FinPitch
	EvapPAR(16)=IDC_Kfin

	EvapPAR(17)=IDC_Nt
	EvapPAR(18)=IDC_Nl
	EvapPAR(19)=IDC_Nckt
	EvapPAR(20)=IsCoolingMode
	EvapPAR(21)=IDC_Nmod
	EvapPAR(22)=IDC_FinType

	EvapPAR(23)=IDC_hciMultiplier
	EvapPAR(24)=IDC_DPrefMultiplier
	EvapPAR(25)=IDC_hcoMultiplier
	EvapPAR(26)=IDC_DPairMultiplier
    
	EvapPAR(27)=PwrIDfan
    EvapPAR(28)=IDdrawblow

	EvapPAR(29)=IDC_SurfAbsorptivity
    EvapPAR(30)=IDC_TubeType

	EvapPAR(35)=IDC_CurveUnit
	EvapPAR(36)=IDC_CurveTypeHTC
	EvapPAR(37)=IDC_PowerAHTC
	EvapPAR(38)=IDC_PowerBHTC
	EvapPAR(39)=IDC_Poly1HTC
	EvapPAR(40)=IDC_Poly2HTC
	EvapPAR(41)=IDC_Poly3HTC
	EvapPAR(42)=IDC_Poly4HTC
	EvapPAR(43)=IDC_CurveTypeDP
	EvapPAR(44)=IDC_PowerADP
	EvapPAR(45)=IDC_PowerBDP
	EvapPAR(46)=IDC_Poly1DP
	EvapPAR(47)=IDC_Poly2DP
	EvapPAR(48)=IDC_Poly3DP
	EvapPAR(49)=IDC_Poly4DP

	ShTbPAR=CoolingShTbPAR
    ShTbPAR(4)=EvapPAR(19) !Number of circuits in evaporator
    ShTbPAR(5)=CoolingDistubeLength

	CapTubePAR=CoolingCapTubePAR
    CapTubePAR(4)=EvapPAR(19) !Number of circuits in evaporator
    CapTubePAR(5)=CoolingDistubeLength

    TxvPAR(5) =EvapPAR(19) !Number of circuits in evaporator
    TxvPAR(6) =CoolingDistubeLength

    ExpDevice=CoolingExpDevice
    TxvPAR(1)=CoolingTXVcapacity
  ELSE
	CFMevp=VdotODfan
    CFMcnd=VdotIDfan
    CoilParams(1)%AirFlowRate=CFMcnd
    CoilParams(2)%AirFlowRate=CFMevp

 	CondPAR(15)=IDC_TubeOD
	CondPAR(16)=IDC_TubeThk
	CondPAR(17)=IDC_Ltube
	CondPAR(18)=IDC_Ktube
	CondPAR(19)=IDC_Pt
	CondPAR(20)=IDC_Pl
	CondPAR(21)=IDC_FinThk
	CondPAR(22)=IDC_FinPitch
	CondPAR(23)=IDC_Kfin

	CondPAR(24)=IDC_Nt
	CondPAR(25)=IDC_Nl
	CondPAR(26)=IDC_Nckt
	CondPAR(27)=IsCoolingMode
	CondPAR(28)=IDC_Nmod
	CondPAR(29)=IDC_FinType

	CondPAR(30)=IDC_hciMultiplier
	CondPAR(31)=IDC_DPrefMultiplier
	CondPAR(32)=IDC_hcoMultiplier
	CondPAR(33)=IDC_DPairMultiplier
    CondPAR(34)=PwrIDfan
    CondPAR(35)=IDdrawblow

	CondPAR(36)=IDC_SurfAbsorptivity
    CondPAR(37)=IDC_TubeType

	CondPAR(41)=IDC_CurveUnit
    CondPAR(42)=IDC_CurveTypeHTC
    CondPAR(43)=IDC_PowerAHTC
    CondPAR(44)=IDC_PowerBHTC
    CondPAR(45)=IDC_Poly1HTC
    CondPAR(46)=IDC_Poly2HTC
    CondPAR(47)=IDC_Poly3HTC
    CondPAR(48)=IDC_Poly4HTC
    CondPAR(49)=IDC_CurveTypeDP
    CondPAR(50)=IDC_PowerADP
    CondPAR(51)=IDC_PowerBDP
    CondPAR(52)=IDC_Poly1DP
    CondPAR(53)=IDC_Poly2DP
    CondPAR(54)=IDC_Poly3DP
    CondPAR(55)=IDC_Poly4DP

	EvapPAR(8)=ODC_TubeOD
	EvapPAR(9)=ODC_TubeThk
	EvapPAR(10)=ODC_Ltube
	EvapPAR(11)=ODC_Ktube
	EvapPAR(12)=ODC_Pt
	EvapPAR(13)=ODC_Pl
	EvapPAR(14)=ODC_FinThk
	EvapPAR(15)=ODC_FinPitch
	EvapPAR(16)=ODC_Kfin

	EvapPAR(17)=ODC_Nt
	EvapPAR(18)=ODC_Nl
	EvapPAR(19)=ODC_Nckt
	EvapPAR(20)=IsCoolingMode
	EvapPAR(21)=ODC_Nmod
	EvapPAR(22)=ODC_FinType

	EvapPAR(23)=ODC_hciMultiplier
	EvapPAR(24)=ODC_DPrefMultiplier
	EvapPAR(25)=ODC_hcoMultiplier
	EvapPAR(26)=ODC_DPairMultiplier
    
	EvapPAR(27)=PwrODfan
    EvapPAR(28)=ODdrawblow

	EvapPAR(29)=ODC_SurfAbsorptivity
    EvapPAR(30)=ODC_TubeType

	EvapPAR(35)=ODC_CurveUnit
	EvapPAR(36)=ODC_CurveTypeHTC
	EvapPAR(37)=ODC_PowerAHTC
	EvapPAR(38)=ODC_PowerBHTC
	EvapPAR(39)=ODC_Poly1HTC
	EvapPAR(40)=ODC_Poly2HTC
	EvapPAR(41)=ODC_Poly3HTC
	EvapPAR(42)=ODC_Poly4HTC
	EvapPAR(43)=ODC_CurveTypeDP
	EvapPAR(44)=ODC_PowerADP
	EvapPAR(45)=ODC_PowerBDP
	EvapPAR(46)=ODC_Poly1DP
	EvapPAR(47)=ODC_Poly2DP
	EvapPAR(48)=ODC_Poly3DP
	EvapPAR(49)=ODC_Poly4DP

	ShTbPAR=HeatingShTbPAR
    ShTbPAR(4)=EvapPAR(19) !Number of circuits in evaporator
    ShTbPAR(5)=HeatingDistubeLength

	CapTubePAR=HeatingCapTubePAR
    CapTubePAR(4)=EvapPAR(19) !Number of circuits in evaporator
    CapTubePAR(5)=HeatingDistubeLength

    TxvPAR(5) =EvapPAR(19) !Number of circuits in evaporator
    TxvPAR(6) =HeatingDistubeLength

    ExpDevice=HeatingExpDevice
    TxvPAR(1)=HeatingTXVcapacity
  END IF

  EvapPAR(31)=BaroPressure
  CondPAR(38)=BaroPressure

  EvapPAR(33)=IsCmpInAirStream
  CondPAR(40)=IsCmpInAirStream

  EvapPAR(34)=SystemType
  CondPAR(57)=SystemType !ISI - 07/14/06

  EvapPAR(52)=CompressorManufacturer !ISI - 10/05/06
  CondPAR(60)=CompressorManufacturer

  SuperStc=TxvPAR(3)
  SuperRtd=TxvPAR(2)

  IF (LineData(1:17) .EQ. 'Microchannel Coil') THEN
	  IF (IsCoolingMode .GT. 0) THEN
	    ODCcoilType = MCCONDENSER
	  ELSE
		ODCcoilType = MCEVAPORATOR
	  END IF
  ELSE
	  IF (IsCoolingMode .GT. 0) THEN
	    ODCcoilType = CONDENSERCOIL
	  ELSE
		ODCcoilType = EVAPORATORCOIL
	  END IF
  END IF

  CLOSE(11)

  !!VL: Previously ...
  !!201 FORMAT(10(E))
  !!202 FORMAT(A150)
  !!203 FORMAT(I1)

  RETURN

END SUBROUTINE

!***********************************************************************************

END MODULE HeatPumpInput

!***********************************************************************************
