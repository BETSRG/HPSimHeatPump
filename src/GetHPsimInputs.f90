!***********************************************************************************

MODULE HeatPumpInput

USE DataSimulation

PRIVATE
CHARACTER*80 :: RefName             !Refrigerant name

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

USE RefNameMod
USE DataStopCodes

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

!Flow:

  ODC_SurfAbsorptivity=1
  IDC_SurfAbsorptivity=1

  INQUIRE(FILE="HPdata.ydd", EXIST=HPDataFileExists)
  IF (.NOT. HPDataFileExists) THEN
    !don't just crash
    WRITE(*,*) "HPdata.ydd was not found in the working directory...program aborting"
    STOP exit_FileIO_Missing_HPData
  END IF
  OPEN(200,FILE="HPdata.ydd")
  
  !***************** System data *****************
  DO I=1, 5; READ(200,*); END DO
  
  CALL GetObjectItem('MainDesignData',1,Alphas,NumAlphas, &
                        Numbers,NumNumbers,Status)      

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

  CoolingDesignCondition = Numbers(3)
  HeatingDesignCondition = Numbers(4)
  Subcooling = Numbers(5)
  Superheat = Numbers(6)
  
  RefrigerantName = Alphas(3)
  
  NumofRefrigerants = Numbers(7)
  NominalCoolingCapacity = Numbers(8)
  NominalHeatingCapacity = Numbers(9)
  ElectricHeating = Numbers(10)
  
  DesignConditionDescription = Alphas(4)
  
  OutdoorEnteringDrybulbTemperature = Numbers(11)
  OutdoorEnteringWetbulbTemperature = Numbers(12)
  IndoorEnteringDrybulbTemperature = Numbers(13)
  IndoorEnteringWetbulbTemperature = Numbers(14)
  DesignRefChg = Numbers(15)
  RefChg = Numbers(16)
  RefMassFlowRate = Numbers(17)
  SystemEER = Numbers(18)
  SystemSEER = Numbers(19)
  SystemCOP = Numbers(20)
  SensibleTotalHeatRatio = Numbers(21)
  CalculatedSubcooling = Numbers(22)
  CalculatedSuperheat = Numbers(23)
  CalculatedCoolingCapacity = Numbers(24)
  CalculatedHeatingCapacity = Numbers(25)


  !***************** Compressor data *****************


  DO I=1,10; READ(200,*); END DO
      
  CALL GetObjectItem('CompressorData',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)   

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
  
  PureRref=PureRef
  
  CompressorPower = Numbers(1)
  CompressorHeatLossFraction = Numbers(2)
  CompressorHeatLoss = Numbers(3)
  CompressorVolume = Numbers(4)
  CompressorMassCoefficient1 = Numbers(5)
  CompressorMassCoefficient2 = Numbers(6)
  CompressorMassCoefficient3 = Numbers(7)
  CompressorMassCoefficient4 = Numbers(8)
  CompressorMassCoefficient5 = Numbers(9)
  CompressorMassCoefficient6 = Numbers(10)
  CompressorMassCoefficient7 = Numbers(11)
  CompressorMassCoefficient8 = Numbers(12)
  CompressorMassCoefficient9 = Numbers(13)
  CompressorMassCoefficient10 = Numbers(14)
  CompressorPowerCoefficient1 = Numbers(15)
  CompressorPowerCoefficient2 = Numbers(16)
  CompressorPowerCoefficient3 = Numbers(17)
  CompressorPowerCoefficient4 = Numbers(18)
  CompressorPowerCoefficient5 = Numbers(19)
  CompressorPowerCoefficient6 = Numbers(20)
  CompressorPowerCoefficient7 = Numbers(21)
  CompressorPowerCoefficient8 = Numbers(22)
  CompressorPowerCoefficient9 = Numbers(23)
  CompressorPowerCoefficient10 = Numbers(24)
  
  CompressorCoefficientsUnitFlag = Alphas(5)
  
  PowerMultiplier = Numbers(25)
  MassFlowRateMultiplier = Numbers(26)
  UserSpecifiedRatingEvapTemperature = Numbers(27)
  UserSpecifiedRatingCondTemperature = Numbers(28)
  UserSpecifiedRatingSubcooling = Numbers(29)
  UserSpecifiedRatingSuperheat = Numbers(30)
  UserSpecifiedRatingHeatingModeSubcooling = Numbers(31)
  UserSpecifiedRatingHeatingModeSuperheat = Numbers(32)

  

  !***************** Outdoor coil data *****************
  !DO I=1,3; READ(200,*); END DO
  
  CALL GetObjectItem('OutdoorCoilData',1,Alphas,NumAlphas, &
                        Numbers,NumNumbers,Status)   

  !Fin type (1-smooth; 2-Wavy; 3-louvered)
  READ(200,202)LineData

  ODC_FinType = Numbers(1)
  
  ODC_FinName = Alphas(1)
  
  ODC_FinPitch = Numbers(2)
  ODC_Kfin = Numbers(3)
  ODC_FinThk = Numbers(4)
  
  ODC_FinMaterial = Alphas(2)
  ODC_TubeName = Alphas(3)
  
  ODC_TubeID = Numbers(5)
  ODC_TubeOD = Numbers(6)
  ODC_Ktube = Numbers(7)
  ODC_Pl = Numbers(8)
  ODC_Pt = Numbers(9)
  ODC_Nl = Numbers(10)
  ODC_Nt = Numbers(11)
  ODC_Nckt = Numbers(12)
  ODC_Nmod = Numbers(13)
  ODC_Ltube = Numbers(14)
  ODC_CoilAirPressureDrop = Numbers(15)
  ODC_CoilAirOutletDrybulbTemp = Numbers(16)
  ODC_CoilAirOutletWetbulbTemp = Numbers(17)
  ODC_CoilAirOutletRelativeHumidity = Numbers(18)
  ODC_CoilAirFaceVelocity = Numbers(19)
  ODC_CoilHeatTransferRate = Numbers(20)
  ODC_MassinCoil = Numbers(21)
  ODC_InternalVolume = Numbers(22)
  ODC_hciMultiplier = Numbers(23)
  ODC_DPrefMultiplier = Numbers(24)
  ODC_hcoMultiplier = Numbers(25)
  ODC_DPairMultiplier = Numbers(26)
  ODC_CoilAirLeakage = Numbers(27)


  !***************** Outdoor fan data *****************
  
  CALL GetObjectItem('OutdoorFanData',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)   
  
  PwrODfan = Numbers(1)
  VdotODfan = Numbers(2)
  ODdrawBlow = Numbers(3)


    !Then we don't end up reading the separator *** line here, we just start with the next line

  !***************** Indoor coil data *****************
  
  CALL GetObjectItem('IndoorCoilData',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)
  
  IDC_FinType = Numbers(1)
  
  IDC_FinName = Alphas(1)
  
  IDC_FinPitch = Numbers(2)
  IDC_Kfin = Numbers(3)
  IDC_FinThk = Numbers(4)
  
  IDC_FinMaterial = Alphas(2)
  IDC_TubeName = Alphas(3)
  
  IDC_TubeID = Numbers(5)
  IDC_TubeOD = Numbers(6)
  IDC_Ktube = Numbers(7)
  IDC_Pl = Numbers(8)
  IDC_Pt = Numbers(9)
  IDC_Nl = Numbers(10)
  IDC_Nt = Numbers(11)
  IDC_Nckt = Numbers(12)
  IDC_Nmod = Numbers(13)
  IDC_Ltube = Numbers(14)
  IDC_CoilAirPressureDrop = Numbers(15)
  IDC_CoilAirOutletDrybulbTemperature = Numbers(16)
  IDC_CoilAirOutletWetbulbTemperature = Numbers(17)
  IDC_CoilAirOutletRelativeHumidity = Numbers(18)
  IDC_CoilAirFaceVelocity = Numbers(19)
  IDC_CoilHeatTransferRate = Numbers(20)
  IDC_MassinCoil = Numbers(21)
  IDC_InternalVolume = Numbers(22)
  IDC_hciMultiplier = Numbers(23)
  IDC_DPrefMultiplier = Numbers(24)
  IDC_hcoMultiplier = Numbers(25)
  IDC_DPairMultiplier = Numbers(26)
  IDC_CoilAirLeakage = Numbers(27)


  !***************** Indoor fan data *****************
  
  CALL GetObjectItem('IndoorFanData',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)  
  
  PwrIDfan = Numbers(1)
  VdotIDfan = Numbers(2)
  IDdrawBlow = Numbers(3)


  !***************** Expansion device data *****************
  
  CALL GetObjectItem('ExpansionDeviceData',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)  
  
  READ(200,202)LineData

  SELECT CASE (Alphas(1)(1:1))
  CASE ('C','c')
    CoolingExpDevice=3 !Cap. tube
  CASE ('T','t')
    CoolingExpDevice=2 !TXV
  CASE DEFAULT
    CoolingExpDevice=1 !Short tube orifice
  END SELECT

  READ(200,202)LineData
  
      SELECT CASE (Alphas(2)(1:1))
      CASE ('C','c')
        HeatingExpDevice=3 !Cap. tube
      CASE ('T','t')
        HeatingExpDevice=2 !TXV
      CASE DEFAULT
        HeatingExpDevice=1 !Short tube orifice
      END SELECT
  
      READ(200,*)
      
  !Short tube orifice

  !Cooling mode
  !READ(200,*)

  CoolingShTbPAR(1) = Numbers(1)    !Length
  CoolingShTbPAR(2) = Numbers(2)    !Diameter
  CoolingShTbPAR(3) = Numbers(3)    !Chamfer Depth

  !Heating mode

  HeatingShTbPAR(1) = Numbers(4)    !Length
  HeatingShTbPAR(2) = Numbers(5)    !Diameter
  HeatingShTbPAR(3) = Numbers(6)    !Chamfer Depth

  READ(200,202)LineData
  
  !Capillary Tube
  
  !Cooling Mode
  
  CoolingCapTubePAR(2) = Numbers(7) !Length
  CoolingCapTubePAR(1) = Numbers(8) !Diameter
  CoolingCapTubePAR(3) = Numbers(9) !Coil Diameter
  
  !Heating Mode
  
  HeatingCapTubePAR(2) = Numbers(10)    !Length
  HeatingCapTubePAR(1) = Numbers(11)    !Diameter
  HeatingCapTubePAR(3) = Numbers(12)    !Coil Diameter
    
      READ(200,202)LineData

  !TXV data
  READ(200,202)LineData !TXV Model

  !Rated TXV capacity, ton

  CoolingTXVcapacity = Numbers(13)
  HeatingTXVcapacity = Numbers(14)

  !Distributor tubes
  READ(200,*)

  CoolingDistubeLength = Numbers(15)
  
  !IF (Unit .EQ. 1) THEN
  !  CoolingDistubeLength=CoolingDistubeLength/1000
  !ELSEIF (Unit .EQ. 2) THEN
  !  CoolingDistubeLength=CoolingDistubeLength/12
  !END IF !ISI - 07/14/06
  IF (Unit .EQ. SI) THEN
    CoolingDistubeLength=CoolingDistubeLength/1000
  ELSEIF (Unit .EQ. IP) THEN
    CoolingDistubeLength=CoolingDistubeLength/12
  END IF

  HeatingDistubeLength = Numbers(16)
  !IF (Unit .EQ. 1) THEN
  !  HeatingDistubeLength=HeatingDistubeLength/1000
  !ELSEIF (Unit .EQ. 2) THEN
  !  HeatingDistubeLength=HeatingDistubeLength/12
  !END IF !ISI 07/14/06
  IF (Unit .EQ. SI) THEN
    HeatingDistubeLength=HeatingDistubeLength/1000
  ELSEIF (Unit .EQ. IP) THEN
    HeatingDistubeLength=HeatingDistubeLength/12
  END IF

  !*****************Refrigerant line data******************
  
  DO I=1,3; READ(200,*); END DO
      
      
  CALL GetObjectItem('RefrigerantLineData',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)  
  
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
  !IF (Unit .EQ. 2) SucLnPAR(3)=SucLnPAR(3)*1000 !ISI - 07/14/06 
  IF (Unit .EQ. IP) SucLnPAR(3)=SucLnPAR(3)*1000 

  DO I=1,3; READ(200,*); END DO

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
  !IF (Unit .EQ. 2) DisLnPAR(3)=DisLnPAR(3)*1000 !ISI - 07/14/06 
  IF (Unit .EQ. IP) DisLnPAR(3)=DisLnPAR(3)*1000 

  DO I=1,3; READ(200,*); END DO

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
  !IF (Unit .EQ. 2)LiqLnPAR(3)=LiqLnPAR(3)*1000 !ISI - 07/14/06 
  IF (Unit .EQ. IP)LiqLnPAR(3)=LiqLnPAR(3)*1000  

  DO I=1,3; READ(200,*); END DO

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
  !IF (Unit .EQ. 2)ValveIDCLnPAR(3)=ValveIDCLnPAR(3)*1000 !ISI - 07/14/06
  IF (Unit .EQ. IP)ValveIDCLnPAR(3)=ValveIDCLnPAR(3)*1000

  DO I=1,3; READ(200,*); END DO

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
  ValveODCLn_Charge = Numbers(45)   !Charge in Line
  
  TubeNumber = Numbers(46)  !Number of Tubes Liq.

  !Valve to ODC line tube wall thickness, mm or mil
  ValveODCLnPAR(3)=(ValveODCLnPAR(2)-ValveODCLn_TubeID)/2
  !IF (Unit .EQ. 2)ValveODCLnPAR(3)=ValveODCLnPAR(3)*1000 !ISI - 07/14/06
  IF (Unit .EQ. IP)ValveODCLnPAR(3)=ValveODCLnPAR(3)*1000



  !********************Refrigerant Cycle Data (Cooling)***********************

  DO I=1,34; READ(200,*); END DO  
  
  CALL GetObjectItem('RefrigerantCycleData(Cooling)',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)  

  !Compressor Suction
  
  RCDC_ComSuc_CyclePoint = Alphas(1)
  
  RCDC_ComSuc_Pressure = Numbers(1)
  RCDC_ComSuc_Enthalpy = Numbers(2)
  RCDC_ComSuc_Temperature = Numbers(3)
  RCDC_ComSuc_Quality - Numbers(4)
  RCDC_ComSuc_Superheat = Numbers(5)
  RCDC_ComSuc_Subcooling = Numbers(6)
  
  !Compressor Discharge
  
  RCDC_ComDis_CyclePoint = Alphas(2)
  
  RCDC_ComDis_Pressure = Numbers(7)
  RCDC_ComDis_Enthalpy = Numbers(8)
  RCDC_ComDis_Temperature = Numbers(9)
  RCDC_ComDis_Quality - Numbers(10)
  RCDC_ComDis_Superheat = Numbers(11)
  RCDC_ComDis_Subcooling = Numbers(12)
  
  !Outdoor Coil Inlet
  
  RCDC_OCI_CyclePoint = Alphas(3)
  
  RCDC_OCI_Pressure = Numbers(13)
  RCDC_OCI_Enthalpy = Numbers(14)
  RCDC_OCI_Temperature = Numbers(15)
  RCDC_OCI_Quality - Numbers(16)
  RCDC_OCI_Superheat = Numbers(17)
  RCDC_OCI_Subcooling = Numbers(18)
  
  !Outdoor Coil Outlet
  
  RCDC_OCO_CyclePoint = Alphas(4)
  
  RCDC_OCO_Pressure = Numbers(19)
  RCDC_OCO_Enthalpy = Numbers(20)
  RCDC_OCO_Temperature = Numbers(21)
  RCDC_OCO_Quality - Numbers(22)
  RCDC_OCO_Superheat = Numbers(23)
  RCDC_OCO_Subcooling = Numbers(24)
  
  !Expansion Device Inlet
  
  RCDC_EDI_CyclePoint = Alphas(5)
  
  RCDC_EDI_Pressure = Numbers(25)
  RCDC_EDI_Enthalpy = Numbers(26)
  RCDC_EDI_Temperature = Numbers(27)
  RCDC_EDI_Quality - Numbers(28)
  RCDC_EDI_Superheat = Numbers(29)
  RCDC_EDI_Subcooling = Numbers(30)
  
  !Expansion Device Outlet
  
  RCDC_EDO_CyclePoint = Alphas(6)
  
  RCDC_EDO_Pressure = Numbers(31)
  RCDC_EDO_Enthalpy = Numbers(32)
  RCDC_EDO_Temperature = Numbers(33)
  RCDC_EDO_Quality - Numbers(34)
  RCDC_EDO_Superheat = Numbers(35)
  RCDC_EDO_Subcooling = Numbers(36)
  
  !Indoor Coil Inlet
  
  RCDC_ICI_CyclePoint = Alphas(7)
  
  RCDC_ICI_Pressure = Numbers(37)
  RCDC_ICI_Enthalpy = Numbers(38)
  RCDC_ICI_Temperature = Numbers(39)
  RCDC_ICI_Quality - Numbers(40)
  RCDC_ICI_Superheat = Numbers(41)
  RCDC_ICI_Subcooling = Numbers(42)
  
  !Indoor Coil Outlet
  
  RCDC_ICO_CyclePoint = Alphas(8)
  
  RCDC_ICO_Pressure = Numbers(43)
  RCDC_ICO_Enthalpy = Numbers(44)
  RCDC_ICO_Temperature = Numbers(45)
  RCDC_ICO_Quality - Numbers(46)
  RCDC_ICO_Superheat = Numbers(47)
  RCDC_ICO_Subcooling = Numbers(48)
  
  
  
  !********************Refrigerant Cycle Data (Heating)***********************

  DO I=1,34; READ(200,*); END DO  
  
  CALL GetObjectItem('RefrigerantCycleData(Heating)',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)  

  !Compressor Suction
  
  RCDH_ComSuc_CyclePoint = Alphas(1)
  
  RCDH_ComSuc_Pressure = Numbers(1)
  RCDH_ComSuc_Enthalpy = Numbers(2)
  RCDH_ComSuc_Temperature = Numbers(3)
  RCDH_ComSuc_Quality - Numbers(4)
  RCDH_ComSuc_Superheat = Numbers(5)
  RCDH_ComSuc_Subcooling = Numbers(6)
  
  !Compressor Discharge
  
  RCDH_ComDis_CyclePoint = Alphas(2)
  
  RCDH_ComDis_Pressure = Numbers(7)
  RCDH_ComDis_Enthalpy = Numbers(8)
  RCDH_ComDis_Temperature = Numbers(9)
  RCDH_ComDis_Quality - Numbers(10)
  RCDH_ComDis_Superheat = Numbers(11)
  RCDH_ComDis_Subcooling = Numbers(12)
  
  !Outdoor Coil Inlet
  
  RCDH_OCI_CyclePoint = Alphas(3)
  
  RCDH_OCI_Pressure = Numbers(13)
  RCDH_OCI_Enthalpy = Numbers(14)
  RCDH_OCI_Temperature = Numbers(15)
  RCDH_OCI_Quality - Numbers(16)
  RCDH_OCI_Superheat = Numbers(17)
  RCDH_OCI_Subcooling = Numbers(18)
  
  !Outdoor Coil Outlet
  
  RCDH_OCO_CyclePoint = Alphas(4)
  
  RCDH_OCO_Pressure = Numbers(19)
  RCDH_OCO_Enthalpy = Numbers(20)
  RCDH_OCO_Temperature = Numbers(21)
  RCDH_OCO_Quality - Numbers(22)
  RCDH_OCO_Superheat = Numbers(23)
  RCDH_OCO_Subcooling = Numbers(24)
  
  !Expansion Device Inlet
  
  RCDH_EDI_CyclePoint = Alphas(5)
  
  RCDH_EDI_Pressure = Numbers(25)
  RCDH_EDI_Enthalpy = Numbers(26)
  RCDH_EDI_Temperature = Numbers(27)
  RCDH_EDI_Quality - Numbers(28)
  RCDH_EDI_Superheat = Numbers(29)
  RCDH_EDI_Subcooling = Numbers(30)
  
  !Expansion Device Outlet
  
  RCDH_EDO_CyclePoint = Alphas(6)
  
  RCDH_EDO_Pressure = Numbers(31)
  RCDH_EDO_Enthalpy = Numbers(32)
  RCDH_EDO_Temperature = Numbers(33)
  RCDH_EDO_Quality - Numbers(34)
  RCDH_EDO_Superheat = Numbers(35)
  RCDH_EDO_Subcooling = Numbers(36)
  
  !Indoor Coil Inlet
  
  RCDH_ICI_CyclePoint = Alphas(7)
  
  RCDH_ICI_Pressure = Numbers(37)
  RCDH_ICI_Enthalpy = Numbers(38)
  RCDH_ICI_Temperature = Numbers(39)
  RCDH_ICI_Quality - Numbers(40)
  RCDH_ICI_Superheat = Numbers(41)
  RCDH_ICI_Subcooling = Numbers(42)
  
  !Indoor Coil Outlet
  
  RCDH_ICO_CyclePoint = Alphas(8)
  
  RCDH_ICO_Pressure = Numbers(43)
  RCDH_ICO_Enthalpy = Numbers(44)
  RCDH_ICO_Temperature = Numbers(45)
  RCDH_ICO_Quality - Numbers(46)
  RCDH_ICO_Superheat = Numbers(47)
  RCDH_ICO_Subcooling = Numbers(48)
  
  RCDH_OCckt = Alphas(9)    !Outdoor Coil Ckt
  RCDH_ICckt = Alphas(10)   !Indoor Coil Ckt
  
  RCDH_Bar_Press = Numbers(49)  !Barometric Pressure
  RCDH_Com_Chg = Numbers(50)    !Charge in Compressor
  RCDH_DistC_Chg = Numbers(51)  !Charge in Distributor Tube (Cooling)
  RCDH_DistH_Chg = Numbers(52)  !Charge in Distributor Tube (Heating)
  RCDH_Com_AS = Numbers(53) !Is Compressor in Air Stream
  

  !Liquid temperature, ISI - 02/08/08
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)Tliq

  DO I=1,35; READ(200,*); END DO
  
  !Discharge temperature, ISI - 02/08/08
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)Tdis
  
  !DO I=1,118; READ(200,*); END DO
  DO I=1,47; READ(200,*); END DO

  !Barometric pressure	
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)BaroPressure

  DO I=1,3; READ(200,*); END DO
    
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IsCmpInAirStream

  !*************** Accumulator ****************
  DO I=1,7; READ(200,*); END DO
      
  CALL GetObjectItem('AccumulatorData',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)  

  Acc_Manufacturer = Alphas(1)
  Acc_Model = Alphas(2)
  
  Acc_ChgCap = Numbers(1)  !Charge Capacity
  Acc_SysCap = Numbers(2)  !Max. Recommended System Capacity
  Acc_Chg = Numbers(3)  !Charge
  Acc_DP = Numbers(4)   !Pressure Drop
  AccumPAR(2) = Numbers(5)  !Height
  AccumPAR(1) = Numbers(6)  !Diameter
  AccumPAR(4) = Numbers(7)  !Upper hole diameter
  AccumPAR(3) = Numbers(8)  !Lower hole diameter
  AccumPAR(7) = Numbers(9)  !Rating Pressure Drop
  AccumPAR(5) = Numbers(10) !Hole distance
  AccumPAR(8) = Numbers(11) !Rating Temperature Drop
  AccumPAR(9) = Numbers(12) !Coefficient M
  AccumPAR(10) = Numbers(13)    !Coefficient B

  AccumPAR(6)=(SucLnPAR(2)-SucLnPAR(3)/1000*2) !J-tube diameter, mm or in


  !*************** Filter Drier ****************
  DO I=1,3; READ(200,*); END DO

  CALL GetObjectItem('FilterDrierData',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)  

  Filter_Manufacturer = Alphas(1)
  Filter_Model = Alphas(2)
  
  FilterPAR(1) = Numbers(1) !Flow capacity
  Filter_DP = Numbers(2)    !Pressure Drop
  FilterPAR(2) = Numbers(3) !Rating DP
  

  !****************** Material Weight Data ******************
  
  CALL GetObjectItem('MaterialWeightData',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)
  
  !---Fin---
  
  MWD_FinIC = Numbers(1)
  MWD_FinOC = Numbers(2)
  
  !---Tube---
  
  MWD_TubeIC = Numbers(3)
  MWD_TubeOC = Numbers(4)
  MWD_SucLn = Numbers(5)
  MWD_DisLn = Numbers(6)
  MWD_RV_ICL = Numbers(7)   !Rev. Valve Indoor Coil Line
  MWD_RV_OCL = Numbers(8)   !Rev. Valve Outdoor Coil Line
  MWD_LiqLn = Numbers(9)
  
  !*************** System Cost Data ***************
  
  CALL GetObjectItem('SystemCostData',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)  
  
  SystemCost = Numbers(1)
  
  !---Unit Cost---
  
  UC_Acc = Numbers(2)   !Unit Cost for Accumulator
  UC_Com = Numbers(3)   !Unit Cost for Compressor
  UC_Dis = Numbers(4)   !Unit Cost for Distributor
  UC_FD = Numbers(5)    !Unit Cost for Filter Drier
  UC_IF = Numbers(6)    !Unit Cost for Indoor Fan
  UC_OF = Numbers(7)    !Unit Cost for Outdoor Fan
  UC_IP = Numbers(8)    !Unit Cost for Interconnecting Piping
  UC_RV = Numbers(9)    !Unit Cost for Reversing Valve
  UC_STO = Numbers(10)  !Unit Cost for Short Tube Orifice
  UC_TXV = Numbers(11)  !Unit Cost for TXV
  UC_ICC = Numbers(12)  !Unit Cost for Indoor Coil Copper
  UC_ICA = Numbers(13)  !Unit Cost for Indoor Coil Aluminum
  UC_OCC = Numbers(14)  !Unit Cost for Outdoor Coil Copper
  UC_OCA = Numbers(15)  !Unit Cost for Outdoor Coil Aluminum
  UC_Other = Numbers(16)    !Unit Cost for "Other"
  
  !---Quantity---
  
  Qu_Acc = Numbers(17)  !Accumulator Quantity
  Qu_Com = Numbers(18)   !Compressor Quantity
  Qu_Dis = Numbers(19)   !Distributor Quantity
  Qu_FD = Numbers(20)    !Filter Drier Quantity
  Qu_IF = Numbers(21)    !Indoor Fan Quantity
  Qu_OF = Numbers(22)    !Outdoor Fan Quantity
  Qu_IP = Numbers(23)    !Interconnecting Piping Quantity
  Qu_RV = Numbers(24)    !Reversing Valve Quantity
  Qu_STO = Numbers(25)  !Short Tube Orifice Quantity
  Qu_TXV = Numbers(26)  !TXV Quantity
  Qu_IC = Numbers(27)  !Indoor Coil Quantity
  Qu_OC = Numbers(28)  !Outdoor Coil Quantity
  Qu_Other = Numbers(29)    !Quantity of "Other"
  
  !---Total Cost---
  
  TC_Acc = Numbers(30)  !Accumulator Total Cost
  TC_Com = Numbers(31)   !Compressor Total Cost
  TC_Dis = Numbers(32)   !Distributor Total Cost
  TC_FD = Numbers(33)    !Filter Drier Total Cost
  TC_IF = Numbers(34)    !Indoor Fan Total Cost
  TC_OF = Numbers(35)    !Outdoor Fan Total Cost
  TC_IP = Numbers(36)    !Interconnecting Piping Total Cost
  TC_RV = Numbers(37)    !Reversing Valve Total Cost
  TC_STO = Numbers(38)  !Short Tube Orifice Total Cost
  TC_TXV = Numbers(39)  !TXV Total Cost
  TC_IC = Numbers(40)  !Indoor Coil Total Cost
  TC_OC = Numbers(41)  !Outdoor Coil Total Cost
  TC_Other = Numbers(42)    !Total Cost of "Other"
  
  
  !*************** Custom Air Side Heat Transfer Data **************
  DO I=1,60; READ(200,*); END DO

  CALL GetObjectItem('CustomAirSideHeatTransferData',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)        

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

  READ(200,*) 
  
  !Pressure drop data

  IDC_CurveTypeDP = Numbers(9) !Curve Type
  IDC_PowerADP = Numbers(10)    !Power Fit Coefficient A
  IDC_PowerBDP = Numbers(11)    !Power Fit Coefficient B
  IDC_Poly1DP = Numbers(12) !Polynomial Fit Coefficient C1
  IDC_Poly2DP = Numbers(13) !Polynomial Fit Coefficient C2
  IDC_Poly3DP = Numbers(14) !Polynomial Fit Coefficient C3
  IDC_Poly4DP = Numbers(15) !Polynomial Fit Coefficent C4
  
  READ(200,*) 
  
  !---Outdoor Coil---
  
  ODC_CurveUnit = Numbers(16)

  READ(200,*)
  
  !Heat Transfer data
  
  ODC_CurveTypeHTC = Numbers(17) !Curve Type
  ODC_PowerAHTC = Numbers(18)    !Power Fit Coefficient A
  ODC_PowerBHTC = Numbers(19)    !Power Fit Coefficient B
  ODC_Poly1HTC = Numbers(20) !Polynomial Fit Coefficient C1
  ODC_Poly2HTC = Numbers(21) !Polynomial Fit Coefficient C2
  ODC_Poly3HTC = Numbers(22) !Polynomial Fit Coefficient C3
  ODC_Poly4HTC = Numbers(23) !Polynomial Fit Coefficent C4

  READ(200,*) 
  
  !Pressure drop data
  
  ODC_CurveTypeDP = Numbers(9) !Curve Type
  ODC_PowerADP = Numbers(10)    !Power Fit Coefficient A
  ODC_PowerBDP = Numbers(11)    !Power Fit Coefficient B
  ODC_Poly1DP = Numbers(12) !Polynomial Fit Coefficient C1
  ODC_Poly2DP = Numbers(13) !Polynomial Fit Coefficient C2
  ODC_Poly3DP = Numbers(14) !Polynomial Fit Coefficient C3
  ODC_Poly4DP = Numbers(15) !Polynomial Fit Coefficent C4


  READ(200,202)LineData
 
  !*************** Charge Tuning Curve ***************
  DO I=1,60; READ(200,*); END DO
  
  CALL GetObjectItem('ChargeTuningCurve',1,Alphas,NumAlphas, &
                        Numbers,NumNumbers,Status)      

  SELECT CASE (Alphas(1)(1:1))  !Is Charge Tuning?
  CASE ('F','f')
      IsChargeTuning=0
  CASE DEFAULT ('T','t')
      IsChargeTuning=1
  END SELECT
  
  RefSimulatedCharge = Numbers(1)   !Tuning Point #1 Simulated Charge
  RefLiquidLength = Numbers(2)  !Tuning Point #1 Liquid Length
  SimulatedCharge2 = Numbers(3) !Tuning Point #2 Simulated Charge
  LiquidLength2 = Numbers(4)    !Tuning Points #2 Liquid Length
  TuningCurveIntercept = Numbers(5)
  TuningCurveSlope = Numbers(6)
  
  !*************** Air Handler Data **************
  DO I=1,60; READ(200,*); END DO

  CALL GetObjectItem('AirHandlerData',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)
  
  SELECT CASE (Alphas(1)(1:1))  !Use Air Handler Data
  CASE ('F','f')
      UseAirHandlerData=0
  CASE DEFAULT ('T','t')
      UseAirHandlerData=1
  END SELECT
  
  AHD_Ton = Numbers(1)  !Tonnage
  
  AHD_CM = Alphas(2)    !Coil Model
  AHD_AHM = Alphas(3)   !Air Handler Model
  
  !*************** Condensor Curve Data **************
  DO I=1,60; READ(200,*); END DO

  CALL GetObjectItem('CondensorCurveData',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)

  CCD_AC = Numbers(1)   !A-Capacity
  CCD_AP = Numbers(2)   !A-Power
  CCD_ASP = Numbers(3)  !A-Suction Pressure
  CCD_BC = Numbers(4)   !B-Capacity
  CCD_BP = Numbers(5)   !B-Power
  CCD_BSP = Numbers(6)  !B-Suction Pressure
  CCD_ACS = Numbers(7)  !A-Capacity Slope
  CCD_APS = Numbers(8)   !A-Power Slope
  CCD_BCS = Numbers(9)  !B-Capacity Slope
  CCD_BPS = Numbers(10) !B-Power Slope
  CCD_ACI = Numbers(11) !A-Capacity Intercept
  CCD_API = Numbers(12) !A-Power Intercept
  CCD_BCI = Numbers(13) !B-Capacity Intercept
  CCD_BPI = Numbers(14) !B-Power Intercept
  CCD_ALT = Numbers(15) !A-Liquid Temperature
  
!  DO I=1,23; READ(200,*); END DO
!  READ(200,202)LineData
!
!  IF(LineData(1:38) .EQ. ' ***************************** Defrost') THEN
!		READ(200,202)LineData
!		I=SCAN(LineData,',')
!		BufferString=ADJUSTL(LineData(I+1:150))
!		READ(BufferString,*)AmbTemp(1)
!
!		READ(200,202)LineData
!		I=SCAN(LineData,',')
!		BufferString=ADJUSTL(LineData(I+1:150))
!		READ(BufferString,*)InitiateTemp(1)
!
!		READ(200,202)LineData
!		I=SCAN(LineData,',')
!		BufferString=ADJUSTL(LineData(I+1:150))
!		READ(BufferString,*)AmbTemp(2)
!
!		READ(200,202)LineData
!		I=SCAN(LineData,',')
!		BufferString=ADJUSTL(LineData(I+1:150))
!		READ(BufferString,*)InitiateTemp(2)
!
!  END IF

  CLOSE(200)
  
  CALL RefrigIn(Ref$)

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
  !IF (Unit .EQ. 1) THEN !SI unit !ISI - 07/14/06
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
	
	  !IF (Unit .EQ. 1) THEN !SI unit !ISI - 07/14/06
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
			!EqDiameter=(DisLnPAR(2)+ValveODCLnPAR(2))/2
			EqLength=DisLnPAR(1)+ValveODCLnPAR(1) !ISI - 08/03/06
			EqThickness=(DisLnPAR(3)+ValveODCLnPAR(3))/2
			TotElevation=DisLnPAR(4)+ValveODCLnPAR(4)
			TotHeatGain=DisLnPAR(5)+ValveODCLnPAR(5)
			TotTempChange=DisLnPAR(6)+ValveODCLnPAR(6)
			TotAddDP=DisLnPAR(7)+ValveODCLnPAR(7)
		END IF
		!EqLength=4*TotVolume/(PI*((EqDiameter-2*EqThickness)*1e-3)**2)
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
			!EqDiameter=(SucLnPAR(2)+ValveIDCLnPAR(2))/2
			EqLength=SucLnPAR(1)+ValveIDCLnPAR(1) !ISI - 08/03/06
			EqThickness=(SucLnPAR(3)+ValveIDCLnPAR(3))/2
			TotElevation=SucLnPAR(4)+ValveIDCLnPAR(4)
			TotHeatGain=SucLnPAR(5)+ValveIDCLnPAR(5)
			TotTempChange=SucLnPAR(6)+ValveIDCLnPAR(6)
			TotAddDP=SucLnPAR(7)+ValveIDCLnPAR(7)
		END IF
		!EqLength=4*TotVolume/(PI*((EqDiameter-2*EqThickness)*1e-3)**2)
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
			!EqDiameter=(DisLnPAR(2)+ValveODCLnPAR(2))/2
			EqLength=DisLnPAR(1)+ValveODCLnPAR(1) !ISI - 08/03/06
			EqThickness=(DisLnPAR(3)+ValveODCLnPAR(3))/2
			TotElevation=DisLnPAR(4)+ValveODCLnPAR(4)
			TotHeatGain=DisLnPAR(5)+ValveODCLnPAR(5)
			TotTempChange=DisLnPAR(6)+ValveODCLnPAR(6)
			TotAddDP=DisLnPAR(7)+ValveODCLnPAR(7)
		END IF
		!EqLength=4*TotVolume/(PI*((EqDiameter-2*EqThickness*1e-3)/12)**2)
		EqDiameter=SQRT(4*TotVolume/(PI*EqLength))*12+2*EqThickness*1e-3 !ISI - 08/03/06

        DisLnPAR(1)=EqLength
	    DisLnPAR(2)=EqDiameter
	    DisLnPAR(3)=EqThickness
	    DisLnPAR(4)=TotElevation
	    DisLnPAR(5)=TotHeatGain
	    DisLnPAR(6)=TotTempChange
	    DisLnPAR(7)=TotAddDP

        !Equilibruim suction line, combines compressor suction line and valve to IDC line
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
			!EqDiameter=(SucLnPAR(2)+ValveIDCLnPAR(2))/2
			EqLength=SucLnPAR(1)+ValveIDCLnPAR(1) !ISI - 08/03/06
			EqThickness=(SucLnPAR(3)+ValveIDCLnPAR(3))/2
			TotElevation=SucLnPAR(4)+ValveIDCLnPAR(4)
			TotHeatGain=SucLnPAR(5)+ValveIDCLnPAR(5)
			TotTempChange=SucLnPAR(6)+ValveIDCLnPAR(6)
			TotAddDP=SucLnPAR(7)+ValveIDCLnPAR(7)
		END IF
		!EqLength=4*TotVolume/(PI*((EqDiameter-2*EqThickness*1e-3)/12)**2)
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

	  !IF (Unit .EQ. 1) THEN !SI unit !ISI - 07/14/06
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
			!EqDiameter=(DisLnPAR(2)+ValveIDCLnPAR(2))/2
			EqLength=DisLnPAR(1)+ValveIDCLnPAR(1) !ISI - 08/03/06
			EqThickness=(DisLnPAR(3)+ValveIDCLnPAR(3))/2
			TotElevation=DisLnPAR(4)+ValveIDCLnPAR(4)
			TotHeatGain=DisLnPAR(5)+ValveIDCLnPAR(5)
			TotTempChange=DisLnPAR(6)+ValveIDCLnPAR(6)
			TotAddDP=DisLnPAR(7)+ValveIDCLnPAR(7)
		END IF
		!EqLength=4*TotVolume/(PI*((EqDiameter-2*EqThickness)*1e-3)**2)
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
			!EqDiameter=(SucLnPAR(2)+ValveODCLnPAR(2))/2
			EqLength=SucLnPAR(1)+ValveODCLnPAR(1) !ISI - 08/03/06
			EqThickness=(SucLnPAR(3)+ValveODCLnPAR(3))/2
			TotElevation=SucLnPAR(4)+ValveODCLnPAR(4)
			TotHeatGain=SucLnPAR(5)+ValveODCLnPAR(5)
			TotTempChange=SucLnPAR(6)+ValveODCLnPAR(6)
			TotAddDP=SucLnPAR(7)+ValveODCLnPAR(7)
		END IF
		!EqLength=4*TotVolume/(PI*((EqDiameter-2*EqThickness)*1e-3)**2)
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
			!EqDiameter=(DisLnPAR(2)+ValveIDCLnPAR(2))/2
			EqLength=DisLnPAR(1)+ValveIDCLnPAR(1) !ISI - 08/03/06
			EqThickness=(DisLnPAR(3)+ValveIDCLnPAR(3))/2
			TotElevation=DisLnPAR(4)+ValveIDCLnPAR(4)
			TotHeatGain=DisLnPAR(5)+ValveIDCLnPAR(5)
			TotTempChange=DisLnPAR(6)+ValveIDCLnPAR(6)
			TotAddDP=DisLnPAR(7)+ValveIDCLnPAR(7)
		END IF
		!EqLength=4*TotVolume/(PI*((EqDiameter-2*EqThickness*1e-3)/12)**2)
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
			!EqDiameter=(SucLnPAR(2)+ValveODCLnPAR(2))/2
			EqLength=SucLnPAR(1)+ValveODCLnPAR(1) !ISI - 08/03/06
			EqThickness=(SucLnPAR(3)+ValveODCLnPAR(3))/2
			TotElevation=SucLnPAR(4)+ValveODCLnPAR(4)
			TotHeatGain=SucLnPAR(5)+ValveODCLnPAR(5)
			TotTempChange=SucLnPAR(6)+ValveODCLnPAR(6)
			TotAddDP=SucLnPAR(7)+ValveODCLnPAR(7)
		END IF
		!EqLength=4*TotVolume/(PI*((EqDiameter-2*EqThickness*1e-3)/12)**2)
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


  IF (IsCoolingMode .GT. 0) THEN

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

  !Get coil type - ISI - 12/25/06
  OPEN (11,FILE='ODCckt.dat',IOSTAT=ErrorFlag,STATUS='OLD')

  IF (ErrorFlag .NE. NOERROR) THEN 
	  WRITE(*,*)'Outdoor coil file missing.'
      !WRITE(*,*)'Press return to terminate program.'
	  !READ(*,*)
	  CALL SLEEP(300) !Wait for 5 minutes and stop
	  STOP
  END IF

  READ (11,202,IOSTAT=ErrorFlag)LineData
  IF (ErrorFlag .NE. NOERROR) THEN 
	  WRITE(*,*)'Outdoor coil file error.'
      !WRITE(*,*)'Press return to terminate program.'
	  !READ(*,*)
	  CALL SLEEP(300) !Wait for 5 minutes and stop
	  STOP
  END IF

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


  OPEN (12,FILE='IDCckt.dat',IOSTAT=ErrorFlag,STATUS='OLD')

  IF (ErrorFlag .NE. NOERROR) THEN 
	  WRITE(*,*)'Indoor coil file missing.'
      !WRITE(*,*)'Press return to terminate program.'
	  !READ(*,*)
	  CALL SLEEP(300) !Wait for 5 minutes and stop
	  STOP
  END IF

  READ (12,202,IOSTAT=ErrorFlag)LineData
  IF (ErrorFlag .NE. NOERROR) THEN 
	  WRITE(*,*)'Indoor coil file error.'
      !WRITE(*,*)'Press return to terminate program.'
	  !READ(*,*)
	  CALL SLEEP(300) !Wait for 5 minutes and stop
	  STOP
  END IF

  IF (LineData(1:17) .EQ. 'Microchannel Coil') THEN
	  IF (IsCoolingMode .GT. 0) THEN
	    IDCcoilType = MCEVAPORATOR
	  ELSE
		IDCcoilType = MCCONDENSER
	  END IF
  ELSE
	  IF (IsCoolingMode .GT. 0) THEN
	    IDCcoilType = EVAPORATORCOIL
	  ELSE
		IDCcoilType = CONDENSERCOIL
	  END IF
  END IF

  CLOSE(12)

  201 FORMAT(10(E))
  202 FORMAT(A150)
  203 FORMAT(I1)

  RETURN

END SUBROUTINE

!***********************************************************************************

END MODULE HeatPumpInput

!***********************************************************************************
