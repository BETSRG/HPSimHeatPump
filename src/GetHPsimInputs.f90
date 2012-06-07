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
USE InputProcessor

IMPLICIT NONE

INTEGER, PARAMETER :: MaxNameLength = 200

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

!INTEGER CoolingDesignCondition  !Selected Cooling Design Condition
!Integer HeatingDesignCondition  !Selected Heating Design Condition
REAL Subcooling   !Design Subcooling
REAL Superheat    !Design Superheat
CHARACTER(len=MaxNameLength)RefrigerantName
REAL NumofRefrigerants    !Number of Refrigerants in Blend
REAL NominalCoolingCapacity
REAL NominalHeatingCapacity
REAL ElectricHeating
CHARACTER(len=MaxNameLength)DesignConditionDescription
REAL OutdoorEnteringDrybulbTemperature
REAL OutdoorEnteringWetbulbTemperature
REAL IndoorEnteringDrybulbTemperature
REAL IndoorEnteringWetbulbTemperature
REAL DesignRefChg    !Design Refrigerant Charge Mass
REAL RefChg  !Refrigerant Charge Mass
REAL RefMassFlowRate !Refrigerant Mass Flow Rate
REAL SystemEER
REAL SystemSEER
REAL SystemCOP
REAL SensibleTotalHeatRatio
REAL CalculatedSubcooling
REAL CalculatedSuperheat
REAL CalculatedCoolingCapacity
REAL CalculatedHeatingCapacity

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
REAL UserSpecifiedRatingHeatingModeSubcooling
REAL UserSpecifiedRatingHeatingModeSuperheat
 
REAL ODC_CoilAirPressureDrop
REAL ODC_CoilAirOutletDrybulbTemp
REAL ODC_CoilAirOutletWetbulbTemp
REAL ODC_CoilAirOutletRelativeHumidity
REAL ODC_CoilAirFaceVelocity
REAL ODC_CoilHeatTransferRate
REAL ODC_MassinCoil
REAL ODC_InternalVolume
REAL ODC_CoilAirLeakage  !Air Leakage Around the Coil

REAL PwrODfan !Outdoor Fan Power
!REAL VdotODfan    !Outdoor Fan Air Flow Rate
!REAL ODdrawBlow   !Outdoor Fan Draw Through (1) or Blow Through (2)

REAL IDC_CoilAirPressureDrop
REAL IDC_CoilAirOutletDrybulbTemperature
REAL IDC_CoilAirOutletWetbulbTemperature
REAL IDC_CoilAirOutletRelativeHumidity
REAL IDC_CoilAirFaceVelocity
REAL IDC_CoilHeatTransferRate 
REAL IDC_MassinCoil
REAL IDC_InternalVolume
REAL IDC_CoilAirLeakage  !Air Leakage Around the Coil

REAL PwrIDfan !Fan Power
!REAL VdotIDfan    !Fan Air Flow Rate
!REAL IDdrawBlow   !Draw Through or Blow Through

CHARACTER(len=MaxNameLength)SucLn_RefrigerantLine
CHARACTER(len=MaxNameLength)SucLn_TubeType
REAL SucLn_KTube    !Conductivity of Suction Line Tube
REAL SucLn_TubeID   !Inner Diameter of Suction Line Tube
REAL SucLn_Charge
CHARACTER(len=MaxNameLength)DisLn_RefrigerantLine
CHARACTER(len=MaxNameLength)DisLn_TubeType
REAL DisLn_KTube    !Conductivity of Discharge Line Tube
REAL DisLn_TubeID   !Inner Diameter of Discharge Line Tube
REAL DisLn_Charge
CHARACTER(len=MaxNameLength)LiqLn_RefrigerantLine
CHARACTER(len=MaxNameLength)LiqLn_TubeType
REAL LiqLn_KTube    !Conductivity of Liquid Line Tube
REAL LiqLn_TubeID   !Inner Diameter of Liquid Line Tube
REAL LiqLn_Charge
CHARACTER(len=MaxNameLength)ValveIDCLn_RefrigerantLine
CHARACTER(len=MaxNameLength)ValveIDCLn_TubeType
REAL ValveIDCLn_KTube    !Conductivity of Valce to IDC Line Tube
REAL ValveIDCLn_TubeID   !Inner Diameter of Valce to IDC Line Tube
REAL ValveIDCLn_Charge
CHARACTER(len=MaxNameLength)ValveODCLn_RefrigerantLine
CHARACTER(len=MaxNameLength)ValveODCLn_TubeType
REAL ValveODCLn_KTube    !Conductivity of Valve to ODC Line Tube
REAL ValveODCLn_TubeID   !Inner Diameter of Valve to ODC Line Tube
REAL ValveODCLn_Charge

CHARACTER(len=MaxNameLength)RCDC_ComSuc_CyclePoint
REAL RCDC_ComSuc_Pressure
REAL RCDC_ComSuc_Enthalpy
REAL RCDC_ComSuc_Temperature 
REAL RCDC_ComSuc_Quality
REAL RCDC_ComSuc_Superheat
REAL RCDC_ComSuc_Subcooling 
CHARACTER(len=MaxNameLength)RCDC_ComDis_CyclePoint
REAL RCDC_ComDis_Pressure
REAL RCDC_ComDis_Enthalpy
REAL RCDC_ComDis_Temperature
REAL RCDC_ComDis_Quality
REAL RCDC_ComDis_Superheat
REAL RCDC_ComDis_Subcooling
CHARACTER(len=MaxNameLength)RCDC_OCI_CyclePoint
REAL RCDC_OCI_Pressure
REAL RCDC_OCI_Enthalpy
REAL RCDC_OCI_Temperature
REAL RCDC_OCI_Quality
REAL RCDC_OCI_Superheat
REAL RCDC_OCI_Subcooling
CHARACTER(len=MaxNameLength)RCDC_OCO_CyclePoint
REAL RCDC_OCO_Pressure
REAL RCDC_OCO_Enthalpy
REAL RCDC_OCO_Temperature
REAL RCDC_OCO_Quality
REAL RCDC_OCO_Superheat
REAL RCDC_OCO_Subcooling
CHARACTER(len=MaxNameLength)RCDC_EDI_CyclePoint
REAL RCDC_EDI_Pressure
REAL RCDC_EDI_Enthalpy
REAL RCDC_EDI_Quality
REAL RCDC_EDI_Superheat
REAL RCDC_EDI_Subcooling
CHARACTER(len=MaxNameLength)RCDC_EDO_CyclePoint
REAL RCDC_EDO_Pressure
REAL RCDC_EDO_Enthalpy
REAL RCDC_EDO_Temperature
REAL RCDC_EDO_Quality
REAL RCDC_EDO_Superheat
REAL RCDC_EDO_Subcooling
CHARACTER(len=MaxNameLength)RCDC_ICI_CyclePoint
REAL RCDC_ICI_Pressure
REAL RCDC_ICI_Enthalpy
REAL RCDC_ICI_Temperature
REAL RCDC_ICI_Quality
REAL RCDC_ICI_Superheat
REAL RCDC_ICI_Subcooling
CHARACTER(len=MaxNameLength)RCDC_ICO_CyclePoint
REAL RCDC_ICO_Pressure
REAL RCDC_ICO_Enthalpy
REAL RCDC_ICO_Temperature
REAL RCDC_ICO_Quality
REAL RCDC_ICO_Superheat
REAL RCDC_ICO_Subcooling

CHARACTER(len=MaxNameLength)RCDH_ComSuc_CyclePoint
REAL RCDH_ComSuc_Pressure
REAL RCDH_ComSuc_Enthalpy
REAL RCDH_ComSuc_Temperature
REAL RCDH_ComSuc_Quality
REAL RCDH_ComSuc_Superheat
REAL RCDH_ComSuc_Subcooling
CHARACTER(len=MaxNameLength)RCDH_ComDis_CyclePoint
REAL RCDH_ComDis_Pressure
REAL RCDH_ComDis_Enthalpy
REAL RCDH_ComDis_Quality
REAL RCDH_ComDis_Superheat
REAL RCDH_ComDis_Subcooling
CHARACTER(len=MaxNameLength)RCDH_OCI_CyclePoint
REAL RCDH_OCI_Pressure
REAL RCDH_OCI_Enthalpy
REAL RCDH_OCI_Temperature
REAL RCDH_OCI_Quality
REAL RCDH_OCI_Superheat
REAL RCDH_OCI_Subcooling
CHARACTER(len=MaxNameLength)RCDH_OCO_CyclePoint
REAL RCDH_OCO_Pressure
REAL RCDH_OCO_Enthalpy
REAL RCDH_OCO_Temperature
REAL RCDH_OCO_Quality
REAL RCDH_OCO_Superheat
REAL RCDH_OCO_Subcooling
CHARACTER(len=MaxNameLength)RCDH_EDI_CyclePoint
REAL RCDH_EDI_Pressure
REAL RCDH_EDI_Enthalpy
REAL RCDH_EDI_Temperature
REAL RCDH_EDI_Quality
REAL RCDH_EDI_Superheat
REAL RCDH_EDI_Subcooling
CHARACTER(len=MaxNameLength)RCDH_EDO_CyclePoint
REAL RCDH_EDO_Pressure
REAL RCDH_EDO_Enthalpy
REAL RCDH_EDO_Temperature
REAL RCDH_EDO_Quality
REAL RCDH_EDO_Superheat
REAL RCDH_EDO_Subcooling
CHARACTER(len=MaxNameLength)RCDH_ICI_CyclePoint
REAL RCDH_ICI_Pressure
REAL RCDH_ICI_Enthalpy
REAL RCDH_ICI_Temperature
REAL RCDH_ICI_Quality
REAL RCDH_ICI_Superheat
REAL RCDH_ICI_Subcooling
CHARACTER(len=MaxNameLength)RCDH_ICO_CyclePoint
REAL RCDH_ICO_Pressure
REAL RCDH_ICO_Enthalpy
REAL RCDH_ICO_Temperature
REAL RCDH_ICO_Quality
REAL RCDH_ICO_Superheat
REAL RCDH_ICO_Subcooling
CHARACTER(len=MaxNameLength)RCDH_OCckt    !Outdoor Coil Ckt
CHARACTER(len=MaxNameLength)RCDH_ICckt   !Indoor Coil Ckt
REAL RCDH_Com_Chg    !Charge in Compressor
REAL RCDH_DistC_Chg  !Charge in Distributor Tube (Cooling)
REAL RCDH_DistH_Chg  !Charge in Distributor Tube (Heating)

CHARACTER(len=MaxNameLength)Acc_Manufacturer
CHARACTER(len=MaxNameLength)Acc_Model
REAL Acc_ChgCap  !Accumulator Charge Capacity
REAL Acc_SysCap  !Accumulator Max. Recommended System Capacity
REAL Acc_Chg  !Accumulator Charge
REAL Acc_DP   !Accumulator Pressure Drop

CHARACTER(len=MaxNameLength)Filter_Manufacturer
CHARACTER(len=MaxNameLength)Filter_Model
REAL Filter_DP    !Filter Pressure Drop

REAL MWD_FinIC    !Indoor Coil Fin
REAL MWD_FinOC    !Outdoor Coil Fin
REAL MWD_TubeIC   !Indoor Coil Tube
REAL MWD_TubeOC   !Outdoor Coil Tunbe
REAL MWD_SucLn    !Suction Line
REAL MWD_DisLn    !Discharge Line
REAL MWD_RV_ICL   !Rev. Valve Indoor Coil Line
REAL MWD_RV_OCL   !Rev. Valve Outdoor Coil Line
REAL MWD_LiqLn

REAL UC_Acc   !Unit Cost for Accumulator
REAL UC_Com   !Unit Cost for Compressor
REAL UC_Dis   !Unit Cost for Distributor
REAL UC_FD    !Unit Cost for Filter Drier
REAL UC_IF   !Unit Cost for Indoor Fan
REAL UC_OF   !Unit Cost for Outdoor Fan
REAL UC_IP    !Unit Cost for Interconnecting Piping
REAL UC_RV    !Unit Cost for Reversing Valve
REAL UC_STO  !Unit Cost for Short Tube Orifice
REAL UC_TXV  !Unit Cost for TXV
REAL UC_ICC  !Unit Cost for Indoor Coil Copper
REAL UC_ICA  !Unit Cost for Indoor Coil Aluminum
REAL UC_OCC  !Unit Cost for Outdoor Coil Copper
REAL UC_OCA  !Unit Cost for Outdoor Coil Aluminum
REAL UC_Other    !Unit Cost for "Other"
REAL Qu_Acc  !Accumulator Quantity
REAL Qu_Com   !Compressor Quantity
REAL Qu_Dis   !Distributor Quantity
REAL Qu_FD    !Filter Drier Quantity
REAL Qu_IF    !Indoor Fan Quantity
REAL Qu_OF   !Outdoor Fan Quantity
REAL Qu_IP    !Interconnecting Piping Quantity
REAL Qu_RV    !Reversing Valve Quantity
REAL Qu_STO  !Short Tube Orifice Quantity
REAL Qu_TXV  !TXV Quantity
REAL Qu_IC  !Indoor Coil Quantity
REAL Qu_OC  !Outdoor Coil Quantity
REAL Qu_Other    !Quantity of "Other"
REAL TC_Acc  !Accumulator Total Cost
REAL TC_Com   !Compressor Total Cost
REAL TC_Dis  !Distributor Total Cost
REAL TC_FD    !Filter Drier Total Cost
REAL TC_IF    !Indoor Fan Total Cost
REAL TC_OF    !Outdoor Fan Total Cost
REAL TC_IP    !Interconnecting Piping Total Cost
REAL TC_RV    !Reversing Valve Total Cost
REAL TC_STO  !Short Tube Orifice Total Cost
REAL TC_TXV  !TXV Total Cost
REAL TC_IC  !Indoor Coil Total Cost
REAL TC_OC  !Outdoor Coil Total Cost
REAL TC_Other    !Total Cost of "Other"

REAL TuningCurveIntercept
REAL TuningCurveSlope

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

REAL SC_mdot  !Mass Flow Rate Specified

REAL GlycolCon    !Glycol Concentration
CHARACTER(len=MaxNameLength)GlycolNa  !Glycol Name
REAL GlycolFR !Flow Rate
REAL GlycolET !Entering Temperature
REAL GlycolLT !Leaving Temperature
REAL GlycolDP !Pressure Drop
REAL GlycolTC !Total Charge

CHARACTER(len=MaxNameLength)GLE_RefrigerantLinE
REAL GLE_TubeType
REAL GLE_Length   !Refrigerant Line Length
REAL GLE_Elev !Refrigerant Line Elevation
REAL GLE_HeatLoss !Refrigerant Line Heat Loss
REAL GLE_TempChange   !Refrigerant Line Temperature Change
REAL GLE_Ktube    !Tube Conductivity
REAL GLE_TubeID   !Tube Inside Diameter
REAL GLE_TubeOD   !Tube Outside Diameter
REAL GLE_ADP  !Additional Pressure Drop
REAL GLE_Charge  !Charge in Line
CHARACTER(len=MaxNameLength)GLL_RefrigerantLine
REAL GLL_TubeType
REAL GLL_Length  !Refrigerant Line Length
REAL GLL_Elev    !Refrigerant Line Elevation
REAL GLL_HeatLoss    !Refrigerant Line Heat Loss
REAL GLL_TempChange  !Refrigerant Line Temperature Change
REAL GLL_Ktube   !Tube Conductivity
REAL GLL_TubeID  !Tube Inside Diameter
REAL GLL_TubeOD  !Tube Outside Diameter
REAL GLL_ADP !Additional Pressure Drop
REAL GLL_Charge  !Charge in Line

CHARACTER(len=MaxNameLength)PumpManufacturer
CHARACTER(len=MaxNameLength)PumpModel
REAL PumpFlowRate
REAL PumpPower

CHARACTER(len=MaxNameLength)BPHXCoil
REAL BPHX_TCW !Total Coil Weight
REAL BPHX_HPC !Hot Pass Capacity
REAL BPHX_HPDP    !Hot Pass Pressure Drop
REAL BPHX_HPDPM   !Hot Pass Pressure Drop Multiplier
REAL BPHX_HPIV    !Hot Pass Internal Volume
REAL BPHX_HPCh    !Hot Pass Charge
REAL BPHX_CPC !Cold Pass Capacity
REAL BPHX_CPDP    !Cold Pass Pressure Drop
REAL BPHX_CPDPM   !Cold Pass Pressure Drop Multiplier
REAL BPHX_CPIV   !Cold Pass Internal Volume
REAL BPHX_CPCh   !Cold Pass Charge

CHARACTER(len=MaxNameLength)GLD_BPHXI_CyclePoint
REAL GLD_BPHXI_Pressure
REAL GLD_BPHXI_Temperature
CHARACTER(len=MaxNameLength)GLD_BPHXO_CyclePoint
REAL GLD_BPHXO_Pressure
REAL GLD_BPHXO_Temperature
CHARACTER(len=MaxNameLength)GLD_ICI_CyclePoint
REAL GLD_ICI_Pressure
REAL GLD_ICI_Temperature
CHARACTER(len=MaxNameLength)GLD_ICO_CyclePoint
REAL GLD_ICO_Pressure
REAL GLD_ICO_Temperature

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
!Flow:

  ODC_SurfAbsorptivity=1
  IDC_SurfAbsorptivity=1

  
  !***************** System data *****************

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

  CoolingDesignCondition = Numbers(3)   !Selected Cooling Design Condition
  HeatingDesignCondition = Numbers(4)   !Selected Heating Design Condition
  Subcooling = Numbers(5)   !Design Subcooling
  Superheat = Numbers(6)    !Design Superheat
  
  RefrigerantName = Alphas(3)
  Ref$=RefrigerantName
  
  NumofRefrigerants = Numbers(7)    !Number of Refrigerants in Blend
  NominalCoolingCapacity = Numbers(8)
  NominalHeatingCapacity = Numbers(9)
  ElectricHeating = Numbers(10)
  
  DesignConditionDescription = Alphas(4)
  
  OutdoorEnteringDrybulbTemperature = Numbers(11)
  OutdoorEnteringWetbulbTemperature = Numbers(12)
  IndoorEnteringDrybulbTemperature = Numbers(13)
  IndoorEnteringWetbulbTemperature = Numbers(14)
  DesignRefChg = Numbers(15)    !Design Refrigerant Charge Mass
  RefChg = Numbers(16)  !Refrigerant Charge Mass
  RefMassFlowRate = Numbers(17) !Refrigerant Mass Flow Rate
  SystemEER = Numbers(18)
  SystemSEER = Numbers(19)
  SystemCOP = Numbers(20)
  SensibleTotalHeatRatio = Numbers(21)
  CalculatedSubcooling = Numbers(22)
  CalculatedSuperheat = Numbers(23)
  CalculatedCoolingCapacity = Numbers(24)
  CalculatedHeatingCapacity = Numbers(25)


  !***************** Compressor data *****************

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
  
  CALL GetObjectItem('OutdoorCoilData',1,Alphas,NumAlphas, &
                        Numbers,NumNumbers,Status)   

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
  ODC_CoilAirPressureDrop = Numbers(16)
  ODC_CoilAirOutletDrybulbTemp = Numbers(17)
  ODC_CoilAirOutletWetbulbTemp = Numbers(18)
  ODC_CoilAirOutletRelativeHumidity = Numbers(19)
  ODC_CoilAirFaceVelocity = Numbers(20)
  ODC_CoilHeatTransferRate = Numbers(21)
  ODC_MassinCoil = Numbers(22)
  ODC_InternalVolume = Numbers(23)
  ODC_hciMultiplier = Numbers(24)   !Ref Side Heat Transfer Multiplier
  ODC_DPrefMultiplier = Numbers(25) !Ref Side Pressure Drop Multiplier
  ODC_hcoMultiplier = Numbers(26)   !Air Side Heat Transfer Multiplier
  ODC_DPairMultiplier = Numbers(27) !Air Side Pressure Drop Multiplier
  ODC_CoilAirLeakage = Numbers(28)  !Air Leakage Around the Coil


  !***************** Outdoor fan data *****************
  
  CALL GetObjectItem('OutdoorFanData',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)   
  
  PwrODfan = Numbers(1) !Fan Power
  VdotODfan = Numbers(2)    !Fan Air Flow Rate
  ODdrawBlow = Numbers(3)   !Draw Through (1) or Blow Through (2)


    !Then we don't end up reading the separator *** line here, we just start with the next line

  !***************** Indoor coil data *****************
  
  CALL GetObjectItem('IndoorCoilData',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)
  
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
  IDC_CoilAirPressureDrop = Numbers(16)
  IDC_CoilAirOutletDrybulbTemperature = Numbers(17)
  IDC_CoilAirOutletWetbulbTemperature = Numbers(18)
  IDC_CoilAirOutletRelativeHumidity = Numbers(19)
  IDC_CoilAirFaceVelocity = Numbers(20)
  IDC_CoilHeatTransferRate = Numbers(21)
  IDC_MassinCoil = Numbers(22)
  IDC_InternalVolume = Numbers(23)
  IDC_hciMultiplier = Numbers(24)   !Ref Side Heat Transfer Multiplier
  IDC_DPrefMultiplier = Numbers(25) !Ref Side Pressure Drop Multiplier
  IDC_hcoMultiplier = Numbers(26)   !Air Side Heat Transfer Multiplier
  IDC_DPairMultiplier = Numbers(27) !Air Side Pressure Drop Multiplier
  IDC_CoilAirLeakage = Numbers(28)  !Air Leakage Around the Coil

  !Tube wall thickness, mm or mil
  IDC_TubeThk=(IDC_TubeOD-IDC_TubeID)/2
  !IF (Unit .EQ. 2) IDC_TubeThk=IDC_TubeThk*1000  !ISI - 07/14/06
  IF (Unit .EQ. IP) IDC_TubeThk=IDC_TubeThk*1000 


  !***************** Indoor fan data *****************
  
  CALL GetObjectItem('IndoorFanData',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)  
  
  PwrIDfan = Numbers(1) !Fan Power
  VdotIDfan = Numbers(2)    !Fan Air Flow Rate
  IDdrawBlow = Numbers(3)   !Draw Through or Blow Through


  !***************** Expansion device data *****************
  
  CALL GetObjectItem('ExpansionDeviceData',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)  
  

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


  CALL GetObjectItem('RefrigerantCycleData(Cooling)',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)  

  !Compressor Suction
  
  RCDC_ComSuc_CyclePoint = Alphas(1)
  
  RCDC_ComSuc_Pressure = Numbers(1)
  RCDC_ComSuc_Enthalpy = Numbers(2)
  RCDC_ComSuc_Temperature = Numbers(3)
  RCDC_ComSuc_Quality = Numbers(4)
  RCDC_ComSuc_Superheat = Numbers(5)
  RCDC_ComSuc_Subcooling = Numbers(6)
  
  !Compressor Discharge
  
  RCDC_ComDis_CyclePoint = Alphas(2)
  
  RCDC_ComDis_Pressure = Numbers(7)
  RCDC_ComDis_Enthalpy = Numbers(8)
  RCDC_ComDis_Temperature = Numbers(9)
  RCDC_ComDis_Quality = Numbers(10)
  RCDC_ComDis_Superheat = Numbers(11)
  RCDC_ComDis_Subcooling = Numbers(12)
  
  !Outdoor Coil Inlet
  
  RCDC_OCI_CyclePoint = Alphas(3)
  
  RCDC_OCI_Pressure = Numbers(13)
  RCDC_OCI_Enthalpy = Numbers(14)
  RCDC_OCI_Temperature = Numbers(15)
  RCDC_OCI_Quality = Numbers(16)
  RCDC_OCI_Superheat = Numbers(17)
  RCDC_OCI_Subcooling = Numbers(18)
  
  !Outdoor Coil Outlet
  
  RCDC_OCO_CyclePoint = Alphas(4)
  
  RCDC_OCO_Pressure = Numbers(19)
  RCDC_OCO_Enthalpy = Numbers(20)
  RCDC_OCO_Temperature = Numbers(21)
  RCDC_OCO_Quality = Numbers(22)
  RCDC_OCO_Superheat = Numbers(23)
  RCDC_OCO_Subcooling = Numbers(24)
  
  !Expansion Device Inlet
  
  RCDC_EDI_CyclePoint = Alphas(5)
  
  RCDC_EDI_Pressure = Numbers(25)
  RCDC_EDI_Enthalpy = Numbers(26)
  Tliq = Numbers(27)    !Inlet Temperature
  RCDC_EDI_Quality = Numbers(28)
  RCDC_EDI_Superheat = Numbers(29)
  RCDC_EDI_Subcooling = Numbers(30)
  
  !Expansion Device Outlet
  
  RCDC_EDO_CyclePoint = Alphas(6)
  
  RCDC_EDO_Pressure = Numbers(31)
  RCDC_EDO_Enthalpy = Numbers(32)
  RCDC_EDO_Temperature = Numbers(33)
  RCDC_EDO_Quality = Numbers(34)
  RCDC_EDO_Superheat = Numbers(35)
  RCDC_EDO_Subcooling = Numbers(36)
  
  !Indoor Coil Inlet
  
  RCDC_ICI_CyclePoint = Alphas(7)
  
  RCDC_ICI_Pressure = Numbers(37)
  RCDC_ICI_Enthalpy = Numbers(38)
  RCDC_ICI_Temperature = Numbers(39)
  RCDC_ICI_Quality = Numbers(40)
  RCDC_ICI_Superheat = Numbers(41)
  RCDC_ICI_Subcooling = Numbers(42)
  
  !Indoor Coil Outlet
  
  RCDC_ICO_CyclePoint = Alphas(8)
  
  RCDC_ICO_Pressure = Numbers(43)
  RCDC_ICO_Enthalpy = Numbers(44)
  RCDC_ICO_Temperature = Numbers(45)
  RCDC_ICO_Quality = Numbers(46)
  RCDC_ICO_Superheat = Numbers(47)
  RCDC_ICO_Subcooling = Numbers(48)
  
  
  
  !********************Refrigerant Cycle Data (Heating)***********************

  CALL GetObjectItem('RefrigerantCycleData(Heating)',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)  

  !Compressor Suction
  
  RCDH_ComSuc_CyclePoint = Alphas(1)
  
  RCDH_ComSuc_Pressure = Numbers(1)
  RCDH_ComSuc_Enthalpy = Numbers(2)
  RCDH_ComSuc_Temperature = Numbers(3)
  RCDH_ComSuc_Quality = Numbers(4)
  RCDH_ComSuc_Superheat = Numbers(5)
  RCDH_ComSuc_Subcooling = Numbers(6)
  
  !Compressor Discharge
  
  RCDH_ComDis_CyclePoint = Alphas(2)
  
  RCDH_ComDis_Pressure = Numbers(7)
  RCDH_ComDis_Enthalpy = Numbers(8)
  Tdis = Numbers(9)  !Discharge Temperature
  RCDH_ComDis_Quality = Numbers(10)
  RCDH_ComDis_Superheat = Numbers(11)
  RCDH_ComDis_Subcooling = Numbers(12)
  
  !Outdoor Coil Inlet
  
  RCDH_OCI_CyclePoint = Alphas(3)
  
  RCDH_OCI_Pressure = Numbers(13)
  RCDH_OCI_Enthalpy = Numbers(14)
  RCDH_OCI_Temperature = Numbers(15)
  RCDH_OCI_Quality = Numbers(16)
  RCDH_OCI_Superheat = Numbers(17)
  RCDH_OCI_Subcooling = Numbers(18)
  
  !Outdoor Coil Outlet
  
  RCDH_OCO_CyclePoint = Alphas(4)
  
  RCDH_OCO_Pressure = Numbers(19)
  RCDH_OCO_Enthalpy = Numbers(20)
  RCDH_OCO_Temperature = Numbers(21)
  RCDH_OCO_Quality = Numbers(22)
  RCDH_OCO_Superheat = Numbers(23)
  RCDH_OCO_Subcooling = Numbers(24)
  
  !Expansion Device Inlet
  
  RCDH_EDI_CyclePoint = Alphas(5)
  
  RCDH_EDI_Pressure = Numbers(25)
  RCDH_EDI_Enthalpy = Numbers(26)
  RCDH_EDI_Temperature = Numbers(27)
  RCDH_EDI_Quality = Numbers(28)
  RCDH_EDI_Superheat = Numbers(29)
  RCDH_EDI_Subcooling = Numbers(30)
  
  !Expansion Device Outlet
  
  RCDH_EDO_CyclePoint = Alphas(6)
  
  RCDH_EDO_Pressure = Numbers(31)
  RCDH_EDO_Enthalpy = Numbers(32)
  RCDH_EDO_Temperature = Numbers(33)
  RCDH_EDO_Quality = Numbers(34)
  RCDH_EDO_Superheat = Numbers(35)
  RCDH_EDO_Subcooling = Numbers(36)
  
  !Indoor Coil Inlet
  
  RCDH_ICI_CyclePoint = Alphas(7)
  
  RCDH_ICI_Pressure = Numbers(37)
  RCDH_ICI_Enthalpy = Numbers(38)
  RCDH_ICI_Temperature = Numbers(39)
  RCDH_ICI_Quality = Numbers(40)
  RCDH_ICI_Superheat = Numbers(41)
  RCDH_ICI_Subcooling = Numbers(42)
  
  !Indoor Coil Outlet
  
  RCDH_ICO_CyclePoint = Alphas(8)
  
  RCDH_ICO_Pressure = Numbers(43)
  RCDH_ICO_Enthalpy = Numbers(44)
  RCDH_ICO_Temperature = Numbers(45)
  RCDH_ICO_Quality = Numbers(46)
  RCDH_ICO_Superheat = Numbers(47)
  RCDH_ICO_Subcooling = Numbers(48)
  
  RCDH_OCckt = Alphas(9)    !Outdoor Coil Ckt
  RCDH_ICckt = Alphas(10)   !Indoor Coil Ckt
  
  BaroPressure = Numbers(49)  !Barometric Pressure
  RCDH_Com_Chg = Numbers(50)    !Charge in Compressor
  RCDH_DistC_Chg = Numbers(51)  !Charge in Distributor Tube (Cooling)
  RCDH_DistH_Chg = Numbers(52)  !Charge in Distributor Tube (Heating)
  IsCmpInAirStream = Numbers(53) !Is Compressor in Air Stream
  

  !Liquid temperature, ISI - 02/08/08
  !TODO: WHATS THIS
!  READ(200,202)LineData
!  I=SCAN(LineData,',')
!  BufferString=ADJUSTL(LineData(I+1:150))
!  READ(BufferString,*)Tliq
!
!  DO I=1,35; READ(200,*); END DO
!
!  !Discharge temperature, ISI - 02/08/08
!  READ(200,202)LineData
!  I=SCAN(LineData,',')
!  BufferString=ADJUSTL(LineData(I+1:150))
!  READ(BufferString,*)Tdis
!
!  !DO I=1,118; READ(200,*); END DO
!  DO I=1,47; READ(200,*); END DO
!
!  !Barometric pressure
!  READ(200,202)LineData
!  I=SCAN(LineData,',')
!  BufferString=ADJUSTL(LineData(I+1:150))
!  READ(BufferString,*)BaroPressure
!
!  DO I=1,3; READ(200,*); END DO
!
!  READ(200,202)LineData
!  I=SCAN(LineData,',')
!  BufferString=ADJUSTL(LineData(I+1:150))
!  READ(BufferString,*)IsCmpInAirStream

  !*************** Accumulator ****************

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
  
  MWD_FinIC = Numbers(1)    !Indoor Coil Fin
  MWD_FinOC = Numbers(2)    !Outdoor Coil Fin
  
  !---Tube---
  
  MWD_TubeIC = Numbers(3)   !Indoor Coil Tube
  MWD_TubeOC = Numbers(4)   !Outdoor Coil Tunbe
  MWD_SucLn = Numbers(5)    !Suction Line
  MWD_DisLn = Numbers(6)    !Discharge Line
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
                        Numbers,NumNumbers,Status)      

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
  TuningCurveIntercept = Numbers(5)
  TuningCurveSlope = Numbers(6)
  
  !*************** Air Handler Data **************

  CALL GetObjectItem('AirHandlerData',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)
  
  SELECT CASE (Alphas(1)(1:1))  !Use Air Handler Data
  CASE ('F','f')
      UseAirHandlerData=0
  CASE ('T','t')
      UseAirHandlerData=1
  END SELECT
  
  AHD_Ton = Numbers(1)  !Tonnage
  
  AHD_CM = Alphas(2)    !Coil Model
  AHD_AHM = Alphas(3)   !Air Handler Model
  
  !*************** Condensor Curve Data **************

  CALL GetObjectItem('CondenserCurveData',1,Alphas,NumAlphas, &
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
  
  !*************** Simulation Control **************

  CALL GetObjectItem('SimulationControl',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)
  
  SC_mdot = Numbers(1)  !Mass Flow Rate Specified
  
  
  !*************** Glycol Data **************

  CALL GetObjectItem('GlycolData',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)
  
  GlycolCon = Numbers(1)    !Glycol Concentration
  
  GlycolNa = Alphas(1)  !Glycol Name
  
  GlycolFR = Numbers(2) !Flow Rate
  GlycolET = Numbers(3) !Entering Temperature
  GlycolLT = Numbers(4) !Leaving Temperature
  GlycolDP = Numbers(5) !Pressure Drop
  GlycolTC = Numbers(6) !Total Charge
  
  !*************** Glycol Line Data **************

  CALL GetObjectItem('GlycolLineData',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)
  
  !Entering Line
  
  GLE_RefrigerantLine = Alphas(1)
  
  GLE_TubeType = Numbers(1)
  GLE_Length = Numbers(2)   !Refrigerant Line Length
  GLE_Elev = Numbers(3) !Refrigerant Line Elevation
  GLE_HeatLoss = Numbers(4) !Refrigerant Line Heat Loss
  GLE_TempChange = Numbers(5)   !Refrigerant Line Temperature Change
  GLE_Ktube = Numbers(6)    !Tube Conductivity
  GLE_TubeID = Numbers(7)   !Tube Inside Diameter
  GLE_TubeOD = Numbers(8)   !Tube Outside Diameter
  GLE_ADP = Numbers(9)  !Additional Pressure Drop
  GLE_Charge = Numbers(10)  !Charge in Line
  
  !Leaving Line
  
  GLL_RefrigerantLine = Alphas(2)
  
  GLL_TubeType = Numbers(11)
  GLL_Length = Numbers(12)  !Refrigerant Line Length
  GLL_Elev = Numbers(13)    !Refrigerant Line Elevation
  GLL_HeatLoss = Numbers(14)    !Refrigerant Line Heat Loss
  GLL_TempChange = Numbers(15)  !Refrigerant Line Temperature Change
  GLL_Ktube = Numbers(16)   !Tube Conductivity
  GLL_TubeID = Numbers(17)  !Tube Inside Diameter
  GLL_TubeOD = Numbers(18)  !Tube Outside Diameter
  GLL_ADP = Numbers(19) !Additional Pressure Drop
  GLL_Charge = Numbers(20)  !Charge in Line
  
  !*************** Pump Data **************

  CALL GetObjectItem('PumpData',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)
  
  PumpManufacturer = Alphas(1)
  PumpModel = Alphas(2)
  
  PumpFlowRate = Numbers(1)
  PumpPower = Numbers(2)
  
  !*************** BPHX Data **************

  CALL GetObjectItem('BPHXData',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)
  
  BPHXCoil = Alphas(1)
  
  BPHX_TCW = Numbers(1) !Total Coil Weight
  BPHX_HPC = Numbers(2) !Hot Pass Capacity
  BPHX_HPDP = Numbers(3)    !Hot Pass Pressure Drop
  BPHX_HPDPM = Numbers(4)   !Hot Pass Pressure Drop Multiplier
  BPHX_HPIV = Numbers(5)    !Hot Pass Internal Volume
  BPHX_HPCh = Numbers(6)    !Hot Pass Charge
  BPHX_CPC = Numbers(7) !Cold Pass Capacity
  BPHX_CPDP = Numbers(8)    !Cold Pass Pressure Drop
  BPHX_CPDPM = Numbers(9)   !Cold Pass Pressure Drop Multiplier
  BPHX_CPIV = Numbers(10)   !Cold Pass Internal Volume
  BPHX_CPCh = Numbers(11)   !Cold Pass Charge
  
  !*************** Glycol Loop Data **************

  CALL GetObjectItem('GlycolLoopData',1,Alphas,NumAlphas, &
                      Numbers,NumNumbers,Status)
  
  !BPHX Inlet
  
  GLD_BPHXI_CyclePoint = Alphas(1)
  
  GLD_BPHXI_Pressure = Numbers(1)
  GLD_BPHXI_Temperature = Numbers(2)
  
  !BPHX Outlet
  
  GLD_BPHXO_CyclePoint = Alphas(2)
  
  GLD_BPHXO_Pressure = Numbers(3)
  GLD_BPHXO_Temperature = Numbers(4)
  
  !Indoor Coil Inlet
  
  GLD_ICI_CyclePoint = Alphas(3)
  
  GLD_ICI_Pressure = Numbers(5)
  GLD_ICI_Temperature = Numbers(6)
  
  !Indoor Coil Outlet
  
  GLD_ICO_CyclePoint = Alphas(4)
  
  GLD_ICO_Pressure = Numbers(7)
  GLD_ICO_Temperature = Numbers(8)
  
  
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
	  !RS Comment: Previously: CALL SLEEP(300) !Wait for 5 minutes and stop
	  STOP
  END IF

  READ (11,202,IOSTAT=ErrorFlag)LineData
  IF (ErrorFlag .NE. NOERROR) THEN 
	  WRITE(*,*)'Outdoor coil file error.'
      !WRITE(*,*)'Press return to terminate program.'
	  !READ(*,*)
	  !RS Comment: Previously: CALL SLEEP(300) !Wait for 5 minutes and stop
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
	  !RS Comment: Previously: CALL SLEEP(300) !Wait for 5 minutes and stop
	  STOP
  END IF

  READ (12,202,IOSTAT=ErrorFlag)LineData
  IF (ErrorFlag .NE. NOERROR) THEN 
	  WRITE(*,*)'Indoor coil file error.'
      !WRITE(*,*)'Press return to terminate program.'
	  !READ(*,*)
	  !RS Comment: Previously: CALL SLEEP(300) !Wait for 5 minutes and stop
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
