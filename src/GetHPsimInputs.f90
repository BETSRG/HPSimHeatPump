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
  
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  SELECT CASE (BufferString(1:1))
  CASE ('F','f')
    !Unit=2
	Unit=IP !ISI - 07/14/06
  CASE DEFAULT
    !Unit=1
	Unit=SI !ISI - 07/14/06
  END SELECT

  !Calculation mode
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)MODE

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)SystemType

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  SELECT CASE (BufferString(1:1))
  CASE ('F','f')
    IsCoolingMode=0
  CASE DEFAULT
    IsCoolingMode=1
  END SELECT

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)CoolingDesignCondition

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)HeatingDesignCondition

!  READ(200,202)LineData
!  I=SCAN(LineData,',')
!  BufferString=ADJUSTL(LineData(I+1:150))
!  READ(BufferString,*)Subcooling

!  READ(200,202)LineData
!  I=SCAN(LineData,',')
!  BufferString=ADJUSTL(LineData(I+1:150))
!  READ(BufferString,*)Superheat

  DO I=1,2; READ(200,*); END DO

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)Ref$

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  SELECT CASE (BufferString(1:1))
  CASE ('1')
    PureRef=1
  CASE DEFAULT
    PureRef=0
  END SELECT

  DO I=1,4; READ(200,*); END DO

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)Temp
  IF (IsCoolingMode .GT. 0) THEN
    TAic=Temp
  ELSE
    TAie=Temp
  END IF
  
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)Temp
  IF (IsCoolingMode .GT. 0) THEN
    RHiC=Temp
  ELSE
    RHiE=Temp
  END IF

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)Temp
  IF (IsCoolingMode .GT. 0) THEN
    TAie=Temp
  ELSE
    TAic=Temp
  END IF
  
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)Temp
  IF (IsCoolingMode .GT. 0) THEN
    RHiE=Temp
  ELSE
    RHiC=Temp
  END IF

  !READ(200,*)

  !READ(200,202)LineData
  !I=SCAN(LineData,',')
  !BufferString=ADJUSTL(LineData(I+1:150))
  !READ(BufferString,*)RefCharge

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)RefChg

  READ(200,*)

  !***************** Compressor data *****************

  DO I=1,10; READ(200,*); END DO

  !Voltage source
  READ(200,202)LineData
  I=SCAN(LineData,',')
  LineData=ADJUSTL(LineData(I+1:150))
  I=SCAN(LineData,',')
  LineData=ADJUSTL(LineData(I+1:150))
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)CompPAR(24)
  
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  
  SELECT CASE (BufferString(1:1))
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

  READ(200,*)
  !DO I=1,2; READ(200,*); END DO

!  READ(200,202)LineData
!  I=SCAN(LineData,',')
!  BufferString=ADJUSTL(LineData(I+1:150))
!  READ(BufferString,*)RrefName
  READ(200,*)

  Rref=RefName
  PureRref=PureRef

  READ(200,*)

  !Compressor shell heat loss fraction of input power
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)CompPAR(21)

  !Compressor shell heat loss W or Btu/hr
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)CompPAR(22)

  !Compressor internal volume, cm^3 or in^3
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)CompPAR(23)            

  !10 coefficients for mass flow rate, kg/hr or lbm/hr
  DO I=11,20
    READ(200,202)LineData
    J=SCAN(LineData,',')
    BufferString=ADJUSTL(LineData(J+1:150))
    READ(BufferString,*)CompPAR(I)
  END DO
  
  !10 coefficients for power, W
  DO I=1,10
    READ(200,202)LineData
    J=SCAN(LineData,',')
    BufferString=ADJUSTL(LineData(J+1:150))
    READ(BufferString,*)CompPAR(I)
  END DO

  READ(200,*)

  !Power multiplier
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)CompPAR(25)            

  !mdot mulitplier
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)CompPAR(26)            

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)TsiCmp

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)TsoCmp

  !Old format
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)Subcool

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)Super
  
  READ(200,202)LineData
  IF (LineData(1:1) .EQ. 'U' .OR. LineData(1:1) .EQ. 'u') THEN !New format 06/12/06

    IF (IsCoolingMode .GT. 0) THEN  
		DO I=1,2; READ(200,*); END DO
	ELSE

	    !READ(200,202)LineData
	    I=SCAN(LineData,',')
	    BufferString=ADJUSTL(LineData(I+1:150))
	    READ(BufferString,*)Subcool

	    READ(200,202)LineData
	    I=SCAN(LineData,',')
	    BufferString=ADJUSTL(LineData(I+1:150))
	    READ(BufferString,*)Super
	  
	    READ(200,*)

	END IF

  END IF

  !***************** Outdoor coil data *****************
  !DO I=1,3; READ(200,*); END DO

  !Fin type (1-smooth; 2-Wavy; 3-louvered)
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_FinType !ODC_PAR(29) 

  READ(200,*)
    
  !Fin pitch, fin/m or fin/in
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_FinPitch !ODC_PAR(22) 

  !Fin thermal conductivity, kW/m-K or Btu-in/hr-ft2-F
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_Kfin !ODC_PAR(23)
  !IF (Unit .EQ. 1) ODC_Kfin=ODC_Kfin/1000 !ISI - 07/14/06
  IF (Unit .EQ. SI) ODC_Kfin=ODC_Kfin/1000 

  !Fin thickness, mm or mil  
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_FinThk !ODC_PAR(21)

  READ(200,*)

  READ(200,202)LineData
  I=SCAN(LineData,',')
  LineData=ADJUSTL(LineData(I+1:150))
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_TubeType

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)InsideDiameter

  !Tube outside diameter, mm or in
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_TubeOD !ODC_PAR(15) 

  !Tube wall thickness, mm or mil
  ODC_TubeThk=(ODC_TubeOD-InsideDiameter)/2
  !IF (Unit .EQ. 2) ODC_TubeThk=ODC_TubeThk*1000 !ISI - 07/14/06
  IF (Unit .EQ. IP) ODC_TubeThk=ODC_TubeThk*1000 

  !Tube thermal conductivity, kW/m-K or Btu-in/hr-ft^2-F
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_Ktube !ODC_PAR(18) 
  !IF (Unit .EQ. 1) ODC_Ktube=ODC_Ktube/1000 !ISI - 07/14/06 
  IF (Unit .EQ. SI) ODC_Ktube=ODC_Ktube/1000 

  !Lateral tube spacing, mm or in
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_Pl 

  !Vertical tube spacing, mm or in
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_Pt 

  !Number of rows
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_Nl 

  !Number of tubes per row
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_Nt 

  !Number of circuits
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_Nckt 

  !Number of modules per tube   
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_Nmod 

  !Tube length, m or in 
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_Ltube 
  !IF (Unit .EQ. 1) ODC_Ltube=ODC_Ltube/1000 !ISI - 07/14/06
  IF (Unit .EQ. SI) ODC_Ltube=ODC_Ltube/1000

  DO I=1,8; READ(200,*); END DO !skip over Internal Volume of new file format

  !Ref side heat transfer multiplier
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_hciMultiplier 

  !Ref side pressure drop multiplier
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_DPrefMultiplier 

  !Air side heat transfer multiplier
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_hcoMultiplier 

  !Air side pressure drop multiplier
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_DPairMultiplier

  READ(200,*) !skip over air leakage around the coil in new file format

  READ(200,*)

  !***************** Outdoor fan data *****************
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)PwrODfan 

  !Outdoor fan inlet air flow rate, m3/s or CFM
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)VdotODfan 

  !READ(200,*)

    !This is reading the ***INDOOR COIL DATA*** line, not the actual drawthrough/blowthrough line
    !It only works because there is no assertion on the input quality
  READ(200,202)LineData

  ODdrawBlow=1 !Default
  SELECT CASE (LineData(1:1))
  CASE ('D','d') !To include fan location for outdoor fan, ISI - 12/07/07
      !Fan location, 1=draw through, 2=blow through
      I=SCAN(LineData,',')
      BufferString=ADJUSTL(LineData(I+1:150))
      READ(BufferString,*)ODdrawBlow
      READ(200,*)
  END SELECT

    !Then we don't end up reading the separator *** line here, we just start with the next line

  !***************** Indoor coil data *****************  
  !Fin type (1-smooth; 2-Wavy; 3-louvered)
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_FinType 
  
  READ(200,*)
  
  !Fin pitch, fin/m or fin/in
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_FinPitch

  !Fin thermal conductivity, kW/m-K or Btu-in/hr-ft2-F
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_Kfin 
  !IF (Unit .EQ. 1) IDC_Kfin=IDC_Kfin/1000 !ISI - 07/14/06
  IF (Unit .EQ. SI) IDC_Kfin=IDC_Kfin/1000

  !Fin thickness, mm or mil
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_FinThk

  READ(200,*)

  READ(200,202)LineData
  I=SCAN(LineData,',')
  LineData=ADJUSTL(LineData(I+1:150))
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_TubeType

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)InsideDiameter

  !Tube outside diameter, mm or in
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_TubeOD 

  !Tube wall thickness, mm or mil
  IDC_TubeThk=(IDC_TubeOD-InsideDiameter)/2
  !IF (Unit .EQ. 2) IDC_TubeThk=IDC_TubeThk*1000  !ISI - 07/14/06
  IF (Unit .EQ. IP) IDC_TubeThk=IDC_TubeThk*1000 

  !Tube thermal conductivity, kW/m-K or Btu-in/hr-ft^2-F  
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_Ktube 
  !IF (Unit .EQ. 1) IDC_Ktube=IDC_Ktube/1000 !ISI - 07/14/06 
  IF (Unit .EQ. SI) IDC_Ktube=IDC_Ktube/1000 

  !Lateral tube spacing, mm or in
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_Pl 

  !Vertical tube spacing, mm or in
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_Pt 

  !Number of rows
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_Nl 

  !Number of tubes per row
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_Nt 

  !Number of circuits
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_Nckt 

  !Number of modules per tube
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_Nmod 

  !Tube length, m or in
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_Ltube 
  !IF (Unit .EQ. 1) IDC_Ltube=IDC_Ltube/1000 !ISI - 07/14/06
  IF (Unit .EQ. SI) IDC_Ltube=IDC_Ltube/1000

  DO I=1,8; READ(200,*); END DO !skip over internal volume in new file format

  !Ref side heat transfer multiplier
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_hciMultiplier 

  !Ref side pressure drop multiplier
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_DPrefMultiplier 

  !Air side heat transfer multiplier
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_hcoMultiplier 

  !Air side pressure drop multiplier
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_DPairMultiplier 

  READ(200,*) !skip air leakage around coil entry in new file format

  READ(200,*)

  !***************** Indoor fan data *****************
  !Indoor Fan power
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)PwrIDfan

  !Indoor coil inlet air flow rate, m3/s or CFM
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)VdotIDfan 

  !Fan location, 1=draw through, 2=blow through
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDdrawBlow
  READ(200,*)

  !***************** Expansion device data *****************
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  SELECT CASE (BufferString(1:1))
  CASE ('C','c')
    CoolingExpDevice=3 !Cap. tube
  CASE ('T','t')
    CoolingExpDevice=2 !TXV
  CASE DEFAULT
    CoolingExpDevice=1 !Short tube orifice
  END SELECT

  READ(200,202)LineData
  
  IF (LineData(1:31) .EQ. 'Expansion device type (heating)') THEN !New format 12/10/2008
      
      I=SCAN(LineData,',')
      BufferString=ADJUSTL(LineData(I+1:150))
      SELECT CASE (BufferString(1:1))
      CASE ('C','c')
        HeatingExpDevice=3 !Cap. tube
      CASE ('T','t')
        HeatingExpDevice=2 !TXV
      CASE DEFAULT
        HeatingExpDevice=1 !Short tube orifice
      END SELECT
  
      READ(200,*)
      
  END IF
  
  !Short tube orifice

  !Cooling mode
  !READ(200,*)

  !Length, mm or in
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)CoolingShTbPAR(1) 

  !Diameter, mm or mil
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)CoolingShTbPAR(2) 

  !45 deg chamfer depth, mm or in
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)CoolingShTbPAR(3) 

  !Heating mode

  !Length, mm or in
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)HeatingShTbPAR(1) 

  !Diameter, mm or mil
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)HeatingShTbPAR(2) 

  !45 deg chamfer depth, mm or in
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)HeatingShTbPAR(3) 

  READ(200,202)LineData
  IF (LineData(1:22) .EQ. '--- Capillary Tube ---') THEN !New format for capillary tube, 04/13/2009 - ISI

      !Length, mm or in
      READ(200,202)LineData
      I=SCAN(LineData,',')
      BufferString=ADJUSTL(LineData(I+1:150))
      READ(BufferString,*)CoolingCapTubePAR(2)

      !Diameter, mm or mil
      READ(200,202)LineData
      I=SCAN(LineData,',')
      BufferString=ADJUSTL(LineData(I+1:150))
      READ(BufferString,*)CoolingCapTubePAR(1)

      !Coil length, mm or in
      READ(200,202)LineData
      I=SCAN(LineData,',')
      BufferString=ADJUSTL(LineData(I+1:150))
      READ(BufferString,*)CoolingCapTubePAR(3)

      !Length, mm or in
      READ(200,202)LineData
      I=SCAN(LineData,',')
      BufferString=ADJUSTL(LineData(I+1:150))
      READ(BufferString,*)HeatingCapTubePAR(2)

      !Diameter, mm or mil
      READ(200,202)LineData
      I=SCAN(LineData,',')
      BufferString=ADJUSTL(LineData(I+1:150))
      READ(BufferString,*)HeatingCapTubePAR(1)

      !Coil length, mm or in
      READ(200,202)LineData
      I=SCAN(LineData,',')
      BufferString=ADJUSTL(LineData(I+1:150))
      READ(BufferString,*)HeatingCapTubePAR(3)
    
      READ(200,202)LineData
  END IF

  !TXV data
  !DO I=1,2; READ(200,*); END DO
  READ(200,202)LineData !TXV Model

  !Rated TXV capacity, ton
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)CoolingTXVcapacity !TxvPAR(1) 
  
  READ(200,202)LineData
  
  IF (LineData(1:15) .EQ. 'Model (heating)') THEN !New format 12/10/2008
      READ(200,202)LineData
      I=SCAN(LineData,',')
      BufferString=ADJUSTL(LineData(I+1:150))
      READ(BufferString,*)HeatingTXVcapacity 
  ELSE

      !Static superheat, C or F
    !  READ(200,202) !LineData
    !  I=SCAN(LineData,',')
    !  BufferString=ADJUSTL(LineData(I+1:150))
    !  READ(BufferString,*)TxvPAR(3) 

      !Maximum effective superheat, C or F
      !READ(200,*)
      READ(200,202) !LineData
    !  I=SCAN(LineData,',')
    !  BufferString=ADJUSTL(LineData(I+1:150))
    !  READ(BufferString,*)TxvPAR(7) 

      !Rated superheat, C or F
      READ(200,202) !LineData
    !  I=SCAN(LineData,',')
    !  BufferString=ADJUSTL(LineData(I+1:150))
    !  READ(BufferString,*)TxvPAR(2) 
     
      !Bleed factor
      READ(200,202) !LineData
    !  I=SCAN(LineData,',')
    !  BufferString=ADJUSTL(LineData(I+1:150))
    !  READ(BufferString,*)TxvPAR(4) 
  
  END IF

  !Distributor tubes
  READ(200,*)

  !Distributor tube length, mm or ft
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)CoolingDistubeLength
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

  !Distributor tube length, mm or ft
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)HeatingDistubeLength  
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

  !Refrigerant line data
  DO I=1,3; READ(200,*); END DO

  !Suction line length, m or ft
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)SucLnPAR(1) 

  !Suction line elevation, m or ft
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)SucLnPAR(4) 

  !Suction line heat gain, W or Btu/hr  
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)SucLnPAR(5) 

  !Suction line temperature rise, C or F
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)SucLnPAR(6) 

  READ(200,*)

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)InsideDiameter

  !Suction line outside diameter, mm or in
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)SucLnPAR(2) 

  !Suction line tube wall thickness, mm or mil
  SucLnPAR(3)=(SucLnPAR(2)-InsideDiameter)/2
  !IF (Unit .EQ. 2) SucLnPAR(3)=SucLnPAR(3)*1000 !ISI - 07/14/06 
  IF (Unit .EQ. IP) SucLnPAR(3)=SucLnPAR(3)*1000 

  !Suction line additional pressure drop
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)SucLnPAR(7) 

  DO I=1,3; READ(200,*); END DO

  !Discharge line length, m or ft
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)DisLnPAR(1) 

  !Discharge line elevation, m or ft
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)DisLnPAR(4) 

  !Discharge line heat loss, W or Btu/hr  
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)DisLnPAR(5) 

  !Discharge line temperature change, C or F
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)DisLnPAR(6) 

  READ(200,*)

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)InsideDiameter

  !Discharge line outside diameter, mm or in
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)DisLnPAR(2) 

  !Discharge line tube wall thickness, mm or mil
  DisLnPAR(3)=(DisLnPAR(2)-InsideDiameter)/2
  !IF (Unit .EQ. 2) DisLnPAR(3)=DisLnPAR(3)*1000 !ISI - 07/14/06 
  IF (Unit .EQ. IP) DisLnPAR(3)=DisLnPAR(3)*1000 

  !Discharge line additional pressure drop
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)DisLnPAR(7) 

  DO I=1,3; READ(200,*); END DO

  !Liquid line length, m or ft
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)LiqLnPAR(1) 

  !Liquid line elevation, m or ft
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)LiqLnPAR(4) 

  !Liquid line heat loss, W or Btu/hr  
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)LiqLnPAR(5) 

  !Liquid line temperature change, C or F
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)LiqLnPAR(6) 

  READ(200,*)

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)InsideDiameter

  !Liquid line outside diameter, mm or in
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)LiqLnPAR(2) 

  !Liquid line tube wall thickness, mm or mil
  LiqLnPAR(3)=(LiqLnPAR(2)-InsideDiameter)/2
  !IF (Unit .EQ. 2)LiqLnPAR(3)=LiqLnPAR(3)*1000 !ISI - 07/14/06 
  IF (Unit .EQ. IP)LiqLnPAR(3)=LiqLnPAR(3)*1000  

  !Liquid line additional pressure drop
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)LiqLnPAR(7) 

  DO I=1,3; READ(200,*); END DO

  !Valve to IDC line length, m or ft
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ValveIDCLnPAR(1)

  !Valve to IDC line elevation, m or ft
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ValveIDCLnPAR(4)

  !Valve to IDC line heat loss, W or Btu/hr  
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ValveIDCLnPAR(5)

  !Valve to IDC line temperature change, C or F
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ValveIDCLnPAR(6)

  READ(200,*)

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)InsideDiameter

  !Valve to IDC line outside diameter, mm or in
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ValveIDCLnPAR(2)

  !Valve to IDC line tube wall thickness, mm or mil
  ValveIDCLnPAR(3)=(ValveIDCLnPAR(2)-InsideDiameter)/2
  !IF (Unit .EQ. 2)ValveIDCLnPAR(3)=ValveIDCLnPAR(3)*1000 !ISI - 07/14/06
  IF (Unit .EQ. IP)ValveIDCLnPAR(3)=ValveIDCLnPAR(3)*1000

  !Valve to IDC line additional pressure drop
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ValveIDCLnPAR(7) 

  DO I=1,3; READ(200,*); END DO

  !Valve to ODC line length, m or ft
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ValveODCLnPAR(1)

  !Valve to ODC line elevation, m or ft
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ValveODCLnPAR(4)

  !Valve to ODC line heat loss, W or Btu/hr  
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ValveODCLnPAR(5)

  !Valve to ODC line temperature change, C or F
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ValveODCLnPAR(6)

  READ(200,*)

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)InsideDiameter

  !Valve to ODC line outside diameter, mm or in
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ValveODCLnPAR(2)

  !Valve to ODC line tube wall thickness, mm or mil
  ValveODCLnPAR(3)=(ValveODCLnPAR(2)-InsideDiameter)/2
  !IF (Unit .EQ. 2)ValveODCLnPAR(3)=ValveODCLnPAR(3)*1000 !ISI - 07/14/06
  IF (Unit .EQ. IP)ValveODCLnPAR(3)=ValveODCLnPAR(3)*1000

  !Valve to ODC line additional pressure drop
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ValveODCLnPAR(7) 

  DO I=1,34; READ(200,*); END DO

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

  !***** Accumulator *****
  DO I=1,7; READ(200,*); END DO

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)AccumPAR(2) !Height, mm or in

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)AccumPAR(1) !Diameter, mm or in

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)AccumPAR(4) !Upper hole diameter, mm or in

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)AccumPAR(3) !Lower hole diameter, mm or in

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)AccumPAR(5) !Hole distance, mm or in

  AccumPAR(6)=(SucLnPAR(2)-SucLnPAR(3)/1000*2) !J-tube diameter, mm or in
  	
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)AccumPAR(7) !Rating DP, kPa or psi

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)AccumPAR(8) !Rating DT, C or F

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)AccumPAR(9) !Coefficient M

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)AccumPAR(10) !Coefficient B

  !***** Filter Drier *****
  DO I=1,3; READ(200,*); END DO

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)FilterPAR(1) !Flow capacity, ton

  READ(200,*)

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)FilterPAR(2) !Rating DP, kPa or psi

  !***** Custom air side data *****
  DO I=1,60; READ(200,*); END DO

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_CurveUnit

  READ(200,*) !Heat Transfer data

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_CurveTypeHTC

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_PowerAHTC

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_PowerBHTC

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_Poly1HTC
  
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_Poly2HTC

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_Poly3HTC

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_Poly4HTC

  READ(200,*) !Pressure drop data

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_CurveTypeDP

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_PowerADP

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_PowerBDP

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_Poly1DP
  
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_Poly2DP

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_Poly3DP

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)IDC_Poly4DP

  READ(200,*) !Outdoor Coil

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_CurveUnit

  READ(200,*) !Heat Transfer data

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_CurveTypeHTC

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_PowerAHTC

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_PowerBHTC

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_Poly1HTC
  
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_Poly2HTC

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_Poly3HTC

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_Poly4HTC

  READ(200,*) !Pressure drop data

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_CurveTypeDP

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_PowerADP

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_PowerBDP

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_Poly1DP
  
  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_Poly2DP

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_Poly3DP

  READ(200,202)LineData
  I=SCAN(LineData,',')
  BufferString=ADJUSTL(LineData(I+1:150))
  READ(BufferString,*)ODC_Poly4DP

  READ(200,202)LineData
  IF (LineData(1:37) .EQ. ' ***************************** Charge') THEN !New format 02/07/07

    IsChargeTuning=0
	IF (MODE .NE. 2) THEN

	    READ(200,202)LineData
	    I=SCAN(LineData,',')
	    BufferString=ADJUSTL(LineData(I+1:150))
	    SELECT CASE (BufferString(1:1))
	    CASE ('F','f')
		  IsChargeTuning=0
	    CASE DEFAULT
		  IsChargeTuning=1
	    END SELECT

		READ(200,202)LineData
		I=SCAN(LineData,',')
		BufferString=ADJUSTL(LineData(I+1:150))
		READ(BufferString,*)RefSimulatedCharge
		  
		READ(200,202)LineData
		I=SCAN(LineData,',')
		BufferString=ADJUSTL(LineData(I+1:150))
		READ(BufferString,*)RefLiquidLength

		READ(200,202)LineData
		I=SCAN(LineData,',')
		BufferString=ADJUSTL(LineData(I+1:150))
		READ(BufferString,*)SimulatedCharge2

		READ(200,202)LineData
		I=SCAN(LineData,',')
		BufferString=ADJUSTL(LineData(I+1:150))
		READ(BufferString,*)LiquidLength2
	END IF

  END IF

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
