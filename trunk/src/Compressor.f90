! ************************************** !
! ** HEAT PUMP SIMULATION CODE HEADER ** !
! ************************************** !

! ************************************** !
! -- HIGH LEVEL OVERVIEW/DESCRIPTION --- !
! -------------------------------------- !
! This module models a compressor.

! ************************************** !
! -- PHYSICAL DESCRIPTION -------------- !
! -------------------------------------- !
! This component represents an ARI compressor by use of a 2nd order polynomial equation fit.
! A description of the component is found at:
! http://www.e-refrigeration.com/learn-refrigeration/system-components/compressors
! From that website: 
!  - A compressor compresses and cycles refrigerant, while also increasing the pressure.

! ************************************** !
! -- SIMULATION DATA RESPONSIBILITIES -- !
! -------------------------------------- !
! This module reads in entering refrigerant and compressor properties, calculates, and returns exiting refrigerant properties and power.

! ************************************** !
! -- INPUT FILES/OUTPUT FILES (none) --- !
! -------------------------------------- !
! There are no input or output files directly connected to this module.

! ************************************** !
! -- MODULE LEVEL VARIABLES/STRUCTURES - !
! -------------------------------------- !
! DataSimulation is called in; otherwise, nothing is defined at the module level.

! ************************************** !
! -- SUMMARY OF METHODS, CALL TREE ----- !
! -------------------------------------- !
! This module contains 2 methods:
!    PUBLIC Compressor -- Models a 2nd order polynomail equation fit compressor model
!      Called by ORNLSolver.f90
!      Called by FlowRateLoop.f90
!    PRIVATE X -- A 10 coefficient polynomial equation calculation
!      Called by Compressor

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
! Some more documentation or narrative might be useful.

    MODULE CompressorMod

    USE DataSimulation
    implicit none

    PUBLIC  Compressor
    PRIVATE X

    CONTAINS

    SUBROUTINE Compressor(Ref$,PureRef,XIN,PAR,OUT)

    ! ----------------------------------------------------------------------
    !
    !   Description: ARI Compressor model
    !
    !   Method: 2nd order polynomial equation fit 
    !
    !   Inputs:
    !       Ref$=Refrigerant name
    !       PureRef=Refrigerant flag: 1=pure refrigerant
    !                                 0=refrigerant mixture
    !       XIN(1) = Suction refrigerant pressure, kPa
    !       XIN(2) = Discharge refrigerant pressure, kPa
    !       XIN(3) = Suction enthalpy, kJ/kg
    !
    !       PAR(1..10)  = Coefficients for power calc.
    !       PAR(11..20) = Coefficients for mass flow rate calc.
    !       PAR(21) = Shell heat loss fraction of power consumption
    !       PAR(22) = Shell heat loss W
    !       PAR(23) = Internal void volume, m^3
    !       PAR(24) = Power Correction, 1 - 230 VAC; 2 - 208 VAC
    !       PAR(25) = Power multiplier
    !       PAR(26) = Mass flow rate multiplier
    !
    !   Outputs:
    !       OUT(1) = Power, KW
    !       OUT(2) = Mass flow rate, kg/s
    !       OUT(3) = Discharge enthalpy, kJ/kg
    !       OUT(4) = Discharge quality
    !       OUT(5) = Discharge temperature, C
    !       OUT(6) = Mass in compressor, kg
    !       OUT(7) = Error flag: 0-No error
    !                            1-Compressor solution error
    !                            2-Refprop error
    !
    !   Author:
    !   Ipseng Iu
    !   Mechanical and Aerospace Engineering
    !   Oklahoma State University, Stillwater
    !
    !   Date: July 2003
    !
    ! ----------------------------------------------------------------------

    USE FluidProperties_HPSim
    USE DataGlobals_HPSim, ONLY: RefrigIndex   !RS: Debugging: Removal of plethora of RefrigIndex definitions in the code
    USE InputProcessor_HPSim    !RS: Debugging: Brought over from GetHPSimInputs
    USE DataGlobals_HPSim, ONLY: MaxNameLength, RefName    !RS Comment: Needs to be used for implementation with Energy+ currently (7/23/12)

    IMPLICIT NONE

    !Subroutine argument declarations
    CHARACTER*80,     INTENT(IN) :: Ref$    !Refrigerant name
    INTEGER(2),       INTENT(IN) :: PureRef !Refrigerant flag: 1-pure refrigerant
    !0-refrigerant mixture

    REAL, INTENT(IN) :: XIN(3)
    REAL :: PAR(26) !, INTENT(IN)
    REAL, INTENT(OUT) :: OUT(7)

    REAL, PARAMETER :: Fv=0.75

    !Subroutine local variables          
    !INTEGER            :: RefrigIndex =0
    REAL Temperature,Quality,Pressure,Enthalpy,Entropy

    REAL A(10),B(10)
    REAL TDPsuc      !Suction dew point temperature, C
    REAL TDPdis      !Discharge dew point temperature, C
    REAL TDPsucF     !Suction dew point temperature, F
    REAL TDPdisF     !Discharge dew point temperature, F
    REAL Power       !Compressor power consumption, KW
    REAL PowerMap    !Map based Compressor power consumption, KW
    REAL mdot        !Refrigerant mass flow rate, kg/s
    REAL mdotMap     !Map based Refrigerant mass flow rate, kg/s
    REAL Tsuc        !Suction temp, C
    REAL TsucMap     !Map based suction temp, C
    REAL Tdis        !Discharge temperature, C
    REAL Psuc        !Suction pressure, kPa
    REAL Pdis        !Discharge pressure, kPa
    REAL Xdis        !Discharge quality
    REAL Hsuc        !Suction enthalpy, kJ/kg
    REAL Hdis        !Discharge enthalpy, kJ/kg
    REAL Qshellfrac  !Compressor shell heat loss fraction of power consumption
    REAL Qshell      !Compressor shell heat loss, kW
    REAL VolCmp      !Compressor internal volume, m3
    REAL MassCmp     !Refrigerant mass in compressor, kg
    REAL rhoDis      !Discharge density, kg/m3
    REAL rhoSuc      !Suction density, kg/m3
    REAL rhoMap      !Map based density value, kg/m3
    REAL Ssuc        !Suction entropy
    REAL SsucMap     !Map based entropy value
    REAL HdisIsen    !Isentropic discharge enthalpy, kJ/kg
    REAL HdisIsenMap !Map based insentropic discharge enthalpy, kJ/kg
    REAL HsucMap     !Map based suction enthalpy, kJ/kg
    REAL Wcorrect    !Correction factor for power calc. with different input voltage
    REAL Mcorrect    !Correction factor for mass flow rate
    REAL PwrMultiplier    !Power multiplier
    REAL mdotMultiplier   !Mass flow rate multiplier
    INTEGER I !Loop control
    INTEGER ErrorFlag          !0-No error
    !1-Compressor solution error
    !2-Refprop error

    INTEGER(2) RefPropErr  !Error flag:1-error; 0-no error
    LOGICAL, EXTERNAL :: IssueRefPropError
    
    !RS: Debugging: Bringing this over from GetHPSimInputs
    CHARACTER(len=MaxNameLength),DIMENSION(200) :: Alphas ! Reads string value from input file
    INTEGER :: NumAlphas               ! States which alpha value to read from a "Number" line
    REAL, DIMENSION(200) :: Numbers    ! brings in data from IP
    INTEGER :: NumNumbers              ! States which number value to read from a "Numbers" line
    INTEGER :: Status                  ! Either 1 "object found" or -1 "not found"
    INTEGER CompressorManufacturer
    REAL, DIMENSION(200) :: TmpNumbers !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)
    
    !Compressor Manufacturer
INTEGER,PARAMETER :: COPELAND  = 1 !ISI - 10/05/06
INTEGER,PARAMETER :: BRISTOL   = 2
INTEGER,PARAMETER :: DANFOSS   = 3
INTEGER,PARAMETER :: PANASONIC = 4
    
    !RS: Debugging: Bringing this over from GetHPSimInputs
      !***************** Compressor data *****************  !RS: Debugging: Moving: Compressor

  CALL GetObjectItem('CompressorData',1,Alphas,NumAlphas, &
                      TmpNumbers,NumNumbers,Status)
  Numbers = DBLE(TmpNumbers) !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)
  
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

  PAR(21) = Numbers(1) !CompressorHeatLossFraction
  PAR(22) = Numbers(2) !CompressorHeatLoss
  PAR(23) = Numbers(3) !CompressorVolume
  PAR(11) = Numbers(4) !CompressorMassCoefficient1
  PAR(12) = Numbers(5) !CompressorMassCoefficient2
  PAR(13) = Numbers(6) !CompressorMassCoefficient3
  PAR(14) = Numbers(7) !CompressorMassCoefficient4
  PAR(15) = Numbers(8) !CompressorMassCoefficient5
  PAR(16) = Numbers(9) !CompressorMassCoefficient6
  PAR(17) = Numbers(10) !CompressorMassCoefficient7
  PAR(18) = Numbers(11) !CompressorMassCoefficient8
  PAR(19) = Numbers(12) !CompressorMassCoefficient9
  PAR(20) = Numbers(13) !CompressorMassCoefficient10
  PAR(1) = Numbers(14) !CompressorPowerCoefficient1
  PAR(2) = Numbers(15) !CompressorPowerCoefficient2
  PAR(3) = Numbers(16) !CompressorPowerCoefficient3
  PAR(4) = Numbers(17) !CompressorPowerCoefficient4
  PAR(5) = Numbers(18) !CompressorPowerCoefficient5
  PAR(6) = Numbers(19) !CompressorPowerCoefficient6
  PAR(7) = Numbers(20) !CompressorPowerCoefficient7
  PAR(8) = Numbers(21) !CompressorPowerCoefficient8
  PAR(9) = Numbers(22) !CompressorPowerCoefficient9
  PAR(10) = Numbers(23) !CompressorPowerCoefficient10
  
  PAR(25) = Numbers(24) !PowerMultiplier
  PAR(26) = Numbers(25) !MassFlowRateMultiplier
  !TsiCmp = Numbers(26) !UserSpecifiedRatingEvapTemperature
  !TsoCmp = Numbers(27) !UserSpecifiedRatingCondTemperature
  !Subcool = Numbers(28) !UserSpecifiedRatingSubcooling
  !Super = Numbers(29) !UserSpecifiedRatingSuperheat
  !_________

    !Flow:

    Psuc = XIN(1)
    Pdis = XIN(2)
    Hsuc = XIN(3)

    DO I=1,10
        A(I)=PAR(I)
        B(I)=PAR(I+10) 
    END DO
    
    Qshellfrac = PAR(21)
    Qshell = PAR(22)
    VolCmp = PAR(23)
    Wcorrect = PAR(24)
    PwrMultiplier=PAR(25)
    mdotMultiplier=PAR(26)

    Wcorrect = 1 !1.21 !1.25

    ErrorFlag=0 !Initialize

    Pressure=Psuc*1000  !RS Comment: Unit Conversion
    Quality=1
    TDPsuc=PQ(Ref$,Pressure,Quality,'temperature',RefrigIndex,RefPropErr)   !Suction Dew Point Temperature, C
    IF (IssueRefPropError(RefPropErr, 'Compressor', 2, ErrorFlag, OUT(7))) THEN
        RETURN
    END IF

    Pressure=Pdis*1000  !RS Comment: Unit Conversion
    Quality=1
    TDPdis=PQ(Ref$,Pressure,Quality,'temperature',RefrigIndex,RefPropErr)   !Discharge Dew Point Temperature, C
    IF (IssueRefPropError(RefPropErr, 'Compressor', 2, ErrorFlag, OUT(7))) THEN
        RETURN
    END IF

    TDPsucF=TDPsuc*1.8+32   !Suction Dew Point Temperature, F
    TDPdisF=TDPdis*1.8+32   !Discharge Dew Point Temperature, F

    PowerMap=X(A,TDPdisF,TDPsucF)/1000      !RS Comment: Unit Conversion
    mdotMap=X(B,TDPdisF,TDPsucF)*0.454/3600 !RS Comment: Unit Conversion?

    TsucMap=((TDPsucF+20)-32)*5/9 !20-rated superheat

    Temperature=TsucMap
    Pressure=Psuc*1000  !RS Comment: Unit Conversion
    HsucMap=TP(Ref$,Temperature,Pressure,'enthalpy',RefrigIndex,RefPropErr) !Map-Based Suction Enthalpy
    IF (IssueRefPropError(RefPropErr, 'Compressor', 2, ErrorFlag, OUT(7))) THEN
        RETURN
    END IF

    HsucMap=HsucMap/1000    !RS Comment: Unit Conversion
    rhoMap=TP(Ref$,Temperature,Pressure,'density',RefrigIndex,RefPropErr)   !Map-Based Density
    IF (IssueRefPropError(RefPropErr, 'Compressor', 2, ErrorFlag, OUT(7))) THEN
        RETURN
    END IF

    SsucMap=TP(Ref$,Temperature,Pressure,'entropy',RefrigIndex,RefPropErr)  !Map-Based Suction Entropy
    IF (IssueRefPropError(RefPropErr, 'Compressor', 2, ErrorFlag, OUT(7))) THEN
        RETURN
    END IF
    SsucMap=SsucMap/1000    !RS Comment: Unit Conversion

    Pressure=Pdis*1000      !RS Comment: Unit Conversion
    Entropy=SsucMap*1000    !RS Comment: Unit Conversion
    HdisIsenMap=PS(Ref$,Pressure,Entropy,'enthalpy',RefrigIndex,RefPropErr) !Map-Based Isentropic Discharge Enthalpy
    IF (IssueRefPropError(RefPropErr, 'Compressor', 2, ErrorFlag, OUT(7))) THEN
        RETURN
    END IF
    HdisIsenMap=HdisIsenMap/1000    !RS Comment: Unit Conversion

    Pressure=Psuc*1000  !RS Comment: Unit Conversion
    Enthalpy=Hsuc*1000  !RS Comment: Unit Conversion
    Tsuc=PH(Ref$,Pressure,Enthalpy,'temperature',RefrigIndex,RefPropErr)    !Suction Temperature
    IF (IssueRefPropError(RefPropErr, 'Compressor', 2, ErrorFlag, OUT(7))) THEN
        RETURN
    END IF

    rhosuc=PH(Ref$,Pressure,Enthalpy,'density',RefrigIndex,RefPropErr)  !Suction Density
    IF (IssueRefPropError(RefPropErr, 'Compressor', 2, ErrorFlag, OUT(7))) THEN
        RETURN
    END IF

    Ssuc=PH(Ref$,Pressure,Enthalpy,'entropy',RefrigIndex,RefPropErr)    !Suction Entropy
    IF (IssueRefPropError(RefPropErr, 'Compressor', 2, ErrorFlag, OUT(7))) THEN
        RETURN
    END IF
    Ssuc=Ssuc/1000  !RS Comment: Unit Conversion

    Pressure=Pdis*1000  !RS Comment: Unit Conversion
    Entropy=Ssuc*1000   !RS Comment: Unit Conversion
    HdisIsen=PS(Ref$,Pressure,Entropy,'enthalpy',RefrigIndex,RefPropErr)    !Isentropic Discharge Enthalpy
    IF (IssueRefPropError(RefPropErr, 'Compressor', 2, ErrorFlag, OUT(7))) THEN
        RETURN
    END IF
    HdisIsen=HdisIsen/1000  !RS Comment: Unit Conversion

    mdot=mdotMap*(rhosuc/rhoMap)

    Mcorrect=1 !0.95
    mdot=mdot*Mcorrect

    Power=PowerMap*(mdot/mdotMap)*(HdisIsen-Hsuc)/(HdisIsenMap-HsucMap)

    Power=Power/Wcorrect

    mdot=mdot*mdotMultiplier
    Power=Power*PwrMultiplier

    IF (Qshellfrac .NE. 0) THEN
        Qshell = Qshellfrac * Power !Fraction of power input
    ELSE
        Qshell = Qshell * 1E-3  !Convert to kW
    END IF
    Hdis=(Power-Qshell)/mdot+Hsuc

    Pressure=Pdis*1000  !RS Comment: Unit Conversion
    Enthalpy=Hdis*1000  !RS Comment: Unit Conversion
    Tdis=PH(Ref$,Pressure,Enthalpy,'temperature',RefrigIndex,RefPropErr)    !Discharge Temperature
    IF (IssueRefPropError(RefPropErr, 'Compressor', 2, ErrorFlag, OUT(7))) THEN
            RETURN
    END IF

    Xdis=PH(Ref$,Pressure,Enthalpy,'quality',RefrigIndex,RefPropErr)    !Discharge Quality
    IF (IssueRefPropError(RefPropErr, 'Compressor', 2, ErrorFlag, OUT(7))) THEN
        RETURN
    END IF

    rhoDis=PH(Ref$,Pressure,Enthalpy,'density',RefrigIndex,RefPropErr)  !Discharge Density
    IF (IssueRefPropError(RefPropErr, 'Compressor', 2, ErrorFlag, OUT(7))) THEN
        RETURN
    END IF

    MassCmp=VolCmp*(rhoDis+rhoSuc)/2

    IF (Power .LT. 0 .OR. MassCmp .LT. 0) THEN
        ErrorFlag=1
        OUT(7)=ErrorFlag
        RETURN
    END IF

    OUT(1)=Power
    OUT(2)=mdot
    OUT(3)=Hdis
    OUT(4)=Xdis
    OUT(5)=Tdis
    OUT(6)=MassCmp
    OUT(7)=ErrorFlag

    RETURN

    END SUBROUTINE Compressor

    !***********************************************************************

    REAL FUNCTION X(C,D,S)
    implicit none

    REAL C(10) !Coefficients
    REAL D !Discharge dew point temperature
    REAL S !Suction dew point temperature

    X=C(1)+C(2)*S+C(3)*D+C(4)*S**2+C(5)*(S*D)+C(6)*D**2+ &
    C(7)*S**3+C(8)*D*S**2+C(9)*S*D**2+C(10)*D**3

    END FUNCTION

    !***********************************************************************

    END MODULE CompressorMod
