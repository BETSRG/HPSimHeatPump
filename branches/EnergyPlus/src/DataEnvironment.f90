MODULE DataEnvironment      ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Rick Strand, Dan Fisher, Linda Lawrie
          !       DATE WRITTEN   December 1997
          !       MODIFIED       November 1998, Fred Winkelmann
          !       MODIFIED       June 1999,June 2000, Linda Lawrie
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This data-only module is a repository for the variables that relate specifically
          ! to the "environment" (i.e. current date data, tomorrow's date data, and
          ! current weather variables)

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength, KelvinConv

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.


          ! MODULE PARAMETER DEFINITIONS:
REAL, PARAMETER :: EarthRadius = 6356000. ! Radius of the Earth (m)
REAL, PARAMETER :: AtmosphericTempGradient = 0.0065 ! Standard atmospheric air temperature gradient (K/m)
REAL, PARAMETER :: SunIsUpValue = .00001 ! if Cos Zenith Angle of the sun is >= this value, the sun is "up"

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
REAL :: BeamSolarRad            ! Current beam normal solar irradiance
LOGICAL   :: EMSBeamSolarRadOverrideOn = .FALSE. ! EMS flag for beam normal solar irradiance
REAL :: EMSBeamSolarRadOverrideValue ! EMS override value for beam normal solar irradiance
INTEGER   :: DayOfMonth              ! Current day of the month
INTEGER   :: DayOfMonthTomorrow      ! Tomorrow's day of the month
INTEGER   :: DayOfWeek               ! Current day of the week (Sunday=1, Monday=2, ...)
INTEGER   :: DayOfWeekTomorrow       ! Tomorrow's day of the week (Sunday=1, Monday=2, ...)
INTEGER   :: DayOfYear               ! Current day of the year (01JAN=1, 02JAN=2, ...)
INTEGER   :: DayOfYear_Schedule      ! Schedule manager always assumes leap years...
REAL :: DifSolarRad             ! Current sky diffuse solar horizontal irradiance
LOGICAL   :: EMSDifSolarRadOverrideOn = .FALSE. ! EMS flag for sky diffuse solar horizontal irradiance
REAL :: EMSDifSolarRadOverrideValue ! EMS override value for sky diffuse solar horizontal irradiance
INTEGER   :: DSTIndicator            ! Daylight Saving Time Indicator (1=yes, 0=no) for Today
REAL :: Elevation               ! Elevation of this building site
LOGICAL   :: EndMonthFlag            ! Set to true on last day of month
REAL :: GndReflectanceForDayltg ! Ground visible reflectance for use in daylighting calc
REAL :: GndReflectance          ! Ground visible reflectance from input
REAL :: GndSolarRad             ! Current ground reflected radiation
REAL :: GroundTemp              ! Current ground temperature {C}
REAL :: GroundTempKelvin        ! Current ground temperature {K}
REAL :: GroundTempFC            ! Current ground temperature defined for F or C factor method {C}
REAL :: GroundTemp_Surface      ! Current surface ground temperature {C}
REAL :: GroundTemp_Deep         ! Current deep ground temperature
REAL, DIMENSION(12)    :: PubGroundTempSurface ! All 12 Surf Gnd Temps (assigned in Weather Mgr, used in PlantPipeHeatTransfer)
LOGICAL :: PubGroundTempSurfFlag   ! Flag for if Surf Ground Temps Exist in idf  (assigned, used same as PubGroundTempSurface)
INTEGER   :: HolidayIndex            ! Indicates whether current day is a holiday and if so what type
                                     ! HolidayIndex=(0-no holiday, 1-holiday type 1, ...)
INTEGER   :: HolidayIndexTomorrow    ! Tomorrow's Holiday Index
LOGICAL   :: IsRain                  ! Surfaces are wet for this time interval
LOGICAL   :: IsSnow                  ! Snow on the ground for this time interval
REAL :: Latitude                ! Latitude of building location
REAL :: Longitude               ! Longitude of building location
INTEGER   :: Month                   ! Current calendar month
INTEGER   :: MonthTomorrow           ! Tomorrow's calendar month
REAL :: OutBaroPress            ! Current outdoor air barometric pressure
REAL :: OutDryBulbTemp          ! Current outdoor air dry bulb temperature
LOGICAL   :: EMSOutDryBulbOverrideOn = .FALSE. ! EMS flag for outdoor air dry bulb temperature
REAL :: EMSOutDryBulbOverrideValue ! EMS override value for outdoor air dry bulb temperature
REAL :: OutHumRat               ! Current outdoor air humidity ratio
REAL :: OutRelHum               ! Current outdoor relative humidity [%]
REAL :: OutRelHumValue          ! Current outdoor relative humidity value [0.0-1.0]
LOGICAL   :: EMSOutRelHumOverrideOn = .FALSE. ! EMS flag for outdoor relative humidity value
REAL :: EMSOutRelHumOverrideValue ! EMS override value for outdoor relative humidity value
REAL :: OutEnthalpy             ! Current outdoor enthalpy
REAL :: OutAirDensity           ! Current outdoor air density
REAL :: OutWetBulbTemp          ! Current outdoor air wet bulb temperature
REAL :: OutDewPointTemp         ! Current outdoor dewpoint temperature
LOGICAL   :: EMSOutDewPointTempOverrideOn = .FALSE. ! EMS flag for outdoor dewpoint temperature
REAL :: EMSOutDewPointTempOverrideValue ! EMS override value for outdoor dewpoint temperature
REAL :: SkyTemp                 ! Current sky temperature {C}
REAL :: SkyTempKelvin           ! Current sky temperature {K}
REAL :: LiquidPrecipitation     ! Current liquid precipitation amount (rain) {mm}
LOGICAL   :: SunIsUp                 ! True when Sun is over horizon, False when not
REAL :: WindDir                 ! Current outdoor air wind direction
LOGICAL   :: EMSWindDirOverrideOn = .FALSE. ! EMS flag for outdoor air wind direction
REAL :: EMSWindDirOverrideValue ! EMS override value for outdoor air wind direction
REAL :: WindSpeed               ! Current outdoor air wind speed
LOGICAL   :: EMSWindSpeedOverrideOn = .FALSE. ! EMS flag for outdoor air wind speed
REAL :: EMSWindSpeedOverrideValue ! EMS override value for outdoor air wind speed
REAL :: WaterMainsTemp          ! Current water mains temperature
INTEGER   :: Year                    ! Current calendar year of the simulation
INTEGER   :: YearTomorrow            ! Tomorrow's calendar year of the simulation
REAL, DIMENSION(3)  :: SOLCOS   ! Solar direction cosines at current time step
REAL :: CloudFraction           ! Fraction of sky covered by clouds
REAL :: HISKF                   ! Exterior horizontal illuminance from sky (lux).
REAL :: HISUNF                  ! Exterior horizontal beam illuminance (lux)
REAL :: HISUNFnorm              ! Exterior beam normal illuminance (lux)
REAL :: PDIRLW                  ! Luminous efficacy (lum/W) of beam solar radiation
REAL :: PDIFLW                  ! Luminous efficacy (lum/W) of sky diffuse solar radiation
REAL :: SkyClearness            ! Sky clearness (see subr. DayltgLuminousEfficacy)
REAL :: SkyBrightness           ! Sky brightness (see subr. DayltgLuminousEfficacy)
REAL :: StdBaroPress =101325. ! Standard "atmospheric pressure" based on elevation (ASHRAE HOF p6.1)
REAL :: StdRhoAir               ! Standard "rho air" set in WeatherManager - based on StdBaroPress
REAL :: TimeZoneNumber          ! Time Zone Number of building location
REAL :: TimeZoneMeridian        ! Standard Meridian of TimeZone
CHARACTER(len=MaxNameLength*2) :: EnvironmentName ! Current environment name (longer for weather file names)
CHARACTER(len=20)  :: CurMnDyHr    ! Current Month/Day/Hour timestamp info
CHARACTER(len=5)   :: CurMnDy      ! Current Month/Day timestamp info
INTEGER :: CurEnvirNum             ! current environment number
INTEGER :: TotDesDays     =0       ! Total number of Design days to Setup
Integer :: TotRunDesPersDays  =0   ! Total number of Run Design Periods [Days] (Weather data) to Setup
INTEGER :: CurrentOverallSimDay    ! Count of current simulation day in total of all sim days
INTEGER :: TotalOverallSimDays     ! Count of all possible simulation days in all environments
INTEGER :: MaxNumberSimYears       ! Maximum number of simulation years requested in all RunPeriod statements
INTEGER :: RunPeriodStartDayOfWeek ! Day of week of the first day of the run period. (or design day - day of week)

REAL :: CosSolarDeclinAngle     ! Cosine of the solar declination angle
REAL :: EquationOfTime          ! Value of the equation of time formula
REAL :: SinLatitude             ! Sine of Latitude
REAL :: CosLatitude             ! Cosine of Latitude
REAL :: SinSolarDeclinAngle     ! Sine of the solar declination angle

REAL :: WeatherFileWindModCoeff = 1.5863  ! =(WindBLHeight/WindSensorHeight)**WindExp for conditions at the weather station
REAL :: WeatherFileTempModCoeff = 0.0     ! =AtmosphericTempGradient*EarthRadius*SensorHeight/(EarthRadius+SensorHeight)

REAL :: SiteWindExp = 0.22                ! Exponent for the wind velocity profile at the site
REAL :: SiteWindBLHeight = 370.           ! Boundary layer height for the wind velocity profile at the site (m)
REAL :: SiteTempGradient = 0.0065         ! Air temperature gradient coefficient (K/m)

LOGICAL :: GroundTempObjInput=.false.         ! Ground temperature object input
LOGICAL :: GroundTemp_SurfaceObjInput=.false. ! Surface ground temperature object input
LOGICAL :: GroundTemp_DeepObjInput=.false.    ! Deep ground temperature object input
LOGICAL :: FCGroundTemps=.false.
LOGICAL :: DisplayWeatherMissingDataWarnings=.false. ! Display missing/out of range weather warnings
LOGICAL :: IgnoreSolarRadiation=.false.   ! TRUE if all solar radiation is to be ignored
LOGICAL :: IgnoreBeamRadiation=.false.    ! TRUE if beam (aka direct normal) radiation is to be ignored
LOGICAL :: IgnoreDiffuseRadiation=.false. ! TRUE if diffuse horizontal radiation is to be ignored

LOGICAL :: PrintEnvrnStampWarmup=.false.
LOGICAL :: PrintEnvrnStampWarmupPrinted=.false.

LOGICAL :: RunPeriodEnvironment=.false.       ! True if Run Period, False if DesignDay
CHARACTER(len=20) :: EnvironmentStartEnd=' '  ! Start/End dates for Environment
LOGICAL :: CurrentYearIsLeapYear = .false.  ! true when current year is leap year (convoluted logic dealing with
                                            ! whether weather file allows leap years, runperiod inputs.

          ! SUBROUTINE SPECIFICATIONS FOR MODULE DataEnvironment:
PUBLIC OutDryBulbTempAt
PUBLIC OutWetBulbTempAt
PUBLIC OutDewPointTempAt
PUBLIC WindSpeedAt
PUBLIC SetOutBulbTempAt

CONTAINS

FUNCTION OutDryBulbTempAt(Z) RESULT(LocalOutDryBulbTemp)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   January 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates outdoor dry bulb temperature at a given altitude.

          ! METHODOLOGY EMPLOYED:
          ! 1976 U.S. Standard Atmosphere.

          ! REFERENCES:
          ! 1976 U.S. Standard Atmosphere. 1976. U.S. Government Printing Office, Washington, D.C.

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL, INTENT(IN) :: Z                     ! Height above ground (m)
  REAL        :: LocalOutDryBulbTemp   ! Return result for function (C)

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL :: BaseTemp                          ! Base temperature at Z = 0 (C)

  BaseTemp = OutDryBulbTemp + WeatherFileTempModCoeff

  IF (SiteTempGradient == 0.0) THEN
    LocalOutDryBulbTemp = OutDryBulbTemp
  ELSE IF (Z <= 0.0) THEN
    LocalOutDryBulbTemp = BaseTemp
  ELSE
    LocalOutDryBulbTemp = BaseTemp - SiteTempGradient * EarthRadius * Z / (EarthRadius + Z)
  END IF

  RETURN

END FUNCTION OutDryBulbTempAt

FUNCTION OutWetBulbTempAt(Z) RESULT(LocalOutWetBulbTemp)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   January 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates outdoor wet bulb temperature at a given altitude.

          ! METHODOLOGY EMPLOYED:
          ! 1976 U.S. Standard Atmosphere.

          ! REFERENCES:
          ! 1976 U.S. Standard Atmosphere. 1976. U.S. Government Printing Office, Washington, D.C.

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL, INTENT(IN) :: Z                     ! Height above ground (m)
  REAL        :: LocalOutWetBulbTemp   ! Return result for function (C)

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL :: BaseTemp                          ! Base temperature at Z = 0 (C)

  BaseTemp = OutWetBulbTemp + WeatherFileTempModCoeff

  IF (SiteTempGradient == 0.0) THEN
    LocalOutWetBulbTemp = OutWetBulbTemp
  ELSE IF (Z <= 0.0) THEN
    LocalOutWetBulbTemp = BaseTemp
  ELSE
    LocalOutWetBulbTemp = BaseTemp - SiteTempGradient * EarthRadius * Z / (EarthRadius + Z)
  END IF

  RETURN

END FUNCTION OutWetBulbTempAt

FUNCTION OutDewPointTempAt(Z) RESULT(LocalOutDewPointTemp)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates outdoor dew point temperature at a given altitude.

          ! METHODOLOGY EMPLOYED:
          ! 1976 U.S. Standard Atmosphere.
          ! copied from outwetbulbtempat

          ! REFERENCES:
          ! 1976 U.S. Standard Atmosphere. 1976. U.S. Government Printing Office, Washington, D.C.

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL, INTENT(IN) :: Z                     ! Height above ground (m)
  REAL        :: LocalOutDewPointTemp  ! Return result for function (C)

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL :: BaseTemp                          ! Base temperature at Z = 0 (C)

  BaseTemp = OutDewPointTemp + WeatherFileTempModCoeff

  IF (SiteTempGradient == 0.0) THEN
    LocalOutDewPointTemp = OutDewPointTemp
  ELSE IF (Z <= 0.0) THEN
    LocalOutDewPointTemp = BaseTemp
  ELSE
    LocalOutDewPointTemp = BaseTemp - SiteTempGradient * EarthRadius * Z / (EarthRadius + Z)
  END IF

  RETURN

END FUNCTION OutDewPointTempAt

FUNCTION WindSpeedAt(Z) RESULT(LocalWindSpeed)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   January 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates local wind speed at a given altitude.

          ! METHODOLOGY EMPLOYED:
          ! 2005 ASHRAE Fundamentals, Chapter 16, Equation 4.  (Different depending on terrain).

          ! REFERENCES:
          ! 2005 ASHRAE Fundamentals, Chapter 16, Equation 4.  (Different depending on terrain).
          ! Terrain variables are set in HeatBalanceManager or entered by the user.

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL, INTENT(IN) :: Z                ! Height above ground (m)
  REAL        :: LocalWindSpeed   ! Return result for function (m/s)

  IF (Z <= 0.0) THEN
    LocalWindSpeed = 0.0
  ELSE IF (SiteWindExp == 0.0) THEN
    LocalWindSpeed = WindSpeed
  ELSE
    !  [Met] - at meterological Station, Height of measurement is usually 10m above ground
    !  LocalWindSpeed = Windspeed [Met] * (Wind Boundary LayerThickness [Met]/Height [Met])**Wind Exponent[Met] &
    !                     * (Height above ground / Site Wind Boundary Layer Thickness) ** Site Wind Exponent
    !
    LocalWindSpeed = WindSpeed * WeatherFileWindModCoeff * (Z / SiteWindBLHeight) ** SiteWindExp
  END IF

  RETURN

END FUNCTION WindSpeedAt

FUNCTION OutBaroPressAt(Z) RESULT(LocalAirPressure)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Daeho Kang
          !       DATE WRITTEN   August 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates local air barometric pressure at a given altitude.

          ! METHODOLOGY EMPLOYED:
          ! U.S. Standard Atmosphere1976, Part 1, Chapter 1.3, Equation 33b.

          ! REFERENCES:
          ! U.S. Standard Atmosphere1976, Part 1, Chapter 1.3, Equation 33b.

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL, INTENT(IN) :: Z                ! Height above ground (m)
  REAL             :: LocalAirPressure ! Return result for function (Pa)

            ! FNCTION PARAMETER DEFINITIONS:
    REAL, PARAMETER :: StdGravity    = 9.80665   ! The acceleration of gravity at the sea level (m/s2)
    REAL, PARAMETER :: AirMolarMass  = 0.028964  ! Molar mass of Earth's air (kg/mol)
    REAL, PARAMETER :: GasConstant   = 8.31432   ! Molar gas constant (J/Mol-K)
    REAL, PARAMETER :: TempGradient  = -0.0065   ! Molecular-scale temperature gradient (K/m)
    REAL, PARAMETER :: GeopotentialH = 0.0       ! Geopotential height (zero within 11km from the sea level) (m)

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL :: BaseTemp                         ! Base temperature at Z

  BaseTemp = OutDryBulbTempAt(Z) + KelvinConv

  IF (Z <= 0.0) THEN
    LocalAirPressure = 0.0
  ELSE IF (SiteTempGradient == 0.0) THEN
    LocalAirPressure = OutBaroPress
  ELSE
    LocalAirPressure = StdBaroPress * (BaseTemp / (BaseTemp + TempGradient * (Z - GeopotentialH)))** &
                       ((StdGravity * AirMolarMass) / (GasConstant * TempGradient))
  END IF

  RETURN

END FUNCTION OutBaroPressAt

SUBROUTINE SetOutBulbTempAt(NumItems, Heights, DryBulb, WetBulb)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Noel Keen (LBL)/Linda Lawrie
          !       DATE WRITTEN   August 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Routine provides facility for doing bulk Set Temperature at Height.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN) :: NumItems
  REAL, INTENT(IN), DIMENSION(:) :: Heights
  REAL, INTENT(INOUT), DIMENSION(:) :: DryBulb
  REAL, INTENT(INOUT), DIMENSION(:) :: WetBulb

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  integer :: i    ! Loop Control
  REAL :: BaseDryTemp, BaseWetTemp         ! Base temperature at Z = 0 (C)
  REAL :: Z ! Centroid value


  BaseDryTemp = OutDryBulbTemp + WeatherFileTempModCoeff
  BaseWetTemp = OutWetBulbTemp + WeatherFileTempModCoeff


  IF (SiteTempGradient == 0.0) THEN
     DryBulb = OutDryBulbTemp
     WetBulb = OutWetBulbTemp
  ELSE
     DO i=1, NumItems
        Z = Heights(i)
        IF (Z <= 0.0) THEN
           DryBulb(i) = BaseDryTemp
           WetBulb(i) = BaseWetTemp
        ELSE
           DryBulb(i) = BaseDryTemp - SiteTempGradient * EarthRadius * Z / (EarthRadius + Z)
           WetBulb(i) = BaseWetTemp - SiteTempGradient * EarthRadius * Z / (EarthRadius + Z)
        ENDIF
     ENDDO
  END IF

  RETURN

END SUBROUTINE SetOutBulbTempAt

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
!

END MODULE DataEnvironment
