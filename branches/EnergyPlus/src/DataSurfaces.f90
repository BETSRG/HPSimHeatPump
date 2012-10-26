MODULE DataSurfaces      ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2000
          !       MODIFIED       July 2003, (CC) added a flag for reference air temperature
          !                      Dec 2006, DJS (PSU) added logical ecoroof variable
          !                      Dec 2008, TH added new properties to SurfaceWindowCalc for thermochromic windows
          !                      Jul 2011, M.J. Witte and C.O. Pedersen, add new fields to OSC for last T, max and min
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This data-only module contains derived types and other variables
          ! associated with Surfaces, their shading calculations, etc.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength
USE DataVectorTypes

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.

          ! MODULE PARAMETER DEFINITIONS:
INTEGER, PARAMETER :: MaxSlatAngs = 19

          ! Parameters to indicate surface shape for use with the Surface
          ! derived type (see below):

INTEGER, PARAMETER :: Triangle              = 1
INTEGER, PARAMETER :: Quadrilateral         = 2
INTEGER, PARAMETER :: Rectangle             = 3
INTEGER, PARAMETER :: Polygonal             = 9
INTEGER, PARAMETER :: RectangularDoorWindow = 4
INTEGER, PARAMETER :: RectangularOverhang   = 5
INTEGER, PARAMETER :: RectangularLeftFin    = 6
INTEGER, PARAMETER :: RectangularRightFin   = 7
INTEGER, PARAMETER :: TriangularWindow      = 8
INTEGER, PARAMETER :: TriangularDoor        = 9

          ! Parameters to indicate exterior boundary conditions for use with
          ! the Surface derived type (see below):
          ! Note:  Positive values correspond to an interzone adjacent surface

INTEGER, PARAMETER :: ExternalEnvironment       =  0
INTEGER, PARAMETER :: Ground                    = -1
INTEGER, PARAMETER :: OtherSideCoefNoCalcExt    = -2
INTEGER, PARAMETER :: OtherSideCoefCalcExt      = -3
INTEGER, PARAMETER :: OtherSideCondModeledExt   = -4
INTEGER, PARAMETER :: GroundFCfactorMethod      = -5

          ! Parameters to indicate the first "corner" of a surface
          ! Currently, these are used only during input of surfaces
          ! They are here in order to facilitate later use in shading setup/calculations.
INTEGER, PARAMETER :: UpperLeftCorner  = 1
INTEGER, PARAMETER :: LowerLeftCorner  = 2
INTEGER, PARAMETER :: LowerRightCorner = 3
INTEGER, PARAMETER :: UpperRightCorner = 4

          ! Parameters to indicate user specified convection coefficients (for surface)
INTEGER, PARAMETER :: ConvCoefValue    = 1 ! User specified "value" as the override type
INTEGER, PARAMETER :: ConvCoefSchedule = 2 ! User specified "schedule" as the override type
INTEGER, PARAMETER :: ConvCoefUserCurve= 3 ! User specified "UserCurve" as the override type
INTEGER, PARAMETER :: ConvCoefSpecifiedModel = 4 ! one of the direct named model equation keys

          ! Parameters to indicate reference air temperatures for inside surface temperature calculations
INTEGER, PARAMETER :: ZoneMeanAirTemp    = 1 ! mean air temperature of the zone => MAT
INTEGER, PARAMETER :: AdjacentAirTemp    = 2 ! air temperature adjacent ot surface => TempEffBulkAir
INTEGER, PARAMETER :: ZoneSupplyAirTemp  = 3 ! supply air temperature of the zone

INTEGER, PARAMETER :: AltAngStepsForSolReflCalc=10  ! Number of steps in altitude angle for solar reflection calc
INTEGER, PARAMETER :: AzimAngStepsForSolReflCalc=9  ! Number of steps in azimuth angle of solar reflection calc

          ! Parameters to indicate surface classes
   ! Surface Class (FLOOR, WALL, ROOF (incl's CEILING), WINDOW, DOOR, GLASSDOOR,
   ! SHADING (includes OVERHANG, WING), DETACHED, INTMASS),
   ! TDD:DOME, TDD:DIFFUSER (for tubular daylighting device)
   ! (Note: GLASSDOOR and TDD:DIFFUSER get overwritten as WINDOW
   ! in SurfaceGeometry.f90, SurfaceWindow%OriginalClass holds the true value)
   ! why aren't these sequential (LKL - 13 Aug 2007)
INTEGER, PARAMETER :: SurfaceClass_Wall=1
INTEGER, PARAMETER :: SurfaceClass_Floor=2
INTEGER, PARAMETER :: SurfaceClass_Roof=3
INTEGER, PARAMETER :: SurfaceClass_IntMass=5
INTEGER, PARAMETER :: SurfaceClass_Detached_B=6
INTEGER, PARAMETER :: SurfaceClass_Detached_F=7
INTEGER, PARAMETER :: SurfaceClass_Window=11
INTEGER, PARAMETER :: SurfaceClass_Door=13
INTEGER, PARAMETER :: SurfaceClass_GlassDoor=12
INTEGER, PARAMETER :: SurfaceClass_Shading=14
INTEGER, PARAMETER :: SurfaceClass_Overhang=15
INTEGER, PARAMETER :: SurfaceClass_Fin=16
INTEGER, PARAMETER :: SurfaceClass_TDD_Dome=17
INTEGER, PARAMETER :: SurfaceClass_TDD_Diffuser=18

  ! Parameters for classification of outside face of surfaces
INTEGER, PARAMETER :: OutConvClass_WindwardVertWall    = 101
INTEGER, PARAMETER :: OutConvClass_LeewardVertWall     = 102
INTEGER, PARAMETER :: OutConvClass_RoofStable          = 103
INTEGER, PARAMETER :: OutConvClass_RoofUnstable        = 104

  ! Parameters for adpative convection algorithm's classification of inside face of surfaces
INTEGER, PARAMETER :: InConvClass_A1_VertWalls          = 1 !flow regime A1, vertical walls
INTEGER, PARAMETER :: InConvClass_A1_StableHoriz        = 2 !flow regime A1
INTEGER, PARAMETER :: InConvClass_A1_UnstableHoriz      = 3 !flow regime A1
INTEGER, PARAMETER :: InConvClass_A1_HeatedFloor        = 4 !flow regime A1
INTEGER, PARAMETER :: InConvClass_A1_ChilledCeil        = 5 !flow regime A1
INTEGER, PARAMETER :: InConvClass_A1_StableTilted       = 6 !flow regime A1
INTEGER, PARAMETER :: InConvClass_A1_UnstableTilted     = 7 !flow regime A1
INTEGER, PARAMETER :: InConvClass_A1_Windows            = 8 !flow regime A1
INTEGER, PARAMETER :: InConvClass_A2_VertWallsNonHeated = 9 !flow regime A2
INTEGER, PARAMETER :: InConvClass_A2_HeatedVerticalWall = 10 !flow regime A2
INTEGER, PARAMETER :: InConvClass_A2_StableHoriz        = 11 !flow regime A2
INTEGER, PARAMETER :: InConvClass_A2_UnstableHoriz      = 12 !flow regime A2
INTEGER, PARAMETER :: InConvClass_A2_StableTilted       = 13 !flow regime A2
INTEGER, PARAMETER :: InConvClass_A2_UnstableTilted     = 14 !flow regime A2
INTEGER, PARAMETER :: InConvClass_A2_Windows            = 15 !flow regime A2
INTEGER, PARAMETER :: InConvClass_A3_VertWalls          = 16 !flow regime A3
INTEGER, PARAMETER :: InConvClass_A3_StableHoriz        = 17 !flow regime A3
INTEGER, PARAMETER :: InConvClass_A3_UnstableHoriz      = 18 !flow regime A3
INTEGER, PARAMETER :: InConvClass_A3_StableTilted       = 19 !flow regime A3
INTEGER, PARAMETER :: InConvClass_A3_UnstableTilted     = 20 !flow regime A3
INTEGER, PARAMETER :: InConvClass_A3_Windows            = 21 !flow regime A3
INTEGER, PARAMETER :: InConvClass_B_VertWalls           = 22 !flow regime B
INTEGER, PARAMETER :: InConvClass_B_VertWallsNearHeat   = 23 !flow regime B
INTEGER, PARAMETER :: InConvClass_B_StableHoriz         = 24 !flow regime B
INTEGER, PARAMETER :: InConvClass_B_UnstableHoriz       = 25 !flow regime B
INTEGER, PARAMETER :: InConvClass_B_StableTilted        = 26 !flow regime B
INTEGER, PARAMETER :: InConvClass_B_UnstableTilted      = 27 !flow regime B
INTEGER, PARAMETER :: InConvClass_B_Windows             = 28 !flow regime B
INTEGER, PARAMETER :: InConvClass_C_Walls               = 29 !flow regime C
INTEGER, PARAMETER :: InConvClass_C_Ceiling             = 30 !flow regime C
INTEGER, PARAMETER :: InConvClass_C_Floor               = 31 !flow regime C
INTEGER, PARAMETER :: InConvClass_C_Windows             = 32 !flow regime C
INTEGER, PARAMETER :: InConvClass_D_Walls               = 33 !flow regime D
INTEGER, PARAMETER :: InConvClass_D_StableHoriz         = 34 !flow regime D
INTEGER, PARAMETER :: InConvClass_D_UnstableHoriz       = 35 !flow regime D
INTEGER, PARAMETER :: InConvClass_D_StableTilted        = 36 !flow regime D
INTEGER, PARAMETER :: InConvClass_D_UnstableTilted      = 37 !flow regime D
INTEGER, PARAMETER :: InConvClass_D_Windows             = 38 !flow regime D
INTEGER, PARAMETER :: InConvClass_E_AssistFlowWalls     = 39 !flow regime E
INTEGER, PARAMETER :: InConvClass_E_OpposFlowWalls      = 40 !flow regime E
INTEGER, PARAMETER :: InConvClass_E_StableFloor         = 41 !flow regime E
INTEGER, PARAMETER :: InConvClass_E_UnstableFloor       = 42 !flow regime E
INTEGER, PARAMETER :: InConvClass_E_StableCeiling       = 43 !flow regime E
INTEGER, PARAMETER :: InConvClass_E_UnstableCieling     = 44 !flow regime E
INTEGER, PARAMETER :: InConvClass_E_Windows             = 45 !flow regime E

          ! Parameters for fenestration relative location in zone
INTEGER, PARAMETER :: InConvWinLoc_NotSet                  = 0
INTEGER, PARAMETER :: InConvWinLoc_LowerPartOfExteriorWall = 1 !this is a window in the lower part of wall
INTEGER, PARAMETER :: InConvWinLoc_UpperPartOfExteriorWall = 2 !this is a window in the upper part of wall
INTEGER, PARAMETER :: InConvWinLoc_WindowAboveThis         = 3 !this is a wall with window above it
INTEGER, PARAMETER :: InConvWinLoc_WindowBelowThis         = 4 !this is a wall with window below it
INTEGER, PARAMETER :: InConvWinLoc_LargePartOfExteriorWall = 5 !this is a big window taking up most of wall

          ! Parameters for window shade status
INTEGER, PARAMETER :: NoShade            =-1
INTEGER, PARAMETER :: ShadeOff           = 0
INTEGER, PARAMETER :: IntShadeOn         = 1      ! Interior shade on
INTEGER, PARAMETER :: SwitchableGlazing  = 2
INTEGER, PARAMETER :: ExtShadeOn         = 3      ! Exterior shade on
INTEGER, PARAMETER :: ExtScreenOn        = 4      ! Exterior screen on
INTEGER, PARAMETER :: IntBlindOn         = 6      ! Interior blind on
INTEGER, PARAMETER :: ExtBlindOn         = 7      ! Exterior blind on
INTEGER, PARAMETER :: BGShadeOn          = 8      ! Between-glass shade on
INTEGER, PARAMETER :: BGBlindOn          = 9      ! Between-glass blind on
INTEGER, PARAMETER :: IntShadeConditionallyOff = 10
INTEGER, PARAMETER :: GlassConditionallyLightened = 20
INTEGER, PARAMETER :: ExtShadeConditionallyOff = 30
INTEGER, PARAMETER :: IntBlindConditionallyOff = 60
INTEGER, PARAMETER :: ExtBlindConditionallyOff = 70

          ! WindowShadingControl Shading Types
INTEGER, PARAMETER :: WSC_ST_NoShade           = 0
INTEGER, PARAMETER :: WSC_ST_InteriorShade     = 1
INTEGER, PARAMETER :: WSC_ST_SwitchableGlazing = 2
INTEGER, PARAMETER :: WSC_ST_ExteriorShade     = 3
INTEGER, PARAMETER :: WSC_ST_InteriorBlind     = 4
INTEGER, PARAMETER :: WSC_ST_ExteriorBlind     = 5
INTEGER, PARAMETER :: WSC_ST_BetweenGlassShade = 6
INTEGER, PARAMETER :: WSC_ST_BetweenGlassBlind = 7
INTEGER, PARAMETER :: WSC_ST_ExteriorScreen    = 8

          ! WindowShadingControl Control Types
INTEGER, PARAMETER :: WSCT_AlwaysOn                      =  1  ! AlwaysOn
INTEGER, PARAMETER :: WSCT_AlwaysOff                     =  2  ! AlwaysOff
INTEGER, PARAMETER :: WSCT_OnIfScheduled                 =  3  ! OnIfScheduleAllows
INTEGER, PARAMETER :: WSCT_HiSolar                       =  4  ! OnIfHighSolarOnWindow
INTEGER, PARAMETER :: WSCT_HiHorzSolar                   =  5  ! OnIfHighHorizontalSolar
INTEGER, PARAMETER :: WSCT_HiOutAirTemp                  =  6  ! OnIfHighOutsideAirTemp
INTEGER, PARAMETER :: WSCT_HiZoneAirTemp                 =  7  ! OnIfHighZoneAirTemp
INTEGER, PARAMETER :: WSCT_HiZoneCooling                 =  8  ! OnIfHighZoneCooling
INTEGER, PARAMETER :: WSCT_HiGlare                       =  9  ! OnIfHighGlare
INTEGER, PARAMETER :: WSCT_MeetDaylIlumSetp              = 10  ! MeetDaylightIlluminanceSetpoint
INTEGER, PARAMETER :: WSCT_OnNightLoOutTemp_OffDay       = 11  ! OnNightIfLowOutsideTemp/OffDay
INTEGER, PARAMETER :: WSCT_OnNightLoInTemp_OffDay        = 12  ! OnNightIfLowInsideTemp/OffDay
INTEGER, PARAMETER :: WSCT_OnNightIfHeating_OffDay       = 13  ! OnNightIfHeating/OffDay
INTEGER, PARAMETER :: WSCT_OnNightLoOutTemp_OnDayCooling = 14  ! OnNightIfLowOutsideTemp/OnDayIfCooling
INTEGER, PARAMETER :: WSCT_OnNightIfHeating_OnDayCooling = 15  ! OnNightIfHeating/OnDayIfCooling
INTEGER, PARAMETER :: WSCT_OffNight_OnDay_HiSolarWindow  = 16  ! OffNight/OnDayIfCoolingAndHighSolarOnWindow
INTEGER, PARAMETER :: WSCT_OnNight_OnDay_HiSolarWindow   = 17  ! OnNight/OnDayIfCoolingAndHighSolarOnWindow
INTEGER, PARAMETER :: WSCT_OnHiOutTemp_HiSolarWindow     = 18  ! OnIfHighOutsideAirTempAndHighSolarOnWindow
INTEGER, PARAMETER :: WSCT_OnHiOutTemp_HiHorzSolar       = 19  ! OnIfHighOutsideAirTempAndHighHorizontalSolar
INTEGER, PARAMETER :: WSCT_OnHiZoneTemp_HiSolarWindow    = 20  ! OnIfHighZoneAirTempAndHighSolarOnWindow
INTEGER, PARAMETER :: WSCT_OnHiZoneTemp_HiHorzSolar      = 21  ! OnIfHighZoneAirTempAndHighHorizontalSolar

          ! WindowShadingControl Slat Angle Control for Blinds
INTEGER, PARAMETER :: WSC_SAC_FixedSlatAngle     = 1
INTEGER, PARAMETER :: WSC_SAC_ScheduledSlatAngle = 2
INTEGER, PARAMETER :: WSC_SAC_BlockBeamSolar     = 3

          ! Parameter for window screens beam reflectance accounting
INTEGER, PARAMETER :: DoNotModel = 0
INTEGER, PARAMETER :: ModelAsDirectBeam = 1
INTEGER, PARAMETER :: ModelAsDiffuse = 2

           ! Parameters for window divider type
INTEGER, PARAMETER :: DividedLite        = 1
INTEGER, PARAMETER :: Suspended          = 2

           ! Parameters for air flow window source
INTEGER, PARAMETER :: AirFlowWindow_Source_IndoorAir  = 1
INTEGER, PARAMETER :: AirFlowWindow_Source_OutdoorAir = 2

           ! Parameters for air flow window destination
INTEGER, PARAMETER :: AirFlowWindow_Destination_IndoorAir  = 1
INTEGER, PARAMETER :: AirFlowWindow_Destination_OutdoorAir = 2
INTEGER, PARAMETER :: AirFlowWindow_Destination_ReturnAir  = 3

           ! Parameters for air flow window control
INTEGER, PARAMETER :: AirFlowWindow_ControlType_MaxFlow   = 1
INTEGER, PARAMETER :: AirFlowWindow_ControlType_AlwaysOff = 2
INTEGER, PARAMETER :: AirFlowWindow_ControlType_Schedule  = 3

          ! DERIVED TYPE DEFINITIONS:
TYPE SurfaceData

  CHARACTER(len=MaxNameLength) :: Name         = ' ' ! User supplied name of the surface (must be unique)
  INTEGER :: Construction                      = 0   ! Pointer to the construction in the Construct derived type
  LOGICAL :: EMSConstructionOverrideON         = .FALSE. ! if true, EMS is calling to override the construction value
  INTEGER :: EMSConstructionOverrideValue      = 0 ! pointer value to use for Construction when overridden
  INTEGER :: ConstructionStoredInputValue      = 0 ! holds the original value for Construction per surface input
  INTEGER :: Class                             =0

          ! Geometry related parameters
  INTEGER :: Shape                             = 0   ! Surface shape (Triangle=1,Quadrilateral=2,Rectangle=3,
                                                     !                Rectangular Window/Door=4,Rectangular Overhang=5,
                                                     !                Rectangular Left Fin=6,Rectangular Right Fin=7,
                                                     !                Triangular Window=8)
  INTEGER :: Sides                             = 0   ! Number of side/vertices for this surface (based on Shape)
  REAL :: Area                            = 0.0 ! Surface area of the surface (less any subsurfaces) {m2}
  REAL :: GrossArea                       = 0.0 ! Surface area of the surface (including subsurfaces) {m2}
  REAL :: NetAreaShadowCalc               = 0.0 ! Area of a wall/floor/ceiling less subsurfaces assuming
                                                     !  all windows, if present, have unity multiplier.
                                                     ! Wall/floor/ceiling/roof areas that include windows include
                                                     !  frame (unity) areas.
                                                     ! Areas of Windows including divider (unity) area.
                                                     ! These areas are used in shadowing / sunlit area calculations.
  REAL :: Perimeter                       = 0.0 ! Perimeter length of the surface {m}
  REAL :: Azimuth                         = 0.0 ! Direction the surface outward normal faces (degrees) or FACING
  REAL :: Height                          = 0.0 ! Height of the surface (m)
  REAL :: Reveal                          = 0.0 ! Depth of the window reveal (m) if this surface is a window
  REAL :: Tilt                            = 0.0 ! Angle (deg) between the ground outward normal and the surface outward normal
  REAL :: Width                           = 0.0 ! Width of the surface (m)

          ! Boundary conditions and interconnections
  LOGICAL :: HeatTransSurf                 = .false. ! True if surface is a heat transfer surface,
                                                     ! False if a (detached) shadowing (sub)surface
  CHARACTER(len=MaxNameLength) :: BaseSurfName = ' ' ! Name of BaseSurf
  INTEGER :: BaseSurf                          = 0   ! "Base surface" for this surface.  Applies mainly to subsurfaces
                                                     ! in which case it points back to the base surface number.
                                                     ! Equals 0 for detached shading.
                                                     ! BaseSurf equals surface number for all other surfaces.
  INTEGER :: NumSubSurfaces                    = 0   ! Number of subsurfaces this surface has (doors/windows)
  CHARACTER(len=MaxNameLength) :: ZoneName     = ' ' ! User supplied name of the Zone
  INTEGER :: Zone                              = 0   ! Interior environment or zone the surface is a part of
                                                     ! Note that though attached shading surfaces are part of a zone, this
                                                     ! value is 0 there to facilitate using them as detached surfaces (more
                                                     ! accurate shading.
  CHARACTER(len=MaxNameLength) :: ExtBoundCondName = ' ' ! Name for the Outside Environment Object
  INTEGER :: ExtBoundCond                      = 0   ! For an "interzone" surface, this is the adjacent surface number.
                                                     ! for an internal/adiabatic surface this is the current surface number.
                                                     ! Otherwise, 0=external environment, -1=ground,
                                                     ! -2=other side coefficients (OSC--won't always use CTFs)
                                                     ! -3=other side conditions model
  INTEGER :: LowTempErrCount               = 0
  INTEGER :: HighTempErrCount              = 0
  LOGICAL :: ExtSolar                      = .false. ! True if the "outside" of the surface is exposed to solar
  LOGICAL :: ExtWind                       = .false. ! True if the "outside" of the surface is exposed to wind

          ! Heat transfer coefficients
  INTEGER :: IntConvCoeff                      = 0   ! Interior Convection Coefficient pointer (different data structure)
                                                     ! when being overridden
  LOGICAL :: EMSOverrideIntConvCoef            = .FALSE. ! if true, EMS is calling to override interior convection coefficeint
  REAL :: EMSValueForIntConvCoef          = 0.0 ! Value EMS is calling to use for interior convection coefficient [W/m2-K]
  INTEGER :: ExtConvCoeff                      = 0   ! Exterior Convection Coefficient pointer (different data structure)
                                                     ! when being overridden
  LOGICAL :: EMSOverrideExtConvCoef            = .FALSE. ! if true, EMS is calling to override exterior convection coefficeint
  REAL :: EMSValueForExtConvCoef          = 0.0 ! Value EMS is calling to use for exterior convection coefficient [W/m2-K]
  REAL :: ViewFactorGround                = 0.0 ! View factor to the ground from the exterior of the surface
                                                     !   for diffuse solar radiation
  REAL :: ViewFactorSky                   = 0.0 ! View factor to the sky from the exterior of the surface
                                                     !   for diffuse solar radiation
  REAL :: ViewFactorGroundIR              = 0.0 ! View factor to the ground and shadowing surfaces from the
                                                     !    exterior of the surface for IR radiation
  REAL :: ViewFactorSkyIR                 = 0.0 ! View factor to the sky from the exterior of the surface for IR radiation

          ! Special/optional other side coefficients (OSC)
  INTEGER :: OSCPtr                            = 0   ! Pointer to OSC data structure
  INTEGER :: OSCMPtr                           = 0   ! "Pointer" to OSCM data structure (other side conditions from a model)

          ! Optional parameters specific to shadowing surfaces and subsurfaces (detached shading, overhangs, wings, etc.)
  INTEGER :: SchedShadowSurfIndex              = 0   ! Schedule for a shadowing (sub)surface
  LOGICAL :: ShadowSurfSchedVaries             = .false. ! true if the scheduling (transmittance) on a shading surface varies.
  LOGICAL :: ShadowingSurf                     = .false. ! True if a surface is a shadowing surface
  LOGICAL :: IsTransparent                     = .false. ! True if the schedule values are always 1.0 (or the minimum is 1.0)
  REAL :: SchedMinValue                   = 0.0   ! Schedule minimum value.

          ! Optional parameters specific to solar reflection from surfaces
  REAL    :: ShadowSurfDiffuseSolRefl     = 0.0 ! Diffuse solar reflectance of opaque portion
  REAL    :: ShadowSurfDiffuseVisRefl     = 0.0 ! Diffuse visible reflectance of opaque portion
  REAL    :: ShadowSurfGlazingFrac        = 0.0 ! Glazing fraction
  INTEGER :: ShadowSurfGlazingConstruct        = 0   ! Glazing construction number
  LOGICAL :: ShadowSurfPossibleObstruction     = .TRUE. ! True if a surface can be an exterior obstruction
  LOGICAL :: ShadowSurfPossibleReflector       = .FALSE. ! True if a surface can be an exterior reflector, not used!
  INTEGER :: ShadowSurfRecSurfNum              = 0   ! Receiving surface number

          ! Optional movable insulation parameters
  INTEGER :: MaterialMovInsulExt               = 0   ! Pointer to the material used for exterior movable insulation
  INTEGER :: MaterialMovInsulInt               = 0   ! Pointer to the material used for interior movable insulation
  INTEGER :: SchedMovInsulExt                  = 0   ! Schedule for exterior movable insulation
  INTEGER :: SchedMovInsulInt                  = 0   ! Schedule for interior movable insulation

          ! Vertices
  TYPE (vector), ALLOCATABLE, DIMENSION(:) :: Vertex     ! Surface Vertices are represented by Number of Sides and Vector (type)
  TYPE (vector) :: Centroid          =vector(0.,0.,0.)   ! computed centroid (also known as center of mass or surface balance point)
  type (vector) :: lcsx              =vector(0.,0.,0.)
  type (vector) :: lcsy              =vector(0.,0.,0.)
  type (vector) :: lcsz              =vector(0.,0.,0.)
  type (vector) :: NewellAreaVector  =vector(0.,0.,0.)
  type (vector) :: NewellSurfaceNormalVector  =vector(0.,0.,0.)  ! same as OutNormVec in vector notation
  REAL, DIMENSION(3) :: OutNormVec        =0.0  ! Direction cosines (outward normal vector) for surface

  REAL :: SinAzim                         = 0.0 ! Sine of surface azimuth angle
  REAL :: CosAzim                         = 0.0 ! Cosine of surface azimuth angle
  REAL :: SinTilt                         = 0.0 ! Sine of surface tilt angle
  REAL :: CosTilt                         = 0.0 ! Cosine of surface tilt angle
  LOGICAL   :: IsConvex                        = .true. ! true if the surface is convex.
  LOGICAL   :: IsDegenerate                    = .false. ! true if the surface is degenerate.

          ! Window Parameters (when surface is Window)
  INTEGER :: WindowShadingControlPtr           = 0   ! Pointer to shading control (windows only)
  INTEGER :: ShadedConstruction                = 0   ! Shaded construction (windows only)
  INTEGER :: StormWinConstruction              = 0   ! Construction with storm window (windows only)
  INTEGER :: StormWinShadedConstruction        = 0   ! Shaded construction with storm window (windows only)
  INTEGER :: FrameDivider                      = 0   ! Pointer to frame and divider information (windows only)
  REAL    :: Multiplier                   = 1.0 ! Multiplies glazed area, frame area and divider area (windows only)
          ! Daylighting pointers
  INTEGER :: Shelf                             = 0   ! Pointer to daylighting shelf
  INTEGER :: TAirRef                           = ZoneMeanAirTemp  ! Flag for reference air temperature
        ! ZoneMeanAirTemp   = 1 = mean air temperature or MAT => for mixing air model with all convection algos
        ! except inlet-dependent algo
        ! AdjacentAirTemp   = 2 = adjacent air temperature or TempEffBulkAir => for nodal or zonal air model
        ! with all convection algos except inlet-dependent algo
        ! ZoneSupplyAirTemp = 3 = supply air temperature => for mixing air model with inlet-dependent algo
        ! Default value is 'ZoneMeanAirTemp' and value for each particular surface will be changed only if
                                ! the inlet-dependent convection algorithm and/or nodal and zonal air models are used.
  REAL :: OutDryBulbTemp                  = 0.0 ! Surface outside dry bulb air temperature, for surface heat balance (C)
  REAL :: OutWetBulbTemp                  = 0.0 ! Surface outside wet bulb air temperature, for surface heat balance (C)
  REAL :: WindSpeed                       = 0.0 ! Surface outside wind speed, for surface heat balance (m/s)
  CHARACTER(len=15) :: UNomWOFilm              = '-' ! Nominal U Value without films stored as string
  CHARACTER(len=15) :: UNomFilm                = '-' ! Nominal U Value with films stored as string

  LOGICAL :: ExtEcoRoof                    = .false. ! True if the top outside construction material is of type Eco Roof
  LOGICAL :: ExtCavityPresent              = .false. ! true if there is an exterior vented cavity on surface
  INTEGER :: ExtCavNum                     = 0        ! index for this surface in ExtVentedCavity structure (if any)
  LOGICAL :: IsPV                          = .false.  ! true if this is a photovoltaic surface (dxf output)
  LOGICAL :: IsICS                         = .false.  ! true if this is an ICS collector
  INTEGER :: ICSPtr                        = 0        ! Index to ICS collector

  ! TH added 3/26/2010
  LOGICAL :: MirroredSurf                  = .false.  ! Ture if it is a mirrored surface

  ! additional attributes for convection correlations
  INTEGER   :: IntConvClassification     = 0 ! current classification for inside face air flow regime and surface orientation
  INTEGER   :: IntConvHcModelEq          = 0 ! current convection model for inside face
  INTEGER   :: IntConvHcUserCurveIndex   = 0 ! current index to user convection model if used
  INTEGER   :: OutConvClassification     = 0 ! current classification for outside face wind regime and convection orientation
  INTEGER   :: OutConvHfModelEq          = 0 ! current convection model for forced convection at outside face
  INTEGER   :: OutConvHfUserCurveIndex   = 0 ! current index to user forced convection model if used
  INTEGER   :: OutConvHnModelEq          = 0 ! current Convection model for natural convection at outside face
  INTEGER   :: OutConvHnUserCurveIndex   = 0 ! current index to user natural convection model if used

  REAL :: OutConvFaceArea           = 0. ! area of larger building envelope facade that surface is a part of
  REAL :: OutConvFacePerimeter      = 0. ! perimeter of larger building envelope facade that surface is a part of
  REAL :: OutConvFaceHeight         = 0. ! height of larger building envelope facade that surface is a part of
  REAL :: IntConvZoneWallHeight     = 0. ! [m] height of larger inside building wall element that surface is a part of
  REAL :: IntConvZonePerimLength    = 0. ! [m] length of perimeter zone's exterior wall
  REAL :: IntConvZoneHorizHydrDiam  = 0. ! [m] hydraulic diameter, usually 4 times the zone floor area div by perimeter
  REAL :: IntConvWindowWallRatio    = 0. ! [-] area of windows over area of exterior wall for zone
  INTEGER   :: IntConvWindowLocation     = InConvWinLoc_NotSet ! relative location of window in zone for interior Hc models
  LOGICAL   :: IntConvSurfGetsRadiantHeat= .FALSE.
  LOGICAL   :: IntConvSurfHasActiveInIt  = .FALSE.
  LOGICAL   :: PartOfVentSlabOrRadiantSurface = .false. ! surface cannot be part of both a radiant surface & ventilated slab group
  ! LG added 1/6/12
  REAL :: GenericContam             = 0. ! [ppm] Surface generic contaminant as a storage term for
                                                ! the surface diffusion model
END TYPE SurfaceData

TYPE SurfaceWindowCalc          ! Calculated window-related values

  INTEGER :: ShadingFlag       = ShadeOff   !  -1: window has no shading device
                                            !   0: shading device is off
                                            !   1: interior shade is on
                                            !   2: glazing is switched to darker state
                                            !   3: exterior shade is on
                                            !   4: exterior screen is on
                                            !   6: interior blind is on
                                            !   7: exterior blind is on
                                            !   8: between-glass shade is on
                                            !   9: between-glass blind is on
                                            !  10: window has an interior shade that is off but may be
                                            !       triggered on later to control daylight glare
                                            !  20: window has switchable glazing that is unswitched but may be switched later
                                            !       to control daylight glare or daylight illuminance
                                            !  30: window has exterior shade that is off but may be triggered on later
                                            !       to control daylight glare
                                            !  60: window has an interior blind that is off but may be
                                            !       triggered on later to control daylight glare
                                            !  70: window has an exterior blind that is off but may be
                                            !       triggered on later to control daylight glare
                                            !  80: window has a between-glass shade that is off but may be
                                            !       triggered on later to control daylight glare
                                            !  90: window has a between-glass blind that is off but may be
                                            !       triggered on later to control daylight glare
  LOGICAL :: ShadingFlagEMSOn = .FALSE.     ! EMS control flag, true if EMS is controlling ShadingFlag with ShadingFlagEMSValue
  INTEGER :: ShadingFlagEMSValue = 0        ! EMS control value for Shading Flag
  INTEGER :: StormWinFlag             = -1  !  -1: Storm window not applicable
                                            !   0: Window has storm window but it is off
                                            !   1: Window has storm window and it is on
  INTEGER :: StormWinFlagPrevDay      = -1  !  Previous time step value of StormWinFlag

  REAL :: FracTimeShadingDeviceOn = 0.0 ! For a single time step, = 0.0 if no shading device or shading device is off,
                                            !                         = 1.0 if shading device is on;
                                            ! For time intervals longer than a time step, = fraction of time that shading
                                            ! device is on.
  INTEGER :: ExtIntShadePrevTS        = 0   ! 1 if exterior or interior blind or shade in place previous time step;
                                            ! 0 otherwise
  INTEGER :: ShadedConstruction       = 0   ! For windows with shading, the construction with shading
  LOGICAL :: SurfDayLightInit         = .false.  ! surface has been initialized for following 5 arrays
  REAL, ALLOCATABLE, DIMENSION(:)   :: SolidAngAtRefPt         ! Solid angle subtended by window from daylit ref points 1 and 2
  REAL, ALLOCATABLE, DIMENSION(:)   :: SolidAngAtRefPtWtd      ! Solid angle subtended by window from
                                                               ! ref pts weighted by glare pos factor
  REAL, ALLOCATABLE, DIMENSION(:,:) :: IllumFromWinAtRefPt     ! Illuminance from window at ref pts for window
                                                               ! with and w/o shade (lux)
  REAL, ALLOCATABLE, DIMENSION(:,:) :: BackLumFromWinAtRefPt   ! Window background luminance from window wrt ref pts (cd/m2)
                                                               ! with and w/o shade (cd/m2)
  REAL, ALLOCATABLE, DIMENSION(:,:) :: SourceLumFromWinAtRefPt ! Window luminance at ref pts for window
                                                               ! with and w/o shade (cd/m2)
  INTEGER :: DaylFacPoint             = 0   ! Pointer to daylight factors for the window
  REAL :: VisTransSelected        = 0.0 ! Window vis trans at normal incidence selected for use in dayltg calculation
  REAL :: SwitchingFactor         = 0.0 ! Window switching factor (0.0 = unswitched; 1.0 = fully switched)
  REAL, DIMENSION(3) :: WinCenter = 0.0 ! X,Y,Z coordinates of window center point in building coord system
  REAL :: Theta                   = 0.0 ! Azimuth of window normal (rad)
  REAL :: Phi                     = 0.0 ! Altitude of window normal (rad)
  REAL :: RhoCeilingWall          = 0.0 ! Average interior reflectance seen by light moving up across horizontal
                                            !  plane thru center of window
  REAL :: RhoFloorWall            = 0.0 ! Same as above, but for light moving down
  REAL :: FractionUpgoing         = 0.0 ! Fraction light entering window that goes upward
  REAL :: VisTransRatio           = 0.0 ! For windows with switchable glazing, ratio of normal transmittance
                                            !  in switched state to that in unswitched state
  REAL, DIMENSION(10) :: ThetaFace    = 296.15 ! Face temperatures of window layers (K)
  REAL :: IRfromParentZone        = 0.0 ! Incident IR from parent zone (W/m2)
  INTEGER :: IRErrCount               = 0   ! For recurring error counts
  INTEGER :: IRErrCountC              = 0   ! For recurring error counts (continuation)
  REAL :: FrameArea               = 0.0 ! Frame projected area (m2)
  REAL :: FrameConductance        = 0.0 ! Frame conductance [no air films] (W/m2-K)
  REAL :: FrameSolAbsorp          = 0.0 ! Frame solar absorptance (assumed same inside and outside)
  REAL :: FrameVisAbsorp          = 0.0 ! Frame visible absorptance (assumed same inside and outside)
  REAL :: FrameEmis               = 0.0 ! Frame thermal emissivity (thermal absorptance) (assumed same
                                            !   inside and outside)
  REAL :: FrameAreaXEmiss         = 0.0 ! Frame area times thermal emissivity (m2)
  REAL :: FrameRadExchangeFactor  = 0.0 ! Frame IR radiant exchange factor
  REAL :: FrameHRadLinIn          = 0.0 ! Frame linearized inside IR radiation conductance (W/m2-K)
  REAL :: FrameRadThermalFluxRec  = 0.0 ! Frame inside IR flux received (W/m2)
  REAL :: FrameRadThermalFluxRecOld  = 0.0 ! Previous value of frame inside IR flux received (W/m2)
  REAL :: FrEdgeToCenterGlCondRatio  = 1.0 ! Ratio of frame edge of glass conductance (without air films) to
                                            ! center of glass conductance (without air films)
  REAL :: FrameEdgeArea           = 0.0 ! Area of glass near frame (m2)
  REAL :: FrameTempSurfIn         = 23.0 ! Frame inside surface temperature (C)
  REAL :: FrameTempSurfInOld      = 23.0 ! Previous value of frame inside surface temperature (C)
  REAL :: FrameTempSurfOut        = 23.0 ! Frame outside surface temperature (C)
  REAL :: FrameQRadInAbs          = 0.0 ! Radiation absorbed by inside of frame (short-wave from solar
                                            !   and lights; long-wave from internal gains) (W/m2)
  REAL :: FrameQRadOutAbs         = 0.0 ! Radiation absorbed by outside of frame (solar) (W/m2)
  REAL :: ProjCorrFrOut           = 0.0 ! Correction factor to absorbed radiation due to frame outside projection
  REAL :: ProjCorrFrIn            = 0.0 ! Correction factor to absorbed radiation due to frame inside projection
  INTEGER :: DividerType              = 0   ! Divider type (1=DividedLite, 2=Suspended (between-pane))
  REAL :: DividerArea             = 0.0 ! Divider projected area (m2)
  REAL :: DividerConductance      = 0.0 ! Divider conductance [no air films] (W/m2-K)
  REAL :: DividerSolAbsorp        = 0.0 ! Divider solar absorptance (assumed same inside and outside)
  REAL :: DividerVisAbsorp        = 0.0 ! Divider visible absorptance (assumed same inside and outside)
  REAL :: DividerEmis             = 0.0 ! Divider thermal emissivity (thermal absorptance) (assumed same
                                            !   inside and outside)
  REAL :: DividerAreaXEmiss       = 0.0 ! Divider area times thermal emissivity (m2)
  REAL :: DividerRadExchangeFactor= 0.0 ! Divider IR radiant exchange factor
  REAL :: DividerHRadLinIn        = 0.0 ! Divider linearized inside IR radiation conductance (W/m2-K)
  REAL :: DividerRadThermalFluxRec    = 0.0 ! Divider inside IR flux received (W/m2)
  REAL :: DividerRadThermalFluxRecOld = 0.0 ! Previous value of divider inside IR flux received (W/m2)

  REAL :: DivEdgeToCenterGlCondRatio  = 1.0 ! Ratio of divider edge of glass conductance (without air films) to
                                            ! center of glass conductance (without air films)
  REAL :: DividerEdgeArea         = 0.0 ! Area of glass near dividers (m2)
  REAL :: DividerTempSurfIn       = 23.0 ! Divider inside surface temperature (C)
  REAL :: DividerTempSurfInOld    = 23.0 ! Previous value of divider inside surface temperature (C)
  REAL :: DividerTempSurfOut      = 23.0 ! Divider outside surface temperature (C)
  REAL :: DividerQRadInAbs        = 0.0 ! Radiation absorbed by inside of divider (short-wave from solar
                                            !   and lights; long-wave from internal gains) (W/m2)
  REAL :: DividerQRadOutAbs       = 0.0 ! Radiation absorbed by outside of divider (solar) (W/m2)
  REAL :: ProjCorrDivOut          = 0.0 ! Correction factor to absorbed radiation due to divider outside projection
  REAL :: ProjCorrDivIn           = 0.0 ! Correction factor to absorbed radiation due to divider inside projection
  REAL :: GlazedFrac              = 1.0 ! (Glazed area)/(Glazed area + divider area)
  REAL, DIMENSION(24) :: OutProjSLFracMult  = 1.0 ! Multiplier on sunlit fraction due to shadowing of glass by frame
                                            ! and divider outside projections
  REAL, DIMENSION(24) :: InOutProjSLFracMult= 1.0 ! Multiplier on sunlit fraction due to shadowing of glass by frame
                                            ! and divider inside and outside projections
  REAL :: CenterGlArea            = 0.0 ! Center of glass area (m2); area of glass where 1-D conduction dominates
  REAL :: EdgeGlCorrFac           = 1.0 ! Correction factor to center-of-glass conductance to account for
                                             !  2-D glass conduction thermal bridging effects near frame and divider
  INTEGER :: OriginalClass             = 0   ! 0 or if entered originally as:
                                             ! Window - SurfaceClass_Window
                                             ! Glass Door - SurfaceClass_GlassDoor
                                             ! tubular daylighting device dome - SurfaceClass_TDD_Dome
                                             ! tubular daylighting device diffuser - SurfaceClass_TDD_Diffuser
  REAL :: ExtBeamAbsByShade       = 0.0 ! Exterior beam solar absorbed by window shade (W/m2)
  REAL :: ExtDiffAbsByShade       = 0.0 ! Exterior diffuse solar absorbed by window shade (W/m2)
  REAL :: IntBeamAbsByShade       = 0.0 ! Interior beam solar absorbed by window shade (W/m2)
  REAL :: IntSWAbsByShade         = 0.0 ! Interior diffuse solar plus short-wave from lights absorbed by window shade (W/m2)
  REAL :: InitialDifSolAbsByShade = 0.0 ! Initial diffuse solar from ext and int windows absorbed by window shade (W/m2)
  REAL :: IntLWAbsByShade         = 0.0 ! Interior long-wave from zone lights and equipment absorbed by window shade (W/m2)
  REAL, DIMENSION(2) :: ShadeAbsFacFace=0.5 ! Fraction of short-wave radiation incident on face 1 that is
                                            !  absorbed by face 1 and by the other face (face 2) when total absorbed
                                            !  radiation is apportioned to the two faces
  REAL :: ConvCoeffWithShade      = 0.0 ! Convection coefficient from glass or shade to gap air when
                                            !  interior or exterior shade is present (W/m2-K)
  REAL :: ConvHeatFlowNatural     = 0.0 ! Convective heat flow from gap between glass and interior shade or blind (W)
  REAL :: ConvHeatGainToZoneAir   = 0.0 ! Convective heat gain to zone air from window gap airflow (W)
  REAL :: RetHeatGainToZoneAir    = 0.0 ! Convective heat gain to return air sent to zone [W]
  REAL :: DividerConduction       = 0.0 ! Conduction through divider from outside to inside face (W)
  INTEGER :: BlindNumber              = 0   ! Blind number for a window with a blind
  REAL, DIMENSION(MaxSlatAngs) :: EffShBlindEmiss= 0.0 ! Effective emissivity of interior blind or shade
  REAL, DIMENSION(MaxSlatAngs) :: EffGlassEmiss  = 0.0 ! Effective emissivity of glass adjacent to interior blind or shade
  REAL :: EffInsSurfTemp              = 23.0 ! Effective inside surface temperature for window with interior blind or
                                            !  shade; combination of shade/blind and glass temperatures (C)
  LOGICAL :: MovableSlats         = .false. ! True if window has a blind with movable slats
  REAL :: SlatAngThisTS           = 0.0 ! Slat angle this time step for window with blind on (radians)
  REAL :: SlatAngThisTSDeg        = 0.0 ! Slat angle this time step for window with blind on (deg)
  Logical   :: SlatAngThisTSDegEMSon = .FALSE.  ! flag that indicate EMS system is actuating SlatAngThisTSDeg
  REAL :: SlatAngThisTSDegEMSValue = 0.0 ! value that EMS sets for slat angle in degrees
  LOGICAL :: SlatsBlockBeam       = .false. ! True if blind slats block incident beam solar
  REAL :: BlindAirFlowPermeability    = 0.0 ! Blind air-flow permeability for calculation of convective flow
                                            !  in gap between blind and glass
  REAL :: TotGlazingThickness     = 0.0 ! Total glazing thickness from outside of outer glass to inside of inner glass (m)
  REAL :: ProfileAngHor           = 0.0 ! Horizontal beam solar profile angle (degrees)
  REAL :: ProfileAngVert          = 0.0 ! Vertical beam solar profile angle (degrees)
  REAL :: TanProfileAngHor        = 0.0 ! Tangent of horizontal profile angle
  REAL :: TanProfileAngVert       = 0.0 ! Tangent of vertical profile angle
  REAL :: InsideSillDepth         = 0.0 ! Depth of inside sill (m)
  REAL :: InsideReveal            = 0.0 ! Depth of inside reveal (m)
  REAL :: InsideSillSolAbs        = 0.0 ! Solar absorptance of inside sill
  REAL :: InsideRevealSolAbs      = 0.0 ! Solar absorptance of inside reveal
  REAL :: OutsideRevealSolAbs     = 0.0 ! Solar absorptance of outside reveal
  REAL :: BmSolAbsdInsReveal      = 0.0 ! Multiplied by BeamSolarRad, gives beam solar absorbed
                                             ! by inside reveal surfaces (m2)
  REAL :: BmSolRefldInsReveal     = 0.0 ! Multiplied by BeamSolarRad, gives beam solar reflected
                                             ! by inside reveal surfaces (m2)
  REAL :: BmSolRefldInsRevealReport   = 0.0 ! Beam solar reflected by inside reveal surfaces, for reporting (W)
  REAL :: BmSolRefldOutsRevealReport  = 0.0 ! Beam solar reflected by outside reveal surfaces, for reporting (m2)
  REAL :: BmSolAbsdOutsReveal     = 0.0     ! Multiplied by BeamSolarRad, gives beam solar absorbed by
                                                 ! outside reveal surfaces (m2)
  REAL :: OutsRevealDiffOntoGlazing   = 0.0 ! Multiplied by BeamSolarRad, gives diffuse from beam reflection from
                                                 !    outside reveal that is incident on the glazing per m2 of glazing (-)
  REAL :: InsRevealDiffOntoGlazing    = 0.0 ! Multiplied by BeamSolarRad, gives diffuse from beam reflection
                                            !  from inside reveal that is incident on the glazing per m2 of glazing (-)
  REAL :: InsRevealDiffIntoZone   = 0.0 ! Multiplied by BeamSolarRad, gives diffuse from beam reflection
                                            !  from inside reveal that goes into zone directly or reflected from glazing (m2)
  REAL :: OutsRevealDiffOntoFrame = 0.0 ! Multiplied by BeamSolarRad, gives diffuse from beam reflection from outside reveal
                                            !   that is incident on the outside of the frame per m2 of frame (-)
  REAL :: InsRevealDiffOntoFrame  = 0.0 ! Multiplied by BeamSolarRad, gives diffuse from beam reflection from inside reveal
                                            !   that is incident on the outside of the frame per m2 of frame (-)

  ! added for debugging CR 7596. TH 5/26/2009
  REAL :: InsRevealDiffOntoGlazingReport = 0.0 ! Diffuse solar from beam reflection
                                                    !  from inside reveal that is incident on the glazing (W)
  REAL :: InsRevealDiffIntoZoneReport    = 0.0 ! Diffuse from beam reflection
                                                    !  from inside reveal that goes into zone directly or reflected from glazing (W)
  REAL :: InsRevealDiffOntoFrameReport   = 0.0 ! Diffuse from beam reflection from inside reveal
                                                    !  that is incident on the frame (W)
  REAL :: BmSolAbsdInsRevealReport       = 0.0 ! Beam solar absorbed by inside reveal (W)


  REAL :: BlTsolBmBm              = 0.0 ! Time-step value of blind beam-beam solar transmittance (-)
  REAL :: BlTsolBmDif             = 0.0 ! Time-step value of blind beam-diffuse solar transmittance (-)
  REAL :: BlTsolDifDif            = 0.0 ! Time-step value of blind diffuse-diffuse solar transmittance (-)
  REAL :: BlGlSysTsolBmBm         = 0.0 ! Time-step value of blind/glass system beam-beam solar transmittance (-)
  REAL :: BlGlSysTsolDifDif       = 0.0 ! Time-step value of blind/glass system diffuse-diffuse solar transmittance (-)
  INTEGER :: ScreenNumber             = 0   ! Screen number for a window with a screen (do not confuse with material number)
  REAL :: ScTsolBmBm              = 0.0 ! Time-step value of screen beam-beam solar transmittance (-)
  REAL :: ScTsolBmDif             = 0.0 ! Time-step value of screen beam-diffuse solar transmittance (-)
  REAL :: ScTsolDifDif            = 0.0 ! Time-step value of screen diffuse-diffuse solar transmittance (-)
  REAL :: ScGlSysTsolBmBm         = 0.0 ! Time-step value of screen/glass system beam-beam solar transmittance (-)
  REAL :: ScGlSysTsolDifDif       = 0.0 ! Time-step value of screen/glass system diffuse-diffuse solar transmittance (-)
  REAL :: GlTsolBmBm              = 0.0 ! Time-step value of glass beam-beam solar transmittance (-)
  REAL :: GlTsolBmDif             = 0.0 ! Time-step value of glass beam-diffuse solar transmittance (-)
  REAL :: GlTsolDifDif            = 0.0 ! Time-step value of glass diffuse-diffuse solar transmittance (-)
  INTEGER :: AirflowSource             = 0   ! Source of gap airflow (INSIDEAIR, OUTSIDEAIR, etc.)
  INTEGER :: AirflowDestination        = 0   ! Destination of gap airflow (INSIDEAIR, OUTSIDEAIR, etc.)
  REAL :: MaxAirflow              = 0.0 ! Maximum gap airflow (m3/s per m of glazing width)
  INTEGER :: AirflowControlType        = 0   ! Gap airflow control type (ALWAYSONATMAXFLOW, etc.)
  LOGICAL :: AirflowHasSchedule       = .FALSE. ! True if gap airflow is scheduled
  INTEGER :: AirflowSchedulePtr       = 0   ! Gap airflow schedule pointer
  REAL :: AirflowThisTS           = 0.0 ! Gap airflow this timestep (m3/s per m of glazing width)
  REAL    :: TAirflowGapOutlet    = 0.0 ! Temperature of air leaving airflow gap between glass panes (C)
  INTEGER :: WindowCalcIterationsRep  = 0   ! Number of iterations in window heat balance calculation
  REAL    :: BmSolTransThruIntWinRep  = 0.0 ! Beam solar transmitted through interior window [W]
  REAL    :: VentingOpenFactorRep = 0.0 ! Window/door venting open factor, for reporting
  REAL    :: VentingOpenFactorMultRep = 0.0 ! Window/door opening modulation multiplier on venting open factor, for reporting
  REAL    :: InsideTempForVentingRep = 0.0 ! Inside air temp used to control window/door venting, for reporting (C)
  REAL    :: VentingAvailabilityRep   = 0.0 ! Venting availability schedule value (0.0/1.0 = no venting allowed/not allowed)
  REAL    :: IllumFromWinAtRefPt1Rep   = 0.0 ! Illuminance from window at reference point #1 [lux]
  REAL    :: IllumFromWinAtRefPt2Rep   = 0.0 ! Illuminance from window at reference point #2 [lux]
  REAL    :: LumWinFromRefPt1Rep   = 0.0 ! Window luminance as viewed from reference point #1 [cd/m2]
  REAL    :: LumWinFromRefPt2Rep   = 0.0 ! Window luminance as viewed from reference point #2 [cd/m2]
  REAL    :: SkySolarInc           = 0.0 ! Incident diffuse solar from sky; if CalcSolRefl is true, includes
                                             ! reflection of sky diffuse and beam solar from exterior obstructions [W/m2]
  REAL    :: GndSolarInc           = 0.0 ! Incident diffuse solar from ground; if CalcSolRefl is true, accounts
                                             ! for shadowing of ground by building and obstructions [W/m2]
  REAL,DIMENSION(3) :: ZoneAreaMinusThisSurf = 0.0 ! Zone inside surface area minus this surface and its subsurfaces
                                             ! for floor/wall/ceiling (m2)
  REAL,DIMENSION(3) :: ZoneAreaReflProdMinusThisSurf   = 0.0 ! Zone product of inside surface area times vis reflectance
                                                                  ! minus this surface and its subsurfaces,
                                                                  ! for floor/wall/ceiling (m2)
  REAL    :: LightWellEff              = 1.0 ! Light well efficiency (multiplier on exterior window vis trans
                                                  !  due to light well losses)
  LOGICAL :: SolarDiffusing            = .false. ! True if exterior window with a construction that contains a
                                                 !  diffusing glass layer
  !energy
  REAL :: BmSolRefldInsRevealRepEnergy   = 0.0 ! energy of BmSolRefldInsRevealReport [J]
  REAL :: BmSolRefldOutsRevealRepEnergy  = 0.0 ! energy of BmSolRefldOutsRevealReport [J]
  REAL :: BmSolTransThruIntWinRepEnergy  = 0.0 ! energy of BmSolTransThruIntWinRep [J]

  ! Reporting
  REAL :: FrameHeatGain  =0.0
  REAL :: DividerHeatGain=0.0
  REAL :: FrameHeatLoss  =0.0
  REAL :: DividerHeatLoss=0.0

  ! Added TH for thermochromic windows. 12/22/2008
  REAL :: TCLayerTemp = 0.0      ! The temperature of the thermochromic layer of the window
  REAL :: SpecTemp = 0.0         ! The specification temperature of the TC layer glass

END TYPE SurfaceWindowCalc

TYPE FrameDividerProperties

  CHARACTER(len=MaxNameLength) :: Name = ' ' ! Name of frame/divider
  REAL :: FrameWidth              = 0.0 ! Average width of frame in plane of window {m}
  REAL :: FrameProjectionOut      = 0.0 ! Distance normal to window between outside face of outer pane
                                             !  and outside of frame {m}
  REAL :: FrameProjectionIn       = 0.0 ! Distance normal to window between inside face of inner pane
                                             !  and inside of frame {m}
  REAL :: FrameConductance        = 0.0 ! Effective conductance of frame (no air films) {W/m2-K}
  REAL :: FrameEdgeWidth          = 0.06355 ! default 2.5 in ! Width of glass edge region near frame {m}
  REAL :: FrEdgeToCenterGlCondRatio   = 1.0 ! Ratio of frame edge of glass conductance (without air films) to
                                                 ! center of glass conductance (without air films)
  REAL :: FrameSolAbsorp          = 0.0 ! Solar absorptance of frame corrected for self-shading
  REAL :: FrameVisAbsorp          = 0.0 ! Visible absorptance of frame corrected for self-shading
  REAL :: FrameEmis               = 0.9 ! Thermal emissivity of frame
  INTEGER   :: DividerType             = 0   ! Type of divider {DividedLite or Suspended (between-glass}
  REAL :: DividerWidth            = 0.0 ! Average width of divider in plane of window {m}
  INTEGER :: HorDividers               = 0   ! Number of horizontal dividers
  INTEGER :: VertDividers              = 0   ! Number of vertical dividers
  REAL :: DividerProjectionOut    = 0.0 ! Distance normal to window between outside face of outer pane
                                             !  and outside of divider {m}
  REAL :: DividerProjectionIn     = 0.0 ! Distance normal to window between inside face of inner pane
                                             !  and inside of divider {m}
  REAL :: DividerEdgeWidth        = 0.06355 ! default 2.5 in ! Width of glass edge region near divider
  REAL :: DividerConductance      = 0.0 ! Effective conductance of divider (no air films) {W/m2-K}
  REAL :: DivEdgeToCenterGlCondRatio  = 1.0 ! Ratio of divider edge of glass conductance (without air films) to
                                                 ! center of glass conductance (without air films)
  REAL :: DividerSolAbsorp        = 0.0 ! Solar absorptance of divider corrected for self-shading
  REAL :: DividerVisAbsorp        = 0.0 ! Visible absorptance of divider corrected for self-shading
  REAL :: DividerEmis             = 0.9 ! Thermal emissivity of divider
  INTEGER   :: MullionOrientation      = 0     ! Horizontal or Vertical; used only for windows with two glazing systems
                                                  !  divided by a mullion; obtained from Window5 data file.
  REAL :: OutsideRevealSolAbs     = 0.0 ! Solar absorptance of outside reveal
  REAL :: InsideSillDepth         = 0.0 ! Inside sill depth (m)
  REAL :: InsideReveal            = 0.0 ! Inside reveal (m)
  REAL :: InsideSillSolAbs        = 0.0 ! Solar absorptance of inside sill
  REAL :: InsideRevealSolAbs      = 0.0 ! Solar absorptance of inside reveal

END TYPE FrameDividerProperties

TYPE StormWindowData
  INTEGER :: BaseWindowNum              = 0   ! Surface number of associated exterior window
  INTEGER :: StormWinMaterialNum        = 0   ! Material number of storm window glass
  REAL    :: StormWinDistance       = 0.0 ! Distance between storm window glass and adjacent glass (m)
  INTEGER :: DateOn                     = 0   ! Date (julian) storm window is put on
  INTEGER :: MonthOn                    = 0   ! Month storm window is put on
  INTEGER :: DayOfMonthOn               = 0   ! Day of month storm window is put on
  INTEGER :: DateOff                    = 0   ! Date (julian) storm window is taken off
  INTEGER :: MonthOff                   = 0   ! Month storm window is taken off
  INTEGER :: DayOfMonthOff              = 0   ! Day of month storm window is taken off
END TYPE

TYPE WindowShadingControlData
  CHARACTER(len=MaxNameLength) :: Name= ' ' ! User supplied name of this set of shading control data
  INTEGER :: ShadingType   = WSC_ST_NoShade ! Shading type (InteriorShade, SwitchableGlazing,
!  CHARACTER(len=32) :: ShadingType    = ' ' ! Shading type (InteriorShade, SwitchableGlazing,
                                            !  ExteriorShade,InteriorBlind,ExteriorBlind,BetweenGlassShade,
                                            !  BetweenGlassBlind, or ExteriorScreen)
  INTEGER :: ShadedConstruction       = 0   ! Pointer to the shaded construction (for ShadingType=ExteriorScreen,InteriorShade,
                                            !  ExteriorShade,BetweenGlassShade,InteriorBlind,ExteriorBlind,BetweenGlassBlind;
                                            !  this must be a window construction with a screen, shade or blind layer)
  INTEGER :: ShadingDevice            = 0   ! Pointer to the material for the shading device (for ShadingType=InteriorShade,
                                            !  ExteriorShade,BetweenGlassShade,InteriorBlind,ExteriorBlind,BetweenGlassBlind,
                                            !  ExteriorScreen;
                                            !  this must be a Material:WindowShade, Material:WindowScreen, or Material:WindowBlind
  INTEGER :: ShadingControlType       = 0   ! Takes one of the following values that specifies type of shading control
!  CHARACTER(len=60) :: ShadingControlType =' ' ! Takes one of the following values that specifies type of shading control
                                            ! (control is active only when schedule value = 1; if no schedule
                                            ! specified, schedule value defaults to 1)
                                !  AlwaysOn: always shaded; not affected by schedule
                                !  AlwaysOff: never shaded; not affected by schedule
                                !  OnIfScheduleAllows: unshaded if sch val = 0, shaded if = 1
                                !  OnIfHighSolarOnWindow: shaded if incident direct + diffuse > setpoint (W/m2 of window)
                                !  OnIfHighHorizontalSolar: shaded if direct + diffuse horizontal solar > setpoint
                                !   (W/m2 of ground)
                                !  OnIfHighOutsideAirTemp: shaded if outside drybulb > setpoint (C)
                                !  OnIfHighZoneAirTemp: shaded if previous time step zone temperature > setpoint (C)
                                !  OnIfHighZoneCooling: shaded if previous time step zone cooling rate > setpoint (W)
                                !  OnIfHighGlare: shaded if total daylight glare index at first daylighting reference point
                                !   from all exterior windows in zone > maximum glare specified in daylighting
                                !   input for zone.
                                !  MeetDaylightIlluminanceSetpoint: shading is adjusted to just meet illuminance setpoint
                                !   at first reference point (only for ShadingType=SwitchableGlazing)
                                !       The following three controls are used primarily to reduce zone heating load. They
                                !       can be used with any shading type but are most appropriate for opaque interior
                                !       or exterior shades with a high insulating value ("opaque movable insulation").
                                !  OnNightIfLowOutsideTemp/OffDay: shaded at night if outside temp < setpoint (C)
                                !  OnNightIfLowInsideTemp/OffDay: shaded at night if previous time step zone air temp < setpoint (C)
                                !  OnNightIfHeating/OffDay: shaded  at night if previous time step zone heating rate > setpoint (W)
                                !       The following two controls are used to reduce zone heating and cooling loads.
                                !       They can be used with any shading type but are most appropriate for translucent
                                !       interior or exterior shades with a high insulating value ("translucent movable insulation")
                                !  OnNightIfLowOutsideTemp/OnDayIfCooling: shaded at night if outside temp < setpoint (C);
                                !                                         shaded daytime if prev. time step cooling rate > 0
                                !  OnNightIfHeating/OnDayIfCooling: shaded at night if prev. time step heating rate > setpoint (W);
                                !                                         shaded daytime if prev. time step cooling rate > 0
                                !       The following two controls are used to reduce zone cooling load. They can be used
                                !       with any shading type but are most appropriate for interior or exterior blinds, interior
                                !       or exterior shades with low insulating value, or switchable glazing.
                                !  OffNight/OnDayIfCoolingAndHighSolarOnWindow: shading off at night; shading on daytime if
                                !                                         solar on window > setpoint (W/m2 of window) and
                                !                                         prev. time step cooling rate > 0
                                !  OnNight/OnDayIfCoolingAndHighSolarOnWindow: shading on at night; shading on daytime if
                                !                                         solar on window > setpoint (W/m2 of window) and
                                !                                         prev. time step cooling rate > 0
  INTEGER  :: Schedule                = 0   ! Pointer to schedule of 0 and 1 values: 0 => window is not shaded;
                                            !  1 => window is shaded if Type=Schedule or Type = ScheduleAnd...
                                            ! and setpoint is exceeded.
  REAL     :: SetPoint            = 0.0 ! Control setpoint (dimension depends on Trigger:
                                            !  W/m2 of window area for solar on window,
                                            !  W/m2 of ground area for horizontal solar,
                                            !  deg C for air temp, W for zone heating and
                                            !  cooling rate). Not used for Shading Control Type =
                                            !  MeetDaylightIlluminanceSetpoint or OnIfHighGlare.
  REAL     :: SetPoint2           = 0.0 ! Second control setpoint for control types that take two setpoints.
                                            !   Dimension is deg C or W/m2.
  LOGICAL  :: ShadingControlIsScheduled = .false. ! True if shading control has a schedule
  LOGICAL  :: GlareControlIsActive  = .false. ! True if shading control to reduce daylight glare is active
  INTEGER  :: SlatAngleSchedule       = 0   ! Pointer to schedule of slat angle values between 0.0 and 180.0 degrees
  INTEGER  :: SlatAngleControlForBlinds = 0 ! Takes one of the following values that specifies
!  CHARACTER(len=32) :: SlatAngleControlForBlinds = ' ' ! Takes one of the following values that specifies
                         !  how slat angle is controled in a blind when ShadingType =
                         !  InteriorBlind, ExteriorBlind or BetweenGlassBlind.
                         !  FixedSlatAngle: the slat angle is fixed at the constant value given in the
                         !    associated Material:WindowBlind
                         !  ScheduledSlatAngle: the slat angle in degrees between 1 and 180 is given
                         !    by the schedule with index SlatAngleSchedule
                         !  BlockBeamSolar: if beam solar is incident on the window, and a blind is on the
                         !    window, the slat angle is adjusted to just block beam solar; otherwise the
                         !    slat angle is set to the value given in the associated Material:WindowBlind.
END TYPE

TYPE OSCData
  CHARACTER(len=MaxNameLength) :: Name = ' ' ! Name of OSC
  REAL :: ConstTemp        = 0.0 ! User selected constant temperature (degrees C)
  REAL :: ConstTempCoef    = 0.0 ! Coefficient modifying the user selected constant temperature
  REAL :: ExtDryBulbCoef   = 0.0 ! Coefficient modifying the external dry bulb temperature
  REAL :: GroundTempCoef   = 0.0 ! Coefficient modifying the ground temperature
  REAL :: SurfFilmCoef     = 0.0 ! Combined convective/radiative film coefficient if >0, else use other coefficients
  REAL :: WindSpeedCoef    = 0.0 ! Coefficient modifying the wind speed term (s/m)
  REAL :: ZoneAirTempCoef  = 0.0 ! Coefficient modifying the zone air temperature part of the equation
  CHARACTER(len=MaxNameLength) :: ConstTempScheduleName = ' ' ! Schedule name for scheduled outside temp
  INTEGER   :: ConstTempScheduleIndex = 0  ! Index for scheduled outside temp.
  LOGICAL   :: SinusoidalConstTempCoef = .FALSE. ! If true then ConstTempCoef varies by sine wave
  REAL :: SinusoidPeriod   = 0.0    ! period of sine wave variation  (hr)
  REAL :: TPreviousCoef    = 0.0    ! Coefficient modifying the OSC temp from the previous timestep (dimensionless)
  REAL :: TOutsideSurfPast = 0.0    ! Ouside surface temperature from previous timestep {C}
  REAL :: MinTempLimit     = 0.0    ! Minimum limit on OSC temp {deg C}
  REAL :: MaxTempLimit     = 0.0    ! Maximum limit on OSC temp {deg C}
  LOGICAL   :: MinLimitPresent  = .FALSE.  ! If TRUE then apply minimum limit on calculated OSC temp
  LOGICAL   :: MaxLimitPresent  = .FALSE.  ! If TRUE then apply maximum limit on calculated OSC temp
  REAL :: OSCTempCalc      = 0.0    ! Result of calculated temperature using OSC (degrees C)
END TYPE

TYPE OSCMData
  CHARACTER(len=MaxNameLength) :: Name  = ' ' ! Name of OSCM
  CHARACTER(len=MaxNameLength) :: Class = ' ' ! type of Model for OSCM
  REAL :: TConv     = 20.0 ! Temperature of bulk air at other side face (degrees C)
  LOGICAL   :: EMSOverrideOnTConv = .FALSE. ! if true then EMS calling for convection bulk air temp override
  REAL :: EMSOverrideTConvValue = 0. ! value for convection air temp when overridden
  REAL :: Hconv     = 4.0 ! Convection coefficient (W/m2-K)
  LOGICAL   :: EMSOverrideOnHConv = .FALSE. ! if true then EMS calling for convection coef override
  REAL :: EMSOverrideHConvValue = 0. ! value to use for convection coef when overridden
  REAL :: TRad      = 20.0 ! Effective temperature of surfaces exposed to other side face (degrees C)
  LOGICAL   :: EMSOverrideOnTRad = .FALSE. ! if true then EMS calling for radiation temp override
  REAL :: EMSOverrideTRadValue = 0. ! value to use for rad temp when overridden
  REAL :: Hrad      = 4.0 ! Linearized Radiation coefficient (W/m2-K)
  LOGICAL   :: EMSOverrideOnHrad = .FALSE. ! if true then EMS calling for radiation coef override
  REAL :: EMSOverrideHradValue = 0. ! value to use for rad coef when overridden
END TYPE

TYPE ConvectionCoefficient
  INTEGER :: WhichSurface  = 0   ! Which surface number this is applied to
  CHARACTER(len=MaxNameLength) :: SurfaceName = ' ' ! Which surface (name)
  INTEGER :: OverrideType  = 0   ! Override type, 1=value, 2=schedule, 3=model, 4=user curve
  REAL :: OverrideValue    = 0.0 ! User specified value
  CHARACTER(len=MaxNameLength) :: ScheduleName = ' ' ! Which surface (name)
  INTEGER :: ScheduleIndex = 0  ! if type="schedule" is used
  INTEGER :: UserCurveIndex =0  ! if type=UserCurve is used
  INTEGER :: HcModelEq=0        ! if type is one of specific model equations
END TYPE

TYPE ShadingVertexData
  INTEGER :: NVert
  REAL, ALLOCATABLE, DIMENSION(:)   :: XV     !
  REAL, ALLOCATABLE, DIMENSION(:)   :: YV     !
  REAL, ALLOCATABLE, DIMENSION(:)   :: ZV     !
END TYPE

TYPE ExtVentedCavityStruct
  ! from input data
  CHARACTER(len=MaxNameLength) :: Name             = ' ' !
  CHARACTER(len=MaxNameLength) :: OSCMName         = ' ' !OtherSideConditionsModel
  INTEGER                      :: OSCMPtr          = 0  ! OtherSideConditionsModel index
  REAL                    :: Porosity         = 0. ! fraction of absorber plate [--]
  REAL                    :: LWEmitt          = 0. ! Thermal Emissivity of Baffle Surface [dimensionless]
  REAL                    :: SolAbsorp        = 0. ! Solar Absorbtivity of Baffle Surface [dimensionless]
  INTEGER                      :: BaffleRoughness    = 1  ! surface roughness for exterior convection calcs.
  REAL                    :: PlenGapThick     = 0. ! Depth of Plenum Behind Baffle [m]
  INTEGER                      :: NumSurfs         = 0  ! a single baffle can have multiple surfaces underneath it
  INTEGER, ALLOCATABLE, DIMENSION(:) ::SurfPtrs    != 0  ! array of pointers for participating underlying surfaces
  REAL                    :: HdeltaNPL        = 0. ! Height scale for Cavity bouyancy  [m]
  REAL                    :: AreaRatio        = 0. ! Ratio of actual surface are to projected surface area [dimensionless]
  REAL                    :: Cv               = 0. ! volume-based effectiveness of openings for wind-driven vent when Passive
  REAL                    :: Cd               = 0. ! discharge coefficient of openings for bouyancy-driven vent when Passive
 ! data from elswhere and calculated
  REAL                    :: ActualArea       = 0. ! Overall Area of Collect with surface corrugations.
  REAL                    :: ProjArea         = 0. ! Overall Area of Collector projected, as if flat [m2]
  TYPE (vector)                :: Centroid         = vector(0.,0.,0.)  ! computed centroid
  REAL                    :: TAirCav          = 0. ! modeled drybulb temperature for air between baffle and wall [C]
  REAL                    :: Tbaffle          = 0. ! modeled surface temperature for baffle[C]
  REAL                    :: TairLast         = 20.0 ! Old Value for modeled drybulb temp of air between baffle and wall [C]
  REAL                    :: TbaffleLast      = 20.0 ! Old value for modeled surface temperature for baffle [C]
  REAL                    :: HrPlen           = 0  ! Modeled radiation coef for OSCM [W/m2-C]
  REAL                    :: HcPlen           = 0. ! Modeled Convection coef for OSCM [W/m2-C]
  REAL                    :: MdotVent         = 0. ! air mass flow exchanging with ambient when passive.
  REAL                    :: Tilt             = 0. ! Tilt from area weighted average of underlying surfaces
  REAL                    :: Azimuth          = 0. ! Azimuth from area weighted average of underlying surfaces
  REAL                    :: QdotSource       = 0. ! Source/sink term
  ! reporting data
  REAL                    :: Isc              = 0. ! total incident solar on baffle [W]
  REAL                    :: PassiveACH       = 0. ! air changes per hour when passive [1/hr]
  REAL                    :: PassiveMdotVent  = 0. ! Total Nat Vent air change rate  [kg/s]
  REAL                    :: PassiveMdotWind  = 0. ! Nat Vent air change rate from Wind-driven [kg/s]
  REAL                    :: PassiveMdotTherm = 0. ! Nat. Vent air change rate from bouyancy-driven flow [kg/s]
END TYPE ExtVentedCavityStruct

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
TYPE (SurfaceData),        ALLOCATABLE, DIMENSION(:) :: Surface
TYPE (SurfaceWindowCalc),  ALLOCATABLE, DIMENSION(:) :: SurfaceWindow
TYPE (FrameDividerProperties), ALLOCATABLE, DIMENSION(:) :: FrameDivider
TYPE (StormWindowData), ALLOCATABLE, DIMENSION(:) :: StormWindow
TYPE (WindowShadingControlData), ALLOCATABLE, DIMENSION(:) :: WindowShadingControl
TYPE (OSCData), ALLOCATABLE, DIMENSION(:) :: OSC
TYPE (OSCMData), ALLOCATABLE, DIMENSION(:) :: OSCM
TYPE (ConvectionCoefficient), ALLOCATABLE, DIMENSION(:) :: UserIntConvectionCoeffs
TYPE (ConvectionCoefficient), ALLOCATABLE, DIMENSION(:) :: UserExtConvectionCoeffs
TYPE (ShadingVertexData), ALLOCATABLE, DIMENSION(:) :: ShadeV
TYPE (ExtVentedCavityStruct), ALLOCATABLE, DIMENSION(:) :: ExtVentedCavity

INTEGER :: TotSurfaces          =0 ! Total number of surfaces (walls, floors, roofs, windows, shading surfaces, etc.--everything)
INTEGER :: TotWindows           =0 ! Total number of windows
INTEGER :: TotStormWin          =0 ! Total number of storm window blocks
INTEGER :: TotWinShadingControl =0 ! Total number of window shading control blocks
INTEGER :: TotIntConvCoeff      =0 ! Total number of interior convection coefficient (overrides)
INTEGER :: TotExtConvCoeff      =0 ! Total number of exterior convection coefficient (overrides)
INTEGER :: TotOSC               =0 ! Total number of Other Side Coefficient Blocks
INTEGER :: TotOSCM              =0 ! Total number of Other Side Conditions Model Blocks.
INTEGER :: TotExtVentCav        = 0
INTEGER :: Corner               =0 ! Which corner is specified as the first vertice
INTEGER :: MaxVerticesPerSurface = 4 ! Maximum number of vertices allowed for a single surface (default -- can go higher)

INTEGER :: BuildingShadingCount =0 ! Total number of Building External Shades
INTEGER :: FixedShadingCount    =0 ! Total number of Fixed External Shades
INTEGER :: AttachedShadingCount =0 ! Total number of Shades attached to Zones

LOGICAL :: AspectTransform=.false.      ! Set to true when GeometryTransform object is used
LOGICAL :: CalcSolRefl=.false.          !Set to true when Solar Reflection Calculations object is used
LOGICAL :: CCW                  =.false. ! True if vertices will be entered in CounterClockWise Order
LOGICAL :: WorldCoordSystem     =.false. ! True if vertices will be "World Coordinates"
                                         ! False means relative coordinates
LOGICAL :: DaylRefWorldCoordSystem     =.false. ! True if Daylight Reference Point vertices will be "World Coordinates"
                                                ! False means relative coordinates
INTEGER :: MaxRecPts=0                  ! Max number of receiving points on a surface for solar reflection calc
INTEGER :: MaxReflRays=0                ! Max number of rays from a receiving surface for solar reflection calc
REAL :: GroundLevelZ=0.0           ! Z value of ground level for solar refl calc (m)
LOGICAL :: AirflowWindows = .FALSE. ! TRUE if one or more airflow windows

LOGICAL :: ShadingTransmittanceVaries=.false.       ! overall, shading transmittance varies for the building

INTEGER,ALLOCATABLE,DIMENSION(:) :: InsideGlassCondensationFlag  ! 1 if innermost glass inside surface temp < zone air dew point;
                                                          ! 0 otherwise
INTEGER,ALLOCATABLE,DIMENSION(:) :: InsideFrameCondensationFlag  ! 1 if frame inside surface temp < zone air dew point;
                                                          ! 0 otherwise
INTEGER,ALLOCATABLE,DIMENSION(:) :: InsideDividerCondensationFlag  ! 1 if divider inside surface temp < zone air dew point;
                                                          ! 0 otherwise
INTEGER,ALLOCATABLE,DIMENSION(:) :: AdjacentZoneToSurface   ! Array of adjacent zones to each surface

REAL, ALLOCATABLE, DIMENSION(:) :: X0     ! X-component of translation vector
REAL, ALLOCATABLE, DIMENSION(:) :: Y0     ! Y-component of translation vector
REAL, ALLOCATABLE, DIMENSION(:) :: Z0     ! Z-component of translation vector
REAL, ALLOCATABLE, DIMENSION(:) :: DSZone ! Factor for sky diffuse solar radiation into a zone
REAL, ALLOCATABLE, DIMENSION(:) :: DGZone ! Factor for ground diffuse solar radiation into a zone
REAL, ALLOCATABLE, DIMENSION(:) :: DBZone ! Factor for diffuse radiation in a zone from
                                               ! beam reflecting from inside surfaces
REAL, ALLOCATABLE, DIMENSION(:) :: CBZone ! Factor for beam solar absorbed by interior shades
REAL, ALLOCATABLE, DIMENSION(:) :: AISurf ! Time step value of factor for beam
                                               ! absorbed on inside of opaque surface
REAL, ALLOCATABLE, DIMENSION(:) :: AOSurf ! Time step value of factor for beam
                                               ! absorbed on outside of opaque surface
REAL, ALLOCATABLE, DIMENSION(:) :: BmToBmReflFacObs   ! Factor for incident solar from specular beam refl
                                                      ! from obstructions (W/m2)/(W/m2)
REAL, ALLOCATABLE, DIMENSION(:) :: BmToDiffReflFacObs ! Factor for incident solar from diffuse beam refl
                                                      ! from obstructions (W/m2)/(W/m2)
REAL, ALLOCATABLE, DIMENSION(:) :: BmToDiffReflFacGnd ! Factor for incident solar from diffuse beam refl from ground

REAL, ALLOCATABLE, DIMENSION(:,:)   :: AWinSurf ! Time step value of factor for beam
                                                     ! absorbed in window glass layers

REAL, ALLOCATABLE, DIMENSION(:) :: AirSkyRadSplit ! Fractional split between the air and
                                                       ! the sky for radiation from the surface
                                         ! Fraction of sky IR coming from sky itself; 1-AirSkyRadSplit comes from the atmosphere.

REAL, ALLOCATABLE, DIMENSION(:) :: WinTransSolar  !Exterior beam plus diffuse solar transmitted through window, or
                                                       ! window plus shade/blind, into zone (W)
REAL, ALLOCATABLE, DIMENSION(:) :: WinBmSolar     !Exterior beam solar transmitted through window, or
                                                       ! window plus blind, into zone (W)

REAL, ALLOCATABLE, DIMENSION(:) :: WinBmBmSolar   !Exterior beam-to-beam solar transmitted through window, or
                                                       ! window plus blind, into zone (W)
REAL, ALLOCATABLE, DIMENSION(:) :: WinBmDifSolar  !Exterior beam-to-diffuse solar transmitted through window, or
                                                       ! window plus blind, into zone (W)

REAL, ALLOCATABLE, DIMENSION(:) :: WinDifSolar    !Exterior diffuse solar transmitted through window, or
                                                       ! window plus shade/blind, into zone (W)
REAL, ALLOCATABLE, DIMENSION(:) :: WinDirSolTransAtIncAngle  !Window's beam-beam solar transmittance at current timestep's
                                                       ! angle of incidence
REAL, ALLOCATABLE, DIMENSION(:) :: WinHeatGain    !Total heat gain from window = WinTransSolar + (IR and convection from
                                                       ! glazing, or, if interior shade, IR and convection from
                                                       ! zone-side of shade plus gap air convection to zone) + (IR and
                                                       ! convection from frame) + (IR and convection from divider if no
                                                       ! interior shade) (W)
REAL, ALLOCATABLE, DIMENSION(:) :: WinHeatGainRep      !Equals WinHeatGain when WinHeatGain >= 0.0
REAL, ALLOCATABLE, DIMENSION(:) :: WinHeatLossRep      !Equals -WinHeatGain when WinHeatGain < 0.0

REAL, ALLOCATABLE, DIMENSION(:) :: WinGainConvGlazToZoneRep ! component of WinHeatGain convect to zone from glazing (W)
REAL, ALLOCATABLE, DIMENSION(:) :: WinGainIRGlazToZoneRep   ! component of WinHeatGain net IR to zone from glazing (W)
REAL, ALLOCATABLE, DIMENSION(:) :: WinLossSWZoneToOutWinRep ! component of WinHeatGain shortwave transmit back out (W)
REAL, ALLOCATABLE, DIMENSION(:) :: WinGainFrameDividerToZoneRep ! component of WinHeatGain to zone from frame/divider (W)
REAL, ALLOCATABLE, DIMENSION(:) :: WinGainConvGlazShadGapToZoneRep ! component of WinHeatGain convection to zone from
                                                                     ! the gap between the inner most glazing and the shade   (W)
REAL, ALLOCATABLE, DIMENSION(:) :: WinGainConvShadeToZoneRep ! component of WinHeatGain convect to zone from front shade (W)
REAL, ALLOCATABLE, DIMENSION(:) :: WinGainIRShadeToZoneRep   ! component of WinHeatGain net IR to zone from front shade (W)

REAL, ALLOCATABLE, DIMENSION(:) :: WinGapConvHtFlowRep !Convective heat flow from gap in airflow window (W)
!REAL, ALLOCATABLE, DIMENSION(:) :: OpaqSurfInsFaceCondGainRep !Equals Opaq Surf Ins Face Cond
!                                                                   ! when Opaq Surf Ins Face Cond >= 0
!REAL, ALLOCATABLE, DIMENSION(:) :: OpaqSurfInsFaceCondLossRep !Equals -Opaq Surf Ins Face Cond
!                                                                   ! when Opaq Surf Ins Face Cond  < 0
REAL, ALLOCATABLE, DIMENSION(:) :: WinShadingAbsorbedSolar ! Exterior beam plus diffuse solar absorbed by
                                                                !  window shading device (W)
REAL, ALLOCATABLE, DIMENSION(:) :: WinSysSolTransmittance ! Effective solar transmittance of window + shading device,
                                                               ! if present
REAL, ALLOCATABLE, DIMENSION(:) :: WinSysSolReflectance   ! Effective solar reflectance of window + shading device,
                                                               ! if present
REAL, ALLOCATABLE, DIMENSION(:) :: WinSysSolAbsorptance   ! Effective solar absorptance of window + shading device,
                                                               ! if present
REAL,DIMENSION(3,24):: SUNCOSHR          ! Hourly values of SUNCOS (solar direction cosines)
REAL, ALLOCATABLE, DIMENSION(:,:) :: ReflFacBmToDiffSolObs
REAL, ALLOCATABLE, DIMENSION(:,:) :: ReflFacBmToDiffSolGnd
REAL, ALLOCATABLE, DIMENSION(:,:) :: ReflFacBmToBmSolObs
REAL, ALLOCATABLE, DIMENSION(:)   :: ReflFacSkySolObs
REAL, ALLOCATABLE, DIMENSION(:)   :: ReflFacSkySolGnd
REAL, ALLOCATABLE, DIMENSION(:,:) :: CosIncAveBmToBmSolObs
REAL, ALLOCATABLE, DIMENSION(:) :: DBZoneIntWin ! Value of factor for beam solar entering a zone through interior windows
                                                ! (considered to contribute to diffuse in zone)
REAL, ALLOCATABLE, DIMENSION(:) :: SurfSunlitArea  ! Sunlit area by surface number
REAL, ALLOCATABLE, DIMENSION(:) :: SurfSunlitFrac  ! Sunlit fraction by surface number
!energy
REAL, ALLOCATABLE, DIMENSION(:) :: WinTransSolarEnergy !Energy of WinTransSolar [J]
REAL, ALLOCATABLE, DIMENSION(:) :: WinBmSolarEnergy    !Energy of WinBmSolar [J]

REAL, ALLOCATABLE, DIMENSION(:) :: WinBmBmSolarEnergy   !Beam-to-beam energy of WinBmSolar [J]
REAL, ALLOCATABLE, DIMENSION(:) :: WinBmDifSolarEnergy  !Beam-to-diffuse energy of WinBmSolar [J]

REAL, ALLOCATABLE, DIMENSION(:) :: WinDifSolarEnergy    !Energy of WinDifSolar [J]
REAL, ALLOCATABLE, DIMENSION(:) :: WinHeatGainRepEnergy !Energy of WinHeatGainRep [J]
REAL, ALLOCATABLE, DIMENSION(:) :: WinHeatLossRepEnergy !Energy of WinHeatLossRep [J]
REAL, ALLOCATABLE, DIMENSION(:) :: WinShadingAbsorbedSolarEnergy !Energy of WinShadingAbsorbedSolar [J]
REAL, ALLOCATABLE, DIMENSION(:) :: WinGapConvHtFlowRepEnergy !Energy of WinGapConvHtFlowRep [J]

          ! SUBROUTINE SPECIFICATIONS FOR MODULE DataSurfaces:
PUBLIC cSurfaceClass

CONTAINS
FUNCTION cSurfaceClass(ClassNo) RESULT(ClassName)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns a string based on class number.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)            :: ClassNo
  CHARACTER(len=25)              :: ClassName

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  SELECT CASE (ClassNo)
    CASE(SurfaceClass_Wall)
      ClassName='Wall'

    CASE(SurfaceClass_Floor)
      ClassName='Floor'

    CASE(SurfaceClass_Roof)
      ClassName='Roof'

    CASE(SurfaceClass_Window)
      ClassName='Window'

    CASE(SurfaceClass_GlassDoor)
      ClassName='Glass Door'

    CASE(SurfaceClass_Door)
      ClassName='Door'

    CASE(SurfaceClass_TDD_Dome)
      ClassName='TubularDaylightDome'

    CASE(SurfaceClass_TDD_Diffuser)
      ClassName='TubularDaylightDiffuser'

    CASE(SurfaceClass_IntMass)
      ClassName='Internal Mass'

    CASE(SurfaceClass_Shading)
      ClassName='Shading'

    CASE(SurfaceClass_Detached_B)
      ClassName='Detached Shading:Building'

    CASE(SurfaceClass_Detached_F)
      ClassName='Detached Shading:Fixed'

    CASE DEFAULT
      ClassName='Invalid/Unknown'

  END SELECT

  RETURN

END FUNCTION cSurfaceClass

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


END MODULE DataSurfaces
