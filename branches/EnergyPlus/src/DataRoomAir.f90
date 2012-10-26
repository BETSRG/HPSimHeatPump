MODULE DataRoomAirModel     ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Weixiu Kong
          !       DATE WRITTEN   March 2003
          !       MODIFIED       July 2003, CC
          !                      Jan 2004, CC
          !                      Aug 2005, BG -- added structures for user-defined patterns
          !                      June 2008, BG -- revised for system time step history terms
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module contain global variables needed in air models

          ! USE STATEMENTS:                       ! UCSD
    USE DataPrecisionGlobals
    USE DataGlobals,        ONLY : MaxNameLength

    IMPLICIT NONE   ! Enforce explicit typing of all variables

    PUBLIC  ! By definition, all variables which are placed in this data-only
            ! module should be available to other modules and routines.  Thus,
            ! all variables in this module must be PUBLIC.

          ! MODULE PARAMETER DEFINITIONS
    CHARACTER(len=MaxNameLength), PARAMETER :: cUserDefinedControlObject       = &
                                                 'RoomAir:TemperaturePattern:UserDefined'
    CHARACTER(len=MaxNameLength), PARAMETER :: cTempPatternConstGradientObject = &
                                                 'RoomAir:TemperaturePattern:ConstantGradient'
    CHARACTER(len=MaxNameLength), PARAMETER :: cTempPatternTwoGradientObject   = &
                                                 'RoomAir:TemperaturePattern:TwoGradient'
    CHARACTER(len=MaxNameLength), PARAMETER :: cTempPatternNDHeightObject      = &
                                                 'RoomAir:TemperaturePattern:NondimensionalHeight'
    CHARACTER(len=MaxNameLength), PARAMETER :: cTempPatternSurfMapObject       = &
                                                 'RoomAir:TemperaturePattern:SurfaceMapping'


          ! Parameters to indicate room air model selected
    INTEGER, Parameter :: RoomAirModel_UserDefined = 1 ! user defined patterns
    INTEGER, PARAMETER :: RoomAirModel_Mixing    = 2 ! mixing air model
    INTEGER, PARAMETER :: RoomAirModel_Mundt     = 3 ! Mundt nodal model
    INTEGER, PARAMETER :: RoomAirModel_UCSDDV    = 4 ! UCSD Displacement Ventilation model
    INTEGER, PARAMETER :: RoomAirModel_UCSDCV    = 5 ! UCSD-CV
    INTEGER, PARAMETER :: RoomAirModel_UCSDUFI   = 6 ! UCSD UFAD interior zone model
    INTEGER, PARAMETER :: RoomAirModel_UCSDUFE   = 7 ! UCSD UFAD interior zone model
    CHARACTER(len=*), PARAMETER,   &
          DIMENSION(0:7) :: ChAirModel=(/'*Invalid*  ',  &
                                         'UserDefined', &
                                         'Mixing     ',  &
                                         'Mundt      ',  &
                                         'UCSD_DV    ',  &
                                         'UCSD_CV    ',  &
                                         'UCSD_UFI   ',  &
                                         'UCSD_UFE   ' /)

          ! Parameters to indicate air temperature coupling scheme
    INTEGER, PARAMETER :: DirectCoupling    = 1 ! direct coupling scheme
    INTEGER, PARAMETER :: IndirectCoupling  = 2 ! indirect coupling scheme

          ! Parameters to indicate type of air node, which is dependent on air models
    INTEGER, PARAMETER :: InletAirNode      = 0     ! air node at inlet (for Mundt and Rees&Haves Models)
    INTEGER, PARAMETER :: FloorAirNode      = 1     ! air node at floor (for Mundt and Rees&Haves Models)
    INTEGER, PARAMETER :: ControlAirNode    = 2     ! air node at control point (for Mundt Model)
    INTEGER, PARAMETER :: CeilingAirNode    = 3     ! air node at ceiling (for Mundt Model)
    INTEGER, PARAMETER :: MundtRoomAirNode  = 4     ! air node for vertical walls (for Mundt Model)
    INTEGER, PARAMETER :: ReturnAirNode     = 10    ! air node for return (for Mundt and Rees&Haves Models)
    INTEGER, PARAMETER :: PlumeAirNode1     = 2     ! air node for plume load (for Rees&Haves Model)
    INTEGER, PARAMETER :: PlumeAirNode2     = 3     ! air node for plume load (for Rees&Haves Model)
    INTEGER, PARAMETER :: PlumeAirNode3     = 4     ! air node for plume load (for Rees&Haves Model)
    INTEGER, PARAMETER :: PlumeAirNode4     = 5     ! air node for plume load (for Rees&Haves Model)
    INTEGER, PARAMETER :: RoomAirNode1      = 6     ! air node for vertical walls (for Rees&Haves Model)
    INTEGER, PARAMETER :: RoomAirNode2      = 7     ! air node for vertical walls (for Rees&Haves Model)
    INTEGER, PARAMETER :: RoomAirNode3      = 8     ! air node for vertical walls (for Rees&Haves Model)
    INTEGER, PARAMETER :: RoomAirNode4      = 9     ! air node for vertical walls (for Rees&Haves Model)

     ! user-defined pattern two gradient interplotation modes
    INTEGER, PARAMETER :: OutdoorDrybulbMode = 21 !by outdoor air bulb.
    INTEGER, PARAMETER :: SensibleCoolingMode = 22 !by sensible cooling load
    INTEGER, PARAMETER :: SensibleHeatingMode = 23 !by sensible heating load
    INTEGER, PARAMETER :: ZoneAirTempMode = 24 !by zone air temperature
    INTEGER, PARAMETER :: DeltaOutdoorZone = 25 !by difference between zone and outdoor

     ! user defined temperature pattern types
    INTEGER, PARAMETER :: ConstGradTempPattern  = 31 ! constant gradient in vertical direction
    INTEGER, PARAMETER :: TwoGradInterpPattern  = 32 ! two gradient interpolation
    INTEGER, PARAMETER :: NonDimenHeightPattern = 33 ! non-dimensionalized height
    INTEGER, PARAMETER :: SurfMapTempPattern    = 34 ! arbitrary surface mappings

          ! Parameters to indicate type of control for the UCSD UFAD interior zone model
    ! INTEGER, PARAMETER :: ConsFlow          = 1     ! constant supply air flow
    ! INTEGER, PARAMETER :: VarFlowConsPress  = 2     ! variable supply air flow, constant supply plenum pressure
    ! INTEGER, PARAMETER :: VarFlowVarPress   = 3     ! variable supply air flow, variable supply plenum pressure

    ! parameters to indicate diffuser type
    INTEGER, PARAMETER :: Swirl                 = 1
    INTEGER, PARAMETER :: VarArea               = 2
    INTEGER, PARAMETER :: DisplVent             = 3
    INTEGER, PARAMETER :: LinBarGrille          = 4
    INTEGER, PARAMETER :: Custom                = 5

    ! parameters for comfort calculations
    INTEGER, PARAMETER :: VComfort_Invalid       = -1
    INTEGER, PARAMETER :: VComfort_Jet           = 1
    INTEGER, PARAMETER :: VComfort_Recirculation = 2

          ! DERIVED TYPE DEFINITIONS

    TYPE AirModelData
        CHARACTER(Len=MaxNameLength) :: AirModelName     =' '
        CHARACTER(Len=MaxNameLength) :: ZoneName         =' '
        INTEGER                      :: ZonePtr          =0   ! Pointer to the zone number for this statement
        INTEGER                      :: AirModelType     =RoomAirModel_Mixing   ! 1 = Mixing, 2 = Mundt, 3 = Rees and Haves,
                                                                   ! 4 = UCSDDV, 5 = UCSDCV, -1 = user defined
                                                                   ! 6 = UCSDUFI
        INTEGER                      :: TempCoupleScheme =DirectCoupling   ! 1 = absolute (direct),
                                                              ! 2 = relative air model temperature passing scheme (indirect)
        LOGICAL                      :: SimAirModel      =.false. ! FALSE if Mixing air model is currently used and
                                                                     ! TRUE if other air models are currently used
    END TYPE AirModelData

    ! Air Node Data
    TYPE AirNodeData
        CHARACTER(len=MaxNameLength)   :: Name              =' ' !name
        CHARACTER(len=MaxNameLength)   :: ZoneName          =' '
        INTEGER                        :: ZonePtr           =0   ! Pointer to the zone number for this statement
        INTEGER                        :: ClassType         =0   !depending on type of model
        REAL                      :: Height            =0.0 !height
        LOGICAL, ALLOCATABLE, DIMENSION(:) :: SurfMask           !limit of 60 surfaces at current sizing
    END TYPE AirNodeData

    ! UCSD
    TYPE DVData
        CHARACTER(len=MaxNameLength)   :: ZoneName          =' ' ! Name of zone
        INTEGER                        :: ZonePtr           =0   ! Pointer to the zone number for this statement
        INTEGER                        :: SchedGainsPtr     =-1  ! Schedule for internal gain fraction to occupied zone
        CHARACTER(len=MaxNameLength)   :: SchedGainsName    =' ' ! Gains Schedule name
        REAL                      :: NumPlumesPerOcc   =1.0 ! Effective number of plumes per occupant
        REAL                      :: ThermostatHeight  =0.0 ! Height of thermostat/ temperature control sensor
        REAL                      :: ComfortHeight     =0.0 ! Height at which air temperature is measured for comfort purposes
        REAL                      :: TempTrigger       =0.0 ! Minimum temperature difference between TOC TMX for stratification
    END TYPE DVData

    TYPE CVData
        CHARACTER(len=MaxNameLength)   :: ZoneName          =' ' ! Name of zone
        INTEGER                        :: ZonePtr           =-1   ! Pointer to the zone number for this statement
        INTEGER                        :: SchedGainsPtr     =-1   ! Schedule for internal gain fraction to occupied zone
        CHARACTER(len=MaxNameLength)   :: SchedGainsName    =' ' ! Gains Schedule name
        INTEGER                        :: VforComfort       =VComfort_Invalid   ! Use Recirculation or Jet velocity and temperatures
                                                                 ! for comfort models
    END TYPE CVData

    TYPE CVTemp
        REAL                      :: In                =23.0
        REAL                      :: Out               =23.0
        REAL                      :: Med               =23.0
        REAL                      :: OutRoom           =23.0
    END TYPE CVTemp

    TYPE CVDVParameters
      REAL                        :: Width             =0.0
      REAL                        :: Height            =0.0
      Integer                          :: Shadow            =0
      REAL                        :: Zmin              =0.0
      REAL                        :: Zmax              =0.0
    END TYPE CVDVParameters

    TYPE UFIData
      CHARACTER(len=MaxNameLength)   :: ZoneName          =' '   ! Name of zone
      INTEGER    :: ZonePtr           =0     ! Pointer to the zone number for this statement
      INTEGER    :: ZoneEquipPtr      = 0    ! Pointer to zone equip for this UFAD zone
      REAL  :: DiffusersPerZone  =0.0   ! Number of diffusers in this zone
      REAL  :: PowerPerPlume     =0.0   ! Power in each plume [W]
      REAL  :: DiffArea          =0.0   ! Effective area of a diffuser [m2]
      REAL  :: DiffAngle         =0.0   ! angle between diffuser slots and vertical (degrees)
      REAL  :: HeatSrcHeight     =0.0   ! height of heat source above floor [m]
      REAL  :: ThermostatHeight  =0.0   ! Height of thermostat/ temperature control sensor [m]
      REAL  :: ComfortHeight     =0.0   ! Height at which air temperature is measured for
                                             ! comfort purposes [m]
      REAL  :: TempTrigger       =0.0   ! Minimum temperature difference between TOC TMX
                                             ! for stratification [deltaC]
      INTEGER    :: DiffuserType      =0     ! 1=Swirl, 2=variable area, 3=displacement, 4=linear bar grille, 5=custom
      REAL  :: TransHeight       =0.0   ! user specified transition height [m]
      LOGICAL    :: CalcTransHeight   =.FALSE. ! flag to calc trans height or use user specified input
      REAL  :: A_Kc              =0.0   ! Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
      REAL  :: B_Kc              =0.0   ! Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
      REAL  :: C_Kc              =0.0   ! Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
      REAL  :: D_Kc              =0.0   ! Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
      REAL  :: E_Kc              =0.0   ! Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
    END TYPE UFIData
    TYPE UFEData
      CHARACTER(len=MaxNameLength)   :: ZoneName          =' '   ! Name of zone
      INTEGER    :: ZonePtr           =0     ! Pointer to the zone number for this statement
      INTEGER    :: ZoneEquipPtr      = 0    ! Pointer to zone equip for this UFAD zone
      REAL  :: DiffusersPerZone  =0.0   ! Number of diffusers in this zone
      REAL  :: PowerPerPlume     =0.0   ! Power in each plume [W]
      REAL  :: DiffArea          =0.0   ! Effective area of a diffuser [m2]
      REAL  :: DiffAngle         =0.0   ! angle between diffuser slots and vertical (degrees)
      REAL  :: HeatSrcHeight     =0.0   ! height of heat source above floor [m]
      REAL  :: ThermostatHeight  =0.0   ! Height of thermostat/ temperature control sensor [m]
      REAL  :: ComfortHeight     =0.0   ! Height at which air temperature is measured for
                                             ! comfort purposes [m]
      REAL  :: TempTrigger       =0.0   ! Minimum temperature difference between TOC TMX
                                             ! for stratification [deltaC]
      INTEGER    :: DiffuserType      =0     ! 1=Swirl, 2=variable area, 3=displacement, 4=linear bar grille, 5=custom
      REAL  :: TransHeight       =0.0   ! user specified transition height [m]
      LOGICAL    :: CalcTransHeight   =.FALSE. ! flag to calc trans height or use user specified input
      REAL  :: A_Kc              =0.0   ! Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
      REAL  :: B_Kc              =0.0   ! Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
      REAL  :: C_Kc              =0.0   ! Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
      REAL  :: D_Kc              =0.0   ! Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
      REAL  :: E_Kc              =0.0   ! Coefficient A in Formula Kc = A*Gamma**B + C + D*Gamma + E*Gamma**2
      REAL  :: WinWidth          =0.0   ! sum of widths of exterior windows in zone
      REAL  :: NumExtWin         =0.0   ! number of exterior windows in the zone
      LOGICAL    :: ShadeDown         =.TRUE. ! signals shade up or down
    END TYPE UFEData
    ! END UCSD

    ! begin NREL RoomAir DERIVED TYPES ******************************************
    TYPE SurfMapPattern ! nested structure in RoomAirPattern
      ! user variables
      CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: SurfName  ! user defined name
      REAL, ALLOCATABLE, DIMENSION(:)     :: DeltaTai   ! (Tai - MAT ) offset from mean air temp
      INTEGER                             :: numSurfs = 0 ! number of surfaces in this pattern
      !calculated and from elsewhere
      INTEGER , ALLOCATABLE, DIMENSION(:) :: SurfID     ! index in HB surface structure array
    END TYPE SurfMapPattern

    Type ConstGradPattern ! nested structure in RoomAirPattern
      !user variables
      CHARACTER(len=MaxNameLength)   :: Name            =' ' !name
      REAL                      :: Gradient        = 0.0 ! value of vertical gradient [C/m]
    END TYPE ConstGradPattern

    TYPE TwoVertGradInterpolPattern ! nested structure in RoomAirPattern
      !user variables
      CHARACTER(len=MaxNameLength)   :: Name           =' ' !name
      REAL                      :: TstatHeight    = 0.0 ! Height of thermostat/ temperature control sensor
      REAL                      :: TleavingHeight = 0.0 ! height of return air node where leaving zone
      REAL                      :: TexhaustHeight = 0.0 ! height of exhaust air node where leaving zone
      REAL                      :: LowGradient    = 0.0 ! lower value of vertical gradient [C/m]
      REAL                      :: HiGradient     = 0.0 ! upper value of vertical gradient [C/m]
      INTEGER                        :: InterpolationMode = 0 ! control for interpolation mode
      REAL                      :: UpperBoundTempScale = 0.0 ! temperature value for HiGradient
      REAL                      :: LowerBoundTempScale = 0.0 ! temperature value for LowGradient
      REAL                      :: UpperBoundHeatRateScale = 0.0 ! load value for HiGradient
      REAL                      :: LowerBoundHeatRateScale = 0.0 ! load value for lowGradient
    END TYPE

    TYPE TempVsHeightPattern   ! to be used as nested structure in RoomAirPattern
      REAL, ALLOCATABLE, DIMENSION(:) :: ZetaPatrn      ! non dimensional height from floor,
      REAL, ALLOCATABLE, DIMENSION(:) :: DeltaTaiPatrn   ! Tai- MAT (TODO, check sign)
    END TYPE TempVsHeightPattern

    TYPE TemperaturePatternStruct !  RoomAirPattern
      CHARACTER(Len=MaxNameLength) :: Name          = ' ' ! unique identifier
      INTEGER                      :: PatrnID       = 0   ! control ID for referencing in Schedules
      INTEGER                      :: PatternMode   = 0   ! Control for what type of calcs in this pattern
      TYPE(ConstGradPattern)           :: GradPatrn    ! Constant gradient pattern
      TYPE(TwoVertGradInterpolPattern) :: TwoGradPatrn ! Two gradient interpolation pattern
      TYPE(TempVsHeightPattern)        :: VertPatrn    ! Vertical gradient profile pattern
      TYPE(SurfMapPattern )            :: MapPatrn     ! Generic Surface map pattern
      REAL                    :: DeltaTstat    = 0.0 ! (Tstat - MAT) offset   deg C
      REAL                    :: DeltaTleaving = 0.0 ! (Tleaving - MAT) deg C
      REAL                    :: DeltaTexhaust = 0.0 ! (Texhaust - MAT) deg C
    END TYPE TemperaturePatternStruct

    TYPE SurfaceAssocNestedStruct
      CHARACTER(Len=MaxNameLength) :: Name = ' ' ! unique identifier
      INTEGER   :: SurfID        = 0    ! id in HB surface structs
      REAL :: TadjacentAir  = 23.0  ! place to put resulting temperature value
      REAL :: Zeta          = 0.0  ! non-dimensional height in zone ot
    END TYPE SurfaceAssocNestedStruct


    TYPE AirPatternInfobyZoneStruct ! becomes AirPatternZoneInfo
      ! user variables
      Logical                      :: IsUsed       = .FALSE. !.true. if user-defined patterns used in zone
      CHARACTER(len=MaxNameLength) :: Name         = ' ' ! Name
      CHARACTER(len=MaxNameLength) :: ZoneName     = ' ' ! Zone name in building
      INTEGER                      :: ZoneID       = 0  ! Index of Zone in Heat Balance
      CHARACTER(len=MaxNameLength) :: AvailSched   = ' ' ! Name of availability schedule
      INTEGER                      :: AvailSchedID = 0  ! index of availability schedule
      CHARACTER(len=MaxNameLength) :: PatternCntrlSched = ' ' !name of schedule that selects pattern
      INTEGER                      :: PatternSchedID = 0 ! index of pattern selecting schedule
      !calculated and from elsewhere
      REAL                    :: ZoneHeight  = 0.0  ! in meters, from Zone%CeilingHeight
      INTEGER                      :: ReturnAirNodeID = 0 ! index in Node array
      INTEGER                      :: ZoneNodeID      = 0 ! index in Node array for this zone
      INTEGER, ALLOCATABLE, DIMENSION(:) :: ExhaustAirNodeID ! indexes in Node array
      REAL                    :: TairMean  = 23.0 ! comes from MAT
      REAL                    :: Tstat     = 23.0 ! temperature for thermostat
      REAL                    :: Tleaving  = 23.0 ! temperature for return air node
      REAL                    :: Texhaust  = 23.0 ! temperature for exhaust air node
      TYPE(SurfaceAssocNestedStruct) , ALLOCATABLE, DIMENSION(:) :: Surf ! nested struct w/ surface info
      INTEGER                      :: totNumSurfs = 0 ! total surfs for this zone
      INTEGER                      :: firstSurfID = 0 ! Index of first surface
      !report
      REAL                    :: Gradient = 0.0 ! result for modeled gradient if using two-gradient interpolation
    END TYPE AirPatternInfobyZoneStruct
    ! end NREL room air derived types*********************************



          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! MODULE VARIABLE DECLARATIONS:
    TYPE (AirModelData), ALLOCATABLE, DIMENSION(:) :: AirModel
    TYPE (AirNodeData), ALLOCATABLE,  DIMENSION(:) :: AirNode
    TYPE (DVData),      ALLOCATABLE,  DIMENSION(:) :: ZoneUCSDDV      ! UCSD
    TYPE (CVData),      ALLOCATABLE,  DIMENSION(:) :: ZoneUCSDCV
    TYPE (UFIData),     ALLOCATABLE, DIMENSION(:) :: ZoneUCSDUI
    TYPE (UFEData),     ALLOCATABLE, DIMENSION(:) :: ZoneUCSDUE
    INTEGER                               :: TotNumOfAirNodes     = 0
    INTEGER, ALLOCATABLE,  DIMENSION(:)   :: TotNumOfZoneAirNodes
    REAL, ALLOCATABLE,  DIMENSION(:)      :: ConvectiveFloorSplit
    REAL, ALLOCATABLE,  DIMENSION(:)      :: InfiltratFloorSplit
    ! UCSD
    REAL, ALLOCATABLE, DIMENSION  (:)       :: DVHcIn
    INTEGER                               :: TotUCSDDV            = 0 ! Total number of UCSDDV zones
    LOGICAL,ALLOCATABLE,DIMENSION(:)      :: IsZoneDV           ! Is the air model for the zone UCSDDV?
    REAL, ALLOCATABLE, DIMENSION(:)       :: ZTOC               ! Temperature of occupied (lower) zone
    REAL, ALLOCATABLE, DIMENSION(:)       :: AvgTempGrad        ! vertical Average Temperature Gradient in the room
    REAL, ALLOCATABLE, DIMENSION(:)       :: ZTMX               ! Temperature of the mixing(upper) layer
    REAL, ALLOCATABLE, DIMENSION(:)       :: MaxTempGrad        ! maximum Average Temperature Gradient in the room
    REAL, ALLOCATABLE, DIMENSION(:)       :: HVACAirTemp        ! HVAC system temperature (DEG C)
    REAL, ALLOCATABLE, DIMENSION(:)       :: HVACMassFlow       ! HVAC system mass flow rate (KG/S)
    REAL, ALLOCATABLE, DIMENSION(:)       :: ZTFLOOR
    REAL, ALLOCATABLE, DIMENSION(:)       :: HeightTransition
    REAL, ALLOCATABLE, DIMENSION(:)       :: FracMinFlow
    INTEGER, ALLOCATABLE, DIMENSION(:)    :: ZoneDVMixedFlag
    REAL, ALLOCATABLE, DIMENSION(:)       :: ZoneDVMixedFlagRep
    LOGICAL, ALLOCATABLE, DIMENSION(:)    :: ZoneAirSystemON
    REAL, ALLOCATABLE, DIMENSION(:)       :: TCMF               ! comfort temperature
    REAL, ALLOCATABLE, DIMENSION(:)       :: ZoneCeilingHeight
    REAL, ALLOCATABLE, DIMENSION(:)       :: MATFloor           ! [C] floor level mean air temp
    REAL, ALLOCATABLE, DIMENSION(:)       :: XMATFloor          ! [C] floor level mean air temp at t minus 1 zone time step
    REAL, ALLOCATABLE, DIMENSION(:)       :: XM2TFloor          ! [C] floor level mean air temp at t minus 2 zone time step
    REAL, ALLOCATABLE, DIMENSION(:)       :: XM3TFloor          ! [C] floor level mean air temp at t minus 3 zone time step
    REAL, ALLOCATABLE, DIMENSION(:)       :: XM4TFloor          ! [C] floor level mean air temp at t minus 4 zone time step
    REAL, ALLOCATABLE, DIMENSION(:)       :: DSXMATFloor        ! [C] floor level mean air temp at t minus 1 system time step
    REAL, ALLOCATABLE, DIMENSION(:)       :: DSXM2TFloor        ! [C] floor level mean air temp at t minus 2 system time step
    REAL, ALLOCATABLE, DIMENSION(:)       :: DSXM3TFloor        ! [C] floor level mean air temp at t minus 3 system time step
    REAL, ALLOCATABLE, DIMENSION(:)       :: DSXM4TFloor        ! [C] floor level mean air temp at t minus 4 system time step
    REAL, ALLOCATABLE, DIMENSION(:)       :: MATOC              ! [C] occupied mean air temp
    REAL, ALLOCATABLE, DIMENSION(:)       :: XMATOC             ! [C] occupied mean air temp at t minus 1 zone time step
    REAL, ALLOCATABLE, DIMENSION(:)       :: XM2TOC             ! [C] occupied mean air temp at t minus 2 zone time step
    REAL, ALLOCATABLE, DIMENSION(:)       :: XM3TOC             ! [C] occupied mean air temp at t minus 3 zone time step
    REAL, ALLOCATABLE, DIMENSION(:)       :: XM4TOC             ! [C] occupied mean air temp at t minus 4 zone time step
    REAL, ALLOCATABLE, DIMENSION(:)       :: DSXMATOC           ! [C] occupied mean air temp at t minus 1 system time step
    REAL, ALLOCATABLE, DIMENSION(:)       :: DSXM2TOC           ! [C] occupied mean air temp at t minus 2 system time step
    REAL, ALLOCATABLE, DIMENSION(:)       :: DSXM3TOC           ! [C] occupied mean air temp at t minus 3 system time step
    REAL, ALLOCATABLE, DIMENSION(:)       :: DSXM4TOC           ! [C] occupied mean air temp at t minus 4 system time step
    REAL, ALLOCATABLE, DIMENSION(:)       :: MATMX              ! [C] mixed (upper) mean air temp
    REAL, ALLOCATABLE, DIMENSION(:)       :: XMATMX             ! [C] mixed (upper) mean air temp at t minus 1 zone time step
    REAL, ALLOCATABLE, DIMENSION(:)       :: XM2TMX             ! [C] mixed (upper) mean air temp at t minus 2 zone time step
    REAL, ALLOCATABLE, DIMENSION(:)       :: XM3TMX             ! [C] mixed (upper) mean air temp at t minus 3 zone time step
    REAL, ALLOCATABLE, DIMENSION(:)       :: XM4TMX             ! [C] mixed (upper) mean air temp at t minus 4 zone time step
    REAL, ALLOCATABLE, DIMENSION(:)       :: DSXMATMX           ! [C] mixed  mean air temp at t minus 1 system time step
    REAL, ALLOCATABLE, DIMENSION(:)       :: DSXM2TMX           ! [C] mixed  mean air temp at t minus 2 system time step
    REAL, ALLOCATABLE, DIMENSION(:)       :: DSXM3TMX           ! [C] mixed  mean air temp at t minus 3 system time step
    REAL, ALLOCATABLE, DIMENSION(:)       :: DSXM4TMX           ! [C] mixed  mean air temp at t minus 4 system time step
    REAL, ALLOCATABLE, DIMENSION(:)       :: ZTM1Floor          ! [C] difference equation's Floor air temp at t minus 1
    REAL, ALLOCATABLE, DIMENSION(:)       :: ZTM2Floor          ! [C] difference equation's Floor air temp at t minus 2
    REAL, ALLOCATABLE, DIMENSION(:)       :: ZTM3Floor          ! [C] difference equation's Floor air temp at t minus 3
    REAL, ALLOCATABLE, DIMENSION(:)       :: ZTM1OC             ! [C] difference equation's Occupied air temp at t minus 1
    REAL, ALLOCATABLE, DIMENSION(:)       :: ZTM2OC             ! [C] difference equation's Occupied air temp at t minus 2
    REAL, ALLOCATABLE, DIMENSION(:)       :: ZTM3OC             ! [C] difference equation's Occupied air temp at t minus 3
    REAL, ALLOCATABLE, DIMENSION(:)       :: ZTM1MX             ! [C] difference equation's Mixed  air temp at t minus 1
    REAL, ALLOCATABLE, DIMENSION(:)       :: ZTM2MX             ! [C] difference equation's Mixed  air temp at t minus 1
    REAL, ALLOCATABLE, DIMENSION(:)       :: ZTM3MX             ! [C] difference equation's Mixed  air temp at t minus 1
    REAL, ALLOCATABLE, DIMENSION(:)       :: AIRRATFloor
    REAL, ALLOCATABLE, DIMENSION(:)       :: AIRRATOC
    REAL, ALLOCATABLE, DIMENSION(:)       :: AIRRATMX
    ! Euler and Exact solution algorithms
    REAL, ALLOCATABLE, DIMENSION(:)       :: Zone1Floor         ! [C] difference equation's Floor air temp at previous dt
    REAL, ALLOCATABLE, DIMENSION(:)       :: ZoneMXFloor        ! [C] difference equation's Floor air temp at t minus 1
    REAL, ALLOCATABLE, DIMENSION(:)       :: ZoneM2Floor        ! [C] difference equation's Floor air temp at t minus 2
    REAL, ALLOCATABLE, DIMENSION(:)       :: Zone1OC            ! [C] difference equation's Occupied air temp at previous dt
    REAL, ALLOCATABLE, DIMENSION(:)       :: ZoneMXOC           ! [C] difference equation's Occupied air temp at t minus 1
    REAL, ALLOCATABLE, DIMENSION(:)       :: ZoneM2OC           ! [C] difference equation's Occupied air temp at t minus 2
    REAL, ALLOCATABLE, DIMENSION(:)       :: Zone1MX            ! [C] difference equation's Mixed  air temp at previous dt
    REAL, ALLOCATABLE, DIMENSION(:)       :: ZoneMXMX           ! [C] difference equation's Mixed  air temp at t minus 1
    REAL, ALLOCATABLE, DIMENSION(:)       :: ZoneM2MX           ! [C] difference equation's Mixed  air temp at t minus 2
    ! UCSD-CV
    REAL, ALLOCATABLE, DIMENSION  (:)     :: CVHcIn
    INTEGER                               :: TotUCSDCV           =0 ! Total number of UCSDDV zones
    LOGICAL,ALLOCATABLE,DIMENSION(:)      :: IsZoneCV            ! Is the air model for the zone UCSDDV?
    REAL,ALLOCATABLE,DIMENSION(:)         :: ZoneCVisMixing      ! Zone set to CV is actually using a mixing model
    TYPE (CVTemp), ALLOCATABLE, DIMENSION(:) :: ZTJET        ! Jet Temperatures
    TYPE (CVTemp), ALLOCATABLE, DIMENSION(:) :: ZTREC        ! Recirculation Temperatures
    REAL, ALLOCATABLE, DIMENSION(:)       :: JetRecAreaRatio
    REAL, ALLOCATABLE, DIMENSION(:)       :: UPsOFo
    REAL, ALLOCATABLE, DIMENSION(:)       :: Urec                ! Recirculation region average velocity
    REAL, ALLOCATABLE, DIMENSION(:)       :: Ujet                ! Jet region average velocity
    REAL, ALLOCATABLE, DIMENSION(:)       :: Uhc
    REAL, ALLOCATABLE, DIMENSION(:)       :: Ain                 ! Inflow aperture area
    REAL, ALLOCATABLE, DIMENSION(:)       :: Lroom               ! CV Zone average length
    REAL, ALLOCATABLE, DIMENSION(:)       :: Tin                 ! Inflow air temperature
    INTEGER, ALLOCATABLE, DIMENSION(:,:)  :: AirflowNetworkSurfaceUCSDCV  ! table for AirflowNetwork surfaces organization
    TYPE (CVDVParameters), ALLOCATABLE,   &
                       DIMENSION(:)       :: SurfParametersCVDV ! Surface parameters
    INTEGER                               :: CVNumAirflowNetworkSurfaces =0 ! total number of AirFlowNetwork surfaces.
                                                                   ! Interzone surfaces counts twice.
    REAL,ALLOCATABLE, DIMENSION(:)        :: Rfr                ! Ration between inflow and recirculation air flows
    REAL,ALLOCATABLE, DIMENSION(:)        :: ZoneCVhasREC       ! Airflow pattern is C(0), CR(1)
    INTEGER, ALLOCATABLE, DIMENSION(:)    :: IsWidth            ! '1' means CV aperture width < aperture heigth
    LOGICAL                               :: UCSDModelUsed = .false.
    LOGICAL                               :: MundtModelUsed = .false.
    ! UCSD-UF
    INTEGER                               :: TotUCSDUI           =0 ! total number of UCSDUI zones
    INTEGER                               :: TotUCSDUE           =0 ! total number of UCSDUE zones
    LOGICAL,ALLOCATABLE,DIMENSION(:)      :: IsZoneUI               ! controls program flow, for interior or exterior UFAD model
    INTEGER,ALLOCATABLE,DIMENSION(:)      :: ZoneUFPtr
    REAL, ALLOCATABLE, DIMENSION  (:)     :: UFHcIn
    INTEGER, ALLOCATABLE, DIMENSION(:)    :: ZoneUFMixedFlag
    REAL, ALLOCATABLE, DIMENSION(:)       :: ZoneUFMixedFlagRep
    REAL, ALLOCATABLE, DIMENSION(:)       :: ZoneUFGamma
    REAL, ALLOCATABLE, DIMENSION(:)       :: ZoneUFPowInPlumes  ! [W]
    REAL, ALLOCATABLE, DIMENSION(:)       :: ZoneUFPowInPlumesfromWindows  ! [W]
    REAL, ALLOCATABLE, DIMENSION(:)       :: Phi                ! dimensionless measure of occupied subzone temperature

    ! END UCSD

    ! Begin NREL User-defined patterns
    TYPE(TemperaturePatternStruct), DIMENSION(:),              &
                              ALLOCATABLE :: RoomAirPattern  ! user defined patterns ,various types

    TYPE(AirPatternInfobyZoneStruct), DIMENSION(:),             &
                              ALLOCATABLE :: AirPatternZoneInfo !added zone information for user defined patterns
    INTEGER        :: numTempDistContrldZones =0 !count of zones with user-defined patterns
    INTEGER        :: NumAirTempPatterns  =0 !count of all different patterns in input file
    INTEGER        :: NumConstantGradient =0 !count of constant gradient patterns in input
    INTEGER        :: NumTwoGradientInterp=0  !count of two gradient interp patterns in input
    INTEGER        :: NumNonDimensionalHeight=0  !count of ND height profile patterns in input
    INTEGER        :: NumSurfaceMapping  =0 ! count of generic surface map patterns in input

    LOGICAL        :: UserDefinedUsed    = .false. ! true if user-defined model used anywhere
    ! End User-defined patterns


!**********************************************************************************************

!     NOTICE
!
!     Copyright � 1996-2012 The Board of Trustees of the University of Illinois
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
END MODULE DataRoomAirModel
