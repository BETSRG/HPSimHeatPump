MODULE DataSizing    ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   December 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This data-only module contains type definitions and variables
          ! associated with HVAC system design flow rates, temperatures and
          ! capacities. This data is available to the HVAC component modules
          ! for their self sizing calculations.

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.

          ! MODULE PARAMETER DEFINITIONS:

! parameters for outside air flow method
INTEGER, PARAMETER :: NumOAFlowMethods      = 6

INTEGER, PARAMETER :: OAFlowPPer            = 1
INTEGER, PARAMETER :: OAFlow                = 2
INTEGER, PARAMETER :: OAFlowPerArea         = 3
INTEGER, PARAMETER :: OAFlowACH             = 4
INTEGER, PARAMETER :: OAFlowSum             = 5
INTEGER, PARAMETER :: OAFlowMax             = 6

CHARACTER(len=*), PARAMETER, DIMENSION(NumOAFlowMethods) :: cOAFlowMethodTypes=  &
         (/'Flow/Person    ',  &
           'Flow/Zone      ',  &
           'Flow/Area      ',  &
           'AirChanges/Hour',  &
           'Sum            ',  &
           'Maximum        '/)

! parameters for outside air
INTEGER, PARAMETER :: AllOA                 = 1
INTEGER, PARAMETER :: MinOA                 = 2

! parameters for loop fluid type
INTEGER, PARAMETER :: HeatingLoop           = 1
INTEGER, PARAMETER :: CoolingLoop           = 2
INTEGER, PARAMETER :: CondenserLoop         = 3
INTEGER, PARAMETER :: SteamLoop             = 4

! paramters for sizing
INTEGER, PARAMETER :: NonCoincident         = 1
INTEGER, PARAMETER :: Coincident            = 2

! paramters for sizing
INTEGER, PARAMETER :: FromDDCalc            = 1
INTEGER, PARAMETER :: InpDesAirFlow         = 2
INTEGER, PARAMETER :: DesAirFlowWithLim     = 3

! parameters for Type of Load to Size On
INTEGER, PARAMETER :: Sensible              = 0
INTEGER, PARAMETER :: Latent                = 1
INTEGER, PARAMETER :: Total                 = 2
INTEGER, PARAMETER :: Ventilation           = 3

! parameter for autosize
REAL, PARAMETER :: AutoSize            = -99999.0

! parameter for (time-of-peak) sizing format
CHARACTER(len=*), PARAMETER :: PeakHrMinFmt = "(I2.2,':',I2.2,':00')"

          ! DERIVED TYPE DEFINITIONS:
TYPE ZoneSizingInputData
  CHARACTER &
    (len=MaxNameLength) :: ZoneName                 = ' '     ! name of a zone
  REAL             :: CoolDesTemp              = 0.0   ! zone design cooling supply air temperature [C]
  REAL             :: HeatDesTemp              = 0.0   ! zone design heating supply air temperature [C]
  REAL             :: CoolDesHumRat            = 0.0   ! zone design cooling supply air humidity ratio [kg-H2O/kg-air]
  REAL             :: HeatDesHumRat            = 0.0   ! zone design heating supply air humidity ratio [kg-H2O/kg-air]
  CHARACTER &
    (len=MaxNameLength) :: DesignSpecOAObjName      = ' '     ! name of the design specification outdoor air object
  INTEGER               :: OADesMethod              = 0       ! choice of how to calculate minimum outside air;
                                                              !  1 = m3/s per person; 2 = m3/s per zone; 3 = m3/s per zone area;
                                                              !  4 = sum of flow from 3 OA input fields;
                                                              !  5 = max of flow from 3 OA input fields
  REAL             :: DesOAFlowPPer            = 0.0   ! design outside air flow per person in zone [m3/s]
  REAL             :: DesOAFlowPerArea         = 0.0   ! design outside air flow per zone area [m3/s / m2]
  REAL             :: DesOAFlow                = 0.0   ! design outside air flow for the zone [m3/s]
  INTEGER               :: CoolAirDesMethod         = 0       ! choice of how to get zone cooling design air flow rates;
                                                              !  1 = calc from des day simulation; 2 = m3/s per zone, user input
                                                              !  3 = apply limits to air flow rate from DD calc
  REAL             :: DesCoolAirFlow           = 0.0   ! design zone supply air flow rate [m3/s]
  REAL             :: DesCoolMinAirFlowPerArea = 0.0   ! design cooling minimum air flow rate per zone area [m3/s / m2]
  REAL             :: DesCoolMinAirFlow        = 0.0   ! design cooling minimum air flow rate [m3/s]
  REAL             :: DesCoolMinAirFlowFrac    = 0.0   ! design cooling minimum air flow rate fraction
                                                              !  (of the cooling design air flow rate)
  INTEGER               :: HeatAirDesMethod         = 0       ! choice of how to get zone heating design air flow rates;
                                                              !  1 = calc from des day simulation; 2 = m3/s per zone, user input
                                                              !  3 = apply limits to air flow rate from DD calc
  REAL             :: DesHeatAirFlow           = 0.0   ! design zone heating supply air flow rate [m3/s]
  REAL             :: DesHeatMaxAirFlowPerArea = 0.0   ! design heating maximum air flow rate per zone area [m3/s / m2]
  REAL             :: DesHeatMaxAirFlow        = 0.0   ! design heating maximum air flow rate [m3/s]
  REAL             :: DesHeatMaxAirFlowFrac    = 0.0   ! design heating maximum air flow rate fraction
                                                              !  (of the cooling design air flow rate)
  REAL             :: HeatSizingFactor             = 0.0   ! the zone heating sizing ratio
  REAL             :: CoolSizingFactor             = 0.0   ! the zone cooling sizing ratio
  REAL             :: ZoneADEffCooling         = 1.0
  REAL             :: ZoneADEffHeating         = 1.0
  CHARACTER &
    (len=MaxNameLength) :: ZoneAirDistEffObjName      = ' '     ! name of the zone air distribution effectiveness object name
  INTEGER               :: ZoneAirDistributionIndex     = 0   ! index to the zone air distribution object
  INTEGER               :: ZoneDesignSpecOAIndex        = 0   ! index to the zone design spec OA object
  REAL             :: ZoneSecondaryRecirculation   = 0.0   ! the zone secondary air recirculation fraction
END TYPE ZoneSizingInputData

TYPE ZoneSizingData
  CHARACTER &
    (len=MaxNameLength) :: ZoneName                 = ' '     ! name of a zone
  CHARACTER &
    (len=MaxNameLength) :: CoolDesDay               = ' '     ! name of a cooling design day
  CHARACTER &
    (len=MaxNameLength) :: HeatDesDay               = ' '     ! name of a heating design day
  REAL             :: CoolDesTemp              = 0.0   ! zone design cooling supply air temperature [C]
  REAL             :: HeatDesTemp              = 0.0   ! zone design heating supply air temperature [C]
  REAL             :: CoolDesHumRat            = 0.0   ! zone design cooling supply air humidity ratio [kg-H2O/kg-air]
  REAL             :: HeatDesHumRat            = 0.0   ! zone design heating supply air humidity ratio [kg-H2O/kg-air]
  INTEGER               :: OADesMethod              = 0       ! choice of how to calculate minimum outside air;
                                                              !  1 = m3/s per person; 2 = m3/s per zone; 3 = m3/s per zone area;
                                                              !  4 = sum of flow from 3 OA input fields;
                                                              !  5 = max of flow from 3 OA input fields
  REAL             :: DesOAFlowPPer            = 0.0   ! design outside air flow per person in zone [m3/s]
  REAL             :: DesOAFlowPerArea         = 0.0   ! design outside air flow per zone area [m3/s / m2]
  REAL             :: DesOAFlow                = 0.0   ! design outside air flow for the zone [m3/s]
  INTEGER               :: CoolAirDesMethod         = 0       ! choice of how to get zone cooling design air flow rates;
                                                              !  1 = calc from des day simulation; 2 = m3/s per zone, user input
                                                              !  3 = apply limits to air flow rate from DD calc
  REAL             :: InpDesCoolAirFlow        = 0.0   ! design zone supply air flow rate [m3/s]
  REAL             :: DesCoolMinAirFlowPerArea = 0.0   ! design cooling minimum air flow rate per zone area [m3/s / m2]
  REAL             :: DesCoolMinAirFlow        = 0.0   ! design cooling minimum air flow rate [m3/s]
  REAL             :: DesCoolMinAirFlowFrac    = 0.0   ! design cooling minimum air flow rate fraction
                                                              !  (of the cooling design air flow rate)
  INTEGER               :: HeatAirDesMethod         = 0       ! choice of how to get zone heating design air flow rates;
                                                              !  1 = calc from des day simulation; 2 = m3/s per zone, user input
                                                              !  3 = apply limits to air flow rate from DD calc
  REAL             :: InpDesHeatAirFlow        = 0.0   ! design zone heating supply air flow rate [m3/s]
  REAL             :: DesHeatMaxAirFlowPerArea = 0.0   ! design heating maximum air flow rate per zone area [m3/s / m2]
  REAL             :: DesHeatMaxAirFlow        = 0.0   ! design heating maximum air flow rate [m3/s]
  REAL             :: DesHeatMaxAirFlowFrac    = 0.0   ! design heating maximum air flow rate fraction
                                                              !  (of the cooling design air flow rate)
  REAL             :: HeatSizingFactor             = 0.0   ! the zone heating sizing ratio
  REAL             :: CoolSizingFactor             = 0.0   ! the zone cooling sizing ratio
  INTEGER               :: ActualZoneNum            = 0       ! index into the Zone data array (in DataHeatBalance)
  INTEGER               :: SupplyAirNode            = 0       ! node number of supply air node

  REAL             :: DesHeatMassFlow          = 0.0   ! zone design heating air mass flow rate [kg/s]
  LOGICAL               :: EMSOverrideDesHeatMassOn = .FALSE. ! true if EMS is acting on this structure
  REAL             :: EMSValueDesHeatMassFlow  = 0.0   ! Value EMS directing to use for Design Heating air mass flow [kg/s]

  REAL             :: DesCoolMassFlow          = 0.0   ! zone design cooling air mass flow rate [kg/s]
  LOGICAL               :: EMSOverrideDesCoolMassOn = .FALSE. ! true if EMS is acting on this structure
  REAL             :: EMSValueDesCoolMassFlow  = 0.0   ! Value EMS directing to use for Design Cooling air mass flow [kg/s]

  REAL             :: DesHeatLoad              = 0.0   ! zone design heating load [W]
  LOGICAL               :: EMSOverrideDesHeatLoadOn = .FALSE. ! true if EMS is acting on this structure
  REAL             :: EMSValueDesHeatLoad      = 0.0   ! Value EMS directing to use for zone design heating load  [W]

  REAL             :: DesCoolLoad              = 0.0   ! zone design cooling load [W]
  LOGICAL               :: EMSOverrideDesCoolLoadOn = .FALSE. ! true if EMS is acting on this structure
  REAL             :: EMSValueDesCoolLoad      = 0.0   ! Value EMS directing to use for zone design cooling load  [W]

  REAL             :: DesHeatDens              = 0.0   ! zone design heating air density [kg/m3]
  REAL             :: DesCoolDens              = 0.0   ! zone design cooling air density [kg/m3]

  REAL             :: DesHeatVolFlow           = 0.0   ! zone design heating air volume flow rate [m3/s]
  LOGICAL               :: EMSOverrideDesHeatVolOn  = .FALSE. ! true if EMS is acting on this structure
  REAL             :: EMSValueDesHeatVolFlow   = 0.0   ! Value EMS directing to use for Design Heating air volume flow [m3/s]

  REAL             :: DesCoolVolFlow           = 0.0   ! zone design cooling air volume flow rate [m3/s]
  LOGICAL               :: EMSOverrideDesCoolVolOn  = .FALSE. ! true if EMS is acting on this structure
  REAL             :: EMSValueDesCoolVolFlow   = 0.0   ! Value EMS directing to use for Design cooling air volume flow [m3/s]

  REAL             :: DesHeatVolFlowMax        = 0.0   ! zone design heating maximum air volume flow rate [m3/s]
  REAL             :: DesCoolVolFlowMin        = 0.0   ! zone design cooling minimum air volume flow rate [m3/s]
  REAL             :: DesHeatCoilInTemp        = 0.0   ! zone heating coil design air inlet temperature [C]
  REAL             :: DesCoolCoilInTemp        = 0.0   ! zone cooling coil design air inlet temperature [C]
  REAL             :: DesHeatCoilInHumRat      = 0.0   ! zone heating coil design air inlet humidity ratio [kg/kg]
  REAL             :: DesCoolCoilInHumRat      = 0.0   ! zone cooling coil design air inlet humidity ratio [kg/kg]
  REAL             :: DesHeatCoilInTempTU      = 0.0   ! zone heating coil design air inlet temperature (supply air)([C]
  REAL             :: DesCoolCoilInTempTU      = 0.0   ! zone cooling coil design air inlet temperature (supply air)[C]
  REAL             :: DesHeatCoilInHumRatTU    = 0.0   ! zone heating coil design air inlet humidity ratio
                                                              !  (supply air) [kg/kg]
  REAL             :: DesCoolCoilInHumRatTU    = 0.0   ! zone cooling coil design air inlet humidity ratio
                                                              !  (supply air) [kg/kg]
  REAL             :: HeatMassFlow             = 0.0   ! current zone heating air mass flow rate (HVAC time step)
  REAL             :: CoolMassFlow             = 0.0   ! current zone cooling air mass flow rate (HVAC time step)
  REAL             :: HeatLoad                 = 0.0   ! current zone heating load (HVAC time step)
  REAL             :: CoolLoad                 = 0.0   ! current zone heating load (HVAC time step)
  REAL             :: HeatZoneTemp             = 0.0   ! current zone temperature (heating, time step)
  REAL             :: HeatOutTemp              = 0.0   ! current outdoor temperature (heating, time step)
  REAL             :: HeatZoneRetTemp          = 0.0   ! current zone return temperature (heating, time step)
  REAL             :: CoolZoneTemp             = 0.0   ! current zone temperature (cooling, time step)
  REAL             :: CoolOutTemp              = 0.0   ! current Outdoor temperature (cooling, time step)
  REAL             :: CoolZoneRetTemp          = 0.0   ! current zone return temperature (cooling, time step)
  REAL             :: HeatZoneHumRat           = 0.0   ! current zone humidity ratio (heating, time step)
  REAL             :: CoolZoneHumRat           = 0.0   ! current zone humidity ratio (cooling, time step)
  REAL             :: HeatOutHumRat            = 0.0   ! current outdoor humidity ratio (heating, time step)
  REAL             :: CoolOutHumRat            = 0.0   ! current outdoor humidity ratio (cooling, time step)
  REAL             :: ZoneTempAtHeatPeak       = 0.0   ! zone temp at max heating [C]
  REAL             :: ZoneRetTempAtHeatPeak    = 0.0   ! zone return temp at max heating [C]
  REAL             :: OutTempAtHeatPeak        = 0.0   ! outdoor temperature at max heating [C]
  REAL             :: ZoneTempAtCoolPeak       = 0.0   ! zone temp at max cooling [C]
  REAL             :: ZoneRetTempAtCoolPeak    = 0.0   ! zone return temp at max cooling [C]
  REAL             :: OutTempAtCoolPeak        = 0.0   ! outdoor temperature at max cooling [C]
  REAL             :: ZoneHumRatAtHeatPeak     = 0.0   ! zone humidity ratio at max heating [kg/kg]
  REAL             :: ZoneHumRatAtCoolPeak     = 0.0   ! zone humidity ratio at max cooling [kg/kg]
  REAL             :: OutHumRatAtHeatPeak      = 0.0   ! outdoor humidity at max heating [kg/kg]
  REAL             :: OutHumRatAtCoolPeak      = 0.0   ! outdoor humidity at max cooling [kg/kg]
  INTEGER               :: TimeStepNumAtHeatMax     = 0       ! time step number (in day) at Heating peak
  INTEGER               :: TimeStepNumAtCoolMax     = 0       ! time step number (in day) at cooling peak
  INTEGER               :: HeatDDNum                = 0       ! design day index of design day causing heating peak
  INTEGER               :: CoolDDNum                = 0       ! design day index of design day causing heating peak
  CHARACTER(len=8)      :: cHeatDDDate              = ' '     ! date of design day causing heating peak
  CHARACTER(len=8)      :: cCoolDDDate              = ' '     ! date of design day causing cooling peak
  REAL             :: MinOA                    = 0.0   ! design minimum outside air in m3/s
  REAL             :: DesCoolMinAirFlow2       = 0.0   ! design cooling minimum air flow rate [m3/s] derived from
                                                              !  DesCoolMinAirFlowPerArea
  REAL             :: DesHeatMaxAirFlow2       = 0.0   ! design heating maximum air flow rate [m3/s] derived from
                                                              !  DesHeatMaxAirFlowPerArea
  REAL, ALLOCATABLE, DIMENSION(:)  :: HeatFlowSeq        ! daily sequence of zone heating air mass flow rate
                                                              !  (zone time step)
  REAL, ALLOCATABLE, DIMENSION(:)  :: CoolFlowSeq        ! daily sequence of zone cooling air mass flow rate
                                                              !  (zone time step)
  REAL, ALLOCATABLE, DIMENSION(:)  :: HeatLoadSeq        ! daily sequence of zone heating load zone time step)
  REAL, ALLOCATABLE, DIMENSION(:)  :: CoolLoadSeq        ! daily sequence of zone cooling load zone time step)
  REAL, ALLOCATABLE, DIMENSION(:)  :: HeatZoneTempSeq    ! daily sequence of zone temperatures (heating, zone time step)
  REAL, ALLOCATABLE, DIMENSION(:)  :: HeatOutTempSeq     ! daily sequence of outdoor temperatures (heating, zone time step)
  REAL, ALLOCATABLE, DIMENSION(:)  :: HeatZoneRetTempSeq ! daily sequence of zone return temperatures (heating,
                                                              !  zone time step)
  REAL, ALLOCATABLE, DIMENSION(:)  :: CoolZoneTempSeq    ! daily sequence of zone temperatures (cooling, zone time step)
  REAL, ALLOCATABLE, DIMENSION(:)  :: CoolOutTempSeq     ! daily sequence of outdoor temperatures (cooling, zone time step)
  REAL, ALLOCATABLE, DIMENSION(:)  :: CoolZoneRetTempSeq ! daily sequence of zone return temperatures (cooling,
                                                              !  zone time step)
  REAL, ALLOCATABLE, DIMENSION(:)  :: HeatZoneHumRatSeq  ! daily sequence of zone humidity ratios (heating, zone time step)
  REAL, ALLOCATABLE, DIMENSION(:)  :: CoolZoneHumRatSeq  ! daily sequence of zone humidity ratios (cooling, zone time step)
  REAL, ALLOCATABLE, DIMENSION(:)  :: HeatOutHumRatSeq   ! daily sequence of outdoor humidity ratios (heating, zone time step)
  REAL, ALLOCATABLE, DIMENSION(:)  :: CoolOutHumRatSeq   ! daily sequence of outdoor humidity ratios (cooling, zone time step)
  REAL             :: ZoneADEffCooling         = 1.0   ! the zone air distribution effectiveness in cooling mode
  REAL             :: ZoneADEffHeating         = 1.0   ! the zone air distribution effectiveness in heating mode
  REAL             :: ZoneSecondaryRecirculation   = 0.0   ! the zone secondary air recirculation fraction
  REAL             :: ZonePrimaryAirFraction       = 0.0   ! the zone primary air fraction for cooling based calculations
  REAL             :: ZonePrimaryAirFractionHtg    = 0.0   ! the zone primary air fraction for heating based calculations
  REAL             :: ZoneOAFracCooling        = 0.0   ! OA fraction in cooling mode
  REAL             :: ZoneOAFracHeating        = 0.0   ! OA fraction in heating mode
  REAL             :: TotalOAFromPeople        = 0.0   ! Zone OA required due to people
  REAL             :: TotalOAFromArea          = 0.0   ! Zone OA required based on floor area
  REAL             :: TotPeopleInZone          = 0.0   ! total number of people in the zone
  REAL             :: ZonePeakOccupancy        = 0.0   ! zone peak occupancy based on max schedule value
  REAL             :: SupplyAirAdjustFactor    = 1.0   ! supply air adjustment factor for next time step if OA is capped
  REAL             :: ZpzClgByZone             = 0.0   ! OA Std 62.1 required fraction in cooling mode
  REAL             :: ZpzHtgByZone             = 0.0   ! OA Std 62.1 required fraction in heating mode
  REAL             :: VozClgByZone             = 0.0   ! value of required cooling vent to zone, used in 62.1 tabular report
  REAL             :: VozHtgByZone             = 0.0   ! value of required heating vent to zone, used in 62.1 tabular report

END TYPE ZoneSizingData

TYPE TermUnitSizingData
  REAL             :: AirVolFlow               = 0.0 ! design air vol flow rate for single duct terminal unit [m3/s]
  REAL             :: MaxHWVolFlow             = 0.0 ! design Hot Water vol flow for single duct terminal unit [m3/s]
  REAL             :: MaxSTVolFlow             = 0.0 ! design Steam vol flow rate for single duct terminal unit [m3/s]
  REAL             :: MaxCWVolFlow             = 0.0 ! design Cold Water vol flow for single duct terminal unit [m3/s]
  REAL             :: MinFlowFrac              = 0.0 ! design minimum flow fraction for a terminal unit
  REAL             :: InducRat                 = 0.0 ! design induction ratio for a terminal unit
  REAL             :: ReheatMult               = 1.0 ! multiplier for reheat coil UA calculation
END TYPE TermUnitSizingData

TYPE ZoneEqSizingData                                       ! data saved from zone eq component sizing and passed to subcomponents
  REAL             :: AirVolFlow               = 0.0 ! design air vol flow rate for zone equipment unit [m3/s]
  REAL             :: MaxHWVolFlow             = 0.0 ! design Hot Water vol flow for zone equipment unit [m3/s]
  REAL             :: MaxCWVolFlow             = 0.0 ! design Cold Water vol flow for zone equipment unit [m3/s]
  REAL             :: OAVolFlow                = 0.0 ! design outside air flow for zone equipment unit [m3/s]
END TYPE ZoneEqSizingData

TYPE SystemSizingInputData
  CHARACTER &
    (len=MaxNameLength) :: AirPriLoopName           = ' '     ! name of an AirLoopHVAC object
  INTEGER               :: LoadSizeType             = 0       ! type of load to size on;
                                                              ! 0=sensible, 1=latent, 2=total, 3=ventilation
  INTEGER               :: SizingOption             = 0       ! 1 = noncoincident, 2 = coincident
  INTEGER               :: CoolOAOption             = 0       ! 1 = use 100% outside air; 2 = use min OA; for cooling sizing
  INTEGER               :: HeatOAOption             = 0       ! 1 = use 100% outside air; 2 = use min OA; for heating sizing
  REAL             :: DesOutAirVolFlow         = 0.0   ! design (minimum) outside air flow rate [m3/s]
  REAL             :: SysAirMinFlowRat         = 0.0   ! minimum system air flow ratio
  REAL             :: PreheatTemp              = 0.0   ! preheat design set temperature [C]
  REAL             :: PrecoolTemp              = 0.0   ! precool design set temperature [C]
  REAL             :: PreheatHumRat            = 0.0   ! preheat design humidity ratio [kg water/kg dry air]
  REAL             :: PrecoolHumRat            = 0.0   ! precool design humidity ratio [kg water/kg dry air]
  REAL             :: CoolSupTemp              = 0.0   ! cooling design supply air temperature [C]
  REAL             :: HeatSupTemp              = 0.0   ! heating design supply air temperature [C]
  REAL             :: CoolSupHumRat            = 0.0   ! cooling design supply air humidity ratio [kg water/kg dry air]
  REAL             :: HeatSupHumRat            = 0.0   ! heating design supply air humidity ratio [kg water/kg dry air]
  INTEGER               :: CoolAirDesMethod         = 0       ! choice of how to get system cooling design air flow rates;
                                                              !  1 = calc from des day simulation; 2=m3/s per system, user input
  REAL             :: DesCoolAirFlow           = 0.0   ! design system supply air flow rate for cooling[m3/s]
  INTEGER               :: HeatAirDesMethod         = 0       ! choice of how to get system heating design air flow rates;
                                                              !  1 = calc from des day simulation; 2=m3/s per zone, user input
  REAL             :: DesHeatAirFlow           = 0.0   ! design system heating supply air flow rate [m3/s]
  INTEGER               :: SystemOAMethod           = 0       ! System Outdoor Air Method; 1 = SOAM_ZoneSum, 2 = SOAM_VRP
  REAL             :: MaxZoneOAFraction        = 0.0   ! maximum value of min OA for zones served by system
  LOGICAL               :: OAAutosized              = .FALSE.  ! Set to true if design OA vol flow is set to 'autosize'
                                                               ! in Sizing:System
END TYPE SystemSizingInputData

TYPE SystemSizingData             ! Contains data for system sizing
  CHARACTER &
    (len=MaxNameLength) :: AirPriLoopName           = ' '     ! name of an AirLoopHVAC object
  CHARACTER &
    (len=MaxNameLength) :: CoolDesDay               = ' '     ! name of a cooling design day
  CHARACTER &
    (len=MaxNameLength) :: HeatDesDay               = ' '     ! name of a heating design day
  INTEGER               :: LoadSizeType             = 0       ! type of load to size on;
                                                              ! 0=sensible, 1=latent, 2=total, 3=ventilation
  INTEGER               :: SizingOption             = 0       ! 1 = noncoincident, 2 = coincident.
  INTEGER               :: CoolOAOption             = 0       ! 1 = use 100% outside air; 2 = use min OA; for cooling sizing
  INTEGER               :: HeatOAOption             = 0       ! 1 = use 100% outside air; 2 = use min OA; for heating sizing
  REAL             :: DesOutAirVolFlow         = 0.0   ! design (minimum) outside air flow rate [m3/s]
  REAL             :: SysAirMinFlowRat         = 0.0   ! minimum system air flow ratio
  REAL             :: PreheatTemp              = 0.0   ! preheat design set temperature
  REAL             :: PrecoolTemp              = 0.0   ! precool design set temperature [C]
  REAL             :: PreheatHumRat            = 0.0   ! preheat design humidity ratio [kg water/kg dry air]
  REAL             :: PrecoolHumRat            = 0.0   ! precool design humidity ratio [kg water/kg dry air]
  REAL             :: CoolSupTemp              = 0.0   ! cooling design supply air temperature [C]
  REAL             :: HeatSupTemp              = 0.0   ! heating design supply air temperature[C]
  REAL             :: CoolSupHumRat            = 0.0   ! cooling design supply air humidity ratio [kg water/kg dry air]
  REAL             :: HeatSupHumRat            = 0.0   ! heating design supply air humidity ratio [kg water/kg dry air]
  INTEGER               :: CoolAirDesMethod         = 0       ! choice of how to get system design cooling air flow rates;
                                                              !  1 = calc from des day simulation; 2=m3/s per system, user input
  INTEGER               :: HeatAirDesMethod         = 0       ! choice of how to get system design heating air flow rates;
                                                              !  1 = calc from des day simulation; 2=m3/s per system, user input
  REAL             :: InpDesCoolAirFlow        = 0.0   ! input design system supply air flow rate [m3/s]
  REAL             :: InpDesHeatAirFlow        = 0.0   ! input design system heating supply air flow rate [m3/s]
  REAL             :: CoinCoolMassFlow         = 0.0   ! coincident peak cooling mass flow rate [kg/s]
  LOGICAL               :: EMSOverrideCoinCoolMassFlowOn = .FALSE. ! If true, EMS to change coincident peak cooling mass flow rate
  REAL             :: EMSValueCoinCoolMassFlow   = 0.0 ! Value EMS wants for coincident peak cooling mass flow rate [kg/s]

  REAL             :: CoinHeatMassFlow         = 0.0   ! coincident peak heating mass flow rate [kg/s]
  LOGICAL               :: EMSOverrideCoinHeatMassFlowOn = .FALSE. ! If true, EMS to set coincident peak heating mass flow rate
  REAL             :: EMSValueCoinHeatMassFlow   = 0.0 ! Value EMS wants for coincident peak heating mass flow rate [kg/s]

  REAL             :: NonCoinCoolMassFlow      = 0.0   ! noncoincident peak cooling mass flow rate [kg/s]
  LOGICAL               :: EMSOverrideNonCoinCoolMassFlowOn = .FALSE. ! true, EMS to set noncoincident peak cooling mass flow rate
  REAL             :: EMSValueNonCoinCoolMassFlow   = 0.0 ! Value EMS for noncoincident peak cooling mass flow rate [kg/s]

  REAL             :: NonCoinHeatMassFlow      = 0.0   ! noncoincident peak heating mass flow rate [kg/s]
  LOGICAL               :: EMSOverrideNonCoinHeatMassFlowOn = .FALSE. ! true, EMS to set noncoincident peak heating mass flow rate
  REAL             :: EMSValueNonCoinHeatMassFlow   = 0.0 ! Value EMS for noncoincident peak heating mass flow rate [kg/s]

  REAL             :: DesMainVolFlow           = 0.0   ! design main supply duct volume flow [m3/s]
  LOGICAL               :: EMSOverrideDesMainVolFlowOn = .FALSE. ! If true, EMS is acting to change DesMainVolFlow
  REAL             :: EMSValueDesMainVolFlow   = 0.0 ! Value EMS providing for design main supply duct volume flow [m3/s]

  REAL             :: DesHeatVolFlow           = 0.0   ! design heat supply duct volume flow [m3/s]
  LOGICAL               :: EMSOverrideDesHeatVolFlowOn = .FALSE. ! If true, EMS is acting to change DesCoolVolFlow
  REAL             :: EMSValueDesHeatVolFlow   = 0.0 ! Value EMS providing for design cool  supply duct volume flow [m3/s]

  REAL             :: DesCoolVolFlow           = 0.0   ! design cool  supply duct volume flow [m3/s]
  LOGICAL               :: EMSOverrideDesCoolVolFlowOn = .FALSE. ! If true, EMS is acting to change DesCoolVolFlow
  REAL             :: EMSValueDesCoolVolFlow   = 0.0 ! Value EMS providing for design cool  supply duct volume flow [m3/s]

  REAL             :: SensCoolCap              = 0.0   ! design sensible cooling capacity [W]
  REAL             :: HeatCap                  = 0.0   ! design heating capacity [W]
  REAL             :: PreheatCap               = 0.0   ! design preheat capacity [W]
  REAL             :: CoolMixTemp              = 0.0   ! design mixed air temperature for cooling [C]
  REAL             :: CoolMixHumRat            = 0.0   ! design mixed air hum ratio for cooling [kg water/kg dry air]
  REAL             :: CoolRetTemp              = 0.0   ! design return air temperature for cooling [C]
  REAL             :: CoolRetHumRat            = 0.0   ! design return air hum ratio for cooling [kg water/kg dry air]
  REAL             :: CoolOutTemp              = 0.0   ! design outside air temperature for cooling [C]
  REAL             :: CoolOutHumRat            = 0.0   ! design outside air hum ratio for cooling [kg water/kg dry air]
  REAL             :: HeatMixTemp              = 0.0   ! design mixed air temperature for heating [C]
  REAL             :: HeatMixHumRat            = 0.0   ! design mixed air hum ratio for heating [kg water/kg dry air]
  REAL             :: HeatRetTemp              = 0.0   ! design return air temperature for heating [C]
  REAL             :: HeatRetHumRat            = 0.0   ! design return air hum ratio for heating [kg water/kg dry air]
  REAL             :: HeatOutTemp              = 0.0   ! design outside air temperature for heating [C]
  REAL             :: HeatOutHumRat            = 0.0   ! design outside air hum ratio for Heating [kg water/kg dry air]
  REAL             :: DesCoolVolFlowMin        = 0.0   ! design minimum system cooling flow rate [m3/s]
  REAL, ALLOCATABLE, DIMENSION(:)  :: HeatFlowSeq         ! daily sequence of system heating air mass flow rate
                                                               !  (zone time step)
  REAL, ALLOCATABLE, DIMENSION(:)  :: CoolFlowSeq         ! daily sequence of system cooling air mass flow rate
                                                               !  (zone time step)
  REAL, ALLOCATABLE, DIMENSION(:)  :: SensCoolCapSeq      ! daily sequence of system sensible cooling capacity
                                                               !  (zone time step)
  REAL, ALLOCATABLE, DIMENSION(:)  :: HeatCapSeq          ! daily sequence of system heating capacity [zone time step]
  REAL, ALLOCATABLE, DIMENSION(:)  :: PreHeatCapSeq       ! daily sequence of system preheat capacity [zone time step]
  REAL, ALLOCATABLE, DIMENSION(:)  :: SysCoolRetTempSeq   ! daily sequence of system cooling return temperatures [C]
                                                               !  [zone time step]
  REAL, ALLOCATABLE, DIMENSION(:)  :: SysCoolRetHumRatSeq ! daily sequence of system cooling return humidity ratios
                                                               !  [kg water/kg dry air] [zone time step]
  REAL, ALLOCATABLE, DIMENSION(:)  :: SysHeatRetTempSeq   ! daily sequence of system heating return temperatures [C]
                                                               !   [zone time step]
  REAL, ALLOCATABLE, DIMENSION(:)  :: SysHeatRetHumRatSeq ! daily sequence of system heating return humidity ratios
                                                               !  [kg water/kg dry air] [zone time step]
  REAL, ALLOCATABLE, DIMENSION(:)  :: SysCoolOutTempSeq   ! daily sequence of system cooling outside temperatures [C]
                                                               !  [zone time step]
  REAL, ALLOCATABLE, DIMENSION(:)  :: SysCoolOutHumRatSeq ! daily sequence of system cooling outside humidity ratios
                                                               !  [kg water/kg dry air] [zone time step]
  REAL, ALLOCATABLE, DIMENSION(:)  :: SysHeatOutTempSeq   ! daily sequence of system heating outside temperatures [C]
                                                               !  [zone time step]
  REAL, ALLOCATABLE, DIMENSION(:)  :: SysHeatOutHumRatSeq ! daily sequence of system heating outside humidity ratios
                                                               !   [kg water/kg dry air] [zone time step]
  INTEGER               :: SystemOAMethod           = 0        ! System Outdoor Air Method; 1 = SOAM_ZoneSum, 2 = SOAM_VRP
  REAL             :: MaxZoneOAFraction        = 0.0    ! maximum value of min OA for zones served by system
  REAL             :: SysUncOA                 = 0.0    ! uncorrected system outdoor air flow based on zone people and
                                                               ! zone area
  LOGICAL               :: OAAutosized              = .FALSE.  ! Set to true if design OA vol flow is set to 'autosize'
                                                               ! in Sizing:System
END TYPE SystemSizingData

TYPE PlantSizingData
  CHARACTER &
    (len=MaxNameLength) :: PlantLoopName            = ' '     ! name of PLANT LOOP or CONDENSER LOOP object
  INTEGER               :: LoopType                 = 0       ! type of loop: 1=heating, 2=cooling, 3=condenser
  REAL             :: ExitTemp                 = 0.0   ! loop design exit (supply) temperature [C]
  REAL             :: DeltaT                   = 0.0   ! loop design temperature drop (or rise) [DelK]
  ! Calculated
  REAL             :: DesVolFlowRate           = 0.0   ! loop design flow rate in m3/s
  LOGICAL               :: VolFlowSizingDone        = .FALSE. ! flag to indicate when this loop has finished sizing flow rate
END TYPE PlantSizingData

TYPE DesDayWeathData
  CHARACTER(len=8)      :: DateString               = ' '     ! date of design day weather values
  REAL, ALLOCATABLE, DIMENSION(:)  :: Temp               ! design day temperatures at the major time step
  REAL, ALLOCATABLE, DIMENSION(:)  :: HumRat             ! design day humidity ratios at the major time step
  REAL, ALLOCATABLE, DIMENSION(:)  :: Press              ! design day braometric pressure at the major time step
END TYPE DesDayWeathData

TYPE CompDesWaterFlowData                                     ! design water flow rate for components that use water as an
                                                              !  energy source or sink
  INTEGER               :: SupNode                  = 0       ! water inlet node number (condenser side for water / water)
  REAL             :: DesVolFlowRate           = 0.0   ! water design flow rate [m3/s]
END TYPE CompDesWaterFlowData

TYPE OARequirementsData
  CHARACTER(len=MaxNameLength) :: Name   = ' '
  INTEGER      :: OAFlowMethod           = 0        !- Method for OA flow calculation
                                                    !- (Flow/Person, Flow/Zone, Flow/Area, FlowACH, Sum, Maximum)
  REAL    :: OAFlowPerPerson        = 0.0    !- OA requirement per person
  REAL    :: OAFlowPerArea          = 0.0    !- OA requirement per zone area
  REAL    :: OAFlowPerZone          = 0.0    !- OA requirement per zone
  REAL    :: OAFlowACH              = 0.0    !- OA requirement per zone per hour
  INTEGER      :: OAFlowFracSchPtr       = 0        !- Fraction schedule applied to total OA requirement
  REAL    :: MaxOAFractionSchValue  = 0.0    !- Maximum value from OAFlow fraction schedule (used for sizing)
END TYPE OARequirementsData

TYPE ZoneAirDistributionData
  CHARACTER(len=MaxNameLength) :: Name       = ' '
  CHARACTER(len=MaxNameLength) :: ZoneADEffSchName = ' '!- Zone air distribution effectiveness schedule name
  REAL    :: ZoneADEffCooling           = 1.0    !- Zone air distribution effectiveness in cooling mode
  REAL    :: ZoneADEffHeating           = 1.0    !- Zone air distribution effectiveness in heating mode
  REAL    :: ZoneSecondaryRecirculation = 0.0    !- Zone air secondary recirculation ratio
  INTEGER      :: ZoneADEffSchPtr       = 0             !- Zone air distribution effectiveness schedule index
END TYPE ZoneAirDistributionData

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! MODULE VARIABLE DECLARATIONS:

TYPE (OARequirementsData), ALLOCATABLE, DIMENSION(:) :: OARequirements

TYPE (ZoneAirDistributionData), ALLOCATABLE, DIMENSION(:) :: ZoneAirDistribution

TYPE (ZoneSizingInputData),   ALLOCATABLE, DIMENSION(:)   :: ZoneSizingInput      ! Input data for zone sizing
TYPE (ZoneSizingData),        ALLOCATABLE, DIMENSION(:,:) :: ZoneSizing           ! Data for zone sizing (all data, all design
                                                                                  !  days; includes effects of user multiplier
                                                                                  !  and user set flows)
TYPE (ZoneSizingData),        ALLOCATABLE, DIMENSION(:)   :: FinalZoneSizing      ! Final data for zone sizing including effects
                                                                                  !  of user input multiplier and flows
TYPE (ZoneSizingData),        ALLOCATABLE, DIMENSION(:,:) :: CalcZoneSizing       ! Data for zone sizing (all data,
                                                                                  !  all design days, calculated only)
TYPE (ZoneSizingData),        ALLOCATABLE, DIMENSION(:)   :: CalcFinalZoneSizing  ! Final data for zone sizing (calculated only)
TYPE (ZoneSizingData),        ALLOCATABLE, DIMENSION(:)   :: TermUnitFinalZoneSizing   ! Final data for sizing terminal units
TYPE (SystemSizingInputData), ALLOCATABLE, DIMENSION(:)   :: SysSizInput          ! Input data array for system sizing object
TYPE (SystemSizingData),      ALLOCATABLE, DIMENSION(:,:) :: SysSizing            ! Data array for system sizing (all data)
TYPE (SystemSizingData),      ALLOCATABLE, DIMENSION(:)   :: FinalSysSizing       ! Data array for system sizing (max heat/cool)
                                                                                  !  using user input system flow rates.
TYPE (SystemSizingData),      ALLOCATABLE, DIMENSION(:)   :: CalcSysSizing        ! Data array for system sizing (max heat/cool)
                                                                                  !  before applying user input sys flow rates.
TYPE (TermUnitSizingData),    ALLOCATABLE, DIMENSION(:)   :: TermUnitSizing       ! Data added in sizing routines
TYPE (ZoneEqSizingData),      ALLOCATABLE, DIMENSION(:)   :: ZoneEqSizing         ! Data added in zone eq component sizing routines
TYPE (PlantSizingData),       ALLOCATABLE, DIMENSION(:)   :: PlantSizData         ! Input data array for plant sizing
TYPE (DesDayWeathData),       ALLOCATABLE, DIMENSION(:)   :: DesDayWeath          ! design day weather saved at major time step
TYPE (CompDesWaterFlowData),  ALLOCATABLE, DIMENSION(:)   :: CompDesWaterFlow     ! array to store components' design water flow

INTEGER   :: NumOARequirements  = 0       ! Number of OA Requirements objects
INTEGER   :: NumZoneAirDistribution = 0   ! Number of zone air distribution objects
INTEGER   :: NumZoneSizingInput = 0       ! Number of Zone Sizing objects
INTEGER   :: NumSysSizInput     = 0       ! Number of System Sizing objects
INTEGER   :: NumPltSizInput     = 0       ! Number of Plant Sizing objects
INTEGER   :: CurSysNum          = 0       ! Current Air System index (0 if not in air loop)
INTEGER   :: CurOASysNum        = 0       ! Current outside air system index (0 if not in OA Sys)
INTEGER   :: CurZoneEqNum       = 0       ! Current Zone Equipment index (0 if not simulating ZoneEq)
INTEGER   :: CurBranchNum       = 0       ! Index of branch being simulated (or 0 if not air loop)
INTEGER   :: CurDuctType        = 0       ! Duct type of current branch
INTEGER   :: CurLoopNum         = 0       ! the current plant loop index
INTEGER   :: CurCondLoopNum     = 0       ! the current condenser loop number
INTEGER   :: CurEnvirNumSimDay  = 0
INTEGER   :: CurOverallSimDay   = 0
INTEGER   :: NumTimeStepsInAvg  = 0       ! number of time steps in the averaging window for the design flow and load sequences
INTEGER   :: SaveNumPlantComps      = 0       ! Number of components using water as an energy source or sink (e.g. water coils)
LOGICAL   :: TermUnitSingDuct   = .FALSE. ! TRUE if a non-induction single duct terminal unit
LOGICAL   :: TermUnitPIU        = .FALSE. ! TRUE if a powered induction terminal unit
LOGICAL   :: TermUnitIU         = .FALSE. ! TRUE if an unpowered induction terminal unit
LOGICAL   :: ZoneEqFanCoil      = .FALSE. ! TRUE if a 4 pipe fan coil unit is being simulated
LOGICAL   :: ZoneEqDXCoil       = .FALSE. ! TRUE if a ZoneHVAC DX coil is being simulated
LOGICAL   :: ZoneHeatingOnlyFan = .FALSE. ! TRUE if zone unit only does heating and contains a fam (such as Unit Heater)
LOGICAL   :: SysSizingRunDone   = .FALSE. ! True if a system sizing run is successfully completed.
LOGICAL   :: ZoneSizingRunDone  = .FALSE. ! True if a zone sizing run has been successfully completed.
REAL :: DXCoolCap          = 0.0   ! The ARI cooling capacity of a DX unit.
REAL :: UnitaryHeatCap     = 0.0   ! the heating capacity of a unitary system
REAL :: SuppHeatCap        = 0.0   ! the heating capacity of the supplemental heater in a unitary system
REAL :: GlobalHeatSizingFactor = 0.0   ! the global heating sizing ratio
REAL :: GlobalCoolSizingFactor = 0.0   ! the global cooling sizing ratio
REAL, ALLOCATABLE, DIMENSION(:) :: ZoneSizThermSetPtHi ! highest zone thermostat setpoint during zone sizing calcs
REAL, ALLOCATABLE, DIMENSION(:) :: ZoneSizThermSetPtLo ! lowest zone thermostat setpoint during zone sizing calcs
CHARACTER(len=15), ALLOCATABLE, DIMENSION(:) :: CoolPeakDateHrMin
CHARACTER(len=15), ALLOCATABLE, DIMENSION(:) :: HeatPeakDateHrMin
CHARACTER(len=1) :: SizingFileColSep=' '  ! Character to separate columns in sizing outputs


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

END MODULE DataSizing
