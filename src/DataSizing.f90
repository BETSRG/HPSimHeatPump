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
USE DataGlobals, ONLY: MaxNameLength

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.

          ! MODULE PARAMETER DEFINITIONS:

INTEGER, PARAMETER :: Coincident = 2
INTEGER, PARAMETER :: NonCoincident = 1
INTEGER, PARAMETER :: OAFlowPPer = 1
INTEGER, PARAMETER :: OAFlow = 2
INTEGER, PARAMETER :: FromDDCalc = 1
INTEGER, PARAMETER :: InpDesAirFlow = 2
INTEGER, PARAMETER :: AllOA = 1
INTEGER, PARAMETER :: MinOA = 2
REAL, PARAMETER    :: AutoSize = -99999.
INTEGER, PARAMETER :: HeatingLoop = 1
INTEGER, PARAMETER :: CoolingLoop = 2
INTEGER, PARAMETER :: CondenserLoop = 3
CHARACTER(len=*), PARAMETER :: PeakHrMinFmt = "(I2.2,':',I2.2,':00')"

          ! DERIVED TYPE DEFINITIONS:
TYPE ZoneSizingInputData
  CHARACTER(len=MaxNameLength) :: ZoneName      ! name of a zone
  REAL                         :: CoolDesTemp   ! zone design cooling supply air temperature [C]
  REAL                         :: HeatDesTemp   ! zone design heating supply air temperature [C]
  REAL                         :: CoolDesHumRat ! zone design cooling supply air humidity ratio [kg-H2O/kg-air]
  REAL                         :: HeatDesHumRat ! zone design heating supply air humidity ratio [kg-H2O/kg-air]
  INTEGER                      :: OADesMethod   ! choice of how to calculate minimum outside air;
                                                ! 1 = m3/s per person; 2 = m3/s per zone
  REAL                         :: DesOAFlowPPer ! design outside air flow per person in zone [m3/s]
  REAL                         :: DesOAFlow     ! design outside air flow for the zone [m3/s]
  INTEGER                      :: CoolAirDesMethod  ! choice of how to get zone cooling design air flow rates;
                                                    ! 1 = calc from des day simulation; 2 = m3/s per zone, user input
  REAL                         :: DesCoolAirFlow    ! design zone supply air flow rate [m3/s]
  INTEGER                      :: HeatAirDesMethod  ! choice of how to get zone heating design air flow rates;
                                                    ! 1 = calc from des day simulation; 2 = m3/s per zone, user input
  REAL                         :: DesHeatAirFlow    ! design zone heating supply air flow rate [m3/s]
  REAL                         :: SizingFactor  ! the zone sizing ratio
END TYPE ZoneSizingInputData

TYPE ZoneSizingData
  CHARACTER(len=MaxNameLength) :: ZoneName      ! name of a zone
  CHARACTER(len=MaxNameLength) :: CoolDesDay    ! name of a cooling design day
  CHARACTER(len=MaxNameLength) :: HeatDesDay    ! name of a heating design day
  REAL                         :: CoolDesTemp   ! zone design cooling supply air temperature [C]
  REAL                         :: HeatDesTemp   ! zone design heating supply air temperature [C]
  REAL                         :: CoolDesHumRat ! zone design cooling supply air humidity ratio [kg-H2O/kg-air]
  REAL                         :: HeatDesHumRat ! zone design heating supply air humidity ratio [kg-H2O/kg-air]
  INTEGER                      :: OADesMethod   ! choice of how to calculate minimum outside air;
                                                ! 1 = m3/s per person; 2 = m3/s per zone
  REAL                         :: DesOAFlowPPer ! design outside air flow per person in zone [m3/s]
  REAL                         :: DesOAFlow     ! design outside air flow for the zone [m3/s]
  INTEGER                      :: CoolAirDesMethod  ! choice of how to get zone design cooling air flow rates;
                                                ! 1 = calc from des day simulation; 2 = m3/s per zone, user input
  INTEGER                      :: HeatAirDesMethod  ! choice of how to get zone design heating air flow rates;
                                                ! 1 = calc from des day simulation; 2 = m3/s per zone, user input
  REAL                         :: InpDesCoolAirFlow ! input design zone supply air flow rate [m3/s]
  REAL                         :: InpDesHeatAirFlow ! input design zone heating supply air flow rate [m3/s]
  REAL                         :: SizingFactor  ! the zone sizing ratio
  INTEGER                      :: ActualZoneNum ! index into the Zone data array (in DataHeatBalance)
  INTEGER                      :: SupplyAirNode ! node number of supply air node
  REAL                         :: DesHeatMassFlow ! zone design heating air mass flow rate
  REAL                         :: DesCoolMassFlow ! zone design cooling air mass flow rate
  REAl                         :: DesHeatLoad     ! zone design heating load
  REAL                         :: DesCoolLoad     ! zone design cooling load
  REAL                         :: DesHeatDens     ! zone design heating air density
  REAL                         :: DesCoolDens     ! zone design cooling air density
  REAL                         :: DesHeatVolFlow  ! zone design heating air volume flow rate
  REAL                         :: DesCoolVolFlow  ! zone design cooling air volume flow rate
  REAL                         :: DesHeatCoilInTemp ! zone heating coil design air inlet temperature [C]
  REAL                         :: DesCoolCoilInTemp ! zone cooling coil design air inlet temperature [C]
  REAL                         :: DesHeatCoilInHumRat ! zone heating coil design air inlet humidity ratio [kg/kg]
  REAL                         :: DesCoolCoilInHumRat ! zone cooling coil design air inlet humidity ratio [kg/kg]
  REAL                         :: HeatMassFlow    ! current zone heating air mass flow rate (HVAC time step)
  REAL                         :: CoolMassFlow    ! current zone cooling air mass flow rate (HVAC time step)
  REAl                         :: HeatLoad        ! current zone heating load (HVAC time step)
  REAL                         :: CoolLoad        ! current zone heating load (HVAC time step)
  REAL                         :: HeatZoneTemp    ! current zone temperature (heating, time step)
  REAL                         :: HeatZoneRetTemp ! current zone return temperature (heating, time step)
  REAL                         :: CoolZoneTemp    ! current zone temperature (cooling, time step)
  REAL                         :: CoolZoneRetTemp ! current zone return temperature (cooling, time step)
  REAL                         :: HeatZoneHumRat  ! current zone humidity ratio (heating, time step)
  REAL                         :: CoolZoneHumRat  ! current zone humidity ratio (cooling, time step)
  REAL                         :: ZoneTempAtHeatPeak      ! zone temp at max heating
  REAL                         :: ZoneRetTempAtHeatPeak   ! zone return temp at max heating
  REAL                         :: ZoneTempAtCoolPeak      ! zone temp at max cooling
  REAL                         :: ZoneRetTempAtCoolPeak   ! zone return temp at max cooling
  REAL                         :: ZoneHumRatAtHeatPeak    ! zone humidity ratio at max heating
  REAL                         :: ZoneHumRatAtCoolPeak    ! zone humidity ratio at max cooling
  INTEGER                      :: TimeStepNumAtHeatMax    ! time step number (in day) at Heating peak
  INTEGER                      :: TimeStepNumAtCoolMax    ! time step number (in day) at cooling peak
  INTEGER                      :: HeatDDNum       ! design day index of design day causing heating peak
  INTEGER                      :: CoolDDNum       ! design day index of design day causing heating peak
  REAL                         :: MinOA           ! design minimum outside air in m3/s
  REAL, DIMENSION(:), POINTER  :: HeatFlowSeq     ! daily sequence of zone heating air mass flow rate (zone time step)
  REAL, DIMENSION(:), POINTER  :: CoolFlowSeq     ! daily sequence of zone cooling air mass flow rate (zone time step)
  REAL, DIMENSION(:), POINTER  :: HeatLoadSeq     ! daily sequence of zone heating load zone time step)
  REAL, DIMENSION(:), POINTER  :: CoolLoadSeq     ! daily sequence of zone cooling load zone time step)
  REAL, DIMENSION(:), POINTER  :: HeatZoneTempSeq    ! daily sequence of zone temperatures (heating, zone time step)
  REAL, DIMENSION(:), POINTER  :: HeatZoneRetTempSeq ! daily sequence of zone return temperatures (heating, zone time step)
  REAL, DIMENSION(:), POINTER  :: CoolZoneTempSeq    ! daily sequence of zone temperatures (cooling, zone time step)
  REAL, DIMENSION(:), POINTER  :: CoolZoneRetTempSeq ! daily sequence of zone return temperatures (cooling, zone time step)
  REAL, DIMENSION(:), POINTER  :: HeatZoneHumRatSeq ! daily sequence of zone humidity ratios (heating, zone time step)
  REAL, DIMENSION(:), POINTER  :: CoolZoneHumRatSeq ! daily sequence of zone humidity ratios (cooling, zone time step)

END TYPE ZoneSizingData

TYPE SystemSizingInputData
  CHARACTER(len=MaxNameLength) :: AirPriLoopName   ! name of an AIR PRIMARY LOOP object
  INTEGER                      :: LoadSizeType     ! type of load to size on; 0=sensible, 1=latent, 2=total
  INTEGER                      :: SizingOption     ! 1 = noncoincident, 2 = coincident
  INTEGER                      :: CoolOAOption     ! 1 = use 100% outside air; 2 = use min OA; for cooling sizing
  INTEGER                      :: HeatOAOption     ! 1 = use 100% outside air; 2 = use min OA; for heating sizing
  REAL                         :: DesOutAirVolFlow ! design (minimum) outside air volumetric flow rate [m3/s]
  REAL                         :: SysAirMinFlowRat ! minimum system air flow ratio
  REAL                         :: PreheatTemp      ! preheat design set temperature
  REAL                         :: CoolSupTemp      ! cooling design supply air temperature [C]
  REAL                         :: HeatSupTemp      ! heating design supply air temperature [C]
  REAL                         :: CoolSupHumRat    ! cooling design supply air humidity ratio [kg water/kg dry air]
  REAL                         :: HeatSupHumRat    ! heating design supply air humidity ratio [kg water/kg dry air]
  INTEGER                      :: CoolAirDesMethod  ! choice of how to get system cooling design air flow rates;
                                                    ! 1 = calc from des day simulation; 2 = m3/s per system, user input
  REAL                         :: DesCoolAirFlow    ! design system supply air flow rate for cooling[m3/s]
  INTEGER                      :: HeatAirDesMethod  ! choice of how to get system heating design air flow rates;
                                                    ! 1 = calc from des day simulation; 2 = m3/s per zone, user input
  REAL                         :: DesHeatAirFlow    ! design system heating supply air flow rate [m3/s]
END TYPE SystemSizingInputData

TYPE SystemSizingData             ! Contains data for system sizing
  CHARACTER(len=MaxNameLength) :: AirPriLoopName   ! name of an AIR PRIMARY LOOP object
  CHARACTER(len=MaxNameLength) :: CoolDesDay    ! name of a cooling design day
  CHARACTER(len=MaxNameLength) :: HeatDesDay    ! name of a heating design day
  INTEGER                      :: LoadSizeType     ! type of load to size on; 0=sensible, 1=latent, 2=total
  INTEGER                      :: SizingOption     ! 1 = noncoincident, 2 = coincident.
  INTEGER                      :: CoolOAOption     ! 1 = use 100% outside air; 2 = use min OA; for cooling sizing
  INTEGER                      :: HeatOAOption     ! 1 = use 100% outside air; 2 = use min OA; for heating sizing
  REAL                         :: DesOutAirVolFlow ! design (minimum) outside air volumetric flow rate [m3/s]
  REAL                         :: SysAirMinFlowRat ! minimum system air flow ratio
  REAL                         :: PreheatTemp      ! preheat design set temperature
  REAL                         :: CoolSupTemp      ! cooling design supply air temperature [C]
  REAL                         :: HeatSupTemp      ! heating design supply air temperature[C]
  REAL                         :: CoolSupHumRat    ! cooling design supply air humidity ratio [kg water/kg dry air]
  REAL                         :: HeatSupHumRat    ! heating design supply air humidity ratio [kg water/kg dry air]
  INTEGER                      :: CoolAirDesMethod  ! choice of how to get system design cooling air flow rates;
                                                    ! 1 = calc from des day simulation; 2 = m3/s per system, user input
  INTEGER                      :: HeatAirDesMethod  ! choice of how to get system design heating air flow rates;
                                                    ! 1 = calc from des day simulation; 2 = m3/s per system, user input
  REAL                         :: InpDesCoolAirFlow ! input design system supply air flow rate [m3/s]
  REAL                         :: InpDesHeatAirFlow ! input design system heating supply air flow rate [m3/s]
  REAL                         :: CoinCoolMassFlow ! coincident peak cooling mass flow rate [kg/s]
  REAL                         :: CoinHeatMassFlow ! coincident peak heating mass flow rate [kg/s]
  REAL                         :: NonCoinCoolMassFlow ! noncoincident peak cooling mass flow rate [kg/s]
  REAL                         :: NonCoinHeatMassFlow ! noncoincident peak heating mass flow rate [kg/s]
  REAL                         :: DesMainVolFlow  ! design main supply duct volume flow [m3/s]
  REAL                         :: DesHeatVolFlow  ! design heat supply duct volume flow [m3/s]
  REAL                         :: DesCoolVolFlow  ! design cool  supply duct volume flow [m3/s]
  REAL                         :: SensCoolCap     ! design sensible cooling capacity [W]
  REAL                         :: HeatCap         ! design heating capacity [W]
  REAL                         :: PreheatCap      ! design preheat capacity [W]
  REAL                         :: CoolMixTemp     ! design mixed air temperature for cooling [C]
  REAL                         :: CoolMixHumRat   ! design mixed air humidity ratio for cooling [kg water/kg dry air]
  REAL                         :: CoolRetTemp     ! design return air temperature for cooling [C]
  REAL                         :: CoolRetHumRat   ! design return air humidity ratio for cooling [kg water/kg dry air]
  REAL                         :: CoolOutTemp     ! design outside air temperature for cooling [C]
  REAL                         :: CoolOutHumRat   ! design outside air humidity ratio for cooling [kg water/kg dry air]
  REAL                         :: HeatMixTemp     ! design mixed air temperature for heating [C]
  REAL                         :: HeatMixHumRat   ! design mixed air humidity ratio for heating [kg water/kg dry air]
  REAL                         :: HeatRetTemp     ! design return air temperature for heating [C]
  REAL                         :: HeatRetHumRat   ! design return air humidity ratio for heating [kg water/kg dry air]
  REAL                         :: HeatOutTemp     ! design outside air temperature for heating [C]
  REAL                         :: HeatOutHumRat   ! design outside air humidity ratio for Heating [kg water/kg dry air]
  REAL, DIMENSION(:), POINTER  :: HeatFlowSeq     ! daily sequence of system heating air mass flow rate (zone time step)
  REAL, DIMENSION(:), POINTER  :: CoolFlowSeq     ! daily sequence of system cooling air mass flow rate (zone time step)
  REAL, DIMENSION(:), POINTER  :: SensCoolCapSeq  ! daily sequence of system sensible cooling capacity [zone time step]
  REAL, DIMENSION(:), POINTER  :: HeatCapSeq      ! daily sequence of system heating capacity [zone time step]
  REAL, DIMENSION(:), POINTER  :: PreHeatCapSeq   ! daily sequence of system preheat capacity [zone time step]
  REAL, DIMENSION(:), POINTER  :: SysCoolRetTempSeq       ! daily sequence of system cooling return temperatures [C]
                                                          !   [zone time step]
  REAL, DIMENSION(:), POINTER  :: SysCoolRetHumRatSeq     ! daily sequence of system cooling return humidity ratios 
                                                          !   [kg water/kg dry air] [zone time step]
  REAL, DIMENSION(:), POINTER  :: SysHeatRetTempSeq       ! daily sequence of system heating return temperatures [C]
                                                          !   [zone time step]
  REAL, DIMENSION(:), POINTER  :: SysHeatRetHumRatSeq     ! daily sequence of system heating return humidity ratios 
                                                          !   [kg water/kg dry air] [zone time step]
  REAL, DIMENSION(:), POINTER  :: SysCoolOutTempSeq       ! daily sequence of system cooling outside temperatures [C]
                                                          !   [zone time step]
  REAL, DIMENSION(:), POINTER  :: SysCoolOutHumRatSeq     ! daily sequence of system cooling outside humidity ratios 
                                                          !   [kg water/kg dry air] [zone time step]
  REAL, DIMENSION(:), POINTER  :: SysHeatOutTempSeq       ! daily sequence of system heating outside temperatures [C]
                                                          !   [zone time step]
  REAL, DIMENSION(:), POINTER  :: SysHeatOutHumRatSeq     ! daily sequence of system heating outside humidity ratios 
                                                          !   [kg water/kg dry air] [zone time step]
END TYPE SystemSizingData

TYPE PlantSizingData
  CHARACTER(len=MaxNameLength) :: PlantLoopName    ! name of PLANT LOOP or CONDENSER LOOP object
  INTEGER                      :: LoopType         ! type of loop: 1=heating, 2=cooling, 3=condenser
  REAL                         :: ExitTemp         ! loop design exit (supply) temperature [C]
  REAL                         :: DeltaT           ! loop design temperature drop (or rise) [DelK]
  ! Calculated
  REAL                         :: DesVolFlowRate   ! loop design flow rate in m3/s
END TYPE PlantSizingData

TYPE DesDayWeathData
  REAL, DIMENSION(:), POINTER  :: Temp             ! design day temperatures at the major time step
  REAL, DIMENSION(:), POINTER  :: HumRat           ! design day humidity ratios at the major time step
  REAL, DIMENSION(:), POINTER  :: Press            ! design day braometric pressure at the major time step
END TYPE DesDayWeathData

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! MODULE VARIABLE DECLARATIONS:
TYPE (ZoneSizingInputData), ALLOCATABLE, DIMENSION(:) :: ZoneSizingInput ! Input data for zone sizing
TYPE (ZoneSizingData), ALLOCATABLE, DIMENSION(:,:) :: ZoneSizing         ! Data for zone sizing (all data, all design
                                                                         !  days; includes effects of user multiplier
                                                                         !  and user set flows)
TYPE (ZoneSizingData), ALLOCATABLE, DIMENSION(:) :: FinalZoneSizing      ! Final data for zone sizing including effects of
                                                                         !  user input multiplier and flows
TYPE (ZoneSizingData), ALLOCATABLE, DIMENSION(:,:) :: CalcZoneSizing     ! Data for zone sizing (all data,
                                                                         !  all design days, calculated only)
TYPE (ZoneSizingData), ALLOCATABLE, DIMENSION(:) :: CalcFinalZoneSizing  ! Final data for zone sizing (calculated only)
TYPE (SystemSizingInputData), ALLOCATABLE, DIMENSION(:) :: SysSizInput   ! Input data array for system sizing object
TYPE (SystemSizingData), ALLOCATABLE, DIMENSION(:,:) :: SysSizing        ! Data array for system sizing (all data)
TYPE (SystemSizingData), ALLOCATABLE, DIMENSION(:) :: FinalSysSizing     ! Data array for system sizing (max heat & cool)
                                                                         ! using user input system flow rates.
TYPE (SystemSizingData), ALLOCATABLE, DIMENSION(:) :: CalcSysSizing      ! Data array for system sizing (max heat & cool)
                                                                         ! before applying user input sys flow rates.
TYPE (PlantSizingData), ALLOCATABLE, DIMENSION(:) :: PlantSizData        ! Input data array for plant sizing
TYPE (DesDayWeathData), ALLOCATABLE, DIMENSION(:) :: DesDayWeath ! design day weather data saved at the major time step

INTEGER :: NumZoneSizingInput ! Number of Zone Sizing objects
INTEGER :: NumSysSizInput     ! Number of System Sizing objects
INTEGER :: NumPltSizInput     ! Number of Plant Sizing objects
INTEGER :: CurSysNum = 0      ! Current Air System index (0 if not in air loop)
INTEGER :: CurZoneEqNum = 0   ! Current Zone Equipment index (0 if not simulating ZoneEq)
INTEGER :: CurBranchNum = 0   ! Index of branch being simulated (or 0 if not air loop)
INTEGER :: CurDuctType = 0    ! Duct type of current branch
INTEGER :: CurLoopNum = 0     ! the current plant loop index
INTEGER :: CurCondLoopNum = 0 ! the current condenser loop number
LOGICAL :: TermUnitFan = .FALSE.  ! TRUE if terminal unit contains a fan
LOGICAL :: ZoneHeatingOnlyFan = .FALSE. ! TRUE if zone unit only does heating and contains a fam (such as Unit Heater)
REAL    :: DXCoolCap = 0.0    ! The ARI cooling capacity of a DX unit.
LOGICAL :: SysSizingRunDone = .FALSE.  ! True if a system sizing run is successfully completed.
LOGICAL :: ZoneSizingRunDone = .FALSE. ! True if a zone sizing run has been successfully completed.
INTEGER :: NumTimeStepsInAvg  ! number of time steps in the averaging window for the design flow and load sequences
REAL    :: GlobalSizingFactor ! the global sizing ratio
CHARACTER(len=8), ALLOCATABLE, DIMENSION(:) :: CoolPeakHrMin
CHARACTER(len=8), ALLOCATABLE, DIMENSION(:) :: HeatPeakHrMin

!     NOTICE
!
!     Copyright © 1996-2003 The Board of Trustees of the University of Illinois
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
