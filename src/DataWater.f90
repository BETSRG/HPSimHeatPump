MODULE DataWater   ! EnergyPlus Data-Only Module

          ! Module containing the routines dealing with the DataWater

          ! MODULE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   August 2006
          !       MODIFIED       D. Sailor -- to add ecoroof irrigation
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This data-only module is a repository for the variables that relate specifically
          ! to the management of water in the simulation

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength

IMPLICIT NONE ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLI

          ! MODULE PARAMETER DEFINITION

INTEGER, PARAMETER :: ScheduledTankTemp = 101 ! tank water temperature is user input via schedule
INTEGER, PARAMETER :: TankZoneThermalCoupled  = 102 ! tank water temperature is modeled using simple UA

INTEGER, PARAMETER :: RainSchedDesign   = 201 ! mode of Rainfall determination is Scheduled Design
INTEGER, PARAMETER :: IrrSchedDesign    = 202 ! mode of Irrigation determination is Scheduled Design (DJS -PSU)
INTEGER, PARAMETER :: IrrSmartSched     = 203 ! mode of irrigation DJS - PSU

INTEGER, PARAMETER :: ConstantRainLossFactor = 301 !
INTEGER, PARAMETER :: ScheduledRainLossFactor = 302 !

INTEGER, PARAMETER :: AmbientTempSchedule    =  1 ! ambient temperature around tank (or HPWH inlet air) is scheduled
INTEGER, PARAMETER :: AmbientTempZone        =  2 ! tank is located in a zone or HPWH inlet air is zone air only
INTEGER, PARAMETER :: AmbientTempExterior    =  3 ! tank is located outdoors or HPWH inlet air is outdoor air only

INTEGER, PARAMETER :: ConstantWaterTable     = 401 !
INTEGER, PARAMETER :: ScheduledWaterTable    = 402 !

INTEGER, PARAMETER :: NoControlLevel = 501 !
INTEGER, PARAMETER :: MainsFloatValve = 502 !
INTEGER, PARAMETER :: WellFloatValve  = 503 !
INTEGER, PARAMETER :: WellFloatMainsBackup = 504 !
INTEGER, PARAMETER :: OtherTankFloatValve = 505 !
INTEGER, PARAMETER :: TankMainsBackup  = 506 !

INTEGER, PARAMETER :: OverflowDiscarded = 601 !
INTEGER, PARAMETER :: OverflowToTank    = 602 !


          ! DERIVED TYPE DEFINITIONS:
TYPE StorageTankDataStruct
   ! user input data
    CHARACTER(len=MaxNameLength) :: Name     = ' '  !name of this Storage Tank
    CHARACTER(len=MaxNameLength) :: QualitySubCategoryName = ' ' !name of water subcategory
 !   INTEGER                      :: QualitySubCategory = 0 !
    REAL                    :: MaxCapacity = 0.0 ! tank capacity Limit [m3]
    INTEGER                      :: OverflowMode = 0 !
    CHARACTER(len=MaxNameLength) :: OverflowTankName = ' ' !
    INTEGER                      :: OverflowTankID = 0 !
    INTEGER                      :: OverflowTankSupplyARRID = 0
    REAL                    :: ValveOnCapacity = 0.0 ! tank capacity at lower control range [m3]
    REAL                    :: ValveOffCapacity = 0.0 ! tank capacity at upper control range [m3]
    INTEGER                      :: ControlSupplyType = 0 ! mode for tank controlled resupply
    INTEGER                      :: GroundWellID = 0 !index "pointer" to well if present
    CHARACTER(len=MaxNameLength) :: SupplyTankName = ' ' !
    INTEGER                      :: SupplyTankID = 0
    INTEGER                      :: SupplyTankDemandARRID = 0
    REAL                    :: BackupMainsCapacity = 0.0
    REAL                    :: InitialVolume = 0.0  ! water in tank at start of simulation period [m3]
    REAL                    :: MaxInFlowRate = 0.0 ! limit on rate of inlet [m3/s]
    REAL                    :: MaxOutFlowRate = 0.0 ! limit on rate of outlet [m3/s]
    INTEGER                      :: ThermalMode  = 0  !
    REAL                    :: InitialTankTemp = 20.0 ! initial tank temperature [C]
    INTEGER                      :: TempSchedID  = 0  !index "pointer" to schedule
    INTEGER                      :: AmbientTempIndicator = 0      ! Indicator for ambient tank losses (SCHEDULE, ZONE, EXTERIOR)
    INTEGER                      :: AmbientTempSchedule = 0       ! Schedule index pointer
    INTEGER                      :: ZoneID   = 0  !index "pointer" to zone where tank is
    REAL                    :: UValue  = 0.0 ! U-value for tank [W/m2-k]
    REAL                    :: SurfArea = 0.0 ! surface are of tank on Zone side... [m2]
    INTEGER                      :: InternalMassID = 0 ! index "pointer" to internal mass object for thermal coupling
    CHARACTER(len=MaxNameLength) :: SurfMaterialName = ' ' ! surface properties
   ! calculated data and from elsewhere

    REAL                    :: ThisTimeStepVolume = 0.0
    REAL                    :: LastTimeStepVolume = 0.0
    REAL                    :: LastTimeStepTemp   = 0.0 ! previous temperature of tank water
    INTEGER                      :: NumWaterSupplies = 0
    REAL, Allocatable, Dimension(:) :: VdotAvailSupply !Each supply component has its own term
    REAL, Allocatable, Dimension(:) :: TwaterSupply !Each supply component has its own term
    CHARACTER(Len=MaxNameLength),  Allocatable, Dimension(:) ::  SupplyCompNames
    CHARACTER(Len=MaxNameLength),  Allocatable, Dimension(:) ::  SupplyCompTypes
    INTEGER                      :: NumWaterDemands = 0
    REAL, Allocatable, Dimension(:) :: VdotRequestDemand !each demand componennt has a slot
    REAL, Allocatable, Dimension(:) :: VdotAvailDemand !each demand componennt has a slot
    CHARACTER(Len=MaxNameLength), Allocatable, Dimension(:) :: DemandCompNames
    CHARACTER(Len=MaxNameLength), Allocatable, Dimension(:) :: DemandCompTypes
    REAL                    :: VdotFromTank  = 0.0
    REAL                    :: VdotToTank = 0.0 !
    REAL                    :: VdotOverflow = 0.0 !
    REAL                    :: VolOverflow = 0.0 !

   ! report variables
    REAL                    :: NetVdot    = 0.0
    REAL                    :: Twater = 0.0
    REAL                    :: TouterSkin = 0.0
    REAL                    :: TwaterOverflow = 0.0
    REAL                    :: MainsDrawVdot  = 0.0
    REAL                    :: MainsDrawVol   = 0.0


    REAL    :: SkinLossPower   = 0.0 ! heat loss to surrounding zone [W]
    REAL    :: SkinLossEnergy  = 0.0 ! heat loss to surround zone [J]
    REAL    :: SkinLossConvect = 0.0 ! convective heat loss to zone [W]
    REAL    :: SkinLossRadiat  = 0.0 ! radiative heat loss to zone [W}

END TYPE StorageTankDataStruct

TYPE RainfallCollectorDataStruct
   ! user input data
    CHARACTER(len=MaxNameLength) :: Name     = ' '  !name of this rain collector
    CHARACTER(len=MaxNameLength) :: StorageTankName = ' ' !
    INTEGER                      :: StorageTankID = 0 ! index "pointer" to storage tank array
    INTEGER                      :: StorageTankSupplyARRID = 0 !
    INTEGER                      :: LossFactorMode = 0 ! control how loss factor(s) are entered
    REAL                    :: LossFactor = 0.0 ! loss factor when constant
    INTEGER                      :: LossFactorSchedID = 0 ! index "pointer" to schedule
    REAL                    :: MaxCollectRate = 0.0 !
    INTEGER                      :: NumCollectSurfs = 0 ! number of surfaces used in the collector
    CHARACTER(len=MaxNameLength), Allocatable, Dimension(:) :: SurfName
    INTEGER, Allocatable, Dimension(:) :: SurfID
    !calculated and from elsewhere
    REAL                    :: HorizArea  = 0.0 ! area of surfaces in the vertical normal direction
    REAL                    :: VdotAvail  = 0.0 !
    REAL                    :: VolCollected = 0.0 !
    REAL                    :: MeanHeight = 0.0
END TYPE

TYPE GroundwaterWellDataStruct
   ! user input data
    CHARACTER(len=MaxNameLength) :: Name     = ' '  !name of this
    CHARACTER(len=MaxNameLength) :: StorageTankName = ' ' !
    INTEGER                      :: StorageTankID = 0 ! index "pointer" to water storage tank
    INTEGER                      :: StorageTankSupplyARRID = 0 ! index "pointer" to storage supply arrays
    REAL                    :: PumpDepth = 0.0 ! depth of pump  [m]
    REAL                    :: PumpNomVolFlowRate = 0.0 ! nominal flow rate of pump [m3/s]
    REAL                    :: PumpNomHead = 0.0 !design nominal capacity of pump
    REAL                    :: PumpNomPowerUse = 0.0 ! design nominal power of pump at nom capacity
    REAL                    :: PumpEfficiency = 0.0 !
    REAL                    :: WellRecoveryRate=0.0 ! rate at which groundwater can enter well [m3/s]
    REAL                    :: NomWellStorageVol=0.0 ! water storage in well at average water table depth [m3]
    INTEGER                      :: GroundwaterTableMode=0 ! method of determining water table depth
    REAL                    :: WaterTableDepth=0.0
    INTEGER                      :: WaterTableDepthSchedID=0 !
    !calculated and from elsewhere
    REAL                    :: VdotRequest = 0.0 ! rate of flow over timestep requested by tank
    REAL                    :: VdotDelivered = 0.0 ! rate of flow provided [m3/s]
    REAL                    :: VolDelivered = 0.0 !water provided [m3]
    REAL                    :: PumpPower = 0.0
    REAL                    :: PumpEnergy = 0.0

END TYPE GroundwaterWellDataStruct

TYPE SiteRainFallDataStruct
    INTEGER                      :: ModeID   = 0 ! type of rainfall modeling
    REAL                    :: DesignAnnualRain = 0.0 !
    INTEGER                      :: RainSchedID = 0 !
    REAL                    :: NomAnnualRain = 0.0 !
    !calculated and from elsewhere.
    REAL                    :: CurrentRate = 0.0
    REAL                    :: CurrentAmount = 0.0
END TYPE SiteRainFallDataStruct

TYPE IrrigationDataStruct
 INTEGER                     :: ModeID = 0 ! type of irrigation modeling
 INTEGER                     :: IrrSchedID = 0
 REAL                   :: ScheduledAmount = 0.0
 REAL                   :: ActualAmount = 0.0
 REAL                   :: IrrigationThreshold = 0.4  ! percent at which no irrigation happens (smart schedule)
END TYPE IrrigationDataStruct

          ! MODULE VARIABLE DECLARATIONS:
INTEGER    :: NumWaterStorageTanks = 0 ! number of water Storage tanks in model
INTEGER    :: NumRainCollectors = 0 ! number of rainfall collectors in model
INTEGER    :: NumGroundWaterWells = 0 !number of
INTEGER    :: NumSiteRainFall = 0 !
INTEGER    :: NumIrrigation = 0 ! DJS PSU Dec 2006 number of irrigation descriptions (1 allowed)
LOGICAL    :: AnyWaterSystemsInModel = .FALSE. ! control flag set true if any water systems
LOGICAL    :: WaterSystemGetInputCalled = .FALSE.  ! set true once input data gotten.
LOGICAL    :: AnyIrrigationInModel   = .FALSE. ! control flag set true if irrigation input for ecoroof DJS PSU Dec 2006


TYPE(SiteRainFallDataStruct)       :: RainFall=SiteRainFallDataStruct(0,0.0,0,0.0,0.0,0.0)
TYPE(IrrigationDataStruct)         :: Irrigation=IrrigationDataStruct(0,0,0.0,0.0,0.4)
TYPE(StorageTankDataStruct),       DIMENSION(:) , ALLOCATABLE :: WaterStorage
TYPE(RainfallCollectorDataStruct), DIMENSION(:), ALLOCATABLE :: RainCollector
TYPE(GroundwaterWellDataStruct),   DIMENSION(:), ALLOCATABLE :: GroundwaterWell

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


END MODULE DataWater

