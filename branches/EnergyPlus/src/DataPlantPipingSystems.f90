MODULE DataPlantPipingSystems

          ! Module containing the data structures dealing with the PlantPipingSystems

          ! MODULE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   Summer 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Contains all the data structures for PlantPipingSystems

          ! METHODOLOGY EMPLOYED:
          ! A pseudo-object-oriented approach is taken to use inheritance in the structure.
          ! For example, an abstract base cell class is defined with temperatures and other
          !  generic properties, then different cell types inherit from this by including it
          !  as a MyBase field within its own structure.  Not exactly OO inheritance, but
          !  it's close, and it increases code reuse, that's for sure!
          ! Enumerations are defined first with an EnumClassName_InstanceName format
          !

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
    USE DataGlobals, ONLY: Pi, MaxNameLength
    USE DataPrecisionGlobals, ONLY: r64


    IMPLICIT NONE ! Enforce explicit typing of all variables

    PUBLIC ! Everything public for this data-only module

          ! MODULE PARAMETER DEFINITIONS:
    INTEGER, PARAMETER :: PartitionType_BasementWall = -1
    INTEGER, PARAMETER :: PartitionType_BasementFloor = -2
    INTEGER, PARAMETER :: PartitionType_Pipe = -3

    INTEGER, PARAMETER :: RegionType_Pipe = -1
    INTEGER, PARAMETER :: RegionType_BasementWall = -2
    INTEGER, PARAMETER :: RegionType_BasementFloor = -3
    INTEGER, PARAMETER :: RegionType_XDirection = -4
    INTEGER, PARAMETER :: RegionType_YDirection = -5
    INTEGER, PARAMETER :: RegionType_ZDirection = -6

    INTEGER, PARAMETER :: MeshDistribution_Uniform = -1
    INTEGER, PARAMETER :: MeshDistribution_SymmetricGeometric = -2

    INTEGER, PARAMETER :: SegmentFlow_IncreasingZ = -1
    INTEGER, PARAMETER :: SegmentFlow_DecreasingZ = -2

    INTEGER, PARAMETER :: Direction_PositiveY = -1
    INTEGER, PARAMETER :: Direction_NegativeY = -2
    INTEGER, PARAMETER :: Direction_PositiveX = -3
    INTEGER, PARAMETER :: Direction_NegativeX = -4
    INTEGER, PARAMETER :: Direction_PositiveZ = -5
    INTEGER, PARAMETER :: Direction_NegativeZ = -6

    INTEGER, PARAMETER :: CellType_Unknown = -1
    INTEGER, PARAMETER :: CellType_Pipe = -2
    INTEGER, PARAMETER :: CellType_GeneralField = -3
    INTEGER, PARAMETER :: CellType_GroundSurface = -4
    INTEGER, PARAMETER :: CellType_FarfieldBoundary = -5
    INTEGER, PARAMETER :: CellType_AdiabaticWall = -6
    INTEGER, PARAMETER :: CellType_BasementWall = -7
    INTEGER, PARAMETER :: CellType_BasementFloor = -8
    INTEGER, PARAMETER :: CellType_BasementCorner = -9
    INTEGER, PARAMETER :: CellType_BasementCutaway = -10

          ! DERIVED TYPE DEFINITIONS:
    TYPE BaseThermalPropertySet
      REAL :: Conductivity = 0.0  !W/mK
      REAL :: Density = 0.0       !kg/m3
      REAL :: SpecificHeat = 0.0  !J/kgK
    END TYPE

    TYPE ExtendedFluidProperties ! : Inherits BaseThermalPropertySet
      TYPE(BaseThermalPropertySet) :: MyBase
      REAL :: Viscosity  !kg/m-s
      REAL :: Prandtl    !-
    END TYPE

    TYPE BaseCell
      REAL :: Temperature = 0.0 !C
      REAL :: Temperature_PrevIteration = 0.0 !C
      REAL :: Temperature_PrevTimeStep = 0.0 !C
      REAL :: Beta = 0.0 !K/W
      TYPE(BaseThermalPropertySet) :: Properties
    END TYPE

    TYPE RadialCellInformation ! : Inherits BaseCell
        TYPE(BaseCell) :: MyBase
        REAL :: RadialCentroid
        REAL :: InnerRadius
        REAL :: OuterRadius
    END TYPE

    TYPE FluidCellInformation ! : Inherits BaseCell
        TYPE(BaseCell) :: MyBase
        REAL :: PipeInnerRadius
        REAL :: Volume
        TYPE(ExtendedFluidProperties) :: Properties
    END TYPE

    TYPE CartesianPipeCellInformation ! Specialized cell information only used by cells which contain pipes
        TYPE(RadialCellInformation), ALLOCATABLE, DIMENSION(:) :: Soil
        TYPE(RadialCellInformation) :: Insulation
        TYPE(RadialCellInformation) :: Pipe
        TYPE(FluidCellInformation) :: Fluid
        REAL :: RadialSliceWidth
        REAL :: InterfaceVolume
    END TYPE

    TYPE Point
        INTEGER :: X
        INTEGER :: Y
    END TYPE

    TYPE PointF
        REAL :: X
        REAL :: Y
    END TYPE

    TYPE Point3DInteger
        INTEGER :: X
        INTEGER :: Y
        INTEGER :: Z
    END TYPE

    TYPE Point3DReal
        REAL :: X
        REAL :: Y
        REAL :: Z
    END TYPE

    TYPE DomainRectangle
        INTEGER :: XMin
        INTEGER :: XMax
        INTEGER :: Ymin
        INTEGER :: YMax
    END TYPE

    TYPE MeshPartition
        REAL :: rDimension
        INTEGER :: PartitionType !From Enum: ParitionType
        REAL :: TotalWidth
    END TYPE

    TYPE GridRegion
        REAL :: Min
        REAL :: Max
        INTEGER :: RegionType !From Enum: RegionType
        REAL, ALLOCATABLE, DIMENSION(:) :: CellWidths
    END TYPE

    TYPE TempGridRegionData
        REAL :: Min
        REAL :: Max
        INTEGER   :: RegionType !From Enum: RegionType
    END TYPE

    TYPE RectangleF
        REAL :: X_min
        REAL :: Y_min
        REAL :: Width
        REAL :: Height
    END TYPE

    TYPE NeighborInformation
        REAL :: ThisCentroidToNeighborCentroid
        REAL :: ThisCentroidToNeighborWall
        REAL :: ThisWallToNeighborCentroid
        REAL :: ConductionResistance
        TYPE(Point3DInteger) :: NeighborCellIndeces
    END TYPE

    TYPE RadialSizing
        REAL :: InnerDia
        REAL :: OuterDia
    END TYPE

    TYPE DirectionNeighbor_Dictionary
        INTEGER :: Direction !From Enum: Direction
        TYPE(NeighborInformation) :: Value
    END TYPE

    TYPE CartesianCell
        TYPE(BaseCell) :: MyBase
        INTEGER :: X_index
        INTEGER :: Y_index
        INTEGER :: Z_index
        REAL :: X_min
        REAL :: X_max
        REAL :: Y_min
        REAL :: Y_max
        REAL :: Z_min
        REAL :: Z_max
        TYPE(Point3DReal) :: Centroid
        INTEGER :: CellType !From Enum: CellType
        INTEGER :: PipeIndex
        TYPE(DirectionNeighbor_Dictionary), ALLOCATABLE, DIMENSION(:) :: NeighborInformation
        TYPE(CartesianPipeCellInformation) :: PipeCellData
    END TYPE

    !Input data structure
    TYPE MeshExtents
        REAL :: Xmax  = 0.0
        REAL :: Ymax  = 0.0
        REAL :: Zmax  = 0.0
    END TYPE

    TYPE DistributionStructure
        INTEGER :: MeshDistribution =0 !From Enum: MeshDistribution
        INTEGER :: RegionMeshCount=0
        REAL :: GeometricSeriesCoefficient  = 0.0
    END TYPE

    TYPE MeshProperties
        TYPE(DistributionStructure) :: X
        TYPE(DistributionStructure) :: Y
        TYPE(DistributionStructure) :: Z
    END TYPE

    TYPE SimulationControl
        REAL :: MinimumTemperatureLimit = -1000
        REAL :: MaximumTemperatureLimit = 1000
        REAL :: Convergence_CurrentToPrevIteration  = 0.0
        INTEGER :: MaxIterationsPerTS=0
    END TYPE

    TYPE FarfieldInfo
        REAL :: AverageGroundTemperature  = 0.0 !C
        REAL :: AverageGroundTemperatureAmplitude  = 0.0 !C
        REAL :: PhaseShiftOfMinGroundTempDays  = 0.0 !days
        REAL :: PhaseShiftOfMinGroundTemp  = 0.0 !seconds
    END TYPE

    TYPE BasementZoneInfo
        REAL :: Depth  = 0.0 !m
        REAL :: Width  = 0.0 !m
        LOGICAL :: ShiftPipesByWidth=.false.
        CHARACTER(len=MaxNameLength) :: WallBoundaryOSCMName=' '
        INTEGER :: WallBoundaryOSCMIndex=0
        CHARACTER(len=MaxNameLength) :: FloorBoundaryOSCMName=' '
        INTEGER :: FloorBoundaryOSCMIndex=0
        INTEGER, ALLOCATABLE, DIMENSION(:) :: WallSurfacePointers
        INTEGER, ALLOCATABLE, DIMENSION(:) :: FloorSurfacePointers
        INTEGER :: BasementWallXIndex = -1
        INTEGER :: BasementFloorYIndex = -1
    END TYPE

    ! Internal structure

    TYPE DirectionReal_Dictionary
        INTEGER :: Direction=0 !From Enum: Direction
        REAL :: Value  = 0.0
    END TYPE

    TYPE ReportingInformation
        TYPE(DirectionReal_Dictionary), ALLOCATABLE, DIMENSION(:) :: SurfaceHeatTransfer
        REAL :: TotalBoundaryHeatTransfer  = 0.0
        REAL :: EnergyStoredInCells  = 0.0
        REAL :: AverageSurfaceTemperature  = 0.0
        REAL :: PipeCircuitHeatTransferMCpDT  = 0.0
        REAL :: PipeCircuitHeatTransferUADT  = 0.0
        REAL :: BasementWallHeatTransfer  = 0.0
        REAL :: BasementFloorHeatTransfer  = 0.0
        REAL :: AverageBasementFloorTemperature  = 0.0
        REAL :: AverageBasementWallTemperature  = 0.0
    END TYPE

    TYPE MeshPartitions
        TYPE(MeshPartition), ALLOCATABLE, DIMENSION(:) :: X
        TYPE(MeshPartition), ALLOCATABLE, DIMENSION(:) :: Y
    END TYPE

    TYPE MoistureInfo
        REAL :: Theta_liq = 0.3 !volumetric moisture content of the soil
        REAL :: Theta_sat = 0.5 !volumetric moisture content of soil at saturation
        REAL :: GroundCoverCoefficient = 0.408
    END TYPE

    !Simulation data structures

    ! 'Current' data structure for variables, this is one-per-domain
    TYPE CurSimConditionsInfo
        !Simulation conditions
        REAL :: PrevSimTimeSeconds = -1.0
        REAL :: CurSimTimeSeconds  = 0.0
        REAL :: CurSimTimeStepSize  = 0.0
        !Environmental conditions
        REAL :: CurAirTemp = 10.0
        REAL :: CurWindSpeed = 2.6
        REAL :: CurIncidentSolar = 0.0
        REAL :: CurRelativeHumidity = 100.0
    END TYPE

    TYPE PipeSegmentInfo

        ! ID
        CHARACTER(len=MaxNameLength) :: Name

        ! Misc inputs
        TYPE(PointF) :: PipeLocation
        TYPE(Point)  :: PipeCellCoordinates
        INTEGER      :: FlowDirection =0 !From Enum: SegmentFlow

        ! Pointer to parent pipe circuit
        INTEGER :: ParentCircuitIndex=0

        ! Reporting variables
        REAL :: InletTemperature  = 0.0
        REAL :: OutletTemperature  = 0.0
        REAL :: FluidHeatLoss  = 0.0

        ! Error handling flags
        LOGICAL :: PipeCellCoordinatesSet = .FALSE.

    END TYPE

    TYPE PipeCircuitInfo

        ! ID
        CHARACTER(len=MaxNameLength) :: Name=' '

        ! Inlet and outlet information
        CHARACTER(len=MaxNameLength) :: InletNodeName=' '
        CHARACTER(len=MaxNameLength) :: OutletNodeName=' '
        INTEGER                      :: InletNodeNum=0
        INTEGER                      :: OutletNodeNum=0
        TYPE(Point3DInteger)         :: CircuitInletCell
        TYPE(Point3DInteger)         :: CircuitOutletCell

        ! Names and pointers to pipe segments found in this pipe circuit
        CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: PipeSegmentNames
        INTEGER, ALLOCATABLE, DIMENSION(:) :: PipeSegmentIndeces

        ! Pointer to the domain which contains this pipe circuit
        INTEGER :: ParentDomainIndex=0

        ! Misc inputs
        TYPE(RadialSizing) :: PipeSize
        TYPE(RadialSizing) :: InsulationSize
        REAL          :: RadialMeshThickness    =0.0
        LOGICAL            :: HasInsulation=.false.
        REAL          :: DesignVolumeFlowRate    =0.0
        REAL          :: DesignMassFlowRate    =0.0
        REAL          :: Convergence_CurrentToPrevIteration    =0.0
        INTEGER            :: MaxIterationsPerTS    =0
        INTEGER            :: NumRadialCells    =0
        TYPE(BaseThermalPropertySet) :: PipeProperties
        TYPE(BaseThermalPropertySet) :: InsulationProperties

        ! A list of 3d cell indeces that span the entire length of this pipe circuit (useful for reporting)
        TYPE(Point3DInteger), ALLOCATABLE, DIMENSION(:) :: ListOfCircuitPoints

        ! Flags
        LOGICAL :: CheckEquipName = .TRUE.
        LOGICAL :: NeedToFindOnPlantLoop = .TRUE.

        ! Location of this pipe circuit in the PlantLoop topology
        INTEGER :: LoopNum    =0
        INTEGER :: LoopSideNum    =0
        INTEGER :: BranchNum    =0
        INTEGER :: CompNum    =0

        ! Current fluid property values
        REAL :: CurFluidDensity = 998.0
        REAL :: CurFluidViscosity = 0.0015
        REAL :: CurFluidConductivity = 0.58
        REAL :: CurFluidPrandtl = 7.0
        REAL :: CurFluidSpecificHeat = 4190.0
        TYPE(ExtendedFluidProperties) :: CurFluidPropertySet

        ! Variables used to pass information from INIT-type routines to CALC-type routines
        REAL :: CurCircuitInletTemp = 23.0
        REAL :: CurCircuitFlowRate = 0.1321
        REAL :: CurCircuitConvectionCoefficient =0.0

        ! Reporting variables
        REAL :: InletTemperature =0.0
        REAL :: OutletTemperature =0.0
        REAL :: FluidHeatLoss =0.0

    END TYPE

    TYPE FullDomainStructureInfo

        ! ID
        CHARACTER(LEN=MaxNameLength) :: Name=' '

        ! Names and pointers to circuits found in this domain
        CHARACTER(LEN=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: CircuitNames
        INTEGER, ALLOCATABLE, DIMENSION(:) :: CircuitIndeces

        ! Flag variables
        LOGICAL :: OneTimeInit = .TRUE.
        LOGICAL :: BeginSimInit = .TRUE.
        LOGICAL :: BeginSimEnvrn = .TRUE.
        LOGICAL :: DomainNeedsSimulation = .TRUE.
        LOGICAL :: DomainNeedsToBeMeshed = .TRUE.

        ! "Input" data structure variables
        TYPE(MeshExtents) :: Extents
        TYPE(MeshProperties) :: Mesh
        TYPE(BaseThermalPropertySet) :: GroundProperties
        TYPE(SimulationControl) :: SimControls
        TYPE(FarfieldInfo) :: Farfield
        TYPE(BasementZoneInfo) :: BasementZone
        TYPE(MoistureInfo) :: Moisture

        ! "Internal" data structure variables
        TYPE(MeshPartitions) :: Partitions
        TYPE(CurSimConditionsInfo) :: Cur
        TYPE(ReportingInformation) :: Reporting
        LOGICAL :: HasBasement=.false.

        ! Main 3D cells array
        TYPE(CartesianCell), ALLOCATABLE, DIMENSION(:,:,:) :: Cells

    END TYPE FullDomainStructureInfo

          ! MODULE VARIABLE DECLARATIONS:
    TYPE(FullDomainStructureInfo), ALLOCATABLE, DIMENSION(:) :: PipingSystemDomains
    TYPE(PipeCircuitInfo),         ALLOCATABLE, DIMENSION(:) :: PipingSystemCircuits
    TYPE(PipeSegmentInfo),         ALLOCATABLE, DIMENSION(:) :: PipingSystemSegments

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


END MODULE
