MODULE DataHVACGlobals      ! EnergyPlus Data-Only Module
          ! PURPOSE OF THIS MODULE:
          ! This data-only module is a repository for HVAC variables which are considered
          ! to be "global" in nature in Plus.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! OTHER NOTES:

          ! USE STATEMENTS:
USE DataGlobals, ONLY: MaxNameLength

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.


          ! MODULE PARAMETER DEFINITIONS:

REAL, PARAMETER :: SmallTempDiff = 1.0e-5
REAL, PARAMETER :: SmallMassFlow = .001
REAL, PARAMETER :: VerySmallMassFlow = 1.0E-30
REAL, PARAMETER :: SmallLoad     = 1.
REAL, PARAMETER :: TempControlTol = 0.1 ! temperature control tolerance for packaged equip. [deg C]
REAL, PARAMETER :: SmallAirVolFlow = 0.001
REAL, PARAMETER :: SmallWaterVolFlow = 0.000001
! The following parameters are used for system availability status
INTEGER, PARAMETER :: NoAction = 0
INTEGER, PARAMETER :: ForceOff = 1
INTEGER, PARAMETER :: CycleOn = 2
INTEGER, PARAMETER :: CycleOnZoneFansOnly = 3
! The following parameters describe the set point types in TempControlType(ActualZoneNum)
INTEGER, PARAMETER :: SingleHeatingSetPoint = 1
INTEGER, PARAMETER :: SingleCoolingSetPoint = 2
INTEGER, PARAMETER :: SingleHeatCoolSetPoint = 3
INTEGER, PARAMETER :: DualSetPointWithDeadBand = 4
! parameters describing air duct type
INTEGER, PARAMETER :: Main = 1
INTEGER, PARAMETER :: Cooling = 2
INTEGER, PARAMETER :: Heating = 3
INTEGER, PARAMETER :: Other = 4

          ! DERIVED TYPE DEFINITIONS

          ! INTERFACE BLOCK SPECIFICATIONS

          ! MODULE VARIABLE DECLARATIONS:

LOGICAL     :: FirstTimeStepSysFlag ! Set to true at the start of each sub-time step

REAL        :: SysUpdateTimeInc     ! System Update Time Increment - the adaptive time step used by the HVAC simulation
REAL        :: TimeStepSys          ! System Update Time Increment - the adaptive time step used by the HVAC simulation (hours)
REAL        :: SysTimeElapsed       ! elapsed system time in zone timestep (hours)
REAL        :: FracTimeStepZone     ! System time step divided by the zone time step

INTEGER     :: NumPlantLoops    = 0 ! Number of plant loops specified in simulation
INTEGER     :: NumCondLoops     = 0 ! Number of condenser plant loops specified in simulation
INTEGER     :: NumElecCircuits  = 0 ! Number of electric circuits specified in simulation
INTEGER     :: NumGasMeters     = 0 ! Number of gas meters specified in simulation
INTEGER     :: NumPrimaryAirSys = 0 ! Number of primary HVAC air systems
REAL        :: FanElecPower = 0.0   ! fan power from last fan simulation
REAL        :: OnOffFanPartLoadFraction = 1.0   ! fan part-load fraction (Fan:Simple:OnOff)
REAL        :: DXElecPower = 0.0           ! Electric power consumed by DX coil last DX simulation
REAL        :: AirToAirHXElecPower = 0.0   ! Electric power consumed by Heat Exchanger:Air To Air (Generic or Flat Plate)
                                           ! from last simulation in HeatRecovery.f90
LOGICAL     :: TurnFansOn = .FALSE. ! If true overrides fan schedule and cycles fans on
LOGICAL     :: TurnFansOff = .FALSE.  ! If True overides fan schedule and TurnFansOn and forces fans off
LOGICAL     :: SetPointErrorFlag = .FALSE. ! True if any needed set points not set; if true, program terminates
LOGICAL     :: EconomizerOperationFlag = .FALSE. ! TRUE if OA economizer is active, communicating from SimOAController
                                                 ! in MixedAir.f90 to CalcAirToAirGenericHeatExch in HeatRecovery.f90

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

END MODULE DataHVACGlobals
