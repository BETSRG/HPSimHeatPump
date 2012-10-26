MODULE DataConvergParams      ! EnergyPlus Data-Only Module

          ! PURPOSE OF THIS MODULE:
          ! This data-only module sets the parameters that control the convergence
          ! of the HVAC simulation.

USE DataPrecisionGlobals

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.


          ! MODULE PARAMETER DEFINITIONS:

          ! Note: Unless otherwise noted, the tolerance parameters listed below were chosen
          ! to represent educated guesses at what the tolerances for individual physical
          ! parameters should be.
  REAL, PARAMETER :: HVACEnthalpyToler    = 260.     ! Tolerance for enthalpy comparisons (in kJ/kgK)
  REAL, PARAMETER :: HVACFlowRateToler    = 0.01     ! Tolerance for mass flow rate convergence (in kg/s) [~20 CFM]
  REAL, PARAMETER :: HVACHumRatToler      = 0.0001   ! Tolerance for humidity ratio comparisons (in kg water/kg air)
  REAL, PARAMETER :: HVACQualityToler     = 0.01     ! Tolerance for fluid quality comparisons (dimensionless)
  REAL, PARAMETER :: HVACPressToler       = 10.0     ! Tolerance for pressure comparisons (in Pascals)
  REAL, PARAMETER :: HVACTemperatureToler = 0.01     ! Tolerance for temperature comparisons (in degrees C or K)
  REAL, PARAMETER :: HVACEnergyToler      = 10.0     ! Tolerance for Energy comparisons (in Watts W)

  REAL, PARAMETER :: HVACCpApprox         = 1004.844 ! Air Cp (20C,0.0Kg/Kg) Only for energy Tolerance Calculation
                                                     ! Only used to scale the answer for a more intuitive answer for comparison

  REAL, PARAMETER :: PlantEnthalpyToler    = 0.10     ! Tolerance for enthalpy comparisons (in kJ/kgK)
  REAL, PARAMETER :: PlantFlowRateToler    = 0.001    ! Tolerance for mass flow rate convergence (in kg/s) [~2 CFM]
  REAL, PARAMETER :: PlantHumRatToler      = 0.0001   ! Tolerance for humidity ratio comparisons (in kg water/kg air)
  REAL, PARAMETER :: PlantQualityToler     = 0.01     ! Tolerance for fluid quality comparisons (dimensionless)
  REAL, PARAMETER :: PlantPressToler       = 10.0     ! Tolerance for pressure comparisons (in Pascals)
  REAL, PARAMETER :: PlantTemperatureToler = 0.01     ! Tolerance for temperature comparisons (in degrees C or K)
  REAL, PARAMETER :: PlantEnergyToler      = 10.0     ! Tolerance for Energy comparisons (in Watts W)

  REAL, PARAMETER :: PlantCpApprox         = 4180.0   ! Approximate Cp used in Interface manager for
                                                             ! Energy Tolerance Calculation, used to scale the answer
                                                             ! for a more intuitive answer for comparison
  REAL, PARAMETER :: PlantFlowFlowRateToler= 0.01     ! Tolerance for mass flow rate convergence (in kg/s)

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
  INTEGER :: HVACEnthTolFlag = 0         ! Flag to show enthalpy convergence (0) or failure (1)
  INTEGER :: HVACFlowTolFlag = 0         ! Flag to show mass flow convergence (0) or failure (1)
  INTEGER :: HVACHumTolFlag = 0          ! Flag to show humidity ratio convergence (0) or failure (1)
  INTEGER :: HVACPressTolFlag = 0        ! Flag to show pressure convergence (0) or failure (1)
  INTEGER :: HVACQualTolFlag = 0         ! Flag to show fluid quality convergence (0) or failure (1)
  INTEGER :: HVACEnergyTolFlag = 0       ! Flag to show energy convergence (0) or failure (1)
  INTEGER :: HVACTempTolFlag = 0         ! Flag to show temperature convergence (0) or failure (1)
  REAL    :: HVACEnthTolValue(5)  =0.0   ! Queue of convergence "results"
  REAL    :: HVACFlowTolValue(5)  =0.0   ! Queue of convergence "results"
  REAL    :: HVACHumTolValue(5)   =0.0   ! Queue of convergence "results"
  REAL    :: HVACPressTolValue(5) =0.0   ! Queue of convergence "results"
  REAL    :: HVACQualTolValue(5)  =0.0   ! Queue of convergence "results"
  REAL    :: HVACEnergyTolValue(5)=0.0   ! Queue of convergence "results"
  REAL    :: HVACTempTolValue(5)  =0.0   ! Queue of convergence "results"
  INTEGER, DIMENSION(5) :: HVACOutletNodeTolOut = 0 ! Queue of outlet node index to identify with tol values
  INTEGER :: HVACQuePtr           =0     ! Pointer to "head" of queue

  INTEGER :: PlantEnthTolFlag = 0        ! Flag to show enthalpy convergence (0) or failure (1)
  INTEGER :: PlantFlowTolFlag = 0        ! Flag to show mass flow convergence (0) or failure (1)
  INTEGER :: PlantHumTolFlag = 0         ! Flag to show humidity ratio convergence (0) or failure (1)
  INTEGER :: PlantPressTolFlag = 0       ! Flag to show pressure convergence (0) or failure (1)
  INTEGER :: PlantQualTolFlag = 0        ! Flag to show fluid quality convergence (0) or failure (1)
  INTEGER :: PlantEnergyTolFlag = 0      ! Flag to show energy convergence (0) or failure (1)
  INTEGER :: PlantTempTolFlag = 0        ! Flag to show temperature convergence (0) or failure (1)
  REAL    :: PlantEnthTolValue(5)  =0.0  ! Queue of convergence "results"
  REAL    :: PlantFlowTolValue(5)  =0.0  ! Queue of convergence "results"
  REAL    :: PlantHumTolValue(5)   =0.0  ! Queue of convergence "results"
  REAL    :: PlantPressTolValue(5) =0.0  ! Queue of convergence "results"
  REAL    :: PlantQualTolValue(5)  =0.0  ! Queue of convergence "results"
  REAL    :: PlantEnergyTolValue(5)=0.0  ! Queue of convergence "results"
  REAL    :: PlantTempTolValue(5)  =0.0  ! Queue of convergence "results"
  INTEGER, DIMENSION(5) :: PlantOutletNodeTolOut = 0 ! Queue of outlet node index to identify with tol values
  INTEGER :: PlantQuePtr           =0

!  INTEGER :: PlantFlowEnthTolFlag = 0    ! Flag to show enthalpy convergence (0) or failure (1)
  INTEGER :: PlantFlowFlowTolFlag = 0    ! Flag to show mass flow convergence (0) or failure (1)
!  INTEGER :: PlantFlowHumTolFlag = 0     ! Flag to show humidity ratio convergence (0) or failure (1)
!  INTEGER :: PlantFlowPressTolFlag = 0   ! Flag to show pressure convergence (0) or failure (1)
!  INTEGER :: PlantFlowQualTolFlag = 0    ! Flag to show fluid quality convergence (0) or failure (1)
!  INTEGER :: PlantFlowEnergyTolFlag = 0  ! Flag to show energy convergence (0) or failure (1)
!  INTEGER :: PlantFlowTempTolFlag = 0    ! Flag to show temperature convergence (0) or failure (1)
  REAL    :: PlantFlowFlowTolValue(5) = 0.0  ! Queue of convergence "results"
  INTEGER :: PlantFlowQuePtr          = 0

  INTEGER :: AirLoopConvergFail = 0

  REAL :: MinTimeStepSys      =(1./60.)  ! =1 minute
  REAL :: MinTimeStepTol      =1.0d-4    != min allowable for ABS(1.-TimeStepSys/(MinTimeStepSys))
  REAL :: MaxZoneTempDiff     = 0.3      !0.3 C = (1% OF 300 C) = max allowable difference between
                                                                   !   zone air temp at Time=T and Time=T-1
  REAL :: MinSysTimeRemaining =(1.0/3600.) != 1 second
  INTEGER          :: MaxIter             =20          !maximum number of iterations allowed

  INTEGER   :: MaxPlantSubIterations  = 8  ! Iteration Max for Plant Simulation sub iterations
  INTEGER   :: MinPlantSubIterations  = 2  ! Iteration Min for Plant Simulation sub iterations

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

END MODULE DataConvergParams
