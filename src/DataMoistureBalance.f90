MODULE DataMoistureBalance      ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   May 2000
          !       MODIFIED       April 2008; CondFD still uses some of this data.
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module should contain the information that is needed to between the
          ! MTF moisture modules and the calculation of the transfer functions
          ! Data is still used in the CondFD solution.

          ! USE STATEMENTS:
USE DataPrecisionGlobals

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC    ! By definition, all variables which are placed in this data-only
          ! module should be available to other modules and routines.  Thus,
          ! all variables in this module must be PUBLIC.

          ! MODULE PARAMETER DEFINITIONS

          ! Parameters for the definition and limitation of arrays:

          ! This is more or less the traditional value from BLAST.
REAL, Parameter :: Lam = 2500000.0 ! heat of adsorption for building materials


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! MODULE VARIABLE DECLARATIONS:
! Public Variables that will also be used in the Moisture Surface Balance
REAL, Allocatable, DIMENSION(:,:,:)    :: FluxH  ! transfer function coeff for calculating the CPF Flux history term
REAL, Allocatable, DIMENSION(:,:,:,:,:):: IcoefH ! transfer function coeff for calculating the CPF history term
REAL, Allocatable, DIMENSION(:,:,:,:)  :: Icoef  ! transfer function coeff for calculating the CPF history term
REAL, Allocatable, DIMENSION(:,:)      :: DiffC  ! Thermal Diffusivity in combined potential formulation (CPF)
                                                             ! for each equation
REAL, Allocatable, DIMENSION(:,:)      :: mtinc  ! # of Moisture transfer function time increment for each equation
REAL, Allocatable, DIMENSION(:)        :: S1     ! Thermal Diffusivity in combined potential formulation (CPF)
                                                             ! for each equation
REAL, Allocatable, DIMENSION(:)        :: R2     ! Thermal Diffusivity in combined potential formulation (CPF)
                                                             ! for each equation
REAL, ALLOCATABLE, DIMENSION(:) :: TempOutsideAirFD ! Temperature outside air for the FD surface

Integer, Allocatable, DIMENSION(:,:)  :: mhstry         ! # of FD History terms for each equation
Integer, Allocatable, Dimension(:)    :: CMTF           ! Type of material layer
Integer, Allocatable, DIMENSION(:,:)  :: Nmrf           ! # of Moisture Response Factors for CPF Solution

    !variables used for MTF moisture implementation
REAL, ALLOCATABLE, DIMENSION(:) :: RhoVaporAirOut  ! Vapor Density outside surface
REAL, ALLOCATABLE, DIMENSION(:) :: RhoVaporAirIn   ! Vapor Density inside surface
REAL, ALLOCATABLE, DIMENSION(:) :: HConvExtFD      ! thermal convection coefficient outside surface
REAL, ALLOCATABLE, DIMENSION(:) :: HMassConvExtFD  ! mass convection coefficient outside surface
REAL, ALLOCATABLE, DIMENSION(:) :: HConvInFD       ! thermal convection coefficient inside surface
REAL, ALLOCATABLE, DIMENSION(:) :: HMassConvInFD   ! mass convection coefficient inside surface
REAL, ALLOCATABLE, DIMENSION(:) :: RhoVaporSurfIn  ! Vapor Density inside surface
REAL, ALLOCATABLE, DIMENSION(:) :: HSkyFD          ! Sky Convection Coefficient
REAL, ALLOCATABLE, DIMENSION(:) :: HGrndFD         ! Ground Convection Coefficient
REAL, ALLOCATABLE, DIMENSION(:) :: HAirFD          ! Air Convection Coefficient

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

END MODULE DataMoistureBalance
