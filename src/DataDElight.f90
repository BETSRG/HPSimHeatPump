MODULE DataDElight      ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Robert J. Hitchcock
          !       DATE WRITTEN   August 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This data-only module is a repository for variables used in daylighting
          ! calculations by DElight.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.


          ! MODULE PARAMETER DEFINITIONS:
REAL, PARAMETER:: M2FT      =3.280840     ! Length:   Meters * M2FT = Feet
REAL, PARAMETER:: FT22M2    =0.09290304   ! Area:     SquareFeet * FT22M2 = SquareMeter
REAL, PARAMETER:: M22FT2    =1.0 / FT22M2 ! Area:     SquareMeter * M22FT2 = SquareFeet
REAL, PARAMETER:: M32FT3    =35.3147      ! Volume:       CubicMeter * M32FT3 = CubicFeet
REAL, PARAMETER:: LUX2FC    =0.09290304   ! Illuminance:  Lux * LUX2FC = Footcandles

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

!     NOTICE
!
!     Copyright 2003    Regents of University of California
!                       through Ernest Orlando Lawrence Berkeley National Laboratory
!                       All rights reserved.
!
!     This work was supported by the Assistant Secretary for Energy Efficiency
!     and Renewable Energy, Office of Building Technologies,
!     Building Systems and Materials Division of the
!     U.S. Department of Energy under Contract No. DE-AC03-76SF00098.
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
!

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

END MODULE DataDElight
