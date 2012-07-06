MODULE GeneralRoutines

  ! Module containing routines for general use

  ! MODULE INFORMATION:
  !       AUTHOR         Fred Buhl, Linda Lawrie
  !       DATE WRITTEN   December 2001
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! contains routines (most likely numeric) that may be needed in several parts
  ! of EnergyPlus

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! This module should not contain variables in the module sense as it is
  ! intended strictly to provide "interfaces" to routines used by other
  ! parts of the simulation.

  ! MODULE PARAMETER DEFINITIONS
  ! na

  ! DERIVED TYPE DEFINITIONS
  ! na

  ! INTERFACE DEFINITIONS
  ! na

  ! MODULE VARIABLE DECLARATIONS:
  ! na

  !SUBROUTINE SPECIFICATIONS FOR MODULE General
PUBLIC IssueRefPropError

CONTAINS

LOGICAL FUNCTION IssueRefPropError(RefPropErrValue, CallingRoutine, ValueIfErrorFound, VariableToSet1, VariableToSet2) RESULT (ErrorFound)

    INTEGER(2), INTENT(IN) :: RefPropErrValue ! the value that was returned from the RefProp call
    CHARACTER(len=*), INTENT(IN) :: CallingRoutine ! an identifier to the routine calling me, for reporting
    INTEGER, INTENT(IN) :: ValueIfErrorFound ! if RefProp was erroneous, this is the signaling value to be used
    INTEGER, INTENT(INOUT), OPTIONAL :: VariableToSet1 ! if RefProp was erroneous, this will be set to the signal value
    REAL, INTENT(INOUT), OPTIONAL :: VariableToSet2 ! another variable to set...optionally

    IF (RefPropErrValue .GT. 0) THEN
        CALL ShowWarningError(CallingRoutine//': RefProp lookup error')
        IF ( PRESENT ( VariableToSet1 ) ) VariableToSet1 = ValueIfErrorFound
        IF ( PRESENT ( VariableToSet2 ) ) VariableToSet2 = ValueIfErrorFound
        ErrorFound = .TRUE.
    END IF

    RETURN

END FUNCTION


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

END MODULE GeneralRoutines
