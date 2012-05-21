MODULE General

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
PUBLIC  TrimSigDigits ! used for better formatting of numeric variables
PUBLIC  RoundSigDigits ! used for better formatting of numeric variables

CONTAINS

FUNCTION TrimSigDigits(RealValue,SigDigits) RESULT(OutputString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   March 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function accepts a number as parameter as well as the number of
          ! significant digits after the decimal point to report and returns a string
          ! that is appropriate.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL, INTENT(IN) :: RealValue
  INTEGER, INTENT(IN) :: SigDigits
  CHARACTER(len=30) OutputString

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER Pos
  CHARACTER(len=30) String
  CHARACTER(len=10) EString

  WRITE(String,*) RealValue
  Pos=INDEX(String,'E')
  IF (Pos > 0) THEN
    EString=String(Pos:)
  ELSE
    EString=' '
  ENDIF
  Pos=INDEX(String,'.')
  String=String(1:Pos+SigDigits)//EString
  OutputString=ADJUSTL(String)

  RETURN

END FUNCTION TrimSigDigits

FUNCTION RoundSigDigits(RealValue,SigDigits) RESULT(OutputString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   March 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function accepts a number as parameter as well as the number of
          ! significant digits after the decimal point to report and returns a string
          ! that is appropriate.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL, INTENT(IN) :: RealValue
  INTEGER, INTENT(IN) :: SigDigits
  CHARACTER(len=30) OutputString

          ! FUNCTION PARAMETER DEFINITIONS:
  CHARACTER(len=11) :: DigitChar='01234567890'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER Pos
  INTEGER TPos
  INTEGER NPos
  INTEGER TPos1
  CHARACTER(len=30) String
  CHARACTER(len=10) EString
  CHARACTER(len=1) TestChar

  WRITE(String,*) RealValue
  Pos=INDEX(String,'E')
  IF (Pos > 0) THEN
    EString=String(Pos:)
  ELSE
    EString=' '
  ENDIF
  Pos=INDEX(String,'.')
  TestChar=String(Pos+SigDigits+1:Pos+SigDigits+1)
  TPos=INDEX(DigitChar,TestChar)
  IF (TPos > 5) THEN
    TestChar=String(Pos+SigDigits:Pos+SigDigits)
    NPos=INDEX(DigitChar,TestChar)
    String(Pos+SigDigits:Pos+SigDigits)=DigitChar(NPos+1:NPos+1)
    IF (NPos == 10) THEN
      ! Must change other char too
      IF (SigDigits == 1) THEN
        TestChar=String(Pos+SigDigits-2:Pos+SigDigits-2)
        TPos1=INDEX(DigitChar,TestChar)
        String(Pos+SigDigits-2:Pos+SigDigits-2)=DigitChar(TPos1+1:TPos1+1)
      ELSE
        TestChar=String(Pos+SigDigits-1:Pos+SigDigits-1)
        TPos1=INDEX(DigitChar,TestChar)
        String(Pos+SigDigits-1:Pos+SigDigits-1)=DigitChar(TPos1+1:TPos1+1)
      ENDIF
    ENDIF
  ENDIF
  String=String(1:Pos+SigDigits)//EString
  OutputString=ADJUSTL(String)

  RETURN

END FUNCTION RoundSigDigits

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

END MODULE General
