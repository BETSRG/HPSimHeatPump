
SUBROUTINE IssueHPFatalError(exitCode, errMessage)

! the fortran keyword STOP cannot accept a variable, only a literal or a parameter
! thus we have to have a ridiculous case statement for all possibilities found in DataStopCodes.f90

    USE DataStopCodes

    INTEGER, INTENT(IN) :: exitCode
    CHARACTER(len=*), INTENT(IN) :: errMessage

    WRITE(*,*) errMessage
    SELECT CASE (exitCode)
    CASE (exit_FileIO_Missing_HPData)
        STOP exit_FileIO_Missing_HPData
    CASE (exit_Diagnostic_RefrigerantName)
        STOP exit_Diagnostic_RefrigerantName
    CASE (exit_SimProblem_BadInitialization)
        STOP exit_SimProblem_BadInitialization
    CASE (exit_SimProblem_EnergyPlusProblem)
        STOP exit_SimProblem_EnergyPlusProblem
    CASE DEFAULT
        WRITE(*,*) '-+-Diagnostic-+- Unimplemented stop code in UtilityRoutines::IssueHPFatalError'
        STOP 1
    END SELECT

END SUBROUTINE

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

SUBROUTINE AbortEnergyPlus

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine causes the program to halt due to a fatal error.

          ! METHODOLOGY EMPLOYED:
          ! Puts a message on output files.
          ! Closes files.
          ! Stops the program.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataStringGlobals
  USE DataStopCodes

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  INTERFACE
    SUBROUTINE ShowMessage(Message,Unit1,Unit2)
    CHARACTER(len=*) Message
    INTEGER, OPTIONAL :: Unit1
    INTEGER, OPTIONAL :: Unit2
    END SUBROUTINE
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER tempfl
  INTEGER, EXTERNAL :: GetNewUnitNumber
  CHARACTER(len=20) NumWarnings
  CHARACTER(len=20) NumSevere


  WRITE(NumWarnings,*) TotalWarningErrors
  NumWarnings=ADJUSTL(NumWarnings)
  WRITE(NumSevere,*) TotalSevereErrors
  NumSevere=ADJUSTL(NumSevere)

  CALL ShowMessage('EnergyPlus Terminated--Fatal Error Detected. '//TRIM(NumWarnings)//' Warning; '//  &
                           TRIM(NumSevere)//' Severe Errors')
  tempfl=GetNewUnitNumber()
  open(tempfl,file='eplusout.end')
  write(tempfl,*) 'EnergyPlus Terminated--Fatal Error Detected. '//TRIM(NumWarnings)//' Warning; '//  &
                           TRIM(NumSevere)//' Severe Errors'
  close(tempfl)
  CALL CloseMiscOpenFiles
  
  CALL IssueHPFatalError(exit_SimProblem_EnergyPlusProblem, 'EnergyPlus Terminated--Error(s) Detected.') !ISI - 03-08-04

END SUBROUTINE AbortEnergyPlus

SUBROUTINE CloseMiscOpenFiles

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine scans potential unit numbers and closes
          ! any that are still open.

          ! METHODOLOGY EMPLOYED:
          ! Use INQUIRE to determine if file is open.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
 ! USE DaylightingManager, ONLY: CloseReportIllumMaps

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
   INTEGER, PARAMETER :: MaxUnitNumber = 1000

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:


      LOGICAL :: exists, opened
      INTEGER :: UnitNumber
      INTEGER :: ios

      !CALL CloseReportIllumMaps

      DO UnitNumber = 1, MaxUnitNumber
         INQUIRE (UNIT = UnitNumber, EXIST = exists,  OPENED = opened, IOSTAT = ios)
         IF (exists .and. opened .and. ios == 0) CLOSE(UnitNumber)
      END DO

  RETURN

END SUBROUTINE CloseMiscOpenFiles

SUBROUTINE EndEnergyPlus

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine causes the program to terminate when complete (no errors).

          ! METHODOLOGY EMPLOYED:
          ! Puts a message on output files.
          ! Closes files.
          ! Stops the program.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataStringGlobals
  USE InputProcessor

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  INTERFACE
    SUBROUTINE ShowMessage(Message,Unit1,Unit2)
    CHARACTER(len=*) Message
    INTEGER, OPTIONAL :: Unit1
    INTEGER, OPTIONAL :: Unit2
    END SUBROUTINE
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER tempfl
  INTEGER, EXTERNAL :: GetNewUnitNumber
  CHARACTER(len=20) NumWarnings
  CHARACTER(len=20) NumSevere
  CHARACTER(len=25) Elapsed
  INTEGER Hours   ! Elapsed Time Hour Reporting
  INTEGER Minutes ! Elapsed Time Minute Reporting
  INTEGER Seconds ! Elapsed Time Second Reporting

  WRITE(NumWarnings,*) TotalWarningErrors
  NumWarnings=ADJUSTL(NumWarnings)
  WRITE(NumSevere,*) TotalSevereErrors
  NumSevere=ADJUSTL(NumSevere)
  Hours=Elapsed_Time/3600.
  Elapsed_Time=Elapsed_Time-Hours*3600
  Minutes=Elapsed_Time/60.
  Elapsed_Time=Elapsed_Time-Minutes*60
  Seconds=Elapsed_Time
  WRITE(Elapsed,"(I2.2,'hr ',I2.2,'min ',I2.2,'sec')") Hours,Minutes,Seconds

  CALL ShowMessage('EnergyPlus Completed Successfully-- '//TRIM(NumWarnings)//' Warning; '//TRIM(NumSevere)//' Severe Errors;'// &
                   ' Elapsed Time='//TRIM(Elapsed))
  tempfl=GetNewUnitNumber()
  open(tempfl,file='eplusout.end')
  write(tempfl,'(A)') 'EnergyPlus Completed Successfully-- '//TRIM(NumWarnings)//' Warning; '//TRIM(NumSevere)//' Severe Errors'
  close(tempfl)
  CALL CloseMiscOpenFiles
  CALL DeallocateArrays !-ISI 02/23/04
  !STOP 'EnergyPlus Completed Successfully.' -ISI 02/19/04

  RETURN

END SUBROUTINE EndEnergyPlus

FUNCTION GetNewUnitNumber ()  RESULT (UnitNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie, adapted from reference
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Returns a unit number of a unit that can exist and is not connected.  Note
          ! this routine does not magically mark that unit number in use.  In order to
          ! have the unit "used", the source code must OPEN the file.

          ! METHODOLOGY EMPLOYED:
          ! Use Inquire function to find out if proposed unit: exists or is opened.
          ! If not, can be used for a new unit number.

          ! REFERENCES:
          ! Copyright (c) 1994 Unicomp, Inc.  All rights reserved.
          !
          ! Developed at Unicomp, Inc.
          !
          ! Permission to use, copy, modify, and distribute this
          ! software is freely granted, provided that this notice
          ! is preserved.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER UnitNumber  ! Result from scanning currently open files

          ! FUNCTION PARAMETER DEFINITIONS:
!  IO Status Values:

  INTEGER, PARAMETER :: END_OF_RECORD = -2
  INTEGER, PARAMETER :: END_OF_FILE = -1

!  Indicate default input and output units:

  INTEGER, PARAMETER :: DEFAULT_INPUT_UNIT = 5
  INTEGER, PARAMETER :: DEFAULT_OUTPUT_UNIT = 6

!  Indicate number and value of preconnected units

  INTEGER, PARAMETER :: NUMBER_OF_PRECONNECTED_UNITS = 2
  INTEGER, PARAMETER :: PRECONNECTED_UNITS (NUMBER_OF_PRECONNECTED_UNITS) = (/ 5, 6 /)

!  Largest allowed unit number (or a large number, if none)
  INTEGER, PARAMETER :: MaxUnitNumber = 1000

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: exists  ! File exists
  LOGICAL :: opened  ! Unit is open
  INTEGER :: ios     ! return value from Inquire intrinsic

  DO UnitNumber = 1, MaxUnitNumber
    IF (UnitNumber == DEFAULT_INPUT_UNIT .or. &
        UnitNumber == DEFAULT_OUTPUT_UNIT) CYCLE
    IF (ANY (UnitNumber == PRECONNECTED_UNITS)) CYCLE
    INQUIRE (UNIT = UnitNumber, EXIST = exists,  OPENED = opened, IOSTAT = ios)
    IF (exists .and. .not. opened .and. ios == 0) RETURN      ! result is set in UnitNumber
  END DO

  UnitNumber = -1

END FUNCTION GetNewUnitNumber



SUBROUTINE ShowFatalError(ErrorMessage,OutUnit1,OutUnit2)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine puts ErrorMessage with a Fatal designation on
          ! designated output files.  Then, the program is aborted.

          ! METHODOLOGY EMPLOYED:
          ! Calls ShowErrorMessage utility routine.
          ! Calls AbortEnergyPlus

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) ErrorMessage
  INTEGER, OPTIONAL :: OutUnit1
  INTEGER, OPTIONAL :: OutUnit2

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  INTERFACE
    SUBROUTINE ShowErrorMessage(Message,Unit1,Unit2)
    CHARACTER(len=*) Message
    INTEGER, OPTIONAL :: Unit1
    INTEGER, OPTIONAL :: Unit2
    END SUBROUTINE
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  CALL ShowErrorMessage(' **  Fatal  ** '//ErrorMessage,OutUnit1,OutUnit2)
  CALL AbortEnergyPlus

  RETURN

END SUBROUTINE ShowFatalError

SUBROUTINE ShowSevereError(ErrorMessage,OutUnit1,OutUnit2)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine puts ErrorMessage with a Severe designation on
          ! designated output files.

          ! METHODOLOGY EMPLOYED:
          ! Calls ShowErrorMessage utility routine.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataStringGlobals

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) ErrorMessage
  INTEGER, OPTIONAL :: OutUnit1
  INTEGER, OPTIONAL :: OutUnit2

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  INTERFACE
    SUBROUTINE ShowErrorMessage(Message,Unit1,Unit2)
    CHARACTER(len=*) Message
    INTEGER, OPTIONAL :: Unit1
    INTEGER, OPTIONAL :: Unit2
    END SUBROUTINE
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  TotalSevereErrors=TotalSevereErrors+1
  CALL ShowErrorMessage(' ** Severe  ** '//ErrorMessage,OutUnit1,OutUnit2)

  !  Could set a variable here that gets checked at some point?

  RETURN

END SUBROUTINE ShowSevereError

SUBROUTINE ShowContinueError(Message,OutUnit1,OutUnit2)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   October 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine displays a 'continued error' message on designated output files.

          ! METHODOLOGY EMPLOYED:
          ! Calls ShowErrorMessage utility routine.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) Message
  INTEGER, OPTIONAL :: OutUnit1
  INTEGER, OPTIONAL :: OutUnit2

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  INTERFACE
    SUBROUTINE ShowErrorMessage(Message,Unit1,Unit2)
    CHARACTER(len=*) Message
    INTEGER, OPTIONAL :: Unit1
    INTEGER, OPTIONAL :: Unit2
    END SUBROUTINE
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  CALL ShowErrorMessage(' **   ~~~   ** '//Message,OutUnit1,OutUnit2)

  RETURN

END SUBROUTINE ShowContinueError

SUBROUTINE ShowMessage(Message,OutUnit1,OutUnit2)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine displays a simple message on designated output files.

          ! METHODOLOGY EMPLOYED:
          ! Calls ShowErrorMessage utility routine.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) Message
  INTEGER, OPTIONAL :: OutUnit1
  INTEGER, OPTIONAL :: OutUnit2

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  INTERFACE
    SUBROUTINE ShowErrorMessage(Message,Unit1,Unit2)
    CHARACTER(len=*) Message
    INTEGER, OPTIONAL :: Unit1
    INTEGER, OPTIONAL :: Unit2
    END SUBROUTINE
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  CALL ShowErrorMessage(' ************* '//Message,OutUnit1,OutUnit2)

  RETURN

END SUBROUTINE ShowMessage

SUBROUTINE ShowWarningError(ErrorMessage,OutUnit1,OutUnit2)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine puts ErrorMessage with a Warning designation on
          ! designated output files.

          ! METHODOLOGY EMPLOYED:
          ! Calls ShowErrorMessage utility routine.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataStringGlobals

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) ErrorMessage
  INTEGER, OPTIONAL :: OutUnit1
  INTEGER, OPTIONAL :: OutUnit2

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
  INTERFACE
    SUBROUTINE ShowErrorMessage(Message,Unit1,Unit2)
    CHARACTER(len=*) Message
    INTEGER, OPTIONAL :: Unit1
    INTEGER, OPTIONAL :: Unit2
    END SUBROUTINE
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  TotalWarningErrors=TotalWarningErrors+1
  CALL ShowErrorMessage(' ** Warning ** '//ErrorMessage,OutUnit1,OutUnit2)

  RETURN

END SUBROUTINE ShowWarningError

SUBROUTINE ShowErrorMessage(ErrorMessage,OutUnit1,OutUnit2)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   December 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine displays the error messages on the indicated
          ! file unit numbers, in addition to the "standard error output"
          ! unit.

          ! METHODOLOGY EMPLOYED:
          ! If arguments OutUnit1 and/or OutUnit2 are present the
          ! error message is written to these as well and the standard one.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataStringGlobals, ONLY: VerString

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) ErrorMessage
  INTEGER, OPTIONAL :: OutUnit1
  INTEGER, OPTIONAL :: OutUnit2

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: ErrorFormat='(2X,A)'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER  :: TotalErrors=0        ! used to determine when to open standard error output file.
  INTEGER  :: StandardErrorOutput
  INTEGER,EXTERNAL  :: GetNewUnitNumber
  SAVE     TotalErrors,StandardErrorOutput

  IF (TotalErrors .eq. 0) THEN
    StandardErrorOutput=GetNewUnitNumber()
    OPEN(StandardErrorOutput,FILE='eplusout.err')
    WRITE(StandardErrorOutput,'(A)') 'Program Version,'//TRIM(VerString)
  ENDIF

  TotalErrors=TotalErrors+1
  WRITE(StandardErrorOutput,ErrorFormat) TRIM(ErrorMessage)
  IF (PRESENT(OutUnit1)) THEN
    WRITE(OutUnit1,ErrorFormat) TRIM(ErrorMessage)
  ENDIF
  IF (PRESENT(OutUnit2)) THEN
    WRITE(OutUnit2,ErrorFormat) TRIM(ErrorMessage)
  ENDIF


  RETURN

END SUBROUTINE ShowErrorMessage

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

