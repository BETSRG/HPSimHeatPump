MODULE DataGlobals      ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   January 1997
          !       MODIFIED       May 1997 (RKS) Added Weather Variables
          !       MODIFIED       December 1997 (RKS,DF,LKL) Split into DataGlobals and DataEnvironment
          !       MODIFIED       February 1999 (FW) Added NextHour, WGTNEXT, WGTNOW
          !       MODIFIED       September 1999 (LKL) Rename WGTNEXT,WGTNOW for clarity
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This data-only module is a repository for all variables which are considered
          ! to be "global" in nature in EnergyPlus.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
          ! None!--This module is USEd by all other modules; it should not USE anything.

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.


          ! MODULE PARAMETER DEFINITIONS:
          INTEGER, PARAMETER :: MaxNameLength = 200      ! Maximum Name Length in Characters
          REAL, PARAMETER    :: AutoSize = -99999.
          CHARACTER(len=55), PARAMETER :: UpperCase='ABCDEFGHIJKLMNOPQRSTUVWXYZ¿¡¬√ƒ≈∆«»… ÀÃÕŒœ–—“”‘’÷ÿŸ⁄€‹›'
          CHARACTER(len=55), PARAMETER :: LowerCase='abcdefghijklmnopqrstuvwxyz‡·‚„‰ÂÊÁËÈÍÎÏÌÓÔÒÚÛÙıˆ¯˘˙˚¸˝'
          CHARACTER(len=255) :: ProgramPath=' '     ! Path for Program, Energy+.ini
          CHARACTER(len=270) :: FullName=' '        ! Full name of file to open, including path
          CHARACTER(len=120) :: VerString='EnergyPlus, Version 1.1.1'      ! String that represents version information
          INTEGER :: TotalSevereErrors = 0 ! Counter
          INTEGER :: TotalWarningErrors = 0 ! Counter
          DOUBLE PRECISION   :: Elapsed_Time=0.0          ! For showing elapsed tiem at end of run
          CHARACTER*80 :: RefName

          ! MODULE VARAIBLES:
!INTEGER :: OutputFileInits      ! Unit number for the standard Initialization output file

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
!  INTERFACE
!    SUBROUTINE ShowMessage(Message,Unit1,Unit2)
!    !  Use when you want to create your own message for the error file.
!    CHARACTER(len=*) Message    ! Message automatically written to "error file"
!    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
!    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
!    END SUBROUTINE
!  END INTERFACE
!  INTERFACE
!    SUBROUTINE ShowContinueError(Message,Unit1,Unit2)
!    !  Use when you are "continuing" an error message over several lines.
!    CHARACTER(len=*) Message    ! Message automatically written to "error file"
!    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
!    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
!    END SUBROUTINE
!  END INTERFACE
!  INTERFACE
!    SUBROUTINE ShowFatalError(Message,Unit1,Unit2)
!    !  Use when you want the program to terminate after writing messages
!    !  to appropriate files
!    CHARACTER(len=*) Message    ! Message automatically written to "error file"
!    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
!    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
!    END SUBROUTINE
!  END INTERFACE
!  INTERFACE
!    SUBROUTINE ShowSevereError(Message,Unit1,Unit2)
!    !  Use for "severe" error messages.  Might have several severe tests and then terminate.
!    CHARACTER(len=*) Message    ! Message automatically written to "error file"
!    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
!    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
!    END SUBROUTINE
!  END INTERFACE
!  INTERFACE
!    SUBROUTINE ShowWarningError(Message,Unit1,Unit2)
!    !  Use for "warning" error messages.
!    CHARACTER(len=*) Message    ! Message automatically written to "error file"
!    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
!    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
!    END SUBROUTINE
!  END INTERFACE

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

END MODULE DataGlobals
