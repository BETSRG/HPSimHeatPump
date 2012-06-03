MODULE DataStopCodes      ! EnergyPlus Data-Only Module

!globally declare program exit codes to alert calling routine to exit status
!will become useful during parametrics, test suites, or otherwise
!exit codes should be 0 for success (not necessary), or 1-255 for specific error codes
!note that sending the exit status back to the OS is not mandatory according to FORTRAN standards, but Intel 11 should do it properly

IMPLICIT NONE

PUBLIC

! normal exit status
INTEGER, PARAMETER :: exit_Normal = 0

! File I/O type exits
INTEGER, PARAMETER :: exit_FileIO_Missing_HPData = 2

! Code diagnostics
INTEGER, PARAMETER :: exit_Diagnostic_RefrigerantName = 52

! Other exit types
INTEGER, PARAMETER :: exit_SimProblem_BadInitialization = 102

END MODULE
