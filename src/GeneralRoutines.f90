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
PUBLIC  SolveRegulaFalsi

CONTAINS

SUBROUTINE SolveRegulaFalsi(Eps, MaxIte, Flag, XRes, f, X_0, DeltaX,IError,Par)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   March 1999
          !       MODIFIED       Fred Buhl November 2000
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Find the value of x between x0 and x1 such that f(x,[,Par])
          ! is equal to zero.

          ! METHODOLOGY EMPLOYED:
          ! Uses the Regula Falsi (false position) method (similar to secant method)

          ! REFERENCES:
          ! See Press et al., Numerical Recipes in Fortran, Cambridge University Press,
          ! 2nd edition, 1992. Page 347 ff.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL, INTENT(IN)     :: Eps    ! required absolute accuracy
  INTEGER, INTENT(IN)  :: MaxIte ! maximum number of allowed iterations
  INTEGER, INTENT(OUT) :: Flag   ! integer storing exit status
                                 ! = -2: f(x0) and f(x1) have the same sign
                                 ! = -1: no convergence
                                 ! >  0: number of iterations performed
  REAL, INTENT(OUT)    :: XRes   ! value of x that solves f(x [,Par]) = 0
  REAL, INTENT(INOUT)     :: X_0    ! 1st bound of interval that contains the solution
  REAL, INTENT(IN)     :: DeltaX    ! 2nd bound of interval that contains the solution
  INTEGER, INTENT(OUT) :: IError
  REAL, DIMENSION(:), INTENT(IN), OPTIONAL :: Par ! array with additional parameters used for function evaluation
                                                  ! optional
          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL, PARAMETER :: SMALL = 1.E-10

          ! INTERFACE BLOCK SPECIFICATIONS
  INTERFACE ! Interface to function to be solved for zero: f(X, Par) = 0
    FUNCTION f(X,IERR,Par) RESULT (Y)
     REAL, INTENT(IN) :: X
     INTEGER, INTENT(IN),OPTIONAL :: IERR
     REAL, INTENT(IN), DIMENSION(:), OPTIONAL :: Par
     REAL             :: Y
    END FUNCTION
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL     :: X0           ! present 1st bound
  REAL     :: X1           ! present 2nd bound
  REAL     :: X_1           ! present 2nd bound
  REAL     :: XTemp        ! new estimate
  REAL     :: Y0           ! f at X0
  REAL     :: Y1           ! f at X1
  REAL     :: Y_0           ! f at X0
  REAL     :: Y_1           ! f at X1
  REAL     :: YTemp        ! f at XTemp
  REAL     :: DY          ! DY = Y0 - Y1
  LOGICAL  :: Conv          ! flag, true if convergence is achieved
  LOGICAL  :: StopMaxIte    ! stop due to exceeding of maximum # of iterations
  LOGICAL  :: Cont          ! flag, if true, continue searching
  INTEGER  :: NIte         ! number of interations
  INTEGER  :: IERR
  
  IError = 0
  
  CALL BracketSolution(X_0,Y_0,X_1,Y_1,f,DeltaX,Eps,IError)
  
  X0 = X_0
  X1 = X_1
  Conv       = .FALSE.
  StopMaxIte = .FALSE.
  Cont       = .TRUE.
  NIte = 0

  !IF (PRESENT(Par)) THEN
  !  Y0 = f(X0, Par)
  !  Y1 = f(X1, Par)
  !ELSE
  !  Y0 = f(X0)
  !  Y1 = f(X1)
  !END IF
  Y0 = Y_0
  Y1 = Y_1
  ! check initial values
  IF ( Y0*Y1 > 0 ) THEN
    Flag = -2
    XRes = X0
    RETURN
  END IF

  DO WHILE (Cont)

    DY = Y0 - Y1
    IF (ABS(DY) < SMALL)    DY = SMALL
    ! new estimation
    XTemp = (Y0 * X1 - Y1 * X0 ) / DY
    IF (PRESENT(Par)) THEN
      YTemp = f(XTemp,IERR,Par)
    ELSE
      YTemp = f(XTemp,IERR)
    END IF

    NIte = NIte + 1

    ! check convergence
    IF (ABS(YTemp) < Eps) Conv = .TRUE.

    IF (NIte > MaxIte) StopMaxIte = .TRUE.

    IF ((.NOT.Conv).AND.(.NOT.StopMaxIte)) THEN
      Cont = .TRUE.
    ELSE
      Cont = .FALSE.
    END IF

    IF (Cont) THEN

    ! reassign values (only if further iteration required)
      IF ( Y0 < 0 ) THEN
        IF ( YTemp < 0 ) THEN
          X0 = XTemp
          Y0 = YTemp
        ELSE
          X1 = XTemp
          Y1 = YTemp
        END IF
      ELSE
        IF ( YTemp < 0 ) THEN
          X1 = XTemp
          Y1 = YTemp
        ELSE
          X0 = XTemp
          Y0 = YTemp
        END IF
      END IF ! ( Y0 < 0 )

    END IF ! (Cont)

  END DO ! Cont

  IF (Conv) THEN
    Flag = NIte
  ELSE
    Flag = -1
  END IF
  XRes = XTemp

RETURN

END SUBROUTINE SolveRegulaFalsi

SUBROUTINE BracketSolution(X1,Y1,X2,Y2,f,DeltaX,Tol,IERROR)

 REAL, INTENT(INOUT)    :: X1
 REAL, INTENT(INOUT)    :: Y1
 REAL, INTENT(INOUT)    :: X2
 REAL, INTENT(INOUT)    :: Y2
 INTERFACE ! Interface to function to be solved for zero: f(X, Par) = 0
   FUNCTION f(X,IErr,Par) RESULT (Y)
     REAL, INTENT(IN)    :: X
     INTEGER, INTENT(IN),OPTIONAL :: IERR
     REAL, INTENT(IN), DIMENSION(:), OPTIONAL :: Par
     REAL             :: Y
   END FUNCTION
 END INTERFACE
 REAL, INTENT(IN)       :: DeltaX
 REAL, INTENT(IN)       :: Tol
 INTEGER, INTENT(OUT) :: IERROR
 
 INTEGER IERR
 INTEGER  :: ICount = 0
 INTEGER  :: Sign = 1
 REAL     :: YMin
 REAL     :: DDX
 REAL     :: Slope
 LOGICAL  :: SearchingBracket 
 
 IERR = -999
 SearchingBracket = .TRUE.
 ICOUNT = 0
 IERROR = 0
 SIGN = 1
 
 DO WHILE(IERR .NE. 0)
  ICount = ICount+1 
  Y1=f(X1,IERR)
  X2 = X1
  IF(IERR == 0) THEN
   EXIT
  ELSE IF(IERR == 1) THEN
   X1 = X1 - DeltaX
  ELSE
   X1 = X1 + DeltaX
  END IF
  
  IF(ICount .GE. 30) THEN
   !Put Error Message
   IERROR = -4
   RETURN
  END IF  
 END DO 
 
 YMin = ABS(Y1)
 IF(YMin .LE. Tol) RETURN

 IF(Y1 .GT. 0.0) SIGN = -1.0
 DDX = ABS(DeltaX)

 X2 = X2 + 2.0**(ICOUNT-1)*DeltaX*SIGN      !To bracket root ISI - 03/26/04
 
 DO WHILE(SearchingBracket)
  Y2=f(X2,IERR)
  ICOUNT = ICOUNT + 1
  
  IF(IERR .NE. 0) THEN
   X2 = (X1 + X2)/2.
  END IF
  
  IF(ICount .GE. 30) THEN
  !Put Error Message
   IERROR = -4
   RETURN
  END IF  

  YMIN = AMIN1(YMIN,ABS(Y2))
 
  IF (YMIN .LE. TOL) RETURN              !ISI - 02/12/06

  DDX = ABS(DeltaX)
  DDX=2.0**(ICOUNT-1)*ABS(DeltaX)				!ISI - 05/10/04

  IF (Y1*Y2 .LE. 0.) THEN
   EXIT
  END IF 
  IF(ICount .GE. 30) THEN
  !Put Error Message
   IERROR = -4
   RETURN
  END IF  

!	IF SLOPE IS POSITIVE AND "Y" IS GREATER THAN 0. OR
!	IF SLOPE IS NEGATIVE AND "Y" IS LESS THAN 0. MOVE THE LOWER
!	POINT TO THE LEFT
  Slope = (Y2 - Y1)/(X2 - X1)
  IF (Slope*Y1 .LT. 0.) THEN
   IF(X1 .GT. X2) THEN
    X2 = X1 + DDX
   ELSE 
    X1 = X2
    Y1 = Y2
    X2 = X1 + DDX
   END IF
  ELSE
   IF(X1 .LT. X2) THEN
    X2 = X1 - DDX
   ELSE 
    X1 = X2
    Y1 = Y2
    X2 = X1 - DDX
   END IF
  END IF
 
 END DO   

 RETURN
 
END SUBROUTINE BracketSolution

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
