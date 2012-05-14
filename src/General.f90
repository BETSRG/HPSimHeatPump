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
PUBLIC  SolveRegulaFalsi
PUBLIC  POLYF
PUBLIC  InterpSw
PUBLIC  InterpBlind
PUBLIC  InterpSlatAng
PUBLIC  InterpProfAng
PUBLIC  InterpProfSlatAng
PUBLIC  BlindBeamBeamTrans
PUBLIC  POLY1F  ! Not currently used in EnergyPlus (Dec 2001)
PUBLIC  POLY2F  ! Not currently used in EnergyPlus (Dec 2001)
PUBLIC  TrimSigDigits ! used for better formatting of numeric variables
PUBLIC  RoundSigDigits ! used for better formatting of numeric variables
PUBLIC  MovingAvg ! used for smoothing a sequence of data by calculating a moving average
PUBLIC  ProcessDateString  ! Used by ScheduleManager and WeatherManager
PRIVATE ValidateMonthDay   ! Used internally here (ProcessDateString)
PUBLIC  InvJulianDay       ! Used by ScheduleManager and WeatherManager and internally here
PUBLIC  JulianDay          ! Used by ScheduleManager and WeatherManager
PUBLIC  CreateSysTimeIntervalString  ! Used in error messages for System Time Interval

CONTAINS

SUBROUTINE SolveRegulaFalsi(Eps, MaxIte, Flag, XRes, f, X_0, X_1, Par)

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
  REAL, INTENT(IN)     :: X_0    ! 1st bound of interval that contains the solution
  REAL, INTENT(IN)     :: X_1    ! 2nd bound of interval that contains the solution
  REAL, DIMENSION(:), INTENT(IN), OPTIONAL :: Par ! array with additional parameters used for function evaluation
                                                  ! optional
          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL, PARAMETER :: SMALL = 1.E-10

          ! INTERFACE BLOCK SPECIFICATIONS
  INTERFACE ! Interface to function to be solved for zero: f(X, Par) = 0
    FUNCTION f(X, Par) RESULT (Y)
      REAL, INTENT(IN) :: X
      REAL, INTENT(IN), DIMENSION(:), OPTIONAL :: Par
      REAL             :: Y
    END FUNCTION
  END INTERFACE

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL :: X0           ! present 1st bound
  REAL :: X1           ! present 2nd bound
  REAL :: XTemp        ! new estimate
  REAL :: Y0           ! f at X0
  REAL :: Y1           ! f at X1
  REAL :: YTemp        ! f at XTemp
  REAL :: DY          ! DY = Y0 - Y1
  LOGICAL :: Conv          ! flag, true if convergence is achieved
  LOGICAL :: StopMaxIte    ! stop due to exceeding of maximum # of iterations
  LOGICAL :: Cont          ! flag, if true, continue searching
  INTEGER  :: NIte         ! number of interations

  X0 = X_0
  X1 = X_1
  Conv       = .FALSE.
  StopMaxIte = .FALSE.
  Cont       = .TRUE.
  NIte = 0

  IF (PRESENT(Par)) THEN
    Y0 = f(X0, Par)
    Y1 = f(X1, Par)
  ELSE
    Y0 = f(X0)
    Y1 = f(X1)
  END IF
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
      YTemp = f(XTemp, Par)
    ELSE
      YTemp = f(XTemp)
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

REAL FUNCTION POLYF(X,A)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   February 1999
          !       DATE MODIFIED  October 1999, FW: change to 6th order polynomial over
          !                        entire incidence angle range

          ! PURPOSE OF THIS FUNCTION:
          ! Evaluates glazing beam transmittance or absorptance of the form
          ! A(1)*X + A(2)*X^2 + A(3)*X^3 + A(4)*X^4 + A(5)*X^5 + A(6)*X^6
          ! where X is the cosine of the angle of incidence (0.0 to 1.0)

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
REAL, INTENT(IN)                :: X   ! Cosine of angle of incidence
REAL, DIMENSION(6), INTENT(IN)  :: A   ! Polynomial coefficients

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

IF(X < 0.0 .OR. X > 1.0) THEN
  POLYF = 0.
ELSE
  POLYF = X*(A(1)+X*(A(2)+X*(A(3)+X*(A(4)+X*(A(5)+X*A(6))))))
END IF
RETURN
END FUNCTION POLYF


REAL FUNCTION InterpSw(SwitchFac,A,B)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   February 1999

          ! PURPOSE OF THIS FUNCTION:
          ! For switchable glazing, calculates a weighted average of properties
          ! A and B

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
REAL, INTENT(IN)  :: SwitchFac ! Switching factor: 0.0 if glazing is unswitched, = 1.0 if fully switched
REAL, INTENT(IN)  :: A        ! Glazing property in unswitched state
REAL, INTENT(IN)  :: B        ! Glazing property in fully switched state

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

InterpSw = (1.-SwitchFac)*A + SwitchFac*B
RETURN
END FUNCTION InterpSw

REAL FUNCTION InterpBlind(ProfAng,PropArray)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   May 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Does profile-angle interpolation of window blind solar-thermal properties

          ! METHODOLOGY EMPLOYED:
          ! Linear interpolation.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  USE DataGlobals, ONLY: Pi,PiOvr2

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
REAL, INTENT(IN), DIMENSION(37) :: PropArray  ! Array of blind properties
REAL, INTENT(IN)                ::  ProfAng        ! Profile angle (rad)

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
REAL InterpFac      ! Interpolation factor
REAL DeltaAng       ! Profile angle increment (rad)
INTEGER IAlpha      ! Profile angle index

DeltaAng = Pi/36.
IF(ProfAng > PiOvr2 .OR. ProfAng < -PiOvr2) THEN
  InterpBlind = 0.0
ELSE
  IAlpha = 1 + AINT((ProfAng+PiOvr2)/DeltaAng)
  InterpFac = (ProfAng - (-PiOvr2 + DeltaAng*(IAlpha-1)))/DeltaAng
  InterpBlind = (1-InterpFac)*PropArray(IAlpha) + InterpFac*PropArray(IAlpha+1)
END IF
RETURN
END FUNCTION InterpBlind

REAL FUNCTION InterpProfAng(ProfAng,PropArray)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   May 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Does profile-angle interpolation of window blind solar-thermal properties

          ! METHODOLOGY EMPLOYED:
          ! Linear interpolation.

          ! REFERENCES:na

IMPLICIT NONE

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

REAL PropArray(37)  ! Array of blind properties
REAL ProfAng        ! Profile angle (rad)
REAL InterpFac      ! Interpolation factor
REAL DeltaAng       ! Profile angle increment (rad)
REAL, PARAMETER    :: PI= 3.141592653589793   ! Pi
REAL, PARAMETER    :: PiOvr2 = PI/2.          ! Pi/2
INTEGER IAlpha      ! Profile angle index

DeltaAng = Pi/36
IF(ProfAng > PiOvr2 .OR. ProfAng < -PiOvr2) THEN
  InterpProfAng = 0.0
ELSE
  IAlpha = 1 + AINT((ProfAng+PiOvr2)/DeltaAng)
  InterpFac = (ProfAng - (-PiOvr2 + DeltaAng*(IAlpha-1)))/DeltaAng
  InterpProfAng = (1-InterpFac)*PropArray(IAlpha) + InterpFac*PropArray(IAlpha+1)
END IF
RETURN
END FUNCTION InterpProfAng

REAL FUNCTION InterpSlatAng(SlatAng,VarSlats,PropArray)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   Dec 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Does slat-angle interpolation of window blind solar-thermal properties that
          ! do not depend on profile angle

          ! METHODOLOGY EMPLOYED:
          ! Linear interpolation.

          ! REFERENCES:na

  USE DataGlobals,   ONLY : MaxSlatAngs

IMPLICIT NONE

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL PropArray(MaxSlatAngs) ! Array of blind properties as function of slat angle
REAL SlatAng        ! Slat angle (rad)
LOGICAL VarSlats    ! True if slat angle is variable
REAL InterpFac      ! Interpolation factor
REAL, PARAMETER    :: PI= 3.141592653589793   ! Pi
REAL, PARAMETER    :: DeltaAng =  PI/(MaxSlatAngs-1)
INTEGER IBeta       ! Slat angle index

IF(SlatAng > Pi .OR. SlatAng < 0.0) THEN
!  InterpSlatAng = 0.0
!  RETURN
!END IF
  SlatAng = MIN(MAX(SlatAng,0.0),PI)
END IF

IF(VarSlats) THEN  ! Variable-angle slats
  IBeta = 1 + AINT(SlatAng/DeltaAng)
  InterpFac = (SlatAng - DeltaAng*(IBeta-1))/DeltaAng
  InterpSlatAng = PropArray(IBeta) + &
       InterpFac*(PropArray(MIN(MaxSlatAngs,IBeta+1))-PropArray(IBeta))
ELSE               ! Fixed-angle slats or shade
  InterpSlatAng = PropArray(1)
END IF

RETURN
END FUNCTION InterpSlatAng

REAL FUNCTION InterpProfSlatAng(ProfAng,SlatAng,VarSlats,PropArray)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   Dec 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Does simultaneous profile-angle and slat-angle interpolation of window
          ! blind solar-thermal properties that depend on profile angle and slat angle

          ! METHODOLOGY EMPLOYED:
          ! Linear interpolation.

          ! REFERENCES:na

  USE DataGlobals,   ONLY : MaxSlatAngs

IMPLICIT NONE

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL PropArray(37,MaxSlatAngs) ! Array of blind properties
REAL ProfAng        ! Profile angle (rad)
REAL SlatAng        ! Slat angle (rad)
LOGICAL VarSlats    ! True if variable-angle slats
REAL ProfAngRatio   ! Profile angle interpolation factor
REAL SlatAngRatio   ! Slat angle interpolation factor
REAL, PARAMETER    :: PI= 3.141592653589793
REAL, PARAMETER    :: PIov2 = PI/2
REAL, PARAMETER    :: DeltaProfAng = PI/36.
REAL, PARAMETER    :: DeltaSlatAng = PI/(MaxSlatAngs-1)
INTEGER IAlpha      ! Profile angle index
INTEGER IBeta       ! Slat angle index
REAL Val1,Val2,Val3,Val4 ! Property values at points enclosing the given ProfAngle and SlatAngle
REAL ValA,ValB      ! Property values at given SlatAngle to be interpolated in profile angle

IF(SlatAng > Pi .OR. SlatAng < 0.0 .OR. ProfAng > PIov2 .OR. ProfAng < -PIov2) THEN
!  InterpProfSlatAng = 0.0
!  RETURN
  SlatAng = MIN(MAX(SlatAng,0.0),PI)
  ProfAng = MIN(MAX(SlatAng,-PIov2),PIov2)
END IF

IAlpha = AINT((ProfAng+PIov2)/DeltaProfAng) + 1
ProfAngRatio = (ProfAng + PIov2 - (IAlpha-1)*DeltaProfAng)/DeltaProfAng

IF(VarSlats) THEN  ! Variable-angle slats: interpolate in profile angle and slat angle
  IBeta  = AINT(SlatAng/DeltaSlatAng) + 1
  SlatAngRatio = (SlatAng - (IBeta-1)*DeltaSlatAng)/DeltaSlatAng
  Val1 = PropArray(IAlpha,IBeta)
  Val2 = PropArray(IAlpha,MIN(MaxSlatAngs,IBeta+1))
  Val3 = PropArray(MIN(37,IAlpha+1),IBeta)
  Val4 = PropArray(MIN(37,IAlpha+1),MIN(MaxSlatAngs,IBeta+1))
  ValA = Val1 + SlatAngRatio*(Val2-Val1)
  ValB = Val3 + SlatAngRatio*(Val4-Val3)
  InterpProfSlatAng = ValA + ProfAngRatio*(ValB-ValA)
ELSE      ! Fixed-angle slats: interpolate only in profile angle
  Val1 = PropArray(IAlpha,1)
  Val2 = PropArray(MIN(37,IAlpha+1),1)
  InterpProfSlatAng = Val1 + ProfAngRatio*(Val2-Val1)
END IF

RETURN
END FUNCTION InterpProfSlatAng

REAL FUNCTION BlindBeamBeamTrans(ProfAng,SlatAng,SlatWidth,SlatSeparation,SlatThickness)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   Jan 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates beam-to-beam transmittance of a window blind

          ! METHODOLOGY EMPLOYED:
          ! Based on solar profile angle and slat geometry

          ! REFERENCES:na

IMPLICIT NONE

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL ProfAng        ! Solar profile angle (rad)
REAL SlatAng        ! Slat angle (rad)
REAL SlatWidth      ! Slat width (m)
REAL SlatSeparation ! Slat separation (distance between surfaces of adjacent slats) (m)
REAL SlatThickness  ! Slat thickness (m)
REAL fEdge          ! Slat edge correction factor
REAL wbar           ! Intermediate variable
REAL gamma          ! Intermediate variable
REAL fEdge1         ! Intermediate variable
REAL CosProfAng     ! Cosine of profile angle
REAL, PARAMETER    :: PI= 3.141592653589793   ! Pi
REAL, PARAMETER    :: PiOvr2 = PI/2.

CosProfAng = COS(ProfAng)
gamma = SlatAng - ProfAng
wbar = SlatSeparation
IF(CosProfAng /= 0.0) wbar = SlatWidth * COS(gamma)/CosProfAng
BlindBeamBeamTrans = MAX(0.0,1.-ABS(wbar/SlatSeparation))

IF(BlindBeamBeamTrans > 0.) THEN

  ! Correction factor that accounts for finite thickness of slats. It is used to modify the
  ! blind transmittance to account for reflection and absorption by the slat edges.
  ! fEdge is ratio of area subtended by edge of slat to area between tops of adjacent slats.

  fEdge  = 0.
  fEdge1 = 0.
  IF(ABS(SIN(gamma))>0.01) THEN
    IF((SlatAng > 0.0 .AND. SlatAng <= PiOvr2 .AND. ProfAng <= SlatAng) .OR. &
       (SlatAng > PiOvr2 .AND. SlatAng <= Pi .AND. ProfAng > -(Pi-SlatAng))) &
      fEdge1 = SlatThickness * ABS(SIN(gamma)) / &
                  ((SlatSeparation + SlatThickness/ABS(SIN(SlatAng)))*CosProfAng)
    fEdge = MIN(1.0,ABS(fEdge1))
  END IF
  BlindBeamBeamTrans = BlindBeamBeamTrans * (1.-fEdge)

END IF

END FUNCTION BlindBeamBeamTrans

REAL FUNCTION POLY1F(X,A,N)

          ! FUNCTION INFORMATION:
          !       AUTHOR         George N. Walton
          !       DATE WRITTEN   May 1977
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function evaluates a polynomial of the form:
          ! POLY = A(1) + A(2)*X + A(3)*X**2 + ... + A(N)*X**(N-1)

          ! METHODOLOGY EMPLOYED:
          ! Uses Horner's Rule.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER  N     ! number of terms in polynomial
  REAL     X     ! independent variable
  REAL     A(N)  ! array of polynomial coefficients

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER  I    ! Loop parameter
  REAL     SUM  ! Temporary summation variable

  SUM=A(N)
  DO I=2,N
    SUM=SUM*X+A(N-I+1)
  ENDDO

  POLY1F=SUM

  RETURN

END FUNCTION POLY1F

REAL FUNCTION POLY2F(X,A,N)
          ! FUNCTION INFORMATION:
          !       AUTHOR         George N. Walton
          !       DATE WRITTEN   May 1977
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function evaluates a polynomial of the form:
          ! POLY = A(1)*X + A(2)*X**2 + ... + A(N)*X**N

          ! METHODOLOGY EMPLOYED:
          ! Uses Horner's Rule.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER  N     ! number of terms in polynomial
  REAL     X     ! independent variable
  REAL     A(N)  ! array of polynomial coefficients

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER  I    ! Loop parameter
  REAL     SUM  ! Temporary summation variable

  SUM=A(N)*X
  DO I=2,N
    SUM=X*(SUM+A(N-I+1))
  ENDDO

  POLY2F=SUM

  RETURN

END FUNCTION POLY2F

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

SUBROUTINE MovingAvg(DataIn,NumDataItems,NumItemsInAvg,SmoothedData)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   January 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Smooth the data in the 1-d array DataIn by averaging over a window NumItemsInAvg
          ! wide. Return the results in the 1-d array SmoothedData

          ! METHODOLOGY EMPLOYED:
          ! Note that DataIn and SmoothedData should have the same size. This is the reponsibility
          ! of the calling routine. NumItemsInAvg should be no bigger than the size of DataIn.

          ! REFERENCES:
          ! na.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) ::             NumDataItems               ! number of values in DataIn
  REAL, INTENT(IN), DIMENSION(NumDataItems) ::  DataIn          ! input data that needs smoothing
  INTEGER, INTENT(IN) ::             NumItemsInAvg              ! number of items in the averaging window
  REAL, INTENT(OUT), DIMENSION(NumDataItems) :: SmoothedData    ! output data after smoothing

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS

          ! DERIVED TYPE DEFINITIONS

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: i             ! loop index
  INTEGER :: j             ! inner loop index
  REAL, DIMENSION(:), ALLOCATABLE :: TempData ! a scratch array

  ALLOCATE(TempData(3*NumDataItems))

  DO i=1,NumDataItems
    TempData(i) = DataIn(i)
    TempData(NumDataItems+i) = DataIn(i)
    TempData(2*NumDataItems+i) = DataIn(i)
    SmoothedData(i) = 0.0
  END DO

  DO i=1,NumDataItems
    DO j=1,NumItemsInAvg
      SmoothedData(i) = SmoothedData(i) + TempData(NumDataItems+i-NumItemsInAvg+j)
    END DO
    SmoothedData(i) = SmoothedData(i) / REAL(NumItemsInAvg)
  END DO

  DEALLOCATE(TempData)

  RETURN

END SUBROUTINE MovingAvg

SUBROUTINE ProcessDateString(String,PMonth,PDay,PWeekDay,DateType,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   December 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine will process a date from a string and determine
          ! the proper month and day for that date string.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: ProcessNumber, FindItemInList
  USE DataStringGlobals
  USE DataGlobals, ONLY: ShowSevereError, ShowFatalError, ShowWarningError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: String
  INTEGER, INTENT(OUT) :: PMonth
  INTEGER, INTENT(OUT) :: PDay
  INTEGER, INTENT(OUT) :: PWeekDay
  INTEGER, INTENT(OUT) :: DateType      ! DateType found (-1=invalid, 1=month/day, 2=nth day in month, 3=last day in month)
  LOGICAL, INTENT(INOUT) :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER FstNum
  LOGICAL ErrFlag
  INTEGER NumTokens
  INTEGER TokenDay
  INTEGER TokenMonth
  INTEGER TokenWeekDay

  FstNum=ProcessNumber(String,ErrFlag)
  DateType=-1
  IF (.not. ErrFlag) THEN
    ! Entered single number, do inverse JDay
    IF (FstNum == 0) THEN
      PMonth=0
      PDay=0
      DateType=1
    ELSEIF (FstNum < 0 .or. FstNum > 366) THEN
      CALL ShowSevereError('Invalid Julian date Entered='//TRIM(String))
      ErrorsFound=.true.
    ELSE
      CALL InvJulianDay(FstNum,PMonth,PDay,0)
      DateType=1
    ENDIF
  ELSE
    ! Error when processing as number, try x/x
    CALL DetermineDateTokens(String,NumTokens,TokenDay,TokenMonth,TokenWeekDay,DateType,ErrorsFound)
    IF (DateType == 1) THEN
      PDay=TokenDay
      PMonth=TokenMonth
    ELSEIF (DateType == 2 .or. DateType == 3) THEN
      ! interpret as TokenDay TokenWeekDay in TokenMonth
      PDay=TokenDay
      PMonth=TokenMonth
      PWeekDay=TokenWeekDay
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE ProcessDateString

SUBROUTINE DetermineDateTokens(String,NumTokens,TokenDay,TokenMonth,TokenWeekday,DateType,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is invoked for date fields that appear to be strings (give
          ! error when ProcessNumber is used).

          ! METHODOLOGY EMPLOYED:
          ! Delete everything that is extraneous to the date information needed.  Process what
          ! is left.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: MaxNameLength, ShowSevereError, ShowFatalError
  USE InputProcessor, ONLY: FindItemInList,ProcessNumber

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: String
  INTEGER, INTENT(OUT) :: NumTokens     ! Number of tokens found in string
  INTEGER, INTENT(OUT) :: TokenDay      ! Value of numeric field found
  INTEGER, INTENT(OUT) :: TokenMonth    ! Value of Month field found (1=Jan, 2=Feb, etc)
  INTEGER, INTENT(OUT) :: TokenWeekDay  ! Value of Weekday field found (1=Sunday, 2=Monday, etc), 0 if none
  INTEGER, INTENT(OUT) :: DateType      ! DateType found (-1=invalid, 1=month/day, 2=nth day in month, 3=last day in month)
  LOGICAL, INTENT(OUT) :: ErrorsFound   ! Set to true if cannot process this string as a date

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=1), PARAMETER :: Blank=' '
  INTEGER, PARAMETER :: NumSingleChars=3
  CHARACTER(len=1), PARAMETER, DIMENSION(NumSingleChars) :: SingleChars=(/"/",":","-"/)
  INTEGER, PARAMETER :: NumDoubleChars=6
  CHARACTER(len=3), PARAMETER, DIMENSION(NumDoubleChars) :: DoubleChars=(/"ST ","ND ","RD ","TH ","OF ","IN "/)
  CHARACTER(len=*), PARAMETER, DIMENSION(12) :: Months=(/"JAN","FEB","MAR","APR","MAY", &
                    "JUN","JUL","AUG","SEP","OCT","NOV","DEC"/)
  CHARACTER(len=*), PARAMETER, DIMENSION(7) :: Weekdays=(/"SUN","MON","TUE","WED","THU","FRI","SAT"/)
  CHARACTER(len=*), PARAMETER :: Numbers="0123456789"


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength) :: CurrentString
  INTEGER Pos
  INTEGER Loop
  CHARACTER(len=15), DIMENSION(3) :: Fields
  INTEGER NumField1
  INTEGER NumField2
  LOGICAL ErrFlag
  LOGICAL InternalError

  CurrentString=String
  NumTokens=0
  TokenDay=0
  TokenMonth=0
  TokenWeekday=0
  DateType=-1
  InternalError=.false.
  ! Take out separator characters, other extraneous stuff

  DO Loop=1,NumSingleChars
    Pos=INDEX(CurrentString,SingleChars(Loop))
    DO WHILE (Pos > 0)
      CurrentString(Pos:Pos)=' '
      Pos=INDEX(CurrentString,SingleChars(Loop))
    ENDDO
  ENDDO

  DO Loop=1,NumDoubleChars
    Pos=INDEX(CurrentString,DoubleChars(Loop))
    DO WHILE (Pos > 0)
      CurrentString(Pos:Pos+1)='  '
      Pos=INDEX(CurrentString,DoubleChars(Loop))
    ENDDO
  ENDDO

  CurrentString=ADJUSTL(CurrentString)
  IF (CurrentString == Blank) THEN
    CALL ShowSevereError('Invalid date field='//TRIM(String))
    ErrorsFound=.true.
  ELSE
    Loop=0
    DO WHILE (Loop < 3)  ! Max of 3 fields
      IF (CurrentString == Blank) EXIT
      Pos=INDEX(CurrentString,' ')
      Loop=Loop+1
      Fields(Loop)=CurrentString(1:Pos-1)
      CurrentString=CurrentString(Pos:)
      CurrentString=ADJUSTL(CurrentString)
    ENDDO
    IF (CurrentString /= Blank) THEN
      CALL ShowSevereError('Invalid date field='//TRIM(String))
      ErrorsFound=.true.
    ELSEIF (Loop == 2) THEN
      ! Field must be Day Month or Month Day (if both numeric, mon / day)
      InternalError=.false.
      NumField1=ProcessNumber(Fields(1),ErrFlag)
      IF (ErrFlag) THEN
        ! Month day, but first field is not numeric, 2nd must be
        NumField2=ProcessNumber(Fields(2),ErrFlag)
        IF (ErrFlag) THEN
          CALL ShowSevereError('Invalid date field='//TRIM(String))
          InternalError=.true.
        ELSE
          TokenDay=NumField2
        ENDIF
        TokenMonth=FindItemInList(Fields(1)(1:3),Months,12)
        CALL ValidateMonthDay(String,TokenDay,TokenMonth,InternalError)
        IF (.not. InternalError) THEN
          DateType=1
        ELSE
          ErrorsFound=.true.
        ENDIF
      ELSE
        ! Month Day, first field was numeric, if 2nd is, then it's month<num> day<num>
        NumField2=ProcessNumber(Fields(2),ErrFlag)
        IF (.not. ErrFlag) THEN
          TokenMonth=NumField1
          TokenDay=NumField2
          CALL ValidateMonthDay(String,TokenDay,TokenMonth,InternalError)
          IF (.not. InternalError) THEN
            DateType=1
          ELSE
            ErrorsFound=.true.
          ENDIF
        ELSE  ! 2nd field was not numeric.  Must be Month
          TokenDay=NumField1
          TokenMonth=FindItemInList(Fields(2)(1:3),Months,12)
          CALL ValidateMonthDay(String,TokenDay,TokenMonth,InternalError)
          IF (.not. InternalError) THEN
            DateType=1
            NumTokens=2
          ELSE
            ErrorsFound=.true.
          ENDIF
        ENDIF
      ENDIF
    ELSEIF (Loop == 3) THEN
      ! Field must be some combination of <num> Weekday Month
      NumField1=ProcessNumber(Fields(1),ErrFlag)
      IF (.not. ErrFlag) THEN ! the expected result
        TokenDay=NumField1
        TokenWeekDay=FindItemInList(Fields(2)(1:3),Weekdays,7)
        IF (TokenWeekDay == 0) THEN
          TokenMonth=FindItemInList(Fields(2)(1:3),Months,12)
          TokenWeekDay=FindItemInList(Fields(3)(1:3),Weekdays,7)
          IF (TokenMonth == 0 .or. TokenWeekDay == 0) InternalError=.true.
        ELSE
          TokenMonth=FindItemInList(Fields(3)(1:3),Months,12)
          IF (TokenMonth == 0) InternalError=.true.
        ENDIF
        DateType=2
        NumTokens=3
        IF (TokenDay < 0 .or. TokenDay > 5) InternalError=.true.
      ELSE   ! first field was not numeric....
        IF (Fields(1) == 'LA ') THEN
          DateType=3
          NumTokens=3
          TokenWeekDay=FindItemInList(Fields(2)(1:3),Weekdays,7)
          IF (TokenWeekDay == 0) THEN
            TokenMonth=FindItemInList(Fields(2)(1:3),Months,12)
            TokenWeekDay=FindItemInList(Fields(3)(1:3),Weekdays,7)
            IF (TokenMonth == 0 .or. TokenWeekDay == 0) InternalError=.true.
          ELSE
            TokenMonth=FindItemInList(Fields(3)(1:3),Months,12)
            IF (TokenMonth == 0) InternalError=.true.
          ENDIF
        ELSE  ! error....
          CALL ShowSevereError('First date field not numeric, field='//TRIM(String))
        ENDIF
      ENDIF
    ELSE
      ! Not enough or too many fields
      CALL ShowSevereError('Invalid date field='//TRIM(String))
      ErrorsFound=.true.
    ENDIF
  ENDIF

  IF (InternalError) THEN
    DateType=-1
    ErrorsFound=.true.
  ENDIF

  RETURN

END SUBROUTINE DetermineDateTokens

SUBROUTINE ValidateMonthDay(String,Day,Month,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine validates a potential Day, Month values, produces an error
          ! message when not valid, and sets error flag.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: ShowSevereError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: String  ! Real string being processed
  INTEGER, INTENT(IN) :: Day
  INTEGER, INTENT(IN) :: Month
  LOGICAL, INTENT(OUT) :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER, DIMENSION(12) :: EndMonthDay=(/31,29,31,30,31,30,31,31,30,31,30,31/)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL InternalError

  InternalError=.false.
  IF (Month < 1 .or. Month > 12) InternalError=.true.
  IF (.not. InternalError) THEN
    IF (Day < 1 .or. Day > EndMonthDay(Month)) InternalError=.true.
  ENDIF
  IF (InternalError) THEN
    CALL ShowSevereError('Invalid Month Day date format='//TRIM(String))
    ErrorsFound=.true.
  ENDIF


  RETURN

END SUBROUTINE ValidateMonthDay

INTEGER FUNCTION JulianDay (Month,Day,LeapYearValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   September 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  from JDAYF in BLAST/IBLAST

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine returns the appropriate Julian Day value for the input
          ! Month and Day.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: Month         ! Month, 1..12
  INTEGER, INTENT(IN) :: Day           ! Day of Month, not validated by month
  INTEGER, INTENT(IN) :: LeapYearValue ! 1 if leap year indicated, 0 if not
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER, DIMENSION(12) :: EndDayofMonth = (/31,59,90,120,151,181,212,243,273,304,334,365/)
                            ! End day numbers of each month (without Leap Year)
!
      SELECT CASE (Month)

      CASE(1)
!                                       CASE 1: JANUARY
        JulianDay = Day

      CASE(2)
!                                       CASE 2: FEBRUARY
        JulianDay = Day + EndDayofMonth(1)

      CASE(3:12)
!                                       CASE 3: REMAINING MONTHS
        JulianDay= Day + EndDayofMonth(Month-1) + LeapYearValue

      END SELECT

      RETURN

END FUNCTION JulianDay

SUBROUTINE InvJulianDay(Number,PMonth,PDay,LeapYr)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   December 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine performs and inverse Julian Day
          ! calculation, using an input JulianDay and returning
          ! appropriate Month and Day.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: Number
  INTEGER, INTENT(OUT) :: PMonth
  INTEGER, INTENT(OUT) :: PDay
  INTEGER, INTENT(IN)  :: LeapYr

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER, DIMENSION(0:12) :: EndOfMonth=(/0,31,59,90,120,151,181,212,243,273,304,334,365/)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER WMonth
  INTEGER LeapAddPrev
  INTEGER LeapAddCur

  IF (Number < 0 .or. Number > 366) RETURN
  DO WMonth=1,12
    IF (WMonth == 1) THEN
      LeapAddPrev=0
      LeapAddCur=0
    ELSEIF (WMonth == 2) THEN
      LeapAddPrev=0
      LeapAddCur=LeapYr
    ELSE
      LeapAddPrev=LeapYr
      LeapAddCur=LeapYr
    ENDIF
    IF (Number > (EndOfMonth(WMonth-1)+LeapAddPrev) .and. Number <= (EndOfMonth(WMonth)+LeapAddCur)) EXIT
  ENDDO
  PMonth=WMonth
  PDay=Number-(EndOfMonth(WMonth-1)+LeapAddCur)


  RETURN

END SUBROUTINE InvJulianDay

FUNCTION CreateSysTimeIntervalString() RESULT(OutputString)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   April 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function creates the current time interval of the system
          ! time step.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY: CurrentTime,TimeStepZone,outputfiledebug
  USE DataHVACGlobals, ONLY: TimeStepSys,SysTimeElapsed

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=30) :: OutputString

          ! FUNCTION PARAMETER DEFINITIONS:
  CHARACTER(len=*),PARAMETER :: TstmpFmt="(I2.2,':',F3.0)"
  REAL,PARAMETER :: FracToMin=60.0

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL ActualTimeS  ! Start of current interval (HVAC time step)
  REAL ActualTimeE  ! End of current interval (HVAC time step)
  CHARACTER(len=10) TimeStmpS  ! Character representation of start of interval
  CHARACTER(len=10) TimeStmpE  ! Character representation of end of interval

  ActualTimeS=INT(CurrentTime)+(SysTimeElapsed+(CurrentTime - INT(CurrentTime)))
  ActualtimeE=ActualTimeS+TimeStepSys
  WRITE(TimeStmpS,TStmpFmt) INT(ActualTimeS),(ActualTimeS - INT(ActualTimeS))*FracToMin
  IF (TimeStmpS(4:4) == ' ') TimeStmpS(4:4)='0'
  TimeStmpS(6:6)=' '
  TimeStmpS=ADJUSTL(TimeStmpS)

  WRITE(TimeStmpE,TStmpFmt) INT(ActualTimeE),(ActualTimeE - INT(ActualTimeE))*FracToMin
  IF (TimeStmpE(4:4) == ' ') TimeStmpE(4:4)='0'
  TimeStmpE=ADJUSTL(TimeStmpE)
  TimeStmpE(6:6)=' '

  OutputString=TRIM(TimeStmpS)//' - '//TRIM(TimeStmpE)

  RETURN

END FUNCTION CreateSysTimeIntervalString

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
