MODULE MoistureBalanceEMPDManager

    ! Module containing the routines to calculate moisture adsorption and desorption
    ! at interior wall surfaces

    ! MODULE INFORMATION:
    !   Authors:        Muthusamy Swami and Lixing Gu
    !   Date written:   August, 1999
    !   Modified:       na
    !   Re-engineered:  na

    ! PURPOSE OF THIS MODULE:
    ! To calculate moisture adsorption and desorption at interior wall surfaces
    ! using EMPD model (Effective Moisture Penetration Depth) developed by
    ! Florida Solar Energy Center. Input consists of interior surface temperatures
    ! and sorption curve of interior layer materials. Output consists of mositure
    ! fluxes from wall interior surfaces, which will be used in zone moisture balance.

    ! METHODOLOGY EMPLOYED:
    ! Add something
    ! EMPD is a simplified method of analysing moisture transport in buildings and
    ! is easy to incorporate into existing building energy analysis computer codes.
    ! The components of the moisture balance equation involving moisture adsorption
    ! and desorption are described in detail where the concept of EMPD is discussed.
    ! The assumptions. parameters required, and limitations of the model are also discussed.
    ! Results of simulation using the model and comparison with measured data are given.
    ! Data of isotherms compiled from the literature of some commonly used building materials are also given.

    ! REFERENCES:
    ! Kerestecioglu A A., Swami M V., Kamel A A., "Theoretical and computational
    ! investigation of simultaneous heat and moisture transfer in buildings: 'Effective
    ! penetration depth' theory," ASHRAE Trans., 1990, Vol. 96, Part 1, 447-454

    ! OTHER NOTES:

    ! USE STATEMENTS:
    ! Use statements for data used in the module
USE DataPrecisionGlobals
USE DataEnvironment, ONLY: OutBaroPress
USE DataHeatBalance
USE DataGlobals
USE DataHeatBalFanSys, Only: ZoneAirHumRat
USE DataSurfaces, Only: TotSurfaces, Surface, SurfaceClass_Window
USE DataMoistureBalanceEMPD
USE DataInterfaces

IMPLICIT NONE       ! Enforce explicit typing of all variables

! MODULE VARIABLE and Function DECLARATIONs
REAL, Allocatable, Dimension(:)   :: RhoVapEMPD           ! Inside Surface Vapor Density Reporting variable
REAL, Allocatable, Dimension(:)   :: WSurfEMPD           ! Inside Surface Humidity Ratio Reporting variable
REAL, Allocatable, Dimension(:)   :: RHEMPD           ! Inside Surface Relative Humidity Reporting variable

! SUBROUTINE SPECIFICATION FOR MODULE MoistureBalanceEMPDManager
PRIVATE InitMoistureBalanceEMPD
PUBLIC CloseMoistureBalanceEMPD
PUBLIC CalcMoistureBalanceEMPD
PRIVATE SolverMoistureBalanceEMPD
PRIVATE GetMoistureBalanceEMPDInput
PUBLIC UpdateMoistureBalanceEMPD
PRIVATE ReportMoistureBalanceEMPD

CONTAINS

!******************************************************************************
SUBROUTINE GetMoistureBalanceEMPDInput  ! Moisture Balance EMPD Input Reader Manager

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Muthusamy V. Swami and Lixing Gu
          !       DATE WRITTEN   August 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the main driver for initializations within the
          ! heat balance using the EMPD model.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound,GetObjectItem,FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: IOStat           ! IO Status when calling get input subroutine
  CHARACTER(len=MaxNameLength),DIMENSION(3) &
          :: MaterialNames ! Number of Material Alpha names defined
  INTEGER :: MaterNum         ! Counter to keep track of the material number
  INTEGER :: MaterialNumAlpha ! Number of material alpha names being passed
  INTEGER :: MaterialNumProp  ! Number of material properties being passed
  REAL, DIMENSION(5) :: MaterialProps !Temporary array to transfer material properties
  LOGICAL :: ErrorsFound = .false. ! If errors detected in input

  INTEGER :: EMPDMat                ! EMPD Moisture Material additional properties for each base material
  Integer :: Loop
  INTEGER :: Layer
  INTEGER :: SurfNum   ! Surface number
  INTEGER :: MatNum    ! Material number at interior layer
  INTEGER :: ConstrNum ! Construction number
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: EMPDzone   ! EMPD property check for each zone
  INTEGER, SAVE :: ErrCount=0

  ! Load the additional EMPD Material properties
  cCurrentModuleObject='MaterialProperty:MoisturePenetrationDepth:Settings'
  EMPDMat=GetNumObjectsFound(TRIM(cCurrentModuleObject))

  IF (EMPDMat == 0) THEN
    CALL ShowSevereError('EMPD Solution requested, but no "'//TRIM(cCurrentModuleObject)//'" objects were found.')
    ErrorsFound=.true.
  ENDIF

  DO Loop=1,EMPDMat

    !Call Input Get routine to retrieve material data
    CALL GetObjectItem(TRIM(cCurrentModuleObject),Loop,MaterialNames,MaterialNumAlpha, &
                       MaterialProps,MaterialNumProp,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)


    !Load the material derived type from the input data.
    MaterNum = FindItemInList(MaterialNames(1),Material%Name,TotMaterials)
    IF (MaterNum == 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(1))//  &
         ' entered='//TRIM(MaterialNames(1))//', must match to a valid Material name.')
      ErrorsFound=.true.
      Cycle
    ENDIF

    ! See if Material was defined with R only.  (No density is defined then and not applicable for EMPD).
    !  What about materials other than "regular materials" (e.g. Glass, Air, etc)
    IF (Material(MaterNum)%Group == RegularMaterial .and. MaterialProps(1) > 0.0) THEN
      IF (Material(MaterNum)%ROnly) THEN
!        CALL ShowSevereError('EMPD base material = "'//TRIM(Material(MaterNum)%Name)//  &
!                             '" was Material:NoMass. It cannot be used for EMPD calculations.')
        CALL ShowContinueError('..Only Material base materials are allowed to have EMPD properties.')
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//  &
           ': Reference Material is not appropriate type for EMPD properties, material='//  &
           TRIM(Material(MaterNum)%Name)//', must have regular properties (L,Cp,K,D)')
        ErrorsFound=.true.
      ENDIF
    ENDIF
    IF (Material(MaterNum)%Group /= RegularMaterial) THEN
!      CALL ShowSevereError('GetMoistureBalanceEMPDInput: Only Material:Regular base materials are allowed '// &
!                           'to have EMPD properties, material = '// TRIM(Material(MaterNum)%Name))
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//  &
         ': Reference Material is not appropriate type for EMPD properties, material='//  &
         TRIM(Material(MaterNum)%Name)//', must have regular properties (L,Cp,K,D)')
      ErrorsFound=.true.
    ENDIF

    ! Once the material derived type number is found then load the additional moisture material properties
    Material(MaterNum)%EMPDMaterialProps = .true.
    Material(MaterNum)%EMPDValue  = MaterialProps(1)
    Material(MaterNum)%MoistACoeff = MaterialProps(2)
    Material(MaterNum)%MoistBCoeff = MaterialProps(3)
    Material(MaterNum)%MoistCCoeff = MaterialProps(4)
    Material(MaterNum)%MoistDCoeff = MaterialProps(5)

  ENDDO

! Ensure at least one interior EMPD surface for each zone
  ALLOCATE(EMPDzone(NumOfZones))
  EMPDzone = .False.
  Do SurfNum=1,TotSurfaces
    if (.NOT. Surface(SurfNum)%HeatTransSurf .OR. &
        Surface(SurfNum)%Class .EQ. SurfaceClass_Window) Cycle ! Heat transfer surface only and not a window
    ConstrNum = Surface(SurfNum)%Construction
    MatNum  = Construct(ConstrNum)%LayerPoint(Construct(ConstrNum)%TotLayers)
    If (Material(MatNum)%EMPDValue .GT. 0.0 .AND. Surface(SurfNum)%Zone .gt. 0) then
       EMPDzone(Surface(SurfNum)%Zone) = .True.
    else
      ErrCount=ErrCount+1
      IF (ErrCount == 1 .and. .not. DisplayExtraWarnings) THEN
        Call ShowMessage('GetMoistureBalanceEMPDInput: EMPD properties are not assigned to the inside layer of Surfaces')
        CALL ShowContinueError('...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual surfaces.')
      ENDIF
      IF (DisplayExtraWarnings) THEN
        Call ShowMessage('GetMoistureBalanceEMPDInput: EMPD properties are not assigned to the '// &
                         'inside layer in Surface='// TRIM(Surface(SurfNum)%Name))
        CALL ShowContinueError('with Construction='//TRIM(Construct(ConstrNum)%Name))
      End if
    end if
    IF (Construct(ConstrNum)%TotLayers .eq. 1) then ! One layer construction
      Cycle
    else ! Multiple layer construction
      IF (Material(Construct(ConstrNum)%LayerPoint(1))%EMPDMaterialProps .AND. & ! The external layer is not exposed to zone
        Surface(SurfNum)%ExtBoundCond <= 0) THEN
        CALL ShowSevereError('GetMoistureBalanceEMPDInput: EMPD properties are assigned to the '// &
                             'outside layer in Construction='// &
                            TRIM(Construct(ConstrNum)%Name))
        CALL ShowContinueError('..Outside layer material with EMPD properties = '//  &
                            TRIM(Material(Construct(ConstrNum)%LayerPoint(1))%Name))
        CALL ShowContinueError('..A material with EMPD properties must be assigned to the inside layer of a construction.')
        ErrorsFound=.true.
      ENDIF
      DO Layer=2,Construct(ConstrNum)%TotLayers-1
        IF (Material(Construct(ConstrNum)%LayerPoint(Layer))%EMPDMaterialProps) THEN
          CALL ShowSevereError('GetMoistureBalanceEMPDInput: EMPD properties are assigned to a '// &
                               'middle layer in Construction='// &
                              TRIM(Construct(ConstrNum)%Name))
          CALL ShowContinueError('..Middle layer material with EMPD properties = '//  &
                              TRIM(Material(Construct(ConstrNum)%LayerPoint(Layer))%Name))
          CALL ShowContinueError('..A material with EMPD properties must be assigned to the inside layer of a construction.')
          ErrorsFound=.true.
        ENDIF
      ENDDO
    end if
  End Do

  Do Loop=1, NumOfZones
     If (.NOT. EMPDZone(Loop)) then
        CALL ShowSevereError('GetMoistureBalanceEMPDInput: None of the constructions for zone = '// &
                           TRIM(Zone(Loop)%Name)//' has an inside layer with EMPD properties')
        CALL ShowContinueError('..For each zone, the inside layer of at least one construction must have EMPD properties')
        ErrorsFound=.true.
     End if
  End Do

  DEALLOCATE(EMPDzone)

  Call ReportMoistureBalanceEMPD

  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetMoistureBalanceEMPDInput: Errors found getting EMPD material properties, program terminated.')
  ENDIF


  RETURN

END SUBROUTINE GetMoistureBalanceEMPDInput

SUBROUTINE InitMoistureBalanceEMPD

    ! SUBROUTINE INFORMATION:
    !   Authors:        Muthusamy Swami and Lixing Gu
    !   Date written:   August, 1999
    !   Modified:       na
    !   Re-engineered:  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! Create dynamic array for surface moisture calculation

    ! METHODOLOGY EMPLOYED:
    !

    ! USE STATEMENTS:

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    Integer :: ZoneNum
    Integer :: Loop
    Integer :: SurfNum
    LOGICAL,SAVE :: InitEnvrnFlag = .True.

  if (InitEnvrnFlag) then
    ALLOCATE(MoistEMPDOld(TotSurfaces))
    ALLOCATE(MoistEMPDInt(TotSurfaces))
    ALLOCATE(MoistEMPDNew(TotSurfaces))
    ALLOCATE(MoistEMPDFlux(TotSurfaces))
    Allocate(RhoVapEMPD(TotSurfaces))
    Allocate(WSurfEMPD(TotSurfaces))
    Allocate(RHEMPD(TotSurfaces))
  end if

    do SurfNum = 1, TotSurfaces
       ZoneNum               = Surface(SurfNum)%Zone
       IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
       IF(ZoneAirHumRat(ZoneNum) == 0.0)Then
          MoistEMPDOld(SurfNum) = 0.0001
          MoistEMPDInt(SurfNum) = 0.0001
          MoistEMPDNew(SurfNum) = 0.0001
       Else
          MoistEMPDOld(SurfNum) = ZoneAirHumRat(ZoneNum) ! Surface moisture level initialization
          MoistEMPDInt(SurfNum) = ZoneAirHumRat(ZoneNum) ! by assuming initial values be equal to ZoneAirHumRat
          MoistEMPDNew(SurfNum) = ZoneAirHumRat(ZoneNum)
       End IF
    end do
    if (.Not. InitEnvrnFlag) Return
    !Initialize the report variable
    RhoVapEMPD = 0.015
    WSurfEMPD  = 0.015
    RHEMPD = 0.0
    MoistEMPDFlux = 0.0

    Call GetMoistureBalanceEMPDInput

    DO Loop=1,TotSurfaces
       IF (.not. Surface(Loop)%HeatTransSurf) CYCLE
       IF (Surface(Loop)%Class == SurfaceClass_Window) CYCLE
       CALL SetupOutputVariable('Inside Surface Vapor Density [kg/m3]',RhoVapEMPD(Loop),'Zone','State',Trim(Surface(Loop)%Name))
       CALL SetupOutputVariable('Inside Surface Humidity Ratio [kgWater/kgDryAir]',WSurfEMPD(Loop),'Zone','State',  &
          Trim(Surface(Loop)%Name))
       CALL SetupOutputVariable('Inside Surface Relative Humidity [%]',RHEMPD(Loop),'Zone','State',Trim(Surface(Loop)%Name))
    ENDDO

    if (InitEnvrnFlag) InitEnvrnFlag = .False.

RETURN
END SUBROUTINE InitMoistureBalanceEMPD

SUBROUTINE CalcMoistureBalanceEMPD(SurfNum, TempSurfIn, TempSurfInOld, TempZone, TempSat)

    ! SUBROUTINE INFORMATION:
    !   Authors:        Muthusamy Swami and Lixing Gu
    !   Date written:   August, 1999
    !   Modified:       na
    !   Re-engineered:  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! Calculate surface moisture level using EMPD model

    ! METHODOLOGY EMPLOYED:
    ! na

    ! USE STATEMENTS:
USE Psychrometrics, ONLY:PsyRhFnTdbWPb,PsyRhFnTdbRhovLBnC,PsyWFnTdbRhPb,PsyRhoAirFnPbTdbW,PsyCpAirFnWTdb,PsyRhovFnTdbWPb

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(In) :: SurfNum
  REAL, Intent(In)    :: TempSurfIn       !INSIDE SURFACE TEMPERATURE at current time step
  REAL, Intent(In)    :: TempSurfInOld    !INSIDE SURFACE TEMPERATURE at previous time step.
  REAL, Intent(In)    :: TempZone         !Zone temperature at current time step.
  REAL, Intent(OUT)   :: TempSat         ! Satutare surface temperature.

          ! SUBROUTINE PARAMETER DEFINITIONS:
    REAL, PARAMETER :: Error = 0.01    ! Totlarence (%)
    REAL, PARAMETER :: RLXM  = 0.3     ! Relaxation factor (0-1)
    REAL, PARAMETER :: Lam  = 2.5d6    ! Heat of vaporization (J/kg)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: NOFITR    ! Number of iterations
    INTEGER :: ZoneNum   ! Surface number
    INTEGER :: MatNum    ! Material number at interior layer
    INTEGER :: ConstrNum ! Construction number
    REAL    :: RHOBULK   ! Material bulk density
    REAL    :: HM        ! Moisture transfer coefficient
    REAL    :: Taver     ! Average zone temperature between current time and previous time
!    REAL    :: Waver     ! Average zone humidity ratio between current time and previous time
    REAL    :: RHaver    ! Average zone relative humidity {0-1} between current time and previous time
    REAL    :: RVaver    ! Average zone vapor density
    REAL    :: AT
    REAL    :: BR
    REAL    :: RALPHA    ! Zone vapor density
    REAL    :: BB        ! Coefficient for ODE
    REAL    :: CC        ! Coefficient for ODE
    REAL    :: ErrorM    ! Percent error
    INTEGER :: Flag      ! Convergence flag (0 - converged)
    LOGICAL,SAVE :: OneTimeFlag = .True.
    REAL    :: Wsurf ! Surface moisture flux
    REAL    :: PVsurf ! Surface vapor pressure

!    if (OneTimeFlag) then
!       Call InitMoistureBalanceEMPD
!       OneTimeFlag = .False.
!    end if

    if (BeginEnvrnFlag .and. OneTimeFlag) then
       Call InitMoistureBalanceEMPD
       OneTimeFlag = .False.
    end if

    if (.not. BeginEnvrnFlag) then
       OneTimeFlag = .True.
    end if

    MoistEMPDFlux(SurfNum) = 0.0
    Flag = 1
    NOFITR = 0
    If ( .NOT. Surface(SurfNum)%HeatTransSurf ) Then
       RETURN
    End If
    ConstrNum = Surface(SurfNum)%Construction
    MatNum  = Construct(ConstrNum)%LayerPoint(Construct(ConstrNum)%TotLayers)   ! Then find the material pointer

    ZoneNum   = Surface(SurfNum)%Zone
    If (Material(MatNum)%EMPDValue .LE. 0.0) Then
       MoistEMPDNew(SurfNum)= PsyRhovFnTdbWPb(TempZone,ZoneAirHumRat(ZoneNum),OutBaroPress,'CalcMoistureEMPD')
       RETURN
    End If

    Taver = (TempSurfIn+TempSurfInOld)/2.0


    DO WHILE (Flag > 0 )
       RVaver = (MoistEMPDNew(SurfNum)+MoistEMPDOld(SurfNum))/2.0
       RHaver = RVaver*461.52*(Taver+KelvinConv)*exp(-23.7093+4111.0/(Taver+237.7))
       if (RHaver .GT. 1.0) RHaver = 1.0
       if (RHaver .LT. 0.0) RHaver = 0.00001

       AT = (Material(MatNum)%MoistACoeff*Material(MatNum)%MoistBCoeff*RHaver**Material(MatNum)%MoistBCoeff + &
           Material(MatNum)%MoistCCoeff*Material(MatNum)%MoistDCoeff*RHaver**Material(MatNum)%MoistDCoeff)/RVaver
       BR = (4111.0/(Taver+237.7)**2-(1.0/(Taver+KelvinConv)))*AT*RVaver
       RHOBULK = Material(MatNum)%density
       HM = HConvIn(SurfNum)/(PsyRhoAirFnPbTdbW(outbaropress,TempZone,ZoneAirHumRat(ZoneNum),'CalcMoistureEMPD') &
                   *PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum),TempZone,'CalcMoistureEMPD'))
       ZoneNum = Surface(SurfNum)%Zone
       RALPHA = ZoneAirHumRat(ZoneNum)*OutBaroPress/(461.52*(TempZone+KelvinConv) &
               *(ZoneAirHumRat(ZoneNum)+0.62198))
       BB = HM/(RHOBULK*Material(MatNum)%EMPDValue*AT)
       CC = BB*RALPHA+BR/AT*(TempSurfIn-TempSurfInOld)/(TimeStepZone*SecInHour)
       CALL SolverMoistureBalanceEMPD (MoistEMPDNew(SurfNum),MoistEMPDOld(SurfNum), &
                                         1.0,BB,CC)

       Flag = 0
       ERRORM = ABS((MoistEMPDNew(SurfNum)-MoistEMPDInt(SurfNum))/MoistEMPDInt(SurfNum))*100.0
       IF (ERRORM .GT. ERROR) Flag = Flag+1


       NOFITR = NOFITR+1
       IF (NOFITR .GT. 500) THEN
         CALL ShowFatalError('Iteration limit exceeded in EMPD model, program terminated.')
       ENDIF

       if (Flag > 0) then
          MoistEMPDNew(SurfNum) = MoistEMPDNew(SurfNum)*RLXM + &
                                  MoistEMPDInt(SurfNum)*(1.0-RLXM)
       End If
       MoistEMPDInt(SurfNum) = MoistEMPDNew(SurfNum)
    END DO

    ! Calculate latent load
    PVsurf = RHaver*exp(23.7093-4111.0/(Taver+237.7))
    Wsurf = 0.62198*RHaver/(exp(-23.7093+4111.0/(Taver+237.7))*OutBaroPress-RHaver)
    MoistEMPDFlux(SurfNum) = HM*(MoistEMPDNew(SurfNum)-  &
           PsyRhoAirFnPbTdbW(OutBaroPress, TempZone, ZoneAirHumRat(ZoneNum),'CalcMoistureEMPD')* &
           ZoneAirHumRat(ZoneNum))*Lam
    ! Calculate surface dew point temperature based on surface vapor density
    TempSat = 4111.0/(23.7093-LOG(PVsurf))+35.45-KelvinConv

    ! Put results in the single precision reporting variable
    RhoVapEMPD(SurfNum) = MoistEMPDNew(SurfNum)
    RHEMPD(SurfNum) = PsyRhFnTdbRhovLBnC(TempSurfIn,RhoVapEMPD(SurfNum),'CalcMoistureEMPD')*100.0
    WSurfEMPD(SurfNum) = PsyWFnTdbRhPb(TempSurfIn,RHEMPD(SurfNum)/100.0,OutBaroPress,'CalcMoistureEMPD')


RETURN
END SUBROUTINE CalcMoistureBalanceEMPD

SUBROUTINE SolverMoistureBalanceEMPD (VARNEW,VAROLD,A,B,C)

    ! SUBROUTINE INFORMATION:
    !   Authors:        Muthusamy Swami and Lixing Gu
    !   Date writtenn:  August, 1999
    !   Modified:       na
    !   Re-engineered:  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! Solve a first order ordinary differential equation, A dV/dt + B V = C

    ! METHODOLOGY EMPLOYED:
    ! Finite difference method

    ! USE STATEMENTS:
    USE DataGlobals, ONLY: TimeStepZone

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
REAL, INTENT(OUT) :: VARNEW ! Value at current time step
REAL, INTENT(IN)  :: VAROLD ! Value at previous time step
REAL, INTENT(IN)  :: A      ! Coefficient of time derivative in AdV/dt+BV=C
REAL, INTENT(IN)  :: B      ! Coefficienct of variable
REAL, INTENT(IN)  :: C      ! Constant

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

   VARNEW = (VAROLD+TimeStepZone*SecInHour*C/A)/(1.+TimeStepZone*SecInHour*B/A)

RETURN

END SUBROUTINE SolverMoistureBalanceEMPD

SUBROUTINE CloseMoistureBalanceEMPD

    ! SUBROUTINE INFORMATION:
    !   Authors:        Muthusamy Swami and Lixing Gu
    !   Date writtenn:  August, 1999
    !   Modified:       na
    !   Re-engineered:  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! Deallocate dynamic arrays for surface moisture calculation

    ! METHODOLOGY EMPLOYED:
    !

    ! USE STATEMENTS:

    IMPLICIT NONE

    DEALLOCATE(MoistEMPDOld)
    DEALLOCATE(MoistEMPDInt)
    DEALLOCATE(MoistEMPDNew)
    DEALLOCATE(MoistEMPDFlux)

RETURN

END SUBROUTINE CloseMoistureBalanceEMPD

SUBROUTINE UpdateMoistureBalanceEMPD(SurfNum)

    ! SUBROUTINE INFORMATION:
    !   Authors:        Muthusamy Swami and Lixing Gu
    !   Date writtenn:  August, 1999
    !   Modified:       na
    !   Re-engineered:  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! Update inside surface vapor density
    ! METHODOLOGY EMPLOYED:
    !

    ! USE STATEMENTS:

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    Integer, INTENT(IN)  :: SurfNum ! Surface number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

    MoistEMPDOld(SurfNum) = MoistEMPDNew(SurfNum)


RETURN

END SUBROUTINE UpdateMoistureBalanceEMPD

SUBROUTINE ReportMoistureBalanceEMPD

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   August 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine gives a detailed report to the user about
          ! EMPD Properties of each construction.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! The subroutine of ReportCTFs written by Linda Lawrie was used to develop this routine.

          ! USE STATEMENTS:
  USE General, ONLY: ScanForReports

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: DoReport

  INTEGER ConstrNum,MatNum

  CALL ScanForReports('Constructions',DoReport,'Constructions')

  IF (.NOT. DoReport) return
!
!   Write Descriptions
    Write(OutputFileInits,'(A)') '! <EMPD Properties>, Construction Name, Inside Layer Material Name, ' &
                                // 'Penetration Depth {m}, a, b, c, d'

    DO ConstrNum=1,TotConstructs
      IF (Construct(ConstrNum)%TypeIsWindow) CYCLE
      MatNum  = Construct(ConstrNum)%LayerPoint(Construct(ConstrNum)%TotLayers)
      If (Material(MatNum)%EMPDMaterialProps) then
      Write(OutputFileInits,700) TRIM(Construct(ConstrNum)%Name),Trim(Material(MatNum)%Name), &
                                 Material(MatNum)%EMPDValue,Material(MatNum)%MoistACoeff, &
                                 Material(MatNum)%MoistBCoeff,Material(MatNum)%MoistCCoeff,Material(MatNum)%MoistDCoeff
      end if
    End Do
!
 700  FORMAT(' EMPD Properties, ',A,', ',A,', ',4(F8.4,', '),F8.4)

END SUBROUTINE ReportMoistureBalanceEMPD

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

END MODULE MoistureBalanceEMPDManager
