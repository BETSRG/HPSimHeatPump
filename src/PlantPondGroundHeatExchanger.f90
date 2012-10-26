MODULE PondGroundHeatExchanger


  ! Module containing the routines dealing with pond ground heat exchangers

  ! MODULE INFORMATION:
  !       AUTHOR         Simon Rees
  !       DATE WRITTEN   September 2002
  !       MODIFIED       Brent Griffith Sept 2010, plant upgrades
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! This model represents a shallow pond with submerged hydronic tubes through
  ! which the heat transfer fluid is circulated. The model represents a 'shallow'
  ! pond in that no attempt is made to model any stratification effects that may
  ! be present in deeper ponds. This type of heat rejector is intended to be
  ! connected in a condenser loop, with or without other forms of heat rejector.
  ! The pond model is a 'lumped parameter' model where the pond is represented
  ! by a single node with thermal mass. The pond surface temperature is the same
  ! as the temperature at this node, i.e. the surface temperature is the same as
  ! the bulk temperature. A first order differential equation is solved in the
  ! model to calculated the pond temperature at each time step. This type of heat
  ! rejector is modelled as several circuits connected in parallel.

  ! METHODOLOGY EMPLOYED:
  ! A heat balance is calculated at a single node that represents the pond.
  ! heat transfer takes palce by surface convection, long-wave radiation to the
  ! sky, absoption of solar energy, ground heat transfer and heat exchange with
  ! the fluid. A heat exchanger analogy is used to calculate the heat transfer
  ! between the heat transfer fluid and the pond. The differential equation
  ! defined by the heat balance is solved using a fourth order Runge-Kutta
  ! numerical integration method.
  !

  ! REFERENCES:
  ! Chiasson, A. Advances in Modeling of Ground-Source Heat Pump Systems.
  !   M.S. Thesis, Oklahoma State University, December 1999.
  ! Chiasson, A.D., J.D. Spitler, S.J. Rees, M.D. Smith.  2000.  A Model For
  !   Simulating The Performance Of A Shallow Pond As A Supplemental Heat Rejecter
  !   With Closed-Loop Ground-Source Heat Pump Systems.
  !   ASHRAE Transactions.  106(2):107-121.

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataInterfaces
USE DataGlobals,       ONLY : MaxNameLength, KelvinConv
USE General, ONLY: TrimSigDigits
USE DataPlant,       ONLY: PlantLoop
  ! Use statements for access to subroutines in other modules


IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS
  REAL, PARAMETER :: SmallNum      = 1.0d-30          ! Very small number to avoid div0 errors
  REAL, PARAMETER :: StefBoltzmann = 5.6697d-08       ! Stefan-Boltzmann constant
!  REAL, PARAMETER :: KelvinConv    = KelvinConv           ! Conversion from Celsius to Kelvin

  ! DERIVED TYPE DEFINITIONS
TYPE PondGroundHeatExchangerData
  ! Input data
  CHARACTER(len=MaxNameLength) :: Name                =' ' ! name of pond GHE
  CHARACTER(len=MaxNameLength) :: InletNode           =' ' ! pond inlet fluid node
  CHARACTER(len=MaxNameLength) :: OutletNode          =' ' ! pond outlet fluid node
  REAL                    :: DesignMassFlowRate  =0. ! design flow rate of circulating fluid
  REAL                    :: DesignCapacity      =0. ! design cooling capacity of pond at 
  REAL                    :: Depth               =0.0 ! depth of pond
  REAL                    :: Area                =0.0 ! area of pond
  REAL                    :: TubeInDiameter      =0.0 ! hydronic tube inside diameter
  REAL                    :: TubeOutDiameter     =0.0 ! hydronic tube outside diameter
  REAL                    :: TubeConductivity    =0.0 ! hydronic tube thermal conductivity
  REAL                    :: GrndConductivity    =0.0 ! ground thermal conductivity
  REAL                    :: CircuitLength       =0.0 ! length of each circuit
  REAL                    :: BulkTemperature     =0.0 ! current pond bulk temperature
  REAL                    :: PastBulkTemperature =0.0 ! past pond bulk temperature
  INTEGER                      :: NumCircuits         =0 ! number of circuits in total
  INTEGER                      :: InletNodeNum        =0 ! inlet node number
  INTEGER                      :: OutletNodeNum       =0 ! oulet node number
  INTEGER                      :: FrozenErrIndex      =0 ! for recurring warnings
  INTEGER                      :: ConsecutiveFrozen   =0 ! count of time steps consecutive frozen
  !loop topology variables
  INTEGER                      :: LoopNum        =0
  INTEGER                      :: LoopSideNum    =0
  INTEGER                      :: BranchNum      =0
  INTEGER                      :: CompNum        =0
END TYPE PondGroundHeatExchangerData


TYPE PondGroundHeatExchangerReport
  ! Report data
  REAL                    :: InletTemp           ! fluid inlet temperature
  REAL                    :: OutletTemp          ! fluid outlet temperature
  REAL                    :: MassFlowRate        ! fluid mass flow rate
  REAL                    :: PondTemp            ! pond bulk temperature
  REAL                    :: HeatTransferRate    ! total fluid heat transfer rate, Watts
  REAL                    :: Energy              ! cumulative energy, Joules
END TYPE PondGroundHeatExchangerReport

TYPE(PondGroundHeatExchangerData),   DIMENSION(:), ALLOCATABLE :: PondGHE
TYPE(PondGroundHeatExchangerReport), DIMENSION(:), ALLOCATABLE :: PondGHEReport


  ! MODULE VARIABLE DECLARATIONS:
  ! utility variables initialized once
INTEGER :: NumOfPondGHEs                 =0 ! Number of pond ground heat exchangers
  ! Utility variables - initialized for each instance of a pond
REAL    :: InletTemp                =0.0  ! water inlet temperature
REAL    :: OutletTemp               =0.0  ! water outlet temperature
REAL    :: FlowRate                 =0.0  ! water mass flow rate
REAL    :: HeatTransRate            =0.0  ! total heat transfer rate, Watts
REAL    :: PondTemp                 =0.0  ! pond temperature
REAL    :: PastPondTemp             =0.0  ! past pond temperature
REAL    :: PondArea                 =0.0  ! pond surface area
REAL    :: PondDepth                =0.0  ! pond depth
REAL    :: TubeInDiameter           =0.0  ! hydronic tube inside diameter
REAL    :: TubeOutDiameter          =0.0  ! hydronic tube outside diameter
REAL    :: TubeConductivity         =0.0  ! hydronic tube thermal conductivity
REAL    :: GrndConductivity         =0.0  ! ground thermal conductivity
REAL    :: Concentration            =0.0  ! fluid/glycol concentration 0.0-1.0 proportion.
REAL    :: CircLength               =0.0  ! length of each circuit
INTEGER :: NumCircuits                   =0 ! number of circuits in total
INTEGER :: InletNodeNum                  =0  ! inlet node number
INTEGER :: OutletNodeNum                 =0  ! oulet node number
INTEGER :: WaterIndex                    =0  ! Fluid index for pond water
LOGICAL :: NoDeepGroundTempObjWarning=.true.  ! This will cause a warning to be issued if no "deep" ground
                                              ! temperature object was input.
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName


  ! SUBROUTINE SPECIFICATIONS FOR MODULE PlantPondGroundHeatExchangers
PUBLIC  SimPondGroundHeatExchanger
PRIVATE GetPondGroundHeatExchanger
PRIVATE InitPondGroundHeatExchanger
PRIVATE CalcPondGroundHeatExchanger
PRIVATE CalcTotalFLux
PRIVATE CalcSolarFlux
PRIVATE CalcEffectiveness
PRIVATE UpdatePondGroundHeatExchanger
PRIVATE ReportPondGroundHeatExchanger

CONTAINS

!==============================================================================

SUBROUTINE SimPondGroundHeatExchanger(CompName,CompIndex,FirstHVACIteration,RunFlag,InitLoopEquip,Maxload,MinLoad,OptLoad)    !DSU

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the public routine that is used to simulate
          ! the operation of pond ground heat exchangers at each system
          ! time step.

          ! METHODOLOGY EMPLOYED:
          ! Several private routines are called to get data, make the calculations
          ! and update stuff. This is called for each instance of pond components.

          ! REFERENCES:
          ! Chiasson, A. Advances in Modeling of Ground-Source Heat Pump Systems.
          !   M.S. Thesis, Oklahoma State University, December 1999.
          ! Chiasson, A.D., J.D. Spitler, S.J. Rees, M.D. Smith.  2000.  A Model For
          !   Simulating The Performance Of A Shallow Pond As A Supplemental Heat
          !   Rejecter With Closed-Loop Ground-Source Heat Pump Systems.
          !   ASHRAE Transactions.  106(2):107-121.

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: CompName            ! name of the pond GHE
  INTEGER, INTENT(INOUT)        :: CompIndex           ! index in local derived types
  LOGICAL,          INTENT(IN)  :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep
  LOGICAL,          INTENT(IN)  :: Runflag             ! TRUE if equipment turned on by loop operation scheme
  LOGICAL                       :: InitLoopEquip
  REAL,INTENT(INOUT)     :: Maxload
  REAL,INTENT(INOUT)     :: MinLoad
  REAL,INTENT(INOUT)     :: OptLoad

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE :: GetInputFlag = .TRUE.  ! Flag first time, input is fetched
  INTEGER                       :: PondGHENum          ! index in local derived types

          ! check for input
  IF (GetInputFlag) THEN
    CALL GetPondGroundHeatExchanger
    GetInputFlag=.FALSE.
  ENDIF

  IF (CompIndex == 0) THEN
    PondGHENum = FindItemInList(CompName,PondGHE%Name,NumOfPondGHEs)
    IF (PondGHENum == 0) THEN
      CALL ShowFatalError('SimPondGroundHeatExchanger: Unit not found='//TRIM(CompName))
    ENDIF
    CompIndex=PondGHENum
  ELSE
    PondGHENum=CompIndex
    IF (PondGHENum > NumOfPondGHEs .or. PondGHENum < 1) THEN
      CALL ShowFatalError('SimPondGroundHeatExchanger:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(PondGHENum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumOfPondGHEs))//  &
                          ', Entered Unit name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(PondGHENum)) THEN
      IF (CompName /= PondGHE(PondGHENum)%Name) THEN
        CALL ShowFatalError('SimPondGroundHeatExchanger: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(PondGHENum))// &
                            ', Unit name='//TRIM(CompName)//', stored Unit Name for that index='//  &
                            TRIM(PondGHE(PondGHENum)%Name))
      ENDIF
      CheckEquipName(PondGHENum)=.false.
    ENDIF
  ENDIF

  IF (InitLoopEquip) THEN
    CALL InitPondGroundHeatExchanger(PondGHENum,FirstHVACIteration,RunFlag)
    Maxload = PondGHE(PondGHENum)%DesignCapacity
    MinLoad = 0.
    OptLoad = PondGHE(PondGHENum)%DesignCapacity
    RETURN
  ENDIF

  ! initialize
  CALL InitPondGroundHeatExchanger(PondGHENum,FirstHVACIteration,RunFlag)
  ! make the calculations
  CALL CalcPondGroundHeatExchanger(PondGHENum)
  ! update vaiables
  CALL UpdatePondGroundHeatExchanger(PondGHENum)
  ! update report variables
  CALL ReportPondGroundHeatExchanger(PondGHENum)

  RETURN

END SUBROUTINE SimPondGroundHeatExchanger

!==============================================================================

SUBROUTINE GetPondGroundHeatExchanger

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reads the input for hydronic Pond Ground Heat Exchangers
          ! from the user input file.  This will contain all of the information
          ! needed to define and simulate the pond.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  USE DataGlobals,        ONLY : MaxNameLength
  USE InputProcessor,     ONLY : GetNumObjectsFound, GetObjectItem, FindItemInList
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE NodeInputManager,   ONLY : GetOnlySingleNode
  USE BranchNodeConnections, ONLY : TestCompSet
  USE FluidProperties,    ONLY : CheckFluidPropertyName, FindGlycol
  USE DataEnvironment,    ONLY : GroundTemp_Deep,GroundTemp_DeepObjInput
  USE General,            ONLY : RoundSigDigits
  USE DataLoopNode

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank = ' '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL                        :: ErrorsFound=.false.  ! Set to true if errors in input,
                                                         ! fatal at end of routine
  INTEGER                        :: IOStatus             ! Used in GetObjectItem
  INTEGER                        :: Item                 ! Item to be "gotten"
  INTEGER                        :: NumAlphas            ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers           ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: NumFluids            ! number of fluids in sim.

          ! Initializations and allocations
  cCurrentModuleObject = 'GroundHeatExchanger:Pond'
  NumOfPondGHEs = GetNumObjectsFound(TRIM(cCurrentModuleObject))
  ! allocate data structures
  IF(ALLOCATED(PondGHE)) DEALLOCATE(PondGHE)
  IF(ALLOCATED(PondGHEReport)) DEALLOCATE(PondGHEReport)

  ALLOCATE(PondGHE(NumOfPondGHEs))
  ALLOCATE(PondGHEReport(NumOfPondGHEs))
  ALLOCATE(CheckEquipName(NumOfPondGHEs))
  CheckEquipName=.true.

  ! record fluid prop index for water
  WaterIndex=FindGlycol('WATER')


  ! Obtain all of the user data related to the ponds...
  DO Item = 1, NumOfPondGHEs

    ! get the input data
    CALL GetObjectItem(TRIM(cCurrentModuleObject),Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus, &
                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    ! General user input data
    PondGHE(Item)%Name = cAlphaArgs(1)

    !get inlet node data
    PondGHE(Item)%InletNode = cAlphaArgs(2)
    PondGHE(Item)%InletNodeNum  = GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    IF (PondGHE(Item)%InletNodeNum == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    END IF

    ! get outlet node data
    PondGHE(Item)%OutletNode = cAlphaArgs(3)
    PondGHE(Item)%OutletNodeNum  = GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    IF (PondGHE(Item)%OutletNodeNum == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(3))//'='//TRIM(cAlphaArgs(3)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    END IF

    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(2),cAlphaArgs(3),'Condenser Water Nodes')

    ! pond geometry data
    PondGHE(Item)%Depth = rNumericArgs(1)
    PondGHE(Item)%Area  = rNumericArgs(2)
    IF (rNumericArgs(1) <= 0.0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(1))//'='//TRIM(RoundSigDigits(rNumericArgs(1),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Value must be greater than 0.0')
      ErrorsFound=.true.
    END IF
    IF (rNumericArgs(2) <= 0.0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(2))//'='//TRIM(RoundSigDigits(rNumericArgs(2),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Value must be greater than 0.0')
      ErrorsFound=.true.
    END IF

    ! tube data
    PondGHE(Item)%TubeInDiameter  = rNumericArgs(3)
    PondGHE(Item)%TubeOutDiameter = rNumericArgs(4)

    IF (rNumericArgs(3) <= 0.0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(3))//'='//TRIM(RoundSigDigits(rNumericArgs(3),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Value must be greater than 0.0')
      ErrorsFound=.true.
    END IF
    IF (rNumericArgs(4) <= 0.0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(4))//'='//TRIM(RoundSigDigits(rNumericArgs(4),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Value must be greater than 0.0')
      ErrorsFound=.true.
    END IF
    IF (rNumericArgs(3) > rNumericArgs(4)) THEN  ! error
      CALL ShowSevereError('For '//TRIM(cCurrentModuleObject)//': '//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError(TRIM(cNumericFieldNames(3))//' ['//TRIM(RoundSigDigits(rNumericArgs(3),2))//'] > '//  &
       TRIM(cNumericFieldNames(4))//' ['//TRIM(RoundSigDigits(rNumericArgs(4),2))//']')
      ErrorsFound=.true.
    ENDIF

    ! thermal conductivity data
    PondGHE(Item)%TubeConductivity  = rNumericArgs(5)
    PondGHE(Item)%GrndConductivity  = rNumericArgs(6)

    IF (rNumericArgs(5) <= 0.0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(5))//'='//TRIM(RoundSigDigits(rNumericArgs(5),4)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Value must be greater than 0.0')
      ErrorsFound=.true.
    END IF
    IF (rNumericArgs(6) <= 0.0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(6))//'='//TRIM(RoundSigDigits(rNumericArgs(6),4)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Value must be greater than 0.0')
      ErrorsFound=.true.
    END IF

    ! circuits
    PondGHE(Item)%NumCircuits  = rNumericArgs(7)

    IF (rNumericArgs(7) <= 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(7))//'='//TRIM(RoundSigDigits(rNumericArgs(7),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Value must be greater than 0.0')
      ErrorsFound=.true.
    END IF
    PondGHE(Item)%CircuitLength  = rNumericArgs(8)
    IF (rNumericArgs(8) <= 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(8))//'='//TRIM(RoundSigDigits(rNumericArgs(8),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Value must be greater than 0.0')
      ErrorsFound=.true.
    END IF

  END DO  ! end of input loop

  ! final error check
  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '//TRIM(cCurrentModuleObject) )
  END IF


  ! Set up the output variables
  DO Item = 1, NumOfPondGHEs
    CALL SetupOutputVariable('Pond ground heat exchanger heat transfer rate[W]',    &
                              PondGHEReport(Item)%HeatTransferRate,'Plant','Average', &
                              PondGHE(Item)%Name)
    CALL SetupOutputVariable('Pond ground heat exchanger energy[J]', &
                              PondGHEReport(Item)%Energy,'Plant','Sum',PondGHE(Item)%Name)
    CALL SetupOutputVariable('Pond ground heat exchanger mass flow rate[kg/s]',      &
                              PondGHEReport(Item)%MassFlowRate,'Plant','Average', &
                              PondGHE(Item)%Name)
    CALL SetupOutputVariable('Pond ground heat exchanger inlet temp[C]',     &
                              PondGHEReport(Item)%InletTemp,'Plant','Average', &
                              PondGHE(Item)%Name)
    CALL SetupOutputVariable('Pond ground heat exchanger outlet temp[C]',     &
                              PondGHEReport(Item)%OutletTemp,'Plant','Average', &
                              PondGHE(Item)%Name)
    CALL SetupOutputVariable('Pond ground heat exchanger bulk temp[C]',     &
                              PondGHEReport(Item)%PondTemp,'Plant','Average', &
                              PondGHE(Item)%Name)
  END DO

  IF (NoDeepGroundTempObjWarning) THEN
    IF (.not. GroundTemp_DeepObjInput) THEN
      CALL ShowWarningError('GetPondGroundHeatExchanger:  No "Site:GroundTemperature:Deep" were input.')
      CALL ShowContinueError('Defaults, constant throughout the year of ('//TRIM(RoundSigDigits(GroundTemp_Deep,1))// &
                             ') will be used.')
    ENDIF
    NoDeepGroundTempObjWarning=.false.
  ENDIF

  RETURN

END SUBROUTINE GetPondGroundHeatExchanger

!==============================================================================

SUBROUTINE InitPondGroundHeatExchanger(PondGHENum,FirstHVACIteration,RunFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine Resets the elements of the data structure as necessary
          ! at the first HVAC iteration of each time step.

          ! METHODOLOGY EMPLOYED:
          ! One of the things done here is to update the record of the past pond
          ! temperature. This is needed in order to solve the diff. eqn. to find
          ! the temperature at the end of the next time step.
          ! Also set module variables to data structure for this pond. Set flow rate
          ! from node data and hypothetical design flow.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  USE DataGlobals,     ONLY: BeginTimeStepFlag, PI,warmupflag
  USE DataLoopNode,    ONLY: Node
  USE DataEnvironment, ONLY: GroundTemp_Deep, OutDryBulbTempAt
  USE DataPlant,       ONLY: TypeOf_GrndHtExchgPond, ScanPlantLoopsForObject
  USE PlantUtilities,  ONLY: SetComponentFlowRate, InitComponentNodes, RegisterPlantCompDesignFlow, &
                             RegulateCondenserCompFlowReqOp
  USE FluidProperties, ONLY: GetDensityGlycol, GetSpecificHeatGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  INTEGER, INTENT(IN) :: PondGHENum          ! component number
  LOGICAL, INTENT(IN) :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep
!  INTEGER, INTENT(IN) :: FlowLock            ! flow initialization/condition flag    !DSU
  LOGICAL, INTENT(IN) :: RunFlag             ! TRUE if equipment scheduled to operate

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL, PARAMETER :: DesignVelocity=0.5  ! Hypothetical design max pipe velocity [m/s]
  REAL, PARAMETER :: PondHeight = 0.0    ! for now

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL           :: DesignFlow              ! Hypothetical design flow rate
  LOGICAL, SAVE       :: OneTimeFlag = .TRUE.    ! flag for one time intializations
  INTEGER             :: PondNum                 ! loop counter
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyFlag
  LOGICAL, SAVE                            :: MyOneTimeFlag = .TRUE.
  INTEGER :: LoopNum
  INTEGER :: LoopSideNum
  REAL :: rho
  REAL :: Cp
  LOGICAL :: errFlag
  !repeated warm up days tend to drive the initial pond temperature toward the drybulb temperature
  !For each environment start the pond midway between drybulb and ground temp.
  IF(OneTimeFlag .or. warmupflag)THEN
    DO PondNum = 1, NumOfPondGHEs
      ! initialize pond temps to mean of drybulb and ground temps.
      PondGHE%BulkTemperature     = 0.5 * (OutDryBulbTempAt(PondHeight) + GroundTemp_Deep)
      PondGHE%PastBulkTemperature = 0.5 * (OutDryBulbTempAt(PondHeight) + GroundTemp_Deep)
      OneTimeFlag = .FALSE.
    END DO
  END IF

  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyFlag(NumOfPondGHEs))
    MyOneTimeFlag = .false.
    MyFlag = .TRUE.
  END IF

  ! Init more variables
  IF (MyFlag(PondGHENum)) THEN
    ! Locate the hx on the plant loops for later usage
    errFlag=.false.
    CALL ScanPlantLoopsForObject(PondGHE(PondGHENum)%Name, &
                                 TypeOf_GrndHtExchgPond, &
                                 PondGHE(PondGHENum)%LoopNum, &
                                 PondGHE(PondGHENum)%LoopSideNum, &
                                 PondGHE(PondGHENum)%BranchNum, &
                                 PondGHE(PondGHENum)%CompNum,  &
                                 errFlag=errFlag)
    IF (errFlag) THEN
      CALL ShowFatalError('InitPondGroundHeatExchanger: Program terminated due to previous condition(s).')
    ENDIF
    rho = GetDensityGlycol(PlantLoop(PondGHE(PondGHENum)%LoopNum)%fluidName, &
                                       constant_zero,&
                                        PlantLoop(PondGHE(PondGHENum)%LoopNum)%fluidIndex, &
                                        'InitPondGroundHeatExchanger')
    Cp = GetSpecificHeatGlycol(PlantLoop(PondGHE(PondGHENum)%LoopNum)%fluidName, &
                                       constant_zero,&
                                        PlantLoop(PondGHE(PondGHENum)%LoopNum)%fluidIndex, &
                                        'InitPondGroundHeatExchanger')
    PondGHE(PondGHENum)%DesignMassFlowRate = PI/4.0 * PondGHE(PondGHENum)%TubeInDiameter**2 * DesignVelocity * &
                                   rho * PondGHE(PondGHENum)%NumCircuits
    PondGHE(PondGHENum)%DesignCapacity = PondGHE(PondGHENum)%DesignMassFlowRate * Cp * 10. !assume 10C delta T?
    CALL InitComponentNodes(0.,  PondGHE(PondGHENum)%DesignMassFlowRate, &
                                 PondGHE(PondGHENum)%InletNodeNum, &
                                 PondGHE(PondGHENum)%OutletNodeNum, &
                                 PondGHE(PondGHENum)%LoopNum, &
                                 PondGHE(PondGHENum)%LoopSideNum, &
                                 PondGHE(PondGHENum)%BranchNum, &
                                 PondGHE(PondGHENum)%CompNum)
    CALL RegisterPlantCompDesignFlow(PondGHE(PondGHENum)%InletNodeNum, PondGHE(PondGHENum)%DesignMassFlowRate /rho)
                                 
    MyFlag(PondGHENum)=.FALSE.
  ENDIF

  ! check if we are in very first call for this zone time step
  LoopNum = PondGHE(PondGHENum)%LoopNum
  LoopSideNum = PondGHE(PondGHENum)%LoopSideNum
  IF (BeginTimeStepFlag.AND.FirstHVACIteration.AND.PlantLoop(LoopNum)%Loopside(LoopSideNum)%FlowLock==1) THEN !DSU
    ! update past temperature
    PondGHE(PondGHENum)%PastBulkTemperature = PondGHE(PondGHENum)%BulkTemperature
  END IF

  ! initialize - module variables
  InletNodeNum     = PondGHE(PondGHENum)%InletNodeNum
  OutletNodeNum    = PondGHE(PondGHENum)%OutletNodeNum
  PondArea         = PondGHE(PondGHENum)%Area
  PondDepth        = PondGHE(PondGHENum)%Depth
  InletTemp        = Node(InletNodeNum)%Temp
  OutletTemp       = Node(OutletNodeNum)%Temp
  TubeInDiameter   = PondGHE(PondGHENum)%TubeInDiameter
  TubeOutDiameter  = PondGHE(PondGHENum)%TubeOutDiameter
  TubeConductivity = PondGHE(PondGHENum)%TubeConductivity
  GrndConductivity = PondGHE(PondGHENum)%GrndConductivity
  NumCircuits      = PondGHE(PondGHENum)%NumCircuits
  CircLength       = PondGHE(PondGHENum)%CircuitLength
  ! temperatures
  PondTemp         = PondGHE(PondGHENum)%BulkTemperature
  PastPondTemp     = PondGHE(PondGHENum)%PastBulkTemperature


    DesignFlow = RegulateCondenserCompFlowReqOp(PondGHE(PondGHENum)%LoopNum,&
                                                PondGHE(PondGHENum)%LoopSideNum,&
                                                PondGHE(PondGHENum)%BranchNum,&
                                                PondGHE(PondGHENum)%CompNum,     &
                                                PondGHE(PondGHENum)%DesignMassFlowRate)


  CALL SetComponentFlowRate(DesignFlow, &
                               PondGHE(PondGHENum)%InletNodeNum,&
                               PondGHE(PondGHENum)%OutletNodeNum,&
                               PondGHE(PondGHENum)%LoopNum,&
                               PondGHE(PondGHENum)%LoopSideNum,&
                               PondGHE(PondGHENum)%BranchNum,&
                               PondGHE(PondGHENum)%CompNum)

  ! get the current flow rate - module variable
  FlowRate   = Node(InletNodeNum)%MassFlowRate


END SUBROUTINE InitPondGroundHeatExchanger

!==============================================================================

SUBROUTINE CalcPondGroundHeatExchanger(PondGHENum)

          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine does all of the stuff that is necessary to simulate
          ! a pond ground heat exchanger.  Calls are made to appropriate subroutines
          ! either in this module or outside of it.

          ! METHODOLOGY EMPLOYED:
          ! The differential equation defined by the heat balance is solved using
          ! a fourth order Runge-Kutta numerical integration method. The differential
          ! equation is:
          !            Mdot*Cp*dT/dt = Sum of fluxes.

          ! REFERENCES:
          ! Chiasson, A. Advances in Modeling of Ground-Source Heat Pump Systems.
          !   M.S. Thesis, Oklahoma State University, December 1999.
          ! Chiasson, A.D., J.D. Spitler, S.J. Rees, M.D. Smith.  2000.  A Model For
          !   Simulating The Performance Of A Shallow Pond As A Supplemental Heat
          !   Rejecter With Closed-Loop Ground-Source Heat Pump Systems.
          !   ASHRAE Transactions.  106(2):107-121.

          ! USE STATEMENTS:
  USE DataLoopNode,      ONLY : Node
  USE DataHVACGlobals,   ONLY : TimeStepSys
  USE FluidProperties,   ONLY : GetSpecificHeatGlycol, GetDensityGlycol
  USE DataGlobals,       ONLY : SimTimeSteps, CurrentTime, BeginDayFlag, SecInHour


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: PondGHENum  ! Number of the Pond GHE

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL    :: PondTempStar
  REAL    :: PondTempStarStar
  REAL    :: PondTempStarStarStar
  REAL    :: Flux
  REAL    :: FluxStar
  REAL    :: FluxStarStar
  REAL    :: NewPondTemp
  REAL    :: SpecificHeat
  REAL    :: PondMass

  PondMass = PondDepth*PondArea* &
             GetDensityGlycol('WATER',MAX(PondTemp, constant_zero),  &
                               WaterIndex,'CalcPondGroundHeatExchanger')
     
  SpecificHeat = GetSpecificHeatGlycol('WATER',MAX(PondTemp, constant_zero),  &
                 WaterIndex,'CalcPondGroundHeatExchanger')  !DSU bug fix here, was using working fluid index

  Flux = CalcTotalFLux(PondTemp,PondGHENum)
  PondTempStar = PastPondTemp + 0.5*SecInHour*TimeStepSys*Flux/ (SpecificHeat*PondMass)

  FluxStar = CalcTotalFLux(PondTempStar,PondGHENum)
  PondTempStarStar = PastPondTemp + 0.5*SecInHour*TimeStepSys*FluxStar/ (SpecificHeat*PondMass)

  FluxStarStar = CalcTotalFLux(PondTempStarStar,PondGHENum)
  PondTempStarStarStar = PastPondTemp + SecInHour*TimeStepSys*FluxStarStar/(SpecificHeat*PondMass)

  NewPondTemp = PastPondTemp + SecInHour*TimeStepSys*(Flux+2.0*FluxStar + 2.0*FluxStarStar + &
                                                          CalcTotalFLux(PondTempStarStarStar,PondGHENum)) / &
                                                          (6.0*SpecificHeat*PondMass)

  PondTemp = NewPondTemp

END SUBROUTINE CalcPondGroundHeatExchanger

!==============================================================================

FUNCTION CalcTotalFLux(PondBulkTemp,PondGHENum)

          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Thic calculates the summation of the heat fluxes on the pond for a
          ! given pond temperature. The following heat fluxes are calculated:
          !   convection,
          !   long-wave radiation,
          !   solar gain,
          !   evaporation,
          !   ground conduction,
          !   along with heat exchange with the fluid

          ! METHODOLOGY EMPLOYED:
          ! Convection is calculated with the ASHRAE simple convection coefficients.
          ! Evaporation is calculated assuming a fixed Lewis number - not as in
          ! Chaisson model. Heat transfer with the fluid is calculated using a heat
          ! exchanger Effectiveness-NTU method, where the pond is seen as a static
          ! fluid - this is also different from Chaisson's original model (assumed
          ! pond at average of inlet and outlet temps).

          ! REFERENCES:
          ! Chiasson, A. Advances in Modeling of Ground-Source Heat Pump Systems.
          !   M.S. Thesis, Oklahoma State University, December 1999.
          ! Chiasson, A.D., J.D. Spitler, S.J. Rees, M.D. Smith.  2000.  A Model For
          !   Simulating The Performance Of A Shallow Pond As A Supplemental Heat
          !   Rejecter With Closed-Loop Ground-Source Heat Pump Systems.
          !   ASHRAE Transactions.  106(2):107-121.
          ! Hull, J.R., K.V. Liu, W.T. Sha, J. Kamal, and C.E. Nielsen, 1984.
          !   Dependence of Ground Heat Losses Upon Solar Pond Size and Perimeter
          !   Insulation Calculated and Experimental Results. Solar Energy,33(1):25-33.

          ! USE STATEMENTS:
  USE DataEnvironment, ONLY: OutDryBulbTempAt,OutWetBulbTempAt,WindSpeedAt,IsSnow,IsRain,SkyTemp,OutBaroPress,  &
                             GroundTemp_Deep
  USE FluidProperties,   ONLY : GetSpecificHeatGlycol
  USE ConvectionCoefficients, ONLY : CalcASHRAESimpExtConvectCoeff
  USE DataGlobals
  USE DataHeatBalance, ONLY: VeryRough
  USE Psychrometrics, ONLY:PsyCpAirFnWTdb,PsyWFnTdbTwbPb,PsyHfgAirFnWTdb

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL         :: CalcTotalFLux            ! function return variable
  REAL, INTENT(IN)  :: PondBulkTemp             ! pond temp for this flux calculation
  INTEGER, INTENT(IN) :: PondGHENum  ! Number of the Pond GHE

          ! FUNCTION PARAMETER DEFINITIONS:
  REAL, PARAMETER   :: PrantlAir = 0.71         ! Prantl number for air - assumed constant
  REAL, PARAMETER   :: SchmidtAir = 0.6         ! Schmidt number for air - assumed constant
  REAL, PARAMETER   :: PondHeight = 0.0         ! for now

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL    :: ConvCoef             ! convection coefficient
  REAL    :: ExternalTemp         ! external environmental temp - drybulb or wetbulb
  REAL    :: FluxSolAbsorbed      ! absorbed solar flux
  REAL    :: FluxLongwave         ! absorbed longwave flux
  REAL    :: FluxConvect          ! convective flux
  REAL    :: FluxEvap             ! evaporative heat flux
  REAL    :: FluxGround           ! ground heat transfer flux
  REAL    :: Qfluid               ! convective flux
  REAL    :: SurfTempAbs          ! absolute value of surface temp
  REAL    :: SkyTempAbs           ! absolute value of sky temp
  REAL    :: ThermalAbs           ! thermal absorptivity
  REAL    :: SpecHeat             ! specific heat capacity
  REAL    :: HumRatioFilm         ! humidity ratio at pond surface/film temperature
  REAL    :: HumRatioAir          ! humidity ratio of air
  REAL    :: SpecHeatAir          ! air specific heat
  REAL    :: LatentHeatAir        ! latent heat of air
  REAL    :: UvalueGround         ! ground heat transfer coefficient
  REAL    :: Perimeter            ! pond perimeter
  REAL    :: OutDryBulb           ! drybulb at pond height
  REAL    :: OutWetBulb           ! wetbulb at pond height


  ! make a surface heat balance and solve for temperature
  ThermalAbs = 0.9

  ! set appropriate external temp
  ! use height dependency --  if there was a height for this unit, it could be inserted.
  ! parameter PondHeight=0.0 is used.
  OutDryBulb=OutDryBulbTempAt(PondHeight)
  OutWetBulb=OutWetBulbTempAt(PondHeight)
  IF(IsSnow)THEN
    ExternalTemp = OutWetBulb
  ELSE IF(IsRain)THEN
    ExternalTemp = OutWetBulb
  ELSE  ! normal dry conditions
    ExternalTemp = OutDryBulb
  END IF

  ! absolute temperatures
  SurfTempAbs = PondBulkTemp + KelvinConv
  SkyTempAbs  = SkyTemp + KelvinConv

  ! ASHRAE simple convection coefficient model for external surfaces.
  ConvCoef     = CalcASHRAESimpExtConvectCoeff(VeryRough,WindSpeedAt(PondHeight))
  ! convective flux
  FluxConvect = ConvCoef*(PondBulkTemp - ExternalTemp)

  ! long-wave radiation between pond and sky.
  FluxLongwave = StefBoltzmann*ThermalAbs*((SurfTempAbs**4)-(SkyTempAbs**4))

  ! total absorbed solar using function - no ground solar
  FluxSolAbsorbed = CalcSolarFlux()

  ! specific heat from fluid prop routines
  SpecHeat = GetSpecificHeatGlycol(PlantLoop(PondGHE(PondGHENum)%LoopNum)%FluidName,MAX(InletTemp,0.0),  &
                                PlantLoop(PondGHE(PondGHENum)%LoopNum)%FluidIndex,'PondGroundHeatExchanger:CalcTotalFlux')
  ! heat transfer with fluid - heat exchanger analogy.
  Qfluid = FlowRate*SpecHeat*CalcEffectiveness(InletTemp,PondBulkTemp,FlowRate,PondGHENum)* &
           (InletTemp - PondBulkTemp)

  HeatTransRate = Qfluid

  ! evaporation flux
  ! get air properties
  HumRatioAir   = PsyWFnTdbTwbPb(OutDrybulb, OutWetBulb, OutBaroPress)
  HumRatioFilm  = PsyWFnTdbTwbPb(PondBulkTemp, PondBulkTemp, OutBaroPress)
  SpecHeatAir   = PsyCpAirFnWTdb(HumRatioAir, OutDrybulb)
  LatentHeatAir = PsyHfgAirFnWTdb(HumRatioAir, OutDrybulb,'PondGroundHeatExchanger:CalcTotalFlux')

  FluxEvap = (PrantlAir/SchmidtAir)**2.0/3.0 * ConvCoef/SpecHeatAir * &
             (HumRatioFilm - HumRatioAir) * LatentHeatAir

  ! ground heat transfer flux
  Perimeter = 4.0*SQRT(PondArea)  ! square assumption
  UvalueGround = 0.999*(GrndConductivity/PondDepth) + 1.37*(GrndConductivity*Perimeter/PondArea)
  FluxGround   = UvalueGround * (PondBulkTemp - GroundTemp_Deep)

  CalcTotalFLux = Qfluid + PondArea*(FluxSolAbsorbed - FluxConvect - FluxLongwave - &
                                     FluxEvap - FluxGround)
  IF(BeginTimeStepFlag)THEN

  END IF

END FUNCTION CalcTotalFLux

!==============================================================================

FUNCTION CalcSolarFlux()

          ! FUNCTION INFORMATION:
          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This is used to calculate the net solar flux absorbed by the pond.

          ! METHODOLOGY EMPLOYED:
          ! This is calculated from basic optical formula using the extinction
          ! coefficient of the pond as the main parameter. This can be in a
          ! wide range: 0.13 - 7.5 in the literature depending on algae, suspended
          ! solids etc. ??

          ! REFERENCES:
          ! Duffie, J.A. and W.A. Beckman, 1991. Solar Engineering of Thermal
          !  Processes, 2 nd Edition. John Wiley and Sons.
          ! Chiasson, A. Advances in Modeling of Ground-Source Heat Pump Systems.
          !   M.S. Thesis, Oklahoma State University, December 1999.
          ! Chiasson, A.D., J.D. Spitler, S.J. Rees, M.D. Smith.  2000.  A Model For
          !   Simulating The Performance Of A Shallow Pond As A Supplemental Heat
          !   Rejecter With Closed-Loop Ground-Source Heat Pump Systems.
          !   ASHRAE Transactions.  106(2):107-121.

          ! USE STATEMENTS:
  USE DataEnvironment, ONLY : BeamSolarRad, DifSolarRad, SunIsUp, SOLCOS


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL           :: CalcSolarFlux      ! Function return variable

          ! FUNCTION PARAMETER DEFINITIONS:
  REAL, PARAMETER    :: WaterRefIndex       = 1.33     ! refractive index of water
  REAL, PARAMETER    :: AirRefIndex         = 1.0003   ! refractive index of air
  REAL, PARAMETER    :: PondExtCoef         = 0.3      ! extinction coefficent of water

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL    :: IncidAngle           ! angle of incidence of beam
  REAL    :: RefractAngle         ! angle of refraction of beam
  REAL    :: Transmitance         ! transmitted solar
  REAL    :: Reflectance          ! reflectance
  REAL    :: Absorbtance          ! absorbed solar

  REAL    :: ParallelRad          ! parallel component of irradiation
  REAL    :: PerpendRad           ! parallel component of irradiation

        ! FLOW:

  ! check for sun up.
  IF (.not. SunIsUp) THEN
    CalcSolarFlux = 0.0
    RETURN
  END IF

  ! get the incidence and reflection angles
  IncidAngle   = ACOS(SOLCOS(3))
  RefractAngle = ASIN( SIN(IncidAngle) * AirRefIndex/WaterRefIndex)

  ! absorbed component: Tau_a
  Absorbtance  = EXP(-PondExtCoef*PondDepth/COS(RefractAngle))

  ! parallel and perpendicular components
  ParallelRad  = TAN(RefractAngle-IncidAngle)**2 / TAN(RefractAngle+IncidAngle)**2
  PerpendRad   = SIN(RefractAngle-IncidAngle)**2 / SIN(RefractAngle+IncidAngle)**2

  ! transmittance: Tau
  Transmitance = 0.5*Absorbtance*((1.0-ParallelRad)/(1.0+ParallelRad)+(1.0-PerpendRad)/(1.0+PerpendRad))

  ! reflectance: Tau_a - Tau
  Reflectance  = Absorbtance - Transmitance

  ! apply reflectance to beam and diffuse solar to find flux
  CalcSolarFlux = (1.0-Reflectance)*(SOLCOS(3)*BeamSolarRad + DifSolarRad)

  RETURN

END FUNCTION CalcSolarFlux

!==============================================================================

FUNCTION CalcEffectiveness(InsideTemperature, PondTemperature,MassFlowRate,PondGHENum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates the "heat exchanger" effectiveness.
          ! This routine is adapted from that in the low temp radiant pond model.

          ! METHODOLOGY EMPLOYED:
          ! The heat transfer coefficient is calculated at the pipe and
          ! consists of inside and outside convection coefficients and conduction
          ! through the pipe. The other assumptions are that the tube inside
          ! surface temperature is equal to the "source location temperature"
          ! and that it is a CONSTANT throughout the pond. External convection is
          ! natural mode using Churchill and Chu correlation. Inside convection
          ! calcualted using the Dittus-Boelter equation.

          ! REFERENCES:
          ! Incropera, F.P. and D.P. DeWitt, 1996. Introduction to Heat Transfer,
          !   3 rd Edition. John Wiley & Sons.
          ! Churchill, S.W. and H.H.S. Chu. 1975. Correlating Equations for
          !   Laminar and Turbulent Free Convection from a Horizontal Cylinder.
          !   International Journal of Heat and Mass Transfer, 18: 1049-1053.
          ! See also RadiantSystemLowTemp module.

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : PI, NumOfTimeStepInHour
  USE General,         ONLY : RoundSigDigits
  USE FluidProperties, ONLY : GetSpecificHeatGlycol,GetConductivityGlycol, &
                              GetViscosityGlycol,GetDensityGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL            :: CalcEffectiveness             ! Function return variable
  REAL,INTENT(IN) :: InsideTemperature             ! Temperature of fluid in pipe circuit, in C
  REAL,INTENT(IN) :: PondTemperature               ! Temperature of pond water (i.e. outside the pipe), in C
  REAL,INTENT(IN) :: MassFlowRate                  ! Mass flow rate, in kg/s
  INTEGER, INTENT(IN)  :: PondGHENum                    ! Number of the Pond GHE

          ! FUNCTION PARAMETER DEFINITIONS:
  REAL, PARAMETER    :: MaxLaminarRe       = 2300.     ! Maximum Reynolds number for laminar flow
  REAL, PARAMETER    :: GravConst          = 9.81      ! gravitational constant - should be fixed!
  CHARACTER(len=*), PARAMETER :: CalledFrom='PondGroundHeatExchanger:CalcEffectiveness'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL    :: NuseltNum              ! Nuselt number (dimensionless)
  REAL    :: PrantlNum              ! Prandtl number (dimensionless)
  REAL    :: ReynoldsNum            ! Reynolds number (dimensionless)
  REAL    :: RayleighNum            ! Rayleigh number (dimensionless)
  REAL    :: ThermDiff              ! thermal diffusivity
  REAL    :: ExpansionCoef          ! Expansion coefficient, in K^-1
  REAL    :: Viscosity              ! Viscosity, in Ns/m2
  REAL    :: Density                ! fluid density
  REAL    :: SpecificHeat           ! Fluid specific heat
  REAL    :: Conductivity           ! Fluid thermal conductivity
  REAL    :: WaterSpecHeat          ! Specific heat of pond water
  REAL    :: NTU                    ! Number of transfer units, non-dimensional
  REAL    :: ConvCoefOut            ! convection coefficient at outside of pipe
  REAL    :: ConvCoefIn             ! convection coefficient at inside of pipe
  REAL    :: PipeResistance         ! pipe wall thermal resistance
  REAL    :: TotalResistance        ! total pipe thermal resistance - conduction and convection
!  INTEGER, SAVE ::ErrCount=0
!  INTEGER, SAVE ::ConsecutiveFrozen=0

  ! evaluate properties at pipe fluid temperature for given pipe fluid

  SpecificHeat  = GetSpecificHeatGlycol(PlantLoop(PondGHE(PondGHENum)%LoopNum)%FluidName,InsideTemperature, &
                                        PlantLoop(PondGHE(PondGHENum)%LoopNum)%FluidIndex,CalledFrom)
  Conductivity  = GetConductivityGlycol(PlantLoop(PondGHE(PondGHENum)%LoopNum)%FluidName,InsideTemperature, &
                                        PlantLoop(PondGHE(PondGHENum)%LoopNum)%FluidIndex,CalledFrom)
  Viscosity     = GetViscosityGlycol(PlantLoop(PondGHE(PondGHENum)%LoopNum)%FluidName,InsideTemperature, &
                                     PlantLoop(PondGHE(PondGHENum)%LoopNum)%FluidIndex,CalledFrom)
  Density       = GetDensityGlycol(PlantLoop(PondGHE(PondGHENum)%LoopNum)%FluidName,InsideTemperature, &
                                   PlantLoop(PondGHE(PondGHENum)%LoopNum)%FluidIndex,CalledFrom)

  ! Calculate the Reynold's number from RE=(4*Mdot)/(Pi*Mu*Diameter)
  ReynoldsNum = 4.0 * MassFlowRate / ( PI * Viscosity * TubeInDiameter * NumCircuits)

  PrantlNum   = Viscosity*SpecificHeat/Conductivity


  ! Calculate the Nusselt number based on what flow regime one is in. h = (k)(Nu)/D
  IF (ReynoldsNum >= MaxLaminarRe) THEN ! Turbulent flow --> use Dittus-Boelter equation
    NuseltNum = 0.023*(ReynoldsNum**(0.8))*(PrantlNum**(0.3))
  ELSE    ! Laminar flow --> use constant surface temperature relation
    NuseltNum = 3.66
  END IF

  ! inside convection resistance, from Nu
  ConvCoefIn = Conductivity * NuseltNum / TubeInDiameter

  ! now find properties of pond water - always assume pond fluid is water
  WaterSpecHeat = GetSpecificHeatGlycol('WATER',MAX(PondTemperature,0.0), WaterIndex,CalledFrom)
  Conductivity  = GetConductivityGlycol('WATER',MAX(PondTemperature,0.0), WaterIndex,CalledFrom)
  Viscosity     = GetViscosityGlycol('WATER',MAX(PondTemperature,0.0), WaterIndex,CalledFrom)
  Density       = GetDensityGlycol('WATER',MAX(PondTemperature,0.0), WaterIndex,CalledFrom)

  ! derived properties for natural convection coefficient
  ! expansion coef (Beta) = -1/Rho. dRho/dT
  ! The following code includes some slight modifications from Simon's original code.
  ! It guarantees that the delta T is 10C and also avoids the problems associated with
  ! water hitting a maximum density at around 4C. (RKS)
  ExpansionCoef = -(GetDensityGlycol('WATER',MAX(PondTemperature,10.0) + 5.0, WaterIndex,CalledFrom) - &
                   GetDensityGlycol('WATER',MAX(PondTemperature,10.0) - 5.0, WaterIndex,CalledFrom)) / &
                   (10.0*Density)

  ThermDiff = Conductivity/(Density*WaterSpecHeat)
  PrantlNum = Viscosity*WaterSpecHeat/Conductivity

  RayleighNum = Density*GravConst*ExpansionCoef*ABS(InsideTemperature - PondTemperature) * &
                     TubeOutDiameter**3 / (Viscosity*ThermDiff)

  ! Calculate the Nusselt number for natural convection at outside of pipe
  NuseltNum = (0.6 + (0.387*RayleighNum**(1.0/6.0)/((1.0+0.559/PrantlNum**(9.0/16.0))**(8.0/27.0))))**2

  ! outside convection resistance, from Nu
  ConvCoefOut = Conductivity * NuseltNum / TubeOutDiameter

  ! conduction resistance of pipe
  PipeResistance = TubeInDiameter/TubeConductivity * LOG(TubeOutDiameter/TubeInDiameter)

  TotalResistance = PipeResistance + 1.0/ConvCoefIn + TubeInDiameter/(TubeOutDiameter*ConvCoefOut)

  ! Calculate the NTU parameter
  ! NTU = UA/[(Mdot*Cp)min] = A/[Rtot*(Mdot*Cp)min]
  ! where: Rtot = Ri,convection + Rconduction + Ro,conveciton
  !        A = Pi*D*TubeLength

  IF(MassFlowRate == 0.0) Then
    CalcEffectiveness = 1.0
  Else
    NTU = PI * TubeInDiameter * CircLength*NumCircuits / (TotalResistance * MassFlowRate * SpecificHeat)
     ! Calculate effectiveness - formula for static fluid
    CalcEffectiveness = (1.-EXP(-NTU))
  End If

  ! Check for frozen pond
  IF (PondTemperature .LT. 0.0) THEN
    PondGHE(PondGHENum)%ConsecutiveFrozen=PondGHE(PondGHENum)%ConsecutiveFrozen+1
    IF (PondGHE(PondGHENum)%FrozenErrIndex == 0) THEN
      CALL ShowWarningMessage('GroundHeatExchanger:Pond="'//trim(PondGHE(PondGHENum)%Name)//  &
         '", is frozen; Pond model not valid. Calculated Pond Temperature=['//  &
         trim(RoundSigDigits(PondTemperature,2))//'] C')
      CALL ShowContinueErrorTimeStamp(' ')
    ENDIF
    CALL ShowRecurringWarningErrorAtEnd('GroundHeatExchanger:Pond="'//trim(PondGHE(PondGHENum)%Name)//  &
         '", is frozen',PondGHE(PondGHENum)%FrozenErrIndex, ReportMinOf=PondTemperature,ReportMinUnits='[C]',  &
         ReportMaxOf=PondTemperature,ReportMaxUnits='[C]')
    IF (PondGHE(PondGHENum)%ConsecutiveFrozen >= NumOfTimeStepInHour*30) THEN
      CALL ShowFatalError('GroundHeatExchanger:Pond="'//trim(PondGHE(PondGHENum)%Name)//  &
         '" has been frozen for 30 consecutive hours.  Program terminates.')
    ENDIF
  ELSE
    PondGHE(PondGHENum)%ConsecutiveFrozen=0
  END IF

  RETURN

END FUNCTION CalcEffectiveness

!==============================================================================

SUBROUTINE UpdatePondGroundHeatExchanger(PondGHENum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine does any updating that needs to be done for pond
          ! ground heat exchangers.   This routine must also set the outlet water
          ! conditions.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : TimeStepZone
  USE DataLoopNode,    ONLY : Node
  USE FluidProperties, ONLY : GetSpecificHeatGlycol
  USE PlantUtilities,  ONLY : SafeCopyPlantNode

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: PondGHENum  ! Index for the pond

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL    :: CpFluid            ! Specific heat of working fluid

  ! Calculate the water side outlet conditions and set the
  ! appropriate conditions on the correct HVAC node.
  CpFluid = GetSpecificHeatGlycol(PlantLoop(PondGHE(PondGHENum)%LoopNum)%FluidName,InletTemp,  &
     PlantLoop(PondGHE(PondGHENum)%LoopNum)%FluidIndex,'PondGroundHeatExchanger:Update')
  ! check for flow
    
  CALL SafeCopyPlantNode(InletNodeNum, OutletNodeNum)
    
  IF ( (CpFluid > 0.0) .AND. (FlowRate > 0.0) ) THEN

    Node(OutletNodeNum)%Temp         = InletTemp - HeatTransRate / (FlowRate*CpFluid)
    Node(OutletNodeNum)%Enthalpy     = Node(OutletNodeNum)%Temp*CpFluid
  END IF

  ! keep track of the bulk temperature
  PondGHE(PondGHENum)%BulkTemperature = PondTemp

  RETURN

END SUBROUTINE UpdatePondGroundHeatExchanger

!==============================================================================

SUBROUTINE ReportPondGroundHeatExchanger(PondGHENum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   August 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simply produces output for Pond ground heat exchangers

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,     ONLY : SecInHour
  USE DataHVACGlobals, ONLY : TimeStepSys
  USE DataLoopNode,    ONLY : Node

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: PondGHENum  ! Index for the pond under consideration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

          ! FLOW:

  ! update flows and temps from node data
  PondGHEReport(PondGHENum)%InletTemp         = Node(PondGHE(PondGHENum)%InletNodeNum)%Temp
  PondGHEReport(PondGHENum)%OutletTemp        = Node(PondGHE(PondGHENum)%OutletNodeNum)%Temp
  PondGHEReport(PondGHENum)%MassFlowRate      = Node(PondGHE(PondGHENum)%InletNodeNum)%MassFlowRate

  ! update other variables from module variables
  PondGHEReport(PondGHENum)%HeatTransferRate  = HeatTransRate
  PondGHEReport(PondGHENum)%Energy            = HeatTransRate*TimeStepSys*SecInHour
  PondGHEReport(PondGHENum)%PondTemp          = PondTemp


  RETURN

END SUBROUTINE ReportPondGroundHeatExchanger

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

END MODULE PondGroundHeatExchanger