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
          !Loop fluid identifiers
!INTEGER, PARAMETER:: Water     =1  ! saturated to compressed liquid
!INTEGER, PARAMETER:: Steam     =2  ! saturated and superheated conditions
!INTEGER, PARAMETER:: EthGlycol =3  ! ethylene glycol brine of various concentrations
!INTEGER, PARAMETER:: Propglycol=4  ! propylene glycol brine of various concentrations
!INTEGER, PARAMETER:: CaChloride=5  ! calcium chloride brine of various concentrations
!INTEGER, PARAMETER:: NaChloride=6  ! sodium chloride brine of various concentrations
!INTEGER, PARAMETER:: Refrig    =7  ! currently, "generic" refrigerant...the parameter
                                   ! list may be extended to include a number of specific
                                   ! refrigerants

INTEGER, PARAMETER :: BeginDay = 1
INTEGER, PARAMETER :: DuringDay = 2
INTEGER, PARAMETER :: EndDay = 3
INTEGER, PARAMETER :: EndZoneSizingCalc = 4
INTEGER, PARAMETER :: EndSysSizingCalc = 5

INTEGER, PARAMETER :: ZoneTSReporting=1  ! value for Zone Time Step Reporting (UpdateDataAndReport)
INTEGER, PARAMETER :: HVACTSReporting=2  ! value for HVAC Time Step Reporting (UpdateDataAndReport)

REAL, PARAMETER    :: PI= 3.141592653589793   ! Pi
REAL, PARAMETER    :: PiOvr2 = PI/2.          ! Pi/2
REAL, PARAMETER    :: DegToRadians = PI/180.  ! Conversion for Degrees to Radians
REAL, PARAMETER    :: SecInHour = 3600.0      ! Conversion for hours to seconds
INTEGER, PARAMETER :: MaxNameLength = 60      ! Maximum Name Length in Characters -- should be the same
                                              ! as MaxAlphaArgLength in InputProcessor module

REAL, PARAMETER    :: InitConvTemp = 5.05     ! [deg C], standard init vol to mass flow conversion temp
INTEGER, PARAMETER :: MaxSlatAngs = 19
INTEGER, PARAMETER :: MaxRefPoints = 102      ! Maximum number of daylighting reference points, 2 + 10*10

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
  INTERFACE
    SUBROUTINE ShowMessage(Message,Unit1,Unit2)
    !  Use when you want to create your own message for the error file.
    CHARACTER(len=*) Message    ! Message automatically written to "error file"
    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
    END SUBROUTINE
  END INTERFACE
  INTERFACE
    SUBROUTINE ShowContinueError(Message,Unit1,Unit2)
    !  Use when you are "continuing" an error message over several lines.
    CHARACTER(len=*) Message    ! Message automatically written to "error file"
    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
    END SUBROUTINE
  END INTERFACE
  INTERFACE
    SUBROUTINE ShowFatalError(Message,Unit1,Unit2)
    !  Use when you want the program to terminate after writing messages
    !  to appropriate files
    CHARACTER(len=*) Message    ! Message automatically written to "error file"
    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
    END SUBROUTINE
  END INTERFACE
  INTERFACE
    SUBROUTINE ShowSevereError(Message,Unit1,Unit2)
    !  Use for "severe" error messages.  Might have several severe tests and then terminate.
    CHARACTER(len=*) Message    ! Message automatically written to "error file"
    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
    END SUBROUTINE
  END INTERFACE
  INTERFACE
    SUBROUTINE ShowWarningError(Message,Unit1,Unit2)
    !  Use for "warning" error messages.
    CHARACTER(len=*) Message    ! Message automatically written to "error file"
    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
    END SUBROUTINE
  END INTERFACE

  INTERFACE SetupOutputVariable
    SUBROUTINE SetupRealOutputVariable(VariableName,ActualVariable,IndexTypeKey,VariableTypeKey,KeyedValue,  &
                                       ReportFreq,ResourceTypeKey,EndUseKey,GroupKey)
      CHARACTER(len=*), INTENT(IN) :: VariableName   ! String Name of variable
      REAL, INTENT(IN), TARGET     :: ActualVariable ! Actual Variable, used to set up pointer
      CHARACTER(len=*), INTENT(IN) :: IndexTypeKey    ! Zone, HeatBalance=1, HVAC, System, Plant=2
      CHARACTER(len=*), INTENT(IN) :: VariableTypeKey ! State, Average=1, NonState, Sum=2
      CHARACTER(len=*), INTENT(IN) :: KeyedValue     ! Associated Key for this variable
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportFreq      ! Internal use -- causes reporting at this freqency
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: ResourceTypeKey ! Meter Resource Type (Electricity, Gas, etc)
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: EndUseKey       ! Meter End Use Key (Task Lights, Heating, Cooling, etc)
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: GroupKey        ! Meter Super Group Key (Building, System, Plant)
    END SUBROUTINE
    SUBROUTINE SetupIntegerOutputVariable(VariableName,IntActualVariable,IndexTypeKey,VariableTypeKey,KeyedValue,ReportFreq)
      CHARACTER(len=*), INTENT(IN) :: VariableName   ! String Name of variable
      INTEGER, INTENT(IN), TARGET  :: IntActualVariable ! Actual Variable, used to set up pointer
      CHARACTER(len=*), INTENT(IN) :: IndexTypeKey    ! Zone, HeatBalance=1, HVAC, System, Plant=2
      CHARACTER(len=*), INTENT(IN) :: VariableTypeKey ! State, Average=1, NonState, Sum=2
      CHARACTER(len=*), INTENT(IN) :: KeyedValue     ! Associated Key for this variable
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportFreq     ! Internal use -- causes reporting at this freqency
    END SUBROUTINE
    SUBROUTINE SetupRealOutputVariable_IntKey(VariableName,ActualVariable,IndexTypeKey,VariableTypeKey,KeyedValue,  &
                                              ReportFreq,ResourceTypeKey,EndUseKey,GroupKey)
      CHARACTER(len=*), INTENT(IN) :: VariableName   ! String Name of variable
      REAL, INTENT(IN), TARGET     :: ActualVariable ! Actual Variable, used to set up pointer
      CHARACTER(len=*), INTENT(IN) :: IndexTypeKey    ! Zone, HeatBalance=1, HVAC, System, Plant=2
      CHARACTER(len=*), INTENT(IN) :: VariableTypeKey ! State, Average=1, NonState, Sum=2
      INTEGER, INTENT(IN)          :: KeyedValue     ! Associated Key for this variable
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: ReportFreq      ! Internal use -- causes reporting at this freqency
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: ResourceTypeKey ! Meter Resource Type (Electricity, Gas, etc)
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: EndUseKey       ! Meter End Use Key (Task Lights, Heating, Cooling, etc)
      CHARACTER(len=*), INTENT(IN), OPTIONAL :: GroupKey        ! Meter Super Group Key (Building, System, Plant)
    END SUBROUTINE
  END INTERFACE

  INTERFACE SetupRealInternalOutputVariable
    INTEGER FUNCTION SetupRealInternalOutputVariable(VariableName,ActualVariable,IndexTypeKey,VariableTypeKey, &
                                                     KeyedValue,ReportFreq)
      CHARACTER(len=*), INTENT(IN) :: VariableName    ! String Name of variable
      CHARACTER(len=*), INTENT(IN) :: IndexTypeKey    ! Zone, HeatBalance or HVAC, System, Plant
      CHARACTER(len=*), INTENT(IN) :: VariableTypeKey ! State, Average, or NonState, Sum
      REAL, INTENT(IN), TARGET     :: ActualVariable  ! Actual Variable, used to set up pointer
      CHARACTER(len=*), INTENT(IN) :: KeyedValue      ! Associated Key for this variable
      CHARACTER(len=*), INTENT(IN) :: ReportFreq      ! Frequency to store 'timestep','hourly','monthly','environment'
    END FUNCTION
  END INTERFACE

  INTERFACE GetInternalVariableValue
    REAL FUNCTION GetInternalVariableValue(WhichVar)
      INTEGER, INTENT(IN) :: WhichVar ! Report number assigned to this variable
    END FUNCTION
  END INTERFACE

          ! MODULE VARIABLE DECLARATIONS:

LOGICAL :: BeginDayFlag         ! Set to true at the start of each day, set to false after first time step in day
LOGICAL :: BeginEnvrnFlag       ! Set to true at the start of each environment, set to false after first time step in environ
LOGICAL :: BeginHourFlag        ! Set to true at the start of each hour, set to false after first time step in hour
LOGICAL :: BeginSimFlag         ! Set to true until any actual simulation (full or sizing) has begun, set to
                                !   false after first time step
LOGICAL :: BeginFullSimFlag     ! Set to true until full simulation has begun, set to
                                !   false after first time step
LOGICAL :: BeginTimeStepFlag    ! Set to true at the start of each time step, set to false after first subtime step of time step
REAL    :: BigNumber            ! Max Number (real) used for initializations
DOUBLE PRECISION :: DBigNumber  ! Max Number (double precision) used for initializations
INTEGER :: DayOfSim             ! Counter for days (during the simulation)
LOGICAL :: EndEnvrnFlag         ! Set to true at the end of each environment (last time step of last hour of last day of environ)
LOGICAL :: EndDayFlag           ! Set to true at the end of each day (last time step of last hour of day)
LOGICAL :: EndHourFlag          ! Set to true at the end of each hour (last time step of hour)
INTEGER :: PreviousHour         ! Previous Hour Index
INTEGER :: HourOfDay            ! Counter for hours in a simulation day
!INTEGER :: NextHour             ! Next hour index
DOUBLE PRECISION :: WeightPreviousHour   ! Weighting of value for previous hour
!REAL    :: WeightNextHour       ! Weighting of value for next hour
DOUBLE PRECISION :: WeightNow            ! Weighting of value for current hour
INTEGER :: NumOfDayInEnvrn      ! Number of days in the simulation for a particular environment
INTEGER :: NumOfTimeStepInHour  ! Number of time steps in each hour of the simulation
INTEGER :: NumOfZones           ! Total number of Zones for simulation
INTEGER :: TimeStep             ! Counter for time steps (fractional hours)
REAL    :: TimeStepZone         ! Zone time step in fractional hours
LOGICAL :: WarmupFlag           ! Set to true during the warmup portion of a simulation
INTEGER :: OutputFileStandard   ! Unit number for the standard output file (hourly data only)
INTEGER :: StdOutputRecordCount ! Count of Standard output records
INTEGER :: OutputFileInits      ! Unit number for the standard Initialization output file
INTEGER :: OutputFileDebug      ! Unit number for debug outputs
INTEGER :: OutputFileZoneSizing ! Unit number of zone sizing calc output file
INTEGER :: OutputFileSysSizing  ! Unit number of system sizing calc output file
INTEGER :: OutputFileMeters     ! Unit number for meters output
INTEGER :: StdMeterRecordCount  ! Count of Meter output records
INTEGER :: OutputFileBNDetails  ! Unit number for Branch-Node Details
INTEGER :: OutputFileConstrainParams ! Unit number for special constrained free parameters output file
                                !  (for penalty functions in optimizing)
LOGICAL :: DebugOutput
LOGICAL :: EvenDuringWarmup
LOGICAL :: ZoneSizingCalc = .FALSE.       ! TRUE if zone sizing calculation
LOGICAL :: SysSizingCalc = .FALSE.        ! TRUE if system sizing calculation
LOGICAL :: DoZoneSizing         ! User input in RUN CONTROL object
LOGICAL :: DoSystemSizing       ! User input in RUN CONTROL object
LOGICAL :: DoPlantSizing        ! User input in RUN CONTROL object
LOGICAL :: DoDesDaySim          ! User input in RUN CONTROL object
LOGICAL :: DoWeathSim           ! User input in RUN CONTROL object
LOGICAL :: WeatherFile          ! TRUE if current environment is a weather file
LOGICAL :: DoOutputReporting    ! TRUE if variables to be written out
DOUBLE PRECISION :: CurrentTime ! CurrentTime, in fractional hours, from start of day. Uses Loads time step.
INTEGER :: SimTimeSteps         ! Number of (Loads) timesteps since beginning of run period (environment).
INTEGER :: MinutesPerTimeStep   ! Minutes per time step calculated from NumTimeStepInHour (number of minutes per load time step)
LOGICAL :: DisplayPerfSimulationFlag ! Set to true when "Performing Simulation" should be displayed
LOGICAL :: AirflowWindows = .FALSE. ! TRUE if one or more airflow windows

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
