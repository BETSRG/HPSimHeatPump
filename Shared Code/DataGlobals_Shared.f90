MODULE DataGlobals_HPSimIntegrated      ! EnergyPlus and HP Standalone Shared Module Data-Only Module

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
USE DataPrecisionGlobals

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.
                
        !RS: Carrying over from DataGlobals_HPSimIntegrated
          !INTEGER, PARAMETER :: MaxNameLength = 200      ! Maximum Name Length in Characters
          REAL, PARAMETER    :: AutoSize = -99999.
          CHARACTER(len=55), PARAMETER :: UpperCase='ABCDEFGHIJKLMNOPQRSTUVWXYZ�����������������������������'
          CHARACTER(len=55), PARAMETER :: LowerCase='abcdefghijklmnopqrstuvwxyz�����������������������������'
          CHARACTER(len=270) :: FullName=' '        ! Full name of file to open, including path
          INTEGER :: TotalSevereErrors = 0 ! Counter
          INTEGER :: TotalWarningErrors = 0 ! Counter
          DOUBLE PRECISION   :: Elapsed_Time=0.0          ! For showing elapsed time at end of run
          CHARACTER*80 :: RefName   !Refrigerant Name

!Karthik- Added variables for E+ and HP single code integration
!CHARACTER(len=255) :: ProgramPath=' '     ! Path for Program, Energy+.ini.
!CHARACTER(len=120) :: VerString='EnergyPlus, Version 1.1.1'      ! String that represents version information
          ! MODULE VARIABLES:
!REAL SigmaTest !RS: Debugging: Cavallini (2/14/14)
!REAL hfgTest    !RS: Debugging: Cavallini (2/14/14)
!REAL DTTest     !RS: Debugging: Cavallini (2/14/14)
!INTEGER, PARAMETER :: r64=KIND(1.0D0)  !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12) 
!Karthik - Assumed r64 is same throughout the module. Need to verify during testing, if this cause any errors

          ! MODULE PARAMETER DEFINITIONS:
INTEGER, PARAMETER :: BeginDay = 1
INTEGER, PARAMETER :: DuringDay = 2
INTEGER, PARAMETER :: EndDay = 3
INTEGER, PARAMETER :: EndZoneSizingCalc = 4
INTEGER, PARAMETER :: EndSysSizingCalc = 5

! Parameters for KindOfSim
INTEGER, PARAMETER :: ksDesignDay = 1
INTEGER, PARAMETER :: ksRunPeriodDesign = 2
INTEGER, PARAMETER :: ksRunPeriodWeather = 3

INTEGER, PARAMETER :: ZoneTSReporting = 1  ! value for Zone Time Step Reporting (UpdateDataAndReport)
INTEGER, PARAMETER :: HVACTSReporting = 2  ! value for HVAC Time Step Reporting (UpdateDataAndReport)

REAL(r64), PARAMETER    :: MaxEXPArg = 709.78        ! maximum exponent in EXP() function
REAL(r64), PARAMETER    :: Pi = 3.141592653589793D0   ! Pi 3.1415926535897932384626435
REAL(r64), PARAMETER    :: PiOvr2 = Pi/2.D0          ! Pi/2
REAL(r64), PARAMETER    :: DegToRadians = Pi/180.D0  ! Conversion for Degrees to Radians
REAL(r64), PARAMETER    :: SecInHour = 3600.0D0      ! Conversion for hours to seconds
REAL(r64), PARAMETER    :: HoursInDay = 24.0D0       ! Number of Hourse in Day
REAL(r64), PARAMETER    :: SecsInDay = SecInHour*HoursInDay  ! Number of seconds in Day
REAL(r64), PARAMETER    :: BigNumber=HUGE(1.0d0)     ! Max Number real used for initializations
REAL(r64), PARAMETER    :: rTinyValue=EPSILON(1.0d0) ! Tiny value to replace use of TINY(x)
INTEGER, PARAMETER :: MaxNameLength = 200     ! Maximum Name Length in Characters -- should be the same
                                              ! as MaxAlphaArgLength in InputProcessor module
                                              !RS: Changed from 100 to 200 for integration with HPSim
                                              
REAL(r64), PARAMETER    :: KelvinConv = 273.15d0     ! Conversion factor for C to K and K to C
REAL(r64), PARAMETER    :: InitConvTemp = 5.05d0     ! [deg C], standard init vol to mass flow conversion temp
REAL(r64), PARAMETER    :: AutoCalculate = -99999.d0 ! automatically calculate some fields.

REAL(r64), PARAMETER    :: StefanBoltzmann = 5.6697D-8   ! Stefan-Boltzmann constant in W/(m2*K4)

! Parameters for EMS Calling Points
INTEGER, PARAMETER :: emsCallFromZoneSizing                           = 1 ! Indentify where EMS called from
INTEGER, PARAMETER :: emsCallFromSystemSizing                         = 2 ! Indentify where EMS called from
INTEGER, PARAMETER :: emsCallFromBeginNewEvironment                   = 3 ! Indentify where EMS called from
INTEGER, PARAMETER :: emsCallFromBeginNewEvironmentAfterWarmUp        = 4 ! Indentify where EMS called from
INTEGER, PARAMETER :: emsCallFromBeginTimestepBeforePredictor         = 5 ! Indentify where EMS called from
INTEGER, PARAMETER :: emsCallFromBeforeHVACManagers                   = 6 ! Indentify where EMS called from
INTEGER, PARAMETER :: emsCallFromAfterHVACManagers                    = 7 ! Indentify where EMS called from
INTEGER, PARAMETER :: emsCallFromHVACIterationLoop                    = 8 ! Indentify where EMS called from
INTEGER, PARAMETER :: emsCallFromEndSystemTimestepBeforeHVACReporting = 9 ! Indentify where EMS called from
INTEGER, PARAMETER :: emsCallFromEndSystemTimestepAfterHVACReporting  = 10 ! Indentify where EMS called from
INTEGER, PARAMETER :: emsCallFromEndZoneTimestepBeforeZoneReporting   = 11 ! Indentify where EMS called from
INTEGER, PARAMETER :: emsCallFromEndZoneTimestepAfterZoneReporting    = 12 ! Indentify where EMS called from
INTEGER, PARAMETER :: emsCallFromSetupSimulation                      = 13 ! identify where EMS called from,
                                                                           ! this is for input processing only
INTEGER, PARAMETER :: emsCallFromExternalInterface                    = 14 ! Indentify where EMS called from
INTEGER, PARAMETER :: emsCallFromComponentGetInput                    = 15  ! EMS called from end of get input for a component
INTEGER, PARAMETER :: emsCallFromUserDefinedComponentModel            = 16  ! EMS called from inside a custom user component model

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! see DataInterfaces for global interface statements

          ! MODULE VARIABLE DECLARATIONS:

LOGICAL :: BeginDayFlag      =.false.   ! True at the start of each day, False after first time step in day
LOGICAL :: BeginEnvrnFlag    =.false.   ! True at the start of each environment, False after first time step in environ
LOGICAL :: BeginHourFlag     =.false.   ! True at the start of each hour, False after first time step in hour
LOGICAL :: BeginSimFlag      =.false.   ! True until any actual simulation (full or sizing) has begun, False after first time step
LOGICAL :: BeginFullSimFlag  =.false.   ! True until full simulation has begun, False after first time step
LOGICAL :: BeginTimeStepFlag =.false.   ! True at the start of each time step, False after first subtime step of time step
INTEGER :: DayOfSim          =0         ! Counter for days (during the simulation)
CHARACTER(len=25) :: DayOfSimChr='0'    ! Counter for days (during the simulation) (character -- for reporting)
LOGICAL :: EndEnvrnFlag      =.false.   ! True at the end of each environment (last time step of last hour of last day of environ)
LOGICAL :: EndDesignDayEnvrnsFlag = .false. ! True at the end of the last design day environment
                                ! (last time step of last hour of last day of environ which is a design day)
LOGICAL :: EndDayFlag        =.false.   ! True at the end of each day (last time step of last hour of day)
LOGICAL :: EndHourFlag       =.false.   ! True at the end of each hour (last time step of hour)
INTEGER :: PreviousHour      =0   ! Previous Hour Index
INTEGER :: HourOfDay         =0   ! Counter for hours in a simulation day
REAL(r64) :: WeightPreviousHour=0.0D0   ! Weighting of value for previous hour
REAL(r64) :: WeightNow=0.0D0            ! Weighting of value for current hour
INTEGER :: NumOfDayInEnvrn      =0 ! Number of days in the simulation for a particular environment
INTEGER :: NumOfTimeStepInHour  =0 ! Number of time steps in each hour of the simulation
INTEGER :: NumOfZones           =0 ! Total number of Zones for simulation
INTEGER :: TimeStep             =0 ! Counter for time steps (fractional hours)
REAL(r64) :: TimeStepZone=0.0d0         ! Zone time step in fractional hours
LOGICAL :: WarmupFlag           =.false. ! True during the warmup portion of a simulation
INTEGER :: OutputFileStandard   =0 ! Unit number for the standard output file (hourly data only)
INTEGER :: StdOutputRecordCount =0 ! Count of Standard output records
INTEGER :: OutputFileInits      =0 ! Unit number for the standard Initialization output file
INTEGER :: OutputFileDebug      =0 ! Unit number for debug outputs
INTEGER :: OutputFileZoneSizing =0 ! Unit number of zone sizing calc output file
INTEGER :: OutputFileSysSizing  =0 ! Unit number of system sizing calc output file
INTEGER :: OutputFileMeters     =0 ! Unit number for meters output
INTEGER :: StdMeterRecordCount  =0 ! Count of Meter output records
INTEGER :: OutputFileBNDetails  =0 ! Unit number for Branch-Node Details
LOGICAL :: ZoneSizingCalc = .FALSE.       ! TRUE if zone sizing calculation
LOGICAL :: SysSizingCalc = .FALSE.        ! TRUE if system sizing calculation
LOGICAL :: DoZoneSizing = .false. ! User input in SimulationControl object
LOGICAL :: DoSystemSizing = .false. ! User input in SimulationControl object
LOGICAL :: DoPlantSizing = .false. ! User input in SimulationControl object
LOGICAL :: DoDesDaySim = .false. ! User input in SimulationControl object
LOGICAL :: DoWeathSim = .false. ! User input in SimulationControl object
LOGICAL :: WeathSimReq = .false. ! Input has a RunPeriod request
INTEGER :: KindOfSim = 0    ! See parameters. (ksDesignDay, ksRunPeriodDesign, ksRunPeriodWeather)
LOGICAL :: DoOutputReporting=.false.    ! TRUE if variables to be written out
LOGICAL :: DoingSizing=.false.  ! TRUE when "sizing" is being performed (some error messages won't be displayed)
LOGICAL :: DoingInputProcessing=.false.  ! TRUE when "IP" is being performed (some error messages are cached)
LOGICAL :: DisplayAllWarnings=.false. ! True when selection for  "DisplayAllWarnings" is entered (turns on other warning flags)
LOGICAL :: DisplayExtraWarnings=.false. ! True when selection for  "DisplayExtraWarnings" is entered
LOGICAL :: DisplayUnusedObjects=.false. ! True when selection for  "DisplayUnusedObjects" is entered
LOGICAL :: DisplayUnusedSchedules=.false. ! True when selection for  "DisplayUnusedSchedules" is entered
LOGICAL :: DisplayAdvancedReportVariables=.false. ! True when selection for  "DisplayAdvancedReportVariables" is entered
LOGICAL :: DisplayZoneAirHeatBalanceOffBalance=.false. ! True when selection for  "DisplayZoneAirHeatBalanceOffBalance" is entered
LOGICAL :: CreateMinimalSurfaceVariables=.false. ! True when selection for  "CreateMinimalSurfaceVariables" is entered
REAL(r64) :: CurrentTime=0.0D0 ! CurrentTime, in fractional hours, from start of day. Uses Loads time step.
INTEGER :: SimTimeSteps    =0     ! Number of (Loads) timesteps since beginning of run period (environment).
INTEGER :: MinutesPerTimeStep   ! Minutes per time step calculated from NumTimeStepInHour (number of minutes per load time step)
LOGICAL :: MetersHaveBeenInitialized=.false.
LOGICAL :: KickOffSimulation=.false.  ! Kick off simulation -- meaning run each environment for 1 or 2 time steps.
LOGICAL :: KickOffSizing=.false.      ! Kick off sizing -- meaning run each environment for 1 or 2 time steps.
LOGICAL :: AnyEnergyManagementSystemInModel=.FALSE.  ! true if there is any EMS or Erl in model.  otherwise false
LOGICAL :: AnyPlantInModel = .FALSE. ! true if there are any plant or condenser loops in model, otherwise false
INTEGER :: CacheIPErrorFile    =0 ! Cache IP errors until IDF processing done.
LOGICAL :: AnyIdealCondEntSetPointInModel=.FALSE.  ! true if there is any ideal condenser entering set point manager in model. 
LOGICAL :: RunOptCondEntTemp =.FALSE. ! true if the ideal condenser entering set point optimization is running
INTEGER :: RefrigIndex = 0  !RS: Debugging: Removal of plethora of RefrigIndex definitions in the code
INTEGER :: DOASFlag=0   !RS: This tells if the system has return air or is a DOAS


!     NOTICE
!
!     Copyright � 1996-2012 The Board of Trustees of the University of Illinois
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

    END MODULE DataGlobals_HPSimIntegrated
    
     MODULE DataPrecisionGlobals

          ! Module containing the routines dealing with the precision of data in EnergyPlus

          ! MODULE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module allows for setting the default precision to "double precision" using
          ! F95 KIND and parameters.  Should it ever be necessary to try a higher precision, it
          ! will be easy to switch for testing.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
          ! na

IMPLICIT NONE ! Enforce explicit typing of all variables

PUBLIC ! This data only module is public.

          ! MODULE PARAMETER DEFINITIONS:
    INTEGER, PARAMETER :: i32=Selected_Int_Kind(6)  ! 6 digits
    INTEGER, PARAMETER :: i64=Selected_Int_Kind(12) ! 12 digits
    INTEGER, PARAMETER :: r32=KIND(1.0)
    INTEGER, PARAMETER :: r64=KIND(1.0D0)
    INTEGER, PARAMETER :: default_prec=r64
    REAL(r64), PARAMETER :: constant_zero=0.0d0
    REAL(r64), PARAMETER :: constant_one=1.0d0
    REAL(r64), PARAMETER :: constant_minusone=-1.0d0
    REAL(r64), PARAMETER :: constant_twenty=20.0d0
    REAL(r64), PARAMETER :: constant_pointfive=.5d0
    REAL(r64), PARAMETER :: EXP_LowerLimit=-20.d0  ! In IVF=2.061153622438558E-009 - used 20
                                                   ! because it's already used in other parts of the code
    REAL(r64), PARAMETER :: EXP_UpperLimit= 40.d0  ! In IVF=2.353852668370200E+017

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
          ! na

          ! SUBROUTINE SPECIFICATIONS FOR MODULE:
          ! na

!     NOTICE
!
!     Copyright � 1996-2012 The Board of Trustees of the University of Illinois
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
END MODULE DataPrecisionGlobals

