MODULE FluidProperties

        ! MODULE INFORMATION:
        !       AUTHOR         Mike Turner
        !       DATE WRITTEN   10 December 99
        !       MODIFIED       Rick Strand (April 2000, May 2000)
        !                      Simon Rees  (May, June 2002)
        !                      Rick Strand (June 2004)
        !                      Linda Lawrie (March 2008)
        !       RE-ENGINEERED  Rick Strand (April 2000, May 2000)

        ! PURPOSE OF THIS MODULE:
        ! This module contains subroutines which determine and return properties
        ! of materials including enthalpy, quality, specific heat, and density.
        ! The module uses InputProcessor to read the material type and the
        ! associated charts from IN.IDF.  The module is only as powerful as the
        ! amount of data loaded into this file.

        ! METHODOLOGY EMPLOYED:
        ! The module will first check if the current refrigerant has been read
        ! in yet.  If not, it will get the data from IN.IDF and "store" it into
        ! a set of variables.  Any future iterations with that refrigerant will
        ! simply retrieve the data from storage instead of reading from the .IDF
        ! file again.  After the data is made available, the module uses input
        ! temperatures, pressures, and either quality or enthalpy to locate the
        ! state point and choose the proper routine.  Finally, it performs a
        ! double interpolation between temperatures and pressures or qualities
        ! which surround the point on a chart specified by the input conditions.
        ! The program is designed to work on either side of or under the vapor
        ! dome.  This data can be added as needed.
        !
        ! Where properties are invalid at particular pressure/temperature points
        ! in the data input file, zeros have to be inserted. This is necessary
        ! as the data structures are rectangular. The zero values are used to detect
        ! bounds of the data and issue appropriate warnings.
        !
        ! Properties of liquids (e.g. water) can be specified as glycol properties by
        ! supplying the same data for concentrations of 0.0 and 1.0 only.
        !
        ! Temperature data has to be supplied in ascending order only.

        ! REFERENCES:

        ! USE STATEMENTS
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength, WarmupFlag, OutputFileDebug !, ShowFatalError, ShowWarningError, ShowSevereError, ShowContinueError, ShowMessage
USE DataInterfaces, ONLY:  ShowRecurringWarningErrorAtEnd, ShowRecurringSevereErrorAtEnd, ShowContinueErrorTimeStamp

IMPLICIT NONE                           ! Enforce explicit typing of all variables
PRIVATE
        ! MODULE PARAMETER DEFINITIONS
CHARACTER(len=11), PARAMETER :: Refrig       = "REFRIGERANT"
CHARACTER(len=6),  PARAMETER :: Glycol       = "GLYCOL"
CHARACTER(len=8),  PARAMETER :: Pressure     = "PRESSURE"
CHARACTER(len=8),  PARAMETER :: Enthalpy     = "ENTHALPY"
CHARACTER(len=7),  PARAMETER :: Density      = "DENSITY"
CHARACTER(len=12), PARAMETER :: SpecificHeat = "SPECIFICHEAT"
CHARACTER(len=12), PARAMETER :: Conductivity = "CONDUCTIVITY"
CHARACTER(len=9),  PARAMETER :: Viscosity    = "VISCOSITY"
CHARACTER(len=5),  PARAMETER :: Fluid        = "FLUID"
CHARACTER(len=8),  PARAMETER :: GasFluid     = "FLUIDGAS"
CHARACTER(len=5),  PARAMETER :: Water           = 'Water'
CHARACTER(len=5),  PARAMETER :: Steam           = 'Steam'
CHARACTER(len=14), PARAMETER :: EthyleneGlycol  = 'EthyleneGlycol'
CHARACTER(len=15), PARAMETER :: PropyleneGlycol = 'PropyleneGlycol'
INTEGER,           PARAMETER :: EthyleneGlycolIndex  = -2
INTEGER,           PARAMETER :: PropyleneGlycolIndex = -1
INTEGER,           PARAMETER :: iRefrig      = 1
INTEGER,           PARAMETER :: iGlycol      = 1

        ! DERIVED TYPE DEFINITIONS
TYPE FluidPropsRefrigerantData
  CHARACTER(len=MaxNameLength) :: Name = ' '    ! Name of the refrigerant
  INTEGER   :: NumPsPoints       = 0              ! Number of saturation pressure
  REAL :: PsLowTempValue  = 0.0     ! Low Temperature Value for Ps (>0.0)
  REAL :: PsHighTempValue = 0.0     ! High Temperature Value for Ps (max in tables)
  INTEGER   :: PsLowTempIndex  = 0       ! Low Temperature Min Index for Ps (>0.0)
  INTEGER   :: PsHighTempIndex = 0       ! High Temperature Max Index for Ps (>0.0)
  REAL :: PsLowPresValue  = 0.0     ! Low Pressure Value for Ps (>0.0)
  REAL :: PsHighPresValue = 0.0     ! High Pressure Value for Ps (max in tables)
  INTEGER   :: PsLowPresIndex  = 0       ! Low Pressure Min Index for Ps (>0.0)
  INTEGER   :: PsHighPresIndex = 0       ! High Pressure Max Index for Ps (>0.0)
  REAL, ALLOCATABLE, DIMENSION(:)  :: PsTemps       ! Temperatures for saturation pressures
  REAL, ALLOCATABLE, DIMENSION(:)  :: PsValues      ! Saturation pressures at PsTemps
  INTEGER   :: NumHPoints        = 0              ! Number of enthalpy points
  REAL :: HfLowTempValue  = 0.0     ! Low Temperature Value for Hf (>0.0)
  REAL :: HfHighTempValue = 0.0     ! High Temperature Value for Hf (max in tables)
  INTEGER   :: HfLowTempIndex  = 0       ! Low Temperature Min Index for Hf (>0.0)
  INTEGER   :: HfHighTempIndex = 0       ! High Temperature Max Index for Hf (>0.0)
  REAL :: HfgLowTempValue  = 0.0    ! Low Temperature Value for Hfg (>0.0)
  REAL :: HfgHighTempValue = 0.0    ! High Temperature Value for Hfg (max in tables)
  INTEGER   :: HfgLowTempIndex  = 0      ! Low Temperature Min Index for Hfg (>0.0)
  INTEGER   :: HfgHighTempIndex = 0      ! High Temperature Max Index for Hfg (>0.0)
  REAL, ALLOCATABLE, DIMENSION(:)  :: HTemps        ! Temperatures for enthalpy points
  REAL, ALLOCATABLE, DIMENSION(:)  :: HfValues      ! Enthalpy of saturated fluid at HTemps
  REAL, ALLOCATABLE, DIMENSION(:)  :: HfgValues     ! Enthalpy of saturated fluid/gas at HTemps
  INTEGER   :: NumCpPoints       = 0              ! Number of specific heat of fluid points
  REAL :: CpfLowTempValue  = 0.0     ! Low Temperature Value for Cpf (>0.0)
  REAL :: CpfHighTempValue = 0.0     ! High Temperature Value for Cpf (max in tables)
  INTEGER   :: CpfLowTempIndex  = 0       ! Low Temperature Min Index for Cpf (>0.0)
  INTEGER   :: CpfHighTempIndex = 0       ! High Temperature Max Index for Cpf (>0.0)
  REAL :: CpfgLowTempValue  = 0.0    ! Low Temperature Value for Cpfg (>0.0)
  REAL :: CpfgHighTempValue = 0.0    ! High Temperature Value for Cpfg (max in tables)
  INTEGER   :: CpfgLowTempIndex  = 0      ! Low Temperature Min Index for Cpfg (>0.0)
  INTEGER   :: CpfgHighTempIndex = 0      ! High Temperature Max Index for Cpfg (>0.0)
  REAL, ALLOCATABLE, DIMENSION(:)  :: CpTemps       ! Temperatures for specific heat points
  REAL, ALLOCATABLE, DIMENSION(:)  :: CpfValues     ! Specific heat of saturated fluid at CpTemps
  REAL, ALLOCATABLE, DIMENSION(:)  :: CpfgValues    ! Specific heat of saturated fluid/gas at CpTemps
  INTEGER   :: NumRhoPoints      = 0              ! Number of density of fluid points
  REAL :: RhofLowTempValue  = 0.0     ! Low Temperature Value for Rhof (>0.0)
  REAL :: RhofHighTempValue = 0.0     ! High Temperature Value for Rhof (max in tables)
  INTEGER   :: RhofLowTempIndex  = 0       ! Low Temperature Min Index for Rhof (>0.0)
  INTEGER   :: RhofHighTempIndex = 0       ! High Temperature Max Index for Rhof (>0.0)
  REAL :: RhofgLowTempValue  = 0.0    ! Low Temperature Value for Rhofg (>0.0)
  REAL :: RhofgHighTempValue = 0.0    ! High Temperature Value for Rhofg (max in tables)
  INTEGER   :: RhofgLowTempIndex  = 0      ! Low Temperature Min Index for Rhofg (>0.0)
  INTEGER   :: RhofgHighTempIndex = 0      ! High Temperature Max Index for Rhofg (>0.0)
  REAL, ALLOCATABLE, DIMENSION(:)  :: RhoTemps      ! Temperatures for density of fluid points
  REAL, ALLOCATABLE, DIMENSION(:)  :: RhofValues    ! Density of saturated fluid at RhoTemps
  REAL, ALLOCATABLE, DIMENSION(:)  :: RhofgValues   ! Density of saturated fluid/gas at RhoTemps
  INTEGER   :: NumSuperTempPts   = 0              ! Number of temperature points for superheated enthalpy
  INTEGER   :: NumSuperPressPts  = 0              ! Number of pressure points for superheated enthalpy
  REAL, ALLOCATABLE, DIMENSION(:)  :: SHTemps       ! Temperatures for superheated gas
  REAL, ALLOCATABLE, DIMENSION(:)  :: SHPress       ! Pressures for superheated gas
  REAL, ALLOCATABLE, DIMENSION(:,:) :: HshValues    ! Enthalpy of superheated gas at HshTemps, HshPress
  REAL, ALLOCATABLE, DIMENSION(:,:) :: RhoshValues  ! Density of superheated gas at HshTemps, HshPress
END TYPE

TYPE FluidPropsGlycolRawData
  CHARACTER(len=MaxNameLength)  :: Name = ' '  ! Name of the glycol
  LOGICAL :: CpDataPresent      = .FALSE.      ! Flag set when specific heat data is available
  INTEGER :: NumCpTempPts       = 0            ! Number of temperature points for specific heat
  INTEGER :: NumCpConcPts       = 0            ! Number of concentration points for specific heat
  REAL, ALLOCATABLE, DIMENSION(:)   :: CpTemps     ! Temperatures for specific heat of glycol
  REAL, ALLOCATABLE, DIMENSION(:)   :: CpConcs     ! Concentration for specific heat of glycol
  REAL, ALLOCATABLE, DIMENSION(:,:) :: CpValues    ! Specific heat data values
  LOGICAL :: RhoDataPresent     = .FALSE.      ! Flag set when density data is available
  INTEGER :: NumRhoTempPts      = 0            ! Number of temperature points for density
  INTEGER :: NumRhoConcPts      = 0            ! Number of concentration points for density
  REAL, ALLOCATABLE, DIMENSION(:)   :: RhoTemps    ! Temperatures for density of glycol
  REAL, ALLOCATABLE, DIMENSION(:)   :: RhoConcs    ! Concentration for density of glycol
  REAL, ALLOCATABLE, DIMENSION(:,:) :: RhoValues   ! Density data values
  LOGICAL :: CondDataPresent    = .FALSE.      ! Flag set when conductivity data is available
  INTEGER :: NumCondTempPts     = 0            ! Number of temperature points for conductivity
  INTEGER :: NumCondConcPts     = 0            ! Number of concentration points for conductivity
  REAL, ALLOCATABLE, DIMENSION(:)   :: CondTemps   ! Temperatures for conductivity of glycol
  REAL, ALLOCATABLE, DIMENSION(:)   :: CondConcs   ! Concentration for conductivity of glycol
  REAL, ALLOCATABLE, DIMENSION(:,:) :: CondValues  ! conductivity values
  LOGICAL :: ViscDataPresent    = .FALSE.      ! Flag set when viscosity data is available
  INTEGER :: NumViscTempPts     = 0            ! Number of temperature points for viscosity
  INTEGER :: NumViscConcPts     = 0            ! Number of concentration points for viscosity
  REAL, ALLOCATABLE, DIMENSION(:)   :: ViscTemps   ! Temperatures for viscosity of glycol
  REAL, ALLOCATABLE, DIMENSION(:)   :: ViscConcs   ! Concentration for viscosity of glycol
  REAL, ALLOCATABLE, DIMENSION(:,:) :: ViscValues  ! viscosity values
END TYPE

TYPE FluidPropsGlycolData
  CHARACTER(len=MaxNameLength) :: Name = ' '                ! Name of the glycol mixture (used by other parts of code)
  CHARACTER(len=MaxNameLength) :: GlycolName = ' '          ! Name of non-water fluid that is part of this mixture
                                                            ! (refers to ethylene glycol, propylene glycol, or user fluid)
  INTEGER                      :: GlycolIndex = 0           ! Index in user defined glycol data (>0 = index in raw data,
                                                            ! -1=propylene glycol, -2=ethylene glycol)
  REAL                    :: Concentration = 1.0       ! Concentration (if applicable)
  LOGICAL                      :: CpDataPresent = .FALSE.   ! Flag set when specific heat data is available
  REAL                    :: CpLowTempValue  = 0.0     ! Low Temperature Value for Cp (>0.0)
  REAL                    :: CpHighTempValue = 0.0     ! High Temperature Value for Cp (max in tables)
  INTEGER                      :: CpLowTempIndex  = 0       ! Low Temperature Min Index for Cp (>0.0)
  INTEGER                      :: CpHighTempIndex = 0       ! High Temperature Max Index for Cp (>0.0)
  INTEGER                      :: NumCpTempPts       = 0    ! Number of temperature points for specific heat
  REAL, ALLOCATABLE, DIMENSION(:)  :: CpTemps                   ! Temperatures for specific heat of glycol
  REAL, ALLOCATABLE, DIMENSION(:)  :: CpValues                  ! Specific heat data values (J/kg-K)
  LOGICAL                      :: RhoDataPresent = .FALSE.  ! Flag set when density data is available
  INTEGER                      :: NumRhoTempPts      = 0    ! Number of temperature points for density
  REAL                    :: RhoLowTempValue  = 0.0     ! Low Temperature Value for Rho (>0.0)
  REAL                    :: RhoHighTempValue = 0.0     ! High Temperature Value for Rho (max in tables)
  INTEGER                      :: RhoLowTempIndex  = 0       ! Low Temperature Min Index for Rho (>0.0)
  INTEGER                      :: RhoHighTempIndex = 0       ! High Temperature Max Index for Rho (>0.0)
  REAL, ALLOCATABLE, DIMENSION(:)  :: RhoTemps                  ! Temperatures for density of glycol
  REAL, ALLOCATABLE, DIMENSION(:)  :: RhoValues                 ! Density data values (kg/m3)
  LOGICAL                      :: CondDataPresent = .FALSE. ! Flag set when conductivity data is available
  INTEGER                      :: NumCondTempPts     = 0    ! Number of temperature points for conductivity
  REAL                    :: CondLowTempValue  = 0.0     ! Low Temperature Value for Cond (>0.0)
  REAL                    :: CondHighTempValue = 0.0     ! High Temperature Value for Cond (max in tables)
  INTEGER                      :: CondLowTempIndex  = 0       ! Low Temperature Min Index for Cond (>0.0)
  INTEGER                      :: CondHighTempIndex = 0       ! High Temperature Max Index for Cond (>0.0)
  REAL, ALLOCATABLE, DIMENSION(:)  :: CondTemps                 ! Temperatures for conductivity of glycol
  REAL, ALLOCATABLE, DIMENSION(:)  :: CondValues                ! conductivity values (W/m-K)
  LOGICAL                      :: ViscDataPresent = .FALSE. ! Flag set when viscosity data is available
  INTEGER                      :: NumViscTempPts     = 0    ! Number of temperature points for viscosity
  REAL                    :: ViscLowTempValue  = 0.0     ! Low Temperature Value for Visc (>0.0)
  REAL                    :: ViscHighTempValue = 0.0     ! High Temperature Value for Visc (max in tables)
  INTEGER                      :: ViscLowTempIndex  = 0       ! Low Temperature Min Index for Visc (>0.0)
  INTEGER                      :: ViscHighTempIndex = 0       ! High Temperature Max Index for Visc (>0.0)
  REAL, ALLOCATABLE, DIMENSION(:)  :: ViscTemps                 ! Temperatures for viscosity of glycol
  REAL, ALLOCATABLE, DIMENSION(:)  :: ViscValues                ! viscosity values (mPa-s)
END TYPE

TYPE (FluidPropsRefrigerantData), ALLOCATABLE, DIMENSION(:) :: RefrigData
TYPE (FluidPropsGlycolRawData), ALLOCATABLE, DIMENSION(:)   :: GlyRawData
TYPE (FluidPropsGlycolData), ALLOCATABLE, DIMENSION(:)      :: GlycolData

        ! INTERFACE BLOCK SPECIFICATIONS
        ! na

        ! MODULE VARIABLE DECLARATIONS
LOGICAL :: GetInput = .TRUE.     ! Used to get the input once only
INTEGER :: NumOfRefrigerants = 0 ! Total number of refrigerants input by user
INTEGER :: NumOfGlycols = 0      ! Total number of glycols input by user
LOGICAL :: DebugReportGlycols=.false.
LOGICAL :: DebugReportRefrigerants=.false.
INTEGER :: GlycolErrorLimitTest = 1    ! how many times error is printed with details before recurring called
INTEGER :: RefrigerantErrorLimitTest = 1    ! how many times error is printed with details before recurring called
LOGICAL, ALLOCATABLE, DIMENSION(:) :: RefrigUsed
LOGICAL, ALLOCATABLE, DIMENSION(:) :: GlycolUsed
INTEGER,PUBLIC :: FluidIndex_Water = 0
INTEGER,PUBLIC :: FluidIndex_EthyleneGlycol = 0
INTEGER,PUBLIC :: FluidIndex_PropoleneGlycol = 0

        ! ACCESSIBLE SPECIFICATIONS OF MODULE SUBROUTINES OR FUNCTONS:
PRIVATE GetFluidPropertiesData
PRIVATE InterpDefValuesForGlycolConc
PRIVATE InterpValuesForGlycolConc
PRIVATE InitializeGlycolTempLimits
PRIVATE InitializeRefrigerantLimits
PRIVATE ReportAndTestGlycols
PRIVATE ReportAndTestRefrigerants
PUBLIC  GetSatPressureRefrig
PUBLIC  GetSatTemperatureRefrig
PUBLIC  GetSatEnthalpyRefrig
PUBLIC  GetSatDensityRefrig
PUBLIC  GetSatSpecificHeatRefrig
PUBLIC  GetSupHeatEnthalpyRefrig
PUBLIC  GetSupHeatPressureRefrig
PUBLIC  GetSupHeatDensityRefrig
PUBLIC  GetSpecificHeatGlycol
PUBLIC  GetConductivityGlycol
PUBLIC  GetDensityGlycol
PUBLIC  GetViscosityGlycol
PRIVATE GetInterpValue
PUBLIC  GetQualityRefrig
PUBLIC  CheckFluidPropertyName
PUBLIC  ReportOrphanFluids
PUBLIC  FindRefrigerant
PUBLIC  FindGlycol
PUBLIC  GetGlycolNameByIndex
PUBLIC  FindArrayIndex
PRIVATE GetInterpolatedSatProp

CONTAINS

          ! MODULE SUBROUTINES:

SUBROUTINE GetFluidPropertiesData

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   April 2000
          !       MODIFIED       May 2002 Simon Rees (Added saturated pressure data retreaval)
          !                      June 2004 Rick Strand (Added glycol defaults and modified glycol data structure)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! The purpose of this subroutine is to read in all of the fluid
          ! property data contained in the user input file.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.  Derived type portions are
          ! allocated as necessary as the data is read into the program.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL, PARAMETER :: TempToler = 0.1    ! Some reasonable value for comparisons
  REAL, PARAMETER :: PressToler = 1.0   ! Some reasonable value for comparisons
  INTEGER, PARAMETER :: DefaultNumGlyTemps = 33 ! Temperature dimension of default glycol data
  INTEGER, PARAMETER :: DefaultNumGlyConcs = 10 ! Concentration dimension of default glycol data
  INTEGER, PARAMETER :: DefaultNumSteamTemps = 111 ! Temperature dimension of default steam data.
  INTEGER, PARAMETER :: DefaultNumSteamSuperheatedTemps = 114 ! Temperature dimension of default steam data.
  INTEGER, PARAMETER :: DefaultNumSteamSuperheatedPressure = 114 ! Temperature dimension of default steam data.

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
  TYPE FluidTempData
    CHARACTER(len=MaxNameLength) :: Name =' '      ! Name of the temperature list
    INTEGER :: NumOfTemps                =0      ! Number of temperatures in a particular arry
    REAL, ALLOCATABLE, DIMENSION(:) :: Temps ! Temperature values (degrees C)
  END TYPE

  TYPE PressureSequence
    REAL :: Pressure =0.0
    INTEGER   :: InPtr    =0
  END TYPE

  TYPE FluidData
    CHARACTER(Len=MaxNameLength) :: Name=' '
    LOGICAL ::                      IsGlycol=.false.
  END TYPE

  TYPE(FluidTempData), ALLOCATABLE, DIMENSION(:) :: FluidTemps
  TYPE(PressureSequence), ALLOCATABLE, DIMENSION(:) :: PressurePtr
  TYPE(FluidData), ALLOCATABLE, DIMENSION(:) :: FluidNames

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: Alphas ! Reads string value from input file
  CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: cAlphaFieldNames ! field names for alpha fields
  CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: cNumericFieldNames ! field names for numeric fields
  INTEGER :: Loop                    ! DO loop counter (various uses)
  INTEGER :: NumAlphas               ! States which alpha value to read from a "Number" line
  REAL, ALLOCATABLE, DIMENSION(:) :: Numbers    ! brings in data from IP
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lAlphaFieldBlanks    ! logical for blank alpha fields
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lNumericFieldBlanks  ! logical for blank numeric fields
  INTEGER :: NumNumbers              ! States which number value to read from a "Numbers" line
  INTEGER :: MaxAlphas               ! maximum number of alphas
  INTEGER :: MaxNumbers              ! maximum number of numbers
  INTEGER :: Status                  ! Either 1 "object found" or -1 "not found" (also used as temp)
  INTEGER :: InData
  INTEGER :: TempLoop
  INTEGER :: NumOfFluidTempArrays
  INTEGER :: NumOfSatFluidPropArrays
  INTEGER :: NumOfSHFluidPropArrays
  INTEGER :: NumOfGlyFluidPropArrays
  CHARACTER(len=MaxNameLength) :: TempsName
  LOGICAL :: FirstSHMatch
  INTEGER :: NumOfPressPts
  INTEGER :: NumOfConcPts
  LOGICAL :: ErrorsFound=.false.
  INTEGER :: Index
  REAL, DIMENSION(DefaultNumGlyTemps) :: DefaultGlycolTemps
  REAL, DIMENSION(DefaultNumGlyConcs) :: DefaultGlycolConcs
  REAL, DIMENSION(DefaultNumGlyTemps) :: DefaultWaterCpData
  REAL, DIMENSION(DefaultNumGlyTemps) :: DefaultWaterViscData
  REAL, DIMENSION(DefaultNumGlyTemps) :: DefaultWaterRhoData
  REAL, DIMENSION(DefaultNumGlyTemps) :: DefaultWaterCondData
  REAL, DIMENSION(DefaultNumGlyConcs,DefaultNumGlyTemps) :: DefaultEthGlyCpData
  REAL, DIMENSION(DefaultNumGlyConcs,DefaultNumGlyTemps) :: DefaultEthGlyViscData
  REAL, DIMENSION(DefaultNumGlyConcs,DefaultNumGlyTemps) :: DefaultEthGlyRhoData
  REAL, DIMENSION(DefaultNumGlyConcs,DefaultNumGlyTemps) :: DefaultEthGlyCondData
  REAL, DIMENSION(DefaultNumGlyConcs,DefaultNumGlyTemps) :: DefaultPropGlyCpData
  REAL, DIMENSION(DefaultNumGlyConcs,DefaultNumGlyTemps) :: DefaultPropGlyViscData
  REAL, DIMENSION(DefaultNumGlyConcs,DefaultNumGlyTemps) :: DefaultPropGlyRhoData
  REAL, DIMENSION(DefaultNumGlyConcs,DefaultNumGlyTemps) :: DefaultPropGlyCondData
  REAL, DIMENSION(DefaultNumSteamTemps) :: DefaultSteamTemps
  REAL, DIMENSION(DefaultNumSteamTemps) :: DefaultSteamPressData
  REAL, DIMENSION(DefaultNumSteamTemps) :: DefaultSteamEnthalpyFluidData
  REAL, DIMENSION(DefaultNumSteamTemps) :: DefaultSteamEnthalpyGasFluidData
  REAL, DIMENSION(DefaultNumSteamTemps) :: DefaultSteamCpFluidData
  REAL, DIMENSION(DefaultNumSteamTemps) :: DefaultSteamCpGasFluidData
  REAL, DIMENSION(DefaultNumSteamTemps) :: DefaultSteamDensityFluidData
  REAL, DIMENSION(DefaultNumSteamTemps) :: DefaultSteamDensityGasFluidData
  REAL, DIMENSION(DefaultNumSteamSuperheatedTemps) :: DefaultSteamSuperheatedTemps
  REAL, DIMENSION(DefaultNumSteamSuperheatedTemps) :: DefaultSteamSuperheatedPressData
  REAL, DIMENSION(DefaultNumSteamSuperheatedTemps,DefaultNumSteamSuperheatedPressure) :: DefaultSteamSuperheatedEnthalpyData
  REAL, DIMENSION(DefaultNumSteamSuperheatedTemps,DefaultNumSteamSuperheatedPressure) :: DefaultSteamSuperheatedDensityData

  INTEGER :: I
  INTEGER :: NumOfGlyConcs
  LOGICAL :: GlycolFound
  INTEGER :: NumOfOptionalInput
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming.
  REAL :: pTemp
  INTEGER :: iTemp
  INTEGER :: j
  LOGICAL ::  ErrorInName
  LOGICAL ::  IsBlank
  INTEGER :: FluidNum
  
  INTEGER :: DebugFile       =0 !RS: Debugging file denotion, hopfully this works.
    
  OPEN(unit=DebugFile,file='Debug.txt')    !RS: Debugging


          ! SUBROUTINE LOCAL DATA:
          ! For default "glycol" fluids of Water, Ethylene Glycol, and Propylene Glycol
  DATA DefaultGlycolTemps /-35.0,-30.0,-25.0,-20.0,-15.0,-10.0,-5.0,0.0,5.0,10.0,15.0,20.0,25.0,30.0, &
                            35.0,40.0,45.0,50.0,55.0,60.0,65.0,70.0,75.0,80.0,85.0, 90.0, 95.0,100.0, &
                            105.0,110.0,115.0,120.0,125.0/ ! 33 total temperature points
  DATA DefaultGlycolConcs /0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9/ ! 10 total concentration points

  DATA DefaultWaterCpData   /0.0,0.0,0.0,0.0,0.0,0.0,0.0,4217.0,4198.0,4191.0, &
                             4185.0,4181.0,4179.0,4180.0,4180.0,4180.0,4180.0,4181.0,4183.0,4185.0, &
                             4188.0,4192.0,4196.0,4200.0,4203.0,4208.0,4213.0,4218.0,4223.0,4228.0, &
                             4233.0,4238.0,4243.0/  ! in J/kg-K
  DATA DefaultWaterViscData /0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.7912, 1.5183, 1.306, &
                             1.1376, 1.0016, 0.8901, 0.7974, 0.7193, 0.653, 0.5961, 0.5468, 0.504, 0.4664, &
                             0.4332, 0.4039, 0.3777, 0.3543, 0.3333, 0.3144, 0.2973, 0.2817, 0.0, 0.0, &
                             0.0, 0.0, 0.0/ ! in mPa-s
  DATA DefaultWaterRhoData  /0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 999.8, 999.9, 999.7, &
                             999.1, 998.2, 997.0, 995.6, 994.0, 992.2, 990.2, 988.0, 985.7, 983.2, &
                             980.5, 977.7, 974.8, 971.8, 968.6, 965.3, 961.9, 958.3, 0.0, 0.0, &
                             0.0, 0.0, 0.0/ ! in kg/m3
  DATA DefaultWaterCondData /0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.561, 0.5705, 0.58, &
                             0.5893, 0.5984, 0.6072, 0.6155, 0.6233, 0.6306, 0.6373, 0.6436, 0.6492, 0.6543, &
                             0.659, 0.6631, 0.6668, 0.67, 0.6728, 0.6753, 0.6773, 0.6791, 0.0, 0.0, &
                             0.0, 0.0, 0.0/ ! in W/mK

          ! Ethylene Glycol Data: Specific Heat in J/(kg-k)
  DATA (DefaultEthGlyCpData(2,I),I=1,DefaultNumGlyTemps) &
                           /0.0,0.0,0.0,0.0,0.0,0.0,0.0,3937.0,3946.0,3954.0, &
                            3963.0,3972.0,3981.0,3989.0,3998.0,4007.0,4015.0,4024.0,4033.0,4042.0, &
                            4050.0,4059.0,4068.0,4077.0,4085.0,4094.0,4103.0,4112.0,4120.0,4129.0, &
                            4138.0,4147.0,4155.0/ ! Conc=0.1
  DATA (DefaultEthGlyCpData(3,I),I=1,DefaultNumGlyTemps) &
                           /0.0,0.0,0.0,0.0,0.0,0.0,3757.0,3769.0,3780.0,3792.0, &
                            3803.0,3815.0,3826.0,3838.0,3849.0,3861.0,3872.0,3884.0,3895.0,3907.0, &
                            3918.0,3930.0,3941.0,3953.0,3964.0,3976.0,3987.0,3999.0,4010.0,4022.0, &
                            4033.0,4045.0,4056.0/ ! Conc=0.2
  DATA (DefaultEthGlyCpData(4,I),I=1,DefaultNumGlyTemps) &
                           /0.0,0.0,0.0,0.0,0.0,3560.0,3574.0,3589.0,3603.0,3617.0, &
                            3631.0,3645.0,3660.0,3674.0,3688.0,3702.0,3716.0,3730.0,3745.0,3759.0, &
                            3773.0,3787.0,3801.0,3816.0,3830.0,3844.0,3858.0,3872.0,3886.0,3901.0, &
                            3915.0,3929.0,3943.0/ ! Conc=0.3
  DATA (DefaultEthGlyCpData(5,I),I=1,DefaultNumGlyTemps) &
                           /0.0,0.0,0.0,3334.0,3351.0,3367.0,3384.0,3401.0,3418.0,3435.0, &
                            3451.0,3468.0,3485.0,3502.0,3518.0,3535.0,3552.0,3569.0,3585.0,3602.0, &
                            3619.0,3636.0,3653.0,3669.0,3686.0,3703.0,3720.0,3736.0,3753.0,3770.0, &
                            3787.0,3804.0,3820.0/ ! Conc=0.4
  DATA (DefaultEthGlyCpData(6,I),I=1,DefaultNumGlyTemps) &
                           /3068.0,3088.0,3107.0,3126.0,3145.0,3165.0,3184.0,3203.0,3223.0,3242.0, &
                            3261.0,3281.0,3300.0,3319.0,3339.0,3358.0,3377.0,3396.0,3416.0,3435.0, &
                            3454.0,3474.0,3493.0,3512.0,3532.0,3551.0,3570.0,3590.0,3609.0,3628.0, &
                            3647.0,3667.0,3686.0/ ! Conc=0.5
  DATA (DefaultEthGlyCpData(7,I),I=1,DefaultNumGlyTemps) &
                           /2844.0,2866.0,2888.0,2909.0,2931.0,2953.0,2975.0,2997.0,3018.0,3040.0, &
                            3062.0,3084.0,3106.0,3127.0,3149.0,3171.0,3193.0,3215.0,3236.0,3258.0, &
                            3280.0,3302.0,3324.0,3345.0,3367.0,3389.0,3411.0,3433.0,3454.0,3476.0, &
                            3498.0,3520.0,3542.0/ ! Conc=0.6
  DATA (DefaultEthGlyCpData(8,I),I=1,DefaultNumGlyTemps) &
                           /2612.0,2636.0,2660.0,2685.0,2709.0,2733.0,2757.0,2782.0,2806.0,2830.0, &
                            2854.0,2878.0,2903.0,2927.0,2951.0,2975.0,3000.0,3024.0,3048.0,3072.0, &
                            3097.0,3121.0,3145.0,3169.0,3193.0,3218.0,3242.0,3266.0,3290.0,3315.0, &
                            3339.0,3363.0,3387.0/ ! Conc=0.7
  DATA (DefaultEthGlyCpData(9,I),I=1,DefaultNumGlyTemps) &
                           /2370.0,2397.0,2423.0,2450.0,2477.0,2503.0,2530.0,2556.0,2583.0,2610.0, &
                            2636.0,2663.0,2690.0,2716.0,2743.0,2770.0,2796.0,2823.0,2850.0,2876.0, &
                            2903.0,2929.0,2956.0,2983.0,3009.0,3036.0,3063.0,3089.0,3116.0,3143.0, &
                            3169.0,3196.0,3223.0/ ! Conc=0.8
  DATA (DefaultEthGlyCpData(10,I),I=1,DefaultNumGlyTemps) &
                           /0.0,0.0,2177.0,2206.0,2235.0,2264.0,2293.0,2322.0,2351.0,2380.0, &
                            2409.0,2438.0,2467.0,2496.0,2525.0,2554.0,2583.0,2612.0,2641.0,2670.0, &
                            2699.0,2728.0,2757.0,2786.0,2815.0,2844.0,2873.0,2902.0,2931.0,2960.0, &
                            2989.0,3018.0,3047.0/ ! Conc=0.9

          ! Ethylene Glycol Data: Viscosity in mPa-s
  DATA (DefaultEthGlyViscData(2,I),I=1,DefaultNumGlyTemps) &
                             /0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 2.08, 1.79, 1.56, &
                              1.37, 1.21, 1.08, 0.97, 0.88, 0.80, 0.73, 0.67, 0.62, 0.57, &
                              0.53, 0.50, 0.47, 0.44, 0.41, 0.39, 0.37, 0.35, 0.33, 0.32, &
                              0.30, 0.29, 0.28/ ! Conc=0.1
  DATA (DefaultEthGlyViscData(3,I),I=1,DefaultNumGlyTemps) &
                             /0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 3.65, 3.02, 2.54, 2.18, &
                              1.89, 1.65, 1.46, 1.30, 1.17, 1.06, 0.96, 0.88, 0.81, 0.74, &
                              0.69, 0.64, 0.59, 0.55, 0.52, 0.49, 0.46, 0.43, 0.40, 0.38, &
                              0.36, 0.34, 0.33/ ! Conc=0.2
  DATA (DefaultEthGlyViscData(4,I),I=1,DefaultNumGlyTemps) &
                             /0.00, 0.00, 0.00, 0.00, 0.00, 6.19, 5.03, 4.15, 3.48, 2.95, &
                              2.53, 2.20, 1.92, 1.69, 1.50, 1.34, 1.21, 1.09, 0.99, 0.90, &
                              0.83, 0.76, 0.70, 0.65, 0.60, 0.56, 0.52, 0.49, 0.46, 0.43, &
                              0.41, 0.38, 0.36/ ! Conc=0.3
  DATA (DefaultEthGlyViscData(5,I),I=1,DefaultNumGlyTemps) &
                             /0.00, 0.00, 0.00, 15.75, 11.74, 9.06, 7.18, 5.83, 4.82, 4.04, &
                              3.44, 2.96, 2.57, 2.26, 1.99, 1.77, 1.59, 1.43, 1.29, 1.17, &
                              1.06, 0.97, 0.89, 0.82, 0.76, 0.70, 0.65, 0.60, 0.56, 0.53, &
                              0.49, 0.46, 0.43/ ! Conc=0.4
  DATA (DefaultEthGlyViscData(6,I),I=1,DefaultNumGlyTemps) &
                             /66.93, 43.98, 30.5, 22.07, 16.53, 12.74, 10.05, 8.09, 6.63, 5.50, &
                              4.63, 3.94, 3.39, 2.94, 2.56, 2.26, 2.00, 1.78, 1.59, 1.43, &
                              1.29, 1.17, 1.07, 0.98, 0.89, 0.82, 0.76, 0.70, 0.65, 0.60, &
                              0.56, 0.53, 0.49/ ! Conc=0.5
  DATA (DefaultEthGlyViscData(7,I),I=1,DefaultNumGlyTemps) &
                             /93.44, 65.25, 46.75, 34.28, 25.69, 19.62, 15.25, 12.05, 9.66, 7.85, &
                              6.46, 5.38, 4.52, 3.84, 3.29, 2.84, 2.47, 2.16, 1.91, 1.69, &
                              1.51, 1.35, 1.22, 1.10, 1.00, 0.92, 0.84, 0.77, 0.71, 0.66, &
                              0.61, 0.57, 0.53/ ! Conc=0.6
  DATA (DefaultEthGlyViscData(8,I),I=1,DefaultNumGlyTemps) &
                             /133.53, 96.57, 70.38, 51.94, 38.88, 29.53, 22.76, 17.79, 14.09, 11.31, &
                              9.18, 7.53, 6.24, 5.23, 4.42, 3.76, 3.23, 2.80, 2.43, 2.13, &
                              1.88, 1.67, 1.49, 1.33, 1.20, 1.09, 0.99, 0.90, 0.82, 0.76, &
                              0.70, 0.64, 0.60/ ! Conc=0.7
  DATA (DefaultEthGlyViscData(9,I),I=1,DefaultNumGlyTemps) &
                             /191.09, 141.02, 102.21, 74.53, 55.09, 41.36, 31.56, 24.44, 19.2, 15.29, &
                              12.33, 10.05, 8.29, 6.90, 5.79, 4.91, 4.19, 3.61, 3.12, 2.72, &
                              2.39, 2.11, 1.87, 1.66, 1.49, 1.34, 1.21, 1.10, 1.00, 0.91, &
                              0.83, 0.77, 0.71/ ! Conc=0.8
  DATA (DefaultEthGlyViscData(10,I),I=1,DefaultNumGlyTemps) &
                             /0.00, 0.00, 196.87, 128.43, 87.52, 61.85, 45.08, 33.74, 25.84, 20.18, &
                              16.04, 12.95, 10.59, 8.77, 7.34, 6.21, 5.30, 4.56, 3.95, 3.45, &
                              3.03, 2.67, 2.37, 2.12, 1.90, 1.71, 1.54, 1.40, 1.27, 1.16, &
                              1.07, 0.98, 0.90/ ! Conc=0.9

          ! Ethylene Glycol Data: Density in kg/m3
  DATA (DefaultEthGlyRhoData(2,I),I=1,DefaultNumGlyTemps) &
                    /0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1018.73, 1017.57, 1016.28, &
                  1014.87, 1013.34, 1011.69, 1009.92, 1008.02, 1006.01, 1003.87, 1001.61, 999.23, 996.72, &
                   994.10, 991.35, 988.49, 985.50, 982.39, 979.15, 975.80, 972.32, 968.73, 965.01, &
                   961.17, 957.21, 953.12/ ! Conc=0.1
  DATA (DefaultEthGlyRhoData(3,I),I=1,DefaultNumGlyTemps) &
                    /0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1036.85, 1035.67, 1034.36, 1032.94, &
                  1031.39, 1029.72, 1027.93, 1026.02, 1023.99, 1021.83, 1019.55, 1017.16, 1014.64, 1011.99, &
                  1009.23, 1006.35, 1003.34, 1000.21, 996.96, 993.59, 990.10, 986.48, 982.75, 978.89, &
                   974.91, 970.81, 966.59/ ! Conc=0.2
  DATA (DefaultEthGlyRhoData(4,I),I=1,DefaultNumGlyTemps) &
                    /0.00, 0.00, 0.00, 0.00, 0.00, 1054.31, 1053.11, 1051.78, 1050.33, 1048.76, &
                  1047.07, 1045.25, 1043.32, 1041.26, 1039.08, 1036.78, 1034.36, 1031.36, 1029.15, 1026.36, &
                  1023.45, 1020.42, 1017.27, 1014.00, 1010.60, 1007.09, 1003.45, 999.69, 995.81, 991.81, &
                   987.68, 983.43, 979.07/ ! Conc=0.3
  DATA (DefaultEthGlyRhoData(5,I),I=1,DefaultNumGlyTemps) &
                    /0.00, 0.00, 0.00, 1071.98, 1070.87, 1069.63, 1068.28, 1066.80, 1065.21, 1063.49, &
                  1061.65, 1059.68, 1057.60, 1055.39, 1053.07, 1050.62, 1048.05, 1045.35, 1042.54, 1039.61, &
                  1036.55, 1033.37, 1030.07, 1026.65, 1023.10, 1019.44, 1015.65, 1011.74, 1007.70, 1003.56, &
                   999.29, 994.90, 990.38/ ! Conc=0.4
  DATA (DefaultEthGlyRhoData(6,I),I=1,DefaultNumGlyTemps) &
                 /1089.94, 1089.04, 1088.01, 1086.87, 1085.61, 1084.22, 1082.71, 1081.08, 1079.33, 1077.46, &
                  1075.46, 1073.35, 1071.11, 1068.75, 1066.27, 1063.66, 1060.94, 1058.09, 1055.13, 1052.04, &
                  1048.83, 1045.49, 1042.04, 1038.46, 1034.77, 1030.95, 1027.01, 1022.95, 1018.76, 1014.46, &
                  1010.03, 1005.48, 1000.81/ ! Conc=0.5
  DATA (DefaultEthGlyRhoData(7,I),I=1,DefaultNumGlyTemps) &
                 /1104.60, 1103.54, 1102.36, 1101.06, 1099.64, 1098.09, 1096.43, 1094.64, 1092.73, 1090.70, &
                  1088.54, 1086.27, 1083.87, 1081.35, 1078.71, 1075.95, 1073.07, 1070.06, 1066.94, 1063.69, &
                  1060.32, 1056.83, 1053.22, 1049.48, 1045.63, 1041.65, 1037.55, 1033.33, 1028.99, 1024.52, &
                  1019.94, 1015.23, 1010.40/ ! Conc=0.6
  DATA (DefaultEthGlyRhoData(8,I),I=1,DefaultNumGlyTemps) &
                 /1118.61, 1117.38, 1116.04, 1114.58, 1112.99, 1111.28, 1109.45, 1107.50, 1105.43, 1103.23, &
                  1100.92, 1098.48, 1095.92, 1093.24, 1090.43, 1087.51, 1084.46, 1081.30, 1078.01, 1074.60, &
                  1071.06, 1067.41, 1063.64, 1059.74, 1055.72, 1051.58, 1047.32, 1042.93, 1038.43, 1033.80, &
                  1029.05, 1024.18, 1019.19/ ! Conc=0.7
  DATA (DefaultEthGlyRhoData(9,I),I=1,DefaultNumGlyTemps) &
                 /1132.11, 1130.72, 1129.21, 1127.57, 1125.82, 1123.94, 1121.94, 1119.82, 1117.58, 1115.22, &
                  1112.73, 1110.13, 1107.40, 1104.55, 1101.58, 1098.48, 1095.27, 1091.93, 1088.48, 1084.90, &
                  1081.20, 1077.37, 1073.43, 1069.36, 1065.18, 1060.87, 1056.44, 1051.88, 1047.21, 1042.41, &
                  1037.50, 1032.46, 1027.30/ ! Conc=0.8
  DATA (DefaultEthGlyRhoData(10,I),I=1,DefaultNumGlyTemps) &
                 /0.00, 0.00, 1141.87, 1140.07, 1138.14, 1136.09, 1133.91, 1131.62, 1129.20, 1126.67, &
                  1124.01, 1121.23, 1118.32, 1115.30, 1112.15, 1108.89, 1105.50, 1101.99, 1098.36, 1094.60, &
                  1090.73, 1086.73, 1082.61, 1078.37, 1074.01, 1069.53, 1064.92, 1060.20, 1055.35, 1050.38, &
                  1045.29, 1040.08, 1034.74/ ! Conc=0.9

          ! Ethylene Glycol Data: Conductivity in W/(m-K)
  DATA (DefaultEthGlyCondData(2,I),I=1,DefaultNumGlyTemps) &
                             /0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.511, 0.520, 0.528, &
                              0.537, 0.545, 0.552, 0.559, 0.566, 0.572, 0.577, 0.583, 0.588, 0.592, &
                              0.596, 0.600, 0.603, 0.606, 0.608, 0.610, 0.612, 0.613, 0.614, 0.614, &
                              0.614, 0.613, 0.612/ ! Conc=0.1
  DATA (DefaultEthGlyCondData(3,I),I=1,DefaultNumGlyTemps) &
                             /0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.460, 0.468, 0.476, 0.483, &
                              0.490, 0.497, 0.503, 0.509, 0.515, 0.520, 0.525, 0.529, 0.534, 0.538, &
                              0.541, 0.544, 0.547, 0.549, 0.551, 0.553, 0.555, 0.556, 0.556, 0.557, &
                              0.557, 0.556, 0.555/ ! Conc=0.2
  DATA (DefaultEthGlyCondData(4,I),I=1,DefaultNumGlyTemps) &
                             /0.000, 0.000, 0.000, 0.000, 0.000, 0.415, 0.422, 0.429, 0.436, 0.442, &
                              0.448, 0.453, 0.459, 0.464, 0.469, 0.473, 0.477, 0.481, 0.485, 0.488, &
                              0.491, 0.494, 0.496, 0.498, 0.500, 0.501, 0.503, 0.504, 0.504, 0.505, &
                              0.505, 0.504, 0.504/ ! Conc=0.3
  DATA (DefaultEthGlyCondData(5,I),I=1,DefaultNumGlyTemps) &
                             /0.000, 0.000, 0.000, 0.371, 0.377, 0.383, 0.389, 0.395, 0.400, 0.405, &
                              0.410, 0.415, 0.419, 0.424, 0.428, 0.431, 0.435, 0.438, 0.441, 0.444, &
                              0.446, 0.449, 0.451, 0.452, 0.454, 0.455, 0.456, 0.457, 0.458, 0.458, &
                              0.458, 0.458, 0.458/ ! Conc=0.4
  DATA (DefaultEthGlyCondData(6,I),I=1,DefaultNumGlyTemps) &
                             /0.328, 0.333, 0.339, 0.344, 0.349, 0.354, 0.359, 0.364, 0.368, 0.373, &
                              0.377, 0.380, 0.384, 0.387, 0.391, 0.394, 0.397, 0.399, 0.402, 0.404, &
                              0.406, 0.408, 0.410, 0.411, 0.413, 0.414, 0.415, 0.416, 0.416, 0.417, &
                              0.417, 0.417, 0.417/ ! Conc=0.5
  DATA (DefaultEthGlyCondData(7,I),I=1,DefaultNumGlyTemps) &
                             /0.307, 0.312, 0.316, 0.321, 0.325, 0.329, 0.333, 0.336, 0.340, 0.343, &
                              0.346, 0.349, 0.352, 0.355, 0.358, 0.360, 0.363, 0.365, 0.367, 0.369, &
                              0.371, 0.372, 0.374, 0.375, 0.376, 0.377, 0.378, 0.379, 0.379, 0.380, &
                              0.380, 0.380, 0.380/ ! Conc=0.6
  DATA (DefaultEthGlyCondData(8,I),I=1,DefaultNumGlyTemps) &
                             /0.289, 0.293, 0.296, 0.300, 0.303, 0.306, 0.309, 0.312, 0.314, 0.317, &
                              0.320, 0.322, 0.324, 0.327, 0.329, 0.331, 0.332, 0.334, 0.336, 0.337, &
                              0.339, 0.340, 0.341, 0.342, 0.343, 0.344, 0.345, 0.346, 0.346, 0.347, &
                              0.347, 0.347, 0.347/ ! Conc=0.7
  DATA (DefaultEthGlyCondData(9,I),I=1,DefaultNumGlyTemps) &
                             /0.274, 0.276, 0.279, 0.281, 0.283, 0.286, 0.288, 0.290, 0.292, 0.294, &
                              0.296, 0.298, 0.299, 0.301, 0.303, 0.304, 0.306, 0.307, 0.308, 0.310, &
                              0.311, 0.312, 0.313, 0.314, 0.314, 0.315, 0.316, 0.316, 0.317, 0.317, &
                              0.318, 0.318, 0.318/ ! Conc=0.8
  DATA (DefaultEthGlyCondData(10,I),I=1,DefaultNumGlyTemps) &
                             /0.000, 0.000, 0.263, 0.265, 0.266, 0.268, 0.269, 0.271, 0.272, 0.274, &
                              0.275, 0.276, 0.278, 0.279, 0.280, 0.281, 0.282, 0.283, 0.284, 0.285, &
                              0.286, 0.287, 0.288, 0.288, 0.289, 0.290, 0.290, 0.291, 0.291, 0.292, &
                              0.292, 0.293, 0.293/ ! Conc=0.9

          ! Propylene Glycol Data: Specific Heat in J/(kg-k)
  DATA (DefaultPropGlyCpData(2,I),I=1,DefaultNumGlyTemps) &
                           /0.0,0.0,0.0,0.0,0.0,0.0,0.0,4042.0,4050.0,4058.0, &
                            4067.0,4075.0,4083.0,4091.0,4099.0,4107.0,4115.0,4123.0,4131.0,4139.0, &
                            4147.0,4155.0,4163.0,4171.0,4179.0,4187.0,4195.0,4203.0,4211.0,4219.0, &
                            4227.0,4235.0,4243.0/ ! Conc=0.1
  DATA (DefaultPropGlyCpData(3,I),I=1,DefaultNumGlyTemps) &
                           /0.0,0.0,0.0,0.0,0.0,0.0,3918.0,3929.0,3940.0,3951.0, &
                            3962.0,3973.0,3983.0,3994.0,4005.0,4016.0,4027.0,4038.0,4049.0,4060.0, &
                            4071.0,4082.0,4093.0,4104.0,4115.0,4126.0,4136.0,4147.0,4158.0,4169.0, &
                            4180.0,4191.0,4202.0/ ! Conc=0.2
  DATA (DefaultPropGlyCpData(4,I),I=1,DefaultNumGlyTemps) &
                           /0.0,0.0,0.0,0.0,0.0,3765.0,3779.0,3793.0,3807.0,3820.0, &
                            3834.0,3848.0,3862.0,3875.0,3889.0,3903.0,3917.0,3930.0,3944.0,3958.0, &
                            3972.0,3985.0,3999.0,4013.0,4027.0,4040.0,4054.0,4068.0,4082.0,4095.0, &
                            4109.0,4123.0,4137.0/ ! Conc=0.3
  DATA (DefaultPropGlyCpData(5,I),I=1,DefaultNumGlyTemps) &
                           /0.0,0.0,0.0,0.0,3586.0,3603.0,3619.0,3636.0,3652.0,3669.0, &
                            3685.0,3702.0,3718.0,3735.0,3751.0,3768.0,3784.0,3801.0,3817.0,3834.0, &
                            3850.0,3867.0,3883.0,3900.0,3916.0,3933.0,3949.0,3966.0,3982.0,3999.0, &
                            4015.0,4032.0,4049.0/ ! Conc=0.4
  DATA (DefaultPropGlyCpData(6,I),I=1,DefaultNumGlyTemps) &
                           /0.0,0.0,3358.0,3378.0,3397.0,3416.0,3435.0,3455.0,3474.0,3493.0, &
                            3513.0,3532.0,3551.0,3570.0,3590.0,3609.0,3628.0,3648.0,3667.0,3686.0, &
                            3706.0,3725.0,3744.0,3763.0,3783.0,3802.0,3821.0,3841.0,3860.0,3879.0, &
                            3898.0,3918.0,3937.0/ ! Conc=0.5
  DATA (DefaultPropGlyCpData(7,I),I=1,DefaultNumGlyTemps) &
                           /3096.0,3118.0,3140.0,3162.0,3184.0,3206.0,3228.0,3250.0,3272.0,3295.0, &
                            3317.0,3339.0,3361.0,3383.0,3405.0,3427.0,3449.0,3471.0,3493.0,3515.0, &
                            3537.0,3559.0,3581.0,3603.0,3625.0,3647.0,3670.0,3692.0,3714.0,3736.0, &
                            3758.0,3780.0,3802.0/ ! Conc=0.6
  DATA (DefaultPropGlyCpData(8,I),I=1,DefaultNumGlyTemps) &
                           /2843.0,2868.0,2893.0,2918.0,2943.0,2968.0,2993.0,3018.0,3042.0,3067.0, &
                            3092.0,3117.0,3142.0,3167.0,3192.0,3217.0,3242.0,3266.0,3291.0,3316.0, &
                            3341.0,3366.0,3391.0,3416.0,3441.0,3465.0,3490.0,3515.0,3540.0,3565.0, &
                            3590.0,3615.0,3640.0/ ! Conc=0.7
  DATA (DefaultPropGlyCpData(9,I),I=1,DefaultNumGlyTemps) &
                           /2572.0,2600.0,2627.0,2655.0,2683.0,2710.0,2738.0,2766.0,2793.0,2821.0, &
                            2849.0,2876.0,2904.0,2931.0,2959.0,2987.0,3014.0,3042.0,3070.0,3097.0, &
                            3125.0,3153.0,3180.0,3208.0,3236.0,3263.0,3291.0,3319.0,3346.0,3374.0, &
                            3402.0,3429.0,3457.0/ ! Conc=0.8
  DATA (DefaultPropGlyCpData(10,I),I=1,DefaultNumGlyTemps) &
                           /2264.0,2295.0,2326.0,2356.0,2387.0,2417.0,2448.0,2478.0,2509.0,2539.0, &
                            2570.0,2600.0,2631.0,2661.0,2692.0,2723.0,2753.0,2784.0,2814.0,2845.0, &
                            2875.0,2906.0,2936.0,2967.0,2997.0,3028.0,3058.0,3089.0,3119.0,3150.0, &
                            3181.0,3211.0,3242.0/ ! Conc=0.9

          ! Propylene Glycol Data: Viscosity in mPa-s
  DATA (DefaultPropGlyViscData(2,I),I=1,DefaultNumGlyTemps) &
                             /0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 2.68, 2.23, 1.89, &
                              1.63, 1.42, 1.25, 1.11, 0.99, 0.89, 0.81, 0.73, 0.67, 0.62, &
                              0.57, 0.53, 0.49, 0.46, 0.43, 0.40, 0.38, 0.35, 0.33, 0.32, &
                              0.30, 0.28, 0.27/ ! Conc=0.1
  DATA (DefaultPropGlyViscData(3,I),I=1,DefaultNumGlyTemps) &
                             /0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 4.98, 4.05, 3.34, 2.79, &
                              2.36, 2.02, 1.74, 1.52, 1.34, 1.18, 1.06, 0.95, 0.86, 0.78, &
                              0.71, 0.66, 0.60, 0.56, 0.52, 0.49, 0.45, 0.43, 0.40, 0.38, &
                              0.36, 0.34, 0.32/ ! Conc=0.2
  DATA (DefaultPropGlyViscData(4,I),I=1,DefaultNumGlyTemps) &
                             /0.00, 0.00, 0.00, 0.00, 0.00, 11.87, 9.08, 7.08, 5.61, 4.52, &
                              3.69, 3.06, 2.57, 2.18, 1.88, 1.63, 1.43, 1.26, 1.13, 1.01, &
                              0.91, 0.83, 0.76, 0.70, 0.65, 0.61, 0.57, 0.53, 0.50, 0.47, &
                              0.45, 0.43, 0.41/ ! Conc=0.3
  DATA (DefaultPropGlyViscData(5,I),I=1,DefaultNumGlyTemps) &
                             /0.00, 0.00, 0.00, 0.00, 33.22, 23.27, 16.75, 12.37, 9.35, 7.22, &
                              5.69, 4.57, 3.73, 3.09, 2.60, 2.21, 1.91, 1.66, 1.47, 1.30, &
                              1.17, 1.06, 0.96, 0.88, 0.81, 0.75, 0.70, 0.66, 0.62, 0.59, &
                              0.56, 0.53, 0.51/ ! Conc=0.4
  DATA (DefaultPropGlyViscData(6,I),I=1,DefaultNumGlyTemps) &
                             /0.00, 0.00, 110.59, 73.03, 49.7, 34.78, 24.99, 18.4, 13.85, 10.65, &
                              8.34, 6.65, 5.39, 4.43, 3.69, 3.11, 2.65, 2.29, 1.99, 1.75, &
                              1.55, 1.38, 1.24, 1.12, 1.02, 0.93, 0.86, 0.79, 0.74, 0.69, &
                              0.64, 0.6, 0.57/ ! Conc=0.5
  DATA (DefaultPropGlyViscData(7,I),I=1,DefaultNumGlyTemps) &
                             /524.01, 330.39, 211.43, 137.96, 92.00, 62.78, 43.84, 31.32, 22.87, 17.05, &
                              12.96, 10.04, 7.91, 6.34, 5.15, 4.25, 3.55, 3.00, 2.57, 2.22, &
                              1.93, 1.70, 1.51, 1.35, 1.22, 1.10, 1.01, 0.92, 0.85, 0.79, &
                              0.74, 0.69, 0.65/ ! Conc=0.6
  DATA (DefaultPropGlyViscData(8,I),I=1,DefaultNumGlyTemps) &
                             /916.18, 551.12, 340.09, 215.67, 140.62, 94.23, 64.83, 45.74, 33.04, 24.41, &
                              18.41, 14.15, 11.08, 8.81, 7.12, 5.84, 4.85, 4.08, 3.46, 2.98, &
                              2.58, 2.26, 1.99, 1.77, 1.59, 1.43, 1.30, 1.18, 1.08, 1.00, &
                              0.93, 0.86, 0.80/ ! Conc=0.7
  DATA (DefaultPropGlyViscData(9,I),I=1,DefaultNumGlyTemps) &
                             /1434.22, 908.47, 575.92, 368.77, 239.86, 159.02, 107.64, 74.45, 52.63, 37.99, &
                              28.00, 21.04, 16.10, 12.55, 9.94, 7.99, 6.52, 5.39, 4.51, 3.82, &
                              3.28, 2.83, 2.47, 2.18, 1.94, 1.73, 1.56, 1.42, 1.29, 1.19, &
                              1.09, 1.02, 0.95/ ! Conc=0.8
  DATA (DefaultPropGlyViscData(10,I),I=1,DefaultNumGlyTemps) &
                             /3813.29, 2071.34, 1176.09, 696.09, 428.19, 272.94, 179.78, 122.03, 85.15, 60.93, &
                              44.62, 33.38, 25.45, 19.76, 15.60, 12.49, 10.15, 8.35, 6.95, 5.85, &
                              4.97, 4.26, 3.69, 3.22, 2.83, 2.50, 2.23, 2.00, 1.80, 1.63, &
                              1.48, 1.35, 1.24/ ! Conc=0.9

          ! Propylene Glycol Data: Density in kg/m3
  DATA (DefaultPropGlyRhoData(2,I),I=1,DefaultNumGlyTemps) &
                  /0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1013.85, 1012.61, 1011.24, &
                   1009.75, 1008.13, 1006.40, 1004.54, 1002.56, 1000.46, 998.23, 995.88, 993.41, 990.82, &
                    988.11, 985.27, 982.31, 979.23, 976.03, 972.70, 969.25, 965.68, 961.99, 958.17, &
                    954.24, 950.18, 945.99/ ! Conc=0.1
  DATA (DefaultPropGlyRhoData(3,I),I=1,DefaultNumGlyTemps) &
                  /0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1027.24, 1025.84, 1024.32, 1022.68, &
                   1020.91, 1019.01, 1016.99, 1014.84, 1012.56, 1010.16, 1007.64, 1004.99, 1002.21, 999.31, &
                    996.28, 993.12, 989.85, 986.44, 982.91, 979.25, 975.47, 971.56, 967.53, 963.37, &
                    959.09, 954.67, 950.14/ ! Conc=0.2
  DATA (DefaultPropGlyRhoData(4,I),I=1,DefaultNumGlyTemps) &
                  /0.00, 0.00, 0.00, 0.00, 0.00, 1039.42, 1037.89, 1036.24, 1034.46, 1032.55, &
                   1030.51, 1028.35, 1026.06, 1023.64, 1021.09, 1018.42, 1015.62, 1012.69, 1009.63, 1006.44, &
                   1003.13, 999.69, 996.12, 992.42, 988.60, 984.65, 980.57, 976.36, 972.03, 967.56, &
                    962.97, 958.26, 953.41/ ! Conc=0.3
  DATA (DefaultPropGlyRhoData(5,I),I=1,DefaultNumGlyTemps) &
                  /0.00, 0.00, 0.00, 0.00, 1050.43, 1048.79, 1047.02, 1045.12, 1043.09, 1040.94, &
                   1038.65, 1036.24, 1033.70, 1031.03, 1028.23, 1025.30, 1022.24, 1019.06, 1015.75, 1012.30, &
                   1008.73, 1005.03, 1001.21, 997.25, 993.17, 988.95, 984.61, 980.14, 975.54, 970.81, &
                    965.95, 960.97, 955.86/ ! Conc=0.4
  DATA (DefaultPropGlyRhoData(6,I),I=1,DefaultNumGlyTemps) &
                  /0.00, 0.00, 1062.11, 1060.49, 1058.73, 1056.85, 1054.84, 1052.71, 1050.44, 1048.04, &
                   1045.52, 1042.87, 1040.09, 1037.18, 1034.15, 1030.98, 1027.69, 1024.27, 1020.72, 1017.04, &
                   1013.23, 1009.30, 1005.24, 1001.05, 996.73, 992.28, 987.70, 983.00, 978.16, 973.20, &
                    968.11, 962.89, 957.55/ ! Conc=0.5
  DATA (DefaultPropGlyRhoData(7,I),I=1,DefaultNumGlyTemps) &
                  /1072.92, 1071.31, 1069.58, 1067.72, 1065.73, 1063.61, 1061.37, 1059.00, 1056.50, 1053.88, &
                   1051.13, 1048.25, 1045.24, 1042.11, 1038.85, 1035.47, 1031.95, 1028.32, 1024.55, 1020.66, &
                   1016.63, 1012.49, 1008.21, 1003.81, 999.28, 994.63, 989.85, 984.94, 979.90, 974.74, &
                    969.45, 964.03, 958.49/ ! Conc=0.6
  DATA (DefaultPropGlyRhoData(8,I),I=1,DefaultNumGlyTemps) &
                  /1079.67, 1077.82, 1075.84, 1073.74, 1071.51, 1069.16, 1066.69, 1064.09, 1061.36, 1058.51, &
                   1055.54, 1052.44, 1049.22, 1045.87, 1042.40, 1038.81, 1035.09, 1031.25, 1027.28, 1023.19, &
                   1018.97, 1014.63, 1010.16, 1005.57, 1000.86, 996.02, 991.06, 985.97, 980.76, 975.42, &
                    969.96, 964.38, 958.67/ ! Conc=0.7
  DATA (DefaultPropGlyRhoData(9,I),I=1,DefaultNumGlyTemps) &
                  /1094.50, 1090.85, 1087.18, 1083.49, 1079.77, 1076.04, 1072.27, 1068.49, 1064.68, 1060.85, &
                   1057.00, 1053.12, 1049.22, 1045.30, 1041.35, 1037.38, 1033.39, 1029.37, 1025.33, 1021.27, &
                   1017.19, 1013.08, 1008.95, 1004.79, 1000.62, 996.41, 992.19, 987.94, 983.68, 979.38, &
                    975.07, 970.73, 966.37/ ! Conc=0.8
  DATA (DefaultPropGlyRhoData(10,I),I=1,DefaultNumGlyTemps) &
                  /1092.46, 1088.82, 1085.15, 1081.46, 1077.74, 1074.00, 1070.24, 1066.46, 1062.65, 1058.82, &
                   1054.96, 1051.09, 1047.19, 1043.26, 1039.32, 1035.35, 1031.35, 1027.34, 1023.30, 1019.24, &
                   1015.15, 1011.04, 1006.91, 1002.76, 998.58, 994.38, 990.16, 985.91, 981.64, 977.35, &
                    973.03, 968.69, 964.33/ ! Conc=0.9

          ! Propylene Glycol Data: Conductivity in W/(m-K)
  DATA (DefaultPropGlyCondData(2,I),I=1,DefaultNumGlyTemps) &
                             /0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.510, 0.518, 0.527, &
                              0.535, 0.543, 0.550, 0.557, 0.563, 0.569, 0.575, 0.580, 0.585, 0.589, &
                              0.593, 0.596, 0.599, 0.602, 0.604, 0.606, 0.607, 0.608, 0.609, 0.609, &
                              0.608, 0.608, 0.606/ ! Conc=0.1
  DATA (DefaultPropGlyCondData(3,I),I=1,DefaultNumGlyTemps) &
                             /0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.456, 0.464, 0.472, 0.479, &
                              0.485, 0.492, 0.498, 0.503, 0.508, 0.513, 0.518, 0.522, 0.526, 0.529, &
                              0.532, 0.535, 0.538, 0.540, 0.541, 0.543, 0.544, 0.544, 0.544, 0.544, &
                              0.544, 0.543, 0.542/ ! Conc=0.2
  DATA (DefaultPropGlyCondData(4,I),I=1,DefaultNumGlyTemps) &
                             /0.000, 0.000, 0.000, 0.000, 0.000, 0.410, 0.416, 0.423, 0.429, 0.434, &
                              0.440, 0.445, 0.449, 0.454, 0.458, 0.462, 0.466, 0.469, 0.472, 0.475, &
                              0.477, 0.479, 0.481, 0.482, 0.484, 0.484, 0.485, 0.485, 0.485, 0.485, &
                              0.485, 0.484, 0.482/ ! Conc=0.3
  DATA (DefaultPropGlyCondData(5,I),I=1,DefaultNumGlyTemps) &
                             /0.000, 0.000, 0.000, 0.000, 0.369, 0.375, 0.380, 0.385, 0.389, 0.394, &
                              0.398, 0.402, 0.406, 0.409, 0.412, 0.415, 0.418, 0.420, 0.423, 0.425, &
                              0.426, 0.428, 0.429, 0.430, 0.431, 0.431, 0.432, 0.432, 0.432, 0.431, &
                              0.430, 0.429, 0.428/ ! Conc=0.4
  DATA (DefaultPropGlyCondData(6,I),I=1,DefaultNumGlyTemps) &
                             /0.000, 0.000, 0.329, 0.334, 0.338, 0.342, 0.346, 0.349, 0.353, 0.356, &
                              0.359, 0.362, 0.365, 0.367, 0.370, 0.372, 0.374, 0.375, 0.377, 0.378, &
                              0.379, 0.380, 0.381, 0.382, 0.382, 0.382, 0.382, 0.382, 0.382, 0.381, &
                              0.380, 0.379, 0.378/ ! Conc=0.5
  DATA (DefaultPropGlyCondData(7,I),I=1,DefaultNumGlyTemps) &
                             /0.296, 0.300, 0.303, 0.306, 0.309, 0.312, 0.314, 0.317, 0.319, 0.321, &
                              0.323, 0.325, 0.327, 0.329, 0.330, 0.331, 0.333, 0.334, 0.335, 0.335, &
                              0.336, 0.336, 0.337, 0.337, 0.337, 0.337, 0.336, 0.336, 0.335, 0.335, &
                              0.334, 0.333, 0.332/ ! Conc=0.6
  DATA (DefaultPropGlyCondData(8,I),I=1,DefaultNumGlyTemps) &
                             /0.275, 0.277, 0.278, 0.280, 0.282, 0.284, 0.285, 0.286, 0.289, 0.290, &
                              0.291, 0.292, 0.293, 0.293, 0.294, 0.294, 0.295, 0.295, 0.295, 0.295, &
                              0.295, 0.295, 0.295, 0.295, 0.295, 0.294, 0.294, 0.293, 0.292, 0.292, &
                              0.291, 0.290, 0.288/ ! Conc=0.7
  DATA (DefaultPropGlyCondData(9,I),I=1,DefaultNumGlyTemps) &
                             /0.255, 0.256, 0.257, 0.257, 0.258, 0.259, 0.259, 0.259, 0.260, 0.260, &
                              0.260, 0.261, 0.261, 0.261, 0.261, 0.261, 0.260, 0.260, 0.260, 0.260, &
                              0.259, 0.259, 0.258, 0.258, 0.257, 0.256, 0.256, 0.255, 0.254, 0.253, &
                              0.252, 0.251, 0.250/ ! Conc=0.8
  DATA (DefaultPropGlyCondData(10,I),I=1,DefaultNumGlyTemps) &
                             /0.237, 0.237, 0.236, 0.236, 0.236, 0.235, 0.235, 0.234, 0.234, 0.233, &
                              0.233, 0.232, 0.233, 0.231, 0.230, 0.229, 0.229, 0.228, 0.227, 0.227, &
                              0.226, 0.225, 0.224, 0.223, 0.222, 0.221, 0.220, 0.219, 0.218, 0.217, &
                              0.216, 0.215, 0.214/ ! Conc=0.9

                  ! Steam Refrigerant Data
  DATA (DefaultSteamTemps(I),I=1,DefaultNumSteamTemps)  &
     /1.00d-002,1.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0,55.0,60.0,65.0,70.0,     &
      72.0,74.0,76.0,78.0,80.0,82.0,84.0,86.0,88.0,90.0,92.0,94.0,96.0,98.0,99.0,100.0,     &
      101.0,102.0,103.0,104.0,105.0,106.0,107.0,108.0,109.0,110.0,111.0,112.0,113.0,114.0,      &
      115.0,116.0,117.0,118.0,119.0,120.0,121.0,122.0,123.0,124.0,125.0,126.0,127.0,128.0,      &
      129.0,130.0,132.0,134.0,136.0,138.0,140.0,142.0,144.0,146.0,148.0,150.0,152.0,154.0,      &
      156.0,158.0,160.0,162.0,164.0,166.0,168.0,170.0,172.0,174.0,176.0,178.0,180.0,185.0,      &
      190.0,195.0,200.0,205.0,210.0,215.0,220.0,225.0,230.0,240.0,250.0,260.0,270.0,280.0,      &
      290.0,300.0,310.0,320.0,330.0,340.0,350.0,360.0,370.0/

  DATA (DefaultSteamPressData(I),I=1,DefaultNumSteamTemps)  &
     /611.7,657.1,872.6,1228.0,1706.0,2339.0,3170.0,4247.0,5629.0,7385.0,9595.0,12350.0,15760.0,  &
      19950.0,25040.0,31200.0,34000.0,37010.0,40240.0,43700.0,47410.0,51390.0,55640.0,60170.0,        &
      65020.0,70180.0,75680.0,81540.0,87770.0,94390.0,97850.0,101400.0,105100.0,108900.0,112800.0,    &
      116800.0,120900.0,125100.0,129500.0,134000.0,138600.0,143400.0,148300.0,153300.0,158400.0,        &
      163700.0,169200.0,174800.0,180500.0,186400.0,192500.0,198700.0,205000.0,211600.0,218300.0,        &
      225200.0,232200.0,239500.0,246900.0,254500.0,262300.0,270300.0,286800.0,304200.0,322400.0,        &
      341500.0,361500.0,382500.0,404400.0,427300.0,451200.0,476200.0,502200.0,529500.0,557800.0,        &
      587400.0,618200.0,650300.0,683700.0,718500.0,754600.0,792200.0,831200.0,871800.0,913800.0,        &
      957500.0,1003000.0,1123000.0,1255000.0,1399000.0,1555000.0,1724000.0,1908000.0,2106000.0,           &
      2320000.0,2550000.0,2797000.0,3347000.0,3976000.0,4692000.0,5503000.0,6417000.0,7442000.0,          &
      8588000.0,9865000.0,11280000.0,12860000.0,14600000.0,16530000.0,18670000.0,21040000.0/

  DATA (DefaultSteamEnthalpyFluidData(I),I=1,DefaultNumSteamTemps)  &
     /0.59,4177.0,21020.0,42020.0,62980.0,83910.0,104800.0,125700.0,146600.0,167500.0,188400.0,       &
      209300.0,230300.0,251200.0,272100.0,293100.0,301400.0,309800.0,318200.0,326600.0,335000.0,        &
      343400.0,351800.0,360200.0,368600.0,377000.0,385500.0,393900.0,402300.0,410700.0,414900.0,        &
      419200.0,423400.0,427600.0,431800.0,436000.0,440300.0,444500.0,448700.0,453000.0,457200.0,        &
      461400.0,465600.0,469900.0,474100.0,478400.0,482600.0,486800.0,491100.0,495300.0,499600.0,        &
      503800.0,508100.0,512300.0,516600.0,520800.0,525100.0,529300.0,533600.0,537900.0,542100.0,        &
      546400.0,554900.0,563500.0,572000.0,580600.0,589200.0,597700.0,606300.0,614900.0,623600.0,        &
      632200.0,640800.0,649500.0,658100.0,666800.0,675500.0,684200.0,692900.0,701600.0,710300.0,        &
      719100.0,727800.0,736600.0,745400.0,754200.0,763100.0,785200.0,807400.0,829800.0,852300.0,        &
      874900.0,897600.0,920500.0,943600.0,966800.0,990200.0,1038000.0,1086000.0,1135000.0,1185000.0,    &
      1237000.0,1290000.0,1345000.0,1402000.0,1462000.0,1526000.0,1595000.0,1671000.0,1762000.0,1891000.0/

  DATA (DefaultSteamEnthalpyGasFluidData(I),I=1,DefaultNumSteamTemps)  &
     /2501000.0,2503000.0,2510000.0,2519000.0,2528000.0,2537000.0,2547000.0,2556000.0,2565000.0,          &
      2574000.0,2582000.0,2591000.0,2600000.0,2609000.0,2618000.0,2626000.0,2630000.0,2633000.0,          &
      2636000.0,2640000.0,2643000.0,2646000.0,2650000.0,2653000.0,2656000.0,2660000.0,2663000.0,          &
      2666000.0,2669000.0,2672000.0,2674000.0,2676000.0,2677000.0,2679000.0,2680000.0,2682000.0,          &
      2683000.0,2685000.0,2686000.0,2688000.0,2690000.0,2691000.0,2693000.0,2694000.0,2696000.0,          &
      2697000.0,2699000.0,2700000.0,2702000.0,2703000.0,2704000.0,2706000.0,2707000.0,2709000.0,          &
      2710000.0,2712000.0,2713000.0,2715000.0,2716000.0,2717000.0,2719000.0,2720000.0,2723000.0,          &
      2726000.0,2728000.0,2731000.0,2733000.0,2736000.0,2739000.0,2741000.0,2744000.0,2746000.0,          &
      2748000.0,2751000.0,2753000.0,2755000.0,2757000.0,2760000.0,2762000.0,2764000.0,2766000.0,          &
      2768000.0,2770000.0,2772000.0,2774000.0,2775000.0,2777000.0,2781000.0,2785000.0,2789000.0,          &
      2792000.0,2795000.0,2797000.0,2799000.0,2801000.0,2802000.0,2803000.0,2803000.0,2801000.0,          &
      2797000.0,2790000.0,2780000.0,2767000.0,2750000.0,2728000.0,2701000.0,2666000.0,2622000.0,          &
      2564000.0,2481000.0,2335000.0/

  DATA (DefaultSteamCpFluidData(I),I=1,DefaultNumSteamTemps)  &
     /4220.0,4217.0,4205.0,4196.0,4189.0,4184.0,4182.0,4180.0,4180.0,4180.0,4180.0,4182.0,          &
      4183.0,4185.0,4187.0,4190.0,4191.0,4193.0,4194.0,4195.0,4197.0,4198.0,4200.0,4202.0,          &
      4203.0,4205.0,4207.0,4209.0,4211.0,4213.0,4215.0,4216.0,4217.0,4218.0,4219.0,4220.0,          &
      4222.0,4223.0,4224.0,4226.0,4227.0,4228.0,4230.0,4231.0,4233.0,4234.0,4236.0,4237.0,          &
      4239.0,4240.0,4242.0,4244.0,4245.0,4247.0,4249.0,4250.0,4252.0,4254.0,4256.0,4258.0,          &
      4260.0,4261.0,4265.0,4270.0,4274.0,4278.0,4283.0,4287.0,4292.0,4297.0,4302.0,4307.0,          &
      4312.0,4318.0,4324.0,4329.0,4335.0,4341.0,4348.0,4354.0,4361.0,4368.0,4375.0,4382.0,          &
      4390.0,4397.0,4405.0,4425.0,4447.0,4471.0,4496.0,4523.0,4551.0,4582.0,4615.0,4650.0,          &
      4688.0,4772.0,4870.0,4986.0,5123.0,5289.0,5493.0,5750.0,6085.0,6537.0,7186.0,8208.0,          &
      10120.0,15000.0,45160.0/

  DATA (DefaultSteamCpGasFluidData(I),I=1,DefaultNumSteamTemps)  &
     /1884.0,1885.0,1889.0,1895.0,1900.0,1906.0,1912.0,1918.0,1925.0,1931.0,1939.0,1947.0,          &
      1955.0,1965.0,1975.0,1986.0,1991.0,1996.0,2001.0,2006.0,2012.0,2018.0,2024.0,2030.0,          &
      2036.0,2043.0,2050.0,2057.0,2064.0,2072.0,2076.0,2080.0,2084.0,2088.0,2093.0,2097.0,          &
      2101.0,2106.0,2110.0,2115.0,2120.0,2124.0,2129.0,2134.0,2139.0,2144.0,2150.0,2155.0,          &
      2160.0,2166.0,2171.0,2177.0,2183.0,2189.0,2195.0,2201.0,2207.0,2213.0,2219.0,2226.0,          &
      2232.0,2239.0,2252.0,2266.0,2281.0,2296.0,2311.0,2327.0,2343.0,2359.0,2376.0,2394.0,          &
      2412.0,2430.0,2449.0,2468.0,2488.0,2509.0,2529.0,2551.0,2572.0,2594.0,2617.0,2640.0,          &
      2664.0,2688.0,2713.0,2777.0,2844.0,2915.0,2990.0,3068.0,3150.0,3237.0,3329.0,3426.0,          &
      3528.0,3754.0,4011.0,4308.0,4656.0,5073.0,5582.0,6220.0,7045.0,8159.0,9753.0,12240.0,         &
      16690.0,27360.0,96600.0/

  DATA (DefaultSteamDensityFluidData(I),I=1,DefaultNumSteamTemps)  &
     /999.8,999.9,999.9,999.7,999.1,998.2,997.0,995.6,994.0,992.2,990.2,988.0,985.7,983.2,      &
      980.5,977.7,976.6,975.4,974.2,973.0,971.8,970.5,969.2,967.9,966.6,965.3,963.9,962.6,      &
      961.2,959.8,959.1,958.3,957.6,956.9,956.2,955.4,954.7,954.0,953.2,952.5,951.7,950.9,      &
      950.2,949.4,948.6,947.9,947.1,946.3,945.5,944.7,943.9,943.1,942.3,941.5,940.7,939.8,      &
      939.0,938.2,937.4,936.5,935.7,934.8,933.1,931.4,929.7,927.9,926.1,924.3,922.5,920.7,      &
      918.9,917.0,915.1,913.2,911.3,909.4,907.4,905.5,903.5,901.5,899.5,897.5,895.4,893.3,      &
      891.2,889.1,887.0,881.6,876.1,870.4,864.7,858.8,852.7,846.5,840.2,833.7,827.1,813.4,      &
      798.9,783.6,767.5,750.3,731.9,712.1,690.7,667.1,640.8,610.7,574.7,527.6,451.4/

  DATA (DefaultSteamDensityGasFluidData(I),I=1,DefaultNumSteamTemps)  &
     /4.86d-003,5.20d-003,6.80d-003,9.41d-003,1.28d-002,1.73d-002,2.31d-002,3.04d-002,3.97d-002,5.12d-002,6.56d-002,        &
      8.32d-002,0.10,0.13,0.16,0.20,0.22,0.23,0.25,0.27,0.29,0.32,0.34,0.37,0.39,0.42,0.45,   &
      0.49,0.52,0.56,0.58,0.60,0.62,0.64,0.66,0.68,0.71,0.73,0.75,0.78,0.80,0.83,0.85,      &
      0.88,0.91,0.94,0.97,1.00,1.03,1.06,1.09,1.12,1.16,1.19,1.23,1.26,1.30,1.34,1.38,      &
      1.42,1.46,1.50,1.58,1.67,1.77,1.86,1.97,2.07,2.19,2.30,2.42,2.55,2.68,2.82,2.96,      &
      3.11,3.26,3.42,3.59,3.76,3.94,4.12,4.32,4.52,4.72,4.94,5.16,5.75,6.40,7.10,7.86,      &
      8.69,9.59,10.56,11.62,12.75,13.99,16.75,19.97,23.71,28.07,33.16,39.13,46.17,54.54,        &
      64.64,77.05,92.76,113.60,143.90,201.80/

  DATA (DefaultSteamSuperheatedTemps(i),i=1,DefaultNumSteamSuperheatedTemps) &
     /1.00d-002,1.0,5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,50.0,55.0,60.0,      &
     65.0,70.0,72.0,74.0,76.0,78.0,80.0,82.0,84.0,86.0,88.0,90.0,92.0,94.0,        &
     96.0,98.0,99.0,100.0,101.0,102.0,103.0,104.0,105.0,106.0,107.0,108.0,109.0,     &
     110.0,111.0,112.0,113.0,114.0,115.0,116.0,117.0,118.0,119.0,120.0,121.0,122.0,  &
     123.0,124.0,125.0,126.0,127.0,128.0,129.0,130.0,132.0,134.0,136.0,138.0,140.0,  &
     142.0,144.0,146.0,148.0,150.0,152.0,154.0,156.0,158.0,160.0,162.0,164.0,166.0,  &
     168.0,170.0,172.0,174.0,176.0,178.0,180.0,185.0,190.0,195.0,200.0,205.0,210.0,  &
     215.0,220.0,225.0,230.0,240.0,250.0,260.0,270.0,280.0,290.0,300.0,310.0,320.0,  &
     330.0,340.0,350.0,360.0,370.0,400.0,450.0,500.0/

  DATA (DefaultSteamSuperheatedPressData(i),i=1,DefaultNumSteamSuperheatedTemps) &
     /611.70,657.10,872.60,1228.0,1706.0,2339.0,3170.0,4247.0,5629.0,7385.0,9595.0,12350.0,    &
     15760.0,19950.0,25040.0,31200.0,34000.0,37010.0,40240.0,43700.0,47410.0,51390.0,55640.0,    &
     60170.0,65020.0,70180.0,75680.0,81540.0,87770.0,94390.0,97850.0,101400.0,105100.0,108900.0, &
     112800.0,116800.0,120900.0,125100.0,129500.0,134000.0,138600.0,143400.0,148300.0,153300.0,    &
     158400.0,163700.0,169200.0,174800.0,180500.0,186400.0,192500.0,198700.0,205000.0,211600.0,    &
     218300.0,225200.0,232200.0,239500.0,246900.0,254500.0,262300.0,270300.0,286800.0,304200.0,    &
     322400.0,341500.0,361500.0,382500.0,404400.0,427300.0,451200.0,476200.0,502200.0,529500.0,    &
     557800.0,587400.0,618200.0,650300.0,683700.0,718500.0,754600.0,792200.0,831200.0,871800.0,    &
     913800.0,957500.0,1003000.0,1123000.0,1255000.0,1399000.0,1555000.0,1724000.0,1908000.0,        &
     2106000.0,2320000.0,2550000.0,2797000.0,3347000.0,3976000.0,4692000.0,5503000.0,6417000.0,      &
     7442000.0,8588000.0,9865000.0,11280000.0,12860000.0,14600000.0,16530000.0,18670000.0,             &
     21040000.0,30000000.0,35000000.0,40000000.0/

  DATA (DefaultSteamSuperheatedEnthalpyData(i,1),i=1,DefaultNumSteamSuperheatedTemps)  &
    /2501000.0,2503000.0,2510000.0,2520000.0,2529000.0,2538000.0,2548000.0,2557000.0,2566000.0,2576000.0,  &
     2585000.0,2595000.0,2604000.0,2613000.0,2623000.0,2632000.0,2636000.0,2640000.0,2643000.0,  &
     2647000.0,2651000.0,2655000.0,2658000.0,2662000.0,2666000.0,2670000.0,2673000.0,2677000.0,  &
     2681000.0,2685000.0,2687000.0,2689000.0,2690000.0,2692000.0,2694000.0,2696000.0,2698000.0,  &
     2700000.0,2702000.0,2704000.0,2706000.0,2708000.0,2709000.0,2711000.0,2713000.0,2715000.0,  &
     2717000.0,2719000.0,2721000.0,2723000.0,2725000.0,2727000.0,2728000.0,2730000.0,2732000.0,  &
     2734000.0,2736000.0,2738000.0,2740000.0,2742000.0,2744000.0,2746000.0,2749000.0,2753000.0,  &
     2757000.0,2761000.0,2765000.0,2768000.0,2772000.0,2776000.0,2780000.0,2784000.0,2788000.0,  &
     2791000.0,2795000.0,2799000.0,2803000.0,2807000.0,2811000.0,2814000.0,2818000.0,2822000.0,  &
     2826000.0,2830000.0,2834000.0,2837000.0,2841000.0,2851000.0,2861000.0,2870000.0,2880000.0,  &
     2890000.0,2899000.0,2909000.0,2919000.0,2929000.0,2938000.0,2958000.0,2978000.0,2997000.0,  &
     3017000.0,3037000.0,3057000.0,3077000.0,3097000.0,3117000.0,3137000.0,3157000.0,3178000.0,  &
     3198000.0,3218000.0,3280000.0,3384000.0,3490000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,2),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,2503000.0,2510000.0,2520000.0,2529000.0,2538000.0,2548000.0,2557000.0,2566000.0,2576000.0,  &
     2585000.0,2595000.0,2604000.0,2613000.0,2623000.0,2632000.0,2636000.0,2640000.0,2643000.0,  &
     2647000.0,2651000.0,2655000.0,2658000.0,2662000.0,2666000.0,2670000.0,2673000.0,2677000.0,  &
     2681000.0,2685000.0,2687000.0,2689000.0,2690000.0,2692000.0,2694000.0,2696000.0,2698000.0,  &
     2700000.0,2702000.0,2704000.0,2706000.0,2708000.0,2709000.0,2711000.0,2713000.0,2715000.0,  &
     2717000.0,2719000.0,2721000.0,2723000.0,2725000.0,2727000.0,2728000.0,2730000.0,2732000.0,  &
     2734000.0,2736000.0,2738000.0,2740000.0,2742000.0,2744000.0,2746000.0,2749000.0,2753000.0,  &
     2757000.0,2761000.0,2765000.0,2768000.0,2772000.0,2776000.0,2780000.0,2784000.0,2788000.0,  &
     2791000.0,2795000.0,2799000.0,2803000.0,2807000.0,2811000.0,2814000.0,2818000.0,2822000.0,  &
     2826000.0,2830000.0,2834000.0,2837000.0,2841000.0,2851000.0,2861000.0,2870000.0,2880000.0,  &
     2890000.0,2899000.0,2909000.0,2919000.0,2929000.0,2938000.0,2958000.0,2978000.0,2997000.0,  &
     3017000.0,3037000.0,3057000.0,3077000.0,3097000.0,3117000.0,3137000.0,3157000.0,3178000.0,  &
     3198000.0,3218000.0,3280000.0,3384000.0,3490000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,3),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,2510000.0,2519000.0,2529000.0,2538000.0,2548000.0,2557000.0,2566000.0,2576000.0,2585000.0,  &
     2594000.0,2604000.0,2613000.0,2623000.0,2632000.0,2636000.0,2640000.0,2643000.0,2647000.0,  &
     2651000.0,2655000.0,2658000.0,2662000.0,2666000.0,2670000.0,2673000.0,2677000.0,2681000.0,  &
     2685000.0,2687000.0,2689000.0,2690000.0,2692000.0,2694000.0,2696000.0,2698000.0,2700000.0,  &
     2702000.0,2704000.0,2706000.0,2708000.0,2709000.0,2711000.0,2713000.0,2715000.0,2717000.0,  &
     2719000.0,2721000.0,2723000.0,2725000.0,2726000.0,2728000.0,2730000.0,2732000.0,2734000.0,  &
     2736000.0,2738000.0,2740000.0,2742000.0,2744000.0,2745000.0,2749000.0,2753000.0,2757000.0,  &
     2761000.0,2765000.0,2768000.0,2772000.0,2776000.0,2780000.0,2784000.0,2788000.0,2791000.0,  &
     2795000.0,2799000.0,2803000.0,2807000.0,2811000.0,2814000.0,2818000.0,2822000.0,2826000.0,  &
     2830000.0,2834000.0,2837000.0,2841000.0,2851000.0,2861000.0,2870000.0,2880000.0,2890000.0,  &
     2899000.0,2909000.0,2919000.0,2929000.0,2938000.0,2958000.0,2978000.0,2997000.0,3017000.0,  &
     3037000.0,3057000.0,3077000.0,3097000.0,3117000.0,3137000.0,3157000.0,3178000.0,3198000.0,  &
     3218000.0,3280000.0,3384000.0,3490000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,4),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,2519000.0,2529000.0,2538000.0,2547000.0,2557000.0,2566000.0,2576000.0,2585000.0,  &
     2594000.0,2604000.0,2613000.0,2623000.0,2632000.0,2636000.0,2639000.0,2643000.0,2647000.0,  &
     2651000.0,2655000.0,2658000.0,2662000.0,2666000.0,2670000.0,2673000.0,2677000.0,2681000.0,  &
     2685000.0,2687000.0,2689000.0,2690000.0,2692000.0,2694000.0,2696000.0,2698000.0,2700000.0,  &
     2702000.0,2704000.0,2706000.0,2707000.0,2709000.0,2711000.0,2713000.0,2715000.0,2717000.0,  &
     2719000.0,2721000.0,2723000.0,2725000.0,2726000.0,2728000.0,2730000.0,2732000.0,2734000.0,  &
     2736000.0,2738000.0,2740000.0,2742000.0,2744000.0,2745000.0,2749000.0,2753000.0,2757000.0,  &
     2761000.0,2765000.0,2768000.0,2772000.0,2776000.0,2780000.0,2784000.0,2787000.0,2791000.0,  &
     2795000.0,2799000.0,2803000.0,2807000.0,2810000.0,2814000.0,2818000.0,2822000.0,2826000.0,  &
     2830000.0,2834000.0,2837000.0,2841000.0,2851000.0,2861000.0,2870000.0,2880000.0,2890000.0,  &
     2899000.0,2909000.0,2919000.0,2929000.0,2938000.0,2958000.0,2978000.0,2997000.0,3017000.0,  &
     3037000.0,3057000.0,3077000.0,3097000.0,3117000.0,3137000.0,3157000.0,3178000.0,3198000.0,  &
     3218000.0,3280000.0,3384000.0,3490000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,5),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,2528000.0,2538000.0,2547000.0,2557000.0,2566000.0,2575000.0,2585000.0,2594000.0,  &
     2604000.0,2613000.0,2622000.0,2632000.0,2636000.0,2639000.0,2643000.0,2647000.0,2651000.0,  &
     2654000.0,2658000.0,2662000.0,2666000.0,2670000.0,2673000.0,2677000.0,2681000.0,2685000.0,  &
     2687000.0,2688000.0,2690000.0,2692000.0,2694000.0,2696000.0,2698000.0,2700000.0,2702000.0,  &
     2704000.0,2706000.0,2707000.0,2709000.0,2711000.0,2713000.0,2715000.0,2717000.0,2719000.0,  &
     2721000.0,2723000.0,2724000.0,2726000.0,2728000.0,2730000.0,2732000.0,2734000.0,2736000.0,  &
     2738000.0,2740000.0,2742000.0,2744000.0,2745000.0,2749000.0,2753000.0,2757000.0,2761000.0,  &
     2764000.0,2768000.0,2772000.0,2776000.0,2780000.0,2784000.0,2787000.0,2791000.0,2795000.0,  &
     2799000.0,2803000.0,2807000.0,2810000.0,2814000.0,2818000.0,2822000.0,2826000.0,2830000.0,  &
     2834000.0,2837000.0,2841000.0,2851000.0,2861000.0,2870000.0,2880000.0,2890000.0,2899000.0,  &
     2909000.0,2919000.0,2929000.0,2938000.0,2958000.0,2978000.0,2997000.0,3017000.0,3037000.0,  &
     3057000.0,3077000.0,3097000.0,3117000.0,3137000.0,3157000.0,3178000.0,3198000.0,3218000.0,  &
     3280000.0,3384000.0,3490000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,6),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,2537000.0,2547000.0,2556000.0,2566000.0,2575000.0,2585000.0,2594000.0,  &
     2603000.0,2613000.0,2622000.0,2632000.0,2635000.0,2639000.0,2643000.0,2647000.0,2651000.0,  &
     2654000.0,2658000.0,2662000.0,2666000.0,2669000.0,2673000.0,2677000.0,2681000.0,2685000.0,  &
     2687000.0,2688000.0,2690000.0,2692000.0,2694000.0,2696000.0,2698000.0,2700000.0,2702000.0,  &
     2704000.0,2705000.0,2707000.0,2709000.0,2711000.0,2713000.0,2715000.0,2717000.0,2719000.0,  &
     2721000.0,2723000.0,2724000.0,2726000.0,2728000.0,2730000.0,2732000.0,2734000.0,2736000.0,  &
     2738000.0,2740000.0,2742000.0,2743000.0,2745000.0,2749000.0,2753000.0,2757000.0,2761000.0,  &
     2764000.0,2768000.0,2772000.0,2776000.0,2780000.0,2784000.0,2787000.0,2791000.0,2795000.0,  &
     2799000.0,2803000.0,2807000.0,2810000.0,2814000.0,2818000.0,2822000.0,2826000.0,2830000.0,  &
     2834000.0,2837000.0,2841000.0,2851000.0,2861000.0,2870000.0,2880000.0,2890000.0,2899000.0,  &
     2909000.0,2919000.0,2929000.0,2938000.0,2958000.0,2978000.0,2997000.0,3017000.0,3037000.0,  &
     3057000.0,3077000.0,3097000.0,3117000.0,3137000.0,3157000.0,3178000.0,3198000.0,3218000.0,  &
     3280000.0,3384000.0,3490000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,7),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,2547000.0,2556000.0,2566000.0,2575000.0,2584000.0,2594000.0,2603000.0,  &
     2613000.0,2622000.0,2632000.0,2635000.0,2639000.0,2643000.0,2647000.0,2650000.0,2654000.0,  &
     2658000.0,2662000.0,2666000.0,2669000.0,2673000.0,2677000.0,2681000.0,2685000.0,2686000.0,  &
     2688000.0,2690000.0,2692000.0,2694000.0,2696000.0,2698000.0,2700000.0,2702000.0,2703000.0,  &
     2705000.0,2707000.0,2709000.0,2711000.0,2713000.0,2715000.0,2717000.0,2719000.0,2721000.0,  &
     2722000.0,2724000.0,2726000.0,2728000.0,2730000.0,2732000.0,2734000.0,2736000.0,2738000.0,  &
     2740000.0,2741000.0,2743000.0,2745000.0,2749000.0,2753000.0,2757000.0,2761000.0,2764000.0,  &
     2768000.0,2772000.0,2776000.0,2780000.0,2784000.0,2787000.0,2791000.0,2795000.0,2799000.0,  &
     2803000.0,2807000.0,2810000.0,2814000.0,2818000.0,2822000.0,2826000.0,2830000.0,2833000.0,  &
     2837000.0,2841000.0,2851000.0,2861000.0,2870000.0,2880000.0,2890000.0,2899000.0,2909000.0,  &
     2919000.0,2929000.0,2938000.0,2958000.0,2978000.0,2997000.0,3017000.0,3037000.0,3057000.0,  &
     3077000.0,3097000.0,3117000.0,3137000.0,3157000.0,3178000.0,3198000.0,3218000.0,3280000.0,  &
     3384000.0,3490000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,8),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,2556000.0,2565000.0,2575000.0,2584000.0,2594000.0,2603000.0,  &
     2612000.0,2622000.0,2631000.0,2635000.0,2639000.0,2643000.0,2646000.0,2650000.0,2654000.0,  &
     2658000.0,2662000.0,2665000.0,2669000.0,2673000.0,2677000.0,2681000.0,2684000.0,2686000.0,  &
     2688000.0,2690000.0,2692000.0,2694000.0,2696000.0,2698000.0,2700000.0,2701000.0,2703000.0,  &
     2705000.0,2707000.0,2709000.0,2711000.0,2713000.0,2715000.0,2717000.0,2719000.0,2720000.0,  &
     2722000.0,2724000.0,2726000.0,2728000.0,2730000.0,2732000.0,2734000.0,2736000.0,2738000.0,  &
     2739000.0,2741000.0,2743000.0,2745000.0,2749000.0,2753000.0,2757000.0,2760000.0,2764000.0,  &
     2768000.0,2772000.0,2776000.0,2780000.0,2783000.0,2787000.0,2791000.0,2795000.0,2799000.0,  &
     2803000.0,2806000.0,2810000.0,2814000.0,2818000.0,2822000.0,2826000.0,2830000.0,2833000.0,  &
     2837000.0,2841000.0,2851000.0,2860000.0,2870000.0,2880000.0,2890000.0,2899000.0,2909000.0,  &
     2919000.0,2929000.0,2938000.0,2958000.0,2978000.0,2997000.0,3017000.0,3037000.0,3057000.0,  &
     3077000.0,3097000.0,3117000.0,3137000.0,3157000.0,3178000.0,3198000.0,3218000.0,3280000.0,  &
     3384000.0,3490000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,9),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2565000.0,2574000.0,2584000.0,2593000.0,2603000.0,2612000.0,  &
     2622000.0,2631000.0,2635000.0,2639000.0,2642000.0,2646000.0,2650000.0,2654000.0,2658000.0,  &
     2661000.0,2665000.0,2669000.0,2673000.0,2677000.0,2680000.0,2684000.0,2686000.0,2688000.0,  &
     2690000.0,2692000.0,2694000.0,2696000.0,2697000.0,2699000.0,2701000.0,2703000.0,2705000.0,  &
     2707000.0,2709000.0,2711000.0,2713000.0,2715000.0,2717000.0,2718000.0,2720000.0,2722000.0,  &
     2724000.0,2726000.0,2728000.0,2730000.0,2732000.0,2734000.0,2736000.0,2737000.0,2739000.0,  &
     2741000.0,2743000.0,2745000.0,2749000.0,2753000.0,2757000.0,2760000.0,2764000.0,2768000.0,  &
     2772000.0,2776000.0,2780000.0,2783000.0,2787000.0,2791000.0,2795000.0,2799000.0,2803000.0,  &
     2806000.0,2810000.0,2814000.0,2818000.0,2822000.0,2826000.0,2829000.0,2833000.0,2837000.0,  &
     2841000.0,2851000.0,2860000.0,2870000.0,2880000.0,2890000.0,2899000.0,2909000.0,2919000.0,  &
     2929000.0,2938000.0,2958000.0,2978000.0,2997000.0,3017000.0,3037000.0,3057000.0,3077000.0,  &
     3097000.0,3117000.0,3137000.0,3157000.0,3178000.0,3198000.0,3218000.0,3280000.0,3384000.0,  &
     3490000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,10),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2574000.0,2583000.0,2593000.0,2602000.0,2612000.0,  &
     2621000.0,2631000.0,2635000.0,2638000.0,2642000.0,2646000.0,2650000.0,2654000.0,2657000.0,  &
     2661000.0,2665000.0,2669000.0,2673000.0,2676000.0,2680000.0,2684000.0,2686000.0,2688000.0,  &
     2690000.0,2692000.0,2693000.0,2695000.0,2697000.0,2699000.0,2701000.0,2703000.0,2705000.0,  &
     2707000.0,2709000.0,2711000.0,2713000.0,2714000.0,2716000.0,2718000.0,2720000.0,2722000.0,  &
     2724000.0,2726000.0,2728000.0,2730000.0,2732000.0,2733000.0,2735000.0,2737000.0,2739000.0,  &
     2741000.0,2743000.0,2745000.0,2749000.0,2753000.0,2756000.0,2760000.0,2764000.0,2768000.0,  &
     2772000.0,2776000.0,2779000.0,2783000.0,2787000.0,2791000.0,2795000.0,2799000.0,2802000.0,  &
     2806000.0,2810000.0,2814000.0,2818000.0,2822000.0,2826000.0,2829000.0,2833000.0,2837000.0,  &
     2841000.0,2851000.0,2860000.0,2870000.0,2880000.0,2889000.0,2899000.0,2909000.0,2919000.0,  &
     2928000.0,2938000.0,2958000.0,2978000.0,2997000.0,3017000.0,3037000.0,3057000.0,3077000.0,  &
     3097000.0,3117000.0,3137000.0,3157000.0,3178000.0,3198000.0,3218000.0,3280000.0,3384000.0,  &
     3490000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,11),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2582000.0,2592000.0,2602000.0,2611000.0,2621000.0,  &
     2630000.0,2634000.0,2638000.0,2642000.0,2646000.0,2649000.0,2653000.0,2657000.0,2661000.0,  &
     2665000.0,2668000.0,2672000.0,2676000.0,2680000.0,2684000.0,2686000.0,2688000.0,2689000.0,  &
     2691000.0,2693000.0,2695000.0,2697000.0,2699000.0,2701000.0,2703000.0,2705000.0,2707000.0,  &
     2708000.0,2710000.0,2712000.0,2714000.0,2716000.0,2718000.0,2720000.0,2722000.0,2724000.0,  &
     2726000.0,2728000.0,2729000.0,2731000.0,2733000.0,2735000.0,2737000.0,2739000.0,2741000.0,  &
     2743000.0,2745000.0,2749000.0,2752000.0,2756000.0,2760000.0,2764000.0,2768000.0,2772000.0,  &
     2775000.0,2779000.0,2783000.0,2787000.0,2791000.0,2795000.0,2798000.0,2802000.0,2806000.0,  &
     2810000.0,2814000.0,2818000.0,2822000.0,2825000.0,2829000.0,2833000.0,2837000.0,2841000.0,  &
     2851000.0,2860000.0,2870000.0,2880000.0,2889000.0,2899000.0,2909000.0,2919000.0,2928000.0,  &
     2938000.0,2958000.0,2977000.0,2997000.0,3017000.0,3037000.0,3057000.0,3077000.0,3097000.0,  &
     3117000.0,3137000.0,3157000.0,3178000.0,3198000.0,3218000.0,3280000.0,3384000.0,  &
     3490000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,12),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2591000.0,2601000.0,2611000.0,2620000.0,  &
     2630000.0,2634000.0,2637000.0,2641000.0,2645000.0,2649000.0,2653000.0,2657000.0,2660000.0,  &
     2664000.0,2668000.0,2672000.0,2676000.0,2680000.0,2683000.0,2685000.0,2687000.0,2689000.0,  &
     2691000.0,2693000.0,2695000.0,2697000.0,2699000.0,2701000.0,2702000.0,2704000.0,2706000.0,  &
     2708000.0,2710000.0,2712000.0,2714000.0,2716000.0,2718000.0,2720000.0,2722000.0,2723000.0,  &
     2725000.0,2727000.0,2729000.0,2731000.0,2733000.0,2735000.0,2737000.0,2739000.0,2741000.0,  &
     2743000.0,2745000.0,2748000.0,2752000.0,2756000.0,2760000.0,2764000.0,2768000.0,2771000.0,  &
     2775000.0,2779000.0,2783000.0,2787000.0,2791000.0,2794000.0,2798000.0,2802000.0,2806000.0,  &
     2810000.0,2814000.0,2818000.0,2821000.0,2825000.0,2829000.0,2833000.0,2837000.0,2841000.0,  &
     2850000.0,2860000.0,2870000.0,2879000.0,2889000.0,2899000.0,2909000.0,2918000.0,2928000.0,  &
     2938000.0,2958000.0,2977000.0,2997000.0,3017000.0,3037000.0,3057000.0,3077000.0,3097000.0,  &
     3117000.0,3137000.0,3157000.0,3178000.0,3198000.0,3218000.0,3280000.0,3384000.0,  &
     3490000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,13),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2600000.0,2610000.0,2620000.0,2629000.0,  &
     2633000.0,2637000.0,2641000.0,2645000.0,2648000.0,2652000.0,2656000.0,2660000.0,2664000.0,  &
     2668000.0,2671000.0,2675000.0,2679000.0,2683000.0,2685000.0,2687000.0,2689000.0,2691000.0,  &
     2692000.0,2694000.0,2696000.0,2698000.0,2700000.0,2702000.0,2704000.0,2706000.0,2708000.0,  &
     2710000.0,2712000.0,2714000.0,2715000.0,2717000.0,2719000.0,2721000.0,2723000.0,2725000.0,  &
     2727000.0,2729000.0,2731000.0,2733000.0,2735000.0,2737000.0,2738000.0,2740000.0,2742000.0,  &
     2744000.0,2748000.0,2752000.0,2756000.0,2760000.0,2763000.0,2767000.0,2771000.0,2775000.0,  &
     2779000.0,2783000.0,2786000.0,2790000.0,2794000.0,2798000.0,2802000.0,2806000.0,2810000.0,  &
     2813000.0,2817000.0,2821000.0,2825000.0,2829000.0,2833000.0,2837000.0,2841000.0,2850000.0,  &
     2860000.0,2870000.0,2879000.0,2889000.0,2899000.0,2909000.0,2918000.0,2928000.0,2938000.0,  &
     2958000.0,2977000.0,2997000.0,3017000.0,3037000.0,3057000.0,3077000.0,3097000.0,3117000.0,  &
     3137000.0,3157000.0,3177000.0,3198000.0,3218000.0,3280000.0,3384000.0,  &
     3490000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,14),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2609000.0,2619000.0,2628000.0,  &
     2632000.0,2636000.0,2640000.0,2644000.0,2648000.0,2652000.0,2655000.0,2659000.0,2663000.0,  &
     2667000.0,2671000.0,2675000.0,2679000.0,2682000.0,2684000.0,2686000.0,2688000.0,2690000.0,  &
     2692000.0,2694000.0,2696000.0,2698000.0,2700000.0,2702000.0,2704000.0,2705000.0,2707000.0,  &
     2709000.0,2711000.0,2713000.0,2715000.0,2717000.0,2719000.0,2721000.0,2723000.0,2725000.0,  &
     2727000.0,2728000.0,2730000.0,2732000.0,2734000.0,2736000.0,2738000.0,2740000.0,2742000.0,  &
     2744000.0,2748000.0,2752000.0,2755000.0,2759000.0,2763000.0,2767000.0,2771000.0,2775000.0,  &
     2778000.0,2782000.0,2786000.0,2790000.0,2794000.0,2798000.0,2802000.0,2805000.0,2809000.0,  &
     2813000.0,2817000.0,2821000.0,2825000.0,2829000.0,2833000.0,2836000.0,2840000.0,2850000.0,  &
     2860000.0,2869000.0,2879000.0,2889000.0,2899000.0,2908000.0,2918000.0,2928000.0,2938000.0,  &
     2957000.0,2977000.0,2997000.0,3017000.0,3037000.0,3057000.0,3076000.0,3097000.0,3117000.0,  &
     3137000.0,3157000.0,3177000.0,3198000.0,3218000.0,3280000.0,3384000.0,  &
     3490000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,15),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2618000.0,2627000.0,2631000.0,  &
     2635000.0,2639000.0,2643000.0,2647000.0,2651000.0,2655000.0,2659000.0,2662000.0,2666000.0,  &
     2670000.0,2674000.0,2678000.0,2682000.0,2684000.0,2686000.0,2688000.0,2689000.0,2691000.0,  &
     2693000.0,2695000.0,2697000.0,2699000.0,2701000.0,2703000.0,2705000.0,2707000.0,2709000.0,  &
     2711000.0,2713000.0,2715000.0,2716000.0,2718000.0,2720000.0,2722000.0,2724000.0,2726000.0,  &
     2728000.0,2730000.0,2732000.0,2734000.0,2736000.0,2738000.0,2740000.0,2741000.0,2743000.0,  &
     2747000.0,2751000.0,2755000.0,2759000.0,2763000.0,2767000.0,2770000.0,2774000.0,2778000.0,  &
     2782000.0,2786000.0,2790000.0,2794000.0,2797000.0,2801000.0,2805000.0,2809000.0,2813000.0,  &
     2817000.0,2821000.0,2825000.0,2828000.0,2832000.0,2836000.0,2840000.0,2850000.0,2859000.0,  &
     2869000.0,2879000.0,2889000.0,2898000.0,2908000.0,2918000.0,2928000.0,2938000.0,2957000.0,  &
     2977000.0,2997000.0,3017000.0,3036000.0,3056000.0,3076000.0,3096000.0,3117000.0,3137000.0,  &
     3157000.0,3177000.0,3198000.0,3218000.0,3280000.0,3384000.0,  &
     3490000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,16),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2626000.0,2630000.0,  &
     2634000.0,2638000.0,2642000.0,2646000.0,2650000.0,2654000.0,2658000.0,2661000.0,2665000.0,  &
     2669000.0,2673000.0,2677000.0,2681000.0,2683000.0,2685000.0,2687000.0,2689000.0,2691000.0,  &
     2693000.0,2695000.0,2696000.0,2698000.0,2700000.0,2702000.0,2704000.0,2706000.0,2708000.0,  &
     2710000.0,2712000.0,2714000.0,2716000.0,2718000.0,2720000.0,2722000.0,2724000.0,2725000.0,  &
     2727000.0,2729000.0,2731000.0,2733000.0,2735000.0,2737000.0,2739000.0,2741000.0,2743000.0,  &
     2747000.0,2751000.0,2754000.0,2758000.0,2762000.0,2766000.0,2770000.0,2774000.0,2778000.0,  &
     2782000.0,2785000.0,2789000.0,2793000.0,2797000.0,2801000.0,2805000.0,2809000.0,2813000.0,  &
     2816000.0,2820000.0,2824000.0,2828000.0,2832000.0,2836000.0,2840000.0,2849000.0,2859000.0,  &
     2869000.0,2879000.0,2888000.0,2898000.0,2908000.0,2918000.0,2928000.0,2937000.0,2957000.0,  &
     2977000.0,2997000.0,3016000.0,3036000.0,3056000.0,3076000.0,3096000.0,3116000.0,3137000.0,  &
     3157000.0,3177000.0,3198000.0,3218000.0,3280000.0,3384000.0,  &
     3489000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,17),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2630000.0,2633000.0,  &
     2637000.0,2641000.0,2645000.0,2649000.0,2653000.0,2657000.0,2661000.0,2665000.0,2669000.0,  &
     2673000.0,2677000.0,2681000.0,2683000.0,2684000.0,2686000.0,2688000.0,2690000.0,2692000.0,  &
     2694000.0,2696000.0,2698000.0,2700000.0,2702000.0,2704000.0,2706000.0,2708000.0,2710000.0,  &
     2712000.0,2714000.0,2716000.0,2717000.0,2719000.0,2721000.0,2723000.0,2725000.0,2727000.0,  &
     2729000.0,2731000.0,2733000.0,2735000.0,2737000.0,2739000.0,2741000.0,2743000.0,2747000.0,  &
     2750000.0,2754000.0,2758000.0,2762000.0,2766000.0,2770000.0,2774000.0,2777000.0,2781000.0,  &
     2785000.0,2789000.0,2793000.0,2797000.0,2801000.0,2805000.0,2808000.0,2812000.0,2816000.0,  &
     2820000.0,2824000.0,2828000.0,2832000.0,2836000.0,2840000.0,2849000.0,2859000.0,2869000.0,  &
     2879000.0,2888000.0,2898000.0,2908000.0,2918000.0,2927000.0,2937000.0,2957000.0,2977000.0,  &
     2996000.0,3016000.0,3036000.0,3056000.0,3076000.0,3096000.0,3116000.0,3137000.0,3157000.0,  &
     3177000.0,3197000.0,3218000.0,3280000.0,3384000.0,  &
     3489000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,18),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2633000.0,  &
     2637000.0,2641000.0,2645000.0,2649000.0,2653000.0,2657000.0,2661000.0,2665000.0,2668000.0,  &
     2672000.0,2676000.0,2680000.0,2682000.0,2684000.0,2686000.0,2688000.0,2690000.0,2692000.0,  &
     2694000.0,2696000.0,2698000.0,2700000.0,2702000.0,2704000.0,2706000.0,2707000.0,2709000.0,  &
     2711000.0,2713000.0,2715000.0,2717000.0,2719000.0,2721000.0,2723000.0,2725000.0,2727000.0,  &
     2729000.0,2731000.0,2733000.0,2735000.0,2737000.0,2738000.0,2740000.0,2742000.0,2746000.0,  &
     2750000.0,2754000.0,2758000.0,2762000.0,2766000.0,2770000.0,2773000.0,2777000.0,2781000.0,  &
     2785000.0,2789000.0,2793000.0,2797000.0,2801000.0,2804000.0,2808000.0,2812000.0,2816000.0,  &
     2820000.0,2824000.0,2828000.0,2832000.0,2835000.0,2839000.0,2849000.0,2859000.0,2869000.0,  &
     2878000.0,2888000.0,2898000.0,2908000.0,2918000.0,2927000.0,2937000.0,2957000.0,2977000.0,  &
     2996000.0,3016000.0,3036000.0,3056000.0,3076000.0,3096000.0,3116000.0,3136000.0,3157000.0,  &
     3177000.0,3197000.0,3218000.0,3280000.0,3384000.0,3489000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,19),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2636000.0,  &
     2640000.0,2644000.0,2648000.0,2652000.0,2656000.0,2660000.0,2664000.0,2668000.0,2672000.0,  &
     2676000.0,2680000.0,2682000.0,2684000.0,2686000.0,2688000.0,2690000.0,2691000.0,2693000.0,  &
     2695000.0,2697000.0,2699000.0,2701000.0,2703000.0,2705000.0,2707000.0,2709000.0,2711000.0,  &
     2713000.0,2715000.0,2717000.0,2719000.0,2721000.0,2723000.0,2725000.0,2727000.0,2728000.0,  &
     2730000.0,2732000.0,2734000.0,2736000.0,2738000.0,2740000.0,2742000.0,2746000.0,2750000.0,  &
     2754000.0,2758000.0,2762000.0,2765000.0,2769000.0,2773000.0,2777000.0,2781000.0,2785000.0,  &
     2789000.0,2793000.0,2796000.0,2800000.0,2804000.0,2808000.0,2812000.0,2816000.0,2820000.0,  &
     2824000.0,2828000.0,2831000.0,2835000.0,2839000.0,2849000.0,2859000.0,2868000.0,2878000.0,  &
     2888000.0,2898000.0,2908000.0,2917000.0,2927000.0,2937000.0,2957000.0,2976000.0,2996000.0,  &
     3016000.0,3036000.0,3056000.0,3076000.0,3096000.0,3116000.0,3136000.0,3157000.0,3177000.0,  &
     3197000.0,3218000.0,3280000.0,3384000.0,3489000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,20),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     2640000.0,2644000.0,2648000.0,2652000.0,2656000.0,2660000.0,2664000.0,2667000.0,2671000.0,  &
     2675000.0,2679000.0,2681000.0,2683000.0,2685000.0,2687000.0,2689000.0,2691000.0,2693000.0,  &
     2695000.0,2697000.0,2699000.0,2701000.0,2703000.0,2705000.0,2707000.0,2709000.0,2711000.0,  &
     2713000.0,2715000.0,2716000.0,2718000.0,2720000.0,2722000.0,2724000.0,2726000.0,2728000.0,  &
     2730000.0,2732000.0,2734000.0,2736000.0,2738000.0,2740000.0,2742000.0,2746000.0,2750000.0,  &
     2753000.0,2757000.0,2761000.0,2765000.0,2769000.0,2773000.0,2777000.0,2781000.0,2785000.0,  &
     2788000.0,2792000.0,2796000.0,2800000.0,2804000.0,2808000.0,2812000.0,2816000.0,2820000.0,  &
     2823000.0,2827000.0,2831000.0,2835000.0,2839000.0,2849000.0,2859000.0,2868000.0,2878000.0,  &
     2888000.0,2898000.0,2907000.0,2917000.0,2927000.0,2937000.0,2957000.0,2976000.0,2996000.0,  &
     3016000.0,3036000.0,3056000.0,3076000.0,3096000.0,3116000.0,3136000.0,3157000.0,3177000.0,  &
     3197000.0,3218000.0,3280000.0,3384000.0,3489000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,21),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     2643000.0,2647000.0,2651000.0,2655000.0,2659000.0,2663000.0,2667000.0,2671000.0,2675000.0,  &
     2679000.0,2681000.0,2683000.0,2685000.0,2687000.0,2689000.0,2691000.0,2693000.0,2695000.0,  &
     2697000.0,2698000.0,2700000.0,2702000.0,2704000.0,2706000.0,2708000.0,2710000.0,2712000.0,  &
     2714000.0,2716000.0,2718000.0,2720000.0,2722000.0,2724000.0,2726000.0,2728000.0,2730000.0,  &
     2732000.0,2734000.0,2736000.0,2738000.0,2740000.0,2741000.0,2745000.0,2749000.0,2753000.0,  &
     2757000.0,2761000.0,2765000.0,2769000.0,2773000.0,2777000.0,2780000.0,2784000.0,2788000.0,  &
     2792000.0,2796000.0,2800000.0,2804000.0,2808000.0,2812000.0,2815000.0,2819000.0,2823000.0,  &
     2827000.0,2831000.0,2835000.0,2839000.0,2849000.0,2858000.0,2868000.0,2878000.0,2888000.0,  &
     2897000.0,2907000.0,2917000.0,2927000.0,2937000.0,2956000.0,2976000.0,2996000.0,3016000.0,  &
     3036000.0,3056000.0,3076000.0,3096000.0,3116000.0,3136000.0,3157000.0,3177000.0,3197000.0,  &
     3218000.0,3280000.0,3384000.0,3489000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,22),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,2646000.0,2650000.0,2654000.0,2658000.0,2662000.0,2666000.0,2670000.0,2674000.0,2678000.0,  &
     2680000.0,2682000.0,2684000.0,2686000.0,2688000.0,2690000.0,2692000.0,2694000.0,2696000.0,  &
     2698000.0,2700000.0,2702000.0,2704000.0,2706000.0,2708000.0,2710000.0,2712000.0,2714000.0,  &
     2716000.0,2718000.0,2720000.0,2722000.0,2724000.0,2725000.0,2727000.0,2729000.0,2731000.0,  &
     2733000.0,2735000.0,2737000.0,2739000.0,2741000.0,2745000.0,2749000.0,2753000.0,2757000.0,  &
     2761000.0,2765000.0,2768000.0,2772000.0,2776000.0,2780000.0,2784000.0,2788000.0,2792000.0,  &
     2796000.0,2800000.0,2804000.0,2807000.0,2811000.0,2815000.0,2819000.0,2823000.0,2827000.0,  &
     2831000.0,2835000.0,2839000.0,2848000.0,2858000.0,2868000.0,2878000.0,2887000.0,2897000.0,  &
     2907000.0,2917000.0,2927000.0,2937000.0,2956000.0,2976000.0,2996000.0,3016000.0,3036000.0,  &
     3056000.0,3076000.0,3096000.0,3116000.0,3136000.0,3156000.0,3177000.0,3197000.0,3218000.0,  &
     3280000.0,3383000.0,3489000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,23),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,2650000.0,2654000.0,2658000.0,2662000.0,2666000.0,2670000.0,2674000.0,2678000.0,  &
     2680000.0,2682000.0,2684000.0,2686000.0,2688000.0,2690000.0,2692000.0,2694000.0,2696000.0,  &
     2698000.0,2699000.0,2701000.0,2703000.0,2705000.0,2707000.0,2709000.0,2711000.0,2713000.0,  &
     2715000.0,2717000.0,2719000.0,2721000.0,2723000.0,2725000.0,2727000.0,2729000.0,2731000.0,  &
     2733000.0,2735000.0,2737000.0,2739000.0,2741000.0,2745000.0,2749000.0,2752000.0,2756000.0,  &
     2760000.0,2764000.0,2768000.0,2772000.0,2776000.0,2780000.0,2784000.0,2788000.0,2792000.0,  &
     2795000.0,2799000.0,2803000.0,2807000.0,2811000.0,2815000.0,2819000.0,2823000.0,2827000.0,  &
     2831000.0,2834000.0,2838000.0,2848000.0,2858000.0,2868000.0,2878000.0,2887000.0,2897000.0,  &
     2907000.0,2917000.0,2927000.0,2936000.0,2956000.0,2976000.0,2996000.0,3016000.0,3036000.0,  &
     3056000.0,3076000.0,3096000.0,3116000.0,3136000.0,3156000.0,3177000.0,3197000.0,3218000.0,  &
     3280000.0,3383000.0,3489000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,24),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,2653000.0,2657000.0,2661000.0,2665000.0,2669000.0,2673000.0,2677000.0,2679000.0,  &
     2681000.0,2683000.0,2685000.0,2687000.0,2689000.0,2691000.0,2693000.0,2695000.0,2697000.0,  &
     2699000.0,2701000.0,2703000.0,2705000.0,2707000.0,2709000.0,2711000.0,2713000.0,2715000.0,  &
     2717000.0,2719000.0,2721000.0,2723000.0,2725000.0,2727000.0,2729000.0,2731000.0,2732000.0,  &
     2734000.0,2736000.0,2738000.0,2740000.0,2744000.0,2748000.0,2752000.0,2756000.0,2760000.0,  &
     2764000.0,2768000.0,2772000.0,2776000.0,2779000.0,2783000.0,2787000.0,2791000.0,2795000.0,  &
     2799000.0,2803000.0,2807000.0,2811000.0,2815000.0,2819000.0,2822000.0,2826000.0,2830000.0,  &
     2834000.0,2838000.0,2848000.0,2858000.0,2867000.0,2877000.0,2887000.0,2897000.0,2907000.0,  &
     2917000.0,2926000.0,2936000.0,2956000.0,2976000.0,2996000.0,3016000.0,3035000.0,3055000.0,  &
     3076000.0,3096000.0,3116000.0,3136000.0,3156000.0,3177000.0,3197000.0,3217000.0,3280000.0,  &
     3383000.0,3489000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,25),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,2656000.0,2660000.0,2664000.0,2668000.0,2672000.0,2676000.0,2678000.0,  &
     2680000.0,2682000.0,2684000.0,2686000.0,2688000.0,2690000.0,2692000.0,2694000.0,2696000.0,  &
     2698000.0,2700000.0,2702000.0,2704000.0,2706000.0,2708000.0,2710000.0,2712000.0,2714000.0,  &
     2716000.0,2718000.0,2720000.0,2722000.0,2724000.0,2726000.0,2728000.0,2730000.0,2732000.0,  &
     2734000.0,2736000.0,2738000.0,2740000.0,2744000.0,2748000.0,2752000.0,2756000.0,2760000.0,  &
     2763000.0,2767000.0,2771000.0,2775000.0,2779000.0,2783000.0,2787000.0,2791000.0,2795000.0,  &
     2799000.0,2803000.0,2807000.0,2810000.0,2814000.0,2818000.0,2822000.0,2826000.0,2830000.0,  &
     2834000.0,2838000.0,2848000.0,2857000.0,2867000.0,2877000.0,2887000.0,2897000.0,2907000.0,  &
     2916000.0,2926000.0,2936000.0,2956000.0,2976000.0,2996000.0,3015000.0,3035000.0,3055000.0,  &
     3075000.0,3095000.0,3116000.0,3136000.0,3156000.0,3176000.0,3197000.0,3217000.0,3280000.0,  &
     3383000.0,3489000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,26),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,2660000.0,2664000.0,2668000.0,2672000.0,2676000.0,2678000.0,2680000.0,  &
     2682000.0,2684000.0,2686000.0,2688000.0,2690000.0,2692000.0,2694000.0,2696000.0,2698000.0,  &
     2700000.0,2702000.0,2704000.0,2706000.0,2708000.0,2710000.0,2712000.0,2714000.0,2716000.0,  &
     2718000.0,2720000.0,2722000.0,2724000.0,2726000.0,2728000.0,2730000.0,2732000.0,2734000.0,  &
     2735000.0,2737000.0,2739000.0,2743000.0,2747000.0,2751000.0,2755000.0,2759000.0,2763000.0,  &
     2767000.0,2771000.0,2775000.0,2779000.0,2783000.0,2787000.0,2791000.0,2794000.0,2798000.0,  &
     2802000.0,2806000.0,2810000.0,2814000.0,2818000.0,2822000.0,2826000.0,2830000.0,2834000.0,  &
     2838000.0,2847000.0,2857000.0,2867000.0,2877000.0,2887000.0,2896000.0,2906000.0,2916000.0,  &
     2926000.0,2936000.0,2956000.0,2975000.0,2995000.0,3015000.0,3035000.0,3055000.0,3075000.0,  &
     3095000.0,3116000.0,3136000.0,3156000.0,3176000.0,3197000.0,3217000.0,3280000.0,3383000.0,  &
     3489000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,27),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,2663000.0,2667000.0,2671000.0,2675000.0,2677000.0,2679000.0,  &
     2681000.0,2683000.0,2685000.0,2687000.0,2689000.0,2691000.0,2693000.0,2695000.0,2697000.0,  &
     2699000.0,2701000.0,2703000.0,2705000.0,2707000.0,2709000.0,2711000.0,2713000.0,2715000.0,  &
     2717000.0,2719000.0,2721000.0,2723000.0,2725000.0,2727000.0,2729000.0,2731000.0,2733000.0,  &
     2735000.0,2737000.0,2739000.0,2743000.0,2747000.0,2751000.0,2755000.0,2759000.0,2763000.0,  &
     2767000.0,2771000.0,2774000.0,2778000.0,2782000.0,2786000.0,2790000.0,2794000.0,2798000.0,  &
     2802000.0,2806000.0,2810000.0,2814000.0,2818000.0,2822000.0,2826000.0,2829000.0,2833000.0,  &
     2837000.0,2847000.0,2857000.0,2867000.0,2877000.0,2886000.0,2896000.0,2906000.0,2916000.0,  &
     2926000.0,2936000.0,2955000.0,2975000.0,2995000.0,3015000.0,3035000.0,3055000.0,3075000.0,  &
     3095000.0,3115000.0,3136000.0,3156000.0,3176000.0,3197000.0,3217000.0,3280000.0,3383000.0,  &
     3489000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,28),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,2666000.0,2670000.0,2674000.0,2676000.0,2678000.0,2680000.0,  &
     2682000.0,2684000.0,2686000.0,2688000.0,2690000.0,2692000.0,2694000.0,2696000.0,2699000.0,  &
     2701000.0,2703000.0,2705000.0,2707000.0,2709000.0,2711000.0,2713000.0,2715000.0,2717000.0,  &
     2719000.0,2721000.0,2723000.0,2725000.0,2726000.0,2728000.0,2730000.0,2732000.0,2734000.0,  &
     2736000.0,2738000.0,2742000.0,2746000.0,2750000.0,2754000.0,2758000.0,2762000.0,2766000.0,  &
     2770000.0,2774000.0,2778000.0,2782000.0,2786000.0,2790000.0,2794000.0,2798000.0,2802000.0,  &
     2806000.0,2809000.0,2813000.0,2817000.0,2821000.0,2825000.0,2829000.0,2833000.0,2837000.0,  &
     2847000.0,2857000.0,2866000.0,2876000.0,2886000.0,2896000.0,2906000.0,2916000.0,2926000.0,  &
     2935000.0,2955000.0,2975000.0,2995000.0,3015000.0,3035000.0,3055000.0,3075000.0,3095000.0,  &
     3115000.0,3136000.0,3156000.0,3176000.0,3197000.0,3217000.0,3280000.0,3383000.0,3489000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,29),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2669000.0,2673000.0,2675000.0,2677000.0,2679000.0,  &
     2682000.0,2684000.0,2686000.0,2688000.0,2690000.0,2692000.0,2694000.0,2696000.0,2698000.0,  &
     2700000.0,2702000.0,2704000.0,2706000.0,2708000.0,2710000.0,2712000.0,2714000.0,2716000.0,  &
     2718000.0,2720000.0,2722000.0,2724000.0,2726000.0,2728000.0,2730000.0,2732000.0,2734000.0,  &
     2736000.0,2738000.0,2742000.0,2746000.0,2750000.0,2754000.0,2758000.0,2762000.0,2766000.0,  &
     2770000.0,2774000.0,2777000.0,2781000.0,2785000.0,2789000.0,2793000.0,2797000.0,2801000.0,  &
     2805000.0,2809000.0,2813000.0,2817000.0,2821000.0,2825000.0,2829000.0,2833000.0,2837000.0,  &
     2846000.0,2856000.0,2866000.0,2876000.0,2886000.0,2896000.0,2906000.0,2915000.0,2925000.0,  &
     2935000.0,2955000.0,2975000.0,2995000.0,3015000.0,3035000.0,3055000.0,3075000.0,3095000.0,  &
     3115000.0,3135000.0,3156000.0,3176000.0,3196000.0,3217000.0,3280000.0,3383000.0,3489000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,30),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2672000.0,2674000.0,2677000.0,2679000.0,2681000.0,  &
     2683000.0,2685000.0,2687000.0,2689000.0,2691000.0,2693000.0,2695000.0,2697000.0,2699000.0,  &
     2701000.0,2703000.0,2705000.0,2707000.0,2709000.0,2711000.0,2713000.0,2715000.0,2717000.0,  &
     2719000.0,2721000.0,2723000.0,2725000.0,2727000.0,2729000.0,2731000.0,2733000.0,2735000.0,  &
     2737000.0,2741000.0,2745000.0,2749000.0,2753000.0,2757000.0,2761000.0,2765000.0,2769000.0,  &
     2773000.0,2777000.0,2781000.0,2785000.0,2789000.0,2793000.0,2797000.0,2801000.0,2805000.0,  &
     2809000.0,2813000.0,2817000.0,2820000.0,2824000.0,2828000.0,2832000.0,2836000.0,2846000.0,  &
     2856000.0,2866000.0,2876000.0,2886000.0,2895000.0,2905000.0,2915000.0,2925000.0,2935000.0,  &
     2955000.0,2975000.0,2995000.0,3015000.0,3035000.0,3055000.0,3075000.0,3095000.0,3115000.0,  &
     3135000.0,3156000.0,3176000.0,3196000.0,3217000.0,3280000.0,3383000.0,3489000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,31),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2674000.0,2676000.0,2678000.0,2680000.0,  &
     2682000.0,2684000.0,2686000.0,2688000.0,2690000.0,2693000.0,2695000.0,2697000.0,2699000.0,  &
     2701000.0,2703000.0,2705000.0,2707000.0,2709000.0,2711000.0,2713000.0,2715000.0,2717000.0,  &
     2719000.0,2721000.0,2723000.0,2725000.0,2727000.0,2729000.0,2731000.0,2733000.0,2735000.0,  &
     2737000.0,2741000.0,2745000.0,2749000.0,2753000.0,2757000.0,2761000.0,2765000.0,2769000.0,  &
     2773000.0,2777000.0,2781000.0,2785000.0,2789000.0,2793000.0,2797000.0,2801000.0,2804000.0,  &
     2808000.0,2812000.0,2816000.0,2820000.0,2824000.0,2828000.0,2832000.0,2836000.0,2846000.0,  &
     2856000.0,2866000.0,2876000.0,2885000.0,2895000.0,2905000.0,2915000.0,2925000.0,2935000.0,  &
     2955000.0,2975000.0,2994000.0,3014000.0,3034000.0,3054000.0,3075000.0,3095000.0,3115000.0,  &
     3135000.0,3156000.0,3176000.0,3196000.0,3217000.0,3280000.0,3383000.0,3489000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,32),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2676000.0,2678000.0,2680000.0,2682000.0,  &
     2684000.0,2686000.0,2688000.0,2690000.0,2692000.0,2694000.0,2696000.0,2698000.0,2700000.0,  &
     2702000.0,2704000.0,2706000.0,2708000.0,2710000.0,2712000.0,2714000.0,2716000.0,2719000.0,  &
     2721000.0,2723000.0,2725000.0,2727000.0,2729000.0,2731000.0,2733000.0,2735000.0,2737000.0,  &
     2741000.0,2745000.0,2749000.0,2753000.0,2757000.0,2761000.0,2765000.0,2769000.0,2773000.0,  &
     2776000.0,2780000.0,2784000.0,2788000.0,2792000.0,2796000.0,2800000.0,2804000.0,2808000.0,  &
     2812000.0,2816000.0,2820000.0,2824000.0,2828000.0,2832000.0,2836000.0,2846000.0,2856000.0,  &
     2866000.0,2875000.0,2885000.0,2895000.0,2905000.0,2915000.0,2925000.0,2935000.0,2955000.0,  &
     2974000.0,2994000.0,3014000.0,3034000.0,3054000.0,3074000.0,3095000.0,3115000.0,3135000.0,  &
     3155000.0,3176000.0,3196000.0,3217000.0,3280000.0,3383000.0,3489000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,33),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2677000.0,2679000.0,2681000.0,  &
     2683000.0,2685000.0,2688000.0,2690000.0,2692000.0,2694000.0,2696000.0,2698000.0,2700000.0,  &
     2702000.0,2704000.0,2706000.0,2708000.0,2710000.0,2712000.0,2714000.0,2716000.0,2718000.0,  &
     2720000.0,2722000.0,2724000.0,2726000.0,2728000.0,2730000.0,2732000.0,2734000.0,2736000.0,  &
     2740000.0,2744000.0,2748000.0,2752000.0,2756000.0,2760000.0,2764000.0,2768000.0,2772000.0,  &
     2776000.0,2780000.0,2784000.0,2788000.0,2792000.0,2796000.0,2800000.0,2804000.0,2808000.0,  &
     2812000.0,2816000.0,2820000.0,2824000.0,2828000.0,2832000.0,2836000.0,2846000.0,2855000.0,  &
     2865000.0,2875000.0,2885000.0,2895000.0,2905000.0,2915000.0,2925000.0,2935000.0,2954000.0,  &
     2974000.0,2994000.0,3014000.0,3034000.0,3054000.0,3074000.0,3095000.0,3115000.0,3135000.0,  &
     3155000.0,3176000.0,3196000.0,3217000.0,3280000.0,3383000.0,3489000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,34),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2679000.0,2681000.0,2683000.0,  &
     2685000.0,2687000.0,2689000.0,2691000.0,2693000.0,2695000.0,2697000.0,2699000.0,2701000.0,  &
     2703000.0,2706000.0,2708000.0,2710000.0,2712000.0,2714000.0,2716000.0,2718000.0,2720000.0,  &
     2722000.0,2724000.0,2726000.0,2728000.0,2730000.0,2732000.0,2734000.0,2736000.0,2740000.0,  &
     2744000.0,2748000.0,2752000.0,2756000.0,2760000.0,2764000.0,2768000.0,2772000.0,2776000.0,  &
     2780000.0,2784000.0,2788000.0,2792000.0,2796000.0,2800000.0,2804000.0,2808000.0,2812000.0,  &
     2816000.0,2820000.0,2824000.0,2828000.0,2832000.0,2835000.0,2845000.0,2855000.0,2865000.0,  &
     2875000.0,2885000.0,2895000.0,2905000.0,2915000.0,2925000.0,2934000.0,2954000.0,2974000.0,  &
     2994000.0,3014000.0,3034000.0,3054000.0,3074000.0,3094000.0,3115000.0,3135000.0,3155000.0,  &
     3176000.0,3196000.0,3217000.0,3280000.0,3383000.0,3489000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,35),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2680000.0,2682000.0,  &
     2684000.0,2687000.0,2689000.0,2691000.0,2693000.0,2695000.0,2697000.0,2699000.0,2701000.0,  &
     2703000.0,2705000.0,2707000.0,2709000.0,2711000.0,2713000.0,2715000.0,2717000.0,2719000.0,  &
     2721000.0,2723000.0,2725000.0,2727000.0,2730000.0,2732000.0,2734000.0,2736000.0,2740000.0,  &
     2744000.0,2748000.0,2752000.0,2756000.0,2760000.0,2764000.0,2768000.0,2772000.0,2776000.0,  &
     2780000.0,2784000.0,2788000.0,2792000.0,2796000.0,2800000.0,2804000.0,2807000.0,2811000.0,  &
     2815000.0,2819000.0,2823000.0,2827000.0,2831000.0,2835000.0,2845000.0,2855000.0,2865000.0,  &
     2875000.0,2885000.0,2895000.0,2905000.0,2914000.0,2924000.0,2934000.0,2954000.0,2974000.0,  &
     2994000.0,3014000.0,3034000.0,3054000.0,3074000.0,3094000.0,3115000.0,3135000.0,3155000.0,  &
     3176000.0,3196000.0,3217000.0,3280000.0,3383000.0,3489000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,36),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2682000.0,2684000.0,  &
     2686000.0,2688000.0,2690000.0,2692000.0,2694000.0,2696000.0,2698000.0,2701000.0,2703000.0,  &
     2705000.0,2707000.0,2709000.0,2711000.0,2713000.0,2715000.0,2717000.0,2719000.0,2721000.0,  &
     2723000.0,2725000.0,2727000.0,2729000.0,2731000.0,2733000.0,2735000.0,2739000.0,2743000.0,  &
     2747000.0,2751000.0,2755000.0,2759000.0,2763000.0,2767000.0,2771000.0,2775000.0,2779000.0,  &
     2783000.0,2787000.0,2791000.0,2795000.0,2799000.0,2803000.0,2807000.0,2811000.0,2815000.0,  &
     2819000.0,2823000.0,2827000.0,2831000.0,2835000.0,2845000.0,2855000.0,2865000.0,2875000.0,  &
     2885000.0,2894000.0,2904000.0,2914000.0,2924000.0,2934000.0,2954000.0,2974000.0,2994000.0,  &
     3014000.0,3034000.0,3054000.0,3074000.0,3094000.0,3115000.0,3135000.0,3155000.0,3176000.0,  &
     3196000.0,3216000.0,3280000.0,3383000.0,3489000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,37),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2683000.0,  &
     2685000.0,2688000.0,2690000.0,2692000.0,2694000.0,2696000.0,2698000.0,2700000.0,2702000.0,  &
     2704000.0,2706000.0,2708000.0,2710000.0,2712000.0,2714000.0,2717000.0,2719000.0,2721000.0,  &
     2723000.0,2725000.0,2727000.0,2729000.0,2731000.0,2733000.0,2735000.0,2739000.0,2743000.0,  &
     2747000.0,2751000.0,2755000.0,2759000.0,2763000.0,2767000.0,2771000.0,2775000.0,2779000.0,  &
     2783000.0,2787000.0,2791000.0,2795000.0,2799000.0,2803000.0,2807000.0,2811000.0,2815000.0,  &
     2819000.0,2823000.0,2827000.0,2831000.0,2835000.0,2845000.0,2855000.0,2865000.0,2874000.0,  &
     2884000.0,2894000.0,2904000.0,2914000.0,2924000.0,2934000.0,2954000.0,2974000.0,2994000.0,  &
     3014000.0,3034000.0,3054000.0,3074000.0,3094000.0,3114000.0,3135000.0,3155000.0,3175000.0,  &
     3196000.0,3216000.0,3280000.0,3383000.0,3489000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,38),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2685000.0,  &
     2687000.0,2689000.0,2691000.0,2693000.0,2695000.0,2697000.0,2700000.0,2702000.0,2704000.0,  &
     2706000.0,2708000.0,2710000.0,2712000.0,2714000.0,2716000.0,2718000.0,2720000.0,2722000.0,  &
     2724000.0,2726000.0,2728000.0,2730000.0,2732000.0,2734000.0,2738000.0,2743000.0,2747000.0,  &
     2751000.0,2755000.0,2759000.0,2763000.0,2767000.0,2771000.0,2775000.0,2779000.0,2783000.0,  &
     2787000.0,2791000.0,2795000.0,2799000.0,2803000.0,2807000.0,2811000.0,2815000.0,2819000.0,  &
     2823000.0,2827000.0,2831000.0,2835000.0,2844000.0,2854000.0,2864000.0,2874000.0,2884000.0,  &
     2894000.0,2904000.0,2914000.0,2924000.0,2934000.0,2954000.0,2974000.0,2994000.0,3014000.0,  &
     3034000.0,3054000.0,3074000.0,3094000.0,3114000.0,3135000.0,3155000.0,3175000.0,3196000.0,  &
     3216000.0,3280000.0,3383000.0,3488000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,39),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     2686000.0,2689000.0,2691000.0,2693000.0,2695000.0,2697000.0,2699000.0,2701000.0,2703000.0,  &
     2705000.0,2707000.0,2709000.0,2711000.0,2714000.0,2716000.0,2718000.0,2720000.0,2722000.0,  &
     2724000.0,2726000.0,2728000.0,2730000.0,2732000.0,2734000.0,2738000.0,2742000.0,2746000.0,  &
     2750000.0,2754000.0,2758000.0,2762000.0,2766000.0,2770000.0,2774000.0,2778000.0,2782000.0,  &
     2786000.0,2790000.0,2794000.0,2798000.0,2802000.0,2806000.0,2810000.0,2814000.0,2818000.0,  &
     2822000.0,2826000.0,2830000.0,2834000.0,2844000.0,2854000.0,2864000.0,2874000.0,2884000.0,  &
     2894000.0,2904000.0,2914000.0,2924000.0,2934000.0,2954000.0,2974000.0,2994000.0,3014000.0,  &
     3034000.0,3054000.0,3074000.0,3094000.0,3114000.0,3135000.0,3155000.0,3175000.0,3196000.0,  &
     3216000.0,3280000.0,3382000.0,3488000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,40),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     2688000.0,2690000.0,2692000.0,2694000.0,2696000.0,2699000.0,2701000.0,2703000.0,2705000.0,  &
     2707000.0,2709000.0,2711000.0,2713000.0,2715000.0,2717000.0,2719000.0,2721000.0,2723000.0,  &
     2725000.0,2727000.0,2730000.0,2732000.0,2734000.0,2738000.0,2742000.0,2746000.0,2750000.0,  &
     2754000.0,2758000.0,2762000.0,2766000.0,2770000.0,2774000.0,2778000.0,2782000.0,2786000.0,  &
     2790000.0,2794000.0,2798000.0,2802000.0,2806000.0,2810000.0,2814000.0,2818000.0,2822000.0,  &
     2826000.0,2830000.0,2834000.0,2844000.0,2854000.0,2864000.0,2874000.0,2884000.0,2894000.0,  &
     2904000.0,2914000.0,2924000.0,2934000.0,2953000.0,2973000.0,2993000.0,3013000.0,3033000.0,  &
     3054000.0,3074000.0,3094000.0,3114000.0,3134000.0,3155000.0,3175000.0,3196000.0,3216000.0,  &
     3280000.0,3382000.0,3488000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,41),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,2690000.0,2692000.0,2694000.0,2696000.0,2698000.0,2700000.0,2702000.0,2704000.0,2706000.0,  &
     2708000.0,2711000.0,2713000.0,2715000.0,2717000.0,2719000.0,2721000.0,2723000.0,2725000.0,  &
     2727000.0,2729000.0,2731000.0,2733000.0,2737000.0,2741000.0,2745000.0,2749000.0,2754000.0,  &
     2758000.0,2762000.0,2766000.0,2770000.0,2774000.0,2778000.0,2782000.0,2786000.0,2790000.0,  &
     2794000.0,2798000.0,2802000.0,2806000.0,2810000.0,2814000.0,2818000.0,2822000.0,2826000.0,  &
     2830000.0,2834000.0,2844000.0,2854000.0,2864000.0,2874000.0,2884000.0,2894000.0,2903000.0,  &
     2913000.0,2923000.0,2933000.0,2953000.0,2973000.0,2993000.0,3013000.0,3033000.0,3053000.0,  &
     3074000.0,3094000.0,3114000.0,3134000.0,3155000.0,3175000.0,3196000.0,3216000.0,3280000.0,  &
     3382000.0,3488000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,42),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,2691000.0,2693000.0,2695000.0,2697000.0,2700000.0,2702000.0,2704000.0,2706000.0,  &
     2708000.0,2710000.0,2712000.0,2714000.0,2716000.0,2718000.0,2720000.0,2722000.0,2724000.0,  &
     2727000.0,2729000.0,2731000.0,2733000.0,2737000.0,2741000.0,2745000.0,2749000.0,2753000.0,  &
     2757000.0,2761000.0,2765000.0,2769000.0,2773000.0,2777000.0,2781000.0,2785000.0,2790000.0,  &
     2794000.0,2798000.0,2802000.0,2806000.0,2810000.0,2814000.0,2818000.0,2822000.0,2826000.0,  &
     2830000.0,2834000.0,2844000.0,2853000.0,2863000.0,2873000.0,2883000.0,2893000.0,2903000.0,  &
     2913000.0,2923000.0,2933000.0,2953000.0,2973000.0,2993000.0,3013000.0,3033000.0,3053000.0,  &
     3073000.0,3094000.0,3114000.0,3134000.0,3155000.0,3175000.0,3195000.0,3216000.0,3280000.0,  &
     3382000.0,3488000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,43),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,2693000.0,2695000.0,2697000.0,2699000.0,2701000.0,2703000.0,2705000.0,2707000.0,  &
     2709000.0,2712000.0,2714000.0,2716000.0,2718000.0,2720000.0,2722000.0,2724000.0,2726000.0,  &
     2728000.0,2730000.0,2732000.0,2736000.0,2740000.0,2745000.0,2749000.0,2753000.0,2757000.0,  &
     2761000.0,2765000.0,2769000.0,2773000.0,2777000.0,2781000.0,2785000.0,2789000.0,2793000.0,  &
     2797000.0,2801000.0,2805000.0,2809000.0,2813000.0,2817000.0,2821000.0,2825000.0,2829000.0,  &
     2833000.0,2843000.0,2853000.0,2863000.0,2873000.0,2883000.0,2893000.0,2903000.0,2913000.0,  &
     2923000.0,2933000.0,2953000.0,2973000.0,2993000.0,3013000.0,3033000.0,3053000.0,3073000.0,  &
     3094000.0,3114000.0,3134000.0,3154000.0,3175000.0,3195000.0,3216000.0,3280000.0,3382000.0,  &
     3488000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,44),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,2694000.0,2696000.0,2698000.0,2700000.0,2703000.0,2705000.0,2707000.0,  &
     2709000.0,2711000.0,2713000.0,2715000.0,2717000.0,2719000.0,2721000.0,2724000.0,2726000.0,  &
     2728000.0,2730000.0,2732000.0,2736000.0,2740000.0,2744000.0,2748000.0,2752000.0,2756000.0,  &
     2760000.0,2765000.0,2769000.0,2773000.0,2777000.0,2781000.0,2785000.0,2789000.0,2793000.0,  &
     2797000.0,2801000.0,2805000.0,2809000.0,2813000.0,2817000.0,2821000.0,2825000.0,2829000.0,  &
     2833000.0,2843000.0,2853000.0,2863000.0,2873000.0,2883000.0,2893000.0,2903000.0,2913000.0,  &
     2923000.0,2933000.0,2953000.0,2973000.0,2993000.0,3013000.0,3033000.0,3053000.0,3073000.0,  &
     3093000.0,3114000.0,3134000.0,3154000.0,3175000.0,3195000.0,3216000.0,3280000.0,3382000.0,  &
     3488000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,45),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,2696000.0,2698000.0,2700000.0,2702000.0,2704000.0,2706000.0,2708000.0,  &
     2710000.0,2713000.0,2715000.0,2717000.0,2719000.0,2721000.0,2723000.0,2725000.0,2727000.0,  &
     2729000.0,2731000.0,2735000.0,2740000.0,2744000.0,2748000.0,2752000.0,2756000.0,2760000.0,  &
     2764000.0,2768000.0,2772000.0,2776000.0,2780000.0,2784000.0,2788000.0,2793000.0,2797000.0,  &
     2801000.0,2805000.0,2809000.0,2813000.0,2817000.0,2821000.0,2825000.0,2829000.0,2833000.0,  &
     2843000.0,2853000.0,2863000.0,2873000.0,2883000.0,2893000.0,2903000.0,2913000.0,2923000.0,  &
     2933000.0,2953000.0,2973000.0,2993000.0,3013000.0,3033000.0,3053000.0,3073000.0,3093000.0,  &
     3114000.0,3134000.0,3154000.0,3175000.0,3195000.0,3216000.0,3280000.0,3382000.0,3488000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,46),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,2697000.0,2699000.0,2701000.0,2703000.0,2706000.0,2708000.0,  &
     2710000.0,2712000.0,2714000.0,2716000.0,2718000.0,2720000.0,2722000.0,2725000.0,2727000.0,  &
     2729000.0,2731000.0,2735000.0,2739000.0,2743000.0,2747000.0,2751000.0,2756000.0,2760000.0,  &
     2764000.0,2768000.0,2772000.0,2776000.0,2780000.0,2784000.0,2788000.0,2792000.0,2796000.0,  &
     2800000.0,2804000.0,2808000.0,2812000.0,2816000.0,2820000.0,2824000.0,2828000.0,2832000.0,  &
     2842000.0,2852000.0,2862000.0,2872000.0,2882000.0,2892000.0,2902000.0,2912000.0,2922000.0,  &
     2932000.0,2952000.0,2972000.0,2992000.0,3013000.0,3033000.0,3053000.0,3073000.0,3093000.0,  &
     3113000.0,3134000.0,3154000.0,3175000.0,3195000.0,3216000.0,3280000.0,3382000.0,3488000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,47),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,2699000.0,2701000.0,2703000.0,2705000.0,2707000.0,2709000.0,  &
     2711000.0,2713000.0,2716000.0,2718000.0,2720000.0,2722000.0,2724000.0,2726000.0,2728000.0,  &
     2730000.0,2734000.0,2739000.0,2743000.0,2747000.0,2751000.0,2755000.0,2759000.0,2763000.0,  &
     2767000.0,2771000.0,2776000.0,2780000.0,2784000.0,2788000.0,2792000.0,2796000.0,2800000.0,  &
     2804000.0,2808000.0,2812000.0,2816000.0,2820000.0,2824000.0,2828000.0,2832000.0,2842000.0,  &
     2852000.0,2862000.0,2872000.0,2882000.0,2892000.0,2902000.0,2912000.0,2922000.0,2932000.0,  &
     2952000.0,2972000.0,2992000.0,3012000.0,3032000.0,3053000.0,3073000.0,3093000.0,3113000.0,  &
     3134000.0,3154000.0,3174000.0,3195000.0,3216000.0,3280000.0,3382000.0,3488000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,48),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2700000.0,2702000.0,2704000.0,2706000.0,2709000.0,  &
     2711000.0,2713000.0,2715000.0,2717000.0,2719000.0,2721000.0,2723000.0,2726000.0,2728000.0,  &
     2730000.0,2734000.0,2738000.0,2742000.0,2746000.0,2750000.0,2755000.0,2759000.0,2763000.0,  &
     2767000.0,2771000.0,2775000.0,2779000.0,2783000.0,2787000.0,2791000.0,2795000.0,2800000.0,  &
     2804000.0,2808000.0,2812000.0,2816000.0,2820000.0,2824000.0,2828000.0,2832000.0,2842000.0,  &
     2852000.0,2862000.0,2872000.0,2882000.0,2892000.0,2902000.0,2912000.0,2922000.0,2932000.0,  &
     2952000.0,2972000.0,2992000.0,3012000.0,3032000.0,3052000.0,3073000.0,3093000.0,3113000.0,  &
     3134000.0,3154000.0,3174000.0,3195000.0,3215000.0,3280000.0,3382000.0,3488000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,49),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2702000.0,2704000.0,2706000.0,2708000.0,2710000.0,  &
     2712000.0,2714000.0,2717000.0,2719000.0,2721000.0,2723000.0,2725000.0,2727000.0,2729000.0,  &
     2733000.0,2738000.0,2742000.0,2746000.0,2750000.0,2754000.0,2758000.0,2762000.0,2766000.0,  &
     2771000.0,2775000.0,2779000.0,2783000.0,2787000.0,2791000.0,2795000.0,2799000.0,2803000.0,  &
     2807000.0,2811000.0,2815000.0,2819000.0,2823000.0,2827000.0,2831000.0,2842000.0,2852000.0,  &
     2862000.0,2872000.0,2882000.0,2892000.0,2902000.0,2912000.0,2922000.0,2932000.0,2952000.0,  &
     2972000.0,2992000.0,3012000.0,3032000.0,3052000.0,3073000.0,3093000.0,3113000.0,3133000.0,  &
     3154000.0,3174000.0,3195000.0,3215000.0,3280000.0,3382000.0,3488000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,50),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2703000.0,2705000.0,2707000.0,2709000.0,  &
     2712000.0,2714000.0,2716000.0,2718000.0,2720000.0,2722000.0,2724000.0,2726000.0,2729000.0,  &
     2733000.0,2737000.0,2741000.0,2745000.0,2749000.0,2754000.0,2758000.0,2762000.0,2766000.0,  &
     2770000.0,2774000.0,2778000.0,2782000.0,2787000.0,2791000.0,2795000.0,2799000.0,2803000.0,  &
     2807000.0,2811000.0,2815000.0,2819000.0,2823000.0,2827000.0,2831000.0,2841000.0,2851000.0,  &
     2861000.0,2871000.0,2881000.0,2891000.0,2901000.0,2911000.0,2922000.0,2932000.0,2952000.0,  &
     2972000.0,2992000.0,3012000.0,3032000.0,3052000.0,3072000.0,3093000.0,3113000.0,3133000.0,  &
     3154000.0,3174000.0,3195000.0,3215000.0,3280000.0,3382000.0,3488000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,51),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2704000.0,2707000.0,2709000.0,2711000.0,  &
     2713000.0,2715000.0,2717000.0,2720000.0,2722000.0,2724000.0,2726000.0,2728000.0,2732000.0,  &
     2736000.0,2741000.0,2745000.0,2749000.0,2753000.0,2757000.0,2761000.0,2766000.0,2770000.0,  &
     2774000.0,2778000.0,2782000.0,2786000.0,2790000.0,2794000.0,2798000.0,2802000.0,2806000.0,  &
     2811000.0,2815000.0,2819000.0,2823000.0,2827000.0,2831000.0,2841000.0,2851000.0,2861000.0,  &
     2871000.0,2881000.0,2891000.0,2901000.0,2911000.0,2921000.0,2931000.0,2951000.0,2971000.0,  &
     2992000.0,3012000.0,3032000.0,3052000.0,3072000.0,3093000.0,3113000.0,3133000.0,3154000.0,  &
     3174000.0,3195000.0,3215000.0,3280000.0,3382000.0,3488000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,52),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2706000.0,2708000.0,2710000.0,  &
     2712000.0,2715000.0,2717000.0,2719000.0,2721000.0,2723000.0,2725000.0,2727000.0,2732000.0,  &
     2736000.0,2740000.0,2744000.0,2748000.0,2753000.0,2757000.0,2761000.0,2765000.0,2769000.0,  &
     2773000.0,2777000.0,2782000.0,2786000.0,2790000.0,2794000.0,2798000.0,2802000.0,2806000.0,  &
     2810000.0,2814000.0,2818000.0,2822000.0,2826000.0,2830000.0,2841000.0,2851000.0,2861000.0,  &
     2871000.0,2881000.0,2891000.0,2901000.0,2911000.0,2921000.0,2931000.0,2951000.0,2971000.0,  &
     2991000.0,3012000.0,3032000.0,3052000.0,3072000.0,3092000.0,3113000.0,3133000.0,3153000.0,  &
     3174000.0,3194000.0,3215000.0,3280000.0,3382000.0,3488000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,53),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2707000.0,2710000.0,2712000.0,  &
     2714000.0,2716000.0,2718000.0,2720000.0,2722000.0,2725000.0,2727000.0,2731000.0,2735000.0,  &
     2739000.0,2744000.0,2748000.0,2752000.0,2756000.0,2760000.0,2765000.0,2769000.0,2773000.0,  &
     2777000.0,2781000.0,2785000.0,2789000.0,2793000.0,2798000.0,2802000.0,2806000.0,2810000.0,  &
     2814000.0,2818000.0,2822000.0,2826000.0,2830000.0,2840000.0,2850000.0,2860000.0,2870000.0,  &
     2881000.0,2891000.0,2901000.0,2911000.0,2921000.0,2931000.0,2951000.0,2971000.0,2991000.0,  &
     3011000.0,3031000.0,3052000.0,3072000.0,3092000.0,3113000.0,3133000.0,3153000.0,3174000.0,  &
     3194000.0,3215000.0,3280000.0,3382000.0,3488000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,54),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2709000.0,2711000.0,  &
     2713000.0,2715000.0,2718000.0,2720000.0,2722000.0,2724000.0,2726000.0,2730000.0,2735000.0,  &
     2739000.0,2743000.0,2747000.0,2752000.0,2756000.0,2760000.0,2764000.0,2768000.0,2772000.0,  &
     2776000.0,2781000.0,2785000.0,2789000.0,2793000.0,2797000.0,2801000.0,2805000.0,2809000.0,  &
     2813000.0,2818000.0,2822000.0,2826000.0,2830000.0,2840000.0,2850000.0,2860000.0,2870000.0,  &
     2880000.0,2890000.0,2900000.0,2910000.0,2921000.0,2931000.0,2951000.0,2971000.0,2991000.0,  &
     3011000.0,3031000.0,3052000.0,3072000.0,3092000.0,3112000.0,3133000.0,3153000.0,3174000.0,  &
     3194000.0,3215000.0,3280000.0,3381000.0,3488000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,55),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2710000.0,2712000.0,  &
     2715000.0,2717000.0,2719000.0,2721000.0,2723000.0,2725000.0,2730000.0,2734000.0,2738000.0,  &
     2743000.0,2747000.0,2751000.0,2755000.0,2759000.0,2764000.0,2768000.0,2772000.0,2776000.0,  &
     2780000.0,2784000.0,2788000.0,2793000.0,2797000.0,2801000.0,2805000.0,2809000.0,2813000.0,  &
     2817000.0,2821000.0,2825000.0,2829000.0,2839000.0,2850000.0,2860000.0,2870000.0,2880000.0,  &
     2890000.0,2900000.0,2910000.0,2920000.0,2930000.0,2950000.0,2971000.0,2991000.0,3011000.0,  &
     3031000.0,3051000.0,3072000.0,3092000.0,3112000.0,3133000.0,3153000.0,3174000.0,3194000.0,  &
     3215000.0,3280000.0,3381000.0,3487000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,56),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2712000.0,  &
     2714000.0,2716000.0,2718000.0,2720000.0,2723000.0,2725000.0,2729000.0,2733000.0,2738000.0,  &
     2742000.0,2746000.0,2750000.0,2755000.0,2759000.0,2763000.0,2767000.0,2771000.0,2775000.0,  &
     2780000.0,2784000.0,2788000.0,2792000.0,2796000.0,2800000.0,2804000.0,2808000.0,2813000.0,  &
     2817000.0,2821000.0,2825000.0,2829000.0,2839000.0,2849000.0,2859000.0,2870000.0,2880000.0,  &
     2890000.0,2900000.0,2910000.0,2920000.0,2930000.0,2950000.0,2970000.0,2991000.0,3011000.0,  &
     3031000.0,3051000.0,3071000.0,3092000.0,3112000.0,3132000.0,3153000.0,3173000.0,3194000.0,  &
     3215000.0,3280000.0,3381000.0,3487000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,57),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2713000.0,  &
     2715000.0,2717000.0,2720000.0,2722000.0,2724000.0,2728000.0,2733000.0,2737000.0,2741000.0,  &
     2746000.0,2750000.0,2754000.0,2758000.0,2762000.0,2767000.0,2771000.0,2775000.0,2779000.0,  &
     2783000.0,2787000.0,2792000.0,2796000.0,2800000.0,2804000.0,2808000.0,2812000.0,2816000.0,  &
     2820000.0,2824000.0,2829000.0,2839000.0,2849000.0,2859000.0,2869000.0,2879000.0,2889000.0,  &
     2900000.0,2910000.0,2920000.0,2930000.0,2950000.0,2970000.0,2990000.0,3011000.0,3031000.0,  &
     3051000.0,3071000.0,3092000.0,3112000.0,3132000.0,3153000.0,3173000.0,3194000.0,3214000.0,  &
     3280000.0,3381000.0,3487000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,58),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     2715000.0,2717000.0,2719000.0,2721000.0,2723000.0,2728000.0,2732000.0,2736000.0,2741000.0,  &
     2745000.0,2749000.0,2753000.0,2758000.0,2762000.0,2766000.0,2770000.0,2774000.0,2779000.0,  &
     2783000.0,2787000.0,2791000.0,2795000.0,2799000.0,2803000.0,2808000.0,2812000.0,2816000.0,  &
     2820000.0,2824000.0,2828000.0,2838000.0,2849000.0,2859000.0,2869000.0,2879000.0,2889000.0,  &
     2899000.0,2909000.0,2919000.0,2930000.0,2950000.0,2970000.0,2990000.0,3010000.0,3031000.0,  &
     3051000.0,3071000.0,3091000.0,3112000.0,3132000.0,3153000.0,3173000.0,3194000.0,3214000.0,  &
     3280000.0,3381000.0,3487000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,59),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     2716000.0,2718000.0,2720000.0,2723000.0,2727000.0,2731000.0,2736000.0,2740000.0,2744000.0,  &
     2748000.0,2753000.0,2757000.0,2761000.0,2765000.0,2770000.0,2774000.0,2778000.0,2782000.0,  &
     2786000.0,2791000.0,2795000.0,2799000.0,2803000.0,2807000.0,2811000.0,2815000.0,2819000.0,  &
     2824000.0,2828000.0,2838000.0,2848000.0,2858000.0,2868000.0,2879000.0,2889000.0,2899000.0,  &
     2909000.0,2919000.0,2929000.0,2949000.0,2970000.0,2990000.0,3010000.0,3030000.0,3051000.0,  &
     3071000.0,3091000.0,3112000.0,3132000.0,3152000.0,3173000.0,3194000.0,3214000.0,3280000.0,  &
     3381000.0,3487000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,60),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,2717000.0,2720000.0,2722000.0,2726000.0,2731000.0,2735000.0,2739000.0,2744000.0,2748000.0,  &
     2752000.0,2756000.0,2761000.0,2765000.0,2769000.0,2773000.0,2777000.0,2782000.0,2786000.0,  &
     2790000.0,2794000.0,2798000.0,2802000.0,2807000.0,2811000.0,2815000.0,2819000.0,2823000.0,  &
     2827000.0,2837000.0,2848000.0,2858000.0,2868000.0,2878000.0,2888000.0,2899000.0,2909000.0,  &
     2919000.0,2929000.0,2949000.0,2969000.0,2990000.0,3010000.0,3030000.0,3050000.0,3071000.0,  &
     3091000.0,3111000.0,3132000.0,3152000.0,3173000.0,3193000.0,3214000.0,3280000.0,3381000.0,  &
     3487000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,61),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,2719000.0,2721000.0,2725000.0,2730000.0,2734000.0,2738000.0,2743000.0,2747000.0,  &
     2751000.0,2756000.0,2760000.0,2764000.0,2768000.0,2773000.0,2777000.0,2781000.0,2785000.0,  &
     2789000.0,2794000.0,2798000.0,2802000.0,2806000.0,2810000.0,2814000.0,2819000.0,2823000.0,  &
     2827000.0,2837000.0,2847000.0,2858000.0,2868000.0,2878000.0,2888000.0,2898000.0,2908000.0,  &
     2919000.0,2929000.0,2949000.0,2969000.0,2989000.0,3010000.0,3030000.0,3050000.0,3071000.0,  &
     3091000.0,3111000.0,3132000.0,3152000.0,3173000.0,3193000.0,3214000.0,3280000.0,3381000.0,  &
     3487000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,62),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,2720000.0,2725000.0,2729000.0,2733000.0,2738000.0,2742000.0,2746000.0,2751000.0,  &
     2755000.0,2759000.0,2764000.0,2768000.0,2772000.0,2776000.0,2781000.0,2785000.0,2789000.0,  &
     2793000.0,2797000.0,2801000.0,2806000.0,2810000.0,2814000.0,2818000.0,2822000.0,2826000.0,  &
     2837000.0,2847000.0,2857000.0,2867000.0,2878000.0,2888000.0,2898000.0,2908000.0,2918000.0,  &
     2928000.0,2949000.0,2969000.0,2989000.0,3009000.0,3030000.0,3050000.0,3070000.0,3091000.0,  &
     3111000.0,3132000.0,3152000.0,3173000.0,3193000.0,3214000.0,3280000.0,3381000.0,3487000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,63),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,2723000.0,2727000.0,2732000.0,2736000.0,2741000.0,2745000.0,2749000.0,  &
     2754000.0,2758000.0,2762000.0,2767000.0,2771000.0,2775000.0,2779000.0,2784000.0,2788000.0,  &
     2792000.0,2796000.0,2800000.0,2805000.0,2809000.0,2813000.0,2817000.0,2821000.0,2825000.0,  &
     2836000.0,2846000.0,2856000.0,2867000.0,2877000.0,2887000.0,2897000.0,2907000.0,2918000.0,  &
     2928000.0,2948000.0,2968000.0,2989000.0,3009000.0,3029000.0,3050000.0,3070000.0,3090000.0,  &
     3111000.0,3131000.0,3152000.0,3172000.0,3193000.0,3213000.0,3280000.0,3380000.0,3487000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,64),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,2726000.0,2730000.0,2735000.0,2739000.0,2743000.0,2748000.0,2752000.0,  &
     2757000.0,2761000.0,2765000.0,2769000.0,2774000.0,2778000.0,2782000.0,2787000.0,2791000.0,  &
     2795000.0,2799000.0,2803000.0,2808000.0,2812000.0,2816000.0,2820000.0,2824000.0,2835000.0,  &
     2845000.0,2855000.0,2866000.0,2876000.0,2886000.0,2896000.0,2907000.0,2917000.0,2927000.0,  &
     2947000.0,2968000.0,2988000.0,3008000.0,3029000.0,3049000.0,3069000.0,3090000.0,3110000.0,  &
     3131000.0,3151000.0,3172000.0,3192000.0,3213000.0,3280000.0,3380000.0,3487000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,65),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,2728000.0,2733000.0,2737000.0,2742000.0,2746000.0,2751000.0,  &
     2755000.0,2759000.0,2764000.0,2768000.0,2772000.0,2777000.0,2781000.0,2785000.0,2790000.0,  &
     2794000.0,2798000.0,2802000.0,2806000.0,2811000.0,2815000.0,2819000.0,2823000.0,2834000.0,  &
     2844000.0,2854000.0,2865000.0,2875000.0,2885000.0,2896000.0,2906000.0,2916000.0,2926000.0,  &
     2947000.0,2967000.0,2987000.0,3008000.0,3028000.0,3049000.0,3069000.0,3089000.0,3110000.0,  &
     3130000.0,3151000.0,3172000.0,3192000.0,3213000.0,3280000.0,3380000.0,3486000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,66),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,2731000.0,2735000.0,2740000.0,2744000.0,2749000.0,2753000.0,  &
     2758000.0,2762000.0,2767000.0,2771000.0,2775000.0,2780000.0,2784000.0,2788000.0,2792000.0,  &
     2797000.0,2801000.0,2805000.0,2809000.0,2814000.0,2818000.0,2822000.0,2833000.0,2843000.0,  &
     2853000.0,2864000.0,2874000.0,2885000.0,2895000.0,2905000.0,2915000.0,2926000.0,2946000.0,  &
     2966000.0,2987000.0,3007000.0,3028000.0,3048000.0,3069000.0,3089000.0,3109000.0,3130000.0,  &
     3151000.0,3171000.0,3192000.0,3212000.0,3280000.0,3380000.0,3486000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,67),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2733000.0,2738000.0,2743000.0,2747000.0,2752000.0,  &
     2756000.0,2761000.0,2765000.0,2769000.0,2774000.0,2778000.0,2782000.0,2787000.0,2791000.0,  &
     2795000.0,2800000.0,2804000.0,2808000.0,2812000.0,2817000.0,2821000.0,2831000.0,2842000.0,  &
     2852000.0,2863000.0,2873000.0,2884000.0,2894000.0,2904000.0,2915000.0,2925000.0,2945000.0,  &
     2966000.0,2986000.0,3007000.0,3027000.0,3048000.0,3068000.0,3089000.0,3109000.0,3130000.0,  &
     3150000.0,3171000.0,3191000.0,3212000.0,3280000.0,3380000.0,3486000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,68),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2736000.0,2741000.0,2745000.0,2750000.0,2754000.0,  &
     2759000.0,2763000.0,2768000.0,2772000.0,2777000.0,2781000.0,2785000.0,2790000.0,2794000.0,  &
     2798000.0,2803000.0,2807000.0,2811000.0,2815000.0,2820000.0,2830000.0,2841000.0,2851000.0,  &
     2862000.0,2872000.0,2883000.0,2893000.0,2903000.0,2914000.0,2924000.0,2945000.0,2965000.0,  &
     2986000.0,3006000.0,3027000.0,3047000.0,3068000.0,3088000.0,3109000.0,3129000.0,3150000.0,  &
     3170000.0,3191000.0,3212000.0,3280000.0,3379000.0,3486000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,69),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2739000.0,2743000.0,2748000.0,2752000.0,  &
     2757000.0,2761000.0,2766000.0,2770000.0,2775000.0,2779000.0,2784000.0,2788000.0,2792000.0,  &
     2797000.0,2801000.0,2805000.0,2810000.0,2814000.0,2818000.0,2829000.0,2840000.0,2850000.0,  &
     2861000.0,2871000.0,2882000.0,2892000.0,2902000.0,2913000.0,2923000.0,2944000.0,2964000.0,  &
     2985000.0,3005000.0,3026000.0,3046000.0,3067000.0,3088000.0,3108000.0,3129000.0,3149000.0,  &
     3170000.0,3191000.0,3211000.0,3280000.0,3379000.0,3485000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,70),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2741000.0,2746000.0,2750000.0,2755000.0,  &
     2760000.0,2764000.0,2769000.0,2773000.0,2778000.0,2782000.0,2786000.0,2791000.0,2795000.0,  &
     2800000.0,2804000.0,2808000.0,2813000.0,2817000.0,2828000.0,2838000.0,2849000.0,2860000.0,  &
     2870000.0,2881000.0,2891000.0,2901000.0,2912000.0,2922000.0,2943000.0,2964000.0,2984000.0,  &
     3005000.0,3025000.0,3046000.0,3066000.0,3087000.0,3108000.0,3128000.0,3149000.0,3170000.0,  &
     3190000.0,3211000.0,3280000.0,3379000.0,3485000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,71),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2744000.0,2748000.0,2753000.0,  &
     2758000.0,2762000.0,2767000.0,2771000.0,2776000.0,2780000.0,2785000.0,2789000.0,2794000.0,  &
     2798000.0,2802000.0,2807000.0,2811000.0,2815000.0,2826000.0,2837000.0,2848000.0,2858000.0,  &
     2869000.0,2879000.0,2890000.0,2900000.0,2911000.0,2921000.0,2942000.0,2963000.0,2983000.0,  &
     3004000.0,3025000.0,3045000.0,3066000.0,3086000.0,3107000.0,3128000.0,3148000.0,3169000.0,  &
     3190000.0,3211000.0,3280000.0,3378000.0,3485000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,72),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2746000.0,2751000.0,2755000.0,  &
     2760000.0,2765000.0,2769000.0,2774000.0,2778000.0,2783000.0,2787000.0,2792000.0,2796000.0,  &
     2801000.0,2805000.0,2810000.0,2814000.0,2825000.0,2836000.0,2846000.0,2857000.0,2868000.0,  &
     2878000.0,2889000.0,2899000.0,2910000.0,2920000.0,2941000.0,2962000.0,2983000.0,3003000.0,  &
     3024000.0,3045000.0,3065000.0,3086000.0,3106000.0,3127000.0,3148000.0,3169000.0,3189000.0,  &
     3210000.0,3280000.0,3378000.0,3485000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,73),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2748000.0,2753000.0,  &
     2758000.0,2763000.0,2767000.0,2772000.0,2776000.0,2781000.0,2786000.0,2790000.0,2795000.0,  &
     2799000.0,2803000.0,2808000.0,2812000.0,2823000.0,2834000.0,2845000.0,2856000.0,2866000.0,  &
     2877000.0,2888000.0,2898000.0,2909000.0,2919000.0,2940000.0,2961000.0,2982000.0,3002000.0,  &
     3023000.0,3044000.0,3064000.0,3085000.0,3106000.0,3127000.0,3147000.0,3168000.0,3189000.0,  &
     3210000.0,3280000.0,3378000.0,3484000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,74),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2751000.0,2755000.0,  &
     2760000.0,2765000.0,2770000.0,2774000.0,2779000.0,2784000.0,2788000.0,2793000.0,2797000.0,  &
     2802000.0,2806000.0,2811000.0,2822000.0,2833000.0,2843000.0,2854000.0,2865000.0,2876000.0,  &
     2886000.0,2897000.0,2908000.0,2918000.0,2939000.0,2960000.0,2981000.0,3002000.0,3022000.0,  &
     3043000.0,3064000.0,3085000.0,3105000.0,3126000.0,3147000.0,3168000.0,3188000.0,3209000.0,  &
     3280000.0,3377000.0,3484000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,75),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2753000.0,  &
     2758000.0,2763000.0,2767000.0,2772000.0,2777000.0,2781000.0,2786000.0,2791000.0,2795000.0,  &
     2800000.0,2804000.0,2809000.0,2820000.0,2831000.0,2842000.0,2853000.0,2864000.0,2874000.0,  &
     2885000.0,2896000.0,2906000.0,2917000.0,2938000.0,2959000.0,2980000.0,3001000.0,3022000.0,  &
     3042000.0,3063000.0,3084000.0,3105000.0,3125000.0,3146000.0,3167000.0,3188000.0,3209000.0,  &
     3280000.0,3377000.0,3484000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,76),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2755000.0,  &
     2760000.0,2765000.0,2770000.0,2775000.0,2779000.0,2784000.0,2789000.0,2793000.0,2798000.0,  &
     2802000.0,2807000.0,2818000.0,2829000.0,2840000.0,2851000.0,2862000.0,2873000.0,2884000.0,  &
     2894000.0,2905000.0,2916000.0,2937000.0,2958000.0,2979000.0,3000000.0,3021000.0,3042000.0,  &
     3062000.0,3083000.0,3104000.0,3125000.0,3146000.0,3166000.0,3187000.0,3208000.0,3280000.0,  &
     3377000.0,3484000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,77),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     2757000.0,2762000.0,2767000.0,2772000.0,2777000.0,2782000.0,2786000.0,2791000.0,2796000.0,  &
     2800000.0,2805000.0,2816000.0,2827000.0,2839000.0,2850000.0,2861000.0,2872000.0,2882000.0,  &
     2893000.0,2904000.0,2915000.0,2936000.0,2957000.0,2978000.0,2999000.0,3020000.0,3041000.0,  &
     3062000.0,3082000.0,3103000.0,3124000.0,3145000.0,3166000.0,3187000.0,3208000.0,3280000.0,  &
     3376000.0,3483000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,78),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     2760000.0,2765000.0,2770000.0,2774000.0,2779000.0,2784000.0,2789000.0,2793000.0,2798000.0,  &
     2803000.0,2814000.0,2826000.0,2837000.0,2848000.0,2859000.0,2870000.0,2881000.0,2892000.0,  &
     2902000.0,2913000.0,2935000.0,2956000.0,2977000.0,2998000.0,3019000.0,3040000.0,3061000.0,  &
     3082000.0,3102000.0,3123000.0,3144000.0,3165000.0,3186000.0,3207000.0,3280000.0,3376000.0,  &
     3483000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,79),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,2762000.0,2767000.0,2772000.0,2777000.0,2781000.0,2786000.0,2791000.0,2796000.0,2800000.0,  &
     2812000.0,2824000.0,2835000.0,2846000.0,2857000.0,2868000.0,2879000.0,2890000.0,2901000.0,  &
     2912000.0,2933000.0,2955000.0,2976000.0,2997000.0,3018000.0,3039000.0,3060000.0,3081000.0,  &
     3102000.0,3123000.0,3144000.0,3164000.0,3185000.0,3206000.0,3280000.0,3375000.0,3483000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,80),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,2764000.0,2769000.0,2774000.0,2779000.0,2784000.0,2789000.0,2793000.0,2798000.0,  &
     2810000.0,2821000.0,2833000.0,2844000.0,2855000.0,2867000.0,2878000.0,2889000.0,2900000.0,  &
     2910000.0,2932000.0,2953000.0,2975000.0,2996000.0,3017000.0,3038000.0,3059000.0,3080000.0,  &
     3101000.0,3122000.0,3143000.0,3164000.0,3185000.0,3206000.0,3280000.0,3375000.0,3482000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,81),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,2766000.0,2771000.0,2776000.0,2781000.0,2786000.0,2791000.0,2796000.0,2808000.0,  &
     2819000.0,2831000.0,2842000.0,2854000.0,2865000.0,2876000.0,2887000.0,2898000.0,2909000.0,  &
     2931000.0,2952000.0,2973000.0,2995000.0,3016000.0,3037000.0,3058000.0,3079000.0,3100000.0,  &
     3121000.0,3142000.0,3163000.0,3184000.0,3205000.0,3280000.0,3374000.0,3482000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,82),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,2768000.0,2773000.0,2778000.0,2783000.0,2788000.0,2793000.0,2805000.0,  &
     2817000.0,2829000.0,2840000.0,2852000.0,2863000.0,2874000.0,2885000.0,2896000.0,2907000.0,  &
     2929000.0,2951000.0,2972000.0,2994000.0,3015000.0,3036000.0,3057000.0,3078000.0,3099000.0,  &
     3120000.0,3141000.0,3162000.0,3183000.0,3204000.0,3280000.0,3374000.0,3481000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,83),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,2770000.0,2775000.0,2780000.0,2785000.0,2790000.0,2802000.0,2814000.0,  &
     2826000.0,2838000.0,2850000.0,2861000.0,2872000.0,2883000.0,2895000.0,2906000.0,2928000.0,  &
     2949000.0,2971000.0,2992000.0,3014000.0,3035000.0,3056000.0,3077000.0,3098000.0,3119000.0,  &
     3140000.0,3162000.0,3183000.0,3204000.0,3280000.0,3373000.0,3481000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,84),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,2772000.0,2777000.0,2782000.0,2787000.0,2800000.0,2812000.0,  &
     2824000.0,2836000.0,2847000.0,2859000.0,2870000.0,2882000.0,2893000.0,2904000.0,2926000.0,  &
     2948000.0,2969000.0,2991000.0,3012000.0,3034000.0,3055000.0,3076000.0,3097000.0,3118000.0,  &
     3140000.0,3161000.0,3182000.0,3203000.0,3280000.0,3373000.0,3480000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,85),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,2774000.0,2779000.0,2784000.0,2797000.0,2809000.0,2821000.0,  &
     2833000.0,2845000.0,2857000.0,2868000.0,2880000.0,2891000.0,2902000.0,2924000.0,2946000.0,  &
     2968000.0,2990000.0,3011000.0,3033000.0,3054000.0,3075000.0,3096000.0,3118000.0,3139000.0,  &
     3160000.0,3181000.0,3202000.0,3280000.0,3372000.0,3480000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,86),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2775000.0,2781000.0,2794000.0,2806000.0,2819000.0,  &
     2831000.0,2843000.0,2854000.0,2866000.0,2878000.0,2889000.0,2900000.0,2923000.0,2945000.0,  &
     2967000.0,2988000.0,3010000.0,3031000.0,3053000.0,3074000.0,3095000.0,3117000.0,3138000.0,  &
     3159000.0,3180000.0,3201000.0,3280000.0,3372000.0,3480000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,87),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2777000.0,2790000.0,2803000.0,2816000.0,2828000.0,  &
     2840000.0,2852000.0,2864000.0,2875000.0,2887000.0,2898000.0,2921000.0,2943000.0,2965000.0,  &
     2987000.0,3009000.0,3030000.0,3052000.0,3073000.0,3094000.0,3116000.0,3137000.0,3158000.0,  &
     3179000.0,3201000.0,3280000.0,3371000.0,3479000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,88),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2781000.0,2795000.0,2808000.0,2821000.0,  &
     2833000.0,2846000.0,2858000.0,2870000.0,2881000.0,2893000.0,2916000.0,2939000.0,2961000.0,  &
     2983000.0,3005000.0,3027000.0,3048000.0,3070000.0,3091000.0,3113000.0,3134000.0,3156000.0,  &
     3177000.0,3198000.0,3280000.0,3370000.0,3478000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,89),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2785000.0,2799000.0,2813000.0,2826000.0,  &
     2838000.0,2851000.0,2863000.0,2875000.0,2887000.0,2910000.0,2933000.0,2956000.0,2979000.0,  &
     3001000.0,3023000.0,3045000.0,3067000.0,3088000.0,3110000.0,3132000.0,3153000.0,3175000.0,  &
     3196000.0,3280000.0,3368000.0,3476000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,90),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2789000.0,2803000.0,2817000.0,  &
     2830000.0,2843000.0,2856000.0,2868000.0,2880000.0,2904000.0,2928000.0,2951000.0,2974000.0,  &
     2996000.0,3019000.0,3041000.0,3063000.0,3085000.0,3107000.0,3128000.0,3150000.0,3172000.0,  &
     3193000.0,3280000.0,3366000.0,3475000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,91),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2792000.0,2807000.0,2821000.0,  &
     2834000.0,2847000.0,2860000.0,2873000.0,2898000.0,2922000.0,2945000.0,2969000.0,2992000.0,  &
     3014000.0,3037000.0,3059000.0,3081000.0,3103000.0,3125000.0,3147000.0,3169000.0,3190000.0,  &
     3280000.0,3364000.0,3473000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,92),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2795000.0,2810000.0,  &
     2824000.0,2838000.0,2851000.0,2864000.0,2890000.0,2915000.0,2939000.0,2963000.0,2986000.0,  &
     3009000.0,3032000.0,3055000.0,3077000.0,3099000.0,3121000.0,3143000.0,3165000.0,3187000.0,  &
     3280000.0,3362000.0,3471000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,93),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2797000.0,2813000.0,  &
     2827000.0,2841000.0,2855000.0,2882000.0,2907000.0,2932000.0,2956000.0,2980000.0,3004000.0,  &
     3027000.0,3050000.0,3072000.0,3095000.0,3117000.0,3140000.0,3162000.0,3184000.0,3280000.0,  &
     3359000.0,3469000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,94),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2799000.0,  &
     2815000.0,2830000.0,2844000.0,2872000.0,2899000.0,2924000.0,2949000.0,2974000.0,2998000.0,  &
     3021000.0,3044000.0,3067000.0,3090000.0,3113000.0,3135000.0,3158000.0,3180000.0,3280000.0,  &
     3357000.0,3467000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,95),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2801000.0,  &
     2817000.0,2832000.0,2862000.0,2889000.0,2916000.0,2941000.0,2966000.0,2991000.0,3015000.0,  &
     3039000.0,3062000.0,3085000.0,3108000.0,3131000.0,3154000.0,3176000.0,3280000.0,3354000.0,  &
     3465000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,96),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     2802000.0,2819000.0,2850000.0,2879000.0,2906000.0,2933000.0,2958000.0,2984000.0,3008000.0,  &
     3032000.0,3056000.0,3080000.0,3103000.0,3126000.0,3149000.0,3172000.0,3280000.0,3351000.0,  &
     3462000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,97),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     2803000.0,2836000.0,2867000.0,2895000.0,2923000.0,2950000.0,2975000.0,3001000.0,3025000.0,  &
     3050000.0,3073000.0,3097000.0,3121000.0,3144000.0,3167000.0,3280000.0,3348000.0,3459000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,98),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,2803000.0,2838000.0,2870000.0,2900000.0,2929000.0,2957000.0,2983000.0,3009000.0,3035000.0,  &
     3060000.0,3084000.0,3108000.0,3132000.0,3156000.0,3280000.0,3340000.0,3453000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,99),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,2801000.0,2838000.0,2872000.0,2904000.0,2934000.0,2963000.0,2990000.0,3017000.0,  &
     3043000.0,3069000.0,3094000.0,3119000.0,3143000.0,3280000.0,3332000.0,3446000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,100),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,2797000.0,2837000.0,2873000.0,2906000.0,2937000.0,2967000.0,2996000.0,3023000.0,  &
     3050000.0,3077000.0,3103000.0,3128000.0,3280000.0,3322000.0,3438000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,101),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,2790000.0,2833000.0,2871000.0,2906000.0,2939000.0,2970000.0,3000000.0,  &
     3029000.0,3057000.0,3084000.0,3110000.0,3280000.0,3310000.0,3429000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,102),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,2780000.0,2826000.0,2867000.0,2905000.0,2939000.0,2972000.0,3003000.0,  &
     3033000.0,3062000.0,3090000.0,3280000.0,3297000.0,3418000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,103),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,2767000.0,2817000.0,2861000.0,2901000.0,2938000.0,2972000.0,  &
     3004000.0,3036000.0,3066000.0,3280000.0,3282000.0,3406000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,104),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,2750000.0,2806000.0,2853000.0,2895000.0,2934000.0,2970000.0,  &
     3004000.0,3037000.0,3280000.0,3264000.0,3392000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,105),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2728000.0,2790000.0,2842000.0,2887000.0,2929000.0,  &
     2967000.0,3003000.0,3280000.0,3244000.0,3377000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,106),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2701000.0,2771000.0,2828000.0,2877000.0,2921000.0,  &
     2961000.0,3280000.0,3222000.0,3359000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,107),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2666000.0,2747000.0,2810000.0,2864000.0,  &
     2911000.0,3280000.0,3195000.0,3339000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,108),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2622000.0,2718000.0,2789000.0,2847000.0,  &
     3280000.0,3165000.0,3316000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,109),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2564000.0,2683000.0,2763000.0,  &
     3280000.0,3130000.0,3290000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,110),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2481000.0,2641000.0,3280000.0,  &
     3089000.0,3260000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,111),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2335000.0,3280000.0,  &
     3040000.0,3226000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,112),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,3280000.0,2821000.0,  &
     3085000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,113),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,3280000.0,2671000.0,  &
     2998000.0/
  DATA (DefaultSteamSuperheatedEnthalpyData(i,114),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,3280000.0,2512000.0,  &
     2906000.0/
  DATA (DefaultSteamSuperheatedDensityData(i,1),i=1,DefaultNumSteamSuperheatedTemps)  &
    /4.855d-003,4.837d-003,4.767d-003,4.683d-003,4.601d-003,4.522d-003,4.446d-003,4.373d-003,4.302d-003,4.233d-003,  &
     4.167d-003,4.102d-003,4.039d-003,3.979d-003,3.920d-003,3.863d-003,3.840d-003,3.818d-003,3.796d-003,3.775d-003,  &
     3.753d-003,3.732d-003,3.711d-003,3.691d-003,3.670d-003,3.650d-003,3.630d-003,3.610d-003,3.591d-003,3.571d-003,  &
     3.562d-003,3.552d-003,3.543d-003,3.533d-003,3.524d-003,3.514d-003,3.505d-003,3.496d-003,3.487d-003,3.477d-003,  &
     3.468d-003,3.459d-003,3.450d-003,3.441d-003,3.432d-003,3.424d-003,3.415d-003,3.406d-003,3.397d-003,3.388d-003,  &
     3.380d-003,3.371d-003,3.363d-003,3.354d-003,3.346d-003,3.337d-003,3.329d-003,3.321d-003,3.312d-003,3.304d-003,  &
     3.296d-003,3.288d-003,3.271d-003,3.255d-003,3.239d-003,3.224d-003,3.208d-003,3.193d-003,3.177d-003,3.162d-003,  &
     3.147d-003,3.132d-003,3.117d-003,3.103d-003,3.088d-003,3.074d-003,3.060d-003,3.046d-003,3.032d-003,3.018d-003,  &
     3.004d-003,2.991d-003,2.977d-003,2.964d-003,2.951d-003,2.938d-003,2.925d-003,2.893d-003,2.862d-003,2.831d-003,  &
     2.801d-003,2.772d-003,2.743d-003,2.715d-003,2.688d-003,2.661d-003,2.634d-003,2.583d-003,2.533d-003,2.486d-003,  &
     2.440d-003,2.396d-003,2.353d-003,2.312d-003,2.273d-003,2.234d-003,2.197d-003,2.162d-003,2.127d-003,2.093d-003,  &
     2.061d-003,3.542d-005,1.833d-003,1.714d-003/
  DATA (DefaultSteamSuperheatedDensityData(i,2),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,5.196d-003,5.121d-003,5.031d-003,4.943d-003,4.859d-003,4.777d-003,4.698d-003,4.622d-003,4.548d-003,4.476d-003,  &
     4.407d-003,4.340d-003,4.274d-003,4.211d-003,4.150d-003,4.126d-003,4.102d-003,4.078d-003,4.055d-003,4.032d-003,  &
     4.009d-003,3.987d-003,3.965d-003,3.943d-003,3.921d-003,3.899d-003,3.878d-003,3.857d-003,3.836d-003,3.826d-003,  &
     3.816d-003,3.806d-003,3.795d-003,3.785d-003,3.775d-003,3.765d-003,3.755d-003,3.746d-003,3.736d-003,3.726d-003,  &
     3.716d-003,3.707d-003,3.697d-003,3.687d-003,3.678d-003,3.668d-003,3.659d-003,3.650d-003,3.640d-003,3.631d-003,  &
     3.622d-003,3.612d-003,3.603d-003,3.594d-003,3.585d-003,3.576d-003,3.567d-003,3.558d-003,3.549d-003,3.541d-003,  &
     3.532d-003,3.514d-003,3.497d-003,3.480d-003,3.463d-003,3.446d-003,3.430d-003,3.413d-003,3.397d-003,3.381d-003,  &
     3.365d-003,3.349d-003,3.333d-003,3.318d-003,3.302d-003,3.287d-003,3.272d-003,3.257d-003,3.242d-003,3.228d-003,  &
     3.213d-003,3.198d-003,3.184d-003,3.170d-003,3.156d-003,3.142d-003,3.108d-003,3.074d-003,3.041d-003,3.009d-003,  &
     2.978d-003,2.947d-003,2.917d-003,2.887d-003,2.858d-003,2.830d-003,2.775d-003,2.722d-003,2.671d-003,2.621d-003,  &
     2.574d-003,2.528d-003,2.484d-003,2.442d-003,2.400d-003,2.361d-003,2.322d-003,2.285d-003,2.249d-003,2.214d-003,  &
     3.542d-005,1.969d-003,1.841d-003/
  DATA (DefaultSteamSuperheatedDensityData(i,3),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,6.802d-003,6.681d-003,6.565d-003,6.453d-003,6.344d-003,6.239d-003,6.138d-003,6.040d-003,5.944d-003,  &
     5.852d-003,5.763d-003,5.676d-003,5.592d-003,5.511d-003,5.479d-003,5.447d-003,5.416d-003,5.385d-003,5.355d-003,  &
     5.324d-003,5.295d-003,5.265d-003,5.236d-003,5.207d-003,5.178d-003,5.150d-003,5.122d-003,5.095d-003,5.081d-003,  &
     5.067d-003,5.054d-003,5.040d-003,5.027d-003,5.014d-003,5.000d-003,4.987d-003,4.974d-003,4.961d-003,4.948d-003,  &
     4.935d-003,4.922d-003,4.909d-003,4.897d-003,4.884d-003,4.871d-003,4.859d-003,4.846d-003,4.834d-003,4.822d-003,  &
     4.809d-003,4.797d-003,4.785d-003,4.773d-003,4.761d-003,4.749d-003,4.737d-003,4.725d-003,4.714d-003,4.702d-003,  &
     4.690d-003,4.667d-003,4.644d-003,4.621d-003,4.599d-003,4.577d-003,4.555d-003,4.533d-003,4.511d-003,4.490d-003,  &
     4.468d-003,4.447d-003,4.427d-003,4.406d-003,4.385d-003,4.365d-003,4.345d-003,4.325d-003,4.306d-003,4.286d-003,  &
     4.267d-003,4.247d-003,4.228d-003,4.210d-003,4.191d-003,4.172d-003,4.127d-003,4.082d-003,4.039d-003,3.996d-003,  &
     3.954d-003,3.913d-003,3.873d-003,3.834d-003,3.796d-003,3.758d-003,3.685d-003,3.614d-003,3.546d-003,3.481d-003,  &
     3.418d-003,3.357d-003,3.299d-003,3.242d-003,3.188d-003,3.135d-003,3.084d-003,3.034d-003,2.986d-003,2.940d-003,  &
     3.542d-005,2.615d-003,2.445d-003/
  DATA (DefaultSteamSuperheatedDensityData(i,4),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,9.407d-003,9.243d-003,9.084d-003,8.931d-003,8.783d-003,8.640d-003,8.502d-003,8.368d-003,8.238d-003,  &
     8.113d-003,7.991d-003,7.872d-003,7.757d-003,7.712d-003,7.668d-003,7.624d-003,7.580d-003,7.537d-003,7.495d-003,  &
     7.453d-003,7.411d-003,7.370d-003,7.330d-003,7.289d-003,7.250d-003,7.210d-003,7.172d-003,7.152d-003,7.133d-003,  &
     7.114d-003,7.095d-003,7.076d-003,7.057d-003,7.039d-003,7.020d-003,7.002d-003,6.983d-003,6.965d-003,6.947d-003,  &
     6.929d-003,6.911d-003,6.893d-003,6.875d-003,6.857d-003,6.840d-003,6.822d-003,6.805d-003,6.787d-003,6.770d-003,  &
     6.753d-003,6.736d-003,6.719d-003,6.702d-003,6.685d-003,6.668d-003,6.651d-003,6.635d-003,6.618d-003,6.602d-003,  &
     6.569d-003,6.537d-003,6.505d-003,6.473d-003,6.442d-003,6.411d-003,6.380d-003,6.350d-003,6.320d-003,6.290d-003,  &
     6.260d-003,6.231d-003,6.202d-003,6.173d-003,6.144d-003,6.116d-003,6.088d-003,6.060d-003,6.033d-003,6.006d-003,  &
     5.979d-003,5.952d-003,5.925d-003,5.899d-003,5.873d-003,5.809d-003,5.746d-003,5.685d-003,5.625d-003,5.566d-003,  &
     5.508d-003,5.452d-003,5.397d-003,5.342d-003,5.289d-003,5.186d-003,5.087d-003,4.992d-003,4.900d-003,4.811d-003,  &
     4.726d-003,4.643d-003,4.564d-003,4.487d-003,4.412d-003,4.340d-003,4.271d-003,4.203d-003,4.138d-003,3.542d-005,  &
     3.680d-003,3.442d-003/
  DATA (DefaultSteamSuperheatedDensityData(i,5),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,1.284d-002,1.262d-002,1.241d-002,1.220d-002,1.200d-002,1.181d-002,1.162d-002,1.144d-002,  &
     1.127d-002,1.110d-002,1.093d-002,1.078d-002,1.071d-002,1.065d-002,1.059d-002,1.053d-002,1.047d-002,1.041d-002,  &
     1.035d-002,1.029d-002,1.024d-002,1.018d-002,1.012d-002,1.007d-002,1.001d-002,9.961d-003,9.934d-003,9.907d-003,  &
     9.881d-003,9.855d-003,9.828d-003,9.802d-003,9.776d-003,9.750d-003,9.725d-003,9.699d-003,9.674d-003,9.649d-003,  &
     9.623d-003,9.598d-003,9.574d-003,9.549d-003,9.524d-003,9.500d-003,9.475d-003,9.451d-003,9.427d-003,9.403d-003,  &
     9.379d-003,9.355d-003,9.332d-003,9.308d-003,9.285d-003,9.261d-003,9.238d-003,9.215d-003,9.192d-003,9.170d-003,  &
     9.124d-003,9.079d-003,9.035d-003,8.991d-003,8.947d-003,8.904d-003,8.862d-003,8.819d-003,8.777d-003,8.736d-003,  &
     8.695d-003,8.654d-003,8.614d-003,8.574d-003,8.534d-003,8.495d-003,8.456d-003,8.417d-003,8.379d-003,8.341d-003,  &
     8.304d-003,8.267d-003,8.230d-003,8.193d-003,8.157d-003,8.068d-003,7.981d-003,7.896d-003,7.812d-003,7.731d-003,  &
     7.651d-003,7.572d-003,7.495d-003,7.420d-003,7.346d-003,7.203d-003,7.065d-003,6.933d-003,6.805d-003,6.682d-003,  &
     6.563d-003,6.449d-003,6.338d-003,6.231d-003,6.128d-003,6.028d-003,5.931d-003,5.838d-003,5.747d-003,3.542d-005,  &
     5.111d-003,4.781d-003/
  DATA (DefaultSteamSuperheatedDensityData(i,6),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,1.731d-002,1.702d-002,1.674d-002,1.646d-002,1.620d-002,1.594d-002,1.570d-002,1.546d-002,  &
     1.522d-002,1.500d-002,1.478d-002,1.469d-002,1.461d-002,1.452d-002,1.444d-002,1.436d-002,1.428d-002,1.420d-002,  &
     1.412d-002,1.404d-002,1.396d-002,1.389d-002,1.381d-002,1.374d-002,1.366d-002,1.362d-002,1.359d-002,1.355d-002,  &
     1.352d-002,1.348d-002,1.344d-002,1.341d-002,1.337d-002,1.334d-002,1.330d-002,1.327d-002,1.323d-002,1.320d-002,  &
     1.316d-002,1.313d-002,1.310d-002,1.306d-002,1.303d-002,1.300d-002,1.296d-002,1.293d-002,1.290d-002,1.286d-002,  &
     1.283d-002,1.280d-002,1.277d-002,1.273d-002,1.270d-002,1.267d-002,1.264d-002,1.261d-002,1.258d-002,1.251d-002,  &
     1.245d-002,1.239d-002,1.233d-002,1.227d-002,1.221d-002,1.215d-002,1.210d-002,1.204d-002,1.198d-002,1.192d-002,  &
     1.187d-002,1.181d-002,1.176d-002,1.170d-002,1.165d-002,1.160d-002,1.154d-002,1.149d-002,1.144d-002,1.139d-002,  &
     1.134d-002,1.129d-002,1.124d-002,1.119d-002,1.107d-002,1.095d-002,1.083d-002,1.071d-002,1.060d-002,1.049d-002,  &
     1.038d-002,1.028d-002,1.018d-002,1.007d-002,9.879d-003,9.690d-003,9.508d-003,9.333d-003,9.164d-003,9.001d-003,  &
     8.844d-003,8.692d-003,8.546d-003,8.404d-003,8.267d-003,8.134d-003,8.006d-003,7.881d-003,3.542d-005,7.009d-003,  &
     6.556d-003/
  DATA (DefaultSteamSuperheatedDensityData(i,7),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,2.307d-002,2.269d-002,2.232d-002,2.196d-002,2.161d-002,2.128d-002,2.095d-002,  &
     2.063d-002,2.033d-002,2.003d-002,1.991d-002,1.980d-002,1.968d-002,1.957d-002,1.946d-002,1.935d-002,1.924d-002,  &
     1.913d-002,1.903d-002,1.892d-002,1.882d-002,1.872d-002,1.862d-002,1.851d-002,1.846d-002,1.842d-002,1.837d-002,  &
     1.832d-002,1.827d-002,1.822d-002,1.817d-002,1.812d-002,1.808d-002,1.803d-002,1.798d-002,1.793d-002,1.789d-002,  &
     1.784d-002,1.779d-002,1.775d-002,1.770d-002,1.766d-002,1.761d-002,1.757d-002,1.752d-002,1.748d-002,1.743d-002,  &
     1.739d-002,1.734d-002,1.730d-002,1.726d-002,1.721d-002,1.717d-002,1.713d-002,1.708d-002,1.704d-002,1.696d-002,  &
     1.687d-002,1.679d-002,1.671d-002,1.663d-002,1.655d-002,1.647d-002,1.639d-002,1.631d-002,1.624d-002,1.616d-002,  &
     1.608d-002,1.601d-002,1.593d-002,1.586d-002,1.579d-002,1.572d-002,1.564d-002,1.557d-002,1.550d-002,1.543d-002,  &
     1.536d-002,1.530d-002,1.523d-002,1.516d-002,1.499d-002,1.483d-002,1.467d-002,1.452d-002,1.437d-002,1.422d-002,  &
     1.407d-002,1.393d-002,1.379d-002,1.365d-002,1.339d-002,1.313d-002,1.288d-002,1.265d-002,1.242d-002,1.220d-002,  &
     1.198d-002,1.178d-002,1.158d-002,1.139d-002,1.120d-002,1.102d-002,1.085d-002,1.068d-002,3.542d-005,9.498d-003,  &
     8.884d-003/
  DATA (DefaultSteamSuperheatedDensityData(i,8),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,3.042d-002,2.992d-002,2.943d-002,2.897d-002,2.851d-002,2.808d-002,2.765d-002,  &
     2.724d-002,2.684d-002,2.669d-002,2.653d-002,2.638d-002,2.623d-002,2.608d-002,2.593d-002,2.579d-002,2.564d-002,  &
     2.550d-002,2.536d-002,2.522d-002,2.508d-002,2.494d-002,2.481d-002,2.474d-002,2.468d-002,2.461d-002,2.454d-002,  &
     2.448d-002,2.441d-002,2.435d-002,2.428d-002,2.422d-002,2.416d-002,2.409d-002,2.403d-002,2.397d-002,2.391d-002,  &
     2.384d-002,2.378d-002,2.372d-002,2.366d-002,2.360d-002,2.354d-002,2.348d-002,2.342d-002,2.336d-002,2.330d-002,  &
     2.324d-002,2.318d-002,2.312d-002,2.306d-002,2.301d-002,2.295d-002,2.289d-002,2.284d-002,2.272d-002,2.261d-002,  &
     2.250d-002,2.239d-002,2.228d-002,2.217d-002,2.207d-002,2.196d-002,2.186d-002,2.175d-002,2.165d-002,2.155d-002,  &
     2.145d-002,2.135d-002,2.125d-002,2.115d-002,2.106d-002,2.096d-002,2.087d-002,2.077d-002,2.068d-002,2.059d-002,  &
     2.049d-002,2.040d-002,2.031d-002,2.009d-002,1.987d-002,1.966d-002,1.945d-002,1.925d-002,1.905d-002,1.885d-002,  &
     1.866d-002,1.848d-002,1.829d-002,1.794d-002,1.759d-002,1.726d-002,1.694d-002,1.664d-002,1.634d-002,1.606d-002,  &
     1.578d-002,1.552d-002,1.526d-002,1.501d-002,1.477d-002,1.453d-002,1.431d-002,3.542d-005,1.273d-002,1.190d-002/
  DATA (DefaultSteamSuperheatedDensityData(i,9),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,3.967d-002,3.903d-002,3.841d-002,3.781d-002,3.723d-002,3.666d-002,  &
     3.612d-002,3.559d-002,3.538d-002,3.518d-002,3.497d-002,3.477d-002,3.457d-002,3.438d-002,3.419d-002,3.399d-002,  &
     3.380d-002,3.362d-002,3.343d-002,3.325d-002,3.307d-002,3.289d-002,3.280d-002,3.271d-002,3.262d-002,3.254d-002,  &
     3.245d-002,3.236d-002,3.228d-002,3.219d-002,3.211d-002,3.202d-002,3.194d-002,3.186d-002,3.177d-002,3.169d-002,  &
     3.161d-002,3.153d-002,3.144d-002,3.136d-002,3.128d-002,3.120d-002,3.112d-002,3.104d-002,3.096d-002,3.089d-002,  &
     3.081d-002,3.073d-002,3.065d-002,3.058d-002,3.050d-002,3.042d-002,3.035d-002,3.027d-002,3.012d-002,2.997d-002,  &
     2.983d-002,2.968d-002,2.954d-002,2.939d-002,2.925d-002,2.911d-002,2.897d-002,2.884d-002,2.870d-002,2.857d-002,  &
     2.843d-002,2.830d-002,2.817d-002,2.804d-002,2.791d-002,2.778d-002,2.766d-002,2.753d-002,2.741d-002,2.729d-002,  &
     2.716d-002,2.704d-002,2.692d-002,2.663d-002,2.634d-002,2.606d-002,2.579d-002,2.552d-002,2.525d-002,2.499d-002,  &
     2.474d-002,2.449d-002,2.425d-002,2.377d-002,2.332d-002,2.288d-002,2.246d-002,2.205d-002,2.166d-002,2.128d-002,  &
     2.092d-002,2.057d-002,2.022d-002,1.989d-002,1.957d-002,1.927d-002,1.897d-002,3.542d-005,1.687d-002,1.578d-002/
  DATA (DefaultSteamSuperheatedDensityData(i,10),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,5.124d-002,5.042d-002,4.963d-002,4.887d-002,4.812d-002,4.741d-002,  &
     4.671d-002,4.644d-002,4.617d-002,4.590d-002,4.564d-002,4.537d-002,4.512d-002,4.486d-002,4.461d-002,4.436d-002,  &
     4.412d-002,4.387d-002,4.363d-002,4.340d-002,4.316d-002,4.304d-002,4.293d-002,4.281d-002,4.270d-002,4.258d-002,  &
     4.247d-002,4.236d-002,4.225d-002,4.213d-002,4.202d-002,4.191d-002,4.180d-002,4.169d-002,4.158d-002,4.148d-002,  &
     4.137d-002,4.126d-002,4.116d-002,4.105d-002,4.094d-002,4.084d-002,4.073d-002,4.063d-002,4.053d-002,4.043d-002,  &
     4.032d-002,4.022d-002,4.012d-002,4.002d-002,3.992d-002,3.982d-002,3.972d-002,3.952d-002,3.933d-002,3.914d-002,  &
     3.895d-002,3.876d-002,3.857d-002,3.838d-002,3.820d-002,3.802d-002,3.784d-002,3.766d-002,3.748d-002,3.731d-002,  &
     3.713d-002,3.696d-002,3.679d-002,3.662d-002,3.646d-002,3.629d-002,3.613d-002,3.596d-002,3.580d-002,3.564d-002,  &
     3.548d-002,3.533d-002,3.494d-002,3.456d-002,3.419d-002,3.383d-002,3.348d-002,3.313d-002,3.279d-002,3.246d-002,  &
     3.213d-002,3.181d-002,3.119d-002,3.059d-002,3.002d-002,2.947d-002,2.893d-002,2.842d-002,2.792d-002,2.744d-002,  &
     2.698d-002,2.653d-002,2.610d-002,2.568d-002,2.528d-002,2.488d-002,3.542d-005,2.213d-002,2.070d-002/
  DATA (DefaultSteamSuperheatedDensityData(i,11),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,6.556d-002,6.453d-002,6.353d-002,6.256d-002,6.163d-002,  &
     6.072d-002,6.036d-002,6.001d-002,5.966d-002,5.932d-002,5.898d-002,5.864d-002,5.831d-002,5.799d-002,5.766d-002,  &
     5.734d-002,5.702d-002,5.671d-002,5.640d-002,5.610d-002,5.594d-002,5.579d-002,5.564d-002,5.549d-002,5.535d-002,  &
     5.520d-002,5.505d-002,5.490d-002,5.476d-002,5.461d-002,5.447d-002,5.433d-002,5.419d-002,5.404d-002,5.390d-002,  &
     5.376d-002,5.362d-002,5.349d-002,5.335d-002,5.321d-002,5.307d-002,5.294d-002,5.280d-002,5.267d-002,5.254d-002,  &
     5.240d-002,5.227d-002,5.214d-002,5.201d-002,5.188d-002,5.175d-002,5.162d-002,5.136d-002,5.111d-002,5.086d-002,  &
     5.061d-002,5.036d-002,5.012d-002,4.988d-002,4.964d-002,4.940d-002,4.917d-002,4.894d-002,4.871d-002,4.848d-002,  &
     4.825d-002,4.803d-002,4.781d-002,4.759d-002,4.737d-002,4.716d-002,4.694d-002,4.673d-002,4.652d-002,4.632d-002,  &
     4.611d-002,4.591d-002,4.540d-002,4.491d-002,4.443d-002,4.396d-002,4.350d-002,4.305d-002,4.261d-002,4.218d-002,  &
     4.175d-002,4.134d-002,4.053d-002,3.975d-002,3.901d-002,3.829d-002,3.759d-002,3.693d-002,3.628d-002,3.566d-002,  &
     3.506d-002,3.448d-002,3.391d-002,3.337d-002,3.284d-002,3.233d-002,3.542d-005,2.875d-002,2.689d-002/
  DATA (DefaultSteamSuperheatedDensityData(i,12),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,8.315d-002,8.185d-002,8.060d-002,7.939d-002,  &
     7.821d-002,7.775d-002,7.730d-002,7.685d-002,7.641d-002,7.597d-002,7.553d-002,7.511d-002,7.468d-002,7.426d-002,  &
     7.385d-002,7.344d-002,7.304d-002,7.264d-002,7.224d-002,7.205d-002,7.185d-002,7.166d-002,7.147d-002,7.128d-002,  &
     7.108d-002,7.090d-002,7.071d-002,7.052d-002,7.033d-002,7.015d-002,6.996d-002,6.978d-002,6.960d-002,6.942d-002,  &
     6.923d-002,6.906d-002,6.888d-002,6.870d-002,6.852d-002,6.835d-002,6.817d-002,6.800d-002,6.782d-002,6.765d-002,  &
     6.748d-002,6.731d-002,6.714d-002,6.697d-002,6.680d-002,6.664d-002,6.647d-002,6.614d-002,6.581d-002,6.549d-002,  &
     6.517d-002,6.485d-002,6.454d-002,6.423d-002,6.392d-002,6.361d-002,6.331d-002,6.301d-002,6.272d-002,6.242d-002,  &
     6.213d-002,6.185d-002,6.156d-002,6.128d-002,6.100d-002,6.072d-002,6.044d-002,6.017d-002,5.990d-002,5.963d-002,  &
     5.937d-002,5.911d-002,5.846d-002,5.783d-002,5.721d-002,5.660d-002,5.601d-002,5.543d-002,5.486d-002,5.430d-002,  &
     5.375d-002,5.322d-002,5.218d-002,5.118d-002,5.022d-002,4.929d-002,4.840d-002,4.754d-002,4.671d-002,4.591d-002,  &
     4.513d-002,4.438d-002,4.366d-002,4.296d-002,4.228d-002,4.162d-002,3.542d-005,3.701d-002,3.462d-002/
  DATA (DefaultSteamSuperheatedDensityData(i,13),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.10460,0.10290,0.10140,9.988d-002,9.929d-002,  &
     9.871d-002,9.813d-002,9.757d-002,9.700d-002,9.645d-002,9.590d-002,9.536d-002,9.482d-002,9.430d-002,9.377d-002,  &
     9.325d-002,9.274d-002,9.224d-002,9.199d-002,9.174d-002,9.149d-002,9.124d-002,9.100d-002,9.075d-002,9.051d-002,  &
     9.027d-002,9.003d-002,8.979d-002,8.955d-002,8.932d-002,8.908d-002,8.885d-002,8.862d-002,8.839d-002,8.816d-002,  &
     8.793d-002,8.770d-002,8.747d-002,8.725d-002,8.703d-002,8.680d-002,8.658d-002,8.636d-002,8.614d-002,8.592d-002,  &
     8.571d-002,8.549d-002,8.528d-002,8.506d-002,8.485d-002,8.443d-002,8.401d-002,8.360d-002,8.319d-002,8.278d-002,  &
     8.238d-002,8.198d-002,8.159d-002,8.120d-002,8.081d-002,8.043d-002,8.005d-002,7.968d-002,7.931d-002,7.894d-002,  &
     7.857d-002,7.821d-002,7.786d-002,7.750d-002,7.715d-002,7.680d-002,7.646d-002,7.611d-002,7.578d-002,7.544d-002,  &
     7.461d-002,7.380d-002,7.301d-002,7.224d-002,7.148d-002,7.074d-002,7.001d-002,6.930d-002,6.860d-002,6.792d-002,  &
     6.659d-002,6.532d-002,6.409d-002,6.291d-002,6.177d-002,6.067d-002,5.961d-002,5.859d-002,5.760d-002,5.664d-002,  &
     5.572d-002,5.482d-002,5.395d-002,5.312d-002,3.542d-005,4.724d-002,4.418d-002/
  DATA (DefaultSteamSuperheatedDensityData(i,14),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.13040,0.12840,0.12650,0.12580,0.125,  &
     0.12430,0.12360,0.12290,0.12220,0.12150,0.12080,0.12010,0.11940,0.11870,0.11810,0.11740,0.11680,0.11650,0.11620,  &
     0.11580,0.11550,0.11520,0.11490,0.11460,0.11430,0.114,0.11370,0.11340,0.11310,0.11280,0.11250,0.11220,0.11190,  &
     0.11160,0.11130,0.111,0.11080,0.11050,0.11020,0.10990,0.10960,0.10930,0.10910,0.10880,0.10850,0.10820,0.108,  &
     0.10770,0.10740,0.10690,0.10640,0.10580,0.10530,0.10480,0.10430,0.10380,0.10330,0.10280,0.10230,0.10180,0.10130,  &
     0.10090,0.10040,9.993d-002,9.946d-002,9.901d-002,9.855d-002,9.810d-002,9.766d-002,9.722d-002,9.678d-002,9.635d-002,  &
     9.592d-002,9.549d-002,9.444d-002,9.342d-002,9.242d-002,9.144d-002,9.048d-002,8.954d-002,8.862d-002,8.771d-002,  &
     8.683d-002,8.597d-002,8.429d-002,8.267d-002,8.112d-002,7.962d-002,7.818d-002,7.678d-002,7.544d-002,7.415d-002,  &
     7.289d-002,7.168d-002,7.051d-002,6.938d-002,6.828d-002,6.722d-002,3.542d-005,5.978d-002,5.591d-002/
  DATA (DefaultSteamSuperheatedDensityData(i,15),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.16150,0.159,0.15810,0.15710,  &
     0.15620,0.15530,0.15440,0.15350,0.15260,0.15180,0.15090,0.15,0.14920,0.14840,0.14760,0.14670,0.14630,0.14590,  &
     0.14550,0.14520,0.14480,0.14440,0.144,0.14360,0.14320,0.14280,0.14250,0.14210,0.14170,0.14130,0.141,0.14060,  &
     0.14020,0.13990,0.13950,0.13910,0.13880,0.13840,0.13810,0.13770,0.13730,0.137,0.13660,0.13630,0.136,0.13560,  &
     0.13530,0.13490,0.13430,0.13360,0.13290,0.13230,0.13160,0.131,0.13040,0.12970,0.12910,0.12850,0.12790,0.12730,  &
     0.12670,0.12610,0.12550,0.12490,0.12430,0.12380,0.12320,0.12260,0.12210,0.12150,0.121,0.12050,0.11990,0.11860,  &
     0.11730,0.11610,0.11480,0.11360,0.11240,0.11130,0.11010,0.109,0.10790,0.10580,0.10380,0.10190,9.997d-002,9.816d-002,  &
     9.641d-002,9.473d-002,9.310d-002,9.152d-002,9.000d-002,8.853d-002,8.711d-002,8.573d-002,8.440d-002,3.542d-005,  &
     7.505d-002,7.019d-002/
  DATA (DefaultSteamSuperheatedDensityData(i,16),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.19840,0.19720,0.19610,  &
     0.19490,0.19370,0.19260,0.19150,0.19040,0.18930,0.18820,0.18720,0.18610,0.18510,0.184,0.183,0.18250,0.182,  &
     0.18150,0.181,0.18050,0.18,0.17960,0.17910,0.17860,0.17810,0.17760,0.17720,0.17670,0.17620,0.17580,0.17530,  &
     0.17480,0.17440,0.17390,0.17350,0.173,0.17260,0.17210,0.17170,0.17120,0.17080,0.17040,0.16990,0.16950,0.16910,  &
     0.16870,0.16820,0.16740,0.16660,0.16570,0.16490,0.16410,0.16330,0.16250,0.16170,0.16090,0.16020,0.15940,0.15870,  &
     0.15790,0.15720,0.15640,0.15570,0.155,0.15430,0.15360,0.15290,0.15220,0.15150,0.15080,0.15010,0.14950,0.14780,  &
     0.14620,0.14460,0.14310,0.14160,0.14010,0.13870,0.13730,0.13590,0.13450,0.13190,0.12940,0.12690,0.12460,0.12230,  &
     0.12010,0.118,0.116,0.11410,0.11220,0.11030,0.10850,0.10680,0.10520,3.542d-005,9.352d-002,8.746d-002/
  DATA (DefaultSteamSuperheatedDensityData(i,17),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.21510,0.21380,0.21250,  &
     0.21130,0.21,0.20880,0.20760,0.20640,0.20520,0.204,0.20290,0.20180,0.20060,0.19950,0.199,0.19840,0.19790,  &
     0.19730,0.19680,0.19630,0.19570,0.19520,0.19470,0.19420,0.19360,0.19310,0.19260,0.19210,0.19160,0.19110,0.19060,  &
     0.19010,0.18960,0.18910,0.18860,0.18810,0.18760,0.18720,0.18670,0.18620,0.18570,0.18520,0.18480,0.18430,0.18380,  &
     0.18340,0.18250,0.18150,0.18060,0.17980,0.17890,0.178,0.17710,0.17630,0.17540,0.17460,0.17380,0.17290,0.17210,  &
     0.17130,0.17050,0.16970,0.16890,0.16820,0.16740,0.16660,0.16590,0.16510,0.16440,0.16360,0.16290,0.16110,0.15940,  &
     0.15770,0.156,0.15430,0.15270,0.15110,0.14960,0.14810,0.14660,0.14370,0.141,0.13830,0.13580,0.13330,0.13090,  &
     0.12860,0.12640,0.12430,0.12220,0.12020,0.11830,0.11640,0.11460,3.542d-005,0.10190,9.531d-002/
  DATA (DefaultSteamSuperheatedDensityData(i,18),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.23290,0.23150,  &
     0.23010,0.22870,0.22740,0.22610,0.22480,0.22350,0.22220,0.221,0.21970,0.21850,0.21730,0.21670,0.21610,0.21550,  &
     0.21490,0.21430,0.21370,0.21310,0.21260,0.212,0.21140,0.21090,0.21030,0.20970,0.20920,0.20860,0.20810,0.20750,  &
     0.207,0.20640,0.20590,0.20540,0.20480,0.20430,0.20380,0.20330,0.20270,0.20220,0.20170,0.20120,0.20070,0.20020,  &
     0.19970,0.19870,0.19770,0.19670,0.19570,0.19480,0.19380,0.19290,0.19190,0.191,0.19010,0.18920,0.18830,0.18740,  &
     0.18650,0.18560,0.18480,0.18390,0.18310,0.18220,0.18140,0.18060,0.17980,0.179,0.17820,0.17740,0.17540,0.17350,  &
     0.17160,0.16980,0.168,0.16630,0.16450,0.16290,0.16120,0.15960,0.15650,0.15350,0.15060,0.14780,0.14510,0.14250,  &
     0.14,0.13760,0.13530,0.133,0.13090,0.12880,0.12670,0.12480,3.542d-005,0.11090,0.1037/
  DATA (DefaultSteamSuperheatedDensityData(i,19),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.25180,  &
     0.25030,0.24890,0.24740,0.246,0.24450,0.24310,0.24170,0.24040,0.239,0.23770,0.23640,0.23570,0.23510,0.23440,  &
     0.23380,0.23310,0.23250,0.23190,0.23120,0.23060,0.23,0.22940,0.22880,0.22810,0.22750,0.22690,0.22630,0.22570,  &
     0.22510,0.22460,0.224,0.22340,0.22280,0.22220,0.22160,0.22110,0.22050,0.21990,0.21940,0.21880,0.21830,0.21770,  &
     0.21720,0.21610,0.215,0.21390,0.21290,0.21180,0.21080,0.20970,0.20870,0.20770,0.20670,0.20570,0.20480,0.20380,  &
     0.20280,0.20190,0.201,0.2,0.19910,0.19820,0.19730,0.19640,0.19550,0.19460,0.19370,0.19290,0.19080,0.18870,  &
     0.18660,0.18470,0.18270,0.18080,0.17890,0.17710,0.17530,0.17360,0.17020,0.16690,0.16370,0.16070,0.15780,0.155,  &
     0.15230,0.14960,0.14710,0.14470,0.14230,0.14,0.13780,0.13560,3.542d-005,0.12060,0.1128/
  DATA (DefaultSteamSuperheatedDensityData(i,20),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.27210,0.27050,0.26890,0.26730,0.26580,0.26420,0.26270,0.26120,0.25970,0.25830,0.25680,0.25610,0.25540,0.25470,  &
     0.254,0.25330,0.25260,0.25190,0.25130,0.25060,0.24990,0.24920,0.24860,0.24790,0.24720,0.24660,0.24590,0.24530,  &
     0.24460,0.244,0.24330,0.24270,0.24210,0.24140,0.24080,0.24020,0.23960,0.239,0.23840,0.23770,0.23710,0.23650,  &
     0.23590,0.23480,0.23360,0.23240,0.23130,0.23010,0.229,0.22790,0.22680,0.22570,0.22460,0.22350,0.22250,0.22140,  &
     0.22040,0.21930,0.21830,0.21730,0.21630,0.21530,0.21430,0.21330,0.21240,0.21140,0.21050,0.20950,0.20720,0.205,  &
     0.20270,0.20060,0.19850,0.19640,0.19440,0.19240,0.19040,0.18850,0.18480,0.18130,0.17790,0.17460,0.17140,0.16830,  &
     0.16540,0.16250,0.15980,0.15710,0.15460,0.15210,0.14970,0.14730,3.542d-005,0.131,0.1225/
  DATA (DefaultSteamSuperheatedDensityData(i,21),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.29370,0.29190,0.29020,0.28850,0.28690,0.28520,0.28360,0.282,0.28040,0.27880,0.278,0.27730,0.27650,0.27570,  &
     0.275,0.27420,0.27350,0.27270,0.272,0.27130,0.27050,0.26980,0.26910,0.26840,0.26760,0.26690,0.26620,0.26550,  &
     0.26480,0.26410,0.26340,0.26280,0.26210,0.26140,0.26070,0.26,0.25940,0.25870,0.258,0.25740,0.25670,0.25610,  &
     0.25480,0.25350,0.25220,0.251,0.24980,0.24850,0.24730,0.24610,0.24490,0.24370,0.24260,0.24140,0.24030,0.23910,  &
     0.238,0.23690,0.23580,0.23470,0.23360,0.23260,0.23150,0.23050,0.22940,0.22840,0.22740,0.22490,0.22240,0.22,  &
     0.21770,0.21540,0.21310,0.21090,0.20880,0.20660,0.20460,0.20060,0.19670,0.193,0.18940,0.186,0.18270,0.17950,  &
     0.17640,0.17340,0.17050,0.16770,0.165,0.16240,0.15990,3.542d-005,0.14210,  &
     0.1329/
  DATA (DefaultSteamSuperheatedDensityData(i,22),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.31660,0.31480,0.31290,0.31110,0.30930,0.30760,0.30580,0.30410,0.30240,0.30150,0.30070,0.29990,0.299,0.29820,  &
     0.29740,0.29660,0.29580,0.295,0.29420,0.29340,0.29260,0.29180,0.291,0.29020,0.28940,0.28870,0.28790,0.28720,  &
     0.28640,0.28560,0.28490,0.28420,0.28340,0.28270,0.282,0.28120,0.28050,0.27980,0.27910,0.27840,0.27760,0.27620,  &
     0.27490,0.27350,0.27210,0.27080,0.26940,0.26810,0.26680,0.26550,0.26430,0.263,0.26170,0.26050,0.25930,0.258,  &
     0.25680,0.25560,0.25450,0.25330,0.25210,0.251,0.24980,0.24870,0.24760,0.24650,0.24380,0.24110,0.23850,0.23590,  &
     0.23350,0.231,0.22860,0.22630,0.224,0.22170,0.21740,0.21320,0.20920,0.20530,0.20160,0.198,0.19450,0.19120,  &
     0.18790,0.18480,0.18180,0.17880,0.176,0.17330,3.542d-005,0.154,  &
     0.1441/
  DATA (DefaultSteamSuperheatedDensityData(i,23),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.34110,0.33910,0.33710,0.33520,0.33320,0.33130,0.32940,0.32760,0.32670,0.32580,0.32490,0.324,0.32310,  &
     0.32220,0.32130,0.32040,0.31950,0.31870,0.31780,0.31690,0.31610,0.31520,0.31440,0.31350,0.31270,0.31190,0.31110,  &
     0.31020,0.30940,0.30860,0.30780,0.307,0.30620,0.30540,0.30460,0.30380,0.30310,0.30230,0.30150,0.30070,0.29920,  &
     0.29770,0.29620,0.29470,0.29330,0.29180,0.29040,0.289,0.28760,0.28620,0.28480,0.28350,0.28210,0.28080,0.27950,  &
     0.27820,0.27690,0.27560,0.27430,0.27310,0.27180,0.27060,0.26930,0.26810,0.26690,0.264,0.26110,0.25830,0.25550,  &
     0.25280,0.25020,0.24760,0.245,0.24260,0.24010,0.23540,0.23090,0.22650,0.22230,0.21830,0.21440,0.21060,0.207,  &
     0.20350,0.20010,0.19680,0.19360,0.19060,0.18760,3.542d-005,0.16680,  &
     0.156/
  DATA (DefaultSteamSuperheatedDensityData(i,24),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.36710,0.36490,0.36280,0.36070,0.35860,0.35660,0.35460,0.35360,0.35260,0.35160,0.35060,0.34960,  &
     0.34870,0.34770,0.34680,0.34580,0.34490,0.34390,0.343,0.34210,0.34110,0.34020,0.33930,0.33840,0.33750,0.33660,  &
     0.33570,0.33480,0.334,0.33310,0.33220,0.33130,0.33050,0.32960,0.32880,0.32790,0.32710,0.32630,0.32540,0.32380,  &
     0.32210,0.32050,0.31890,0.31730,0.31580,0.31420,0.31270,0.31120,0.30970,0.30820,0.30670,0.30520,0.30380,0.30240,  &
     0.30090,0.29950,0.29820,0.29680,0.29540,0.29410,0.29270,0.29140,0.29010,0.28880,0.28560,0.28250,0.27940,0.27640,  &
     0.27350,0.27060,0.26780,0.26510,0.26240,0.25980,0.25460,0.24970,0.245,0.24050,0.23610,0.23190,0.22780,0.22390,  &
     0.22010,0.21640,0.21290,0.20940,0.20610,0.20290,3.542d-005,0.18040,  &
     0.1687/
  DATA (DefaultSteamSuperheatedDensityData(i,25),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.39460,0.39230,0.39010,0.38780,0.38560,0.38340,0.38230,0.38120,0.38020,0.37910,0.37810,  &
     0.377,0.376,0.37490,0.37390,0.37290,0.37190,0.37080,0.36980,0.36880,0.36780,0.36690,0.36590,0.36490,0.36390,  &
     0.363,0.362,0.361,0.36010,0.35920,0.35820,0.35730,0.35640,0.35540,0.35450,0.35360,0.35270,0.35180,0.35,  &
     0.34820,0.34650,0.34470,0.343,0.34130,0.33970,0.338,0.33640,0.33470,0.33310,0.33150,0.32990,0.32840,0.32680,  &
     0.32530,0.32380,0.32230,0.32080,0.31930,0.31780,0.31640,0.315,0.31350,0.31210,0.30870,0.30530,0.302,0.29870,  &
     0.29560,0.29250,0.28940,0.28650,0.28360,0.28070,0.27520,0.26990,0.26480,0.25990,0.25510,0.25060,0.24620,0.24190,  &
     0.23780,0.23390,0.23,0.22630,0.22270,0.21930,3.542d-005,0.19490,  &
     0.1823/
  DATA (DefaultSteamSuperheatedDensityData(i,26),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.42390,0.42140,0.419,0.41660,0.41420,0.413,0.41190,0.41070,0.40960,0.40840,0.40730,  &
     0.40610,0.405,0.40390,0.40280,0.40170,0.40060,0.39950,0.39840,0.39730,0.39630,0.39520,0.39410,0.39310,0.392,  &
     0.391,0.39,0.38890,0.38790,0.38690,0.38590,0.38490,0.38390,0.38290,0.38190,0.38090,0.37990,0.378,0.37610,  &
     0.37420,0.37230,0.37050,0.36860,0.36680,0.365,0.36320,0.36150,0.35970,0.358,0.35630,0.35460,0.35290,0.35130,  &
     0.34960,0.348,0.34640,0.34480,0.34320,0.34160,0.34010,0.33860,0.337,0.33330,0.32960,0.32610,0.32260,0.31910,  &
     0.31580,0.31250,0.30930,0.30620,0.30310,0.29710,0.29140,0.28590,0.28060,0.27540,0.27050,0.26580,0.26120,0.25680,  &
     0.25250,0.24830,0.24430,0.24050,0.23670,3.542d-005,0.21040,  &
     0.1968/
  DATA (DefaultSteamSuperheatedDensityData(i,27),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.45490,0.45230,0.44970,0.44710,0.44580,0.44450,0.44330,0.442,0.44080,0.43960,  &
     0.43830,0.43710,0.43590,0.43470,0.43350,0.43230,0.43110,0.43,0.42880,0.42760,0.42650,0.42530,0.42420,0.42310,  &
     0.42190,0.42080,0.41970,0.41860,0.41750,0.41640,0.41530,0.41420,0.41320,0.41210,0.411,0.41,0.40790,0.40580,  &
     0.40380,0.40170,0.39970,0.39770,0.39580,0.39380,0.39190,0.39,0.38810,0.38620,0.38440,0.38260,0.38080,0.379,  &
     0.37720,0.37540,0.37370,0.372,0.37030,0.36860,0.36690,0.36520,0.36360,0.35950,0.35560,0.35170,0.34790,0.34420,  &
     0.34060,0.33710,0.33360,0.33020,0.32690,0.32050,0.31430,0.30830,0.30260,0.29710,0.29180,0.28660,0.28170,0.27690,  &
     0.27230,0.26780,0.26350,0.25930,0.25530,3.542d-005,0.22690,  &
     0.2122/
  DATA (DefaultSteamSuperheatedDensityData(i,28),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.48780,0.48490,0.48210,0.48080,0.47940,0.478,0.47670,0.47530,0.474,  &
     0.47270,0.47130,0.47,0.46870,0.46740,0.46620,0.46490,0.46360,0.46230,0.46110,0.45980,0.45860,0.45740,0.45610,  &
     0.45490,0.45370,0.45250,0.45130,0.45010,0.44890,0.44780,0.44660,0.44540,0.44430,0.44310,0.442,0.43970,0.43750,  &
     0.43530,0.43310,0.43090,0.42870,0.42660,0.42450,0.42240,0.42040,0.41830,0.41630,0.41430,0.41240,0.41040,0.40850,  &
     0.40650,0.40460,0.40280,0.40090,0.39910,0.39720,0.39540,0.39360,0.39190,0.38750,0.38320,0.37910,0.375,0.371,  &
     0.36710,0.36330,0.35950,0.35590,0.35230,0.34530,0.33870,0.33230,0.32610,0.32010,0.31440,0.30890,0.30350,0.29840,  &
     0.29340,0.28860,0.28390,0.27940,0.27510,3.542d-005,0.24450,  &
     0.2287/
  DATA (DefaultSteamSuperheatedDensityData(i,29),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.52250,0.51950,0.518,0.51650,0.51510,0.51360,0.51210,0.51070,  &
     0.50920,0.50780,0.50640,0.505,0.50360,0.50220,0.50080,0.49940,0.49810,0.49670,0.49540,0.494,0.49270,0.49140,  &
     0.49010,0.48870,0.48740,0.48610,0.48490,0.48360,0.48230,0.481,0.47980,0.47850,0.47730,0.47610,0.47360,0.47120,  &
     0.46880,0.46640,0.46410,0.46180,0.45950,0.45720,0.455,0.45270,0.45050,0.44840,0.44620,0.44410,0.442,0.43990,  &
     0.43780,0.43580,0.43370,0.43170,0.42970,0.42780,0.42580,0.42390,0.422,0.41730,0.41270,0.40820,0.40380,0.39950,  &
     0.39530,0.39110,0.38710,0.38320,0.37930,0.37180,0.36460,0.35770,0.35110,0.34460,0.33850,0.33250,0.32680,0.32120,  &
     0.31590,0.31070,0.30570,0.30080,0.29610,3.542d-005,0.26320,  &
     0.2461/
  DATA (DefaultSteamSuperheatedDensityData(i,30),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.55930,0.55770,0.55610,0.55450,0.55290,0.55130,0.54980,0.54820,  &
     0.54670,0.54510,0.54360,0.54210,0.54060,0.53910,0.53760,0.53610,0.53460,0.53320,0.53170,0.53030,0.52890,0.52740,  &
     0.526,0.52460,0.52320,0.52180,0.52050,0.51910,0.51770,0.51640,0.515,0.51370,0.51230,0.50970,0.50710,0.50450,  &
     0.50190,0.49940,0.49690,0.49440,0.492,0.48960,0.48720,0.48480,0.48240,0.48010,0.47780,0.47550,0.47330,0.47110,  &
     0.46880,0.46670,0.46450,0.46230,0.46020,0.45810,0.456,0.454,0.44890,0.44390,0.43910,0.43440,0.42970,0.42520,  &
     0.42080,0.41640,0.41220,0.408,0.4,0.39220,0.38480,0.37760,0.37070,0.36410,0.35760,0.35150,0.34550,0.33970,  &
     0.33410,0.32870,0.32350,0.31850,3.542d-005,0.28310,  &
     0.2647/
  DATA (DefaultSteamSuperheatedDensityData(i,31),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.57850,0.57680,0.57510,0.57350,0.57180,0.57020,0.56860,  &
     0.567,0.56540,0.56380,0.56220,0.56070,0.55910,0.55760,0.556,0.55450,0.553,0.55150,0.55,0.54850,0.547,  &
     0.54550,0.54410,0.54260,0.54120,0.53980,0.53830,0.53690,0.53550,0.53410,0.53270,0.53130,0.52860,0.52590,0.52320,  &
     0.52050,0.51790,0.51530,0.51270,0.51020,0.50770,0.50520,0.50270,0.50030,0.49790,0.49550,0.49310,0.49080,0.48850,  &
     0.48620,0.48390,0.48160,0.47940,0.47720,0.475,0.47290,0.47070,0.46550,0.46030,0.45530,0.45040,0.44560,0.44090,  &
     0.43630,0.43180,0.42740,0.423,0.41470,0.40660,0.39890,0.39150,0.38430,0.37740,0.37080,0.36440,0.35820,0.35220,  &
     0.34640,0.34080,0.33540,0.33020,3.542d-005,0.29350,  &
     0.2744/
  DATA (DefaultSteamSuperheatedDensityData(i,32),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.59820,0.59640,0.59470,0.593,0.59130,0.58960,  &
     0.588,0.58630,0.58470,0.583,0.58140,0.57980,0.57820,0.57660,0.575,0.57340,0.57180,0.57030,0.56870,0.56720,  &
     0.56570,0.56420,0.56270,0.56120,0.55970,0.55820,0.55670,0.55520,0.55380,0.55230,0.55090,0.548,0.54520,0.54240,  &
     0.53970,0.53690,0.53420,0.53160,0.52890,0.52630,0.52370,0.52120,0.51870,0.51620,0.51370,0.51120,0.50880,0.50640,  &
     0.504,0.50170,0.49930,0.497,0.49470,0.49250,0.49020,0.488,0.48250,0.47720,0.472,0.46690,0.46190,0.457,  &
     0.45220,0.44760,0.443,0.43850,0.42980,0.42150,0.41350,0.40580,0.39840,0.39120,0.38430,0.37770,0.37130,0.36510,  &
     0.35910,0.35330,0.34760,0.34220,3.542d-005,0.30420,  &
     0.2844/
  DATA (DefaultSteamSuperheatedDensityData(i,33),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.61840,0.61660,0.61480,0.61310,0.61130,  &
     0.60960,0.60790,0.60620,0.60450,0.60280,0.60110,0.59940,0.59780,0.59610,0.59450,0.59280,0.59120,0.58960,0.588,  &
     0.58640,0.58490,0.58330,0.58170,0.58020,0.57860,0.57710,0.57560,0.57410,0.57260,0.57110,0.56810,0.56520,0.56230,  &
     0.55940,0.55660,0.55380,0.551,0.54830,0.54560,0.54290,0.54020,0.53760,0.535,0.53240,0.52990,0.52740,0.52490,  &
     0.52240,0.52,0.51750,0.51510,0.51280,0.51040,0.50810,0.50580,0.50010,0.49460,0.48920,0.48390,0.47870,0.47360,  &
     0.46870,0.46390,0.45910,0.45450,0.44550,0.43680,0.42850,0.42050,0.41290,0.40540,0.39830,0.39140,0.38470,0.37830,  &
     0.37210,0.36610,0.36030,0.35460,3.542d-005,0.31520,  &
     0.2948/
  DATA (DefaultSteamSuperheatedDensityData(i,34),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.63920,0.63740,0.63550,0.63370,0.63190,  &
     0.63010,0.62830,0.62660,0.62480,0.623,0.62130,0.61960,0.61790,0.61620,0.61450,0.61280,0.61110,0.60950,0.60780,  &
     0.60620,0.60460,0.60290,0.60130,0.59970,0.59810,0.59660,0.595,0.59340,0.59190,0.58880,0.58580,0.58270,0.57980,  &
     0.57680,0.57390,0.571,0.56820,0.56540,0.56260,0.55990,0.55710,0.55440,0.55180,0.54910,0.54650,0.54390,0.54140,  &
     0.53880,0.53630,0.53380,0.53140,0.52890,0.52650,0.52410,0.51820,0.51250,0.50690,0.50140,0.496,0.49080,0.48570,  &
     0.48060,0.47570,0.47090,0.46160,0.45260,0.444,0.43570,0.42780,0.42010,0.41270,0.40550,0.39860,0.392,0.38550,  &
     0.37930,0.37330,0.36740,3.542d-005,0.32660,  &
     0.3054/
  DATA (DefaultSteamSuperheatedDensityData(i,35),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.66060,0.65870,0.65680,0.65490,  &
     0.653,0.65120,0.64930,0.64750,0.64570,0.64390,0.64210,0.64030,0.63850,0.63680,0.635,0.63330,0.63160,0.62990,  &
     0.62820,0.62650,0.62480,0.62310,0.62150,0.61980,0.61820,0.61650,0.61490,0.61330,0.61010,0.607,0.60380,0.60070,  &
     0.59770,0.59470,0.59170,0.58870,0.58580,0.58290,0.58010,0.57720,0.57440,0.57170,0.56890,0.56620,0.56350,0.56090,  &
     0.55820,0.55560,0.55310,0.55050,0.548,0.54550,0.543,0.53690,0.53090,0.52510,0.51940,0.51390,0.50840,0.50310,  &
     0.49790,0.49280,0.48780,0.47820,0.46890,0.46,0.45140,0.44310,0.43510,0.42750,0.42010,0.41290,0.406,0.39930,  &
     0.39290,0.38660,0.38060,3.542d-005,0.33830,  &
     0.3163/
  DATA (DefaultSteamSuperheatedDensityData(i,36),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.68250,0.68050,0.67860,  &
     0.67660,0.67470,0.67280,0.67090,0.669,0.66710,0.66530,0.66340,0.66160,0.65980,0.658,0.65620,0.65440,0.65260,  &
     0.65080,0.64910,0.64730,0.64560,0.64390,0.64210,0.64040,0.63870,0.63710,0.63540,0.63210,0.62880,0.62550,0.62230,  &
     0.61920,0.616,0.61290,0.60990,0.60690,0.60390,0.60090,0.598,0.59510,0.59220,0.58930,0.58650,0.58370,0.581,  &
     0.57830,0.57560,0.57290,0.57020,0.56760,0.565,0.56240,0.55610,0.54990,0.54390,0.538,0.53230,0.52660,0.52110,  &
     0.51570,0.51040,0.50530,0.49520,0.48560,0.47640,0.46750,0.45890,0.45070,0.44270,0.435,0.42760,0.42050,0.41360,  &
     0.40690,0.40040,0.39410,3.542d-005,0.35030,  &
     0.3276/
  DATA (DefaultSteamSuperheatedDensityData(i,37),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.705,0.703,  &
     0.701,0.699,0.697,0.695,0.69310,0.69110,0.68920,0.68730,0.68530,0.68350,0.68160,0.67970,0.67780,0.676,  &
     0.67420,0.67230,0.67050,0.66870,0.66690,0.66510,0.66340,0.66160,0.65990,0.65810,0.65470,0.65130,0.64790,0.64460,  &
     0.64130,0.63810,0.63480,0.63170,0.62850,0.62540,0.62230,0.61930,0.61630,0.61330,0.61040,0.60740,0.60460,0.60170,  &
     0.59890,0.59610,0.59330,0.59050,0.58780,0.58510,0.58250,0.57590,0.56950,0.56330,0.55710,0.55120,0.54530,0.53960,  &
     0.534,0.52860,0.52320,0.51280,0.50280,0.49330,0.484,0.47520,0.46660,0.45840,0.45050,0.44280,0.43540,0.42820,  &
     0.42130,0.41460,0.40810,3.542d-005,0.36270,  &
     0.3391/
  DATA (DefaultSteamSuperheatedDensityData(i,38),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.72820,0.72610,  &
     0.724,0.72190,0.71990,0.71780,0.71580,0.71380,0.71180,0.70980,0.70790,0.70590,0.704,0.702,0.70010,0.69820,  &
     0.69630,0.69440,0.69250,0.69070,0.68880,0.687,0.68520,0.68340,0.68160,0.678,0.67450,0.671,0.66750,0.66410,  &
     0.66070,0.65740,0.65410,0.65080,0.64760,0.64440,0.64130,0.63810,0.63510,0.632,0.629,0.626,0.623,0.62010,  &
     0.61720,0.61430,0.61150,0.60860,0.60580,0.60310,0.59630,0.58960,0.58320,0.57680,0.57060,0.56460,0.55870,0.55290,  &
     0.54720,0.54170,0.53090,0.52060,0.51060,0.50110,0.49190,0.48310,0.47450,0.46630,0.45840,0.45070,0.44330,0.43610,  &
     0.42920,0.42240,3.542d-005,0.37540,  &
     0.3511/
  DATA (DefaultSteamSuperheatedDensityData(i,39),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.75190,  &
     0.74970,0.74760,0.74550,0.74330,0.74120,0.73920,0.73710,0.735,0.733,0.73090,0.72890,0.72690,0.72490,0.723,  &
     0.721,0.719,0.71710,0.71520,0.71320,0.71130,0.70940,0.70760,0.70570,0.702,0.69830,0.69470,0.69110,0.68760,  &
     0.68410,0.68060,0.67720,0.67380,0.67050,0.66720,0.66390,0.66060,0.65740,0.65430,0.65110,0.648,0.645,0.64190,  &
     0.63890,0.63590,0.633,0.63010,0.62720,0.62430,0.61730,0.61040,0.60370,0.59710,0.59070,0.58440,0.57830,0.57230,  &
     0.56640,0.56070,0.54950,0.53880,0.52850,0.51870,0.50910,0.5,0.49120,0.48260,0.47440,0.46650,0.45880,0.45140,  &
     0.44420,0.43720,3.542d-005,0.38860,  &
     0.3633/
  DATA (DefaultSteamSuperheatedDensityData(i,40),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.77630,0.774,0.77180,0.76960,0.76740,0.76530,0.76310,0.761,0.75890,0.75670,0.75470,0.75260,0.75050,0.74840,  &
     0.74640,0.74440,0.74240,0.74040,0.73840,0.73640,0.73440,0.73250,0.73050,0.72670,0.72290,0.71910,0.71540,0.71170,  &
     0.70810,0.70450,0.701,0.69750,0.694,0.69060,0.68720,0.68380,0.68050,0.67720,0.674,0.67070,0.66760,0.66440,  &
     0.66130,0.65820,0.65510,0.65210,0.64910,0.64610,0.63880,0.63170,0.62480,0.618,0.61130,0.60480,0.59850,0.59230,  &
     0.58620,0.58020,0.56870,0.55760,0.547,0.53670,0.52690,0.51740,0.50820,0.49940,0.49090,0.48270,0.47470,0.46710,  &
     0.45960,0.45240,3.542d-005,0.40210,  &
     0.3759/
  DATA (DefaultSteamSuperheatedDensityData(i,41),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.80130,0.799,0.79670,0.79440,0.79220,0.78990,0.78770,0.78550,0.78330,0.78110,0.779,0.77680,0.77470,0.77260,  &
     0.77050,0.76840,0.76630,0.76420,0.76220,0.76010,0.75810,0.75610,0.75210,0.74820,0.74430,0.74040,0.73660,0.73280,  &
     0.72910,0.72540,0.72180,0.71820,0.71470,0.71110,0.70770,0.70420,0.70080,0.69740,0.69410,0.69080,0.68750,0.68430,  &
     0.68110,0.67790,0.67480,0.67170,0.66860,0.661,0.65370,0.64650,0.63940,0.63250,0.62580,0.61920,0.61280,0.60650,  &
     0.60030,0.58840,0.57690,0.56590,0.55530,0.54510,0.53530,0.52580,0.51670,0.50790,0.49940,0.49110,0.48320,0.47550,  &
     0.468,3.542d-005,0.41590,  &
     0.3889/
  DATA (DefaultSteamSuperheatedDensityData(i,42),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.82690,0.82460,0.82220,0.81990,0.81750,0.81520,0.81290,0.81070,0.80840,0.80620,0.80390,0.80170,0.79950,  &
     0.79730,0.79520,0.793,0.79090,0.78870,0.78660,0.78450,0.78240,0.77830,0.77420,0.77010,0.76610,0.76220,0.75830,  &
     0.75440,0.75060,0.74690,0.74310,0.73940,0.73580,0.73220,0.72860,0.72510,0.72160,0.71810,0.71470,0.71130,0.708,  &
     0.70470,0.70140,0.69810,0.69490,0.69170,0.68390,0.67630,0.66880,0.66150,0.65440,0.64740,0.64060,0.63390,0.62740,  &
     0.621,0.60870,0.59680,0.58540,0.57440,0.56390,0.55370,0.54390,0.53450,0.52530,0.51650,0.508,0.49980,0.49180,  &
     0.48410,3.542d-005,0.43020,  &
     0.4023/
  DATA (DefaultSteamSuperheatedDensityData(i,43),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.85320,0.85080,0.84840,0.846,0.84360,0.84120,0.83880,0.83650,0.83410,0.83180,0.82950,0.82730,  &
     0.825,0.82270,0.82050,0.81830,0.81610,0.81390,0.81170,0.80950,0.80520,0.801,0.79680,0.79260,0.78850,0.78450,  &
     0.78050,0.77650,0.77260,0.76880,0.76490,0.76120,0.75740,0.75370,0.75010,0.74650,0.74290,0.73930,0.73580,0.73240,  &
     0.72890,0.72550,0.72210,0.71880,0.71550,0.70740,0.69950,0.69180,0.68420,0.67680,0.66960,0.66260,0.65570,0.64890,  &
     0.64230,0.62950,0.61720,0.60540,0.59410,0.58310,0.57260,0.56250,0.55270,0.54330,0.53420,0.52540,0.51690,0.50860,  &
     0.50060,3.542d-005,0.44490,  &
     0.416/
  DATA (DefaultSteamSuperheatedDensityData(i,44),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.88020,0.87770,0.87520,0.87270,0.87030,0.86780,0.86540,0.86290,0.86050,0.85820,0.85580,  &
     0.85340,0.85110,0.84880,0.84650,0.84420,0.84190,0.83960,0.83740,0.83290,0.82850,0.82420,0.81990,0.81560,0.81140,  &
     0.80730,0.80320,0.79920,0.79510,0.79120,0.78730,0.78340,0.77960,0.77580,0.772,0.76830,0.76460,0.761,0.75740,  &
     0.75390,0.75030,0.74680,0.74340,0.74,0.73160,0.72340,0.71540,0.70760,0.69990,0.69240,0.68510,0.678,0.671,  &
     0.66420,0.65090,0.63820,0.626,0.61430,0.603,0.59210,0.58160,0.57150,0.56170,0.55230,0.54320,0.53440,0.52590,  &
     0.51760,3.542d-005,0.46,  &
     0.4301/
  DATA (DefaultSteamSuperheatedDensityData(i,45),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.90790,0.90530,0.90270,0.90020,0.89760,0.89510,0.89260,0.89010,0.88760,0.88520,0.88270,  &
     0.88030,0.87790,0.87550,0.87310,0.87070,0.86840,0.86610,0.86140,0.85690,0.85240,0.84790,0.84350,0.83920,0.83490,  &
     0.83060,0.82640,0.82230,0.81820,0.81410,0.81010,0.80610,0.80220,0.79830,0.79450,0.79070,0.78690,0.78320,0.77950,  &
     0.77590,0.77220,0.76870,0.76510,0.75640,0.74790,0.73970,0.73160,0.72370,0.71590,0.70840,0.701,0.69380,0.68670,  &
     0.673,0.65980,0.64720,0.635,0.62340,0.61210,0.60130,0.59080,0.58070,0.571,0.56150,0.55240,0.54360,0.53510,  &
     3.542d-005,0.47550,  &
     0.4446/
  DATA (DefaultSteamSuperheatedDensityData(i,46),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.93630,0.93360,0.931,0.92830,0.92570,0.92310,0.92050,0.91790,0.91540,0.91280,  &
     0.91030,0.90780,0.90530,0.90290,0.90040,0.898,0.89560,0.89080,0.886,0.88140,0.87680,0.87220,0.86770,0.86320,  &
     0.85880,0.85450,0.85020,0.84590,0.84170,0.83760,0.83340,0.82940,0.82540,0.82140,0.81740,0.81350,0.80970,0.80590,  &
     0.80210,0.79840,0.79460,0.791,0.782,0.77320,0.76460,0.75620,0.74810,0.74010,0.73220,0.72460,0.71710,0.70980,  &
     0.69560,0.682,0.66890,0.65640,0.64430,0.63270,0.62150,0.61060,0.60020,0.59010,0.58040,0.571,0.56190,0.553,  &
     3.542d-005,0.49140,  &
     0.4594/
  DATA (DefaultSteamSuperheatedDensityData(i,47),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.96540,0.96260,0.95990,0.95720,0.95450,0.95180,0.94910,0.94650,0.94380,  &
     0.94120,0.93860,0.93610,0.93350,0.93090,0.92840,0.92590,0.92090,0.916,0.91120,0.90640,0.90170,0.897,0.89240,  &
     0.88780,0.88330,0.87890,0.87450,0.87010,0.86580,0.86150,0.85730,0.85320,0.849,0.845,0.84090,0.83690,0.833,  &
     0.82910,0.82520,0.82140,0.81760,0.80830,0.79920,0.79030,0.78160,0.77310,0.76490,0.75680,0.74890,0.74110,0.73360,  &
     0.71890,0.70480,0.69130,0.67830,0.66580,0.65380,0.64220,0.631,0.62020,0.60980,0.59970,0.59,0.58060,0.57150,  &
     3.542d-005,0.50780,  &
     0.4747/
  DATA (DefaultSteamSuperheatedDensityData(i,48),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.99520,0.99240,0.98950,0.98670,0.984,0.98120,0.97840,0.97570,  &
     0.973,0.97030,0.96760,0.965,0.96230,0.95970,0.95710,0.952,0.94690,0.94190,0.93690,0.932,0.92720,0.92240,  &
     0.91770,0.913,0.90840,0.90380,0.89930,0.89480,0.89040,0.88610,0.88170,0.87750,0.87320,0.86910,0.86490,0.86080,  &
     0.85680,0.85280,0.84880,0.84490,0.83520,0.82580,0.81670,0.80770,0.79890,0.79040,0.782,0.77380,0.76580,0.758,  &
     0.74280,0.72830,0.71430,0.70090,0.68790,0.67550,0.66350,0.652,0.64080,0.63,0.61960,0.60960,0.59980,0.59040,  &
     3.542d-005,0.52460,  &
     0.4905/
  DATA (DefaultSteamSuperheatedDensityData(i,49),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.026,1.023,1.02,1.017,1.014,1.011,1.008,1.006,  &
     1.003,1.0,0.99740,0.99460,0.99190,0.98920,0.98390,0.97860,0.97340,0.96830,0.96320,0.95820,0.95320,0.94830,0.94350,  &
     0.93870,0.934,0.92930,0.92470,0.92010,0.91560,0.91110,0.90670,0.90230,0.898,0.89370,0.88950,0.88530,0.88110,  &
     0.877,0.873,0.863,0.85330,0.84380,0.83450,0.82540,0.81660,0.80790,0.79940,0.79120,0.78310,0.76740,0.75230,  &
     0.73790,0.724,0.71060,0.69780,0.68540,0.67350,0.66190,0.65080,0.64010,0.62970,0.61960,0.60990,3.542d-005,0.54180,  &
     0.5066/
  DATA (DefaultSteamSuperheatedDensityData(i,50),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.057,1.054,1.051,1.048,1.045,1.042,1.039,  &
     1.036,1.034,1.031,1.028,1.025,1.022,1.017,1.011,1.006,1.0,0.99520,0.99,0.98490,0.97980,0.97480,  &
     0.96990,0.965,0.96010,0.95530,0.95060,0.94590,0.94130,0.93670,0.93220,0.92770,0.92330,0.91890,0.91460,0.91030,  &
     0.906,0.90180,0.89150,0.88140,0.87160,0.862,0.85260,0.84350,0.83450,0.82580,0.81720,0.80880,0.79260,0.77710,  &
     0.76210,0.74780,0.734,0.72070,0.70790,0.69550,0.68360,0.67210,0.661,0.65030,0.63990,0.62980,3.542d-005,0.55960,  &
     0.5232/
  DATA (DefaultSteamSuperheatedDensityData(i,51),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.089,1.086,1.083,1.08,1.077,1.074,  &
     1.071,1.068,1.065,1.062,1.059,1.056,1.05,1.045,1.039,1.034,1.028,1.023,1.017,1.012,  &
     1.007,1.002,0.99680,0.99180,0.98680,0.982,0.97710,0.97230,0.96760,0.96290,0.95830,0.95370,0.94910,0.94470,  &
     0.94020,0.93580,0.93150,0.92080,0.91040,0.90020,0.89030,0.88060,0.87110,0.86190,0.85280,0.844,0.83530,0.81850,  &
     0.80250,0.787,0.77220,0.75790,0.74420,0.731,0.71820,0.70590,0.694,0.68260,0.67150,0.66070,0.65030,3.542d-005,  &
     0.57780,  &
     0.5402/
  DATA (DefaultSteamSuperheatedDensityData(i,52),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.122,1.119,1.116,1.113,1.109,  &
     1.106,1.103,1.1,1.097,1.094,1.091,1.085,1.079,1.073,1.068,1.062,1.056,1.051,1.045,  &
     1.04,1.035,1.03,1.024,1.019,1.014,1.009,1.004,0.99930,0.99440,0.98960,0.98490,0.98020,0.97560,  &
     0.971,0.96640,0.96190,0.95090,0.94010,0.92960,0.91930,0.90930,0.89950,0.88990,0.88060,0.87140,0.86250,0.84510,  &
     0.82850,0.81260,0.79730,0.78250,0.76830,0.75470,0.74150,0.72880,0.71650,0.70470,0.69320,0.68210,0.67140,3.542d-005,  &
     0.59640,  &
     0.5576/
  DATA (DefaultSteamSuperheatedDensityData(i,53),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.156,1.152,1.149,1.146,1.143,  &
     1.139,1.136,1.133,1.13,1.127,1.121,1.115,1.109,1.103,1.097,1.091,1.085,1.08,1.074,  &
     1.069,1.063,1.058,1.052,1.047,1.042,1.037,1.032,1.027,1.022,1.017,1.012,1.007,1.003,  &
     0.99790,0.99320,0.98180,0.97060,0.95970,0.94910,0.93880,0.92860,0.91880,0.90910,0.89960,0.89040,0.87250,0.85530,  &
     0.83880,0.823,0.80780,0.79310,0.779,0.76540,0.75230,0.73960,0.72740,0.71550,0.70410,0.693,3.542d-005,0.61560,  &
     0.5755/
  DATA (DefaultSteamSuperheatedDensityData(i,54),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.19,1.187,1.183,1.18,  &
     1.177,1.173,1.17,1.167,1.164,1.157,1.151,1.145,1.139,1.133,1.127,1.121,1.115,1.109,  &
     1.103,1.098,1.092,1.087,1.081,1.076,1.071,1.065,1.06,1.055,1.05,1.045,1.04,1.035,  &
     1.03,1.025,1.013,1.002,0.99070,0.97970,0.969,0.95860,0.94840,0.93840,0.92860,0.919,0.90050,0.88280,  &
     0.86580,0.84940,0.83370,0.81860,0.804,0.78990,0.77640,0.76330,0.75070,0.73840,0.72660,0.71520,3.542d-005,0.63530,  &
     0.5939/
  DATA (DefaultSteamSuperheatedDensityData(i,55),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.226,1.222,1.219,  &
     1.215,1.212,1.208,1.205,1.202,1.195,1.188,1.182,1.176,1.169,1.163,1.157,1.151,1.145,  &
     1.139,1.133,1.127,1.122,1.116,1.111,1.105,1.1,1.094,1.089,1.084,1.079,1.073,1.068,  &
     1.063,1.058,1.046,1.034,1.023,1.011,1.0,0.98930,0.97870,0.96840,0.95830,0.94840,0.92930,0.911,0.89340,  &
     0.87650,0.86030,0.84470,0.82960,0.81510,0.80110,0.78760,0.77460,0.76190,0.74970,0.73790,3.542d-005,0.65550,  &
     0.6128/
  DATA (DefaultSteamSuperheatedDensityData(i,56),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.262,1.258,  &
     1.254,1.251,1.247,1.244,1.24,1.234,1.227,1.22,1.213,1.207,1.201,1.194,1.188,1.182,  &
     1.176,1.17,1.164,1.158,1.152,1.146,1.141,1.135,1.129,1.124,1.118,1.113,1.108,1.102,  &
     1.097,1.092,1.08,1.067,1.055,1.043,1.032,1.021,1.01,0.99920,0.98880,0.97860,0.95890,0.93990,  &
     0.92180,0.90440,0.88760,0.87150,0.85590,0.84090,0.82650,0.81260,0.79910,0.78610,0.77350,0.76130,3.542d-005,0.67620,  &
     0.6321/
  DATA (DefaultSteamSuperheatedDensityData(i,57),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.299,1.295,  &
     1.291,1.288,1.284,1.28,1.273,1.266,1.259,1.252,1.246,1.239,1.232,1.226,1.22,1.213,  &
     1.207,1.201,1.195,1.189,1.183,1.177,1.171,1.165,1.16,1.154,1.149,1.143,1.138,1.132,  &
     1.127,1.114,1.101,1.089,1.077,1.065,1.053,1.042,1.031,1.02,1.01,0.98920,0.96960,0.95090,  &
     0.93290,0.91560,0.89890,0.88290,0.86740,0.85250,0.83810,0.82420,0.81080,0.79780,0.78520,3.542d-005,0.69740,  &
     0.652/
  DATA (DefaultSteamSuperheatedDensityData(i,58),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.337,  &
     1.333,1.329,1.325,1.321,1.314,1.307,1.3,1.292,1.285,1.279,1.272,1.265,1.258,1.252,  &
     1.245,1.239,1.233,1.227,1.22,1.214,1.208,1.202,1.196,1.191,1.185,1.179,1.174,1.168,  &
     1.163,1.149,1.136,1.123,1.111,1.098,1.086,1.075,1.063,1.052,1.041,1.02,1.0,0.98080,0.96220,  &
     0.94430,0.92710,0.91060,0.89460,0.87920,0.86440,0.85,0.83620,0.82280,0.80980,3.542d-005,0.7192,0.6723/
  DATA (DefaultSteamSuperheatedDensityData(i,59),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     1.375,1.371,1.367,1.364,1.356,1.348,1.341,1.334,1.326,1.319,1.312,1.305,1.298,1.292,  &
     1.285,1.278,1.272,1.265,1.259,1.253,1.246,1.24,1.234,1.228,1.222,1.216,1.211,1.205,  &
     1.199,1.185,1.172,1.158,1.145,1.133,1.12,1.108,1.097,1.085,1.074,1.052,1.031,1.011,  &
     0.99220,0.97380,0.956,0.939,0.92250,0.90660,0.89130,0.87650,0.86220,0.84840,0.835,3.542d-005,0.7416,0.6932/
  DATA (DefaultSteamSuperheatedDensityData(i,60),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,1.415,1.411,1.407,1.399,1.391,1.383,1.376,1.368,1.361,1.354,1.346,1.339,1.332,1.325,  &
     1.319,1.312,1.305,1.299,1.292,1.286,1.279,1.273,1.267,1.261,1.255,1.249,1.243,1.237,  &
     1.222,1.208,1.195,1.181,1.168,1.155,1.143,1.131,1.119,1.107,1.085,1.063,1.043,1.023,  &
     1.004,0.98570,0.96810,0.95110,0.93470,0.91890,0.90360,0.88890,0.87460,0.86080,3.542d-005,0.7645,0.7146/
  DATA (DefaultSteamSuperheatedDensityData(i,61),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,1.455,1.451,1.443,1.435,1.427,1.419,1.411,1.404,1.396,1.389,1.381,1.374,1.367,  &
     1.36,1.353,1.346,1.339,1.332,1.326,1.319,1.313,1.306,1.3,1.294,1.287,1.281,1.275,  &
     1.26,1.246,1.232,1.218,1.204,1.191,1.178,1.166,1.154,1.142,1.118,1.096,1.075,1.055,  &
     1.035,1.016,0.99790,0.98040,0.96350,0.94720,0.93140,0.91620,0.90150,0.88730,3.542d-005,0.7879,0.7365/
  DATA (DefaultSteamSuperheatedDensityData(i,62),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,1.497,1.488,1.48,1.472,1.464,1.456,1.448,1.44,1.432,1.425,1.417,1.41,  &
     1.402,1.395,1.388,1.381,1.374,1.367,1.36,1.354,1.347,1.34,1.334,1.327,1.321,1.315,  &
     1.299,1.284,1.27,1.255,1.242,1.228,1.215,1.202,1.189,1.177,1.153,1.13,1.108,1.087,  &
     1.067,1.047,1.028,1.01,0.993,0.97620,0.95990,0.94420,0.92910,0.91440,3.542d-005,0.812,0.759/
  DATA (DefaultSteamSuperheatedDensityData(i,63),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,1.583,1.574,1.565,1.556,1.548,1.539,1.531,1.522,1.514,1.506,1.498,  &
     1.49,1.483,1.475,1.468,1.46,1.453,1.445,1.438,1.431,1.424,1.417,1.41,1.404,1.397,  &
     1.38,1.364,1.349,1.334,1.319,1.304,1.29,1.276,1.263,1.25,1.224,1.2,1.177,1.154,  &
     1.133,1.112,1.092,1.073,1.054,1.036,1.019,1.002,0.98630,0.97070,3.542d-005,0.8619,0.8056/
  DATA (DefaultSteamSuperheatedDensityData(i,64),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,1.673,1.663,1.654,1.644,1.635,1.626,1.617,1.609,1.6,1.592,1.583,  &
     1.575,1.567,1.559,1.551,1.543,1.535,1.527,1.52,1.512,1.505,1.498,1.49,1.483,1.466,  &
     1.449,1.432,1.416,1.4,1.385,1.37,1.355,1.341,1.327,1.299,1.273,1.249,1.225,1.202,  &
     1.18,1.159,1.138,1.119,1.1,1.081,1.063,1.046,1.03,3.542d-005,0.9143,0.8546/
  DATA (DefaultSteamSuperheatedDensityData(i,65),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,1.766,1.756,1.746,1.737,1.727,1.717,1.708,1.699,1.69,1.681,  &
     1.672,1.663,1.655,1.646,1.638,1.629,1.621,1.613,1.605,1.597,1.589,1.582,1.574,1.555,  &
     1.537,1.519,1.502,1.485,1.469,1.453,1.437,1.422,1.407,1.378,1.351,1.324,1.299,1.274,  &
     1.251,1.229,1.207,1.186,1.166,1.146,1.128,1.109,1.092,3.542d-005,0.9692,0.9059/
  DATA (DefaultSteamSuperheatedDensityData(i,66),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.864,1.854,1.843,1.833,1.823,1.813,1.803,1.793,1.784,  &
     1.774,1.765,1.755,1.746,1.737,1.729,1.72,1.711,1.703,1.694,1.686,1.678,1.669,1.649,  &
     1.63,1.611,1.593,1.575,1.557,1.54,1.524,1.507,1.492,1.461,1.432,1.403,1.377,1.351,  &
     1.326,1.302,1.279,1.257,1.235,1.215,1.195,1.175,1.157,3.542d-005,1.027,0.9597/
  DATA (DefaultSteamSuperheatedDensityData(i,67),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1.967,1.955,1.944,1.933,1.923,1.912,1.902,1.891,  &
     1.881,1.871,1.861,1.852,1.842,1.833,1.823,1.814,1.805,1.796,1.787,1.778,1.77,1.748,  &
     1.728,1.707,1.688,1.669,1.65,1.632,1.614,1.597,1.58,1.548,1.516,1.487,1.458,1.431,  &
     1.404,1.379,1.354,1.331,1.308,1.286,1.265,1.245,1.225,3.542d-005,1.087,1.016/
  DATA (DefaultSteamSuperheatedDensityData(i,68),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.074,2.062,2.05,2.038,2.027,2.016,2.005,1.994,  &
     1.983,1.973,1.962,1.952,1.942,1.932,1.922,1.912,1.903,1.893,1.884,1.875,1.852,1.83,  &
     1.809,1.788,1.767,1.748,1.728,1.709,1.691,1.673,1.639,1.605,1.574,1.543,1.514,1.486,  &
     1.459,1.434,1.409,1.384,1.361,1.339,1.317,1.296,3.542d-005,1.15,1.075/
  DATA (DefaultSteamSuperheatedDensityData(i,69),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.185,2.172,2.16,2.148,2.136,2.124,2.112,  &
     2.101,2.09,2.079,2.068,2.057,2.046,2.036,2.025,2.015,2.005,1.995,1.985,1.961,1.937,  &
     1.915,1.892,1.871,1.85,1.829,1.809,1.79,1.771,1.734,1.699,1.665,1.633,1.602,1.572,  &
     1.544,1.516,1.49,1.464,1.44,1.416,1.393,1.371,3.542d-005,1.216,1.137/
  DATA (DefaultSteamSuperheatedDensityData(i,70),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.301,2.288,2.275,2.262,2.249,2.237,  &
     2.225,2.213,2.201,2.189,2.177,2.166,2.155,2.144,2.133,2.122,2.111,2.101,2.075,2.05,  &
     2.026,2.002,1.979,1.957,1.935,1.914,1.893,1.873,1.834,1.796,1.761,1.727,1.694,1.662,  &
     1.632,1.603,1.575,1.548,1.522,1.497,1.473,1.449,3.542d-005,1.286,1.201/
  DATA (DefaultSteamSuperheatedDensityData(i,71),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.422,2.408,2.394,2.381,2.367,  &
     2.354,2.341,2.329,2.316,2.304,2.292,2.28,2.268,2.256,2.245,2.233,2.222,2.195,2.168,  &
     2.142,2.117,2.093,2.069,2.046,2.023,2.001,1.98,1.938,1.899,1.861,1.825,1.79,1.757,  &
     1.725,1.694,1.664,1.635,1.608,1.581,1.556,1.531,3.542d-005,1.358,1.269/
  DATA (DefaultSteamSuperheatedDensityData(i,72),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.548,2.533,2.519,2.505,2.491,  &
     2.477,2.463,2.45,2.437,2.424,2.411,2.398,2.386,2.373,2.361,2.349,2.32,2.292,2.264,  &
     2.238,2.212,2.186,2.162,2.138,2.114,2.091,2.048,2.006,1.965,1.927,1.89,1.855,1.821,  &
     1.789,1.757,1.727,1.698,1.67,1.642,1.616,3.542d-005,1.433,1.339/
  DATA (DefaultSteamSuperheatedDensityData(i,73),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.679,2.664,2.648,2.633,  &
     2.619,2.604,2.59,2.576,2.562,2.548,2.535,2.522,2.508,2.495,2.483,2.452,2.421,2.392,  &
     2.364,2.336,2.309,2.283,2.258,2.233,2.209,2.162,2.117,2.075,2.034,1.995,1.958,1.922,  &
     1.888,1.854,1.822,1.792,1.762,1.733,1.705,3.542d-005,1.512,1.413/
  DATA (DefaultSteamSuperheatedDensityData(i,74),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.816,2.8,2.783,  &
     2.768,2.752,2.737,2.722,2.707,2.692,2.678,2.664,2.65,2.636,2.622,2.589,2.557,2.526,  &
     2.496,2.466,2.438,2.41,2.383,2.357,2.331,2.282,2.234,2.189,2.146,2.105,2.066,2.028,  &
     1.991,1.956,1.922,1.89,1.858,1.828,1.799,3.542d-005,1.595,1.490/
  DATA (DefaultSteamSuperheatedDensityData(i,75),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.958,2.941,  &
     2.924,2.907,2.891,2.875,2.859,2.843,2.828,2.813,2.798,2.783,2.769,2.733,2.699,2.666,  &
     2.634,2.603,2.572,2.543,2.514,2.486,2.459,2.407,2.357,2.309,2.263,2.22,2.178,2.138,  &
     2.099,2.062,2.026,1.992,1.959,1.927,1.896,3.542d-005,1.681,1.570/
  DATA (DefaultSteamSuperheatedDensityData(i,76),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,3.106,3.088,  &
     3.07,3.052,3.035,3.018,3.001,2.985,2.969,2.953,2.937,2.922,2.884,2.848,2.812,2.778,  &
     2.745,2.713,2.682,2.651,2.622,2.593,2.537,2.484,2.434,2.386,2.34,2.295,2.253,2.212,  &
     2.173,2.135,2.099,2.064,2.03,1.997,3.542d-005,1.77,1.654/
  DATA (DefaultSteamSuperheatedDensityData(i,77),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,3.26,  &
     3.24,3.222,3.203,3.185,3.167,3.15,3.132,3.115,3.099,3.082,3.042,3.003,2.966,2.929,  &
     2.894,2.86,2.827,2.794,2.763,2.732,2.674,2.618,2.564,2.513,2.465,2.418,2.373,2.33,  &
     2.289,2.249,2.21,2.173,2.138,2.103,3.542d-005,1.864,1.741/
  DATA (DefaultSteamSuperheatedDensityData(i,78),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     3.419,3.399,3.379,3.36,3.341,3.322,3.304,3.286,3.268,3.25,3.207,3.166,3.126,3.087,  &
     3.05,3.014,2.978,2.944,2.911,2.878,2.816,2.757,2.7,2.646,2.595,2.546,2.498,2.453,  &
     2.409,2.367,2.326,2.287,2.25,2.213,3.542d-005,1.961,1.832/
  DATA (DefaultSteamSuperheatedDensityData(i,79),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,3.585,3.564,3.543,3.523,3.503,3.483,3.464,3.445,3.426,3.38,3.336,3.294,3.253,3.213,  &
     3.174,3.137,3.1,3.065,3.031,2.965,2.902,2.842,2.785,2.731,2.679,2.629,2.581,2.535,  &
     2.49,2.448,2.406,2.367,2.328,3.542d-005,2.063,1.926/
  DATA (DefaultSteamSuperheatedDensityData(i,80),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,3.758,3.735,3.713,3.692,3.671,3.65,3.63,3.61,3.561,3.514,3.469,3.425,3.383,  &
     3.342,3.302,3.264,3.226,3.19,3.12,3.054,2.99,2.93,2.873,2.818,2.765,2.714,2.665,  &
     2.619,2.574,2.53,2.488,2.448,3.542d-005,2.168,2.025/
  DATA (DefaultSteamSuperheatedDensityData(i,81),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,3.937,3.913,3.89,3.867,3.845,3.823,3.802,3.75,3.7,3.652,3.605,3.561,  &
     3.517,3.475,3.434,3.394,3.356,3.282,3.212,3.145,3.081,3.02,2.962,2.907,2.853,2.802,  &
     2.752,2.705,2.659,2.615,2.573,3.542d-005,2.278,2.127/
  DATA (DefaultSteamSuperheatedDensityData(i,82),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,4.122,4.097,4.073,4.049,4.026,4.003,3.948,3.895,3.843,3.794,3.746,  &
     3.7,3.655,3.612,3.57,3.529,3.451,3.376,3.306,3.238,3.174,3.113,3.054,2.998,2.944,  &
     2.892,2.842,2.794,2.747,2.702,3.542d-005,2.392,2.234/
  DATA (DefaultSteamSuperheatedDensityData(i,83),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,4.315,4.289,4.263,4.238,4.214,4.155,4.098,4.043,3.991,3.94,3.891,  &
     3.843,3.797,3.753,3.709,3.627,3.548,3.473,3.402,3.335,3.27,3.208,3.148,3.091,3.037,  &
     2.984,2.933,2.884,2.837,3.542d-005,2.511,2.344/
  DATA (DefaultSteamSuperheatedDensityData(i,84),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,4.515,4.487,4.46,4.434,4.371,4.31,4.252,4.196,4.142,4.09,  &
     4.04,3.991,3.944,3.898,3.81,3.727,3.648,3.573,3.501,3.433,3.368,3.305,3.245,3.187,  &
     3.132,3.079,3.027,2.977,3.542d-005,2.635,2.459/
  DATA (DefaultSteamSuperheatedDensityData(i,85),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,4.722,4.693,4.665,4.597,4.532,4.47,4.411,4.353,4.298,  &
     4.244,4.193,4.143,4.094,4.001,3.913,3.83,3.751,3.675,3.603,3.534,3.468,3.405,3.344,  &
     3.286,3.23,3.176,3.123,3.542d-005,2.763,2.579/
  DATA (DefaultSteamSuperheatedDensityData(i,86),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,4.936,4.906,4.833,4.764,4.698,4.635,4.574,4.515,  &
     4.458,4.403,4.35,4.298,4.2,4.107,4.019,3.935,3.856,3.78,3.707,3.638,3.571,3.507,  &
     3.446,3.387,3.33,3.275,3.542d-005,2.896,2.703/
  DATA (DefaultSteamSuperheatedDensityData(i,87),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,5.159,5.081,5.007,4.936,4.868,4.803,4.741,4.681,  &
     4.622,4.566,4.512,4.407,4.309,4.216,4.128,4.044,3.964,3.887,3.814,3.744,3.677,3.612,  &
     3.55,3.49,3.432,3.542d-005,3.035,2.832/
  DATA (DefaultSteamSuperheatedDensityData(i,88),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,5.75,5.662,5.579,5.499,5.423,5.35,5.28,  &
     5.212,5.147,5.084,4.964,4.851,4.744,4.643,4.547,4.456,4.369,4.286,4.206,4.13,4.056,  &
     3.986,3.918,3.853,3.542d-005,3.404,3.176/
  DATA (DefaultSteamSuperheatedDensityData(i,89),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,6.395,6.296,6.202,6.112,6.027,5.945,  &
     5.866,5.79,5.717,5.579,5.449,5.327,5.211,5.102,4.998,4.898,4.804,4.714,4.627,4.544,  &
     4.464,4.388,4.314,3.542d-005,3.808,3.552/
  DATA (DefaultSteamSuperheatedDensityData(i,90),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,7.098,6.985,6.879,6.779,6.683,  &
     6.591,6.503,6.418,6.258,6.108,5.968,5.836,5.711,5.593,5.48,5.373,5.27,5.172,5.078,  &
     4.988,4.902,4.819,3.542d-005,4.25,3.962/
  DATA (DefaultSteamSuperheatedDensityData(i,91),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,7.861,7.734,7.615,7.502,7.395,  &
     7.292,7.193,7.008,6.835,6.674,6.523,6.38,6.245,6.118,5.996,5.88,5.769,5.663,5.561,  &
     5.464,5.37,3.542d-005,4.732,4.410/
  DATA (DefaultSteamSuperheatedDensityData(i,92),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,8.69,8.547,8.413,8.286,  &
     8.166,8.051,7.835,7.636,7.451,7.278,7.115,6.961,6.816,6.678,6.547,6.421,6.302,6.187,  &
     6.078,5.972,3.542d-005,5.257,4.897/
  DATA (DefaultSteamSuperheatedDensityData(i,93),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,9.588,9.428,9.277,  &
     9.135,9.0,8.749,8.519,8.305,8.106,7.92,7.745,7.58,7.423,7.275,7.133,6.998,6.87,6.746,  &
     6.628,3.542d-005,5.827,5.425/
  DATA (DefaultSteamSuperheatedDensityData(i,94),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,10.56,10.38,  &
     10.21,10.05,9.759,9.491,9.244,9.016,8.803,8.603,8.415,8.238,8.069,7.91,7.758,7.613,  &
     7.474,7.341,3.542d-005,6.445,5.998/
  DATA (DefaultSteamSuperheatedDensityData(i,95),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,11.62,  &
     11.41,11.22,10.88,10.56,10.28,10.01,9.769,9.541,9.328,9.126,8.936,8.756,8.584,8.421,  &
     8.265,8.116,3.542d-005,7.115,6.618/
  DATA (DefaultSteamSuperheatedDensityData(i,96),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,12.75,  &
     12.53,12.11,11.75,11.41,11.11,10.83,10.57,10.32,10.1,9.88,9.676,9.483,9.299,  &
     9.124,8.957,3.542d-005,7.84,7.288/
  DATA (DefaultSteamSuperheatedDensityData(i,97),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     13.99,13.49,13.05,12.67,12.31,11.99,11.69,11.41,11.15,10.91,10.68,10.46,10.25,  &
     10.06,9.869,3.542d-005,8.623,8.011/
  DATA (DefaultSteamSuperheatedDensityData(i,98),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,16.75,16.12,15.58,15.1,14.66,14.26,13.9,13.56,13.25,12.95,12.67,12.41,  &
     12.16,11.93,3.542d-005,10.38,9.628/
  DATA (DefaultSteamSuperheatedDensityData(i,99),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,19.97,19.17,18.49,17.89,17.36,16.87,16.43,16.02,15.64,15.28,14.95,  &
     14.63,14.34,3.542d-005,12.42,11.5/
  DATA (DefaultSteamSuperheatedDensityData(i,100),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,23.71,22.7,21.85,21.1,20.45,19.85,19.31,18.81,18.35,17.93,17.53,  &
     17.15,3.542d-005,14.77,13.65/
  DATA (DefaultSteamSuperheatedDensityData(i,101),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,28.07,26.78,25.71,24.79,23.97,23.25,22.59,21.99,21.44,20.93,  &
     20.45,3.542d-005,17.48,16.12/
  DATA (DefaultSteamSuperheatedDensityData(i,102),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,33.16,31.5,30.15,29.0,28.0,27.11,26.31,25.59,24.92,24.31,  &
     3.542d-005,20.6,18.94/
  DATA (DefaultSteamSuperheatedDensityData(i,103),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,39.13,36.97,35.25,33.82,32.58,31.5,30.53,29.65,28.86,  &
     3.542d-005,24.19,22.16/
  DATA (DefaultSteamSuperheatedDensityData(i,104),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,46.17,43.33,41.13,39.33,37.8,36.47,35.29,34.24,  &
     3.542d-005,28.31,25.84/
  DATA (DefaultSteamSuperheatedDensityData(i,105),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,54.54,50.75,47.92,45.65,43.75,42.11,40.68,  &
     3.542d-005,33.07,30.03/
  DATA (DefaultSteamSuperheatedDensityData(i,106),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,64.64,59.47,55.78,52.9,50.53,48.51,3.542d-005,  &
     38.55,34.81/
  DATA (DefaultSteamSuperheatedDensityData(i,107),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,77.05,69.8,64.93,61.24,58.27,3.542d-005,  &
     44.92,40.28/
  DATA (DefaultSteamSuperheatedDensityData(i,108),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,92.76,82.18,75.63,70.87,3.542d-005,  &
     52.35,46.54/
  DATA (DefaultSteamSuperheatedDensityData(i,109),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,113.6,97.22,88.27,3.542d-005,  &
     61.12,53.76/
  DATA (DefaultSteamSuperheatedDensityData(i,110),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,143.9,115.8,3.542d-005,71.6,  &
     62.15/
  DATA (DefaultSteamSuperheatedDensityData(i,111),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,201.8,3.542d-005,84.38,  &
     71.99/
  DATA (DefaultSteamSuperheatedDensityData(i,112),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,3.542d-005,148.4,  &
     115.1/
  DATA (DefaultSteamSuperheatedDensityData(i,113),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,3.542d-005,201.7,  &
     144.2/
  DATA (DefaultSteamSuperheatedDensityData(i,114),i=1,DefaultNumSteamSuperheatedTemps)  &
    /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,  &
     0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,3.542d-005,270.9,  &
     177.8/

          ! Okay, the following is no longer strictly a DATA statement, but it is still data transfer for 0% concentration
          ! Convert mPa-s viscosity data to Pa-s
  DefaultWaterViscData = DefaultWaterViscData/1000.0
  DefaultEthGlyViscData = DefaultEthGlyViscData/1000.0
  DefaultPropGlyViscData = DefaultPropGlyViscData/1000.0

          ! Set zero concentration data
  DefaultEthGlyCpData(1,:)    = DefaultWaterCpData
  DefaultEthGlyViscData(1,:)  = DefaultWaterViscData
  DefaultEthGlyRhoData(1,:)   = DefaultWaterRhoData
  DefaultEthGlyCondData(1,:)  = DefaultWaterCondData
  DefaultPropGlyCpData(1,:)   = DefaultWaterCpData
  DefaultPropGlyViscData(1,:) = DefaultWaterViscData
  DefaultPropGlyRhoData(1,:)  = DefaultWaterRhoData
  DefaultPropGlyCondData(1,:) = DefaultWaterCondData

          ! FLOW:
  MaxAlphas=0
  MaxNumbers=0
  IF (GetNumObjectsFound('FluidProperties:Name') > 0) THEN
    CALL GetObjectDefMaxArgs('FluidProperties:Name',Status,NumAlphas,NumNumbers)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    MaxNumbers=MAX(MaxNumbers,NumNumbers)
  ENDIF
  IF (GetNumObjectsFound('FluidProperties:GlycolConcentration') > 0) THEN
    CALL GetObjectDefMaxArgs('FluidProperties:GlycolConcentration',Status,NumAlphas,NumNumbers)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    MaxNumbers=MAX(MaxNumbers,NumNumbers)
  ENDIF
  NumOfFluidTempArrays    = GetNumObjectsFound('FluidProperties:Temperatures')
  IF (NumOfFluidTempArrays > 0) THEN
    CALL GetObjectDefMaxArgs('FluidProperties:Temperatures',Status,NumAlphas,NumNumbers)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    MaxNumbers=MAX(MaxNumbers,NumNumbers)
  ENDIF
  NumOfSatFluidPropArrays = GetNumObjectsFound('FluidProperties:Saturated')
  IF (NumOfSatFluidPropArrays > 0) THEN
    CALL GetObjectDefMaxArgs('FluidProperties:Saturated',Status,NumAlphas,NumNumbers)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    MaxNumbers=MAX(MaxNumbers,NumNumbers)
  ENDIF
  NumOfSHFluidPropArrays  = GetNumObjectsFound('FluidProperties:Superheated')
  IF (NumOfSHFluidPropArrays > 0) THEN
    CALL GetObjectDefMaxArgs('FluidProperties:Superheated',Status,NumAlphas,NumNumbers)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    MaxNumbers=MAX(MaxNumbers,NumNumbers)
  ENDIF
  NumOfGlyFluidPropArrays = GetNumObjectsFound('FluidProperties:Concentration')
  IF (NumOfGlyFluidPropArrays > 0) THEN
    CALL GetObjectDefMaxArgs('FluidProperties:Concentration',Status,NumAlphas,NumNumbers)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    MaxNumbers=MAX(MaxNumbers,NumNumbers)
  ENDIF

  ALLOCATE(Alphas(MaxAlphas))
  ALLOCATE(cAlphaFieldNames(MaxAlphas))
  ALLOCATE(lAlphaFieldBlanks(MaxAlphas))

  Alphas=' '
  cAlphaFieldNames=' '
  lAlphaFIeldBlanks=.false.

  ALLOCATE(Numbers(MaxNumbers))
  ALLOCATE(cNumericFieldNames(MaxNumbers))
  ALLOCATE(lNumericFieldBlanks(MaxNumbers))

  Numbers=0.0
  cNumericFieldNames=' '
  lNumericFieldBlanks=.false.

          ! Check to see if there is any FluidName input.  If not, this is okay as
          ! long as the user only desires to simulate loops with water.  More than
          ! one FluidName input is not allowed.
  CurrentModuleObject = 'FluidProperties:Name'
  NumOfOptionalInput = GetNumObjectsFound(TRIM(CurrentModuleObject))

  ALLOCATE(FluidNames(NumOfOptionalInput))

          ! Get a count on the number of refrigerants and the number of glycols entered
          ! so that the main derived types can be allocated
  FluidNum=0
  DO Loop = 1, NumOfOptionalInput
    CALL GetObjectItem(TRIM(CurrentModuleObject),Loop,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    ErrorInName=.false.
    IsBlank=.false.
    CALL VerifyName(Alphas(1),FluidNames%Name,FluidNum,ErrorInName,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (ErrorInName) THEN
      CALL ShowContinueError('...Fluid names must be unique regardless of subtype.')
      ErrorsFound=.true.
      CYCLE
    ENDIF
    FluidNum=FluidNum+1
    FluidNames(FluidNum)%Name=Alphas(1)
    IF (SameString(Alphas(2),Refrig)) THEN
      NumOfRefrigerants = NumOfRefrigerants + 1
      FluidNames(FluidNum)%IsGlycol=.false.
    ELSEIF (SameString(Alphas(2),Glycol)) THEN
      NumOfGlycols = NumOfGlycols + 1
      FluidNames(FluidNum)%IsGlycol=.true.
    ELSE
      CALL ShowSevereError('GetFluidPropertiesData: '//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//'", invalid type')
      CALL ShowContinueError('...entered value="'//TRIM(Alphas(2))//', Only REFRIGERANT or GLYCOL allowed as '//  &
         trim(cAlphaFieldNames(2)))
      ErrorsFound=.true.
    END IF
  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetFluidPropertiesData: Previous errors in input cause program termination.')
  ENDIF

  IF (NumOfRefrigerants+1 > 0) THEN
    ALLOCATE(RefrigData(NumOfRefrigerants+1))
    ALLOCATE(RefrigUsed(NumOfRefrigerants+1))
    RefrigUsed=.false.
  ENDIF
  IF (NumOfGlycols > 0) THEN
    ALLOCATE(GlyRawData(NumOfGlycols))
  ENDIF

          ! Take the fluid names and assign them to the appropriate derived type
  NumOfRefrigerants = 1
  NumOfGlycols      = 0
  RefrigData(1)%Name = 'STEAM'
  RefrigUsed(1)=.true.
  DO Loop = 1, FluidNum
    IF (.not. FluidNames(Loop)%IsGlycol) THEN
      NumOfRefrigerants = NumOfRefrigerants + 1
      RefrigData(NumOfRefrigerants)%Name = FluidNames(Loop)%Name
    ELSEIF (FluidNames(Loop)%IsGlycol) THEN
      NumOfGlycols = NumOfGlycols + 1
      GlyRawData(NumOfGlycols)%Name = FluidNames(Loop)%Name
    END IF
  END DO

  DEALLOCATE(FluidNames)

  RefrigData(1)%NumPsPoints = DefaultNumSteamTemps
  ALLOCATE(RefrigData(1)%PsTemps(DefaultNumSteamTemps))
  ALLOCATE(RefrigData(1)%PsValues(DefaultNumSteamTemps))
  RefrigData(1)%NumHPoints = DefaultNumSteamTemps
  ALLOCATE(RefrigData(1)%HTemps(DefaultNumSteamTemps))
  ALLOCATE(RefrigData(1)%HfValues(DefaultNumSteamTemps))
  ALLOCATE(RefrigData(1)%HfgValues(DefaultNumSteamTemps))
  RefrigData(1)%NumCpPoints = DefaultNumSteamTemps
  ALLOCATE(RefrigData(1)%CpTemps(DefaultNumSteamTemps))
  ALLOCATE(RefrigData(1)%CpfValues(DefaultNumSteamTemps))
  ALLOCATE(RefrigData(1)%CpfgValues(DefaultNumSteamTemps))
  RefrigData(1)%NumRhoPoints = DefaultNumSteamTemps
  ALLOCATE(RefrigData(1)%RhoTemps(DefaultNumSteamTemps))
  ALLOCATE(RefrigData(1)%RhofValues(DefaultNumSteamTemps))
  ALLOCATE(RefrigData(1)%RhofgValues(DefaultNumSteamTemps))

  RefrigData(1)%PsTemps=    DefaultSteamTemps
  RefrigData(1)%PsValues=   DefaultSteamPressData
  RefrigData(1)%HTemps=     DefaultSteamTemps
  RefrigData(1)%HfValues=   DefaultSteamEnthalpyFluidData
  RefrigData(1)%HfgValues=  DefaultSteamEnthalpyGasFluidData
  RefrigData(1)%CpTemps=    DefaultSteamTemps
  RefrigData(1)%CpfValues=  DefaultSteamCpFluidData
  RefrigData(1)%CpfgValues= DefaultSteamCpGasFluidData
  RefrigData(1)%RhoTemps=   DefaultSteamTemps
  RefrigData(1)%RhofValues= DefaultSteamDensityFluidData
  RefrigData(1)%RhofgValues=DefaultSteamDensityGasFluidData

  RefrigData(1)%NumSuperTempPts=DefaultNumSteamSuperheatedTemps
  RefrigData(1)%NumSuperPressPts=DefaultNumSteamSuperheatedPressure
  ALLOCATE(RefrigData(1)%SHTemps(RefrigData(1)%NumSuperTempPts))
  ALLOCATE(RefrigData(1)%SHPress(RefrigData(1)%NumSuperPressPts))
  ALLOCATE(RefrigData(1)%HshValues(RefrigData(1)%NumSuperTempPts,RefrigData(1)%NumSuperPressPts))
  ALLOCATE(RefrigData(1)%RhoshValues(RefrigData(1)%NumSuperTempPts,RefrigData(1)%NumSuperPressPts))
  RefrigData(1)%SHTemps=DefaultSteamSuperheatedTemps
  RefrigData(1)%SHPress=DefaultSteamSuperheatedPressData
  RefrigData(1)%HshValues=DefaultSteamSuperheatedEnthalpyData
  RefrigData(1)%RhoshValues=DefaultSteamSuperheatedDensityData


          ! Read in all of the temperature arrays in the input file
  ALLOCATE(FluidTemps(NumOfFluidTempArrays))

  CurrentModuleObject = 'FluidProperties:Temperatures'

  DO Loop = 1, NumOfFluidTempArrays

    CALL GetObjectItem(TRIM(CurrentModuleObject),Loop,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    FluidTemps(Loop)%Name       = Alphas(1)
    FluidTemps(Loop)%NumOfTemps = NumNumbers

    ALLOCATE(FluidTemps(Loop)%Temps(FluidTemps(Loop)%NumOfTemps))
    FluidTemps(Loop)%Temps = Numbers(1:NumNumbers)

    DO TempLoop = 2, FluidTemps(Loop)%NumOfTemps
      IF (FluidTemps(Loop)%Temps(TempLoop) <= FluidTemps(Loop)%Temps(TempLoop-1)) THEN
        !CALL ShowSevereError('GetFluidPropertiesData: '//TRIM(CurrentModuleObject)//' lists must have data in ascending order')
        !CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' name='//TRIM(FluidTemps(Loop)%Name))
        !CALL ShowContinueError('First Occurance at Temp('//TRIM(RoundSigDigits(TempLoop-1))//  &
        !         ') {'//TRIM(RoundSigDigits(FluidTemps(Loop)%Temps(TempLoop-1),3))//'} >= Temp('//  &
        !         TRIM(RoundSigDigits(TempLoop))//') {'//TRIM(RoundSigDigits(FluidTemps(Loop)%Temps(TempLoop),3))//'}') !RS: Secret Search String
        WRITE(DebugFile,*) 'GetFluidPropertiesData: '//TRIM(CurrentModuleObject)//' lists must have data in ascending order'
        WRITE(DebugFile,*) 'Error occurs in '//TRIM(CurrentModuleObject)//' name='//TRIM(FluidTemps(Loop)%Name)
        WRITE(DebugFile,*) 'First Occurance at Temp('//TRIM(RoundSigDigits(TempLoop-1))// &
                ') {'//TRIM(RoundSigDigits(FluidTemps(Loop)%Temps(TempLoop-1),3))//'} >= Temp('// &
                TRIM(RoundSigDigits(TempLoop))//') {'//TRIM(RoundSigDigits(FluidTemps(Loop)%Temps(TempLoop),3))//'}'
        ErrorsFound=.true.
        EXIT
      ENDIF
    END DO

  END DO

          ! *************** REFRIGERANTS ***************
          ! Go through each refrigerant found in the fluid names statement and read in the data
          ! Note that every valid fluid must have ALL of the necessary data or a fatal error will
          ! be produced.
  DO Loop = 2, NumOfRefrigerants

          ! For each property, cycle through all the valid input until the proper match is found.

          ! **********    SATURATED DATA SECTION    **********

          ! Get: ***** Saturation Pressure temperatures and data (fluidgas only) *****
          ! This section added by S.J.Rees May 2002.
    CurrentModuleObject = 'FluidProperties:Saturated'
    TempsName     = ' '
    DO InData = 1, NumOfSatFluidPropArrays

      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IF ( (SameString(Alphas(1),RefrigData(Loop)%Name) ) .AND. &
           (SameString(Alphas(2),Pressure) )              .AND. &
           (SameString(Alphas(3),GasFluid ) )           ) THEN

        DO TempLoop = 1, NumOfFluidTempArrays

          IF (SameString(Alphas(4),FluidTemps(TempLoop)%Name)) THEN
            TempsName = FluidTemps(TempLoop)%Name
            ! At this point, we have found the correct input line and found a match
            ! for the temperature array.  It's time to load up the local derived type.
            RefrigData(Loop)%NumPsPoints = FluidTemps(TempLoop)%NumOfTemps
            ALLOCATE(RefrigData(Loop)%PsTemps(RefrigData(Loop)%NumPsPoints))
            ALLOCATE(RefrigData(Loop)%PsValues(RefrigData(Loop)%NumPsPoints))

            ! Make sure the number of points in the two arrays (temps and values) are the same
            IF (NumNumbers /= RefrigData(Loop)%NumPsPoints) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Temperature array and fluid saturation pressure '// &
                                                            'array must have the same number of points')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
              CALL ShowContinueError('with Temperature Name='//TRIM(TempsName))
              CALL ShowContinueError('Temperature # points='//TRIM(RoundSigDigits(NumNumbers))//' whereas '//  &
                                      TRIM(RefrigData(Loop)%Name)//' # points='//TRIM(RoundSigDigits(RefrigData(Loop)%NumPsPoints)))
              ErrorsFound=.true.
              EXIT ! the TempLoop DO Loop
            ENDIF

            ! Same number of points so assign the values
            RefrigData(Loop)%PsTemps  = FluidTemps(TempLoop)%Temps
            RefrigData(Loop)%PsValues = Numbers(1:NumNumbers)

            EXIT ! the TempLoop DO loop

          END IF

          ! If it made it all the way to the last temperature array and didn't find a match, then no match was found
          IF (TempLoop == NumOfFluidTempArrays) THEN
            CALL ShowSevereError('GetFluidPropertiesData: '//  &
               'Found saturated fluid gas/fluid pressure input but no matching temperature array')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' name='//TRIM(RefrigData(Loop)%Name))
            ErrorsFound=.true.
          ENDIF

        END DO  ! ...end of FluidTemps DO loop

        EXIT ! the InData DO loop

      END IF

          ! If it made it all the way to the last input occurrence and didn't find a match,
          ! then no sat press data found
      IF (InData == NumOfSatFluidPropArrays) THEN
        !CALL ShowSevereError('GetFluidPropertiesData: No Gas/Fluid Saturation Pressure found')
        !CALL ShowContinueError('Was looking for properties for Refrigerant='//TRIM(RefrigData(Loop)%Name))
        !CALL ShowContinueError('Need properties to be entered for '//trim(CurrentModuleObject)//' object.')
        !CALL ShowContinueError('with '//trim(cAlphaFieldNames(2))//'="Pressure" and '//trim(cAlphaFieldNames(3))//  &
        !   '="FluidGas".') !RS: Secret Search String
        WRITE(DebugFile,*) 'GetFluidPropertiesData: No Gas/Fluid Saturation Pressure found'
        WRITE(DebugFile,*) 'Was looking for properties for Refrigerant='//TRIM(RefrigData(Loop)%Name)
        WRITE(DebugFile,*) 'with '//TRIM(cAlphaFieldNames(2))//'="Pressure" and '//TRIM(cAlphaFieldNames(3))//'="FluidGas".'
        ErrorsFound=.true.
      ENDIF

    END DO  ! ...end of DO loop through all of the input syntax trying to find saturation pressure for this refrigerant



          ! Get: ***** ENTHALPY of SATURATED LIQUID *****
    CurrentModuleObject = 'FluidProperties:Saturated'
    TempsName     = " "
    DO InData = 1, NumOfSatFluidPropArrays

      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IF ( (SameString(Alphas(1),RefrigData(Loop)%Name) ) .AND. &
           (SameString(Alphas(2),Enthalpy) )              .AND. &
           (SameString(Alphas(3),Fluid ) )                ) THEN

        DO TempLoop = 1, NumOfFluidTempArrays

          IF (SameString(Alphas(4),FluidTemps(TempLoop)%Name)) THEN
            TempsName = FluidTemps(TempLoop)%Name
            ! At this point, we have found the correct input line and found a match
            ! for the temperature array.  It's time to load up the local derived type.
            RefrigData(Loop)%NumHPoints = FluidTemps(TempLoop)%NumOfTemps
            ALLOCATE(RefrigData(Loop)%HTemps(RefrigData(Loop)%NumHPoints))
            ALLOCATE(RefrigData(Loop)%HfValues(RefrigData(Loop)%NumHPoints))

            ! Make sure the number of points in the two arrays (temps and values) are the same
            IF (NumNumbers /= RefrigData(Loop)%NumHPoints) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Temperature array and saturated fluid enthalpy '//  &
                                   'array must have the same number of points')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
              CALL ShowContinueError('with Temperature Name='//TRIM(TempsName))
              CALL ShowContinueError('Temperature # points='//TRIM(RoundSigDigits(NumNumbers))//' whereas '//  &
                                      TRIM(RefrigData(Loop)%Name)//' # points='//TRIM(RoundSigDigits(RefrigData(Loop)%NumHPoints)))
              ErrorsFound=.true.
              EXIT ! the TempLoop DO Loop
            ENDIF

            ! Same number of points so assign the values
            RefrigData(Loop)%HTemps  = FluidTemps(TempLoop)%Temps
            RefrigData(Loop)%HfValues = Numbers(1:NumNumbers)

            EXIT ! the TempLoop DO loop

          END IF

          ! If it made it all the way to the last temperature array and didn't find a match, then no match was found
          IF (TempLoop == NumOfFluidTempArrays) THEN
            CALL ShowSevereError('GetFluidPropertiesData: Found saturated fluid enthalpy input but no matching temperature array')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
            ErrorsFound=.true.
          ENDIF

        END DO  ! ...end of FluidTemps DO loop

        EXIT ! the InData DO loop

      END IF

          ! If it made it all the way to the last input occurrence and didn't find a match, then no sat fluid enthalpy data found
      IF (InData == NumOfSatFluidPropArrays) THEN
        !CALL ShowSevereError('GetFluidPropertiesData: No Saturated Fluid Enthalpy found')  !RS: Secret Search String
        !CALL ShowContinueError('Was looking for properties for Refrigerant='//TRIM(RefrigData(Loop)%Name))
        !CALL ShowContinueError('Need properties to be entered for '//trim(CurrentModuleObject)//' object:')
        !CALL ShowContinueError('with '//trim(cAlphaFieldNames(2))//'="Enthalpy" and '//trim(cAlphaFieldNames(3))//  &
        !   '="Fluid".')
        WRITE(DebugFile,*) 'GetFluidPropertiesData: No Saturated Fluid Enthalpy found'
        WRITE(DebugFile,*) 'Was looking for properties for Refrigerant='//TRIM(RefrigData(Loop)%Name)
        WRITE(DebugFile,*) 'Need properties to be entered for '//TRIM(CurrentModuleObject)//' object:'
        WRITE(DebugFile,*) 'with '//TRIM(cAlphaFieldNames(2))//'="Enthalpy" and '//TRIM(cAlphaFieldNames(3))//'="Fluid".'
        ErrorsFound=.true.
      ENDIF

    END DO  ! ...end of DO loop through all of the input syntax trying to find saturated fluid enthalpy for this refrigerant

          ! Get: ***** ENTHALPY of SATURATED LIQUID/VAPOR ***** (difference between Hf and Hg, i.e. Hfg)
    CurrentModuleObject = 'FluidProperties:Saturated'
    DO InData = 1, NumOfSatFluidPropArrays

      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IF ( (SameString(Alphas(1),RefrigData(Loop)%Name) ) .AND. &
           (SameString(Alphas(2),Enthalpy) )              .AND. &
           (SameString(Alphas(3),GasFluid ) )             ) THEN

        DO TempLoop = 1, NumOfFluidTempArrays

          IF (SameString(Alphas(4),FluidTemps(TempLoop)%Name)) THEN
            IF (.NOT.SameString(FluidTemps(TempLoop)%Name,TempsName)) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Temperatures for enthalpy fluid and '// &
                                   'gas/fluid points are not the same')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
              CALL ShowContinueError('Name='//TRIM(Alphas(4))//' => '//TRIM(FluidTemps(TempLoop)%Name)//' /= '//TRIM(TempsName))
              ErrorsFound=.true.
              EXIT
            ENDIF
            ! At this point, we have found the correct input line and found a match
            ! for the temperature array.  It's time to load up the local derived type.
            ALLOCATE(RefrigData(Loop)%HfgValues(RefrigData(Loop)%NumHPoints))

            ! Make sure the number of points in the two arrays (temps and values) are the same
            IF (NumNumbers /= RefrigData(Loop)%NumHPoints) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Temperature array and saturated gas/fluid '// &
                                   'enthalpy array must have the same number of points')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
              CALL ShowContinueError('with Temperature Name='//TRIM(TempsName))
              CALL ShowContinueError('Temperature # points='//TRIM(RoundSigDigits(NumNumbers))//' whereas '//  &
                                      TRIM(RefrigData(Loop)%Name)//' # points='//TRIM(RoundSigDigits(RefrigData(Loop)%NumHPoints)))
              ErrorsFound=.true.
              EXIT ! the TempLoop DO Loop
            ENDIF

            ! Same number of points so assign the values
            RefrigData(Loop)%HfgValues = Numbers(1:NumNumbers)

            EXIT ! the TempLoop DO loop

          END IF

          ! If it made it all the way to the last temperature array and didn't find a match, then no match was found
          IF (TempLoop == NumOfFluidTempArrays) THEN
            CALL ShowSevereError('GetFluidPropertiesData: Found saturated gas/fluid enthalpy input '// &
                                 'but no matching temperature array')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
            ErrorsFound=.true.
          ENDIF

        END DO  ! ...end of FluidTemps DO loop

        EXIT ! the InData DO loop

      END IF

          ! If it made it all the way to the last input occurrence and didn't find a match, then no sat f/g enthalpy data found
      IF (InData == NumOfSatFluidPropArrays) THEN
        !CALL ShowSevereError('GetFluidPropertiesData: No Saturated Gas/Fluid Enthalpy found')
        !CALL ShowContinueError('Was looking for properties for Refrigerant='//TRIM(RefrigData(Loop)%Name))
        !CALL ShowContinueError('Need properties to be entered for '//trim(CurrentModuleObject)//' object.')
        !CALL ShowContinueError('with '//trim(cAlphaFieldNames(2))//'="Enthalpy" and '//trim(cAlphaFieldNames(3))//  &
        !   '="FluidGas".') !RS: Secret Search String
        WRITE(DebugFile,*) 'GetFluidPropertiesData: No Saturated Gas/Fluid Enthalpy found'
        WRITE(DebugFile,*) 'Was looking for properties for Refrigerant='//TRIM(RefrigData(Loop)%Name)
        WRITE(DebugFile,*) 'Need properties to be entered for '//TRIM(CurrentModuleObject)//' object.'
        WRITE(DebugFile,*) 'with '//TRIM(cAlphaFieldNames(2))//'="Enthalpy" and'//TRIM(cAlphaFieldNames(3))//'="FluidGas".'
        ErrorsFound=.true.
      ENDIF

    END DO  ! ...end of DO loop through all of the input syntax trying to find saturated gas/fluid enthalpy for this refrigerant

          ! Get: ***** SPECIFIC HEAT of SATURATED LIQUID *****
    CurrentModuleObject = 'FluidProperties:Saturated'
    TempsName     = " "
    DO InData = 1, NumOfSatFluidPropArrays

      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IF ( (SameString(Alphas(1),RefrigData(Loop)%Name) ) .AND. &
           (SameString(Alphas(2),SpecificHeat) )          .AND. &
           (SameString(Alphas(3),Fluid ) )                ) THEN

        DO TempLoop = 1, NumOfFluidTempArrays

          IF (SameString(Alphas(4),FluidTemps(TempLoop)%Name)) THEN
            TempsName = FluidTemps(TempLoop)%Name
            ! At this point, we have found the correct input line and found a match
            ! for the temperature array.  It's time to load up the local derived type.
            RefrigData(Loop)%NumCpPoints = FluidTemps(TempLoop)%NumOfTemps
            ALLOCATE(RefrigData(Loop)%CpTemps(RefrigData(Loop)%NumCpPoints))
            ALLOCATE(RefrigData(Loop)%CpfValues(RefrigData(Loop)%NumCpPoints))

            ! Make sure the number of points in the two arrays (temps and values) are the same
            IF (NumNumbers /= RefrigData(Loop)%NumCpPoints) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Temperature array and saturated fluid Cp '//  &
                                   'array must have the same number of points')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
              CALL ShowContinueError('with Temperature Name='//TRIM(TempsName))
              CALL ShowContinueError('Temperature # points='//TRIM(RoundSigDigits(NumNumbers))//' whereas '//  &
                                      TRIM(RefrigData(Loop)%Name)//' # points='//TRIM(RoundSigDigits(RefrigData(Loop)%NumCpPoints)))
              ErrorsFound=.true.
              EXIT ! the TempLoop DO Loop
            ENDIF

            ! Same number of points so assign the values
            RefrigData(Loop)%CpTemps  = FluidTemps(TempLoop)%Temps
            RefrigData(Loop)%CpfValues = Numbers(1:NumNumbers)

            EXIT ! the TempLoop DO loop

          END IF

          ! If it made it all the way to the last temperature array and didn't find a match, then no match was found
          IF (TempLoop == NumOfFluidTempArrays) THEN
            CALL ShowSevereError('GetFluidPropertiesData: Found saturated fluid specific heat (Cp) input '//  &
               'but no matching temperature array')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
            ErrorsFound=.true.
          ENDIF

        END DO  ! ...end of FluidTemps DO loop

        EXIT ! the InData DO loop

      END IF

          ! If it made it all the way to the last input occurrence and didn't find a match, then no sat fluid Cp data found
      IF (InData == NumOfSatFluidPropArrays) THEN
        !CALL ShowSevereError('GetFluidPropertiesData: No Saturated Fluid Specific Heat found')
        !CALL ShowContinueError('Was looking for properties for Refrigerant='//TRIM(RefrigData(Loop)%Name))
        !CALL ShowContinueError('Need properties to be entered for '//trim(CurrentModuleObject)//' object.')
        !CALL ShowContinueError('with '//trim(cAlphaFieldNames(2))//'="SpecificHeat" and '//trim(cAlphaFieldNames(3))//  &
        !   '="Fluid".')    !RS: Secret Search String
        WRITE(DebugFile,*) 'GetFluidPropertiesData: No Saturated Fluid Specific Heat found'
        WRITE(DebugFile,*) 'Was looking for properties for Refrigerant='//TRIM(RefrigData(Loop)%Name)
        WRITE(DebugFile,*) 'Need properties to be entered for '//TRIM(CurrentModuleObject)//' object.'
        WRITE(DebugFile,*) 'with '//TRIM(cAlphaFieldNames(2))//'="SpecificHeat" and '//TRIM(cAlphaFieldNames(3))//'="Fluid".'
        ErrorsFound=.true.
      ENDIF

    END DO  ! ...end of DO loop through all of the input syntax trying to find saturated fluid Cp for this refrigerant

          ! Get: ***** SPECIFIC HEAT of SATURATED LIQUID/VAPOR ***** (difference between Cpf and Cpg, i.e. Cpfg)
    CurrentModuleObject = 'FluidProperties:Saturated'
    DO InData = 1, NumOfSatFluidPropArrays

      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IF ( (SameString(Alphas(1),RefrigData(Loop)%Name) ) .AND. &
           (SameString(Alphas(2),SpecificHeat) )          .AND. &
           (SameString(Alphas(3),GasFluid ) )             ) THEN

        DO TempLoop = 1, NumOfFluidTempArrays

          IF (SameString(Alphas(4),FluidTemps(TempLoop)%Name)) THEN
            IF (.NOT.SameString(FluidTemps(TempLoop)%Name,TempsName)) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Temperatures for specific heat fluid and '// &
                                   'gas/fluid points are not the same')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
              CALL ShowContinueError('Name='//TRIM(Alphas(4))//' => '//TRIM(FluidTemps(TempLoop)%Name)//' /= '//TRIM(TempsName))
              ErrorsFound=.true.
              EXIT
            ENDIF
            ! At this point, we have found the correct input line and found a match
            ! for the temperature array.  It's time to load up the local derived type.
            ALLOCATE(RefrigData(Loop)%CpfgValues(RefrigData(Loop)%NumCpPoints))

            ! Make sure the number of points in the two arrays (temps and values) are the same
            IF (NumNumbers /= RefrigData(Loop)%NumCpPoints) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Temperature array and saturated gas/fluid Cp '//  &
                                   'array must have the same number of points')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
              CALL ShowContinueError('with Temperature Name='//TRIM(TempsName))
              CALL ShowContinueError('Temperature # points='//TRIM(RoundSigDigits(NumNumbers))//' whereas '//  &
                                      TRIM(RefrigData(Loop)%Name)//' # points='//TRIM(RoundSigDigits(RefrigData(Loop)%NumCpPoints)))
              ErrorsFound=.true.
              EXIT ! the TempLoop DO Loop
            ENDIF

            ! Same number of points so assign the values
            RefrigData(Loop)%CpfgValues = Numbers(1:NumNumbers)

            EXIT ! the TempLoop DO loop

          END IF

          ! If it made it all the way to the last temperature array and didn't find a match, then no match was found
          IF (TempLoop == NumOfFluidTempArrays) THEN
            CALL ShowSevereError('GetFluidPropertiesData: Found saturated gas/fluid specific heat (Cp) input '//  &
               'but no matching temperature array')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
            ErrorsFound=.true.
          ENDIF

        END DO  ! ...end of FluidTemps DO loop

        EXIT ! the InData DO loop

      END IF

          ! If it made it all the way to the last input occurrence and didn't find a match, then no sat f/g Cp data found
      IF (InData == NumOfSatFluidPropArrays) THEN
        !CALL ShowSevereError('GetFluidPropertiesData: No Saturated Gas/Fluid Specific Heat found')
        !CALL ShowContinueError('Was looking for properties for Refrigerant='//TRIM(RefrigData(Loop)%Name))
        !CALL ShowContinueError('Need properties to be entered for '//trim(CurrentModuleObject)//' object.')
        !CALL ShowContinueError('with '//trim(cAlphaFieldNames(2))//'="SpecificHeat" and '//trim(cAlphaFieldNames(3))//  &
        !   '="FluidGas".') !RS: Secret Search String
        WRITE(DebugFile,*) 'GetFluidPropertiesData: No Saturated Gas/Fluid Specific Heat found'
        WRITE(DebugFile,*) 'Was looking for properties for Refrigerant='//TRIM(RefrigData(Loop)%Name)
        WRITE(DebugFile,*) 'Need properties to be entered for '//TRIM(CurrentModuleObject)//' object.'
        WRITE(DebugFile,*) 'with '//TRIM(cAlphaFieldNames(2))//'="SpecificHeat" amd '//TRIM(cAlphaFieldNames(3))//'="FluidGas".'
        ErrorsFound=.true.
      ENDIF

    END DO  ! ...end of DO loop through all of the input syntax trying to find saturated gas/fluid Cp for this refrigerant

          ! Get: ***** DENSITY of SATURATED LIQUID *****
    CurrentModuleObject = 'FluidProperties:Saturated'
    TempsName     = " "
    DO InData = 1, NumOfSatFluidPropArrays

      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IF ( (SameString(Alphas(1),RefrigData(Loop)%Name) ) .AND. &
           (SameString(Alphas(2),Density) )               .AND. &
           (SameString(Alphas(3),Fluid ) )                ) THEN

        DO TempLoop = 1, NumOfFluidTempArrays

          IF (SameString(Alphas(4),FluidTemps(TempLoop)%Name)) THEN
            TempsName = FluidTemps(TempLoop)%Name
            ! At this point, we have found the correct input line and found a match
            ! for the temperature array.  It's time to load up the local derived type.
            RefrigData(Loop)%NumRhoPoints = FluidTemps(TempLoop)%NumOfTemps
            ALLOCATE(RefrigData(Loop)%RhoTemps(RefrigData(Loop)%NumRhoPoints))
            ALLOCATE(RefrigData(Loop)%RhofValues(RefrigData(Loop)%NumRhoPoints))

            ! Make sure the number of points in the two arrays (temps and values) are the same
            IF (NumNumbers /= RefrigData(Loop)%NumRhoPoints) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Temperature array and saturated fluid density '//  &
                                   'array must have the same number of points')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
              CALL ShowContinueError('with Temperature Name='//TRIM(TempsName))
              CALL ShowContinueError('Temperature # points='//TRIM(RoundSigDigits(NumNumbers))//' whereas '//  &
                                      TRIM(RefrigData(Loop)%Name)//' # points='//  &
                                      TRIM(RoundSigDigits(RefrigData(Loop)%NumRhoPoints)))
              ErrorsFound=.true.
              EXIT ! the TempLoop DO Loop
            ENDIF

            ! Same number of points so assign the values
            RefrigData(Loop)%RhoTemps  = FluidTemps(TempLoop)%Temps
            RefrigData(Loop)%RhofValues = Numbers(1:NumNumbers)

            EXIT ! the TempLoop DO loop

          END IF

          ! If it made it all the way to the last temperature array and didn't find a match, then no match was found
          IF (TempLoop == NumOfFluidTempArrays) THEN
            CALL ShowSevereError('GetFluidPropertiesData: Found saturated fluid density input but no matching temperature array')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
            ErrorsFound=.true.
          ENDIF

        END DO  ! ...end of FluidTemps DO loop

        EXIT ! the InData DO loop

      END IF

          ! If it made it all the way to the last input occurrence and didn't find a match, then no sat fluid density data found
      IF (InData == NumOfSatFluidPropArrays) THEN
        !CALL ShowSevereError('GetFluidPropertiesData: No Saturated Fluid Density found')
        !CALL ShowContinueError('Was looking for properties for Refrigerant='//TRIM(RefrigData(Loop)%Name))
        !CALL ShowContinueError('Need properties to be entered for '//trim(CurrentModuleObject)//' object.')
        !CALL ShowContinueError('with '//trim(cAlphaFieldNames(2))//'="Density" and '//trim(cAlphaFieldNames(3))//  &
        !   '="Fluid".')    !RS: Secret Search String
        WRITE(DebugFile,*) 'GetFluidPropertiesData: No Saturated Fluid Density found'
        WRITE(DebugFile,*) 'Was looking for properties for Refrigerant='//TRIM(RefrigData(Loop)%Name)
        WRITE(DebugFile,*) 'Need properties to be entered for '//TRIM(CurrentModuleObject)//' object.'
        WRITE(DebugFile,*) 'with '//TRIM(cAlphaFieldNames(2))//'="Density" and '//TRIM(cAlphaFieldNames(3))//'="Fluid".'
        ErrorsFound=.true.
      ENDIF

    END DO  ! ...end of DO loop through all of the input syntax trying to find saturated fluid enthalpy for this refrigerant

          ! Get: ***** DENSITY of SATURATED LIQUID/VAPOR ***** (difference between Rhof and Rhog, i.e. Rhofg)
    CurrentModuleObject = 'FluidProperties:Saturated'
    DO InData = 1, NumOfSatFluidPropArrays

      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IF ( (SameString(Alphas(1),RefrigData(Loop)%Name) ) .AND. &
           (SameString(Alphas(2),Density) )               .AND. &
           (SameString(Alphas(3),GasFluid ) )             ) THEN

        DO TempLoop = 1, NumOfFluidTempArrays

          IF (SameString(Alphas(4),FluidTemps(TempLoop)%Name)) THEN
            IF (.NOT.SameString(FluidTemps(TempLoop)%Name,TempsName)) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Temperatures for density fluid and '// &
                                   'gas/fluid points are not the same')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
              CALL ShowContinueError('Name='//TRIM(Alphas(4))//' => '//TRIM(FluidTemps(TempLoop)%Name)//' /= '//TRIM(TempsName))
              ErrorsFound=.true.
              EXIT
            ENDIF
            ! At this point, we have found the correct input line and found a match
            ! for the temperature array.  It's time to load up the local derived type.
            ALLOCATE(RefrigData(Loop)%RhofgValues(RefrigData(Loop)%NumRhoPoints))

            ! Make sure the number of points in the two arrays (temps and values) are the same
            IF (NumNumbers /= RefrigData(Loop)%NumRhoPoints) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Temperature array and saturated gas/fluid density '//  &
                                   'array must have the same number of points')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
              CALL ShowContinueError('with Temperature Name='//TRIM(TempsName))
              CALL ShowContinueError('Temperature # points='//TRIM(RoundSigDigits(NumNumbers))//' whereas '//  &
                                      TRIM(RefrigData(Loop)%Name)//' # points='//  &
                                      TRIM(RoundSigDigits(RefrigData(Loop)%NumRhoPoints)))
              ErrorsFound=.true.
              EXIT ! the TempLoop DO Loop
            ENDIF

            ! Same number of points so assign the values
            RefrigData(Loop)%RhofgValues = Numbers(1:NumNumbers)

            EXIT ! the TempLoop DO loop

          END IF

          ! If it made it all the way to the last temperature array and didn't find a match, then no match was found
          IF (TempLoop == NumOfFluidTempArrays) THEN
            CALL ShowFatalError('Found saturated gas/fluid density input but no matching temperature array')
            CALL ShowSevereError('GetFluidPropertiesData: Found saturated gas/fluid density input '// &
                                 'but no matching temperature array')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
            ErrorsFound=.true.
          ENDIF

        END DO  ! ...end of FluidTemps DO loop

        EXIT ! the InData DO loop

      END IF

          ! If it made it all the way to the last input occurrence and didn't find a match, then no sat f/g density data found
      IF (InData == NumOfSatFluidPropArrays) THEN
        !CALL ShowSevereError('GetFluidPropertiesData: No Saturated Gas/Fluid Density found')
        !CALL ShowContinueError('Was looking for properties for Refrigerant='//TRIM(RefrigData(Loop)%Name))
        !CALL ShowContinueError('Need properties to be entered for '//trim(CurrentModuleObject)//' object.')
        !CALL ShowContinueError('with '//trim(cAlphaFieldNames(2))//'="Density" and '//trim(cAlphaFieldNames(3))//  &
        !   '="FluidGas".') !RS: Secret Search String
        WRITE(DebugFile,*) 'GetFluidPropertiesData: No Saturated Gas/Fluid Density found.'
        WRITE(DebugFile,*) 'Was looking for properties for Refrigerant='//TRIM(RefrigData(Loop)%Name)
        WRITE(DebugFile,*) 'Need properties to be entered for '//TRIM(CurrentModuleObject)//' object.'
        WRITE(DebugFile,*) 'With '//TRIM(cAlphaFieldNames(2))//'="Density" and '//TRIM(cAlphaFieldNames(3))//'="FluidGas".'
        ErrorsFound=.true.
      ENDIF

    END DO  ! ...end of DO loop through all of the input syntax trying to find saturated gas/fluid density for this refrigerant

          ! Check: TEMPERATURES for saturated density (must all be the same)
!    IF (RefrigData(Loop)%NumCpPoints /= RefrigData(Loop)%NumCpPoints) THEN
    !!!!  Error -- can never happen, does this mean NumCp vs. NumRho?
!      CALL ShowFatalError('GetFluidPropertiesData: Number of specific heat fluid and gas/fluid points are not the same')
!    ELSE
!      DO TempLoop = 1, RefrigData(Loop)%NumCpPoints
        !!!! Error -- something else that can never happen
!        IF (ABS(RefrigData(Loop)%CpTemps(TempLoop)-RefrigData(Loop)%CpTemps(TempLoop)) > TempToler) THEN
!          CALL ShowSevereError('GetFluidPropertiesData: Temperatures for specific heat fluid and '// &
!                               'gas/fluid points are not the same')
!          CALL ShowContinueError('Error occurs in Refrigerant Data Name='//TRIM(RefrigData(Loop)%Name))
!          WRITE(String1,*) TempLoop
!          String1=ADJUSTL(String1)
!          String2=TrimSigDigits(RefrigData(Loop)%CpTemps(TempLoop),3)
!          String2=ADJUSTL(String2)
!          String4=TrimSigDigits(RefrigData(Loop)%CpTemps(TempLoop),3)
!          String4=ADJUSTL(String4)
!          CALL ShowContinueError('First Occurance at CpTemp('//TRIM(String1)//') {'//TRIM(String2)//'} /= {'//TRIM(String4)//'}')
!          ErrorsFound=.true.
!          EXIT
!        ENDIF
!      END DO
!    END IF

          ! **********   SUPERHEATED DATA SECTION   **********
          ! Get: ***** ENTHALPY of SUPERHEATED GAS  *****
          ! First find the number of pressure value syntax lines have been entered and
          ! make sure that all of the pressure input is linked to the same temperature list
    CurrentModuleObject = 'FluidProperties:Superheated'
    TempsName     = " "
    FirstSHMatch  = .TRUE.
    NumOfPressPts = 0
    DO InData = 1, NumOfSHFluidPropArrays
      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IF ((SameString(Alphas(1),RefrigData(Loop)%Name)).AND.(SameString(Alphas(2),Enthalpy))) THEN
        NumOfPressPts = NumOfPressPts + 1
        IF (FirstSHMatch) THEN
          TempsName = Alphas(3)
          FirstSHMatch = .FALSE.
        ELSE
          IF (.NOT.SameString(TempsName,Alphas(3))) THEN
            CALL ShowSevereError('GetFluidPropertiesData: All superheated data for the same property must use '// &
                                 'the same temperature list')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
            CALL ShowContinueError('Expected Temperature name='//TRIM(TempsName)//', Input had name='//TRIM(Alphas(3)))
            ErrorsFound=.true.
          ENDIF
        END IF
      END IF
    END DO
    IF (NumOfPressPts == 0) THEN
      !CALL ShowSevereError('GetFluidPropertiesData: No pressure data found for superheated enthalpy')
      !CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name)) !RS: Secret Search String
      WRITE(DebugFile,*) 'GetFluidPropertiesData: No pressure data found for superheated enthalpy'
      WRITE(DebugFile,*) 'Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name)
      ErrorsFound=.true.
    ENDIF

          ! Now allocate the arrays and read the data into the proper place
          ! First, allocate the temperature array and transfer the data from the FluidTemp array
    DO TempLoop = 1, NumOfFluidTempArrays
      IF (SameString(TempsName,FluidTemps(TempLoop)%Name)) THEN
        RefrigData(Loop)%NumSuperTempPts = FluidTemps(TempLoop)%NumOfTemps
        ALLOCATE(RefrigData(Loop)%SHTemps(RefrigData(Loop)%NumSuperTempPts))
        RefrigData(Loop)%SHTemps = FluidTemps(TempLoop)%Temps
        EXIT ! the TempLoop DO loop
      END IF
      IF (TempLoop == NumOfFluidTempArrays) THEN
        !CALL ShowSevereError('GetFluidPropertiesData: No match for temperature array name found with '// &
        !                     'superheated enthalpy data')
        !CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))   !RS: Secret Search String
        WRITE(DebugFile,*) 'GetFluidPropertiesData: No match for temperature array name found with superheated enthalpy data'
        WRITE(DebugFile,*) 'Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name)
        ErrorsFound=.true.
      ENDIF
    END DO

          ! Next, allocate the pressure related arrays
    RefrigData(Loop)%NumSuperPressPts = NumOfPressPts
    ALLOCATE(RefrigData(Loop)%SHPress(RefrigData(Loop)%NumSuperPressPts))
    ALLOCATE(RefrigData(Loop)%HshValues(RefrigData(Loop)%NumSuperTempPts,RefrigData(Loop)%NumSuperPressPts))

          ! Finally, get the pressure and enthalpy values from the user input
    CurrentModuleObject = 'FluidProperties:Superheated'
    NumOfPressPts = 0
    ALLOCATE(PressurePtr(NumOfSHFluidPropArrays))
    DO InData = 1, NumOfSHFluidPropArrays
      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IF ((SameString(Alphas(1),RefrigData(Loop)%Name)).AND.(SameString(Alphas(2),Enthalpy))) THEN
        NumOfPressPts = NumOfPressPts + 1
        IF (Numbers(1) <= 0.0) THEN
          CALL ShowSevereError('GetFluidPropertiesData: Negative pressures not allowed in fluid property input data')
          CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
          CALL ShowContinueError('...Value =['//trim(RoundSigDigits(Numbers(1),3))//'].')
          ErrorsFound=.true.
        ENDIF
        PressurePtr(NumOfPressPts)%Pressure = Numbers(1)
        PressurePtr(NumOfPressPts)%InPtr = InData
      END IF
    ENDDO

    ! Sort Pressure list
    ! insertionSort
    do InData=2,NumOfPressPts
      pTemp = PressurePtr(InData)%Pressure
      iTemp = PressurePtr(InData)%InPtr
      j = InData-1
      do while (j >= 1 .and. PressurePtr(j)%Pressure > pTemp)
        PressurePtr(j+1)%Pressure = PressurePtr(j)%Pressure
        PressurePtr(j+1)%InPtr = PressurePtr(j)%InPtr
        j = j-1
        if (j == 0) exit
      enddo
      PressurePtr(j+1)%Pressure = pTemp
      PressurePtr(j+1)%InPtr = iTemp
    end do

    DO InData = 1, NumOfPressPts
      CALL GetObjectItem(TRIM(CurrentModuleObject),PressurePtr(InData)%InPtr,Alphas,NumAlphas, &
                          Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      RefrigData(Loop)%SHPress(InData) = Numbers(1)
      ! a little error trapping
      IF (InData > 1) THEN
        IF (RefrigData(Loop)%SHPress(InData) <= RefrigData(Loop)%SHPress(Indata-1)) THEN
          CALL ShowSevereError('GetFluidPropertiesData: Pressures must be entered in ascending order for fluid property data')
          CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
          CALL ShowContinueError('First Occurance at Pressure('//TRIM(RoundSigDigits(Indata-1))//  &
                                 ') {'//TRIM(RoundSigDigits(RefrigData(Loop)%SHPress(Indata-1),3))//  &
                                 '} >= Pressure('//TRIM(RoundSigDigits(Indata))//  &
                                 ') {'//TRIM(RoundSigDigits(RefrigData(Loop)%SHPress(Indata),3))//'}')
          ErrorsFound=.true.
          EXIT
        ENDIF
      END IF
      IF ((NumNumbers-1) == RefrigData(Loop)%NumSuperTempPts) THEN
        RefrigData(Loop)%HshValues(1:RefrigData(Loop)%NumSuperTempPts,Indata) = Numbers(2:NumNumbers)
      ELSE
        CALL ShowSevereError('GetFluidPropertiesData: Number of superheated enthalpy data points '// &
                             'not equal to number of temperature points')
        CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
        ErrorsFound=.true.
      END IF
    END DO

    DEALLOCATE(PressurePtr)

          ! Get: ***** DENSITY of SUPERHEATED GAS  *****
          ! First find the number of pressure value syntax lines have been entered and
          ! make sure that all of the pressure input is linked to the same temperature list
          ! Then allocate the arrays and read the data into the proper place
    ALLOCATE(RefrigData(Loop)%RhoshValues(RefrigData(Loop)%NumSuperTempPts,RefrigData(Loop)%NumSuperPressPts))
    CurrentModuleObject = 'FluidProperties:Superheated'
    NumOfPressPts = 0
    ALLOCATE(PressurePtr(NumOfSHFluidPropArrays))
    DO InData = 1, NumOfSHFluidPropArrays
      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas, &
                          Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IF ((SameString(Alphas(1),RefrigData(Loop)%Name)).AND.(SameString(Alphas(2),Density))) THEN
        NumOfPressPts = NumOfPressPts + 1
        IF (Numbers(1) <= 0.0) THEN
          CALL ShowSevereError('GetFluidPropertiesData: Negative pressures not allowed in fluid property input data')
          CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
          CALL ShowContinueError('...Value =['//trim(RoundSigDigits(Numbers(1),3))//'].')
          ErrorsFound=.true.
        ENDIF
        PressurePtr(NumOfPressPts)%Pressure = Numbers(1)
        PressurePtr(NumOfPressPts)%InPtr = InData
      END IF
    ENDDO

    ! Sort Pressure list
    ! insertionSort
    do InData=2,NumOfPressPts
      pTemp = PressurePtr(InData)%Pressure
      iTemp = PressurePtr(InData)%InPtr
      j = InData-1
      do while (j >= 1 .and. PressurePtr(j)%Pressure > pTemp)
        PressurePtr(j+1)%Pressure = PressurePtr(j)%Pressure
        PressurePtr(j+1)%InPtr = PressurePtr(j)%InPtr
        j = j-1
        if (j == 0) exit
      enddo
      PressurePtr(j+1)%Pressure = pTemp
      PressurePtr(j+1)%InPtr = iTemp
    end do

    DO InData = 1, NumOfPressPts
      CALL GetObjectItem(TRIM(CurrentModuleObject),PressurePtr(InData)%InPtr,Alphas,NumAlphas, &
                          Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IF (ABS(Numbers(1)-RefrigData(Loop)%SHPress(Indata)) > PressToler) THEN
        CALL ShowSevereError('GetFluidPropertiesData: All superheated data for the same refrigerant must '// &
                             'use the same pressure data')
        CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
        ErrorsFound=.true.
      END IF
      IF (.NOT.SameString(TempsName,Alphas(3))) THEN
        CALL ShowSevereError('GetFluidPropertiesData: All superheated data for the same property must use '// &
                             'the same temperature list')
        CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
        ErrorsFound=.true.
      END IF
      IF ((NumNumbers-1) == RefrigData(Loop)%NumSuperTempPts) THEN
        RefrigData(Loop)%RhoshValues(1:RefrigData(Loop)%NumSuperTempPts,InData) = Numbers(2:NumNumbers)
      ELSE
        CALL ShowSevereError('GetFluidPropertiesData: Number of superheated density data points not equal to '// &
                             'number of temperature points')
        CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
        ErrorsFound=.true.
      END IF
    END DO

    DEALLOCATE(PressurePtr)

    IF (NumOfPressPts == 0) THEN
      !CALL ShowSevereError('GetFluidPropertiesData: No pressure data found for superheated density')
      !CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name)) !RS: Secret Search String
      WRITE(DebugFile,*) 'GetFluidPropertiesData: No pressure data found for superheated density'
      WRITE(DebugFile,*) 'Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name)
      ErrorsFound=.true.
    END IF
    IF (NumOfPressPts /= RefrigData(Loop)%NumSuperPressPts) THEN
      CALL ShowSevereError('GetFluidPropertiesData: Number of pressure points for superheated data '// &
                           'different for enthalpy and density')
      CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(RefrigData(Loop)%Name))
      ErrorsFound=.true.
    END IF

  END DO    ! ...end of DO loop through all of the refrigerants

          ! *************** GLYCOLS ***************
          ! Go through each glycol found in the fluid names statement and read in the data
          ! Note that every valid fluid must have ALL of the necessary data or a fatal error will
          ! be produced.
  CurrentModuleObject = 'FluidProperties:Concentration'
  DO Loop = 1, NumOfGlycols

          ! Get: ***** SPECIFIC HEAT of GLYCOLS  *****
          ! First find the number of concentration value syntax lines have been entered and
          ! make sure that all of the concentration input is linked to the same temperature list
    TempsName     = " "
    FirstSHMatch  = .TRUE.
    NumOfConcPts = 0
    GlyRawData(Loop)%CpDataPresent = .FALSE.
    DO InData = 1, NumOfGlyFluidPropArrays  ! check temperatures given for specific heat are consistant
      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IF ((SameString(Alphas(1),GlyRawData(Loop)%Name)).AND.(SameString(Alphas(2),SpecificHeat))) THEN
        NumOfConcPts = NumOfConcPts + 1
        IF (FirstSHMatch) THEN
          TempsName = Alphas(3)
          FirstSHMatch = .FALSE.
        ELSE
          IF (.NOT.SameString(TempsName,Alphas(3))) THEN
            CALL ShowSevereError('GetFluidPropertiesData: All glycol specific heat data for the same glycol must use '// &
                                 'the same temperature list')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
            CALL ShowContinueError('Expected name='//TRIM(TempsName)//', Entered name='//TRIM(Alphas(3)))
            ErrorsFound=.true.
          END IF
        END IF
      END IF
    END DO
    IF (NumOfConcPts > 0) THEN
          ! Now allocate the arrays and read the data into the proper place
          ! First, allocate the temperature array and transfer the data from the FluidTemp array
      GlyRawData(Loop)%CpDataPresent = .TRUE.
      DO TempLoop = 1, NumOfFluidTempArrays
        IF (SameString(TempsName,FluidTemps(TempLoop)%Name)) THEN
          GlyRawData(Loop)%NumCpTempPts = FluidTemps(TempLoop)%NumOfTemps
          ALLOCATE(GlyRawData(Loop)%CpTemps(GlyRawData(Loop)%NumCpTempPts))
          GlyRawData(Loop)%CpTemps = FluidTemps(TempLoop)%Temps
          EXIT ! the TempLoop DO loop
        END IF
        IF (TempLoop == NumOfFluidTempArrays) THEN
          CALL ShowSevereError('GetFluidPropertiesData: No match for temperature array name found with glycol data')
          CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
          ErrorsFound=.true.
        END IF
      END DO

            ! Next, allocate the specific heat related arrays
      GlyRawData(Loop)%NumCpConcPts = NumOfConcPts
      ALLOCATE(GlyRawData(Loop)%CpConcs(GlyRawData(Loop)%NumCpConcPts))
      ALLOCATE(GlyRawData(Loop)%CpValues(GlyRawData(Loop)%NumCpTempPts,GlyRawData(Loop)%NumCpConcPts))

            ! Finally, get the specific heat and concentration values from the user input
      CurrentModuleObject = 'FluidProperties:Concentration'
      NumOfConcPts = 0
      DO InData = 1, NumOfGlyFluidPropArrays
        CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        IF ((SameString(Alphas(1),GlyRawData(Loop)%Name)).AND.(SameString(Alphas(2),SpecificHeat))) THEN
          NumOfConcPts = NumOfConcPts + 1
          GlyRawData(Loop)%CpConcs(NumOfConcPts) = Numbers(1)
          ! a little error trapping
          IF (NumOfConcPts == 1) THEN
            IF (GlyRawData(Loop)%CpConcs(NumOfConcPts) < 0.0) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Negative concentrations not allowed in fluid property input data')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
              ErrorsFound=.true.
            END IF
          ELSE
            IF (GlyRawData(Loop)%CpConcs(NumOfConcPts) <= GlyRawData(Loop)%CpConcs(NumOfConcPts-1)) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Concentrations must be entered in ascending order '// &
                                   'for fluid property data')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
              ErrorsFound=.true.
            END IF
          END IF
          IF ((NumNumbers-1) == GlyRawData(Loop)%NumCpTempPts) THEN
            GlyRawData(Loop)%CpValues(1:GlyRawData(Loop)%NumCpTempPts,NumOfConcPts) = Numbers(2:NumNumbers)
          ELSE
            CALL ShowSevereError('GetFluidPropertiesData: Number of specific heat data points not equal to number of '// &
                                 'temperature points')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
            ErrorsFound=.true.
          END IF
        END IF
      END DO
    END IF
          ! Get: ***** DENSITY of GLYCOLS  *****
          ! First find the number of concentration value syntax lines have been entered and
          ! make sure that all of the concentration input is linked to the same temperature list
    TempsName     = " "
    FirstSHMatch  = .TRUE.
    NumOfConcPts = 0
    GlyRawData(Loop)%RhoDataPresent = .FALSE.
    CurrentModuleObject = 'FluidProperties:Concentration'
    DO InData = 1, NumOfGlyFluidPropArrays  ! check temperatures given for density are consistant
      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IF ((SameString(Alphas(1),GlyRawData(Loop)%Name)).AND.(SameString(Alphas(2),Density))) THEN
        NumOfConcPts = NumOfConcPts + 1
        IF (FirstSHMatch) THEN
          TempsName = Alphas(3)
          FirstSHMatch = .FALSE.
        ELSE
          IF (.NOT.SameString(TempsName,Alphas(3))) THEN
            CALL ShowSevereError('GetFluidPropertiesData: All glycol density data for the same glycol must use '// &
                                 'the same temperature list')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
            CALL ShowContinueError('Expected name='//TRIM(TempsName)//', Entered name='//TRIM(Alphas(3)))
            ErrorsFound=.true.
          END IF
        END IF
      END IF
    END DO
    IF (NumOfConcPts > 0) THEN
          ! Now allocate the arrays and read the data into the proper place
          ! First, allocate the temperature array and transfer the data from the FluidTemp array
      GlyRawData(Loop)%RhoDataPresent = .TRUE.
      DO TempLoop = 1, NumOfFluidTempArrays
        IF (SameString(TempsName,FluidTemps(TempLoop)%Name)) THEN
          GlyRawData(Loop)%NumRhoTempPts = FluidTemps(TempLoop)%NumOfTemps
          ALLOCATE(GlyRawData(Loop)%RhoTemps(GlyRawData(Loop)%NumRhoTempPts))
          GlyRawData(Loop)%RhoTemps = FluidTemps(TempLoop)%Temps
          EXIT ! the TempLoop DO loop
        END IF
        IF (TempLoop == NumOfFluidTempArrays) THEN
          CALL ShowSevereError('GetFluidPropertiesData: No match for temperature array name found with glycol data')
          CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
          ErrorsFound=.true.
        END IF
      END DO

            ! Next, allocate the density related arrays
      GlyRawData(Loop)%NumRhoConcPts = NumOfConcPts
      ALLOCATE(GlyRawData(Loop)%RhoConcs(GlyRawData(Loop)%NumRhoConcPts))
      ALLOCATE(GlyRawData(Loop)%RhoValues(GlyRawData(Loop)%NumRhoTempPts,GlyRawData(Loop)%NumRhoConcPts))

            ! Finally, get the density and concentration values from the user input
      NumOfConcPts = 0
      CurrentModuleObject = 'FluidProperties:Concentration'
      DO InData = 1, NumOfGlyFluidPropArrays
        CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        IF ((SameString(Alphas(1),GlyRawData(Loop)%Name)).AND.(SameString(Alphas(2),Density))) THEN
          NumOfConcPts = NumOfConcPts + 1
          GlyRawData(Loop)%RhoConcs(NumOfConcPts) = Numbers(1)
          ! a little error trapping
          IF (NumOfConcPts == 1) THEN
            IF (GlyRawData(Loop)%RhoConcs(NumOfConcPts) < 0.0) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Negative concentrations not allowed in fluid property input data')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
              ErrorsFound=.true.
            END IF
          ELSE
            IF (GlyRawData(Loop)%RhoConcs(NumOfConcPts) <= GlyRawData(Loop)%RhoConcs(NumOfConcPts-1)) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Concentrations must be entered in ascending order '// &
                                   'for fluid property data')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
              ErrorsFound=.true.
            END IF
          END IF
          IF ((NumNumbers-1) == GlyRawData(Loop)%NumRhoTempPts) THEN
            GlyRawData(Loop)%RhoValues(1:GlyRawData(Loop)%NumRhoTempPts,NumOfConcPts) = Numbers(2:NumNumbers)
          ELSE
            CALL ShowSevereError('GetFluidPropertiesData: Number of density data points not equal to number of '// &
                                 'temperature points')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
            ErrorsFound=.true.
          END IF
        END IF
      END DO
    END IF
          ! Get: ***** CONDUCTIVITY of GLYCOLS  *****
          ! First find the number of concentration value syntax lines have been entered and
          ! make sure that all of the concentration input is linked to the same temperature list
    TempsName     = " "
    FirstSHMatch  = .TRUE.
    NumOfConcPts = 0
    GlyRawData(Loop)%CondDataPresent = .FALSE.
    CurrentModuleObject = 'FluidProperties:Concentration'
    DO InData = 1, NumOfGlyFluidPropArrays  ! check temperatures given for conductivity are consistant
      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IF ((SameString(Alphas(1),GlyRawData(Loop)%Name)).AND.(SameString(Alphas(2),Conductivity))) THEN
        NumOfConcPts = NumOfConcPts + 1
        IF (FirstSHMatch) THEN
          TempsName = Alphas(3)
          FirstSHMatch = .FALSE.
        ELSE
          IF (.NOT.SameString(TempsName,Alphas(3))) THEN
            CALL ShowSevereError('GetFluidPropertiesData: All glycol conductivity data for the same glycol must use '// &
                                 'the same temperature list')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
            CALL ShowContinueError('Expected name='//TRIM(TempsName)//', Entered name='//TRIM(Alphas(3)))
            ErrorsFound=.true.
          END IF
        END IF
      END IF
    END DO
    IF (NumOfConcPts > 0) THEN
          ! Now allocate the arrays and read the data into the proper place
          ! First, allocate the temperature array and transfer the data from the FluidTemp array
      GlyRawData(Loop)%CondDataPresent = .TRUE.
      DO TempLoop = 1, NumOfFluidTempArrays
        IF (SameString(TempsName,FluidTemps(TempLoop)%Name)) THEN
          GlyRawData(Loop)%NumCondTempPts = FluidTemps(TempLoop)%NumOfTemps
          ALLOCATE(GlyRawData(Loop)%CondTemps(GlyRawData(Loop)%NumCondTempPts))
          GlyRawData(Loop)%CondTemps = FluidTemps(TempLoop)%Temps
          EXIT ! the TempLoop DO loop
        END IF
        IF (TempLoop == NumOfFluidTempArrays) THEN
          CALL ShowSevereError('GetFluidPropertiesData: No match for temperature array name found with glycol data')
          CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
          ErrorsFound=.true.
        END IF
      END DO

            ! Next, allocate the conductivity related arrays
      GlyRawData(Loop)%NumCondConcPts = NumOfConcPts
      ALLOCATE(GlyRawData(Loop)%CondConcs(GlyRawData(Loop)%NumCondConcPts))
      ALLOCATE(GlyRawData(Loop)%CondValues(GlyRawData(Loop)%NumCondTempPts,GlyRawData(Loop)%NumCondConcPts))

            ! Finally, get the conductivity and concentration values from the user input
      NumOfConcPts = 0
      CurrentModuleObject = 'FluidProperties:Concentration'
      DO InData = 1, NumOfGlyFluidPropArrays
        CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        IF ((SameString(Alphas(1),GlyRawData(Loop)%Name)).AND.(SameString(Alphas(2),Conductivity))) THEN
          NumOfConcPts = NumOfConcPts + 1
          GlyRawData(Loop)%CondConcs(NumOfConcPts) = Numbers(1)
          ! a little error trapping
          IF (NumOfConcPts == 1) THEN
            IF (GlyRawData(Loop)%CondConcs(NumOfConcPts) < 0.0) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Negative concentrations not allowed in fluid property input data')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
              ErrorsFound=.true.
            END IF
          ELSE
            IF (GlyRawData(Loop)%CondConcs(NumOfConcPts) <= GlyRawData(Loop)%CondConcs(NumOfConcPts-1)) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Concentrations must be entered in ascending order '// &
                                   'for fluid property data')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
              ErrorsFound=.true.
            END IF
          END IF
          IF ((NumNumbers-1) == GlyRawData(Loop)%NumCondTempPts) THEN
            GlyRawData(Loop)%CondValues(1:GlyRawData(Loop)%NumCondTempPts,NumOfConcPts) = Numbers(2:NumNumbers)
          ELSE
            CALL ShowSevereError('GetFluidPropertiesData: Number of conductivity data points not equal to number of '// &
                                 'temperature points')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
            ErrorsFound=.true.
          END IF
        END IF
      END DO
    END IF
          ! Get: ***** VISCOSITY of GLYCOLS  *****
          ! First find the number of concentration value syntax lines have been entered and
          ! make sure that all of the concentration input is linked to the same temperature list
    TempsName     = " "
    FirstSHMatch  = .TRUE.
    NumOfConcPts = 0
    GlyRawData(Loop)%ViscDataPresent = .FALSE.
    CurrentModuleObject = 'FluidProperties:Concentration'
    DO InData = 1, NumOfGlyFluidPropArrays  ! check temperatures given for viscosity are consistant
      CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IF ((SameString(Alphas(1),GlyRawData(Loop)%Name)).AND.(SameString(Alphas(2),Viscosity))) THEN
        NumOfConcPts = NumOfConcPts + 1
        IF (FirstSHMatch) THEN
          TempsName = Alphas(3)
          FirstSHMatch = .FALSE.
        ELSE
          IF (.NOT.SameString(TempsName,Alphas(3))) THEN
            CALL ShowSevereError('GetFluidPropertiesData: All glycol viscosity data for the same glycol must use '// &
                                 'the same temperature list')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
            CALL ShowContinueError('Expected name='//TRIM(TempsName)//', Entered name='//TRIM(Alphas(3)))
            ErrorsFound=.true.
          END IF
        END IF
      END IF
    END DO
    IF (NumOfConcPts > 0) THEN
      GlyRawData(Loop)%ViscDataPresent = .TRUE.
          ! Now allocate the arrays and read the data into the proper place
          ! First, allocate the temperature array and transfer the data from the FluidTemp array
      DO TempLoop = 1, NumOfFluidTempArrays
        IF (SameString(TempsName,FluidTemps(TempLoop)%Name)) THEN
          GlyRawData(Loop)%NumViscTempPts = FluidTemps(TempLoop)%NumOfTemps
          ALLOCATE(GlyRawData(Loop)%ViscTemps(GlyRawData(Loop)%NumViscTempPts))
          GlyRawData(Loop)%ViscTemps = FluidTemps(TempLoop)%Temps
          EXIT ! the TempLoop DO loop
        END IF
        IF (TempLoop == NumOfFluidTempArrays) THEN
          CALL ShowSevereError('GetFluidPropertiesData: No match for temperature array name found with glycol data')
          CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
          ErrorsFound=.true.
        END IF
      END DO

            ! Next, allocate the viscosity related arrays
      GlyRawData(Loop)%NumViscConcPts = NumOfConcPts
      ALLOCATE(GlyRawData(Loop)%ViscConcs(GlyRawData(Loop)%NumViscConcPts))
      ALLOCATE(GlyRawData(Loop)%ViscValues(GlyRawData(Loop)%NumViscTempPts,GlyRawData(Loop)%NumViscConcPts))

            ! Finally, get the viscosity and concentration values from the user input
      NumOfConcPts = 0
      CurrentModuleObject = 'FluidProperties:Concentration'
      DO InData = 1, NumOfGlyFluidPropArrays
        CALL GetObjectItem(TRIM(CurrentModuleObject),InData,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        IF ((SameString(Alphas(1),GlyRawData(Loop)%Name)).AND.(SameString(Alphas(2),Viscosity))) THEN
          NumOfConcPts = NumOfConcPts + 1
          GlyRawData(Loop)%ViscConcs(NumOfConcPts) = Numbers(1)
          ! a little error trapping
          IF (NumOfConcPts == 1) THEN
            IF (GlyRawData(Loop)%ViscConcs(NumOfConcPts) < 0.0) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Negative concentrations not allowed in fluid property input data')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
              ErrorsFound=.true.
            END IF
          ELSE
            IF (GlyRawData(Loop)%ViscConcs(NumOfConcPts) <= GlyRawData(Loop)%ViscConcs(NumOfConcPts-1)) THEN
              CALL ShowSevereError('GetFluidPropertiesData: Concentrations must be entered in ascending order '// &
                                   'for fluid property data')
              CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
              ErrorsFound=.true.
            END IF
          END IF
          IF ((NumNumbers-1) == GlyRawData(Loop)%NumViscTempPts) THEN
            GlyRawData(Loop)%ViscValues(1:GlyRawData(Loop)%NumViscTempPts,NumOfConcPts) = Numbers(2:NumNumbers)
          ELSE
            CALL ShowSevereError('GetFluidPropertiesData: Number of viscosity data points not equal to number of '// &
                                 'temperature points')
            CALL ShowContinueError('Error occurs in '//TRIM(CurrentModuleObject)//' Name='//TRIM(GlyRawData(Loop)%Name))
            ErrorsFound=.true.
          END IF
        END IF
      END DO
    END IF
  END DO  ! glycol loop

          ! Get: ***** GLYCOL CONCENTRATIONS *****
          ! Read in the GlycolConcentrations input and then set the property data accordingly
          ! Input Syntax:
          ! FluidProperties:GlycolConcentration,
          !       \memo glycol and what concentration it is
          !  A1,  \field Name
          !       \type alpha
          !       \required-field
          !       \reference GlycolConcentrations
          !  A2,  \field Glycol Type
          !       \required-field
          !       \type choice
          !       \key EthyleneGlycol
          !       \key PropyleneGlycol
          !       \key UserDefinedGlycolType
          !       \note or UserDefined Fluid (must show up as a glycol in FluidProperties:Name object)
          !  A3,  \field User Defined Glycol Name
          !       \type object-list
          !       \object-list FluidAndGlycolNames
          !  N1;  \field Glycol Concentration
          !       \type real
          !       \minimum 0.0
          !       \maximum 1.0

          ! Check to see if there is any GlycolConcentrations input.  If not, this
          ! is okay as long as the user only desires to simulate loops with water.
          ! More than one GlycolConcentrations input is not allowed.

  CurrentModuleObject = 'FluidProperties:GlycolConcentration'
  NumOfOptionalInput = GetNumObjectsFound(TRIM(CurrentModuleObject))

  NumOfGlyConcs=NumOfOptionalInput+1
  ALLOCATE(GlycolData(NumOfGlyConcs))
  ALLOCATE(GlycolUsed(NumOfGlyConcs))
  GlycolUsed=.false.
  GlycolUsed(1)=.true.  ! mark Water as always used

          ! First "glycol" is always pure water.  Load data from default arrays
  GlycolData(1)%Name = 'WATER'
  GlycolData(1)%GlycolName      = 'WATER'
  GlycolData(1)%GlycolIndex     = 0
  GlycolData(1)%Concentration   = 1.0
  GlycolData(1)%CpDataPresent   = .TRUE.
  GlycolData(1)%NumCpTempPts    = DefaultNumGlyTemps
  GlycolData(1)%RhoDataPresent  = .TRUE.
  GlycolData(1)%NumRhoTempPts   = DefaultNumGlyTemps
  GlycolData(1)%CondDataPresent = .TRUE.
  GlycolData(1)%NumCondTempPts  = DefaultNumGlyTemps
  GlycolData(1)%ViscDataPresent = .TRUE.
  GlycolData(1)%NumViscTempPts  = DefaultNumGlyTemps
  ALLOCATE(GlycolData(1)%CpTemps(GlycolData(1)%NumCpTempPts))
  ALLOCATE(GlycolData(1)%CpValues(GlycolData(1)%NumCpTempPts))
  ALLOCATE(GlycolData(1)%RhoTemps(GlycolData(1)%NumRhoTempPts))
  ALLOCATE(GlycolData(1)%RhoValues(GlycolData(1)%NumRhoTempPts))
  ALLOCATE(GlycolData(1)%CondTemps(GlycolData(1)%NumCondTempPts))
  ALLOCATE(GlycolData(1)%CondValues(GlycolData(1)%NumCondTempPts))
  ALLOCATE(GlycolData(1)%ViscTemps(GlycolData(1)%NumViscTempPts))
  ALLOCATE(GlycolData(1)%ViscValues(GlycolData(1)%NumViscTempPts))
  GlycolData(1)%CpTemps         = DefaultGlycolTemps
  GlycolData(1)%CpValues        = DefaultWaterCpData
  GlycolData(1)%RhoTemps        = DefaultGlycolTemps
  GlycolData(1)%RhoValues       = DefaultWaterRhoData
  GlycolData(1)%CondTemps       = DefaultGlycolTemps
  GlycolData(1)%CondValues      = DefaultWaterCondData
  GlycolData(1)%ViscTemps       = DefaultGlycolTemps
  GlycolData(1)%ViscValues      = DefaultWaterViscData

  NumOfGlyConcs = 1  ! Water is always available, everything else must be specified

  DO Loop = 1, NumOfOptionalInput
    CALL GetObjectItem(TRIM(CurrentModuleObject),Loop,Alphas,NumAlphas,Numbers,NumNumbers,Status,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
          ! Check to see if glycol name is one of the defaults or is listed in the Fluid Name list
    ErrorInName=.false.
    IsBlank=.false.
    CALL VerifyName(Alphas(1),GlycolData%Name,NumOfGlyConcs,ErrorInName,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (ErrorInName) THEN
      CALL ShowContinueError('...Fluid names must be unique regardless of subtype.')
      ErrorsFound=.true.
      CYCLE
    ENDIF
    GlycolFound = .FALSE.
    IF (SameString(Alphas(2),EthyleneGlycol)) THEN
      GlycolFound = .TRUE.
      NumOfGlyConcs=NumOfGlyConcs+1
      GlycolData(NumOfGlyConcs)%Name = Alphas(1)
      GlycolData(NumOfGlyConcs)%GlycolName = Alphas(2)
    ELSE IF (SameString(Alphas(2),PropyleneGlycol)) THEN
      GlycolFound = .TRUE.
      NumOfGlyConcs=NumOfGlyConcs+1
      GlycolData(NumOfGlyConcs)%Name = Alphas(1)
      GlycolData(NumOfGlyConcs)%GlycolName = Alphas(2)
    ELSEIF (SameString(Alphas(2),'UserDefinedGlycolType')) THEN
      DO InData = 1, NumOfGlycols
        IF (SameString(Alphas(3),GlyRawData(InData)%Name)) THEN
          GlycolFound = .TRUE.
          EXIT ! DO LOOP through user defined glycols
        END IF
      END DO
      IF (GlycolFound) THEN
        NumOfGlyConcs = NumOfGlyConcs + 1
        GlycolData(NumOfGlyConcs)%Name = Alphas(1)
        GlycolData(NumOfGlyConcs)%GlycolName = Alphas(3)
      ELSE
        CALL ShowSevereError('GetFluidPropertiesData: '//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//'", invalid reference')
        CALL ShowContinueError('... not found in the FluidProperties:Name list: "'//TRIM(Alphas(3))//'".')
        ErrorsFound = .TRUE.
      END IF
    ELSE
      CALL ShowSevereError('GetFluidPropertiesData: '//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//'", invalid field')
      CALL ShowContinueError('...'//trim(cAlphaFieldNames(2))//'="'//trim(Alphas(2))//'".')
      CALL ShowContinueError('... Legal values are PropoleneGlycol, EthyleneGlycol or UserDefinedGlycolType.')
      ErrorsFound=.true.
    END IF
    IF (.not. GlycolFound) CYCLE
    GlycolData(NumOfGlyConcs)%Concentration = Numbers(1)
  END DO


          ! Now initialize the rest of the data for the glycols
  DO Loop = 2, NumOfGlyConcs
        ! Check to see if glycol name is one of the defaults or is listed in the Fluid Name list
    IF (SameString(GlycolData(Loop)%GlycolName,EthyleneGlycol)) THEN
      GlycolData(Loop)%GlycolIndex = EthyleneGlycolIndex
    ELSE IF (SameString(GlycolData(Loop)%GlycolName,PropyleneGlycol)) THEN
      GlycolData(Loop)%GlycolIndex = PropyleneGlycolIndex
    ELSE
      DO InData = 1, NumOfGlycols
        IF (SameString(GlycolData(Loop)%GlycolName,GlyRawData(InData)%Name)) THEN
          GlycolData(Loop)%GlycolIndex = InData
          EXIT ! DO LOOP through user defined glycols
        END IF
      END DO
    END IF

          ! Set the rest of the parameters...
    IF ( (GlycolData(Loop)%GlycolIndex == EthyleneGlycolIndex) .OR. &
         (GlycolData(Loop)%GlycolIndex == PropyleneGlycolIndex) ) THEN

      GlycolData(Loop)%CpDataPresent   = .TRUE.
      GlycolData(Loop)%NumCpTempPts    = DefaultNumGlyTemps
      GlycolData(Loop)%RhoDataPresent  = .TRUE.
      GlycolData(Loop)%NumRhoTempPts   = DefaultNumGlyTemps
      GlycolData(Loop)%CondDataPresent = .TRUE.
      GlycolData(Loop)%NumCondTempPts  = DefaultNumGlyTemps
      GlycolData(Loop)%ViscDataPresent = .TRUE.
      GlycolData(Loop)%NumViscTempPts  = DefaultNumGlyTemps
      ALLOCATE(GlycolData(Loop)%CpTemps(GlycolData(Loop)%NumCpTempPts))
      ALLOCATE(GlycolData(Loop)%CpValues(GlycolData(Loop)%NumCpTempPts))
      ALLOCATE(GlycolData(Loop)%RhoTemps(GlycolData(Loop)%NumRhoTempPts))
      ALLOCATE(GlycolData(Loop)%RhoValues(GlycolData(Loop)%NumRhoTempPts))
      ALLOCATE(GlycolData(Loop)%CondTemps(GlycolData(Loop)%NumCondTempPts))
      ALLOCATE(GlycolData(Loop)%CondValues(GlycolData(Loop)%NumCondTempPts))
      ALLOCATE(GlycolData(Loop)%ViscTemps(GlycolData(Loop)%NumViscTempPts))
      ALLOCATE(GlycolData(Loop)%ViscValues(GlycolData(Loop)%NumViscTempPts))
      GlycolData(Loop)%CpTemps         = DefaultGlycolTemps
      GlycolData(Loop)%RhoTemps        = DefaultGlycolTemps
      GlycolData(Loop)%CondTemps       = DefaultGlycolTemps
      GlycolData(Loop)%ViscTemps       = DefaultGlycolTemps

      IF (GlycolData(Loop)%GlycolIndex == EthyleneGlycolIndex) THEN
        CALL InterpDefValuesForGlycolConc(DefaultNumGlyConcs,DefaultNumGlyTemps, &
                                          DefaultGlycolConcs,DefaultEthGlyCpData, &
                                          GlycolData(Loop)%Concentration,GlycolData(Loop)%CpValues)
        CALL InterpDefValuesForGlycolConc(DefaultNumGlyConcs,DefaultNumGlyTemps, &
                                          DefaultGlycolConcs,DefaultEthGlyRhoData, &
                                          GlycolData(Loop)%Concentration,GlycolData(Loop)%RhoValues)
        CALL InterpDefValuesForGlycolConc(DefaultNumGlyConcs,DefaultNumGlyTemps, &
                                          DefaultGlycolConcs,DefaultEthGlyCondData, &
                                          GlycolData(Loop)%Concentration,GlycolData(Loop)%CondValues)
        CALL InterpDefValuesForGlycolConc(DefaultNumGlyConcs,DefaultNumGlyTemps, &
                                          DefaultGlycolConcs,DefaultEthGlyViscData, &
                                          GlycolData(Loop)%Concentration,GlycolData(Loop)%ViscValues)
      ELSE    ! == PropyleneGlycolIndex
        CALL InterpDefValuesForGlycolConc(DefaultNumGlyConcs,DefaultNumGlyTemps, &
                                          DefaultGlycolConcs,DefaultPropGlyCpData, &
                                          GlycolData(Loop)%Concentration,GlycolData(Loop)%CpValues)
        CALL InterpDefValuesForGlycolConc(DefaultNumGlyConcs,DefaultNumGlyTemps, &
                                          DefaultGlycolConcs,DefaultPropGlyRhoData, &
                                          GlycolData(Loop)%Concentration,GlycolData(Loop)%RhoValues)
        CALL InterpDefValuesForGlycolConc(DefaultNumGlyConcs,DefaultNumGlyTemps, &
                                          DefaultGlycolConcs,DefaultPropGlyCondData, &
                                          GlycolData(Loop)%Concentration,GlycolData(Loop)%CondValues)
        CALL InterpDefValuesForGlycolConc(DefaultNumGlyConcs,DefaultNumGlyTemps, &
                                          DefaultGlycolConcs,DefaultPropGlyViscData, &
                                          GlycolData(Loop)%Concentration,GlycolData(Loop)%ViscValues)
      END IF

    ELSE  ! User-defined fluid

      Index = GlycolData(Loop)%GlycolIndex

          ! Specific heat data:
      IF (GlyRawData(Index)%CpDataPresent) THEN
        GlycolData(Loop)%CpDataPresent = .TRUE.
        GlycolData(Loop)%NumCpTempPts  = GlyRawData(Index)%NumCpTempPts
        ALLOCATE(GlycolData(Loop)%CpTemps(GlycolData(Loop)%NumCpTempPts))
        ALLOCATE(GlycolData(Loop)%CpValues(GlycolData(Loop)%NumCpTempPts))
        GlycolData(Loop)%CpTemps = GlyRawData(Index)%CpTemps
        CALL InterpValuesForGlycolConc(GlyRawData(Index)%NumCpConcPts,GlyRawData(Index)%NumCpTempPts, &
                                       GlyRawData(Index)%CpConcs,GlyRawData(Index)%CpValues, &
                                       GlycolData(Loop)%Concentration,GlycolData(Loop)%CpValues)
      ELSE
        CALL ShowSevereError('GetFluidPropertiesData: Specific heat data not entered for a '//TRIM(CurrentModuleObject))
        CALL ShowContinueError('ALL data must be entered for user-defined glycols')
        CALL ShowContinueError('Glycol mixture name = '//TRIM(GlycolData(Loop)%Name))
        CALL ShowContinueError('Glycol fluid name = '//TRIM(GlycolData(Loop)%GlycolName))
        ErrorsFound=.TRUE.
      END IF

          ! Density data:
      IF (GlyRawData(Index)%CpDataPresent) THEN
        GlycolData(Loop)%RhoDataPresent = .TRUE.
        GlycolData(Loop)%NumRhoTempPts  = GlyRawData(Index)%NumRhoTempPts
        ALLOCATE(GlycolData(Loop)%RhoTemps(GlycolData(Loop)%NumRhoTempPts))
        ALLOCATE(GlycolData(Loop)%RhoValues(GlycolData(Loop)%NumRhoTempPts))
        GlycolData(Loop)%RhoTemps = GlyRawData(Index)%RhoTemps
        CALL InterpValuesForGlycolConc(GlyRawData(Index)%NumRhoConcPts,GlyRawData(Index)%NumRhoTempPts, &
                                       GlyRawData(Index)%RhoConcs,GlyRawData(Index)%RhoValues, &
                                       GlycolData(Loop)%Concentration,GlycolData(Loop)%RhoValues)
      ELSE
        CALL ShowSevereError('GetFluidPropertiesData: Density data not entered for a '//TRIM(CurrentModuleObject))
        CALL ShowContinueError('ALL data must be entered for user-defined glycols')
        CALL ShowContinueError('Glycol mixture name = '//TRIM(GlycolData(Loop)%Name))
        CALL ShowContinueError('Glycol fluid name = '//TRIM(GlycolData(Loop)%GlycolName))
        ErrorsFound=.TRUE.
      END IF

          ! Conductivity data:
      IF (GlyRawData(Index)%CondDataPresent) THEN
        GlycolData(Loop)%CondDataPresent = .TRUE.
        GlycolData(Loop)%NumCondTempPts  = GlyRawData(Index)%NumCondTempPts
        ALLOCATE(GlycolData(Loop)%CondTemps(GlycolData(Loop)%NumCondTempPts))
        ALLOCATE(GlycolData(Loop)%CondValues(GlycolData(Loop)%NumCondTempPts))
        GlycolData(Loop)%CondTemps = GlyRawData(Index)%CondTemps
        CALL InterpValuesForGlycolConc(GlyRawData(Index)%NumCondConcPts,GlyRawData(Index)%NumCondTempPts, &
                                       GlyRawData(Index)%CondConcs,GlyRawData(Index)%CondValues, &
                                       GlycolData(Loop)%Concentration,GlycolData(Loop)%CondValues)
      ELSE
        CALL ShowSevereError('GetFluidPropertiesData: Conductivity data not entered for a '//TRIM(CurrentModuleObject))
        CALL ShowContinueError('ALL data must be entered for user-defined glycols')
        CALL ShowContinueError('Glycol mixture name = '//TRIM(GlycolData(Loop)%Name))
        CALL ShowContinueError('Glycol fluid name = '//TRIM(GlycolData(Loop)%GlycolName))
        ErrorsFound=.TRUE.
      END IF

          ! Viscosity data:
      IF (GlyRawData(Index)%ViscDataPresent) THEN
        GlycolData(Loop)%ViscDataPresent = .TRUE.
        GlycolData(Loop)%NumViscTempPts  = GlyRawData(Index)%NumViscTempPts
        ALLOCATE(GlycolData(Loop)%ViscTemps(GlycolData(Loop)%NumViscTempPts))
        ALLOCATE(GlycolData(Loop)%ViscValues(GlycolData(Loop)%NumViscTempPts))
        GlycolData(Loop)%ViscTemps = GlyRawData(Index)%ViscTemps
        CALL InterpValuesForGlycolConc(GlyRawData(Index)%NumViscConcPts,GlyRawData(Index)%NumViscTempPts, &
                                       GlyRawData(Index)%ViscConcs,GlyRawData(Index)%ViscValues, &
                                       GlycolData(Loop)%Concentration,GlycolData(Loop)%ViscValues)
      ELSE
        CALL ShowSevereError('GetFluidPropertiesData: Viscosity data not entered for a '//TRIM(CurrentModuleObject))
        CALL ShowContinueError('ALL data must be entered for user-defined glycols')
        CALL ShowContinueError('Glycol mixture name = '//TRIM(GlycolData(Loop)%Name))
        CALL ShowContinueError('Glycol fluid name = '//TRIM(GlycolData(Loop)%GlycolName))
        ErrorsFound=.TRUE.
      END IF

    END IF

  END DO

  NumOfGlycols = NumOfGlyConcs  ! Reset number of glycols to actual number

  IF (.not. ErrorsFound) CALL InitializeGlycolTempLimits(ErrorsFound)   ! Initialize the Temp limits for the glycols

  IF (.not. ErrorsFound) CALL InitializeRefrigerantLimits(ErrorsFound) ! Initialize the limits for the refrigerants

  DEALLOCATE(FluidTemps)

  DEALLOCATE(Alphas)
  DEALLOCATE(cAlphaFieldNames)
  DEALLOCATE(lAlphaFieldBlanks)
  DEALLOCATE(Numbers)
  DEALLOCATE(cNumericFieldNames)
  DEALLOCATE(lNumericFieldBlanks)

  IF (ErrorsFound) THEN
    !CALL ShowFatalError('GetFluidPropertiesData: Previous errors in input cause program termination.') !RS: Secret Search String
    WRITE(DebugFile,*) 'GetFluidPropertiesData: Previous errors in input cause program termination. Or, at least, they should. We are currently still ignoring them.'
  ENDIF

  IF (GetNumSectionsFound(MakeUPPERCase('ReportGlycols')) > 0) DebugReportGlycols=.true.
  IF (GetNumSectionsFound(MakeUPPERCase('ReportRefrigerants')) > 0) DebugReportRefrigerants=.true.
  IF (GetNumSectionsFound(MakeUPPERCase('IncreaseGlycolErrorLimit')) > 0)   &
                    GlycolErrorLimitTest=GlycolErrorLimitTest+10
  IF (GetNumSectionsFound(MakeUPPERCase('IncreaseRefrigerantErrorLimit')) > 0)   &
                    RefrigerantErrorLimitTest=RefrigerantErrorLimitTest+10

  IF (DebugReportGlycols) CALL ReportAndTestGlycols
  IF (DebugReportRefrigerants) CALL ReportAndTestRefrigerants

  RETURN

END SUBROUTINE GetFluidPropertiesData

!*****************************************************************************

SUBROUTINE InterpDefValuesForGlycolConc(NumOfConcs,NumOfTemps,RawConcData,RawPropData,Concentration,InterpData)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   June 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! The purpose of this subroutine is to find the values for the property
          ! data at a particular concentration from default data that is at "generic"
          ! concentrations.  This is then returned to the main get routine and
          ! then used later in the program to find values at various temperatures.
          ! The ultimate purpose of this is to avoid double interpolation during
          ! the simulation.  Since concentration does not change during the simulation,
          ! there is no reason to do a double interpolation every time a property
          ! value is needed.

          ! METHODOLOGY EMPLOYED:
          ! Fairly straight forward--find the two concentrations between which
          ! the actual concentration falls and then interpolate the property
          ! data using standard linear interpolation.  Note that data is stored
          ! in the format: 2dArray(Concentration,Temperature)

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER,              INTENT(IN)  :: NumOfConcs       ! number of concentrations (dimension of raw data)
  INTEGER,              INTENT(IN)  :: NumOfTemps       ! number of temperatures (dimension of raw data)
  REAL, DIMENSION(:),   INTENT(IN)  :: RawConcData      ! concentrations for raw data
  REAL, DIMENSION(:,:), INTENT(IN)  :: RawPropData      ! raw property data (concentration, temperature)
  REAL,                 INTENT(IN)  :: Concentration    ! concentration of actual fluid mix
  REAL, DIMENSION(:),   INTENT(OUT) :: InterpData       ! interpolated output data at proper concentration

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL, PARAMETER :: ConcToler = 0.0001    ! Some reasonable value for comparisons

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER           :: HiIndex      ! index on the high side of the concentration
  REAL         :: InterpFrac   ! intermediate value for interpolations
  INTEGER           :: LoopC        ! loop counter for concentration
  INTEGER           :: LoopT        ! loop counter for temperature

          ! FLOW:
          ! First, find where the actual concentration falls between the concentration data.
          ! Then, interpolate if necessary.
  IF (Concentration < RawConcData(1)) THEN  ! Concentration too low
    CALL ShowWarningError('Glycol concentration out of range for data (too low), concentration = '//  &
                          TRIM(RoundSigDigits(Concentration,3)))
    CALL ShowContinueError('Check your data or the definition of your glycols in the GlycolConcentrations input')
    CALL ShowContinueError('Property data set to data for lowest concentration entered')
    InterpData = RawPropData(1,:)
  ELSE IF (Concentration > RawConcData(NumOfConcs)) THEN    ! Concentration too high
    CALL ShowWarningError('Glycol concentration out of range for data (too high), concentration = '//  &
                          TRIM(RoundSigDigits(Concentration,3)))
    CALL ShowContinueError('Check your data or the definition of your glycols in the GlycolConcentrations input')
    CALL ShowContinueError('Property data set to data for highest concentration entered')
    InterpData = RawPropData(NumOfConcs,:)
  ELSE  ! Concentration somewhere between lowest and highest point--interpolate
    HiIndex = NumOfConcs    ! Default to highest concentration
    DO LoopC = 2, NumOfConcs-1
      IF (Concentration <= RawConcData(LoopC)) THEN
        HiIndex = LoopC
        EXIT ! LoopC DO loop
      END IF
    END DO
    IF ( ABS(RawConcData(HiIndex)-RawConcData(HiIndex-1)) >= ConcToler ) THEN
      InterpFrac = ( RawConcData(HiIndex) - Concentration ) / ( RawConcData(HiIndex) - RawConcData(HiIndex-1) )
      DO LoopT = 1, NumOfTemps
        IF ( (RawPropData(HiIndex,LoopT) < ConcToler) .OR. (RawPropData(HiIndex-1,LoopT) < ConcToler) ) THEN
          ! One of the two values is zero--so we cannot interpolate for this point (assign to zero)
          InterpData(LoopT) = 0.0
        ELSE
          InterpData(LoopT) = RawPropData(HiIndex,LoopT) &
                             -( InterpFrac * (RawPropData(HiIndex,LoopT)-RawPropData(HiIndex-1,LoopT)) )
        END IF
      END DO
    ELSE    ! user has input data for concentrations that are too close or repeated, this must be fixed
      CALL ShowFatalError('InterpDefValuesForGlycolConc: concentration values too close or data repeated, ' &
                          //'check your fluid property input data')
    END IF
  END IF

  RETURN

END SUBROUTINE InterpDefValuesForGlycolConc

!*****************************************************************************

SUBROUTINE InterpValuesForGlycolConc(NumOfConcs,NumOfTemps,RawConcData,RawPropData,Concentration,InterpData)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   June 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! The purpose of this subroutine is to find the values for the property
          ! data at a particular concentration from default data that is at "generic"
          ! concentrations.  This is then returned to the main get routine and
          ! then used later in the program to find values at various temperatures.
          ! The ultimate purpose of this is to avoid double interpolation during
          ! the simulation.  Since concentration does not change during the simulation,
          ! there is no reason to do a double interpolation every time a property
          ! value is needed.

          ! METHODOLOGY EMPLOYED:
          ! Fairly straight forward--find the two concentrations between which
          ! the actual concentration falls and then interpolate the property
          ! data using standard linear interpolation.  Note that data is stored
          ! in the format: 2dArray(Temperature,Concentration).  Temperature
          ! data is not needed here since we are only interpolating to eliminate
          ! the concentration as a variable (it really isn't one during the
          ! simulation).

          ! REFERENCES:
          ! GetFluidPropertiesData--subroutine forces user to input data in
          ! order of increasing concentration.  This is assumed in this subroutine.

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER,              INTENT(IN)  :: NumOfConcs       ! number of concentrations (dimension of raw data)
  INTEGER,              INTENT(IN)  :: NumOfTemps       ! number of temperatures (dimension of raw data)
  REAL, DIMENSION(:),   INTENT(IN)  :: RawConcData      ! concentrations for raw data
  REAL, DIMENSION(:,:), INTENT(IN)  :: RawPropData      ! raw property data (temperature,concentration)
  REAL,                 INTENT(IN)  :: Concentration    ! concentration of actual fluid mix
  REAL, DIMENSION(:),   INTENT(OUT) :: InterpData       ! interpolated output data at proper concentration

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL, PARAMETER :: ConcToler = 0.0001    ! Some reasonable value for comparisons

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER           :: HiIndex      ! index on the high side of the concentration
  REAL         :: InterpFrac   ! intermediate value for interpolations
  INTEGER           :: LoopC        ! loop counter for concentration
  INTEGER           :: LoopT        ! loop counter for temperature

          ! FLOW:
          ! First, find where the actual concentration falls between the concentration data.
          ! Then, interpolate if necessary.
  IF (Concentration < RawConcData(1)) THEN  ! Concentration too low
    CALL ShowWarningError('Glycol concentration out of range for data (too low), concentration = '//  &
                          TRIM(RoundSigDigits(Concentration,3)))
    CALL ShowContinueError('Check your data or the definition of your glycols in the GlycolConcentrations input')
    CALL ShowContinueError('Property data set to data for lowest concentration entered')
    InterpData = RawPropData(:,1)
  ELSE IF (Concentration > RawConcData(NumOfConcs)) THEN    ! Concentration too high
    CALL ShowWarningError('Glycol concentration out of range for data (too high), concentration = '//  &
                          TRIM(RoundSigDigits(Concentration,3)))
    CALL ShowContinueError('Check your data or the definition of your glycols in the GlycolConcentrations input')
    CALL ShowContinueError('Property data set to data for highest concentration entered')
    InterpData = RawPropData(:,NumOfConcs)
  ELSE  ! Concentration somewhere between lowest and highest point--interpolate
    HiIndex = NumOfConcs    ! Default to highest concentration
    DO LoopC = 2, NumOfConcs-1
      IF (Concentration <= RawConcData(LoopC)) THEN
        HiIndex = LoopC
        EXIT ! LoopC DO loop
      END IF
    END DO
    IF ( ABS(RawConcData(HiIndex)-RawConcData(HiIndex-1)) >= ConcToler ) THEN
      InterpFrac = ( RawConcData(HiIndex) - Concentration ) / ( RawConcData(HiIndex) - RawConcData(HiIndex-1) )
      DO LoopT = 1, NumOfTemps
        IF ( (RawPropData(LoopT,HiIndex) < ConcToler) .OR. (RawPropData(LoopT,HiIndex-1) < ConcToler) ) THEN
          InterpData(LoopT) = 0.0
        ELSE
          InterpData(LoopT) = RawPropData(LoopT,HiIndex) &
                             -( InterpFrac * (RawPropData(LoopT,HiIndex)-RawPropData(LoopT,HiIndex-1)) )
        END IF
      END DO
    ELSE    ! user has input data for concentrations that are too close or repeated, this must be fixed
      CALL ShowFatalError('InterpValuesForGlycolConc: concentration values too close or data repeated, check ' &
                          //'your fluid property input data')
    END IF
  END IF

  RETURN

END SUBROUTINE InterpValuesForGlycolConc

!*****************************************************************************

SUBROUTINE InitializeGlycolTempLimits(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine sets up the min/max temperature limits for the glycol properties.
          ! Most properties requested (e.g., Specific Heat) must be > 0 but the tables may
          ! be set up for symmetry and not be limited to just valid values.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound  ! set to true if errors found here

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: GlycolNum
  INTEGER :: IndexNum
  LOGICAL :: Failure

  DO GlycolNum=1,NumOfGlycols
    IF (GlycolData(GlyColNum)%CPDataPresent) THEN
      ! check for lowest non-zero value by referencing temp data
      DO IndexNum = 1, GlycolData(GlycolNum)%NumCpTempPts
        IF (GlycolData(GlycolNum)%CpValues(IndexNum) <= 0.0) CYCLE
        GlycolData(GlycolNum)%CpLowTempIndex = IndexNum
        GlycolData(GlycolNum)%CpLowTempValue = GlycolData(GlycolNum)%CpTemps(IndexNum)
        EXIT
      ENDDO
      ! check for highest non-zero value  by referencing temp data
      DO IndexNum = GlycolData(GlycolNum)%NumCpTempPts, 1, -1
        IF (GlycolData(GlycolNum)%CpValues(IndexNum) <= 0.0) CYCLE
        GlycolData(GlycolNum)%CpHighTempIndex = IndexNum
        GlycolData(GlycolNum)%CpHighTempValue = GlycolData(GlycolNum)%CpTemps(IndexNum)
        EXIT
      ENDDO
    ENDIF
    IF (GlycolData(GlyColNum)%RhoDataPresent) THEN
      ! check for lowest non-zero value by referencing temp data
      DO IndexNum = 1, GlycolData(GlycolNum)%NumRhoTempPts
        IF (GlycolData(GlycolNum)%RhoValues(IndexNum) <= 0.0) CYCLE
        GlycolData(GlycolNum)%RhoLowTempIndex = IndexNum
        GlycolData(GlycolNum)%RhoLowTempValue = GlycolData(GlycolNum)%RhoTemps(IndexNum)
        EXIT
      ENDDO
      ! check for highest non-zero value  by referencing temp data
      DO IndexNum = GlycolData(GlycolNum)%NumRhoTempPts, 1, -1
        IF (GlycolData(GlycolNum)%RhoValues(IndexNum) <= 0.0) CYCLE
        GlycolData(GlycolNum)%RhoHighTempIndex = IndexNum
        GlycolData(GlycolNum)%RhoHighTempValue = GlycolData(GlycolNum)%RhoTemps(IndexNum)
        EXIT
      ENDDO
    ENDIF
    IF (GlycolData(GlyColNum)%CondDataPresent) THEN
      ! check for lowest non-zero value by referencing temp data
      DO IndexNum = 1, GlycolData(GlycolNum)%NumCondTempPts
        IF (GlycolData(GlycolNum)%CondValues(IndexNum) <= 0.0) CYCLE
        GlycolData(GlycolNum)%CondLowTempIndex = IndexNum
        GlycolData(GlycolNum)%CondLowTempValue = GlycolData(GlycolNum)%CondTemps(IndexNum)
        EXIT
      ENDDO
      ! check for highest non-zero value  by referencing temp data
      DO IndexNum = GlycolData(GlycolNum)%NumCondTempPts, 1, -1
        IF (GlycolData(GlycolNum)%CondValues(IndexNum) <= 0.0) CYCLE
        GlycolData(GlycolNum)%CondHighTempIndex = IndexNum
        GlycolData(GlycolNum)%CondHighTempValue = GlycolData(GlycolNum)%CondTemps(IndexNum)
        EXIT
      ENDDO
    ENDIF
    IF (GlycolData(GlyColNum)%ViscDataPresent) THEN
      ! check for lowest non-zero value by referencing temp data
      DO IndexNum = 1, GlycolData(GlycolNum)%NumViscTempPts
        IF (GlycolData(GlycolNum)%ViscValues(IndexNum) <= 0.0) CYCLE
        GlycolData(GlycolNum)%ViscLowTempIndex = IndexNum
        GlycolData(GlycolNum)%ViscLowTempValue = GlycolData(GlycolNum)%ViscTemps(IndexNum)
        EXIT
      ENDDO
      ! check for highest non-zero value  by referencing temp data
      DO IndexNum = GlycolData(GlycolNum)%NumViscTempPts, 1, -1
        IF (GlycolData(GlycolNum)%ViscValues(IndexNum) <= 0.0) CYCLE
        GlycolData(GlycolNum)%ViscHighTempIndex = IndexNum
        GlycolData(GlycolNum)%ViscHighTempValue = GlycolData(GlycolNum)%ViscTemps(IndexNum)
        EXIT
      ENDDO
    ENDIF
    Failure=.false.
    ! Check to see that all are set to non-zero
    IF (GlycolData(GlyColNum)%CpDataPresent) THEN
      IF (GlycolData(GlycolNum)%CpLowTempIndex == 0) Failure=.true.
      IF (GlycolData(GlycolNum)%CpHighTempIndex == 0) Failure=.true.
    ENDIF
    IF (GlycolData(GlyColNum)%RhoDataPresent) THEN
      IF (GlycolData(GlycolNum)%RhoLowTempIndex == 0) Failure=.true.
      IF (GlycolData(GlycolNum)%RhoHighTempIndex == 0) Failure=.true.
    ENDIF
    IF (GlycolData(GlyColNum)%CondDataPresent) THEN
      IF (GlycolData(GlycolNum)%CondLowTempIndex == 0) Failure=.true.
      IF (GlycolData(GlycolNum)%CondHighTempIndex == 0) Failure=.true.
    ENDIF
    IF (GlycolData(GlyColNum)%ViscDataPresent) THEN
      IF (GlycolData(GlycolNum)%ViscLowTempIndex == 0) Failure=.true.
      IF (GlycolData(GlycolNum)%ViscHighTempIndex == 0) Failure=.true.
    ENDIF
    IF (Failure) THEN
      CALL ShowSevereError('InitializeGlycolTempLimits: Required values for Glycol='//TRIM(GlycolData(GlycolNum)%Name)//  &
          ' are all zeroes for some data types.')
      ErrorsFound=.true.
    ENDIF
  ENDDO
  RETURN

END SUBROUTINE InitializeGlycolTempLimits

!*****************************************************************************

SUBROUTINE InitializeRefrigerantLimits(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine sets up the min/max limits (usually temperature and/or pressure)
          ! for the refrigerant properties.
          ! Most properties requested (e.g., Specific Heat) must be > 0 but the tables may
          ! be set up for symmetry and not be limited to just valid values.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound  ! set to true if errors found here

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: RefrigNum
  INTEGER :: IndexNum
  LOGICAL :: Failure

  DO RefrigNum=1,NumOfRefrigerants
    DO IndexNum=1,RefrigData(RefrigNum)%NumPsPoints
      IF (RefrigData(RefrigNum)%PsValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%PsLowPresIndex=IndexNum
      RefrigData(RefrigNum)%PsLowPresValue=RefrigData(RefrigNum)%PsValues(IndexNum)
      RefrigData(RefrigNum)%PsLowTempValue=RefrigData(RefrigNum)%PsTemps(IndexNum)
      RefrigData(RefrigNum)%PsLowTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=RefrigData(RefrigNum)%NumPsPoints,1,-1
      IF (RefrigData(RefrigNum)%PsValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%PsHighPresIndex=IndexNum
      RefrigData(RefrigNum)%PsHighPresValue=RefrigData(RefrigNum)%PsValues(IndexNum)
      RefrigData(RefrigNum)%PsHighTempValue=RefrigData(RefrigNum)%PsTemps(IndexNum)
      RefrigData(RefrigNum)%PsHighTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=1,RefrigData(RefrigNum)%NumHPoints
      IF (RefrigData(RefrigNum)%HfValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%HfLowTempValue=RefrigData(RefrigNum)%HfValues(IndexNum)
      RefrigData(RefrigNum)%HfLowTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=RefrigData(RefrigNum)%NumHPoints,1,-1
      IF (RefrigData(RefrigNum)%HfValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%HfHighTempValue=RefrigData(RefrigNum)%HfValues(IndexNum)
      RefrigData(RefrigNum)%HfHighTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=1,RefrigData(RefrigNum)%NumHPoints
      IF (RefrigData(RefrigNum)%HfgValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%HfgLowTempValue=RefrigData(RefrigNum)%HfgValues(IndexNum)
      RefrigData(RefrigNum)%HfgLowTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=RefrigData(RefrigNum)%NumHPoints,1,-1
      IF (RefrigData(RefrigNum)%HfgValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%HfgHighTempValue=RefrigData(RefrigNum)%HfgValues(IndexNum)
      RefrigData(RefrigNum)%HfgHighTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=1,RefrigData(RefrigNum)%NumCpPoints
      IF (RefrigData(RefrigNum)%CpfValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%CpfLowTempValue=RefrigData(RefrigNum)%CpfValues(IndexNum)
      RefrigData(RefrigNum)%CpfLowTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=RefrigData(RefrigNum)%NumCpPoints,1,-1
      IF (RefrigData(RefrigNum)%CpfValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%CpfHighTempValue=RefrigData(RefrigNum)%CpfValues(IndexNum)
      RefrigData(RefrigNum)%CpfHighTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=1,RefrigData(RefrigNum)%NumCpPoints
      IF (RefrigData(RefrigNum)%CpfgValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%CpfgLowTempValue=RefrigData(RefrigNum)%CpfgValues(IndexNum)
      RefrigData(RefrigNum)%CpfgLowTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=RefrigData(RefrigNum)%NumCpPoints,1,-1
      IF (RefrigData(RefrigNum)%CpfgValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%CpfgHighTempValue=RefrigData(RefrigNum)%CpfgValues(IndexNum)
      RefrigData(RefrigNum)%CpfgHighTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=1,RefrigData(RefrigNum)%NumRhoPoints
      IF (RefrigData(RefrigNum)%RhofValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%RhofLowTempValue=RefrigData(RefrigNum)%RhofValues(IndexNum)
      RefrigData(RefrigNum)%RhofLowTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=RefrigData(RefrigNum)%NumRhoPoints,1,-1
      IF (RefrigData(RefrigNum)%RhofValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%RhofHighTempValue=RefrigData(RefrigNum)%RhofValues(IndexNum)
      RefrigData(RefrigNum)%RhofHighTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=1,RefrigData(RefrigNum)%NumRhoPoints
      IF (RefrigData(RefrigNum)%RhofgValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%RhofgLowTempValue=RefrigData(RefrigNum)%RhofgValues(IndexNum)
      RefrigData(RefrigNum)%RhofgLowTempIndex=IndexNum
      EXIT
    ENDDO
    DO IndexNum=RefrigData(RefrigNum)%NumRhoPoints,1,-1
      IF (RefrigData(RefrigNum)%RhofgValues(IndexNum) <= 0.0) CYCLE
      RefrigData(RefrigNum)%RhofgHighTempValue=RefrigData(RefrigNum)%RhofgValues(IndexNum)
      RefrigData(RefrigNum)%RhofgHighTempIndex=IndexNum
      EXIT
    ENDDO
    Failure=.false.
    ! Check to see that all are set to non-zero
    IF (RefrigData(RefrigNum)%NumPsPoints > 0) THEN
      IF (RefrigData(RefrigNum)%PsLowPresIndex == 0) Failure=.true.
      IF (RefrigData(RefrigNum)%PsLowTempIndex == 0) Failure=.true.
      IF (RefrigData(RefrigNum)%PsHighPresIndex == 0) Failure=.true.
      IF (RefrigData(RefrigNum)%PsHighTempIndex == 0) Failure=.true.
    ENDIF
    IF (RefrigData(RefrigNum)%NumHPoints > 0) THEN
      IF (RefrigData(RefrigNum)%HfLowTempIndex == 0) Failure=.true.
      IF (RefrigData(RefrigNum)%HfgLowTempIndex == 0) Failure=.true.
      IF (RefrigData(RefrigNum)%HfHighTempIndex == 0) Failure=.true.
      IF (RefrigData(RefrigNum)%HfgHighTempIndex == 0) Failure=.true.
    ENDIF
    IF (RefrigData(RefrigNum)%NumCpPoints > 0) THEN
      IF (RefrigData(RefrigNum)%CpfLowTempIndex == 0) Failure=.true.
      IF (RefrigData(RefrigNum)%CpfgLowTempIndex == 0) Failure=.true.
      IF (RefrigData(RefrigNum)%CpfHighTempIndex == 0) Failure=.true.
      IF (RefrigData(RefrigNum)%CpfgHighTempIndex == 0) Failure=.true.
    ENDIF
    IF (RefrigData(RefrigNum)%NumRhoPoints > 0) THEN
      IF (RefrigData(RefrigNum)%RhofLowTempIndex == 0) Failure=.true.
      IF (RefrigData(RefrigNum)%RhofgLowTempIndex == 0) Failure=.true.
      IF (RefrigData(RefrigNum)%RhofHighTempIndex == 0) Failure=.true.
      IF (RefrigData(RefrigNum)%RhofgHighTempIndex == 0) Failure=.true.
    ENDIF
    IF (Failure) THEN
      CALL ShowSevereError('InitializeRefrigerantLimits: Required values for Refrigerant='//  &
           TRIM(RefrigData(RefrigNum)%Name)//  &
          ' are all zeroes for some data types.')
      ErrorsFound=.true.
    ENDIF
  ENDDO

  RETURN

END SUBROUTINE InitializeRefrigerantLimits

!*****************************************************************************

SUBROUTINE ReportAndTestGlycols

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is written to report and test glycols through their range
          ! of temperatures and make sure that proper values will be returned.

          ! METHODOLOGY EMPLOYED:
          ! Use internal structure as the temperature limits. Write output to the
          ! debug output file.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: fmta="(A)"
  REAL, PARAMETER        :: incr=10.0

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: GlycolNum        ! Loop Counter
  REAL :: Temperature    ! Temperature to drive values
  REAL :: ReturnValue    ! Values returned from glycol functions
  INTEGER :: Loop             ! Loop Counter
  INTEGER :: GlycolIndex      ! index used in routine / function calls, value is returned on first use (when index=0)

  GetInput = .FALSE.  ! input has already been gotten

  DO GlycolNum=1,NumOfGlycols
    GlycolIndex=0     ! used in routine calls -- value is returned when first 0
    ! Lay out the basic values:
    IF (GlycolData(GlycolNum)%GlycolName /= ' ') THEN
      write(OutputFileDebug,fmta) 'Glycol='//TRIM(GlycolData(GlycolNum)%Name)//  &
            ', Mixture fluid='//TRIM(GlycolData(GlycolNum)%GlycolName)
    ELSE
      write(OutputFileDebug,fmta) 'Glycol='//TRIM(GlycolData(GlycolNum)%Name)
    ENDIF
    write(OutputFileDebug,fmta) 'Concentration:,'//TRIM(RoundSigDigits(GlycolData(GlycolNum)%Concentration,2))
    IF (GlycolData(GlyColNum)%CPDataPresent) THEN
      write(OutputFileDebug,fmta) 'Specific Heat Data points:,Low Temperature=,'//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%CpLowTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%CpLowTempIndex))//  &
         ',High Temperature=,'//TRIM(RoundSigDigits(GlycolData(GlycolNum)%CpHighTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%CpHighTempIndex))
      write(OutputFileDebug,fmta,advance='No') 'Temperatures:'
      do Loop=1,GlycolData(GlycolNum)%NumCpTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%CpTemps(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%CpTemps(GlycolData(GlycolNum)%NumCpTempPts),2))
      write(OutputFileDebug,fmta,advance='No') 'Specific Heat:'
      do Loop=1,GlycolData(GlycolNum)%NumCpTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%CpValues(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%CpValues(GlycolData(GlycolNum)%NumCpTempPts),2))
    ENDIF
    IF (GlycolData(GlyColNum)%RhoDataPresent) THEN
      write(OutputFileDebug,fmta) 'Density Data points:,Low Temperature=,'//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%RhoLowTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%RhoLowTempIndex))//  &
         ',High Temperature=,'//TRIM(RoundSigDigits(GlycolData(GlycolNum)%RhoHighTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%RhoHighTempIndex))
      write(OutputFileDebug,fmta,advance='No') 'Temperatures:'
      do Loop=1,GlycolData(GlycolNum)%NumRhoTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%RhoTemps(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%RhoTemps(GlycolData(GlycolNum)%NumRhoTempPts),2))
      write(OutputFileDebug,fmta,advance='No') 'Density:'
      do Loop=1,GlycolData(GlycolNum)%NumRhoTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%RhoValues(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%RhoValues(GlycolData(GlycolNum)%NumRhoTempPts),2))
    ENDIF
    IF (GlycolData(GlyColNum)%CondDataPresent) THEN
      write(OutputFileDebug,fmta) 'Conductivity Data points:,Low Temperature=,'//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%CondLowTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%CondLowTempIndex))//  &
         ',High Temperature=,'//TRIM(RoundSigDigits(GlycolData(GlycolNum)%CondHighTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%CondHighTempIndex))
      write(OutputFileDebug,fmta,advance='No') 'Temperatures:'
      do Loop=1,GlycolData(GlycolNum)%NumCondTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%CondTemps(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//  &
                   TRIM(RoundSigDigits(GlycolData(GlycolNum)%CondTemps(GlycolData(GlycolNum)%NumCondTempPts),2))
      write(OutputFileDebug,fmta,advance='No') 'Conductivity:'
      do Loop=1,GlycolData(GlycolNum)%NumCondTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%CondValues(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//  &
                   TRIM(RoundSigDigits(GlycolData(GlycolNum)%CondValues(GlycolData(GlycolNum)%NumCondTempPts),2))
    ENDIF
    IF (GlycolData(GlyColNum)%ViscDataPresent) THEN
      write(OutputFileDebug,fmta) 'Viscosity Data points:,Low Temperature=,'//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%ViscLowTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%ViscLowTempIndex))//  &
         ',High Temperature=,'//TRIM(RoundSigDigits(GlycolData(GlycolNum)%ViscHighTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%ViscHighTempIndex))
      write(OutputFileDebug,fmta,advance='No') 'Temperatures:'
      do Loop=1,GlycolData(GlycolNum)%NumViscTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%ViscTemps(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//  &
                   TRIM(RoundSigDigits(GlycolData(GlycolNum)%ViscTemps(GlycolData(GlycolNum)%NumViscTempPts),2))
      write(OutputFileDebug,fmta,advance='No') 'Viscosity:'
      do Loop=1,GlycolData(GlycolNum)%NumViscTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%ViscValues(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//  &
                   TRIM(RoundSigDigits(GlycolData(GlycolNum)%ViscValues(GlycolData(GlycolNum)%NumViscTempPts),2))
    ENDIF
! ============================================
! Glycol Results, using out of bounds to out of bounds values in calling
! ============================================

! ========= Specific Heat from Temperatures
    write(OutputFileDebug,fmta) 'Glycol='//TRIM(GlycolData(GlycolNum)%Name)//' **** Results ****'
    IF (GlycolData(GlyColNum)%CPDataPresent) THEN
      write(OutputFileDebug,fmta,advance='No') 'Specific Heat Results at Temperatures:'
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%CpTemps(1)-incr,2))
      do Loop=1,GlycolData(GlycolNum)%NumCpTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%CpTemps(Loop),2))
        Temperature=GlycolData(GlycolNum)%CpTemps(Loop) +   &
           (GlycolData(GlycolNum)%CpTemps(Loop+1)-GlycolData(GlycolNum)%CpTemps(Loop))/2.0
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(Temperature,2))
      enddo
      write(OutputFileDebug,fmta,advance='No') ','//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%CpTemps(GlycolData(GlycolNum)%NumCpTempPts),2))
      write(OutputFileDebug,fmta) ','//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%CpTemps(GlycolData(GlycolNum)%NumCpTempPts)+incr,2))
      write(OutputFileDebug,fmta,advance='No') 'Specific Heat:'
      Temperature=GlycolData(GlycolNum)%CpTemps(1)-incr
      ReturnValue=GetSpecificHeatGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      do Loop=1,GlycolData(GlycolNum)%NumCpTempPts-1
        Temperature=GlycolData(GlycolNum)%CpTemps(Loop)
        ReturnValue=GetSpecificHeatGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
        Temperature=GlycolData(GlycolNum)%CpTemps(Loop) +   &
           (GlycolData(GlycolNum)%CpTemps(Loop+1)-GlycolData(GlycolNum)%CpTemps(Loop))/2.0
        ReturnValue=GetSpecificHeatGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      enddo
      Temperature=GlycolData(GlycolNum)%CpTemps(GlycolData(GlycolNum)%NumCpTempPts)
      ReturnValue=GetSpecificHeatGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      Temperature=GlycolData(GlycolNum)%CpTemps(GlycolData(GlycolNum)%NumCpTempPts)+incr
      ReturnValue=GetSpecificHeatGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(ReturnValue,2))
    ENDIF

! ========= Density from Temperatures
    IF (GlycolData(GlyColNum)%RhoDataPresent) THEN
      write(OutputFileDebug,fmta,advance='No') 'Density Results at Temperatures:'
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%RhoTemps(1)-incr,2))
      do Loop=1,GlycolData(GlycolNum)%NumRhoTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%RhoTemps(Loop),2))
        Temperature=GlycolData(GlycolNum)%RhoTemps(Loop) +   &
           (GlycolData(GlycolNum)%RhoTemps(Loop+1)-GlycolData(GlycolNum)%RhoTemps(Loop))/2.0
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(Temperature,2))
      enddo
      write(OutputFileDebug,fmta,advance='No') ','//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%RhoTemps(GlycolData(GlycolNum)%NumRhoTempPts),2))
      write(OutputFileDebug,fmta) ','//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%RhoTemps(GlycolData(GlycolNum)%NumRhoTempPts)+incr,2))
      write(OutputFileDebug,fmta,advance='No') 'Density:'
      Temperature=GlycolData(GlycolNum)%RhoTemps(1)-incr
      ReturnValue=GetDensityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,3))
      do Loop=1,GlycolData(GlycolNum)%NumRhoTempPts-1
        Temperature=GlycolData(GlycolNum)%RhoTemps(Loop)
        ReturnValue=GetDensityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,3))
        Temperature=GlycolData(GlycolNum)%RhoTemps(Loop) +   &
           (GlycolData(GlycolNum)%RhoTemps(Loop+1)-GlycolData(GlycolNum)%RhoTemps(Loop))/2.0
        ReturnValue=GetDensityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,3))
      enddo
      Temperature=GlycolData(GlycolNum)%RhoTemps(GlycolData(GlycolNum)%NumRhoTempPts)
      ReturnValue=GetDensityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,3))
      Temperature=GlycolData(GlycolNum)%RhoTemps(GlycolData(GlycolNum)%NumRhoTempPts)+incr
      ReturnValue=GetDensityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(ReturnValue,3))
    ENDIF

! ========= Conductivity from Temperatures
    IF (GlycolData(GlyColNum)%CondDataPresent) THEN
      write(OutputFileDebug,fmta,advance='No') 'Conductivity Results at Temperatures:'
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%CondTemps(1)-incr,2))
      do Loop=1,GlycolData(GlycolNum)%NumCondTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%CondTemps(Loop),2))
        Temperature=GlycolData(GlycolNum)%CondTemps(Loop) +   &
           (GlycolData(GlycolNum)%CondTemps(Loop+1)-GlycolData(GlycolNum)%CondTemps(Loop))/2.0
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(Temperature,2))
      enddo
      write(OutputFileDebug,fmta,advance='No') ','//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%CondTemps(GlycolData(GlycolNum)%NumCondTempPts),2))
      write(OutputFileDebug,fmta) ','//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%CondTemps(GlycolData(GlycolNum)%NumCondTempPts)+incr,2))
      write(OutputFileDebug,fmta,advance='No') 'Conductivity:'
      Temperature=GlycolData(GlycolNum)%CondTemps(1)-incr
      ReturnValue=GetConductivityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,3))
      do Loop=1,GlycolData(GlycolNum)%NumCondTempPts-1
        Temperature=GlycolData(GlycolNum)%CondTemps(Loop)
        ReturnValue=GetConductivityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,3))
        Temperature=GlycolData(GlycolNum)%CondTemps(Loop) +   &
           (GlycolData(GlycolNum)%CondTemps(Loop+1)-GlycolData(GlycolNum)%CondTemps(Loop))/2.0
        ReturnValue=GetConductivityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,3))
      enddo
      Temperature=GlycolData(GlycolNum)%CondTemps(GlycolData(GlycolNum)%NumCondTempPts)
      ReturnValue=GetConductivityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,3))
      Temperature=GlycolData(GlycolNum)%CondTemps(GlycolData(GlycolNum)%NumCondTempPts)+incr
      ReturnValue=GetConductivityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(ReturnValue,3))
    ENDIF

! ========= Viscosity from Temperatures
    IF (GlycolData(GlyColNum)%ViscDataPresent) THEN
      write(OutputFileDebug,fmta,advance='No') 'Viscosity Results at Temperatures:'
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%ViscTemps(1)-incr,2))
      do Loop=1,GlycolData(GlycolNum)%NumViscTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(GlycolData(GlycolNum)%ViscTemps(Loop),2))
        Temperature=GlycolData(GlycolNum)%ViscTemps(Loop) +   &
           (GlycolData(GlycolNum)%ViscTemps(Loop+1)-GlycolData(GlycolNum)%ViscTemps(Loop))/2.0
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(Temperature,2))
      enddo
      write(OutputFileDebug,fmta,advance='No') ','//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%ViscTemps(GlycolData(GlycolNum)%NumViscTempPts),2))
      write(OutputFileDebug,fmta) ','//  &
         TRIM(RoundSigDigits(GlycolData(GlycolNum)%ViscTemps(GlycolData(GlycolNum)%NumViscTempPts)+incr,2))
      write(OutputFileDebug,fmta,advance='No') 'Viscosity:'
      Temperature=GlycolData(GlycolNum)%ViscTemps(1)-incr
      ReturnValue=GetViscosityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,4))
      do Loop=1,GlycolData(GlycolNum)%NumViscTempPts-1
        Temperature=GlycolData(GlycolNum)%ViscTemps(Loop)
        ReturnValue=GetViscosityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,4))
        Temperature=GlycolData(GlycolNum)%ViscTemps(Loop) +   &
           (GlycolData(GlycolNum)%ViscTemps(Loop+1)-GlycolData(GlycolNum)%ViscTemps(Loop))/2.0
        ReturnValue=GetViscosityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,4))
      enddo
      Temperature=GlycolData(GlycolNum)%ViscTemps(GlycolData(GlycolNum)%NumViscTempPts)
      ReturnValue=GetViscosityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,4))
      Temperature=GlycolData(GlycolNum)%ViscTemps(GlycolData(GlycolNum)%NumViscTempPts)+incr
      ReturnValue=GetViscosityGlycol(GlycolData(GlycolNum)%Name,Temperature,GlycolIndex,'ReportAndTestGlycols')
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(ReturnValue,4))
    ENDIF
  ENDDO

  RETURN

END SUBROUTINE ReportAndTestGlycols

!*****************************************************************************

SUBROUTINE ReportAndTestRefrigerants

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2008; only stub provided to satisfy calling programs.
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is written to report and test refrigerants through their range
          ! of inputs (temperatures?) and make sure that proper values will be returned.

          ! METHODOLOGY EMPLOYED:
          ! Use internal structure as the range limits. Write output to the
          ! debug output file.


          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: fmta="(A)"
  REAL, PARAMETER        :: incr=10.0
  REAL, PARAMETER        :: Quality=1.0

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: RefrigNum        ! Loop Counter
  REAL :: Temperature    ! Temperature to drive values
!  REAL :: Pressure       ! Pressure to drive values
  REAL :: ReturnValue    ! Values returned from refrigerant functions
  INTEGER :: Loop             ! Loop Counter
  INTEGER :: Loop1            ! Loop Counter
  INTEGER :: RefrigIndex      !

  GetInput = .FALSE.  ! input has already been gotten

  DO RefrigNum=1,NumOfRefrigerants
    RefrigIndex=0     ! used in routine calls -- value is returned when first 0
    ! Lay out the basic values:
    IF (RefrigData(RefrigNum)%Name /= ' ') THEN
      write(OutputFileDebug,fmta) 'Refrigerant='//TRIM(RefrigData(RefrigNum)%Name)
    ENDIF
    IF (RefrigData(RefrigNum)%NumPsPoints > 0) THEN
      write(OutputFileDebug,fmta) 'Saturation Pressures Data points:,Low Temperature=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%PsLowTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%PsLowTempIndex))//  &
         ',High Temperature=,'//TRIM(RoundSigDigits(RefrigData(RefrigNum)%PsHighTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%PsHighTempIndex))
      write(OutputFileDebug,fmta,advance='No') 'Temperatures:'
      do Loop=1,RefrigData(RefrigNum)%NumPsPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%PsTemps(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%PsTemps(RefrigData(RefrigNum)%NumPsPoints),2))
      write(OutputFileDebug,fmta,advance='No') 'Saturation Pressure:'
      do Loop=1,RefrigData(RefrigNum)%NumPsPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%PsValues(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%PsValues(RefrigData(RefrigNum)%NumPsPoints),2))
    ENDIF
    IF (RefrigData(RefrigNum)%NumHPoints > 0) THEN
      write(OutputFileDebug,fmta) 'Enthalpy Saturated Fluid Data points:,Low Temperature=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%HfLowTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%HfLowTempIndex))//  &
         ',High Temperature=,'//TRIM(RoundSigDigits(RefrigData(RefrigNum)%HfHighTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%HfHighTempIndex))
      write(OutputFileDebug,fmta,advance='No') 'Temperatures:'
      do Loop=1,RefrigData(RefrigNum)%NumHPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%HTemps(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%HTemps(RefrigData(RefrigNum)%NumHPoints),2))
      write(OutputFileDebug,fmta,advance='No') 'Enthalpy Saturated Fluid:'
      do Loop=1,RefrigData(RefrigNum)%NumHPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%HfValues(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%HfValues(RefrigData(RefrigNum)%NumHPoints),2))
      write(OutputFileDebug,fmta) 'Enthalpy Saturated Fluid/Gas Data points:,Low Temperature=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%HfgLowTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%HfgLowTempIndex))//  &
         ',High Temperature=,'//TRIM(RoundSigDigits(RefrigData(RefrigNum)%HfgHighTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%HfgHighTempIndex))
      write(OutputFileDebug,fmta,advance='No') 'Temperatures:'
      do Loop=1,RefrigData(RefrigNum)%NumHPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%HTemps(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%HTemps(RefrigData(RefrigNum)%NumHPoints),2))
      write(OutputFileDebug,fmta,advance='No') 'Enthalpy Saturated Fluid/Gas:'
      do Loop=1,RefrigData(RefrigNum)%NumHPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%HfgValues(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%HfgValues(RefrigData(RefrigNum)%NumHPoints),2))
    ENDIF
    IF (RefrigData(RefrigNum)%NumCpPoints > 0) THEN
      write(OutputFileDebug,fmta) 'Specific Heat Saturated Fluid Data points:,Low Temperature=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpfLowTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpfLowTempIndex))//  &
         ',High Temperature=,'//TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpfHighTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpfHighTempIndex))
      write(OutputFileDebug,fmta,advance='No') 'Temperatures:'
      do Loop=1,RefrigData(RefrigNum)%NumCpPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpTemps(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpTemps(RefrigData(RefrigNum)%NumCpPoints),2))
      write(OutputFileDebug,fmta,advance='No') 'Specific Heat Saturated Fluid:'
      do Loop=1,RefrigData(RefrigNum)%NumCpPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpfValues(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpfValues(RefrigData(RefrigNum)%NumCpPoints),2))
      write(OutputFileDebug,fmta) 'Specific Heat Saturated Fluid/Gas Data points:,Low Temperature=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpfgLowTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpfgLowTempIndex))//  &
         ',High Temperature=,'//TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpfgHighTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpfgHighTempIndex))
      write(OutputFileDebug,fmta,advance='No') 'Temperatures:'
      do Loop=1,RefrigData(RefrigNum)%NumCpPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpTemps(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpTemps(RefrigData(RefrigNum)%NumCpPoints),2))
      write(OutputFileDebug,fmta,advance='No') 'Specific Heat Saturated Fluid/Gas:'
      do Loop=1,RefrigData(RefrigNum)%NumCpPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpfgValues(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpfgValues(RefrigData(RefrigNum)%NumCpPoints),2))
    ENDIF
    IF (RefrigData(RefrigNum)%NumRhoPoints > 0) THEN
      write(OutputFileDebug,fmta) 'Density Saturated Fluid Data points:,Low Temperature=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhofLowTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhofLowTempIndex))//  &
         ',High Temperature=,'//TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhofHighTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhofHighTempIndex))
      write(OutputFileDebug,fmta,advance='No') 'Temperatures:'
      do Loop=1,RefrigData(RefrigNum)%NumRhoPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhoTemps(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhoTemps(RefrigData(RefrigNum)%NumRhoPoints),2))
      write(OutputFileDebug,fmta,advance='No') 'Density Saturated Fluid:'
      do Loop=1,RefrigData(RefrigNum)%NumRhoPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhofValues(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhofValues(RefrigData(RefrigNum)%NumRhoPoints),2))
      write(OutputFileDebug,fmta) 'Density Saturated Fluid/Gas Data points:,Low Temperature=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhofgLowTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhofgLowTempIndex))//  &
         ',High Temperature=,'//TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhofgHighTempValue,2))//',Index=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhofgHighTempIndex))
      write(OutputFileDebug,fmta,advance='No') 'Temperatures:'
      do Loop=1,RefrigData(RefrigNum)%NumRhoPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhoTemps(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhoTemps(RefrigData(RefrigNum)%NumRhoPoints),2))
      write(OutputFileDebug,fmta,advance='No') 'Density Saturated Fluid/Gas:'
      do Loop=1,RefrigData(RefrigNum)%NumRhoPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhofgValues(Loop),2))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhofgValues(RefrigData(RefrigNum)%NumRhoPoints),2))
    ENDIF

    IF (RefrigData(RefrigNum)%NumSuperTempPts > 0 .and. RefrigData(RefrigNum)%NumSuperPressPts > 0) THEN
      write(OutputFileDebug,fmta) 'Superheated Gas Fluid Data points:,NumTemperaturePoints=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%NumSuperTempPts))//',NumPressurePoints=,'//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%NumSuperPressPts))
      write(OutputFileDebug,fmta,advance='No') 'Superheated Temperatures:'
      do Loop=1,RefrigData(RefrigNum)%NumSuperTempPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%SHTemps(Loop),3))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%SHTemps(RefrigData(RefrigNum)%NumSuperTempPts),3))
      write(OutputFileDebug,fmta,advance='No') 'Superheated Pressures:'
      do Loop=1,RefrigData(RefrigNum)%NumSuperPressPts-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%SHPress(Loop),3))
      enddo
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%SHPress(RefrigData(RefrigNum)%NumSuperPressPts),3))
      do Loop=1,RefrigData(RefrigNum)%NumSuperPressPts
        write(OutputFileDebug,fmta) 'Superheated Pressure:#'//Trim(RoundSigDigits(Loop))//'='//  &
           trim(RoundSigDigits(RefrigData(RefrigNum)%SHPress(Loop),2))
        write(OutputFileDebug,fmta,advance='No') 'Enthalpy Superheated Gas:'
        do Loop1=1,RefrigData(RefrigNum)%NumSuperTempPts-1
          write(OutputFileDebug,fmta,advance='No') ','//  &
             TRIM(RoundSigDigits(RefrigData(RefrigNum)%HshValues(Loop1,Loop),3))
        enddo
        write(OutputFileDebug,fmta) ','//  &
           TRIM(RoundSigDigits(RefrigData(RefrigNum)%HshValues(RefrigData(RefrigNum)%NumSuperTempPts,Loop),3))
      enddo
      do Loop=1,RefrigData(RefrigNum)%NumSuperPressPts
        write(OutputFileDebug,fmta) 'Superheated Pressure:#'//Trim(RoundSigDigits(Loop))//'='//  &
           trim(RoundSigDigits(RefrigData(RefrigNum)%SHPress(Loop),2))
        write(OutputFileDebug,fmta,advance='No') 'Density Superheated Gas:'
        do Loop1=1,RefrigData(RefrigNum)%NumSuperTempPts-1
          write(OutputFileDebug,fmta,advance='No') ','//  &
             TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhoshValues(Loop1,Loop),3))
        enddo
        write(OutputFileDebug,fmta) ','//  &
           TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhoshValues(RefrigData(RefrigNum)%NumSuperTempPts,Loop),3))
      enddo
      do Loop=1,RefrigData(RefrigNum)%NumSuperTempPts
        write(OutputFileDebug,fmta) 'Superheated Temperature:#'//Trim(RoundSigDigits(Loop))//'='//  &
           trim(RoundSigDigits(RefrigData(RefrigNum)%SHTemps(Loop),2))
        write(OutputFileDebug,fmta,advance='No') 'Enthalpy Superheated Gas:'
        do Loop1=1,RefrigData(RefrigNum)%NumSuperPressPts-1
          write(OutputFileDebug,fmta,advance='No') ','//  &
             TRIM(RoundSigDigits(RefrigData(RefrigNum)%HshValues(Loop,Loop1),3))
        enddo
        write(OutputFileDebug,fmta) ','//  &
           TRIM(RoundSigDigits(RefrigData(RefrigNum)%HshValues(Loop,RefrigData(RefrigNum)%NumSuperPressPts),3))
      enddo
      do Loop=1,RefrigData(RefrigNum)%NumSuperTempPts
        write(OutputFileDebug,fmta) 'Superheated Temperature:#'//Trim(RoundSigDigits(Loop))//'='//  &
           trim(RoundSigDigits(RefrigData(RefrigNum)%SHTemps(Loop),2))
        write(OutputFileDebug,fmta,advance='No') 'Density Superheated Gas:'
        do Loop1=1,RefrigData(RefrigNum)%NumSuperPressPts-1
          write(OutputFileDebug,fmta,advance='No') ','//  &
             TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhoshValues(Loop,Loop1),3))
        enddo
        write(OutputFileDebug,fmta) ','//  &
           TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhoshValues(Loop,RefrigData(RefrigNum)%NumSuperPressPts),3))
      enddo
    ENDIF

! ============================================
! Refrigeration Results, using out of bounds to out of bounds values in calling
! ============================================

! ========= Pressure from Temperatures
    write(OutputFileDebug,fmta) 'Refrigerant='//TRIM(RefrigData(RefrigNum)%Name)//' **** Results ****'
    IF (RefrigData(RefrigNum)%NumPsPoints > 0) THEN
      write(OutputFileDebug,fmta,advance='No') 'Pressure Results at Temperatures:'
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%PsTemps(1)-incr,2))
      do Loop=1,RefrigData(RefrigNum)%NumPsPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%PsTemps(Loop),2))
        Temperature=RefrigData(RefrigNum)%PsTemps(Loop) +   &
           (RefrigData(RefrigNum)%PsTemps(Loop+1)-RefrigData(RefrigNum)%PsTemps(Loop))/2.0
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(Temperature,2))
      enddo
      write(OutputFileDebug,fmta,advance='No') ','//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%PsTemps(RefrigData(RefrigNum)%NumPsPoints),2))
      write(OutputFileDebug,fmta) ','//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%PsTemps(RefrigData(RefrigNum)%NumPsPoints)+incr,2))
      write(OutputFileDebug,fmta,advance='No') 'Saturated Pressures:'
      Temperature=RefrigData(RefrigNum)%PsTemps(1)-incr
      ReturnValue=GetSatPressureRefrig(RefrigData(RefrigNum)%Name,Temperature,RefrigIndex,'ReportAndTestRefrigerants')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      do Loop=1,RefrigData(RefrigNum)%NumPsPoints-1
        Temperature=RefrigData(RefrigNum)%PsTemps(Loop)
        ReturnValue=GetSatPressureRefrig(RefrigData(RefrigNum)%Name,Temperature,RefrigIndex,'ReportAndTestRefrigerants')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
        Temperature=RefrigData(RefrigNum)%PsTemps(Loop) +   &
           (RefrigData(RefrigNum)%PsTemps(Loop+1)-RefrigData(RefrigNum)%PsTemps(Loop))/2.0
        ReturnValue=GetSatPressureRefrig(RefrigData(RefrigNum)%Name,Temperature,RefrigIndex,'ReportAndTestRefrigerants')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      enddo
      Temperature=RefrigData(RefrigNum)%PsTemps(RefrigData(RefrigNum)%NumPsPoints)
      ReturnValue=GetSatPressureRefrig(RefrigData(RefrigNum)%Name,Temperature,RefrigIndex,'ReportAndTestRefrigerants')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      Temperature=RefrigData(RefrigNum)%PsTemps(RefrigData(RefrigNum)%NumPsPoints)+incr
      ReturnValue=GetSatPressureRefrig(RefrigData(RefrigNum)%Name,Temperature,RefrigIndex,'ReportAndTestRefrigerants')
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(ReturnValue,2))
    ENDIF

! ========= Enthalpy from Temperatures
    IF (RefrigData(RefrigNum)%NumHPoints > 0) THEN
      write(OutputFileDebug,fmta,advance='No') 'Enthalpy Results at Temperatures:'
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%HTemps(1)-incr,2))
      do Loop=1,RefrigData(RefrigNum)%NumHPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%HTemps(Loop),2))
        Temperature=RefrigData(RefrigNum)%HTemps(Loop) +   &
           (RefrigData(RefrigNum)%HTemps(Loop+1)-RefrigData(RefrigNum)%HTemps(Loop))/2.0
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(Temperature,2))
      enddo
      write(OutputFileDebug,fmta,advance='No') ','//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%HTemps(RefrigData(RefrigNum)%NumHPoints),2))
      write(OutputFileDebug,fmta) ','//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%HTemps(RefrigData(RefrigNum)%NumHPoints)+incr,2))
      write(OutputFileDebug,fmta,advance='No') 'Saturated Enthalpy:'
      Temperature=RefrigData(RefrigNum)%HTemps(1)-incr
      ReturnValue=GetSatEnthalpyRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      do Loop=1,RefrigData(RefrigNum)%NumHPoints-1
        Temperature=RefrigData(RefrigNum)%HTemps(Loop)
        ReturnValue=GetSatEnthalpyRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
        Temperature=RefrigData(RefrigNum)%HTemps(Loop) +   &
           (RefrigData(RefrigNum)%HTemps(Loop+1)-RefrigData(RefrigNum)%HTemps(Loop))/2.0
        ReturnValue=GetSatEnthalpyRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      enddo
      Temperature=RefrigData(RefrigNum)%HTemps(RefrigData(RefrigNum)%NumHPoints)
      ReturnValue=GetSatEnthalpyRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      Temperature=RefrigData(RefrigNum)%HTemps(RefrigData(RefrigNum)%NumHPoints)+incr
      ReturnValue=GetSatEnthalpyRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(ReturnValue,2))
    ENDIF

! ========= Specific Heat from Temperatures
    IF (RefrigData(RefrigNum)%NumCpPoints > 0) THEN
      write(OutputFileDebug,fmta,advance='No') 'Specific Heat Results at Temperatures:'
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpTemps(1)-incr,2))
      do Loop=1,RefrigData(RefrigNum)%NumCpPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpTemps(Loop),2))
        Temperature=RefrigData(RefrigNum)%CpTemps(Loop) +   &
           (RefrigData(RefrigNum)%CpTemps(Loop+1)-RefrigData(RefrigNum)%CpTemps(Loop))/2.0
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(Temperature,2))
      enddo
      write(OutputFileDebug,fmta,advance='No') ','//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpTemps(RefrigData(RefrigNum)%NumCpPoints),2))
      write(OutputFileDebug,fmta) ','//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%CpTemps(RefrigData(RefrigNum)%NumCpPoints)+incr,2))
      write(OutputFileDebug,fmta,advance='No') 'Saturated Specific Heat:'
      Temperature=RefrigData(RefrigNum)%CpTemps(1)-incr
      ReturnValue=GetSatSpecificHeatRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      do Loop=1,RefrigData(RefrigNum)%NumCpPoints-1
        Temperature=RefrigData(RefrigNum)%CpTemps(Loop)
        ReturnValue=GetSatSpecificHeatRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
        Temperature=RefrigData(RefrigNum)%CpTemps(Loop) +   &
           (RefrigData(RefrigNum)%CpTemps(Loop+1)-RefrigData(RefrigNum)%CpTemps(Loop))/2.0
        ReturnValue=GetSatSpecificHeatRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      enddo
      Temperature=RefrigData(RefrigNum)%CpTemps(RefrigData(RefrigNum)%NumCpPoints)
      ReturnValue=GetSatSpecificHeatRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      Temperature=RefrigData(RefrigNum)%CpTemps(RefrigData(RefrigNum)%NumCpPoints)+incr
      ReturnValue=GetSatSpecificHeatRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(ReturnValue,2))
    ENDIF

! ========= Density from Temperatures
    IF (RefrigData(RefrigNum)%NumRhoPoints > 0) THEN
      write(OutputFileDebug,fmta,advance='No') 'Density Results at Temperatures:'
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhoTemps(1)-incr,2))
      do Loop=1,RefrigData(RefrigNum)%NumRhoPoints-1
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhoTemps(Loop),2))
        Temperature=RefrigData(RefrigNum)%RhoTemps(Loop) +   &
           (RefrigData(RefrigNum)%RhoTemps(Loop+1)-RefrigData(RefrigNum)%RhoTemps(Loop))/2.0
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(Temperature,2))
      enddo
      write(OutputFileDebug,fmta,advance='No') ','//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhoTemps(RefrigData(RefrigNum)%NumRhoPoints),2))
      write(OutputFileDebug,fmta) ','//  &
         TRIM(RoundSigDigits(RefrigData(RefrigNum)%RhoTemps(RefrigData(RefrigNum)%NumRhoPoints)+incr,2))
      write(OutputFileDebug,fmta,advance='No') 'Saturated Density:'
      Temperature=RefrigData(RefrigNum)%RhoTemps(1)-incr
      ReturnValue=GetSatDensityRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      do Loop=1,RefrigData(RefrigNum)%NumRhoPoints-1
        Temperature=RefrigData(RefrigNum)%RhoTemps(Loop)
        ReturnValue=GetSatDensityRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
        Temperature=RefrigData(RefrigNum)%RhoTemps(Loop) +   &
           (RefrigData(RefrigNum)%RhoTemps(Loop+1)-RefrigData(RefrigNum)%RhoTemps(Loop))/2.0
        ReturnValue=GetSatDensityRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
        write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      enddo
      Temperature=RefrigData(RefrigNum)%RhoTemps(RefrigData(RefrigNum)%NumRhoPoints)
      ReturnValue=GetSatDensityRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
      write(OutputFileDebug,fmta,advance='No') ','//TRIM(RoundSigDigits(ReturnValue,2))
      Temperature=RefrigData(RefrigNum)%RhoTemps(RefrigData(RefrigNum)%NumRhoPoints)+incr
      ReturnValue=GetSatDensityRefrig(RefrigData(RefrigNum)%Name,Temperature,Quality,RefrigIndex,'ReportAndTestRefrigerants')
      write(OutputFileDebug,fmta) ','//TRIM(RoundSigDigits(ReturnValue,2))
    ENDIF
  ENDDO

  RETURN

END SUBROUTINE ReportAndTestRefrigerants

!*****************************************************************************

FUNCTION GetSatPressureRefrig(Refrigerant,Temperature,RefrigIndex,calledfrom) RESULT(ReturnValue)

        ! SUBROUTINE INFORMATION:
        !       AUTHOR         Simon Rees
        !       DATE WRITTEN   24 May 2002
        !       MODIFIED       na
        !       RE-ENGINEERED  na

        ! PURPOSE OF THIS FUNCTION:
        ! This finds the saturation pressure for given temperature.

        ! METHODOLOGY EMPLOYED:
        ! Calls FindArrayIndex to find indices either side of requested temperature
        ! and linearly interpolates the corresponding saturation pressure values.

        ! REFERENCES:
        ! na

        ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

        ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: Refrigerant ! carries in substance name
  REAL,        INTENT(IN)  :: Temperature ! actual temperature given as input
  INTEGER,       INTENT(INOUT)  :: RefrigIndex ! Index to Refrigerant Properties
  character(len=*), intent(in)  :: calledfrom  ! routine this function was called from (error messages)
  REAL                     :: ReturnValue

        ! INTERFACE BLOCK SPECIFICATIONS:
        ! na

        ! DERIVED TYPE DEFINITIONS:
        ! na

        ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: HiTempIndex                ! index value of next highest Temperature from table
  INTEGER :: LoTempIndex                ! index value of next lowest Temperature from table
  INTEGER :: RefrigNum                  ! index for refrigerant under consideration
  REAL    :: TempInterpRatio            ! ratio to interpolate in temperature domain
 ! error counters and dummy string
  LOGICAL :: ErrorFlag                  ! error flag for current call
  INTEGER,SAVE :: TempRangeErrCount=0   ! cumulative error counter
  INTEGER, SAVE :: TempRangeErrIndex=0

          ! FLOW:
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

  RefrigNum=0
  IF (NumOfRefrigerants == 0) THEN
    CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
       'GetSatPressureRefrig','properties',calledfrom)
  ENDIF

  ErrorFlag = .False.

  IF (RefrigIndex > 0) THEN
    RefrigNum=RefrigIndex
  ELSE
    ! Find which refrigerant (index) is being requested
    RefrigNum = FindRefrigerant(Refrigerant)
    IF (RefrigNum == 0) THEN
      CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
         'GetSatPressureRefrig','properties',calledfrom)
    ENDIF
    RefrigIndex=RefrigNum
  ENDIF

  ! determine array indices for
  LoTempIndex = FindArrayIndex(Temperature, RefrigData(RefrigNum)%PsTemps,  &
                                            RefrigData(RefrigNum)%PsLowTempIndex,RefrigData(RefrigNum)%PsHighTempIndex)
  HiTempIndex = LoTempIndex + 1

  ! check for out of data bounds problems
  IF (LoTempIndex == 0) THEN
    ReturnValue = RefrigData(RefrigNum)%PsValues(RefrigData(RefrigNum)%PsLowTempIndex)
    ErrorFlag = .True.
  ELSE IF(HiTempIndex > RefrigData(RefrigNum)%PsHighTempIndex) THEN
    ReturnValue = RefrigData(RefrigNum)%PsValues(RefrigData(RefrigNum)%PsHighTempIndex)
    ErrorFlag = .True.
  ELSE
   ! find interpolation ratio w.r.t temperature
    TempInterpRatio = (Temperature - RefrigData(RefrigNum)%PsTemps(LoTempIndex)) / &
                         (RefrigData(RefrigNum)%PsTemps(HiTempIndex) - RefrigData(RefrigNum)%PsTemps(LoTempIndex))

    ! apply final linear interpolation
    ReturnValue = RefrigData(RefrigNum)%PsValues(LoTempIndex) + TempInterpRatio * &
                              (RefrigData(RefrigNum)%PsValues(HiTempIndex) - RefrigData(RefrigNum)%PsValues(LoTempIndex))
  ENDIF

  IF (.not. WarmupFlag .and. ErrorFlag) THEN
     TempRangeErrCount = TempRangeErrCount + 1
    ! send warning
    IF (TempRangeErrCount <= RefrigerantErrorLimitTest) THEN
      CALL ShowSevereError('GetSatPressureRefrig: Saturation temperature requested is out of range for supplied data: **')
      CALL ShowContinueError(' Called From:'//trim(calledfrom)//' Refrigerant='//TRIM(RefrigData(RefrigNum)%Name))
      CALL ShowContinueErrorTimeStamp(' ')
      CALL ShowContinueError('..Refrigerant Temperature='//TRIM(RoundSigDigits(Temperature,2))//  &
                          ' Returned saturated pressure value = '//TRIM(RoundSigDigits(ReturnValue,0)))
    ELSE
      CALL ShowRecurringWarningErrorAtEnd('GetSatPressureRefrig: Refrigerant Saturation temperature out of range error',  &
                                             TempRangeErrIndex,ReportMaxOf=Temperature,ReportMinOf=Temperature,  &
                                             ReportMaxUnits='{C}',ReportMinUnits='{C}')
    ENDIF
  END IF

  RETURN

END FUNCTION GetSatPressureRefrig

!*****************************************************************************

FUNCTION GetSatTemperatureRefrig(Refrigerant, Pressure, RefrigIndex,calledfrom) RESULT(ReturnValue)

        ! SUBROUTINE INFORMATION:
        !       AUTHOR         Simon Rees
        !       DATE WRITTEN   24 May 2002
        !       MODIFIED       na
        !       RE-ENGINEERED  na

        ! PURPOSE OF THIS FUNCTION:
        ! This finds the saturation temperature for given pressure.

        ! METHODOLOGY EMPLOYED:
        ! Calls FindArrayIndex to find indices either side of requested pressure
        ! and linearly interpolates the corresponding saturation temperature values.

        ! REFERENCES:
        ! na

        ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

        ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: Refrigerant    ! carries in substance name
  REAL, INTENT(IN)         :: Pressure       ! actual temperature given as input
  INTEGER,       INTENT(INOUT)  :: RefrigIndex ! Index to Refrigerant Properties
  character(len=*), intent(in)  :: calledfrom  ! routine this function was called from (error messages)
  REAL                     :: ReturnValue

        ! INTERFACE BLOCK SPECIFICATIONS:
        ! na

        ! DERIVED TYPE DEFINITIONS:
        ! na

        ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: HiPresIndex        ! index value of next highest Temperature from table
  INTEGER :: LoPresIndex        ! index value of next lowest Temperature from table
  INTEGER :: RefrigNum          ! index for refrigerant under consideration
  REAL    :: PresInterpRatio    ! ratio to interpolate in temperature domain
 ! error counters and dummy string
  LOGICAL :: ErrorFlag                  ! error flag for current call
  INTEGER,SAVE :: PresRangeErrCount=0   ! cumulative error counter
  INTEGER,SAVE :: PresRangeErrIndex=0

          ! FLOW:
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

  RefrigNum=0
  IF (NumOfRefrigerants == 0) THEN
    CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
       'GetSatTemperatureRefrig','properties',calledfrom)
  ENDIF

  ErrorFlag = .False.

  IF (RefrigIndex > 0) THEN
    RefrigNum=RefrigIndex
  ELSE
    ! Find which refrigerant (index) is being requested
    RefrigNum = FindRefrigerant(Refrigerant)
    IF (RefrigNum == 0) THEN
      CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
         'GetSatTemperatureRefrig','properties',calledfrom)
    ENDIF
    RefrigIndex=RefrigNum
  ENDIF

  ! get the array indices
  LoPresIndex = FindArrayIndex(Pressure, RefrigData(RefrigNum)%PsValues,  &
                                RefrigData(RefrigNum)%PsLowPresIndex,RefrigData(RefrigNum)%PsHighPresIndex)
  HiPresIndex = LoPresIndex + 1

  ! check for out of data bounds problems
  IF (LoPresIndex == 0) THEN
    ReturnValue = RefrigData(RefrigNum)%PsTemps(RefrigData(RefrigNum)%PsLowPresIndex)
    ErrorFlag = .True.
  ELSE IF(HiPresIndex > RefrigData(RefrigNum)%PsHighPresIndex) THEN
    ReturnValue = RefrigData(RefrigNum)%PsTemps(RefrigData(RefrigNum)%PsHighPresIndex)
    ErrorFlag = .True.
  ELSE
  ! find interpolation ratio w.r.t temperature
    PresInterpRatio = (Pressure - RefrigData(RefrigNum)%PsValues(LoPresIndex)) / &
                         (RefrigData(RefrigNum)%PsValues(HiPresIndex) - RefrigData(RefrigNum)%PsValues(LoPresIndex))

    ! apply final linear interpolation
    ReturnValue = RefrigData(RefrigNum)%PsTemps(LoPresIndex) + PresInterpRatio * &
                              (RefrigData(RefrigNum)%PsTemps(HiPresIndex) - &
                               RefrigData(RefrigNum)%PsTemps(LoPresIndex))
  ENDIF

  IF(.NOT. WarmupFlag .and. ErrorFlag)THEN
     PresRangeErrCount = PresRangeErrCount + 1
   ! send warning
    IF (PresRangeErrCount <= RefrigerantErrorLimitTest) THEN
      CALL ShowSevereError('GetSatTemperatureRefrig: Saturation pressure requested is out of range for supplied data: **')
      CALL ShowContinueError(' Called From:'//trim(calledfrom)//' Refrigerant='//TRIM(RefrigData(RefrigNum)%Name))
      CALL ShowContinueErrorTimeStamp(' ')
      CALL ShowContinueError('..Refrigerant Pressure='//TRIM(RoundSigDigits(Pressure,2))//  &
                          ' Returned saturated temperature value ='//TRIM(RoundSigDigits(ReturnValue,0)))
    ELSE
      CALL ShowRecurringWarningErrorAtEnd('GetSatTemperatureRefrig: Refrigerant saturation pressure out of range error',  &
                                             PresRangeErrIndex,ReportMinOf=Pressure,ReportMaxOf=Pressure,  &
                                             ReportMinUnits='{Pa}',ReportMaxUnits='{Pa}')
    ENDIF
  END IF
  RETURN

END FUNCTION GetSatTemperatureRefrig

!*****************************************************************************

FUNCTION GetSatEnthalpyRefrig(Refrigerant,Temperature,Quality,RefrigIndex,calledfrom) RESULT(ReturnValue)

        ! SUBROUTINE INFORMATION:
        !       AUTHOR         Mike Turner
        !       DATE WRITTEN   10 December 99
        !       MODIFIED       Rick Strand (April 2000, May 2000)
        !                      Simon Rees (May 2002)
        !       RE-ENGINEERED  na

        ! PURPOSE OF THIS FUNCTION:
        ! This finds enthalpy for given temperature and a quality under the vapor dome.
        ! This fucntion is only called with a valid refrigerant and quality between 0 and 1.

        ! METHODOLOGY EMPLOYED:
        ! Calls GetInterpolatedSatProp to linearly interpolate between the saturated
        ! liquid  and vapour enthalpies according to the given quality.

        ! REFERENCES:
        ! na

        ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

        ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: Refrigerant ! carries in substance name
  REAL,        INTENT(IN)  :: Temperature ! actual temperature given as input
  REAL,        INTENT(IN)  :: Quality     ! actual quality given as input
  INTEGER,       INTENT(INOUT)  :: RefrigIndex ! Index to Refrigerant Properties
  character(len=*), intent(in)  :: calledfrom  ! routine this function was called from (error messages)
  REAL                     :: ReturnValue

        ! INTERFACE BLOCK SPECIFICATIONS:
        ! na

        ! DERIVED TYPE DEFINITIONS:
        ! na

        ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: RefrigNum    ! index for refrigerant under consideration

          ! FLOW:
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

  RefrigNum=0
  IF (NumOfRefrigerants == 0) THEN
    CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
       'GetSatEnthalpyRefrig','properties',calledfrom)
  ENDIF

  IF ((Quality < 0.0) .OR. (Quality > 1.0)) THEN
    CALL ShowSevereError('GetSatEnthalpyRefrig: Refrigerant "'//TRIM(Refrigerant)//  &
         '", invalid quality, called from '//calledfrom)
    CALL ShowContinueError('Saturated refrigerant quality must be between 0 and 1, entered value=['//  &
      trim(RoundSigDigits(Quality,4))//'].')
    CALL ShowFatalError('Program terminates due to preceding condition.')
  ENDIF

  IF (RefrigIndex > 0) THEN
    RefrigNum=RefrigIndex
  ELSE
    ! Find which refrigerant (index) is being requested
    RefrigNum = FindRefrigerant(Refrigerant)
    IF (RefrigNum == 0) THEN
      CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
         'GetSatEnthalpyRefrig','properties',calledfrom)
    ENDIF
    RefrigIndex=RefrigNum
  ENDIF

  ! Apply linear interpolation function
  ReturnValue = GetInterpolatedSatProp(Temperature, RefrigData(RefrigNum)%HTemps, RefrigData(RefrigNum)%HfValues,  &
                                           RefrigData(RefrigNum)%HfgValues, Quality, calledfrom,        &
                                           RefrigData(RefrigNum)%HfLowTempIndex,RefrigData(RefrigNum)%HfHighTempIndex)

  RETURN

END FUNCTION GetSatEnthalpyRefrig

!*****************************************************************************

FUNCTION GetSatDensityRefrig(Refrigerant,Temperature,Quality,RefrigIndex,calledfrom) RESULT(ReturnValue)

        ! SUBROUTINE INFORMATION:
        !       AUTHOR         Mike Turner
        !       DATE WRITTEN   10 December 99
        !       MODIFIED       Rick Strand (April 2000, May 2000)
        !                      Simon Rees (May 2002); Kenneth Tang (Jan 2004)
        !       RE-ENGINEERED  na

        ! PURPOSE OF THIS SUBROUTINE:
        ! This finds density for given temperature and a quality under the vapor dome.
        ! This function is only called with a valid refrigerant and quality between 0 and 1.

        ! METHODOLOGY EMPLOYED:
        ! Calls GetInterpolatedSatProp to linearly interpolate between the saturated
        ! liquid  and vapour densities according to the given quality.

        ! REFERENCES:
        ! na

        ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

        ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: Refrigerant ! carries in substance name
  REAL,        INTENT(IN)  :: Temperature ! actual temperature given as input
  REAL,        INTENT(IN)  :: Quality     ! actual quality given as input
  INTEGER,       INTENT(INOUT)  :: RefrigIndex ! Index to Refrigerant Properties
  character(len=*), intent(in)  :: calledfrom  ! routine this function was called from (error messages)
  REAL                     :: ReturnValue

        ! INTERFACE BLOCK SPECIFICATIONS:
        ! na

        ! DERIVED TYPE DEFINITIONS:
        ! na

        ! FUNCTION LOCAL VARIABLE DECLARATIONS:

  INTEGER :: RefrigNum                ! index for refrigerant under consideration
  INTEGER :: HiTempIndex              ! array index for temp above input temp
  INTEGER :: LoTempIndex              ! array index for temp below input temp
  REAL    :: LoSatProp                ! Sat. prop. at lower temp & given quality
  REAL    :: HiSatProp                ! Sat. prop. at higher temp & given quality
  REAL    :: TempInterpRatio          ! ratio to interpolate in temperature domain
  LOGICAL :: ErrorFlag                ! error flag for current call

  ! error counters and dummy string
  INTEGER,SAVE :: TempRangeErrCount=0   ! cumulative error counter
  INTEGER,SAVE :: TempRangeErrIndex=0   ! cumulative error counter

  ! FLOW:
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

  RefrigNum=0
  IF (NumOfRefrigerants == 0) THEN
    CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
       'GetSatDensityRefrig','properties',calledfrom)
  ENDIF

  IF ((Quality < 0.0) .OR. (Quality > 1.0)) THEN
    CALL ShowSevereError('GetSatDensityRefrig: Refrigerant "'//TRIM(Refrigerant)//  &
         '", invalid quality, called from '//TRIM(calledfrom))
    CALL ShowContinueError('Saturated density quality must be between 0 and 1, entered value=['//  &
      trim(RoundSigDigits(Quality,4))//'].')
    CALL ShowFatalError('Program terminates due to preceding condition.')
  ENDIF

  ! Find which refrigerant (index) is being requested and then determine
  ! where the temperature is within the temperature array
  IF (RefrigIndex > 0) THEN
    RefrigNum=RefrigIndex
  ELSE
    ! Find which refrigerant (index) is being requested
    RefrigNum = FindRefrigerant(Refrigerant)
    IF (RefrigNum == 0) THEN
      CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
         'GetSatDensityRefrig','properties',calledfrom)
    ENDIF
    RefrigIndex=RefrigNum
  ENDIF

  ErrorFlag = .False.

  LoTempIndex = FindArrayIndex(Temperature, RefrigData(RefrigNum)%RhoTemps,  &
                                  RefrigData(RefrigNum)%RhofLowTempIndex,RefrigData(RefrigNum)%RhofHighTempIndex)
  HiTempIndex = LoTempIndex + 1

  !Error check to make sure the temperature is not out of bounds
  IF (LoTempIndex == 0) THEN
    !Give the lowest density value if the temperature is below than the minimum
    !temperature in the refrigerant table
    ReturnValue = 1.0/RefrigData(RefrigNum)%RhofValues(RefrigData(RefrigNum)%RhofLowTempIndex) +                  &
                       Quality*(1.0/RefrigData(RefrigNum)%RhofgValues(RefrigData(RefrigNum)%RhofLowTempIndex) -   &
                       1.0/RefrigData(RefrigNum)%RhofValues(RefrigData(RefrigNum)%RhofLowTempIndex))
    ReturnValue=1.0/ReturnValue
    ErrorFlag = .True.
  ELSE IF(HiTempIndex > RefrigData(RefrigNum)%RhofHighTempIndex) THEN
    !Give the highest density value if the temperature is higher than the maximum
    !temperature in the refrigerant table
    ReturnValue = 1.0/RefrigData(RefrigNum)%RhofValues(RefrigData(RefrigNum)%RhofHighTempIndex) +        &
               Quality*(1.0/RefrigData(RefrigNum)%RhofgValues(RefrigData(RefrigNum)%RhofHighTempIndex) -   &
               1.0/RefrigData(RefrigNum)%RhofValues(RefrigData(RefrigNum)%RhofHighTempIndex))
    ReturnValue=1.0/ReturnValue
    ErrorFlag = .True.
  ELSE    ! Okay

    !Calculate the specific volume for the lower temperature index based on linear
    !interpolation of the quality
    LoSatProp = 1.0/RefrigData(RefrigNum)%RhofValues(LoTempIndex) + &
                  Quality*(1.0/RefrigData(RefrigNum)%RhofgValues(LoTempIndex) -   &
                    1.0/RefrigData(RefrigNum)%RhofValues(LoTempIndex))

    !Calculate the specific volume for the higher temperature index based on linear
    !interpolation of the quality
    HiSatProp = 1.0/RefrigData(RefrigNum)%RhofValues(HiTempIndex) + &
                  Quality*(1.0/RefrigData(RefrigNum)%RhofgValues(HiTempIndex) -   &
                    1.0/RefrigData(RefrigNum)%RhofValues(HiTempIndex))

    !Find interpolation ratio in temperature direction
    TempInterpRatio = (Temperature - RefrigData(RefrigNum)%RhoTemps(LoTempIndex)) / &
                      (RefrigData(RefrigNum)%RhoTemps(HiTempIndex) - RefrigData(RefrigNum)%RhoTemps(LoTempIndex))

    !Apply final linear interpolation to find the specific volume
    ReturnValue = LoSatProp + TempInterpRatio*(HiSatProp - LoSatProp)
    !Convert the specific volume to density
    ReturnValue = 1.0/ReturnValue
  ENDIF

  IF (.not. WarmupFlag .and. ErrorFlag) THEN
    TempRangeErrCount = TempRangeErrCount + 1
   ! send warning
    IF (TempRangeErrCount <= RefrigerantErrorLimitTest) THEN
      CALL ShowSevereError('GetInterpolatedSatProp: Saturation temperature for interpolation is out of range '// &
                           'for supplied data: **')
      CALL ShowContinueError(' Called From:'//trim(calledfrom)//' Refrigerant='//TRIM(RefrigData(RefrigNum)%Name))
      CALL ShowContinueErrorTimeStamp(' ')
      CALL ShowContinueError('..Refrigerant Temperature='//TRIM(RoundSigDigits(Temperature,2))//  &
                          ' Returned saturated property value = '//TRIM(RoundSigDigits(ReturnValue,0)))
    ELSE
      CALL ShowRecurringWarningErrorAtEnd('GetInterpolatedSatProp: Saturation temperature out of range error',     &
                                               TempRangeErrIndex,ReportMinOf=Temperature,ReportMaxOf=Temperature,  &
                                               ReportMinUnits='{C}',ReportMaxUnits='{C}')
    ENDIF
  END IF
  RETURN

END FUNCTION GetSatDensityRefrig

!*****************************************************************************

FUNCTION GetSatSpecificHeatRefrig(Refrigerant,Temperature,Quality,RefrigIndex,calledfrom) RESULT(ReturnValue)

        ! SUBROUTINE INFORMATION:
        !       AUTHOR         Mike Turner
        !       DATE WRITTEN   10 December 99
        !       MODIFIED       Rick Strand (April 2000, May 2000)
        !                      Simon Rees (May 2002)
        !       RE-ENGINEERED  na

        ! PURPOSE OF THIS SUBROUTINE:
        ! This finds specific heat for given temperature and a quality under the vapor dome.
        ! This fucntion is only called with a valid refrigerant and quality between 0 and 1.

        ! METHODOLOGY EMPLOYED:
        ! Calls GetInterpolatedSatProp to linearly interpolate between the saturated
        ! liquid  and vapour specific heats according to the given quality.

        ! REFERENCES:
        ! na

        ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

        ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: Refrigerant ! carries in substance name
  REAL,        INTENT(IN)  :: Temperature ! actual temperature given as input
  REAL,        INTENT(IN)  :: Quality     ! actual quality given as input
  INTEGER,       INTENT(INOUT)  :: RefrigIndex ! Index to Refrigerant Properties
  character(len=*), intent(in)  :: calledfrom  ! routine this function was called from (error messages)
  REAL                     :: ReturnValue

        ! INTERFACE BLOCK SPECIFICATIONS:
        ! na

        ! DERIVED TYPE DEFINITIONS:
        ! na

        ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: RefrigNum    ! index for refrigerant under consideration

          ! FLOW:
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

  RefrigNum=0
  IF (NumOfRefrigerants == 0) THEN
      CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
         'GetSatSpecificHeatRefrig','properties',calledfrom)
  ENDIF

  IF ((Quality < 0.0) .OR. (Quality > 1.0)) THEN
    CALL ShowSevereError('GetSatSpecificHeatRefrig: Refrigerant "'//TRIM(Refrigerant)//  &
         '", invalid quality, called from '//TRIM(calledfrom))
    CALL ShowContinueError('Saturated density quality must be between 0 and 1, entered value=['//  &
      trim(RoundSigDigits(Quality,4))//'].')
    CALL ShowFatalError('Program terminates due to preceding condition.')
  ENDIF

        ! Find which refrigerant (index) is being requested and then determine
        ! where the temperature is within the temperature array
  IF (RefrigIndex > 0) THEN
    RefrigNum=RefrigIndex
  ELSE
    ! Find which refrigerant (index) is being requested
    RefrigNum = FindRefrigerant(Refrigerant)
    IF (RefrigNum == 0) THEN
      CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
         'GetSatSpecificHeatRefrig','properties',calledfrom)
    ENDIF
    RefrigIndex=RefrigNum
  ENDIF

  ! Apply linear interpolation function
  ReturnValue = GetInterpolatedSatProp(Temperature, RefrigData(RefrigNum)%CpTemps, RefrigData(RefrigNum)%CpfValues,     &
                                                 RefrigData(RefrigNum)%CpfgValues, Quality, calledfrom, &
                                                 RefrigData(RefrigNum)%CpfLowTempIndex,RefrigData(RefrigNum)%CpfHighTempIndex)

  RETURN

END FUNCTION GetSatSpecificHeatRefrig

!*****************************************************************************

FUNCTION GetSupHeatEnthalpyRefrig(Refrigerant,Temperature,Pressure,RefrigIndex,calledfrom) RESULT(ReturnValue)

        ! SUBROUTINE INFORMATION:
        !       AUTHOR         Mike Turner
        !       DATE WRITTEN   10 December 99
        !       MODIFIED       Rick Strand (April 2000, May 2000)
        !       MODIFIED       Simon Rees (May 2002)
        !       RE-ENGINEERED  N/A

        ! PURPOSE OF THIS SUBROUTINE:
        ! Performs linear interpolation between pressures and temperatures and
        ! returns enthalpy values.  Works only in superheated region.

        ! METHODOLOGY EMPLOYED:
        ! Double linear interpolation is used with enthalpy values at four
        ! pressure/temperature input points surrounding the given temperature
        ! and pressure argument values.
        !
        ! With enthalpy data it is assumed that zero values in the data are in
        ! the saturated region. Hence, values near the saturation line are
        ! approximated using the saturation value instead of the zero data value.
        ! points completely in the saturation region are given the saturation value
        ! at the given temperature. Points at the upper limits of pressure/temperature
        ! have the pressure/temperature capped. Warnings are given if the point
        ! is not clearly in the bounds of the superheated data.

        ! REFERENCES:
        ! na

        ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

        ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: Refrigerant ! carries in substance name
  REAL,        INTENT(IN) :: Temperature ! actual temperature given as input
  REAL,        INTENT(IN) :: Pressure    ! actual pressure given as input
  INTEGER,      INTENT(INOUT)  :: RefrigIndex ! Index to Refrigerant Properties
  character(len=*), intent(in) :: calledfrom  ! routine this function was called from (error messages)
  REAL                    :: ReturnValue

        ! INTERFACE BLOCK SPECIFICATIONS:
        ! na

        ! DERIVED TYPE DEFINITIONS:
        ! na

        ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL :: PressInterpRatio   ! Interpolation factor w.r.t pressure
  REAL :: TempInterpRatio    ! Interpolation factor w.r.t temperature
  REAL :: EnthalpyHigh              ! Enthalpy value at interpolated pressure and high temperature
  REAL :: EnthalpyLow               ! Enthalpy value at interpolated pressure and low temperature
  REAL :: LoTempLoEnthalpy          ! Enthalpy value at low pressure and low temperature
  REAL :: LoTempHiEnthalpy          ! Enthalpy value at high pressure and low temperature
  REAL :: HiTempLoEnthalpy          ! Enthalpy value at low pressure and high temperature
  REAL :: HiTempHiEnthalpy          ! Enthalpy value at high pressure and high temperature

  INTEGER :: HiTempIndex            ! high temperature index value
  INTEGER :: HiPressIndex           ! high pressure index value
  INTEGER :: LoPressIndex           ! low index value of Pressure from table
  INTEGER :: RefrigNum              ! index for refrigerant under consideration
  INTEGER :: TempIndex              ! low index value of Temperature from table

  ! error counters and dummy string
  INTEGER :: ErrCount               ! error counter for current call
  INTEGER :: CurTempRangeErrCount   ! error counter for current call
  INTEGER :: CurPresRangeErrCount   ! error counter for current call
  INTEGER,SAVE :: TempRangeErrCount=0
  INTEGER,SAVE :: TempRangeErrIndex=0
  INTEGER,SAVE :: PresRangeErrCount=0
  INTEGER,SAVE :: PresRangeErrIndex=0
  INTEGER,SAVE :: SatErrCount=0
  INTEGER,SAVE :: SatErrIndex=0

  ! see if data is there
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

  RefrigNum=0
  IF (NumOfRefrigerants == 0) THEN
    CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
       'GetSupHeatEnthalpyRefrig','properties',calledfrom)
  ENDIF

  ErrCount = 0
  CurTempRangeErrCount = 0
  CurPresRangeErrCount = 0

  ! Find which refrigerant (index) is being requested and then determine
  ! where the temperature and pressure are within the temperature and
  ! pressure arrays, respectively
  IF (RefrigIndex > 0) THEN
    RefrigNum=RefrigIndex
  ELSE
    ! Find which refrigerant (index) is being requested
    RefrigNum = FindRefrigerant(Refrigerant)
    IF (RefrigNum == 0) THEN
      CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
         'GetSupHeatEnthalpyRefrig','properties',calledfrom)
    ENDIF
    RefrigIndex=RefrigNum
  ENDIF

  TempIndex  = FindArrayIndex(Temperature,RefrigData(RefrigNum)%SHTemps,1,RefrigData(RefrigNum)%NumSuperTempPts)
  LoPressIndex = FindArrayIndex(Pressure,RefrigData(RefrigNum)%SHPress,1,RefrigData(RefrigNum)%NumSuperPressPts)

  ! check temperature data range and attempt to cap if necessary
  IF((TempIndex > 0) .AND. (TempIndex < RefrigData(RefrigNum)%NumSuperTempPts) )THEN ! in range
    HiTempIndex   = TempIndex + 1
    TempInterpRatio  = (Temperature - RefrigData(RefrigNum)%SHTemps(TempIndex)) / &
                              (RefrigData(RefrigNum)%SHTemps(HiTempIndex) - RefrigData(RefrigNum)%SHTemps(TempIndex))
  ELSE IF(TempIndex <1)THEN
    CurTempRangeErrCount = CurTempRangeErrCount + 1
    ErrCount = ErrCount + 1
    TempIndex = 1
    HiTempIndex = TempIndex
    TempInterpRatio = 0.0
  ELSE  ! out of range
    CurTempRangeErrCount = CurTempRangeErrCount + 1
    ErrCount = ErrCount + 1
    ! FindArrayIndex will return upper or lower bound so TempIndex gives upper/lower limit
    HiTempIndex = TempIndex
    TempInterpRatio = 0.0
  END IF

  ! check pressure data range and attempt to cap if necessary
  IF((LoPressIndex > 0) .AND. (LoPressIndex < RefrigData(RefrigNum)%NumSuperPressPts) ) THEN ! in range
    HiPressIndex = LoPressIndex + 1
    PressInterpRatio = (Pressure - RefrigData(RefrigNum)%SHPress(LoPressIndex)) / &
                              (RefrigData(RefrigNum)%SHPress(HiPressIndex) - RefrigData(RefrigNum)%SHPress(LoPressIndex))
  ELSE IF(LoPressIndex < 1)THEN
    CurPresRangeErrCount = CurPresRangeErrCount + 1
    ErrCount = ErrCount + 1
    ! FindArrayIndex will return upper or lower bound so TempIndex gives upper/lower limit
    LoPressIndex = 1
    HiPressIndex = LoPressIndex
    PressInterpRatio = 0.0
  ELSE  ! out of range
    CurPresRangeErrCount = CurPresRangeErrCount + 1
    ErrCount = ErrCount + 1
    HiPressIndex = LoPressIndex
    PressInterpRatio = 0.0
  END IF

  ! get interpolation point values
  LoTempLoEnthalpy = RefrigData(RefrigNum)%HshValues(TempIndex,LoPressIndex)
  LoTempHiEnthalpy = RefrigData(RefrigNum)%HshValues(TempIndex,HiPressIndex)
  HiTempLoEnthalpy = RefrigData(RefrigNum)%HshValues(HiTempIndex,LoPressIndex)
  HiTempHiEnthalpy = RefrigData(RefrigNum)%HshValues(HiTempIndex,HiPressIndex)

  ! to give reasonable interpolation near saturation reset any point with zero value
  ! in table to saturation value
  IF(LoTempLoEnthalpy <= 0.0) THEN
    LoTempLoEnthalpy = GetSatEnthalpyRefrig(Refrigerant,Temperature, 1.0, RefrigNum, 'GetSupHeatEnthalpyRefrig')
  END IF
  IF(LoTempHiEnthalpy <= 0.0) THEN
    LoTempHiEnthalpy = GetSatEnthalpyRefrig(Refrigerant,Temperature, 1.0, RefrigNum, 'GetSupHeatEnthalpyRefrig')
  END IF
  IF(HiTempLoEnthalpy <= 0.0) THEN
    HiTempLoEnthalpy = GetSatEnthalpyRefrig(Refrigerant,Temperature, 1.0, RefrigNum, 'GetSupHeatEnthalpyRefrig')
  END IF
  IF(HiTempHiEnthalpy <= 0.0) THEN
    HiTempHiEnthalpy = GetSatEnthalpyRefrig(Refrigerant,Temperature, 1.0, RefrigNum, 'GetSupHeatEnthalpyRefrig')
  END IF

  ! interpolate w.r.t. pressure
  EnthalpyLow = PressInterpRatio*LoTempHiEnthalpy + (1.0-PressInterpRatio)*LoTempLoEnthalpy

  EnthalpyHigh = PressInterpRatio*HiTempHiEnthalpy + (1.0-PressInterpRatio)*HiTempLoEnthalpy

  ! interpolate w.r.t. temperature
  ReturnValue = TempInterpRatio*EnthalpyHigh + (1.0-TempInterpRatio)*EnthalpyLow

  ! Check to see if all data is at zero. In this case we are completely
  ! inside the saturation dome. Best thing we can do is return saturation value
  IF((RefrigData(RefrigNum)%HshValues(TempIndex,LoPressIndex) <= 0.0) .AND. &
     (RefrigData(RefrigNum)%HshValues(TempIndex,HiPressIndex) <= 0.0) .AND. &
     (RefrigData(RefrigNum)%HshValues(HiTempIndex,LoPressIndex) <= 0.0) .AND. &
     (RefrigData(RefrigNum)%HshValues(HiTempIndex,HiPressIndex) <= 0.0) ) THEN
    SatErrCount = SatErrCount +1
    ! set return value
    ReturnValue = GetSatEnthalpyRefrig(Refrigerant,Temperature, 1.0,   &
        RefrigNum,'GetSupHeatEnthalpyRefrig:'//trim(calledfrom))
    ! send warning
    IF (.not. WarmupFlag) THEN
      IF (SatErrCount <= RefrigerantErrorLimitTest) THEN
        CALL ShowSevereError('GetSupHeatEnthalpyRefrig: Refrigerant is saturated at the given conditions: **')
        CALL ShowContinueError('saturated enthalpy at given temperature returned.    **')
        CALL ShowContinueError(' Called From:'//trim(calledfrom)//' Refrigerant='//TRIM(RefrigData(RefrigNum)%Name))
        CALL ShowContinueErrorTimeStamp(' ')
        CALL ShowContinueError('Refrigerant temperature = '//TRIM(RoundSigDigits(Temperature,2)))
        CALL ShowContinueError('Refrigerant pressure = '//TRIM(RoundSigDigits(Pressure,0)))
        CALL ShowContinueError('Returned Enthalpy value = '//TRIM(RoundSigDigits(ReturnValue,3)))
      ELSEIF (SatErrCount>RefrigerantErrorLimitTest) THEN
        CALL ShowRecurringSevereErrorAtEnd('GetSupHeatEnthalpyRefrig: Refrigerant saturated at the given conditions error',  &
                          SatErrIndex, ReportMinOf=Temperature,ReportMaxOf=Temperature,ReportMinUnits='{C}',ReportMaxUnits='{C}')
      ENDIF
    ENDIF
    RETURN
  ENDIF

  IF (.not. WarmupFlag) THEN
      ! some checks...
    IF(ErrCount > 0)THEN
      ! send temp range error if flagged
      TempRangeErrCount=TempRangeErrCount + CurTempRangeErrCount
      IF (TempRangeErrCount > 1 .AND. TempRangeErrCount <= RefrigerantErrorLimitTest) THEN
        CALL ShowWarningError('GetSupHeatEnthalpyRefrig: Temperature is out of range for superheated refrigerant '// &
                              'enthalpy: values capped **')
        CALL ShowContinueError(' Called From:'//trim(calledfrom)//' Refrigerant='//TRIM(RefrigData(RefrigNum)%Name))
        CALL ShowContinueErrorTimeStamp(' ')
      ELSEIF (TempRangeErrCount>1) THEN
        CALL ShowRecurringWarningErrorAtEnd(  &
           'GetSupHeatEnthalpyRefrig: Temperature out of range for superheated refrigerant enthalpy',TempRangeErrIndex,   &
              ReportMaxOf=Temperature,ReportMinOf=Temperature,ReportMaxUnits='{C}',ReportMinUnits='{C}')
      ENDIF

      ! send pressure range error if flagged
      PresRangeErrCount=PresRangeErrCount + CurPresRangeErrCount
      IF (PresRangeErrCount > 1 .AND. PresRangeErrCount <= RefrigerantErrorLimitTest) THEN
        CALL ShowWarningError('GetSupHeatEnthalpyRefrig: Pressure is out of range for superheated refrigerant enthalpy: '// &
                              'values capped **')
      ELSEIF (PresRangeErrCount>1) THEN
        CALL ShowRecurringWarningErrorAtEnd(  &
           'GetSupHeatEnthalpyRefrig: Pressure out of range for superheated refrigerant enthalpy',PresRangeErrIndex, &
              ReportMaxOf=Pressure,ReportMinOf=Pressure,ReportMaxUnits='{Pa}',ReportMinUnits='{Pa}')
      ENDIF
    END IF ! end error checking
  ENDIF


  RETURN

END FUNCTION GetSupHeatEnthalpyRefrig

!*****************************************************************************

FUNCTION GetSupHeatPressureRefrig(Refrigerant,Temperature,Enthalpy,RefrigIndex,calledfrom) RESULT(ReturnValue)

        ! SUBROUTINE INFORMATION:
        !       AUTHOR         Rick Strand
        !       DATE WRITTEN   May 2000
        !       MODIFIED       Simon Rees (May 2002)
        !       RE-ENGINEERED  na

        ! PURPOSE OF THIS SUBROUTINE:
        ! Performs linear interpolation between enthalpy and temperatures and
        ! returns pressure values.  Works only in superheated region.

        ! METHODOLOGY EMPLOYED:
        ! Double linear interpolation is used with pressure values at four
        ! enthalpy/temperature input points surrounding the given temperature
        ! and enthalpy argument values.
        !
        ! All enthalpies have to be calculated at the given temperature before a
        ! search is made for the data adjacent to the given enthalpy. Linear interpolation
        ! using the enthalpy data is used to interpolate the correspondng pressures.
        ! Temperatures and enthalpies outside the bounds of the available data are capped
        ! and warnings given. For enthlpys lower than the saturated vapour value at the
        ! given temperature result in the saturation pressure being returned (calls to
        ! GetSatEnthalpy and GetSatPressure are made.)

        ! REFERENCES:
        ! na

        ! USE STATEMENTS:
        ! na

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

        ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: Refrigerant ! carries in substance name
  REAL,        INTENT(IN) :: Temperature ! actual temperature given as input
  REAL,        INTENT(IN) :: Enthalpy    ! actual enthalpy given as input
  INTEGER,      INTENT(INOUT)  :: RefrigIndex ! Index to Refrigerant Properties
  character(len=*), intent(in) :: calledfrom  ! routine this function was called from (error messages)
  REAL                    :: ReturnValue

        ! FUNCTION PARAMETERS:
  REAL, PARAMETER :: EnthalpyDiff = 0.01    ! Allows a 1% difference in the enthalpy input and
                                            ! the enthalpy calculated from the pressure found

        ! INTERFACE BLOCK SPECIFICATIONS:
        ! na

        ! DERIVED TYPE DEFINITIONS:
        ! na

        ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL    :: EnthalpyCheck      ! recalculates enthalpy based on calculated pressure
  REAL    :: EnthalpyHigh       ! Enthalpy value at interpolated pressure and high temperature
  REAL    :: EnthalpyLow        ! Enthalpy value at interpolated pressure and low temperature
  REAL    :: EnthalpyMax        ! Enthalpy value at interpolated pressure and high temperature
  REAL    :: EnthalpyMin        ! Enthalpy value at interpolated pressure and low temperature
  REAL    :: SatEnthalpy        ! Saturated vapour enthalpy
  REAL    :: TempInterpRatio    ! Interpolation ratio w.r.t temperature
  REAL    :: EnthInterpRatio    ! Interpolation ratio w.r.t enthalpy

  INTEGER :: finish             ! index of high end of enthalpy values
  INTEGER :: start              ! index of high end of enthalpy values
  INTEGER :: Loop               ! DO loop counter
  INTEGER :: middle             ! mid-point for interval halving

  INTEGER :: RefrigNum          ! index for refrigerant under consideration
  INTEGER :: LoTempStart        ! lower non-zero index of enthalpy values at lower temp.
  INTEGER :: LoTempFinish       ! upper non-zero index of enthalpy values at lower temp.
  INTEGER :: HiTempStart        ! lower non-zero index of enthalpy values at higher temp.
  INTEGER :: HiTempFinish       ! upper non-zero index of enthalpy values at higher temp.
  INTEGER :: TempStart          ! corrected lower non-zero index of enthalpy values
  INTEGER :: TempFinish         ! corrected upper non-zero index of enthalpy values


  INTEGER :: LoTempIndex        ! Index value of lower temperature from data
  INTEGER :: HiTempIndex        ! Index value of higher temperature from data
  INTEGER :: LoEnthalpyIndex    ! Index value of lower enthalpy from data
  INTEGER :: HiEnthalpyIndex    ! Index value of higher enthalpy from data

  ! error counters and dummy string
  INTEGER,SAVE :: TempRangeErrCount=0
  INTEGER,SAVE :: EnthalpyRangeErrCount=0
  INTEGER,SAVE :: SatErrCount=0
  INTEGER,SAVE :: TempRangeErrIndex=0
  INTEGER,SAVE :: EnthalpyRangeErrIndex=0
  INTEGER,SAVE :: SatErrIndex=0
  INTEGER :: ErrCount                  ! error counter for current call
  INTEGER :: CurTempRangeErrCount      ! error counter for current call
  INTEGER :: CurEnthalpyRangeErrCount  ! error counter for current call
  INTEGER :: CurSatErrCount            ! error counter for current call
          ! FLOW:
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

  RefrigNum=0
  IF (NumOfRefrigerants == 0) THEN
    CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
       'GetSupHeatPressureRefrig','properties',calledfrom)
  ENDIF

  ErrCount = 0
  CurTempRangeErrCount = 0
  CurEnthalpyRangeErrCount = 0
  CurSatErrCount = 0

  ! Find which refrigerant (index) is being requested and then determine
  ! where the temperature is within the temperature array
  IF (RefrigIndex > 0) THEN
    RefrigNum=RefrigIndex
  ELSE
    ! Find which refrigerant (index) is being requested
    RefrigNum = FindRefrigerant(Refrigerant)
    IF (RefrigNum == 0) THEN
      CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
         'GetSupHeatPressureRefrig','properties',calledfrom)
    ENDIF
    RefrigIndex=RefrigNum
  ENDIF

  LoTempIndex = FindArrayIndex(Temperature,RefrigData(RefrigNum)%SHTemps,1,RefrigData(RefrigNum)%NumSuperTempPts)
  HiTempIndex = LoTempIndex + 1

  ! check temperature data range and attempt to cap if necessary
  IF((LoTempIndex > 0) .AND. (LoTempIndex < RefrigData(RefrigNum)%NumSuperTempPts) )THEN ! in range
    HiTempIndex  = LoTempIndex + 1
  ELSE IF (LoTempIndex<1)THEN ! below lower bound
    CurTempRangeErrCount = CurTempRangeErrCount + 1
    LoTempIndex = 1
    HiTempIndex = LoTempIndex
  ELSE  ! out of range
    CurTempRangeErrCount = CurTempRangeErrCount + 1
    HiTempIndex = LoTempIndex
  END IF

  ! check for lowest non-zero value in lower temp data
  LoTempStart=RefrigData(RefrigNum)%NumSuperPressPts
  DO Loop = 1, RefrigData(RefrigNum)%NumSuperPressPts
    IF (RefrigData(RefrigNum)%HshValues(LoTempIndex,Loop) > 0.0) THEN
      LoTempStart = Loop
      EXIT
    END IF
  END DO
  ! check for highest non-zero value in lower temp data
  LoTempFinish=1
  DO Loop = RefrigData(RefrigNum)%NumSuperPressPts, 1, -1
    IF (RefrigData(RefrigNum)%HshValues(LoTempIndex,Loop) <= 0.0) THEN
      LoTempFinish = Loop
      !EXIT
    END IF
  END DO
  ! check for lowest non-zero value in high temp data
  HiTempStart=RefrigData(RefrigNum)%NumSuperPressPts
  DO Loop = 1, RefrigData(RefrigNum)%NumSuperPressPts
    IF (RefrigData(RefrigNum)%HshValues(HiTempIndex,Loop) > 0.0) THEN
      HiTempStart = Loop
      EXIT
    END IF
  END DO

  ! check for highest non-zero value in high temp data
  HiTempFinish=1
  DO Loop = RefrigData(RefrigNum)%NumSuperPressPts, 1, -1
    IF (RefrigData(RefrigNum)%HshValues(HiTempIndex,Loop) <= 0.0) THEN
      HiTempFinish = Loop
    END IF
  END DO

  ! find bounds of both hi and lo temp data
  TempStart = MAX(LoTempStart, HiTempStart)
  TempFinish = MIN(LoTempFinish, HiTempFinish)
  ! calculate interpolation ratio w.r.t temperature
  ! This ratio is used to find enthalpies at the given temperature
  TempInterpRatio = (Temperature - RefrigData(RefrigNum)%SHTemps(LoTempIndex))/ &
                    (RefrigData(RefrigNum)%SHTemps(HiTempIndex) - &
                     RefrigData(RefrigNum)%SHTemps(LoTempIndex) )

  ! search for array index by bisection
  start = TempStart     ! set the bounds
  finish = TempFinish

  ! find the bounds of the enthalpy data available
  EnthalpyMax = MAX(RefrigData(RefrigNum)%HshValues(LoTempIndex,TempStart), &
                    RefrigData(RefrigNum)%HshValues(HiTempIndex,TempStart))
  EnthalpyMin = MIN(RefrigData(RefrigNum)%HshValues(LoTempIndex,TempFinish), &
                    RefrigData(RefrigNum)%HshValues(HiTempIndex,TempFinish))
  ! get saturated enthalpy for checking
  SatEnthalpy = GetSatEnthalpyRefrig(Refrigerant, Temperature, 1.0, &
           RefrigNum,'GetSupHeatPressureRefrig:'//trim(calledfrom))

  ! make some checks on the data before interpolating
  IF(Enthalpy < SatEnthalpy)THEN
    ! flag error
    CurSatErrCount = CurSatErrCount + 1
    ErrCount = ErrCount + 1
    ! return sat pressure at this temperature
    ReturnValue = GetSatPressureRefrig(Refrigerant, Temperature,   &
         RefrigNum,'GetSupHeatPressureRefrig:'//trim(calledfrom))

  ELSE IF (EnthalpyMax < Enthalpy .OR. EnthalpyMin > Enthalpy) THEN
    ! out of range error
    CurEnthalpyRangeErrCount = CurEnthalpyRangeErrCount +1
    ErrCount = ErrCount + 1
    IF(Enthalpy > EnthalpyMax)THEN
      ! return min pressure
      ReturnValue = RefrigData(RefrigNum)%SHPress(HiTempStart)
    ELSE
      ! return max pressure
      ReturnValue = RefrigData(RefrigNum)%SHPress(LoTempFinish)
    END IF
  ELSE
    ! go ahead and search
    DO WHILE ((finish - start) > 1)
      middle = (finish + start) / 2

      ! calc enthalpy at middle index for given temperature
      EnthalpyCheck = RefrigData(RefrigNum)%HshValues(LoTempIndex,middle) + &
                      TempInterpRatio * (RefrigData(RefrigNum)%HshValues(HiTempIndex,middle) - &
                      RefrigData(RefrigNum)%HshValues(LoTempIndex,middle) )

      IF (Enthalpy < EnthalpyCheck) THEN
        start = middle
      ELSE
        finish = middle
      END IF
    END DO
    LoEnthalpyIndex  = start
    HiEnthalpyIndex = start + 1

    ! calculate enthalpies adjacent specified enthalpy at given temperature
    EnthalpyLow = RefrigData(RefrigNum)%HshValues(LoTempIndex,LoEnthalpyIndex) + &
                  TempInterpRatio * (RefrigData(RefrigNum)%HshValues(HiTempIndex,LoEnthalpyIndex) - &
                  RefrigData(RefrigNum)%HshValues(LoTempIndex,LoEnthalpyIndex) )

    EnthalpyHigh =  RefrigData(RefrigNum)%HshValues(LoTempIndex,HiEnthalpyIndex) + &
                    TempInterpRatio * (RefrigData(RefrigNum)%HshValues(HiTempIndex,HiEnthalpyIndex) - &
                    RefrigData(RefrigNum)%HshValues(LoTempIndex,HiEnthalpyIndex) )
    ! calculate an interpolation ratio
    EnthInterpRatio = (Enthalpy - EnthalpyLow) / (EnthalpyHigh - EnthalpyLow)
    ! apply this interpolation ratio to find the final pressure
    ReturnValue = RefrigData(RefrigNum)%SHPress(LoEnthalpyIndex) + &
                               EnthInterpRatio * (RefrigData(RefrigNum)%SHPress(HiEnthalpyIndex) - &
                               RefrigData(RefrigNum)%SHPress(LoEnthalpyIndex))
  END IF

  IF (.not. WarmupFlag) THEN
    ! ** make error checks **
    IF(ErrCount > 0) THEN
      ! send near saturation warning if flagged
      SatErrCount=SatErrCount+CurSatErrCount
      IF (SatErrCount > 1 .AND. SatErrCount <= RefrigerantErrorLimitTest) THEN
        CALL ShowWarningError('GetSupHeatPressureRefrig: Refrigerant is saturated at given enthalpy and temperature: '// &
                               'saturation pressure returned **')
        CALL ShowContinueError(' Called From:'//trim(calledfrom)//' Refrigerant='//TRIM(RefrigData(RefrigNum)%Name))
        CALL ShowContinueErrorTimeStamp(' ')
      ELSEIF (SatErrCount > 1) THEN
        CALL ShowRecurringWarningErrorAtEnd(  &
             'GetSupHeatPressureRefrig: Refrigerant near saturation error',SatErrIndex)
      ENDIF

      ! send temp range error if flagged
      TempRangeErrCount=TempRangeErrCount+CurTempRangeErrCount
      IF (TempRangeErrCount > 1 .AND. TempRangeErrCount <= RefrigerantErrorLimitTest) THEN
        CALL ShowWarningError('GetSupHeatPressureRefrig: Temperature is out of range for superheated refrigerant '// &
                               'pressure: values capped **')
        CALL ShowContinueError(' Called From:'//trim(calledfrom)//' Refrigerant='//TRIM(RefrigData(RefrigNum)%Name))
        CALL ShowContinueErrorTimeStamp(' ')
      ELSEIF (TempRangeErrCount > 1) THEN
        CALL ShowRecurringWarningErrorAtEnd(  &
                'GetSupHeatPressureRefrig: Temperature out of range for superheated refrigerant pressure',TempRangeErrIndex)
      ENDIF
      ! send enthalpy range error if flagged
      EnthalpyRangeErrCount=EnthalpyRangeErrCount+CurEnthalpyRangeErrCount
      IF (EnthalpyRangeErrCount > 1 .AND. EnthalpyRangeErrCount <= RefrigerantErrorLimitTest) THEN
        CALL ShowWarningError('GetSupHeatPressureRefrig: Enthlalpy is out of range for superheated refrigerant pressure: '// &
                               'values capped **')
        CALL ShowContinueError(' Called From:'//trim(calledfrom)//' Refrigerant='//TRIM(RefrigData(RefrigNum)%Name))
        CALL ShowContinueErrorTimeStamp(' ')
      ELSEIF (EnthalpyRangeErrCount > 1) THEN
        CALL ShowRecurringWarningErrorAtEnd(  &
                'GetSupHeatPressureRefrig: Enthlalpy out of range for superheated refrigerant pressure',EnthalpyRangeErrIndex)
      ENDIF
    END IF ! end error checking
  ENDIF

  RETURN

END FUNCTION GetSupHeatPressureRefrig

!*****************************************************************************

FUNCTION GetSupHeatDensityRefrig(Refrigerant,Temperature,Pressure,RefrigIndex,calledfrom) RESULT(ReturnValue)

        ! SUBROUTINE INFORMATION:
        !       AUTHOR         Mike Turner
        !       DATE WRITTEN   10 December 99
        !       MODIFIED       Rick Strand (April 2000, May 2000)
        !       MODIFIED       Simon Rees (May 2002)
        !       RE-ENGINEERED  N/A

        ! PURPOSE OF THIS SUBROUTINE:
        ! Performs linear interpolation between pressures and temperatures and
        ! returns Density values.  Works only in superheated region.

        ! METHODOLOGY EMPLOYED:
        ! Double linear interpolation is used with Density values at four
        ! pressure/temperature input points surrounding the given temperature
        ! and pressure arguments.
        !
        ! With Density data it is assumed that zero values in the data are in
        ! the saturated region. Hence, values near the saturation line are
        ! approximated using the saturation value instead of the zero data value.
        ! points completely in the saturation region are given the saturation value
        ! at the given temperature. Points at the upper limits of pressure/temperature
        ! have the pressure/temperature capped. Warnings are given if the point
        ! is not clearly in the bounds of the superheated data.

        ! REFERENCES:
        ! na

        ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

        ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: Refrigerant ! carries in substance name
  REAL,        INTENT(IN) :: Temperature ! actual temperature given as input
  REAL,        INTENT(IN) :: Pressure    ! actual pressure given as input
  INTEGER,      INTENT(INOUT)  :: RefrigIndex ! Index to Refrigerant Properties
  character(len=*), intent(in) :: calledfrom  ! routine this function was called from (error messages)
  REAL                    :: ReturnValue

        ! INTERFACE BLOCK SPECIFICATIONS:
        ! na

        ! DERIVED TYPE DEFINITIONS:
        ! na

        ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL    :: TempInterpRatio     ! Interpolation ratio w.r.t temperature
  REAL    :: PressInterpRatio    ! Interpolation ratio w.r.t pressures
  REAL    :: DensityHigh         ! Density value at interpolated pressure and high temperature
  REAL    :: DensityLow          ! Density value at interpolated pressure and low temperature
  REAL    :: LoTempLoDensity     ! Density value at low pressure and low temperature
  REAL    :: LoTempHiDensity     ! Density value at high pressure and low temperature
  REAL    :: HiTempLoDensity     ! Density value at low pressure and high temperature
  REAL    :: HiTempHiDensity     ! Density value at high pressure and high temperature

  INTEGER :: HiTempIndex         ! high temperature index value
  INTEGER :: HiPressIndex        ! high pressure index value
  INTEGER :: LoPressIndex        ! low index value of Pressure from table
  INTEGER :: RefrigNum           ! index for refrigerant under consideration
  INTEGER :: TempIndex           ! low index value of Temperature from table
  ! error counters and dummy string
  INTEGER,SAVE :: TempRangeErrCount=0
  INTEGER,SAVE :: PresRangeErrCount=0
  INTEGER,SAVE :: SatErrCount=0
  INTEGER,SAVE :: TempRangeErrIndex=0
  INTEGER,SAVE :: PresRangeErrIndex=0
  INTEGER,SAVE :: SatErrIndex=0
  INTEGER :: ErrCount               ! error counter for current call
  INTEGER :: CurTempRangeErrCount   ! error counter for current call
  INTEGER :: CurPresRangeErrCount   ! error counter for current call

  ! see if data is there
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

  RefrigNum=0
  IF (NumOfRefrigerants == 0) THEN
    CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
       'GetSupHeatDensityRefrig','properties',calledfrom)
  ENDIF

  ErrCount = 0  ! initialize for this call
  CurTempRangeErrCount = 0
  CurPresRangeErrCount = 0

  ! Find which refrigerant (index) is being requested and then determine
  ! where the temperature and pressure are within the temperature and
  ! pressure arrays, respectively
  IF (RefrigIndex > 0) THEN
    RefrigNum=RefrigIndex
  ELSE
    ! Find which refrigerant (index) is being requested
    RefrigNum = FindRefrigerant(Refrigerant)
    IF (RefrigNum == 0) THEN
      CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,  &
         'GetSupHeatDensityRefrig','properties',calledfrom)
    ENDIF
    RefrigIndex=RefrigNum
  ENDIF

  TempIndex  = FindArrayIndex(Temperature,RefrigData(RefrigNum)%SHTemps,1,RefrigData(RefrigNum)%NumSuperTempPts)
  LoPressIndex = FindArrayIndex(Pressure,RefrigData(RefrigNum)%SHPress,1,RefrigData(RefrigNum)%NumSuperPressPts)

  ! check temperature data range and attempt to cap if necessary
  IF((TempIndex > 0) .AND. (TempIndex < RefrigData(RefrigNum)%NumSuperTempPts) )THEN ! in range
    HiTempIndex   = TempIndex + 1
    TempInterpRatio  = (Temperature - RefrigData(RefrigNum)%SHTemps(TempIndex)) &
                 /(RefrigData(RefrigNum)%SHTemps(HiTempIndex) &
                 - RefrigData(RefrigNum)%SHTemps(TempIndex))
  ELSE IF(TempIndex <1)THEN
    CurTempRangeErrCount = CurTempRangeErrCount + 1
    ErrCount = ErrCount + 1
    ! FindArrayIndex will return upper or lower bound so TempIndex gives upper/lower limit
    TempIndex = 1
    HiTempIndex = TempIndex
    TempInterpRatio = 0.0
  ELSE  ! out of range
    CurTempRangeErrCount = CurTempRangeErrCount + 1
    ErrCount = ErrCount + 1
    ! FindArrayIndex will return upper or lower bound so TempIndex gives upper/lower limit
    HiTempIndex = TempIndex
    TempInterpRatio = 0.0
  END IF

  ! check pressure data range and attempt to cap if necessary
  IF((LoPressIndex > 0) .AND. (LoPressIndex < RefrigData(RefrigNum)%NumSuperPressPts) ) THEN ! in range
    HiPressIndex = LoPressIndex + 1
    PressInterpRatio = (Pressure - RefrigData(RefrigNum)%SHPress(LoPressIndex)) &
                 /(RefrigData(RefrigNum)%SHPress(HiPressIndex) &
                 - RefrigData(RefrigNum)%SHPress(LoPressIndex))
  ELSE IF(LoPressIndex<1)THEN
    CurPresRangeErrCount = CurPresRangeErrCount + 1
    ErrCount = ErrCount + 1
    LoPressIndex=1
    HiPressIndex = LoPressIndex
    PressInterpRatio = 0.0
  ELSE  ! out of range
    CurPresRangeErrCount = CurPresRangeErrCount + 1
    ErrCount = ErrCount + 1
    ! FindArrayIndex will return upper or lower bound so TempIndex gives upper/lower limit
    HiPressIndex = LoPressIndex
    PressInterpRatio = 0.0
  END IF

  ! get interpolation point values
  LoTempLoDensity = RefrigData(RefrigNum)%RhoshValues(TempIndex,LoPressIndex)
  LoTempHiDensity = RefrigData(RefrigNum)%RhoshValues(TempIndex,HiPressIndex)
  HiTempLoDensity = RefrigData(RefrigNum)%RhoshValues(HiTempIndex,LoPressIndex)
  HiTempHiDensity = RefrigData(RefrigNum)%RhoshValues(HiTempIndex,HiPressIndex)

  ! to give reasonable interpolation near saturation reset any point with zero value
  ! in table to saturation value
  IF(LoTempLoDensity <= 0.0) THEN
    LoTempLoDensity = GetSatDensityRefrig(Refrigerant,Temperature, 1.0, RefrigNum, 'GetSupHeatDensityRefrig')
  END IF
  IF(LoTempHiDensity <= 0.0) THEN
    LoTempHiDensity = GetSatDensityRefrig(Refrigerant,Temperature, 1.0, RefrigNum, 'GetSupHeatDensityRefrig')
  END IF
  IF(HiTempLoDensity <= 0.0) THEN
    HiTempLoDensity = GetSatDensityRefrig(Refrigerant,Temperature, 1.0, RefrigNum, 'GetSupHeatDensityRefrig')
  END IF
  IF(HiTempHiDensity <= 0.0) THEN
    HiTempHiDensity = GetSatDensityRefrig(Refrigerant,Temperature, 1.0, RefrigNum, 'GetSupHeatDensityRefrig')
  END IF

  ! interpolate w.r.t. pressure
  DensityLow = PressInterpRatio*LoTempHiDensity + (1.0-PressInterpRatio)*LoTempLoDensity

  DensityHigh = PressInterpRatio*HiTempHiDensity + (1.0-PressInterpRatio)*HiTempLoDensity

  ! interpolate w.r.t. temperature
  ReturnValue = TempInterpRatio*DensityHigh + (1.0-TempInterpRatio)*DensityLow

  ! some checks...
  ! Check to see if all data is at zero. In this case we are completely
  ! inside the saturation dome. Best thing we can do is return saturation value
  IF((RefrigData(RefrigNum)%RhoshValues(TempIndex,LoPressIndex) <= 0.0) .AND. &
     (RefrigData(RefrigNum)%RhoshValues(TempIndex,HiPressIndex) <= 0.0) .AND. &
     (RefrigData(RefrigNum)%RhoshValues(HiTempIndex,LoPressIndex) <= 0.0) .AND. &
     (RefrigData(RefrigNum)%RhoshValues(HiTempIndex,HiPressIndex) <= 0.0) ) THEN
    SatErrCount = SatErrCount +1
    ! set return value
    ReturnValue = GetSatDensityRefrig(Refrigerant,Temperature, 1.0, RefrigNum, ' GetSupHeatDensityRefrig')
    ! send warning
    IF (SatErrCount <= RefrigerantErrorLimitTest) THEN
      CALL ShowSevereError('Refrigerant is saturated at the given conditions: **')
      CALL ShowContinueError('saturated density at given temperature returned.    **')
      CALL ShowContinueError(' Called From:'//trim(calledfrom)//' Refrigerant='//TRIM(RefrigData(RefrigNum)%Name))
      CALL ShowContinueErrorTimeStamp(' ')
      CALL ShowContinueError('Refrigerant temperature = '//TRIM(RoundSigDigits(Temperature,2)))
      CALL ShowContinueError('Refrigerant pressure = '//TRIM(RoundSigDigits(Pressure,0)))
      CALL ShowContinueError('Returned Density value = '//TRIM(RoundSigDigits(ReturnValue,3)))
    ELSEIF (SatErrCount>1) THEN
      CALL ShowRecurringSevereErrorAtEnd('Refrigerant saturated at the given conditions error',SatErrIndex)
    ENDIF
    RETURN
  ENDIF

  IF (.not. WarmupFlag) THEN
      ! some checks...
    IF(ErrCount > 0)THEN
      ! send temp range error if flagged
      TempRangeErrCount=TempRangeErrCount+CurTempRangeErrCount
      IF (TempRangeErrCount > 1 .AND. TempRangeErrCount <= RefrigerantErrorLimitTest) THEN
        CALL ShowWarningError('GetSupHeatDensityRefrig: Temperature is out of range for superheated refrigerant '// &
                               'density: values capped **')
        CALL ShowContinueError(' Called From:'//trim(calledfrom)//' Refrigerant='//TRIM(RefrigData(RefrigNum)%Name))
        CALL ShowContinueErrorTimeStamp(' ')
      ELSEIF (TempRangeErrCount > 1) THEN
        CALL ShowRecurringWarningErrorAtEnd(  &
            'GetSupHeatDensityRefrig: Temperature out of range for superheated refrigerant density',TempRangeErrIndex,  &
               ReportMinOf=Temperature,ReportMaxOf=Temperature,ReportMinUnits='{C}',ReportMaxUnits='{C}')
      ENDIF
      ! send pressure range error if flagged
      PresRangeErrCount=PresRangeErrCount+CurPresRangeErrCount
      IF (PresRangeErrCount > 1 .AND. PresRangeErrCount <= RefrigerantErrorLimitTest) THEN
        CALL ShowWarningError('GetSupHeatDensityRefrig: Pressure is out of range for superheated refrigerant density: '// &
                               'values capped **')
        CALL ShowContinueError(' Called From:'//trim(calledfrom)//' Refrigerant='//TRIM(RefrigData(RefrigNum)%Name))
        CALL ShowContinueErrorTimeStamp(' ')
      ELSEIF (PresRangeErrCount > 1) THEN
        CALL ShowRecurringWarningErrorAtEnd(  &
            'GetSupHeatDensityRefrig: Pressure out of range for superheated refrigerant density',PresRangeErrIndex,  &
               ReportMinOf=Pressure,ReportMaxOf=Pressure,ReportMinUnits='{Pa}',ReportMaxUnits='{Pa}')
      ENDIF
    END IF ! end error checking
  ENDIF

  RETURN

END FUNCTION GetSupHeatDensityRefrig

!*****************************************************************************

FUNCTION GetSpecificHeatGlycol(Glycol,Temperature,GlycolIndex,calledfrom) RESULT(ReturnValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   June 2004
          !       MODIFIED       N/A
          !       RE-ENGINEERED  N/A

          ! PURPOSE OF THIS FUNCTION:
          ! This subroutine finds specific heats for glycols at different
          ! temperatures.

          ! METHODOLOGY EMPLOYED:
          ! Linear interpolation is used to find specific heat values for a
          ! particular glycol (water or some mixture of water and another fluid).
          ! Warnings are given if the point is not clearly in the bounds of the
          ! glycol data.  The value returned is the appropriate limit value.

          ! REFERENCES:
          ! GetFluidPropertiesData: subroutine enforces that temperatures in
          ! all temperature lists are entered in ascending order.

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)    :: Glycol         ! carries in substance name
  REAL,        INTENT(IN)    :: Temperature    ! actual temperature given as input
  INTEGER,          INTENT(INOUT) :: GlycolIndex    ! Index to Glycol Properties
  character(len=*), intent(in)    :: calledfrom  ! routine this function was called from (error messages)
  REAL                       :: ReturnValue    ! Value for function

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Loop   ! DO loop counter
  INTEGER,SAVE :: HighTempLimitErr = 0
  INTEGER,SAVE :: LowTempLimitErr = 0
  INTEGER,SAVE :: HighTempLimitIndex = 0
  INTEGER,SAVE :: LowTempLimitIndex = 0
  INTEGER :: GlycolNum
  LOGICAL :: LowErrorThisTime
  LOGICAL :: HighErrorThisTime

          ! FLOW:
  LowErrorThisTime = .FALSE.
  HighErrorThisTime = .FALSE.

          ! Get the input if we haven't already
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

          ! If no glycols, no fluid properties can be evaluated
  GlycolNum=0
  IF (NumOfGlycols == 0) &
    CALL ReportFatalGlycolErrors(NumOfGlycols,GlycolNum,.true.,Glycol,'GetSpecificHeatGlycol','specific heat',calledfrom)

          ! If glycol index has not yet been found for this fluid, find its value now
  IF (GlycolIndex > 0) THEN
    GlycolNum=GlycolIndex
  ELSE    ! Find which glycol (index) is being requested
    GlycolNum = FindGlycol(Glycol)
    IF (GlycolNum == 0) THEN
      CALL ReportFatalGlycolErrors(NumOfGlycols,GlycolNum,.true.,Glycol,'GetSpecificHeatGlycol','specific heat',calledfrom)
    ENDIF
    GlycolIndex=GlycolNum
  ENDIF

          ! If user didn't input data (shouldn't get this far, but just in case...), we can't find a value
  IF (.NOT. GlycolData(GlycolIndex)%CpDataPresent)THEN
    CALL ReportFatalGlycolErrors(NumOfGlycols,GlycolNum,GlycolData(GlycolIndex)%CpDataPresent,Glycol,  &
       'GetSpecificHeatGlycol','specific heat',calledfrom)
  ENDIF

          ! Now determine the value of specific heat using interpolation
  IF (Temperature < GlycolData(GlycolIndex)%CpLowTempValue) THEN ! Temperature too low
    LowErrorThisTime         = .TRUE.
    ReturnValue = GlycolData(GlycolIndex)%CpValues(GlycolData(GlycolIndex)%CpLowTempIndex)
  ELSE IF (Temperature > GlycolData(GlycolIndex)%CpHighTempValue) THEN ! Temperature too high
    HighErrorThisTime         = .TRUE.
    ReturnValue = GlycolData(GlycolIndex)%CpValues(GlycolData(GlycolIndex)%CpHighTempIndex)
  ELSE    ! Temperature somewhere between the lowest and highest value
    ! make sure there is a return value
    ReturnValue = GlycolData(GlycolIndex)%CpValues(GlycolData(GlycolIndex)%CpLowTempIndex)
    ! bracket is temp > low, <= high (for interpolation
    DO Loop = GlycolData(GlycolIndex)%CpLowTempIndex+1, GlycolData(GlycolIndex)%CpHighTempIndex
      IF (Temperature > GlycolData(GlycolIndex)%CpTemps(Loop)) CYCLE
      ReturnValue = GetInterpValue(Temperature,                                   &
                                   GlycolData(GlycolIndex)%CpTemps(Loop-1),  &
                                   GlycolData(GlycolIndex)%CpTemps(Loop),    &
                                   GlycolData(GlycolIndex)%CpValues(Loop-1), &
                                   GlycolData(GlycolIndex)%CpValues(Loop))
      EXIT ! DO loop
    END DO
  END IF

          ! Error handling
  IF (.not. WarmupFlag) THEN

    IF (LowErrorThisTime)  LowTempLimitErr = LowTempLimitErr + 1
    IF (HighErrorThisTime) HighTempLimitErr = HighTempLimitErr + 1

    IF ( (LowErrorThisTime) .AND. (LowTempLimitErr <= GlycolErrorLimitTest) ) THEN
       CALL ShowWarningError('GetSpecificHeatGlycol: Temperature is out of range (too low) for fluid specific heat **')
       CALL ShowContinueError('..Called From:'//trim(calledfrom)//' Fluid name ='//TRIM(GlycolData(GlycolIndex)%Name)//  &
                                         ',Temperature='//TRIM(RoundSigDigits(Temperature,2)))
       CALL ShowContinueErrorTimeStamp(' ')
    ELSE IF ( LowErrorThisTime ) THEN
      CALL ShowRecurringWarningErrorAtEnd('GetSpecificHeatGlycol: Temperature out of range (too low) for fluid specific heat', &
            LowTempLimitIndex,ReportMinOf=Temperature,ReportMaxOf=Temperature,  &
            ReportMaxUnits='{C}',ReportMinUnits='{C}')
    END IF

    IF ( (HighErrorThisTime) .AND. (HighTempLimitErr <= GlycolErrorLimitTest) ) THEN
       CALL ShowWarningError('GetSpecificHeatGlycol: Temperature is out of range (too high) for fluid specific heat **')
       CALL ShowContinueError('..Called From:'//trim(calledfrom)//' Fluid name ='//TRIM(GlycolData(GlycolIndex)%Name)//  &
                                         ',Temperature='//TRIM(RoundSigDigits(Temperature,2)))
       CALL ShowContinueErrorTimeStamp(' ')
    ELSE IF ( HighErrorThisTime ) THEN
      CALL ShowRecurringWarningErrorAtEnd('GetSpecificHeatGlycol: Temperature out of range (too high) for fluid specific heat', &
            HighTempLimitIndex,ReportMinOf=Temperature,ReportMaxOf=Temperature,   &
            ReportMaxUnits='{C}',ReportMinUnits='{C}')
    END IF
  ENDIF


  RETURN

END FUNCTION GetSpecificHeatGlycol

!*****************************************************************************

FUNCTION GetDensityGlycol(Glycol,Temperature,GlycolIndex,calledfrom) RESULT(ReturnValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   June 2004
          !       MODIFIED       N/A
          !       RE-ENGINEERED  N/A

          ! PURPOSE OF THIS FUNCTION:
          ! This subroutine finds the density for glycols at different
          ! temperatures.

          ! METHODOLOGY EMPLOYED:
          ! Linear interpolation is used to find density values for a
          ! particular glycol (water or some mixture of water and another fluid).
          ! Warnings are given if the point is not clearly in the bounds of the
          ! glycol data.  The value returned is the appropriate limit value.

          ! REFERENCES:
          ! GetFluidPropertiesData: subroutine enforces that temperatures in
          ! all temperature lists are entered in ascending order.

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)    :: Glycol         ! carries in substance name
  REAL,        INTENT(IN)    :: Temperature    ! actual temperature given as input
  INTEGER,          INTENT(INOUT) :: GlycolIndex    ! Index to Glycol Properties
  character(len=*), intent(in)    :: calledfrom  ! routine this function was called from (error messages)
  REAL                       :: ReturnValue

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Loop   ! DO loop counter
  INTEGER,SAVE :: HighTempLimitErr = 0
  INTEGER,SAVE :: HighTempLimitIndex = 0
  INTEGER,SAVE :: LowTempLimitErr = 0
  INTEGER,SAVE :: LowTempLimitIndex = 0
  INTEGER :: GlycolNum
  LOGICAL :: LowErrorThisTime
  LOGICAL :: HighErrorThisTime

          ! FLOW:
  LowErrorThisTime = .FALSE.
  HighErrorThisTime = .FALSE.

          ! Get the input if we haven't already
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

          ! If no glycols, no fluid properties can be evaluated
  GlycolNum=0
  IF (NumOfGlycols == 0) &
      CALL ReportFatalGlycolErrors(NumOfGlycols,GlycolNum,.true.,Glycol,'GetDensityGlycol','density',calledfrom)

          ! If glycol index has not yet been found for this fluid, find its value now
  IF (GlycolIndex > 0) THEN
    GlycolNum=GlycolIndex
  ELSE    ! Find which refrigerant (index) is being requested
    GlycolNum = FindGlycol(Glycol)
    IF (GlycolNum == 0) THEN
      CALL ReportFatalGlycolErrors(NumOfGlycols,GlycolNum,.true.,Glycol,'GetDensityGlycol','density',calledfrom)
    ENDIF
    GlycolIndex=GlycolNum
  ENDIF

          ! If user didn't input data (shouldn't get this far, but just in case...), we can't find a value
  IF (.NOT. GlycolData(GlycolIndex)%RhoDataPresent)THEN
    CALL ReportFatalGlycolErrors(NumOfGlycols,GlycolNum,GlycolData(GlycolIndex)%RhoDataPresent,Glycol,  &
       'GetDensityGlycol','density',calledfrom)
  ENDIF

          ! Now determine the value of specific heat using interpolation
  IF (Temperature < GlycolData(GlycolIndex)%RhoLowTempValue) THEN ! Temperature too low
    LowErrorThisTime         = .TRUE.
    ReturnValue = GlycolData(GlycolIndex)%RhoValues(GlycolData(GlycolIndex)%RhoLowTempIndex)
  ELSE IF (Temperature > GlycolData(GlycolIndex)%RhoHighTempValue) THEN ! Temperature too high
    HighErrorThisTime         = .TRUE.
    ReturnValue = GlycolData(GlycolIndex)%RhoValues(GlycolData(GlycolIndex)%RhoHighTempIndex)
  ELSE    ! Temperature somewhere between the lowest and highest value
    ReturnValue = GlycolData(GlycolIndex)%RhoValues(GlycolData(GlycolIndex)%RhoLowTempIndex)
    ! bracket is temp > low, <= high (for interpolation
    DO Loop = GlycolData(GlycolIndex)%RhoLowTempIndex+1, GlycolData(GlycolIndex)%RhoHighTempIndex
      IF (Temperature > GlycolData(GlycolIndex)%RhoTemps(Loop)) CYCLE
      ReturnValue = GetInterpValue(Temperature,                                   &
                                   GlycolData(GlycolIndex)%RhoTemps(Loop-1),  &
                                   GlycolData(GlycolIndex)%RhoTemps(Loop),    &
                                   GlycolData(GlycolIndex)%RhoValues(Loop-1), &
                                   GlycolData(GlycolIndex)%RhoValues(Loop))
      EXIT ! DO loop
    END DO
  END IF

          ! Error handling
  IF (.not. WarmupFlag) THEN

    IF (LowErrorThisTime)  LowTempLimitErr = LowTempLimitErr + 1
    IF (HighErrorThisTime) HighTempLimitErr = HighTempLimitErr + 1

    IF ( (LowErrorThisTime) .AND. (LowTempLimitErr <= GlycolErrorLimitTest) ) THEN
       CALL ShowWarningError('GetDensityGlycol: Temperature is out of range (too low) for fluid density **')
       CALL ShowContinueError('..Called From:'//trim(calledfrom)//' Glycol='//TRIM(GlycolData(GlycolIndex)%Name)//  &
                                         ',Temperature='//TRIM(RoundSigDigits(Temperature,2)))
       CALL ShowContinueErrorTimeStamp(' ')
    ELSE IF ( LowErrorThisTime ) THEN
      CALL ShowRecurringWarningErrorAtEnd('GetDensityGlycol: Temperature is out of range (too low) for fluid density',  &
         LowTempLimitIndex,ReportMinOf=Temperature,ReportMaxOf=Temperature,  &
            ReportMaxUnits='{C}',ReportMinUnits='{C}')
    END IF

    IF ( (HighErrorThisTime) .AND. (HighTempLimitErr <= GlycolErrorLimitTest) ) THEN
       CALL ShowWarningError('GetDensityGlycol: Temperature is out of range (too high) for fluid density **')
       CALL ShowContinueError('..Called From:'//trim(calledfrom)//' Fluid name ='//TRIM(GlycolData(GlycolIndex)%Name)//  &
                                         ',Temperature='//TRIM(RoundSigDigits(Temperature,2)))
       CALL ShowContinueErrorTimeStamp(' ')
    ELSE IF ( HighErrorThisTime ) THEN
      CALL ShowRecurringWarningErrorAtEnd('GetDensityGlycol: Temperature out of range (too high) for fluid density',   &
            HighTempLimitIndex,ReportMinOf=Temperature,ReportMaxOf=Temperature,   &
            ReportMaxUnits='{C}',ReportMinUnits='{C}')
    END IF
  ENDIF

  RETURN

END FUNCTION GetDensityGlycol

!*****************************************************************************

FUNCTION GetConductivityGlycol(Glycol,Temperature,GlycolIndex,calledfrom) RESULT(ReturnValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   June 2004
          !       MODIFIED       N/A
          !       RE-ENGINEERED  N/A

          ! PURPOSE OF THIS FUNCTION:
          ! This subroutine finds the conductivity for glycols at different
          ! temperatures.

          ! METHODOLOGY EMPLOYED:
          ! Linear interpolation is used to find conductivity values for a
          ! particular glycol (water or some mixture of water and another fluid).
          ! Warnings are given if the point is not clearly in the bounds of the
          ! glycol data.  The value returned is the appropriate limit value.

          ! REFERENCES:
          ! GetFluidPropertiesData: subroutine enforces that temperatures in
          ! all temperature lists are entered in ascending order.

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)    :: Glycol         ! carries in substance name
  REAL,        INTENT(IN)    :: Temperature    ! actual temperature given as input
  INTEGER,          INTENT(INOUT) :: GlycolIndex    ! Index to Glycol Properties
  character(len=*), intent(in)    :: calledfrom  ! routine this function was called from (error messages)
  REAL                       :: ReturnValue

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Loop   ! DO loop counter
  INTEGER,SAVE :: HighTempLimitErr = 0
  INTEGER,SAVE :: LowTempLimitErr = 0
  INTEGER,SAVE :: HighTempLimitIndex = 0
  INTEGER,SAVE :: LowTempLimitIndex = 0
  INTEGER :: GlycolNum
  LOGICAL :: LowErrorThisTime
  LOGICAL :: HighErrorThisTime

          ! FLOW:
  LowErrorThisTime = .FALSE.
  HighErrorThisTime = .FALSE.

          ! Get the input if we haven't already
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

          ! If no glycols, no fluid properties can be evaluated
  GlycolNum=0
  IF (NumOfGlycols == 0) &
    CALL ReportFatalGlycolErrors(NumOfGlycols,GlycolNum,.true.,Glycol,'GetConductivityGlycol','conductivity',calledfrom)

          ! If glycol index has not yet been found for this fluid, find its value now
  IF (GlycolIndex > 0) THEN
    GlycolNum=GlycolIndex
  ELSE    ! Find which refrigerant (index) is being requested
    GlycolNum = FindGlycol(Glycol)
    IF (GlycolNum == 0) THEN
      CALL ReportFatalGlycolErrors(NumOfGlycols,GlycolNum,.true.,Glycol,'GetConductivityGlycol','conductivity',calledfrom)
    ENDIF
    GlycolIndex=GlycolNum
  ENDIF

          ! If user didn't input data (shouldn't get this far, but just in case...), we can't find a value
  IF (.NOT. GlycolData(GlycolIndex)%CondDataPresent)THEN
    CALL ReportFatalGlycolErrors(NumOfGlycols,GlycolNum,GlycolData(GlycolIndex)%CondDataPresent,Glycol,  &
       'GetConductivityGlycol','conductivity',calledfrom)
  ENDIF

          ! Now determine the value of specific heat using interpolation
  IF (Temperature < GlycolData(GlycolIndex)%CondLowTempValue) THEN ! Temperature too low
    LowErrorThisTime         = .TRUE.
    ReturnValue = GlycolData(GlycolIndex)%CondValues(GlycolData(GlycolIndex)%CondLowTempIndex)
  ELSE IF (Temperature > GlycolData(GlycolIndex)%CondHighTempValue) THEN ! Temperature too high
    HighErrorThisTime         = .TRUE.
    ReturnValue = GlycolData(GlycolIndex)%CondValues(GlycolData(GlycolIndex)%CondHighTempIndex)
  ELSE    ! Temperature somewhere between the lowest and highest value
    ReturnValue = GlycolData(GlycolIndex)%CondValues(GlycolData(GlycolIndex)%CondLowTempIndex)
    ! bracket is temp > low, <= high (for interpolation
    DO Loop = GlycolData(GlycolIndex)%CondLowTempIndex+1, GlycolData(GlycolIndex)%CondHighTempIndex
      IF (Temperature > GlycolData(GlycolIndex)%CondTemps(Loop)) CYCLE
      ReturnValue = GetInterpValue(Temperature,                                   &
                                   GlycolData(GlycolIndex)%CondTemps(Loop-1),  &
                                   GlycolData(GlycolIndex)%CondTemps(Loop),    &
                                   GlycolData(GlycolIndex)%CondValues(Loop-1), &
                                   GlycolData(GlycolIndex)%CondValues(Loop))
      EXIT ! DO loop
    END DO
  END IF

          ! Error handling
  IF (.not. WarmupFlag) THEN

    IF (LowErrorThisTime)  LowTempLimitErr = LowTempLimitErr + 1
    IF (HighErrorThisTime) HighTempLimitErr = HighTempLimitErr + 1

    IF ( (LowErrorThisTime) .AND. (LowTempLimitErr <= GlycolErrorLimitTest) ) THEN
       CALL ShowWarningError('GetConductivityGlycol: Temperature is out of range (too low) for glycol conductivity **')
       CALL ShowContinueError('..Called From:'//trim(calledfrom)//' Glycol='//TRIM(GlycolData(GlycolIndex)%Name)//  &
                                         ',Temperature='//TRIM(RoundSigDigits(Temperature,2)))
       CALL ShowContinueErrorTimeStamp(' ')
    ELSE IF ( LowErrorThisTime ) THEN
      CALL ShowRecurringWarningErrorAtEnd('GetConductivityGlycol: Temperature is out of range (too low) for glycol conductivity',  &
            LowTempLimitIndex,ReportMinOf=Temperature,ReportMaxOf=Temperature,  &
            ReportMaxUnits='{C}',ReportMinUnits='{C}')
    END IF

    IF ( (HighErrorThisTime) .AND. (HighTempLimitErr <= GlycolErrorLimitTest) ) THEN
       CALL ShowWarningError('GetConductivityGlycol: Temperature is out of range (too high) for glycol conductivity **')
       CALL ShowContinueError('..Called From:'//trim(calledfrom)//' Glycol='//TRIM(GlycolData(GlycolIndex)%Name)//  &
                                         ',Temperature='//TRIM(RoundSigDigits(Temperature,2)))
       CALL ShowContinueErrorTimeStamp(' ')
    ELSE IF ( HighErrorThisTime ) THEN
      CALL ShowRecurringWarningErrorAtEnd('GetConductivityGlycol: Temperature is out of range (too high) for glycol conductivity', &
            HighTempLimitIndex,ReportMinOf=Temperature,ReportMaxOf=Temperature,   &
            ReportMaxUnits='{C}',ReportMinUnits='{C}')
    END IF
  ENDIF

  RETURN

END FUNCTION GetConductivityGlycol

!*****************************************************************************

FUNCTION GetViscosityGlycol(Glycol,Temperature,GlycolIndex,calledfrom) RESULT(ReturnValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   June 2004
          !       MODIFIED       N/A
          !       RE-ENGINEERED  N/A

          ! PURPOSE OF THIS FUNCTION:
          ! This subroutine finds the viscosity for glycols at different
          ! temperatures.

          ! METHODOLOGY EMPLOYED:
          ! Linear interpolation is used to find viscosity values for a
          ! particular glycol (water or some mixture of water and another fluid).
          ! Warnings are given if the point is not clearly in the bounds of the
          ! glycol data.  The value returned is the appropriate limit value.

          ! REFERENCES:
          ! GetFluidPropertiesData: subroutine enforces that temperatures in
          ! all temperature lists are entered in ascending order.

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)    :: Glycol         ! carries in substance name
  REAL,        INTENT(IN)    :: Temperature    ! actual temperature given as input
  INTEGER,          INTENT(INOUT) :: GlycolIndex    ! Index to Glycol Properties
  character(len=*), intent(in)    :: calledfrom  ! routine this function was called from (error messages)
  REAL                       :: ReturnValue    ! Value for function

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Loop   ! DO loop counter
  INTEGER,SAVE :: HighTempLimitErr = 0
  INTEGER,SAVE :: HighTempLimitIndex = 0
  INTEGER,SAVE :: LowTempLimitErr = 0
  INTEGER,SAVE :: LowTempLimitIndex = 0
  INTEGER :: GlycolNum
  LOGICAL :: LowErrorThisTime
  LOGICAL :: HighErrorThisTime

          ! FLOW:
  LowErrorThisTime = .FALSE.
  HighErrorThisTime = .false.

          ! Get the input if we haven't already
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

          ! If no glycols, no fluid properties can be evaluated
  GlycolNum=0
  IF (NumOfGlycols == 0) &
    CALL ReportFatalGlycolErrors(NumOfGlycols,GlycolNum,.true.,Glycol,'GetViscosityGlycol','viscosity',calledfrom)

          ! If glycol index has not yet been found for this fluid, find its value now
  IF (GlycolIndex > 0) THEN
    GlycolNum=GlycolIndex
  ELSE    ! Find which refrigerant (index) is being requested
    GlycolNum = FindGlycol(Glycol)
    IF (GlycolNum == 0) THEN
      CALL ReportFatalGlycolErrors(NumOfGlycols,GlycolNum,.true.,Glycol,'GetViscosityGlycol','viscosity',calledfrom)
    ENDIF
    GlycolIndex=GlycolNum
  ENDIF

          ! If user didn't input data (shouldn't get this far, but just in case...), we can't find a value
  IF (.NOT. GlycolData(GlycolIndex)%ViscDataPresent)THEN
    CALL ReportFatalGlycolErrors(NumOfGlycols,GlycolNum,GlycolData(GlycolIndex)%ViscDataPresent,Glycol,  &
       'GetViscosityGlycol','viscosity',calledfrom)
  ENDIF

          ! Now determine the value of specific heat using interpolation
  IF (Temperature < GlycolData(GlycolIndex)%ViscLowTempValue) THEN ! Temperature too low
    LowErrorThisTime         = .TRUE.
    ReturnValue = GlycolData(GlycolIndex)%ViscValues(GlycolData(GlycolIndex)%ViscLowTempIndex)
  ELSE IF (Temperature > GlycolData(GlycolIndex)%ViscHighTempValue) THEN ! Temperature too high
    HighErrorThisTime         = .TRUE.
    ReturnValue = GlycolData(GlycolIndex)%ViscValues(GlycolData(GlycolIndex)%ViscHighTempIndex)
  ELSE    ! Temperature somewhere between the lowest and highest value
    ReturnValue = GlycolData(GlycolIndex)%ViscValues(GlycolData(GlycolIndex)%ViscLowTempIndex)
    ! bracket is temp > low, <= high (for interpolation
    DO Loop = GlycolData(GlycolIndex)%ViscLowTempIndex+1, GlycolData(GlycolIndex)%ViscHighTempIndex
      IF (Temperature > GlycolData(GlycolIndex)%ViscTemps(Loop)) CYCLE
      ReturnValue = GetInterpValue(Temperature,                                   &
                                   GlycolData(GlycolIndex)%ViscTemps(Loop-1),  &
                                   GlycolData(GlycolIndex)%ViscTemps(Loop),    &
                                   GlycolData(GlycolIndex)%ViscValues(Loop-1), &
                                   GlycolData(GlycolIndex)%ViscValues(Loop))
      EXIT ! DO loop
    END DO
  END IF

          ! Error handling
  IF (.not. WarmupFlag) THEN

    IF (LowErrorThisTime)  LowTempLimitErr = LowTempLimitErr + 1
    IF (HighErrorThisTime) HighTempLimitErr = HighTempLimitErr + 1

    IF ( (LowErrorThisTime) .AND. (LowTempLimitErr <= GlycolErrorLimitTest) ) THEN
       CALL ShowWarningError('GetViscosityGlycol: Temperature is out of range (too low) for glycol viscosity **')
       CALL ShowContinueError('..Called From:'//trim(calledfrom)//' Glycol='//TRIM(GlycolData(GlycolIndex)%Name)//  &
                                         ',Temperature='//TRIM(RoundSigDigits(Temperature,2)))
       CALL ShowContinueErrorTimeStamp(' ')
    ELSE IF ( LowErrorThisTime ) THEN
      CALL ShowRecurringWarningErrorAtEnd('GetViscosityGlycol: Temperature is out of range (too low) for glycol viscosity',  &
         LowTempLimitIndex,ReportMinOf=Temperature,ReportMaxOf=Temperature,  &
         ReportMaxUnits='{C}',ReportMinUnits='{C}')
    END IF

    IF ( (HighErrorThisTime) .AND. (HighTempLimitErr <= GlycolErrorLimitTest) ) THEN
       CALL ShowWarningError('GetViscosityGlycol: Temperature is out of range (too high) for glycol viscosity **')
       CALL ShowContinueError('..Called From:'//trim(calledfrom)//' Glycol='//TRIM(GlycolData(GlycolIndex)%Name)//  &
                                         ',Temperature='//TRIM(RoundSigDigits(Temperature,2)))
       CALL ShowContinueErrorTimeStamp(' ')
    ELSE IF ( HighErrorThisTime ) THEN
      CALL ShowRecurringWarningErrorAtEnd('GetViscosityGlycol: Temperature is out of range (too high) for glycol viscosity',   &
         HighTempLimitIndex,ReportMinOf=Temperature,ReportMaxOf=Temperature,  &
         ReportMaxUnits='{C}',ReportMinUnits='{C}')
    END IF
  ENDIF


  RETURN

END FUNCTION GetViscosityGlycol

!*****************************************************************************

FUNCTION GetInterpValue(Tact,Tlo,Thi,Xlo,Xhi) RESULT(ReturnValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   June 2004
          !       MODIFIED       N/A
          !       RE-ENGINEERED  N/A

          ! PURPOSE OF THIS FUNCTION:
          ! This subroutine does a simple linear interpolation.

          ! METHODOLOGY EMPLOYED:
          ! No mysteries here...just plain-old linear interpolation.

          ! REFERENCES:
          ! Any basic engineering mathematic text.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE           ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL, INTENT(IN) :: Tact  ! actual temperature at which we want the property of interest
  REAL, INTENT(IN) :: Tlo   ! temperature below Tact for which we have property data
  REAL, INTENT(IN) :: Thi   ! temperature above Tact for which we have property data
  REAL, INTENT(IN) :: Xlo   ! value of property at Tlo
  REAL, INTENT(IN) :: Xhi   ! value of property at Thi
  REAL             :: ReturnValue

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL, PARAMETER :: TempToler = 0.001    ! Some reasonable value for comparisons

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:
  IF ( ABS(Thi-Tlo) > TempToler) THEN
    ReturnValue = Xhi - ( ( (Thi-Tact)/(Thi-Tlo) ) * (Xhi-Xlo) )
  ELSE
    CALL ShowFatalError('GetInterpValue: Temperatures for fluid property data too close together, division by zero')
    ReturnValue = 0.0
  END IF

  RETURN

END FUNCTION GetInterpValue

!*****************************************************************************

FUNCTION GetQualityRefrig(Refrigerant,Temperature,Enthalpy,RefrigIndex,calledfrom) RESULT(ReturnValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Simon Rees (May 2002)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function determines the quality of a refrigerant in the saturate
          ! region based on its temperature and enthalpy

          ! METHODOLOGY EMPLOYED:
          ! Just checks to see whether or not the refrigerant name coming in can
          ! be found in the refrigerant derived type.  If so, the "reverse" of the
          ! GetSatEnthalpyRefrig function is performed.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: Refrigerant ! carries in substance name
  REAL,        INTENT(IN)  :: Temperature ! actual temperature given as input
  REAL,        INTENT(IN)  :: Enthalpy    ! actual enthalpy given as input
  INTEGER,       INTENT(INOUT)  :: RefrigIndex ! Index to Refrigerant Properties
  character(len=*), intent(in)  :: calledfrom  ! routine this function was called from (error messages)
  REAL                     :: ReturnValue

        ! INTERFACE BLOCK SPECIFICATIONS:
        ! na

        ! DERIVED TYPE DEFINITIONS:
        ! na

        ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL    :: SatVapEnthalpy ! value of enthalpy at hi index value for given Quality
  REAL    :: SatLiqEnthalpy  ! value of enthalpy at TempIndex index value for given Quality
  INTEGER :: RefrigNum    ! index for refrigerant under consideration
  INTEGER :: HiTempIndex              ! array index for temp above input temp
  INTEGER :: LoTempIndex              ! array index for temp below input temp
  REAL    :: TempInterpRatio            ! ratio to interpolate in temperature domain
  INTEGER,SAVE :: TempLoRangeErrIndex=0
  INTEGER,SAVE :: TempHiRangeErrIndex=0

          ! FLOW:
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

  RefrigNum=0
  IF (NumOfRefrigerants == 0) THEN
    CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,'GetQualityRefrig','enthalpy',calledfrom)
  ENDIF

  ! Find which refrigerant (index) is being requested and then determine
  ! where the temperature is within the temperature array
  IF (RefrigIndex > 0) THEN
    RefrigNum=RefrigIndex
  ELSE
    ! Find which refrigerant (index) is being requested
    RefrigNum = FindRefrigerant(Refrigerant)
    IF (RefrigNum == 0) THEN
      CALL ReportFatalRefrigerantErrors(NumOfRefrigerants,RefrigNum,.true.,Refrigerant,'GetQualityRefrig','enthalpy',calledfrom)
    ENDIF
    RefrigIndex=RefrigNum
  ENDIF

  LoTempIndex = FindArrayIndex(Temperature,RefrigData(RefrigNum)%HTemps,  &
                           RefrigData(RefrigNum)%HfLowTempIndex,RefrigData(RefrigNum)%HfHighTempIndex)
  HiTempIndex = LoTempIndex + 1

  ! check on the data bounds and adjust indices to give clamped return value
  IF (LoTempIndex == 0) THEN
    SatLiqEnthalpy = RefrigData(RefrigNum)%HfValues(RefrigData(RefrigNum)%HfLowTempIndex)
    SatVapEnthalpy = RefrigData(RefrigNum)%HfgValues(RefrigData(RefrigNum)%HfLowTempIndex)
    IF (.not. WarmupFlag)   &
      ! Temperature supplied is out of bounds--produce an error message...
      CALL ShowRecurringWarningErrorAtEnd(  &
         'GetQualityRefrig: ** Temperature for requested quality is below the range of data supplied **',TempLoRangeErrIndex,  &
          ReportMinOf=Temperature,ReportMaxOf=Temperature,ReportMinUnits='{C}',ReportMaxUnits='{C}')

  ELSE IF(HiTempIndex > RefrigData(RefrigNum)%NumHPoints) THEN
    SatLiqEnthalpy = RefrigData(RefrigNum)%HfValues(RefrigData(RefrigNum)%HfHighTempIndex)
    SatVapEnthalpy = RefrigData(RefrigNum)%HfgValues(RefrigData(RefrigNum)%HfHighTempIndex)
    IF (.not. WarmupFlag)   &
     ! Temperature supplied is out of bounds--produce an error message...
      CALL ShowRecurringWarningErrorAtEnd(  &
          'GetQualityRefrig: ** Temperature requested quality is above the range of data supplied **',TempHiRangeErrIndex,  &
          ReportMinOf=Temperature,ReportMaxOf=Temperature,ReportMinUnits='{C}',ReportMaxUnits='{C}')

  ELSE  ! in normal range work out interpolated liq and gas enthalpies
    TempInterpRatio = (Temperature - RefrigData(RefrigNum)%HTemps(LoTempIndex)) &
            /(RefrigData(RefrigNum)%HTemps(HiTempIndex) - RefrigData(RefrigNum)%HTemps(LoTempIndex))
    SatLiqEnthalpy  = TempInterpRatio*RefrigData(RefrigNum)%HfValues(HiTempIndex) &
                  +(1.0-TempInterpRatio)*RefrigData(RefrigNum)%HfValues(LoTempIndex)
    SatVapEnthalpy = TempInterpRatio*RefrigData(RefrigNum)%HfgValues(HiTempIndex) &
                  +(1.0-TempInterpRatio)*RefrigData(RefrigNum)%HfgValues(LoTempIndex)
  END IF

  ! calculate final quality value from enthalpy ratio
  ReturnValue = (Enthalpy-SatLiqEnthalpy)/(SatVapEnthalpy - SatLiqEnthalpy)

  ! final check to bound returned quality value
  IF (ReturnValue < 0.0) THEN
!    CALL ShowRecurringWarningErrorAtEnd('GetQualityRefrig: ** '//  &
!                   'Quality is less than zero in GetQualityRefrig; Quality reset to 0.0 **')
    ReturnValue = 0.0
  ELSE IF (ReturnValue > 1.0) THEN
!    CALL ShowRecurringWarningErrorAtEnd('GetQualityRefrig: ** '//  &
!                   'Quality is greater than one in GetQualityRefrig; refrigerant is superheated **')
    ReturnValue = 2.0
  END IF

  RETURN

END FUNCTION GetQualityRefrig

!*****************************************************************************

INTEGER FUNCTION FindRefrigerant(Refrigerant)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Simon Rees (June 2002)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function simply determines the index of the refrigerant named
          ! in the input variable to this routine within the derived type.

          ! METHODOLOGY EMPLOYED:
          ! Just checks to see whether or not the refrigerant name coming in can
          ! be found in the refrigerant derived type.  If so, the function is set
          ! to the index within the derived type.  If the input has not been read
          ! yet for some reason, that must be done.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList, MakeUPPERCase

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: Refrigerant ! carries in substance name

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Found   ! Indicator for found item

          ! FLOW:
          ! Make sure we have already read in the input
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

          ! Check to see if this glycol shows up in the glycol data
  Found=FindItemInList(MakeUPPERCase(Refrigerant),RefrigData%Name,NumOfRefrigerants)

  IF (Found > 0) THEN
    FindRefrigerant = Found
    RefrigUsed(Found)=.true.
  ELSE ! not found - errors handled in calling proceedure
    FindRefrigerant = 0
  ENDIF

  RETURN

END FUNCTION FindRefrigerant

!*****************************************************************************

INTEGER FUNCTION FindGlycol(Glycol)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Simon Rees (June 2002)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function simply determines the index of the glycol named
          ! in the input variable to this routine within the derived type.

          ! METHODOLOGY EMPLOYED:
          ! Just checks to see whether or not the glycol name coming in can
          ! be found in the glycol derived type.  If so, the function is set
          ! to the index within the derived type.  If the input has not been read
          ! yet for some reason, that must be done.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList,MakeUPPERCase

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: Glycol ! carries in substance name

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Found   ! Indicator for found item

          ! FLOW:
          ! Make sure we have already read in the input
  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

          ! Check to see if this glycol shows up in the glycol data
  Found=FindItemInList(MakeUPPERCase(Glycol),GlycolData%Name,NumOfGlycols)

  IF (Found > 0) THEN
    FindGlycol=Found
    GlycolUsed(Found)=.true.
  ELSE        ! return zero - error checking in calling proceedure
    FindGlycol = 0
  ENDIF

  RETURN

END FUNCTION FindGlycol

!*****************************************************************************

CHARACTER(Len=MaxNameLength) FUNCTION GetGlycolNameByIndex(Index)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   May 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function simply returns the glycol name by index from the
          ! GlycolData data structure.  This is needed to expose the name
          ! as the data structure is private.
          ! This is used by plant equipment to pass in both the proper index
          ! and the proper name when calling glycol routines.  Thus, the index
          ! is already known, and the input is assumed to be found.

          ! METHODOLOGY EMPLOYED:
          ! Just checks to see whether or not the glycol index is valid
          ! and if so, the function returns the name.  If not, it returns ' '

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: Index ! carries in substance index

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

          ! FLOW:

          ! Check to see if this glycol shows up in the glycol data
!  ArrayLength = SIZE(GlycolData)

  IF (Index <= NumOfGlycols) THEN
    GetGlycolNameByIndex = GlycolData(Index)%Name
  ELSE        ! return blank - error checking in calling proceedure
    GetGlycolNameByIndex = ' '
  ENDIF

  RETURN

END FUNCTION GetGlycolNameByIndex

!*****************************************************************************

INTEGER FUNCTION FindArrayIndex(Value,Array,LowBound,UpperBound)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Simon Rees (May 2002)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This generic function simply finds the points in an array between
          ! which a single value is found.  The returned value is the index of
          ! the low point.

          ! METHODOLOGY EMPLOYED:
          ! Straight interval halving. It is assumed that the values in the array
          ! appear in ascending order. If the value is below that in the supplied
          ! data array a zero index is returned. If the value is above that in the
          ! supplied data array, the max index is returned. This allows some error
          ! checking in the calling routine.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL, INTENT(IN)               :: Value      ! Value to be placed/found within the array of values
  REAL, INTENT(IN), DIMENSION(:) :: Array      ! Array of values in ascending order
  INTEGER, INTENT(IN), OPTIONAL       :: LowBound   ! Valid values lower bound (set by calling program)
  INTEGER, INTENT(IN), OPTIONAL       :: UpperBound   ! Valid values upper bound (set by calling program)

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: start                   ! sets low index value
  INTEGER :: finish                  ! sets high index value
  INTEGER :: middle                  ! difference of finish & start

          ! FLOW:
  IF (PRESENT(LowBound) .and. PRESENT(UpperBound)) THEN
    start  = LowBound
    finish = UpperBound
  ELSEIF (PRESENT(LowBound)) THEN
    start = LowBound
    finish = SIZE(Array)
  ELSEIF (PRESENT(UpperBound)) THEN
    start  = 1
    finish = UpperBound
  ELSE
    start  = 1
    finish = SIZE(Array)
  ENDIF

  ! check bounds of data and set limiting values of the index
  IF(Value < Array(start)) THEN
    FindArrayIndex = 0
  ELSE IF(Value >Array(finish)) THEN
    FindArrayIndex = finish
  ELSE  ! start searching by bisection method
    DO WHILE ((finish - start) > 1)
      middle = (finish + start) / 2
      IF (Value > Array(middle)) THEN
        start = middle
      ELSE
        finish = middle
      END IF
    END DO
    FindArrayIndex = start
  END IF

  RETURN

END FUNCTION FindArrayIndex

!*****************************************************************************

FUNCTION GetInterpolatedSatProp(Temperature, PropTemps, LiqProp, VapProp, Quality, calledfrom, LowBound, UpperBound)   &
                                     RESULT(ReturnValue)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Simon Rees
          !       DATE WRITTEN   May 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This generic function performs an interpolation on the supplied saturated
          ! liquid and vapor data to find the saturated property value at a given
          ! temperature and quality. This function is used by all the functions that
          ! get saturated property values.

          ! METHODOLOGY EMPLOYED:
          ! Index of arrays either side of given temperature is found using FindArrayIndex.
          ! Double linear interpolation is used to first find property values at the given
          ! quality bounding the required temperature. These values are interpolated in the
          ! temperature domain to find the final value.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL, INTENT(IN)               :: Temperature   ! Saturation Temp.
  REAL, INTENT(IN), DIMENSION(:) :: PropTemps   ! Array of temperature at which props are available
  REAL, INTENT(IN), DIMENSION(:) :: LiqProp     ! Array of saturated liquid properties
  REAL, INTENT(IN), DIMENSION(:) :: VapProp     ! Array of saturatedvapour properties
  REAL, INTENT(IN)               :: Quality     ! Quality
  character(len=*), intent(in)        :: calledfrom  ! routine this function was called from (error messages)
  INTEGER, INTENT(IN)                 :: LowBound    ! Valid values lower bound (set by calling program)
  INTEGER, INTENT(IN)                 :: UpperBound  ! Valid values upper bound (set by calling program)
  REAL                           :: ReturnValue

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

  INTEGER :: HiTempIndex              ! array index for temp above input temp
  INTEGER :: LoTempIndex              ! array index for temp below input temp
  REAL    :: LoSatProp                  ! Sat. prop. at lower temp & given quality
  REAL    :: HiSatProp                ! Sat. prop. at higher temp & given quality
  REAL    :: TempInterpRatio            ! ratio to interpolate in temperature domain
 ! error counters and dummy string
  LOGICAL :: ErrorFlag                  ! error flag for current call
  INTEGER,SAVE :: TempRangeErrCount=0   ! cumulative error counter
  INTEGER,SAVE :: TempRangeErrIndex=0

  ErrorFlag = .False.

  LoTempIndex = FindArrayIndex(Temperature, PropTemps, LowBound, UpperBound)
  HiTempIndex = LoTempIndex + 1

  IF (LoTempIndex == 0) THEN
    LoTempIndex = LowBound ! MAX(1, LoTempIndex)
    ReturnValue = LiqProp(LoTempIndex) + &
               Quality*(VapProp(LoTempIndex) - LiqProp(LoTempIndex))
    ErrorFlag = .True.
  ELSE IF(HiTempIndex > UpperBound) THEN
    HiTempIndex = UpperBound
    ReturnValue = LiqProp(HiTempIndex) + &
               Quality*(VapProp(HiTempIndex) - LiqProp(HiTempIndex))
    ErrorFlag = .True.
  ELSE
    ! find adjacent property values at the given quality
    LoSatProp = LiqProp(LoTempIndex) + &
                 Quality*(VapProp(LoTempIndex) - LiqProp(LoTempIndex))

    HiSatProp = LiqProp(HiTempIndex) + &
                 Quality*(VapProp(HiTempIndex) - LiqProp(HiTempIndex))

    ! find interpolation ratio in temperature direction
    TempInterpRatio = (Temperature - PropTemps(LoTempIndex)) / &
                      (PropTemps(HiTempIndex) - PropTemps(LoTempIndex))

    ! apply final linear interpolation
    ReturnValue = LoSatProp + TempInterpRatio*(HiSatProp - LoSatProp)
  ENDIF

  IF(ErrorFlag .and. .not. calledfrom == 'ReportAndTestRefrigerants' )THEN
      TempRangeErrCount = TempRangeErrCount + 1
     ! send warning
      IF (TempRangeErrCount <= RefrigerantErrorLimitTest) THEN
        CALL ShowSevereError('GetInterpolatedSatProp: Saturation temperature for interpolation is out of range '// &
                             'of data supplied: **')
        CALL ShowContinueErrorTimeStamp(' Called from:'//trim(calledfrom))
        CALL ShowContinueError('Refrigerant temperature = '//TRIM(RoundSigDigits(Temperature,2)))
        CALL ShowContinueError('Returned saturated property value = '//TRIM(RoundSigDigits(ReturnValue,3)))
      ELSE
        CALL ShowRecurringSevereErrorAtEnd(  &
             'GetInterpolatedSatProp: Refrigerant temperature for interpolation out of range error',TempRangeErrIndex,  &
                ReportMinOf=Temperature,ReportMaxOf=Temperature,ReportMinUnits='{C}',ReportMaxUnits='{C}')
      ENDIF
  END IF

  RETURN

END FUNCTION GetInterpolatedSatProp

!*****************************************************************************

INTEGER FUNCTION CheckFluidPropertyName(NameToCheck)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   October 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function checks on an input fluid property to make sure it is valid.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: NameToCheck  ! Name from input(?) to be checked against valid FluidPropertyNames

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER Found

  IF (GetInput) THEN
    CALL GetFluidPropertiesData
    GetInput = .FALSE.
  END IF

  ! Item must be either in Refrigerant or Glycol list
  Found = 0
  IF (NumOfRefrigerants > 0) THEN
    Found=FindItemInList(NameToCheck,RefrigData%Name,NumOfRefrigerants)
  ENDIF
  IF (Found == 0) THEN
    IF (NumOfGlycols > 0) THEN
      Found=FindItemInlist(NameToCheck,GlycolData%Name,NumOfGlycols)
    ENDIF
  ENDIF

  CheckFluidPropertyName=Found

  RETURN

END FUNCTION CheckFluidPropertyName

SUBROUTINE ReportOrphanFluids

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   March 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! In response to CR8008, report orphan (unused) fluid items.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: DisplayUnusedObjects
  USE General, ONLY: RoundSigDigits
  USE InputProcessor, ONLY: SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: NeedOrphanMessage
  INTEGER :: Item
  INTEGER :: NumUnusedRefrig
  INTEGER :: NumUnusedGlycol

  NeedOrphanMessage=.true.
  NumUnusedRefrig=0

  DO Item=1,NumOfRefrigerants
    IF (RefrigUsed(Item)) CYCLE
    IF (SameString(RefrigData(Item)%Name,Steam)) CYCLE
    IF (NeedOrphanMessage .and. DisplayUnusedObjects) THEN
      CALL ShowWarningError('The following fluid names are "Unused Fluids".  These fluids are in the idf')
      CALL ShowContinueError(' file but are never obtained by the simulation and therefore are NOT used.')
      NeedOrphanMessage=.false.
    ENDIF
    IF (DisplayUnusedObjects) THEN
      CALL ShowMessage('Refrigerant='//TRIM(RefrigData(Item)%Name))
    ELSE
      NumUnusedRefrig=NumUnusedRefrig+1
    ENDIF
  ENDDO

  NumUnusedGlycol=0

  DO Item=1,NumOfGlycols
    IF (GlycolUsed(Item)) CYCLE
    IF (SameString(GlycolData(Item)%Name,Water)) CYCLE
    IF (SameString(GlycolData(Item)%Name,EthyleneGlycol)) CYCLE
    IF (SameString(GlycolData(Item)%Name,PropyleneGlycol)) CYCLE
    IF (NeedOrphanMessage .and. DisplayUnusedObjects) THEN
      CALL ShowWarningError('The following fluid names are "Unused Fluids".  These fluids are in the idf')
      CALL ShowContinueError(' file but are never obtained by the simulation and therefore are NOT used.')
      NeedOrphanMessage=.false.
    ENDIF
    IF (DisplayUnusedObjects) THEN
      CALL ShowMessage('Glycol='//TRIM(GlycolData(Item)%Name))
    ELSE
      NumUnusedGlycol=NumUnusedGlycol+1
    ENDIF
  ENDDO

  IF (NumUnusedRefrig > 0 .or. NumUnusedGlycol > 0) THEN
    IF (NumUnusedRefrig > 0)  &
       CALL ShowMessage('There are '//trim(RoundSigDigits(NumUnusedRefrig))//' unused refrigerants in input.')
    IF (NumUnusedGlycol > 0)  &
       CALL ShowMessage('There are '//trim(RoundSigDigits(NumUnusedGlycol))//' unused glycols in input.')
    CALL ShowMessage('Use Output:Diagnostics,DisplayUnusedObjects; to see them.')
  ENDIF

  RETURN

END SUBROUTINE ReportOrphanFluids

SUBROUTINE ReportFatalGlycolErrors(NumGlycols,GlycolNum,DataPresent,GlycolName,RoutineName,Property,calledfrom)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   July 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Consolidate fatal error reporting for glycols.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER :: NumGlycols  ! Number of Glycols in input/data
  INTEGER :: GlycolNum   ! Glycol Index
  LOGICAL :: DataPresent ! data is present for this fluid.
  CHARACTER(len=*) :: GlycolName  ! Name being reported
  CHARACTER(len=*) :: RoutineName ! Routine name to show
  CHARACTER(len=*) :: Property    ! Property being requested
  CHARACTER(len=*) :: calledfrom  ! original called from (external to fluid properties)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: RefrigNo

  ! check and see if it might be a refrigerant
  RefrigNo=FindRefrigerant(GlycolName)

  IF (NumGlycols == 0) THEN
    CALL ShowSevereError(RoutineName//': no glycols found -- cannot evaluate fluid '//Property//  &
      ' for "'//trim(GlycolName)//'", called from: '//calledfrom)
  ELSEIF (GlycolNum == 0) THEN
    CALL ShowSevereError(RoutineName//': data not found in input for requested glycol "'//   &
      trim(GlycolName)//'", called from: '//calledfrom)
  ELSEIF (.not. DataPresent) THEN
    CALL ShowSevereError(RoutineName//': '//Property//' data not found in input for requested glycol "'//   &
      trim(GlycolName)//'", called from: '//calledfrom)
  ENDIF
  IF (RefrigNo > 0)   &
      CALL ShowContinueError('Note: that fluid is listed as a Refrigerant from input.')

  CALL ShowFatalError('Program terminates due to preceding condition.')


  RETURN

END SUBROUTINE ReportFatalGlycolErrors

SUBROUTINE ReportFatalRefrigerantErrors(NumRefrigerants,RefrigerantNum,DataPresent,RefrigerantName,RoutineName,Property,calledfrom)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   July 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Consolidate fatal error reporting for refrigerants.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER :: NumRefrigerants  ! Number of Refrigerants in input/data
  INTEGER :: RefrigerantNum   ! Refrigerant Index
  LOGICAL :: DataPresent ! data is present for this fluid.
  CHARACTER(len=*) :: RefrigerantName  ! Name being reported
  CHARACTER(len=*) :: RoutineName ! Routine name to show
  CHARACTER(len=*) :: Property    ! Property being requested
  CHARACTER(len=*) :: calledfrom  ! original called from (external to fluid properties)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: GlycolNo

  ! check and see if it might be a refrigerant
  GlycolNo=FindGlycol(RefrigerantName)

  IF (NumRefrigerants == 0) THEN
    CALL ShowSevereError(RoutineName//': no refrigerants found -- cannot evaluate fluid '//Property//  &
      ' for "'//trim(RefrigerantName)//'", called from: '//calledfrom)
  ELSEIF (RefrigerantNum == 0) THEN
    CALL ShowSevereError(RoutineName//': data not found in input for requested refrigerant "'//   &
      trim(RefrigerantName)//'", called from: '//calledfrom)
  ELSEIF (.not. DataPresent) THEN
    CALL ShowSevereError(RoutineName//': '//Property//' data not found in input for requested refrigerant "'//   &
      trim(RefrigerantName)//'", called from: '//calledfrom)
  ENDIF
  IF (GlycolNo > 0)   &
      CALL ShowContinueError('Note: that fluid is listed as a Glycol from input.')

  CALL ShowFatalError('Program terminates due to preceding condition.')


  RETURN

END SUBROUTINE ReportFatalRefrigerantErrors


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

END MODULE FluidProperties

