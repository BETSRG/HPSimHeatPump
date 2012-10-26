MODULE DataCostEstimate      ! EnergyPlus Data-Only Module
          ! PURPOSE OF THIS MODULE:
          ! This data-only module is a repository for Cost Estimation variables which are considered
          ! to be "global" in nature in EnergyPlus.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! OTHER NOTES:

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.


          ! MODULE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
TYPE                   :: CostLineItemStruct
  CHARACTER(len=MaxNameLength) :: LineName = '' ! object name (needed ?)
  CHARACTER(len=MaxNameLength) :: LineType = '' ! Case statement driver?
  CHARACTER(len=MaxNameLength) :: ParentObjType = '' ! parent reference to IDD object type
  CHARACTER(len=MaxNameLength) :: ParentObjName = '' ! parent instance in IDF
  CHARACTER(len=MaxNameLength) :: ParentObjKey  = '' ! end use key for parent object
  INTEGER :: ParentObjIDinList  = 1  !
  REAL    :: PerSquareMeter     = 0.0 ! cost per square meter
  REAL    :: PerEach            = 0.0 ! cost per each
  REAL    :: PerKiloWattCap     = 0.0 ! cost per kW of nominal capacity
  REAL    :: PerKWCapPerCOP     = 0.0 ! cost per kW of nominal capacity per COP
  REAL    :: PerCubicMeter      = 0.0 ! cost per cubic meter
  REAL    :: PerCubMeterPerSec  = 0.0 ! cost per cubic meter per second
  REAL    :: PerUAinWattperDelK = 0.0 ! cost per (UA) in Watt/deltaK
!  REAL    :: AnnualMaintFract   = 0.0 ! cost for annual service and non energy consumables
!  REAL    :: MinorOverhallFract = 0.0 ! cost for minor overhalls
!  INTEGER :: MinorOverhallYears = 0   ! year interval for minor overhalls
!  REAL    :: MajorOverhallFract = 0.0 ! cost for major overhall
!  INTEGER :: MajorOverhallYears = 0   ! year interval for major overhalls
!  INTEGER :: LifeYears          = 0.0 ! expected life in years
!  REAL    :: ValueAtReplacement = 0.0 ! residual value at end of life
  INTEGER :: LineNumber         = -1  ! number of line item in detail list
  REAL    :: Qty                = 0.0 ! quantity in calculations (can be input)
  Character(len=MaxNameLength)  :: Units = ''! Reported units
  REAL    :: ValuePer           = 0.0 ! Cost used in final calculation
  REAL    :: LineSubTotal       = 0.0 ! line item total  Qty * ValuePer
END TYPE CostLineItemStruct

TYPE  :: CostAdjustmentStruct
  REAL    :: LineItemTot        != 0.0 ! holds total from line item cost calculations
  REAL    :: MiscCostperSqMeter != 0.0 ! holds user-defined constant cost model
  REAL    :: DesignFeeFrac      != 0.0 ! holds user-defined fraction for design fees
  REAL    :: ContractorFeeFrac  != 0.0 ! holds user-defined fraction for contractor fees
  REAL    :: ContingencyFrac    != 0.0 ! holds user-defined fraction for contingencies
  REAL    :: BondCostFrac       != 0.0 ! holds user-defined fraction for bonding costs
  REAL    :: CommissioningFrac  != 0.0 ! holds user-defined fraction for commissioning costs
  REAL    :: RegionalModifier   != 1.0 ! holds user-defined multiplier to account for regional diffs
  REAL    :: GrandTotal         != 0.0 ! the Grand Total of all line items plus all other costs
END TYPE CostAdjustmentStruct

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! MODULE VARIABLE DECLARATIONS:
TYPE (CostLineItemStruct), ALLOCATABLE, DIMENSION(:) :: CostLineItem

 ! CurntBldg holds results for current bldg. cost estimate
TYPE (CostAdjustmentStruct) :: CurntBldg  =CostAdjustmentStruct(0.,0.,0.,0.,0.,0.,0.,1.,0.)
 ! RefrnceBldg holds user input for comparison.
TYPE (CostAdjustmentStruct) :: RefrncBldg =CostAdjustmentStruct(0.,0.,0.,0.,0.,0.,0.,1.,0.)

INTEGER  :: NumLineItems=0  ! number of cost estimate line items
LOGICAL  :: DoCostEstimate   = .FALSE. !set to true if any cost estimating needed

TYPE monetaryUnitType
  CHARACTER(len=MaxNameLength)    :: code      = '' !ISO code for currency such as USD or EUR
  CHARACTER(len=MaxNameLength)    :: txt       = '' !text representation of the currency
  CHARACTER(len=MaxNameLength)    :: html      = '' !representation for HTML file - contains unicode references
END TYPE
TYPE (monetaryUnitType), ALLOCATABLE, DIMENSION(:) :: monetaryUnit
INTEGER                           :: numMonetaryUnit=0
INTEGER                           :: selectedMonetaryUnit=0


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

END MODULE DataCostEstimate
