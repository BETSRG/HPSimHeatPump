! ************************************** !
! ** HEAT PUMP SIMULATION CODE HEADER ** !
! ************************************** !

! ************************************** !
! -- HIGH LEVEL OVERVIEW/DESCRIPTION --- !
! -------------------------------------- !
! Provide a 1 or 2 sentence overview of this module.  
! In most cases, it is probably not a useful entry and can be inferred from the name of the module anyway.
!
! ************************************** !
! -- PHYSICAL DESCRIPTION -------------- !
! -------------------------------------- !
! This component represents something...or nothing...in a heat pump system.
! A description of the component is found at:
! some website
! From that website: 
!  - It does something

! ************************************** !
! -- SIMULATION DATA RESPONSIBILITIES -- !
! -------------------------------------- !
! Here's a one line summary of what this does for the simulation itself.
! This module takes inputs such as...and modifies them like so...and outputs these things

! ************************************** !
! -- INPUT FILES/OUTPUT FILES (none) --- !
! -------------------------------------- !
! Check for any OPEN statements in the code
! Check for any WRITE statements in the code
!  Note that writing to unit "*" or "6" means just write to the terminal, not to a file

! ************************************** !
! -- MODULE LEVEL VARIABLES/STRUCTURES - !
! -------------------------------------- !
! What vars and structures are defined at the *module* level...are units defined?  Any other notes?

! ************************************** !
! -- SUMMARY OF METHODS, CALL TREE ----- !
! -------------------------------------- !
! This module contains X methods:
!    PUBLIC InitSomething -- What does this do (in one line)?
!      Called by what other modules?

! ************************************** !
! -- ISSUES/BUGS/TICKETS --------------- !
! -------------------------------------- !
! Are there any interesting issues with this, unfuddle ticket numbers?

! ************************************** !
! -- CHANGELOG ------------------------- !
! -------------------------------------- !
! 2012-12-11 | ESL | Initial header
! YEAR-MM-DD | ABC | Some other log message? 

! ************************************** !
! -- TODO/NOTES/RECOMMENDATIONS -------- !
! -------------------------------------- !
! Put some notes for what needs to happen here
! Silly things are fine
! Somethings these small silly things are great to grab on to when starting up with the code after being off for a while

SUBROUTINE CalcFilterDrierDP(XIN,PAR,OUT)

! ----------------------------------------------------------------------
!
!   Purpose: To calculate the pressure drop across the filter drier.
!
!   Method: Catalog curve fit 
!
!	Inputs:
!		RefName=Refrigerant name
!		XIN(1) = Mass flow rate, kg/s
!
!	Parameters:
!		PAR(1)=Flow capacity, ton
!		PAR(2)=Rating pressure drop, kPa
!
!	Outputs:
!		OUT(1) = Pressure drop, kPa
!
!   Author:
!   Ipseng Iu
!   Mechanical and Aerospace Engineering
!   Oklahoma State University, Stillwater
!
!   Date: July 2005
!
! ----------------------------------------------------------------------
USE DataGlobals_HPSim, ONLY: RefName    !RS Comment: Needs to be used for implementation with Energy+ currently (7/23/12)
USE DataSimulation, ONLY: FilFlowCap, FilRatDP  !RS: Debugging: Brings variable names instead of numbers through
implicit none

!Flow:

REAL, INTENT(IN) :: XIN(1)
REAL, INTENT(IN) :: PAR(2)
REAL, INTENT(OUT) :: OUT(1)

!Constants from ARI std 70 
REAL,PARAMETER :: FlowRatePerTonR22 = 0.0224 !kg/s/ton '0.0064 'kg/s/kW
REAL,PARAMETER :: FlowRatePerTonR134A = 0.02345 !kg/s/ton '0.0067 'kg/s/kW
REAL,PARAMETER :: FlowRatePerTonR407C = 0.0224 !kg/s/ton '0.0064 'kg/s/kW
REAL,PARAMETER :: FlowRatePerTonR410A = 0.021 !kg/s/ton '0.006  'kg/s/kW

REAL mdot !Refrigerant mass flow rate, kg/s
REAL FlowCapacity !Flow capacity, ton
REAL RatedDP !Rating pressure drop, kPa
REAL FlowRatePerTon !Flow rate per ton, kg/s/ton
REAL DP !Pressure drop, kPa

!Flow:

	mdot=XIN(1)

	FlowCapacity=PAR(FilFlowCap) !RS: Debugging: Formerly PAR(1)
	RatedDP=PAR(FilRatDP)  !RS: Debugging: Formerly PAR(2)

	IF (FlowCapacity .GT. 0) THEN
		SELECT CASE (TRIM(RefName))
		CASE('R22')
			FlowRatePerTon = FlowRatePerTonR22
		CASE('R134A')
			FlowRatePerTon = FlowRatePerTonR134A
		CASE('R410A')
			FlowRatePerTon = FlowRatePerTonR410A
		Case('R407C')
			FlowRatePerTon = FlowRatePerTonR407C
		Case DEFAULT
			FlowRatePerTon = FlowRatePerTonR22
		END SELECT

		DP = RatedDP / (FlowCapacity * FlowRatePerTon) * mdot   !Determining Pressure Drop
	ELSE
		DP=0
	END IF

	OUT(1)=DP

RETURN

END SUBROUTINE CalcFilterDrierDP




