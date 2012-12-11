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

MODULE ReversingValveMod

IMPLICIT NONE

!------------------------------------------------------------------------------
!
!The reversing valve model from York empirical data
!Reference:
!Damasceno G D S, Lee W N T, Rooke S P., Goldschmidt V W. 
!"Performance of a heat pump reversing valves and comparison through 
!characterising parameters." ASHRAE Transactions, 1988, Vol 94, Part 1, 
!pp. 304-317
!
!Written by IPSENG IU
!July 2007
!
!------------------------------------------------------------------------------

PUBLIC SuctionHeatTransfer
PUBLIC SuctionPressureDrop

CONTAINS

!------------------------------------------------------------------------------

SUBROUTINE SuctionHeatTransfer(mdot,Tdis,Tsuc,hsi,hso)

!Calculate heat gain at suction side

IMPLICIT NONE

REAL, INTENT(IN) :: mdot !refrigerant mass flow rate, kg/s
REAL, INTENT(IN) :: Tdis !Discharge temperature, C
REAL, INTENT(IN) :: Tsuc !Suction temperature, C
REAL, INTENT(IN) :: hsi !Suction inlet enthalpy, kJ/kg
REAL, INTENT(OUT) :: hso !Suction outlet enthalpy, kJ/kg

REAL :: UA !Overall heat transfer coefficient, kW/K

UA = 0.1541*mdot - 0.002    !RS Comment: What are the values from?
IF (UA .LT. 0) THEN
    UA=0
END IF

hso=UA*(Tdis-Tsuc)/mdot+hsi

END SUBROUTINE SuctionHeatTransfer

!------------------------------------------------------------------------------

SUBROUTINE SuctionPressureDrop(mdot,DP)

!Calculate suction side pressure drop

IMPLICIT NONE

REAL, INTENT(IN) :: mdot !Refrigerant mass flow rate, kg/s
REAL, INTENT(OUT) :: DP !Pressure drop kPa

DP = 327.69*mdot + 3.9633   !RS Comment: What are the values from?

IF (DP .LT. 0) THEN
    DP = 0
END IF

END SUBROUTINE SuctionPressureDrop

!------------------------------------------------------------------------------

END MODULE ReversingValveMod
