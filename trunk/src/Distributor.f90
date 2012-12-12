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

SUBROUTINE Distributor(Ref$,LTUBE,Nckts,mdotRef,TiExp,HiExp,PoEvp, &
                       HoEvpRtd,Qtube,DPtot,ErrorFlag)

!To calculate total pressure drop in distributor

USE FluidProperties_HPSim !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)
USE DataGlobals_HPSim, ONLY: RefrigIndex   !RS: Debugging: Removal of plethora of RefrigIndex definitions in the code

IMPLICIT NONE

CHARACTER*80,     INTENT(IN) :: Ref$    !Refrigerant name
REAL, INTENT(IN) :: LTUBE   !Distributor tube length, in
INTEGER(2),       INTENT(IN) :: Nckts   !Number of circuits in evaporator
REAL, INTENT(IN) :: mdotRef !Refrigerant mass flow rate, kg/s
REAL, INTENT(IN) :: TiExp   !Inlet temperature of expansion device, C
REAL, INTENT(IN) :: HiExp   !Inlet enthalpy of expansion device, kJ/kg
REAL, INTENT(IN) :: PoEvp   !Oulet pressure of evaproator, kPa
REAL, INTENT(OUT) :: Qtube  !Distributor tube capacity, kW
REAL, INTENT(OUT) :: DPtot  !Total pressure drop across distributor, kPa
INTEGER, INTENT(OUT) ::ErrorFlag !Error flag: 0 = no error
                                             !1 = solution error
											 !2 = refprop error

REAL, PARAMETER :: SuperRtd = 6.11 !11 F  !Rated superheat, C
REAL, PARAMETER :: UnitP = 6.895 !(psi X UnitP = kPa)
           
!INTEGER            :: RefrigIndex =0
REAL Temperature,Quality,Pressure,Enthalpy

INTEGER(2)       :: RefPropOpt  !Ref prop calc. option
INTEGER(2)       :: RefPropErr  !Error flag:1-error; 0-no error
REAL :: RefProp(28)

REAL TiExpF   !Inlet temperature of expansion device, F
REAL TsoEvp   !Evaporator outlet saturation temperature, C
REAL TsoEvpF  !Evaporator outlet saturation temperature, F
REAL ToEvpRtd !Rated evaporator outlet temperautre, C
REAL HoEvpRtd !Rated evaporator outlet enthalpy, kJ/kg
REAL Qnoz     !Nozzle capacity, kW
REAL QnozRtd  !Nozzle rated capacity, kW
REAL QtubeRtd !Tube rated capacity, kW
REAL LoadNoz  !Nozzle loading
REAL LoadTube !Tube loading
REAL DPnoz    !Pressure drop through nozzle, kPa
REAL DPtube   !Pressure drop through tube, kPa

LOGICAL, EXTERNAL :: IssueRefPropError

!Flow:

  ErrorFlag = 0

  Pressure=PoEvp*1000   !RS Comment: Unit Conversion
  Quality=1
  TsoEvp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr) !Evaporator Outlet Saturation Temperature
  IF (IssueRefPropError(RefPropErr, 'Distributor', 2, ErrorFlag)) THEN
      RETURN
  END IF

  ToEvpRtd=TsoEvp+SuperRtd !Rated evaporator outlet temp.

  Temperature=ToEvpRtd
  Pressure=PoEvp*1000   !RS Comment: Unit Conversion
  HoEvpRtd=TP(Ref$, Temperature, Pressure, 'enthalpy', RefrigIndex,RefPropErr)  !Rated Evaporator Outlet Enthalpy
  IF (IssueRefPropError(RefPropErr, 'Distributor', 2, ErrorFlag)) THEN
      RETURN
  END IF
  HoEvpRtd=HoEvpRtd/1000    !RS Comment: Unit Conversion

  !Capacities
  Qnoz=mdotRef*(HoEvpRtd-HiExp) !Nozzle
  IF (Qnoz .LT. 0) THEN
      Qnoz=0.
  END IF
  Qtube=Qnoz/Nckts              !Tube

  TiExpF=TiExp*1.8+32   !RS Comment: Unit Conversion, from C to F
  TsoEvpF=TsoEvp*1.8+32 !RS Comment: Unit Conversion, from C to F
  
  !Rated capacities
  CALL CalcQnozRated(TiExpF,TsoEvpF,Ref$,QNOZRTD)         !Rated nozzle capacity
  CALL CalcQtubeRated(LTUBE,TiExpF,TsoEvpF,Ref$,QTUBERTD) !Rated tube capacity
  QnozRtd=QNOZRTD*3.517   !Convert ton to kW
  QtubeRtd=QTUBERTD*3.517 !Convert ton to kW

  !Loadings
  LoadNoz=Qnoz/QnozRtd    !Nozzle
  LoadTube=Qtube/QtubeRtd !Tube

  IF (LoadNoz .GT. 1) THEN
      LoadNoz=1
  END IF
  IF (LoadTube .GT. 1) THEN
      LoadTube=1
  END IF

  !Pressure drops
  CALL CalcDPnoz(LoadNoz,Ref$,DPNOZ)    !Nozzle pressure drop
  CALL CalcDPtube(LoadTube,Ref$,DPTUBE) !Tube pressure
  DPnoz=DPNOZ*UnitP
  DPtube=DPTUBE*UnitP

  DPtot=DPnoz+DPtube    !Total Distributor Pressure Drop

  IF (DPtot .LT. 0) THEN
      ErrorFlag = 3
  END IF

  RETURN

END SUBROUTINE

!***********************************************************************************

SUBROUTINE CalcDPnoz(Load,Ref$,DPNOZ)

!To calculate pressure drop through nozzle

IMPLICIT NONE

CHARACTER*80, INTENT(IN) :: Ref$ !Refrigerant name
REAL, INTENT(IN) :: Load !Loading
REAL, INTENT(OUT) :: DPNOZ !Pressure drop through nozzle, psi

  SELECT CASE (TRIM(Ref$))

  CASE ("R12")
      IF (Load .NE. 1.1) THEN
        DPNOZ=15*Load**1.817
      ELSE
        DPNOZ=15.81*Load**1.265
      END IF
  CASE ("R134A")
      DPNOZ = 0.0175*Load**2 + 22.942*Load - 7.8955
  CASE ("R22")
      IF (Load .NE. 1.2) THEN
        DPNOZ=25*Load**1.838
      Else
        DPNOZ=29.4*Load**0.9547
      END IF
  CASE ("R407C")
	  DPNOZ = -6.5301*Load**2 + 50.928*Load - 18.632  
  CASE ("R410A")
      IF (Load .EQ. 1.0) THEN
	      DPNOZ=25*Load**1.838 !ISI - 07/14/06
      ELSE
	      DPNOZ=29.4*Load**0.9547
      END IF
  CASE DEFAULT !R22
      IF (Load .NE. 1.1) THEN
        DPNOZ=15*Load**1.817
      ELSE
        DPNOZ=15.81*Load**1.265
      END IF
  END SELECT

  RETURN

END SUBROUTINE

!***********************************************************************************

SUBROUTINE CalcDPtube(Load,Ref$,DPTUBE)

!To calculate pressure drop drop distributor tube

IMPLICIT NONE

CHARACTER*80, INTENT(IN) :: Ref$ !Refrigerant name
REAL, INTENT(IN) :: Load !Loading
REAL, INTENT(OUT) :: DPTUBE !Tube pressure drop, psi

  SELECT CASE (TRIM(Ref$))

  CASE ("R12")
      IF (Load .NE. 1.6) THEN
          DPTUBE=10*Load**1.772
      ELSE
          DPTUBE=12.265*Load**1.3377
      END IF
  CASE ("R134A")
      DPTUBE = 3.4314*Load**2 + 11.098*Load - 4.2132
  CASE ("R22")
      DPTUBE=10*Load**1.8122
  CASE ("R407C")
	  DPTUBE = 6.6527*Load**2 + 5.1331*Load - 1.7248  
  CASE ("R410A")
	  DPTUBE=10*Load**1.8122
  CASE DEFAULT !R22
      IF (Load .NE. 1.6) THEN
          DPTUBE=10*Load**1.772
      ELSE
          DPTUBE=12.265*Load**1.3377
      END IF
  END SELECT

  RETURN

END SUBROUTINE

!***********************************************************************************

SUBROUTINE CalcQnozRated(TITXV,TSOEVP,Ref$,QNOZRTD)

!To calculate the rated nozzle capacity for Nozzle # 3
!Reference: Sporlan Bulletin 20-10

IMPLICIT NONE

!Subroutine arguments
CHARACTER*80, INTENT(IN) :: Ref$ !Refrigerant name
REAL, INTENT(IN) :: TITXV !Ref. liqui temperature, F
REAL, INTENT(IN) :: TSOEVP !Saturated oulet evaporator temperature, F
REAL, INTENT(OUT) :: QNOZRTD !Rated nozzle capacity, ton

!Local variable
REAL a,b !Equation fit constants
REAL CFnoz !Correction factor for liquid temp. other then 100 F

  SELECT CASE (TRIM(Ref$))

  CASE ("R22")
      !a = 2.971341163
      !b = 228.570796
	  !a=3.562877741
	  !b=230.1924712
      QNOZRTD = 0.0002*TSOEVP**2 + 0.025*TSOEVP + 2.3157
  CASE ("R407C")
      !a = 2.349772583
      !b = 225.0518653
	  !a=2.813484528
	  !b=225.153044
	  QNOZRTD = 0.0002*TSOEVP**2 + 0.02*TSOEVP + 1.8149
  CASE ("R410A")
      !a = 3.5727421
      !b = 219.3375409
	  !a=4.288474872	
	  !b=219.7987884
	  QNOZRTD = 0.0002*TSOEVP**2 + 0.0308*TSOEVP + 2.7557
  CASE ("R134A")
      !a = 1.749168694
      !b = 194.9632043
	  !a=2.107814156	
	  !b=192.5209776
	  QNOZRTD = 0.0002*TSOEVP**2 + 0.0146*TSOEVP + 1.2725
  CASE DEFAULT !R22
	  !a=3.562877741
	  !b=230.1924712
      QNOZRTD = 0.0002*TSOEVP**2 + 0.025*TSOEVP + 2.3157
  END SELECT

  IF (TITXV .GT. 40) THEN
      CFnoz=0.0001*TITXV**2 - 0.0394*TITXV + 3.7791
  END IF

  QNOZRTD = CFnoz*QNOZRTD

  RETURN

END SUBROUTINE

!***********************************************************************************

SUBROUTINE CalcQtubeRated(LTUBE,TITXV,TSOEVP,Ref$,QTUBERTD)

!To calculate the rated tube capacity for tube OD = 1/4"
!Reference: Sporlan bulletin 20-10

IMPLICIT NONE

!Subroutine arguments
CHARACTER*80, INTENT(IN) :: Ref$ !Refrigerant name
REAL, INTENT(IN) :: LTUBE !Distributor tube length, in
REAL, INTENT(IN) :: TITXV !Ref. liqui temperature, F
REAL, INTENT(IN) :: TSOEVP !Saturated oulet evaporator temperature, F
REAL, INTENT(OUT) :: QTUBERTD !Rated tube capacity, ton

!Local variable
REAL a,b !Equation fit constants
REAL CFtube !Correction factor for tube length other than 30"
REAL CFnoz !Correction factor for liquid temp. other then 100 F

  SELECT CASE (TRIM(Ref$))

  CASE ("R22")
      !a = 1.131323846
      !b = 174.815541
      QTUBERTD = 7E-05*TSOEVP**2 + 0.0092*TSOEVP + 0.6574
  CASE ("R407C")
      !a = 0.894312279
      !b = 171.577027
	  QTUBERTD = 6E-05*TSOEVP**2 + 0.0074*TSOEVP + 0.5134
  CASE ("R410A")
      !a = 1.19503552
      !b = 166.7218097
	  QTUBERTD = 8E-05*TSOEVP**2 + 0.01*TSOEVP + 0.6791
  CASE ("R134A")  
      !a = 0.838211512
      !b =	161.3728006
      QTUBERTD = 7E-05*TSOEVP**2 + 0.0066*TSOEVP + 0.464
  CASE DEFAULT !R22
      !a = 1.131323846
      !b = 174.815541
	  QTUBERTD = 7E-05*TSOEVP**2 + 0.0092*TSOEVP + 0.6574
  END SELECT

  IF (TITXV .GT. 40) THEN
      CFnoz=0.0001*TITXV**2 - 0.0394*TITXV + 3.7791
  END IF

  CFtube=(30/LTUBE)**0.333

  QTUBERTD = CFtube*CFnoz*QTUBERTD

  RETURN

END SUBROUTINE

!***********************************************************************************


