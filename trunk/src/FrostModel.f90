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

MODULE FrostModel

USE DataSimulation

IMPLICIT NONE

PUBLIC EvaluateFrostModel

CONTAINS

SUBROUTINE EvaluateFrostModel

REAL Time
REAL FrostWeightCoeff
REAL FaceArea
REAL DimLessFrostWeight
REAL FrostMaxWeight
REAL CurFrostWeight
REAL CurFrostVolume
REAL CurFrostThickness
REAL CurFrostDensity
REAL AvgFrostDensity
REAL FrostThickness
REAL CoilTemp
REAL, SAVE :: PrevFrostThickness = 0.0
REAL, SAVE :: PrevFrostDensity = 0.0
REAL,DIMENSION(2):: FrostCurve
REAL :: MaxFrostThickness

FrostCurve(1)= 1.116   !0.06877
FrostCurve(2)= 348.54607
FrostMaxWeight=2.5446

Time=CurSimTime

IF(Time==0.0) THEN
 FrostParam%Conductivity = 0.0
 FrostParam%Density = 0.0
 FrostParam%Thickness = 0.0
 PrevFrostDensity=0.0
 PrevFrostThickness=0.0
 RETURN
END IF
MaxFrostThickness=0.495/CoilParams(2)%FinPitch-0.495*CoilParams(2)%FinThickness
!InitialFrostCurveSlope=FrostCurve(1)/FrostCurve(2)
CoilTemp=(CoilParams(2)%CoilInletRefTemp+CoilParams(2)%CoilOutletRefTemp)/2
FrostWeightCoeff=FrostCurve(1)*Time/(FrostCurve(2)+Time)
DimLessFrostWeight= FrostWeightCoeff*CoilParams(2)%CoilFaceArea*10.76
CurFrostWeight=DimLessFrostWeight*FrostMaxWeight/2.54
CurFrostDensity=650*EXP(0.277*CoilTemp)
CurFrostVolume=CurFrostWeight/CurFrostDensity
!CurFrostThickness=CurFrostVolume/CoilParams(2)%CoilFinArea
FrostThickness=FrostCurve(1)*Time/(FrostCurve(2)+Time)*MaxFrostThickness  !Frost Curve-1
!FrostThickness=(Time/2500)*MaxFrostThickness  !Frost Curve-2
!FrostThickness=(4E-11*Time**3 + 5E-08*Time**2 + 2E-05*Time + 2E-14)*MaxFrostThickness    !Frost Curve-3
!FrostThickness=CurFrostThickness+PrevFrostThickness
CurFrostThickness=FrostThickness-PrevFrostThickness
AvgFrostDensity=(CurFrostDensity*CurFrostThickness &
                +PrevFrostDensity*PrevFrostThickness)/FrostThickness
FrostParam%Conductivity=(0.0121+0.00373*AvgFrostDensity+0.562)*0.001703
FrostParam%Density=AvgFrostDensity
FrostParam%Thickness=FrostThickness
PrevFrostDensity=AvgFrostDensity
PrevFrostThickness=FrostThickness

 RETURN

END SUBROUTINE EvaluateFrostModel

SUBROUTINE DetermineDefrostInitiate

REAL, SAVE :: TimeFromInitTrigger
REAL, SAVE :: SlopeInitCurve
REAL, SAVE :: InterceptInitCurve

LOGICAL, SAVE :: DefrostTrigger
LOGICAL, SAVE :: OneTimeInit = .TRUE.

REAL       :: TimeFromLastDefrost

IF(OneTimeInit) THEN
 SlopeInitCurve = (InitiateTemp(1)-InitiateTemp(2))/(AmbTemp(1)-AmbTemp(2))
 InterceptInitCurve = (InitiateTemp(1)-InitiateTemp(2))/(AmbTemp(1)-AmbTemp(2))*AmbTemp(1)+InitiateTemp(1)
 OneTimeInit = .FALSE.
END IF

DefrostSetPoint = SlopeInitCurve*TAIE + InterceptInitCurve
TimeFromLastDefrost = CurSimTime-LastDefrostInitTime
IF(DefrostControlTemp .LE. DefrostSetPoint) THEN
 IF(DefrostTrigger) THEN
     TimeFromInitTrigger = TimeFromInitTrigger + TimeInterval
 END IF
 IF(.NOT. DefrostTrigger) THEN
     DefrostTrigger = .TRUE.
 END IF
ELSE
 DefrostTrigger = .FALSE. 
END IF

IF(TimeFromInitTrigger .GE. TimeInhibit .AND. TimeFromLastDefrost .GE. DefrostInhibitTime) THEN 
  DefrostInitiate = .TRUE.
  LastDefrostInitTime = CurSimTime
END IF  

 IF(TimeStep==100) THEN
  DefrostInitiate=.TRUE.  !Remove the comment here and other changes in EPS in evaporator
 ELSE
  DefrostInitiate = .FALSE.
 END IF
  
RETURN

END SUBROUTINE DetermineDefrostInitiate

SUBROUTINE EvaluateDefrostModel

 RETURN

END SUBROUTINE EvaluateDefrostModel

END MODULE FrostModel
