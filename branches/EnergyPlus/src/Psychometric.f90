SUBROUTINE PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)

!-----------------------------------------------------------------------------------
!
!  Description:	
!  Given two moist air properties, calculates others
!
!  Input:
!  BaroPressure = Barometric pressure, kPa
!  Inputs/Outputs:
!  AirProp(1)=Dry bulb temeprature, C
!  AirProp(2)=Humidity ratio
!  AirProp(3)=Relative humidity
!  AirProp(4)=Enthalpy, kJ/kg
!  AirProp(5)=Wet bulb temperature, C
!  AirProp(6)=Dew point temperature, C
!  AirProp(7)=Dry air density, kg/m^3
!  AirProp(8)=Wet air density, kg/m^3
!  AirPropOpt=Air property calc. options
!         1:Given: Dry bulb temperature, C
!                  and Enthalpy, kJ/kg                
!         2:Given: Dry bulb temperature, C
!                  and Relative humidity, from 0 to 1
!         3:Given: Dry bulb temperature, C
!                  and Wet bulb temperature, C           
!         4:Given: Dry bulb temperature, C
!                  and Humidity ratio, from 0 to 1
!        *5:Given: Wet bulb temperataure, C *-not functioning
!                  and enthalpy, kJ/kg
!  AirPropErr=Error status: 1=error; 0=No error
!
!  Reference: 
!  ASHRAE Building Loads Toolkit (2000)
!
!  Author:
!  Ipseng Iu
!  Mechanical and Aerospace Engineering
!  Oklahoma State University, Stillwater	
!
!  Date: August 2002
!
!-----------------------------------------------------------------------------------

IMPLICIT NONE

!Subroutine argument declarations
REAL, INTENT(IN) :: BaroPressure
INTEGER(2), INTENT(IN) :: AirPropOpt
INTEGER(2), INTENT(OUT) :: AirPropErr
REAL, INTENT(INOUT) :: AirProp(8)

!Subroutine local vairables
INTEGER, PARAMETER :: MaxIter=20 !Maximum number of iterations
REAL, PARAMETER :: Small=1E-4 !Small number 
REAL, PARAMETER :: DT=1.0 !Delta temperature, C
REAL TDB  !Dry bulb temperature, C
REAL W    !Humidity ratio
REAL RH   !Relative humidity
REAL H    !Enthalpy, kJ/kg
REAL TWB  !Wet bulb temperature, C
REAL TDP  !Dew point temperature, C
REAL RhoD !Dry air density, kg/m^3
REAL RhoM !Moist air density, kg/m^3
INTEGER ErrStat       !Error status
!INTEGER I !Loop counter    !RS: Debugging: Extraneous
!REAL Diff !Difference  !RS: Debugging: Extraneous
!REAL TDBmax !Maximum dry bulb temp. C  !RS: Debugging: Extraneous
!REAL TDBmin !Minimum dry bulb temp. C  !RS: Debugging: Extraneous
!REAL TWBsetpt !Set point wet bulb temp. C  !RS: Debugging: Extraneous

!Flow**
  
  OPEN (77,FILE='AirProp.err')

  TDB  = AirProp(1)
  W    = AirProp(2)
  RH   = AirProp(3)
  H    = AirProp(4)*1000
  TWB  = AirProp(5)
  TDP  = AirProp(6)
  RhoD = AirProp(7)
  RhoM = AirProp(8)
  
  SELECT CASE (AirPropOpt)
  CASE (1)
	CALL TDB_H (TDB,W,RH,H,TWB,TDP,RhoD,RhoM,BaroPressure,ErrStat)
  CASE (2)
    CALL TDB_RH (TDB,W,RH,H,TWB,TDP,RhoD,RhoM,BaroPressure,ErrStat)
  CASE (3)
    CALL TDB_TWB (TDB,W,RH,H,TWB,TDP,RhoD,RhoM,BaroPressure,ErrStat)
  CASE (4)
    CALL TDB_W (TDB,W,RH,H,TWB,TDP,RhoD,RhoM,BaroPressure,ErrStat)
  !CASE (5)
  !	TWBsetpt=TWB !Assign setpoint wet bulb temperature
	
	!Initialize
!	TDBmax=100.0
!	TDBmin=0.0
!    TDB=TWBsetpt 
!	DO I=1, MaxIter
!	  CALL TDB_H (TDB,W,RH,H,TWB,TDP,RhoD,RhoM,ErrStat) 
!	  Diff=(TWB-TWBsetpt)/TWBsetpt
!	  IF (ABS(Diff) .LT. Small) THEN
!	    EXIT
!      ELSE
!        IF (DIFF .GT. 0) THEN
!		  TDBmax=TDB
!        ELSE
!		  TDBmin=TDB
!		END IF
!		TDB=(TDBmax+TDBmin)/2
!	  END IF
!	END DO
!	IF (I .GT. MaxIter) THEN
!	  WRITE(*,*)'Cannot find dry bulb temperature for'
!      WRITE(*,*)'Wet bulb =',TWBsetpt
!	  WRITE(*,*)'Enthalpy =',H
!	  TDB=TWBsetpt
!    END IF
  CASE DEFAULT
    ErrStat=1
    WRITE(77,*)'AirProp error: Invalid input data'
  END SELECT

  IF (TWB .GE. TDB) THEN
      RH = 1
  END IF

  AirProp(1)=TDB
  AirProp(2)=W    
  AirProp(3)=RH   
  AirProp(4)=H/1000
  AirProp(5)=TWB 
  AirProp(6)=TDP
  AirProp(7)=RhoD 
  AirProp(8)=RhoM

  AirPropErr=ErrStat
  IF (AirPropErr .EQ. 0) THEN
    IF (W .LT. 0 .OR. H .LT. 0 .OR. RhoD .LT. 0 .OR. RhoM .LT. 0) THEN
      WRITE(77,*)'AirProp error: Invalid input data'
	  AirPropErr=1
	END IF
  END IF
  
  CLOSE (77)

END SUBROUTINE