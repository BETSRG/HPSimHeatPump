SUBROUTINE PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)

!-----------------------------------------------------------------------------------
!
!  Description:	
!  Given two moist air properties, calculates others
!
!  Input:
!  BaroPressure = Barometric pressure, kPa
!  Inputs/Outputs:
!  AirProp(1)=Dry bulb temperature, C
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
USE DataSimulation, ONLY: APTDB, APHumRat, APRelHum, APEnth, APTWB, APTDP, APDryDens, APWetDens

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

!Flow**
  
  OPEN (77,FILE='AirProp.err')

  TDB  = AirProp(APTDB) !RS: Debugging: Formerly AirProp(1)
  W    = AirProp(APHumRat)  !RS: Debugging: Formerly AirProp(2)
  RH   = AirProp(APRelHum)  !RS: Debugging: Formerly AirProp(3)
  H    = AirProp(APEnth)*1000   !RS: Debugging: Formerly AirProp(4)
  TWB  = AirProp(APTWB) !RS: Debugging: Formerly AirProp(5)
  TDP  = AirProp(APTDP) !RS: Debugging: Formerly AirProp(6)
  RhoD = AirProp(APDryDens) !RS: Debugging: Formerly AirProp(7)
  RhoM = AirProp(APWetDens) !RS: Debugging: Formerly AirProp(8)
  
  SELECT CASE (AirPropOpt)
  CASE (1)
	CALL TDB_H (TDB,W,RH,H,TWB,TDP,RhoD,RhoM,BaroPressure,ErrStat)
  CASE (2)
    CALL TDB_RH (TDB,W,RH,H,TWB,TDP,RhoD,RhoM,BaroPressure,ErrStat)
  CASE (3)
    CALL TDB_TWB (TDB,W,RH,H,TWB,TDP,RhoD,RhoM,BaroPressure,ErrStat)
  CASE (4)
    CALL TDB_W (TDB,W,RH,H,TWB,TDP,RhoD,RhoM,BaroPressure,ErrStat)
  CASE DEFAULT
    ErrStat=1
    WRITE(77,*)'AirProp error: Invalid input data'
  END SELECT

  IF (TWB .GE. TDB) THEN
      RH = 1
  END IF

  AirProp(APTDB)=TDB    !RS: Debugging: Formerly AirProp(1)
  AirProp(APHumRat)=W      !RS: Debugging: Formerly AirProp(2)
  AirProp(APRelHum)=RH     !RS: Debugging: Formerly AirProp(3)
  AirProp(APEnth)=H/1000 !RS: Debugging: Formerly AirProp(4)
  AirProp(APTWB)=TWB    !RS: Debugging: Formerly AirProp(5)
  AirProp(APTDP)=TDP    !RS: Debugging: Formerly AirProp(6)
  AirProp(APDryDens)=RhoD   !RS: Debugging: Formerly AirProp(7)
  AirProp(APWetDens)=RhoM   !RS: Debugging: Formerly AirProp(8)

  AirPropErr=ErrStat
  IF (AirPropErr .EQ. 0) THEN
    IF (W .LT. 0 .OR. H .LT. 0 .OR. RhoD .LT. 0 .OR. RhoM .LT. 0) THEN
      WRITE(77,*)'AirProp error: Invalid input data'
	  AirPropErr=1
	END IF
  END IF
  
  CLOSE (77)

END SUBROUTINE
