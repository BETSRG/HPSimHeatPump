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
REAL, INTENT(IN) :: Tdis !Discharge temperatuer, C
REAL, INTENT(IN) :: Tsuc !Suction temperatuer, C
REAL, INTENT(IN) :: hsi !Suction inlet enthalpy, kJ/kg
REAL, INTENT(OUT) :: hso !Suction outlet enthalpy, kJ/kg

REAL :: UA !Overall heat transfer coefficient, kW/K

UA = 0.1541*mdot - 0.002
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

DP = 327.69*mdot + 3.9633

IF (DP .LT. 0) THEN
    DP = 0
END IF

END SUBROUTINE SuctionPressureDrop

!------------------------------------------------------------------------------

END MODULE ReversingValveMod