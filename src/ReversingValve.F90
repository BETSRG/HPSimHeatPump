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
!PUBLIC DischargeHeatTransfer
!PUBLIC DischargePressureDrop

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
IF (UA .LT. 0) UA=0

hso=UA*(Tdis-Tsuc)/mdot+hsi

END SUBROUTINE SuctionHeatTransfer

!------------------------------------------------------------------------------

!SUBROUTINE DischargeHeatTransfer(mdot,Tdis,Tsuc,hdi,hdo)
!
!!Calculate heat gain at discharge side
!
!IMPLICIT NONE
!
!REAL, INTENT(IN) :: mdot !refrigerant mass flow rate, kg/s
!REAL, INTENT(IN) :: Tdis !Discharge temperatuer, C
!REAL, INTENT(IN) :: Tsuc !Suction temperatuer, C
!REAL, INTENT(IN) :: hdi !Discharge inlet enthalpy, kJ/kg
!REAL, INTENT(OUT) :: hdo !Discharge outlet enthalpy, kJ/kg
!
!REAL :: UA !Overall heat transfer coefficient, kW/K
!
!UA = 1.5968*mdot - 0.0156
!
!hdo=hdi-UA*(Tdis-Tsuc)/mdot
!
!END SUBROUTINE DischargeHeatTransfer

!------------------------------------------------------------------------------

SUBROUTINE SuctionPressureDrop(mdot,DP)

!Calculate suction side pressure drop

IMPLICIT NONE

REAL, INTENT(IN) :: mdot !Refrigerant mass flow rate, kg/s
REAL, INTENT(OUT) :: DP !Pressure drop kPa

DP = 327.69*mdot + 3.9633

IF (DP .LT. 0) DP = 0

END SUBROUTINE SuctionPressureDrop

!------------------------------------------------------------------------------

!SUBROUTINE DischargePressureDrop(Tonnage,mdot,rho,DP)
!
!!Calculate discharge side pressure drop
!
!IMPLICIT NONE
!
!REAL, INTENT(IN) :: Tonnage !Tonnage of system, ton
!REAL, INTENT(IN) :: mdot !Refrigerant mass flow rate, kg/s
!REAL, INTENT(IN) :: rho !Refrigerant density, kg/m3
!REAL, INTENT(OUT) :: DP !Pressure drop kPa
!
!REAL :: CPD2 !Pressure drop parameter for 2-ton valve
!REAL :: CPD6 !Pressure drop parameter for 6-ton valve
!REAL :: CPD  !Pressure drop parameter
!
!CPD2=4.1e-5*(6.895/0.0283*3600**2/0.4536) !convert to SI
!CPD6=0.5e-5*(6.895/0.0283*3600**2/0.4536) !convert to SI
!
!!Interpolate or extrapolate
!CPD=(Tonnage-2)/(6-2)*(CPD6-CPD2)+CPD2
!
!DP=mdot**2*CPD/rho
!
!END SUBROUTINE DischargePressureDrop

!------------------------------------------------------------------------------

END MODULE ReversingValveMod