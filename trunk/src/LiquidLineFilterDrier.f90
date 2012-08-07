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

	FlowCapacity=PAR(1)
	RatedDP=PAR(2)

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




