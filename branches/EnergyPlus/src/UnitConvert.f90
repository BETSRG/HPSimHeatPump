MODULE UnitConvertMod

IMPLICIT NONE

PRIVATE
REAL, PARAMETER :: UnitPwr   = 0.2927E-3 !(Btu/hr X UnitPwr = KW)
REAL, PARAMETER :: UnitArFlw = 0.472E-3  !(CFM X UnitArFlw = m^3/s)
REAL, PARAMETER :: UnitRfFlw = 0.4536    !(Lbm/hr X UnitRfFlw = kg/hr)
REAL, PARAMETER :: UnitP     = 6.8947453 !(psi X UnitP = kPa)
REAL, PARAMETER :: UnitM     = 0.4536    !(lbm X UnitM = kg)
REAL, PARAMETER :: UnitL     = 0.3048    !(ft X UnitL = m)
REAL, PARAMETER :: UnitK     = 0.1442E-3 !(Btu-in/hr-ft2-F X UnitK = kW/m-C)

!Unit flags !ISI - 07/14/06
INTEGER,PARAMETER :: SI=1
INTEGER,PARAMETER :: IP=2

PUBLIC UnitConvert
PUBLIC MicroChannelCoilUnitConvert
PUBLIC FinTubeCoilUnitConvert

PUBLIC Temperature_F2C  ! VL
PUBLIC Temperature_C2F  ! VL

CONTAINS

!***********************************************************************************

SUBROUTINE UnitConvert(Unit,CompPAR,CondPAR,EvapPAR,ShTbPAR,CapTubePAR,TxvPAR,  &
                       AccumPAR,FilterPAR,XMaC,XMaE,TaiC,TaiE,TWiC,TWiE, &
				       Refchg,TSOCMP,TSICMP,SUPER,SUBCOOL,BaroPressure, &
					   ChargeCurveSlope,ChargeCurveIntercept,RefLiquidLength, &
					   Tdis,Tliq)
! ----------------------------------------------------------------------
!
!   Description: To convert all input data into standard SI
!                Some data needs to be in IP unit for the
!                Corresponding model
!
!   Author:
!   Ipseng Iu
!   Mechanical and Aerospace Engineering
!   Oklahoma State University, Stillwater
!
!   Date:
!   December, 2002
!
! ----------------------------------------------------------------------

IMPLICIT NONE

!Subroutine parameters

!Subroutine arguments
INTEGER(2), INTENT(IN) :: Unit !Unit flag: 1=SI; 2=IP
REAL, INTENT(INOUT) :: CompPAR(26) !Compressor model input data
REAL, INTENT(INOUT) :: CondPAR(61) !Condenser model real number input data
REAL, INTENT(INOUT) :: EvapPAR(53) !Evaporator model real number input data
REAL, INTENT(INOUT) :: ShTbPAR(5)  !Short tube model input data
REAL, INTENT(INOUT) :: CapTubePAR(5) !Capillary tube model input data
REAL, INTENT(INOUT) :: TxvPAR(7)   !TXV model input data
REAL, INTENT(INOUT) :: AccumPAR(10) !Accumulator input data
REAL, INTENT(INOUT) :: FilterPAR(2) !Filter drier input data
REAL, INTENT(INOUT) :: XMaC      !Condenser inlet air flow rate, kg/s
REAL, INTENT(INOUT) :: XMaE      !Evaporator inlet air flow rate, kg/s
REAL, INTENT(INOUT) :: TaiC      !Condenser inlet air DB temp, F
REAL, INTENT(INOUT) :: TaiE      !Evaporator inlet air DB temp, F
REAL, INTENT(INOUT) :: TWiC      !Condenser inlet air WB temp, F
REAL, INTENT(INOUT) :: TWiE      !Evaporator inlet air WB temp, F
REAL, INTENT(INOUT) :: RefChg    !Refrigerant charge, Lbm
REAL, INTENT(INOUT) :: TSOCMP    !High side saturation temp., F
REAL, INTENT(INOUT) :: TSICMP    !Low side saturation temp., F
REAL, INTENT(INOUT) :: SUPER     !Superheat, F
REAL, INTENT(INOUT) :: SUBCOOL   !Subcooling, F
REAL, INTENT(INOUT) :: BaroPressure !Barometric pressure, kPa
REAL, INTENT(INOUT) :: ChargeCurveSlope !Charge curve slope, kg/m
REAL, INTENT(INOUT) :: ChargeCurveIntercept !Charge curve intercept, kg
REAL, INTENT(INOUT) :: RefLiquidLength !Liquid length at reference point, m
REAL, INTENT(INOUT) ::  Tdis !Discharge temperature, C
REAL, INTENT(INOUT) ::  Tliq !Liquid temperature, C

!Local variables
INTEGER I !Loop counter

  IF (Unit .EQ. SI)THEN !SI unit inputs
    
	CompPAR(23)=CompPAR(23)/(100**3) !Compressor internal volume, m^3

	!****Condenser input data****
    !CondPAR(1)                   !Discharge line length, m
	CondPAR(2)=CondPAR(2)/1000   !Discharge line outside diameter, m
	CondPAR(3)=CondPAR(3)/1000   !Discharge line tube wall thickness, m
	!CondPAR(4)                   !Discharge line elevation, m
	CondPAR(5)=CondPAR(5)/1000   !Discharge line heat loss, kW
	!CondPAR(6)                   !Discharge line temperature drop, C
	!CondPAR(7)                   !Discharge line addiational pressure drop, kPa
	!CondPAR(8)                   !Liquid line length, m
	CondPAR(9)=CondPAR(9)/1000   !Liquid line outside diameter, m
	CondPAR(10)=CondPAR(10)/1000   !Liquid line tube wall thickness, m
	!CondPAR(11)                  !Liquid line elevation, m
	CondPAR(12)=CondPAR(12)/1000 !Liquid line heat loss, kW
	!CondPAR(13)                  !Liquid line temperature drop, C
	!CondPAR(14)                  !Liquid line additional pressure drop, kPa
	CondPAR(15)=CondPAR(15)/1000   !Tube outside diameter, m
    CondPAR(16)=CondPAR(16)/1000   !Tube wall thickness, m
    !CondPAR(17)                   !Tube length, m 
    !CondPAR(18)                   !Tube thermal conductivity, kW/m-C 
    CondPAR(19)=CondPAR(19)/1000   !Tube spacing in transverse direction (normal to air flow), m
    CondPAR(20)=CondPAR(20)/1000   !Tube spacing in longitudinal direction (parallel to air flow), m
    CondPAR(21)=CondPAR(21)/1000   !Fin thickness, m
    !CondPAR(22)                   !Fin pitch, fin/m 
    !CondPAR(23)                   !Fin thermal conductivity, kW/m-C 
    !CondPAR(24)                   !Number of tubes in transverse direction (normal to air flow) 
    !CondPAR(25)                   !Number of tubes in longitudinal direction (parallel to air flow) 
    !CondPAR(26)                   !Number of circuits 
    !CondPAR(27)                   !Equivalent circuits (1=yes; 2=no) 
    !CondPAR(28)                   !Number of modules per tube 
    !CondPAR(29)                   !Fin type (1-smooth; 2-Wavy; 3-louvered) 
	CondPAR(34)=CondPAR(34)/1000   !Fan power, kW

	!CondPAR(38)                   !Barometric pressure, kPa
    !CondPAR(58)                   !Distributor tube length, m

	!****Evaporator input data****
	!EvapPAR(1)                   !Suction line length, m
	EvapPAR(2)=EvapPAR(2)/1000   !Suction line outside diameter, m
	EvapPAR(3)=EvapPAR(3)/1000   !Suction line tube wall thickness, m
	!EvapPAR(4)                   !Suction line elevation, m
	EvapPAR(5)=EvapPAR(5)/1000   !Suction line heat gain, kW
	!EvapPAR(6)                   !Suction line temperature rise, C
	!EvapPAR(7)                   !Suction line additional pressure drop, kPa
    EvapPAR(8)=EvapPAR(8)/1000   !Tube outside diameter, m
    EvapPAR(9)=EvapPAR(9)/1000   !Tube wall thickness, m
    !EvapPAR(10)                  !Tube length, m 
    !EvapPAR(11)                 !Tube thermal conductivity, kW/m-C 
    EvapPAR(12)=EvapPAR(12)/1000 !Tube spacing in transverse direction (normal to air flow), m
    EvapPAR(13)=EvapPAR(13)/1000 !Tube spacing in longitudinal direction (parallel to air flow), m
    EvapPAR(14)=EvapPAR(14)/1000 !Fin thickness, m
    !EvapPAR(15)                 !Fin pitch, fin/m 
    !EvapPAR(16)                 !Fin thermal conductivity, kW/m-C 
    !EvapPAR(17)                 !Number of tubes in transverse direction (normal to air flow) 
    !EvapPAR(18)                 !Number of tubes in longitudinal direction (parallel to air flow) 
    !EvapPAR(19)                 !Number of circuits 
    !EvapPAR(20)                 !Equivalent circuits (1=yes; 2=no) 
    !EvapPAR(21)                 !Number of modules per tube 
    !EvapPAR(22)                 !Fin type (1-smooth; 2-Wavy; 3-louvered) 
	EvapPAR(27)=EvapPAR(27)/1000 !Fan power, kW

	!EvapPAR(31)                   !Barometric pressure, kPa

    !****Short tube input data****
    ShTbPAR(1)=ShTbPAR(1)/1000 !Length, m 
    ShTbPAR(2)=ShTbPAR(2)/1000 !Diameter, m
    ShTbPAR(3)=ShTbPAR(3)/1000 !45 deg chamfer depth, m
    !ShTbPAR(4) !Number of circuits
	!ShTbPAR(5) !Distributor tube length, m

    !****Cap. tube input data****
    CapTubePAR(1)=CapTubePAR(1)/1000 !Length, m 
    CapTubePAR(2)=CapTubePAR(2)/1000 !Diameter, m
    CapTubePAR(3)=CapTubePAR(3)/1000 !Coil diameter, m
    !CapTubePAR(4) !Number of circuits
	!CapTubePAR(5) !Distributor tube length, m

    !****TXV input data****
    !TxvPAR(1) !Rated TXV capacity, ton
    !TxvPAR(2) !Rated superheat, C
    !TxvPAR(3) !Static superheat, C
    !TxvPAR(4) !Bleed factor
    !TxvPAR(5) !Number of circuits in evaporator
	!TxvPAR(6) !Distributor tube length, m
	!TxvPAR(7) !Maximum effective superheat, C

	!***** Accumulator input data *****
	AccumPAR(1) = AccumPAR(1)/1000 !Accumulator inside diameter, m
	AccumPAR(2) = AccumPAR(2)/1000 !Accumulator internal height, m
	AccumPAR(3) = AccumPAR(3)/1000 !J-tube lower hole diameter, m
	AccumPAR(4) = AccumPAR(4)/1000 !J-tube upper hole diameter, m
	AccumPAR(5) = AccumPAR(5)/1000 !Distance between holes on J-tube, m
	AccumPAR(6) = AccumPAR(6)/1000 !J-tube inside diameter, m
	!AccumPAR(7) = Rating pressure drop, kPa
	!AccumPAR(8) = Rating temperature drop, K
	!AccumPAR(9) = Curve fit coefficient M
	!AccumPAR(10) = Curve fit coefficient B

	!Filter drier input data
	!FilterPAR(1)=Flow capacity, ton
	!FilterPAR(2)=Rating pressure drop, kPa

    !****Refrigerant input data****
    RefChg=RefChg/UnitM !Refrigerant charge lbm, ORNL solver uses IP unit

    !Air side boundary conditions
    !XMaC             !Condenser inlet air flow rate, kg/s 
    TaiC= TaiC*1.8+32 !Condenser inlet DB temperature F, ORNL solver uses IP unit
    !TWiC             !Condenser inlet WB temperature 
    !XMaE             !Evaporator inlet air flow rate, kg/s 
    TaiE=TaiE*1.8+32  !Evaporator inlet DB temperature F, ORNL solver uses IP unit
    !TWiE             !Evaporator inlet WB temperature F

    !Initial guesses
    TsoCmp=TsoCmp*1.8+32 !High side saturation temp. F, ORNL solver uses IP unit
    TsiCmp=TsiCmp*1.8+32 !Low side saturation temp. F, ORNL solver uses IP unit
    Super=Super*1.8      !Superheat, F, ORNL solver uses IP unit
	Subcool=Subcool*1.8  !Subcooling, F, ORNL solver uses IP unit
	!BaroPressure !barometric pressure kPa

  ELSE !IP unit inputs
  
    !****Compressor input data****
    !CompPAR(1-10) !10 coefficients for power - DO nothing
  
    !10 coefficients for mass flow rate
    !CompPAR(11-20) !DO - nothing
      
    !Compressor shell heat loss
	CompPAR(22)=CompPAR(22)*UnitPwr*1000 !Compressor shell heat loss W
   
	CompPAR(23)=CompPAR(23)/(12**3)*(UnitL**3) !Compressor internal volume, m^3

	!****Condenser input data****
	CondPAR(1)=CondPAR(1)*UnitL            !Discharge line length, m
	CondPAR(2)=CondPAR(2)/12*UnitL         !Discharge line outside diameter, m
	CondPAR(3)=CondPAR(3)*0.001/12*UnitL   !Discharge line tube wall thickness, m
	CondPAR(4)=CondPAR(4)*UnitL            !Discharge line elevation, m
	CondPAR(5)=CondPAR(5)*UnitPwr          !Discharge line heat loss, kW
	CondPAR(6)=CondPAR(6)/1.8              !Discharge line temperature drop, C
	CondPAR(7)=CondPAR(7)*UnitP            !Discharge line additional pressure drop, kPa
	CondPAR(8)=CondPAR(8)*UnitL          !Liquid line length, m
	CondPAR(9)=CondPAR(9)/12*UnitL       !Liquid line outside diameter, m
	CondPAR(10)=CondPAR(10)*0.001/12*UnitL !Liquid line tube wall thickness, m
	CondPAR(11)=CondPAR(11)*UnitL          !Liquid line elevation, m
	CondPAR(12)=CondPAR(12)*UnitPwr        !Liquid line heat loss, kW
	CondPAR(13)=CondPAR(13)/1.8            !Liquid line temperature drop, C
    CondPAR(14)=CondPAR(14)*UnitP          !Liquid line additional pressure drop, kPa
	CondPAR(15)=CondPAR(15)/12*UnitL       !Tube outside diameter, m
    CondPAR(16)=CondPAR(16)*0.001/12*UnitL !Tube wall thickness, m
    CondPAR(17)=CondPAR(17)/12*UnitL       !Tube length, m
    CondPAR(18)=CondPAR(18)*UnitK          !Tube thermal conductivity, kW/m-C
    CondPAR(19)=CondPAR(19)/12*UnitL       !Tube spacing in transverse direction (normal to air flow), m
    CondPAR(20)=CondPAR(20)/12*UnitL       !Tube spacing in longitudinal direction (parallel to air flow), m
    CondPAR(21)=CondPAR(21)*0.001/12*UnitL !Fin thickness, m
    CondPAR(22)=CondPAR(22)*12/UnitL       !Fin pitch, fin/m
    CondPAR(23)=CondPAR(23)*UnitK          !Fin thermal conductivity, kW/m-C 
    !CondPAR(24)                           !Number of tubes in transverse direction (normal to air flow) 
    !CondPAR(25)                           !Number of tubes in longitudinal direction (parallel to air flow) 
    !CondPAR(26)                           !Number of circuits 
    !CondPAR(27)                           !Equivalent circuits (1=yes; 2=no) 
    !CondPAR(28)                           !Number of modules per tube 
    !CondPAR(29)                           !Fin type (1-smooth; 2-Wavy; 3-louvered) 
	CondPAR(34)=CondPAR(34)*1E-3           !Fan power, kW

	CondPAR(38)=CondPAR(38)*UnitP          !Barometric pressure, kPa
	CondPAR(58)=CondPAR(58)*UnitL          !Distributor tube length, m

	!****Evaporator input data****
	EvapPAR(1)=EvapPAR(1)*UnitL            !Suction line length, m
	EvapPAR(2)=EvapPAR(2)/12*UnitL         !Suction line outside diameter, m
	EvapPAR(3)=EvapPAR(3)*0.001/12*UnitL   !Suction line tube wall thickness, m
    EvapPAR(4)=EvapPAR(4)*UnitL            !Suction line elevation, m
	EvapPAR(5)=EvapPAR(5)*UnitPwr          !Suction line heat gain, kW
	EvapPAR(6)=EvapPAR(6)/1.8              !Suction line temperature rise, C
	EvapPAR(7)=EvapPAR(7)*UnitP            !Suction line additional pressure drop, kPa
    EvapPAR(8)=EvapPAR(8)/12*UnitL         !Tube outside diameter, m
    EvapPAR(9)=EvapPAR(9)*0.001/12*UnitL   !Tube wall thickness, m
    EvapPAR(10)=EvapPAR(10)/12*UnitL       !Tube length, m
    EvapPAR(11)=EvapPAR(11)*UnitK          !Tube thermal conductivity, kW/m-C
    EvapPAR(12)=EvapPAR(12)/12*UnitL       !Tube spacing in transverse direction (normal to air flow), m
    EvapPAR(13)=EvapPAR(13)/12*UnitL       !Tube spacing in longitudinal direction (parallel to air flow), m
    EvapPAR(14)=EvapPAR(14)*0.001/12*UnitL !Fin thickness, m
    EvapPAR(15)=EvapPAR(15)*12/UnitL       !Fin pitch, fin/m
    EvapPAR(16)=EvapPAR(16)*UnitK          !Fin thermal conductivity, kW/m-C 
    !EvapPAR(17)                           !Number of tubes in transverse direction (normal to air flow) 
    !EvapPAR(18)                           !Number of tubes in longitudinal direction (parallel to air flow) 
    !EvapPAR(19)                           !Number of circuits 
    !EvapPAR(20)                           !Equivalent circuits (1=yes; 2=no) 
    !EvapPAR(21)                           !Number of modules per tube 
    !EvapPAR(22)                           !Fin type (1-smooth; 2-Wavy; 3-louvered) 
    EvapPAR(27)=EvapPAR(27)*1E-3           !Fan power, kW

	EvapPAR(31)=EvapPAR(31)*UnitP          !Barometric pressure, kPa

    !****Short tube input data****
    ShTbPAR(1)=ShTbPAR(1)/12*UnitL !Length, m 
    ShTbPAR(2)=ShTbPAR(2)/12*UnitL !Diameter, m
    ShTbPAR(3)=ShTbPAR(3)/12*UnitL !45 deg chamfer depth, m
    !ShTbPAR(4) !Number of circuits
	ShTbPAR(5)=ShTbPAR(5)*UnitL !Distributor tube length, m

    !****Cap. tube input data****
    CapTubePAR(1)=CapTubePAR(1)/12*UnitL !Length, m 
    CapTubePAR(2)=CapTubePAR(2)/12*UnitL !Diameter, m
    CapTubePAR(3)=CapTubePAR(3)/12*UnitL !Coil diameter, m
    !CapTubePAR(4) !Number of circuits
	CapTubePAR(5)=CapTubePAR(5)*UnitL !Distributor tube length, m

    !****TXV input data****
    !TxvPAR(1)                !Rated TXV capacity, ton
    TxvPAR(2)=TxvPAR(2)/1.8   !Rated superheat, C
    TxvPAR(3)=TxvPAR(3)/1.8   !Static superheat, C
    !TxvPAR(4)                !Bleed factor
    !TxvPAR(5)                !Number of circuits in evaporator
	TxvPAR(6)=TxvPAR(6)*UnitL !Distributor tube length, m
	TxvPAR(7)=TxvPAR(7)/1.8   !Maximum effective superheat, C

	!***** Accumulator input data *****
	AccumPAR(1) = AccumPAR(1)/12*0.3048 !Accumulator inside diameter, m
	AccumPAR(2) = AccumPAR(2)/12*0.3048 !Accumulator internal height, m
	AccumPAR(3) = AccumPAR(3)/12*0.3048 !J-tube lower hole diameter, m
	AccumPAR(4) = AccumPAR(4)/12*0.3048 !J-tube upper hole diameter, m
	AccumPAR(5) = AccumPAR(5)/12*0.3048 !Distance between holes on J-tube, m
	AccumPAR(6) = AccumPAR(6)/12*0.3048 !J-tube inside diameter, m
	AccumPAR(7) = AccumPAR(7)*UnitP !Rating pressure drop, kPa
	AccumPAR(8) = AccumPAR(8)/1.8 !Rating temperature drop, K
	!AccumPAR(9) = Curve fit coefficient M
	!AccumPAR(10) = Curve fit coefficient B

	!***** Filter drier input data *****
	!FilterPAR(1)=Flow capacity, ton
	FilterPAR(2)=FilterPAR(2)*UnitP !Rating pressure drop, kPa

    !****Refrigerant input data****
    !RefChg !Refrigerant charge lbm, , ORNL solver uses IP unit

    !Air side boundary conditions
	XMaC=XMaC*UnitArFlw                 !Condenser inlet air flow rate, m^3/s
    !TaiC                               !Condenser inlet DB temperature F,  ORNL solver uses IP unit
    !IF (TWiC .GT. 1) THEN
    !    TWiC=(TWiC-32)/1.8 !Condenser inlet WB temp    !RS: Debugging: The temps are already in C; they need to be in F
    !END IF
    XMaE=XMaE*UnitArFlw                 !Evaporator inlet air flow rate, m^3/s
    !TaiE                               !Evaporator inlet DB temperature F,  ORNL solver uses IP unit
    !IF (TWiE .GT. 1) THEN
    !    TWiE=(TWiE-32)/1.8 !Evaporator inlet WB temp   !RS: Debugging: The temps are already in C; they need to be in F
    !END IF
    TaiC=TaiC*1.8+32    !Converting Outdoor Drybulb Temp from C to F
    !TWiC=TWiC*1.8+32    !Converting Outdoor Wetbulb Temp from C to F   !RS: Debugging: Apparently, these are used just in C?
    TaiE=TaiE*1.8+32    !Converting Indoor Drybulb Temp from C to F
    !TWiE=TWiE*1.8+32    !Converting Indoor Wetbulb Temp from C to F    !RS: Debugging: Apparently, these are used just in C?
    !
    !Initial guesses
    !TsoCmp    !Condenser temp. F,  ORNL solver uses IP unit
    !TsiCmp    !Evaparator temp. F,  ORNL solver uses IP unit
    !Super !Superheat, F,  ORNL solver uses IP unit
	!Subcool !Subcooling, F,  ORNL solver uses IP unit
	BaroPressure=BaroPressure*UnitP !barometric pressure kPa

	ChargeCurveSlope=ChargeCurveSlope*UnitM/UnitL
	ChargeCurveIntercept=ChargeCurveIntercept*UnitM
	RefLiquidLength=RefLiquidLength*UnitL
	
	Tdis=(Tdis-32)/1.8
	Tliq=(Tliq-32)/1.8

  END IF

  RETURN

END SUBROUTINE UnitConvert

!***********************************************************************************

SUBROUTINE MicroChannelCoilUnitConvert(IsSIUnit,FinPitch,Kfin,FinThk, &
								       TubeHeight,TubeDepth,TubeThk,Ktube, &
									   Pt,Ltube,Dchannel)

! ----------------------------------------------------------------------
!
!   Description: To convert microchannel coil input data into standard SI
!                Some data needs to be in IP unit for the
!                Corresponding model
!
!   Author:
!   Ipseng Iu
!   Mechanical and Aerospace Engineering
!   Oklahoma State University, Stillwater
!
!   Date:
!   November, 2005
!
! ----------------------------------------------------------------------

IMPLICIT NONE

LOGICAL,INTENT(IN)    :: IsSIunit    !SI unit flag
REAL,   INTENT(INOUT) :: FinPitch    !Fin pitch, fins/m
REAL,   INTENT(INOUT) :: Kfin        !Fin conductivity, kW/m-C 
REAL,   INTENT(INOUT) :: FinThk      !Fin thickness, m
REAL,   INTENT(INOUT) :: TubeHeight  !Tube height, m
REAL,   INTENT(INOUT) :: TubeDepth   !Tube depth, m
REAL,   INTENT(INOUT) :: TubeThk     !Tube thickness, m
REAL,   INTENT(INOUT) :: Ktube       !Tube conductivity, kW/m-C
REAL,   INTENT(INOUT) :: Pt          !Tube spacing, m
REAL,   INTENT(INOUT) :: Ltube       !Single tube length, m
REAL,   INTENT(INOUT) :: Dchannel    !Channel diameter, m

!FLOW:

  IF (IsSIunit) THEN !SI unit inputs
	  !FinPitch
	  Kfin       =Kfin*1e-3       !W/m-C to kW/m-C
	  FinThk     =FinThk*1e-3     !mm to m
	  TubeHeight =TubeHeight*1e-3 !mm to m
	  TubeDepth  =TubeDepth*1e-3  !mm to m
	  TubeThk    =TubeThk*1e-3    !mm to m
	  Ktube      =Ktube*1e-3      !W/m-C to kW/m-C
	  Pt         =Pt*1e-3         !mm to m
	  Ltube      =Ltube*1e-3      !mm to m
	  Dchannel   =Dchannel*1e-2   !mm to m
  ELSE
	  FinPitch   =FinPitch*12/UnitL     !fins/in to fins/m
	  Kfin       =Kfin*UnitK            !Btu-in/hr-ft2-F to kW/m-C
	  FinThk     =FinThk*0.001/12*UnitL !mil to m
	  TubeHeight =TubeHeight/12*UnitL   !in to m
	  TubeDepth  =TubeDepth/12*UnitL    !in to m
	  TubeThk    =TubeThk/12*UnitL      !in to m
	  Ktube      =Ktube*UnitK           !Btu-in/hr-ft2-F to kW/m-C 
	  Pt         =Pt/12*UnitL           !in to m
	  Ltube      =Ltube/12*UnitL        !in to m 
	  Dchannel   =Dchannel/12*UnitL     !in to m
  END IF

RETURN

END SUBROUTINE MicroChannelCoilUnitConvert

!***********************************************************************************

SUBROUTINE FinTubeCoilUnitConvert(IsSIUnit,FinPitch,Kfin,FinThk, &
								  ODtube,IDtube,Ktube,Pt,Pl,Ltube)
									  
! ----------------------------------------------------------------------
!
!   Description: To convert fin-tube coil input data into standard SI
!                Some data needs to be in IP unit for the
!                Corresponding model
!
!   Author:
!   Ipseng Iu
!   Mechanical and Aerospace Engineering
!   Oklahoma State University, Stillwater
!
!   Date:
!   November, 2005
!
! ----------------------------------------------------------------------

IMPLICIT NONE

LOGICAL,INTENT(IN)    :: IsSIunit    !SI unit flag
REAL,   INTENT(INOUT) :: FinPitch    !Fin pitch, fins/m
REAL,   INTENT(INOUT) :: Kfin        !Fin conductivity, kW/m-C 
REAL,   INTENT(INOUT) :: FinThk      !Fin thickness, m
REAL,   INTENT(INOUT) :: ODtube      !Tube height, m
REAL,   INTENT(INOUT) :: IDtube      !Tube depth, m
REAL,   INTENT(INOUT) :: Ktube       !Tube conductivity, kW/m-C
REAL,   INTENT(INOUT) :: Pt          !Tube spacing, m
REAL,   INTENT(INOUT) :: Pl          !Row spacing, m
REAL,   INTENT(INOUT) :: Ltube       !Single tube length, m

!FLOW:

  IF (IsSIunit) THEN !SI unit inputs
	  !FinPitch
	  Kfin       =Kfin*1e-3       !W/m-C to kW/m-C
	  FinThk     =FinThk*1e-3     !mm to m
	  ODtube     =ODtube*1e-3     !mm to m
	  IDtube     =IDtube*1e-3     !mm to m
	  Ktube      =Ktube*1e-3      !W/m-C to kW/m-C
	  Pt         =Pt*1e-3         !mm to m
	  Pl         =Pl*1e-3         !mm to m
	  Ltube      =Ltube*1e-3      !mm to m
  ELSE
	  FinPitch   =FinPitch*12/UnitL     !fins/in to fins/m
	  Kfin       =Kfin*UnitK            !Btu-in/hr-ft2-F to kW/m-C
	  FinThk     =FinThk*0.001/12*UnitL !mil to m
	  ODtube     =ODtube/12*UnitL       !in to m
	  IDtube     =IDtube/12*UnitL       !in to m
	  Ktube      =Ktube*UnitK           !Btu-in/hr-ft2-F to kW/m-C 
	  Pt         =Pt/12*UnitL           !in to m
	  Pl         =Pl/12*UnitL           !in to m
	  Ltube      =Ltube/12*UnitL        !in to m 
  END IF

RETURN

END SUBROUTINE FinTubeCoilUnitConvert

!***********************************************************************************

REAL FUNCTION Temperature_F2C(tF) 
	
!   Description: Convert temperature from Fahrenheit to Centigrade
!   Author: Venu Lolla

    IMPLICIT NONE
    REAL, INTENT(IN) :: tF

    Temperature_F2C = (tF - 32.0) / 1.8

END FUNCTION Temperature_F2C

!***********************************************************************************

REAL FUNCTION Temperature_C2F(tC) 
	
!   Description: Convert temperature from Centigrade to Fahrenheit
!   Author: Venu Lolla

    IMPLICIT NONE
    REAL, INTENT(IN) :: tC

    Temperature_C2F = tC * 1.8 + 32.0

END FUNCTION Temperature_C2F

!***********************************************************************************
END MODULE UnitConvertMod