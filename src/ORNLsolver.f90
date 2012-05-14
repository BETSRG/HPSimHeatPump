PROGRAM SimulationCycle

!
!
!
!
!          
!                   ___________________
!                  |                   |
!        3  -------|     Condenser     |-------  2 
!          |       |___________________|       |
!          |                                   |/|
!          |                                  /  |
!        |\ /| Expansion                     |   | Compressor
!        |/ \| Device                         \  |
!          |                                   |\|
!          |        ___________________        |
!          |       |                   |       |
!        4  -------|     Evaporator    |-------  1
!                  |___________________|
!
!
!
!
!
!
!
!
!
 
USE FluidProperties
USE HeatPumpInput
USE CompressorMod
USE CondenserMod
USE EvaporatorMod
USE AccumulatorMod
USE UnitConvertMod
USE DataSimulation
USE IFPORT
USE FrostModel
USE GeneralRoutines

IMPLICIT NONE

!Subroutine parameters

CHARACTER(len=80)   :: Refrigerant
CHARACTER (len=15) :: Property           
INTEGER            :: RefrigIndex =0
REAL Temperature,Quality,Pressure,Enthalpy

INTEGER(2) RefPropOpt			!Ref prop calc. option
INTEGER(2) RefPropErr			!Error flag:1-error; 0-no error
REAL RefProp(28)	!Refrigerant properties

INTEGER(2) AirPropOpt			!Air prop calc. option
INTEGER(2) AirPropErr			!Error flag:1-error; 0-no error
REAL AirProp(8)		!Air properties

REAL TimeStart, TimeStop, TimeSpent

INTEGER ICHRGE,I
REAL DTVALU
REAL DTCONV
REAL CHRGECONV
INTEGER IERROR
REAL DTVAL
REAL STEP
REAL CHGDIF

REAL TsubExp,TsubCnd,TsupEvp,TsupCmp,Qcnd,Qevp,QevpSens
REAL PwrCmp,mdot,TsiExp,TsoCnd,TsoEvp
REAL Dshtb,WinTrans,Qloss
REAL mdotRmax,mdotRmin,mdotRprev !mass flow rate iteration parameter
REAL DetailedQevp,SimpleQevp !Evaporator capacity from detailed and simple models
REAL DetailedQcnd,SimpleQcnd !Condenser capacity from detailed and simple models
REAL MassCoil,MassLiqCoil,MassVapCoil
INTEGER(2) IsCoolingMode !1=yes; 0=no
REAL, EXTERNAL :: ZEROCH
REAL, EXTERNAL :: CHARGM
!INTEGER :: TimeStep !Added Sankar transient
LOGICAL:: Trues
REAL :: SUPERAct
REAL :: TsiCmpAct
REAL :: TsoCmpAct
REAL :: RHiCAct
REAL :: RHiEAct
REAL, SAVE :: IDCFlowConst
REAL, SAVE :: ODCFlowConst
INTEGER   :: Flag

!Flow**:

  CoarseConvergenceCriteriaMet=.FALSE. !.TRUE. !.FALSE.
  FirstTimeAirTempLoop=.TRUE.
  FirstTimeFlowRateLoop=.TRUE.
  FirstTimeChargeLoop=.TRUE.
  PrnLog=1
  PrnCon=1

  !AMBCON=1 !air temperature, F
  !CNDCON=1 !subcooling, F
  !CHRGECONV=.5 !charge, lbm

  !EVPCON=1 !superheat, F

  !FLOCON=5 !mass flow rate, lbm/hr
  !EVAPPAR(50)=7 !Pressure, kPa
  !CONDPAR(56)=7 !.05 !Pressure, kPa

  WinTrans=0.9
  CondIN(7)=0*WinTrans !stillwater 0.83 kW/m2 !Harbin 0.82 kW/m2 !Singapore 1.03 kW/m2
  CondPAR(36)=0.8

  EvapIN(8)=0*WinTrans !stillwater 0.63 kW/m2 !Harbin 0.52 kW/m2 !Singapore 0.88 kW/m2
  EvapPAR(29)=0.8

  OPEN(5,FILE='YorkHP.out')
  OPEN(6,FILE='YorkHP.log')

  CALL GetInputs

  !Oil fraction
  CondPAR(59)=0.007
  EvapPAR(51)=0.007

  !IF (TsiCmp .GT. TaiE) THEN
  IF (TaiE-TsiCmp .LT. 10) THEN
      TsiCmp = TaiE - 10 !Correct initial guess
  END IF

  IF (TsoCmp-TaiC .LT. 10) THEN
      TsoCmp = TaiC + 10 !Correct initial guess
  END IF

  IF (TsoCmp .LE. TsiCmp) THEN
	  WRITE(*,*)'Compressor suction temperature is greater than discharge temperature.'
	  WRITE(*,*)'## ERROR ## Main: Wrong initial guess!'
      !WRITE(*,*)'Press return to terminate program.'
	  !READ(*,*)
	  CALL SLEEP(300) !Wait for 5 minutes and stop
	  STOP
  END IF
  Punit = ' (kPa)'
  Hunit = ' (kJ/kg)'
  Tunit = ' (C)'
  DTunit = ' (K)'
  MdotUnit = ' (kg/hr)'
  MassUnit = ' (kg)'
  PwrUnit = ' (W)'
  CapUnit = ' (W)'
  EERunit = ' (Btu/W-hr)'
  SysUnit = ' (ton)'
  NoUnit = ' (-)'
  Xunit = ' (%)'
  Lunit = ' (m)'
  MiniLunit = ' (mm)'
  IF (Unit .EQ. 2) THEN
      Punit = ' (psi)'
      Hunit = ' (Btu/lbm)'
      Tunit = ' (F)'
	  DTunit = ' (R)'
      MdotUnit = ' (lbm/h)'
      MassUnit = ' (lbm)'
      CapUnit = ' (Btu/h)'
      Lunit = ' (ft)'
	  MiniLunit = ' (in)'
  END IF

  CALL UnitConvert(Unit,CompPAR,CondPAR,EvapPAR,ShTbPAR,CapTubePAR,TxvPAR,  &
                   AccumPAR,FilterPAR,CFMcnd,CFMevp,TaiC,TaiE,RHiC,RHiE, &
				   Refchg,TSOCMP,TSICMP,SUPER,SUBCOOL,BaroPressure, &
				   ChargeCurveSlope,ChargeCurveIntercept,RefLiquidLength,Tdis,Tliq)

  CALL InitAccumulator(AccumPAR,Ref$)

  CALL EnergyPlus !Initialize EnergyPlus files, and functions for refprop calculations

SUPERAct=SUPER
TsiCmpAct=TsiCmp
TsoCmpAct=TsoCmp
RHiCAct=RHiC
RHiEAct=RHiE
IsCoolingMode=CondPAR(27)

  !Get simulation starting time
TimeStart=SECNDS(0.0)
CoolHeatModeFlag = IsCoolingMode
TimeInterval = 25.0
PrevSimTime = 0.0
Timestep=0
LastDefrostInitTime = 0.0

!DO WHILE(SimRunning)
DO WHILE(FrostingPeriod) !Added by Sankar
  !FirstTimeAirTempLoop=.TRUE.
  !FirstTimeFlowRateLoop=.TRUE.
  !FirstTimeChargeLoop=.TRUE.
  TimeStep = TimeStep+1

 CurSimTime=(TimeStep-1)*TimeInterval  !PrevSimTime+
 !PrevSimTime=CurSimTime
 IF(FrostingPeriod) THEN
  CALL EvaluateFrostModel
 ELSE IF(DefrostingPeriod) THEN
  CALL EvaluateDefrostModel
 END IF  

TsiCmp=TsiCmpAct
TsoCmp=TsoCmpAct
RHiC=RHiCAct
RHiE=RHiEAct
SUPER=SUPERAct
EvapIn=0.0
EvapOut=0.0
CondIn=0.0
CondOut=0.0

      IF (RHiC .GT. TaiC) THEN
		  WRITE(*,*)'## ERROR ## Main: Condenser wet bulb temperatuer is greater than dry bulb temperature.'
		  !WRITE(*,*)'Press return to terminate program.'
		  !READ(*,*)
		  CALL SLEEP(300) !Wait for 5 minutes and stop
		  STOP
	  END IF
      AirPropOpt=3
      AirProp(1)=(TaiC-32)/1.8
      AirProp(5)=RHiC
      CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
      RHiC=AirProp(3)
	  RhoAiC=AirProp(7)

  CondIN(5)=(TaiC-32)/1.8  
  CondIN(6)=RHiC           
  
      IF (RHiE .GT. TaiE) THEN !ISI - 11/04/07
		  WRITE(*,*)'## ERROR ## Main: Evaporator wet bulb temperatuer is greater than dry bulb temperature.'
		  !WRITE(*,*)'Press return to terminate program.'
		  !READ(*,*)
		  CALL SLEEP(300) !Wait for 5 minutes and stop
		  STOP
	  END IF
      AirPropOpt=3
      AirProp(1)=(TaiE-32)/1.8
      AirProp(5)=RHiE
      CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
      RHiE=AirProp(3)
	  RhoAiE=AirProp(7)

  EvapIN(5)=(TaiE-32)/1.8  !Air side inlet temp. C
  EvapIN(6)=RHiE            !Air side inlet relative humidity

  !Initialize
  Temperature=(TSICMP-32)/1.8
  Quality=1
  PiCmp=TQ(Ref$,Temperature,Quality,'pressure',RefrigIndex,RefPropErr)
  IF (RefPropErr .GT. 0) THEN
	  WRITE(*,*)'## ERROR ## Main: Refrigernat property is out of bound!'
      !WRITE(*,*)'Press return to terminate program.'
	  !READ(*,*)
	  CALL SLEEP(300) !Wait for 5 minutes and stop
	  STOP
  END IF
  PiCmp=PiCmp/1000.0

  PiEvp=PiCmp !Evaporator inlet pressure
  EvapIN(2)=PiEvp
  EvapOUT(1)=PiEvp
  EvapOUT(6)=PiEvp

  Temperature=(TSOCMP-32)/1.8
  Quality=1
  PoCmp=TQ(Ref$,Temperature,Quality,'pressure',RefrigIndex,RefPropErr)
  IF (RefPropErr .GT. 0) THEN
	  WRITE(*,*)'## ERROR ## Main: Refrigernat property is out of bound!'
      !WRITE(*,*)'Press return to terminate program.'
	  !READ(*,*)
	  CALL SLEEP(300) !Wait for 5 minutes and stop
	  STOP
  END IF  
  PoCmp=PoCmp/1000.0

  !IF (SUPER .GT. 0) THEN
  IF (SUPER .GE. 0) THEN !ISI - 11/16/07
	
	  Temperature=(TSICMP+SUPER-32)/1.8
	  Pressure=PiCmp*1000
	  HiCmp=TP(Ref$,Temperature,Pressure,'enthalpy',RefrigIndex,RefPropErr)
      IF (RefPropErr .GT. 0) THEN
		  WRITE(*,*)'## ERROR ## Main: Refrigernat property is out of bound!'
		  !WRITE(*,*)'Press return to terminate program.'
		  !READ(*,*)
		  CALL SLEEP(300) !Wait for 5 minutes and stop
		  STOP
      END IF
	  HiCmp=HiCmp/1000

  ELSE
      Pressure=PiCmp*1000
	  Quality=-SUPER
	  HiCmp=PQ(Ref$,Pressure,Quality,'enthalpy',RefrigIndex,RefPropErr)
      IF (RefPropErr .GT. 0) THEN
		  WRITE(*,*)'## ERROR ## Main: Refrigernat property is out of bound!'
		  !WRITE(*,*)'Press return to terminate program.'
		  !READ(*,*)
		  CALL SLEEP(300) !Wait for 5 minutes and stop
		  STOP
      END IF
	  HiCmp=HiCmp/1000

  END IF


  CompIN(1)=PiCmp
  CompIN(2)=PoCmp
  CompIN(3)=HiCmp
  IF (SystemType .NE. EVAPORATORONLY) THEN
      CALL Compressor(Ref$,PureRef,CompIN,CompPAR,CompOUT)
      IF (CompOUT(7) .NE. 0) THEN
	      SELECT CASE (INT(CompOUT(7)))
	      CASE (1)
		      WRITE(*,*)'## ERROR ## Highside: Compressor solution error!'
		      !WRITE(*,*)'Press return to terminate program.'
		      !READ(*,*)
		      CALL SLEEP(300) !Wait for 5 minutes and stop
		      STOP
	      CASE (2)
		      WRITE(*,*)'-- WARNING -- Highside: Refprop out of range in compressor model.'
	      END SELECT
      END IF 
  END IF
  WRITE(*,*) 
  
  EvapOUT(3)=(TSICMP-32)/1.8 !Initialize for reversing valve calculation
  
  IsCoolingMode=CondPAR(27)
  WRITE(6,*)'Heat Pump Design Tool (ver. 2.0 12/17/09)' 
  WRITE(*,*)'Heat Pump Design Tool (ver. 2.0 12/17/09)'
  IF (IsCoolingMode .EQ. 1) THEN
      IF (PrnLog .EQ. 1) WRITE(6,*)'***** Cooling Mode *****'
      IF (PrnCon .EQ. 1) WRITE(*,*)'***** Cooling Mode *****'
  ELSE
      IF (PrnLog .EQ. 1) WRITE(6,*)'***** Heating Mode *****'
      IF (PrnCon .EQ. 1) WRITE(*,*)'***** Heating Mode *****'
  END IF
!CoolHeatModeFlag = IsCoolingMode
!TimeInterval = 25.0
!PrevSimTime = 0.0
!Timestep=0
!DO WHILE(FrostingPeriod) !Added by Sankar
! TimeStep = TimeStep+1
! If(Timestep==31) THEN
!  trues=.TRUE.
! END IF
! CurSimTime=(TimeStep-1)*TimeInterval  !PrevSimTime+
! !PrevSimTime=CurSimTime
! IF(TimeStep==100) FrostingPeriod=.FALSE.
! CALL EvaluateFrostModel

  SELECT CASE(MODE)

  CASE(FIXEDORIFICESIM)
	  IF (PrnLog .EQ. 1) WRITE(6,*)'***** System Simulation (Fixed Orifice) *****'
	  IF (PrnCon .EQ. 1) WRITE(*,*)'***** System Simulation (Fixed Orifice) *****'
	  ICHRGE=2

	  !ISI - 08/07/06
	  AMBCON=1E-3 !1 !air temperature, F
	  CNDCON=1 !subcooling, F
	  CHRGECONV=.5 !charge, lbm

	  EVPCON=1 !superheat, F

	  FLOCON=5 !mass flow rate, lbm/hr
	  EVAPPAR(50)=7 !Pressure, kPa
	  CONDPAR(56)=7 !.05 !Pressure, kPa

  CASE(ORIFICEANDTXVDESIGN)
	  IF (PrnLog .EQ. 1) WRITE(6,*)'***** Design Calculation (Orifice and TXV) *****'
	  IF (PrnCon .EQ. 1) WRITE(*,*)'***** Design Calculation (Orifice and TXV) *****'
	  ICHRGE=0

	  AMBCON=1E-3 !1 !air temperature, F
	  CNDCON=1 !subcooling, F
	  CHRGECONV=.5 !charge, lbm

	  EVPCON=1 !superheat, F

	  FLOCON=5 !mass flow rate, lbm/hr
	  EVAPPAR(50)=7 !Pressure, kPa
	  CONDPAR(56)=7 !.05 !Pressure, kPa

  CASE(FIXEDSUPERHEATSIM)
	  IF (PrnLog .EQ. 1) WRITE(6,*)'***** Design Calculation (Fixed Orifice) *****'
	  IF (PrnCon .EQ. 1) WRITE(*,*)'***** Design Calculation (Fixed Orifice) *****'
	  ICHRGE=0

	  AMBCON=1E-3 !1 !air temperature, F
	  CNDCON=1 !subcooling, F
	  CHRGECONV=.5 !charge, lbm

	  EVPCON=1 !superheat, F

	  FLOCON=5 !mass flow rate, lbm/hr
	  EVAPPAR(50)=7 !Pressure, kPa
	  CONDPAR(56)=7 !.05 !Pressure, kPa

  CASE(TXVSIMULATION)
	  IF (PrnLog .EQ. 1) WRITE(6,*)'***** System Simulation (TXV) *****'
	  IF (PrnCon .EQ. 1) WRITE(*,*)'***** System Simulation (TXV) *****'
	  ICHRGE=2

	  !ISI - 08/07/06
	  AMBCON=1E-3 !1 !air temperature, F
	  CNDCON=0.5 !subcooling, F
	  CHRGECONV=.5 !charge, lbm

	  EVPCON=1 !superheat, F

	  FLOCON=5 !mass flow rate, lbm/hr
	  EVAPPAR(50)=7 !Pressure, kPa
	  CONDPAR(56)=7 !.05 !Pressure, kPa

  CASE(CONDENSERUNITSIM)
	  IF (PrnLog .EQ. 1) WRITE(6,*)'***** Condenser Unit Simulation *****'
	  IF (PrnCon .EQ. 1) WRITE(*,*)'***** Condenser Unit Simulation *****'
	  ICHRGE=0

	  !ISI - 08/07/06
	  AMBCON=1E-3 !1 !air temperature, F
	  CNDCON=1 !subcooling, F
	  CHRGECONV=.5 !charge, lbm

	  EVPCON=1 !superheat, F

	  FLOCON=5 !mass flow rate, lbm/hr
	  EVAPPAR(50)=7 !Pressure, kPa
	  CONDPAR(56)=7 !.05 !Pressure, kPa

  CASE(COILONLYSIM) !Added for coil only simulation - ISI - 10/23/07
	  IF (PrnLog .EQ. 1) WRITE(6,*)'***** Coil Only Simulation *****'
	  IF (PrnCon .EQ. 1) WRITE(*,*)'***** Coil Only Simulation *****'

	  IF (PrnLog .EQ. 1) WRITE(6,*)
	  IF (PrnCon .EQ. 1) WRITE(*,*)

	  IF (Unit .EQ. 1) THEN !SI Unit
	      IF (PrnLog .EQ. 1) WRITE(6,1006)'Iteration','mdot(kg/hr)','Capacity(kW)'
	      IF (PrnCon .EQ. 1) WRITE(*,1006)'Iteration','mdot(kg/hr)','Capacity(kW)'
      ELSE !IP Unit
	      IF (PrnLog .EQ. 1) WRITE(6,1006)'Iteration','mdot(lbm/hr)','Capacity(MBtu/hr)'
	      IF (PrnCon .EQ. 1) WRITE(*,1006)'Iteration','mdot(lbm/hr)','Capacity(MBtu/hr)'     
      END IF
      
        
	  IF (IsCoolingMode .GT. 0) THEN
    	  CondOUT(7)=Tliq
    	  Temperature=Tliq

	      Temperature=Tliq+SUBCOOL*5/9
	      Quality=0
	      PiExp=TQ(Ref$,Temperature,Quality,'pressure',RefrigIndex,RefPropErr)
	      PiExp=PiExp/1000

	      Pressure=PiExp*1000
	      Temperature=Tliq
	      HiExp=TP(Ref$,Temperature,Pressure,'enthalpy',RefrigIndex,RefPropErr)
	      HiExp=HiExp/1000
	      HiEvp=HiExp

	      Temperature=(TSICMP-32)*5/9
	      Quality=1
	      PiEvp=TQ(Ref$,Temperature,Quality,'pressure',RefrigIndex,RefPropErr)
	      PiEvp=PiEvp/1000

	      Pressure=PiEvp*1000
	      Temperature=(TSICMP+SUPER-32)*5/9
	      HoEvp=TP(Ref$,Temperature,Pressure,'enthalpy',RefrigIndex,RefPropErr)
	      HoEvp=HoEvp/1000

	  ELSE

	      Temperature=(TSOCMP-32)*5/9
	      Quality=1
	      PoCmp=TQ(Ref$,Temperature,Quality,'pressure',RefrigIndex,RefPropErr)
	      PoCmp=PoCmp/1000

	      CompOUT(5)=Tdis
	      Tocmp=Tdis
	      
	      Pressure=PoCmp*1000
	      Temperature=ToCmp
	      HoCmp=TP(Ref$,Temperature,Pressure,'enthalpy',RefrigIndex,RefPropErr)
	      HoCmp=HoCmp/1000

	      Temperature=(TSOCMP-32)*5/9
	      Quality=0
	      PiExp=TQ(Ref$,Temperature,Quality,'pressure',RefrigIndex,RefPropErr)
	      PiExp=PiExp/1000
          
          Pressure=PiExp*1000
	      Temperature=(TSOCMP-SUBCOOL-32)*5/9
	      HiExp=TP(Ref$,Temperature,Pressure,'enthalpy',RefrigIndex,RefPropErr)
	      HiExp=HiExp/1000
	      
	  END IF

	  	        
      !Initial guess
	  MdotRmax=999
	  MdotRmin=0
      MdotR=0.01 !0.1 is too big, not easy to converge, ISI - 02/11/08

      IF (IsCoolingMode .GT. 0) THEN !Cooling mode, indoor coil is evaporator

          XMaE=RhoAiE*CFMevp

	      EvapPAR(1)=0 !Suction line length
	      EVAPPAR(50) =0.5 !0.1 ! Mass flow rate convergence criterion
	      EvapPAR(54)=1 !First time

          !Determine if detailed model is needed, ISI - 02/07/08

          EvapIN(1)=MdotR			!Refrigerant side mass flow rate, kg/s
	      EvapIN(2)=PiEvp			!Evap. inlet pressure, kPa
	      EvapIN(3)=HiEvp			!Refrigerant side inlet enthalpy, kJ/kg
	      EvapIN(4)=XMaE            !Air side mass flow rate, kg/s
	      EvapIN(5)=(TaiE-32)/1.8   !Air side inlet temp. C
	      EvapIN(6)=RHiE            !Air side inlet relative humidity
	      EvapIN(9)=0.0             !Discharge temperature, C, not used for this

	      EvapPAR(53)=0 !Detailed model 			  
	      CALL Evaporator(Ref$,PureRef,EvapIN,EvapPAR,EvapOUT)	
	      DetailedQevp=-EvapOUT(11)
          CALL EndEvaporatorCoil

          EvapPAR(53)=1 !Simple model
	      CALL Evaporator(Ref$,PureRef,EvapIN,EvapPAR,EvapOUT)	
	      SimpleQevp=-EvapOUT(11)
          CALL EndEvaporatorCoil
    	  

	      IF (ABS((SimpleQevp-DetailedQevp)/DetailedQevp) .LT. 0.005) THEN
		    EvapPAR(53)=1 !Simple version
	      ELSE
		    EvapPAR(53)=0 !Detailed version
	      END IF
          
	      !Iterate mass flow rate to match outlet enthalpy
	      DO I=1,MaxIter
    				
              EvapIN(1)=MdotR			!Refrigerant side mass flow rate, kg/s
	          EvapIN(2)=PiEvp			!Evap. inlet pressure, kPa
	          EvapIN(3)=HiEvp			!Refrigerant side inlet enthalpy, kJ/kg
	          EvapIN(4)=XMaE            !Air side mass flow rate, kg/s
	          EvapIN(5)=(TaiE-32)/1.8   !Air side inlet temp. C
	          EvapIN(6)=RHiE            !Air side inlet relative humidity
	          EvapIN(9)=0.0             !Discharge temperature, C, not used for this
            			  
		      CALL Evaporator(Ref$,PureRef,EvapIN,EvapPAR,EvapOUT)			
              EvapPAR(54)=0 !First time

		      Qevp=-EvapOUT(11) 
    		            
	          IF (Unit .EQ. 1) THEN !SI Unit
	              IF (PrnLog .EQ. 1) WRITE(6,1005)I,MdotR*3600,Qevp
	              IF (PrnCon .EQ. 1) WRITE(*,1005)I,MdotR*3600,Qevp
              ELSE
	              IF (PrnLog .EQ. 1) WRITE(6,1005)I,MdotR/Umass*3600,Qevp/UnitPwr
	              IF (PrnCon .EQ. 1) WRITE(*,1005)I,MdotR/Umass*3600,Qevp/UnitPwr
              END IF
              
		      IF (ABS(EvapOUT(2)-HoEvp)>0.1 .AND. (mdotRmax-mdotRmin)/mdotR > 0.001) THEN

                  !Take half time step if not converged - ISI 12/09/2009
                  IF (EvapOUT(20) .GT. 0) THEN
                      mdotR=mdotRprev+(mdotR-mdotRprev)/2
                      CYCLE
                  END IF

                  !EvapOUT(2) is enthalpy, EvapOUT(20) is error flag
                  IF (EvapOUT(2) .LT. HoEvp .OR. EvapOUT(20) .GT. 0) THEN 
                      mdotRmax=mdotR
                  ELSE
                      mdotRmin=mdotR
                  END IF

                  mdotRprev=mdotR !Stored prevous value - ISI 12/09/2009

                  IF (mdotRmax .NE. 999 .AND. mdotRmin .NE. 0) THEN
                      mdotR=(mdotRmax+mdotRmin)/2                        
                  ELSE IF (mdotRmax .EQ. 999) THEN
                      mdotR=mdotR*1.5**I
                  ELSE IF (mdotRmin .EQ. 0) THEN
                      mdotR=mdotR*1.5**(-I)
                  END IF
		      ELSE
		          EXIT
		      END IF
    		        
	      END DO
	      
		  CALL CalcEvaporatorInventory(MassCoil,MassLiqCoil,MassVapCoil,EvapLiqTubeLength,EvapVapTubeLength,EvapTwoPhaseTubeLength,EvapNumLiqTubes)
          EvapOUT(14)=MassCoil	      
      ELSE !Heating mode, indoor coil is condenser
          XMaC=RhoAiC*CFMcnd

	      CondPAR(1)=0 !Discharge line length
	      CondPAR(8)=0 !Liquid line length
	      CondPAR(56)=0.5 !0.1 ! Mass flow rate convergence criterion
	      CondPAR(62)=1 !First time

	      CondIN(1)=MdotR
	      CondIN(2)=PoCmp         
	      CondIN(3)=HoCmp         
	      CondIN(4)=XMaC           
	      CondIN(5)=(TAIC-32)/1.8  
	      CondIN(6)=RHIC           
	      CondIN(8)=0 !Evaporator outlet temperature, C, not used for this
	      CondIN(9)=(TAIE-32)/1.8

          !Determine if detailed model is needed, ISI - 02/07/08
		  CondPAR(61)=1 !Simple version
		  CALL Condenser(Ref$,PureRef,CondIN,CondPAR,CondOUT)
		  SimpleQcnd=CondOUT(15)
		  CALL EndCondenserCoil

		  CondPAR(61)=0 !Detailed version
		  CALL Condenser(Ref$,PureRef,CondIN,CondPAR,CondOUT)
		  DetailedQcnd=CondOUT(15)
		  CALL EndCondenserCoil
			  
		  IF (ABS((SimpleQcnd-DetailedQcnd)/DetailedQcnd) .LT. 0.1) THEN
			  CondPAR(61)=1 !Simple version
		  ELSE
			  CondPAR(61)=0 !Detailed version
		  END IF 

	      !Iterate mass flow rate to match outlet enthalpy
	      DO I=1,MaxIter
    				
	          CondIN(1)=MdotR
	          CondIN(2)=PoCmp         
	          CondIN(3)=HoCmp         
	          CondIN(4)=XMaC           
	          CondIN(5)=(TAIC-32)/1.8  
	          CondIN(6)=RHIC           
	          CondIN(8)=0 !Evaporator outlet temperature, C, not used for this
	          CondIN(9)=(TAIE-32)/1.8
	                  			  
		      CALL Condenser(Ref$,PureRef,CondIN,CondPAR,CondOUT)			
              CondPAR(62)=0 !First time

		      Qcnd=CondOUT(15) 
    		            
	          IF (Unit .EQ. 1) THEN !SI Unit
	              IF (PrnLog .EQ. 1) WRITE(6,1005)I,MdotR*3600,Qcnd
	              IF (PrnCon .EQ. 1) WRITE(*,1005)I,MdotR*3600,Qcnd
              ELSE
	              IF (PrnLog .EQ. 1) WRITE(6,1005)I,MdotR/Umass*3600,Qcnd/UnitPwr
	              IF (PrnCon .EQ. 1) WRITE(*,1005)I,MdotR/Umass*3600,Qcnd/UnitPwr
              END IF
              
		      IF (ABS(CondOUT(6)-HiExp)>0.1 .AND. (mdotRmax-mdotRmin)/mdotR > 0.001) THEN

                   !Take half time step if not converged - ISI 12/09/2009
!                  IF (CondOUT(24) .GT. 0) THEN
!                      mdotR=mdotRprev-(mdotR-mdotRprev)/2
!                      CYCLE
!                  END IF

                  !CondOUT(6) is enthalpy, CondOUT(24) is error flag
                  IF (CondOUT(6) .LT. HiExp .OR. CondOUT(24) .GT. 0) THEN 
                      mdotRmin=mdotR
                  ELSE
                      mdotRmax=mdotR
                  END IF
                  
                  mdotRprev=mdotR !Stored prevous value - ISI 12/09/2009

                  IF (mdotRmax .NE. 999 .AND. mdotRmin .NE. 0) THEN
                      mdotR=(mdotRmax+mdotRmin)/2                        
                  ELSE IF (mdotRmax .EQ. 999) THEN
                      mdotR=mdotR*1.5**I
                  ELSE IF (mdotRmin .EQ. 0) THEN
                      mdotR=mdotR*1.5**(-I)
                  END IF
		      ELSE
		          EXIT
		      END IF
    		        
	      END DO

		  CALL CalcCondenserInventory(MassCoil,MassLiqCoil,MassVapCoil,CondLiqTubeLength,CondVapTubeLength,CondTwoPhaseTubeLength,CondNumLiqTubes)
		  CondOUT(18)=MassCoil      
      END IF
      	  
	  GOTO 30
  
  CASE DEFAULT
	  IF (PrnLog .EQ. 1) WRITE(6,*)'***** Design Calculation (Orifice and TXV) *****'
	  IF (PrnCon .EQ. 1) WRITE(*,*)'***** Design Calculation (Orifice and TXV) *****'
	  ICHRGE=0

	  !ISI - 08/07/06
	  AMBCON=1E-3 !1 !air temperature, F
	  CNDCON=1 !subcooling, F
	  CHRGECONV=.5 !charge, lbm

	  EVPCON=1 !superheat, F

	  FLOCON=5 !mass flow rate, lbm/hr
	  EVAPPAR(50)=7 !Pressure, kPa
	  CONDPAR(56)=7 !.05 !Pressure, kPa

  END SELECT
!comment block starts here
  FirstTimeHPdesignMode=.TRUE. !Moved from HPdesignMod - ISI 02/06/2009
  FirstTimeFlowRateLoop=.TRUE. !Moved from HPdesignMod - ISI 02/06/2009

  AMBCON=1E-3 !0.01 !0.05 !0.3 !0.01 !1E-3 !Air temp. F
  CNDCON=0.1 !0.3 !Subcooling, F
  CHRGECONV=0.05 !0.1 !Charge, lbm

  EVPCON=1 !0.1 !0.2 !SUPERHEAT

  FLOCON=5 !Mass flow rate, lbm/hr
  EVAPPAR(50) =0.1 ! 7
  CONDPAR(56)=0.1 !7 !.05

  DTVALU = SUPER
  IF (MODE .EQ. TXVSIMULATION) DTVALU=SUBCOOL
  DTCONV = EVPCON
  
10 CONTINUE

  !IERROR = 0
  IF(ICHRGE.EQ.0) GO TO 20
  IF (Refchg .EQ. 0) GO TO 20

  DTVAL = DTVALU 
  STEP = 30.0
  IF(ICHRGE.EQ.2) STEP = 5 !10

  !1st run is for coarse convergence criteria
  DTVALU = ZEROCH(DTVAL,CHARGM,CHRGECONV,CHRGECONV,STEP,CHGDIF,IERROR)
  !CALL SolveRegulaFalsi(CHRGECONV, MaxIter, Flag, DTVALU, CHARGM, DTVAL, STEP, IError)
 !      SolveRegulaFalsi(Eps, MaxIte, Flag, XRes, f, X_0, X_1, Par)
  GO TO 30 !Skip refined simulation

!  IF (PrnLog .EQ. 1)  THEN 
!	WRITE(6,*)	
!   WRITE(6,*)'~~~~ Start refined simulation ~~~~'
!  END IF
!  
!  IF (PrnCon .EQ. 1) THEN
!    WRITE(*,*)
!    WRITE(*,*)'~~~~ Start refined simulation ~~~~'
!  END IF

  !~Refine convergence criteria and run simulation again
  CoarseConvergenceCriteriaMet=.TRUE.

  IF (MODE .EQ. CONDENSERUNITSIM) DTVAL=DTROC !Use previouse iterated value, ISI - 07/26/07
  AMBCON=1E-3 !0.01 !0.05 !0.3 !0.01 !1E-3 !Air temp. F
  CNDCON=0.1 !0.3 !Subcooling, F
  CHRGECONV=0.05 !0.1 !Charge, lbm

  EVPCON=1 !0.1 !0.2 !SUPERHEAT

  FLOCON=5 !Mass flow rate, lbm/hr
  EVAPPAR(50) =0.1 ! 7
  CONDPAR(56)=0.1 !7 !.05

  DTVALU = ZEROCH(DTVAL,CHARGM,CHRGECONV,CHRGECONV,STEP,CHGDIF,IERROR)
  !CALL SolveRegulaFalsi(CHRGECONV, MaxIter, Flag, DTVALU, CHARGM, DTVAL, STEP,IError)

  GO TO 30

20 CONTINUE

  DTROC=SUBCOOL !Specified subcooling


  !1st run is for coarse convergence criteria
  CALL HPDM(DTVALU)

  GO TO 30 !Skip refined simulation

!  IF (PrnLog .EQ. 1)  THEN 
!	WRITE(6,*)	
!    WRITE(6,*)'~~~~ Start refined simulation ~~~~'
!  END IF
!  
!  IF (PrnCon .EQ. 1) THEN
!    WRITE(*,*)
!    WRITE(*,*)'~~~~ Start refined simulation ~~~~'
!  END IF

  !~Refine convergence criteria and run simulation again
  CoarseConvergenceCriteriaMet=.TRUE.
	
  AMBCON=1E-3 !0.01 !0.05 !0.3 !0.01 !1E-3 !Air temp. F
  CNDCON=0.1 !0.3 !Subcooling, F
  CHRGECONV=0.1 !0.1 !Charge, lbm

  EVPCON=1 !0.1 !0.2 !SUPERHEAT

  FLOCON=5 !Mass flow rate, lbm/hr
  EVAPPAR(50) =0.1 !0.01 !7
  CONDPAR(56)=0.1 !0.01 !7 !.05

  !2nd run is for refined convergence criteria
  CALL HPDM(DTVALU)
 
30 CONTINUE
!Comment block ends here
  !CLOSE(5)
  !CLOSE(6)

  CALL DumpOutputs

  TimeSpent=SECNDS(TimeStart)
  WRITE(*,*)
  WRITE(*,*)'Calculation completed successfully.'
  WRITE(*,103)'Time Spent (Min):',TimeSpent/60
  WRITE(5,*) 
  WRITE(5,103)'Time Spent (Min):',TimeSpent/60  
  WRITE(*,*)'Press return to end program.'      
  !READ(*,*)                                     !For parametric run, comment this line

  IF (TaiE .GT. 32) THEN !only update time step above freezing temp.  - ISI 12/22/2009
    IF (MODE .NE. FIXEDORIFICESIM .OR. MODE .NE. TXVSIMULATION) EXIT !only update time step for simulation mode - ISI 12/22/2009
    IF (IsCoolingMode .EQ. 2) EXIT !only update time step for heating mode - ISI 12/22/2009
    IF (RHIE .LT. 0.8 ) EXIT !only update time step for wet condition - ISI 12/22/2009    
  END IF
  
  IF(TimeStep == 1) THEN
   IDCFlowConst = CoilParams(1)%AirFlowRate*SQRT(CoilParams(1)%DPair)
   !ODCFlowConst = CoilParams(2)%AirFlowRate*SQRT(CoilParams(2)%DPair)
   ODCFlowConst=926.8     !497.17
  END IF
  
  CFMEvp= ODCFlowConst*(CoilParams(2)%DPair*1000/UairPres)**(-0.3973)*UnitArFlw    !CoilParams(2)%AirFlowRate*(1-0.008*TimeStep)*UnitArFlw
  
  CALL DetermineDefrostInitiate
  DefrostInitiate = .FALSE.
  IF(DefrostInitiate==.TRUE.) THEN
   FrostingPeriod=.FALSE.
   DefrostingPeriod = .TRUE.
  END IF 
 END DO

  CLOSE(5)
  CLOSE(6)
  !CLOSE(7)
  CLOSE(9)

  CALL PrintCondenserResult 
  CALL EndCondenserCoil

  IF (MODE .NE. CONDENSERUNITSIM) THEN
	  CALL PrintEvaporatorResult 
	  CALL EndEvaporatorCoil
  END IF

  CALL EndEnergyPlus
  
  CLOSE(666)

  STOP

103 FORMAT(A20,F30.2) 
1005 FORMAT(I20,10(F20.2))
1006 FORMAT(10(A20))
1007 FORMAT(A17,A42)
1008 FORMAT(A13,F10.3,A10,A10,A13,F10.3,A10)
2001 FORMAT(A13,F10.3,A10)
2004 FORMAT(A56,F10.3,A10)
2007 FORMAT(A16,F10.3,A9)

END PROGRAM
