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

SUBROUTINE DumpOutputs

USE FluidProperties_HPSim
USE AirPropMod
USE DataSimulation
USE DataGlobals_HPSim, ONLY: RefrigIndex   !RS: Debugging: Removal of plethora of RefrigIndex definitions in the code

IMPLICIT NONE

INTEGER(2) RefPropOpt			!Ref prop calc. option
INTEGER(2) RefPropErr			!Error flag:1-error; 0-no error
REAL RefProp(28)	!Refrigerant properties

INTEGER(2) AirPropOpt			!Air prop calc. option
INTEGER(2) AirPropErr			!Error flag:1-error; 0-no error
REAL AirProp(8)		!Air properties

REAL TsubExp,TsubCnd,TsupEvp,TsupCmp,Qcnd,Qevp,QevpSens,QevpLat,Tsat
REAL PwrCmp,mdot,TsiExp,TsoCnd,TsoEvp,Qtxv,IsCoolingMode
REAL Dshtb,DrawBlow,CPair,Qtot,QtotSens
REAL DcapTube,LcapTube
REAL TAOCND,TAOEVP,COP,SHR,EER

REAL TSATICMP,TSUBICMP,TSUPICMP
REAL TSATOCMP,TSUBOCMP,TSUPOCMP
REAL TSATICND,TSUBICND,TSUPICND
REAL TSATOCND,TSUBOCND,TSUPOCND
REAL TSATIEXP,TSUBIEXP,TSUPIEXP
REAL TSATOEXP,TSUBOEXP,TSUPOEXP
REAL TSATIEVP,TSUBIEVP,TSUPIEVP
REAL TSATOEVP,TSUBOEVP,TSUPOEVP

REAL TDBICND,TWBICND,RHICND,HAICND
REAL TDBOCND,TWBOCND,RHOCND,DPACND,WAOCND,HAOCND
REAL TDBIEVP,TWBIEVP,RHIEVP,HAIEVP
REAL TDBOEVP,TWBOEVP,RHOEVP,DPAEVP,HAOEVP,WAOEVP

REAL MassCmp,MassCnd,MassEvp,MassSucLn,MassDisLn,MassLiqLn,MassDistube
REAL AccumDP,FilterDP,MassAccum
REAL WeightEvpAluminum,WeightEvpCopper,WeightCndAluminum,WeightCndCopper
REAL,PARAMETER :: StandardDensity=1.2 !Standard density, kg/m3
REAL,PARAMETER :: StandardSpecHeat=1.02 !Standard specific heat, kJ/kg-K
REAL SpecHeat     !Specific heat, kJ/kg-K
CHARACTER (len=15) :: Property           
!INTEGER            :: RefrigIndex =0
REAL Temperature,Quality,Pressure,Enthalpy
CHARACTER (len=50) :: Title !Output file title

REAL :: CoilSurfTemp = 0.0

CHARACTER(LEN=9),PARAMETER :: FMT_1007 = "(A17,A42)"
CHARACTER(LEN=33),PARAMETER :: FMT_1008 = "(A13,F10.3,A10,A10,A13,F10.3,A10)"
CHARACTER(LEN=15),PARAMETER :: FMT_2001 = "(A13,F10.3,A10)"
CHARACTER(LEN=15),PARAMETER :: FMT_2004 = "(A56,F10.3,A10)"
CHARACTER(LEN=14),PARAMETER :: FMT_2007 = "(A16,F10.3,A9)"
CHARACTER(LEN=13),PARAMETER :: FMT_2009 = "(60(A16,','))"
CHARACTER(LEN=11),PARAMETER :: FMT_2100 = "(60(E,','))"
CHARACTER(LEN=13),PARAMETER :: FMT_2200 = "(A32,',',A50)"
CHARACTER(LEN=3),PARAMETER :: FMT_2204 = "(A)"
CHARACTER(LEN=23),PARAMETER :: FMT_2208 = "(A33,',',F40.3,',',A15)"
CHARACTER(LEN=69),PARAMETER :: FMT_2212 = "(A24,',',A18,',',A18,',',A21,',',A29,',',A14,',',A17,',',A16,',',A26)"
CHARACTER(LEN=83),PARAMETER :: FMT_2216 = "(A24,',',F18.3,',',F18.3,',',F21.3,',',F29.3,',',F14.1,',',F17.3,',',F16.3,',',A26)"
CHARACTER(LEN=53),PARAMETER :: FMT_2220 = "(A18,',',A27,',',A27,',',A24,',',A33,',',A25,',',A20)"
CHARACTER(LEN=63),PARAMETER :: FMT_2224 = "(A18,',',F27.3,',',F27.3,',',F24.1,',',F33.3,',',F25.3,',',A20)"

  IsCoolingMode=EvapPAR(20)
  SELECT CASE(MODE)
  CASE(FIXEDORIFICESIM)
 
	  IF (SystemType .EQ. 4) THEN
		  Title='Short Tube Simulation, Reheat mode'
	  ELSEIF (IsCoolingMode .GT. 0) THEN	  
		  Title='Short Tube Simulation, Cooling Mode'
	  ELSE
		  Title='Short Tube Simulation, Heating Mode'
	  END IF
  CASE(ORIFICEANDTXVDESIGN)

	  IF (SystemType .EQ. 4) THEN
		  Title='Short Tube Simulation, Reheat mode'
	  ELSEIF (IsCoolingMode .GT. 0) THEN	  
		  Title='Fixed Subcooling Design Calculation, Cooling Mode'
	  ELSE
		  Title='Fixed Subcooling Design Calculation, Heating Mode'
	  END IF
  CASE(FIXEDSUPERHEATSIM)
   
	  IF (SystemType .EQ. 4) THEN
		  Title='Short Tube Simulation, Reheat mode'
	  ELSEIF (IsCoolingMode .GT. 0) THEN	  
		  Title='Fixed Orifice Design Calculation, Cooling Mode'
	  ELSE
		  Title='Fixed Orifice Design Calculation, Heating Mode'
	  END IF
  CASE(TXVSIMULATION)
  
	  IF (SystemType .EQ. 4) THEN
		  Title='Short Tube Simulation, Reheat mode'
	  ELSEIF (IsCoolingMode .GT. 0) THEN	  
		  Title='TXV Simulation, Cooling Mode'
	  ELSE
		  Title='TXV Simulation, Heating Mode'
	  END IF
  CASE(CONDENSERUNITSIM)  
	  Title='Condenser Unit, Cooling Mode'
  CASE(COILONLYSIM)  
	  IF (IsCoolingMode .GT. 0) THEN
	    Title='Coil Only, Cooling Mode'
	  ELSE
	    Title='Coil Only, Heating Mode'
	  END IF
  END SELECT

  !Calculate corresponding output depending on the unit type
  IF (MODE .EQ. CONDENSERUNITSIM) THEN

      !*******Compressor data*******
      PiCmp=CompIN(1)
      HiCmp=CompIN(3)

      Pressure=PiCmp*1000   !RS Comment: Unit Conversion
      Enthalpy=HiCmp*1000   !RS Comment: Unit Conversion
      TiCmp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr) !Compressor Inlet Temperature
      XiCmp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr) !Compressor Inlet Quality

      Quality=1
      TsatiCmp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Compressor Saturation Temperature

      TsupiCmp=TiCmp-TsatiCmp
      IF (TsupiCmp .LT. 0 ) THEN
          TsupiCmp=0
      END IF

      Quality=0
      TsatiCmp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Compressor Saturation Temperature
      TsubiCmp=TsatiCmp-TiCmp
      IF (TsubiCmp .LT. 0 ) THEN
          TsubiCmp=0
      END IF

      IF (XiCmp .GE. 1) THEN
          XiCmp=1
      END IF
      IF (XiCmp .LE. 0) THEN
          XiCmp=0
      END IF
  
      PoCmp=CompIN(2)
      HoCmp=CompOUT(3)

      Pressure=PoCmp*1000   !RS Comment: Unit Conversion
      Enthalpy=HoCmp*1000   !RS Comment: Unit Conversion
      ToCmp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr) !Compressor Outlet Temperature
      XoCmp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr) !Compressor Outlet Quality

      Quality=1
      TsatoCmp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Compressor Saturation Temperature

      TsupoCmp=ToCmp-TsatoCmp
      IF (TsupoCmp .LT. 0 ) THEN
          TsupoCmp=0
      END IF

      Quality=0
      TsatoCmp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Compressor Saturation Temperature
      TsuboCmp=TsatoCmp-ToCmp
      IF (TsuboCmp .LT. 0 ) THEN
          TsuboCmp=0
      END IF

      IF (XoCmp .GE. 1) THEN
          XoCmp=1
      END IF
      IF (XoCmp .LE. 0) THEN
          XoCmp=0
      END IF

      PwrCmp=CompOUT(1)*1000    !RS Comment: Unit Conversion
      MassCmp=CompOUT(6)

      !*******Condenser*******
      PiCnd=CondOUT(1)
      HiCnd=CondOUT(2)

      Pressure=PiCnd*1000   !RS Comment: Unit Conversion
      Enthalpy=HiCnd*1000   !RS Comment: Unit Conversion
      TiCnd=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr) !Condenser Inlet Temperature
      XiCnd=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr) !Condenser Inlet Quality

      Quality=1
      TsatiCnd=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Condenser Saturation Temperature

      TsupiCnd=TiCnd-TsatiCnd
      IF (TsupiCnd .LT. 0 ) THEN
          TsupiCnd=0
      END IF

      Quality=0
      TsatiCnd=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Condenser Saturation Temperature
      TsubiCnd=TsatiCnd-TiCnd
      IF (TsubiCnd .LT. 0 ) THEN
          TsubiCnd=0
      END IF

      IF (XiCnd .GE. 1) THEN
          XiCnd=1
      END IF
      IF (XiCnd .LE. 0) THEN
          XiCnd=0
      END IF

      TdbiCnd=CondIN(5)
      RHiCnd=CondIN(6)

      AirPropOpt=2
      AirProp(1)=TdbiCnd
      AirProp(3)=RHiCnd
      CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
      hAiCnd=AirProp(4)
      TwbiCnd=AirProp(5)
      RhoAiC=AirProp(7)

      CPair=CPA(TdbiCnd)

      PoCnd=CondOUT(5)
      HoCnd=CondOUT(6)

      Pressure=PoCnd*1000   !RS Comment: Unit Conversion
      Enthalpy=HoCnd*1000   !RS Comment: Unit Conversion
      ToCnd=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr) !Condenser Outlet Temperature
      XoCnd=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr) !Condenser Outlet Quality

      Quality=1
      TsatoCnd=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Condenser Saturation Temperature

      TsupoCnd=ToCnd-TsatoCnd
      IF (TsupoCnd .LT. 0 ) THEN
          TsupoCnd=0
      END IF

      Quality=0
      TsatoCnd=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Condenser Saturation Temperature
      TsuboCnd=TsatoCnd-ToCnd
      IF (TsuboCnd .LT. 0 ) THEN
          TsuboCnd=0
      END IF

      IF (XoCnd .GE. 1) THEN
          XoCnd=1
      END IF
      IF (XoCnd .LE. 0) THEN
          XoCnd=0
      END IF

      !CFMcnd=SCFMcnd
      Qcnd =CondOUT(15)*1000    !RS Comment: Unit Conversion

      TdboCnd=CondOUT(21)
      RHoCnd=CondOUT(22)

      AirPropOpt=2
      AirProp(1)=TdboCnd
      AirProp(3)=RHoCnd
      CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
      WaoCnd=AirProp(2)
      hAoCnd=AirProp(4)
      TwboCnd=AirProp(5)
      RhoAoC=AirProp(7)

      DPaCND=CondOUT(23)*1000   !RS Comment: Unit Conversion

      MassCnd=CondOUT(18)
      MassDisLn=CondOUT(16)
      MassLiqLn=CondOUT(17)

      WeightCndAluminum=CondOUT(28)
      WeightCndCopper=CondOUT(29)

      !*******Evaporator*******
      PiEvp=0
      HiEvp=0
      TiEvp=0
      XiEvp=0
      TsatiEvp=0
      TsupiEvp=0
      TsubiEvp=0
      TdbiEvp=0
      RHiEvp=0
      hAiEvp=0
      TwbiEvp=0
      RhoAiE=0
      PoEvp=0
      HoEvp=0

      ToEvp=0
      XoEvp=0
      TsatoEvp=0
      TsupoEvp=0
      TsatoEvp=0
      TsuboEvp=0

      CFMevp=0
      Qevp=mdotR*(HiCmp-HoCnd)*1000
      QevpSens=0
      QevpLat=0
      
      TdboEvp=0
      RHoEvp=0
      WaoEvp=0
      hAoEvp=0
      TwboEvp=0
      RhoAoE=0

      DPaEVP=0

      MassEvp=0
      MassSucLn=0

      WeightEvpAluminum=0
      WeightEvpCopper=0      

	  !*******Exp. device*******
	  PiExp=0
	  HiExp=0
	  MassDisTube=0

	  Dshtb=0
	  DcapTube=0
	  LcapTube=0
	  Qtxv=0
    	  
	  PiExp=0
	  HiExp=0
	  MassDisTube=0
    	  	  
	  TiExp=0
	  XiExp=0

	  TsatiExp=0
      TsupiExp=0
	  TsatiExp=0
      TsubiExp=SUBCOOL/1.8
	  PoExp=0
	  HoExp=0

	  ToExp=0
	  XoExp=0

	  TsatoExp=0
	  TsupoExp=0
	  TsuboExp=0

      EER=0
      COP=0

      CalChg=0
      MassAccum=0
      AccumDP=0
      FilterDP=0
  
  ELSEIF (MODE .EQ. COILONLYSIM) THEN

      IF (IsCoolingMode .GT. 0) THEN

          !*******Condenser*******
          PiCnd=0
          HiCnd=0
          TiCnd=0
          XiCnd=0
          TsatiCnd=0
          TsupiCnd=0
          TsubiCnd=0
          TdbiCnd=0
          RHiCnd=0
          hAiCnd=0
          TwbiCnd=0
          RhoAiC=0
          PoCnd=0
          HoCnd=0

          ToCnd=CondOUT(7)
          XoCnd=0
          TsatoCnd=0
          TsupoCnd=0
          TsatoCnd=0
          TsuboCnd=0

          CFMcnd=0
          Qcnd =0

          TdboCnd=0
          RHoCnd=0
          WaoCnd=0
          hAoCnd=0
          TwboCnd=0
          RhoAoC=0

          DPaCND=0

          MassCnd=0
          MassDisLn=0
          MassLiqLn=0

          WeightCndAluminum=0
          WeightCndCopper=0

	      !*******Evaporator*******
	      PiEvp=EvapIN(2)
	      HiEvp=EvapIN(3)

	      Pressure=PiEvp*1000   !RS Comment: Unit Conversion
	      Enthalpy=HiEvp*1000   !RS Comment: Unit Conversion
	      TiEvp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr) !Evaporator Inlet Temperature
	      XiEvp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr) !Evaporator Inlet Quality

	      Quality=1
	      TsatiEvp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Evaporator Saturation Temperature

	      TsupiEvp=TiEvp-TsatiEvp
	      IF (TsupiEvp .LT. 0 ) THEN
              TsupiEvp=0
          END IF

	      Quality=0
	      TsatiEvp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Evaporator Saturation Temperature
	      TsubiEvp=TsatiEvp-TiEvp
	      IF (TsubiEvp .LT. 0 ) THEN
              TsubiEvp=0
          END IF

	      IF (XiEvp .GE. 1) THEN
              XiEvp=1
          END IF
	      IF (XiEvp .LE. 0) THEN
              XiEvp=0
          END IF

	      TdbiEvp=EvapIN(5)
	      RHiEvp=EvapIN(6)

	      AirPropOpt=2
	      AirProp(1)=TdbiEvp
	      AirProp(3)=RHiEvp
	      CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
	      hAiEvp=AirProp(4)
	      TwbiEvp=AirProp(5)
	      RhoAiE=AirProp(7)

	      CPair=CPA(TdbiEvp)

	      PoEvp=EvapOUT(1)
	      HoEvp=EvapOUT(2)

	      Pressure=PoEvp*1000   !RS Comment: Unit Conversion
	      Enthalpy=HoEvp*1000   !RS Comment: Unit Conversion
	      ToEvp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr) !Evaporator Outlet Temperature
	      XoEvp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr) !Evaporator Outlet Temperature

	      Quality=1
	      TsatoEvp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Evaporator Saturation Temperature

	      TsupoEvp=ToEvp-TsatoEvp
	      IF (TsupoEvp .LT. 0 ) THEN
              TsupoEvp=0
          END IF

	      Quality=0
	      TsatoEvp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Evaporator Saturation Temperature
	      TsuboEvp=TsatoEvp-ToEvp
	      IF (TsuboEvp .LT. 0 ) THEN
              TsuboEvp=0
          END IF

	      IF (XoEvp .GE. 1) THEN
              XoEvp=1
          END IF
	      IF (XoEvp .LE. 0) THEN
              XoEvp=0
          END IF

	      DPaEvp=EvapOUT(19)*1000   !RS Comment: Unit Conversion

	      Qevp =-EvapOUT(11)*1000   !RS Comment: Unit Conversion
	      QevpSens=-EvapOUT(12)*1000    !RS Comment: Unit Conversion
	      IF (ABS(QevpSens) .GT. ABS(Qevp)) THEN !Make sure sensible heat is never higher than total heat. ISI - 08/02/07
	          QevpSens = Qevp
	          hAoEvp=-Qevp/1000/(CFMevp*RhoAiE)+hAiEvp
	          SpecHeat=CPA(TdbiEvp)
	          TdboEvp=-QevpSens/1000/(CFMevp*RhoAiE*SpecHeat)+TdbiEvp
	          AirPropOpt=1
	          AirProp(1)=TdboEvp
	          AirProp(4)=hAoEvp
	          CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
	          TwboEvp=AirProp(5)
	          RHoEvp=AirProp(3)
	      ELSE
    	      TdboEvp=EvapOUT(17)
	          RHoEvp=EvapOUT(18) 
	          AirPropOpt=2
	          AirProp(1)=TdboEvp
	          AirProp(3)=RHoEvp
	          CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
	          TwboEvp=AirProp(5)
	          RhoAoE=AirProp(7)
	      END IF

	      QevpLat=Qevp-QevpSens
	      SHR=QevpSens/Qevp

	      MassEvp=EvapOUT(14)
	      MassSucLn=EvapOUT(13)+AccumOUT(1)

	      WeightEvpAluminum=EvapOUT(24)
	      WeightEvpCopper=EvapOUT(25)

	      !*******Exp. device*******
	      PiExp=0
	      HiExp=0
	      MassDisTube=0

	      Dshtb=0
	      DcapTube=0
	      LcapTube=0
	      Qtxv=0
        	      	  	  
	      TiExp=ToCnd
	      XiExp=1

	      TsatiExp=0

	      TsupiExp=0

          TsubiExp=0

	      IF (XiExp .GE. 1) THEN
              XiExp=1
          END IF
	      IF (XiExp .LE. 0) THEN
              XiExp=0
          END IF

	      PoExp=0
	      HoExp=0

	      ToExp=0
	      XoExp=0

	      TsatoExp=0
	      TsupoExp=0
	      TsuboExp=0

          !*******Compressor data*******
          PoCmp=0
          HoCmp=0
          ToCmp=0
          XoCmp=0
          TsatoCmp=(TSOCMP-32)*5/9  !RS Comment: Unit Conversion, from F to C
          TsupoCmp=0
          TsuboCmp=0

          PiCmp=CompIN(1) !PoEvp
          HiCmp=CompIN(3) !HoEvp

	      Pressure=PiCmp*1000   !RS Comment: Unit Conversion
	      Enthalpy=HiCmp*1000   !RS Comment: Unit Conversion
	      TiCmp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr) !Compressor Inlet Temperature
	      XiCmp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr) !Compressor Inlet Quality

          TsatiCmp=(TSICMP-32)*5/9 !TsatoEvp    !RS Comment: Unit Conversion, from F to C
          TsupiCmp=TsupoEvp
          TsubiCmp=TsuboEvp

          PwrCmp=0
          mdot=0
          MassCmp=0

      ELSE

          !*******Condenser*******
          PiCnd=CondOUT(1)
          HiCnd=CondOUT(2)

          Pressure=PiCnd*1000   !RS Comment: Unit Conversion
          Enthalpy=HiCnd*1000   !RS Comment: Unit Conversion
          TiCnd=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr) !Condenser Inlet Temperature
          XiCnd=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr) !Condenser Inlet Quality

          Quality=1
          TsatiCnd=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Condenser Saturation Temperature

          TsupiCnd=TiCnd-TsatiCnd
          IF (TsupiCnd .LT. 0 ) THEN
              TsupiCnd=0
          END IF

          Quality=0
          TsatiCnd=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Condenser Saturation Temperature
          TsubiCnd=TsatiCnd-TiCnd
          IF (TsubiCnd .LT. 0 ) THEN
              TsubiCnd=0
          END IF

          IF (XiCnd .GE. 1) THEN
              XiCnd=1
          END IF
          IF (XiCnd .LE. 0) THEN
              XiCnd=0
          END IF

          TdbiCnd=CondIN(5)
          RHiCnd=CondIN(6)

          AirPropOpt=2
          AirProp(1)=TdbiCnd
          AirProp(3)=RHiCnd
          CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
          hAiCnd=AirProp(4)
          TwbiCnd=AirProp(5)
          RhoAiC=AirProp(7)

          CPair=CPA(TdbiCnd)

          PoCnd=CondOUT(5)
          HoCnd=CondOUT(6)

          Pressure=PoCnd*1000   !RS Comment: Unit Conversion
          Enthalpy=HoCnd*1000   !RS Comment: Unit Conversion
          ToCnd=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr) !Condenser Outlet Temperature
          XoCnd=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr) !Condenser Outlet Quality

          Quality=1
          TsatoCnd=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Condenser Saturation Temperature

          TsupoCnd=ToCnd-TsatoCnd
          IF (TsupoCnd .LT. 0 ) THEN
              TsupoCnd=0
          END IF

          Quality=0
          TsatoCnd=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Condenser Saturation Temperature
          TsuboCnd=TsatoCnd-ToCnd
          IF (TsuboCnd .LT. 0 ) THEN
              TsuboCnd=0
          END IF

          IF (XoCnd .GE. 1) THEN
              XoCnd=1
          END IF
          IF (XoCnd .LE. 0) THEN
              XoCnd=0
          END IF

          Qcnd =CondOUT(15)*1000    !RS Comment: Unit Conversion

          TdboCnd=CondOUT(21)
          RHoCnd=CondOUT(22)

          AirPropOpt=2
          AirProp(1)=TdboCnd
          AirProp(3)=RHoCnd
          CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
          WaoCnd=AirProp(2)
          hAoCnd=AirProp(4)
          TwboCnd=AirProp(5)
          RhoAoC=AirProp(7)

          DPaCND=CondOUT(23)*1000   !RS Comment: Unit Conversion

          MassCnd=CondOUT(18)
          MassDisLn=CondOUT(16)
          MassLiqLn=CondOUT(17)

          WeightCndAluminum=CondOUT(28)
          WeightCndCopper=CondOUT(29)

          !*******Evaporator*******
          PiEvp=0
          HiEvp=0
          TiEvp=0
          XiEvp=0
          TsatiEvp=0
          TsupiEvp=0
          TsubiEvp=0
          TdbiEvp=0
          RHiEvp=0
          hAiEvp=0
          TwbiEvp=0
          RhoAiE=0
          PoEvp=0
          HoEvp=0

          ToEvp=0
          XoEvp=0
          TsatoEvp=0
          TsupoEvp=0
          TsatoEvp=0
          TsuboEvp=0

          CFMevp=0
          Qevp=0
          QevpSens=0
          QevpLat=0

          TdboEvp=0
          RHoEvp=0
          WaoEvp=0
          hAoEvp=0
          TwboEvp=0
          RhoAoE=0

          DPaEVP=0
          SHR=1

          MassEvp=0
          MassSucLn=0

          WeightEvpAluminum=0
          WeightEvpCopper=0      

	      !*******Exp. device*******
	      PiExp=PoCnd
	      HiExp=HoCnd
	      MassDisTube=0

	      Dshtb=0
	      DcapTube=0
	      LcapTube=0
	      Qtxv=0
        	          	  	  
	      TiExp=ToCnd
	      XiExp=XoCnd

	      TsatiExp=TsatoCnd
          TsupiExp=TsupoCnd
	      TsatiExp=TsatoCnd
          TsubiExp=TsuboCnd
          
	      PoExp=0
	      HoExp=0

	      ToExp=0
	      XoExp=0

	      TsatoExp=0
	      TsupoExp=0
	      TsuboExp=0

          !*******Compressor data*******
          PoCmp=PiCnd
          HoCmp=HiCnd

          ToCmp=TiCnd
          XoCmp=XiCnd

          TsatoCmp=(TSOCMP-32)*5/9 !TsatiCnd    !RS Comment: Unit Conversion, from F to C
          TsupoCmp=TsupiCnd
          TsuboCmp=TsubiCnd

          IF (XoCmp .GE. 1) THEN
              XoCmp=1
          END IF
          IF (XoCmp .LE. 0) THEN
              XoCmp=0
          END IF

          PiCmp=0
          HiCmp=0
          TiCmp=0
          XiCmp=0
          TsatiCmp=(TSICMP-32)*5/9  !RS Comment: Unit Conversionm, from F to C
          TsupiCmp=0
          TsubiCmp=0

          PwrCmp=0
          mdot=0
          MassCmp=0

      END IF

      EER=0
      COP=0

      CalChg=0
      MassAccum=0
      AccumDP=0
      FilterDP=0
  
  ELSE

      !*******Compressor data*******
      PiCmp=CompIN(1)
      HiCmp=CompIN(3)

      Pressure=PiCmp*1000   !RS Comment: Unit Conversion
      Enthalpy=HiCmp*1000   !RS Comment: Unit Conversion
      TiCmp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr) !Compressor Inlet Temperature
      XiCmp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr) !Compressor Inlet Quality

      Quality=1
      TsatiCmp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Compressor Saturation Temperature

      TsupiCmp=TiCmp-TsatiCmp
      IF (TsupiCmp .LT. 0 ) THEN
          TsupiCmp=0
      END IF

      Quality=0
      TsatiCmp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Compressor Saturation Temperature
      TsubiCmp=TsatiCmp-TiCmp
      IF (TsubiCmp .LT. 0 ) THEN
          TsubiCmp=0
      END IF

      IF (XiCmp .GE. 1) THEN
          XiCmp=1
      END IF
      IF (XiCmp .LE. 0) THEN
          XiCmp=0
      END IF
  
      PoCmp=CompIN(2)
      HoCmp=CompOUT(3)

      Pressure=PoCmp*1000   !RS Comment: Unit Conversion
      Enthalpy=HoCmp*1000   !RS Comment: Unit Conversion
      ToCmp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr) !Compressor Outlet Temperature
      XoCmp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr) !Compressor Outlet Quality

      Quality=1
      TsatoCmp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Compressor Saturation Temperature

      TsupoCmp=ToCmp-TsatoCmp
      IF (TsupoCmp .LT. 0 ) THEN
          TsupoCmp=0
      END IF

      Quality=0
      TsatoCmp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Compressor Saturation Temperature
      TsuboCmp=TsatoCmp-ToCmp
      IF (TsuboCmp .LT. 0 ) THEN
          TsuboCmp=0
      END IF

      IF (XoCmp .GE. 1) THEN
          XoCmp=1
      END IF
      IF (XoCmp .LE. 0) THEN
          XoCmp=0
      END IF

      PwrCmp=CompOUT(1)*1000    !RS Comment: Unit Conversion
      MassCmp=CompOUT(6)

      !*******Condenser*******
      PiCnd=CondOUT(1)
      HiCnd=CondOUT(2)

      Pressure=PiCnd*1000   !RS Comment: Unit Conversion
      Enthalpy=HiCnd*1000   !RS Comment: Unit Conversion
      TiCnd=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr) !Condenser Inlet Temperature
      XiCnd=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr) !Condenser Inlet Quality

      Quality=1
      TsatiCnd=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Condenser Saturation Temperature

      TsupiCnd=TiCnd-TsatiCnd
      IF (TsupiCnd .LT. 0 ) THEN
          TsupiCnd=0
      END IF

      Quality=0
      TsatiCnd=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Condenser Saturation Temperature
      TsubiCnd=TsatiCnd-TiCnd
      IF (TsubiCnd .LT. 0 ) THEN
          TsubiCnd=0
      END IF

      IF (XiCnd .GE. 1) THEN
          XiCnd=1
      END IF
      IF (XiCnd .LE. 0) THEN
          XiCnd=0
      END IF

      TdbiCnd=CondIN(5)
      RHiCnd=CondIN(6)

      AirPropOpt=2
      AirProp(1)=TdbiCnd
      AirProp(3)=RHiCnd
      CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
      hAiCnd=AirProp(4)
      TwbiCnd=AirProp(5)
      RhoAiC=AirProp(7)

      CPair=CPA(TdbiCnd)

      PoCnd=CondOUT(5)
      HoCnd=CondOUT(6)

      Pressure=PoCnd*1000   !RS Comment: Unit Conversion
      Enthalpy=HoCnd*1000   !RS Comment: Unit Conversion
      ToCnd=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr) !Condenser Outlet Temperature
      XoCnd=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr) !Condenser Outlet Quality

      Quality=1
      TsatoCnd=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Condenser Saturation Temperature

      TsupoCnd=ToCnd-TsatoCnd
      IF (TsupoCnd .LT. 0 ) THEN
          TsupoCnd=0
      END IF

      Quality=0
      TsatoCnd=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Condenser Saturation Temperature
      TsuboCnd=TsatoCnd-ToCnd
      IF (TsuboCnd .LT. 0 ) THEN
          TsuboCnd=0
      END IF

      IF (XoCnd .GE. 1) THEN
          XoCnd=1
      END IF
      IF (XoCnd .LE. 0) THEN
          XoCnd=0
      END IF

      Qcnd =CondOUT(15)*1000    !RS Comment: Unit Conversion

      TdboCnd=CondOUT(21)
      RHoCnd=CondOUT(22)

      AirPropOpt=2
      AirProp(1)=TdboCnd
      AirProp(3)=RHoCnd
      CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
      WaoCnd=AirProp(2)
      hAoCnd=AirProp(4)
      TwboCnd=AirProp(5)
      RhoAoC=AirProp(7)

      DPaCND=CondOUT(23)*1000   !RS Comment: Unit Conversion

      MassCnd=CondOUT(18)
      MassDisLn=CondOUT(16)
      MassLiqLn=CondOUT(17)

      WeightCndAluminum=CondOUT(28)
      WeightCndCopper=CondOUT(29)

	  !*******Exp. device*******
      IF (ExpDevice .EQ. 3) THEN
	      PiExp=CapTubeIN(2)
	      HiExp=CapTubeIN(3)
	      MassDisTube=CapTubeOUT(5)     
      ELSE
	      PiExp=ShTbIN(2)
	      HiExp=ShTbIN(3)
	      MassDisTube=ShTbOUT(5)
      END IF
      
	  Dshtb=ShTbPAR(2)
	  Qtxv=TxvPAR(1)
	  DcapTube=CapTubePAR(1)
	  LcapTube=CapTubePAR(2)

	  Pressure=PiExp*1000   !RS Comment: Unit Conversion
	  Enthalpy=HiExp*1000   !RS Comment: Unit Conversion
	  TiExp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr) !Expansion Device Inlet Temperature
	  XiExp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr) !Expansion Device Inlet Quality

	  Quality=1
	  TsatiExp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Expansion Device Saturation Temperature

	  TsupiExp=TiExp-TsatiExp
	  IF (TsupiExp .LT. 0 ) THEN
          TsupiExp=0
      END IF

	  Quality=0
	  TsatiExp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Expansion Device Saturation Temperature
	  TsubiExp=TsatiExp-TiExp
	  IF (TsubiExp .LT. 0 ) THEN
          TsubiExp=0
      END IF

	  IF (XiExp .GE. 1) THEN
          XiExp=1
      END IF
	  IF (XiExp .LE. 0) THEN
          XiExp=0
      END IF

      IF (ExpDevice .EQ. 3) THEN
	      PoExp=CapTubeOUT(2)
	      HoExp=CapTubeIN(3)
      ELSE
	      PoExp=ShTbOUT(2)
	      HoExp=ShTbIN(3)
      END IF

	  Pressure=PoExp*1000   !RS Comment: Unit Conversion
	  Enthalpy=HoExp*1000   !RS Comment: Unit Conversion
	  ToExp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr) !Expansion Device Outlet Temperature
	  XoExp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr) !Expansion Device Outlet Quality

	  Quality=1
	  TsatoExp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Expansion Device Saturation Temperature

	  TsupoExp=ToExp-TsatoExp
	  IF (TsupoExp .LT. 0 ) THEN
          TsupoExp=0
      END IF

	  Quality=0
	  TsatoExp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Expansion Device Saturation Temperature

	  TsuboExp=TsatoExp-ToExp
	  IF (TsuboExp .LT. 0 ) THEN
          TsuboExp=0
      END IF

	  IF (XoExp .GE. 1) THEN
          XoExp=1
      END IF
	  IF (XoExp .LE. 0) THEN
          XoExp=0
      END IF

	  !*******Evaporator*******
	  PiEvp=EvapIN(2)
	  HiEvp=EvapIN(3)
      
      PwrODfan=CondPAR(34)*1000 !RS Comment: PwrIDfan is empty unless repopulated; 1000 accounts for CondPAR conversion
      PwrIDfan=EvapPAR(27)*1000 !RS Comment: PwrODfan is empty unless repopulated; 1000 accounts for EvapPAR conversion

	  Pressure=PiEvp*1000   !RS Comment: Unit Conversion
	  Enthalpy=HiEvp*1000   !RS Comment: Unit Conversion
	  TiEvp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr) !Evaporator Inlet Temperature
	  XiEvp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr) !Evaporator Inlet Quality

	  Quality=1
	  TsatiEvp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Evaporator Saturation Temperature

	  TsupiEvp=TiEvp-TsatiEvp
	  IF (TsupiEvp .LT. 0 ) THEN
          TsupiEvp=0
      END IF

	  Quality=0
	  TsatiEvp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Evaporator Saturation Temperature
	  TsubiEvp=TsatiEvp-TiEvp
	  IF (TsubiEvp .LT. 0 ) THEN
          TsubiEvp=0
      END IF

	  IF (XiEvp .GE. 1) THEN
          XiEvp=1
      END IF
	  IF (XiEvp .LE. 0) THEN
          XiEvp=0
      END IF

	  TdbiEvp=EvapIN(5)
	  RHiEvp=EvapIN(6)

	  AirPropOpt=2
	  AirProp(1)=TdbiEvp
	  AirProp(3)=RHiEvp
	  CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
	  hAiEvp=AirProp(4)
	  TwbiEvp=AirProp(5)
	  RhoAiE=AirProp(7)

	  CPair=CPA(TdbiEvp)

	  PoEvp=EvapOUT(1)
	  HoEvp=EvapOUT(2)

	  Pressure=PoEvp*1000   !RS Comment: Unit Conversion
	  Enthalpy=HoEvp*1000   !RS Comment: Unit Conversion
	  ToEvp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr) !Evaporator Outlet Temperature
	  XoEvp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr) !Evaporator Outlet Quality

	  Quality=1
	  TsatoEvp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Evaporator Saturation Temperature

	  TsupoEvp=ToEvp-TsatoEvp
	  IF (TsupoEvp .LT. 0 ) THEN
          TsupoEvp=0
      END IF

	  Quality=0
	  TsatoEvp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)   !Evaporator Saturation Temperature
	  TsuboEvp=TsatoEvp-ToEvp
	  IF (TsuboEvp .LT. 0 ) THEN
          TsuboEvp=0
      END IF

	  IF (XoEvp .GE. 1) THEN
          XoEvp=1
      END IF
	  IF (XoEvp .LE. 0) THEN
          XoEvp=0
      END IF

	  DPaEvp=EvapOUT(19)*1000   !RS Comment: Unit Conversion

	  Qevp =-EvapOUT(11)*1000   !RS Comment: Unit Conversion
	  QevpSens=-EvapOUT(12)*1000    !RS Comment: Unit Conversion
	  IF (ABS(QevpSens) .GT. ABS(Qevp)) THEN !Make sure sensible heat is never higher than total heat. ISI - 08/02/07
	      QevpSens = Qevp
	      hAoEvp=-Qevp/1000/(CFMevp*RhoAiE)+hAiEvp
	      SpecHeat=CPA(TdbiEvp)
	      TdboEvp=-QevpSens/1000/(CFMevp*RhoAiE*SpecHeat)+TdbiEvp
	      AirPropOpt=1
	      AirProp(1)=TdboEvp
	      AirProp(4)=hAoEvp
	      CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
	      TwboEvp=AirProp(5)
	      RHoEvp=AirProp(3)
	  ELSE
    	  TdboEvp=EvapOUT(17)
	      RHoEvp=EvapOUT(18) 
	      AirPropOpt=2
	      AirProp(1)=TdboEvp
	      AirProp(3)=RHoEvp
	      CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
	      TwboEvp=AirProp(5)
	      RhoAoE=AirProp(7)
	  END IF

	  QevpLat=Qevp-QevpSens

	  MassEvp=EvapOUT(14)
	  MassSucLn=EvapOUT(13)+AccumOUT(1)

	  WeightEvpAluminum=EvapOUT(24)
	  WeightEvpCopper=EvapOUT(25)

	  IF (IsCoolingMode .GT. 0) THEN !ISI - 11/03/2008
	    DrawBlow=EvapPAR(28)
	  ELSE
	    DrawBlow=CondPAR(35)
	  END IF

	  IF (DrawBlow .LT. 2) THEN !draw through
		  IF (IsCoolingMode .GT. 0) THEN
			  EER=((Qevp-PwrIDfan)/UnitPwr)/(PwrCmp+PwrIDfan+PwrODfan)
			  COP=(Qevp-PwrIDfan)/(PwrCmp+PwrIDfan+PwrODfan)
			  SHR=(QevpSens-PwrIDfan)/(Qevp-PwrIDfan)
		  ELSE
			  EER=((Qcnd+PwrIDfan)/UnitPwr)/(PwrCmp+PwrIDfan+PwrODfan)
			  COP=(Qcnd+PwrIDfan)/(PwrCmp+PwrIDfan+PwrODfan)
			  SHR=(Qcnd+PwrIDfan)/(Qcnd+PwrIDfan)
		  END IF
	  ELSE
		  IF (IsCoolingMode .GT. 0) THEN
			  EER=(Qevp/UnitPwr)/(PwrCmp+PwrIDfan+PwrODfan)
			  COP=Qevp/(PwrCmp+PwrIDfan+PwrODfan)
			  SHR=QevpSens/Qevp
		  ELSE
			  EER=(Qcnd/UnitPwr)/(PwrCmp+PwrIDfan+PwrODfan)
			  COP=Qcnd/(PwrCmp+PwrIDfan+PwrODfan)
			  SHR=Qcnd/Qcnd
		  END IF
	  END IF

      CalChg=CALCHG*UnitM
      Dshtb=ShTbPAR(2)*1000 !RS Comment: Unit Conversion
	  DcapTube=CapTubePAR(1)*1000   !RS Comment: Unit Conversion
	  LcapTube=CapTubePAR(2)*1000   !RS Comment: Unit Conversion

      MassAccum=AccumOUT(1)
      AccumDP=AccumOUT(5)
      FilterDP=FilterOUT(1)
  
  END IF
  
  IF (SystemType .EQ. 4) THEN !Reheat system
	  IF (DrawBlow .LT. 2) THEN !draw through
		  Qtot=-Qevp+Qcnd+PwrIDfan
		  QtotSens=-QevpSens+Qcnd+PwrIDfan
	  ELSE
		  Qtot=-Qevp+Qcnd
		  QtotSens=-QevpSens+Qcnd	  
	  END IF
	  	
	  CPair=CPA(REAL(TdboCnd))
	  hAoCnd=Qtot/1000/(CFMevp*StandardDensity)+hAiEvp
	  TdboCnd=QtotSens/1000/(CFMevp*StandardDensity*CPair)+TdbiEvp

	  AirPropOpt=1
	  AirProp(1)=TdboCnd
	  AirProp(4)=hAoCnd
	  CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
	  WaoCnd=AirProp(2)
	  AirProp(3)=RHoCnd	  
	  TwboCnd=AirProp(5)
	  RhoAoC=AirProp(7)

	  Qevp=Qtot
	  QevpSens=QtotSens
	  SHR=QevpSens/Qevp
  END IF

  mdot=MdotR*3600   !RS Comment: Unit Conversion
      
  !Convert output data to IP unit
  IF (Unit .EQ. 2) THEN
      PICMP=PiCmp/UnitP     !RS Comment: Unit Conversion, from kPa to psi
      HICMP=HiCmp/UnitH     !RS Comment: Unit Conversion, from kJ/kg to Btu/lbm
      TICMP=TiCmp*1.8+32    !RS Comment: Unit Conversion, from C to F
      POCMP=PoCmp/UnitP     !RS Comment: Unit Conversion, from kPa to psi
      HOCMP=HoCmp/UnitH     !RS Comment: Unit Conversion, from kJ/kg to Btu/lbm
      TOCMP=ToCmp*1.8+32    !RS Comment: Unit Conversion, from C to F
      TSUPICMP=TsupiCmp*1.8
	  TSUPOCMP=TsupoCmp*1.8
      TSUBICMP=TsubiCmp*1.8
	  TSUBOCMP=TsuboCmp*1.8
	  TSATICMP=TsatiCmp*1.8+32  !RS Comment: Unit Conversion, from C to F
	  TSATOCMP=TsatoCmp*1.8+32  !RS Comment: Unit Conversion, from C to F
	  MDOT=mdot/UrefFlow

      PICND=PiCnd/UnitP     !RS Comment: Unit Conversion, from kPa to psi
	  POCND=PoCnd/UnitP     !RS Comment: Unit Conversion, from kPa to psi
	  TICND=TiCnd*1.8+32    !RS Comment: Unit Conversion, from C to F
	  TOCND=ToCnd*1.8+32    !RS Comment: Unit Conversion, from C to F
	  HICND=HiCnd/UnitH     !RS Comment: Unit Conversion, from kJ/kg to Btu/lbm
	  HOCND=HoCnd/UnitH     !RS Comment: Unit Conversion, from kJ/kg to Btu/lbm
      TSUPICND=TsupiCnd*1.8
	  TSUPOCND=TsupoCnd*1.8
      TSUBICND=TsubiCnd*1.8
	  TSUBOCND=TsuboCnd*1.8
	  TSATICND=TsatiCnd*1.8+32  !RS Comment: Unit Conversion, from C to F
	  TSATOCND=TsatoCnd*1.8+32  !RS Comment: Unit Conversion, from C to F
	  QCND=Qcnd/UnitPwr
	  TDBICND=TdbiCnd*1.8+32    !RS Comment: Unit Conversion, from C to F
	  TDBOCND=TdboCnd*1.8+32    !RS Comment: Unit Conversion, from C to F
	  TWBICND=TwbiCnd*1.8+32    !RS Comment: Unit Conversion, from C to F
	  TWBOCND=TwboCnd*1.8+32    !RS Comment: Unit Conversion, from C to F
	  CFMCND=CFMcnd/UnitArFlw
	  DPACND=DPaCnd/UairPres

      PIEXP=PiExp/UnitP     !RS Comment: Unit Conversion, from kPa to psi
      HIEXP=HiExp/UnitH     !RS Comment: Unit Conversion, from kJ/kg to Btu/lbm
      TIEXP=TiExp*1.8+32    !RS Comment: Unit Conversion, from C to F
	  POEXP=PoExp/UnitP     !RS Comment: Unit Conversion, from kPa to psi
	  HOEXP=HoExp/UnitH     !RS Comment: Unit Conversion, from kJ/kg to Btu/lbm
	  TOEXP=ToExp*1.8+32    !RS Comment: Unit Conversion, from C to F
      TSUPIEXP=TsupiExp*1.8
	  TSUPOEXP=TsupoExp*1.8
      TSUBIEXP=TsubiExp*1.8
	  TSUBOEXP=TsuboExp*1.8
	  TSATIEXP=TsatiExp*1.8+32  !RS Comment: Unit Conversion, from C to F
	  TSATOEXP=TsatoExp*1.8+32  !RS Comment: Unit Conversion, from C to F

      PIEVP=PiEvp/UnitP     !RS Comment: Unit Conversion, from kPa to psi
	  POEVP=PoEvp/UnitP     !RS Comment: Unit Conversion, from kPa to psi
	  TIEVP=TiEvp*1.8+32    !RS Comment: Unit Conversion, from C to F
	  TOEVP=ToEvp*1.8+32    !RS Comment: Unit Conversion, from C to F
	  HIEVP=HiEvp/UnitH     !RS Comment: Unit Conversion, from kJ/kg to Btu/lbm
	  HOEVP=HoEvp/UnitH     !RS Comment: Unit Conversion, from kJ/kg to Btu/lbm
      TSUPIEVP=TsupiEvp*1.8
	  TSUPOEVP=TsupoEvp*1.8
      TSUBIEVP=TsubiEvp*1.8
	  TSUBOEVP=TsuboEvp*1.8
	  TSATIEVP=TsatiEvp*1.8+32  !RS Comment: Unit Conversion, from C to F
	  TSATOEVP=TsatoEvp*1.8+32  !RS Comment: Unit Conversion, from C to F

	  QEVP=Qevp/UnitPwr
	  QEVPSENS=QevpSens/UnitPwr
	  QEVPLAT=QevpLat/UnitPwr
	  TDBIEVP=TdbiEvp*1.8+32    !RS Comment: Unit Conversion, from C to F
	  TDBOEVP=TdboEvp*1.8+32    !RS Comment: Unit Conversion, from C to F
	  TWBIEVP=TwbiEvp*1.8+32    !RS Comment: Unit Conversion, from C to F
	  TWBOEVP=TwboEvp*1.8+32    !RS Comment: Unit Conversion, from C to F
	  CFMEVP=CFMevp/UnitArFlw
	  DPAEVP=DPaEvp/UairPres

	  CALCHG=CalChg/UnitM       !RS Comment: Unit Conversion, from kg to lbm
	  MassCmp=MassCmp/UnitM     !RS Comment: Unit Conversion, from kg to lbm
	  MassCnd=MassCnd/UnitM     !RS Comment: Unit Conversion, from kg to lbm
	  MassEvp=MassEvp/UnitM     !RS Comment: Unit Conversion, from kg to lbm
	  MassSucLn=MassSucLn/UnitM !RS Comment: Unit Conversion, from kg to lbm
	  MassDisLn=MassDisLn/UnitM !RS Comment: Unit Conversion, from kg to lbm
	  MassLiqLn=MassLiqLn/UnitM !RS Comment: Unit Conversion, from kg to lbm
	  MassDisTube=MassDisTube/UnitM !RS Comment: Unit Conversion, from kg to lbm
	  MassAccum=MassAccum/UnitM     !RS Comment: Unit Conversion, from kg to lbm
      Dshtb=ShTbPAR(2)/UnitL*12         !RS Comment: Unit Conversion, from m to in
	  DcapTube=CapTubePAR(1)/UnitL*12   !RS Comment: Unit Conversion, from m to in
	  LcapTube=CapTubePAR(2)/UnitL*12   !RS Comment: Unit Conversion, from m to in

      TaoEVP=TaoEVP*1.8+32  !RS Comment: Unit Conversion, from C to F
	  TaoCND=TaoCND*1.8+32  !RS Comment: Unit Conversion, from C to F
    
	  AccumDP=AccumDP/UnitP     !RS Comment: Unit Conversion, from kPa to psi
	  FilterDP=FilterDP/UnitP   !RS Comment: Unit Conversion, from kPa to psi

	  WeightEvpAluminum=WeightEvpAluminum/UnitM !RS Comment: Unit Conversion, from kg to lbm
	  WeightEvpCopper=WeightEvpCopper/UnitM     !RS Comment: Unit Conversion, from kg to lbm

	  WeightCndAluminum=WeightCndAluminum/UnitM !RS Comment: Unit Conversion, from kg to lbm
	  WeightCndCopper=WeightCndCopper/UnitM     !RS Comment: Unit Conversion, from kg to lbm

	  WeightSucLn=WeightSucLn/UnitM             !RS Comment: Unit Conversion, from kg to lbm
	  WeightDisLn=WeightDisLn/UnitM             !RS Comment: Unit Conversion, from kg to lbm
	  WeightLiqLn=WeightLiqLn/UnitM             !RS Comment: Unit Conversion, from kg to lbm
	  WeightValveIDCLn=WeightValveIDCLn/UnitM   !RS Comment: Unit Conversion, from kg to lbm
	  WeightValveODCLn=WeightValveODCLn/UnitM   !RS Comment: Unit Conversion, from kg to lbm

	  CondLiqTubeLength=CondLiqTubeLength/UnitL             !RS Comment: Unit Conversion, from m to ft
	  CondVapTubeLength=CondVapTubeLength/UnitL             !RS Comment: Unit Conversion, from m to ft
	  CondTwoPhaseTubeLength=CondTwoPhaseTubeLength/UnitL   !RS Comment: Unit Conversion, from m to ft

	  EvapLiqTubeLength=EvapLiqTubeLength/UnitL             !RS Comment: Unit Conversion, from m to ft
	  EvapVapTubeLength=EvapVapTubeLength/UnitL             !RS Comment: Unit Conversion, from m to ft
	  EvapTwoPhaseTubeLength=EvapTwoPhaseTubeLength/UnitL   !RS Comment: Unit Conversion, from m to ft

	  !Conver Pressure to gauge basis
      !RS Comment: Unit Conversion, from kPa to psi
	  PiCmp=PiCmp-BaroPressure/UnitP
	  PoCmp=PoCmp-BaroPressure/UnitP
	  PiCnd=PiCnd-BaroPressure/UnitP
	  PoCnd=PoCnd-BaroPressure/UnitP
	  PiEvp=PiEvp-BaroPressure/UnitP
	  PoEvp=PoEvp-BaroPressure/UnitP
	  PiExp=PiExp-BaroPressure/UnitP
	  PoExp=PoExp-BaroPressure/UnitP

  END IF

  !Conver Pressure to gauge basis
  IF (Unit .EQ. 1) THEN
	  PiCmp=PiCmp-BaroPressure
	  PoCmp=PoCmp-BaroPressure
	  PiCnd=PiCnd-BaroPressure
	  PoCnd=PoCnd-BaroPressure
	  PiEvp=PiEvp-BaroPressure
	  PoEvp=PoEvp-BaroPressure
	  PiExp=PiExp-BaroPressure
	  PoExp=PoExp-BaroPressure
  END IF
CoilSurfTemp = 0.0
CoilSurfTemp=CoilParams(2)%TSurfCoil*9/5+32 !RS Comment: Unit Conversion, from C to F
  !VB program report format
  WRITE(5,*)
  WRITE(5,FMT_2200)'Title (ver. 2.0 12/17/09): ',Title
  WRITE(5,*)
  WRITE(5,FMT_2204)'*** System Performance Summary ***'
  WRITE(5,*)
  WRITE(5,FMT_2208)'Evaporator gross capacity       ',QEVP,CapUnit
  WRITE(5,FMT_2208)'Condenser gross capacity        ',QCND,CapUnit
  WRITE(5,FMT_2208)'Gross sensible capacity         ',QEVPSENS,CapUnit
  WRITE(5,FMT_2208)'Gross latent capacity           ',QEVPLAT,CapUnit
  WRITE(5,FMT_2208)'Compressor power                ',PWRCMP,PwrUnit
  WRITE(5,FMT_2208)'Refrigerant mass flow rate      ',MDOT,mdotUnit
  WRITE(5,FMT_2208)'COP (coefficient of performance)',COP,NoUnit
  WRITE(5,FMT_2208)'EER (energy efficiency ratio)   ',EER,EERunit
  WRITE(5,FMT_2208)'SHR (sensible heat ratio)       ',SHR,NoUnit
  WRITE(5,FMT_2208)'Condenser subcooling            ',TSUBOCND,DTunit
  WRITE(5,FMT_2208)'Expansion device subcooling     ',TSUBIEXP,DTunit
  WRITE(5,FMT_2208)'Evaporator superheat            ',TSUPOEVP,DTunit
  WRITE(5,FMT_2208)'Compressor superheat            ',TSUPICMP,DTunit
  WRITE(5,FMT_2208)'System charge                   ',CALCHG,MassUnit
  WRITE(5,FMT_2208)'Refrigerant in compressor       ',MassCmp,MassUnit
  WRITE(5,FMT_2208)'Refrigerant in condenser        ',MassCnd,MassUnit
  WRITE(5,FMT_2208)'Refrigerant in evaporator       ',MassEvp,MassUnit
  WRITE(5,FMT_2208)'Refrigerant in suction line     ',MassSucLn,MassUnit
  WRITE(5,FMT_2208)'Refrigerant in discharge line   ',MassDisLn,MassUnit
  WRITE(5,FMT_2208)'Refrigerant in liquid line      ',MassLiqLn,MassUnit
  WRITE(5,FMT_2208)'Refrigerant in distributor tubes',MassDistube,MassUnit
  WRITE(5,FMT_2208)'Refrigerant in accumulator      ',MassAccum,MassUnit
  WRITE(5,FMT_2208)'Short tube diameter             ',Dshtb,MiniLunit
  WRITE(5,FMT_2208)'TXV capacity                    ',Qtxv,SysUnit
  WRITE(5,FMT_2208)'Capillary tube diameter         ',DcapTube,MiniLunit !Added for Cap. tube 04/13/2009 - ISI
  WRITE(5,FMT_2208)'Capillary tube length           ',LcapTube,MiniLunit !Added for Cap. tube 04/13/2009 - ISI 
  WRITE(5,*)
  WRITE(5,FMT_2204)'***** Refrigerant Side Data *****'
  WRITE(5,*)
  IF (Unit .EQ. 2) THEN
      WRITE(5,FMT_2212)'Location               ','Temperature (F)','Pressure (psig)','Enthalpy (Btu/lbm)','Saturation Temperature (F)','Quality (%)','Subcooling (R)','Superheat (R)','Location               '
  ELSE
      WRITE(5,FMT_2212)'Location               ','Temperature (C)','Pressure (kPa)','Enthalpy (kJ/kg)','Saturation Temperature (C)','Quality (%)','Subcooling (K)','Superheat (K)','Location               '
  END IF
  WRITE(5,FMT_2216)'Compressor suction     ',TICMP,PICMP,HICMP,TSATICMP,XICMP*100,TSUBICMP,TSUPICMP,'Compressor suction     '
  WRITE(5,FMT_2216)'Compressor discharge   ',TOCMP,POCMP,HOCMP,TSATOCMP,XOCMP*100,TSUBOCMP,TSUPOCMP,'Compressor discharge   '
  WRITE(5,FMT_2216)'Condenser inlet        ',TICND,PICND,HICND,TSATICND,XICND*100,TSUBICND,TSUPICND,'Condenser inlet        '
  WRITE(5,FMT_2216)'Condenser outlet       ',TOCND,POCND,HOCND,TSATOCND,XOCND*100,TSUBOCND,TSUPOCND,'Condenser outlet       '
  WRITE(5,FMT_2216)'Expansion device inlet ',TIEXP,PIEXP,HIEXP,TSATIEXP,XIEXP*100,TSUBIEXP,TSUPIEXP,'Expansion device inlet '
  WRITE(5,FMT_2216)'Expansion device outlet',TOEXP,POEXP,HOEXP,TSATOEXP,XOEXP*100,TSUBOEXP,TSUPOEXP,'Expansion device outlet'
  WRITE(5,FMT_2216)'Evaporator inlet       ',TIEVP,PIEVP,HIEVP,TSATIEVP,XIEVP*100,TSUBIEVP,TSUPIEVP,'Evaporator inlet       '
  WRITE(5,FMT_2216)'Evaporator outlet      ',TOEVP,POEVP,HOEVP,TSATOEVP,XOEVP*100,TSUBOEVP,TSUPOEVP,'Evaporator outlet      '
  WRITE(5,*)
  WRITE(5,FMT_2204)'********* Air Side Data *********'
  WRITE(5,*)
  IF (Unit .EQ. 2) THEN
      WRITE(5,FMT_2220)'Location         ','Dry bulb temperature (F)','Wet bulb temperature (F)','Relative Humidity (%)','Volumetric flow rate (CFM)','Pressure Drop (in-H2O)','Location         '
  ELSE
      WRITE(5,FMT_2220)'Location         ','Dry bulb temperature (C)','Wet bulb temperature (C)','Relative Humidity (%)','Volumetric flow rate (m^3/min)','Pressure Drop (Pa)','Location         '
  END IF
  WRITE(5,FMT_2224)'Condenser inlet  ',TDBICND,TWBICND,RHICND*100,CFMCND,0.0d0,'Condenser inlet  '
  WRITE(5,FMT_2224)'Condenser outlet ',TDBOCND,TWBOCND,RHOCND*100,CFMCND,DPACND,'Condenser outlet '
  WRITE(5,FMT_2224)'Evaporator inlet ',TDBIEVP,TWBIEVP,RHIEVP*100,CFMEVP,0.0d0,'Evaporator inlet '
  WRITE(5,FMT_2224)'Evaporator outlet',TDBOEVP,TWBOEVP,RHOEVP*100,CFMEVP,DPAEVP,'Evaporator outlet'
  WRITE(5,*)
  WRITE(5,FMT_2204)'********* Pressure Drop *********'
  WRITE(5,*)
  WRITE(5,FMT_2208)'Accumulator                     ',AccumDP,Punit
  WRITE(5,FMT_2208)'Filter Drier                    ',FilterDP,Punit   
  WRITE(5,*)
  WRITE(5,FMT_2204)'*********** Material ************'
  WRITE(5,*)
  WRITE(5,FMT_2208)'Aluminum Evaporator             ',WeightEvpAluminum,MassUnit
  WRITE(5,FMT_2208)'Aluminum Condenser              ',WeightCndAluminum,MassUnit
  WRITE(5,FMT_2208)'Copper Evaporator               ',WeightEvpCopper,MassUnit
  WRITE(5,FMT_2208)'Copper Condenser                ',WeightCndCopper,MassUnit
  WRITE(5,FMT_2208)'Copper Suction Line             ',WeightSucLn,MassUnit
  WRITE(5,FMT_2208)'Copper Discharge Line           ',WeightDisLn,MassUnit
  WRITE(5,FMT_2208)'Copper Valve-Indoor Coil Line   ',WeightValveIDCLn,MassUnit
  WRITE(5,FMT_2208)'Copper Valve-Outdoor Coil Line  ',WeightValveODCLn,MassUnit
  WRITE(5,FMT_2208)'Copper Liquid Line              ',WeightLiqLn,MassUnit
  WRITE(5,*)
  WRITE(5,FMT_2204)'*** Refrigerant Distributions ***'
  WRITE(5,*)
  WRITE(5,FMT_2208)'Condenser liquid length         ',CondLiqTubeLength,Lunit
  WRITE(5,FMT_2208)'Condenser two-phase length      ',CondTwoPhaseTubeLength,Lunit
  WRITE(5,FMT_2208)'Condenser vapor length          ',CondVapTubeLength,Lunit
  WRITE(5,FMT_2208)'Evaporator two-phase length     ',EvapTwoPhaseTubeLength,Lunit
  WRITE(5,FMT_2208)'Evaporator vapor length         ',EvapVapTubeLength,Lunit
  WRITE(5,*)
  WRITE(5,FMT_2204)'*** Frost Parameters ***'
  WRITE(5,*)
  WRITE(5,FMT_2208)'Evaporator Tube Area            ',EvapTubeArea,NoUnit
  WRITE(5,FMT_2208)'Evaporator Fin Area             ',EvapFinArea,NoUnit
  WRITE(5,FMT_2208)'Evaporator Total Area           ',EvapTotArea,NoUnit
  WRITE(5,FMT_2208)'Evaporator Bare Area            ',EvapBareArea,NoUnit
  WRITE(5,FMT_2208)'Evaporator Min Area             ',EvapMinArea,NoUnit
  WRITE(5,FMT_2208)'Condenser Tube Area             ',CondTubeArea,NoUnit
  WRITE(5,FMT_2208)'Condenser Fin Area              ',CondFinArea,NoUnit
  WRITE(5,FMT_2208)'Evaporator Surface Temperature  ',CoilSurfTemp,NoUnit

!!VL: Previously: 
!!1007 FORMAT(A17,A42)
!!1008 FORMAT(A13,F10.3,A10,A10,A13,F10.3,A10)
!!2001 FORMAT(A13,F10.3,A10)
!!2004 FORMAT(A56,F10.3,A10)
!!2007 FORMAT(A16,F10.3,A9)
!!2009 FORMAT(60(A16,','))
!!2100 FORMAT(60(E,','))
!!
!!2200 FORMAT(A32,',',A50)
!!2204 FORMAT(A)
!!2208 FORMAT(A33,',',F40.3,',',A15)
!!2212 FORMAT(A24,',',A18,',',A18,',',A21,',',A29,',',A14,',',A17,',',A16,',',A26)
!!2216 FORMAT(A24,',',F18.3,',',F18.3,',',F21.3,',',F29.3,',',F14.1,',',F17.3,',',F16.3,',',A26)
!!2220 FORMAT(A18,',',A27,',',A27,',',A24,',',A33,',',A25,',',A20)
!!2224 FORMAT(A18,',',F27.3,',',F27.3,',',F24.1,',',F33.3,',',F25.3,',',A20) 

RETURN

END SUBROUTINE
