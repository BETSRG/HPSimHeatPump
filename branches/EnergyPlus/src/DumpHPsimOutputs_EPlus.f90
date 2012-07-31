SUBROUTINE DumpOutputs

USE FluidProperties_HPSim
USE AirPropMod
USE DataSimulation

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
INTEGER            :: RefrigIndex =0
REAL Temperature,Quality,Pressure,Enthalpy
CHARACTER (len=50) :: Title !Output file title

REAL :: CoilSurfTemp = 0.0

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

      Pressure=PiCmp*1000
      Enthalpy=HiCmp*1000
      TiCmp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
      XiCmp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)

      Quality=1
      TsatiCmp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)

      TsupiCmp=TiCmp-TsatiCmp
      IF (TsupiCmp .LT. 0 ) TsupiCmp=0

      Quality=0
      TsatiCmp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)
      TsubiCmp=TsatiCmp-TiCmp
      IF (TsubiCmp .LT. 0 ) TsubiCmp=0

      IF (XiCmp .GE. 1) XiCmp=1
      IF (XiCmp .LE. 0) XiCmp=0
  
      PoCmp=CompIN(2)
      HoCmp=CompOUT(3)

      Pressure=PoCmp*1000
      Enthalpy=HoCmp*1000
      ToCmp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
      XoCmp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)

      Quality=1
      TsatoCmp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)

      TsupoCmp=ToCmp-TsatoCmp
      IF (TsupoCmp .LT. 0 ) TsupoCmp=0

      Quality=0
      TsatoCmp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)
      TsuboCmp=TsatoCmp-ToCmp
      IF (TsuboCmp .LT. 0 ) TsuboCmp=0

      IF (XoCmp .GE. 1) XoCmp=1
      IF (XoCmp .LE. 0) XoCmp=0

      PwrCmp=CompOUT(1)*1000
      MassCmp=CompOUT(6)

      !*******Condenser*******
      PiCnd=CondOUT(1)
      HiCnd=CondOUT(2)

      Pressure=PiCnd*1000
      Enthalpy=HiCnd*1000
      TiCnd=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
      XiCnd=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)

      Quality=1
      TsatiCnd=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)

      TsupiCnd=TiCnd-TsatiCnd
      IF (TsupiCnd .LT. 0 ) TsupiCnd=0

      Quality=0
      TsatiCnd=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)
      TsubiCnd=TsatiCnd-TiCnd
      IF (TsubiCnd .LT. 0 ) TsubiCnd=0

      IF (XiCnd .GE. 1) XiCnd=1
      IF (XiCnd .LE. 0) XiCnd=0

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

      Pressure=PoCnd*1000
      Enthalpy=HoCnd*1000
      ToCnd=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
      XoCnd=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)

      Quality=1
      TsatoCnd=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)

      TsupoCnd=ToCnd-TsatoCnd
      IF (TsupoCnd .LT. 0 ) TsupoCnd=0

      Quality=0
      TsatoCnd=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)
      TsuboCnd=TsatoCnd-ToCnd
      IF (TsuboCnd .LT. 0 ) TsuboCnd=0

      IF (XoCnd .GE. 1) XoCnd=1
      IF (XoCnd .LE. 0) XoCnd=0

      !CFMcnd=SCFMcnd
      Qcnd =CondOUT(15)*1000

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

      DPaCND=CondOUT(23)*1000

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

	      Pressure=PiEvp*1000
	      Enthalpy=HiEvp*1000
	      TiEvp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
	      XiEvp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)

	      Quality=1
	      TsatiEvp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)

	      TsupiEvp=TiEvp-TsatiEvp
	      IF (TsupiEvp .LT. 0 ) TsupiEvp=0

	      Quality=0
	      TsatiEvp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)
	      TsubiEvp=TsatiEvp-TiEvp
	      IF (TsubiEvp .LT. 0 ) TsubiEvp=0

	      IF (XiEvp .GE. 1) XiEvp=1
	      IF (XiEvp .LE. 0) XiEvp=0

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

	      Pressure=PoEvp*1000
	      Enthalpy=HoEvp*1000
	      ToEvp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
	      XoEvp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)

	      Quality=1
	      TsatoEvp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)

	      TsupoEvp=ToEvp-TsatoEvp
	      IF (TsupoEvp .LT. 0 ) TsupoEvp=0

	      Quality=0
	      TsatoEvp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)
	      TsuboEvp=TsatoEvp-ToEvp
	      IF (TsuboEvp .LT. 0 ) TsuboEvp=0

	      IF (XoEvp .GE. 1) XoEvp=1
	      IF (XoEvp .LE. 0) XoEvp=0

	      !CFMevp=StdCFMevp

	      DPaEvp=EvapOUT(19)*1000

	      Qevp =-EvapOUT(11)*1000
	      QevpSens=-EvapOUT(12)*1000
	      IF (ABS(QevpSens) .GT. ABS(Qevp)) THEN !Make sure sensible heat is never higher than total heat. ISI - 08/02/07
	          QevpSens = Qevp
	          !hAoEvp=-Qevp/1000/(StdCFMevp*StandardDensity)+hAiEvp
	          !TdboEvp=-QevpSens/1000/(StdCFMevp*StandardDensity*StandardSpecHeat)+TdbiEvp
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

	      IF (XiExp .GE. 1) XiExp=1
	      IF (XiExp .LE. 0) XiExp=0

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
          TsatoCmp=(TSOCMP-32)*5/9
          TsupoCmp=0
          TsuboCmp=0

          PiCmp=CompIN(1) !PoEvp
          HiCmp=CompIN(3) !HoEvp

	      Pressure=PiCmp*1000
	      Enthalpy=HiCmp*1000
	      TiCmp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
	      XiCmp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)

          !TiCmp=ToEvp
          !XiCmp=XoEvp
          TsatiCmp=(TSICMP-32)*5/9 !TsatoEvp
          TsupiCmp=TsupoEvp
          TsubiCmp=TsuboEvp

          PwrCmp=0
          mdot=0
          MassCmp=0

      ELSE

          !*******Condenser*******
          PiCnd=CondOUT(1)
          HiCnd=CondOUT(2)

          Pressure=PiCnd*1000
          Enthalpy=HiCnd*1000
          TiCnd=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
          XiCnd=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)

          Quality=1
          TsatiCnd=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)

          TsupiCnd=TiCnd-TsatiCnd
          IF (TsupiCnd .LT. 0 ) TsupiCnd=0

          Quality=0
          TsatiCnd=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)
          TsubiCnd=TsatiCnd-TiCnd
          IF (TsubiCnd .LT. 0 ) TsubiCnd=0

          IF (XiCnd .GE. 1) XiCnd=1
          IF (XiCnd .LE. 0) XiCnd=0

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

          Pressure=PoCnd*1000
          Enthalpy=HoCnd*1000
          ToCnd=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
          XoCnd=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)

          Quality=1
          TsatoCnd=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)

          TsupoCnd=ToCnd-TsatoCnd
          IF (TsupoCnd .LT. 0 ) TsupoCnd=0

          Quality=0
          TsatoCnd=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)
          TsuboCnd=TsatoCnd-ToCnd
          IF (TsuboCnd .LT. 0 ) TsuboCnd=0

          IF (XoCnd .GE. 1) XoCnd=1
          IF (XoCnd .LE. 0) XoCnd=0

          !CFMcnd=StdCFMcnd
          Qcnd =CondOUT(15)*1000

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

          DPaCND=CondOUT(23)*1000

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

          TsatoCmp=(TSOCMP-32)*5/9 !TsatiCnd
          TsupoCmp=TsupiCnd
          TsuboCmp=TsubiCnd

          IF (XoCmp .GE. 1) XoCmp=1
          IF (XoCmp .LE. 0) XoCmp=0

          PiCmp=0
          HiCmp=0
          TiCmp=0
          XiCmp=0
          TsatiCmp=(TSICMP-32)*5/9
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

      Pressure=PiCmp*1000
      Enthalpy=HiCmp*1000
      TiCmp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
      XiCmp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)

      Quality=1
      TsatiCmp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)

      TsupiCmp=TiCmp-TsatiCmp
      IF (TsupiCmp .LT. 0 ) TsupiCmp=0

      Quality=0
      TsatiCmp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)
      TsubiCmp=TsatiCmp-TiCmp
      IF (TsubiCmp .LT. 0 ) TsubiCmp=0

      IF (XiCmp .GE. 1) XiCmp=1
      IF (XiCmp .LE. 0) XiCmp=0
  
      PoCmp=CompIN(2)
      HoCmp=CompOUT(3)

      Pressure=PoCmp*1000
      Enthalpy=HoCmp*1000
      ToCmp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
      XoCmp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)

      Quality=1
      TsatoCmp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)

      TsupoCmp=ToCmp-TsatoCmp
      IF (TsupoCmp .LT. 0 ) TsupoCmp=0

      Quality=0
      TsatoCmp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)
      TsuboCmp=TsatoCmp-ToCmp
      IF (TsuboCmp .LT. 0 ) TsuboCmp=0

      IF (XoCmp .GE. 1) XoCmp=1
      IF (XoCmp .LE. 0) XoCmp=0

      PwrCmp=CompOUT(1)*1000
      MassCmp=CompOUT(6)

      !*******Condenser*******
      PiCnd=CondOUT(1)
      HiCnd=CondOUT(2)

      Pressure=PiCnd*1000
      Enthalpy=HiCnd*1000
      TiCnd=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
      XiCnd=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)

      Quality=1
      TsatiCnd=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)

      TsupiCnd=TiCnd-TsatiCnd
      IF (TsupiCnd .LT. 0 ) TsupiCnd=0

      Quality=0
      TsatiCnd=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)
      TsubiCnd=TsatiCnd-TiCnd
      IF (TsubiCnd .LT. 0 ) TsubiCnd=0

      IF (XiCnd .GE. 1) XiCnd=1
      IF (XiCnd .LE. 0) XiCnd=0

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

      Pressure=PoCnd*1000
      Enthalpy=HoCnd*1000
      ToCnd=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
      XoCnd=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)

      Quality=1
      TsatoCnd=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)

      TsupoCnd=ToCnd-TsatoCnd
      IF (TsupoCnd .LT. 0 ) TsupoCnd=0

      Quality=0
      TsatoCnd=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)
      TsuboCnd=TsatoCnd-ToCnd
      IF (TsuboCnd .LT. 0 ) TsuboCnd=0

      IF (XoCnd .GE. 1) XoCnd=1
      IF (XoCnd .LE. 0) XoCnd=0

      !CFMcnd=StdCFMcnd
      Qcnd =CondOUT(15)*1000

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

      DPaCND=CondOUT(23)*1000

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

	  Pressure=PiExp*1000
	  Enthalpy=HiExp*1000
	  TiExp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
	  XiExp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)

	  Quality=1
	  TsatiExp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)

	  TsupiExp=TiExp-TsatiExp
	  IF (TsupiExp .LT. 0 ) TsupiExp=0

	  Quality=0
	  TsatiExp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)
	  TsubiExp=TsatiExp-TiExp
	  IF (TsubiExp .LT. 0 ) TsubiExp=0

	  IF (XiExp .GE. 1) XiExp=1
	  IF (XiExp .LE. 0) XiExp=0

      IF (ExpDevice .EQ. 3) THEN
	      PoExp=CapTubeOUT(2)
	      HoExp=CapTubeIN(3)
      ELSE
	      PoExp=ShTbOUT(2)
	      HoExp=ShTbIN(3)
      END IF

	  Pressure=PoExp*1000
	  Enthalpy=HoExp*1000
	  ToExp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
	  XoExp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)

	  Quality=1
	  TsatoExp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)

	  TsupoExp=ToExp-TsatoExp
	  IF (TsupoExp .LT. 0 ) TsupoExp=0

	  Quality=0
	  TsatoExp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)

	  TsuboExp=TsatoExp-ToExp
	  IF (TsuboExp .LT. 0 ) TsuboExp=0

	  IF (XoExp .GE. 1) XoExp=1
	  IF (XoExp .LE. 0) XoExp=0

	  !*******Evaporator*******
	  PiEvp=EvapIN(2)
	  HiEvp=EvapIN(3)

	  Pressure=PiEvp*1000
	  Enthalpy=HiEvp*1000
	  TiEvp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
	  XiEvp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)

	  Quality=1
	  TsatiEvp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)

	  TsupiEvp=TiEvp-TsatiEvp
	  IF (TsupiEvp .LT. 0 ) TsupiEvp=0

	  Quality=0
	  TsatiEvp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)
	  TsubiEvp=TsatiEvp-TiEvp
	  IF (TsubiEvp .LT. 0 ) TsubiEvp=0

	  IF (XiEvp .GE. 1) XiEvp=1
	  IF (XiEvp .LE. 0) XiEvp=0

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

	  Pressure=PoEvp*1000
	  Enthalpy=HoEvp*1000
	  ToEvp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
	  XoEvp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)

	  Quality=1
	  TsatoEvp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)

	  TsupoEvp=ToEvp-TsatoEvp
	  IF (TsupoEvp .LT. 0 ) TsupoEvp=0

	  Quality=0
	  TsatoEvp=PQ(Ref$, Pressure, Quality, 'temperature', RefrigIndex,RefPropErr)
	  TsuboEvp=TsatoEvp-ToEvp
	  IF (TsuboEvp .LT. 0 ) TsuboEvp=0

	  IF (XoEvp .GE. 1) XoEvp=1
	  IF (XoEvp .LE. 0) XoEvp=0

	  !CFMevp=StdCFMevp

	  DPaEvp=EvapOUT(19)*1000

	  Qevp =-EvapOUT(11)*1000
	  QevpSens=-EvapOUT(12)*1000
	  IF (ABS(QevpSens) .GT. ABS(Qevp)) THEN !Make sure sensible heat is never higher than total heat. ISI - 08/02/07
	      QevpSens = Qevp
	      !hAoEvp=-Qevp/1000/(StdCFMevp*StandardDensity)+hAiEvp
	      !TdboEvp=-QevpSens/1000/(StdCFMevp*StandardDensity*StandardSpecHeat)+TdbiEvp
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
      Dshtb=ShTbPAR(2)*1000
	  DcapTube=CapTubePAR(1)*1000
	  LcapTube=CapTubePAR(2)*1000

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

  mdot=MdotR*3600 
      
  !Convert output data to IP unit
  IF (Unit .EQ. 2) THEN
      PICMP=PiCmp/UnitP
      HICMP=HiCmp/UnitH
      TICMP=TiCmp*1.8+32
      POCMP=PoCmp/UnitP
      HOCMP=HoCmp/UnitH
      TOCMP=ToCmp*1.8+32
      TSUPICMP=TsupiCmp*1.8
	  TSUPOCMP=TsupoCmp*1.8
      TSUBICMP=TsubiCmp*1.8
	  TSUBOCMP=TsuboCmp*1.8
	  TSATICMP=TsatiCmp*1.8+32
	  TSATOCMP=TsatoCmp*1.8+32
	  MDOT=mdot/UrefFlow

      PICND=PiCnd/UnitP
	  POCND=PoCnd/UnitP
	  TICND=TiCnd*1.8+32
	  TOCND=ToCnd*1.8+32
	  HICND=HiCnd/UnitH
	  HOCND=HoCnd/UnitH
      TSUPICND=TsupiCnd*1.8
	  TSUPOCND=TsupoCnd*1.8
      TSUBICND=TsubiCnd*1.8
	  TSUBOCND=TsuboCnd*1.8
	  TSATICND=TsatiCnd*1.8+32
	  TSATOCND=TsatoCnd*1.8+32
	  QCND=Qcnd/UnitPwr
	  TDBICND=TdbiCnd*1.8+32
	  TDBOCND=TdboCnd*1.8+32
	  TWBICND=TwbiCnd*1.8+32
	  TWBOCND=TwboCnd*1.8+32
	  !CFMCND=CFMcnd/UnitL**3
	  CFMCND=CFMcnd/UnitArFlw
	  DPACND=DPaCnd/UairPres

      PIEXP=PiExp/UnitP
      HIEXP=HiExp/UnitH
      TIEXP=TiExp*1.8+32
	  POEXP=PoExp/UnitP
	  HOEXP=HoExp/UnitH
	  TOEXP=ToExp*1.8+32
      TSUPIEXP=TsupiExp*1.8
	  TSUPOEXP=TsupoExp*1.8
      TSUBIEXP=TsubiExp*1.8
	  TSUBOEXP=TsuboExp*1.8
	  TSATIEXP=TsatiExp*1.8+32
	  TSATOEXP=TsatoExp*1.8+32

      PIEVP=PiEvp/UnitP
	  POEVP=PoEvp/UnitP
	  TIEVP=TiEvp*1.8+32
	  TOEVP=ToEvp*1.8+32
	  HIEVP=HiEvp/UnitH
	  HOEVP=HoEvp/UnitH
      TSUPIEVP=TsupiEvp*1.8
	  TSUPOEVP=TsupoEvp*1.8
      TSUBIEVP=TsubiEvp*1.8
	  TSUBOEVP=TsuboEvp*1.8
	  TSATIEVP=TsatiEvp*1.8+32
	  TSATOEVP=TsatoEvp*1.8+32

	  QEVP=Qevp/UnitPwr
	  QEVPSENS=QevpSens/UnitPwr
	  QEVPLAT=QevpLat/UnitPwr
	  TDBIEVP=TdbiEvp*1.8+32
	  TDBOEVP=TdboEvp*1.8+32
	  TWBIEVP=TwbiEvp*1.8+32
	  TWBOEVP=TwboEvp*1.8+32
	  CFMEVP=CFMevp/UnitArFlw
	  DPAEVP=DPaEvp/UairPres

	  CALCHG=CalChg/UnitM
	  MassCmp=MassCmp/UnitM
	  MassCnd=MassCnd/UnitM
	  MassEvp=MassEvp/UnitM
	  MassSucLn=MassSucLn/UnitM
	  MassDisLn=MassDisLn/UnitM
	  MassLiqLn=MassLiqLn/UnitM
	  MassDisTube=MassDisTube/UnitM
	  MassAccum=MassAccum/UnitM
      Dshtb=ShTbPAR(2)/UnitL*12
	  DcapTube=CapTubePAR(1)/UnitL*12
	  LcapTube=CapTubePAR(2)/UnitL*12

      TaoEVP=TaoEVP*1.8+32
	  TaoCND=TaoCND*1.8+32
    
	  AccumDP=AccumDP/UnitP
	  FilterDP=FilterDP/UnitP

	  WeightEvpAluminum=WeightEvpAluminum/UnitM
	  WeightEvpCopper=WeightEvpCopper/UnitM

	  WeightCndAluminum=WeightCndAluminum/UnitM
	  WeightCndCopper=WeightCndCopper/UnitM

	  WeightSucLn=WeightSucLn/UnitM
	  WeightDisLn=WeightDisLn/UnitM
	  WeightLiqLn=WeightLiqLn/UnitM
	  WeightValveIDCLn=WeightValveIDCLn/UnitM
	  WeightValveODCLn=WeightValveODCLn/UnitM

	  CondLiqTubeLength=CondLiqTubeLength/UnitL
	  CondVapTubeLength=CondVapTubeLength/UnitL
	  CondTwoPhaseTubeLength=CondTwoPhaseTubeLength/UnitL

	  EvapLiqTubeLength=EvapLiqTubeLength/UnitL
	  EvapVapTubeLength=EvapVapTubeLength/UnitL
	  EvapTwoPhaseTubeLength=EvapTwoPhaseTubeLength/UnitL

	  !Conver Pressure to gauge basis
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
CoilSurfTemp=CoilParams(2)%TSurfCoil*9/5+32
  !VB program report format
  WRITE(5,*)
  WRITE(5,2200)'Title (ver. 2.0 12/17/09): ',Title
  WRITE(5,*)
  WRITE(5,2204)'*** System Performance Summary ***'
  WRITE(5,*)
  WRITE(5,2208)'Evaporator gross capacity       ',QEVP,CapUnit
  WRITE(5,2208)'Condenser gross capacity        ',QCND,CapUnit
  WRITE(5,2208)'Gross sensible capacity         ',QEVPSENS,CapUnit
  WRITE(5,2208)'Gross latent capacity           ',QEVPLAT,CapUnit
  WRITE(5,2208)'Compressor power                ',PWRCMP,PwrUnit
  WRITE(5,2208)'Refrigerant mass flow rate      ',MDOT,mdotUnit
  WRITE(5,2208)'COP (coefficient of performance)',COP,NoUnit
  WRITE(5,2208)'EER (energy efficiency ratio)   ',EER,EERunit
  WRITE(5,2208)'SHR (sensible heat ratio)       ',SHR,NoUnit
  WRITE(5,2208)'Condenser subcooling            ',TSUBOCND,DTunit
  WRITE(5,2208)'Expansion device subcooling     ',TSUBIEXP,DTunit
  WRITE(5,2208)'Evaporator superheat            ',TSUPOEVP,DTunit
  WRITE(5,2208)'Compressor superheat            ',TSUPICMP,DTunit
  WRITE(5,2208)'System charge                   ',CALCHG,MassUnit
  WRITE(5,2208)'Refrigerant in compressor       ',MassCmp,MassUnit
  WRITE(5,2208)'Refrigerant in condenser        ',MassCnd,MassUnit
  WRITE(5,2208)'Refrigerant in evaporator       ',MassEvp,MassUnit
  WRITE(5,2208)'Refrigerant in suction line     ',MassSucLn,MassUnit
  WRITE(5,2208)'Refrigerant in discharge line   ',MassDisLn,MassUnit
  WRITE(5,2208)'Refrigerant in liquid line      ',MassLiqLn,MassUnit
  WRITE(5,2208)'Refrigerant in distributor tubes',MassDistube,MassUnit
  WRITE(5,2208)'Refrigerant in accumulator      ',MassAccum,MassUnit
  WRITE(5,2208)'Short tube diameter             ',Dshtb,MiniLunit
  WRITE(5,2208)'TXV capacity                    ',Qtxv,SysUnit
  WRITE(5,2208)'Capillary tube diameter         ',DcapTube,MiniLunit !Added for Cap. tube 04/13/2009 - ISI
  WRITE(5,2208)'Capillary tube length           ',LcapTube,MiniLunit !Added for Cap. tube 04/13/2009 - ISI 
  WRITE(5,*)
  WRITE(5,2204)'***** Refrigerant Side Data *****'
  WRITE(5,*)
  IF (Unit .EQ. 2) THEN
      WRITE(5,2212)'Location               ','Temperature (F)','Pressure (psig)','Enthalpy (Btu/lbm)','Saturation Temperature (F)','Quality (%)','Subcooling (R)','Superheat (R)','Location               '
  ELSE
      WRITE(5,2212)'Location               ','Temperature (C)','Pressure (kPa)','Enthalpy (kJ/kg)','Saturation Temperature (C)','Quality (%)','Subcooling (K)','Superheat (K)','Location               '
  END IF
  WRITE(5,2216)'Compressor suction     ',TICMP,PICMP,HICMP,TSATICMP,XICMP*100,TSUBICMP,TSUPICMP,'Compressor suction     '
  WRITE(5,2216)'Compressor discharge   ',TOCMP,POCMP,HOCMP,TSATOCMP,XOCMP*100,TSUBOCMP,TSUPOCMP,'Compressor discharge   '
  WRITE(5,2216)'Condenser inlet        ',TICND,PICND,HICND,TSATICND,XICND*100,TSUBICND,TSUPICND,'Condenser inlet        '
  WRITE(5,2216)'Condenser outlet       ',TOCND,POCND,HOCND,TSATOCND,XOCND*100,TSUBOCND,TSUPOCND,'Condenser outlet       '
  WRITE(5,2216)'Expansion device inlet ',TIEXP,PIEXP,HIEXP,TSATIEXP,XIEXP*100,TSUBIEXP,TSUPIEXP,'Expansion device inlet '
  WRITE(5,2216)'Expansion device outlet',TOEXP,POEXP,HOEXP,TSATOEXP,XOEXP*100,TSUBOEXP,TSUPOEXP,'Expansion device outlet'
  WRITE(5,2216)'Evaporator inlet       ',TIEVP,PIEVP,HIEVP,TSATIEVP,XIEVP*100,TSUBIEVP,TSUPIEVP,'Evaporator inlet       '
  WRITE(5,2216)'Evaporator outlet      ',TOEVP,POEVP,HOEVP,TSATOEVP,XOEVP*100,TSUBOEVP,TSUPOEVP,'Evaporator outlet      '
  WRITE(5,*)
  WRITE(5,2204)'********* Air Side Data *********'
  WRITE(5,*)
  IF (Unit .EQ. 2) THEN
      WRITE(5,2220)'Location         ','Dry bulb temperature (F)','Wet bulb temperature (F)','Relative Humidity (%)','Volumetric flow rate (CFM)','Pressure Drop (in-H2O)','Location         '
  ELSE
      WRITE(5,2220)'Location         ','Dry bulb temperature (C)','Wet bulb temperature (C)','Relative Humidity (%)','Volumetric flow rate (m^3/min)','Pressure Drop (Pa)','Location         '
  END IF
  WRITE(5,2224)'Condenser inlet  ',TDBICND,TWBICND,RHICND*100,CFMCND,0.0d0,'Condenser inlet  '
  WRITE(5,2224)'Condenser outlet ',TDBOCND,TWBOCND,RHOCND*100,CFMCND,DPACND,'Condenser outlet '
  WRITE(5,2224)'Evaporator inlet ',TDBIEVP,TWBIEVP,RHIEVP*100,CFMEVP,0.0d0,'Evaporator inlet '
  WRITE(5,2224)'Evaporator outlet',TDBOEVP,TWBOEVP,RHOEVP*100,CFMEVP,DPAEVP,'Evaporator outlet'
  WRITE(5,*)
  WRITE(5,2204)'********* Pressure Drop *********'
  WRITE(5,*)
  WRITE(5,2208)'Accumulator                     ',AccumDP,Punit
  WRITE(5,2208)'Filter Drier                    ',FilterDP,Punit   
  WRITE(5,*)
  WRITE(5,2204)'*********** Material ************'
  WRITE(5,*)
  WRITE(5,2208)'Aluminum Evaporator             ',WeightEvpAluminum,MassUnit
  WRITE(5,2208)'Aluminum Condenser              ',WeightCndAluminum,MassUnit
  WRITE(5,2208)'Copper Evaporator               ',WeightEvpCopper,MassUnit
  WRITE(5,2208)'Copper Condenser                ',WeightCndCopper,MassUnit
  WRITE(5,2208)'Copper Suction Line             ',WeightSucLn,MassUnit
  WRITE(5,2208)'Copper Discharge Line           ',WeightDisLn,MassUnit
  WRITE(5,2208)'Copper Valve-Indoor Coil Line   ',WeightValveIDCLn,MassUnit
  WRITE(5,2208)'Copper Valve-Outdoor Coil Line  ',WeightValveODCLn,MassUnit
  WRITE(5,2208)'Copper Liquid Line              ',WeightLiqLn,MassUnit
  WRITE(5,*)
  WRITE(5,2204)'*** Refrigerant Distributions ***'
  WRITE(5,*)
  WRITE(5,2208)'Condenser liquid length         ',CondLiqTubeLength,Lunit
  WRITE(5,2208)'Condenser two-phase length      ',CondTwoPhaseTubeLength,Lunit
  WRITE(5,2208)'Condenser vapor length          ',CondVapTubeLength,Lunit
  WRITE(5,2208)'Evaporator two-phase length     ',EvapTwoPhaseTubeLength,Lunit
  WRITE(5,2208)'Evaporator vapor length         ',EvapVapTubeLength,Lunit
  WRITE(5,*)
  WRITE(5,2204)'*** Frost Parameters ***'
  WRITE(5,*)
  WRITE(5,2208)'Evaporator Tube Area            ',EvapTubeArea,NoUnit
  WRITE(5,2208)'Evaporator Fin Area             ',EvapFinArea,NoUnit
  WRITE(5,2208)'Evaporator Total Area           ',EvapTotArea,NoUnit
  WRITE(5,2208)'Evaporator Bare Area            ',EvapBareArea,NoUnit
  WRITE(5,2208)'Evaporator Min Area             ',EvapMinArea,NoUnit
  WRITE(5,2208)'Condenser Tube Area             ',CondTubeArea,NoUnit
  WRITE(5,2208)'Condenser Fin Area              ',CondFinArea,NoUnit
  WRITE(5,2208)'Evaporator Surface Temperature  ',CoilSurfTemp,NoUnit

1007 FORMAT(A17,A42)
1008 FORMAT(A13,F10.3,A10,A10,A13,F10.3,A10)
2001 FORMAT(A13,F10.3,A10)
2004 FORMAT(A56,F10.3,A10)
2007 FORMAT(A16,F10.3,A9)
2009 FORMAT(60(A16,','))
2100 FORMAT(60(E,','))

2200 FORMAT(A32,',',A50)
2204 FORMAT(A)
2208 FORMAT(A33,',',F40.3,',',A15)
2212 FORMAT(A24,',',A18,',',A18,',',A21,',',A29,',',A14,',',A17,',',A16,',',A26)
2216 FORMAT(A24,',',F18.3,',',F18.3,',',F21.3,',',F29.3,',',F14.1,',',F17.3,',',F16.3,',',A26)
2220 FORMAT(A18,',',A27,',',A27,',',A24,',',A33,',',A25,',',A20)
2224 FORMAT(A18,',',F27.3,',',F27.3,',',F24.1,',',F33.3,',',F25.3,',',A20) 

RETURN

END SUBROUTINE