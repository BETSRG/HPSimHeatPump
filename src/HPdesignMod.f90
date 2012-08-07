    SUBROUTINE HPDM(DTVALU)

    USE FluidProperties_HPSim !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)
    USE CondenserMod
    USE EvaporatorMod
    USE ShortTubeMod
    USE CapillaryTubeMod
    USE TXVMOD
    USE AccumulatorMod
    USE DataSimulation
    !USE IFPORT !RS: This seems to be an obsolete module

    IMPLICIT NONE

    CHARACTER (len=15) :: Property           
    INTEGER            :: RefrigIndex =0
    REAL Temperature,Quality,Pressure,Enthalpy

    REAL DTVALU

    LOGICAL PRINT
    REAL NTE,NSECTE,NTC,NSECTC

    INTEGER(2) RefPropOpt			!Ref prop calc. option
    INTEGER(2) RefPropErr			!Error flag:1-error; 0-no error
    REAL RefProp(28)	!Refrigerant properties

    INTEGER(2) AirPropOpt			!Air prop calc. option
    INTEGER(2) AirPropErr			!Error flag:1-error; 0-no error
    REAL AirProp(8)		!Air properties

    INTEGER ICHRGE,IMASS,IREFC,LPRINT
    REAL TAIICI,TAIIEI
    INTEGER NTAMB,NCROSS
    REAL DELT2,DTVLMN,DTVLMX
    REAL TAIIE,TAIIC
    INTEGER ITRPIE,I
    REAL ERRMSG(2)
    REAL TSAT1,CONV,STEP,DIFFER,XMR,TSATEO
    INTEGER IERROR,IER
    REAL TAIE1,DIFF,DIFSGN,PROD,TSATSV,TSATDM,TAISV,TAIDM
    REAL ID,L,Elevation,mdot,xi,xo,mu,muVap,muLiq,rhoi,rhoo,rhoiVap,rhoiLiq, &
    rhooVap,rhooLiq,DPfric,DPmom,DPgrav,DPtot,TsoEvp,LsucLn
    REAL MassCoil,MassLiqCoil,MassVapCoil
    REAL Root1,Root2,Dprev1,Dprev2,Qprev1,Qprev2,Lprev1,Lprev2
    INTEGER NumIter,MaxIteration
    REAL XMRFLD,ErrXMR,TSICMPprev
    REAL Dshtb,MaxDshTb,MinDshTb
    REAL CapTubeDimension,MaxLen,MinLen
    REAL,PARAMETER :: StandardDensity=1.2 !kg/m3
    REAL TaoE,RHoE,TaoC,RHoC
    REAL Qtxv,MaxQtxv,MinQtxv,SuperStc,SuperRtd
    REAL Subcooling, Superheat, DPtxv, TsatEvp, TsatCnd, AccumDP
    REAL SUPERMAX !TXV maximum effective superheat, F
    REAL SUPERMIN !TXV static (minimum) superheat, F 
    REAL ChargeCorrection !Correction charge for the charge tuning method, lbm
    REAL, EXTERNAL :: CNDNSR, EVPTR
    REAL ZERO3
    REAL, PARAMETER :: Dstep=1
    REAL, PARAMETER :: CapTubeDimStep=1E-3

    LOGICAL IsSizeDiameter
    REAL SimpleEvapOUT(25),DetailedEvapOUT(25) 
    REAL DetailedQevp,DetailedDPevp
    REAL SimpleQevp,SimpleDPevp
    LOGICAL,SAVE :: IsFirstTimeEvaporator = .TRUE. !First time to call evaporator flag
    INTEGER IsCoolingMode !Cooling mode flag: 1=yes, otherwise=no
    INTEGER ChargeOption !Charge option, 1=no tuning; 2=w/charge tuning
    REAL, SAVE:: PrevTime = 0.0                                               
    INTEGER   :: Flag

    LOGICAL :: FLAG_GOTO_950
    
    CHARACTER(LEN=93),PARAMETER :: FMT_1002 = "('0HPDM: **** FAILED TO CONVERGE ON SUBCOOLING *****',/,  '         DIFFERENCE  =',F8.3,' F')"
    CHARACTER(LEN=93),PARAMETER :: FMT_1006 = "('0HPDM: **** FAILED TO CONVERGE ON SUPERHEAT *****',/,   '         DIFFERENCE  =',F8.3,' F')"
    CHARACTER(LEN=213),PARAMETER :: FMT_1013 = "('0        DID NOT CONVERGE ON  EVAPORATOR INLET ',  'AIR TEMPERATURE FOR THIS SATURATION TEMPERATURE.' ,/,'         SET COMPRESSOR INLET SATURATION TEMPERATURE TO',  F8.3,' F AND GO BACK TO CONDENSER ITERATION.')"
    CHARACTER(LEN=128),PARAMETER :: FMT_1014 = "('0DRIVER: ***** FAILED TO CONVERGE ON EVAPORATOR ',  'INLET AIR TEMPERATURE *****',/, '               DIFFERENCE  =',F8.3,' F')"
    CHARACTER(LEN=13),PARAMETER :: FMT_700 = "(A44,F7.2,A5)"
    CHARACTER(LEN=13),PARAMETER :: FMT_704 = "(A13,F7.2,A5)"
    CHARACTER(LEN=20) :: tmpString

    LOGICAL, EXTERNAL :: IssueRefPropError

    MaxIteration=30
    ICHRGE=1
    IMASS=1
    LPRINT=1

    !for specified subcooiling, set IREFC to zero
    !for specifed flow control, set IREFC to 3 
    IF (MODE .EQ. 1 .OR. MODE .EQ. 3) THEN
        IREFC=3
    ELSE
        IREFC=0
    END IF

    TAIICI=TaiC
    TAIIEI=TaiE
    TAIIC=TaiC

    NTAMB = 0
    NCROSS = 0
    DELT2 = 1.25

    DTVLMN = -150.0

    IF(DTVALU.LT.DTVLMN) THEN
        DTVALU = DTVLMN
    END IF

    IF(ICHRGE.NE.0) THEN
        IF(ICHRGE.NE.2) THEN
            IF (MODE .EQ. 4) THEN
                DTROC = DTVALU
                IF(DTROC.LT.0.0) THEN
                    DTROC = DTROC/200.
                END IF
            ELSE
                SUPER = DTVALU
                IF(SUPER.LE.0.0) THEN
                    SUPER = -(1.0+SUPER/500.)
                END IF
            END IF

            Temperature=(TSICMP-32)/1.8 !RS Comment: Unit Conversion, from F to C
            Quality=1
            PiCmp=TQ(Ref$,Temperature,Quality,'pressure',RefrigIndex,RefPropErr)    !Compressor Inlet Pressure
            IF (IssueRefPropError(RefPropErr, 'HPdesign')) THEN
                STOP
            END IF
            PiCmp=PiCmp/1000    !RS Comment: Unit Conversion

            IF (SUPER .GT. 0) THEN
                Temperature=(TSICMP+SUPER-32)/1.8   !RS Comment: Unit Conversion, from F to C
                Pressure=PiCmp*1000 !RS Comment: Unit Conversion
                HiCmp=TP(Ref$,Temperature,Pressure,'enthalpy',RefrigIndex,RefPropErr)   !Compressor Inlet Enthalpy
                IF (IssueRefPropError(RefPropErr, 'HPdesign')) THEN
                    STOP
                END IF
                HiCmp=HiCmp/1000    !RS Comment: Unit Conversion
                TiCmp=((TSICMP+SUPER)-32)/1.8   !RS Comment: Unit Conversion, from F to C
            ELSE
                Pressure=PiCmp*1000 !RS Comment: Unit Conversion
                Quality=-SUPER
                HiCmp=PQ(Ref$,Pressure,Quality,'enthalpy',RefrigIndex,RefPropErr)   !Compressor Inlet Enthalpy
                IF (IssueRefPropError(RefPropErr, 'HPdesign')) THEN
                    STOP
                END IF
                HiCmp=HiCmp/1000    !RS Comment: Unit Conversion
                TiCmp=PQ(Ref$,Pressure,Quality,'temperature',RefrigIndex,RefPropErr)    !Compressor Inlet Temperature
                IF (IssueRefPropError(RefPropErr, 'HPdesign')) THEN
                    STOP
                END IF
            END IF

            Xmr=CompOUT(2)

            PoEvp=EvapOUT(1)

            QsucLn=EvapPAR(5) 
            DTsucLn=EvapPAR(6)
            LsucLn=EvapPAR(1) 

            IF (LsucLn .GT. 0) THEN
                IF (QsucLn .NE. 0) THEN
                    HoEvp=HiCmp-QsucLn/Xmr

                    Pressure=PoEvp*1000 !RS Comment: Unit Conversion
                    Enthalpy=HoEvp*1000 !RS Comment: Unit Conversion
                    ToEvp=PH(Ref$,Pressure,Enthalpy,'temperature',RefrigIndex,RefPropErr)   !Evaporator Outlet Temperature
                    IF (IssueRefPropError(RefPropErr, 'HPdesign')) THEN
                        STOP
                    END IF

                    XoEvp=PH(Ref$,Pressure,Enthalpy,'quality',RefrigIndex,RefPropErr)   !Evaporator Outlet Quality
                    IF (IssueRefPropError(RefPropErr, 'HPdesign')) THEN
                        STOP
                    END IF

                    Quality=1
                    TsoEvp=PQ(Ref$,Pressure,Quality,'temperature',RefrigIndex,RefPropErr)   !Evaporator Outlet Saturation Temperature
                    IF (IssueRefPropError(RefPropErr, 'HPdesign')) THEN
                        STOP
                    END IF

                    SUPERE=(ToEvp-TsoEvp)*1.8

                    IF (XoEvp .LT. 1.) THEN
                        SUPERE = -XoEvp
                    END IF

                ELSEIF (DTsucLn .NE. 0) THEN

                    ToEvp=TiCmp-DTsucLn

                    Temperature=ToEvp
                    Pressure=PoEvp*1000 !RS Comment: Unit Conversion
                    HoEvp=TP(Ref$, Temperature, Pressure, 'enthalpy', RefrigIndex,RefPropErr)   !Evaporator Outlet Enthalpy
                    IF (IssueRefPropError(RefPropErr, 'HPdesign')) THEN
                        STOP
                    END IF
                    HoEvp=HoEvp/1000    !RS Comment: Unit Conversion

                    Pressure=PoEvp*1000 !RS Comment: Unit Conversion
                    Enthalpy=HoEvp*1000 !RS Comment: Unit Conversion
                    XoEvp=PH(Ref$,Pressure,Enthalpy,'quality',RefrigIndex,RefPropErr)   !Evaporator Outlet Quality
                    IF (IssueRefPropError(RefPropErr, 'HPdesign')) THEN
                        STOP
                    END IF

                    Quality=1
                    TsoEvp=PQ(Ref$,Pressure,Quality,'temperature',RefrigIndex,RefPropErr)   !Evaporator Outlet Saturation Temperature
                    IF (IssueRefPropError(RefPropErr, 'HPdesign')) THEN
                        STOP
                    END IF

                    SUPERE=(ToEvp-TsoEvp)*1.8

                    IF (XoEvp .LT. 1.) THEN
                        SUPERE = -XoEvp
                    END IF

                ELSE
                    SUPERE=SUPER
                END IF

            ELSE
                SUPERE=SUPER
            END IF
        ELSE
            DTROC = DTVALU
            IF(DTROC.LT.0.0) THEN
                DTROC = DTROC/200.
            END IF
        END IF

    END IF

    FLAG_GOTO_950 = .FALSE.
    DO WHILE (.TRUE.)
        
        DO I=1,2
            ERRMSG(I) = 0.0
        END DO

        !   FIND DESIRED CONDENSER SUBCOOLING
        !        OR REFRIGERANT MASS FLOW RATE BALANCE
        !   BY ADJUSTING COMPRESSOR EXIT SATURATION TEMPERATURE

        !       USE 'ZERO3' AND 'CNDNSR' TO FIND TSOCMP SUCH THAT
        !       ABS(CDTROC - DTROC)<CNDCON --  IF IREFC = 0 OR
        !       ABS(XMRFLD - XMR)<FLOCON/20 -- IF IREFC IS NOT EQUAL TO 0

        TSAT1 = TSOCMP

        CONV = CNDCON
        IF(IREFC .NE. 0) THEN
            CONV = FLOCON !/20.
        END IF
        STEP = 3

        CALL IssueOutputMessage('|-------------------- Highside Iteration --------------------|')

        IF (FirstTimeHPdesignMode) THEN
            TaoC=CondIN(5)
            RHoC=CondIN(6)
            TaoE=EvapIN(5)
            RHoE=EvapIN(6)	  
        ELSE
            TaoC=CondOUT(21)
            RHoC=CondOUT(22)
            TaoE=EvapOUT(17)
            RHoE=EvapOUT(18)
        END IF

        AirPropOpt=2
        AirProp(1)=(TaiC-32)*5/9    !RS Comment: Unit Conversion, from F to C
        AirProp(3)=RHiC
        CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
        RhoAiC=AirProp(7)

        AirPropOpt=2
        AirProp(1)=(TaiE-32)*5/9    !RS Comment: Unit Conversion, from F to C
        AirProp(3)=RHiE
        CALL PsyChart(AirProp,AirPropOpt,BaroPressure,AirPropErr)  
        RhoAiE=AirProp(7)

        !Correct standard CFM to actual CFM first
        !XMaC=StandardDensity/RhoAoC*StdCFMcnd
        !XMaE=StandardDensity/RhoAoE*StdCFMevp

        !Actual mass flow rate
        XMaC=CFMcnd*RhoAiC
        XMaE=CFMevp*RhoAiE

        TSOCMP = ZERO3(TSAT1,CNDNSR,1E-3,CNDCON,STEP,DIFFER,IERROR)
        !CALL SolveRegulaFalsi(CNDCON, MaxIter, Flag, TSOCMP, CNDNSR, TSAT1, STEP,IError)

        IF (IERROR .GE. 3) THEN
            CALL IssueOutputMessage('')
            CALL IssueOutputMessage('## ERROR ## Highside: Failed to find a solution.')
            CALL IssueOutputMessage('Try another condenser, compressor, or change boundary conditions.')
            STOP
        END IF
        FirstTimeFlowRateLoop=.FALSE.
        !ISI 05-25-05
        IF (MODE .EQ. 5) THEN !Peform the rest of the calculation for Condenser Unit
            FLAG_GOTO_950 = .TRUE.
            EXIT
        END IF

        IF (LPRINT.EQ.2) THEN 
            PRINT = .TRUE.
            DIFFER = CNDNSR(TSOCMP,IER)
        END IF
        IF (ABS(DIFFER) .GT. CONV) THEN
            IF (LPRINT .GT. 1) THEN
                IF (Unit .EQ. 1) THEN
                    CALL IssueOutputMessage('## ERROR ## Highside: Solution not converged on subcooling.')
                    WRITE(tmpString,'(F10.3)') DIFFER/1.8
                    CALL IssueOutputMessage('Difference: '//TRIM(tmpString)//DTunit)
                ELSE
                    CALL IssueOutputMessage('## ERROR ## Highside: Solution not converged on subcooling.')
                    WRITE(tmpString,'(F10.3)') DIFFER
                    CALL IssueOutputMessage('Difference: '//TRIM(tmpString)//DTunit)
                END IF  
            END IF

            ERRMSG(1) = DIFFER
        END IF
        
        EvapIN(1)=MdotR           !Refrigerant side mass flow rate, kg/s
        !EvapIN(2)=CompIN(1)       !Compressor inlet pressure
        EvapIN(3)=CondOUT(11)     !Exp. device inlet enthalpy, kJ/kg
        EvapIN(4)=XMaE            !Air side mass flow rate, kg/s
        EvapIN(5)=(TAIIEI-32)/1.8 !Air side inlet temp. C
        EvapIN(6)=RHiE            !Air side inlet relative humidity
        EvapIN(9)=CompOUT(5)      !Discharge temperature, C

        !Take compressor shell loss into account
        IF (CompPAR(21) .NE. 0) THEN !Shell loss in fraction
            EvapPAR(32)=CompPAR(21)*CompOUT(1)
        ELSE !Shell loss in W
            EvapPAR(32)=CompPAR(22)/1000
        END IF

        IsCoolingMode=EvapPAR(20)
        IF (FirstTimeHPdesignMode) THEN

            IF ((IsCoolingMode .GT. 0 .AND. IDCcoilType .EQ. MCEVAPORATOR) .OR. &
            (IsCoolingMode .LT. 1 .AND. ODCcoilType .EQ. MCEVAPORATOR)) THEN
                !Microchannel coil
                EvapPAR(54)=1 !First time
                EvapPAR(53)=0 !Detailed version
                CALL Evaporator(Ref$,PureRef,EvapIN,EvapPAR,EvapOUT)
                EvapPAR(54)=0 !No longer first time
            ELSE
                !Plate-fin coil
                !Run both simple and detailed version to determine which one to use
                !Change the logic to reset IsFirstTimeEvaporator
                IF (IsFirstTimeEvaporator) THEN
                    EvapPAR(54)=1 !First time
                    EvapPAR(53)=0 !Detailed version
                    CALL Evaporator(Ref$,PureRef,EvapIN,EvapPAR,DetailedEvapOUT)
                    CALL EndEvaporatorCoil
                    DetailedQevp=DetailedEvapOUT(11)
                    DetailedDPevp=EvapIN(2)-DetailedEvapOUT(6)

                    EvapPAR(53)=1 !Simple version
                    CALL Evaporator(Ref$,PureRef,EvapIN,EvapPAR,SimpleEvapOUT)
                    CALL EndEvaporatorCoil
                    SimpleQevp=SimpleEvapOUT(11)
                    SimpleDPevp=EvapIN(2)-SimpleEvapOUT(6)

                    IF (ABS((SimpleQevp-DetailedQevp)/DetailedQevp) .LT. 0.1 .AND. &
                    ABS((SimpleDPevp-DetailedDPevp)/DetailedDPevp) .LT. 0.1) THEN
                        EvapPAR(53)=1 !Simple version
                        EvapOUT=SimpleEvapOUT
                    ELSE
                        EvapPAR(53)=0 !Detailed version
                        EvapOUT=DetailedEvapOUT
                    END IF
                    IsFirstTimeEvaporator=.FALSE. 

                    !Always detailed
                    EvapPAR(53)=0 !Detailed version
                    EvapOUT=DetailedEvapOUT

                ELSE
                    CALL Evaporator(Ref$,PureRef,EvapIN,EvapPAR,EvapOUT)
                    EvapPAR(54)=0 !No longer first time
                END IF
            END IF

            IF (EvapOUT(20) .NE. 0) THEN
                SELECT CASE (INT(EvapOUT(20)))
                CASE (3,4,5)
                    STOP
                END SELECT
            END IF
            FirstTimeHPdesignMode=.FALSE.

        END IF

        IF (LPRINT .EQ. 2) THEN
            PRINT = .FALSE.
        END IF

        !   FIND DESIRED EVAPORATOR EXIT SUPERHEAT OR QUALITY
        !   BY ADJUSTING EVAPORATOR INLET AIR TEMPERATURE

        TAIE1 = TAIIEI

        STEP = 2

        CALL IssueOutputMessage(' ')
        CALL IssueOutputMessage('|-------------------- Lowside Iteration ---------------------|')
        IF (Unit .EQ. 1) THEN
            WRITE(tmpString, '(F10.4)') (TSICMP-32)*5/9
        ELSE
            WRITE(tmpString, '(F10.4)') TSICMP
        END IF
        CALL IssueOutputMessage( '>> Compressor suction saturation temperature: '//TRIM(tmpString)//Tunit)

        TAIIE = ZERO3(TAIE1,EVPTR,AMBCON,EVPCON,STEP,DIFFER,IERROR)

        IF (IERROR .GE. 3) THEN
            CALL IssueOutputMessage('')
            CALL IssueOutputMessage('## ERROR ## Lowside: Failed to find a solution.')
        END IF

        IF (LPRINT .GT. 2) THEN
            PRINT = .TRUE.
            DIFFER = EVPTR(TAIIE,IER)
            PRINT = .FALSE.
        END IF
        
        IF (ABS(DIFFER) .GT. EVPCON) THEN
            IF (LPRINT .GT. 1) THEN
                IF (Unit .EQ. 1) THEN
                    CALL IssueOutputMessage('## ERROR ## Lowside: Solution not converged on superheat.')
                    WRITE(tmpString,'(F10.4)') DIFFER/1.8
                    CALL IssueOutputMessage('Difference: '//tmpString//DTunit)
                ELSE
                    CALL IssueOutputMessage('## ERROR ## Lowside: Solution not converged on superheat.')
                    WRITE(tmpString,'(F10.4)') DIFFER
                    CALL IssueOutputMessage('Difference: '//tmpString//DTunit)
                END IF  
            END IF
            ERRMSG(2) = DIFFER
        END IF

        IF(LPRINT.GT.1.AND.IMASS.NE.0) THEN
            IF (AccumPAR(2) .GT. 0) THEN !Height
                AccumIN(1)=MdotR
                AccumIN(2)=CompIN(1) !Pressure
                AccumIN(3)=CompIN(3) !Enthalpy
                CALL CalcAccumulatorMass(AccumIN,AccumOUT)
            ELSE
                AccumOUT(1)=0
            END IF

            CALL CalcCondenserInventory(MassCoil,MassLiqCoil,MassVapCoil,CondLiqTubeLength,CondVapTubeLength,CondTwoPhaseTubeLength,CondNumLiqTubes)
            CondOUT(18)=MassCoil
            CALL CalcEvaporatorInventory(MassCoil,MassLiqCoil,MassVapCoil,EvapLiqTubeLength,EvapVapTubeLength,EvapTwoPhaseTubeLength,EvapNumLiqTubes)
            EvapOUT(14)=MassCoil

            IF (ExpDevice .EQ. 1) THEN
                CALCHG=(CompOUT(6)+CondOUT(16)+CondOUT(17)+CondOUT(18)+ &
                EvapOUT(13)+EvapOUT(14)+ShTbOUT(5)+AccumOUT(1))/UnitM
            ELSE
                CALCHG=(CompOUT(6)+CondOUT(16)+CondOUT(17)+CondOUT(18)+ &
                EvapOUT(13)+EvapOUT(14)+TxvOUT(5)+AccumOUT(1))/UnitM
            END IF
        END IF

        !   FIND DESIRED EVAPORATOR INLET AIR TEMPERATURE
        !   BY ADJUSTING COMPRESSOR INLET SATURATION TEMPERATURE

        DIFF = TAIIE-TAIIEI
        !VL: Previously: IF(ABS(DIFF).LE.AMBCON) GO TO 900
        IF(ABS(DIFF).LE.AMBCON) THEN
            EXIT
        END IF

        !VL: Previously: IF(NTAMB.NE.0) GO TO 810
        IF(NTAMB.EQ.0) THEN
            DIFSGN = DIFF
        END IF

        !VL: Previously:810 CONTINUE
        PROD = DIFF*DIFSGN

        IF(PROD.GT.0.0.AND.NCROSS.EQ.0) THEN

            IF(NTAMB.GT.0) THEN
                DELT2 = (TAIIE-TAIDM)/(TSICMP-TSATDM)
            END IF
            IF (ABS(DELT2) .LE. 0.05) THEN !ISI - 06/13/07
                !VL: Previously: GO TO 900 !0.05 F !ISI - 08/02/06
                EXIT
            END IF

            TSATDM = TSICMP
            TAIDM = TAIIE
            TSICMPprev=TSICMP
            TSICMP = TSICMP-DIFF/DELT2
            IF (TSICMP .GT. TAIIEI) THEN
                TSICMP=(TSICMPprev+TAIIEI)/2 !Make sure TSICMP < TAIIEI
            END IF

        ELSE

            NCROSS = 1
            !VL: Previously:IF(PROD.GT.0.0) GO TO 820
            IF(PROD.LE.0.0) THEN
                TSATSV = TSATDM
                TAISV = TAIDM
            END IF

            !VL: Previously:820     CONTINUE
            TSATDM = TSICMP
            TAIDM = TAIIE
            TSICMPprev=TSICMP
            TSICMP = TSICMP-(TSATSV-TSICMP)/(TAISV-TAIIE)*DIFF
            IF (ABS(TSICMPprev-TSICMP) .LE. 0.01) THEN
                !VL: Previously: GO TO 900 !0.05 F !ISI - 08/02/06
                EXIT
            END IF
            DIFSGN = DIFF
            IF (TSICMP .GT. TAIIEI) THEN
                TSICMP=(TSICMPprev+TAIIEI)/2 !Make sure TSICMP < TAIIEI
            END IF

        END IF

        NTAMB = NTAMB + 1
        IF(NTAMB.GT.15) THEN
            IF (PrnLog .EQ. 1) THEN
                WRITE(6,FMT_1014) DIFF
            END IF
            EXIT
        END IF
        IF (LPRINT .GT. 1) THEN
            IF (PrnLog .EQ. 1) THEN
                WRITE(6,FMT_1013)TSICMP
            END IF
        END IF

        FirstTimeAirTempLoop=.TRUE.

        IF (TSICMP .GE. TSOCMP) THEN
            CALL IssueOutputMessage('')
            CALL IssueOutputMessage('## ERROR ## HPdesign: Failed to find a solution.')
            STOP
        END IF

    END DO

    !VL: Functionality moved near GOTO Call ... previously: 850     IF (PrnLog .EQ. 1) WRITE(6,FMT_1014) DIFF

    IF (FLAG_GOTO_950 .EQ. .FALSE.) THEN 

        IF (IREFC .EQ. 0) THEN
            !**************Size short tube orifice**************

            XMR=CompOUT(2)*3600/UnitM

            ShTbIN(1)=CompOUT(2)  !Compressor mass flow rate, kg/s
            ShTbIN(2)=CondOUT(10) !Exp. device inlet pressure, kPa
            ShTbIN(3)=CondOUT(11) !Exp. device inlet enthalpy, kJ/kg
            ShTbIN(4)=EvapIN(2)   !Evaporator inlet pressure, kPa
            ShTbIN(5)=EvapOUT(1)  !Evaporator outlet pressure, kPa

            IF (ShTbPAR(1) .LE. 0) THEN
                ShTbPAR(1)=0.0127
                !Short Tube: Parameters not defined.
            ELSE

                !Initial guess
                Root1=999
                Root2=0.0
                Dprev1=0.0
                Dprev2=0.0
                NumIter=0
                MaxDshTb=0
                MinDshTb=0

                Dshtb=2.0 !1.0 !Initial guess !Short tube diameter, mm
                ShTbPAR(2)=Dshtb/1000   !RS Comment: Unit Conversion

                DO NumIter=1, MaxIteration

                    !CALL ShortTube(Ref$,PureRef,ShTbIN,ShTbPAR,ShTbOUT)
                    CALL ShortTubePayne(Ref$,PureRef,ShTbIN,ShTbPAR,ShTbOUT)
                    IF (ShTbOUT(7) .NE. 0) THEN
                        SELECT CASE (INT(ShTbOUT(7)))
                        CASE (1)
                            ShTbPAR(2)=ShTbPAR(2)*1.2
                            CYCLE
                        END SELECT
                    END IF

                    XMRFLD=ShTbOUT(1)*3600/UnitM    !RS Comment: Unit Conversion, lbm/s??
                    ToExp=ShTbOUT(3)
                    XoExp=ShTbOUT(4)

                    ErrXMR=ABS((XMRFLD-XMR))
                    IF (MaxDshTb .NE. 0 .AND. MinDshTb .NE. 0 .AND. ErrXMR .GT. 1E-4) THEN
                        IF (XMRFLD .GT. XMR) THEN
                            MaxDshTb=Dshtb
                        ELSE
                            MinDshTb=Dshtb
                        END IF
                        Dshtb=(MaxDshTb+MinDshTb)/2
                        ShTbPAR(2)=Dshtb/1000 !Short tube diameter, m
                    ELSEIF (ErrXMR .GT. 1E-4) THEN !Find short tube diameter by secant method
                        IF (XMRFLD .GT. XMR) THEN
                            MaxDshTb=Dshtb
                            DshTb=DshTb/(2**NumIter*Dstep)
                        ELSE
                            MinDshTb=Dshtb
                            DshTb=DshTb+2**NumIter*Dstep
                        END IF
                        IF (MaxDshTb .NE. 0 .AND. MinDshTb .NE. 0) THEN
                            Dshtb=(MaxDshTb+MinDshTb)/2
                        END IF
                        ShTbPAR(2)=Dshtb/1000 !Short tube diameter, m
                    ELSE
                        EXIT
                    END IF
                END DO
            END IF

            IF (INT(ShTbOUT(7)) .EQ. 1) THEN
                CALL IssueOutputMessage( '')
                CALL IssueOutputMessage('## ERROR ## HPdesign: Short tube solution error.')
                STOP
            END IF

            !**************Size TXV**************
            mdotr=CompOUT(2)
            PiCmp=CompIN(1)
            PoCmp=CompIN(2)
            Subcooling=CondOUT(14)
            Superheat=EvapOUT(10)
            IF (ShTbOUT(2) .NE. 0) THEN
                DPtxv=CondOUT(10)-ShTbOUT(2)
            ElSE
                DPtxv=CondOUT(10)-EvapIN(2)
            END IF

            CALL TXV(mdotr,PiCmp,PoCmp,Subcooling,Superheat,DPtxv,Qtxv)
            TxvPAR(1)=Qtxv

            !**************Size Capillary Tube**************
            CapTubeIN(1)=CompOUT(2)  !Compressor mass flow rate, kg/s
            CapTubeIN(2)=CondOUT(10) !Exp. device inlet pressure, kPa
            CapTubeIN(3)=CondOUT(11) !Exp. device inlet enthalpy, kJ/kg
            CapTubeIN(4)=EvapIN(2)   !Evaporator inlet pressure, kPa
            CapTubeIN(5)=EvapOUT(1)  !Evaporator outlet pressure, kPa

            !Initial guess
            Root1=999
            Root2=0.0
            Lprev1=0.0
            Lprev2=0.0
            NumIter=0
            MaxLen=0
            MinLen=0

            IsSizeDiameter=.TRUE. !Always size diameter

            CapTubeDimension=1e-4 !1E-3 !Initial guess of capillary tube diameter
            IF (IsSizeDiameter .EQ. .TRUE.) THEN
                CapTubePAR(1)=CapTubeDimension
            ELSE
                CapTubePAR(2)=CapTubeDimension
            END IF

            DO NumIter=1, MaxIter

                !CALL CapillaryTubeChoi(Ref$,PureRef,CapTubeIN,CapTubePAR,CapTubeOUT)  
                CALL CapillaryTubeORNL(Ref$,PureRef,CapTubeIN,CapTubePAR,CapTubeOUT)

                IF (CapTubeOUT(7) .NE. 0) THEN
                    SELECT CASE (INT(CapTubeOUT(7)))
                    CASE (1)
                        CapTubePAR(1)=CapTubePAR(1)*1.2
                        CYCLE
                    END SELECT
                END IF

                XMRFLD=CapTubeOUT(1)*3600/UnitM !RS Comment: Unit Conversion, lbm/s??
                ToExp=CapTubeOUT(3)
                XoExp=CapTubeOUT(4)

                ErrXMR=ABS((XMRFLD-XMR))

                IF (MaxLen .NE. 0 .AND. MinLen .NE. 0 .AND. ErrXMR .GT. 1E-2) THEN
                    IF (XMRFLD .GT. XMR) THEN
                        IF (IsSizeDiameter .EQ. .TRUE.) THEN
                            MaxLen=CapTubeDimension
                        ELSE
                            MinLen=CapTubeDimension
                        END IF
                    ELSE
                        IF (IsSizeDiameter .EQ. .TRUE.) THEN
                            MinLen=CapTubeDimension
                        ELSE
                            MaxLen=CapTubeDimension
                        END IF
                    END IF
                    CapTubeDimension=(MaxLen+MinLen)/2

                ELSEIF (ErrXMR .GT. 1E-2) THEN !Find capillary tube dimension by secant method

                    IF (XMRFLD .GT. XMR) THEN
                        IF (IsSizeDiameter .EQ. .TRUE.) THEN
                            MaxLen=CapTubeDimension
                        ELSE
                            MinLen=CapTubeDimension
                        END IF
                        CapTubeDimension=CapTubeDimension/(2**NumIter*CapTubeDimStep)
                    ELSE
                        IF (IsSizeDiameter .EQ. .TRUE.) THEN
                            MinLen=CapTubeDimension
                        ELSE
                            MaxLen=CapTubeDimension
                        END IF
                        CapTubeDimension=CapTubeDimension+2**NumIter*CapTubeDimStep
                    END IF
                    IF (MaxLen .NE. 0 .AND. MinLen .NE. 0) THEN
                        CapTubeDimension=(MaxLen+MinLen)/2
                    END IF

                ELSE
                    EXIT
                END IF

                IF (IsSizeDiameter .EQ. .TRUE.) THEN
                    CapTubePAR(1)=CapTubeDimension
                ELSE
                    CapTubePAR(2)=CapTubeDimension
                END IF

            END DO

            IF (NumIter .GT. MaxIter) THEN
                CALL IssueOutputMessage( '')
                CALL IssueOutputMessage('## ERROR ## HPdesign: Capillary tube solution not converged.')
                STOP
            END IF

        END IF

        IF (LPRINT.LE.1.AND.IMASS.NE.0) THEN
            IF (AccumPAR(2) .GT. 0) THEN !Height
                AccumIN(1)=MdotR
                AccumIN(2)=CompIN(1) !Pressure
                AccumIN(3)=CompIN(3) !Enthalpy
                CALL CalcAccumulatorMass(AccumIN,AccumOUT)
            ELSE
                AccumOUT(1)=0
            END IF

            CALL CalcCondenserInventory(MassCoil,MassLiqCoil,MassVapCoil,CondLiqTubeLength,CondVapTubeLength,CondTwoPhaseTubeLength,CondNumLiqTubes)
            CondOUT(18)=MassCoil
            CALL CalcEvaporatorInventory(MassCoil,MassLiqCoil,MassVapCoil,EvapLiqTubeLength,EvapVapTubeLength,EvapTwoPhaseTubeLength,EvapNumLiqTubes)
            EvapOUT(14)=MassCoil

            CALCHG=(CompOUT(6)+CondOUT(16)+CondOUT(17)+CondOUT(18)+ &
            EvapOUT(13)+EvapOUT(14)+ShTbOUT(5)+AccumOUT(1))/UnitM
        END IF

        IF(ICHRGE.EQ.0.AND.ERRMSG(1).NE.0.) THEN 
            IF (PrnLog .EQ. 1) THEN
                WRITE(6,FMT_1002) ERRMSG(1)
            END IF
        END IF 
        IF(ICHRGE.EQ.0.AND.ERRMSG(2).NE.0.) THEN 
            IF (PrnLog .EQ. 1) THEN
                WRITE(6,FMT_1006) ERRMSG(2)
            END IF
        END IF

        IF (IsChargeTuning .GT. 0 .AND. MODE .NE. 2) THEN !Apply charge tuning
            ChargeCorrection=(ChargeCurveIntercept+ChargeCurveSlope*(CondLiqTubeLength-RefLiquidLength))/UnitM

            IF (FirstTimeChargeLoop) THEN
                IF (CALCHG + ChargeCorrection .LT. 0) THEN
                    ChargeOption=1
                ELSE
                    ChargeOption=2
                END IF
            END IF

            IF (ChargeOption .EQ. 2) THEN
                CALCHG = CALCHG + ChargeCorrection
            ELSE
                CALCHG = CALCHG
            END IF
        END IF

        RETURN

    END IF

    CALL CalcCondenserInventory(MassCoil,MassLiqCoil,MassVapCoil,CondLiqTubeLength,CondVapTubeLength,CondTwoPhaseTubeLength,CondNumLiqTubes)
    CondOUT(18)=MassCoil

    CALCHG=(CompOUT(6)+CondOUT(16)+CondOUT(17)+CondOUT(18))/UnitM

    IF (IsChargeTuning .GT. 0 .AND. MODE .NE. 2) THEN !Apply charge tuning
        ChargeCorrection=(ChargeCurveIntercept+ChargeCurveSlope*(CondLiqTubeLength-RefLiquidLength))/UnitM

        IF (FirstTimeChargeLoop) THEN
            IF (CALCHG + ChargeCorrection .LT. 0) THEN
                ChargeOption=1
            ELSE
                ChargeOption=2
            END IF
        END IF

        IF (ChargeOption .EQ. 2) THEN
            CALCHG = CALCHG + ChargeCorrection
        ELSE
            CALCHG = CALCHG
        END IF
    END IF

    RETURN

    !!VL: Previously: 
!!1002 FORMAT('0HPDM: **** FAILED TO CONVERGE ON SUBCOOLING *****',/,  '         DIFFERENCE  =',F8.3,' F')
!!1006 FORMAT('0HPDM: **** FAILED TO CONVERGE ON SUPERHEAT *****',/,   '         DIFFERENCE  =',F8.3,' F')
!!1013 FORMAT('0        DID NOT CONVERGE ON  EVAPORATOR INLET ',  'AIR TEMPERATURE FOR THIS SATURATION TEMPERATURE.' ,/,'         SET COMPRESSOR INLET SATURATION TEMPERATURE TO',  F8.3,' F AND GO BACK TO CONDENSER ITERATION.')
!!1014 FORMAT('0DRIVER: ***** FAILED TO CONVERGE ON EVAPORATOR ',  'INLET AIR TEMPERATURE *****',/, '               DIFFERENCE  =',F8.3,' F')
!!
!!
!!700 FORMAT(A44,F7.2,A5)
!!704 FORMAT(A13,F7.2,A5)

    END SUBROUTINE
