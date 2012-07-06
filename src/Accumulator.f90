    MODULE AccumulatorMod

    USE DataSimulation
    USE GeneralRoutines, ONLY: IssueRefPropError
    !IMPLICIT NONE

    PUBLIC InitAccumulator
    PUBLIC CalcAccumulatorMass
    PUBLIC CalcAccumulatorDP

    PRIVATE

    CHARACTER*80:: RefName !Refrigerant name
    REAL :: RatedDP !Rating pressure drop, kPa
    REAL :: RatedDT !Rating temperature drop, K
    REAL :: CoeffM !Curve fit coefficient
    REAL :: CoeffB !Curve fit coefficient
    REAL :: AccumDP !Pressure drop, kPa
    REAL DHOLE(2) !Hole diameters, ft
    REAL DTUBE    !Tube diameter, ft
    REAL HDIS     !Distance between holes, ft
    REAL AHGT     !Accumulator height, ft
    REAL DACC     !Accumulator diameter, ft

    INTEGER            :: RefrigIndex =0
    INTEGER(2) RefPropErr  !Error flag:1-error; 0-no error
    REAL Temperature,Quality,Pressure,Enthalpy,Entropy

    CONTAINS

    SUBROUTINE CalcAccumulatorMass(XIN,OUT)

    ! ----------------------------------------------------------------------
    !
    !   Description: HPSIM accumulator model
    !
    !   Purpose: To calculate the mass of refrigerant and liquid level
    !            in the accumulator.
    !
    !   Method: Perform pressure balance in accumulator 
    !
    !	Inputs:
    !		RefName=Refrigerant name
    !		PureRef=Refrigerant flag: 1=pure refrigerant
    !			                      0=refrigerant mixture
    !		XIN(1) = Mass flow rate, kg/s
    !		XIN(2) = Outlet refrigerant pressure, kPa
    !		XIN(3) = Outlet refrigerant enthalpy, kJ/kg
    !
    !
    !
    !	Outputs:
    !		OUT(1) = Total mass inventory, kg
    !		OUT(2) = Liquid refrigerant, kg
    !		OUT(3) = Vapor refrigerant, kg
    !		OUT(4) = Liquid level, m
    !       OUT(5) = Pressure drop, kPa
    !       OUT(6) = Error flag: 0-No error
    !                            1-Accumulator solution error
    !                            2-Refprop error
    !
    !   Author:
    !   Ipseng Iu
    !   Mechanical and Aerospace Engineering
    !   Oklahoma State University, Stillwater
    !
    !   Date: June 2005
    !
    ! ----------------------------------------------------------------------

    USE FluidProperties
    !USE FluidProperties_HPSim !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)

    !IMPLICIT NONE

    !Subroutine argument declarations
    REAL, INTENT(IN) :: XIN(3)
    REAL, INTENT(OUT) :: OUT(6)

    INTEGER ErrorFlag          !0-No error
    !1-Accumulator solution not converge
    !2-Refprop error

    REAL mdot      !Refrigerant mass flow rate, kg/s
    REAL pRo       !Outlet refrigerant pressure, kPa
    REAL hRo       !Outlet refrigerant enthalpy, kJ/kg
    REAL tRo       !Outlet refrigerant temperature, C
    REAL Tsat      !Saturated temperature, C
    REAL xRo       !Outlet refrigerant quality, -


    ! VL : Flags to assist with dismantling of GOTO-based control structures ....
    ! Change names of the flags to reflect the intention of the GOTO statements ... 
    INTEGER   :: FLAG_GOTO_15, FLAG_GOTO_40
    INTEGER   ::  FLAG_GOTO_100, FLAG_GOTO_201

    CHARACTER(LEN=12),PARAMETER :: FMT_1234 = "(8(1PE12.3))"
    CHARACTER(LEN=24),PARAMETER :: FMT_600 = "(' ACCUMULATOR IS FULL')"
    CHARACTER(LEN=57),PARAMETER :: FMT_602 = "(' ACCUML DOES NOT CONVERGE, MAX.ERROR =',1PE10.3,' LBM')"
    CHARACTER(LEN=54),PARAMETER :: FMT_604 = "(/'  ACCMAS = ',F8.3,' LBM',3X,'LIQUID LEVEL = ',F8.2)"

    !
    !**** PURPOSE:
    !       TO CALCULATE REFRIGERANT LIQUID LEVEL AND MASS IN AN ACCUMULATOR
    !
    !     ADAPTED 10/85 BY C. K. RICE
    !       FROM 'ACCUM' ROUTINE WRITTEN BY PIOTR A. DOMANSKI,
    !       NATIONAL BUREAU OF STANDARDS, 6/28/82
    !
    !**** INPUT DATA:
    !       AHGT(ACCHGT)    - ACCUMULATOR HEIGHT  (FT)
    !       DACC(ACCDIA)    - INNER DIAMETER OF ACCUMULATOR  (FT)
    !       DHOLE(1)        - INNER DIA. OF OIL RETURN HOLE  (FT)
    !       DHOLE(2)        - INNER DIA. OF UPPER HOLE  (FT)
    !       DTUBE(ATBDIA)   - INNER DIA. OF ACCUMULATOR TUBE  (FT)
    !       HDIS(HOLDIS)    - VERTICAL DISTANCE BETWEEN HOLES  (FT)
    !       RMASS           - REFRIG. MASS FLOW RATE  (LBM/H)
    !       TSUP            - REFRIG. SUPERHEAT  (F)
    !       V               - SPECIFIC VOLUME OF REFRIGERANT LEAVING ACCUMULATOR
    !                         (FT**3/LBM)
    !       VSATLQ          - SPECIFIC VOLUME OF LIQUID AT ACCUMULATOR PRESSURE
    !                         (FT**3/LBM)
    !**** OUTPUT DATA:
    !       ACCMAS          - MASS OF REFRIG. IN THE ACCUMULATOR  (LBM)
    !       VOLACC          - INTERNAL VOLUME OF ACCUMULATOR  (FT**3)
    !       XLEVEL          - LEVEL OF LIQUID IN ACCUMULATOR  (IN)
    !
    !        COMMON / ACCDIM / ACCHGT,ACCDIA,DHOLE(2),ATBDIA,HOLDIS
    !
    DIMENSION HL(2), AHOLE(2)  !, DHOLE(2)
    !
    REAL,PARAMETER :: PI=3.14159265
    !

    ! VL: Initialize default values for GOTO flags
    FLAG_GOTO_15 = .FALSE.
    FLAG_GOTO_40 = .FALSE.
    FLAG_GOTO_100 = .FALSE.
    FLAG_GOTO_201 = .FALSE.


    mdot=XIN(1)
    pRo=XIN(2)
    hRo=XIN(3)
    RMASS=mdot/0.000126 !Convert from kg/s to lbm/hr

    ErrorFlag=0

    Pressure=pRo*1000
    Enthalpy=hRo*1000
    tRo=PH(RefName,Pressure,Enthalpy,'temperature',RefrigIndex,RefPropErr)
    IF (IssueRefPropError(RefPropErr, 'Accumulator', 2, ErrorFlag, OUT(6))) RETURN

    xRo=PH(RefName,Pressure,Enthalpy,'quality',RefrigIndex,RefPropErr)
    IF (IssueRefPropError(RefPropErr, 'Accumulator', 2, ErrorFlag, OUT(6))) RETURN

    Pressure=pRo*1000
    Quality=1
    Tsat=PQ(RefName,Pressure,Quality,'temperature',RefrigIndex,RefPropErr)
    IF (IssueRefPropError(RefPropErr, 'Accumulator', 2, ErrorFlag, OUT(6))) RETURN

    rhoVap=PQ(RefName,Pressure,Quality,'density',RefrigIndex,RefPropErr)
    IF (IssueRefPropError(RefPropErr, 'Accumulator', 2, ErrorFlag, OUT(6))) RETURN

    V=1/(rhoVap*0.0625) !Convert from kg/m3 to lbm/ft3
    Quality=0
    rhoLiq=PQ(RefName,Pressure,Quality,'density',RefrigIndex,RefPropErr)
    IF (IssueRefPropError(RefPropErr, 'Accumulator', 2, ErrorFlag, OUT(6))) RETURN

    VSATLQ=1/(rhoLiq*0.0625) !Convert from kg/m3 to lbm/ft3

    TSUP=(tRo-tSat)*1.8

    !
    CNVACC = 0.001
    !
    VOLACC = 0.0
    ACCMAS = 0.0
    XLEVEL = 0.0

    ! VL: Previously: IF(AHGT.LE.0.001) GO TO 201
    IF(AHGT.LE.0.001) FLAG_GOTO_201 = .TRUE. 
    IF (FLAG_GOTO_201 .EQ. .FALSE.) THEN 
        !
        AACC = PI*DACC*DACC/4.
        VOLACC = AACC*AHGT
        !
        !       ONLY VAPOR IN ACCUMULATOR
        !
        !IF(TSUP.LT.0.) GO TO 10
        !VL: Previously: IF(xRo.LT.1.) GO TO 10
        IF(.NOT. xRo.LT.1.) THEN
            HL(1) = 0.0
            ACCMAS = VOLACC/V
            VHGT=AHGT
            !VL: Previously: GO TO 100
            FLAG_GOTO_100 = .TRUE.
        END IF

        IF (FLAG_GOTO_100 .EQ. FALSE) THEN

            !
            !       LIQUID IN ACCUMULATOR
            !
            !10      X = -TSUP
            !VL: Previously: 10  X = xRo


            X = xRo
            RMS = RMASS/3600.
            RMSL = (1.-X)*RMS
            RO = 1./VSATLQ
            !
            !       LIQUID LEVEL FOR CASE OF BOTTOM HOLE ALONE
            !
            AHOLE(1) = PI*DHOLE(1)*DHOLE(1)/4.
            ATUBE = PI*DTUBE*DTUBE/4.
            WL = RMSL/AHOLE(1)
            PD = WL*WL/(.585*.585*2.*RO)
            !PD=0.5*(RMSL/AHOLE(1))**2./RO
            PDYN = 0.5*(X*RMS/ATUBE)**2.*V
            HL(1) = (PD-PDYN)/(RO*32.2)
            IF(HL(1).LT.0.) HL(1) = 0.
            VHGT = AHGT-HL(1)
            VHGT = AMAX1(0.,VHGT)
            AMASS2 = AACC*(HL(1)*RO+VHGT/V)
            !
            !       CHECK IF BELOW SECOND HOLE
            !
            !VL: Previously: IF(HL(1).LT.HDIS.OR.HDIS.EQ.0.) GO TO 40
            IF(HL(1).LT.HDIS.OR.HDIS.EQ.0.) FLAG_GOTO_40 = .TRUE.
            IF (FLAG_GOTO_40 .EQ. FALSE) THEN

                AHOLE(2) = PI*DHOLE(2)*DHOLE(2)/4.
                !
                !       CHECK FOR FULL ACCUMULATOR
                !
                DP1MAX = AHGT*RO*32.2 + PDYN
                DP2MAX = (AHGT-HDIS)*RO*32.2 + PDYN
                RMMAX = 0.585*AHOLE(1)*SQRT(2.*RO*DP1MAX) +0.585*AHOLE(2)*SQRT(2.*RO*DP2MAX)


                ! VL: Previously: IF(RMSL.LT.RMMAX) GO TO 15
                IF(RMSL.LT.RMMAX) FLAG_GOTO_15 = .TRUE.
                IF (FLAG_GOTO_15 .EQ. .FALSE.) THEN
                    VHGT = 0.0
                    HL(1) = AHGT
                    AMASS2 = AACC*AHGT*RO
                    !VL: Previously:GO TO 40
                    FLAG_GOTO_40 = .TRUE.

                    !
                    !       FIND LEVEL ABOVE SECOND HOLE
                    !
                END IF        
            END IF

            IF (FLAG_GOTO_40 .EQ. FALSE) THEN
                ! VL : Previously 15  CONTINUE
                HL(2) = HL(1)-HDIS
                DP = HL(2)*RO*32.2 + PDYN
                RM = 0.585*AHOLE(2)*SQRT(2.*RO*DP)
                DIFF1 = RM
                AMASS1 = AMASS2
                Z1 = HL(1)
                Z2 = HDIS
                IHOLE = 0
                !
                !VL: Previously:DO 30 J=1,12
                DO J=1,12
                    HL(1) = Z2
                    HL(2) = Z2-HDIS
                    RMT = 0.
                    !
                    !VL: Previously:DO 16 I=1,2
                    DO I=1,2
                        DP = HL(I)*RO*32.2 + PDYN
                        !       IF(HL(I).EQ.0.) DP=0.
                        RM = 0.585*AHOLE(I)*SQRT(2.*RO*DP)
                        !VL: Previously:16                  RMT = RMT + RM
                        RMT = RMT + RM
                    END DO

                    !
                    DIFF2 = RMT-RMSL
                    IF(J.EQ.1.AND.DIFF2.GT.0.0) IHOLE = 1
                    VHGT = AHGT - HL(1)
                    VHGT = AMAX1(0.,VHGT)
                    AMASS2 = AACC* (HL(1)*RO + VHGT/V)
                    !       WRITE(6,FMT_1234) Z1,Z2,DIFF1,DIFF2,AMASS1,AMASS2,RMT,RMSL
                    !!VL: Previously: 1234   FORMAT(8(1PE12.3))
                    !
                    !       SKIP OUT IF LEVEL WILL NOT RISE ABOVE SECOND HOLE
                    !
                    !VL: Previously: IF(IHOLE.EQ.1) GO TO 40
                    IF(IHOLE.EQ.1) THEN
                        FLAG_GOTO_40 = .TRUE.
                        EXIT
                    END IF

                    !
                    !       CHECK FOR CONVERGENCE ON LEVEL ABOVE SECOND HOLE
                    !
                    !VL: Previously: IF(ABS(AMASS1-AMASS2).LT.CNVACC) GO TO 40
                    IF(ABS(AMASS1-AMASS2).LT.CNVACC) THEN
                        FLAG_GOTO_40 = .TRUE.
                        EXIT
                    END IF
                    SLOPE = (Z1-Z2)/(DIFF1-DIFF2)
                    !VL: Previously: IF(ABS(DIFF1).LT.ABS(DIFF2)) GO TO 20
                    IF(.NOT. ABS(DIFF1).LT.ABS(DIFF2)) THEN
                        AMASS1 = AMASS2
                        DIFF1 = DIFF2
                        Z1 = Z2
                    END IF

                    !VL: Previously: 20                  Z2 = Z1 - DIFF1*SLOPE
                    Z2 = Z1 - DIFF1*SLOPE
                    Z2 = AMAX1(Z2,HDIS)
                    !VL: Previously:30          CONTINUE
                END DO

                IF (FLAG_GOTO_40 .EQ. FALSE) THEN
                    !
                    !       PRINT RESULTS
                    !
                    ERROR = AACC*ABS(Z2-Z1)*RO
                    WRITE(6,FMT_602) ERROR
                END IF

            END IF
            !VL: Previously: 40          CONTINUE
            !        IF(VHGT.EQ.0.) WRITE(6,FMT_600)
            ACCMAS = AMASS2
        END IF
        !VL: Previously:100 XLEVEL = HL(1)*12.
        XLEVEL = HL(1)*12.
    END IF

    !VL: Previously:201 CONTINUE
    !       WRITE(6,FMT_604) ACCMAS,XLEVEL
    !
    !!VL: Previously: 600 FORMAT(' ACCUMULATOR IS FULL')
    !!VL: Previously: 602 FORMAT(' ACCUML DOES NOT CONVERGE, MAX.ERROR =',1PE10.3,' LBM')


    !!VL: Previously: 604    FORMAT(/'  ACCMAS = ',F8.3,' LBM',3X,'LIQUID LEVEL = ',F8.2,
    !     &          ' IN')
    !

    OUT(1)=ACCMAS*0.4563    !Total mass, convert from lbm to kg
    OUT(3)=OUT(1)*VHGT/AHGT !Liquid mass
    OUT(2)=OUT(1)-OUT(3)    !Vapor mass
    OUT(4)=XLEVEL*0.0254    !Liquid level, convert from in to m
    OUT(5)=AccumDP



    !VL: Previously: 200 CONTINUE
    OUT(6)=ErrorFlag

    RETURN

    END SUBROUTINE CalcAccumulatorMass

    !******************************************************************************

    SUBROUTINE CalcAccumulatorDP(mdot,TsatEvp,TsatCnd,Subcooling,Superheat,Xliq,Xvap,EstDP)

    ! ----------------------------------------------------------------------
    !
    !   Purpose: To calculate the pressure drop.
    !
    !   Method: Catalog data curve fit
    !
    !   Author:
    !   Ipseng Iu
    !   Mechanical and Aerospace Engineering
    !   Oklahoma State University, Stillwater
    !
    !   Date: July 2005
    !
    ! ----------------------------------------------------------------------

    USE FluidProperties

    IMPLICIT NONE

    REAL, INTENT(IN) :: mdot !Refrigerant mass flow rate, kg/s
    REAL, INTENT(IN) :: TsatEvp !Evaporating temperature, C
    REAL, INTENT(IN) :: TsatCnd !Condensing temperature, C
    REAL, INTENT(IN) :: Subcooling !Subcooling, K
    REAL, INTENT(IN) :: Superheat !Superheat, K
    REAL, INTENT(INOUT) :: Xliq !Liquid line quality
    REAL, INTENT(INOUT) :: Xvap !Suction line quality
    REAL, INTENT(OUT) :: EstDP !Estimated pressure drop, kPa

    INTEGER ErrorFlag          !0-No error
    !1-Accumulator solution not converge
    !2-Refprop error

    REAL MaxCapacity !Max. system capacity, ton 
    REAL QsysTon     !System capacity, ton
    REAL EstDT       !Estimated temperature drop, kPa
    REAL Psuc        !Suction presure, kPa
    REAL Pdis        !Discharge pressure, kPa 
    REAL Hliq        !Liquid enthalpy, kJ/kg
    REAL Hvap        !Vapor enthalpy, kJ/kg
    REAL Psat1,Psat2 !Saturation pressure, kPa

    !Flow:

    IF (Xliq .GT. 1) Xliq=1
    IF (Xliq .LT. 0) Xliq=0
    IF (Xvap .GT. 1) Xvap=1
    IF (Xvap .LT. 0) Xvap=0

    ErrorFlag=0

    Temperature=TsatEvp
    Quality=1
    Psuc=TQ(RefName,Temperature,Quality,'pressure',RefrigIndex,RefPropErr)
    IF (IssueRefPropError(RefPropErr, 'Accumulator', 2, ErrorFlag)) RETURN
    Psuc=Psuc/1000

    Temperature=TsatCnd
    Quality=1
    Pdis=TQ(RefName,Temperature,Quality,'pressure',RefrigIndex,RefPropErr)
    IF (IssueRefPropError(RefPropErr, 'Accumulator', 2, ErrorFlag)) RETURN
    Pdis=Pdis/1000

    IF (Superheat .GT. 0) THEN
        Pressure=Psuc*1000
        Temperature=TsatEvp+Superheat
        Hvap=TP(RefName,Temperature,Pressure,'enthalpy',RefrigIndex,RefPropErr)
        IF (IssueRefPropError(RefPropErr, 'Accumulator', 2, ErrorFlag)) RETURN
        Hvap=Hvap/1000
    ELSE
        Pressure=Psuc*1000
        Quality=Xvap
        Hvap=PQ(RefName,Pressure,Quality,'enthalpy',RefrigIndex,RefPropErr)
        IF (IssueRefPropError(RefPropErr, 'Accumulator', 2, ErrorFlag)) RETURN
        Hvap=Hvap/1000
    END IF

    IF (Subcooling .GT. 0) THEN
        Pressure=Pdis*1000
        Temperature=TsatCnd-Subcooling
        Hliq=TP(RefName,Temperature,Pressure,'enthalpy',RefrigIndex,RefPropErr)
        IF (IssueRefPropError(RefPropErr, 'Accumulator', 2, ErrorFlag)) RETURN
        Hliq=Hliq/1000
    ELSE
        Pressure=Pdis*1000
        Quality=Xliq
        Hliq=PQ(RefName,Pressure,Quality,'enthalpy',RefrigIndex,RefPropErr)
        IF (IssueRefPropError(RefPropErr, 'Accumulator', 2, ErrorFlag)) RETURN
        Hliq=Hliq/1000
    END IF

    QsysTon=mdot*(Hvap-Hliq)*0.28435 !Convert from kW to ton

    IF (CoeffM .NE. 0 .OR. CoeffB .NE. 0) THEN
        MaxCapacity = CoeffM * TsatEvp + CoeffB

        IF (RatedDP .NE. 0) THEN
            EstDP = QsysTon / MaxCapacity * RatedDP
        ELSE
            EstDT = QsysTon / MaxCapacity * RatedDT
            Psat1 = Psuc

            Temperature=TsatEvp-EstDT
            Quality=1
            Psat2=TQ(RefName,Temperature,Quality,'pressure',RefrigIndex,RefPropErr)
            IF (IssueRefPropError(RefPropErr, 'Accumulator', 2, ErrorFlag)) RETURN
            Psat2=Psat2/1000

            RatedDP = Psat1 - Psat2
            EstDP = QsysTon / MaxCapacity * RatedDP
        END IF

    END IF

    AccumDP=EstDP

    RETURN

    END SUBROUTINE CalcAccumulatorDP

    !******************************************************************************

    SUBROUTINE InitAccumulator(PAR,Ref$)

    ! ----------------------------------------------------------------------
    !
    !   Purpose: To initialize accumulator parameters
    !
    !   Method: Load the parameters to public variables
    !
    !   Inputs:
    !		Ref$ = Refrigerant name
    !		PAR(1) = Accumulator inside diameter, m
    !       PAR(2) = Accumulator internal height, m
    !		PAR(3) = J-tube lower hole diameter, m
    !		PAR(4) = J-tube upper hole diameter, m
    !		PAR(5) = Distance between holes on J-tube, m
    !		PAR(6) = J-tube inside diameter, m
    !		PAR(7) = Rating pressure drop, kPa
    !	    PAR(8) = Rating temperature drop, K
    !       PAR(9) = Curve fit coefficient M
    !       PAR(10) = Curve fit coefficient B

    !
    !   Author:
    !   Ipseng Iu
    !   Mechanical and Aerospace Engineering
    !   Oklahoma State University, Stillwater
    !
    !   Date: July 2005
    !
    ! ----------------------------------------------------------------------

    IMPLICIT NONE

    CHARACTER*80, INTENT(IN) :: Ref$ !Refrigerant name
    REAL, INTENT(IN) :: PAR(10)

    !Flow:

    Refname=Ref$

    !Convert from m to ft
    DACC = PAR(1)/0.3048 !ACCDIA
    AHGT = PAR(2)/0.3048 !ACCHGT
    DHOLE(1)=PAR(3)/0.3048
    DHOLE(2)=PAR(4)/0.3048
    HDIS = PAR(5)/0.3048 !HOLDIS
    DTUBE = PAR(6)/0.3048 !ATBDIA

    RatedDP=PAR(7)
    RatedDT=PAR(8)
    CoeffM=PAR(9)
    CoeffB=PAR(10)

    RETURN

    END SUBROUTINE InitAccumulator

    END MODULE
