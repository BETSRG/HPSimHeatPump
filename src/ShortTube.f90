    MODULE ShortTubeMod

    IMPLICIT NONE

    PUBLIC ShortTube
    PUBLIC ShortTubeChoi
    PUBLIC ShortTubePayne

    PRIVATE 

    REAL, PARAMETER :: PI=3.14159265
    REAL, PARAMETER :: UnitL=0.3048  !(ft * UnitL = m)

    REAL :: PiExpDev
    REAL :: HiExpDev
    REAL :: TiExpDev
    REAL :: PoExpDev
    REAL :: HoExpDev
    REAL :: ToExpDev

    CONTAINS
    !***********************************************************************************

    SUBROUTINE ShortTube(Ref$,PureRef,XIN,PAR,OUT)

    !-----------------------------------------------------------------------------------
    !
    !  Description:
    !  To calculate mass flow rate through a short tube orifice using semi-empirical
    !  model
    !
    !  EnergyPlus REFPROP subroutines required
    !
    !  Inputs:
    !  Ref$=Refrigerant name
    !  PureRef=Refrigerant flag: 1=pure refrigerant
    !                            0=refrigerant mixture
    !  XIN(1)=Compressor mass flow rate, kg/s
    !  XIN(2)=Exp. device inlet pressure, kPa
    !  XIN(3)=Exp. device inlet enthalpy, kJ/kg
    !  XIN(4)=Evaporator inlet pressure, kPa
    !  XIN(5)=Evaporator outlet pressure, kPa
    !
    !  Parameters:
    !  PAR(1)=Short tube length, m
    !  PAR(2)=Short tube diameter, m
    !  PAR(3)=45 deg chamfer depth, m
    !  PAR(4)=Number of circuits in evaporator
    !  PAR(5)=Distributor tube length, m
    !
    !  Outputs:
    !  OUT(1)=Exp. device mass flow rate, kg/s
    !  OUT(2)=Exp. device outlet pressure, kPa
    !  OUT(3)=Exp. device outlet temperature, C
    !  OUT(4)=Exp. device outlet quality
    !  OUT(5)=Mass in distributor tubes, kg
    !  OUT(6)=Distributor tube capacity, kW
    !  OUT(7) = Error flag: 0-No error
    !                       1-Short tube solution error
    !                       2-Refprop error
    !
    !  Reference:
    !  Aaron, D.A. and Domanski, P.A. (1990). Experimentation, analysis, and
    !    correlation of refrigerant-22 flow through short tube restrictors. ASHRAE
    !    transactions. 1(2), pp. 729-742.
    !
    !  Kim, Y. and O'Neal, D.L. (1994). Two-phase flow of R22 through short tube
    !    orifices. ASHRAE transactions. 100(1), pp. 323-334.
    !
    !  Kim, Y., O'Neal, D.L. and Yuan, X. (1994). Two-phase flow of HFC-134A and
    !    CFC-12 through short tube orifices. ASHRAE transactions. 100(2), pp. 582-591.
    !
    !  Payne, W.V. and O'Neal, D.L. (1998). Mass flow characteristics of R407C
    !    through short-tube orifice. ASHRAE transactions. 104(1A), pp. 197-209.
    !
    !  Payne, W.V. and O'Neal, D.L. (1999). Multiphase flow of refrigerant 410A
    !    through short tube orifices. ASHRAE transactions. 105(2), pp. 66-74.
    !
    !  Author:
    !  Ipseng Iu
    !  Mechanical and Aerospace Engineering
    !  Oklahoma State University, Stillwater	
    !
    !  Date: June 2002
    !
    !-----------------------------------------------------------------------------------

    USE FluidProperties_HPSim !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)

    IMPLICIT NONE

    !Subroutine argument declarations
    CHARACTER*80,     INTENT(IN)  :: Ref$    !Refrigerant name
    INTEGER(2),       INTENT(IN)  :: PureRef !Refrigerant flag: 1-pure refrigerant
    !0-refrigerant mixture
    REAL, INTENT(IN)  :: XIN(5)
    REAL, INTENT(IN)  :: PAR(5)
    REAL, INTENT(OUT) :: OUT(7)

    !Subroutine local variables
    !CHARACTER (len=15) :: Property           
    INTEGER            :: RefrigIndex =0
    REAL Temperature,Quality,Pressure,Enthalpy

    REAL :: LshTube     !Short tube length, m
    REAL :: LdisTube    !Distributor tube length, m
    REAL :: DshTube     !Short tube diameter, m
    REAL :: IDDISTUBE   !Distributor inside diameter, in
    REAL :: VolDisTube  !Distributor tube volumn, m^3
    REAL :: MassDisTube !Mass in distributor tube, kg
    REAL :: Depth       !Short tube 45 deg chamfer depth, m
    REAL :: Subcool     !Degree of subcooling, C
    REAL :: PiExp       !Inlet pressure of exp. device (Up stream pressure), kPa
    REAL :: HiExp       !Exp. device inlet enthalpy, kJ/kg
    REAL :: TiExp       !Exp. device inlet temperature, C
    REAL :: XiExp       !Exp. device inlet quality
    REAL :: PoExp       !Exp. device outlet pressure, kPa
    REAL :: HoExp       !Exp. device outlet enthalpy, kJ/kg
    REAL :: ToExp       !Exp. device outlet temperature, C
    REAL :: XoExp       !Exp. device outlet quality
    REAL :: DPdisTot    !Total pressure drop in distributor, kPa
    REAL :: PiEvp       !Evaporator inlet pressure, kPa
    REAL :: HiEvp       !Evaporator inlet enthalpy, kJ/kg
    REAL :: TiEvp       !Evaporator inlet temperature, C
    REAL :: XiEvp       !Evaporator inlet quality
    REAL :: PoEvp       !Evaporator outlet pressure, kPa
    REAL :: mdotCmp     !Mass flow rate from compressor, kg/s
    REAL :: mdotExp     !Mass flow rate from exp. device, kg/s
    REAL :: TsiExp      !Liquid saturation temperature of the upstream fluid, C
    REAL :: QdisTube    !Distributor tube capacity, kW
    REAL :: rhoiExp     !Upstream liquid density, kg/m^3
    REAL :: rhofiExp    !Upstream fluid density, kg/m^3
    REAL :: rhogiExp    !Upstream vapor density, kg/m^3
    REAL :: rhoiEvp     !Evaporator inlet density, kg/m^3
    REAL :: Acs         !Cross-sectional area, m^2
    REAL :: SUBC        !Temperature ratio, T is in K
    REAL :: EVAP        !Pressure ratio, P is in absolute pressure
    REAL :: PRA         !Pressure ratio, P is in absolute pressure
    REAL :: DR          !Diameter ratio
    REAL :: LD          !Ratio of tube length to diameter
    REAL :: Pf          !Downstream pressure for unchoked orifice flow or correlated 
    !saturation pressure used in short tube model, kPa
    REAL :: Cc          !Correction factor for chamfered inlet short tubes
    REAL :: Ctp         !Correction factor for two-phase quality
    REAL :: Psat        !Saturation pressure, kPa
    REAL :: Pcr         !Critical pressure, kPa
    REAL :: Tcr         !Critical temperature, K
    REAL :: Y           !Parameter for two-phase quality correction factor calc.
    REAL :: Dref        !Reference diameter, mm
    REAL :: HoEvpRtd    !Evaporator outlet rated enthalpy, kJ/kg
    INTEGER(2)		 :: Nckts		!Number of circuits in evaporator
    INTEGER			 :: ErrorFlag	!0-No error
    !1-Solution error
    !2-Refprop error

    REAL :: a(4)  !Emperical constants
    REAL :: b(11) !Emperical constants
    REAL :: c(3)  !Emperical constants
    REAL, PARAMETER :: Const=5.08e-005 !Emperical constants

    !NIST Refrigerant property variables and functions
    INTEGER(2) RefPropOpt			!Ref prop calc. option
    INTEGER(2) RefPropErr			!Error flag:1-error; 0-no error
    REAL RefProp(28)

    !Flow:

    mdotCmp=XIN(1)
    PiExp=XIN(2)
    HiExp=XIN(3)
    PiEvp=XIN(4)
    PoEvp=XIN(5)

    LshTube=PAR(1)
    DshTube=PAR(2)
    Depth=PAR(3)
    Nckts=PAR(4)
    LdisTube=PAR(5)

    ErrorFlag=0 !Initialize

    Tcr=Tcrit(Ref$)+273.15
    Pcr=Pcrit(Ref$)/1000

    Pressure=PiExp*1000
    Enthalpy=HiExp*1000
    TiExp=PH(Ref$,Pressure,Enthalpy,'temperature',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    XiExp=PH(Ref$,Pressure,Enthalpy,'quality',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    rhoiExp=PH(Ref$,Pressure,Enthalpy,'density',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF

    Quality=0
    rhofiExp=PQ(Ref$,Pressure,Quality,'density',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    Quality=1
    rhogiExp=PQ(Ref$,Pressure,Quality,'density',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF

    Pressure=PiExp*1000
    Quality=0
    TsiExp=PQ(Ref$,Pressure,Quality,'temperature',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF

    HiEvp=HiExp

    Pressure=PiEvp*1000
    Enthalpy=HiEvp*1000
    TiEvp=PH(Ref$,Pressure,Enthalpy,'temperature',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    XiEvp=PH(Ref$,Pressure,Enthalpy,'quality',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    rhoiEvp=PH(Ref$,Pressure,Enthalpy,'density',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF

    IF (LdisTube .GT. 0) THEN
        LDISTUBE=LdisTube*12/UnitL !Distributor tube length, in
        CALL Distributor(Ref$,LDISTUBE,Nckts,mdotCmp,TiExp,HiExp,PoEvp, &
        HoEvpRtd,QdisTube,DPdisTot,ErrorFlag)

        IF (DPdisTot .LT. 0) THEN
            DPdisTot = 0
        END IF
        IF (DPdisTot .GT. PiEvp) THEN
            DPdisTot = 0
        END IF

        IDDISTUBE=1./4.-12.0E-3 !1/4 in OD, 12 mil-think
        VolDisTube=((IDDISTUBE/12**2)*PI/4*LDISTUBE/12*Nckts)*UnitL**3
        MassDisTube=rhoiEvp*VolDisTube

        PoExp=PiEvp+DPdisTot

    ELSE
        PoExp=PiEvp
        MassDisTube=0
    END IF

    HoExp=HiExp

    Pressure=PoExp*1000
    Enthalpy=HoExp*1000
    ToExp=PH(Ref$,Pressure,Enthalpy,'temperature',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    XoExp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF

    IF (XiExp .LT. 0.0) THEN
        XiExp = 0.0
    END IF

    Temperature=TiExp
    Quality=0
    Psat=TQ(Ref$, Temperature, Quality, 'pressure', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    Psat=Psat/1000

    Acs=DshTube**2*PI/4 

    SUBC=(TsiExp-TiExp)/Tcr
    IF ((TsiExp-TiExp) .LT. 0) THEN
        SUBC=0
    END IF

    EVAP=(Pcr-PoExp)/Pcr

    PRA=PiExp/Pcr

    LD=LshTube/DshTube

    IF (TRIM(Ref$) .EQ. "R407C") THEN !R407C
        Dref=1.524E-3
        a(1)=-4.45974577;  a(2)=10.69467130;  a(3)=-0.55303036;   a(4)=0.39429366
        b(1)=0.963034325;  b(2)=4.286408416;  b(3)=-0.278235435;  b(4)=-0.043090943
        b(5)=0.916226528;  b(6)=0.071794702;  b(7)=0.499098698;   b(8)=-0.208417565
        b(9)=-0.034680678; b(10)=1.844061084; b(11)=-0.091235910; 

    ELSE IF (TRIM(Ref$) .EQ. "R410A") THEN !R410A
        Dref=1.524E-3
        a(1)=3.693038038;  a(2)=0.120175996;  a(3)=0.194241638;  a(4)=0.022577667
        b(1)=0.874805831;  b(2)=3.131470913;  b(3)=-0.214726407; b(4)=0.083394737
        b(5)=0.901559277;  b(6)=-0.020574536; b(7)=0.944446846;  b(8)=-0.418400083
        b(9)=-0.025322802; b(10)=2.33507746;  b(11)=0.068890172 

    ELSE IF (TRIM(Ref$) .EQ. "R12") THEN !R12
        Dref=1.35E-3
        a(1)=-2.6519;  a(2)=5.7705;  a(3)=-0.4474; a(4)=0.3820 !use R134a data
        b(1)=1.0011;   b(2)=11.3523; b(3)=-0.2176;  b(4)=-0.2150
        b(5)=1.0643;   b(6)=0.0;     b(7)=0.0;      b(8)=-0.1905
        b(9)=-0.00615; b(10)=2.4291; b(11)=-0.0437 
    ELSE IF (TRIM(Ref$) .EQ. "R22" .OR. TRIM(Ref$) .EQ. "PROPANE") THEN !R22
        Dref=1.35E-3
        a(1)=3.693038038;  a(2)=0.120175996;  a(3)=0.194241638;  a(4)=0.022577667 !use R410a data
        b(1)=1.005;   b(2)=5.7367;  b(3)=-0.485; b(4)=-0.179
        b(5)=0.9948;  b(6)=0.268;   b(7)=2.716;  b(8)=-0.226
        b(9)=-0.021;  b(10)=2.0;    b(11)=-0.092
    ELSE 	!R134a, default refrigerant
        Dref=1.35E-3
        a(1)=-2.6519;  a(2)=5.7705;  a(3)=-0.4474; a(4)=0.3820
        b(1)=1.0156;   b(2)=10.0612; b(3)=-0.3296; b(4)=-0.1758
        b(5)=1.0831;   b(6)=0.0;     b(7)=0.0;     b(8)=-0.1802
        b(9)=-0.00214; b(10)=2.9596; b(11)=-0.0745 
    END IF
    !use R410a data, not available for other refrigerants or the coefficients don't work well for two-phase inlet condition
    a(1)=3.693038038;  a(2)=0.120175996;  a(3)=0.194241638;  a(4)=0.022577667 
    c(1)=0.02655; c(2)=0.70775; c(3)=0.22684 !Use R22 data, not available for other refrigerants

    DR=DshTube/Dref

    Pf=Psat*(b(1)+b(2)*PRA**b(3)*LD**b(4)*SUBC**b(5) &
    +b(6)*PRA**b(7) &
    +b(8)*EXP(b(9)*DR*LD**b(10))+b(11)*EVAP)

    !Domanski
    !SUBC=(TsiExp-TiExp)/(TsiExp+237.15)
    !EVAP=(Psat-PoExp)/Psat
    !Pf=Psat*(1+12.599*SUBC**1.293-0.1229*EXP(-0.0169*(Depth/DshTube)**2)-0.04753*EVAP**0.6192)

    Cc=1.0+c(1)*LD**c(2)*(Depth/DshTube)**c(3)

    IF (SUBC .LE. 0 .AND. XiExp .GE. 0.02) THEN
        Y=XiExp/(1-XiExp)*(rhofiExp/rhogiExp)**0.5
        Ctp=1/(1+a(1)*XiExp)*(1+a(2)*LD**a(3)*Y**(a(4)*LOG(LD)))
    ELSE
        Ctp=1.0  !Inlet liquid is subcooled, no correction needed
    END IF

    IF (PiExp-Pf .LT. 0) THEN

        ErrorFlag=1
        !VL: Previously: GOTO 200
        OUT(7)=ErrorFlag
        PiExpDev = XIN(2)
        HiExpDev = XIN(3)
        PoExpDev = OUT(2)
        ToExpDev = OUT(3)
        hoExpDev= PQ(Ref$,PoExpDev,XoExp,'enthalpy',RefrigIndex,RefPropErr)
        RETURN

    END IF
    mdotExp=Ctp*Cc*Acs*(2*rhoiExp*(PiExp-Pf)*1000)**0.5

    Pressure=PiEvp*1000
    Enthalpy=HiEvp*1000
    TiEvp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    XiEvp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF

    IF (mdotExp .LT. 0) THEN

        ErrorFlag=1
        !VL: Previously: GOTO 200
        OUT(7)=ErrorFlag
        PiExpDev = XIN(2)
        HiExpDev = XIN(3)
        PoExpDev = OUT(2)
        ToExpDev = OUT(3)
        hoExpDev= PQ(Ref$,PoExpDev,XoExp,'enthalpy',RefrigIndex,RefPropErr)
        RETURN

    END IF

    OUT(1)=mdotExp
    OUT(2)=PoExp
    OUT(3)=ToExp
    OUT(4)=XoExp
    OUT(5)=MassDisTube
    OUT(6)=QdisTube

    !VL: Previously: 200 CONTINUE
    OUT(7)=ErrorFlag

    PiExpDev = XIN(2)
    HiExpDev = XIN(3)
    PoExpDev = OUT(2)
    ToExpDev = OUT(3)
    hoExpDev= PQ(Ref$,PoExpDev,XoExp,'enthalpy',RefrigIndex,RefPropErr)

    RETURN

    END SUBROUTINE ShortTube

    !***********************************************************************************

    SUBROUTINE ShortTubeChoi(Ref$,PureRef,XIN,PAR,OUT)

    !-----------------------------------------------------------------------------------
    !
    !  Description:
    !  To calculate mass flow rate through a short tube orifice using Buckingham Pi
    !  theorem
    !
    !  EnergyPlus REFPROP subroutines required
    !
    !  Inputs:
    !  Ref$=Refrigerant name
    !  PureRef=Refrigerant flag: 1=pure refrigerant
    !                            0=refrigerant mixture
    !  XIN(1)=Compressor mass flow rate, kg/s
    !  XIN(2)=Exp. device inlet pressure, kPa
    !  XIN(3)=Exp. device inlet enthalpy, kJ/kg
    !  XIN(4)=Evaporator inlet pressure, kPa
    !  XIN(5)=Evaporator outlet pressure, kPa
    !
    !  Parameters:
    !  PAR(1)=Short tube length, m
    !  PAR(2)=Short tube diameter, m
    !  PAR(3)=45 deg chamfer depth, m
    !  PAR(4)=Number of circuits in evaporator
    !  PAR(5)=Distributor tube length, m
    !
    !  Outputs:
    !  OUT(1)=Exp. device mass flow rate, kg/s
    !  OUT(2)=Exp. device outlet pressure, kPa
    !  OUT(3)=Exp. device outlet temperature, C
    !  OUT(4)=Exp. device outlet quality
    !  OUT(5)=Mass in distributor tubes, kg
    !  OUT(6)=Distributor tube capacity, kW
    !  OUT(7) = Error flag: 0-No error
    !                       1-Short tube solution error
    !                       2-Refprop error
    !
    !  Reference:
    !  Choi,J.; Chung, J.T. and Kim, Y. (2004). A generalized correlation for
    !    two-phase flow of alternative refrigerants through short tube orifices.
    !    Int. J. of Refrig. 27, pp. 393-400.
    !
    !  Author:
    !  Ipseng Iu
    !  Mechanical and Aerospace Engineering
    !  Oklahoma State University, Stillwater	
    !
    !  Date: June 2002
    !
    !-----------------------------------------------------------------------------------

    USE FluidProperties_HPSim !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12)

    IMPLICIT NONE

    !Subroutine argument declarations
    CHARACTER*80,     INTENT(IN)  :: Ref$    !Refrigerant name
    INTEGER(2),       INTENT(IN)  :: PureRef !Refrigerant flag: 1-pure refrigerant
    !0-refrigerant mixture
    REAL, INTENT(IN)  :: XIN(5)
    REAL, INTENT(IN)  :: PAR(5)
    REAL, INTENT(OUT) :: OUT(7)

    !Subroutine local variables         
    INTEGER            :: RefrigIndex =0
    REAL Temperature,Quality,Pressure,Enthalpy

    REAL :: LshTube     !Short tube length, m
    REAL :: LdisTube    !Distributor tube length, m
    REAL :: DshTube     !Short tube diameter, m
    REAL :: IDDISTUBE   !Distributor inside diameter, in
    REAL :: VolDisTube  !Distributor tube volumn, m^3
    REAL :: MassDisTube !Mass in distributor tube, kg
    REAL :: Depth       !Short tube 45 deg chamfer depth, m
    REAL :: Subcooling  !Degree of subcooling, C
    REAL :: PiExp       !Inlet pressure of exp. device (Up stream pressure), kPa
    REAL :: HiExp       !Exp. device inlet enthalpy, kJ/kg
    REAL :: TiExp       !Exp. device inlet temperature, C
    REAL :: XiExp       !Exp. device inlet quality
    REAL :: PoExp       !Exp. device outlet pressure, kPa
    REAL :: HoExp       !Exp. device outlet enthalpy, kJ/kg
    REAL :: ToExp       !Exp. device outlet temperature, C
    REAL :: XoExp       !Exp. device outlet quality
    REAL :: DPdisTot    !Total pressure drop in distributor, kPa
    REAL :: PiEvp       !Evaporator inlet pressure, kPa
    REAL :: HiEvp       !Evaporator inlet enthalpy, kJ/kg
    REAL :: TiEvp       !Evaporator inlet temperature, C
    REAL :: XiEvp       !Evaporator inlet quality
    REAL :: PoEvp       !Evaporator outlet pressure, kPa
    REAL :: mdotCmp     !Mass flow rate from compressor, kg/s
    REAL :: mdotExp     !Mass flow rate from exp. device, kg/s
    REAL :: TsiExp      !Liquid saturation temperature of the upstream fluid, C
    REAL :: QdisTube    !Distributor tube capacity, kW
    REAL :: rhoAvg      !Average density, kg/m^3
    REAL :: rhoiExp     !Upstream liquid density, kg/m^3
    REAL :: rhofiExp    !Upstream fluid density, kg/m^3
    REAL :: rhogiExp    !Upstream vapor density, kg/m^3
    REAL :: rhoiEvp     !Evaporator inlet density, kg/m^3
    REAL :: mufiExp     !Upstream fluid viscosity, Pa-s
    REAL :: mugiExp     !Upstream vapor viscosity, Pa-s
    REAL :: sigma       !Surface tension, N/m
    REAL :: Acs         !Cross-sectional area, m^2
    REAL :: SUBC        !Temperature ratio, T is in K
    REAL :: EVAP        !Pressure ratio, P is in absolute pressure
    REAL :: PRA         !Pressure ratio, P is in absolute pressure
    REAL :: DR          !Diameter ratio
    REAL :: LD          !Ratio of tube length to diameter
    REAL :: Pf          !Downstream pressure for unchoked orifice flow or correlated 
    !saturation pressure used in short tube model, kPa
    REAL :: Cc          !Correction factor for chamfered inlet short tubes
    REAL :: Ctp         !Correction factor for two-phase quality
    REAL :: Psat        !Saturation pressure, kPa
    REAL :: Pcr         !Critical pressure, kPa
    REAL :: Tcr         !Critical temperature, K
    REAL :: Y           !Parameter for two-phase quality correction factor calc.
    REAL :: Dref        !Reference diameter, mm
    REAL :: HoEvpRtd    !Evaporator outlet rated enthalpy, kJ/kg
    INTEGER(2)		 :: Nckts		!Number of circuits in evaporator
    INTEGER			 :: ErrorFlag	!0-No error
    !1-Solution error
    !2-Refprop error

    !Buckingham pi parameters
    REAL pi01,pi02,pi03,pi04,pi05,pi06,pi07,pi08,pi09,pi10,pi11

    REAL :: a(4)  !Emperical constants
    REAL :: b(11) !Emperical constants
    REAL :: c(3)  !Emperical constants
    REAL, PARAMETER :: Const=5.08e-005 !Emperical constants

    !NIST Refrigerant property variables and functions
    INTEGER(2) RefPropOpt			!Ref prop calc. option
    INTEGER(2) RefPropErr			!Error flag:1-error; 0-no error
    REAL RefProp(28)

    !Flow:

    mdotCmp=XIN(1)
    PiExp=XIN(2)
    HiExp=XIN(3)
    PiEvp=XIN(4)
    PoEvp=XIN(5)

    LshTube=PAR(1)
    DshTube=PAR(2)
    Depth=PAR(3)
    Nckts=PAR(4)
    LdisTube=PAR(5)

    ErrorFlag=0 !Initialize

    Pressure=PiExp*1000
    Enthalpy=HiExp*1000
    TiExp=PH(Ref$,Pressure,Enthalpy,'temperature',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    XiExp=PH(Ref$,Pressure,Enthalpy,'quality',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    IF (XiExp .LT. 0.0) XiExp = 0.0
    rhoiExp=PH(Ref$,Pressure,Enthalpy,'density',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF

    Quality=0
    rhofiExp=PQ(Ref$,Pressure,Quality,'density',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    mufiExp=PQ(Ref$,Pressure,Quality,'viscosity',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF

    Quality=1
    rhogiExp=PQ(Ref$,Pressure,Quality,'density',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    mugiExp=PQ(Ref$,Pressure,Quality,'viscosity',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF

    Pressure=PiExp*1000
    Quality=0
    TsiExp=PQ(Ref$,Pressure,Quality,'temperature',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF

    Temperature=TiExp
    Quality=0
    Psat=TQ(Ref$,Temperature,Quality,'pressure',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    Psat=Psat/1000

    sigma=TQ(Ref$,Temperature,Quality,'surfacetension',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF

    HiEvp=HiExp

    Pressure=PiEvp*1000
    Enthalpy=HiEvp*1000
    TiEvp=PH(Ref$,Pressure,Enthalpy,'temperature',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    XiEvp=PH(Ref$,Pressure,Enthalpy,'quality',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    rhoiEvp=PH(Ref$,Pressure,Enthalpy,'density',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF

    IF (LdisTube .GT. 0) THEN
        LDISTUBE=LdisTube*12/UnitL !Distributor tube length, in
        CALL Distributor(Ref$,LDISTUBE,Nckts,mdotCmp,TiExp,HiExp,PoEvp, &
        HoEvpRtd,QdisTube,DPdisTot,ErrorFlag)

        IF (DPdisTot .LT. 0) THEN
            DPdisTot = 0
        END IF
        IF (DPdisTot .GT. PiEvp) THEN
            DPdisTot = 0
        END IF

        IDDISTUBE=1./4.-12.0E-3 !1/4 in OD, 12 mil-think
        VolDisTube=((IDDISTUBE/12**2)*PI/4*LDISTUBE/12*Nckts)*UnitL**3
        MassDisTube=rhoiEvp*VolDisTube

        PoExp=PiEvp+DPdisTot

    ELSE
        PoExp=PiEvp
        MassDisTube=0
    END IF

    Tcr=Tcrit(Ref$) 
    Pcr=Pcrit(Ref$)/1000

    rhoAvg=1/((1-XiExp)/rhofiExp+XiExp/rhogiExp)
    Subcooling=TsiExp-TiExp

    pi02=(Pcr-PiExp)/Pcr
    pi03=(Pcr-PoExp)/Pcr
    pi04=(Pcr-Psat)/Pcr
    pi05=Subcooling/Tcr
    pi06=LshTube/DshTube
    pi07=rhofiExp/rhogiExp
    pi08=(mufiExp-mugiExp)/mugiExp
    pi09=(sigma)/(DshTube*PiExp*1000)
    pi10=XiExp/(1-XiExp)
    pi11=rhoAvg/rhofiExp

    IF (XiExp .GT. 0) THEN
        pi01=0.0720*pi02**(-0.090)*pi03**(0.406)*pi06**(-0.149)* &
        pi08**(-0.099)*pi10**(-0.013)*pi11**(0.283)
    ELSE
        pi01=0.1378*pi02**(-0.950)*pi03**(0.033)*pi04**(0.769)* &
        pi05**(0.082)*pi06**(-0.099)*pi07**(-0.104)* &
        pi08**(0.554)*pi09**(-0.034)
    END IF
    mdotExp=pi01*DshTube**2*(rhofiExp*PiExp*1000)**0.5

    HoExp=HiExp

    Pressure=PoExp*1000
    Enthalpy=HoExp*1000
    ToExp=PH(Ref$,Pressure,Enthalpy,'temperature',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    XoExp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF

    Pressure=PiEvp*1000
    Enthalpy=HiEvp*1000
    TiEvp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    XiEvp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF

    !VL: Previously:
    !IF (mdotExp .LT. 0) THEN
    !    ErrorFlag=1
    !    GOTO 200
    !END IF
    !
    !OUT(1)=mdotExp
    !OUT(2)=PoExp
    !OUT(3)=ToExp
    !OUT(4)=XoExp
    !OUT(5)=MassDisTube
    !OUT(6)=QdisTube
    IF (mdotExp .LT. 0) THEN
        ErrorFlag=1
        !VL: Previously: GOTO 200
    ELSE

        OUT(1)=mdotExp
        OUT(2)=PoExp
        OUT(3)=ToExp
        OUT(4)=XoExp
        OUT(5)=MassDisTube
        OUT(6)=QdisTube
    END IF

    !VL: Previously: 200 CONTINUE
    OUT(7)=ErrorFlag

    PiExpDev = XIN(2)
    HiExpDev = XIN(3)
    PoExpDev = OUT(2)
    ToExpDev = OUT(3)
    hoExpDev= PQ(Ref$,PoExpDev,XoExp,'enthalpy',RefrigIndex,RefPropErr)

    RETURN

    END SUBROUTINE ShortTubeChoi

    !***********************************************************************************

    SUBROUTINE ShortTubePayne(Ref$,PureRef,XIN,PAR,OUT)

    !-----------------------------------------------------------------------------------
    !
    !  Description:
    !  To calculate mass flow rate through a short tube orifice using Buckingham Pi
    !  theorem
    !
    !  EnergyPlus REFPROP subroutines required
    !
    !  Inputs:
    !  Ref$=Refrigerant name
    !  PureRef=Refrigerant flag: 1=pure refrigerant
    !                            0=refrigerant mixture
    !  XIN(1)=Compressor mass flow rate, kg/s
    !  XIN(2)=Exp. device inlet pressure, kPa
    !  XIN(3)=Exp. device inlet enthalpy, kJ/kg
    !  XIN(4)=Evaporator inlet pressure, kPa
    !  XIN(5)=Evaporator outlet pressure, kPa
    !
    !  Parameters:
    !  PAR(1)=Short tube length, m
    !  PAR(2)=Short tube diameter, m
    !  PAR(3)=45 deg chamfer depth, m
    !  PAR(4)=Number of circuits in evaporator
    !  PAR(5)=Distributor tube length, m
    !
    !  Outputs:
    !  OUT(1)=Exp. device mass flow rate, kg/s
    !  OUT(2)=Exp. device outlet pressure, kPa
    !  OUT(3)=Exp. device outlet temperature, C
    !  OUT(4)=Exp. device outlet quality
    !  OUT(5)=Mass in distributor tubes, kg
    !  OUT(6)=Distributor tube capacity, kW
    !  OUT(7) = Error flag: 0-No error
    !                       1-Short tube solution error
    !                       2-Refprop error
    !
    !  Reference:
    !  Payne, V. and O'Neal, D.L. (2004). A mass flow rate correlation for refrigerants
    !  and refrigerant mixtures flowing through short tubes. HVAC&R, 10(1), pp. 73-87
    !
    !  Author:
    !  Ipseng Iu
    !  Mechanical and Aerospace Engineering
    !  Oklahoma State University, Stillwater	
    !
    !  Date: October 2006
    !
    !-----------------------------------------------------------------------------------

    USE FluidProperties_HPSim

    IMPLICIT NONE

    !Subroutine argument declarations
    CHARACTER*80,     INTENT(IN)  :: Ref$    !Refrigerant name
    INTEGER(2),       INTENT(IN)  :: PureRef !Refrigerant flag: 1-pure refrigerant
    !0-refrigerant mixture
    REAL, INTENT(IN)  :: XIN(5)
    REAL, INTENT(IN)  :: PAR(5)
    REAL, INTENT(OUT) :: OUT(7)

    !Subroutine local variables
    !CHARACTER (len=15) :: Property           
    INTEGER            :: RefrigIndex =0
    REAL Temperature,Quality,Pressure,Enthalpy

    REAL :: LshTube     !Short tube length, m
    REAL :: LdisTube    !Distributor tube length, m
    REAL :: DshTube     !Short tube diameter, m
    REAL :: IDDISTUBE   !Distributor inside diameter, in
    REAL :: VolDisTube  !Distributor tube volumn, m^3
    REAL :: MassDisTube !Mass in distributor tube, kg
    REAL :: Depth       !Short tube 45 deg chamfer depth, m
    REAL :: Subcooling  !Degree of subcooling, C
    REAL :: PiExp       !Inlet pressure of exp. device (Up stream pressure), kPa
    REAL :: HiExp       !Exp. device inlet enthalpy, kJ/kg
    REAL :: TiExp       !Exp. device inlet temperature, C
    REAL :: XiExp       !Exp. device inlet quality
    REAL :: PoExp       !Exp. device outlet pressure, kPa
    REAL :: HoExp       !Exp. device outlet enthalpy, kJ/kg
    REAL :: ToExp       !Exp. device outlet temperature, C
    REAL :: XoExp       !Exp. device outlet quality
    REAL :: DPdisTot    !Total pressure drop in distributor, kPa
    REAL :: PiEvp       !Evaporator inlet pressure, kPa
    REAL :: HiEvp       !Evaporator inlet enthalpy, kJ/kg
    REAL :: TiEvp       !Evaporator inlet temperature, C
    REAL :: XiEvp       !Evaporator inlet quality
    REAL :: PoEvp       !Evaporator outlet pressure, kPa
    REAL :: mdotCmp     !Mass flow rate from compressor, kg/s
    REAL :: mdotExp     !Mass flow rate from exp. device, kg/s
    REAL :: Gref        !Mass flux, kg/s-m2
    REAL :: TsiExp      !Liquid saturation temperature of the upstream fluid, C
    REAL :: QdisTube    !Distributor tube capacity, kW
    REAL :: rhoAvg      !Average density, kg/m^3
    REAL :: rhoiExp     !Upstream liquid density, kg/m^3
    REAL :: rhofiExp    !Upstream fluid density, kg/m^3
    REAL :: rhogiExp    !Upstream vapor density, kg/m^3
    REAL :: rhoiEvp     !Evaporator inlet density, kg/m^3
    REAL :: mufiExp     !Upstream fluid viscosity, Pa-s
    REAL :: mugiExp     !Upstream vapor viscosity, Pa-s
    REAL :: sigma       !Surface tension, N/m
    REAL :: Acs         !Cross-sectional area, m^2
    REAL :: SUBC        !Temperature ratio, T is in K
    REAL :: EVAP        !Pressure ratio, P is in absolute pressure
    REAL :: PRA         !Pressure ratio, P is in absolute pressure
    REAL :: DR          !Diameter ratio
    REAL :: LD          !Ratio of tube length to diameter
    REAL :: Pf          !Downstream pressure for unchoked orifice flow or correlated 
    !saturation pressure used in short tube model, kPa
    REAL :: Cc          !Correction factor for chamfered inlet short tubes
    REAL :: Ctp         !Correction factor for two-phase quality
    REAL :: Psat        !Saturation pressure, kPa
    REAL :: Pcr         !Critical pressure, kPa
    REAL :: Tcr         !Critical temperature, K
    REAL :: Y           !Parameter for two-phase quality correction factor calc.
    REAL :: Dref        !Reference diameter, mm
    REAL :: HoEvpRtd    !Evaporator outlet rated enthalpy, kJ/kg
    INTEGER(2)		 :: Nckts		!Number of circuits in evaporator
    INTEGER			 :: ErrorFlag	!0-No error
    !1-Solution error
    !2-Refprop error

    !Buckingham pi parameters
    REAL pi01,pi03,pi06,pi09,pi10
    REAL tp06,tp27,tp28,tp32,tp34,tp35

    REAL :: a(7)  !Emperical constants
    REAL :: b(9) !Emperical constants
    REAL, PARAMETER :: Const=5.08e-005 !Emperical constants

    !NIST Refrigerant property variables and functions
    INTEGER(2) RefPropOpt			!Ref prop calc. option
    INTEGER(2) RefPropErr			!Error flag:1-error; 0-no error
    REAL RefProp(28)

    !Flow:

    mdotCmp=XIN(1)
    PiExp=XIN(2)
    HiExp=XIN(3)
    PiEvp=XIN(4)
    PoEvp=XIN(5)

    LshTube=PAR(1)
    DshTube=PAR(2)
    Depth=PAR(3)
    Nckts=PAR(4)
    LdisTube=PAR(5)

    ErrorFlag=0 !Initialize

    Pressure=PiExp*1000
    Enthalpy=HiExp*1000
    TiExp=PH(Ref$,Pressure,Enthalpy,'temperature',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    XiExp=PH(Ref$,Pressure,Enthalpy,'quality',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    IF (XiExp .LT. 0.0) THEN
        XiExp = 0.0
    END IF
    rhoiExp=PH(Ref$,Pressure,Enthalpy,'density',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF

    Quality=0
    rhofiExp=PQ(Ref$,Pressure,Quality,'density',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    mufiExp=PQ(Ref$,Pressure,Quality,'viscosity',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF

    Quality=1
    rhogiExp=PQ(Ref$,Pressure,Quality,'density',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    mugiExp=PQ(Ref$,Pressure,Quality,'viscosity',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF

    Pressure=PiExp*1000
    Quality=0
    TsiExp=PQ(Ref$,Pressure,Quality,'temperature',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF

    Temperature=TiExp
    Quality=0
    Psat=TQ(Ref$,Temperature,Quality,'pressure',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    Psat=Psat/1000

    sigma=TQ(Ref$,Temperature,Quality,'surfacetension',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF

    HiEvp=HiExp

    Pressure=PiEvp*1000
    Enthalpy=HiEvp*1000
    TiEvp=PH(Ref$,Pressure,Enthalpy,'temperature',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    XiEvp=PH(Ref$,Pressure,Enthalpy,'quality',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    rhoiEvp=PH(Ref$,Pressure,Enthalpy,'density',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF

    IF (LdisTube .GT. 0) THEN
        LDISTUBE=LdisTube*12/UnitL !Distributor tube length, in
        CALL Distributor(Ref$,LDISTUBE,Nckts,mdotCmp,TiExp,HiExp,PoEvp, &
        HoEvpRtd,QdisTube,DPdisTot,ErrorFlag)

        IF (DPdisTot .LT. 0) THEN
            DPdisTot = 0
        END IF
        IF (DPdisTot .GT. PiEvp) THEN
            DPdisTot = 0
        END IF

        IDDISTUBE=1./4.-12.0E-3 !1/4 in OD, 12 mil-think
        VolDisTube=((IDDISTUBE/12**2)*PI/4*LDISTUBE/12*Nckts)*UnitL**3
        MassDisTube=rhoiEvp*VolDisTube

        PoExp=PiEvp+DPdisTot

    ELSE
        PoExp=PiEvp
        MassDisTube=0
    END IF

    Tcr=Tcrit(Ref$)+273.15
    Pcr=Pcrit(Ref$)/1000

    rhoAvg=1/((1-XiExp)/rhofiExp+XiExp/rhogiExp)
    Subcooling=TsiExp-TiExp

    a(1)=3.8811e-1;  a(2)=1.1427e1;  a(3)=-1.4194e1;  a(4)=1.0703
    a(5)=-9.1928e-2; a(6)=2.1425e1;  a(7)=-5.8195e2

    pi03=(PiExp-Psat)/Pcr
    pi06=rhogiExp/rhofiExp
    pi09=Subcooling/Tcr
    pi10=LshTube/DshTube

    pi01=(a(1)+a(2)*pi03+a(3)*pi09+a(4)*pi06+a(5)*LOG(pi10)) / &
    (1+a(6)*pi03+a(7)*pi09**2) 

    IF (XiExp .GT. 0) THEN !Two-phase inlet correction

        b(1)=1.1831;	 b(2)=-1.4680;	b(3)=-1.5285e-1; b(4)=-1.4639e1; b(5)=9.8401
        b(6)=-1.9798e-2; b(7)=-1.5348;	b(8)=-2.0533;	 b(9)=-1.7195e1

        tp06=rhoAvg/rhofiExp
        tp27=LshTube/DshTube
        tp28=PiExp/Pcr
        tp32=(Pcr-PiExp)/Pcr
        tp34=XiExp/(1-XiExp)*(rhofiExp/rhogiExp)**0.5
        tp35=(Pcr-Psat)/Pcr

        Ctp=(b(1)*tp06+b(2)*tp06**2+b(3)*(LOG(tp06))**2+b(4)*(LOG(tp35))**2+ &
        b(5)*(LOG(tp32))**2+b(6)*(LOG(tp27))**2) / &
        (1+b(7)*tp06+b(8)*tp34+b(9)*tp28**3)

        pi01=Ctp*pi01
    END IF

    Gref=pi01*SQRT(rhofiExp*Pcr*1000)
    mdotExp=Gref*(PI*DshTube**2/4)

    HoExp=HiExp

    Pressure=PoExp*1000
    Enthalpy=HoExp*1000
    ToExp=PH(Ref$,Pressure,Enthalpy,'temperature',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    XoExp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF

    Pressure=PiEvp*1000
    Enthalpy=HiEvp*1000
    TiEvp=PH(Ref$, Pressure, Enthalpy, 'temperature', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF
    XiEvp=PH(Ref$, Pressure, Enthalpy, 'quality', RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- ShortTube: Refprop error.'
        RETURN
    END IF

    !VL: Previously: 
    !IF (mdotExp .LT. 0) THEN
    !    ErrorFlag=1
    !    GOTO 200
    !END IF
    !
    !OUT(1)=mdotExp
    !OUT(2)=PoExp
    !OUT(3)=ToExp
    !OUT(4)=XoExp
    !OUT(5)=MassDisTube
    !OUT(6)=QdisTube

    IF (mdotExp .LT. 0) THEN
        ErrorFlag=1
        !VL: Previously:GOTO 200
    ELSE

        OUT(1)=mdotExp
        OUT(2)=PoExp
        OUT(3)=ToExp
        OUT(4)=XoExp
        OUT(5)=MassDisTube
        OUT(6)=QdisTube    

    END IF

    !VL: Previously:200 CONTINUE
    OUT(7)=ErrorFlag

    PiExpDev = XIN(2)
    HiExpDev = XIN(3)
    PoExpDev = OUT(2)
    ToExpDev = OUT(3)
    hoExpDev= PQ(Ref$,PoExpDev,XoExp,'enthalpy',RefrigIndex,RefPropErr)

    RETURN

    END SUBROUTINE ShortTubePayne

    !***********************************************************************************

    END MODULE ShortTubeMod