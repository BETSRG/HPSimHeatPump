    MODULE CompressorMod

    USE DataSimulation

    PUBLIC  Compressor
    PRIVATE X

    CONTAINS

    SUBROUTINE Compressor(Ref$,PureRef,XIN,PAR,OUT)

    ! ----------------------------------------------------------------------
    !
    !   Description: ARI Compressor model
    !
    !   Method: 2nd order polynomial equation fit 
    !
    !   Inputs:
    !       Ref$=Refrigerant name
    !       PureRef=Refrigerant flag: 1=pure refrigerant
    !                                 0=refrigerant mixture
    !       XIN(1) = Suction refrigerant pressure, kPa
    !       XIN(2) = Discharge refrigerant pressure, kPa
    !       XIN(3) = Suction enthalpy, kJ/kg
    !
    !       PAR(1..10)  = Coefficients for power calc.
    !       PAR(11..20) = Coefficients for mass flow rate calc.
    !       PAR(21) = Shell heat loss fraction of power consumption
    !       PAR(22) = Shell heat loss W
    !       PAR(23) = Internal void volume, m^3
    !       PAR(24) = Power Correction, 1 - 230 VAC; 2 - 208 VAC
    !       PAR(25) = Power multiplier
    !       PAR(26) = Mass flow rate multiplier
    !
    !   Outputs:
    !       OUT(1) = Power, KW
    !       OUT(2) = Mass flow rate, kg/s
    !       OUT(3) = Discharge enthalpy, kJ/kg
    !       OUT(4) = Discharge quality
    !       OUT(5) = Discharge temperature, C
    !       OUT(6) = Mass in compressor, kg
    !       OUT(7) = Error flag: 0-No error
    !                            1-Compressor solution error
    !                            2-Refprop error
    !
    !   Author:
    !   Ipseng Iu
    !   Mechanical and Aerospace Engineering
    !   Oklahoma State University, Stillwater
    !
    !   Date: July 2003
    !
    ! ----------------------------------------------------------------------

    USE FluidProperties
    !USE FluidProperties_HPSim !RS Comment: Currently needs to be used for integration with Energy+ Code (6/28/12) 

    IMPLICIT NONE

    !Subroutine argument declarations
    CHARACTER*80,     INTENT(IN) :: Ref$    !Refrigerant name
    INTEGER(2),       INTENT(IN) :: PureRef !Refrigerant flag: 1-pure refrigerant
    !0-refrigerant mixture

    REAL, INTENT(IN) :: XIN(3)
    REAL, INTENT(IN) :: PAR(26)
    REAL, INTENT(OUT) :: OUT(7)

    REAL, PARAMETER :: Fv=0.75

    !Subroutine local variables
    !CHARACTER (len=15) :: Property           
    INTEGER            :: RefrigIndex =0
    REAL Temperature,Quality,Pressure,Enthalpy,Entropy

    REAL A(10),B(10)
    REAL TDPsuc      !Suction dew point temperature, C
    REAL TDPdis      !Discharge dew point temperature, C
    REAL TDPsucF     !Suction dew point temperature, F
    REAL TDPdisF     !Discharge dew point temperature, F
    REAL Power       !Compressor power consumption, KW
    REAL PowerMap    !Map based Compressor power consumption, KW
    REAL mdot        !Refrigerant mass flow rate, kg/s
    REAL mdotMap     !Map based Refrigerant mass flow rate, kg/s
    REAL tSH         !Superheat, C
    REAL Tsuc        !Suction temp, C
    REAL TsucMap     !Map based suction temp, C
    REAL Tdis        !Discharge temperature, C
    REAL Psuc        !Suction pressure, kPa
    REAL PsucMap     !Map based suction pressure, kPa
    REAL Pdis        !Discharge pressure, kPa
    REAL Xdis        !Discharge quality
    REAL Hsuc        !Suction enthalpy, kJ/kg
    REAL Hdis        !Discharge enthalpy, kJ/kg
    REAL Qshellfrac  !Compressor shell heat loss fraction of power consumption
    REAL Qshell      !Compressor shell heat loss, kW
    REAL VolCmp      !Compressor internal volume, m3
    REAL MassCmp     !Refrigerant mass in compressor, kg
    REAL rhoDis      !Discharge density, kg/m3
    REAL rhoSuc      !Suction density, kg/m3
    REAL rhoMap      !Map based density value, kg/m3
    REAL Ssuc        !Suction entropy
    REAL SsucMap     !Map based entropy value
    REAL HdisIsen    !Isentropic discharge enthalpy, kJ/kg
    REAL HdisIsenMap !Map based insentropic discharge enthalpy, kJ/kg
    REAL HdisMap     !Map based discharge enthalpy, kJ/kg
    REAL HsucMap     !Map based suction enthalpy, kJ/kg
    REAL Wcorrect    !Correction factor for power calc. with different input voltage
    REAL Mcorrect    !Correction factor for mass flow rate
    REAL PwrMultiplier    !Power multiplier
    REAL mdotMultiplier   !Mass flow rate multiplier
    !REAL X           !Polynomial function
    INTEGER I !Loop control
    INTEGER ErrorFlag          !0-No error
    !1-Compressor solution error
    !2-Refprop error

    !NIST Refrigerant property variables and functions
    INTEGER(2) RefPropOpt  !Ref prop calc. option
    INTEGER(2) RefPropErr  !Error flag:1-error; 0-no error
    REAL RefProp(28)

    !Flow:

    Psuc = XIN(1)
    Pdis = XIN(2)
    Hsuc = XIN(3)

    DO I=1,10
        A(I)=PAR(I)
        B(I)=PAR(I+10) 
    END DO
    Qshellfrac = PAR(21)
    Qshell = PAR(22)
    VolCmp = PAR(23)
    Wcorrect = PAR(24)
    PwrMultiplier=PAR(25)
    mdotMultiplier=PAR(26)

    Wcorrect = 1 !1.21 !1.25

    ErrorFlag=0 !Initialize

    Pressure=Psuc*1000
    Quality=1
    TDPsuc=PQ(Ref$,Pressure,Quality,'temperature',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- Compressor: Refprop error.'
        ErrorFlag=2
        !VL: Previously: GOTO 200
        OUT(7)=ErrorFlag
        RETURN
    END IF

    Pressure=Pdis*1000
    Quality=1
    TDPdis=PQ(Ref$,Pressure,Quality,'temperature',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- Compressor: Refprop error.'
        ErrorFlag=2
        !VL: Previously: GOTO 200
        OUT(7)=ErrorFlag
        RETURN
    END IF

    TDPsucF=TDPsuc*1.8+32
    TDPdisF=TDPdis*1.8+32

    PowerMap=X(A,TDPdisF,TDPsucF)/1000
    mdotMap=X(B,TDPdisF,TDPsucF)*0.454/3600

    TsucMap=((TDPsucF+20)-32)*5/9 !20-rated superheat

    Temperature=TsucMap
    Pressure=Psuc*1000
    HsucMap=TP(Ref$,Temperature,Pressure,'enthalpy',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- Compressor: Refprop error.'
        ErrorFlag=2
        !VL: Previously: GOTO 200
        OUT(7)=ErrorFlag
        RETURN
    END IF
    HsucMap=HsucMap/1000
    rhoMap=TP(Ref$,Temperature,Pressure,'density',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- Compressor: Refprop error.'
        ErrorFlag=2
        !VL: Previously: GOTO 200
        OUT(7)=ErrorFlag
        RETURN
    END IF
    SsucMap=TP(Ref$,Temperature,Pressure,'entropy',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- Compressor: Refprop error.'
        ErrorFlag=2
        !VL: Previously: GOTO 200
        OUT(7)=ErrorFlag
        RETURN
    END IF
    SsucMap=SsucMap/1000

    Pressure=Pdis*1000
    Entropy=SsucMap*1000
    HdisIsenMap=PS(Ref$,Pressure,Entropy,'enthalpy',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- Compressor: Refprop error.'
        ErrorFlag=2
        !VL: Previously: GOTO 200
        OUT(7)=ErrorFlag
        RETURN
    END IF
    HdisIsenMap=HdisIsenMap/1000

    Pressure=Psuc*1000
    Enthalpy=Hsuc*1000
    Tsuc=PH(Ref$,Pressure,Enthalpy,'temperature',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- Compressor: Refprop error.'
        ErrorFlag=2
        !VL: Previously: GOTO 200
        OUT(7)=ErrorFlag
        RETURN
    END IF
    rhosuc=PH(Ref$,Pressure,Enthalpy,'density',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- Compressor: Refprop error.'
        ErrorFlag=2
        !VL: Previously: GOTO 200
        OUT(7)=ErrorFlag
        RETURN
    END IF
    Ssuc=PH(Ref$,Pressure,Enthalpy,'entropy',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- Compressor: Refprop error.'
        ErrorFlag=2
        !VL: Previously: GOTO 200
        OUT(7)=ErrorFlag
        RETURN
    END IF
    Ssuc=Ssuc/1000

    Pressure=Pdis*1000
    Entropy=Ssuc*1000
    HdisIsen=PS(Ref$,Pressure,Entropy,'enthalpy',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- Compressor: Refprop error.'
        ErrorFlag=2
        !VL: Previously: GOTO 200
        OUT(7)=ErrorFlag
        RETURN
    END IF
    HdisIsen=HdisIsen/1000

    !mdot=mdotMap*(1+Fv*(rhosuc/rhoMap-1))
    mdot=mdotMap*(rhosuc/rhoMap)
    !mdot=mdotMap

    Mcorrect=1 !0.95
    mdot=mdot*Mcorrect

    Power=PowerMap*(mdot/mdotMap)*(HdisIsen-Hsuc)/(HdisIsenMap-HsucMap)
    !Power=PowerMap

    Power=Power/Wcorrect

    mdot=mdot*mdotMultiplier
    Power=Power*PwrMultiplier

    IF (Qshellfrac .NE. 0) THEN
        Qshell = Qshellfrac * Power !Fraction of power input
    ELSE
        Qshell = Qshell * 1E-3  !Convert to kW
    END IF
    Hdis=(Power-Qshell)/mdot+Hsuc

    Pressure=Pdis*1000
    Enthalpy=Hdis*1000
    Tdis=PH(Ref$,Pressure,Enthalpy,'temperature',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0 .OR. (Tdis+273.15) .LT. 0) THEN !ISI - 06/06/07
        WRITE(*,*)'-- WARNING -- Compressor: Refprop error.'
        ErrorFlag=2
        !VL: Previously: GOTO 200
        OUT(7)=ErrorFlag
        RETURN
    END IF
    Xdis=PH(Ref$,Pressure,Enthalpy,'quality',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- Compressor: Refprop error.'
        ErrorFlag=2
        !VL: Previously: GOTO 200
        OUT(7)=ErrorFlag
        RETURN
    END IF
    rhoDis=PH(Ref$,Pressure,Enthalpy,'density',RefrigIndex,RefPropErr)
    IF (RefPropErr .GT. 0) THEN
        WRITE(*,*)'-- WARNING -- Compressor: Refprop error.'
        ErrorFlag=2
        !VL: Previously: GOTO 200
        OUT(7)=ErrorFlag
        RETURN
    END IF

    MassCmp=VolCmp*(rhoDis+rhoSuc)/2

    IF (Power .LT. 0 .OR. MassCmp .LT. 0) THEN
        ErrorFlag=1
        !VL: Previously: GOTO 200
        OUT(7)=ErrorFlag
        RETURN
    END IF

    OUT(1)=Power
    OUT(2)=mdot
    OUT(3)=Hdis
    OUT(4)=Xdis
    OUT(5)=Tdis
    OUT(6)=MassCmp

!VL: Previously: 200 CONTINUE
    OUT(7)=ErrorFlag

    RETURN

    END SUBROUTINE Compressor

    !***********************************************************************

    FUNCTION X(C,D,S)

    REAL C(10) !Coefficients
    REAL D !Discharge dew point temperature
    REAL S !Suction dew point temperature

    X=C(1)+C(2)*S+C(3)*D+C(4)*S**2+C(5)*(S*D)+C(6)*D**2+ &
    C(7)*S**3+C(8)*D*S**2+C(9)*S*D**2+C(10)*D**3

    END FUNCTION

    !***********************************************************************

    END MODULE CompressorMod