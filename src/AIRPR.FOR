	MODULE AirPropMod
C=======================================================================
C
C                  AIR PROPERITES
C
C=======================================================================
C
C  Ref:  Irvine, T.F.Jr., and Liley, P.E., "Steam and Gas Tables
C        with Computer Equations," Academic Press, Inc., 1984.
C
C***********************************************************************

	PUBLIC CPCVA
	PUBLIC CPA
	PUBLIC HA
	PUBLIC PHIA
	PUBLIC TPHIA
	PUBLIC VISCA
	PUBLIC AKA
	PUBLIC TS

	CONTAINS

        SUBROUTINE CPCVA(TCA,CPA,CVA,GAMMA,SONIC)

C ----------------------------------------------------------------------
C
C   This subroutine takes Celsius air temperature, TCA, and computes:
C     CPA: Specific heat of air at constant pressure [KJ/(kg K)]
C     CVA: Specific heat of air at constant volume [KJ/(kg K)]
C     GAMMA: The ratio CPA/CVA [dimensionless]
C     SONIC: Speed of sound in air (M/S)
C
C***********************************************************************

        DATA A0,A1,A2/1.03409,-0.284887E-3,0.7816818E-6/
        DATA A3,A4,TCONV,R/-0.4970786E-9,0.1077024E-12,273.15,0.287040/
        T=TCA+TCONV
        CPA=A0+T*(A1+T*(A2+T*(A3+T*A4)))
        CVA=CPA-R
        GAMMA=CPA/CVA
        SONIC=SQRT(GAMMA*R*T)
        RETURN
        END SUBROUTINE

C***********************************************************************

        FUNCTION CPA(TC)

C ----------------------------------------------------------------------
C
C   This subroutine takes Celsius air temperature, TC, and computes:
C     CP: Specific heat of air at constant pressure [KJ/(kg K)]
C
C***********************************************************************

        DATA A0,A1,A2/1.03409,-0.284887E-3,0.7816818E-6/
        DATA A3,A4,TCONV,R/-0.4970786E-9,0.1077024E-12,273.15,0.287040/
        T=TC+TCONV
        CPA=A0+T*(A1+T*(A2+T*(A3+T*A4)))
        RETURN
        END FUNCTION

C***********************************************************************

        FUNCTION HA(TC)

C ----------------------------------------------------------------------
C
C   Enthalpy of air [KJ/kg] as a function of temperature [C]
C
C***********************************************************************

        DATA A0,A1,A2,A3/12.0740,0.924502,0.115984E-3,-0.563568E-8/
        DATA TCONV/273.15/
        T=TC+TCONV
        HA=A0+T*(A1+T*(A2+T*A3))

C   Note that internal energy, U, is equal to (HA - R*T)
C   where R = 0.287040

        RETURN
        END FUNCTION
C***********************************************************************

        FUNCTION PHIA(TCA)

C ----------------------------------------------------------------------
C
C  Entropy function, PHI(T) = [ CPA(T)/T dT ] integrated from T0 to T
C      where T0 is a reference temperature at which entropy = 0.
C      PHIA has same units as entropy [KJ/(kg K)]
C  Note that S2 - S1 = PHIA(TC2) - PHIA(TC1) - R*LOG(P2/P1)
C      where R = 0.287040 [KJ/(kg K)]
C  Other useful relationships:
C      Isentropic pressure function, LOG(P/P0) = PHIA(TCA) / R
C      Isentropic volume function, LOG(V/V0) = LOG(R*T) - LOG(P/P0)
C
C***********************************************************************

        DATA A0,A1,A2,TCONV/1.386989,0.184930E-3,0.95,273.15/
        T=TCA+TCONV
        PHIA=A0+A1*T+A2*ALOG(T)
        RETURN
        END FUNCTION
C***********************************************************************

        FUNCTION TPHIA(PHIA)

C ----------------------------------------------------------------------
C
C  Temperature [C] of air as a function of the entropy function, PHIA
C
C***********************************************************************

        DATA A0,A1,A2,A3/-8800.92,1269.74,-61.9391,1.03530/
        DATA R,TCONV/0.287040,273.15/
        PR=PHIA/R
        TPHIA=A0+PR*(A1+PR*(A2+PR*A3))-TCONV
        RETURN
        END FUNCTION
C***********************************************************************

        FUNCTION VISCA(TC)

C ----------------------------------------------------------------------
C
C   Dynamic viscosity [(N S)/(M*M)] of air, from celsius temperature
C
C***********************************************************************

        DATA A0,A1,A2/-0.98601,9.080125E-2,-1.17635575E-4/
        DATA A3,A4/1.2349703E-7,-5.7971299E-11/
        DATA B0,B1,B2/4.8856745,5.43232E-2,-2.4261775E-5/
        DATA B3,B4,TCONV/7.9306E-9,-1.10398E-12,273.15/
        T=TC+TCONV
        VISCA=A0+T*(A1+T*(A2+T*(A3+T*A4)))
        IF(T .GE. 600.) VISCA=B0+T*(B1+T*(B2+T*(B3+T*B4)))
        VISCA=VISCA*1.E-6
        RETURN
        END FUNCTION
C***********************************************************************

        FUNCTION AKA(TC)

C ----------------------------------------------------------------------
C
C  Thermal conductivity of air [KW/(M K)], given Celsius temperature
C
C***********************************************************************

        DATA C0,C1,C2/-2.276501E-3,1.2598485E-4,-1.4815235E-7/
        DATA C3,C4,C5/1.73550646E-10,-1.066657E-13,2.47663035E-17/
        DATA TCONV/273.15/
        T=TC+TCONV
        AKA=0.001*(C0+T*(C1+T*(C2+T*(C3+T*(C4+T*C5)))))
        RETURN
        END FUNCTION
C***********************************************************************	

        FUNCTION TS(HS)

C ----------------------------------------------------------------------
C
C  Saturation temperature [C], given saturation enthalpy [kJ/kg]
C
C***********************************************************************

        DATA C0,C1/1.050415E-05,-3.801049E-03/
	  DATA C2,C3/6.297500E-01,-5.549214/

	  TS=C0*HS**3+C1*HS**2+C2*HS+C3

        RETURN
        END FUNCTION
C***********************************************************************	
	
	END MODULE AirPropMod