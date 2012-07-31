	MODULE AirPropMod
!=======================================================================
!
!                  AIR PROPERITES
!
!=======================================================================
!
!  Ref:  Irvine, T.F.Jr., and Liley, P.E., "Steam and Gas Tables
!        with Computer Equations," Academic Press, Inc., 1984.
!
!***********************************************************************

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

! ----------------------------------------------------------------------
!
!   This subroutine takes Celsius air temperature, TCA, and computes:
!     CPA: Specific heat of air at constant pressure [KJ/(kg K)]
!     CVA: Specific heat of air at constant volume [KJ/(kg K)]
!     GAMMA: The ratio CPA/CVA [dimensionless]
!     SONIC: Speed of sound in air (M/S)
!
!***********************************************************************

        DATA A0,A1,A2/1.03409,-0.284887E-3,0.7816818E-6/
        DATA A3,A4,TCONV,R/-0.4970786E-9,0.1077024E-12,273.15,0.287040/
        T=TCA+TCONV
        CPA=A0+T*(A1+T*(A2+T*(A3+T*A4)))
        CVA=CPA-R
        GAMMA=CPA/CVA
        SONIC=SQRT(GAMMA*R*T)
        RETURN
        END SUBROUTINE

!***********************************************************************

        FUNCTION CPA(TC)

! ----------------------------------------------------------------------
!
!   This subroutine takes Celsius air temperature, TC, and computes:
!     CP: Specific heat of air at constant pressure [KJ/(kg K)]
!
!***********************************************************************

        DATA A0,A1,A2/1.03409,-0.284887E-3,0.7816818E-6/
        DATA A3,A4,TCONV,R/-0.4970786E-9,0.1077024E-12,273.15,0.287040/
        T=TC+TCONV
        CPA=A0+T*(A1+T*(A2+T*(A3+T*A4)))
        RETURN
        END FUNCTION

!***********************************************************************

        FUNCTION HA(TC)

! ----------------------------------------------------------------------
!
!   Enthalpy of air [KJ/kg] as a function of temperature [C]
!
!***********************************************************************

        DATA A0,A1,A2,A3/12.0740,0.924502,0.115984E-3,-0.563568E-8/
        DATA TCONV/273.15/
        T=TC+TCONV
        HA=A0+T*(A1+T*(A2+T*A3))

!   Note that internal energy, U, is equal to (HA - R*T)
!   where R = 0.287040

        RETURN
        END FUNCTION
!***********************************************************************

        FUNCTION PHIA(TCA)

! ----------------------------------------------------------------------
!
!  Entropy function, PHI(T) = [ CPA(T)/T dT ] integrated from T0 to T
!      where T0 is a reference temperature at which entropy = 0.
!      PHIA has same units as entropy [KJ/(kg K)]
!  Note that S2 - S1 = PHIA(TC2) - PHIA(TC1) - R*LOG(P2/P1)
!      where R = 0.287040 [KJ/(kg K)]
!  Other useful relationships:
!      Isentropic pressure function, LOG(P/P0) = PHIA(TCA) / R
!      Isentropic volume function, LOG(V/V0) = LOG(R*T) - LOG(P/P0)
!
!***********************************************************************

        DATA A0,A1,A2,TCONV/1.386989,0.184930E-3,0.95,273.15/
        T=TCA+TCONV
        PHIA=A0+A1*T+A2*ALOG(T)
        RETURN
        END FUNCTION
!***********************************************************************

        FUNCTION TPHIA(PHIA)

! ----------------------------------------------------------------------
!
!  Temperature [C] of air as a function of the entropy function, PHIA
!
!***********************************************************************

        DATA A0,A1,A2,A3/-8800.92,1269.74,-61.9391,1.03530/
        DATA R,TCONV/0.287040,273.15/
        PR=PHIA/R
        TPHIA=A0+PR*(A1+PR*(A2+PR*A3))-TCONV
        RETURN
        END FUNCTION
!***********************************************************************

        FUNCTION VISCA(TC)

! ----------------------------------------------------------------------
!
!   Dynamic viscosity [(N S)/(M*M)] of air, from celsius temperature
!
!***********************************************************************

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
!***********************************************************************

        FUNCTION AKA(TC)

! ----------------------------------------------------------------------
!
! Thermal conductivity of air [KW/(M K)], given Celsius temperature
!
!***********************************************************************

        DATA C0,C1,C2/-2.276501E-3,1.2598485E-4,-1.4815235E-7/
        DATA C3,C4,C5/1.73550646E-10,-1.066657E-13,2.47663035E-17/
        DATA TCONV/273.15/
        T=TC+TCONV
        AKA=0.001*(C0+T*(C1+T*(C2+T*(C3+T*(C4+T*C5)))))
        RETURN
        END FUNCTION
!***********************************************************************	

        FUNCTION TS(HS)

! ----------------------------------------------------------------------
!
!  Saturation temperature [C], given saturation enthalpy [kJ/kg]
!
!***********************************************************************

        DATA C0,C1/1.050415E-05,-3.801049E-03/
	  DATA C2,C3/6.297500E-01,-5.549214/

	  TS=C0*HS**3+C1*HS**2+C2*HS+C3

        RETURN
        END FUNCTION
!***********************************************************************	
	
	END MODULE AirPropMod