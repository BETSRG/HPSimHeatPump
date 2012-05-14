

!PUBLIC Refrig1
!PUBLIC Refrigtest

!FUNCTION Refrig()
!CHARACTER(len=15)   :: Refrig
!Refrig='Propane' 
!RETURN
!END FUNCTION Refrig


!SUBROUTINE Refrig(Refrigerant)
!CHARACTER(len=32), INTENT(OUT)  :: Refrigerant ! carries in substance name
!Refrigerant='R22' 
!RETURN
!END SUBROUTINE Refrig


SUBROUTINE Refrigtest

USE FluidProperties
USE RefNameMod

COMMON T(20),Q(20),H(20),P(20),D(20),CP(20),S(20),C(20)
COMMON TSH(20),PSH(20),HSH(20),DSH(20),CPSH(20),SSH(20),CSH(20),DVSH(20)
COMMON TSC(20),PSC(20),HSC(20),DSC(20),CPSC(20),SSC(20),CSC(20),DVSC(20)
COMMON DV(20)
COMMON  T1(20),Q1(20),P1(20),H1(20),S1(20),TSH1(20),PSH1(20),HSH1(20),SSH1(20),TSC1(20),PSC1(20),HSC1(20),SSC1(20)
 
!CHARACTER(len=15) ::  Refrigerant ! carries in substance name
CHARACTER(len=80) ::  Refrigerant ! carries in substance name
CHARACTER (len=15) :: Property           

INTEGER            :: RefrigIndex =0
REAL :: T1       !Saturated Temperature (Celcius)
REAL :: Q1       !Saturated Ratio=0.1,0.2,....
REAL :: P1       !Saturated Pressure (Pa)
REAL :: S1       !Saturated Entropy
REAL :: TSH1     !Superheat Temperature (Celcius)
REAL :: PSH1     !Superheat Pressure (Pa)
REAL :: HSH1     !Superheat Density (Kg/m^3)
REAL :: SSH1     !Superheat Entropy
REAL :: TSC1     !Subcool Temperature (Celcius)
REAL :: PSC1     !Subcool Pressure (Pa)
REAL :: HSC1     !Subcool Density (Kg/m^3)
REAL :: SSC1     !Subcool Entropy


REAL :: Pcritical
REAL :: Tcritical
REAL :: MoleW
REAL :: T       !Temperature (Celcius)
REAL :: Q       !Ratio=0.1,0.2,....
REAL :: S       !Entropy (J/Kg)
REAL :: DV      !Dynamic Viscosity (Pa-s)
REAL :: C       !Conductivity(W/mK)
REAl :: H       !Enthalpy (J/Kg)
REAL :: P       !Pressure (Pa)
REAL :: D       !Density (Kg/m^3)
REAL :: CP      !Specific Heat (J/Kg)
REAL :: TSH     !Superheat Temperature (Celcius)
REAL :: PSH     !Superheat Pressure (Pa)
REAL :: DSH     !Superheat Density (Kg/m^3)
REAL :: DVSH    !Superheat Dynamic Viscosity (Ns/m^2)
REAL :: CSH     !Superheat Conductivity (W/m-K)
REAL :: TSC     !Subcool Temperature (Celcius)
REAL :: PSC     !Subcool Pressure (Pa)
REAL :: DSC     !Subcool Density (Kg/m^3)
REAL :: DVSC    !Subcool Dynamic Viscosity (Ns/m^2)
REAL :: CSC     !Subcool Conductivity (W/m-K)

!OPEN(8,FILE='R22.TXT',STATUS='UNKNOWN')
!OPEN(6,FILE='OUTPUT.TXT',STATUS='UNKNOWN')
!OPEN(8,FILE='R22.TXT',STATUS='UNKNOWN')

!DO 50 K = 1,20
!   READ (8,*) T1(K),Q1(K),P1(K),H1(K),S1(K),TSH1(K),PSH1(K),HSH1(K),SSH1(K),TSC1(K),PSC1(K),HSC1(K),SSC1(K)
!50 CONTINUE
CALL Refrig(Refrigerant)
Refrigerant=TRIM(Refrigerant)

!DO 100 K=1,20
!Pcritical=Pcrit(Refrigerant)
!Tcritical=Tcrit(Refrigerant)
!MoleW=MW(Refrigerant)

!PQ()
!H(K)=PQ(Refrigerant,P1(K),Q1(K),'enthalpy',RefrigIndex)
!T(K)=PQ(Refrigerant,P1(K),Q1(K),'temperature',RefrigIndex)
!DV(K)=PQ(Refrigerant,P1(K),Q1(K),'viscosity',RefrigIndex)
!C(K)=PQ(Refrigerant,P1(K),Q1(K),'conductivity',RefrigIndex)
!CP(K)=PQ(Refrigerant,P1(K),Q1(K),'specificheat',RefrigIndex)
!S(K)=PQ(Refrigerant,P1(K),Q1(K),'entropy',RefrigIndex)

!TQ() 
!H(K)=TQ(Refrigerant,T1(K),Q1(K),'enthalpy',RefrigIndex)
!P(K)=TQ(Refrigerant,T1(K),Q1(K),'pressure',RefrigIndex)
!S(K)=TQ(Refrigerant,T1(K),Q1(K),'entropy',RefrigIndex)

!TP() 
!HSH(K)=TP(Refrigerant,TSH1(K),PSH1(K),'enthalpy',RefrigIndex)
!DSH(K)=TP(Refrigerant,TSH1(K),PSH1(K),'density',RefrigIndex)
!SSH(K)=TP(Refrigerant,TSH1(K),PSH1(K),'entropy',RefrigIndex)
!HSC(K)=TP(Refrigerant,TSC1(K),PSC1(K),'enthalpy',RefrigIndex)
!DSC(K)=TP(Refrigerant,TSC1(K),PSC1(K),'density',RefrigIndex)
!SSC(K)=TP(Refrigerant,TSC1(K),PSC1(K),'entropy',RefrigIndex)

!PH() 
!Saturated
!Q(K)=PH(Refrigerant,P1(K),H1(K),'quality',RefrigIndex)
!D(K)=PH(Refrigerant,P1(K),H1(K),'density',RefrigIndex)
!S(K)=PH(Refrigerant,P1(K),H1(K),'entropy',RefrigIndex)
!DV(K)=PH(Refrigerant,P1(K),H1(K),'viscosity',RefrigIndex)
!C(K)=PH(Refrigerant,P1(K),H1(K),'conductivity',RefrigIndex)
!CP(K)=PH(Refrigerant,P1(K),H1(K),'specificheat',RefrigIndex)
!T(K)=PH(Refrigerant,P1(K),H1(K),'temperature',RefrigIndex)
!Superheat
!HSH(K)=PH(Refrigerant,PSH1(K),HSH1(K),'enthalpy',RefrigIndex)
!DSH(K)=PH(Refrigerant,PSH1(K),HSH1(K),'density',RefrigIndex)
!SSH(K)=PH(Refrigerant,PSH1(K),HSH1(K),'entropy',RefrigIndex)
!DVSH(K)=PH(Refrigerant,PSH1(K),HSH1(K),'viscosity',RefrigIndex)
!CSH(K)=PH(Refrigerant,PSH1(K),HSH1(K),'conductivity',RefrigIndex)
!CPSH(K)=PH(Refrigerant,PSH1(K),HSH1(K),'specificheat',RefrigIndex)
!TSH(K)=PH(Refrigerant,PSH1(K),HSH1(K),'temperature',RefrigIndex)
!Subcool
!HSC(K)=PH(Refrigerant,PSC1(K),HSC1(K),'enthalpy',RefrigIndex)
!DSC(K)=PH(Refrigerant,PSC1(K),HSC1(K),'density',RefrigIndex)
!SSC(K)=PH(Refrigerant,PSC1(K),HSC1(K),'entropy',RefrigIndex)
!DVSC(K)=PH(Refrigerant,PSC1(K),HSC1(K),'viscosity',RefrigIndex)
!CSC(K)=PH(Refrigerant,PSC1(K),HSC1(K),'conductivity',RefrigIndex)
!CPSC(K)=PH(Refrigerant,PSC1(K),HSC1(K),'specificheat',RefrigIndex)
!TSC(K)=PH(Refrigerant,PSC1(K),HSC1(K),'temperature',RefrigIndex)

!PS() 
!H(K)=PS(Refrigerant,P1(K),S1(K),'enthalpy',RefrigIndex)
!HSC(K)=PS(Refrigerant,PSC1(K),SSC1(K),'enthalpy',RefrigIndex)
!HSH(K)=PS(Refrigerant,PSH1(K),SSH1(K),'enthalpy',RefrigIndex)

100 CONTINUE

!DO 150 K=1,20
!PQ()
!WRITE(6,008) T1(K),Q1(K),P1(K),T(K),H(K),S(K),DV(K),C(K),CP(K)
!TQ()
!WRITE(6,008) T1(K),Q1(K),P1(K),P(K),H(K),S(K),Pcritical,Tcritical,MoleW
!TP()
!WRITE(6,008) TSH1(K),PSH1(K),HSH(K),DSH(K),SSH(K),TSC1(K),PSC1(K),HSC(K),DSC(K),SSC(K)

!PH()
!Saturated
!WRITE(6,008) T1(K),P1(K),H1(K),D(K),S(K),DV(K),C(K),CP(K),T(K),Q(K)
!Superheated
!WRITE(6,008) TSH1(K),PSH1(K),HSH(K),DSH(K),SSH(K),DVSH(K),CSH(K),CPSH(K),TSH(K)
!Subcooled
!WRITE(6,008) TSC1(K),PSC1(K),HSC(K),DSC(K),SSC(K),DVSC(K),CSC(K),CPSC(K),TSC(K)

!PS()
!WRITE(6,008) H(K),HSH(K),HSC(K)

008   FORMAT(10E18.8)
150 CONTINUE
END SUBROUTINE Refrigtest



