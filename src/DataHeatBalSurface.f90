MODULE DataHeatBalSurface      ! EnergyPlus Data-Only Module

  ! MODULE INFORMATION:
  !       AUTHOR         Rick Strand
  !       DATE WRITTEN   December 2000
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! The purpose of this module is to contain data needed for the surface
  ! heat balances which are now "external" subroutines.

  ! METHODOLOGY EMPLOYED:
  ! NA

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
USE DataPrecisionGlobals

IMPLICIT NONE         ! Enforce explicit typing of all variables

PUBLIC ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS
  REAL, PARAMETER :: MinSurfaceTempLimit = -100. ! Lowest inside surface temperature allowed in Celsius
  REAL, PARAMETER :: MinSurfaceTempLimitBeforeFatal = -250. ! 2.5 times MinSurfaceTempLimit
  REAL, PARAMETER :: DefaultSurfaceTempLimit = 200.  ! Highest inside surface temperature allowed in Celsius

  ! DERIVED TYPE DEFINITIONS

  ! MODULE VARIABLE DECLARATIONS:

  ! SUBROUTINE SPECIFICATIONS FOR MODULE DataHeatBalSurface
!Integer Variables for the Heat Balance Simulation
INTEGER, ALLOCATABLE, DIMENSION(:) :: SUMH             !From Old Bldctf.inc

!Variables Dimensioned to Max Number of Heat Transfer Surfaces (maxhts)
REAL :: MaxSurfaceTempLimit = 200.  ! Highest inside surface temperature allowed in Celsius
REAL :: MaxSurfaceTempLimitBeforeFatal = 500.  ! 2.5 times MaxSurfaceTempLimit
REAL, ALLOCATABLE, DIMENSION(:) :: CTFConstInPart      !Constant Inside Portion of the CTF calculation
REAL, ALLOCATABLE, DIMENSION(:) :: CTFConstOutPart     !Constant Outside Portion of the CTF calculation
REAL, ALLOCATABLE, DIMENSION(:) :: TempSurfIn          !Temperature of the Inside Surface for each heat transfer surface
REAL, ALLOCATABLE, DIMENSION(:) :: TempSurfInTmp       !Inside Surface Temperature Of Each Heat Transfer Surface
REAL, ALLOCATABLE, DIMENSION(:) :: HcExtSurf           !Outside Convection Coefficient
REAL, ALLOCATABLE, DIMENSION(:) :: HAirExtSurf         !Outside Convection Coefficient
REAL, ALLOCATABLE, DIMENSION(:) :: HSkyExtSurf         !Outside Convection Coefficient
REAL, ALLOCATABLE, DIMENSION(:) :: HGrdExtSurf         !Outside Convection Coefficient
REAL, ALLOCATABLE, DIMENSION(:) :: TempSource          !Temperature at the source location for each heat transfer surface
REAL, ALLOCATABLE, DIMENSION(:) :: TempSurfInRep       !Temperature of the Inside Surface for each heat transfer surface
                                                            ! (report)
REAL, ALLOCATABLE, DIMENSION(:) :: QConvInReport       !Surface convection heat gain at inside face [J]
REAL, ALLOCATABLE, DIMENSION(:) :: QdotConvInRep       !Surface convection heat transfer rate at inside face surface [W]
                                                            ! (report)
REAL, ALLOCATABLE, DIMENSION(:) :: QdotConvInRepPerArea !Surface conv heat transfer rate per m2 at inside face surf
                                                            !  (report){w/m2]

!these next three all are for net IR thermal radiation exchange with other surfaces in the model. 
REAL, ALLOCATABLE, DIMENSION(:) :: QRadNetSurfInReport       !Surface thermal radiation heat gain at Inside face [J]
REAL, ALLOCATABLE, DIMENSION(:) :: QdotRadNetSurfInRep       !Surface thermal radiation heat transfer inside face surface [W]
REAL, ALLOCATABLE, DIMENSION(:) :: QdotRadNetSurfInRepPerArea ![W/m2]Surface thermal radiation heat transfer rate per m2 at 
                                                              !      Inside face surf
!these next three all are for solar radiation gains on inside face
REAL, ALLOCATABLE, DIMENSION(:) :: QRadSolarInReport       !Surface thermal radiation heat gain at Inside face [J]
REAL, ALLOCATABLE, DIMENSION(:) :: QdotRadSolarInRep       !Surface thermal radiation heat transfer inside face surface [W]
REAL, ALLOCATABLE, DIMENSION(:) :: QdotRadSolarInRepPerArea ![W/m2]Surface thermal radiation heat transfer rate per m2 at 
                                                              !      Inside face surf
!these next three all are for Lights visible radiation gains on inside face
REAL, ALLOCATABLE, DIMENSION(:) :: QRadLightsInReport       !Surface thermal radiation heat gain at Inside face [J]
REAL, ALLOCATABLE, DIMENSION(:) :: QdotRadLightsInRep       !Surface thermal radiation heat transfer inside face surface [W]
REAL, ALLOCATABLE, DIMENSION(:) :: QdotRadLightsInRepPerArea ![W/m2]Surface thermal radiation heat transfer rate per m2 at 
                                                              !      Inside face surf
!these next three all are for Internal Gains sources of radiation gains on inside face
REAL, ALLOCATABLE, DIMENSION(:) :: QRadIntGainsInReport       !Surface thermal radiation heat gain at Inside face [J]
REAL, ALLOCATABLE, DIMENSION(:) :: QdotRadIntGainsInRep       !Surface thermal radiation heat transfer inside face surface [W]
REAL, ALLOCATABLE, DIMENSION(:) :: QdotRadIntGainsInRepPerArea ![W/m2]Surface thermal radiation heat transfer rate per m2 at 
                                                              !      Inside face surf
!these next three all are for Radiative HVAC sources of radiation gains on inside face
REAL, ALLOCATABLE, DIMENSION(:) :: QRadHVACInReport       !Surface thermal radiation heat gain at Inside face [J]
REAL, ALLOCATABLE, DIMENSION(:) :: QdotRadHVACInRep       !Surface thermal radiation heat transfer inside face surface [W]
REAL, ALLOCATABLE, DIMENSION(:) :: QdotRadHVACInRepPerArea ![W/m2]Surface thermal radiation heat transfer rate per m2 at 
                                                              !      Inside face surf

REAL, ALLOCATABLE, DIMENSION(:) :: QConvOutReport       !Surface convection heat gain at Outside face [J]
REAL, ALLOCATABLE, DIMENSION(:) :: QdotConvOutRep       !Surface convection heat transfer rate at Outside face surface [W]
REAL, ALLOCATABLE, DIMENSION(:) :: QdotConvOutRepPerArea !Surface conv heat transfer rate per m2 at Outside face surf
                                                            !  (report){w/m2]

REAL, ALLOCATABLE, DIMENSION(:) :: QRadOutReport       !Surface thermal radiation heat gain at Outside face [J]
REAL, ALLOCATABLE, DIMENSION(:) :: QdotRadOutRep       !Surface thermal radiation heat transfer outside face surface [W]
REAL, ALLOCATABLE, DIMENSION(:) :: QdotRadOutRepPerArea ![W/m2]Surface thermal radiation heat transfer rate per m2 at 
                                                              !      Outside face surf 

REAL, ALLOCATABLE, DIMENSION(:) :: OpaqSurfInsFaceCondGainRep !Equals Opaq Surf Ins Face Cond
                                                                   ! when Opaq Surf Ins Face Cond >= 0
REAL, ALLOCATABLE, DIMENSION(:) :: OpaqSurfInsFaceCondLossRep !Equals -Opaq Surf Ins Face Cond
                                                                   ! when Opaq Surf Ins Face Cond  < 0
REAL, ALLOCATABLE, DIMENSION(:) :: OpaqSurfInsFaceConduction !Opaque surface inside face heat conduction flow (W)
                                                                  ! from inside of opaque surfaces, for reporting (W)
REAL, ALLOCATABLE, DIMENSION(:) :: OpaqSurfInsFaceConductionFlux ! Opaque surface inside face heat conduction flux (W/m2)
                                                                      ! from inside of opaque surfaces, for reporting (W/m2)
REAL, ALLOCATABLE, DIMENSION(:) :: OpaqSurfInsFaceConductionEnergy !Opaque surface inside face heat conduction flow (J)
                                                                  ! from inside of opaque surfaces, for reporting (J)

REAL, ALLOCATABLE, DIMENSION(:) :: OpaqSurfExtFaceCondGainRep !Equals Opaq Surf Ext Face Cond
                                                                   ! when Opaq Surf Ext Face Cond >= 0
REAL, ALLOCATABLE, DIMENSION(:) :: OpaqSurfExtFaceCondLossRep !Equals -Opaq Surf Ext Face Cond
                                                                   ! when Opaq Surf Ext Face Cond  < 0
REAL, ALLOCATABLE, DIMENSION(:) :: OpaqSurfOutsideFaceConduction !Opaque surface outside face heat conduction flow (W)
                                                                  ! from inside of opaque surfaces, for reporting (W)
REAL, ALLOCATABLE, DIMENSION(:) :: OpaqSurfOutsideFaceConductionFlux ! Opaque surface outside face heat conduct flux (W/m2)
                                                                         ! from outside of opaque surfaces, for reporting (W/m2)
REAL, ALLOCATABLE, DIMENSION(:) :: OpaqSurfOutsideFaceConductionEnergy !Opaque surface outside face heat conduction flow (J)
                                                                  ! from inside of opaque surfaces, for reporting (J)

REAL, ALLOCATABLE, DIMENSION(:) :: OpaqSurfAvgFaceCondGainRep !Equals Opaq Surf average Face Cond
                                                                   ! when Opaq Surf average Face Cond >= 0
REAL, ALLOCATABLE, DIMENSION(:) :: OpaqSurfAvgFaceCondLossRep !Equals -Opaq Surf average Face Cond
                                                                   ! when Opaq Surf average Face Cond  < 0
REAL, ALLOCATABLE, DIMENSION(:) :: OpaqSurfAvgFaceConduction !Opaque surface average heat conduction flow (W)
                                                                  ! net conduction from outside environ toward inside zone
                                                                  !  from inside of opaque surfaces, for reporting (W)
REAL, ALLOCATABLE, DIMENSION(:) :: OpaqSurfAvgFaceConductionFlux ! Opaque surface average face heat conduction flux (W/m2)
                                                                      ! net conduction from outside environ to inside zone
                                                                      !  from inside of opaque surfaces, for reporting (W/m2)
REAL, ALLOCATABLE, DIMENSION(:) :: OpaqSurfAvgFaceConductionEnergy !Opaque surface average heat conduction flow (J)
                                                                  ! net conduction from outside environ toward inside zone
                                                                  !  from inside of opaque surfaces, for reporting (J)

REAL, ALLOCATABLE, DIMENSION(:) :: OpaqSurfStorageGainRep !Equals Opaque surface stored heat conduction flow 
                                                                   ! when Opaque surface stored heat conduction flow  >= 0
REAL, ALLOCATABLE, DIMENSION(:) :: OpaqSurfStorageCondLossRep !Equals -Opaque surface stored heat conduction flow 
                                                                   ! when Opaque surface stored heat conduction flow   < 0
REAL, ALLOCATABLE, DIMENSION(:) :: OpaqSurfStorageConduction !Opaque surface stored heat conduction flow (W)
                                                                  ! storage of heat inside surface, positive is increasing in surf
REAL, ALLOCATABLE, DIMENSION(:) :: OpaqSurfStorageConductionFlux ! Opaque surface stored heat conduction flux (W/m2)
                                                                      ! storage of heat inside surface, positive is increasing in surf
REAL, ALLOCATABLE, DIMENSION(:) :: OpaqSurfStorageConductionEnergy !Opaque surface stored heat conduction flow (J)
                                                                  ! storage of heat inside surface, positive is increasing in surf

REAL, ALLOCATABLE, DIMENSION(:) :: OpaqSurfInsFaceBeamSolAbsorbed !Opaque surface inside face absorbed beam solar,
                                                                       ! for reporting (W)
REAL, ALLOCATABLE, DIMENSION(:) :: TempSurfOut         !Temperature of the Outside Surface for each heat transfer surface
                                                            ! used for reporting purposes only.  Ref: TH(x,1,1)
REAL, ALLOCATABLE, DIMENSION(:) :: QRadSWOutMvIns      !Short wave radiation absorbed on outside of movable insulation
REAL, ALLOCATABLE, DIMENSION(:) :: QBV                 !Beam solar absorbed by interior shades in a zone, plus
                                                            ! diffuse from beam not absorbed in zone, plus
                                                            ! beam absorbed at inside surfaces
REAL, ALLOCATABLE, DIMENSION(:) :: QC                  !Short-Wave Radiation Converted Direct To Convection
REAL, ALLOCATABLE, DIMENSION(:) :: QD                  !Diffuse solar radiation in a zone from sky and ground diffuse entering
                                                            !through exterior windows and reflecting from interior surfaces,
                                                            !beam from exterior windows reflecting from interior surfaces,
                                                            !and beam entering through interior windows (considered diffuse)
REAL, ALLOCATABLE, DIMENSION(:) :: QDforDaylight       !Diffuse solar radiation in a zone from sky and ground diffuse entering
                                                            !through exterior windows, beam from exterior windows reflecting
                                                            !from interior surfaces, and beam entering through interior windows
                                                            !(considered diffuse)
                                                            !Originally QD, now used only for QSDifSol calc for daylighting
REAL, ALLOCATABLE, DIMENSION(:) :: QDV                 !Diffuse solar radiation in a zone from sky and ground diffuse entering
                                                            !through exterior windows
REAL, ALLOCATABLE, DIMENSION(:) :: TCONV               !Fraction Of Radiated Thermal Converted To Convection In Interior Shades
REAL, ALLOCATABLE, DIMENSION(:) :: VMULT               !1/(Sum Of A Zone's Inside Surfaces Area*Absorptance)
REAL, ALLOCATABLE, DIMENSION(:) :: VCONV               !Fraction Of Short-Wave Radiation From Lights Converted To Convection
REAL, ALLOCATABLE, DIMENSION(:) :: NetLWRadToSurf    ! Net interior long wavelength radiation to a surface from other surfaces
REAL, ALLOCATABLE, DIMENSION(:) :: ZoneMRT           ! Zone Mean Radiant Temperature
REAL, ALLOCATABLE, DIMENSION(:) :: QRadSWLightsInAbs  !Short wave from Lights radiation absorbed on inside of opaque surface
! Variables that are used in both the Surface Heat Balance and the Moisture Balance
REAL, ALLOCATABLE, DIMENSION(:) :: QRadSWOutAbs        !Short wave radiation absorbed on outside of opaque surface
REAL, ALLOCATABLE, DIMENSION(:) :: QRadSWInAbs         !Short wave radiation absorbed on inside of opaque surface


REAL, ALLOCATABLE, DIMENSION(:) :: InitialDifSolInAbs    !Initial diffuse solar absorbed on inside of opaque surface [W/m2]
REAL, ALLOCATABLE, DIMENSION(:) :: InitialDifSolInTrans  !Initial diffuse solar transmitted out through window surface [W/m2]

!REAL variables from BLDCTF.inc and only used in the Heat Balance
REAL, ALLOCATABLE, DIMENSION(:,:,:):: TH    !Temperature History (SurfNum,Hist Term,In/Out) where:
                                                             !Hist Term (1 = Current Time, 2-MaxCTFTerms = previous times),
                                                             !In/Out (1 = Outside, 2 = Inside)
REAL, ALLOCATABLE, DIMENSION(:,:,:):: QH    !Flux History (TH and QH are interpolated from THM and QHM for
                                                             !the next user requested time step)
REAL, ALLOCATABLE, DIMENSION(:,:,:):: THM   !Master Temperature History (on the time step for the construct)
REAL, ALLOCATABLE, DIMENSION(:,:,:):: QHM   !Master Flux History (on the time step for the construct)
REAL, ALLOCATABLE, DIMENSION(:,:)  :: TsrcHist ! Temperature history at the source location (SurfNum,Term)
REAL, ALLOCATABLE, DIMENSION(:,:)  :: QsrcHist ! Heat source/sink history for the surface (SurfNum,Term)
REAL, ALLOCATABLE, DIMENSION(:,:)  :: TsrcHistM ! Master temperature history at the source location (SurfNum,Term)
REAL, ALLOCATABLE, DIMENSION(:,:)  :: QsrcHistM ! Master heat source/sink history for the surface (SurfNum,Term)

REAL, ALLOCATABLE, DIMENSION(:,:)  :: FractDifShortZtoZ ! Fraction of diffuse short radiation in Zone 2 transmitted to Zone 1
LOGICAL, ALLOCATABLE, DIMENSION(:) :: RecDifShortFromZ  ! True if Zone gets short radiation from another
LOGICAL                            :: InterZoneWindow=.false.   ! True if there is an interzone window


!     NOTICE
!
!     Copyright © 1996-2012 The Board of Trustees of the University of Illinois
!     and The Regents of the University of California through Ernest Orlando Lawrence
!     Berkeley National Laboratory.  All rights reserved.
!
!     Portions of the EnergyPlus software package have been developed and copyrighted
!     by other individuals, companies and institutions.  These portions have been
!     incorporated into the EnergyPlus software package under license.   For a complete
!     list of contributors, see "Notice" located in EnergyPlus.f90.
!
!     NOTICE: The U.S. Government is granted for itself and others acting on its
!     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
!     reproduce, prepare derivative works, and perform publicly and display publicly.
!     Beginning five (5) years after permission to assert copyright is granted,
!     subject to two possible five year renewals, the U.S. Government is granted for
!     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
!     worldwide license in this data to reproduce, prepare derivative works,
!     distribute copies to the public, perform publicly and display publicly, and to
!     permit others to do so.
!
!     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.
!

END MODULE DataHeatBalSurface



