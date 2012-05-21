      !PROGRAM EnergyPlus
	  SUBROUTINE EnergyPlus


!     NOTICE
!
!     Copyright © 1996-2003 The Board of Trustees of the University of Illinois
!     and The Regents of the University of California through Ernest Orlando Lawrence
!     Berkeley National Laboratory.  All rights reserved.
!
!     Portions of the EnergyPlus software package have been developed and copyrighted
!     by other individuals, companies and institutions.  These portions have been
!     incorporated into the EnergyPlus software package under license.
!
!     The following have contributed to EnergyPlus v1.0:
!
!     Portions of the EnergyPlus weather processor were developed by Office of Building
!     Technology, State and Community Programs, Energy Efficiency and Renewable Energy,
!     US Department of Energy, www.eren.doe.gov/buildings
!
!     Portions of the input processing, output processing, weather processor,
!     BLAST Translator were developed by US Army Corps of Engineers, Engineer Research and
!     Development Center, Construction Engineering Research Laboratory, 2902 Newmark Drive,
!     Champaign IL  61821. www.cecer.army.mil
!
!     Portions of the EnergyPlus utility software (e.g. EP-Launch, IDFEditor, DOE2Translator,
!     System Templates) were developed by GARD Analytics, Inc. 1028 Busse Highway, Park Ridge,
!     Illinois 60068-1802, USA (847) 698-5690, www.gard.com.  GARD performed independent
!     verification and validation testing of the software after developing the testing strategy
!     and plan.  GARD analytics was also responsible for gas absorption chiller and
!     desiccant dehumidifier models.
!
!     Portions of flow resolver, chiller models (absorption, electric, const cop, engine-driven,
!     gas-turbine), generator models (diesel electric, gas turbine), furnace models,
!     heat recovery loop, plant loop, plant condenser loop, air-change dependent inside film
!     coefficients were developed by Oklahoma State University, Stillwater, OK 74078.
!
!     Portions of EMPD moisture calculation model, DX Coil models, Air-cooled condenser model,
!     furnace models, cooling tower model, unitary system models, air-to-air heat pump and
!     air-to-air heat recovery model were developed by University of Central Florida,
!     Florida Solar Energy Center (FSEC), 1679 Clearlake Road, Cocoa, FL  32922, www.fsec.ucf.edu
!
!     EnergyPlus includes COMIS (Conjunction Of Multizone Infiltration Specialists) developed
!     by a multinational, multi-institutional effort under the auspices of the International
!     Energy Agency's Buildings and Community Systems Agreement working group focusing on
!     multizone air flow modeling (Annex 23) and now administered by the Swiss Federal Laboratories
!     for Materials Testing and Research (EMPA), Division 175, Überlandstrasse 129,
!     CH-8600 Dübendorf, Switzerland.
!
!     EnergyPlus v1.0.1 includes a link to TRNSYS (The Transient Energy System
!     Simulation Tool) for Photovoltaics calculations developed by Thermal Energy
!     System Specialists, 2916 Marketplace Drive, Suite 104, Madison, WI 53719; 
!     Tel: (608) 274-2577"
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





          ! PROGRAM INFORMATION:
          !       AUTHOR         Linda K. Lawrie, et al
          !       DATE WRITTEN   January 1997.....
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS PROGRAM:
          ! This program implements the calls for EnergyPlus (originally configured
          ! as the merger of BLAST/IBLAST and DOE-2 energy analysis programs.

          ! METHODOLOGY EMPLOYED:
          ! The method used in EnergyPlus is to simplify the main program as much
          ! as possible and contain all "simulation" code in other modules and files.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE InputProcessor
USE RefNameMod

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! PROGRAM PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! PROGRAM LOCAL VARIABLE DECLARATIONS:
      CHARACTER(len=80) ::  Refrigerant

      CALL ProcessInput
	  CALL Refrig(Refrigerant)

END SUBROUTINE


