MODULE InputPreProcessor

IMPLICIT NONE !require explicit declaration of everything

PRIVATE !unless otherwise declared public

PUBLIC PreProcessInput

CONTAINS

    SUBROUTINE PreProcessInput
    
        !this routine will rename hpdata.idf, run epmacro to get the fluid properties, and rename that to in.idf for the E+ input processor
    
!DEC$ IF DEFINED(_WIN32)
        ! delete the previously backed up version of the input file
        call system('if exist inBackup.idf del inBackup.idf')
        ! backup the last input file
        call system('if exist in.idf rename in.idf inBackup.idf')
        ! rename the heat pump input file in preparation for epmacro
        !call system('copy E+_hpdata.idf in.imf')
        !call system('rename E+_hpdata.idf in.imf')
        !call system('copy Minimal_HPSim.idf in.imf')
        call system('copy ZoneWSHP_wDOAS_HPSim.idf in.imf')
        ! call epmacro on it
        call system('EPMacro.exe')
        ! now rename the file to be read by the E+ input processor
        call system('rename out.idf in.idf')
!DEC$ ELSEIF DEFINED(__linux)
        ! delete the previously backed up version of the input file
        call system('rm -f inBackup.idf > /dev/null')
        ! backup the last input file
        call system('mv in.idf inBackup.idf > /dev/null')
        ! rename the heat pump input file in preparation for epmacro
        call system('cp HPdataUnix.idf in.imf > /dev/null')
        ! call epmacro on it
        call system('./epmacro')
        ! now rename the file to be read by the E+ input processor
        call system('mv out.idf in.idf > /dev/null')
!DEC$ ELSE
        !write(*,*) 'Get off your mac!! :)'
!DEC$ ENDIF
    
    END SUBROUTINE

END MODULE
