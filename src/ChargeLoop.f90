REAL FUNCTION CHARGM(DTVALU,IERR)

    USE DataGlobals, ONLY: MaxNameLength
    USE DataSimulation

    IMPLICIT NONE

    REAL DTVALU
    INTEGER ICHRGE
    INTEGER IERR

    REAL DTVAL

    CHARACTER(LEN=13),PARAMETER :: FMT_1000 = "(A32,F7.2,A9)"
    CHARACTER(LEN=119),PARAMETER :: FMT_1003 = "('0CHARGM: TEST FOR CONVERGENCE ON REFRIGERANT CHARGE',/, 11  X,'ESTIMATED  CONDENSER SUBCOOLING      ',F10.3,' F DEG')"
    CHARACTER(LEN=110),PARAMETER :: FMT_1004 = "('0CHARGM: TEST FOR CONVERGENCE ON REFRIGERANT CHARGE',/, 11  X,'ESTIMATED  CONDENSER EXIT QUALITY    ',F10.4)"
    CHARACTER(LEN=MaxNameLength) :: sPrint

    IERR = 0
    ICHRGE=1

    IF (MODE .EQ. TXVSIMULATION .AND. DTVALU .GT. 80) THEN
        DTVALU=(DTVALU+DTVAL)/2 !If it is higher then 80 F subcooling, take the average of the current and previous guesses ISI - 01/02/09
    END IF

    CALL HPDM(DTVALU)

    IF (FirstTimeChargeLoop) FirstTimeChargeLoop=.FALSE.

    IF (MODE .EQ. 4) THEN
        CHARGM = ( CALCHG - REFCHG ) !More subcooling, more charge
    ELSE
        CHARGM = ( REFCHG - CALCHG ) !Less superheat, more charge
    END IF
    IF(ICHRGE.EQ.2) CHARGM = -CHARGM

    DTVAL = DTVALU

    IF(ICHRGE.EQ.2) THEN
        IF(DTVALU.LT.0.0) THEN
            DTVAL = -DTVALU/200.
            WRITE(sPrint,FMT_1004) DTVAL
        ELSE
            WRITE(sPrint,FMT_1003) DTVAL
        END IF
    ELSE
        IF(DTVALU.LT.0.0) THEN
            Xunit=' (%)'
            IF (MODE .EQ. 1) THEN
                DTVAL = 1.0 + DTVALU/500.
                WRITE(sPrint,FMT_1000)'Compressor suction quality: ',DTVAL*100,Xunit
            ELSE
                DTVAL = 1.0 + DTVALU/500.
                DTVAL = -DTVALU/200.
                WRITE(sPrint,FMT_1000)'Condenser quality: ',DTVAL*100,Xunit
            END IF
        ELSE
            IF (MODE .EQ. 1) THEN
                IF (Unit .EQ. 1) THEN
                    DTunit=' (K)'
                    WRITE(sPrint,FMT_1000)'Compressor suction supreheat: ',DTVAL/1.8,DTunit
                ELSE
                    DTunit=' (R)'
                    WRITE(sPrint,FMT_1000)'Compressor suction supreheat: ',DTVAL,DTunit
                END IF
            ELSE
                IF (Unit .EQ. 1) THEN
                    DTunit=' (K)'
                    WRITE(sPrint,FMT_1000)'Condenser subcooling: ',DTVAL/1.8,DTunit
                ELSE
                    DTunit=' (R)'
                    WRITE(sPrint,FMT_1000)'Condenser subcooling: ',DTVAL,DTunit
                END IF
            END IF

        END IF

    END IF
    CALL IssueOutputMessage(PrnLog, PrnCon, '')
    CALL IssueOutputMessage(PrnLog, PrnCon, TRIM(sPrint))

    IF (Unit .EQ. 1) THEN
        MassUnit = ' (kg)'
        WRITE(sPrint,FMT_1000)'           Desired charge = ',REFCHG*0.4536,MassUnit
        CALL IssueOutputMessage(PrnLog, PrnCon, TRIM(sPrint))
        WRITE(sPrint,FMT_1000)'        Calculated charge = ',CALCHG*0.4536,MassUnit
        CALL IssueOutputMessage(PrnLog, PrnCon, TRIM(sPrint))
    ELSE
        MassUnit = ' (lbm)'
        WRITE(sPrint,FMT_1000)'           Desired charge = ',REFCHG,MassUnit
        CALL IssueOutputMessage(PrnLog, PrnCon, TRIM(sPrint))
        WRITE(sPrint,FMT_1000)'        Calculated charge = ',CALCHG,MassUnit
        CALL IssueOutputMessage(PrnLog, PrnCon, TRIM(sPrint))
    END IF

    RETURN 

END FUNCTION
