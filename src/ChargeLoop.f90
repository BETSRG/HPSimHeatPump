    REAL FUNCTION CHARGM(DTVALU,IERR)

    USE DataSimulation

    IMPLICIT NONE

    REAL DTVALU
    INTEGER ICHRGE
    INTEGER IERR

    REAL DTVAL

    CHARACTER(LEN=13),PARAMETER :: FMT_1000 = "(A32,F7.2,A9)"
    CHARACTER(LEN=119),PARAMETER :: FMT_1003 = "('0CHARGM: TEST FOR CONVERGENCE ON REFRIGERANT CHARGE',/, 11  X,'ESTIMATED  CONDENSER SUBCOOLING      ',F10.3,' F DEG')"
    CHARACTER(LEN=110),PARAMETER :: FMT_1004 = "('0CHARGM: TEST FOR CONVERGENCE ON REFRIGERANT CHARGE',/, 11  X,'ESTIMATED  CONDENSER EXIT QUALITY    ',F10.4)"


    IERR = 0
    ICHRGE=1

    IF (MODE .EQ. TXVSIMULATION .AND. DTVALU .GT. 80) THEN
        DTVALU=(DTVALU+DTVAL)/2 !If it is higher then 80 F subcooling, take the average of the current and previous guesses ISI - 01/02/09
    END IF

    CALL HPDM(DTVALU)

    IF (FirstTimeChargeLoop) FirstTimeChargeLoop=.FALSE.

    !CHARGM = ( REFCHG - CALCHG ) * 4.0
    IF (MODE .EQ. 4) THEN
        CHARGM = ( CALCHG - REFCHG ) !More subcooling, more charge
    ELSE
        CHARGM = ( REFCHG - CALCHG ) !Less superheat, more charge
    END IF
    IF(ICHRGE.EQ.2) CHARGM = -CHARGM

    DTVAL = DTVALU

    IF (PrnLog .EQ. 1) WRITE(6,*)
    WRITE(*,*)

    !   VL: Previously:    
    !	IF(ICHRGE.EQ.2) GO TO 2
    !
    !	IF(DTVALU.LT.0.0) GO TO 1
    !    IF (MODE .EQ. 1) THEN
    !		IF (Unit .EQ. 1) THEN
    !		  DTunit=' (K)'
    !		  IF (PrnLog .EQ. 1) WRITE(6,FMT_1000)'Compressor suction supreheat: ',DTVAL/1.8,DTunit
    !		  WRITE(*,FMT_1000)'Compressor suction supreheat: ',DTVAL/1.8,DTunit
    !		ELSE
    !		  DTunit=' (R)'
    !		  IF (PrnLog .EQ. 1) WRITE(6,FMT_1000)'Compressor suction supreheat: ',DTVAL,DTunit
    !		  WRITE(*,FMT_1000)'Compressor suction supreheat: ',DTVAL,DTunit
    !		END IF
    !	ELSE
    !		IF (Unit .EQ. 1) THEN
    !		  DTunit=' (K)'
    !		  IF (PrnLog .EQ. 1) WRITE(6,FMT_1000)'Condenser subcooling: ',DTVAL/1.8,DTunit
    !		  WRITE(*,FMT_1000)'Condenser subcooling: ',DTVAL/1.8,DTunit
    !		ELSE
    !		  DTunit=' (R)'
    !		  IF (PrnLog .EQ. 1) WRITE(6,FMT_1000)'Condenser subcooling: ',DTVAL,DTunit
    !		  WRITE(*,FMT_1000)'Condenser subcooling: ',DTVAL,DTunit
    !		END IF
    !	END IF
    !
    !	GO TO 5
    !1	CONTINUE
    !	Xunit=' (%)'
    !	IF (MODE .EQ. 1) THEN
    !		DTVAL = 1.0 + DTVALU/500.
    !		IF (PrnLog .EQ. 1) WRITE(6,FMT_1000)'Compressor suction quality: ',DTVAL*100,Xunit
    !		WRITE(*,FMT_1000)'Compressor suction quality: ',DTVAL*100,Xunit
    !	ELSE
    !		DTVAL = 1.0 + DTVALU/500.
    !		DTVAL = -DTVALU/200.
    !		IF (PrnLog .EQ. 1) WRITE(6,FMT_1000)'Condenser quality: ',DTVAL*100,Xunit
    !		WRITE(*,FMT_1000)'Condenser quality: ',DTVAL*100,Xunit
    !	END IF
    !
    !	GO TO 5
    !
    !2	CONTINUE
    !	IF(DTVALU.LT.0.0) GO TO 3
    !	IF (PrnLog .EQ. 1) WRITE(6,FMT_1003) DTVAL
    !	IF (PrnCon .EQ. 1) WRITE(*,FMT_1003) DTVAL
    !	GO TO 5
    !3	CONTINUE
    !	DTVAL = -DTVALU/200.
    !	IF (PrnLog .EQ. 1) WRITE(6,FMT_1004) DTVAL
    !	IF (PrnCon .EQ. 1) WRITE(*,FMT_1004) DTVAL
    !
    !5	CONTINUE


    IF(ICHRGE.EQ.2) THEN

        IF(DTVALU.LT.0.0) THEN

            DTVAL = -DTVALU/200.
            IF (PrnLog .EQ. 1) WRITE(6,FMT_1004) DTVAL
            IF (PrnCon .EQ. 1) WRITE(*,FMT_1004) DTVAL

        ELSE

            IF (PrnLog .EQ. 1) WRITE(6,FMT_1003) DTVAL
            IF (PrnCon .EQ. 1) WRITE(*,FMT_1003) DTVAL

        END IF

    ELSE

        IF(DTVALU.LT.0.0) THEN

            Xunit=' (%)'
            IF (MODE .EQ. 1) THEN
                DTVAL = 1.0 + DTVALU/500.
                IF (PrnLog .EQ. 1) WRITE(6,FMT_1000)'Compressor suction quality: ',DTVAL*100,Xunit
                WRITE(*,FMT_1000)'Compressor suction quality: ',DTVAL*100,Xunit
            ELSE
                DTVAL = 1.0 + DTVALU/500.
                DTVAL = -DTVALU/200.
                IF (PrnLog .EQ. 1) WRITE(6,FMT_1000)'Condenser quality: ',DTVAL*100,Xunit
                WRITE(*,FMT_1000)'Condenser quality: ',DTVAL*100,Xunit
            END IF

        ELSE

            IF (MODE .EQ. 1) THEN
                IF (Unit .EQ. 1) THEN
                    DTunit=' (K)'
                    IF (PrnLog .EQ. 1) WRITE(6,FMT_1000)'Compressor suction supreheat: ',DTVAL/1.8,DTunit
                    WRITE(*,FMT_1000)'Compressor suction supreheat: ',DTVAL/1.8,DTunit
                ELSE
                    DTunit=' (R)'
                    IF (PrnLog .EQ. 1) WRITE(6,FMT_1000)'Compressor suction supreheat: ',DTVAL,DTunit
                    WRITE(*,FMT_1000)'Compressor suction supreheat: ',DTVAL,DTunit
                END IF
            ELSE
                IF (Unit .EQ. 1) THEN
                    DTunit=' (K)'
                    IF (PrnLog .EQ. 1) WRITE(6,FMT_1000)'Condenser subcooling: ',DTVAL/1.8,DTunit
                    WRITE(*,FMT_1000)'Condenser subcooling: ',DTVAL/1.8,DTunit
                ELSE
                    DTunit=' (R)'
                    IF (PrnLog .EQ. 1) WRITE(6,FMT_1000)'Condenser subcooling: ',DTVAL,DTunit
                    WRITE(*,FMT_1000)'Condenser subcooling: ',DTVAL,DTunit
                END IF
            END IF

        END IF

    END IF


    IF (Unit .EQ. 1) THEN
        MassUnit = ' (kg)'
        IF (PrnLog .EQ. 1) WRITE(6,FMT_1000)'           Desired charge = ',REFCHG*0.4536,MassUnit
        WRITE(*,FMT_1000)'           Desired charge = ',REFCHG*0.4536,MassUnit
        IF (PrnLog .EQ. 1) WRITE(6,FMT_1000)'        Calculated charge = ',CALCHG*0.4536,MassUnit
        WRITE(*,FMT_1000)'        Calculated charge = ',CALCHG*0.4536,MassUnit
    ELSE
        MassUnit = ' (lbm)'
        IF (PrnLog .EQ. 1) WRITE(6,FMT_1000)'           Desired charge = ',REFCHG,MassUnit
        WRITE(*,FMT_1000)'           Desired charge = ',REFCHG,MassUnit
        IF (PrnLog .EQ. 1) WRITE(6,FMT_1000)'        Calculated charge = ',CALCHG,MassUnit
        WRITE(*,FMT_1000)'        Calculated charge = ',CALCHG,MassUnit
    END IF

    RETURN 

    !!VL: Previously: 1000 FORMAT(A32,F7.2,A9)
    !!VL: Previously: 1003 FORMAT('0CHARGM: TEST FOR CONVERGENCE ON REFRIGERANT CHARGE',/, 11  X,'ESTIMATED  CONDENSER SUBCOOLING      ',F10.3,' F DEG')
    !!VL: Previously: 1004 FORMAT('0CHARGM: TEST FOR CONVERGENCE ON REFRIGERANT CHARGE',/, 11  X,'ESTIMATED  CONDENSER EXIT QUALITY    ',F10.4)

    END FUNCTION
