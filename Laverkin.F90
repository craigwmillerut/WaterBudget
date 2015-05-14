!     ******************************************************************
!     LNDCLC.F90
!     Copyright(c) Utah Division of Water Resources 2000
!
!     Created: 3/29/2011 2:48:35 PM
!     Author : STATE OF UTAH
!     Last change: CWM 5/23/2012 3:38:34 PM
!     ******************************************************************

      SUBROUTINE LNDCLC
!-----------------------------------------------------------------------
!     SUBROUTINE LNDCLC
!     Created 02/21/89 by Craig W. Miller
!
!     This subroutine is the main program logic which makes each
!     modification of BUDGET2 unique.  The only thing which must be
!     modified is the order for handling land areas (this establishes
!     priority for water deliveries) and flow logic within the program.
!     Input and output are standardized and do not require modification
!     by the modeler.
!
!     Variables: see also each subroutine called
!     QX(I,J,K)    - Simulation flow I for year J and month K
!     NYRS         - Number of years in simulation
!     NLND         - Number of land areas in each simulation
!
!     Subroutines called:
!     ARSUB        - Performs water budget calculations for a land area
!     AVCALC       - Calculates values for last page of printout
!-----------------------------------------------------------------------
!     For most applications this is the only subroutine that requires
!     modification.
!
!     Functions of the Subroutines:
!     REINI  - With all inflows to reservoir IR calculated, call
!              REINI(IR) to calculate reservoir evaporation, spills.
!              REINI calculates spill down through each QX defined in
!              the input data.
!     ARSUB  - Performs all land area calculations as defined in the
!              input data.  All relevant upstream QX flows must
!              be calculated first.
!     MUNSUB - Performs caculations for municipal areas as defined in the
!              input data.  Shortages are not allowed in municipal areas and
!              when needed groundwater pumping is increased to meet demands.
!     ARLIM  - For those occasions where diversions must be limited
!              call ARLIM(L,FMAX).  L is the land area number and
!              FMAX represents the maximum diversion
!     ARDEL  - For those occasions where diversions must be certain
!              values, set diversions (QX Values) and then call ARDEL.
!     ARBAK  - If you do iterative calcs before going back to ARSUB(L)
!              call ARBAK(L) to reset QX'S, soil moisture and
!              reservoirs
!     RSBAK  - For situations where outflow, stage, and some inflow to a
!              reservoir are known call the function RSBAK(IR) to
!              calculate ungaged inflow.  FLOW = RESBAK(IR).  REINI
!              must be called later to do reservoir initialization.
!     RESR   - To call water down from a reservoir through several
!              QX's, use RESR.  Before calling RESR, set DEM=(Demand from
!              reservoir) and use the call RESR(IR,IQXS,NUMQX), where IR
!              is the reservoir number, IQXS, is an INTEGER*4 array of
!              numbers representing the QX numbers to route water through,
!              and NUMQX in the number of QX's in the array.  After the
!              call, QDVM is set to the amount actually called.
!     RESRR  - Same as RESR but calls water from one reservoir to another.
!              Call RESRR with CALL RESRR(IR,IQXS,NUMQX,IR2) where IR2 is
!              the destination reservoir.
!     WETLND - Calculates wetland consumptive use.  All relevant QX
!              flows must be calculated first.
!     ARDEM  - Function returning the total diversion demand for area.
!              Use as follows:   DEM=ARDEM(L), where L is the land area
!              number.
!     DMDMD -  Function returning the total domestic diversion demand
!              for area.  Use as follows DEM=DMDMD(L), where L is the
!              land area number.
!     SOLVE -  For those hard to solve iterative solutions, SOLVE works
!              wonders.  SOLVE is a function called as follows:
!              DIFF2 = SOLVE(DIFF,CFAC,ITER).  Before calling
!              SOLVE the first time in an iterative loop, set ITER to 0.
!              Solve returns the amount of correction to be given for
!              a value DIFF where DIFF is the difference between a
!              gaged value and a calculated value.  Often iterative
!              solutions yield hundreds of tiny steps to reach a desired
!              answer.  Solve increases the step size by CFAC^(ITER-1)
!              if the step is continually in the same direction.  This
!              increases solution efficiency tremendously.  CFAC must
!              should be greater than 1.0 or a default of 1.5 is used.
!-----------------------------------------------------------------------
use PARAMDIM
use COMMO
use PrintStuff
REAL (KIND=8),EXTERNAL :: ARDEM,DMDMD,WETDMD
REAL (KIND=8) :: DIFF,DIFFBEF
REAL (KIND=4) :: JJJ
!-----------------------------------------------------------------------
!     CODE FOR LaVerkin MODEL
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!Calculate evaporation for Harris Gubler and Meadow Hollow Reservoirs
!-----------------------------------------------------------------------
CALL REINI(2)
CALL REINI(3)
!-----------------------------------------------------------------------
!Calculate Kanarraville demands
!-----------------------------------------------------------------------
CALL MUNSUB(2)
QX(6,J,K)=QX(3,J,K)+QX(5,J,K)
CALL ARSUB(1)
QX(82,J,K)=MAX(0.0,MIN(WETDMD(1),QX(12,J,K)+QX(10,J,K)))
QX(13,J,K)=QX(10,J,K)+QX(12,J,K)-QX(82,J,K)
!-----------------------------------------------------------------------
!Calculate Harmony Group demands
!-----------------------------------------------------------------------
CALL MUNSUB(1)
QX(19,J,K)=QX(16,J,K)+QX(18,J,K)
CALL ARSUB(2)
QX(81,J,K)=MAX(0.0,MIN(WETDMD(2),QX(23,J,K)+QX(25,J,K)))
QX(26,J,K)=QX(23,J,K)+QX(25,J,K)-QX(81,J,K)
QX(27,J,K)=QX(26,J,K)+QX(13,J,K)
!-----------------------------------------------------------------------
!Ash Creek Reservoir
!-----------------------------------------------------------------------
CALL REINI(1)
IF(J.EQ.1.AND.K.EQ.7)THEN
  JJJ=1
ENDIF
QX(29,J,K)=QIN(9,J,K)-QX(28,J,K)
QX(30,J,K)=QX(28,J,K)+QX(29,J,K)
!-----------------------------------------------------------------------
!Calculate Toquerville demands
!-----------------------------------------------------------------------
CALL MUNSUB(3)
QX(35,J,K)=QX(34,J,K)+QX(32,J,K)
CALL ARSUB(3)
QX(83,J,K)=MAX(0.0,MIN(WETDMD(3),QX(39,J,K)+QX(41,J,K)))
QX(42,J,K)=QX(39,J,K)+QX(41,J,K)-QX(83,J,K)
!-----------------------------------------------------------------------
!Calculate Virgin Demands
!-----------------------------------------------------------------------
CALL MUNSUB(4)
QX(48,J,K)=QX(47,J,K)+QX(45,J,K)
CALL ARSUB(4)
QX(55,J,K)=QX(52,J,K)+QX(54,J,K)
QX(57,J,K)=QX(55,J,K)
!-----------------------------------------------------------------------
!Calculate LaVerkin Demands
!-----------------------------------------------------------------------
CALL MUNSUB(5)
QX(63,J,K)=QX(60,J,K)+QX(62,J,K)
CALL ARSUB(5)
QX(70,J,K)=QX(69,J,K)+QX(67,J,K)
DIFF=QIN(5,J,K)-QX(70,J,K)
DIFFBEF=1.0E10
!-----------------------------------------------------------------------
!Match Gauged LaVerkin Creek Flows
!-----------------------------------------------------------------------
IF (ABS(DIFF).GT.0.5) THEN
  DO
    DIFFBEF=DIFF
    CALL ARBAK(5)
    QX(58,J,K)=QX(58,J,K)+DIFF
    CALL MUNSUB(5)
    QX(63,J,K)=QX(60,J,K)+QX(62,J,K)
    CALL ARSUB(5)
    QX(70,J,K)=QX(69,J,K)+QX(67,J,K)
    DIFF=QIN(5,J,K)-QX(70,J,K)
    IF (ABS(DIFF).LE.0.5.OR.ABS(DIFF-DIFFBEF).LE.0.5) THEN
      EXIT
    END IF
  END DO
END IF
!-----------------------------------------------------------------------
!Calculate Balancing flows for VirginLower Virgin River Flows
!-----------------------------------------------------------------------
QX(65,J,K)=QX(57,J,K)-QX(64,J,K)
QX(72,J,K)=QX(65,J,K)-QX(71,J,K)
DIFF=QIN(6,J,K)-QX(72,J,K)
QX(56,J,K)=DIFF
QX(57,J,K)=QX(57,J,K)+DIFF
QX(65,J,K)=QX(65,J,K)+DIFF
QX(72,J,K)=QX(72,J,K)+DIFF
!-----------------------------------------------------------------------
!Calculate Lower Virgin River Flows
!-----------------------------------------------------------------------
QX(73,J,K)=QX(70,J,K)+QX(42,J,K)
QX(74,J,K)=QX(72,J,K)+QX(73,J,K)
CALL WETLND(4)
QX(76,J,K)=QX(74,J,K)-QX(75,J,K)
RETURN
END

REAL (KIND=8) FUNCTION ErrorFunc()
ErrorFunc=0.0
END
