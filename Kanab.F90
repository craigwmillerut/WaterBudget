!     ******************************************************************
!     LNDCLC.F90
!     Copyright(c) Utah Division of Water Resources 2000
!
!     Created: 3/29/2011 2:48:35 PM
!     Author : STATE OF UTAH
!     Last change: SOU 12/5/2012 8:00:59 AM
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
REAL (KIND=8) :: DIFF,GWPRO,TOL,SURPRO,DIFFBEF,Balance
DATA GWPRO,TOL,SURPRO/.56753568899,.05,.2106860215/
INTEGER (KIND=4) :: JJ
REAL (KIND=8),EXTERNAL :: ARDEM,DMDMD,WETDMD
!-----------------------------------------------------------------------
!     CODE FOR Kanab Model
!-----------------------------------------------------------------------
QX(1,J,K)=MIN(QIN(1,J,K)*0.5+ARDEM(1)+MUNSURF(1,J,K),QIN(1,J,K))
CALL MUNSUB(1)
QX(5,J,K)=GWPRO*QX(4,J,K)
QX(6,J,K)=QX(4,J,K)-QX(5,J,K)
QX(8,J,K)=QX(7,J,K)+QX(6,J,K)
CALL ARSUB(1)
QX(15,J,K)=QX(14,J,K)+QX(12,J,K)
CALL WETLND(1)
QX(17,J,K)=QX(15,J,K)-QX(16,J,K)
DIFF=QX(19,J,K)-(QX(17,J,K)+QX(18,J,K))
IF (J.EQ.23.AND.K.EQ.3) THEN
  JJ=1
END IF
!Code added 11/29/2012
!This adds ungauged positive flows to QX1
!And accumulates ungauged negative flows in QX18
DO WHILE (ABS(DIFF).GE.TOL)
  Balance=Diff+QX(1,J,K)+QX(18,J,K)
  QX(1,J,K)=MAX(0.0,Balance)
  QX(18,J,K)=MIN(0.0,Balance)
  QX(10,J,K)=0.0
  QX(12,J,K)=0.0
  CALL ARBAK(1)
  CALL MUNSUB(1)
  QX(5,J,K)=GWPRO*QX(4,J,K)
  QX(6,J,K)=QX(4,J,K)-QX(5,J,K)
  QX(8,J,K)=QX(7,J,K)+QX(6,J,K)
  CALL ARSUB(1)
  QX(15,J,K)=QX(14,J,K)+QX(12,J,K)
  CALL WETLND(2)
  QX(17,J,K)=QX(15,J,K)-QX(16,J,K)
  DIFF=QX(19,J,K)-(QX(17,J,K)+QX(18,J,K))
END DO
!Code removed 3/08/2012 - this iterated to give QX1
!QIN 1 is 0940360, Kanab Creek near Kanab, UT
!DIFF=QIN(1,J,K)-QX(17,J,K)
!IF (ABS(DIFF).LE.TOL.OR.ABS(DIFF-DIFFBEF).LE.TOL) EXIT
!DIFFBEF=DIFF
!CALL ARBAK(1)
!QX(7,J,K)=0.0
!QX(8,J,K)=0.0
!QX(10,J,K)=0.0
!QX(12,J,K)=0.0
!QX(15,J,K)=0.0
!QX(1,J,K)=MAX(0.0,QX(1,J,K)+DIFF)
!END DO
!---------
!The following was commented out 11/29/2012
!QX(18,J,K)=QX(19,J,K)-QX(17,J,K)
CALL MUNSUB(2)
QX(24,J,K)=QX(23,J,K)*GWPRO
QX(25,J,K)=QX(23,J,K)-QX(24,J,K)
QX(26,J,K)=QX(25,J,K)+QX(21,J,K)
CALL ARSUB(2)
QX(33,J,K)=QX(30,J,K)+QX(32,J,K)
CALL WETLND(1)
!QX(34,J,K)=MAX(0.0,MIN(.90*WETDMD(2),QX(33,J,K)))
QX(35,J,K)=QX(33,J,K)-QX(34,J,K)
!Surface outflow, from USGS estimates in Reach File, V1 from BASINS 4.0
!Approximately 1,085 AF/Yr
QX(36,J,K)=QX(37,J,K)-QX(35,J,K)
!Surface outflow estimated from Reach File, V1 in Basins 4.0
!Aproximately 1085 Af/yr
!QX(37,J,K)=QX(35,J,K)*SURPRO
!QX(36,J,K)=QX(37,J,K)-QX(35,J,K)
!Subsurface groundwater basin outflow = QX28
QX(38,J,K)=QX(5,J,K)+QX(24,J,K)
RETURN
END

REAL (KIND=8) FUNCTION ErrorFunc()
ErrorFunc=0.0
END
