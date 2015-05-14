!     ******************************************************************
!     LNDCLC.F90
!     Copyright(c) Utah Division of Water Resources 2000
!
!     Created: 2/29/2012 8:26:35 AM
!     Author : STATE OF UTAH
!     Last change: SOU 7/9/2014 10:05:26 AM
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
!     For most applications this is the only subroutine that requires
!     modification.
!
!     Variables: see also each subroutine called
!     QX(I,J,K)    - Simulation flow I for year J and month K
!     NYRS         - Number of years in simulation
!     NLND         - Number of land areas in each simulation
!
!     Subroutines called:
!     ARSUB        - Performs water budget calculations for a land area
!     AVCALC       - Calculates values for last page of printout
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
!     RSOUT  - Like RSBAK, but calculates outflow.
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
!     WETDMD - Function to return riparian demand for riparian area ILND,
!		   called with WETDMD(ILND)
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
REAL (KIND=8),EXTERNAL :: ARDEM,DMDMD,WETDMD,RSBAK,RSOUT
REAL (KIND=8) :: CottBlanding(13),AgFraction,RecapFraction,AvailFlow
DATA CottBlanding/4*41.0,8*42.0,500.0/AgFraction/0.80/RecapFraction/.6/
REAL (KIND=8) :: CottDue(13),CottDeficit
DATA CottDue/41.,82.,123.,164.,206.,248.,290.,332.,374.,416.,458.,2*500./
REAL (KIND=8) :: CottTotal,GWMultiplier,GWLower
!DATA GWMultiplier/8.0/GWLower/12.0/
DATA GWMultiplier/1.0/GWLower/1.0/
!-----------------------------------------------------------------------
!     SAMPLE CODE FOR Blanding Water Budget Model
!-----------------------------------------------------------------------
QX(17,J,K)=QX(17,J,K)*GWMultiplier
QX(21,J,K)=QX(21,J,K)*GWMultiplier
QX(28,J,K)=QX(28,J,K)*GWLower
IF (K.EQ.1) THEN
  CottTotal=0.0
END IF
QX(3,J,K)=QX(2,J,K)+QX(1,J,K)
QX(7,J,K)=MIN(MAX(CottDue(K)-CottTotal+CottDeficit,CottBlanding(K)),QX(6,J,K))
CottTotal=CottTotal+QX(7,J,K)
CottDeficit=max(0.0,CottDeficit+CottDue(K)-CottTotal)
IF (K.EQ.12) THEN
  CottDeficit=CottDeficit+(500-CottTotal)
END IF
QX(8,J,K)=QX(6,J,K)-QX(7,J,K)
!Blanding lands above Recapture
CALL ARSUB(3)
AvailFlow=QX(3,J,K)+QX(7,J,K)-(QX(44,J,K)-QX(19,J,K))
IF (AvailFlow.LT.0.0) THEN
  QX(47,J,K)=-AvailFlow
  AvailFlow=0.0
END IF
QX(9,J,K)=MIN(MAX(0.0,SMX(2)-STO(2))+MAX(0.0,SMX(3)-STO(3)), &
  AvailFlow*(1.0-AgFraction))
QX(15,J,K)=AvailFlow-QX(9,J,K)
QX(5,J,K)=QX(3,J,K)+QX(7,J,K)-QX(9,J,K)-QX(15,J,K)
CALL REINI(2)
CALL REINI(3)
CALL REINI(4)
!Blanding Upper Canal
QX(43,J,K)=QX(16,J,K)
CALL ARSUB(2)
QX(44,J,K)=QX(19,J,K)+QX(5,J,K)
CALL REINI(1)
CALL MUNSUB(1)
CALL ARSUB(1)
QX(34,J,K)=QX(14,J,K)+QX(18,J,K)+QX(32,J,K)+QX(33,J,K)
QX(35,J,K)=QX(34,J,K)+QX(8,J,K)
CALL MUNSUB(2)
QX(40,J,K)=QX(37,J,K)+QX(38,J,K)
CALL WETLND(1)
QX(42,J,K)=QX(40,J,K)-QX(41,J,K)
RETURN
END

REAL (KIND=8) FUNCTION ErrorFunc()
ErrorFunc=0.0
END
