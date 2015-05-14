!     ******************************************************************
!     LNDCLC.F90
!     Copyright(c) Utah Division of Water Resources 2000
!
!     Created: 2/29/2012 8:26:35 AM
!     Author : STATE OF UTAH
!     Last change: SOU 4/8/2015 2:46:28 PM
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
!              calculate ungaged inflow.  FLOW = RESBAK(IR).
!     RSOUT  - Like RSBAK, but calculates outflow--equivalent to -RSBAK(IR).
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
!              called with WETDMD(ILND)
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
REAL (KIND=8) :: SWAN(13),IRRET
DATA SWAN/454.,0.,0.,0.,0.,0.,483.,2526.,2536.,1424.,985.,791.,9200./
!-----------------------------------------------------------------------
!     SAMPLE CODE FOR GENERAL MODEL
!-----------------------------------------------------------------------
!Bear River Ag Area 5 downstream of Bear River at Harer
CALL ARSUB(5)
!Balance flow at Stewart Dam
QX(75,J,K)=QX(57,J,K)+QX(62,J,K)-QX(5,J,K)
!Montpelier Creek
QX(54,J,K)=QX(59,J,K)+QX(57,J,K)

!North Eden Creek area
CALL ARSUB(3)
!QX12 flows to Bear Lake
QX(12,J,K)=QX(11,J,K)+QX(10,J,K)

!Little Creek area
CALL ARSUB(2)
!Little Creek flow to Bear Lake
QX(19,J,K)=QX(17,J,K)+QX(18,J,K)

!Big Creek area on south shore
CALL ARSUB(1)
QX(26,J,K)=QX(24,J,K)+QX(25,J,K)
!Garden City etc.
CALL MUNSUB(1)
!Big Creek flow into Bear Lake
QX(31,J,K)=QX(28,J,K)+QX(30,J,K)

!East shore Ag Lands
QX(33,J,K)=SWAN(K)
QX(42,J,K)=QX(33,J,K)+QX(35,J,K)+QX(38,J,K)+QX(41,J,K)
CALL ARSUB(4)
!return flow goes to Bear Lake.
QX(37,J,K)=QX(46,J,K)+QX(48,J,K)

!Calculate Bear Lake Outflow
QX(63,J,K)=0.0
!Calculate the balancing flow for Bear Lake
QX(34,J,K)=RSBAK(1)

!Main stem Bear River
QX(55,J,K)=QX(63,J,K)+QX(54,J,K)
!Riparian depletions
CALL WETLND(1)
QX(64,J,K)=QX(55,J,K)-QX(53,J,K)
!Idaho municipal
CALL MUNSUB(2)
QX(71,J,K)=QX(68,J,K)+QX(70,J,K)
!Main stem Bear River ag lands
CALL ARSUB(5)

!Balance flow for Bear River
QX(73,J,K)=QX(74,J,K)-QX(71,J,K)
RETURN
END

REAL (KIND=8) FUNCTION ErrorFunc()
ErrorFunc=0.0
END
