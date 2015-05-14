!     ******************************************************************
!     LNDCLC.F90
!     Copyright(c) Utah Division of Water Resources 2000
!
!     Created: 3/29/2011 2:48:35 PM
!     Author : STATE OF UTAH
!     Last change: CWM 7/2/2012 1:33:20 PM
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
REAL (KIND=8),EXTERNAL :: ARDEM,DMDMD,RSBAK,WETDMD
REAL (KIND=8) :: TotHunt,AreaHunt,StageHunt,MaxDiv,TotCCH,CUMHunt(12)
REAL (KIND=8) :: CUMCCH(12),CCHDef
REAL (KIND=8) :: Hunter(13),CCHCanal(13),HuntOutflow
INTEGER (KIND=4) :: JJJ,PriceQX(3)
DATA PriceQX/29,28,27/
DATA Hunter/1197.,1001.,739.,156.,99.,185.,1798.,1246.,1200.,1146., &
  1359.,1053.,11177./
DATA CUMHunt/1197.,2198.,2937.,3093.,3191.,3376.,5174.,6421.,7621., &
  8767.,10126.,11177./
!Average Cotttonwood Creek and Huntington Canal Deliveries
DATA CCHCanal/622.,0.,0.,0.,0.,0.,0.,542.,1264.,3543.,3599., &
  2805.,12374./
!Average Cotttonwood Creek and Huntington Canal Deliveries
DATA CUMCCH/622.,622.,622.,622.,622.,622.,622.,1164.,2428., &
  5971.,9569.,12374./
!-----------------------------------------------------------------------
!     CODE FOR CASTLE DALE, 08-02-04, MODEL 19
!-----------------------------------------------------------------------
IF (J.EQ.1) THEN
  CCHDef=0.0
END IF
QX(49,J,K)=QX(39,J,K)+DMDMD(2)-QX(50,J,K)
QX(1,J,K)=QX(49,J,K)+QX(50,J,K)
CALL MUNSUB(2)
QX(51,J,K)=MAX(0.0,QIN(6,J,K)-QX(39,J,K))
QX(29,J,K)=QIN(6,J,K)
CALL ARSUB(3)
QX(6,J,K)=QX(39,J,K)+QX(51,J,K)-QX(29,J,K)
IF (K.EQ.1) THEN
  TotCCH=0.0
END IF
IF (J.EQ.6.AND.K.EQ.11) THEN
  JJJ=1
END IF
CALL MUNSUB(3)
QX(11,J,K)=QX(9,J,K)+QX(47,J,K)
CALL MUNSUB(1)
QX(16,J,K)=QX(12,J,K)+QX(15,J,K)
CALL ARSUB(2)
QX(23,J,K)=QX(20,J,K)+QX(21,J,K)
!QX23 is now the amount which can be diverted to Hunter and Huntington
!Calculate the amount which can be delivered through the CCH Canal
SELECT CASE (K)
  CASE (2,3,4,5)
    QX(10,J,K)=0.0
  CASE DEFAULT
    QX(10,J,K)=MIN(QX(23,J,K),4620.,QX(20,J,K),QX(18,J,K),QX(16,J,K),QX(12,J,K), &
      QX(11,J,K),Max(CCHCanal(K),CUMCCH(K)-TotCCH)+CCHDef)
    !QX(10,J,K)=MIN(QX(23,J,K),4620.,QX(20,J,K),QX(18,J,K),QX(16,J,K), &
    !  QX(12,J,K),QX(11,J,K))
END SELECT
!Cumulative amounts diverted
TotCCH=TotCCH+QX(10,J,K)
IF (K.EQ.12) THEN
  CCHDef=max(0.0,CCHDef+CUMCCH(K)-TotCCH)
END IF
!Recalculate flows downstream
QX(11,J,K)=QX(9,J,K)-QX(10,J,K)
QX(12,J,K)=QX(11,J,K)-QX(13,J,K)
QX(16,J,K)=QX(12,J,K)+QX(15,J,K)
QX(18,J,K)=QX(16,J,K)-QX(17,J,K)
QX(20,J,K)=QX(18,J,K)-QX(19,J,K)
QX(23,J,K)=QX(20,J,K)+QX(21,J,K)
!Flows in Huntington Creek
QX(24,J,K)=QX(6,J,K)+QX(10,J,K)
!Do not have target storage in the first two years.  No records
QX(25,J,K)=QX(24,J,K)
IF (J.LE.2) THEN
  TargetStorOpt(1)=0
ELSE
  TargetStorOpt(1)=2
END IF
CALL REINI(1)
HuntOutflow=QX(27,J,K)
QX(26,J,K)=HuntOutflow
QX(25,J,K)=QX(25,J,K)-HuntOutflow
QX(27,J,K)=QX(27,J,K)-HuntOutflow
!Calculate flows downstream of Huntington
QX(28,J,K)=QX(26,J,K)+QX(27,J,K)
!IF (STO(1)-SMN(1).GT.0.0.AND.QX(29,J,K).GT.QX(28,J,K)) THEN
!  DMD=MAX(0.0,MIN(STO(1)-SMN(1),QIN(6,J,K)-QX(28,J,K)))
!  IF (DMD.GT.0.0) THEN
!    STO(1)=STO(1)-DMD
!    ST(1,J,K)=ST(1,J,K)-DMD
!    QX(27,J,K)=QX(27,J,K)+DMD
!    QX(28,J,K)=QX(28,J,K)+DMD
!  END IF
!END IF
!QX(29,J,K)=MIN(QX(28,J,K),QIN(6,J,K))
!
!The following is used only if the demand of Land Area 3
!rather than measured canal flows are to be used to serve Price
!through the Cleveland canal
!
!DMD=MAX(0.0,QIN(6,J,K)-QX(29,J,K))
!DMD=MAX(0.0,ARDEM(3)-QX(29,J,K))
!IF (DMD.GT.0.0) THEN
!  QDVM=MAX(0.0,MIN(QX(23,J,K),4620.-QX(10,J,K),QX(20,J,K),QX(18,J,K), &
!    QX(16,J,K),QX(12,J,K),QX(11,J,K),DMD))
!  IF (QDVM.GT.0.0) THEN
!    QX(10,J,K)=QX(10,J,K)+QDVM
!    QX(11,J,K)=QX(9,J,K)-QX(10,J,K)
!    QX(12,J,K)=QX(11,J,K)-QX(13,J,K)
!    QX(16,J,K)=QX(12,J,K)+QX(15,J,K)
!    QX(18,J,K)=QX(16,J,K)-QX(17,J,K)
!    QX(20,J,K)=QX(18,J,K)-QX(19,J,K)
!    QX(23,J,K)=QX(20,J,K)+QX(21,J,K)
!  END IF
!END IF
QX(30,J,K)=QX(28,J,K)
!QX(30,J,K)=QX(28,J,K)-QX(29,J,K)
CALL ARSUB(1)
!Calculate Riparian demand
QX(44,J,K)=MIN(QX(34,J,K)+QX(35,J,K),WETDMD(1))
QX(37,J,K)=QX(35,J,K)+QX(34,J,K)-QX(44,J,K)
QX(38,J,K)=QIN(9,J,K)-QIN(11,J,K)
QX(45,J,K)=QX(38,J,K)-QX(40,J,K)-QX(23,J,K)-QX(37,J,K)
RETURN
END

REAL (KIND=8) FUNCTION ErrorFunc()
ErrorFunc=0.0
END
