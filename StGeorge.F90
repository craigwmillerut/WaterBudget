!     ******************************************************************
!     LNDCLC.F90
!     Copyright(c) Utah Division of Water Resources 2000
!
!     Created: 3/29/2011 2:48:35 PM
!     Author : STATE OF UTAH
!     Last change: CWM 5/18/2012 11:39:47 AM
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
REAL (KIND=8),EXTERNAL :: ARDEM,DMDMD,RSOUT
REAL (KIND=8) :: DIFF,DIFFBEF
INTEGER (KIND=4) :: GunQXAg(2),GunQXAgNo,GunQXMun(5),GunQXMunNo
DATA GunQXAg/36,39/GunQXAgNo/2/GunQXMun/36,41,43,45,46/ &
  GunQXMunNo/5/
REAL (KIND=8) :: DemLeft,DemBef1,DemBef2
INTEGER (KIND=4) :: JJJ
INTEGER (KIND=4) :: QuailWash(2),QuailWashNo
DATA QuailWash/61,76/QuailWashNo/2/
REAL (KIND=8) :: GunMunDel,GunCorr
LOGICAL (KIND=1) :: TargGunnStor,TargSandStor
!-----------------------------------------------------------------------
!     ST GEORGE WATER BUDGET MODEL, Model 31, Subarea 10-01-05
!-----------------------------------------------------------------------
! Hilldale area
!-------------------------------------------------------------------------------
CALL ARSUB(3)
QX(8,J,K)=QX(5,J,K)+QX(7,J,K)
! Hilldale municipal
CALL MUNSUB(2)
QX(79,J,K)=QX(10,J,K)+QX(11,J,K)
CALL WETLND(1)
QX(13,J,K)=QX(79,J,K)-QX(80,J,K)
!-------------------------------------------------------------------------------
! Fort Pearce Agricultural and Municipal Demands
!-------------------------------------------------------------------------------
QX(14,J,K)=QX(13,J,K)+ARDEM(1)-QIN(12,J,K)
QX(15,J,K)=QX(13,J,K)+QX(14,J,K)
CALL ARSUB(1)
DIFFBEF=1.E9
! QIN(12,J,K) is USGS 09408195, Fort Pearce Wash near St. George
DIFF=QIN(12,J,K)-QX(17,J,K)
DO WHILE (ABS(DIFF).GT..1.AND.ABS(DIFF-DIFFBEF).GT.0.1)
  QX(14,J,K)=QX(14,J,K)+DIFF
  CALL ARBAK(1)
  QX(15,J,K)=QX(14,J,K)+QX(13,J,K)
  CALL ARSUB(1)
  DIFFBEF=DIFF
  DIFF=QIN(12,J,K)-QX(17,J,K)
END DO
QX(81,J,K)=QX(20,J,K)+QX(21,J,K)
CALL WETLND(2)
QX(22,J,K)=QX(81,J,K)-QX(82,J,K)
!-------------------------------------------------------------------------------
! Gunlock Landarea municipal and agricultural demands
!-------------------------------------------------------------------------------
QX(78,J,K)=QIN(3,J,K)-QX(23,J,K)+DMDMD(1)+AGDEM(2)
! QIN(3,J,K) is USGS 09409880, Santa Clara River at Gunlock, UT
QX(77,J,K)=QX(23,J,K)+QX(78,J,K)
CALL MUNSUB(1)
QX(28,J,K)=QX(26,J,K)+QX(27,J,K)
CALL ARSUB(2)
QX(83,J,K)=QX(33,J,K)+QX(34,J,K)
DIFFBEF=1.E9
DIFF=QIN(3,J,K)-QX(83,J,K)
! Loop to match 09409880, Santa Clara River at Gunlock, UT
DO WHILE (ABS(DIFF).GT.0.1.AND.ABS(DIFF-DIFFBEF).GT.0.1)
  CALL MUNBAK(1)
  CALL ARBAK(2)
  QX(78,J,K)=QX(78,J,K)+DIFF
  QX(77,J,K)=QX(23,J,K)+QX(78,J,K)
  CALL MUNSUB(1)
  QX(28,J,K)=QX(26,J,K)+QX(27,J,K)
  CALL ARSUB(2)
  QX(83,J,K)=QX(33,J,K)+QX(34,J,K)
  DIFFBEF=DIFF
  DIFF=QIN(3,J,K)-QX(83,J,K)
END DO
!SELECT CASE (J+INYR-1)
!  CASE (1989:1990)
!    TargetStorOpt(1)=0
!    TargGunnStor=.FALSE.
!  CASE (1991)
!    SELECT CASE (K)
!      CASE (1:2)
!        TargetStorOpt(1)=0
!        TargGunnStor=.FALSE.
!      CASE DEFAULT
!        TargetStorOpt(1)=2
!        TargGunnStor=.TRUE.
!    END SELECT
!  CASE (2000)
!    SELECT CASE (K)
!      CASE (1:3)
!        TargetStorOpt(1)=2
!        TargGunnStor=.TRUE.
!      CASE DEFAULT
!        TargetStorOpt(1)=0
!        TargGunnStor=.FALSE.
!    END SELECT
!  CASE (2001)
!    SELECT CASE (K)
!      CASE (1:2)
!        TargetStorOpt(1)=0
!        TargGunnStor=.FALSE.
!      CASE DEFAULT
!        TargetStorOpt(1)=2
!        TargGunnStor=.TRUE.
!    END SELECT
!  CASE DEFAULT
!    TargetStorOpt(1)=2
!    TargGunnStor=.TRUE.
!END SELECT
CALL WETLND(3)
QX(35,J,K)=QX(83,J,K)-QX(84,J,K)
!IF (TargGunnStor) THEN
!  QX(36,J,K)=RSOUT(1)
!ELSE
  CALL REINI(1)
!END IF

!-------------------------------------------------------------------------------
! Santa Clara land area and municipal demand
! Calculate reach gain to match USGS 09413000, Santa Clara River near St. George
! QX35 is 09413000
!-------------------------------------------------------------------------------
!QX(37,J,K)=QIN(9,J,K)-QX(36,J,K)+ARDEM(4)+DMDMD(3)
!QX(38,J,K)=QX(36,J,K)+QX(37,J,K)
!GunMunDel=MIN(DMDMD(3),QX(36,J,K))
!QX(41,J,K)=GunMunDel
!QX(43,J,K)=GunMunDel
!QX(45,J,K)=GunMunDel
!QX(46,J,K)=GunMunDel
!IF (QX(38,J,K).LT.DMDMD(3).AND..NOT.TargGunnStor) THEN
!  DMD=DMDMD(1)-GunMunDel
!  CALL RESR(1,GunQXMun,GunQXMunNo)
!  MRESCAL(3,1)=MRESCAL(3,1)+QDVM
!END IF
!Add in Pahcoon Springs
QX(38,J,K)=QX(36,J,K)+QX(92,J,K)+QX(93,J,K)
CALL MUNSUB(3)
CALL ARSUB(4)
!IF(SHORT(4,J,K).GT.0.AND..NOT.TargGunnStor) then
!  CALL ARBAK(4)
!  DMD=SHORT(4,J,K)
!  CALL RESR(1,GunQXAg,GunQXAgNo)
!  RESCAL(4,1)=RESCAL(4,1)+QDVM
!  CALL ARSUB(4)
!end if
QX(41,J,K)=QX(36,J,K)-QX(49,J,K)
QX(43,J,K)=QX(41,J,K)-QX(42,J,K)
QX(45,J,K)=QX(43,J,K)+QX(44,J,K)
QX(47,J,K)=QX(45,J,K)-QX(46,J,K)
QX(85,J,K)=QX(47,J,K)+QX(49,J,K)
CALL WETLND(4)
QX(37,J,K)=QIN(9,J,K)-QX(85,J,K)+QX(86,J,K)
QX(50,J,K)=QX(85,J,K)-QX(86,J,K)+QX(37,J,K)
!DIFFBEF=1.E9
!DIFF=QIN(9,J,K)-QX(50,J,K)
!DemBef1=MRESCAL(3,1)
!DemBef2=RESCAL(4,1)
!DO WHILE (ABS(DIFF).GT.0.1.AND.ABS(DIFF-DIFFBEF).GT.0.1)
!  QX(37,J,K)=QX(37,J,K)+DIFF
!  GunMunDel=MIN(DMDMD(3),QX(37,J,K))
!  STO(1)=STO(1)+MRESCAL(3,1)+RESCAL(4,1)
!  MRESCAL(3,1)=0.0
!  RESCAL(4,1)=0.0
!  QX(41,J,K)=GunMunDel
!  QX(43,J,K)=GunMunDel
!  QX(45,J,K)=GunMunDel
!  QX(46,J,K)=GunMunDel
!  IF (QX(46,J,K).LT.MunSurf(3,J,K).AND..NOT.TargGunnStor) THEN
!    DMD=MunSurf(3,J,K)-QX(46,J,K)
!    CALL RESR(1,GunQXMun,GunQXMunNo)
!    MRESCAL(3,1)=QDVM
!  END IF
!  CALL ARBAK(4)
!  CALL ARSUB(4)
!  IF(SHORT(4,J,K).GT.0.AND..NOT.TargGunnStor) then
!    CALL ARBAK(4)
!    DMD=SHORT(4,J,K)
!    CALL RESR(1,GunQXAg,GunQXAgNo)
!    RESCAL(4,1)=RESCAL(4,1)+QDVM
!    CALL ARSUB(4)
!  end if
!  QX(45,J,K)=QX(43,J,K)+QX(44,J,K)
!  QX(47,J,K)=QX(45,J,K)-QX(46,J,K)
!  QX(85,J,K)=QX(47,J,K)+QX(49,J,K)
!  CALL WETLND(4)
!  QX(50,J,K)=QX(85,J,K)-QX(86,J,K)
!  DIFFBEF=DIFF
!  DIFF=QIN(9,J,K)-QX(50,J,K)
!END DO

!-------------------------------------------------------------------------------
! St. George land area
! Calculate reach gain to match USGS 09415000, Virgin River near St. George
!-------------------------------------------------------------------------------
QX(54,J,K)=MIN(QX(75,J,K),QIN(17,J,K))
DemLeft=MAX(0.0,QIN(17,J,K)-QX(54,J,K))
!-------------------------------------------------------------------------------
! Calculate inflow to Sand Hollow
!-------------------------------------------------------------------------------
QX(55,J,K)=QX(75,J,K)-QX(54,J,K)
IF (J+INYR-1.GE.2003) THEN
  QX(58,J,K)=RSOUT(3)
ELSE
  QX(58,J,K)=QX(55,J,K)
  ST(3,J,K)=0.0
END IF
QX(61,J,K)=RSOUT(2)
QX(60,J,K)=MIN(QX(61,J,K),DMDMD(4))
QX(72,J,K)=QX(61,J,K)-QX(60,J,K)
!IF (DemLeft>0.0) THEN
!  DMD=DemLeft
!  CALL RESR(2,QuailWash,QuailWashNo)
!ELSE
!  QX(76,J,K)=0.0
!END IF
!-------------------------------------------------------------------------------
! Calculate Virgin River reach gain
!-------------------------------------------------------------------------------
QX(56,J,K)=QIN(15,J,K)-QX(53,J,K)-QX(54,J,K)
QX(57,J,K)=QX(53,J,K)+QX(54,J,K)+QX(56,J,K)
QX(59,J,K)=MIN(QIN(16,J,K),QX(57,J,K))
QX(64,J,K)=MAX(0.0,QX(57,J,K)-QX(59,J,K))
!-------------------------------------------------------------------------------
! Calculate St. George municipal demand and return flow
!-------------------------------------------------------------------------------
IF (J.EQ.1.AND.K.EQ.6) THEN
  JJJ=1
END IF
!QX(60,J,K)=MIN(DMDMD(4),QX(61,J,K))
CALL MUNSUB(4)
IF (QX(60,J,K).GT.DMDMD(4)) THEN
  JJJ=1
END IF
!-------------------------------------------------------------------------------
! Calculate St. George wastewater return flow available to secondary uses 2007
! and beyond.
!-------------------------------------------------------------------------------
IF (J+INYR-1.GE.2008) THEN
  QX(73,J,K)=MIN(QX(69,J,K),QIN(18,J,K))
ELSE
  QX(73,J,K)=0.0
END IF
QX(74,J,K)=QX(69,J,K)-QX(73,J,K)
!-------------------------------------------------------------------------------
! Calculate St. George agricultural demand
!-------------------------------------------------------------------------------
IF (J.EQ.17.AND.K.EQ.8) THEN
  JJJ=1
END IF
CALL ARSUB(5)
QX(65,J,K)=QX(64,J,K)-QX(62,J,K)
QX(67,J,K)=QX(65,J,K)+QX(66,J,K)
QX(70,J,K)=QX(67,J,K)+QX(74,J,K)
QX(87,J,K)=QX(70,J,K)+QX(50,J,K)+QX(22,J,K)
!-------------------------------------------------------------------------------
! Calculate St. George riparian demand
!-------------------------------------------------------------------------------
CALL WETLND(5)
QX(71,J,K)=QX(87,J,K)-QX(88,J,K)
QX(90,J,K)=QIN(14,J,K)-QX(71,J,K)
QX(91,J,K)=QX(71,J,K)+QX(90,J,K)

RETURN
END

REAL (KIND=8) FUNCTION ErrorFunc()
ErrorFunc=0.0
END
