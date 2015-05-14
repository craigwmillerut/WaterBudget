!     ******************************************************************
!     LNDCLC.F90
!     Copyright(c) Utah Division of Water Resources 2000
!
!     Created: 2/29/2012 8:26:35 AM
!     Author : STATE OF UTAH
!     Last change: SOU 5/29/2014 11:13:20 AM
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
REAL (KIND=8) :: Diff,Erro
REAL (KIND=8) :: ShortFac
Data ShortFac/.40/
INTEGER (KIND=4) :: JJJ
INTEGER (KIND=4) :: PETTYQX(3),NUMPETTY
DATA PETTYQX/50,51,52/NUMPETTY/3/
INTEGER (KIND=4) :: HIGHQX(3),NUMHIGH
DATA HIGHQX/54,55,58/NUMHIGH/3/
INTEGER (KIND=4) :: OLDQX(5),NUMOLD
DATA OLDQX/50,51,53,62,63/NUMOLD/5/
INTEGER (KIND=4) :: LOWQX(5),NUMLOW
DATA LOWQX/50,51,53,62,64/NUMLOW/5/
!-----------------------------------------------------------------------
!     SAMPLE CODE FOR GENERAL MODEL
!-----------------------------------------------------------------------
!Upper SanPitch River
QX(3,J,K)=QX(2,J,K)+QX(1,J,K)
QX(5,J,K)=QX(4,J,K)+QX(3,J,K)
QX(7,J,K)=QX(5,J,K)+QX(6,J,K)
QX(9,J,K)=QX(7,J,K)+QX(8,J,K)

!Silver Creek River
CALL MUNSUB(3)
QX(15,J,K)=QX(14,J,K)+QX(13,J,K)
QX(16,J,K)=(MAX(0.0,ARDEM(3)-QX(15,J,K)))*ShortFac
QX(17,J,K)=QX(16,J,K)+QX(15,J,K)
CALL ARSUB(3)
QX(24,J,K)=QX(22,J,K)+QX(23,J,K)

!Below confluence Silver Creek and SanPitch
QX(25,J,K)=QX(9,J,K)+QX(24,J,K)
!Proportion water from Six Mile Creek
!Above Gunnison Reservoir to QX30
SELECT CASE (K)
  CASE (1:6)
    QX(30,J,K)=.5*QX(29,J,K)
  CASE DEFAULT
    QX(30,J,K)=.25*QX(29,J,K)
END SELECT
QX(45,J,K)=QX(29,J,K)-QX(30,J,K)
!Find Inflow to Nine Mile Reservoir
SELECT CASE (K)
  CASE (1:6)
    QX(46,J,K)=MIN(QX(45,J,K),.5*(QX(29,J,K)))
  CASE DEFAULT
    QX(46,J,K)=MIN(QX(45,J,K),.25*(QX(29,J,K)))
END SELECT
CALL REINI(2)
QX(47,J,K)=QX(45,J,K)-QX(46,J,K)
!Inflow to Gunnison Reservoir
SELECT CASE (K)
  CASE (1:6)
    QX(48,J,K)=MIN(QX(47,J,K),.5*QX(29,J,K))
  CASE DEFAULT
    QX(48,J,K)=MIN(QX(47,J,K),.25*QX(29,J,K))
END SELECT
QX(49,J,K)=QX(47,J,K)-QX(48,J,K)
QX(31,J,K)=QX(28,J,K)+QX(30,J,K)
!Estimate QX26 tributary flow
QX(26,J,K)=(MAX(0.0,ARDEM(1)-QX(25,J,K)-QX(31,J,K)))*ShortFac
QX(27,J,K)=QX(26,J,K)+QX(25,J,K)

!Just Above Gunnison Reservoir
QX(32,J,K)=QX(31,J,K)+QX(27,J,K)
CALL ARSUB(1)
QX(39,J,K)=QX(37,J,K)+QX(38,J,K)
CALL MUNSUB(1)
QX(44,J,K)=QX(42,J,K)+QX(43,J,K)
CALL REINI(1)

!Below Gunnison Reservoir
QX(49,J,K)=QX(47,J,K)-QX(48,J,K)
QX(51,J,K)=QX(50,J,K)+QX(49,J,K)
QX(52,J,K)=MIN(QX(51,J,K),QIN(9,J,K))
!Divert to Pettyville Canal
IF (QX(52,J,K).LT.QIN(9,J,K)) THEN
  DMD=MAX(0.0,QIN(9,J,K)-QX(52,J,K))
  !First call on Gunnison to fill PettyVille
  CALL RESR(1,PETTYQX,NUMPETTY)
  !Next add water to upstream reach gain
  IF (QDVM.LT.DMD) THEN
    Diff=DMD-QDVM
    QX(26,J,K)=QX(26,J,K)+Diff
    QX(27,J,K)=QX(27,J,K)+Diff
    QX(32,J,K)=QX(32,J,K)+Diff
    QX(35,J,K)=QX(35,J,K)+Diff
    QX(37,J,K)=QX(37,J,K)+Diff
    QX(39,J,K)=QX(39,J,K)+Diff
    QX(42,J,K)=QX(42,J,K)+Diff
    QX(44,J,K)=QX(44,J,K)+Diff
    QX(50,J,K)=QX(50,J,K)+Diff
    QX(51,J,K)=QX(51,J,K)+Diff
    QX(52,J,K)=QX(52,J,K)+Diff
  END IF
END IF
!Sanpitch below Pettyville
QX(53,J,K)=QX(51,J,K)-QX(52,J,K)
!12 Mile Creek
QX(61,J,K)=MAX(0.0,QX(56,J,K)-QX(57,J,K))
!Twelve Mile Creek to Highland Canal
QX(57,J,K)=QX(56,J,K)-QX(61,J,K)
!Sanpitch below 12-Mile Creek
QX(62,J,K)=QX(53,J,K)+QX(61,J,K)
!Calculate Old Field Canal
QX(63,J,K)=MIN(QIN(14,J,K),QX(62,J,K))
!Bring down water for Old Field Canal
!Call from Gunnison Reservoir
IF (QX(63,J,K).LT.QIN(14,J,K)) THEN
  DMD=QIN(14,J,K)-QX(63,J,K)
  CALL RESR(1,OLDQX,NUMOLD)
END IF

!Initialize 9-Mile Reservoir
CALL REINI(2)
!Combine Pettyville with outflow from 9-mile
QX(55,J,K)=QX(52,J,K)+QX(54,J,K)
!Highland Canal below 12-mile inflow.
QX(58,J,K)=QX(55,J,K)+QX(57,J,K)
!Increase flow in Highland Canal if lower than recorded
!Call water from Nine Mile Reservoir
IF (QX(58,J,K).LT.QIN(13,J,K)) THEN
  DMD=QIN(13,J,K)-QX(58,J,K)
  CALL RESR(2,HIGHQX,NUMHIGH)
  IF (QDVM.LT.DMD)THEN
    DIFF=MAX(0.0,DMD-QDVM)
    QX(26,J,K)=QX(26,J,K)+Diff
    QX(27,J,K)=QX(27,J,K)+Diff
    QX(32,J,K)=QX(32,J,K)+Diff
    QX(35,J,K)=QX(35,J,K)+Diff
    QX(37,J,K)=QX(37,J,K)+Diff
    QX(39,J,K)=QX(39,J,K)+Diff
    QX(42,J,K)=QX(42,J,K)+Diff
    QX(44,J,K)=QX(44,J,K)+Diff
    QX(50,J,K)=QX(50,J,K)+Diff
    QX(51,J,K)=QX(51,J,K)+Diff
    QX(52,J,K)=QX(52,J,K)+Diff
    QX(55,J,K)=QX(55,J,K)+Diff
    QX(58,J,K)=QX(58,J,K)+Diff
  ENDIF
END IF
!Enable Highland water to be used in Area 2
QX(59,J,K)=.25*QX(58,J,K)
!Calculate Highland for export
QX(60,J,K)=QX(58,J,K)-QX(59,J,K)

!Lower Sanpitch River above Ag Lands
QX(64,J,K)=QX(62,J,K)-QX(63,J,K)
IF (QIN(15,J,K).GT.QX(64,J,K)) THEN
  !Call Gunnison Reservoir
  DMD=MAX(0.0,QIN(15,J,K)-QX(64,J,K))
  CALL RESR(1,LOWQX,NUMLOW)
  IF (QDVM.LT.DMD)THEN
    DIFF=MAX(0.0,DMD-QDVM)
    QX(26,J,K)=QX(26,J,K)+Diff
    QX(27,J,K)=QX(27,J,K)+Diff
    QX(32,J,K)=QX(32,J,K)+Diff
    QX(35,J,K)=QX(35,J,K)+Diff
    QX(37,J,K)=QX(37,J,K)+Diff
    QX(39,J,K)=QX(39,J,K)+Diff
    QX(42,J,K)=QX(42,J,K)+Diff
    QX(44,J,K)=QX(44,J,K)+Diff
    QX(50,J,K)=QX(50,J,K)+Diff
    QX(51,J,K)=QX(51,J,K)+Diff
    QX(53,J,K)=QX(53,J,K)+Diff
    QX(62,J,K)=QX(62,J,K)+Diff
    QX(64,J,K)=QX(64,J,K)+Diff
  ENDIF
ELSE
  Diff=QX(64,J,K)-QIN(15,J,K)
  JJJ=1
ENDIF
QX(65,J,K)=QX(64,J,K)+QX(59,J,K)
!Lower Sanpitch Ag
CALL ARSUB(2)
QX(72,J,K)=QX(71,J,K)+QX(70,J,K)
!Gunnison & Mayfield
CALL MUNSUB(2)
QX(77,J,K)=QX(75,J,K)+QX(76,J,K)
CALL WETLND(1)
QX(79,J,K)=QX(77,J,K)-QX(78,J,K)

!Influenced by Other Sevier Subareas
!Gunnison-Fayette Canal inflow
QX(81,J,K)=QX(79,J,K)+QX(80,J,K)
!Gunnison-Fayette export
IF (K.GE.2.AND.K.LE.8) THEN
  QX(82,J,K)=MAX(0.0,MIN(QX(81,J,K),1200.))
END IF
!San Pitch River to Sevier
QX(83,J,K)=QX(81,J,K)-QX(82,J,K)

RETURN
END

REAL (KIND=8) FUNCTION ErrorFunc()
ErrorFunc=0.0
END
