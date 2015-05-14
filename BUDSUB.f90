!     Last change: SOU 2/25/2015 2:58:49 PM
!NAME FLOWDR
      SUBROUTINE FLOWDR (N,MONTH,TITLE,IYRBEG,IYREND,IQX)
      use PARAMDIM
      COMMON /FLOW/ Y
      REAL*8 Y(MPTS),FN,FI,FPER(MPTS),FDUR(MYEAR),FDT(8,25)
      REAL*8 FJ,FACT
      character, EXTERNAL :: ALLTRIM*255
      INTEGER*4 N,II,JJ,IYRBEG,IYREND,IQX,LL
      CHARACTER MONTH(3)*4
      CHARACTER*(*) TITLE
      LOGICAL (KIND=2) :: QXused(MQX)
!      SORT DATA FROM LARGEST TO SMALLEST
      CALL SORT (N)
!      CALCULATE AND INTERPOLATE PER CENT OF TIME
      FN=N
      DO 19 II=1,N
      FI=II
      FPER(II)=FI/FN*100.
   19 CONTINUE
      DO 23 JJ=1,MYEAR
      FJ=JJ
      DO 20 II=2,N
      IF (FPER(II)-FJ) 20,22,21
   20 CONTINUE
   21 FACT=(FJ-FPER(II-1))/(FPER(II)-FPER(II-1))
      FDUR(JJ)= Y(II-1)+FACT*(Y(II)-Y(II-1))
      GO TO 23
   22 FDUR(JJ)=Y(II)
   23 CONTINUE
!
!      PRINT FLOW DURATION TABLE
!
      CALL HEADIN
      WRITE(6,110) MONTH,IYRBEG,IYREND,IQX,TITLE
  110 FORMAT(    /35X,3A4,'FLOW DURATION TABLE (',I4,'-',I4,')  FOR ', &
             'QX(',I2,')'/35X,A//)
      WRITE(6,111)
  111 FORMAT(3X,4(6X,'PER CENT   DISCHARGE ')/ &
             11X,'TIME',6X,' (CFS) ',3(10X,'TIME',6X,' (CFS) ')/ &
             1X,4(8X,'********   ********')/)
!      CREATE PRINT TABLE
      DO 25 L=1,MYEAR
      II=L
      IF(L.GT.25) II=L-25
      IF(L.GT.50) II=L-50
      IF(L.GT.75) II=L-75
      JJ=2
      IF(L.GT.25) JJ=4
      IF(L.GT.50) JJ=6
      IF(L.GT.75) JJ=8
      FDT(JJ,II)= FDUR(L)
      FDT(JJ-1,II) = L
   25 CONTINUE
      DO 28 L=1,25
      LL=L-1
      IF(MOD(LL,5)) 26,26,27
   26 WRITE(6,103)
  103 FORMAT(80X)
   27 WRITE(6,112) (FDT(JJ,L),JJ=1,8)
  112 FORMAT(4(10X,F6.0,4X,F7.0))
   28 CONTINUE
   30 CONTINUE
      RETURN
      END

!NAME HEADIN
      SUBROUTINE HEADIN
      use PARAMDIM
      use PrintStuff
      character, EXTERNAL :: ALLTRIM*255
      IPAGE=IPAGE+1
      if (IPAGE.EQ.1) then
        WRITE (6,"(' ',//9X,A,T89,'  PAGE NO.',I4,2X,A/)") &
          trim(AllTrim(TITLE)),IPAGE,TRIM(DATE2)
      else
        WRITE (6,1510) trim(AllTrim(TITLE)),IPAGE,TRIM(DATE2)
      end if
 1510 FORMAT(1H1,//9X,A,T89,'  PAGE NO.',I4,2X,A/)
      LINE=5
      RETURN
      END

!----------------------------------------------------------------------
!NAME SEEP
!----------------------------------------------------------------------
      REAL*8 FUNCTION SEEP(IR)
      use PARAMDIM
      use COMMO
      use PrintStuff
      LOGICAL*1 FOUND
      REAL*8 RESELEV,RESAREA
      INTEGER*4 IDAYS(12),IR
      DATA IDAYS/31,30,31,31,28,31,30,31,30,31,31,30/
      CALL RACE(IR,STO(IR),RESAREA,RESELEV)
      FOUND=.FALSE.
      SEEP=0.0
      !---------------------------------------------------------------
      !No seepage, return SEEP=0.0
      !---------------------------------------------------------------
      IF(SeepOpt(IR).LE.0)THEN
         SEEP=0.0
         RETURN
      ENDIF
      !---------------------------------------------------------------
      !Look up seepage from a table
      !---------------------------------------------------------------
      IF (SeepOpt(IR).EQ.1) THEN
        SEEP=0.0
        DO I=1,NSEEP(IR)
           IF(RESELEV.LE.ESEEP(IR,I))THEN
              IF(I.EQ.1)THEN
                 SEEP=(RESELEV-E(IR,1))/(ESEEP(IR,I)-E(IR,1))*SSEEP(IR,I)
              ELSE
                 SEEP=(SSEEP(IR,I)-SSEEP(IR,I-1))*( &
                       RESELEV-ESEEP(IR,I-1))/(ESEEP(IR,I)- &
                       ESEEP(IR,I-1))+ &
                       SSEEP(IR,I-1)
              ENDIF
              FOUND=.TRUE.
              EXIT
           ENDIF
        ENDDO
        IF(.NOT.FOUND)THEN
           SEEP=(SSEEP(IR,NSEEP(IR))-SSEEP(IR,NSEEP(IR)-1))*( &
                 RESELEV-ESEEP(IR,NSEEP(IR)-1))/(ESEEP(IR,NSEEP(IR))- &
                 ESEEP(IR,NSEEP(IR)-1))+ &
                 SSEEP(IR,NSEEP(IR)-1)
        ENDIF
      !---------------------------------------------------------------
      !Return a standard monthly value for seepage
      !---------------------------------------------------------------
      ELSEIF (SeepQXOpt(IR).EQ.2) THEN
        SEEP=QSM(IR,K)
      END IF
!     CONVERT TO ACRE-FEET
      SEEP=SEEP*FLOAT(IDAYS(K))*1.983471074
      RETURN
      END
!----------------------------------------------------------------------
!NAME PLMLPT
!----------------------------------------------------------------------
      SUBROUTINE PLMLPT(NPOS,NMAX,NPTS,K,IPLT,QDVR,QQ)
!      THIS SUBROUTINE COMPUTES MONTHLY VALUES FOR 2 DIMENSIONAL ARRAYS
!      FOR CREATING PLOT FILES
      INTEGER*4 NPOS
      INTEGER*4 NMAX,IPLT(NPOS),I,L,NPTS,K
      REAL*8 QDVR(NMAX,13),QQ(23)
      DO I=1,NPOS
          L=IPLT(I)
          IF(L.LE.0) CYCLE
          NPTS=NPTS+1
          QQ(NPTS)=QDVR(L,K)
          IF(NPTS.GE.23) RETURN
      ENDDO
      RETURN
      END
!----------------------------------------------------------------------
!NAME PLTLPT
!----------------------------------------------------------------------
      SUBROUTINE PLTLPT(NPOS,NMAX,NPTS,IPLT,QDVR,QQ)
!      THIS SUBROUTINE COMPUTES ANNUAL VALUES FOR 2 DIMENSIONAL ARRAYS
!      FOR CREATING PLOT FILES
      INTEGER*4 NMAX,I,L,NPTS,K
      REAL*8 QDVR(NMAX,13),QQ(23),SUM
      INTEGER*4 NPOS
      INTEGER*4 IPLT(NPOS)
      DO 10 I=1,NPOS
      L=IPLT(I)
      IF(L.LE.0) GOTO 10
      NPTS=NPTS+1
      SUM=0.
      DO 5 K=1,12
    5 SUM=SUM+QDVR(L,K)
      QQ(NPTS)=SUM
      IF(NPTS.GE.23) RETURN
   10 CONTINUE
      RETURN
      END
!----------------------------------------------------------------------
!NAME PLTMPT
!----------------------------------------------------------------------
      SUBROUTINE PLTMPT(NPOS,NMAX,NPTS,IPLT,QXX,QQ)
!      THIS SUBROUTINE COMPUTES MONTHLY VALUES FOR 3 DIMENSIONAL ARRAYS
!      FOR CREATING PLOT FILES
      use PARAMDIM
      INTEGER*4 NMAX,I,NPTS,NPOS,LL
      REAL*8 QXX(NMAX,MYEAR,13),QQ(23)
      INTEGER*4 IPLT(NPOS)
      DO I=1,NPOS
          LL=IPLT(I)
          IF(LL.LE.0) CYCLE
          NPTS=NPTS+1
          QQ(NPTS)=QXX(LL,J,K)
          IF(NPTS.GE.23) RETURN
      ENDDO
      RETURN
      END
!----------------------------------------------------------------------
!NAME PLTPTS
!----------------------------------------------------------------------
      SUBROUTINE PLTPTS(NPOS,NMAX,NPTS,IPLT,QXX,QQ,RESSTO)
!      THIS SUBROUTINE COMPUTES ANNUAL VALUES FOR 3 DIMENSIONAL ARRAYS
!      FOR CREATING PLOT FILES
      use PARAMDIM
      INTEGER*4 NPOS
      INTEGER*4 NMAX,IPLT(NPOS),I,JJ,LL,KK,NPTS
      REAL*8 QXX(NMAX,MYEAR,13),QQ(23),SUM
      LOGICAL*1 RESSTO
      DO 10 I=1,NPOS
      LL=IPLT(I)
      IF(LL.LE.0) GOTO 10
      NPTS=NPTS+1
      SUM=0.
      DO 5 KK=1,12
    5 SUM=SUM+QXX(LL,J,KK)
!        IF RES STORAGE GIVE AVERAGE INSTEAD OF TOTAL
      IF(RESSTO) SUM=SUM/12.
      QQ(NPTS)=SUM
      IF(NPTS.GE.23) RETURN
   10 CONTINUE
      RETURN
      END

!----------------------------------------------------------------------
!NAME PRINTR
!----------------------------------------------------------------------
      SUBROUTINE PRINTR
!
!     GENERAL WATER BUDGET PRINT SUBROUTINE
!
      use PARAMDIM
      USE SubVars
      use COMMO
      use PrintStuff
!     REAL*8 TOTMI(13),TDIV(13),TOTWT(13)
!     INTEGER*4 IQXFL(13)
      REAL*8 PRAV(13),TMAV(13),TEMP(13)
      REAL*8 SHORTM(MLAND,13),STM(MRES,13),EVAPM(MRES,13)
      REAL*8 AGPOT(MLAND,13),AGPERC(MLAND,13),AGTEMP(MLAND,13)
      REAL*8 AGPRE(MLAND,13),SUBPOT(MLAND,13),RIPDEM(MLAND,13)
      REAL*8 RIPPRA(MLAND,13),RIPPRE(MLAND,13),RIPTEM(MLAND,13)
      REAL*8 SRTPC(MLAND,MYEAR),SRTPCM(MLAND),TEMPARR(13)
      REAL*8 HEADM(MPP,13),ENERM(MPP,13),ELVM(MRES,13),SARM(MRES,13)
      REAL*8 FMAX,FMIN,RESBAL(13),AnnYield(13),MDem(MLAND,13)
      REAL*8 MDiv(MLAND,13),MGW(MLAND,13),MSec(MLAND,13)
      CHARACTER (LEN=255),EXTERNAL :: JustRight, AllTrim, CenterIt
      REAL*8 SUM,SUM1,SUM2,FUN,TotAcres,BasOut(13),DepleteM(MLAND,13)
      REAL (KIND=8) :: SubSupply(MYEAR,13),SubSupplyM(13)
      REAL (KIND=8) :: TotGWUse(MYEAR,13),TotGWUseM(13)
      REAL (KIND=8) :: SubInflowM(13)
      REAL (KIND=8) :: SubOutflowM(13)
      REAL (KIND=8) :: AgSurface(MYEAR,13),AgSurfaceM(13)
      REAL (KIND=8) :: AgGw(MYEAR,13),AgGwM(13)
      REAL (KIND=8) :: AgTotal(MYEAR,13),AgTotalM(13)
      REAL (KIND=8) :: AgDepletion(MYEAR,13),AgDepletionM(13)
      REAL (KIND=8) :: BasinInM(13),BasinOutM(13)
      REAL (KIND=8) :: MandISurface(MYEAR,13),MandISurfaceM(13)
      REAL (KIND=8) :: MandIGw(MYEAR,13),MandIGwM(13)
      REAL (KIND=8) :: MandITotal(MYEAR,13),MandITotalM(13)
      REAL (KIND=8) :: MandIDepM(13)
      REAL (KIND=8) :: WetDepM(13)
      REAL (KIND=8) :: AgDepleteM(MLAND,13)
      REAL (KIND=8) :: AgExcess(MYEAR,13),AgExcessM(13)
      REAL (KIND=8) :: SubSurface(MYEAR,13),SubSurfaceM(13)
      REAL (KIND=8) :: SubSurfDiv(MYEAR,13),SubSurfDivM(13)
      REAL (KIND=8) :: TotMining(13),TotWMining(13)
      REAL (KIND=8) :: YieldWMiningM(13)
      !REAL (KIND=8) :: GWMiningQ(MYEAR,13)
      REAL (KIND=8) :: ReserEvap(MYEAR,13),ReserEvapM(13)
      REAL (KIND=8) :: SubImportM(13)
      REAL (KIND=8) :: SubExportM(13)
      REAL (KIND=8) :: SubResStDif(MYEAR,13),SubResStDifM(13)
      REAL (KIND=8) :: SubResEvap(MYEAR,13),SubResEvapM(13)
      REAL (KIND=8) :: SubTranExpoM(13)
      REAL (KIND=8) :: SubTranImpM(13)
      REAL*8 SubAvAcres(MYEAR,MCROP)
      REAL*8 DepPot(13),DepSec(13),DemIn(13),DemOut(13)
      Character (Len=255) :: Allt,LeftAdj
      REAL*8 ROUND,X,VALU
      INTEGER*8 IX,M,IP,LNEX,LL,LX5,LX,IRSFLG
      LOGICAL*1 WTPAST,isFirst,QXused(MQX),isFound
      CHARACTER (LEN=20),DIMENSION(18) :: AcreTitles=(/ "Alfalfa  ", &
        "Pasture  ","Hay      ","Grain    ","Corn     ","Orchard  ", &
        "Sorghum  ","Turf     ","Onions   ","OtherHort","Potatoes ", &
        "Berries  ","OtherVeg ","Tomatoes ","Beans    ","Vineyard ", &
        "SubPast  ","SubHay   "/)
      CHARACTER (LEN=20) :: CropTitles(20)
      INTEGER (KIND=4) :: iCropMap(20),nCrops,iFirst,iLast,JJ
      CHARACTER RECOUT*150,RECOU2*150
!        ROUND FUNCTION
      ROUND(X,IX)= NINT(X*10.**IX)/10.**IX
!
!     PRINT SIMULATION DATA
!
!     PRINT OUT DIVERSION REQUIREMENTS
!
      WTPAST=.FALSE.
      DO L=1,NLND
          CALL HEADIN
          WRITE(6,'(9X,"Agricultural Data"/)')
      WRITE(6,"(//30X,'Cropland Acres by Year for Land Area(',I2,') ',A/)") &
        L,trim(AllTrim(PLAND(L)))
      LINE=LINE+5
      nCrops=0
      iCropMap=0
      CropTitles=""
      DO I=1,18
        IF (SubCropAc(L,I).GT.0.0) THEN
          nCrops=nCrops+1
          CropTitles(nCrops)=AcreTitles(I)
          iCropMap(nCrops)=I
        END IF
      END DO
      nCrops=nCrops+1
      CropTitles(nCrops)="Total"
      iCropMap(nCrops)=19
      SubAvAcres=0.0
      DO I=1,(INT((nCrops-1)/12)+1)
        IF (LINE>LinesPerPage-NYRS-5) THEN
          CALL HEADIN()
        END IF
        iFirst=1+(I-1)*12
        iLast=min(nCrops,I*12)
        WRITE(6,"(6X,'Year',2X,12(A9,2X)/)")(JustRight(CropTitles(M),9) &
          ,M=iFirst,iLast)
        LINE=LINE+2
        DO J=1,NYRS
          WRITE(6,"(6X,I4,2X,12(F9.1,2X))")IYEAR(J),(SubCropAcres(L,J,iCropMap(M)), &
            M=iFirst,iLast)
            LINE=LINE+1
        END DO
        WRITE(6,"(/6X,'MEAN',2X,12(F9.1,2X))")(SubCropAc(L,iCropMap(M)), &
            M=iFirst,iLast)
            LINE=LINE+2
      ENDDO
      IF (LINE>LinesPerPage-NYRS-5) THEN
        CALL HEADIN()
      END IF
          LINE=LINE+1
          DO K=1,12
            SUM=0.0
            DO J=1,NYRS
              SUM=SUM+CONUSE(K,J,L)+WCUSE(L,J,K)
            END DO
            AGPOT(L,K)=SUM/FNYRS
            AGPOT(L,13)=AGPOT(L,13)+AGPOT(L,K)
            SUM=0.0
            DO J=1,NYRS
              SUM=SUM+PERCO(K,J,L)
            END DO
            AGPERC(L,K)=SUM/FNYRS
            AGPERC(L,13)=AGPERC(L,13)+AGPERC(L,K)
            SUM=0.0
            DO J=1,NYRS
              SUM=SUM+TEM(L,J,K)
            END DO
            AGTEMP(L,K)=SUM/FNYRS
            AGTEMP(L,13)=AGTEMP(L,13)+AGTEMP(L,K)/12.
            SUM=0.0
            DO J=1,NYRS
              SUM=SUM+PRE(L,J,K)
            END DO
            AGPRE(L,K)=SUM/FNYRS
            AGPRE(L,13)=AGPRE(L,13)+AGPRE(L,K)
          END DO
          WRITE (6,1301) L,trim(AllTrim(PLAND(L)))
 1301     FORMAT (      /,25X,'Potential CU for Land Area(',I2,') ',A/)
          WRITE (6,202) (VAR(I),I=1,14)
          LINE=LINE+3
          DO J=1,NYRS
            WRITE (6,302) IYEAR(J),((CONUSE(K,J,L)+WCUSE(L,J,K)),K=1,13)
            LINE=LINE+1
          END DO
          WRITE (6,303) (AGPOT(L,K),K=1,13)
          LINE=LINE+2
          IF (LINE>LinesPerPage-NYRS-5) THEN
            CALL HEADIN()
            WRITE(6,'(9X,"Agricultural Data"/)')
            LINE=LINE+1
          END IF
          WRITE (6,1401) L,trim(AllTrim(PLAND(L)))
 1401     FORMAT (      /,25X,'Excess Rainfall for Land Area(',I2,') ',A/)
          WRITE (6,202) (VAR(I),I=1,14)
          LINE=LINE+3
          DO J=1,NYRS
            WRITE (6,302) IYEAR(J),(PERCO(K,J,L),K=1,13)
            LINE=LINE+1
          END DO
          WRITE (6,303) (AGPERC(L,K),K=1,13)
          LINE=LINE+2
          IF (LINE>LinesPerPage-NYRS-5) THEN
            CALL HEADIN()
            WRITE(6,'(9X,"Agricultural Data"/)')
            LINE=LINE+1
          END IF
          WRITE (6,1501) L,trim(AllTrim(PLAND(L)))
 1501     FORMAT (      /,25X,'Rainfall for Land Area(',I2,') ',A/)
          WRITE (6,202) (VAR(I),I=1,14)
          LINE=LINE+3
          DO J=1,NYRS
            WRITE (6,"(6X,I4,12F9.2,F10.2)") IYEAR(J),(PRE(L,J,K),K=1,13)
            LINE=LINE+1
          END DO
          WRITE (6,"(/,6X,'MEAN',12F9.2,F10.2)") (AGPRE(L,K),K=1,13)
          LINE=LINE+2
          IF (LINE>LinesPerPage-NYRS-5) THEN
            CALL HEADIN()
            WRITE(6,'(9X,"Agricultural Data"/)')
            LINE=LINE+1
          END IF
          WRITE (6,1601) L,trim(AllTrim(PLAND(L)))
 1601     FORMAT (      /,25X,'Temperature for Land Area(',I2,') ',A/)
          WRITE (6,202) (VAR(I),I=1,14)
          LINE=LINE+3
          DO J=1,NYRS
            WRITE (6,"(6X,I4,12F9.1,F10.1)") IYEAR(J),(TEM(L,J,K),K=1,13)
            LINE=LINE+1
          END DO
          WRITE (6,"(/,6X,'MEAN',12F9.1,F10.1)") (AGTEMP(L,K),K=1,13)
          LINE=LINE+2
    END DO
      DO L=1,NRIP
          DO K=1,12
            SUM=0.0
            DO J=1,NYRS
              SUM=SUM+WETUSE(K,J,L)
            END DO
            RIPDEM(L,K)=SUM/FNYRS
            RIPDEM(L,13)=RIPDEM(L,13)+RIPDEM(L,K)
            SUM=0.0
            DO J=1,NYRS
              SUM=SUM+WETPRE(K,J,L)
            END DO
            RIPPRA(L,K)=SUM/FNYRS
            RIPPRA(L,13)=RIPPRA(L,13)+RIPPRA(L,K)
            SUM=0.0
            DO J=1,NYRS
              SUM=SUM+WTPRE(L,J,K)
            END DO
            RIPPRE(L,K)=SUM/FNYRS
            RIPPRE(L,13)=RIPPRE(L,13)+RIPPRE(L,K)
            SUM=0.0
            DO J=1,NYRS
              SUM=SUM+WTEM(L,J,K)
            END DO
            RIPTEM(L,K)=SUM/FNYRS
            RIPTEM(L,13)=RIPTEM(L,13)+RIPTEM(L,K)/12.0
          END DO
          CALL HEADIN()
          WRITE(6,'(9X,"Riparian Area Data"/)')
          LINE=LINE+2
          WRITE(6,"(/,25X,'Rainfall for Riparian Area(',I2,') ',A/)") &
            L,trim(AllTrim(RipHeading(L)))
 2401     FORMAT (/,25X,'Rainfall for Riparian Area(',I2,') ',A/)
          WRITE (6,202) (VAR(I),I=1,14)
          LINE=LINE+3
          DO J=1,NYRS
            WRITE (6,"(6X,I4,12F9.2,F10.2)") IYEAR(J),(WTPRE(L,J,K),K=1,13)
            LINE=LINE+1
          END DO
          WRITE (6,"(/,6X,'MEAN',12F9.2,F10.2)") (RIPPRE(L,K),K=1,13)
          LINE=LINE+2
          IF (LINE.GT.LinesPerPage/2) THEN
            CALL HEADIN()
            WRITE(6,'(9X,"Riparian Area Data"/)')
            LINE=LINE+1
          END IF
          IF (LINE.GT.LinesPerPage/2) THEN
            CALL HEADIN()
            WRITE(6,'(9X,"Riparian Area Data"/)')
            LINE=LINE+1
          END IF
          WRITE (6,2501) L,trim(AllTrim(RipHeading(L)))
 2501     FORMAT (/,25X,'Temperature for Riparian Area(',I2,') ',A/)
          WRITE (6,202) (VAR(I),I=1,14)
          LINE=LINE+3
          DO J=1,NYRS
            WRITE (6,"(6X,I4,12F9.1,F10.1)") IYEAR(J),(WTEM(L,J,K),K=1,13)
            LINE=LINE+1
          END DO
          WRITE (6,"(/,6X,'MEAN',12F9.1,F10.1)") (RIPTEM(L,K),K=1,13)
          LINE=LINE+2
          IF (LINE.GT.LinesPerPage/2) THEN
            CALL HEADIN()
            WRITE(6,'(9X,"Riparian Area Data"/)')
            LINE=LINE+1
          END IF
          WRITE (6,2301) L,trim(AllTrim(RipHeading(L)))
 2301     FORMAT (      /,25X,'Consumptive use for Riparian Area(',I2,') ',A/)
          WRITE (6,202) (VAR(I),I=1,14)
          LINE=LINE+3
          DO J=1,NYRS
            WRITE (6,302) IYEAR(J),(WETUSE(K,J,L),K=1,13)
            LINE=LINE+1
          END DO
          WRITE (6,303) (RIPDEM(L,K),K=1,13)
          LINE=LINE+2
      END DO
      DO L=1,NMUN
        DO K=1,12
          SUM=0.0
          DO J=1,NYRS
            SUM=SUM+MunGW(L,J,K)
          END DO
          MGW(L,K)=SUM/FNYRS
          MGW(L,13)=MGW(L,13)+MGW(L,K)
          SUM=0.0
          DO J=1,NYRS
            SUM=SUM+MunSurf(L,J,K)
          END DO
          MDiv(L,K)=SUM/FNYRS
          MDiv(L,13)=MDiv(L,13)+MDiv(L,K)
          SUM=0.0
          DO J=1,NYRS
            SUM=SUM+MunGW(L,J,K)+MunSurf(L,J,K)
          END DO
          MDem(L,K)=SUM/FNYRS
          MDem(L,13)=MDem(L,13)+MDem(L,K)
          SUM=0.0
          DO J=1,NYRS
            SUM=SUM+Secondary(L,J,K)
          END DO
          MSec(L,K)=SUM/FNYRS
          MSec(L,13)=MSec(L,13)+MSec(L,K)
        END DO
        CALL HEADIN()
        Write(6,"(10X,'Municipal Data'/)")
        LINE=LINE+2
        WRITE(6,"(/25X,'Groundwater Pumping for Municipal Area',2X,A)") &
          Trim(AllTrim(MunGroup(L)))
        WRITE(6,202)(VAR(I),I=1,14)
        LINE=LINE+3
        DO J=1,NYRS
          MunGW(L,J,13)=0.0
          DO K=1,12
            MunGW(L,J,13)=MunGW(L,J,13)+MunGW(L,J,K)
          END DO
          WRITE(6,"(6X,I4,12F9.1,F10.1)")IYEAR(J),(MunGW(L,J,K),K=1,13)
          LINE=LINE+1
        END DO
        WRITE (6,"(/,6X,'MEAN',12F9.1,F10.1)") (MGW(L,K),K=1,13)
        LINE=LINE+2
        IF (LINE>LinesPerPage-NYRS-5) THEN
          CALL HEADIN()
          Write(6,"(10X,'Municipal Data'/)")
          LINE=LINE+1
        END IF
        WRITE(6,"(/25X,'Surface Diversions for Municipal Area',2X,A)") &
          Trim(AllTrim(MunGroup(L)))
        WRITE(6,202)(VAR(I),I=1,14)
        LINE=LINE+3
        DO J=1,NYRS
          MunSurf(L,J,13)=0.0
          DO K=1,12
            MunSurf(L,J,13)=MunSurf(L,J,13)+MunSurf(L,J,K)
          END DO
          WRITE(6,"(6X,I4,12F9.1,F10.1)")IYEAR(J),(MunSurf(L,J,K),K=1,13)
          LINE=LINE+1
        END DO
        WRITE (6,"(/,6X,'MEAN',12F9.1,F10.1)") (MDiv(L,K),K=1,13)
        LINE=LINE+2
        IF (LINE>LinesPerPage-NYRS-5) THEN
          CALL HEADIN()
          Write(6,"(10X,'Municipal Data'/)")
          LINE=LINE+2
        END IF
        WRITE(6,"(/25X,'Secondary Water Use for Municipal Area',2X,A)") &
          Trim(AllTrim(MunGroup(L)))
        WRITE(6,202)(VAR(I),I=1,14)
        LINE=LINE+3
        DO J=1,NYRS
          Secondary(L,J,13)=0.0
          DO K=1,12
            Secondary(L,J,13)=Secondary(L,J,13)+Secondary(L,J,K)
          END DO
          WRITE(6,"(6X,I4,12F9.1,F10.1)")IYEAR(J),(Secondary(L,J,K),K=1,13)
          LINE=LINE+1
        END DO
        WRITE (6,"(/,6X,'MEAN',12F9.1,F10.1)") (MSec(L,K),K=1,13)
        LINE=LINE+2
        IF (LINE>LinesPerPage-NYRS-5) THEN
          CALL HEADIN()
          Write(6,"(10X,'Municipal Data'/)")
          LINE=LINE+1
        END IF
        WRITE(6,"(/25X,'Total (GW & Surface) Diversions for " // &
          "Municipal Area ',2X,A)") Trim(AllTrim(MunGroup(L)))
        WRITE(6,202)(VAR(I),I=1,14)
        LINE=LINE+3
        DO J=1,NYRS
          WRITE(6,"(6X,I4,12F9.1,F10.1)")IYEAR(J),((MunSurf(L,J,K)+MunGW(L,J,K)),K=1,13)
          LINE=LINE+1
        END DO
        WRITE (6,"(/,6X,'MEAN',12F9.1,F10.1)") (MDem(L,K),K=1,13)
        LINE=LINE+2
    404 FORMAT (5X,'TOTAL',12F9.0,F10.0)
    !  PRINT MUNICIPAL POTABLE DEPLETIONS
        IF (LINE>LinesPerPage-NYRS-5) THEN
          CALL HEADIN()
          Write(6,"(10X,'Municipal Data'/)")
          LINE=LINE+2
        END IF
        WRITE(6,"(/25X,'Municipal Potable Depletions'" // &
          ",2X,A)")
        LINE=LINE+5
        WRITE(6,202)(VAR(I),I=1,14)
        DepPot=0.0
        DO J=1,NYRS
          MunDepPot(L,J,13)=0.0
          DO K=1,12
            DepPot(K)=DepPot(K)+MunDepPot(L,J,K)/FNYRS
            DepPot(13)=DepPot(13)+MunDepPot(L,J,K)/FNYRS
            MunDepPot(L,J,13)=MunDepPot(L,J,13)+MunDepPot(L,J,K)
          END DO
          WRITE(6,"(6X,I4,12F9.1,F10.1)")IYEAR(J),(MunDepPot(L,J,K),K=1,13)
          LINE=LINE+1
        END DO
        WRITE (6,"(/,6X,'MEAN',12F9.1,F10.1)") (DepPot(K),K=1,13)
        LINE=LINE+2
    !   PRINT MUNICIPAL SECONDARY DEPLETIONS
        IF (LINE>LinesPerPage-NYRS-5) THEN
          CALL HEADIN()
          Write(6,"(10X,'Municipal Data'/)")
          LINE=LINE+2
        END IF
        WRITE(6,"(/25X,'Municipal Secondary Depletions'" // &
          ",2X,A)")
        LINE=LINE+5
        DepSec=0.0
        DO J=1,NYRS
          MunDepSec(L,J,13)=0.0
          DO K=1,12
            DepSec(K)=DepSec(K)+MunDepSec(L,J,K)/FNYRS
            DepSec(13)=DepSec(13)+MunDepSec(L,J,K)/FNYRS
            MunDepSec(L,J,13)=MunDepSec(L,J,13)+MunDepSec(L,J,K)
          END DO
          WRITE(6,"(6X,I4,12F9.1,F10.1)")IYEAR(J),(MunDepSec(L,J,K),K=1,13)
          LINE=LINE+1
        END DO
        WRITE (6,"(/,6X,'MEAN',12F9.1,F10.1)") (DepSec(K),K=1,13)
        LINE=LINE+2
        IF (SSIDepM(L,13).GT.0.1) THEN
          IF (LINE>LinesPerPage-NYRS-5) THEN
            CALL HEADIN()
            Write(6,"(10X,'Municipal Data'/)")
            LINE=LINE+2
          END IF
          WRITE(6,"(/25X,'Municipal Self-supplied Industry Depletions'" // &
            ",2X,A)")
          LINE=LINE+5
          DO J=1,NYRS
            WRITE(6,"(6X,I4,12F9.1,F10.1)")IYEAR(J),(SSIDep(L,J,K),K=1,13)
            LINE=LINE+1
          END DO
          WRITE (6,"(/,6X,'MEAN',12F9.1,F10.1)") (SSIDepM(L,K),K=1,13)
          LINE=LINE+2
        END IF
      END DO
!
!     PRINT OUT STREAM FLOWS QX(L,J,K)
!
!        COMPUTE MEANS
      FNYRS=NYRS
      DO M=1,NQX
        L=IPQX(M)
        DO J=1,NYRS
          SUM=0.0
          DO K=1,12
            SUM=SUM+QX(L,J,K)
          END DO
          QX(L,J,13)=SUM
        END DO
        DO K=1,13
          SUM=0.0
          DO J=1,NYRS
            SUM=SUM+QX(L,J,K)
          END DO
          QXM(L,K)=SUM/FNYRS
        END DO
      END DO
!      PRINT SELECTED QX'S
      DO M=1,NQX
        L=IPQX(M)
        LL=QxMap(L)
        IF (L.GT.0)THEN
          IF (LINE>LinesPerPage-NYRS-5.OR.M.EQ.1) THEN
            CALL HEADIN
          END IF
          QXNAM(LL)=LeftAdj(QXNAM(LL),75)
          WRITE (6,"(/,25X,'QX(',I3,')  FLOW ACRE-FT',2X,A/)") L,QXNAM(LL)
          WRITE (6,"(6X,A4,12(4X,A4,1X),5X,A4/)") (VAR(I),I=1,14)
          LINE=LINE+5
          DO J=1,NYRS
            WRITE (6,"(6X,I4,12F9.0,F10.0)") IYEAR(J),(QX(L,J,K),K=1,13)
            LINE=LINE+1
          END DO
          WRITE (6,"(/,6X,'MEAN',12F9.0,F10.0)") (QXM(L,K),K=1,13)
          LINE=LINE+2
        END IF
      END DO
  508 CONTINUE
  303 FORMAT (/,6X,'MEAN',12F9.0,F10.0)
  202 FORMAT(6X,A4,12(4X,A4,1X),5X,A4/)
  301 FORMAT (/,25X,'QX(',I2,')  FLOW ACRE-FT',2X,A/)
  302 FORMAT (6X,I4,12F9.0,F10.0)
!     SHORTAGE SUMMARY
      DO 515 L=1,NLND
      DO 512 J=1,NYRS
      SUM=0.0
      DO 511 K=1,12
  511 SUM=SUM+SHORT(L,J,K)
  512 SHORT(L,J,13)=SUM
      DO 515 K=1,13
      SUM=0.0
      DO 514 J=1,NYRS
  514 SUM=SUM+SHORT(L,J,K)
  515 SHORTM(L,K)=SUM/FNYRS
!     SHORTAGE SUMMARY-ANNUAL PERCENT
      DO 794 L=1,NLND
      QDVR(L,13)=0.
      DO 794 K=1,12
      QDVR(L,13)=QDVR(L,13)+QDVR(L,K)
  794 CONTINUE
      DO 796 L=1,NLND
      IF (QDVR(L,13).LE.0.0) QDVR(L,13)=1.0
      DO 796 J=1,NYRS
  796 SRTPC(L,J)=100.*SHORT(L,J,13)/(QDVR(L,13)+RTUSE(L,13)+ &
       GWUSE(L,13)+SBUSE(L,13))
      DO 798 L=1,NLND
      SUM=0.0
      DO 797 J=1,NYRS
  797 SUM=SUM+SRTPC(L,J)
  798 SRTPCM(L)=SUM/FNYRS
!**********************************************************************
!     Calculate Monthly and annual depletion
!     Calculate yield
!**********************************************************************
  AgDeplete=0.0
  AgDepleteM=0.0
  AgDepletion=0.0
  AgDepletionM=0.0
  AgGW=0.0
  AgGWM=0.0
  AgSurface=0.0
  AgSurfaceM=0.0
  AgTotal=0.0
  AgTotalM=0.0
  AnnYield=0.0
  MandIDep=0.0
  MandIDepM=0.0
  MandIGW=0.0
  MandIGWM=0.0
  MandISurface=0.0
  MandISurfaceM=0.0
  MandITotal=0.0
  MandITotalM=0.0
  MonYield=0.0
  ReserEvap=0.0
  ReserEvapM=0.0
  StudyDeplete=0.0
  SubExport=0.0
  SubExportM=0.0
  BasinIn=0.0
  BasinInM=0.0
  BasinOut=0.0
  BasinOutM=0.0
  SubImport=0.0
  SubImportM=0.0
  SubInflow=0.0
  SubInflowM=0.0
  SubOutflow=0.0
  SubOutflowM=0.0
  SubTranImp=0.0
  SubTranImpM=0.0
  SubTranExpo=0.0
  SubTranExpoM=0.0
  SubResStDif=0.0
  SubResStDifM=0.0
  SubResEvap=0.0
  SubResEvapM=0.0
  SubSupply=0.0
  SubSupplyM=0.0
  SubSurface=0.0
  SubSurfaceM=0.0
  SubSurfDiv=0.0
  SubSurfDivM=0.0
  TotGWUse=0.0
  TotGWUseM=0.0
  TotInflow=0.0
  WetDep=0.0
  WetDepM=0.0
  TermOutEvap=0.0
  TermOutEvapM=0.0
  QXused=.FALSE.
  !Find unique QX numbers for each land area and M&I service area
  !This is needed because often a QX number is reused to link to a
  !series of reservoirs
  UniqAgDivQX=0
  UniqMIDiv=0
  !The following are the number of unique AG and M&I QX numbers
  NumAGQX=0
  NumMIQX=0
  DO L=1,NLND
    DO M=1,NQLIN(L)
      IF (M.EQ.1) THEN
        NumAGQX(L)=1
        UniqAgDivQX(L,NumAGQX(L))=IDV(L,M)
      ELSE
        isFound=.FALSE.
        DO LL=1,NumAGQX(L)
          IF (UniqAgDivQX(L,LL).EQ.IDV(L,M)) THEN
            isFound=.TRUE.
            EXIT
          END IF
        END DO
        IF (.NOT.isFound) THEN
          NumAGQX(L)=NumAGQX(L)+1
          UniqAgDivQX(L,NumAGQX(L))=IDV(L,M)
        END IF
      END IF
    END DO
  END DO
  DO L=1,NMUN
    DO M=1,NQXMun(L)
      IF (M.EQ.1) THEN
        NumMIQX(L)=1
        UniqMIDiv(L,NumMIQX(L))=MunQXin(L,M)
      ELSE
        isFound=.FALSE.
        DO LL=1,NumMIQX(L)
          IF (UniqMIDiv(L,LL).EQ.MunQXin(L,M)) THEN
            isFound=.TRUE.
            EXIT
          END IF
        END DO
        IF (.NOT.isFound) THEN
          NumMIQX(L)=NumMIQX(L)+1
          UniqMIDiv(L,NumMIQX(L))=MunQXin(L,M)
        END IF
      END IF
    END DO
  END DO
  DO J=1,NYRS
    YIELD=0.0
    DO K=1,12
      !Subtract all subarea inflows
      QXused=.FALSE.
      IF (NumMainStem.GT.0) THEN
        DO M=1,NumMainStem
          SubSupply(J,K)=SubSupply(J,K)+QX(IINFL(M),J,K)
          SubSupplyM(K)=SubSupplyM(K)+QX(IINFL(M),J,K)/FNYRS
          SubSurface(J,K)=SubSurface(J,K)+QX(IINFL(M),J,K)
          SubSurfaceM(K)=SubSurfaceM(K)+QX(IINFL(M),J,K)/FNYRS
          SubInflow(J,K)=SubInflow(J,K)+QX(IINFL(M),J,K)
          SubInflowM(K)=SubInflowM(K)+QX(IINFL(M),J,K)/FNYRS
          YIELD(K)=YIELD(K)-QX(IINFL(M),J,K)
          IF (J.EQ.NYRS.AND.K.EQ.12) THEN
            IF (QXused(IINFL(M))) THEN
              WRITE(6,"(10X,'MAINSTEM QX(',I3,') USED ELSEWHERE')")IINFL(M)
              WRITE(*,"(10X,'MAINSTEM QX(',I3,') USED ELSEWHERE')")IINFL(M)
            END IF
            QXused(IINFL(M))=.TRUE.
          END IF
        END DO
      END IF
      !Subtract basin Inflows
      IF (NUMBASIN.GT.0) THEN
        DO M=1,NUMBASIN
          SubSupply(J,K)=SubSupply(J,K)+QX(IBASIN(M),J,K)
          SubSupplyM(K)=SubSupplyM(K)+QX(IBASIN(M),J,K)/FNYRS
          SubSurface(J,K)=SubSurface(J,K)+QX(IBASIN(M),J,K)
          SubSurfaceM(K)=SubSurfaceM(K)+QX(IBASIN(M),J,K)/FNYRS
          BasinIn(J,K)=BasinIn(J,K)+QX(IBASIN(M),J,K)
          BasinInM(K)=BasinInM(K)+QX(IBASIN(M),J,K)/FNYRS
          YIELD(K)=YIELD(K)-QX(IBASIN(M),J,K)
          IF (J.EQ.NYRS.AND.K.EQ.12) THEN
            IF (QXused(IBASIN(M))) THEN
              WRITE(6,"(10X,'Basin Inflow QX(',I3,') USED ELSEWHERE')")IBASIN(M)
              WRITE(*,"(10X,'Basin Inflow QX(',I3,') USED ELSEWHERE')")IBASIN(M)
            END IF
            QXused(IBASIN(M))=.TRUE.
          END IF
        END DO
      END IF
      !Add basin outflows
      IF (NUMBASOUT.GT.0) THEN
        DO M=1,NUMBASOUT
          SubSupply(J,K)=SubSupply(J,K)-QX(IBASOUT(M),J,K)
          SubSupplyM(K)=SubSupplyM(K)-QX(IBASOUT(M),J,K)/FNYRS
          SubSurface(J,K)=SubSurface(J,K)-QX(IBASOUT(M),J,K)
          SubSurfaceM(K)=SubSurfaceM(K)-QX(IBASOUT(M),J,K)/FNYRS
          BasinOut(J,K)=BasinOut(J,K)+QX(IBASOUT(M),J,K)
          BasinOutM(K)=BasinOutM(K)+QX(IBASOUT(M),J,K)/FNYRS
          YIELD(K)=YIELD(K)+QX(IBASOUT(M),J,K)
          IF (J.EQ.NYRS.AND.K.EQ.12) THEN
            IF (QXused(IBASOUT(M))) THEN
              WRITE(6,"(10X,'Basin Outflow QX(',I3,') USED ELSEWHERE')")IBASOUT(M)
              WRITE(*,"(10X,'Basin Outflow QX(',I3,') USED ELSEWHERE')")IBASOUT(M)
            END IF
            QXused(IBASOUT(M))=.TRUE.
          END IF
        END DO
      END IF
      !Subtract imports
      IF (NumImport.GT.0) THEN
        DO M=1,NumImport
          SubSupply(J,K)=SubSupply(J,K)+QX(IIMP(M),J,K)
          SubSupplyM(K)=SubSupplyM(K)+QX(IIMP(M),J,K)/FNYRS
          !Added 4/5/2012
          SubImport(J,K)=SubImport(J,K)+QX(IIMP(M),J,K)
          SubImportM(K)=SubImportM(K)+QX(IIMP(M),J,K)/FNYRS
          !Commented out 4/5/2012
          !SubInflow(J,K)=SubInflow(J,K)+QX(IIMP(M),J,K)
          !SubInflowM(K)=SubInflowM(K)+QX(IIMP(M),J,K)/FNYRS
          SubSurface(J,K)=SubSurface(J,K)+QX(IIMP(M),J,K)
          SubSurfaceM(K)=SubSurfaceM(K)+QX(IIMP(M),J,K)/FNYRS
          YIELD(K)=YIELD(K)-QX(IIMP(M),J,K)
          IF (J.EQ.NYRS.AND.K.EQ.12) THEN
            IF (QXused(IIMP(M))) THEN
              WRITE(6,"(10X,'IMPORT QX(',I3,') USED ELSEWHERE')")IIMP(M)
              WRITE(*,"(10X,'IMPORT QX(',I3,') USED ELSEWHERE')")IIMP(M)
            END IF
            QXused(IIMP(M))=.TRUE.
          END IF
        END DO
      END IF
      !Add Exports
      IF (NumExport.GT.0) THEN
        DO M=1,NumExport
          SubSupply(J,K)=SubSupply(J,K)-QX(IEXPO(M),J,K)
          SubSupplyM(K)=SubSupplyM(K)-QX(IEXPO(M),J,K)/FNYRS
          SubExport(J,K)=SubExport(J,K)+QX(IEXPO(M),J,K)
          SubExportM(K)=SubExportM(K)+QX(IEXPO(M),J,K)/FNYRS
          !SubOutflow(J,K)=SubOutflow(J,K)+QX(IEXPO(M),J,K)
          !SubOutflowM(K)=SubOutflowM(K)+QX(IEXPO(M),J,K)/FNYRS
          SubSurface(J,K)=SubSurface(J,K)-QX(IEXPO(M),J,K)
          SubSurfaceM(K)=SubSurfaceM(K)-QX(IEXPO(M),J,K)/FNYRS
          YIELD(K)=YIELD(K)+QX(IEXPO(M),J,K)
          IF (J.EQ.NYRS.AND.K.EQ.12) THEN
            IF (QXused(IEXPO(M))) THEN
              WRITE(6,"(10X,'EXPORT QX(',I3,') USED ELSEWHERE')")IEXPO(M)
              WRITE(*,"(10X,'EXPORT QX(',I3,') USED ELSEWHERE')")IEXPO(M)
            END IF
            QXused(IEXPO(M))=.TRUE.
          END IF
        END DO
      END IF
      !Account for Transbasin Imports and Exports
      IF (TranNum.GT.0) THEN
        DO M=1,TranNum
          IF (Trim(TranDir(M)).EQ.'Import') THEN
            SubSupply(J,K)=SubSupply(J,K)+QX(TranQX(M),J,K)
            SubSupplyM(K)=SubSupplyM(K)+QX(TranQX(M),J,K)/FNYRS
            SubSurface(J,K)=SubSurface(J,K)+QX(TranQX(M),J,K)
            SubSurfaceM(K)=SubSurfaceM(K)+QX(TranQX(M),J,K)/FNYRS
            SubTranImp(J,K)=SubTranImp(J,K)+QX(TranQX(M),J,K)
            SubTranImpM(K)=SubTranImpM(K)+QX(TranQX(M),J,K)/FNYRS
            YIELD(K)=YIELD(K)-QX(TranQX(M),J,K)
          ELSEIF (Trim(TranDir(M)).EQ.'Export') THEN
            SubSupply(J,K)=SubSupply(J,K)-QX(TranQX(M),J,K)
            SubSupplyM(K)=SubSupplyM(K)-QX(TranQX(M),J,K)/FNYRS
            SubSurface(J,K)=SubSurface(J,K)-QX(TranQX(M),J,K)
            SubSurfaceM(K)=SubSurfaceM(K)-QX(TranQX(M),J,K)/FNYRS
            SubTranExpo(J,K)=SubTranExpo(J,K)+QX(TranQX(M),J,K)
            SubTranExpoM(K)=SubTranExpoM(K)+QX(TranQX(M),J,K)/FNYRS
            YIELD(K)=YIELD(K)+QX(TranQX(M),J,K)
          END IF
          IF (J.EQ.NYRS.AND.K.EQ.12) THEN
            IF (QXused(TranQX(M))) THEN
              WRITE(6,"(10X,'Trans Basin QX(',I3,') USED ELSEWHERE')")TranQX(M)
              WRITE(*,"(10X,'Trans Basin QX(',I3,') USED ELSEWHERE')")TranQX(M)
            END IF
            QXused(TranQX(M))=.TRUE.
          END IF
        END DO
      END IF
      !Add Basin Outflows
      IF (NumOutflow.GT.0) THEN
        DO M=1,NumOutflow
          YIELD(K)=YIELD(K)+QX(NOUFL(M),J,K)
          SubOutflow(J,K)=SubOutflow(J,K)+QX(NOUFL(M),J,K)
          SubOutflowM(K)=SubOutflowM(K)+QX(NOUFL(M),J,K)/FNYRS
          IF (J.EQ.NYRS.AND.K.EQ.12) THEN
            IF (QXused(NOUFL(M))) THEN
              WRITE(6,"(10X,'OUTFLOW QX(',I3,') USED ELSEWHERE')")NOUFL(M)
              WRITE(*,"(10X,'OUTFLOW QX(',I3,') USED ELSEWHERE')")NOUFL(M)
            END IF
            QXused(NOUFL(M))=.TRUE.
          END IF
        END DO
      END IF
      !Add Outflows that go to evaporation
      IF (NumOutEvap.GT.0.0) THEN
        DO M=1,NumOutEvap
          TermOutEvap(J,K)=TermOutEvap(J,K)+QX(NOUEVFL(M),J,K)
          TermOutEvapM(K)=TermOutEvapM(K)+QX(NOUEVFL(M),J,K)/FNYRS
          YIELD(K)=YIELD(K)+QX(NOUEVFL(M),J,K)
          StudyDeplete(J,K)=StudyDeplete(J,K)+QX(NOUEVFL(M),J,K)
        END DO
      END IF
      !Add Tributary Flows
      IF (NTRIB.GT.0) THEN
        DO M=1,NTRIB
          SubSupply(J,K)=SubSupply(J,K)+QX(ITRIB(M),J,K)
          SubSupplyM(K)=SubSupplyM(K)+QX(ITRIB(M),J,K)/FNYRS
          SubSurface(J,K)=SubSurface(J,K)-QX(ITRIB(M),J,K)
          SubSurfaceM(K)=SubSurfaceM(K)-QX(ITRIB(M),J,K)/FNYRS
        END DO
      END IF
      !Add Depletions from agriculture
      DO L=1,NLND
        !Add diversions to land area
        DO M=1,NumAGQX(L)
          YIELD(K)=YIELD(K)+QX(UniqAgDivQX(L,M),J,K)
          AgDeplete(L,J,K)=AgDeplete(L,J,K)+QX(UniqAgDivQX(L,M),J,K)
          AgDepleteM(L,K)=AgDepleteM(L,K)+QX(UniqAgDivQX(L,M),J,K)/FNYRS
          AgDepletion(J,K)=AgDepletion(J,K)+QX(UniqAgDivQX(L,M),J,K)
          AgDepletionM(K)=AgDepletionM(K)+QX(UniqAgDivQX(L,M),J,K)/FNYRS
          AgSurface(J,K)=AgSurface(J,K)+QX(UniqAgDivQX(L,M),J,K)
          SubSurfDiv(J,K)=SubSurfDiv(J,K)+QX(UniqAgDivQX(L,M),J,K)
          SubSurfDivM(K)=SubSurfDivM(K)+QX(UniqAgDivQX(L,M),J,K)/FNYRS
          AgSurfaceM(K)=AgSurfaceM(K)+QX(UniqAgDivQX(L,M),J,K)/FNYRS
          AgTotal(J,K)=AgTotal(J,K)+QX(UniqAgDivQX(L,M),J,K)
          AgTotalM(K)=AgTotalM(K)+QX(UniqAgDivQX(L,M),J,K)/FNYRS
          StudyDeplete(J,K)=StudyDeplete(J,K)+QX(UniqAgDivQX(L,M),J,K)
          IF (J.EQ.NYRS.AND.K.EQ.12) THEN
            IF (QXused(UniqAgDivQX(L,M))) THEN
              IF (M.EQ.1) THEN
                WRITE(6,"(10X,'INFLOW QX(',I3,') USED ELSEWHERE')") &
                  UniqAgDivQX(L,M)
                WRITE(*,"(10X,'INFLOW QX(',I3,') USED ELSEWHERE')") &
                  UniqAgDivQX(L,M)
              END IF
              QXused(UniqAgDivQX(L,M))=.TRUE.
            END IF
          END IF
        END DO
        !Add subirrigation depletions
        IF (IRV(L).GT.0) THEN
          YIELD(K)=YIELD(K)+QX(IRV(L),J,K)
          AgDeplete(L,J,K)=AgDeplete(L,J,K)+QX(IRV(L),J,K)
          AgDepleteM(L,K)=AgDepleteM(L,K)+QX(IRV(L),J,K)/FNYRS
          AgDepletion(J,K)=AgDepletion(J,K)+QX(IRV(L),J,K)
          AgDepletionM(K)=AgDepletionM(K)+QX(IRV(L),J,K)/FNYRS
          AgSurface(J,K)=AgSurface(J,K)+QX(IRV(L),J,K)
          AgSurfaceM(K)=AgSurfaceM(K)+QX(IRV(L),J,K)/FNYRS
          SubSurfDiv(J,K)=SubSurfDiv(J,K)+QX(IRV(L),J,K)
          SubSurfDivM(K)=SubSurfDivM(K)+QX(IRV(L),J,K)/FNYRS
          AgTotal(J,K)=AgTotal(J,K)+QX(IRV(L),J,K)
          AgTotalM(K)=AgTotalM(K)+QX(IRV(L),J,K)/FNYRS
          StudyDeplete(J,K)=StudyDeplete(J,K)+QX(IRV(L),J,K)
          IF (J.EQ.NYRS.AND.K.EQ.12) THEN
            IF (QXused(IRV(L))) THEN
              WRITE(6,"(10X,'SUBIRRIGATION QX(',I3,') USED ELSEWHERE')")IRV(L)
              WRITE(*,"(10X,'SUBIRRIGATION QX(',I3,') USED ELSEWHERE')")IRV(L)
            END IF
            QXused(IRV(L))=.TRUE.
          END IF
        END IF
        !Add groundwater pumping
        IF (GWQX(L).GT.0) THEN
          YIELD(K)=YIELD(K)+QX(GWQX(L),J,K)
          AgDeplete(L,J,K)=AgDeplete(L,J,K)+QX(GWQX(L),J,K)
          AgDepleteM(L,K)=AgDepleteM(L,K)+QX(GWQX(L),J,K)/FNYRS
          AgDepletion(J,K)=AgDepletion(J,K)+QX(GWQX(L),J,K)
          AgDepletionM(K)=AgDepletionM(K)+QX(GWQX(L),J,K)/FNYRS
          AgGW(J,K)=AgGW(J,K)+QX(GWQX(L),J,K)
          AgGWM(K)=AgGWM(K)+QX(GWQX(L),J,K)/FNYRS
          TotGWUse(J,K)=TotGWUse(J,K)+QX(GWQX(L),J,K)
          TotGWUseM(K)=TotGWUseM(K)+QX(GWQX(L),J,K)/FNYRS
          AgTotal(J,K)=AgTotal(J,K)+QX(GWQX(L),J,K)
          AgTotalM(K)=AgTotalM(K)+QX(GWQX(L),J,K)/FNYRS
          SubSupply(J,K)=SubSupply(J,K)+QX(GWQX(L),J,K)
          SubSupplyM(K)=SubSupplyM(K)+QX(GWQX(L),J,K)/FNYRS
          StudyDeplete(J,K)=StudyDeplete(J,K)+QX(GWQX(L),J,K)
          IF (J.EQ.NYRS.AND.K.EQ.12) THEN
            IF (QXused(GWQX(L))) THEN
              WRITE(6,"(10X,'GW PUMPING QX(',I3,') USED ELSEWHERE')")GWQX(L)
              WRITE(*,"(10X,'GW PUMPING QX(',I3,') USED ELSEWHERE')")GWQX(L)
            END IF
            QXused(GWQX(L))=.TRUE.
          END IF
        END IF
        !Add excess precipitation
        YIELD(K)=YIELD(K)+PERCO(K,J,L)+WPERC(K,J,L)
        AgDeplete(L,J,K)=AgDeplete(L,J,K)+PERCO(K,J,L)+WPERC(K,J,L)
        AgDepleteM(L,K)=AgDepleteM(L,K)+(PERCO(K,J,L)+WPERC(K,J,L))/FNYRS
        AgDepletion(J,K)=AgDepletion(J,K)+PERCO(K,J,L)+WPERC(K,J,L)
        AgDepletionM(K)=AgDepletionM(K)+(PERCO(K,J,L)+WPERC(K,J,L))/FNYRS
        StudyDeplete(J,K)=StudyDeplete(J,K)+PERCO(K,J,L)+WPERC(K,J,L)
        !Subtract return flow
        YIELD(K)=YIELD(K)-QX(IRT(L),J,K)
        AgDeplete(L,J,K)=AgDeplete(L,J,K)-QX(IRT(L),J,K)
        AgDepleteM(L,K)=AgDepleteM(L,K)+QX(IRT(L),J,K)/FNYRS
        AgDepletion(J,K)=AgDepletion(J,K)-QX(IRT(L),J,K)
        AgDepletionM(K)=AgDepletionM(K)-QX(IRT(L),J,K)/FNYRS
        StudyDeplete(J,K)=StudyDeplete(J,K)-QX(IRT(L),J,K)
        IF (J.EQ.NYRS.AND.K.EQ.12) THEN
          IF (QXused(IRT(L))) THEN
            WRITE(6,"(10X,'RETURN FLOW QX(',I3,') USED ELSEWHERE')")IRT(L)
            WRITE(*,"(10X,'RETURN FLOW QX(',I3,') USED ELSEWHERE')")IRT(L)
          END IF
          QXused(IRT(L))=.TRUE.
        END IF
      END DO
      !Add depletions from reservoirs
      DO M=1,NRES
        IF (K==1) THEN
          IF (J==1) THEN
            SubSupply(J,K)=SubSupply(J,K)+STOIC(M)-ST(M,J,K)
            SubSupplyM(K)=SubSupplyM(K)+(STOIC(M)-ST(M,J,K))/FNYRS
            SubResStDif(J,K)=SubResStDif(J,K)-(STOIC(M)-ST(M,J,K))
            SubResStDifM(K)=SubResStDifM(K)-(STOIC(M)-ST(M,J,K))/FNYRS
            YIELD(K)=YIELD(K)-STOIC(M)+ST(M,J,K)
          ELSE
            SubSupply(J,K)=SubSupply(J,K)+ST(M,J-1,12)-ST(M,J,K)
            SubSupplyM(K)=SubSupplyM(K)+(ST(M,J-1,12)-ST(M,J,K))/FNYRS
            SubResStDif(J,K)=SubResStDif(J,K)-(ST(M,J-1,12)-ST(M,J,K))
            SubResStDifM(K)=SubResStDifM(K)-(ST(M,J-1,12)-ST(M,J,K))/FNYRS
            YIELD(K)=YIELD(K)-ST(M,J-1,12)+ST(M,J,K)
          END IF
        ELSE
          SubSupply(J,K)=SubSupply(J,K)+ST(M,J,K-1)-ST(M,J,K)
          SubSupplyM(K)=SubSupplyM(K)+(ST(M,J,K-1)-ST(M,J,K))/FNYRS
          SubResStDif(J,K)=SubResStDif(J,K)-(ST(M,J,K-1)-ST(M,J,K))
          SubResStDifM(K)=SubResStDifM(K)-(ST(M,J,K-1)-ST(M,J,K))/FNYRS
          YIELD(K)=YIELD(K)-ST(M,J,K-1)+ST(M,J,K)
        END IF
        SubSupply(J,K)=SubSupply(J,K)-EVAP(M,J,K)
        SubSupplyM(K)=SubSupplyM(K)+EVAP(M,J,K)/FNYRS
        ReserEvap(J,K)=ReserEvap(J,K)+EVAP(M,J,K)
        ReserEvapM(K)=ReserEvapM(K)+EVAP(M,J,K)/FNYRS
        SubResEvap(J,K)=SubResEvap(J,K)+EVAP(M,J,K)
        SubResEvapM(K)=SubResEvapM(K)+EVAP(M,J,K)/FNYRS
        YIELD(K)=YIELD(K)+EVAP(M,J,K)
        StudyDeplete(J,K)=StudyDeplete(J,K)+EVAP(M,J,K)
      END DO
      !Add depletions from man-influenced riparian
      DO M=1,NRIP
        IF (QXused(IPH(M))) THEN
          WRITE(6,"(10X,'RIPARIAN QX(',I3,') USED ELSEWHERE')")IPH(M)
          WRITE(*,"(10X,'RIPARIAN QX(',I3,') USED ELSEWHERE')")IPH(M)
        END IF
        WetDep(J,K)=WetDep(J,K)+MAX(0.0,QX(IPH(M),J,K))
        WetDepM(K)=WetDepM(K)+MAX(0.0,QX(IPH(M),J,K))/FNYRS
        YIELD(K)=YIELD(K)+MAX(0.0,QX(IPH(M),J,K))
        !StudyDeplete(J,K)=StudyDeplete(J,K)+QX(IPH(M),J,K)
        QXused(IPH(M))=.TRUE.
      END DO
      !Add depletions from M&I
      DO M=1,NMUN
        !Add surface diversions to municipal areas
        DO L=1,NumMIQX(M)
          MandISurface(J,K)=MandISurface(J,K)+QX(UniqMIDiv(M,L),J,K)
          MandISurfaceM(K)=MandISurfaceM(K)+QX(UniqMIDiv(M,L),J,K)/FNYRS
          MandITotal(J,K)=MandITotal(J,K)+QX(UniqMIDiv(M,L),J,K)
          MandITotalM(K)=MandITotalM(K)+QX(UniqMIDiv(M,L),J,K)/FNYRS
          SubSurfDiv(J,K)=SubSurfDiv(J,K)+QX(UniqMIDiv(M,L),J,K)
          SubSurfDivM(K)=SubSurfDivM(K)+QX(UniqMIDiv(M,L),J,K)/FNYRS
          YIELD(K)=YIELD(K)+QX(UniqMIDiv(M,L),J,K)
          StudyDeplete(J,K)=StudyDeplete(J,K)+QX(UniqMIDiv(M,L),J,K)
          MandIDep(J,K)=MandIDep(J,K)+QX(UniqMIDiv(M,L),J,K)
          MandIDepM(K)=MandIDepM(K)+QX(UniqMIDiv(M,L),J,K)/FNYRS
          IF (J.NE.NYRS.AND.K.NE.12) THEN
            IF (QXused(MunQXin(M,L))) THEN
              WRITE(6,"(10X,'M&I Surface QX(',I3,') USED ELSEWHERE')")MunQXin(M,L)
              WRITE(*,"(10X,'M&I Surface QX(',I3,') USED ELSEWHERE')")MunQXin(M,L)
            END IF
            QXused(MunQXin(M,L))=.TRUE.
          END IF
        END DO
        !Add groundwater pumping to municipal area
        YIELD(K)=YIELD(K)+QX(MunGWQx(M),J,K)
        MandIGW(J,K)=MandIGW(J,K)+QX(MunGWQx(M),J,K)
        MandIGWM(K)=MandIGWM(K)+QX(MunGWQx(M),J,K)/FNYRS
        SubSupply(J,K)=SubSupply(J,K)+QX(MunGWQx(M),J,K)
        SubSupplyM(K)=SubSupplyM(K)+QX(MunGWQx(M),J,K)/FNYRS
        MandITotal(J,K)=MandITotal(J,K)+QX(MunGWQx(M),J,K)
        MandITotalM(K)=MandITotalM(K)+QX(MunGWQx(M),J,K)/FNYRS
        TotGWUse(J,K)=TotGWUse(J,K)+QX(MunGWQx(M),J,K)
        TotGWUseM(K)=TotGWUseM(K)+QX(MunGWQx(M),J,K)/FNYRS
        StudyDeplete(J,K)=StudyDeplete(J,K)+QX(MunGWQx(M),J,K)
        !Subtract return flows from municipal areas
        YIELD(K)=YIELD(K)-QX(MunQXRet(M),J,K)
        StudyDeplete(J,K)=StudyDeplete(J,K)-QX(MunQXRet(M),J,K)
        MandIDep(J,K)=MandIDep(J,K)+QX(MunGWQx(M),J,K) &
          -QX(MunQXRet(M),J,K)
        MandIDepM(K)=MandIDepM(K)+(QX(MunGWQx(M),J,K) &
          -QX(MunQXRet(M),J,K))/FNYRS
      END DO
      !Add monthly yield to annual values
      YIELD(13)=YIELD(13)+YIELD(K)
      AnnYield(K)=AnnYield(K)+YIELD(K)/REAL(NYRS,KIND=8)
      AnnYield(13)=AnnYield(13)+YIELD(K)/REAL(NYRS,KIND=8)
    END DO
    DO K=1,12
      SubSupply(J,13)=SubSupply(J,13)+SubSupply(J,K)
      TermOutEvap(J,13)=TermOutEvap(J,13)+TermOutEvap(J,K)
      TotGWUse(J,13)=TotGWUse(J,13)+TotGWUse(J,K)
      SubInflow(J,13)=SubInflow(J,13)+SubInflow(J,K)
      BasinIn(J,13)=BasinIn(J,13)+BasinIn(J,K)
      BasinOut(J,13)=BasinOut(J,13)+BasinOut(J,K)
      SubSupply(J,13)=SubSupply(J,13)+SubSupply(J,K)
      SubOutflow(J,13)=SubOutflow(J,13)+SubOutflow(J,K)
      SubExport(J,13)=SubExport(J,13)+SubExport(J,K)
      SubImport(J,13)=SubImport(J,13)+SubImport(J,K)
      SubTranImp(J,13)=SubTranImp(J,13)+SubTranImp(J,K)
      SubTranExpo(J,13)=SubTranExpo(J,13)+SubTranExpo(J,13)
      SubSurfDiv(J,13)=SubSurfDiv(J,13)+SubSurfDiv(J,K)
      AgSurface(J,13)=AgSurface(J,13)+AgSurface(J,K)
      SubResStDif(J,13)=SubResStDif(J,13)+SubResStDif(J,K)
      SubResEvap(J,13)=SubResEvap(J,13)+SubResEvap(J,K)
      AgGW(J,13)=AgGW(J,13)+AgGW(J,K)
      AgTotal(J,13)=AgTotal(J,13)+AgTotal(J,K)
      AgDepletion(J,13)=AgDepletion(J,13)+AgDepletion(J,K)
      MandISurface(J,13)=MandISurface(J,13)+MandISurface(J,K)
      MandIGW(J,13)=MandIGW(J,13)+MandIGW(J,K)
      MandITotal(J,13)=MandITotal(J,13)+MandITotal(J,K)
      MandIDep(J,13)=MandIDep(J,13)+MandIDep(J,K)
      WetDep(J,13)=WetDep(J,13)+WetDep(J,K)
      ReserEvap(J,13)=ReserEvap(J,13)+ReserEvap(J,K)
    END DO
    DO K=1,13
      MonYield(J,K)=Yield(K)
    END DO
  END DO
  DO K=1,12
    SubSupplyM(13)=SubSupplyM(13)+SubSupplyM(K)
    TotGWUseM(13)=TotGWUseM(13)+TotGWUseM(K)
    SubInflowM(13)=SubInflowM(13)+SubInflowM(K)
    BasinInM(13)=BasinInM(13)+BasinInM(K)
    BasinOutM(13)=BasinOutM(13)+BasinOutM(K)
    SubSupplyM(13)=SubSupplyM(13)+SubSupplyM(K)
    SubSurfaceM(13)=SubSurfaceM(13)+SubSurfaceM(K)
    SubSurfDivM(13)=SubSurfDivM(13)+SubSurfDivM(K)
    SubOutflowM(13)=SubOutflowM(13)+SubOutflowM(K)
    SubExportM(13)=SubExportM(13)+SubExportM(K)
    SubImportM(13)=SubImportM(13)+SubImportM(K)
    SubTranImpM(13)=SubTranImpM(13)+SubTranImpM(K)
    SubTranExpoM(13)=SubTranExpoM(13)+SubTranExpoM(K)
    TermOutEvapM(13)=TermOutEvapM(13)+TermOutEvapM(K)
    SubResStDifM(13)=SubResStDifM(13)+SubResStDifM(K)
    SubResEvapM(13)=SubResEvapM(13)+SubResEvapM(K)
    AgSurfaceM(13)=AgSurfaceM(13)+AgSurfaceM(K)
    AgGWM(13)=AgGWM(13)+AgGWM(K)
    AgTotalM(13)=AgTotalM(13)+AgTotalM(K)
    AgDepletionM(13)=AgDepletionM(13)+AgDepletionM(K)
    MandISurfaceM(13)=MandISurfaceM(13)+MandISurfaceM(K)
    MandIGWM(13)=MandIGWM(13)+MandIGWM(K)
    MandITotalM(13)=MandITotalM(13)+MandITotalM(K)
    MandIDepM(13)=MandIDepM(13)+MandIDepM(K)
    WetDepM(13)=WetDepM(13)+WetDepM(K)
    ReserEvapM(13)=ReserEvapM(13)+ReserEvapM(K)
  END DO
  DO L=1,NLND
    AgDeplete(L,J,13)=0.0
    DO J=1,NYRS
      DO K=1,12
        AgDeplete(L,J,13)=AgDeplete(L,J,13)+AgDeplete(L,J,K)
        AgDepleteM(L,13)=AgDepleteM(L,13)+AgDeplete(L,J,K)/FNYRS
      END DO
    END DO
  END DO
!     PRINT OUT LAND AREA SHORTAGE
      !~~IF (.NOT.IPSH) GO TO 520
      DO L=1,NLND
         IF (LINE>LinesPerPage-NYRS-5.OR.L.EQ.1) THEN
          CALL HEADIN
         ENDIF
         WRITE (6,"(/,30X,A32,' SHORTAGE ACRE-FT', /)") JustRight(PLAND(L),32)
         WRITE (6,202) (VAR(I),I=1,14)
         LINE=LINE+5
         DO J=1,NYRS
            WRITE (6,302) IYEAR(J),(SHORT(L,J,K),K=1,13)
            LINE=LINE+1
         ENDDO
         WRITE (6,303) (SHORTM(L,K),K=1,13)
         LINE=LINE+2
      ENDDO
!     PRINT OUT LAND AREA DEPLETION
      DO L=1,NLND
         IF (LINE>LinesPerPage-NYRS-5.OR.L.EQ.1) THEN
           CALL HEADIN
         ENDIF
         WRITE (6,"(/,30X,A32,' DEPLETION ACRE-FT', /)")JustRight(PLAND(L),32)
         WRITE (6,202) (VAR(I),I=1,14)
         LINE=LINE+5
         DO J=1,NYRS
            WRITE (6,302) IYEAR(J),(AgDeplete(L,J,K),K=1,13)
            LINE=LINE+1
         ENDDO
         WRITE(6,303) (AgDepleteM(L,K),K=1,13)
         LINE=LINE+2
      ENDDO
  520 DO 528 L=1,NRES
      DO 522 J=1,NYRS
      SUM=0.0
      DO 521 K=1,12
  521 SUM=SUM+ST(L,J,K)
  522 ST(L,J,13)=SUM/12.0
      DO 525 K=1,13
      SUM=0.0
      DO 524 J=1,NYRS
  524 SUM=SUM+ST(L,J,K)
  525 STM(L,K)=SUM/FNYRS
  528 CONTINUE
!
      IF ((PrintAllResInt<>0.OR.PrintCapacityInt<>0).AND.NRES>0) THEN
!       PRINT OUT EOM STORAGE
        CALL HEADIN()
        DO L=1,NRES
!          BYPASS PRINT IF RESERVOIR CAPACITY ZERO
          IF(SMX(L).GE.1E-3)THEN
            IF (LINE>LinesPerPage-NYRS-5) THEN
              CALL HEADIN
            END IF
            WRITE (6,321) Trim(AllTrim(PRESV(L)))
  321       FORMAT (      /,30X,A32,' END OF MONTH STORAGE ACRE-FT', /)
            LINE=LINE+5
            WRITE (6,202) (VAR(I),I=1,14)
            DO J=1,NYRS
  526         WRITE (6,302) IYEAR(J),(ST(L,J,K),K=1,13)
              LINE=LINE+1
            END DO
            WRITE (6,303) (STM(L,K),K=1,13)
            LINE=LINE+2
        ENDIF
        END DO
      END IF

      IF (PrintAllResInt<>0.AND.NRES>0) THEN
        DO L=1,NRES
          DO  J=1,NYRS
            SUM=0.0
            DO K=1,12
              SUM=SUM+EVAP(L,J,K)
            END DO
            EVAP(L,J,13)=SUM
          END DO
          DO K=1,13
            SUM=0.0
            DO J=1,NYRS
              SUM=SUM+EVAP(L,J,K)
            END DO
            EVAPM(L,K)=SUM/FNYRS
          END DO
        END DO
      END IF
!
      IF ((PrintAllResInt<>0.OR.PrintStageInt<>0).AND.NRES>0) THEN
        CALL HEADIN()
  !     PRINT OUT EVAPORATION FROM RESERVOIR
        DO L=1,NRES
  !       BYPASS PRINT IF RESERVOIR CAPACITY ZERO
          IF(SMX(L).GE.1E-3) THEN
            IF (LINE>LinesPerPage-NYRS-5) THEN
             CALL HEADIN
            END IF
            WRITE (6,331) trim(AllTrim(PRESV(L)))
      331   FORMAT (      /,30X,A32,' EVAPORATION ACRE-FT', /)
            WRITE (6,202) (VAR(I),I=1,14)
            LINE=LINE+5
            DO J=1,NYRS
              WRITE (6,302) IYEAR(J),(EVAP(L,J,K),K=1,13)
              LINE=LINE+1
            END DO
            WRITE (6,303) (EVAPM(L,K),K=1,13)
            LINE=LINE+2
          END IF
        END DO
      END IF

  550 DO 558 L=1,NRES
      DO 552 J=1,NYRS
      SUM=0.0
      DO 551 K=1,12
  551 SUM=SUM+ELV(L,J,K)
  552 ELV(L,J,13)=SUM/12.0
      DO 555 K=1,13
      SUM=0.0
      DO 554 J=1,NYRS
  554 SUM=SUM+ELV(L,J,K)
  555 ELVM(L,K)=SUM/FNYRS
  558 CONTINUE
!     PRINT OUT EOM ELEVATION
      IF (IPEL.AND.NRES>0) THEN
        CALL HEADIN
        DO L=1,NRES
          IF(SMX(L).GT.1E-3) THEN
            IF (LINE>LinesPerPage-NYRS-5) THEN
              CALL HEADIN
            END IF
  !         BYPASS PRINT IF RESERVOIR CAPACITY ZERO
            WRITE (6,"(/,30X,A32,' END OF MONTH ELEVATION FEET',/)") &
              Trim(AllTrim(PRESV(L)))
            WRITE (6,202) (VAR(I),I=1,14)
            LINE=LINE+5
            DO J=1,NYRS
              WRITE (6,643) IYEAR(J),(ELV(L,J,K),K=1,13)
              LINE=LINE+1
            END DO
            WRITE (6,644) (ELVM(L,K),K=1,13)
            LINE=LINE+2
          END IF
        END DO
      END IF
!
      DO L=1,NRES
        DO J=1,NYRS
        SUM=0.0
        DO K=1,12
          SUM=SUM+SAR(L,J,K)
        END DO
        SAR(L,J,13)=SUM/12.0
        END DO
        DO K=1,13
          SUM=0.0
          DO J=1,NYRS
            SUM=SUM+SAR(L,J,K)
          END DO
          SARM(L,K)=SUM/FNYRS
        END DO
      END DO
!     PRINT OUT EOM SURFACE AREA
      IF (IPSA.AND.NRES>0) THEN
        CALL HEADIN()
        DO L=1,NRES
  !        BYPASS PRINT IF RESERVOIR CAPACITY ZERO
          IF(SMX(L).GT.1E-3) THEN
            IF (LINE>LinesPerPage-NYRS-5) THEN
              CALL HEADIN
            ENDIF
            WRITE (6,"(/,30X,A32,' END OF MONTH SURFACE AREA ACRES',/)") &
              Trim(AllTrim(PRESV(L)))
            WRITE (6,202) (VAR(I),I=1,14)
            LINE=LINE+5
            DO J=1,NYRS
              WRITE (6,302) IYEAR(J),(SAR(L,J,K),K=1,13)
              LINE=LINE+1
            ENDDO
            WRITE (6,303) (SARM(L,K),K=1,13)
            LINE=LINE+2
          END IF
        END DO
      END IF
      IF (NHPW.LT.1) GO TO 648
      DO 647 IP=1,NHPW
      DO 632 J=1,NYRS
      SUM1=0.0
      SUM2=0.0
      DO 631 K=1,12
      SUM1=SUM1+HEAD(IP,J,K)
  631 SUM2=SUM2+ENER(IP,J,K)
      HEAD(IP,J,13)=SUM1/12.0
  632 ENER(IP,J,13)=SUM2
      DO 635 K=1,13
      SUM1=0.0
      SUM2=0.0
      DO 634 J=1,NYRS
      SUM1=SUM1+HEAD(IP,J, K)
  634 SUM2=SUM2+ENER(IP,J, K)
      HEADM(IP,K) = SUM1/FNYRS
  635 ENERM(IP,K) = SUM2/FNYRS
!        BYPASS PRINT IF NO ENERGY GENERATED
      IF(SUM2.LE.1E-3) GO TO 647
      CALL HEADIN
      WRITE (6,641) PHYPW(IP)
  641 FORMAT (    /,30X,A32,' AVERAGE MONTHLY POWER HEAD FEET',/)
      WRITE (6,202) (VAR(I),I=1,14)
      DO 642 J=1,NYRS
  642 WRITE (6,643) IYEAR(J),(HEAD(IP,J,K),K=1,13)
      WRITE (6,644) (HEADM(IP,K),K=1,13)
  644 FORMAT (/,6X,'MEAN',12F9.1,F10.1)
  643 FORMAT (6X,I4,12F9.1,F10.1)
      CALL HEADIN
      WRITE (6,653) PHYPW(IP)
  653 FORMAT (    /,30X,A32,' MONTHLY ENERGY GENERATION MEGAWATT-HOURS' &
      ,/)
      WRITE (6,202) (VAR(I),I=1,14)
      DO 656 J=1,NYRS
  656 WRITE (6,302) IYEAR(J),(ENER(IP,J,K),K=1,13)
      WRITE (6,303) (ENERM(IP,K),K=1,13)
  647 CONTINUE
  648 CONTINUE
!
!     PRINT OUT ANNUAL SHORTAGE ACRE-FT AND PERCENT
!
      IF(IPAS)THEN
      DO L=1,NLND,3
         CALL HEADIN
         WRITE (6,"(/,40X,'ANNUAL SHORTAGE ACRE-FT AND PERCENT'/)")
         LNEX=MIN(NLND,L+2)
         WRITE (RECOU2,"('(10X,',I2,'(11X,''LAND AREA'',I3,7X))')") &
               LNEX-L+1
         WRITE (RECOUT,491)LNEX-L+1
  491    FORMAT ("(6X,'YEAR'",I2,"(6X,'ACRE-FEET',8X,'PERCENT'))")
         WRITE(6,RECOU2)(LL,LL=L,LNEX)
         WRITE(6,RECOUT)
         DO J=1,NYRS
            WRITE (6,"(6X,I4,3(F15.0,F15.2,:))") &
                  IYEAR(J),(SHORT(LL,J,13),SRTPC(LL,J), &
                  LL=L,LNEX)
         ENDDO
  492    FORMAT (6X,I4,3( F15.0,F15.2,:))
         WRITE (6,493)(SHORTM(LL,13),SRTPCM(LL),LL=L,LNEX)
  493    FORMAT (/,6X,'MEAN',3(F15.0,F15.2,:))
      ENDDO
      END IF
!
!      PRINT SUMMARY PAGE
!
      CALL HEADIN
      WRITE(6,900)
  900 FORMAT(//30X,'SUMMARY PAGE'/)
      WRITE(6,902)
  902 FORMAT(/ 30X,'MEAN ANNUAL SURFACE FLOWS'/)
      DO L=1,NQX,6
          LX5=L+5
          IF(LX5.GE.NQX) LX5=NQX
          WRITE(6,901) (IPQX(LX),QXM(IPQX(LX),13), LX=L,LX5)
      ENDDO
 9030 CONTINUE
  901 FORMAT( 8X,6('  QX(',I3,') =',F9.0,:))
      WRITE(6,903)
  903 FORMAT(//20X,'RESERVOIR',33X,'INITIAL', 2X,'END CONTENT', &
             4X,'MAXIMUM',5X,'MINIMUM',1X,'EVAPORATION'/)
      DO 9000 L=1,NRES
      FMAX=0.
      FMIN=1E+20
      DO 8990 J=1,NYRS
      DO 8990 K=1,12
      IF(ST(L,J,K).GT.FMAX) FMAX=ST(L,J,K)
      IF(ST(L,J,K).LT.FMIN) FMIN=ST(L,J,K)
 8990 CONTINUE
 9000 WRITE(6,904) trim(AllTrim(PRESV(L))),STOIC(L),STO(L),FMAX,FMIN, &
                   EVAPM(L,13)
  904 FORMAT(20X,A32,6X,5F12.0)
      WRITE(6,905)
  905 FORMAT(//20X,'LAND AREA',30X,'SHORTAGES', 9X,'PCT'/)
      DO 9010 L=1,NLND
 9010 WRITE(6,9020) trim(AllTrim(PLAND(L))),SHORTM(L,13),SRTPCM(L)
 9020 FORMAT(20X,A32,5X,F12.0,F11.2,'%')
      WRITE(6,904)
!**********************************************************************
!     Calculate total subarea precipitation in inches and acre-feet
!     Calculate natural use as the total precipitation minus the yield
!**********************************************************************
  SubNatUse=0.0
  TempArr=0.0
  isFirst=.TRUE.
  TotAcres=0.0
  AvPre=0.0
  DO J=1,NYRS
    DO K=1,12
      DO L=1,NSUBS
        SubNatUse(J,K)=SubNatUse(J,K)+SubAcres(L,J)*SubPrecip(L,J,K)
        IF (isFirst) THEN
          TotAcres=TotAcres+SubAcres(L,J)
        END IF
      END DO
      AreaPrecip(J,K)=SubNatUse(J,K)/TotAcres
      AreaPrecip(J,13)=AreaPrecip(J,13)+SubNatUse(J,K)/TotAcres
      isFirst=.FALSE.
      AvPre(K)=AvPre(K)+SubNatUse(J,K)/(TotAcres*REAL(NYRS))
      AvPre(13)=AvPre(13)+SubNatUse(J,K)/(TotAcres*REAL(NYRS))
    END DO
  END DO
  AvRainAF=0.0
  DO J=1,NYRS
    SubNatUse(J,13)=0.0
    DO K=1,12
      SubNatUse(J,K)=SubNatUse(J,K)/12.0
      SubNatUse(J,13)=SubNatUse(J,13)+SubNatUse(J,K)
      AreaPrAF(J,K)=SubNatUse(J,K)
      AreaPrAF(J,13)=AreaPrAF(J,13)+AreaPrAF(J,K)
      AvRainAF(K)=AvRainAF(K)+SubNatUse(J,K)/REAL(NYRS)
      AvRainAF(13)=AvRainAF(13)+SubNatUse(J,K)/REAL(NYRS)
    END DO
  END DO
  AvNatuse=0.0
  DO J=1,NYRS
    SubNatUse(J,13)=0.0
    DO K=1,12
      SubNatUse(J,K)=SubNatUse(J,K)-MonYield(J,K)
      SubNatUse(J,13)=SubNatUse(J,13)+SubNatUse(J,K)
      AvNatuse(K)=AvNatuse(K)+SubNatUse(J,K)/REAL(NYRS)
      AvNatuse(13)=AvNatuse(13)+SubNatUse(J,K)/REAL(NYRS)
    END DO
  END DO
!**********************************************************************
!     Calculate groundwater mining and effects on yield
!**********************************************************************
  TotMining=0.0
  TotWMining=0.0
  GWMiningQ=0.0
  GWMiningQM=0.0
  YieldWMining=0.0
  YieldWMiningM=0.0
  DO L=1,NLND
    TotMining(13)=TotMining(13)+GWMining(L)
  END DO
  YieldWMining=MonYield
  IF (TotMining(13).GT.0.5) THEN
    DO J=1,NYRS
      TotMining=0.0
      DO L=1,NLND
        DO K=1,12
          GWMiningQ(J,K)=GWMiningQ(J,K)+GWMining(L)*CONUSE(K,J,L)/CONUSE(13,J,L)
          GWMiningQ(J,13)=GWMiningQ(J,13)+GWMining(L)*CONUSE(K,J,L)/CONUSE(13,J,L)
          GWMiningQM(K)=GWMiningQM(K)+GWMining(L)*CONUSE(K,J,L)/CONUSE(13,J,L) &
            /FNYRS
          GWMiningQM(13)=GWMiningQM(13)+GWMining(L)*CONUSE(K,J,L)/CONUSE(13,J,L) &
            /FNYRS
        END DO
      END DO
    END DO
    DO K=1,13
      DO J=1,NYRS
        YieldWMining(J,K)=YieldWMining(J,K)-GWMiningQ(J,K)
        YieldWMiningM(K)=YieldWMiningM(K)+YieldWMining(J,K)/FNYRS
      END DO
    END DO
  END IF
      CALL HEADIN
      WRITE(6,'(A)')Trim(CenterIt("OUTPUT SUMMARY TABLE",132))
      LINE=LINE+1
!**********************************************************************
!     WRITE OUT A SUMMARY OF THE LAND AREA DEMANDS, WETLAND
!     DEMANDS AND SURFACE FLOWS AND GROUNDWATER
!**********************************************************************
      PRAV=0.0
      TMAV=0.0
 2905 CONTINUE
      DO L=1,NLND
        DO J=1,NYRS
          DO K=1,12
            CALL FAVER(TMAV,TEM(L,J,K),REAL(NLND*NYRS,KIND=8), &
              REAL(NLND*NYRS,KIND=8)*12.,K)
            CALL FAVER(PRAV,PRE(L,J,K),REAL(NLND*NYRS,KIND=8), &
              REAL(NLND*NYRS,KIND=8),K)
          END DO
        END DO
      END DO
      WRITE(6,3000)(VAR(I),I=2,14)
 3000 FORMAT(/41X,12(3X,A4),4X,A4)
      CALL ULINE('CLIMATOLOGICAL DATA',2,0)
      CALL TPRIN(TMAV,'TEMPERATURE (DEG F.)',1_4, &
       '(4X,A,T42,12F7.1,F8.1)')
      CALL TPRIN(PRAV,'CROPLAND PRECIPITATION (INCHES)',2_4, &
       '(4X,A,T42,12F7.2,F8.2)')
      CALL TPRIN(AvPre,'AVERAGE AREA PRECIPITATION (INCHES)',2_4, &
       '(4X,A,T42,12F7.2,F8.2)')
      CALL FZER(QXTOT)
      DO I=1,19
         IEXPO(I)=-IEXPO(I)
      ENDDO
      !YIELD=0.0
      QXFL=0.0
      QXTOT=0.0

      IF(NINFL.GT.0)THEN
        CALL SPRIN('RIVER INFLOW',IINFL,REAL(-1.0,KIND=8),.FALSE.)
      ENDIF
      IF (NUMBASIN.GT.0) THEN
        CALL SPRIN('BASIN INFLOW',IBASIN,REAL(-1.0,KIND=8),.FALSE.)
      END IF
      IF(NTRIB.GT.0)CALL SPRIN('TRIBUTARY FLOWS',ITRIB,REAL(0.0,KIND=8),.FALSE.)
      IF(NIMP.GT.0)CALL SPRIN('IMPORTS',IIMP,REAL(-1.0,KIND=8),.FALSE.)
      IF(NEXPO.GT.0)CALL SPRIN('EXPORTS',IEXPO,REAL(1.0,KIND=8),.FALSE.)
      IF(NUNG.GT.0)CALL SPRIN('UNGAGED FLOWS',IUNG,REAL(0.0,KIND=8),.FALSE.)
      IF(TRANNUM.GT.0)THEN
        IF (ABS(SubTranImpM(13)).GT.0.0) THEN
          CALL SPRIN('TRANSBASIN IMPORTS',TRANQX,REAL(-1.0,KIND=8),.TRUE.)
        ELSEIF (ABS(SubTranExpoM(13)).GT.0.0) THEN
          CALL SPRIN('TRANSBASIN EXPORTS',TRANQX,REAL(1.0,KIND=8),.TRUE.)
        END IF
      END IF
      IF(NRES.GT.0)THEN
          IRSFLG=0
          DO L=1,NRES
              IF(SMX(L).GT.0)IRSFLG=1
          ENDDO
      END IF
      if (LINE.GT.LINESPERPAGE*2/3) then
        CALL HEADIN()
        WRITE(6,3000)(VAR(I),I=2,14)
      end if
      IF(IRSFLG.GT.0)THEN
          CALL ULINE('RESERVOIR OPERATION',1,0)
          DO L=1,NRES
              IF(SMX(L).GT.0.0)THEN
                  WRITE(6,"(/' ',A32)")Trim(AllTrim(PRESV(L)))
                  CALL FZER(QXFL)
                  CALL FZER(RESBAL)
                  DO K=1,12
                      !CALL FAVER(YIELD,EVAPM(L,K),REAL(1.,KIND=8), &
                      !  REAL(1.,KIND=8),K)
                      CALL FAVER(QXFL,-EVAPM(L,K),REAL(1.,KIND=8), &
                        REAL(1.,KIND=8),K)
                      CALL FAVER(RESBAL,-EVAPM(L,K),REAL(1.,KIND=8), &
                        REAL(1.,KIND=8),K)
                      CALL FAVER(QXTOT,-EVAPM(L,K),REAL(1.,KIND=8), &
                        REAL(1.,KIND=8),K)
                  ENDDO
                  CALL TPRIN(QXFL,'NET EVAPORATION '//Trim(AllTrim(PRESV(L))), &
                    0_4,'(3X,A,T42,12I7,I8)')
                  CALL FZER(QXFL)
                  DO K=1,12
                      !CALL FAVER(YIELD,-STDIF(L,K),REAL(1.,KIND=8), &
                      !  REAL(1.,KIND=8),K)
                      CALL FAVER(QXFL,STDIF(L,K),REAL(1.,KIND=8), &
                        REAL(1.,KIND=8),K)
                      CALL FAVER(RESBAL,STDIF(L,K),REAL(1.,KIND=8), &
                        REAL(1.,KIND=8),K)
                      CALL FAVER(QXTOT,STDIF(L,K),REAL(1.,KIND=8), &
                        REAL(1.,KIND=8),K)
                  ENDDO
                  CALL TPRIN(QXFL,'+/- STORAGE '//PRESV(L),0_4, &
                          '(3X,A,T42,12I7,I8)')
              END IF
          ENDDO
      END IF
      CALL TPRIN(QXTOT,'TOTAL SUBAREA SUPPLY',0_4, &
            '(/1X,A,T42,12I7,I8)')
      LINE=LINE+1
      CALL ULINE(' ',0_4,LEN('TOTAL SUBAREA SUPPLY'))
      if (LINE.GT.LINESPERPAGE*2/3) then
        CALL HEADIN()
        WRITE(6,3000)(VAR(I),I=2,14)
      end if
      CALL ULINE('AGRICULTURE LAND BUDGET',2,0)
      RSSP=0.0
      DO K=1,12
          CALL FAVER(RSSP,RZ(K)+EFCR(K),REAL(1.,KIND=8),REAL(1.,KIND=8),K)
      ENDDO
      CPCU=0.0
      CACU=0.0
      DO K=1,12
          DO J=1,NYRS
              DO L=1,NLND
                  WTPAST=WPRO(L,J).GT.0.0.OR.WTPAST
!                  CALL FAVER(CPCU,MAX(0.0,(CPACFT(L,J,K)- &
!                          LGPOT(L,J,K))/FLOAT(NYRS)),REAL(1.,KIND=8), &
!                        REAL(1.,KIND=8),K)
                  CALL FAVER(CPCU,(CONUSE(K,J,L)+WCUSE(L,J,K)+ &
                    MAX(0.0,EffPRE(L,J,K)-PERCO(K,J,L)-WPERC(K,J,L)))/ &
                      REAL(NYRS,KIND=8),REAL(1.,KIND=8),REAL(1.,KIND=8),K)
                  FUN=MAX(0.0,EffPRE(L,J,K)-PERCO(K,J,L)-WPERC(K,J,L))/ &
                      REAL(NYRS,KIND=8)
                  IF (K.EQ.4.AND.FUN.GT.0.0) THEN
                    JJ=1
                  END IF
!                  FUN=MAX(0.0,(CPACFT(L,J,K)-LGPOT(L,J,K) &
!                          -MIN((CONUSE(K,J,L)+WCUSE(L,J,K)) &
!                          *TEF(L)+RTUSE(L,K)+GWUSE(L,K)+SBUSE(L,K) &
!                          ,SHORT(L,J,K)*TEF(L)))/FLOAT(NYRS))
                  FUN=MAX(0.0,(CONUSE(K,J,L)+WCUSE(L,J,K)+ &
                    MAX(0.0,EffPRE(L,J,K)-PERCO(K,J,L)-WPERC(K,J,L)) - &
                    SHORT(L,J,K)*TEF(L,J))/REAL(NYRS,KIND=8))
                  CALL FAVER(CACU,FUN,REAL(1.,KIND=8),REAL(1.,KIND=8),K)
              ENDDO
          ENDDO
      ENDDO
      CALL TPRIN(EFCR,'CROPLAND EFFECTIVE PRECIPITATION' &
       ,0_4,'(4X,A,T42,12I7,I8)')
      CALL TPRIN(TDIVR,'SURFACE DIVERSION TO CROPLAND',0_4, &
                 '(4X,A,T42,12I7,I8)')
      AGPUMP=0.0
      DO L=1,NLND
        IF (GWQX(L).GT.0) THEN
          DO K=1,13
            AGPUMP(K)=AGPUMP(K)+QXM(GWQX(L),K)
          END DO
        END IF
      END DO
      CALL TPRIN(AGPUMP,'GROUNDWATER PUMPING TO CROPLAND',0_4, &
        '(4X,A,T42,12I7,I8)')
      CALL TPRIN(RIVGW,'RIVER USE BY WET PASTURE', &
       0_4,'(4X,A,T42,12I7,I8)')
      CALL TPRIN(RZ,'AMOUNT TO ROOTZONE',0_4,'(4X,A,T42,12I7,I8)')
      !CALL TPRIN(RSSP,'ROOT ZONE SUPPLY',0_4,'(4X,A,T42,12I7,I8)')
      CALL TPRIN(RFLO,'UNROUTED RETURN FLOW',0_4,'(4X,A,T42,12I7,I8)')
      CALL TPRIN(CPCU,'CROPLAND POTENTIAL CONSUMPTIVE USE',0_4, &
              '(4X,A,T42,12I7,I8)')
      TEMP(13)=0.0
      DO K=1,12
        DO L=1,NLND
          DO J=1,NYRS
            TEMP(K)=TEMP(K)+(EffPRE(L,J,K)-PERCO(K,J,L)-WPERC(K,J,L))/FNYRS
          END DO
        END DO
        TEMP(13)=TEMP(13)+TEMP(K)
      END DO
      !DO I = 1,12
      !    SELECT CASE(I)
      !    CASE (1,8:12)
      !        VALU=EFCR(I)
      !    CASE DEFAULT
      !        VALU=0.00
      !    END SELECT
      !    TEMP(I)=VALU
      !    TEMP(13)=TEMP(13)+VALU
      !ENDDO
      CALL TPRIN(TEMP,'IRRIGATION SEASON EFFECTIVE PRECIP.' &
       ,0_4,'(4X,A,T42,12I7,I8)')
      CALL TPRIN(RZSUP,'CROP CONSUMPTIVE USE FROM RETURN FLOW',0_4, &
       '(4X,A,T42,12I7,I8)')
      IF(ABS(SMSPL(13)).GT.0.0)THEN
         CALL TPRIN(SMSPL,'SPILLS FROM SOIL MOISTURE RESERVOIR',0_4, &
          '(4X,A,T42,12I7,I8)')
      ELSE
         CALL TPRIN(SMSPL,'SPILLS FROM SOIL MOISTURE RESERVOIR',1_4, &
          '(4X,A,T42,12F7.1,F8.1)')
      ENDIF
      CALL TPRIN(ACUSM,'ACCUMULATED CROP SOIL MOISTURE',0_4, &
       '(4X,A,T42,12I7,I8)')
      IF(WTPAST)CALL TPRIN(WPCUM,'ACCUMULATED WET PAST. SOIL MOISTURE' &
       ,0_4,'(4X,A,T42,12I7,I8)')
      CALL TPRIN(CHSM,'CHANGE IN CROP SOIL MOISTURE',0_4, &
       '(4X,A,T42,12I7,I8)')
      IF(WTPAST)CALL TPRIN(WPCHG,'CHANGE IN WET PAST. SOIL MOISTURE', &
        0_4,'(4X,A,T42,12I7,I8)')
      CALL TPRIN(CUDEF,'SHORTAGES (DIVERSION)',0_4, &
              '(4X,A,T42,12I7,I8)')
      CALL TPRIN(CACU,'CROPLAND ACTUAL CONSUMPTIVE USE', &
       0_4,'(4X,A,T42,12I7,I8)')
      IF(GWLOS(13).GT.0.0)THEN
          CALL TPRIN(GWLOS,'RETURN FLOW LOSSES',0_4, &
            '(4X,A,T42,12I7,I8)')
      ENDIF
!     CALL TPRIN(RFGW,'TIME ROUTED RETURN FLOW',0_4,
!    * '(4X,A,T42,12I7,I8)')
      CPDEP=0.00
      TOTRF=0.0
      DO K=1,12
        DO J=1,NYRS
          DO L=1,NLND
            TOTRF(K)=TOTRF(K)+QX(IRT(L),J,K)/REAL(NYRS,KIND=8)
          END DO
        END DO
        TOTRF(13)=TOTRF(13)+TOTRF(K)
      END DO
      DO I = 1,12
          DO J=1,NYRS
            DO L=1,NLND
              CPDEP(I)=CPDEP(I)+(PERCO(I,J,L)+WPERC(I,J,L))/REAL(NYRS,KIND=8)
            END DO
          END DO
          CPDEP(I)=CPDEP(I)+TDIVR(I)+AGPUMP(I)-TOTRF(I)+RIVGW(I)
          CPDEP(13)=CPDEP(13)+CPDEP(I)
      ENDDO
      CALL TPRIN(CPDEP,'CROPLAND ACTUAL DEPLETION',0_4, &
              '(4X,A,T42,12I7,I8)')
      CALL TPRIN(TOTRF,'TOTAL RETURN FLOW',0_4,'(4X,A,T42,12I7,I8)')
!
!     DOMESTIC USE
!
      if (LINE.GT.LINESPERPAGE*2/3) then
        CALL HEADIN()
        WRITE(6,3000)(VAR(I),I=2,14)
      end if
      CALL ULINE('MUNICIPAL AND INDUSTRIAL USE',2,0)
      CALL TPRIN(DDIV,'M&I SURFACE DIVERSIONS',1_4, &
         '(4X,A,T42,12F7.1,F8.1)')
      IF(DPUM(13).GT.0.0) &
         CALL TPRIN(DPUM,'M&I GROUNDWATER PUMPED',1_4, &
              '(4X,A,T42,12F7.1,F8.1)')
      CALL TPRIN(DSUP,'TOTAL M&I DEMAND',1_4 &
              ,'(4X,A,T42,12F7.1,F8.1)')
      DemIn=0.0
      DemOut=0.0
      DO K=1,12
        DO J=1,NYRS
          DO L=1,NMUN
            DemIn(K)=DemIn(K)+MunDemIn(L,J,K)/REAL(NYRS,KIND=8)
            DemOut(K)=DemOut(K)+MunDemOut(L,J,K)/REAL(NYRS,KIND=8)
          END DO
        END DO
        DemIn(13)=DemIn(13)+DemIn(K)
        DemOut(13)=DemOut(13)+DemOut(K)
      END DO
      CALL TPRIN(DemIn,'INDOOR DEMAND',1_4,'(4X,A,T42,12F7.1,F8.1)')
      CALL TPRIN(DemOut,'OUTDOOR DEMAND',1_4,'(4X,A,T42,12F7.1,F8.1)')
      DMUSE=0.0
      DO K=1,12
        DO J=1,NYRS
          DO L=1,NMUN
            DMUSE(K)=DMUSE(K)+DepIndoor(L,J,K)/FNYRS
          END DO
        END DO
        DMUSE(13)=DMUSE(13)+DMUSE(K)
      END DO
      CALL TPRIN(SSIDepleteM,'SSI DEPLETION',1_4,'(4X,A,T42,12F7.1,F8.1)')
      CALL TPRIN(DMUSE,'INDOOR DEPLETION',1_4,'(4X,A,T42,12F7.1,F8.1)')
      LGOUT=0.0
      DO K=1,12
        DO J=1,NYRS
          DO L=1,NMUN
            LGOUT(K)=LGOUT(K)+DepOutdoor(L,J,K)/FNYRS
          END DO
        END DO
        LGOUT(13)=LGOUT(13)+LGOUT(K)
      END DO
      CALL TPRIN(LGOUT,'OUTDOOR DEPLETION',1_4 &
              ,'(4X,A,T42,12F7.1,F8.1)')
      DMACU=0.0
      DO K=1,12
        DO J=1,NYRS
          DO L=1,NMUN
            DMACU(K)=DMACU(K)+MunDep(L,J,K)/FNYRS
          END DO
        END DO
        DMACU(13)=DMACU(13)+DMACU(K)
      END DO
      CALL TPRIN(DMACU,'TOTAL M&I DEPLETION',1_4 &
              ,'(4X,A,T42,12F7.1,F8.1)')
      DRET=0.0
      DO K=1,12
        DO J=1,NYRS
          DO L=1,NMUN
            DRET(K)=DRET(K)+QX(MunQXRet(L),J,K)/FNYRS
          END DO
        END DO
        DRET(13)=DRET(13)+DRET(K)
      END DO
      CALL TPRIN(DRET,'RETURN FLOW',1_4,'(4X,A,T42,12F7.1,F8.1)')
      IF(WCONS(13).GT.0.0)THEN
          CALL ULINE('WET/OPEN WATER AREA BUDGET',2,0)
          !CALL TPRIN(WPRE,'WET/OPEN WATER EFFECTIVE PRECIP.',0_4, &
          !        '(4X,A,T42,12I7,I8)')
!          CALL TPRIN(WCONS,'RIPARIAN/OPEN WATER NET CONSUMPTIVE USE',0_4, &
!                  '(4X,A,T42,12I7,I8)')
!         IF(WOPN(13).GT.0.0)
!    *        CALL TPRIN(WOPN,'OPEN WATER EVAPORATION',0_4,
!    +                '(4X,A,T42,12I7,I8)')
          WTDEP(13)=0.00
          DO I=1,12
              VALU=MAX(0.0,WCONS(I))
              WTDEP(I)=VALU
              WTDEP(13)=WTDEP(13)+VALU
          ENDDO
          CALL TPRIN(WTDEP,'WET/OPEN WATER DEPLETION',0_4, &
                  '(4X,A,T42,12I7,I8)')
          TEMP(13)=0.00
          DO I=1,12
              VALU=MAX(0.0,WPRE(I)-WCONS(I))
              TEMP(I)=VALU
              TEMP(13)=TEMP(13)+VALU
          ENDDO
!          CALL TPRIN(TEMP,'WET/OPEN WATER RETURN FLOWS',0_4, &
!                  '(4X,A,T42,12I7,I8)')
      END IF
      CALL ULINE('SUBAREA CONSUMPTIVE USE/DEPLETIONS',2,0)
      TEMP(13)=0.00
      DO I=1,12
          VALU=CACU(I)+DMACU(I)+WCONS(I)
          TEMP(I)=VALU
          TEMP(13)=TEMP(13)+VALU
      ENDDO
      CALL TPRIN(TEMP,'TOTAL CONSUMPTIVE USE',0_4, &
                 '(4X,A,T42,12I7,I8)')
      QXFL(13)=0.00
      DO I=1,12
          VALU=CPDEP(I)+DMACU(I)+WTDEP(I)
          QXFL(I)=VALU
          !YIELD(I)=YIELD(I)+VALU+TGAGE(I)
          !YIELD(13)=YIELD(13)+VALU+TGAGE(I)
          QXFL(13)=QXFL(13)+VALU
      ENDDO
      CALL TPRIN(QXFL,'TOTAL DEPLETIONS',0_4,'(4X,A,T42,12I7,I8)')
      CALL ULINE('OUTFLOW AND/OR GROUNDWATER CHANGE',2,0)
      CALL TPRIN(CHGW,'CHANGE IN GROUND WATER STORAGE',0_4, &
            '(4X,A,T42,12I7,I8)')
      BasOut=0.0
      IF(NumOutflow+NUMBASOUT.GT.0)THEN
        DO M =1,NumOutflow
          LL=NOUFL(M)
          DO K=1,13
            TEMPARR(K)=QXM(LL,K)
            BasOut(K)=BasOut(K)+QXM(LL,K)
          END DO
          CALL TPRIN(TEMPARR,'BUDGET OUTFLOW '//QXNAM(QxMap(LL)),0_4, &
                   '(4X,A,T42,12I7,I8)')
        END DO
        DO M=1,NUMBASOUT
          LL=IBASOUT(M)
          DO K=1,13
            TEMPARR(K)=QXM(LL,K)
            BasOut(K)=BasOut(K)+QXM(LL,K)
          END DO
          CALL TPRIN(TEMPARR,'BUDGET OUTFLOW '//QXNAM(QxMap(LL)),0_4, &
                   '(4X,A,T42,12I7,I8)')
        END DO
      ELSE
        TEMPARR=0.0
        CALL TPRIN(TEMPARR,'NO BUDGET OUTFLOW',0_4, &
          '(4X,A,T42,12I7,I8)')
         !WRITE(6,"(12X'WARNING, NOUFL (OUTFLOW QX) NOT DEFINED)')")
      ENDIF
      IF (NumOutEvap.GT.0) THEN
        DO M =1,NumOutEvap
          LL=NOUEVFL(M)
          DO K=1,13
            TEMPARR(K)=QXM(LL,K)
            BasOut(K)=BasOut(K)+QXM(LL,K)
          END DO
          CALL TPRIN(TEMPARR,'TERMINAL EVAP '//QXNAM(QxMap(LL)),0_4, &
                   '(4X,A,T42,12I7,I8)')
        END DO
      END IF
      CALL ULINE('SUBAREA YIELD',2,0)
      CALL TPRIN(AnnYield,'SUMMATION OF YIELD',0_4, &
       '(4X,A,T42,12I7,I8)')
      CALL TPRIN(AvNatUse,'NATURAL SYSTEM USE',0_4, &
       '(4X,A,T42,12I7,I8)')
      CALL ULINE('SUMMARY',2,0)
      CALL TPRIN(AvRainAF,'TOTAL PRECIPITATION',0_4,'(4X,A,T42,12I7,I8)')
      CALL TPRIN(AvNatUse,'NATURAL SYSTEM USE',0_4,'(4X,A,T42,12I7,I8)')
      CALL TPRIN(SubSurfDivM,'TOTAL SURFACE DIVERSIONS',0_4,'(4X,A,T42,12I7,I8)')
      CALL TPRIN(AGPUMP+DPUM,'TOTAL GROUNDWATER USE', &
        0_4,'(4X,A,T42,12I7,I8)')
      CALL TPRIN(SubInflowM,'INFLOW',0_4,'(4X,A,T42,12I7,I8)')
      IF (NUMBASIN.GT.0) THEN
        CALL TPRIN(BasinInM,'BASIN INFLOW',0_4,'(4X,A,T42,12I7,I8)')
      END IF
      CALL TPRIN(SubOutflowM,'OUTFLOW',0_4,'(4X,A,T42,12I7,I8)')
      if (NUMBASOUT.GT.0) then
        CALL TPRIN(BasinOutM,'BASIN OUTFLOW',0_4,'(4X,A,T42,12I7,I8)')
      end if
      IF (NumOutEvap.GT.0) THEN
        CALL TPRIN(TermOutEvapM,'TERMINAL EVAPORATION',0_4,'(4X,A,T42,12I7,I8)')
      END IF
      IF (ABS(SubExportM(13))>0.0) THEN
        CALL TPRIN(SubExportM,'EXPORTS',0_4,'(4X,A,T42,12I7,I8)')
      END IF
      IF (ABS(SubImportM(13))>0.0) THEN
        CALL TPRIN(SubImportM,'IMPORTS',0_4,'(4X,A,T42,12I7,I8)')
      END IF
      IF (TranNum.GT.0) THEN
        IF (ABS(SubTranImpM(13)).GT.0.01) THEN
          CALL TPRIN(SubTranImpM,'TRANSBASIN IMPORTS',0_4,'(4X,A,T42,12I7,I8)')
          LINE=LINE+1
        END IF
        IF (ABS(SubTranExpoM(13)).GT.0.01) THEN
          CALL TPRIN(SubTranExpoM,'TRANSBASIN EXPORTS',0_4,'(4X,A,T42,12I7,I8)')
          LINE=LINE+1
        END IF
      END IF
      CALL TPRIN(TDIVR+RIVGW,'AGRICULTURAL SURFACE DIVERSIONS',0_4, &
        '(4X,A,T42,12I7,I8)')
      CALL TPRIN(AGPUMP,'AGRICULTURAL GROUNDWATER DIVERSION',0_4, &
        '(4X,A,T42,12I7,I8)')
      CALL TPRIN(TDIVR+AGPUMP+RIVGW, &
        'AGRICULTURAL TOTAL DIVERSIONS',0_4, &
        '(4X,A,T42,12I7,I8)')
      CALL TPRIN(CPDEP,'AGRICULTURAL DEPLETIONS',0_4,'(4X,A,T42,12I7,I8)')
      CALL TPRIN(DDIV,'M&I SURFACE DIVERSIONS',0_4,'(4X,A,T42,12I7,I8)')
      CALL TPRIN(DPUM,'M&I GROUNDWATER DIVERSIONS',0_4,'(4X,A,T42,12I7,I8)')
      CALL TPRIN(DSUP,'M&I TOTAL DIVERSIONS',0_4,'(4X,A,T42,12I7,I8)')
      CALL TPRIN(DMACU,'M&I DEPLETIONS',0_4,'(4X,A,T42,12I7,I8)')
      CALL TPRIN(WTDEP,'WETLAND DEPLETIONS',0_4,'(4X,A,T42,12I7,I8)')
      IF (NRES.GT.0) THEN
        CALL TPRIN(ReserEvapM,'RESERVOIR EVAPORATION (DEPLETION)',0_4, &
          '(4X,A,T42,12I7,I8)')
        CALL TPRIN(SubResStDifM,'CHANGE IN RESERVOIR VOLUME',0_4, &
          '(4X,A,T42,12I7,I8)')
      END IF
      CALL TPRIN(AnnYield,'YIELD',0_4,'(4X,A,T42,12I7,I8)')
      IF (GWMiningQM(13).GT..5) THEN
        CALL TPRIN(GWMiningQM,'GROUNDWATER MINING', &
          0_4,'(4X,A,T42,12I7,I8)')
        CALL TPRIN(YieldWMiningM,'YIELD MINUS GW MINING', &
          0_4,'(4X,A,T42,12I7,I8)')
      END IF

  CALL HEADIN()
  WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
  LINE=LINE+2
  WRITE(6,"(//30X,'Study Area Precipitation (Inches)'/)")
  WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
  LINE=LINE+5
  DO J=1,NYRS
    WRITE(6,"(6X,I4,12F9.2,F10.2)")IYEAR(J),(AreaPrecip(J,K),K=1,13)
    LINE=LINE+1
  END DO
  WRITE(6,"(/,6X,'MEAN',12F9.2,F10.2)")(AvPre(K),K=1,13)
  LINE=LINE+2

  IF (LINE>LinesPerPage-NYRS-5) THEN
    CALL HEADIN()
    WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
    LINE=LINE+2
  END IF
  WRITE(6,"(//30X,'Study Area Precipitation (Acre-Feet)'/)")
  WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
  LINE=LINE+5
  DO J=1,NYRS
    WRITE(6,302)IYEAR(J),(AreaPrAF(J,K),K=1,13)
    LINE=LINE+1
  END DO
  WRITE(6,303)(AvRainAF(K),K=1,13)
  LINE=LINE+2

  IF (LINE>LinesPerPage-NYRS-5) THEN
    CALL HEADIN()
    WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
    LINE=LINE+2
  END IF
  WRITE(6,"(//30X,'Study Area Natural System Use (Acre-Feet)'/)")
  WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
  LINE=LINE+5
  DO J=1,NYRS
    WRITE(6,302)IYEAR(J),(SubNatUse(J,K),K=1,13)
    LINE=LINE+1
  END DO
  WRITE(6,303)(AvNatuse(K),K=1,13)
  LINE=LINE+2

  if (SubSurfaceM(13).GT.0.0) then
    IF (LINE>LinesPerPage-NYRS-5) THEN
      CALL HEADIN()
      WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
      LINE=LINE+2
    END IF
    WRITE(6,"(//30X,'Study Area Surface Supply (Acre-Feet)')")
    WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
    LINE=LINE+5
    DO J=1,NYRS
      SubSurface(J,13)=0.0
      DO K=1,12
        SubSurface(J,13)=SubSurface(J,13)+SubSurface(J,K)
      END DO
      WRITE(6,302)IYEAR(J),(SubSurface(J,K),K=1,13)
      LINE=LINE+1
    END DO
    WRITE(6,303)(SubSurfaceM(K),K=1,13)
    LINE=LINE+2
  end if

  CALL HEADIN()
  WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
  LINE=LINE+2
  WRITE(6,"(//30X,'Study Area Surface Use (Acre-Feet)'/)")
  WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
  LINE=LINE+5
  DO J=1,NYRS
    WRITE(6,302)IYEAR(J),(SubSurfDiv(J,K),K=1,13)
    LINE=LINE+1
  END DO
  WRITE(6,303)(SubSurfDivM(K),K=1,13)
  LINE=LINE+2

  if (TotGwUseM(13).GT.0.0) then
    IF (LINE>LinesPerPage-nyrs-5) THEN
      CALL HEADIN()
      WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
      LINE=LINE+2
    END IF
    WRITE(6,"(//30X,'Study Area Groundwater Use (Acre-Feet)'/)")
    WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
    LINE=LINE+5
    DO J=1,NYRS
      WRITE(6,302)IYEAR(J),(TotGwUse(J,K),K=1,13)
      LINE=LINE+1
    END DO
    WRITE(6,303)(TotGwUseM(K),K=1,13)
    LINE=LINE+2
  end if

  IF (LINE>LinesPerPage-nyrs-5) THEN
    CALL HEADIN()
    WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
    LINE=LINE+2
  END IF
  WRITE(6,"(//30X,'Study Area Surface and Groundwater Use (Acre-Feet)'/)")
  WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
  LINE=LINE+5
  DO J=1,NYRS
    WRITE(6,302)IYEAR(J),(SubSurfDiv(J,K)+TotGwUse(J,K),K=1,13)
    LINE=LINE+1
  END DO
  WRITE(6,303)(SubSurfDivM(K)+TotGwUseM(K),K=1,13)
  LINE=LINE+2

  !if (ABS(SubInflowM(13)).GT.0.5) then
    IF (LINE>LinesPerPage-nyrs-5) THEN
      CALL HEADIN()
      WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
      LINE=LINE+2
    END IF
    WRITE(6,"(//30X,'Study Area Inflow (Acre-Feet)'/)")
    WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
    LINE=LINE+5
    DO J=1,NYRS
      WRITE(6,302)IYEAR(J),(SubInflow(J,K),K=1,13)
      LINE=LINE+1
    END DO
    WRITE(6,303)(SubInflowM(K),K=1,13)
    LINE=LINE+2
  !end if

  if (ABS(BasinInM(13)).GT.0.5) then
    IF (LINE>LinesPerPage-nyrs-5) THEN
      CALL HEADIN()
      WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
      LINE=LINE+2
    END IF
    WRITE(6,"(//30X,'Basin Inflow (Acre-Feet)'/)")
    WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
    LINE=LINE+5
    DO J=1,NYRS
      WRITE(6,302)IYEAR(J),(BasinIn(J,K),K=1,13)
      LINE=LINE+1
    END DO
    WRITE(6,303)(BasinInM(K),K=1,13)
    LINE=LINE+2
  end if

  !if (ABS(SubOutflowM(13)).GT.0.5) then
    IF (LINE>LinesPerPage-nyrs-5) THEN
      CALL HEADIN()
      WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
      LINE=LINE+2
    END IF
    WRITE(6,"(//30X,'Study Area Outflow (Acre-Feet)'/)")
    WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
    LINE=LINE+5
    DO J=1,NYRS
      WRITE(6,302)IYEAR(J),(SubOutflow(J,K),K=1,13)
      LINE=LINE+1
    END DO
    WRITE(6,303)(SubOutflowM(K),K=1,13)
    LINE=LINE+2
  !end if

  if (ABS(BasinOutM(13)).GT.0.5) then
    IF (LINE>LinesPerPage-nyrs-5) THEN
      CALL HEADIN()
      WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
      LINE=LINE+2
    END IF
    WRITE(6,"(//30X,'Basin Outflow (Acre-Feet)'/)")
    WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
    LINE=LINE+5
    DO J=1,NYRS
      WRITE(6,302)IYEAR(J),(BasinOut(J,K),K=1,13)
      LINE=LINE+1
    END DO
    WRITE(6,303)(BasinOutM(K),K=1,13)
    LINE=LINE+2
  end if

  !Terminal Evaporation
  IF (NumOutEvap.GT.0) THEN
    IF (LINE>LinesPerPage-nyrs-5) THEN
      CALL HEADIN()
      WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
      LINE=LINE+2
    END IF
    WRITE(6,"(//30X,'Terminal Lake Evaporation (Acre-Feet)'/)")
    WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
    LINE=LINE+5
    DO J=1,NYRS
      WRITE(6,302)IYEAR(J),(TermOutEvap(J,K),K=1,13)
      LINE=LINE+1
    END DO
    WRITE(6,303)(TermOutEvapM(K),K=1,13)
    LINE=LINE+2
  END IF

  if (ABS(SubExportM(13)).GT.0.0) then
    IF (LINE>LinesPerPage-nyrs-5) THEN
      CALL HEADIN()
      WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
      LINE=LINE+2
    END IF
    WRITE(6,"(//30X,'Study Area Export (Acre-Feet)'/)")
    WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
    LINE=LINE+5
    DO J=1,NYRS
      WRITE(6,302)IYEAR(J),(SubExport(J,K),K=1,13)
      LINE=LINE+1
    END DO
    WRITE(6,303)(SubExportM(K),K=1,13)
  end if

  if (ABS(SubImportM(13)).GT.0.0) then
    IF (LINE>LinesPerPage-nyrs-5) THEN
      CALL HEADIN()
      WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
      LINE=LINE+2
    END IF
    WRITE(6,"(//30X,'Study Area Import (Acre-Feet)'/)")
    WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
    LINE=LINE+5
    DO J=1,NYRS
      WRITE(6,302)IYEAR(J),(SubImport(J,K),K=1,13)
      LINE=LINE+1
    END DO
    WRITE(6,303)(SubImportM(K),K=1,13)
  end if

  IF (TranNum.GT.0) THEN
    IF (ABS(SubTranImpM(13)).GT.0.01) THEN
      IF (LINE>LinesPerPage-nyrs-5) THEN
        CALL HEADIN()
        WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
        LINE=LINE+2
      END IF
      WRITE(6,"(//30X,'Transbasin Imports (Acre-Feet)'/)")
      WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
      LINE=LINE+5
      DO J=1,NYRS
        WRITE(6,302)IYEAR(J),(SubTranImp(J,K),K=1,13)
        LINE=LINE+1
      END DO
      WRITE(6,303)(SubTranImpM(K),K=1,13)
    END IF
    IF (ABS(SubTranExpoM(13)).GT.0.01) THEN
      IF (LINE>LinesPerPage-nyrs-5) THEN
        CALL HEADIN()
        WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
        LINE=LINE+2
      END IF
      WRITE(6,"(//30X,'Transbasin Exports (Acre-Feet)'/)")
      WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
      LINE=LINE+5
      DO J=1,NYRS
        WRITE(6,302)IYEAR(J),(SubTranExpo(J,K),K=1,13)
        LINE=LINE+1
      END DO
      WRITE(6,303)(SubTranExpoM(K),K=1,13)
    END IF
  END IF

  IF (ReserEvapM(13)>0.0) THEN
    IF (LINE>LinesPerPage-nyrs-5) THEN
      CALL HEADIN()
      WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
      LINE=LINE+2
    END IF
    WRITE(6,"(//30X,'Reservoir Evaporation (Acre-Feet)'/)")
    WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
    LINE=LINE+5
    DO J=1,NYRS
      WRITE(6,302)IYEAR(J),(SubResEvap(J,K),K=1,13)
      LINE=LINE+1
    END DO
    WRITE(6,303)(SubResEvapM(K),K=1,13)

    IF (LINE>LinesPerPage-nyrs-5) THEN
      CALL HEADIN()
      WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
      LINE=LINE+2
    END IF
    WRITE(6,"(//30X,'Change in Reservoir Storage (Acre-Feet)'/)")
    WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
    DO J=1,NYRS
      WRITE(6,302)IYEAR(J),(SubResStDif(J,K),K=1,13)
      LINE=LINE+1
    END DO
    WRITE(6,303)(SubResStDifM(K),K=1,13)
  end if

  if (AgSurfaceM(13).GT.0.0) then
    CALL HEADIN()
    WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
    LINE=LINE+2
    WRITE(6,"(//30X,'Study Area Agricultural Diversions (Acre-Feet)'/)")
    WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
    LINE=LINE+5
    DO J=1,NYRS
      WRITE(6,302)IYEAR(J),(AgSurface(J,K),K=1,13)
      LINE=LINE+1
    END DO
    WRITE(6,303)(AgSurfaceM(K),K=1,13)
    LINE=LINE+2
  end if

  if (AgGWM(13).GT.0.0) then
    IF (LINE>LinesPerPage-nyrs-5) THEN
      CALL HEADIN()
      WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
      LINE=LINE+2
    END IF
    WRITE(6,"(//30X,'Study Area Agricultural Groundwater Pumping (Acre-Feet)'/)")
    WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
    LINE=LINE+5
    DO J=1,NYRS
      WRITE(6,302)IYEAR(J),(AgGW(J,K),K=1,13)
      LINE=LINE+1
    END DO
    WRITE(6,303)(AgGWM(K),K=1,13)
    LINE=LINE+2
  end if

  if (AgTotalM(13).GT.0.0) then
    IF (LINE>LinesPerPage-nyrs-5) THEN
      CALL HEADIN()
      WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
      LINE=LINE+2
    END IF
    WRITE(6,"(//30X,'Study Area Total Agricultural Diversions (Acre-Feet)'/)")
    WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
    LINE=LINE+5
    DO J=1,NYRS
      WRITE(6,302)IYEAR(J),(AgTotal(J,K),K=1,13)
      LINE=LINE+1
    END DO
    WRITE(6,303)(AgTotalM(K),K=1,13)
    LINE=LINE+2
  end if

  if (AgDepletionM(13).GT.0.0) then
    IF (LINE>LinesPerPage-nyrs-5) THEN
      CALL HEADIN()
      WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
      LINE=LINE+2
    END IF
    WRITE(6,"(//30X,'Study Area Agricultural Depletion (Acre-Feet)'/)")
    WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
    LINE=LINE+5
    DO J=1,NYRS
      WRITE(6,302)IYEAR(J),(AgDepletion(J,K),K=1,13)
      LINE=LINE+1
    END DO
    WRITE(6,303)(AgDepletionM(K),K=1,13)
    LINE=LINE+2
  end if

  if (MandISurfaceM(13).GT.0.0) then
    CALL HEADIN()
    WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
    LINE=LINE+2
    WRITE(6,"(//30X,'M&I Surface Diversions (Acre-Feet)'/)")
    WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
    LINE=LINE+5
    DO J=1,NYRS
      WRITE(6,302)IYEAR(J),(MandISurface(J,K),K=1,13)
      LINE=LINE+1
    END DO
    WRITE(6,303)(MandISurfaceM(K),K=1,13)
    LINE=LINE+2
  end if

  if (MandIGWM(13).GT.0.0) then
    IF (LINE>LinesPerPage-nyrs-5) THEN
      CALL HEADIN()
      WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
      LINE=LINE+2
    END IF
    WRITE(6,"(//30X,'M&I Groundwater Pumping (Acre-Feet)'/)")
    WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
    LINE=LINE+5
    DO J=1,NYRS
      WRITE(6,302)IYEAR(J),(MandIGW(J,K),K=1,13)
      LINE=LINE+1
    END DO
    WRITE(6,303)(MandIGWM(K),K=1,13)
    LINE=LINE+2
  end if

  if (ABS(MandITotalM(13)).GT.0.1) then
    IF (LINE>LinesPerPage-nyrs-5) THEN
      CALL HEADIN()
      WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
      LINE=LINE+2
    END IF
    WRITE(6,"(//30X,'M&I Total Diversions (Acre-Feet)'/)")
    WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
    LINE=LINE+5
    DO J=1,NYRS
      WRITE(6,302)IYEAR(J),(MandITotal(J,K),K=1,13)
      LINE=LINE+1
    END DO
    WRITE(6,303)(MandITotalM(K),K=1,13)
    LINE=LINE+2
  end if

  if (ABS(SSIDepleteM(13)).GT.0.1) then
    IF (LINE>LinesPerPage-nyrs-5) THEN
      CALL HEADIN()
      WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
      LINE=LINE+2
    END IF
    WRITE(6,"(//30X,'SSI M&I Depletions (Acre-Feet)'/)")
    WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
    LINE=LINE+5
    DO J=1,NYRS
      WRITE(6,302)IYEAR(J),(SSIDeplete(J,K),K=1,13)
      LINE=LINE+1
    END DO
    WRITE(6,303)(SSIDepleteM(K),K=1,13)
    LINE=LINE+2
  end if

  if (ABS(MandIDepM(13)).GT.0.1) then
    IF (LINE>LinesPerPage-nyrs-5) THEN
      CALL HEADIN()
      WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
      LINE=LINE+2
    END IF
    WRITE(6,"(//30X,'M&I Depletions (Acre-Feet)'/)")
    WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
    LINE=LINE+5
    DO J=1,NYRS
      WRITE(6,302)IYEAR(J),(MandIDep(J,K),K=1,13)
      LINE=LINE+1
    END DO
    WRITE(6,303)(MandIDepM(K),K=1,13)
    LINE=LINE+2
  end if

  if (WetDepM(13).GT.0.0) then
    CALL HEADIN()
    WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
    LINE=LINE+2
    WRITE(6,"(//30X,'Riparian Depletions (Acre-Feet)'/)")
    WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
    LINE=LINE+5
    DO J=1,NYRS
      WRITE(6,302)IYEAR(J),(WetDep(J,K),K=1,13)
      LINE=LINE+1
    END DO
    WRITE(6,303)(WetDepM(K),K=1,13)
    LINE=LINE+2
  end if

! Print out annual yield information
  CALL HEADIN()
  WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
  LINE=LINE+2
  WRITE(6,"(//30X,'Monthly Yield'/)")
  WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
  LINE=LINE+5
  DO J=1,NYRS
    WRITE(6,302)IYEAR(J),(MonYield(J,K),K=1,13)
    LINE=LINE+1
  END DO
  WRITE(6,303)(AnnYield(K),K=1,13)
  LINE=LINE+2

  IF (GWMiningQM(13).GT.0.5) THEN

    if (LINE>LinesPerPage-nyrs-5) then
      CALL HEADIN()
      WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
      LINE=LINE+2
    end if
    WRITE(6,"(//30X,'Monthly Groundwater Mining'/)")
    WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
    LINE=LINE+5
    DO J=1,NYRS
      WRITE(6,302)IYEAR(J),(GWMiningQ(J,K),K=1,13)
      LINE=LINE+1
    END DO
    WRITE(6,303)(GWMiningQM(K),K=1,13)
    LINE=LINE+2

    if (LINE>LinesPerPage-nyrs-5) then
      CALL HEADIN()
      WRITE(6,'(A)')Trim(CenterIt("OUTPUT TABLES",132))
      LINE=LINE+2
    end if
    WRITE(6,"(//30X,'Monthly Yield Minus Groundwater Mining'/)")
    WRITE(6,"(6X,A4,12(4X,A4,1X),5X,A4/)")(VAR(I),I=1,14)
    LINE=LINE+5
    DO J=1,NYRS
      WRITE(6,302)IYEAR(J),(YieldWMining(J,K),K=1,13)
      LINE=LINE+1
    END DO
    WRITE(6,303)(YieldWMiningM(K),K=1,13)
    LINE=LINE+2

  END IF
  RETURN
  END

!----------------------------------------------------------------------
!NAME PRNTQI
!----------------------------------------------------------------------
      SUBROUTINE PRNTQI
!      THIS SUBROUTINE WILL PRINT QINS
      use PARAMDIM
      use PrintStuff
      character, EXTERNAL :: ALLTRIM*255
      REAL*8 QINM(75,13),ROUND,X,SUM
      INTEGER*4 IX
!     ROUND FUNCTION
      ROUND(X,IX)= NINT(X*10.**IX)/10.**IX
      DO L=1,NQIN
!        ANNUAL SUMS
        DO J=1,NYRS
          SUM=0.
          DO K=1,12
            SUM=SUM+QIN(L,J,K)
          ENDDO
          QIN(L,J,13)=SUM
        END DO
!        COMPUTE MONTHLY AND ANNUAL MEANS
        DO K=1,13
          SUM=0.
          DO J=1,NYRS
            SUM=SUM+QIN(L,J,K)
          END DO
          QINM(L,K)=ROUND(REAL(SUM/FNYRS,KIND=8),int(0,kind=4))
        END DO
!        PRINT QINS
        if (LINE>LinesPerPage-NYRS-5.OR.L.EQ.1) then
          CALL HEADIN
        END IF
        WRITE (6,"(/10X,'QIN(',I2,')  INPUT ACRE-FT',2X,A90/)") &
          L,AllTrim(CDESCR(L))
        LINE=LINE+2
        WRITE (6,"(6X,A4,12(4X,A4,1X),5X,A4/)") VAR
        DO J=1,NYRS
          WRITE (6,"(6X,I4,12F9.0,F10.0)") IYEAR(J),(QIN(L,J,K),K=1,13)
          LINE=LINE+1
        END DO
        WRITE (6,"(/,6X,'MEAN',12F9.0,F10.0)") (QINM(L,K),K=1,13)
        LINE=LINE+2
      END DO
      RETURN
      END
!----------------------------------------------------------------------
!NAME RACE
!----------------------------------------------------------------------
      SUBROUTINE RACE (LL,VL,AR,EL)
      use PARAMDIM
      use COMMO
      use PrintStuff
      !----------------------------------------------------------------
      !Given reservoir volume, calculates reservoir area and elevation
      !----------------------------------------------------------------
      !COMMON/RVAE/V(MRES,400),A(MRES,400),E(MRES,400),NP(MRES)
      !REAL*8 V,A,E,XX,VL,EL,XFACT,AR
      REAL*8 XX,VL,EL,XFACT,AR
      LOGICAL (KIND=1) :: IsFound,Exceeded(MRES)
      DATA Exceeded/MRES*.FALSE./
      Character (LEN=3) :: Response
      INTEGER*4 M,LL,JJ,KK
      XX=VL
      IsFound=.FALSE.
      DO M=2,NP(LL)
        IF (XX.LT.V(LL,M))THEN
          IF(V(LL,M)-V(LL,M-1).EQ.0)THEN
             XFACT=0
          ELSE
             XFACT=(XX-V(LL,M-1))/(V(LL,M)-V(LL,M-1))
          ENDIF
          EL= E(LL,M-1) + (E(LL,M)-E(LL,M-1))*XFACT
          AR= A(LL,M-1) + (A(LL,M)-A(LL,M-1))*XFACT
          IsFound=.TRUE.
          EXIT
        ELSE IF(XX.EQ.V(LL,M))THEN
          EL = E(LL,M)
          AR = A(LL,M)
          IsFound=.TRUE.
          EXIT
        END IF
      END DO
      IF (.NOT.IsFound.AND..NOT.Exceeded(LL)) THEN
        !The volume searched for exceeds the greated value in the table
        IF (J.GT.NYRS) THEN
          JJ=1
          KK=1
        ELSE
          JJ=J
          KK=K
        END IF
        WRITE(6,"(10X,'Stage Area Capacity table exceeded for reservoir '," // &
          "I3,' in month ',I3,' year ',I3,' volume ',F12.2)") &
          LL,KK,JJ,VL
        WRITE(*,*)'Stage Area Capacity table exceeded for reservoir ',LL, &
          ' in month ',KK,' year ',JJ,' volume ',VL
        WRITE(*,*)'Continue with the simulation? (y or n)'
        WRITE(*,*)' '
        READ(*,*)Response
        IF (Response(1:1).NE."y".AND.Response(1:1).NE."Y") THEN
          STOP
        ELSE
          Exceeded(LL)=.TRUE.
        END IF
      END IF
      IF (.NOT.IsFound) THEN
        M=NP(LL)
        XFACT=(XX-V(LL,M-1))/(V(LL,M)-V(LL,M-1))
        EL= E(LL,M-1) + (E(LL,M)-E(LL,M-1))*XFACT
        AR= A(LL,M-1) + (A(LL,M)-A(LL,M-1))*XFACT
      END IF
      RETURN
      END

!----------------------------------------------------------------------
!NAME RESR
!----------------------------------------------------------------------
      SUBROUTINE RESR (IR,II,NI)
      use PARAMDIM
      use COMMO
      use PrintStuff
      INTEGER*4 NI,II(NI),IR,M,LL
      EXTERNAL QRICALC
      REAL*8 QRICALC,QRI,QDVM2,QDVM3,QDVM4,QADD
      !QDVM2 is the spill from the reservoir after initialization with
      ! inflows and evaporation.
      !QDVM is the amount drawn or spilled from the reservoir available for
      ! satisfying demands.
      QDVM=0.0
      QDVM2=0.0
      IF(.NOT.DRAWN(IR))THEN
          !If the reservoir hasn't been initialized, initialize it.
          QX(IQTG(IR,1),J,K)=0.0
          CALL REINI(IR)
          QDVM=MIN(DMD,QX(IQTG(IR,1),J,K))
          QDVM2=QDVM
      ENDIF
      !CALLED(IR)=.TRUE.
      IF (QDVM.LT.DMD) THEN
        QDVM3=MAX(0.0,MIN(MAX(DMD-QDVM,0.0),STO(IR)-SMN(IR)))+QDVM
        STO(IR)=STO(IR)-QDVM3
        QDVM=QDVM+QDVM3
      END IF
!     ROUT WATER TO AREAS
      DO M=1,NI
        LL=II(M)
        !IF (M.EQ.1) THEN
        !  QADD=QDVM
        !ELSE
        !  QADD=QDVM3
        !END IF
        IF (LL.GT.0) THEN
          QX(LL,J,K)=QX(LL,J,K)+QDVM
        END IF
      ENDDO
      ST(IR,J,K)=STO(IR)
      RETURN
      END

!----------------------------------------------------------------------
!NAME RESRR
!----------------------------------------------------------------------
      SUBROUTINE RESRR(IR,II,NI,IR2)
      use PARAMDIM
      use PrintStuff
      use COMMO
      INTEGER*4 NI,II(NI),IR,IR2
      CALL RESR(IR,II,NI)
      IF(CALLED(IR2))THEN
          STO(IR2)=STO(IR2)+QDVM
      ENDIF
      RETURN
      END

!----------------------------------------------------------------------
!NAME RESV
!----------------------------------------------------------------------
      SUBROUTINE RESV(IR,QRI,QRO)
      use PARAMDIM
      use COMMO
      use PrintStuff
      INTEGER*4 IR,IY,IXP,KK,JJ,IND
      REAL*8 DAT(13),QRI,QRO,VALU,AR,DSMX,DIFF,EVUSE,SEEPAGE,SEEP,AVAIL
      REAL (KIND=8) :: MINSTO,STONOW,ThisFactor
      CHARACTER*80 HEADC
!     COMPUTE AND LIMIT EVAPORATION
      AR=MAX(0.0,RAR(IR))
      IF (EvapFactor(IR).LE.0.0) THEN
        ThisFactor=1.0
      ELSE
        ThisFactor=EvapFactor(IR)
      END IF
      IF(.NOT.DRAWN(IR))THEN
         IF(EvapOpt(IR).EQ.2.AND.EvapQIN(IR).GT.0)THEN
            EVAP(IR,J,K)= QIN(EvapQIN(IR),J,K)*AR/12.*ThisFactor
         ELSE IF(EvapOpt(IR).EQ.1)THEN
            EVAP(IR,J,K)=EvapFac(IR,K)*AR*ThisFactor
         ELSE IF(EvapOpt(IR).EQ.3.AND.EvapQIN(IR).GT.0)THEN
            EVAP(IR,J,K)=QIN(EvapQIN(IR),J,K)*ThisFactor
         ELSE
            EVAP(IR,J,K)=EVRT(IR,J,K)*AR*ThisFactor
         ENDIF
         EVAP(IR,J,K)=MIN(EVAP(IR,J,K),MAX(0.0,STO(IR)+QRI))
         EVUSE=EVAP(IR,J,K)
         SEEPAGE=SEEP(IR)
      ELSE
         SEEPAGE=0.0
         EVUSE=0.0
      ENDIF
      IF (TargetRelOpt(IR).GT.0) THEN
        IREL(IR)=.TRUE.
      ELSE
        IREL(IR)=.FALSE.
      END IF
      IF (.NOT.DRAWN(IR)) THEN
        MINSTO=MIN(STO(IR)-EVAP(IR,J,K)+QRI,SMN(IR))
      ELSE
        MINSTO=MIN(STO(IR)+QRI,SMN(IR))
      END IF
      IF(TargetStorOpt(IR).EQ.1)THEN
         RSTORE(IR,J,K)=MAX(StorTarg(IR,K),MINSTO)
      ELSE IF (TargetStorOpt(IR).EQ.2) THEN
         RSTORE(IR,J,K)=MAX(QIN(TargetStorQIN(IR),J,K),MINSTO)
      END IF
      IF (TargetStorOpt(IR).GT.0) THEN
        ISTOR(IR)=.TRUE.
      ELSE
        ISTOR(IR)=.FALSE.
      END IF
      IF (.NOT.DRAWN(IR)) THEN
        AVAIL=MAX(0.0,STO(IR)+QRI-EVAP(IR,J,K)-SMN(IR))
      ELSE
        AVAIL=MAX(0.0,STO(IR)+QRI-SMN(IR))
      END IF
      IF(TargetRelOpt(IR).EQ.1)THEN
         RELEAS(IR,J,K)=MIN(QRMN(IR,K),AVAIL)
      ELSE IF (TargetRelOpt(IR).EQ.2) THEN
         RELEAS(IR,J,K)=MIN(AVAIL,QIN(TargetRelQIN(IR),J,K),AVAIL)
      ENDIF
!      COMPUTE AND LIMIT OUTFLOW
!       MAXIMUM RELEASE
!       MAXIMUM OUTFLOW
      IF(IREL(IR))THEN
         DSMX=MAX(0.0,STO(IR)+QRI-EVUSE-SMN(IR))
         QRO=MAX(0.0,MAX(MIN(DSMX,RELEAS(IR,J,K)), &
            STO(IR)+QRI-EVUSE-SMX(IR)))
      ELSE
         QRO=MAX(0.0,STO(IR)+QRI-EVUSE-SMX(IR),MIN(STO(IR)+QRI-EVUSE, &
               QRMN(IR,K)+SEEPAGE))
      ENDIF
!      COMPUTE CHANGE IN STORAGE
      STO(IR)=STO(IR)+QRI-EVUSE-QRO
      IF(ISEPQ(IR).GT.0.AND..NOT.DRAWN(IR))THEN
         QX(ISEPQ(IR),J,K)=MIN(STO(IR),SEEPAGE+QSM(IR,K))
         QRO=QRO-QX(ISEPQ(IR),J,K)
      ENDIF
      IF(ISTOR(IR))THEN
         IF(STO(IR).GT.RSTORE(IR,J,K))THEN
            DIFF=MAX(0.0,STO(IR)-RSTORE(IR,J,K))
            STO(IR)=STO(IR)-DIFF
            QRO=QRO+DIFF
         ENDIF
      ENDIF
      ST(IR,J,K)=STO(IR)
      DRAWN(IR)=.TRUE.
      RETURN
      END

!----------------------------------------------------------------------
!NAME RTFLO
!----------------------------------------------------------------------
      SUBROUTINE RTFLO (IL,QDV,RTF)
      use PARAMDIM
      use PrintStuff
      use COMMO
      INTEGER*4 IL,M,II,IM
      REAL*8 QDV,RTF
      IF (IL.EQ.0) THEN
         DO II=1,MLAND
            DO M=1,11
               STEM(II,M)=0.0
               SSTO(II,M)=0.0
            ENDDO
         ENDDO
      ELSE IF(IL.GT.0)THEN
         DO IM=1,10
            STEM(IL,IM)=SSTO(IL,IM)
            SSTO(IL,IM)=SSTO(IL,IM)+QDV*PCRF(IL,IM)
         ENDDO
         RTF=SSTO(IL,1)
         DO IM=1,10
            IF (IM.LT.10) THEN
              SSTO(IL,IM)=SSTO(IL,IM+1)
            ELSE
              SSTO(IL,IM)=0.0
            END IF
         ENDDO
      ELSE IF(IL.LT.0)THEN
         DO IM=1,10
            SSTO(-IL,IM)=STEM(-IL,IM)
         ENDDO
      ENDIF
      RETURN
      END
!----------------------------------------------------------------------
!NAME SORT
!----------------------------------------------------------------------
      SUBROUTINE SORT(N)
      use PARAMDIM
      COMMON /FLOW/ Y
      INTEGER*4 N,M,KL,JJ,I,LL
      REAL*8 Y(MPTS),B
      M=N+1
   10 M=M/2
      IF(M) 20,20,11
   11 KL=N-M
      JJ=1
   12 I=JJ
   13 LL=I+M
      IF(Y(LL).GT.Y(I)) GO TO 15
   14 JJ=JJ+1
      IF(JJ.GT.KL) GO TO 10
      GO TO 12
   15 B=Y(I)
      Y(I)=Y(LL)
      Y(LL)=B
      I=I-M
      IF(I-1) 14,14,13
   20 IF (Y(2).LT.Y(1)) GO TO 21
      B=Y(1)
      Y(1)=Y(2)
      Y(2)=B
   21 RETURN
      END
!----------------------------------------------------------------------
!GETQIN
!This program reads inflow files and places them in the correct QIN
!array to be retrieved for calculations within the program
!Modified 8/11/2010 by Craig Miller to accomodate
!ModelData files
!Modified 12/13/2010 by Craig Miller to allow recursive reading of
! correlated files (correlations of correlations)
!----------------------------------------------------------------------
      SUBROUTINE GETQIN
!     READ QIN Files FROM DISK
      use PARAMDIM
      use PrintStuff
      use COMMO
      use F90SQLConstants
      use F90SQL
      use F90SQLVARIABLES
      use FSQL_DATA
      INTEGER*4 locComma(5),posComma,charLen,IENYR
      INTEGER*4 locSlash,locDot,iCnt,iTemp,lenDec
      CHARACTER*255 AllTrim
      CHARACTER*200 StrmName,GaugeID,GaugeFile,charCoeff,readFmt,trimFile
      CHARACTER*200 leadDig,Decimals,exportFiles,importFiles,strmFile
      REAL (KIND=8) :: GaugeCoeff,FlowRead(MYEAR,13),GaugeLarge,GaugeDec
      REAL*8 QI(13)
      integer(SQLHENV_KIND):: EnvHndl
      integer(SQLHDBC_KIND):: ConnHndl 
      integer(SQLRETURN_KIND)::iRet
      integer(SQLSMALLINT_KIND)::FIELDNumber,ConnStrLength
      character(len=MaxStringLen):: ConnStrOut,tempString,SurfFile
      if (atHome) then
        SurfFile="C:\Work\WATBUDG\Surface.mdb"
      ELSE
        SurfFile="O:\DATABASE\WATBUDG\Surface.mdb"
      end if
      ConnStr='DBQ='//trim(SurfFile)// &
        ';DRIVER={Microsoft Access Driver (*.mdb)}'
      !************************************************************************
      call initializeaccessvars()
      Table="Correlations"
      WillOpen=.TRUE.
      WillClose=.FALSE.
      indexstring="WHERE GageID=''"
      CALL AccessRead()
      WillOpen=.FALSE.
      !iRet=SQL_SUCCESS
      !allocate an environment handle
      !call f90SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, EnvHndl, iRet)

      !Set ODBC version to use (3.x in this case)
      !call f90SQLSetEnvAttr(EnvHndl, SQL_ATTR_ODBC_VERSION, SQL_OV_ODBC3, iRet)

      !Allocate a connection handle
      !call f90SQLAllocHandle(SQL_HANDLE_DBC,EnvHndl, ConnHndl, iRet)

      !Note that we use a file-connection instead of a DSN connection
      !open a connection to the excel workbook
      !call f90SQLDriverConnect(ConnHndl, f90SQL_NULL_PTR, ConnStr, &
      !  ConnStrOut, ConnStrLength, SQL_DRIVER_COMPLETE,iRet)
      !************************************************************************
      DO L=1,NQIN
        Needed=.TRUE.
        !Test to see if this is a StreamStats or Area Altitude file
        !If there is a comma in the file name then, YES! it is.
        IF (INDEX(Trim(AllTrim(QFILE(L))),",")>0) THEN
          !Find out comma locations to determine how to parse
          QFILE(L)=Trim(AllTrim(QFILE(L)))
          locComma(1)=INDEX(QFILE(L),",")
          locComma(2)=INDEX(QFILE(L)((locComma(1)+1):),",")+locComma(1)
          locComma(3)=INDEX(QFILE(L),"(")-1
          locComma(4)=INDEX(QFILE(L)((locComma(3)+2):),"(")+locComma(3)
          locComma(5)=INDEX(QFILE(L)((locComma(4)+1):),",")+locComma(4)
          !StrmName is not used, only here for debugging
          StrmName=QFILE(L)(1:(locComma(1)-1))
          !Read in base gauge station
          GaugeID=QFILE(L)((locComma(1)+1):(locComma(2)-1))
          !GaugeFile=trim(BasePath) // TRIM(GaugeID) // ".MON"
          Call ReadFile(GaugeID,FlowRead)
          !Create a format statement to read the factor charCoeff
          charCoeff=AllTrim(QFILE(L)((locComma(2)+1):(locComma(3)-1)))
          charLen=LEN_TRIM(charCoeff)
          posComma=INDEX(charCoeff,".")
          if (posComma.eq.0) then
            charCoeff=trim(AllTrim(charCoeff)) // ".0"
            posComma=INDEX(charCoeff,".")
            charLen=charLen+2
          end if
          GaugeCoeff=0.0
          GaugeLarge=0.0
          GaugeDec=0.0
          do i=1,charLen
            if (i<posComma.AND.posComma>0.OR.posComma.eq.0) then
              GaugeLarge=GaugeLarge*10.0+REAL(iachar(charCoeff(i:i))-48, &
                KIND=8)
            else if (i>posComma) then
              GaugeDec=GaugeDec+REAL(iachar(charCoeff(i:i))-48,KIND=8)* &
                10.0**(-(i-posComma))
            end if
          end do
          GaugeCoeff=GaugeLarge+GaugeDec
          !Set the QIN to base gauge times the fraction
          DO J=1,NYRS
            DO K=1,13
              QIN(L,J,K)=FlowRead(J,K)*GaugeCoeff
            END DO
          END DO
          !Add the import files
          if (locComma(3)+2<locComma(4)-2) then
            !Extract individual import files and add to QIN
            importFiles=QFILE(L)((locComma(3)+2):locComma(4)-2)
            posComma=INDEX(exportFiles,",")
            if (LEN_TRIM(importFiles)>0) then
              DO
                if (posComma>0) then
                  GaugeID=importFiles(1:(posComma-1))
                  importFiles=importFiles((posComma+1):)
                else
                  GaugeID=importFiles
                  importFiles=""
                end if
                Call ReadFile(GaugeID,FlowRead)
                DO J=1,NYRS
                  DO K=1,13
                    QIN(L,J,K)=QIN(L,J,K)+FlowRead(J,K)
                  END DO
                END DO
                if (LEN_TRIM(importFiles)<1) then
                  EXIT
                else
                  posComma=INDEX(importFiles,",")
                end if
              END DO
            end if
          end if
          !Subtract export files
          if (charLen-locComma(4)-3>0.OR.locComma(4)+2<locComma(5)-2) then
            if (locComma(5).GT.locComma(4)) then
              exportFiles=QFILE(L)((locComma(4)+2):locComma(5)-2)
            else
              exportFiles=QFILE(L)((locComma(4)+2):(charLen-1))
            end if
            if (LEN_TRIM(exportFiles)>0) then
              posComma=INDEX(exportFiles,",")
              DO
                if (posComma>0) then
                  GaugeID=exportFiles(1:(posComma-1))
                  exportFiles=exportFiles((posComma+1):)
                else
                  GaugeID=trim(AllTrim(exportFiles))
                  exportFiles=""
                end if
                Call ReadFile(GaugeID,FlowRead)
                DO J=1,NYRS
                  DO K=1,13
                    QIN(L,J,K)=QIN(L,J,K)-FlowRead(J,K)
                  END DO
                END DO
                if (LEN_TRIM(exportFiles)<1) then
                  EXIT
                else
                  posComma=INDEX(exportFiles,",")
                end if
              END DO
            end if
          end if
        ELSE
          trimFile=Trim(AllTrim(QFILE(L)))
          locSlash=INDEX(trimFile,"\",.TRUE.)
          if (locSlash>0) then
            GaugeID=trimFile((locSlash+1):)
          else
            GaugeID=trimFile
          end if
          locDot=INDEX(GaugeID,".",.TRUE.)
          IF (locDot>0) THEN
            GaugeID=GaugeID(1:(locDot-1))
          END IF
          Call ReadFile(QFILE(L),FlowRead)
          DO J=1,NYRS
            QIN(L,J,13)=0.0
            DO K=1,12
              IF (NotNegative(L)) THEN
                QIN(L,J,K)=max(0.0,FlowRead(J,K))
              ELSE
                QIN(L,J,K)=FlowRead(J,K)
              END IF
              QIN(L,J,13)=QIN(L,J,13)+QIN(L,J,K)
            END DO
          END DO
        END IF
      END DO
      !************************************************************************
      !disconnect
      call initializeaccessvars()
      Table="Correlations"
      WillClose=.TRUE.
      NDPFIELD=1
      Field(1)="A01"
      indexstring="WHERE GageID=''"
      CALL AccessRead()
      !call f90SQLDisconnect(ConnHndl,iRet)
      !release connection handle
      !call f90SQLFreeHandle(SQL_HANDLE_DBC,ConnHndl,iRet)
      !release environment handle
      !call f90SQLFreeHandle(SQL_HANDLE_ENV, EnvHndl, iRet)
      !************************************************************************
      RETURN
      END

!----------------------------------------------------------------------
!ReadFile
!----------------------------------------------------------------------
!If optional Base is not true, loads QIN(L,j,k) with data from FileIn
!If Base is true, loads InflowQ with values from FileIn
!----------------------------------------------------------------------
    RECURSIVE Subroutine ReadFile(FileIn,InflowQ)
    use PARAMDIM
    use PrintStuff
    use COMMO
    use F90SQLConstants
    use F90SQL
    use F90SQLVARIABLES
    use FSQL_DATA
    CHARACTER(len=200),INTENT(IN) :: FileIn
    REAL (KIND=8),INTENT(INOUT) :: InflowQ(MYEAR,13)
    character, EXTERNAL :: ALLTRIM*255
    CHARACTER(len=255)::SurfFile,FileTitle,trimFile,GaugeID,CorrFile
    CHARACTER(len=5) :: MonRead(12)
    CHARACTER(len=8)::BaseFile
    INTEGER (KIND=4) :: LL,KK,iLoc
    LOGICAL :: Base
    LOGICAL (KIND=2) :: isBase,IsRead(MYEAR),IsGood(MYEAR,13)
    INTEGER*4 IENYR,IYR,IEXP,ICOUNT,nLog,locSlash,locDot
    REAL*8 QI(13),Acoef(13),Bcoef(13),BaseFlow(MYEAR,13),thisFlow
    IsRead=.FALSE.
    IsGood=.FALSE.
    IENYR=NYRS+INYR-1
    BaseFlow=0.0
    iLoc=INDEX(Trim(AllTrim(FileIn)),":")
    if (iLoc.EQ.0) then
      SurfFile=trim(BasePath) // trim(AllTrim(FileIn))  // ".mon"
    ELSE
      SurfFile=trim(AllTrim(FileIn))
    end if
     OPEN(1,FILE=SurfFile,STATUS='OLD',ERR=5)
     GO TO 7
5    CONTINUE
     WRITE(6,*)'CANNOT OPEN QIN FILE '//trim(AllTrim(FileIn))
     WRITE(*,*)'CANNOT OPEN QIN FILE '//trim(AllTrim(FileIn))
     ISTOP=.TRUE.
7    CONTINUE
     READ(1,"(A80)") FileTitle
     J=0
     ICOUNT=0
     DO WHILE(.TRUE.)
        READ (1,"(8X,I4,I2,12A5,F6.0)",END=10) IYR,IEXP,MonRead, QI(13)
        IF(IYR.LT.INYR) CYCLE
        IF(IYR.GT.IENYR) CYCLE
        J = IYR - INYR + 1
        ICOUNT=ICOUNT+1
        IsRead(J)=.TRUE.
        Needed(J)=.FALSE.
        IsGood(J,13)=.TRUE.
        DO KK=1,12
           IF (Trim(MonRead(KK)).NE.'') THEN
             IsGood(J,KK)=.TRUE.
             READ(MonRead(KK),"(F5.0)")QI(KK)
             InflowQ(J,KK)=QI(KK)*10.**IEXP
             InflowQ(J,13)=InflowQ(J,13)+InflowQ(J,KK)
           ELSE
             IsGood(J,13)=.FALSE.
             IF (.NOT.Needed(J)) THEN
               ICOUNT=ICOUNT-1
             END IF
             Needed(J)=.TRUE.
           END IF
        ENDDO
     ENDDO
10   CONTINUE
     CLOSE(1)
     IF(ICOUNT.LT.NYRS)THEN
      DO J=1,NYRS
        IF (Needed(J)) THEN
          EXIT
        END IF
        IF (J.EQ.NYRS) THEN
          RETURN
        END IF
      END DO
      trimFile=Trim(AllTrim(FileIn))
      locSlash=INDEX(trimFile,"\",.TRUE.)
      if (locSlash>0) then
        GaugeID=trimFile((locSlash+1):)
      else
        GaugeID=trimFile
      end if
      locDot=INDEX(GaugeID,".",.TRUE.)
      IF (locDot>0) THEN
        GaugeID=GaugeID(1:(locDot-1))
      END IF
      call initializeaccessvars()
      Table="Correlations"
      NLOGICFIELD=1
      logicfields(1)="LogLogReg"
      NSTRINGFIELD=1
      STRINGFIELDS(1)="CorrWith"
      NDPFIELD=24
      Field(1)="A01"
      Field(2)="A02"
      Field(3)="A03"
      Field(4)="A04"
      Field(5)="A05"
      Field(6)="A06"
      Field(7)="A07"
      Field(8)="A08"
      Field(9)="A09"
      Field(10)="A10"
      Field(11)="A11"
      Field(12)="A12"
      Field(13)="B01"
      Field(14)="B02"
      Field(15)="B03"
      Field(16)="B04"
      Field(17)="B05"
      Field(18)="B06"
      Field(19)="B07"
      Field(20)="B08"
      Field(21)="B09"
      Field(22)="B10"
      Field(23)="B11"
      Field(24)="B12"
      indexstring = "WHERE GageID='" // Trim(AllTrim(GaugeID)) // "'"
      call AccessRead()
      DO i = 1,KOUNTFSQL
        BaseFile=trim(AllTrim((F90SQLSTRINGS(i,1))))
        nLog=F90SQLINTEGER(i,1)
        DO KK=1,12
          Acoef(KK)=XI(i,KK)
          Bcoef(KK)=XI(i,KK+12)
        END DO
      END DO
      IF (KOUNTFSQL>0) THEN
        isBase=.FALSE.
        CorrFile=trim(AllTrim(BaseFile))
        Call ReadFile(CorrFile,BaseFlow)
        DO J=1,NYRS
          BaseFlow(J,13)=0.0
          IF (.NOT.IsRead(J).OR..NOT.IsGood(J,13)) THEN
            InflowQ(J,13)=0.0
            DO KK=1,12
              IF (.NOT.IsGood(J,KK)) THEN
                IF(nLog.NE.0) then
                  thisFlow=0.0
                  if (BaseFlow(J,KK)>0.0) then
                    thisFlow=BaseFlow(J,KK)**BCoef(KK)
                  end if
                  InflowQ(J,KK)=Acoef(KK)*thisFlow
                ELSE
                  InflowQ(J,KK)=MAX(0.0, &
                    BaseFlow(J,KK)*Bcoef(KK)+Acoef(KK))
                end if
              END IF
              InflowQ(J,13)=InflowQ(J,13)+InflowQ(J,KK)
              BaseFlow(J,13)=BaseFlow(J,13)+BaseFlow(J,KK)
            END DO
            Needed(J)=.FALSE.
            ICOUNT=ICOUNT+1
          END IF
        END DO
      ELSE
        WRITE(0,"(' YEARS OF RECORD NOT FOUND IN QIN FILE:',A"// &
              "/1X,A)") trim(AllTrim(FileIn)),FileTitle
        WRITE(6,"(' YEARS OF RECORD NOT FOUND IN QIN FILE:',A"// &
              "/1X,A)") trim(AllTrim(FileIn)),FileTitle
        ISTOP=.TRUE.
      END IF
     END IF
    IF (ICOUNT<NYRS) THEN
      WRITE(0,"(' YEARS OF RECORD NOT FOUND IN QIN FILE:',A"// &
            "/1X,A)") trim(AllTrim(FileIn)),FileTitle
      WRITE(6,"(' YEARS OF RECORD NOT FOUND IN QIN FILE:',A"// &
            "/1X,A)") trim(AllTrim(FileIn)),FileTitle
      IStop=.TRUE.
    END IF
    QINAM(L)=FileTitle
RETURN
END
!----------------------------------------------------------------------
!     SUBROUTINE CRPUSE
!----------------------------------------------------------------------
      SUBROUTINE CRPUSE (M,BEGSEA,SES,CPFCT,IDM,USEFAC)
      INTEGER*4 M,IDM,IP,NUM,ND
      REAL*8 CPFCT(30),BEGSEA,SES,USEFAC,ENDSEA,P,RNUM
      REAL*8 PSES,RIN,PART
      ENDSEA = BEGSEA + SES
      NUM = 15
      USEFAC = 0.0
      DO 1500  IP = 1,NUM
      P = IP
      RNUM = NUM
      PSES = M + (P-1)/(RNUM-1)
      IF(PSES.LE.ENDSEA.AND.PSES.GE.BEGSEA)THEN
      RIN = (IDM - 1) * (PSES - BEGSEA) / SES + 1
      ND = RIN
      IF(ND.EQ.IDM) RIN = RIN -.0001
      PART =  RIN - ND
      USEFAC =((CPFCT(ND+1) - CPFCT(ND)) * PART +CPFCT(ND)) &
       / NUM + USEFAC
      END IF
 1500 CONTINUE
      RETURN
      END
!----------------------------------------------------------------------
!     SUBROUTINE LNPRT
!----------------------------------------------------------------------
      SUBROUTINE LNDPRT(ICRP,TITL,ACRE,CODE)
      use PARAMDIM
      use COMMO
      use PrintStuff
      REAL*8 ACRE,AVTEM(13),AVDEM(13),ETNET(13)
      REAL*8 EFP
      INTEGER*4 ICRP
      CHARACTER*(*) TITL,CODE
      DO 10 K = 1,13
      ETNET(K)=0.0
      AVPRE(K)=0.0
      AVTEM(K)=0.0
 10   AVDEM(K)=0.0
      IF (CODE(1:3).EQ.'IIF')THEN
         EFP=100.
      ELSE
         EFP=EFPRE(L)
      END IF
      DO J = 1,NYRS
         DO K = 1,12
            AVPRE(K)=AVPRE(K) + PRE(L,K,J)/FLOAT(NYRS)
            AVPRE(13)=AVPRE(13)+PRE(L,K,J)/FLOAT(NYRS)
            AVTEM(K)=AVTEM(K) + TEM(L,K,J)/FLOAT(NYRS)
            AVTEM(13)=AVTEM(13)+TEM(L,K,J)/(FLOAT(NYRS)*12.)
            AVDEM(K)=AVDEM(K) + CROPS(ICRP,J,K)/FLOAT(NYRS)
            AVDEM(13)=AVDEM(13)+CROPS(ICRP,J,K)/FLOAT(NYRS)
            ETNET(K)=ETNET(K)+(CROPS(ICRP,J,K)-PRE(L,K,J)*EFP/100.) &
            /FLOAT(NYRS)
            ETNET(13)=ETNET(13)+(CROPS(ICRP,J,K)-PRE(L,K,J)*EFP/100.) &
                  /FLOAT(NYRS)
         ENDDO
      ENDDO
      IF(LINE.GT.53)CALL HEADIN
      LINE = LINE+6
      WRITE(6,"(/' ',A50,' ACRES = ',F8.2)")TITL,ACRE
      WRITE(6,"(' KC',12X,12F8.2)")(CCOEF(ICRP,K),K=1,12)
      WRITE(6,50)(AVDEM(K),K=1,13)
  50  FORMAT(' ETP INCH',6X,12F8.2,2X,F8.2)
      WRITE(6,60)(ETNET(K),K=1,13)
 60   FORMAT(' NET INCH',6X,12F8.2,2X,F8.2)
      DO 65 K=1,13
      ETNET(K)=ETNET(K)/12.
 65   CONTINUE
      WRITE(6,70)(ETNET(K),K=1,13)
 70   FORMAT(' NET FEET',6X,12F8.2,2X,F8.2)
      RETURN
      END
!----------------------------------------------------------------------
!     SUBROUTINE FZER
!----------------------------------------------------------------------
      SUBROUTINE FZER(FLO)
      INTEGER*4 I
      REAL*8 FLO(13)
      DO 10 I=1,13
      FLO(I)=0.0
 10   CONTINUE
      RETURN
      END
!----------------------------------------------------------------------
!     SUBROUTINE IZER
!----------------------------------------------------------------------
      SUBROUTINE IZER(IQXFL)
      INTEGER*4 IQXFL(13),I
      DO 10 I=1,13
      IQXFL(I)=0
 10   CONTINUE
      RETURN
      END
!----------------------------------------------------------------------
!     SUBROUTINE TPRIN
!----------------------------------------------------------------------
      SUBROUTINE TPRIN(FLO,TIT,IDEC,FMT)
      USE PARAMDIM
      USE PRINTSTUFF
      character, EXTERNAL :: ALLTRIM*255
      CHARACTER FMT*(*),TIT*(*)
      REAL*8 FLO(13),OFLO(13)
      INTEGER*4 IDEC
      INTEGER*4 IQXFL(13)
      I=1
      DO I=1,13
         IF(IDEC.GT.0)THEN
            OFLO(I)=NINT(FLO(I)*10.**IDEC)/10.**IDEC
         ELSE
            IQXFL(I)=NINT(FLO(I)*10.**IDEC)/10.**IDEC
         END IF
      ENDDO
      IF(IDEC.GT.0)THEN
         WRITE(6,FMT)trim(AllTrim(TIT)),(OFLO(I),I=1,13)
      ELSE
         WRITE(6,FMT)trim(AllTrim(TIT)),(IQXFL(I),I=1,13)
      END IF
      LINE=LINE+1
      RETURN
      END
!----------------------------------------------------------------------
!     SUBROUTINE FAVER
!----------------------------------------------------------------------
      SUBROUTINE FAVER(AVE,FUNC,DIV1,DIV2,I)
      REAL*8 AVE(13),FUNC,DIV1,DIV2
      INTEGER*4 I
      AVE(I)=AVE(I)+FUNC/DIV1
      AVE(13)=AVE(13)+FUNC/DIV2
      RETURN
      END
!----------------------------------------------------------------------
!     SUBROUTINE ULINE
!----------------------------------------------------------------------
      SUBROUTINE ULINE(TITPRIN,N,LL)
      USE PARAMDIM
      USE COMMO
      USE PRINTSTUFF
      CHARACTER TITPRIN*(*),DASH*1(130),TIT*255
      INTEGER (KIND=4),INTENT(IN):: N,LL
      INTEGER (KIND=4) :: MM
      DATA (DASH(I),I=1,130)/'+',129*'_'/
      MM=LL
      IF(MM.LE.0)MM=LEN_TRIM(TITPRIN)
      IF (MM.GT.255) THEN
        TIT=TRIM(TITPRIN(1:255))
      ELSE
        TIT=TITPRIN
      END IF
      IF(N.GT.1)THEN
      !DO I=1,N-1
      !  WRITE(6,'('' '')')
      !  LINE=LINE+1
      !ENDDO
      END IF
      !IF(TIT.EQ.' ')THEN
      !   WRITE(6,'(130A1)')(DASH(I),I=1,LL+1)
      !   LINE=LINE+1
      !ELSE
      !   WRITE(6,'('' '',A/130A1)')TIT,(DASH(I),I=1,LL+1)
      !   LINE=LINE+2
      !END IF
      WRITE(6,'(/" ",A)')TRIM(TIT)
      LINE=LINE+2
      RETURN
      END
!----------------------------------------------------------------------
!     SUBROUTINE SPRIN
!----------------------------------------------------------------------
      SUBROUTINE SPRIN(TIT,IITRB,FLOWFAC,TRANSBASIN)
      use PARAMDIM
      use PrintStuff
      LOGICAL :: TRANSBASIN
      CHARACTER TIT*(*)
      REAL*8 SIGN1,FLOWFAC
      INTEGER*4 IITRB(*),LNUM,CNUM,LL,KK
      CALL ULINE(TIT,2,0)
      TFLO=0.0
      DO LL=1,19
          LNUM=IITRB(LL)
          IF(LNUM.NE.0)THEN
            CNUM=QxMap(abs(LNUM))
            SIGN1=LNUM/ABS(LNUM)
            LNUM=ABS(LNUM)
            QXFL=0.0
            DO KK=1,12
                !CALL FAVER(YIELD,SIGN1*QXM(LNUM,KK)*FLOWFAC,REAL(1.,KIND=8), &
                !  REAL(1.,KIND=8),KK)
                IF (TRANSBASIN) THEN
                  IF ((Trim(TranDir(LL)).EQ.'Import'.AND. &
                    INDEX(TIT,"IMPORT").GT.0))  THEN
                    IF (FLOWFAC.LT.0.0) THEN
                      CALL FAVER(TotInflow,SIGN1*QXM(LNUM,KK)*FLOWFAC, &
                        REAL(1.,KIND=8),REAL(1.,KIND=8),KK)
                      CALL FAVER(QXFL,SIGN1*QXM(LNUM,KK),REAL(1.,KIND=8), &
                        REAL(1.,KIND=8),KK)
                      CALL FAVER(QXTOT,SIGN1*QXM(LNUM,KK),REAL(1.,KIND=8), &
                        REAL(1.,KIND=8),KK)
                      CALL FAVER(TFLO,SIGN1*QXM(LNUM,KK),REAL(1.,KIND=8), &
                        REAL(1.,KIND=8),KK)
                    END IF
                  ELSEIF (Trim(TranDir(LL)).EQ.'Export'.AND. &
                    INDEX(TIT,"EXPORT").GT.0) THEN
                    IF (FLOWFAC.GT.0.0) THEN
                      CALL FAVER(TotInflow,SIGN1*QXM(LNUM,KK)*FLOWFAC, &
                        REAL(1.,KIND=8),REAL(1.,KIND=8),KK)
                      CALL FAVER(QXFL,SIGN1*QXM(LNUM,KK),REAL(1.,KIND=8), &
                        REAL(1.,KIND=8),KK)
                      CALL FAVER(QXTOT,SIGN1*QXM(LNUM,KK),REAL(1.,KIND=8), &
                        REAL(1.,KIND=8),KK)
                      CALL FAVER(TFLO,SIGN1*QXM(LNUM,KK),REAL(1.,KIND=8), &
                        REAL(1.,KIND=8),KK)
                    END IF
                  END IF
                ELSE
                  CALL FAVER(TotInflow,SIGN1*QXM(LNUM,KK)*FLOWFAC, &
                    REAL(1.,KIND=8),REAL(1.,KIND=8),KK)
                  CALL FAVER(QXFL,SIGN1*QXM(LNUM,KK),REAL(1.,KIND=8), &
                    REAL(1.,KIND=8),KK)
                  CALL FAVER(QXTOT,SIGN1*QXM(LNUM,KK),REAL(1.,KIND=8), &
                    REAL(1.,KIND=8),KK)
                  CALL FAVER(TFLO,SIGN1*QXM(LNUM,KK),REAL(1.,KIND=8), &
                    REAL(1.,KIND=8),KK)
                END IF
            ENDDO
            CALL TPRIN(QXFL,QXNAM(CNUM),0_4,'(4X,A,T42,12I7,I8)')
            LINE=LINE+1
          ENDIF
      ENDDO
      CALL TPRIN(TFLO,'SUBTOTAL',0_4,'(3X,A,T42,12I7,I8)')
      LINE=LINE+1
      RETURN
      END
!----------------------------------------------------------------------
!     SUBROUTINE ADDIN
!----------------------------------------------------------------------
      SUBROUTINE ADDIN(QXFLO,IITRB,SIGN2)
      use PARAMDIM
      use PrintStuff
      REAL*8 SIGN1,SIGN2,QXFLO(13)
      INTEGER*4 IITRB(*),LNUM,LL,KK
      DO LL=1,19
          LNUM=IITRB(LL)
          IF(LNUM.NE.0)THEN
              SIGN1=LNUM/ABS(LNUM)
              LNUM=ABS(LNUM)
              DO KK=1,12
                  QXFLO(K)=QXFLO(K)+SIGN1*QXM(LNUM,KK)*SIGN2
                  QXFLO(13)=QXFLO(13)+SIGN1*QXM(LNUM,KK)*SIGN2
              ENDDO
          ENDIF
      ENDDO
      RETURN
      END
!----------------------------------------------------------------------
!     REAL FUNCTION FROST
!----------------------------------------------------------------------
      REAL*8 FUNCTION FROST(BMON,BDAY)
      REAL*8 DAYS(12),BMON,BDAY,DYS
      INTEGER*4 IN
      INTEGER:: IA
      DATA (DAYS(IA),IA=1,12)/31,28,31,30,31,30,31,31,30,31,30,31/
      FROST=0.0
      IF(BMON.GT.0)THEN
         IN=BMON
         DYS=DAYS(IN)
         FROST=BMON+BDAY/DYS
      END IF
      RETURN
      END
!-----------------------------------------------------------------------
!     SUBROUTINE AVER2(N,M,VARI,FACT)
!-----------------------------------------------------------------------
      SUBROUTINE AVER2(N,M,VARI,FACT)
      use PARAMDIM
      use COMMO
      use PrintStuff
      INTEGER*4 N,M
      REAL*8 VARI(M,13),FACT
      VARI(N,K)=VARI(N,K)+FACT/FLOAT(NYRS)
      VARI(N,13)=VARI(N,13)+FACT/FLOAT(NYRS)
      RETURN
      END
!-----------------------------------------------------------------------
!     SUBROUTINE ARRANG(IINFL,NINFL)
!-----------------------------------------------------------------------
      SUBROUTINE ARRANG(IINFL,NUMFLOW,NINFL)
      INTEGER*4 numflow,IINFL(NUMFLOW),NINFL,IMAX,II,I
      IMAX=19
      NINFL=0
      DO 10 I=1,IMAX-1
      IF(IINFL(I).EQ.0)THEN
         DO 20 II=I+1,IMAX
            IF(IINFL(II).NE.0)THEN
               IINFL(I)=IINFL(II)
               IINFL(II)=0
               NINFL=NINFL+1
            ENDIF
  20     CONTINUE
      ELSE
         NINFL=NINFL+1
      ENDIF
  10  CONTINUE
      RETURN
      END

!-------------------------------------------------------------------------
!     FUNCTION QRICALC
!-------------------------------------------------------------------------
      REAL*8 FUNCTION QRICALC(IR)
      use PARAMDIM
      use COMMO
      use PrintStuff
      INTEGER*4 IR,II,IQXI
      QRICALC=0.0
      DO II=1,19
         IQXI=IQXAD(IR,II)
         IF(IQXI.GT.0)THEN
            QRICALC=QRICALC+QX(IQXI,J,K)
         ENDIF
      ENDDO
      RETURN
      END

!----------------------------------------------------------------------
!     ALLTRIM is a character function used to trim leading and trailing
!     blanks.
!----------------------------------------------------------------------
  CHARACTER (LEN=255) FUNCTION ALLT(CHARIN)
  use F90SQLVARIABLES
  CHARACTER*(*) CHARIN
  INTEGER*4 SM,BG,THELEN
  DATA SM,BG/33,127/
  INTEGER*4 I,ILEN,IBEG,ILEN2,J,IVAL,ILAST
  CHARACTER*255 RES,TheLetter*1
  ILEN=LEN(trim(CHARIN))
  ILAST=ILEN
  RES=CHARIN
  DO I=ILEN,1,-1
     TheLetter=RES(I:I)
     IVAL=IACHAR(RES(I:I))
     IF (IACHAR(RES(I:I)).LE.SM .OR.IACHAR(RES(I:I)).GE.BG) THEN
        RES(I:I)=" "
     END IF
     IF (IACHAR(RES(I:I)).LE.SM .OR.IACHAR(RES(I:I)).GE.BG) THEN
        ILAST=MAX(1,I-1)
     ELSE
        EXIT
     END IF
  END DO
  ILEN=LEN(trim(RES))
  IBEG=1
  DO I=1,ILAST
     IF (IACHAR(RES(I:I)).GE.SM .AND.IACHAR(RES(I:I)).LT.BG) THEN
        EXIT
     ELSE
        IBEG=MIN(I+1,ILEN)
     END IF
  ENDDO
  ALLT=trim(RES(IBEG:ILAST))
  RETURN
  END

  CHARACTER (LEN=255) FUNCTION LeftAdj(CHARIN,theLen)
  CHARACTER (LEN=255) :: ALLT
  CHARACTER (LEN=*),INTENT(IN) :: CHARIN
  Integer (Kind=4),INTENT(IN) :: theLen
  Integer (KIND=4) :: charLen,I,SM,BG,Ival
  DATA SM,BG/31,127/
  CHARACTER (LEN=255) :: CharOut,TheLetter*1
  charLen=len_trim(ALLT(CHARIN))
  CharOut=trim(ALLT(CHARIN))
  DO I=1,charLen
    TheLetter=CharOut(I:I)
    Ival=IACHAR(TheLetter)
    IF (IVAL.LE.SM.OR.IVAL.GE.BG) THEN
      charLen=max(1,i-1)
      CharOut=CharOut(1:charLen)
      EXIT
    END IF
  END DO
  IF (charLen<theLen) THEN
    DO I=charLen+1,theLen
      CharOut(I:I)=" "
    END DO
  END IF
  if (charLen>theLen) then
    LeftAdj=CharOut(1:theLen)
  else
    LeftAdj=CharOut(1:charLen)
    DO I=charLen+1,theLen
      LeftAdj(I:I)=" "
    END DO
  end if
  RETURN
  END

  REAL (KIND=8) FUNCTION BRENT(AX,BX,CX,F,TOL,XMIN)
  !PARAMETER (ITMAX=100,CGOLD=.3819660,ZEPS=1.0E-10)
  INTEGER (KIND=4),PARAMETER :: ITMAX=100
  REAL (KIND=8),PARAMETER:: CGOLD=.3819660
  REAL (KIND=8),PARAMETER:: ZEPS=1.0E-10
      INTEGER (KIND=4) :: ITCOPY
      REAL (KIND=8) :: XMIN,TOL,F,AX,BX,CX,A,B,V,W,X,E,FX,FV,XM,FW
      EXTERNAL F
      REAL (KIND=8) :: TOL1,TOL2,R,Q,P,ETEMP,D,U,H1I,CN,FU
      INTEGER (KIND=4) :: ITER
      A=MIN(AX,CX)
      B=MAX(AX,CX)
      V=BX
      W=V
      X=V
      E=0.
      FX=F(X)
      FV=FX
      FW=FX
      DO ITER=1,ITMAX
        ITCOPY=ITER
        XM=0.5*(A+B)
        TOL1=TOL*ABS(X)+ZEPS
        TOL2=2.*TOL1
        IF(ABS(X-XM).LE.(TOL2-.5*(B-A))) THEN
          EXIT
        END IF
        IF(ABS(E).GT.TOL1) THEN
          R=(X-W)*(FX-FV)
          Q=(X-V)*(FX-FW)
          P=(X-V)*Q-(X-W)*R
          Q=2.*(Q-R)
          IF(Q.GT.0.) P=-P
          Q=ABS(Q)
          ETEMP=E
          E=D
          IF(ABS(P).GE.ABS(.5*Q*ETEMP).OR.P.LE.Q*(A-X).OR. &
             P.GE.Q*(B-X)) GOTO 1
          D=P/Q
          U=X+D
          IF(U-A.LT.TOL2 .OR. B-U.LT.TOL2) D=SIGN(TOL1,XM-X)
          GOTO 2
        ENDIF
1       IF(X.GE.XM) THEN
          E=A-X
        ELSE
          E=B-X
        ENDIF
        D=CGOLD*E
2       IF(ABS(D).GE.TOL1) THEN
          U=X+D
        ELSE
          U=X+SIGN(TOL1,D)
        ENDIF
        FU=F(U)
        IF(FU.LE.FX) THEN
          IF(U.GE.X) THEN
            A=X
          ELSE
            B=X
          ENDIF
          V=W
          FV=FW
          W=X
          FW=FX
          X=U
          FX=FU
        ELSE
          IF(U.LT.X) THEN
            A=U
          ELSE
            B=U
          ENDIF
          IF(FU.LE.FW .OR. W.EQ.X) THEN
            V=W
            FV=FW
            W=U
            FW=FU
          ELSE IF(FU.LE.FV .OR. V.EQ.X .OR. V.EQ.W) THEN
            V=U
            FV=FU
          ENDIF
        ENDIF
      END DO
      IF (ITCOPY.GE.ITMAX) THEN
        PAUSE 'Brent exceed maximum iterations.'
      END IF
      XMIN=X
      BRENT=FX
      RETURN
      END
!     ******************************************************************
!     simul.90
!     Copyright(c) Utah Division of Water Resorces 2000
!
!     Created: 5/1/2011 10:30:10 AM
!     Author : STATE OF UTAH
!     Last change: CWM 5/2/2011 6:51:56 AM
!     ******************************************************************
Real (KIND=8) FUNCTION Simul(X)
      use PARAMDIM
      use COMMO
      use PrintStuff
      use F90SQLConstants
      use F90SQL
      use F90SQLVARIABLES
      use FSQL_DATA
      REAL (KIND=8) :: X,GBEF,MBEF,GAFT,MAFT,RV,RE,RA,STBEG(MRES),QMR
      REAL (KIND=8) :: TempArr(13)
      INTEGER (KIND=4) :: II,IQXI,IQXNUM,JJ,KK
      COMMON /IARQI/IARQ(MQX)
      INTEGER*4 IARQ,ILEN,M,EXENUM
      REAL (KIND=8) :: ErrorFunc
      EXTERNAL ErrorFunc
      STEM=0.0
      SSTO=0.0
      EXENUM=0
!     INITILIZE GROUNDWATER AND SOIL MOISTURE STORAGE
      if (Calibrate) then
        if (TypeCalib.EQ.0) then
          !Calibrate On-Farm Efficiency
          CEFF(CalibArea,J)=X
        elseif (TypeCalib.EQ.1) then
          !Calibrate Canal conveyance efficiency
          IEFF(CalibArea,J)=X
        elseif (TypeCalib.EQ.2) THEN
          !Calibrate Return Flow Factors
          PCRF(CalibArea,1)=X
          DO M=2,10
            PCRF(CalibArea,M)=PCRF(CalibArea,1)*RetProp(M)
          END DO
        end if
        TEF(CalibArea,J)=CEFF(CalibArea,J)*IEFF(CalibArea,J)
      end if
      GBEF = 0.0
      MBEF = 0.0
      DO L = 1,NLND
       MBEF = MBEF + MOIST(L)
       DO II = 1,10
          GBEF = GBEF + SSTO(L,II)
       ENDDO
      ENDDO
      GOTO 40
 38   CONTINUE
      GAFT = 0.0
!      LGAFT=0.0
      MAFT = 0.0
      DO L = 1,NLND
        MAFT = MAFT + MOIST(L)
        DO II = 1,10
          GAFT = GAFT + SSTO(L,II)
        ENDDO
      ENDDO
      IF (EXENUM.GT.7) THEN
        GO TO 37
      END IF
      IF (ABS (MBEF - MAFT) + ABS (GBEF - GAFT).LT. 1.0) GOTO 37
      FNYRS=FLOAT(NYRS)
!
!     START MONTHLY AND YEARLY OPERATION
!
      MBEF = MAFT
      GBEF = GAFT
 40   CONTINUE
      DO M=1,NRES
        STO(M)=STOIC(M)
        RV=STO(M)
        CALL RACE(M,RV,RA,RE)
        REL(M)= RE
        RAR(M)= RA
      ENDDO
      DO K=1,13
        ACUSM  (K)=0.0
        CACU   (K)=0.0
        CHGW   (K)=0.0
        CHSM   (K)=0.0
        CPCU   (K)=0.0
        CUDEF  (K)=0.0
        DDIV   (K)=0.0
        DPUM   (K)=0.0
        DRET   (K)=0.0
        LGOUT  (K)=0.0
        DMUSE  (K)=0.0
        DSUP   (K)=0.0
        DMACU  (K)=0.0
        GWLOS  (K)=0.0
        GWMI   (K)=0.0
        RFGW   (K)=0.0
        RIVGW  (K)=0.0
        RFLO   (K)=0.0
        RSSP   (K)=0.0
        RZ     (K)=0.0
        RZSUP  (K)=0.0
        SMSPL  (K)=0.0
        TDIVR  (K)=0.0
        TGAGE  (K)=0.0
        TMUN   (K)=0.0
        TOTRF  (K)=0.0
        WCONS  (K)=0.0
        WPCUM  (K)=0.0
        WPCHG  (K)=0.0
        WSHOR  (K)=0.0
        WSUP   (K)=0.0
      ENDDO
      DO L=1,NQX
       IQXI=IPQX(L)
       IF(IQXI.GT.0)THEN
          DO J=1,NYRS
             DO K=1,13
              QX(IQXI,J,K)=0.0
             ENDDO
          ENDDO
       ENDIF
      ENDDO
      DO L=1,NQIN
          IQXNUM=ABS(IQXN(L))
          IF(IQXNUM.GT.0)THEN
          DO J=1,NYRS
             DO K=1,13
               IF(IQXN(L).LT.0)THEN
                     QX(IQXNUM,J,K) = -QIN(L,J,K)
               ELSE
                     QX(IQXNUM,J,K) = QIN(L,J,K)
               ENDIF
             ENDDO
          ENDDO
          ENDIF
      ENDDO
      DO L=1,NLND
        if (GWQX(L)>0) then
          DO J=1,NYRS
            DO K=1,13
              IF (CONUSE(13,J,L).GT.0.0) THEN
                QX(GWQX(L),J,K)=AnnGW(L,J)*CONUSE(K,J,L)/CONUSE(13,J,L)
              ELSE
                QX(GWQX(L),J,K)=0.0
              END IF
            END DO
          END DO
        end if
      END DO
      DO M=1,NRES
         DO K=1,13
            STDIF  (M,K)=0.0
         ENDDO
         RV=STOIC(M)
         STO(M)=RV
         CALL RACE(M,RV,RA,RE)
         RAR(M)=RA
         REL(M)=RE
      ENDDO
      LINE=100
      CumError=0.0
      TGAGE=0.0
      DO J=1,NYRS
       DO K=1,12
          !Zero reservoir deliveries to Municipal and Ag areas
          MResCal=0.0
          ResCal=0.0
          DRAWN=.FALSE.
          DO M=1,NRES
             STBEG(M)=STO(M)
          ENDDO
          ITERA=0
          ITERA=ITERA
!*******************************************************************
! Make land area calculateions
!*******************************************************************
          CALL LNDCLC
          IF (Calibrate) THEN
            CumError = CumError+ ErrorFunc()
          ELSE
            CumError = 0.0
          ENDIF
!*******************************************************************
!   Debug statements - titles are loaded at first of BUDGET
!   In this case, values are loaded in ARSUB
!*******************************************************************
         !~~
         !IF(K.EQ.12)THEN
         !   DO JJ=1,NLND
         !     BUG(JJ,13)=0.0
         !     DO KK=1,12
         !       BUG(JJ,13)=BUG(JJ,KK)+BUG(JJ,13)
         !     END DO
         !   END DO
         !  IF(J.EQ.1)THEN
         !    WRITE(6,'(/5X,"YEAR",T12,12(4X,A4),5X,A4)')(VAR(I),I=2,14)
         !  ENDIF
         !  DO I=1,NLND
           !   WRITE(6,'(5X,A15,12F8.1,F9.1)')BUGTITL(I), &
           !        (BUG(I,KK),KK=1,13)
         !    WRITE(6,'(5X,I4,2X,I2,T15,12F8.1,F9.1)')IYEAR(J),I, &
         !       (BUG(I,KK),KK=1,13)
         !  ENDDO
         !ENDIF
!*******************************************************************
!   End of Debug statements
!*******************************************************************
          CALL AVCLC
          DO M=1,NRES
             CALLED(M)=.FALSE.
             DRAWN(M)=.FALSE.
             STDIF(M,K)=STDIF(M,K)+(STBEG(M)-STO(M))/FNYRS
             IF (K.EQ.1) THEN
               IF (J.EQ.1) THEN
                 StoDiff(M,J,K)=ST(M,J,K)-STOIC(M)
               ELSE
                 StoDiff(M,J,K)=ST(M,J,K)-ST(M,J-1,12)
               END IF
             ELSE
               StoDiff(M,J,K)=ST(M,J,K)-ST(M,J,K-1)
             END IF
          ENDDO
!
!     CALCULATE AVERAGE VALUES FOR TABLE
!
!           MAKE TARGET RELEASES
!
          DO M=1,NRES
              IF (IPQN(M))THEN
                    QMR = MAX(QTG(M,13) - QX(IQTG(M,1),J,K),0.0)
                    DMD = MAX(0.0,MIN(STO(M)-QTG(M,K),QMR))
                    IF (DMD.GT.0.0) THEN
                        CALL RESREL(M)
                    END IF
              END IF
          ENDDO
!
!              COMPUTE AND SAVE RESERVOIR AREAS AND ELEVATIONS
!
          DO M=1,NRES
             RV=STO(M)
             CALL RACE(M,RV,RA,RE)
             RAR(M)=RA
             REL(M)=RE
             ELV(M,J,K)=RE
             SAR(M,J,K)=RA
          ENDDO
!
!        HYDROPOWER SECTION
!
!        Variables:
!        QPMXI(IP)  - Maximum Flow in CFS through Plant
!        QPMNI(IP)  - Minimum Flow in CFS
!        HDMNI(IP)  - Minimum pressure head required
!        KSI(IP)    - Beginning month of power generation
!        KEI(IP)    - Ending month of power generation
!        KI(IP)     - QX number into power plant
!        KU(IP)     - QX number upstream of the power plant
!        KB(IP)     - QX number bypassing the power plant
!        REL(IP)    - ARRAY WITH THE INITIAL RESERVOIR ELEVATIONS
!        H1I(IP)    - ARRAY WITH INITIAL HEAD (ELEVATION-TAIL WATER
!                    ELEVATION) - updated each time period
!
!        Variables used:
!        H2I(IP)      - Power head available, end of this time period
!        HEAD(IP,J,K) - The average of the power head available at the
!                       beginning and end of the time period.
!        ENER(IP,J,K) - Total energy in KWH produced during the month
!-----------------------------------------------------------------------
!          IF (NHPW.GT.0) THEN
!             DO IP=1,NHPW
!              QPMX=QPMXI(IP)*60.
!              QPMN=QPMNI(IP)*60.
!              HDMN=HDMNI(IP)
!              KS=KSI(IP)
!              KE=KEI(IP)
!              QX(IHP(IP),J,K)=QX(IHS(IP),J,K)
!              IF (K.LT.KS.OR.K.GT.KE)  QX(IHP(IP),J,K)=0.0
!              QX(IHP(IP),J,K)=MIN(QX(IHP(IP),J,K), QPMX)
!              IF (QX(IHP(IP),J,K).LT.QPMN) QX(IHP(IP),J,K)=0.0
!              QX(IHB(IP),J,K)=QX(IHS(IP),J,K)-QX(IHP(IP),J,K)
!              QHPI(IP)=QX(IHP(IP),J,K)
!              SUM=0.
!              DO I=1,IHNP(IP)
!                 SUM=SUM+HLOSS(IP,I)
!              END DO
!              HGL=REL(IHR(IP))-SUM
!              H2I(IP)=HGL-ELTWI(IP)
!              H1 = H1I(IP)
!              H2 = H2I(IP)
!              HA=0.5*(H1+H2)
!              HP1=CP1I*HA*QHPI(IP)*E1I(IP)
!              CHP1=1.0
!              HMN=AMIN1(H1,H2)
!              IF (HMN.LE.HDMN) THEN
!              AH12 = ABS(H1-H2)
!              AH12=MAX(AH12, 0.0001)
!              CHP1 = 1.0-(HDMN-HMN)/AH12
!              CHP1=MAX(CHP1,0.)
!              CHP1=MIN(CHP1,1.)
!              END IF
!              H1I(IP)=H2I(IP)
!              HEAD(IP,J,K)=HA
!              ENER(IP,J,K)=CHP1*HP1
!             END DO
!          ENDIF
          IF(NumOutflow.GT.0)THEN
            DO M=1,NumOutflow
              CALL FAVER(TGAGE,QX(NOUFL(M),J,K)/FNYRS,REAL(1.0,KIND=8), &
                REAL(1.0,KIND=8),K)
            END DO
          ENDIF
       ENDDO
      ENDDO
      EXENUM=EXENUM+1
      if (BegSoilMatch.AND.NLND.GT.0) then
        GO TO 38
      end if
   37 CONTINUE
      IF (Calibrate) THEN
        IF (TypeCalib.EQ.0) THEN
          WRITE(6,"(10X,'On Farm Eff.=',F6.3,' Error=',F10.2)")X,CumError
          PRINT *,"'On Farm Eff.=",X," Error =",CumError
        ELSEIF (TypeCalib.EQ.1) THEN
          WRITE(6,"(10X,'Conveyance Eff.=',F6.3,' Error=',F10.2)")X,CumError
          PRINT *,"'Conv. Eff.=",X," Error =",CumError
        ELSEIF (TypeCalib.EQ.2) THEN
          WRITE(6,"(10X,'Return Flow Factor (lag 0)=',F6.3,' Error=',F10.2)") &
            X,CumError
          PRINT *,"'Ret Flow Factor Lag 0=",X," Error =",CumError
        END IF
        Simul = CumError*CumError
      ELSE
        Simul = 0.0
      ENDIF
      END FUNCTION

CHARACTER (LEN=255) FUNCTION CenterIt(CharIn,CharLen)
CHARACTER (LEN=*),INTENT(IN) :: CharIn
INTEGER (KIND=4),INTENT(IN) :: CharLen
INTEGER (KIND=4) :: FirstPart,SecondPart
CHARACTER (LEN=255) :: CharCopy,Spaces
CHARACTER (LEN=255) :: AllTrim
DATA SPACES/"                                                        "/
integer (kind=4) :: theLen
CharCopy=AllTrim(CharIn)
theLen = len_trim(AllTrim(CharIn))
if (theLen>CharLen) then
  CharCopy=CharCopy(1:CharLen)
else
  FirstPart=(CharLen-theLen)/2
  SecondPart=CharLen-theLen-FirstPart
  CharCopy=SPACES(1:(FirstPart)) // CharCopy(1:theLen)//SPACES(1:(SecondPart))
end if
CenterIt=CharCopy
end function