!     ******************************************************************
!     ARSUB.FOR
!     Copyright(c) Water Resources 2000
!
!     Created: 11/29/1988 3:32:54 PM
!     Author : CRAIG W MILLER
!     Last change: SOU 7/25/2013 2:35:57 PM
!     ******************************************************************

      SUBROUTINE ARSUB(ILND)
!***********************************************************************
!
!     UTAH DIVISION OF WATER RESOURCES
!
!     WRITTEN:  11-29-1988 BY Craig W. Miller
!     MODIFIED: 03-19-1996  BY: CWM
!
!***********************************************************************
!
!     Subroutine to perform water budget calculations for land area ILND
!
!     ILND - Land area number
!     QWAD - Water available for diversion upstream of land area
!     QUPST - Total water upstream of land area
!
!     Variables passed through COMMON statements:
!     IDV - QX number of diversion
!     IBY - Bypass past diversion
!     IRV - QX number of grounwater interflow from stream used by
!        adjacent croplands
!     IFT - River and groundwater flows past land area
!     IRT - Total Return flows from agricultural land area
!     MUNGRDL - Lawn and Garden deliveries to municipalities
!
!     SUBROUTINE ARLIM(ILND,RLIM)
!     The diversions to land area ILND are limited to RLIM
!
!     SUBROUTINE ARDEL(ILND)
!     The diversions to land area ILND are QX(IDV(ILND,I),J,K)
!
!     Variable Definitions
!     SBSOI(ILND) - Sub Irrigation water use from return flow, Uses RETFL
!     SBUSE(ILND,K) - Sub Irrigation Demand
!     SBMOI(ILND) - Sub Irrigation water use from soil, MOIST
!     SHRTAG - Ag shortage
!     SHRTMI - M&I shortage
!     AGWAT - Total water available for plants (Delivery * eff. +
!        excess rainfall
!     CPWAT - Deliveries only to non-subirrigated and lawn&garden lands
!     DIV - Diversion to land area
!   GWUSE(L,K)----------
!   IOPT----------------  0 for ARSUB, 1 for ARDEL in ARSUB
!   RMAX----------------  Used with ARLIM(ILND,RMAX), Max delivery allowed.
!   TOTDEM--------------
!   BEGWAT--------------
!   ENDWAT--------------
!   ENDDEL(IRSNO,ICALL)-
!   IRSNO---------------
!   ICALL---------------
!   INCN----------------  Input QIN number which determines max diversions
!-----------------------------------------------------------------------
      use PARAMDIM
      use COMMO
      use PrintStuff
      COMMON/IARQI/IARQ(MQX)
      INTEGER*4 IRQX(20),DEL(20),ICALL,IRSNO
      INTEGER*4 IOPT,ILND,II,IA,IUPQX,INCN,INCAN,IARQ
      INTEGER*4 IBYPAS,IDVQX,IBYQX,M,LRESNO,LIMQ,N,IRES
      INTEGER*4 INGWRV,IBYFIR,IQXRT,BEGWAT,ENDWAT,IMon
      REAL*8 RMAX,RRMAX,TOTEF,TOTDEM,AGTOT,QDEL,JJ
      REAL*8 CANAL,DELX,QLEFT,DIV,DEMLFT,TDIV,BYPAS
      REAL*8 DMDT,CPWAT,SHRTAG,SHRTMI,BYPASS,CROPDEM,WETDEM
      REAL*8 SUBSHRT,MinCarryOver,SoilLossW,CarryOverW
      REAL*8 TOTSB,GWSB,MUNGRDL,Minflow,Maxflow,SoilLoss,CarryOver
      REAL (KIND=8) :: MinCarryOverW
      LOGICAL*4 UseThis,Diverted(MQX)
      IOPT=0
      RMAX=6.E7
      GO TO 5
      ENTRY ARDEL(ILND)
      IOPT=1
      RMAX=6.E7
      GO TO 5
      ENTRY ARLIM(ILND,RRMAX)
      IOPT=0
      RMAX=RRMAX
5     DO 200 II=1,NRES
         RESCAL(ILND,II)=0.0
 200  CONTINUE
      Pumped(ILND)=0.0
      Diverted=.FALSE.
      AGDEM(ILND)=CONUSE(K,J,ILND)
      IF(TEF(ILND,J).GT.0.000001)THEN
         TOTEF=TEF(ILND,J)
      ELSE
         TOTEF=1
      ENDIF
      WETDEM=WCUSE(ILND,J,K)+MAX(0.0,WSOIL(ILND)-WMOIS(ILND)- &
        WPERC(K,J,ILND))
      IF (CONUSE(K,J,ILND).GT.0.0) THEN
        TOTDEM=MIN(RMAX,MAX(0.0,CONUSE(K,J,ILND)+MAX(0.0,SOIL(ILND) &
         -MOIST(ILND)-PERCO(K,J,ILND))))/TOTEF
        CROPDEM=MIN(RMAX,MAX(0.0,CONUSE(K,J,ILND)+MAX(0.0,SOIL(ILND) &
          -MOIST(ILND)-PERCO(K,J,ILND))))/TOTEF
      ELSE
        TOTDEM=0.0
        CROPDEM=0.0
      END IF
      AGTOT=AGDEM(ILND)+MAX(0.0,SOIL(ILND)-MOIST(ILND)- &
        PERCO(K,J,ILND))
      DIV=0.0
      QDEL=0.0
      IF (.NOT.IOPT==1) THEN
        DO IA=1,NQLIN(ILND)
        !I believe that INCN is no longer used 7/26/2012
          IUPQX=ILUP(ILND,IA)
          INCN=ILQIN(ILND,IA)
          IF (IUPQX.GT.0.AND.INCN.GT.0) THEN
            QX(INCN,J,K)=0.0
          END IF
        END DO
      END IF
      DO 50 IA=1,NQLIN(ILND)
         IF(IOPT.NE.1)THEN
            IUPQX=ILUP(ILND,IA)
            INCN=ILQIN(ILND,IA)
            IBYPAS=IBY(ILND,IA)
         ELSE
            IUPQX=0
            INCN=0
         ENDIF
!     DETERMINE REQUIREMENTS FOR BYPASS, IF NO BYPASS, IGNORE CODE
         DMD=0.0
         IF(IUPQX.GT.0)THEN
           DMD=MAX(0.0,-(QX(IUPQX,J,K)-FBYPS(IUPQX,J,K)), &
              (FBYPS(IBYPAS,J,K)-QX(IBYPAS,J,K)))
           IF(INCN.GT.0.OR.IOPT.EQ.1)THEN
              IF(IOPT.EQ.0)THEN
                 INCAN=INCN
                 CANAL=QIN(INCAN,J,K)
              ELSE
                 IF(INCN.GT.0)THEN
                    INCAN=INCN
                    CANAL=QIN(INCAN,J,K)
                    QX(IDV(ILND,IA),J,K)=CANAL
                    DEL(IA)=CANAL
                 ELSE
                    INCAN=IDV(ILND,IA)
                    CANAL=QX(INCAN,J,K)
                    DEL(IA)=CANAL
                 ENDIF
              ENDIF
              DMD=MAX(CANAL-(QX(IUPQX,J,K)-FBYPS(ILND,IA,K)),0.0)
           ENDIF
           IF(DMD.GT.0.0)THEN
              IRSNO=LRESROW(ILND,IA)
              IF (IRSNO.GT.0) THEN
  !-----------------------------------------------------------------------
  !     ICKRS - ARRAY OF RESERVOIRS SERVING CONNECTING QX'S TO LAND AREAS
  !     LMRES - ARRAY OF INDEX NUMBERS FOR RESERVOIRS SERVING LAND AREA
  !-----------------------------------------------------------------------
                  IRQX=0
                  DO II=1,NILNQ(L,IA)
                      IRQX(II)=ILNQ(ILND,ICALL,II)
                  ENDDO
                  QDVM=0.0
                  CALL RESR(IRSNO,IRQX,NILNQ(L,IA))
                  RESCAL(ILND,IA)=RESCAL(ILND,IA)+QDVM
                  IF(IOPT.EQ.1)THEN
                      QX(IDV(ILND,IA),J,K)=DEL(IA)
                  ENDIF
                  IBYPAS=IBY(ILND,IA)
                  QX(IBYPAS,J,K)=QX(IBYPAS,J,K)+QDVM
                  DMD=MAX(0.0,DMD-QDVM)
                  IF (DMD.LE.0.01) THEN
                    EXIT
                  END IF
              END IF
           ENDIF
         ENDIF
         DELX=0.0
         if (IBYPAS.GT.0) then
           Minflow=FBYPS(IBYPAS,J,K)
         end if
         if (INCN.GT.0) then
           Maxflow=CANAL
         ELSE
           Maxflow=1.0E7
         end if
         IF(IUPQX.EQ.0)THEN
            IF(INCN.GT.0)THEN
               DELX=MAX(0.0,QIN(INCN,J,K))
            ELSE
               DELX=MAX(0.0,QX(IDV(ILND,IA),J,K))
            ENDIF
         ELSE
            IF(INCN.GT.0)THEN
               DELX=MAX(0.0,MIN(QX(IUPQX,J,K)-FBYPS(ILND,I,K),CANAL))
            ENDIF
         ENDIF
         QDEL=QDEL+DELX
 50   CONTINUE
      !QLEFT=MAX(QDEL-TOTDEM,0.0)
      QLEFT=MAX(QDEL-CROPDEM,0.0)
      !DEMLFT=MAX(TOTDEM-QDEL,0.0)
      DEMLFT=MAX(CROPDEM-QDEL,0.0)
      if (GWQX(ILND)>0) then
        DEMLFT=DEMLFT-QX(GWQX(ILND),J,K)/CEFF(ILND,J)
        Pumped(ILND)=QX(GWQX(ILND),J,K)
      ELSE
        Pumped(ILND)=0.0
      end if
      I=NQLIN(ILND)
      TDIV=0.0
 160  CONTINUE
      UseThis=.TRUE.
      DO IA=1,NQLIN(ILND)
         IF(IOPT.NE.1)THEN
            IUPQX=ILUP(ILND,IA)
            IF (IUPQX.GT.0) THEN
              IF (Diverted(IUPQX)) THEN
                CYCLE
              END IF
            END IF
            INCAN=ILQIN(ILND,IA)
         ELSE
            IUPQX=0
            INCAN=0
         ENDIF
         IDVQX=IDV(ILND,IA)
         IBYPAS=IBY(ILND,IA)
         !IF (DEMLFT.LE.0.001) THEN
         !  IF (IUPQX.GT.0.AND.IBYPAS.GT.0) THEN
         !     QX(IBYPAS,J,K)=QX(IDVQX,J,K)
         !     Diverted(IUPQX)=.TRUE.
         !     CYCLE
         !  ELSE
         !     IF (IUPQX.GT.0) THEN
         !       QX(IDVQX,J,K)=QX(IUPQX,J,K)
         !       TDIV=QX(IDVQX,J,K)
         !       DIV=DIV+TDIV
         !       Diverted(IUPQX)=.TRUE.
         !       CYCLE
         !     ELSE
         !       TDIV=QX(IDVQX,J,K)
         !       DIV=DIV+TDIV
         !       DIVERTED(IDVQX)=.TRUE.
         !       CYCLE
         !     END IF
         !  END IF
         !END IF
         IF (IBYPAS.GT.0) THEN
           BYPAS=MAX(FBYPS(IBYPAS,J,K),QX(IBYPAS,J,K))
         END IF
         IF(INCAN.GT.0.AND.IUPQX.GT.0)THEN
            TDIV=MAX(0.0,MIN(QX(IUPQX,J,K)-BYPAS,QIN(INCAN,J,K),DEMLFT))
            DEMLFT=MAX(0.0,DEMLFT-TDIV)
         ELSE IF(IOPT.EQ.1.AND.IUPQX.GT.0)THEN
            IF(INCAN.EQ.0)THEN
               TDIV=MAX(0.0,MIN(QX(IUPQX,J,K)-BYPAS,QX(IDVQX,J,K),DEMLFT))
            ELSE
               TDIV=MAX(0.0,MIN(QX(IUPQX,J,K)-BYPAS,QIN(INCAN,J,K),DEMLFT))
            ENDIF
            DEMLFT=MAX(0.0,DEMLFT-TDIV)
         ELSE IF(IUPQX.EQ.0)THEN
            TDIV=MAX(0.0,QX(IDVQX,J,K))
            IF(TDIV.LT.0)THEN
               WRITE(6,*)'DIVERSION ',IA,' TO LAND AREA ',ILND, &
                  ' NEGATIVE IN ',VAR(K+1),' YEAR ',J,' IGNORED'
            ENDIF
         ELSE
            IF (CONUSE(K,J,ILND).GT.0.0) THEN
              TDIV=MAX(0.0,MIN(DEMLFT,QX(IUPQX,J,K)-QLEFT-BYPAS))
            ELSE
              TDIV=0.0
            END IF
            QLEFT=MAX(0.0,QLEFT-MAX(0.0,QX(IUPQX,J,K)-TDIV+BYPAS))
            DEMLFT=MAX(0.0,DEMLFT-TDIV)
         ENDIF
         DIV=DIV+TDIV
         IBYQX=IBY(ILND,IA)
         QX(IDVQX,J,K)=TDIV
         IF(IUPQX.GT.0)THEN
            QX(IBYQX,J,K)=QX(IUPQX,J,K)-TDIV
            Diverted(IUPQX)=.TRUE.
         ENDIF
         !IF (DEMLFT.LE.0.001) THEN
         !  EXIT
         !END IF
      END DO
      !IF (TOTDEM.GT.DIV.AND.CONUSE(K,J,ILND).GT.0.0) THEN
      IF (CROPDEM-Pumped(ILND)/CEFF(ILND,J).GT.DIV.AND.CONUSE(K,J,ILND).GT.0.0) THEN
         DMDT=CROPDEM - DIV - Pumped(ILND)/CEFF(ILND,J)
         IF(IOPT.NE.1)THEN
            DO M=1,NQLIN(ILND)
               IF (LRESROW(ILND,M).EQ.0) THEN
                  CYCLE
               END IF
               LRESNO=LRESROW(ILND,M)
               !ICALL=LMRES(ILND,LRESNO)
!-----------------------------------------------------------------------
!     NDEML HOLDS THE DIVERSION QX NUMBER THAT CONNECTS RESERVOIR
!     LRESNO TO LAND AREA ILND.  IF ZERO, QX DOES NOT EXIST
!-----------------------------------------------------------------------
               LIMQ=NDEML(ILND,LRESNO)
!-----------------------------------------------------------------------
!     IARQ - AN ARRAY HOLDING QIN NUMBERS RELATING TO DIVERSIONS INTO
!     A LAND AREA.  IARQ(QX NO.) IS THE QIN INTO LAND AREA
!     IF THIS IS A NON-ZERO VALUE, THIS WATER HAS ALREADY BEEN DIVERTED
!     ABOVE - THEN CYCLE
!-----------------------------------------------------------------------
               IF (LIMQ.GT.0) THEN
                 IF(IARQ(LIMQ).GT.0)CYCLE
               END IF
               DMD=DMDT
               IRQX=0
               DO N=1,NILNQ(ILND,M)
                   IRQX(N)=ILNQ(ILND,M,N)
               ENDDO
               QDVM=0.0
               CALL RESR(LRESNO,IRQX,NILNQ(ILND,M))
               RESCAL(ILND,M)=RESCAL(ILND,M)+QDVM
               DMDT=MAX(0.0,DMDT-QDVM)
               DIV=DIV+QDVM
               TDIV=TDIV+QDVM
               IF(DMDT.EQ.0.0)EXIT
            ENDDO
         ENDIF
      END IF
!----------------------------------------------------------------------
!     CALCULATE INTERFLOW
!     GWRIV is the demand from subirrigated flow
!        it is not routed through the soil moisture but is taken
!        directly from the river or stream.
!----------------------------------------------------------------------
      INGWRV=IRV(ILND)
      IF(INGWRV.GT.0)THEN
         QX(INGWRV,J,K)=0.0
         IF (IFT(ILND).GT.0) THEN
           BYPAS=MAX(FBYPS(IFT(ILND),J,K),QX(IFT(ILND),J,K),0.0)
         ELSE
           BYPAS=0.0
         END IF
         GWRIV(ILND)=MAX(0.0,MIN(QX(IUR(ILND),J,K)-BYPAS, &
          WETDEM))
         !IF(TOTSB.LT.WCUSE(ILND,J,K))THEN
         ! GWSB=MAX(0.0,MIN(QX(IBYFIR,J,K)- &
         !    GWRIV(ILND),WCUSE(ILND,J,K)-TOTSB))
         !GWSB=MAX(0.0,MIN(QX(IBYFIR,J,K)- &
         !   GWRIV(ILND),WCUSE(ILND,J,K)))
         !ENDIF
         GWSB=0.0
         QX(INGWRV,J,K)=MAX(0.0,GWRIV(ILND))
         if (QX(INGWRV,J,K).LT.(WCUSE(ILND,J,K)-WMOIS(ILND)-WPERC(K,J,ILND)) &
          .AND.LNRS(ILND).GT.0) then
          GetQX: DO M=1,NLndQX(ILND)
             IF (LRES(ILND,M).EQ.0) THEN
                CYCLE
             END IF
             LRESNO=LRESROW(ILND,M)
             ICALL=LMRES(ILND,LRESNO)
             IF (IBY(ILND,M).EQ.IUR(ILND)) THEN
                IRQX=0
                IDVQX=IDV(ILND,M)
                DO N=1,NILNQ(ILND,M)
                  IF (IDVQX.EQ.ILNQ(ILND,M,N)) THEN
                    IRQX(N)=IBY(ILND,M)
                  ELSE
                    IRQX(N)=ILNQ(ILND,M,N)
                  END IF
                ENDDO
                IRQX(NILNQ(ILND,M)+1)=INGWRV
                DMD=WCUSE(ILND,J,K)-QX(INGWRV,J,K)+MAX(0.0,WSOIL(ILND)- &
                  WMOIS(ILND))
                CALL RESR(LRESNO,IRQX,NILNQ(ILND,M)+1)
                RESCAL(ILND,M)=RESCAL(ILND,M)+QDVM
                DMD=DMD-QDVM
                !Commented out 4/19/2012
                !DIV=DIV+QDVM
                !Added 4/19/2012
                GWRIV(ILND)=GWRIV(ILND)+QDVM
                IF (DMD.LE.0.001) THEN
                  EXIT GetQX
                END IF
             END IF
          END DO GetQX
          GWRIV(ILND)=QX(INGWRV,J,K)
         end if
         BYPASS=QX(IUR(ILND),J,K)-QX(INGWRV,J,K)
         QX(IFT(ILND),J,K)=BYPASS
      ELSE IF(IBYFIR.GT.0)THEN
         BYPASS=QX(IBYFIR,J,K)
         GWRIV(ILND)=0.0
      ELSE
         BYPASS=0.0
         GWRIV(ILND)=0.0
      ENDIF
!     CALCULATE WATER TO TO AGRICULTURAL LAND AREAS
      AGDEL(ILND)=MAX(DIV,0.0)
!      BUG(ILND,K)=BUG(ILND,K)+DIV
!     CALCULATE TOTAL WATER TO AGRICULTURAL LANDS
      AGWAT(ILND)=AGDEL(ILND)*TEF(ILND,J)+PERCO(K,J,ILND)
      CPWAT=AGDEL(ILND)*TEF(ILND,J)+ &
        PERCO(K,J,ILND)
      IF (GWQX(ILND).GT.0) THEN
        AGWAT(ILND)=AGWAT(ILND)+QX(GWQX(ILND),J,K)*IEFF(ILND,J)
        CPWAT=CPWAT+QX(GWQX(ILND),J,K)*IEFF(ILND,J)
      END IF
!     CALCULATE SHORTAGE TO AGRICULTURAL LANDS
!~~This doesn't seem right
! Don't include the shortage from subirrigated lands here.
! That should be calculated after return flows are calculated.
      SHRTAG=MAX((CONUSE(K,J,ILND)-CPWAT-MOIST(ILND)),0.0)/TOTEF
!      + & MAX(WCUSE(ILND,J,K)-GWRIV(ILND)-WMOIS(ILND),0.0)/TOTEF
!~~Change this so that groundwater deliveries are from data
!      SHRTMI=MAX(0.0,(1.0-IGRND(ILND,K))*QDMI(ILND,K)-DIV)
      BEFM(ILND)=MOIST(ILND)
      BEFP(ILND)=WMOIS(ILND)
      MOIST(ILND)=MAX(MOIST(ILND)+CPWAT-CONUSE(K,J,ILND),0.0)
      WMOIS(ILND)=MAX(WMOIS(ILND)+GWRIV(ILND)-WCUSE(ILND,J,K)+ &
        WPERC(K,J,ILND),0.0)
      RETFL(ILND)=+MAX(MOIST(ILND)-SOIL(ILND),0.0)+ &
        DIV*(1.0-TEF(ILND,J))+ Pumped(ILND)*(1.0-IEFF(ILND,J)) + &
        MAX(WMOIS(ILND)- WSOIL(ILND),0.0)
      MOIST(ILND)=MIN(SOIL(ILND),MOIST(ILND))
      WMOIS(ILND)=MIN(WSOIL(ILND),WMOIS(ILND))
      SPILL(ILND)=0.0
! In March take a 33% loss from soil moisture and route it downstream
      if ((TypeYear==WaterYear.AND.K==6).OR. &
         (TypeYear==WaterYearNov.AND.K==5).OR. &
         (TypeYear==CalendarYear.AND.K==3)) Then
         CarryOver=0.0
         CarryOverW=0.0
         DO IMON=K-6,K
          IF (IMON<1) THEN
            IF (J>1) THEN
              CarryOver=CarryOver+PERCO(12+IMON,J-1,ILND)*.67
              CarryOverW=CarryOverW+WPERC(12+IMON,J-1,ILND)*.67
            ELSE
              CarryOver=CarryOver+PERCO(12+IMON,J,ILND)*.67
              CarryOverW=CarryOverW+WPERC(12+IMON,J,ILND)*.67
            END IF
          ELSE
            CarryOver=CarryOver+PERCO(K,J,ILND)*.67
            CarryOverW=CarryOverW+WPERC(K,J,ILND)*.67
          END IF
         END DO
         MinCarryOver=MIN(SOIL(ILND)*.75,CarryOver,MOIST(ILND))
         MinCarryOverW=MIN(WSOIL(ILND)*.75,CarryOverW,WMOIS(ILND))
         SoilLoss = MAX(MOIST(ILND)-MinCarryOver,0.0)
         SoilLossW = MAX(WMOIS(ILND)-MinCarryOverW,0.0)
         SPILL(ILND)=SPILL(ILND)+SoilLoss + SoilLossW
         MOIST(ILND)=MOIST(ILND)-SoilLoss
         WMOIS(ILND)=WMOIS(ILND)-SoilLossW
         RETFL(ILND)=RETFL(ILND)+SoilLoss+SoilLossW
      END IF
      SPILL(ILND)=SPILL(ILND)+MAX(MOIST(ILND)-SOIL(ILND),0.0)+MAX(WMOIS(ILND)- &
        WSOIL(ILND),0.0)
      MOIST(ILND)=MIN(SOIL(ILND),MOIST(ILND))
      WMOIS(ILND)=MIN(WSOIL(ILND),WMOIS(ILND))
      QDVA(ILND)=RETFL(ILND)*RETI(ILND)
      CALL RTFLO(ILND,QDVA(ILND),QRTF(ILND))
      SUBSHRT= MAX(WCUSE(ILND,J,K)-GWRIV(ILND)-BEFP(ILND)-WPERC(K,J,ILND),0.0)
      RETUSE(ILND)=MAX(MIN(SUBSHRT,QRTF(ILND)),0.0)
      QRTF(ILND)=QRTF(ILND)-RETUSE(ILND)
      SUBSHRT=MAX(0.0,SUBSHRT-RETUSE(ILND))
      SHRTAG=SHRTAG+MAX(0.0,SUBSHRT)/TOTEF
      INGWRV=IRV(ILND)
      IBYFIR=IUR(ILND)
      GWSB=0.0
      SHORT(ILND,J,K)=SHRTAG
      IQXRT=IRT(ILND)
      QX(IQXRT,J,K)=QRTF(ILND)
      IF (IRV(ILND).GT.0.AND.IUR(ILND).GT.0.AND.IFT(ILND).GT.0.0) THEN
        QX(IFT(ILND),J,K)=QX(IUR(ILND),J,K)-QX(IRV(ILND),J,K)
      END IF
      AGOUT(ILND)=QX(IQXRT,J,K)
!*******************************************************************
!   Debug statements - titles are loaded at first of BUDGET
!   Print statements are found in BUDSUB IN Simula
!*******************************************************************
      !IF (ILND.EQ.1) THEN
      !  BUG(1,K)=DIV
      !  BUG(2,K)=PUMPED(ILND)
      !  BUG(3,K)=GWRIV(ILND)
      !  BUG(4,K)=CONUSE(K,J,ILND)
      !  BUG(5,K)=WCUSE(ILND,J,K)
      !  BUG(6,K)=SHORT(1,J,K)*TEF(ILND)
      !  BUG(7,K)=QDVA(ILND)
      !  BUG(8,K)=MOIST(ILND)-BEFM(ILND)
      !  BUG(9,K)=PERCO(K,J,ILND)+WPERC(K,J,ILND)
        !EffPRE(ILND,J,K)
      !  BUG(10,K)=BUG(1,K)+BUG(2,K)+BUG(3,K)-BUG(7,K)-BUG(4,K)-BUG(5,K)+ &
      !    BUG(6,K)-BUG(8,K)+BUG(9,K)
      !END IF
!*******************************************************************
!   End of Debug statements
!*******************************************************************
      RETURN
      END
      SUBROUTINE MUNSUB(ILND)
!***********************************************************************
!
!     UTAH DIVISION OF WATER RESOURCES
!
!     WRITTEN:  11-29-1988 BY Craig W. Miller
!     MODIFIED: 03-19-1996  BY: CWM
!
!***********************************************************************
!
!     Subroutine to perform water budget calculations for Municipal area
!     ILND
      use PARAMDIM
      use COMMO
      use PrintStuff
      INTEGER (KIND=4),INTENT(IN) :: ILND
      INTEGER (KIND=4) :: IUPQX,IDNQX,INCN,IDVQX,INCAN,M
      REAL*8 RRMAX,TOTEF,TOTDEM,AGTOT,CUPRO,QDEL,SURFDM
      REAL*8 CANAL,DELX,QLEFT,DIV,DEMLFT,TDIV,BYPAS
      REAL*8 DMDT,CPWAT,SHRTAG,SHRTMI,ExcessDiv,ExcessCorr
      REAL*8 TOTSB,GWSB,MUNGRDL,BYPASS,QGW
      INTEGER (KIND=4) :: IRSNO,IRQX(20),II
      LOGICAL (KIND=2) :: Diverted(MQX)
      Diverted=.FALSE.
      DIV=0.0
      TDIV=0.0
      QDEL=0.0
      DEMLFT=MunSurf(ILND,J,K)
      TOTDEM=MunGW(ILND,J,K)+MunSurf(ILND,J,K)
      DMDEM(ILND)=MunGW(ILND,J,K)+MunSurf(ILND,J,K)
!     CALCULATE WATER DELIVERY TO MUNICIPALITIES
      !DMDEL(ILND)=MunSurf(ILND,J,K)
      DMDEL(ILND)=0.0
!     CALCULATE WATER PUMPED TO MUNICIPALITIES
      IF (MunGWQx(ILND)>0) THEN
        QX(MunGWQx(ILND),J,K)=MunGW(ILND,J,K)
      END IF
      DMPUM(ILND)=MunGW(ILND,J,K)
      !Satisfy minimum flow demands past municipal diversions
      !Divert water to area as needed
      DO I=1,NQXMun(ILND)
        IUPQX=MunQXup(ILND,I)
        INCN=MunQXin(ILND,I)
        IF (IUPQX.GT.0) THEN
          QX(INCN,J,K)=0.0
        END IF
      END DO
      DO I=1,NQXMun(ILND)
        IUPQX=MunQXup(ILND,I)
        IDNQX=MunQXdn(ILND,I)
        INCN=MunQXin(ILND,I)
! Determine requirements for bypass, if no bypass, ignore code
        IF (IDNQX.GT.0) THEN
          BYPASS=MAX(FBYPS(IDNQX,J,K),QX(IDNQX,J,K))
        ELSE
          BYPASS=0.0
        END IF
        IF (IUPQX.GT.0) THEN
          DMD=MAX(0.0,BYPASS-QX(IUPQX,J,K))
          IF (DMD.GT.0.0) THEN
            IF(MResNum(ILND,I).GT.0)THEN
              IRQX=0
              DO II=1,NQXup(ILND,I)
                IRQX(II)=MQXup(ILND,I,II)
              ENDDO
              IRQX(NQXup(ILND,I)+1)=IDNQX
              CALL RESR(MResNum(ILND,I),IRQX,NQXup(ILND,I)+1)
              MRESCAL(ILND,I)=MRESCAL(ILND,I)+QDVM
            END IF
          END IF
          IF (.NOT.Diverted(IUPQX)) THEN
            QDEL=MIN(DEMLFT,MAX(0.0,QX(IUPQX,J,K)-BYPASS))
            DEMLFT=MAX(0.0,DEMLFT-QDEL)
            QX(INCN,J,K)=QDEL
            DMDEL(ILND)=DMDEL(ILND)+QDEL
            Diverted(IUPQX)=.TRUE.
          END IF
        ELSE
          IF (.NOT.Diverted(INCN)) THEN
            QDEL=QX(INCN,J,K)
            DEMLFT=MAX(0.0,DEMLFT-QDEL)
            DMDEL(ILND)=DMDEL(ILND)+QDEL
            Diverted(INCN)=.TRUE.
          END IF
        END IF
      END DO
      IF (MunGWQx(ILND).GT.0) THEN
        QGW=QX(MunGWQx(ILND),J,K)
      END IF
      QLEFT=MAX(QDEL-MunSurf(ILND,J,K),0.0)
      DIV=QDEL
      TDIV=DIV
      DO I=1,NQXMun(ILND)
        IUPQX=MunQXup(ILND,I)
        IDNQX=MunQXdn(ILND,I)
        IDVQX=MunQXin(ILND,I)
        INCAN=0
        IF (IUPQX.GT.0) THEN
          IF (.NOT.Diverted(IUPQX)) THEN
            IF (IDNQX.GT.0) THEN
              BYPAS=MAX(FBYPS(IDNQX,J,K),QX(IDNQX,J,K))
            ELSE
              BYPAS=FBYPS(IDNQX,J,K)
            END IF
            IF (IUPQX.GT.0) THEN
              TDIV=MAX(0.0,QX(IUPQX,J,K)-BYPAS)
            ELSE IF (IUPQX.EQ.0) THEN
              TDIV=MAX(0.0,QX(IDVQX,J,K))
            ELSE
              TDIV=MAX(0.0,MIN(DEMLFT,QX(IUPQX,J,K)-QLEFT-BYPAS))
            END IF
            DIV=DIV+TDIV
            DMDEL(ILND)=DMDEL(ILND)+TDIV
            QLEFT=QLEFT-MAX(0.0,MIN(QLEFT,QX(IUPQX,J,K)-TDIV+BYPAS))
            DEMLFT=MAX(0.0,DEMLFT-TDIV)
            QX(IDVQX,J,K)=TDIV
            IF(IUPQX.GT.0)THEN
              QX(IDNQX,J,K)=QX(IUPQX,J,K)-TDIV
              Diverted(IUPQX)=.TRUE.
            ENDIF
            IF (DEMLFT.LE.0.0) THEN
              EXIT
            END IF
            Diverted(IUPQX)=.TRUE.
          END IF
        END IF
        IF (DEMLFT.GT.0.0) THEN
          IF(MResNum(ILND,I).GT.0)THEN
            IRSNO=MResNum(ILND,I)
            IRQX=0
            DO II=1,NQXup(ILND,I)
              IRQX(II)=mqxUP(ILND,I,II)
            ENDDO
            QDVM=0.0
            DMD=DEMLFT
            CALL RESR(IRSNO,IRQX,NQXup(ILND,I))
            DMDEL(ILND)=DMDEL(ILND)+QDVM
            DEMLFT=MAX(0.0,DEMLFT-QDVM)
            MRESCAL(ILND,I)=MRESCAL(ILND,I)+QDVM
            IF (DEMLFT.LE.0.001) THEN
              EXIT
            END IF
          ENDIF
        END IF
        IF (IDNQX.GT.0.AND.IUPQX.GT.0) THEN
          QX(IDNQX,J,K)=QX(IUPQX,J,K)-QX(IDVQX,J,K)
        END IF
      END DO
      !If there are excess surface deliveries:
      !1. First cut back surface diversions
      !2. Next cut back groundwater diversions
      !3. If an excess of water is diverted to municipal area, increase return flow
      IF (DMDEL(ILND).GT.MunSurf(ILND,J,K)) THEN
        !First correct surface diversions from other sources
        ExcessDiv=DMDEL(ILND)-MunSurf(ILND,J,K)
        DO I=1,NQXMun(ILND)
          IUPQX=MunQXup(ILND,I)
          IDNQX=MunQXdn(ILND,I)
          IDVQX=MunQXin(ILND,I)
          IF (IUPQX.GT.0.AND.IDNQX.GT.0.AND.QX(IDVQX,J,K).GT.0.0) THEN
            ExcessCorr=MIN(ExcessDiv,QX(IDVQX,J,K))
            QX(IDVQX,J,K)=QX(IDVQX,J,K)-ExcessCorr
            QX(IDNQX,J,K)=QX(IDNQX,J,K)+ExcessCorr
            DMDEL(ILND)=DMDEL(ILND)-ExcessCorr
            ExcessDiv=ExcessDiv-ExcessCorr
            IF (ExcessDiv.LE.0.0) THEN
              EXIT
            END IF
          END IF
        END DO
        IF (ExcessDiv>0.0) THEN
        !Next correct groundwater diversions
          IF (QX(MunGWQx(ILND),J,K).GT.0.0) THEN
            ExcessCorr=MIN(ExcessDiv,QX(MunGWQx(ILND),J,K))
            QX(MunGWQx(ILND),J,K)=QX(MunGWQx(ILND),J,K)-ExcessCorr
            ExcessDiv=ExcessDiv-ExcessCorr
            DMPUM(ILND)=QX(MunGWQx(ILND),J,K)
          END IF
        END IF
        IF (ExcessDiv.GT.0.0) THEN
          QX(MunQXRet(ILND),J,K)=MunReturn(ILND,J,K)+ExcessDiv
        ELSE
          QX(MunQXRet(ILND),J,K)=MunReturn(ILND,J,K)
        END IF
      ELSE
        QX(MunQXRet(ILND),J,K)=MunReturn(ILND,J,K)
      END IF
      !If there is unsatisfied demand, increase GW pumping
      IF(MunGWQx(ILND).GT.0)THEN
        IF (DEMLFT.GT.0) THEN
          QX(MunGWQx(ILND),J,K)=QX(MunGWQx(ILND),J,K)+DEMLFT
          QGW=QGW+DEMLFT
          DMPUM(ILND)=QX(MunGWQx(ILND),J,K)
        END IF
      END IF
      RETURN
      END
      SUBROUTINE WETLND(ILND)
!-----------------------------------------------------------------------
!     SUBROUTINE WETLND
!     Created 11/29/88 by Craig W. Miller
!
!     This subroutine calculates depletions by wetlands
!     It requires that the QX number upstream and downstream
!     as well as the QX number of the wetland depletion be
!     specified in the input data
!
!     Variables passed in common blocks:
!     IQWET  - QX number of the wetland water use
!     IQUP   - QX number of QX upstream
!     IQDOWN - QX number downstream of wetlands
!-----------------------------------------------------------------------
      use PARAMDIM
      use COMMO
      use PrintStuff
      INTEGER*4 ILND,IQWET,IQUPST,IQDOWN
      IQWET = IPH(ILND)
      IQUPST=IPHU(ILND)
      IQDOWN=IPHD(ILND)
      IF(IQWET.GT.0)THEN
        IF (IQUPST.GT.0) THEN
          QX(IQWET,J,K)  = MIN(WETUSE(K,J,ILND),MAX(0.0,QX(IQUPST,J,K)))
        END IF
        IF (IQDOWN.GT.0) THEN
          QX(IQDOWN,J,K) = QX(IQUPST,J,K) - QX(IQWET,J,K)
        END IF
      ENDIF
      RETURN
      END
      REAL (KIND=8) FUNCTION WETDMD (ILND)
        use PARAMDIM
        use COMMO
        use PrintStuff
        INTEGER (KIND=4) :: ILND
        WETDMD = WETUSE(K,J,ILND)
      END

      SUBROUTINE REINI(IRES)
!-----------------------------------------------------------------------
!     SUBROUTINE REINI
!     Created 12/06/88 by Craig W. Miller
!
!     This subroutine calculates the initial storage and evaporation
!     into a reservoir.  This is code that originally was in the main
!     program.
!
!     Variables:
!     IQXIN(IRES)  - QX number of inflow to reservoir
!     IQXOU(IRES)  - QX number of reservoir outflow
!     IQXAD(IRES)  - QX number of to add reservoir leakage
!     QF(IRES)     - Proportion of reservoir seepage returning to stream
!     QSM(IRES,K)  - Reservoir seepage
!     QMNLMT(IRES) - Logic variable, .TRUE. means to limit minimum
!                    releases to minimum inflow
!     QRMN(IRES,K) - Required minimum release from the reservoir
!-----------------------------------------------------------------------
      use PARAMDIM
      use COMMO
      use PrintStuff
      EXTERNAL SEEP
      REAL*8 QRI,QRMNX,QRO,AR,EVUSE,SEEPAGE,SEEP,DSMX
      INTEGER*4 II,IQXI,IRES,IOPT
      IOPT=1
      GO TO 10
      ENTRY READD(IRES)
      IOPT=2
 10   CONTINUE
      QRI=0.0
      QRO=0.0
      IF(IRES.LT.1.OR.IRES.GT.NRES)THEN
         WRITE(*,*)'Call to REINI does not have valid reservoir #'
         WRITE(6,*)'Call to REINI does not have valid reservoir #'
         STOP
      ENDIF
      IF (DRAWN(IRES)) THEN
        RETURN
      END IF
      IF(.NOT.DRAWN(IRES))THEN
          DO II=1,NQXIN(IRES)
              IQXI=IQXAD(IRES,II)
              IF(IQXI.GT.0)THEN
                  QRI=QRI+QX(IQXI,J,K)
              ENDIF
          ENDDO
      ENDIF
      AR=MAX(0.0,RAR(IRES))

      IF(QMNLMT(IRES).AND.QRI.GE.QRMN(IRES,K)) THEN
!        LIMIT MIN RELEASE TO INFLOW
         QRMNX=QRMN(IRES,K)
         QRMN(IRES,K)=QRI
         CALL RESV(IRES,QRI,QRO)
         QRMN(IRES,K)=QRMNX
      ELSE
         CALL RESV(IRES,QRI,QRO)
      END IF
!      COMPUTE CHANGE IN STORAGE
      DO II=1,NQTG(IRES)
         IQXI=IQTG(IRES,II)
         IF(IQXI.GT.0)THEN
           QX(IQXI,J,K)=QRO+QX(IQXI,J,K)
         ENDIF
      ENDDO
      DRAWN(IRES)=.TRUE.
      CALLED(IRES)=.FALSE.
      RETURN
      END

      SUBROUTINE AVCLC
!-----------------------------------------------------------------------
!     SUBROUTINE AVCLC
!     Created 02/21/89 by Craig W. Miller
!
!     This subroutine calculates the average values printed on the
!     last page of program output.  It must be called after each
!     call to LNDCLC.  Since this is performed in BUDGET2, the user does
!     not need to be aware of its functions.
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
      use PARAMDIM
      use COMMO
      use PrintStuff
!
!     CALCULATE AVERAGE VALUES FOR TABLE
!
      REAL*8 TOTEF,WSHR
      REAL*8 VALU,VALU1,VALU2,VALU3,VALU4
      INTEGER*4 NQXPH,IQRT
!     REAL*8 TFIRS
!     DATA TFIRS/0/
      DO 10 L=1,NLND
        IF(TEF(L,J).GT.0)THEN
           TOTEF=TEF(L,J)
        ELSE
           TOTEF=1.0
        ENDIF
        CALL FAVER(GWLOS,RETFL(L)*(1.-RETI(L))/FNYRS,REAL(1.,KIND=8), &
          REAL(1.,KIND=8),K)
  !     changed 5/4/98
  !      CALL FAVER(CUDEF,MIN(SHORT(L,J,K),WCUSE(L,J,K)+CONUSE(K,J,L)
  !     +        + LGUSE(L,J,K)
  !     * +(RTUSE(L,K)+SBUSE(L,K)+GWUSE(L,K))/
  !     * TOTEF)*TOTEF/FNYRS,1.,1.,K)
        CALL FAVER(CUDEF,MIN(SHORT(L,J,K),WCUSE(L,J,K)+CONUSE(K,J,L)) &
          /FNYRS,REAL(1.,KIND=8),REAL(1.,KIND=8),K)
        CALL FAVER(TDIVR,AGDEL(L)/FNYRS,REAL(1.,KIND=8),REAL(1.,KIND=8),K)
        CALL FAVER(RZSUP,RETUSE(L)/FNYRS,REAL(1.,KIND=8),REAL(1.,KIND=8),K)
        CALL FAVER(RIVGW,GWRIV(L)/FNYRS,REAL(1.,KIND=8),REAL(1.,KIND=8),K)
        !CALL FAVER(GWMI,GWMIRT(L)/FNYRS,REAL(1.,KIND=8),REAL(1.,KIND=8),K)
        CALL FAVER(RFLO,RETFL(L)/FNYRS,REAL(1.,KIND=8),REAL(1.,KIND=8),K)
        CALL FAVER(RZ,(AGWAT(L)+GWRIV(L)+WPERC(K,J,L)+RetUse(L))/ &
          FNYRS,REAL(1.,KIND=8), REAL(1.,KIND=8),K)
        CALL FAVER(CHGW,(QRTF(L)+RETUSE(L)-QDVA(L))/FNYRS,REAL(1.,KIND=8), &
          REAL(1.,KIND=8),K)
  !     BUG(1,K)=LGDEL(L)+DMPUMA(L)
  !     SELECT CASE (K)
  !     CASE (1,8:12)
  !         BUG(2,K)=MAX(0.0,MIN(LGPOT(L,J,K),
  !    +            (LGDEL(L)+DMPUMA(L))*IEFF(L)/100.+
  !    +            LGEFPRE(L,K)))-LGEFPR(ILND,K)
  !     CASE DEFAULT
  !         BUG(2,K)=-LGEFPRE(L,K)
  !     END SELECT
  !     BUG(3,K)=LGQDVA(L)
  !     BUG(4,K)=BUG(1,K)-BUG(2,K)-BUG(3,K)
        CALL FAVER(RFGW,QRTF(L)/FNYRS,REAL(1.,KIND=8),REAL(1.,KIND=8),K)
        CALL FAVER(SMSPL,SPILL(L)/FNYRS,REAL(1.,KIND=8),REAL(1.,KIND=8),K)
        CALL FAVER(ACUSM,MOIST(L)/FNYRS,REAL(1.,KIND=8),REAL(12.,KIND=8),K)
        CALL FAVER(WPCUM,WMOIS(L)/FNYRS,REAL(1.,KIND=8),REAL(12.,KIND=8),K)
        CALL FAVER(CHSM,(BEFM(L)-MOIST(L))/FNYRS,REAL(1.,KIND=8), &
          REAL(1.,KIND=8),K)
        CALL FAVER(WPCHG,(BEFP(L)-WMOIS(L))/FNYRS,REAL(1.,KIND=8), &
          REAL(1.,KIND=8),K)
        IQRT=IRT(L)
  !      CALL FAVER(TOTRF,(QX(IQRT,J,K)-(DMPUM(L)+DMDEL(L))*DEF(L,K)- &
  !              LGQRTF(L)) &
  !              /FNYRS,REAL(1.,KIND=8),REAL(1.,KIND=8),K)
        CALL FAVER(TOTRF,QX(IRT(L),J,K)/FNYRS,REAL(1.,KIND=8),REAL(1.,KIND=8),K)
 10   CONTINUE
      DO L=1,NMUN
        CALL FAVER(DDIV,(DMDEL(L))/FNYRS,REAL(1.,KIND=8), &
          REAL(1.,KIND=8),K)
        CALL FAVER(DPUM,(DMPUM(L))/FNYRS,REAL(1.,KIND=8), &
          REAL(1.,KIND=8),K)
        VALU2=DMPUM(L)
        VALU3=DMDEL(L)
        VALU4=DEF(L,K)
        VALU1=(VALU2+VALU3)*VALU4
  !     VALU=(VALU1+LGQDVA(L))/FNYRS
        VALU=(VALU1+LGQRTF(L))/FNYRS
        CALL FAVER(DRET,VALU,REAL(1.,KIND=8),REAL(1.,KIND=8),K)
        CALL FAVER(TMUN,(DMPUM(L)+DMDEL(L))/FNYRS,REAL(1.,KIND=8), &
          REAL(1.,KIND=8),K)
        VALU1=(DMPUM(L)+DMDEL(L))*(1.-DEF(L,K))/FNYRS
        CALL FAVER(DMUSE,VALU1,REAL(1.,KIND=8),REAL(1.,KIND=8),K)
        VALU2=MAX(0.0,MIN(LGPOT(L,J,K),(LGDEL(L)+DMPUMA(L))* &
                LGEFF(L)/100.+LGEFPRE(L,K))/FNYRS)
        CALL FAVER(LGOUT,VALU2,REAL(1.,KIND=8),REAL(1.,KIND=8),K)
        CALL FAVER(DSUP,VALU1+VALU2,REAL(1.,KIND=8),REAL(1.,KIND=8),K)
        SELECT CASE (K)
        CASE (1,8:12)
            CALL FAVER(DMACU,VALU1+VALU2-LGEFPRE(L,K)/FNYRS, &
              REAL(1.,KIND=8),REAL(1.,KIND=8),K)
        CASE DEFAULT
            CALL FAVER(DMACU,VALU1+VALU2,REAL(1.,KIND=8), &
              REAL(1.,KIND=8),K)
        END SELECT
      END DO
      DO L=1,NRIP
        NQXPH=IPH(L)
        IF(NQXPH.GT.0)THEN
           CALL FAVER(WCONS,QX(NQXPH,J,K)/FNYRS,REAL(1.,KIND=8), &
             REAL(1.,KIND=8),K)
           WSHR=MAX(WETUSE(K,J,L)-QX(NQXPH,J,K),0.0)/FNYRS
           CALL FAVER(WSHOR,-WSHR,REAL(1.,KIND=8),REAL(1.,KIND=8),K)
        ENDIF
      END DO
      RETURN
      END

      SUBROUTINE ARBAK(ILND)
!-----------------------------------------------------------------------
!     SUBROUTINE FOR ITERATIVE SOLUTIONS INVOLVING LAND AREAS
!-----------------------------------------------------------------------
      use PARAMDIM
      use COMMO
      use PrintStuff
      INTEGER*4 II,ILND,M,LL,IRQX
      ITERA=ITERA+1
      IF(ITERA.GT.ITERMX)THEN
         WRITE(6,1)ITERA,J,K,(II,QX(II,J,K),II=1,NQX)
 1       FORMAT('0ITERATION ',I3,' YEAR ',I4,' MONTH ',I3, &
            ' PRINTOUT OF QX NUMBERS AND FLOWS FOLLOWS '/ &
            (6(' (',I3,') ',E11.4)))
         IF(ITERA.GT.ITERMX+10)THEN
            WRITE(6,'('' ITERATION LIMIT EXCEEDED, PROGRAM HALTED'')')
            WRITE(*,'('' ITERATION LIMIT EXCEEDED, Press ENTER'')')
            READ(*,*)
            STOP
         ENDIF
      ENDIF
      DO II=1,NLndQX(ILND)
        IF (LRES(ILND,II).GT.0) THEN
          DO M=1,NILNQ(ILND,II)
            IRQX=ILNQ(ILND,II,M)
            IF(IRQX.GT.0)THEN
               QX(IRQX,J,K)=QX(IRQX,J,K)-RESCAL(ILND,II)
            ENDIF
          END DO
          STO(LResRow(ILND,II))=STO(LResRow(ILND,II))+RESCAL(ILND,II)
          RESCAL(ILND,II)=0
        END IF
      END DO
      IF(LNRS(ILND).GT.0)THEN
      ENDIF
      MOIST(ILND)=BEFM(ILND)
      WMOIS(ILND)=BEFP(ILND)
      CALL RTFLO(-ILND,QDVA(ILND),QRTF(ILND))
      RETURN
      END

      SUBROUTINE MUNBAK(ILND)
!-----------------------------------------------------------------------
!     SUBROUTINE FOR ITERATIVE SOLUTIONS INVOLVING LAND AREAS
!-----------------------------------------------------------------------
      use PARAMDIM
      use COMMO
      use PrintStuff
      INTEGER*4 II,ILND,M,LL,IRQX
      ITERA=ITERA+1
      IF(ITERA.GT.ITERMX)THEN
         WRITE(6,1)ITERA,J,K,(II,QX(II,J,K),II=1,NQX)
 1       FORMAT('0ITERATION ',I3,' YEAR ',I4,' MONTH ',I3, &
            ' PRINTOUT OF QX NUMBERS AND FLOWS FOLLOWS '/ &
            (6(' (',I3,') ',E11.4)))
         IF(ITERA.GT.ITERMX+10)THEN
            WRITE(6,'('' ITERATION LIMIT EXCEEDED, PROGRAM HALTED'')')
            WRITE(0,'('' ITERATION LIMIT EXCEEDED, Press ENTER'')')
            READ(0,*)
            STOP
         ENDIF
      ENDIF
      DO II=1,NQXMun(ILND)
        IF (MResNum(ILND,II).GT.0) THEN
          DO LL=1,NQXup(ILND,II)
              IRQX=MQXup(ILND,II,LL)
              IF(IRQX.GT.0)THEN
                 QX(IRQX,J,K)=QX(IRQX,J,K)-MRESCAL(ILND,II)
              ENDIF
          END DO
          STO(MResNum(ILND,II))=STO(MResNum(ILND,II))+MRESCAL(ILND,II)
          RESCAL(ILND,II)=0
        END IF
      END DO
      RETURN
      END SUBROUTINE

      REAL*8 FUNCTION RSBAK(IR)
!-----------------------------------------------------------------------
!     SUBROUTINE RSBAK
!     CREATED 03/15/89 BY Craig W. Miller
!
!     Subroutine to calculate balancing inflow to reservoir IR given stage and
!     reservoir outflow and/or inflow through time.
!
!-----------------------------------------------------------------------
      use PARAMDIM
      use COMMO
      use PrintStuff
      REAL*8 DAT(13),VALU,QRO,QRI,AR,EV,STBEF,SEEP,RESELEV,ThisFactor
      INTEGER*4 IR,IY,IXP,KK,IND,II,IQXI
      CHARACTER*80 HEADC,Message
      LOGICAL (KIND=1) :: FlowDefined,StorageDefined
      FlowDefined=.FALSE.
      StorageDefined=.FALSE.
      QRI=0.0
      QRO=0.0
      IF(TargetRelOpt(IR).EQ.1)THEN
         RELEAS(IR,J,K)=QRMN(IR,K)
         FlowDefined=.TRUE.
      ELSE IF (TargetRelOpt(IR).EQ.2) THEN
         RELEAS(IR,J,K)=QIN(TargetRelQIN(IR),J,K)
         DO II=1,NQTG(IR)
           IQXI=IQTG(IR,II)
           IF(IQXI.GT.0)THEN
             QX(IQXI,J,K)=QIN(TargetRelQIN(IR),J,K)+QX(IQXI,J,K)
           ENDIF
         END DO
         FlowDefined=.TRUE.
      ENDIF
      IF (TargetRelOpt(IR).GT.0) THEN
        QRO=RELEAS(IR,J,K)
        FlowDefined=.TRUE.
      END IF
      IF(TargetStorOpt(IR).EQ.1)THEN
         RSTORE(IR,J,K)=StorTarg(IR,K)
         StorageDefined=.TRUE.
      ELSE IF (TargetStorOpt(IR).EQ.2) THEN
         RSTORE(IR,J,K)=QIN(TargetStorQIN(IR),J,K)
         StorageDefined=.TRUE.
      END IF
      DO II=1,NQXIN(IR)
         IQXI=IQXAD(IR,II)
         IF(IQXI.GT.0)THEN
            QRI=QRI+QX(IQXI,J,K)
         ENDIF
      END DO
      IF (QRI.NE.0.0) THEN
         FlowDefined=.TRUE.
      END IF
      IF (K.EQ.1) THEN
        IF (J.EQ.1) THEN
          STBEF=STOIC(IR)
        ELSE
          STBEF=ST(IR,J-1,12)
        END IF
      ELSE
        STBEF=ST(IR,J,K-1)
      END IF
      ST(IR,J,K)=RSTORE(IR,J,K)
      CALL RACE(IR,STBEF,AR,RESELEV)
      RAR(IR)=AR
      REL(IR)=RESELEV
      STO(IR)=ST(IR,J,K)
      IF (EvapFactor(IR).LE.0.0) THEN
        ThisFactor=1.0
      ELSE
        ThisFactor=EvapFactor(IR)
      END IF
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
      EV=EVAP(IR,J,K)
      IF (.NOT.(FlowDefined.AND.StorageDefined)) THEN
        Write(6,"(10X,'Insufficient Res(',I2,') Data for Month ',I2," // &
          "' Year ',I2)")IR,K,J
        WRITE(Message,"(10X,'Insufficient Res(',I2,') Data for Month ',I2," // &
          "' Year ',I2)")IR,K,J
        Print *,Message
      END IF
      DRAWN(IR)=.TRUE.
      CALLED(IR)=.FALSE.
      RSBAK=QRO+EV+RSTORE(IR,J,K)-STBEF-QRI
      RETURN
      END
!-----------------------------------------------------------------------
!     SUBROUTINE RSOUT
!     CREATED 06/22/11 BY Craig W. Miller
!
!     Subroutine to calculate balancing outflow to reservoir IR given stage and
!     reservoir outflow and/or inflow through time.
!-----------------------------------------------------------------------
      REAL*8 FUNCTION RSOUT(IR)
      use PARAMDIM
      use COMMO
      use PrintStuff
      INTEGER (KIND=4),INTENT(IN) :: IR
      REAL (KIND=8),EXTERNAL :: RSBAK
      RSOUT=-RSBAK(IR)
      if (RSOUT.LT.0.0) then
        STO(IR)=STO(IR)+RSOUT
        ST(IR,J,K)=STO(IR)
        RSOUT=0.0
      end if
      RETURN
      END
!-----------------------------------------------------------------------
!     SUBROUTINE TO ROUTE TARGET RELEASES
!-----------------------------------------------------------------------
      RECURSIVE SUBROUTINE RESREL(IR)
      use PARAMDIM
      use COMMO
      use PrintStuff
      SAVE
      INTEGER*4 II,IR,IQXI,IRD
      DO 10 II=1,19
         IQXI=IQTG(IR,II)
         IF(IQXI.GT.0)THEN
            QX(IQXI,J,K)=QX(IQXI,J,K)+DMD
         ENDIF
 10   CONTINUE
      IF(IREST(IR).GT.0)THEN
         IRD=IREST(IR)
         STO(IRD)=STO(IRD)+DMD
         IF(STO(IRD).GT.SMX(IRD))THEN
            DMD=STO(IRD)-SMX(IRD)
            CALL RESREL(IRD)
         ENDIF
      ENDIF
      RETURN
      END
!-----------------------------------------------------------------------
!     SUBROUTINE TO DETERMINE LAND AREA DEMAND
!-----------------------------------------------------------------------
      REAL*8 FUNCTION ARDEM(ILND)
      use PARAMDIM
      use COMMO
      use PrintStuff
      REAL*8 TOTEF,AGDM,DMDM,TOTDM,GWATER
      INTEGER*4 ILND
      IF(TEF(ILND,J).GT..0000001)THEN
         TOTEF=TEF(ILND,J)
      ELSE
         TOTEF=1.0
      ENDIF
      IF (GWQX(ILND)>0) THEN
        GWATER=QX(GWQX(ILND),J,K)
      ELSE
        GWATER=0.0
      END IF
      AGDM=(CONUSE(K,J,ILND)-GWATER*CEFF(ILND,J))/TOTEF
      TOTDM=+AGDM+(MAX(SOIL(ILND)-MOIST(ILND)- &
       PERCO(K,J,ILND),0.0))/TOTEF
      ARDEM=TOTDM
      RETURN
      END
!-----------------------------------------------------------------------
!     SUBROUTINE TO DETERMINE MUNICIPAL DEMAND
!-----------------------------------------------------------------------
      REAL (KIND=8) FUNCTION DMDMD(ILND)
      use PARAMDIM
      use COMMO
      use PrintStuff
      INTEGER*4 ILND
      DMDMD=MunSurf(ILND,J,K)
      RETURN
      END
!-----------------------------------------------------------------------
!     FUNCTION SOLVE SOLVES FOR STEPSIZE GIVEN A DIFFERENCE DIFF
!     BETWEEN A MEASURED AND A GAGED AMOUNT.  CFACI MUST BE GREATER
!     THAN 1.0 OR A DEFAULT VALUE OF 1.5 IS USED.
!-----------------------------------------------------------------------
      REAL*8 FUNCTION SOLVE(DIFF,CFACI,ITER)
      use PARAMDIM
      use COMMO
      REAL*8 CFAC,CFACI,DIFF2,CORR,C1,C2,DIFF
      INTEGER*4 ITER
      CFAC=CFACI
      IF(CFAC.LT.1.0)CFAC=1.5
      ITER=MAX(0,ITER)
      IF(ITER.EQ.0)THEN
         DIFF2=0.0
      ENDIF
      ITER=ITER+1
      CORR = DIFF
      IF(DIFF2.NE.0.0)THEN
         C1=DIFF2/ABS(DIFF2)
         C2=DIFF /ABS(DIFF)
         IF(C1*C2.GT.0.0)THEN
            CORR=DIFF*CFAC**(ITER-1)
            DIFF2=DIFF
         ELSE
            ITER=0
            DIFF2=0.0
         ENDIF
      ELSE
         DIFF2=DIFF
      ENDIF
      SOLVE = CORR
      RETURN
      END
