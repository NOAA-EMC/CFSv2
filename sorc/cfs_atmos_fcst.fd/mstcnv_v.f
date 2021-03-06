      SUBROUTINE MSTCNV(IM,IX,KM,DT,T1,Q1,PRSL,DEL,PRSLK,RAIN
     &,                                                 lprnt,ipr)
!!
      USE MACHINE , ONLY : kind_phys
      USE FUNCPHYS , ONLY : fpvs, ftdp, fthe, stma, fpkap, ftlcl
      USE PHYSCONS, HVAP => con_HVAP, CP => con_CP, RV => con_RV
     &,             EPS => con_eps, EPSM1 => con_epsm1, grav => con_g
      implicit none
!!
!  PHYSICAL PARAMETERS
      real(kind=kind_phys) elocp, el2orc
      PARAMETER(ELOCP=HVAP/CP, EL2ORC=HVAP*HVAP/(RV*CP))
!
!
      logical              lprnt
      integer              im,ix,km,ipr
      real(kind=kind_phys) dt,rain(im)
!
      real(kind=kind_phys) PRSL(IX,KM), PRSLK(IX,KM), DEL(IX,KM)
      real(kind=kind_phys) T1(IX,KM),   Q1(IX,KM)
!
!  LOCAL VARIABLES
!
      real(kind=kind_phys) P(IM,KM),   TO(IM,KM), QO(IM,KM),   QS(IM,KM)
     &,                    THE(IM,KM), DQ(IM,KM), RAINLVL(IM,KM)
     &,                    ES(IM,KM)
      real(kind=kind_phys) pint(im), delqbar(im), deltbar(im), dqint(im)
     &,                    ei(im),   thebar(im),  theint(im)
     &,                    qevap,    dpovg, rnevap, slklcl, tdpd
     &,                    thelcl,   tlcl,  pmax
      integer KMLEV(im,KM), k, kmax, kk(im), ks(im), ke(im), i
!
      LOGICAL FLG(im), TOPFLG(im), TOTFLG
cc
cc--------------------------------------------------------------------
cc
      real(kind=kind_phys) cons_0               !constant
      real(kind=kind_phys) cons_1pdm8           !constant
cc
      cons_0          =         0.d0            !constant
      cons_1pdm8      =         1.d-8           !constant
cc
cc--------------------------------------------------------------------
cc
      KMAX   = 0
      DO K = 1, KM
        pmax   = 0.0
        do i=1,im
          pmax = max(prsl(i,k), pmax)
        enddo
        IF(pmax .GT. 60.0) KMAX = K + 1
      ENDDO
      if (lprnt) print *,' kmax=',kmax
!
cselaDG3 LATD = 57
cselaDG3 LOND = 1
cselaDG3 LATD = 48
cselaDG3 LOND = 176
C
C   SURFACE PRESSURE UNIT IS CB
C
      do i=1,im
        RAIN(i)    = 0.
        DELTBAR(i) = 0.
        DELQBAR(i) = 0.
        FLG(i)     = .FALSE.
        ks(i)      = 0
        ke(i)      = kmax + 1
        TOPFLG(i)  = .FALSE.
      enddo
      TOTFLG = .FALSE.
!
cselaDG3 IF(LAT.EQ.LATD) THEN
cselaDG3   PRINT *, ' T AND Q BEFORE ADJUSTMENT'
cselaDG3   PRINT 6000, (T1(k)-273.16,K=1,KMAX)
cselaDG3   PRINT 6000, (Q1(k)*1.E3,K=1,KMAX)
cselaDG3   PRINT *, ' PS =', PS
cselaDG3 ENDIF
!
C
C  COLUMN VARIABLES
C  P IS PRESSURE OF THE LAYER (CB)
C  TO IS TEMPERATURE AT T+DT (K)... THIS IS AFTER ADVECTION AND TURBULAN
C  QO IS MIXING RATIO AT T+DT (KG/KG)..Q1
C
      DO K = 1, KMAX
        do i=1,im
          P(i,k)  = 1000.0 * PRSL(I,K)
          TO(i,k) = T1(i,k)
          QO(i,k) = Q1(i,k)
        enddo
      ENDDO
C
C  MODEL CONSISTENT SATURATION MIXING RATIO
C
      DO K = 1, KMAX
        do i=1,im
          ES(i,k) = min(P(i,k), FPVS(T1(i,k)))
          QS(i,k) = EPS * ES(i,k) / (P(i,k) + EPSM1 * ES(i,k))
          QS(i,k) = MAX(QS(i,k),cons_1pdm8)     !constant
          IF(QO(i,k) .GT. QS(i,k)) FLG(i) = .TRUE.
        enddo
      ENDDO
      do i=1,im
        IF (FLG(i)) TOTFLG = .TRUE.
      enddo
      IF (.NOT. TOTFLG) RETURN
!
      DO K = 1, KMAX
        do i=1,im
          DQ(i,k)  = 0.
          THE(i,k) = TO(i,k)
        enddo
      ENDDO
C
C  COMPUTE THETA-E
C
      DO K = 1, KMAX
        do i=1,im
          IF(FLG(i)) THEN
            THE(i,k) = FTHE(TO(i,k),prslk(i,k))
            IF (THE(i,k) .EQ. 0.) THEN
              THE(i,k) = TO(i,k) / prslk(i,k)
            ENDIF
C           THE(i,k) = TO(i,k) * ((P(i,k) - ES(i,k))*0.01) ** (-ROCP)
C    &               * EXP(ELOCP * QS(i,k) / TO(i,k))
            DQ(i,k)  = QO(i,k)- QS(i,k)
C
C  MODIFICATION OF THETA-E FOR SUPER-SATURATION
C
            THE(i,k)= THE(i,k) * (1. + HVAP*MAX(DQ(i,k),cons_0)     !constant
     &                         / (CP*TO(i,k)))
      if (lprnt .and. i .eq. ipr) print *,' k=',k,' the=',the(i,k)
          ENDIF
        enddo
      ENDDO
!
cselaDG3 IF(LAT.EQ.LATD.AND.FLG(LOND)) THEN
cselaDG3   PRINT *, ' THETA-E, QS AND DQ BEFORE ADJUSTMENT'
cselaDG3   PRINT 6000, (THE(k)-273.16,K=1,KMAX)
cselaDG3   PRINT 6000, (QS(k)*1.E3,K=1,KMAX)
cselaDG3   PRINT 6000, (DQ(k)*1.E3,K=1,KMAX)
cselaDG3 ENDIF
!
      DO K = 1, KMAX
        do i=1,im
          KMLEV(i,k)   = 0
          RAINLVL(i,k) = 0.
        enddo
      ENDDO
C
C  STARTING POINT OF ADJUSTMENT
C
      k = 1
      do i=1,im
        KK(i)     = 0
        DQINT(i)  = 0.
        THEINT(i) = 0.
        THEBAR(i) = 0.
        PINT(i)   = 0.
C
C  FOR CONDITIONALLY UNSTABLE AND SUPERSATURATED LAYERS,
C    OBTAIN INTEGRATED THETA AND Q-QS
C
C  KMLEV KEEPS TRACK OF THE NUMBER OF LAYERS THAT SATISFIES
C    THE CONDITION FOR ADJUSTMENT
C
        IF(DQ(i,k).GT.0..AND.THE(i,k).GE.THE(i,K+1).AND.FLG(i)) THEN
          DQINT(i)   = DQINT(i)  + DQ(i,k) * DEL(i,K)
          THEINT(i)  = THEINT(i) + THE(i,k) * DEL(i,K)
          PINT(i)    = PINT(i)   + DEL(i,K)
          KK(i)      = KK(i) + 1
          KMLEV(i,k) = KK(i)
        ENDIF
      enddo
      DO K = 2, KMAX - 1
        do i=1,im
          IF(DQ(i,k).GT.0..AND.THE(i,k).GE.THE(i,K+1).AND.FLG(i)) THEN
            DQINT(i)  = DQINT(i)  + DQ(i,k) * DEL(i,K)
            THEINT(i) = THEINT(i) + THE(i,k) * DEL(i,K)
            PINT(i)   = PINT(i)   + DEL(i,K)
            KK(i)     = KK(i) + 1
            KMLEV(i,k) = KK(i)
          ENDIF
!
          IF (PINT(i) .GT. 0.)THEBAR(i) = THEINT(i) / PINT(i)
C
C  IF THE LAYER BELOW SATISFIES THE CONDITION AND THE PRESENT
C    LAYER IS COLDER THAN THE ADJSUTED THETA-E,
C    THE LAYER IS INCLUDED IF THE INTEGRATED MOISTURE EXCESS
C    CAN BE MAINTAINED
C
          IF (KMLEV(i,k) .EQ.0 .AND. KMLEV(i,K-1) .GT. 0 .AND.
     &       THEBAR(i) .GE. THE(i,k) .AND. .NOT. TOPFLG(i)) THEN
               DQINT(i) = DQINT(i) + DQ(i,k) * DEL(i,K)
!         ENDIF
!         IF (KMLEV(i,k) .EQ. 0 .AND. KMLEV(i,K-1) .GT. 0 .AND.
!    &        THEBAR(i)  .GE. THE(i,k) .AND. DQINT(i) .GT. 0.
!    &       .AND. .NOT. TOPFLG(i)) THEN
            if (dqint(i) .gt. 0) then
              KK(i) = KK(i) + 1
              KMLEV(i,k) = KK(i)
              TOPFLG(i)  = .TRUE.
              EI(i)   = P(i,k) * QO(i,k) / (EPS - EPSM1 * QO(i,k))
              EI(i)   = MIN(MAX(EI(i),cons_1pdm8),ES(i,k))     !constant
              TDPD    = MAX(TO(i,k)-FTDP(EI(i)),cons_0)     !constant
              TLCL    = FTLCL(TO(i,k), TDPD)
              SLKLCL  = prSLK(i,K) * TLCL / TO(i,k)
              THELCL  = FTHE(TLCL,SLKLCL)
              IF(THELCL.NE.0.) THEN
                THE(i,k) = THELCL
C               THE(i,k) = TO(i,k) * ((P(i,k) - EI(i))*.01) ** (-ROCP)
C    &               * EXP(ELOCP * MAX(QO(i,k),1.E-8) / TO(i,k))
              ENDIF
              THEINT(i) = THEINT(i) + THE(i,k) * DEL(i,K)
              PINT(i)   = PINT(i)   + DEL(i,K)
            endif
          ENDIF
C
C  RESET THE INTEGRAL IF THE LAYER IS NOT IN THE CLOUD
C
          IF (KMLEV(i,k) .EQ. 0 .AND. KMLEV(i,K-1) .GT. 0) THEN
            THEBAR(i) = THEINT(i) / PINT(i)
            DQINT(i)  = 0.
            THEINT(i) = 0.
            PINT(i)   = 0.
            KK(i)     = 0
            ks(i)     = k - 1
            ke(i)     = ks(i) - kmlev(i,k-1) + 1
            flg(i)    = .false.
          ENDIF
        enddo
      enddo
C
C  When within A CLOUD LAYER, COMPUTE THE MOIST-ADIABATIC
C    (TO AND QO) USING THE AVERAGED THETA-E AND THE RESULTANT RAIN
C
      do k = 1, kmax
        do i=1,im
          if (k .ge. ke(i) .and. k .le. ks(i)) then
            CALL STMA(THEBAR(i),PRSLK(i,k),TO(i,k),QO(i,k))
            THE(i,k) = THEBAR(i)
            QS(i,k)  = QO(i,k)
!
            DPOVG        = DEL(i,K) / grav
            RAINLVL(i,k) = (Q1(i,k) - QO(i,k)) * DPOVG
            DELTBAR(i)   = DELTBAR(i) + (TO(i,k) - T1(i,k)) * DPOVG
     &                                                      / PRSLK(i,k)
            DELQBAR(i)   = DELQBAR(i) - RAINLVL(i,K)
      if (lprnt) print *,' k=',k,' to=',to(i,k),' qo=',qo(i,k),
     & ' rainlvl=',rainlvl(i,k)
          ENDIF
        enddo
      ENDDO
!
!  EVAPORATION OF FALLING RAIN
!
      DO K = KMAX, 1, -1
        do i=1,im
          T1(i,k) = TO(i,k)
          Q1(i,k) = QO(i,k)
          RAIN(i) = RAIN(i) + RAINLVL(i,k)
          DQ(i,k) = (QO(i,k) - QS(i,k)) / (1. +
     &                EL2ORC*QS(i,k)/TO(i,k)**2)
          DPOVG = DEL(i,K) / grav
          IF (RAIN(i) .GT. 0. .AND. RAINLVL(i,k) .LE. 0.) THEN
            QEVAP   =-DQ(i,k)*(1.-EXP(-0.32*SQRT(DT*RAIN(i))))
            RNEVAP  = MIN(QEVAP*DPOVG,RAIN(i))
            Q1(i,k) = Q1(i,k)+RNEVAP/DPOVG
            T1(i,k) = T1(i,k)-RNEVAP/DPOVG*ELOCP
            RAIN(i) = RAIN(i) - RNEVAP
            DELTBAR(i) = DELTBAR(i) - RNEVAP * ELOCP
            DELQBAR(i) = DELQBAR(i) + RNEVAP
          ENDIF
        enddo
      ENDDO
!
cselaDG3 IF(LAT.EQ.LATD.AND.FLG(LOND)) THEN
cselaDG3   PRINT *, ' THETA-E AFTER ADJUSTMENT'
cselaDG3   PRINT 6000, (THE(k)-273.16,K=1,KMAX)
cselaDG3   PRINT *, ' T AND Q AFTER ADJUSTMENT'
cselaDG3   PRINT 6000, (T1(k)-273.16,K=1,KMAX)
cselaDG3   PRINT 6000, (Q1(k)*1.E3,K=1,KMAX)
cselaDG3   PRINT *, ' DELTBAR, DELQBAR =', DELTBAR*CP,DELQBAR*HVAP
cselaDG3   PRINT *, ' RAIN =', HVAP*RAIN
cselaDG3 ENDIF
!6000 FORMAT(2X,0P,11(F6.2,1H,))
!6100 FORMAT(2X,3P11F7.2)
!
      do i=1,im
        RAIN(i) = MAX(RAIN(i),cons_0)     !constant
      enddo
!
      RETURN
      END
