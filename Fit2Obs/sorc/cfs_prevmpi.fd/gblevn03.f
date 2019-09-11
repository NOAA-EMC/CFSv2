C***********************************************************************
C***********************************************************************
C  GBLEVN03 - INTERPOLATE MODEL DATA (FIRST GUESS OR ANALYSIS) TO OB
C             LOCATIONS
C-----------------------------------------------------------------------
      SUBROUTINE GBLEVN03(SUBSET) ! FORMERLY SUBROUTINE GETFC

      USE GBLEVN_MODULE

      REAL(8)   OBS,QMS,BAK,SID
      CHARACTER*8  SUBSET

      COMMON /GBEVAA/ SID,OBS(15,255),QMS(12,255),BAK(12,255),XOB,
     $ YOB,DHR,TYP,NLEV
      COMMON /GBEVEE/PSG01,ZSG01,TG01(500),UG01(500),VG01(500),
     x     QG01(500),zint(500),pint(500),pintlog(500),plev(500),
     x     plevlog(500)



      DATA TZERO / 273.15 /
      DATA BETAP / .0552  /
      DATA BETA  / .00650 /
      DATA ROG   / 29.261 /

C  CLEAR THE BACKGROUND EVENT ARRAY
C  --------------------------------

      BAK = BMISS

C  GET GUESS PROFILE AT OB LOCATION
C  --------------------------------
      CALL GBLEVN06(XOB,YOB)

      
C  INTERPOLATE GUESS PROFILES TO OB PRESSURES
C  ------------------------------------------

      IF(NLEV.GT.0)  THEN
      DO 10 L=1,NLEV

         POB  = OBS( 1,L)
         QOB  = OBS( 2,L)
         TOB  = OBS( 3,L)
         ZOB  = OBS( 4,L)
         UOB  = OBS( 5,L)
         VOB  = OBS( 6,L)
         PWO  = OBS( 7,L)
         PW1O = OBS( 8,L)
         PW2O = OBS( 9,L)
         PW3O = OBS(10,L)
         PW4O = OBS(11,L)
         CAT  = OBS(12,L)

         IF(POB.LE.0. .OR. POB.GE.BMISS) GOTO 10

         poblog = log(pob)
         
         la = -999
         lb = -999
         do k=1,kmax-1
            if (poblog<=plevlog(k) .and. poblog>plevlog(k+1)) then
               la = k
               lb = k+1
               exit
            endif
         enddo
         if (la > 0) then
            wt = (poblog-plevlog(lb)) / (plevlog(la)-plevlog(lb))
         else
            la = 1
            lb = la+1
            wt = 0.0
         endif
         
         li=0
         do k=1,kmax-1
            if (poblog<=pintlog(k) .and. poblog>pintlog(k+1)) then
               li = k
               exit
            endif
         enddo

C  SURFACE PRESSURE
C  ----------------

         IF(CAT.EQ.0 .AND. ZOB.LT.BMISS) THEN
            TS = TG01(1) + (PSG01-PLEV(1))*BETAP
            DZ  = ZOB-ZSG01
            TM  = TS - DZ*BETA*.5
            PFC = PSG01*EXP(-DZ/(TM*ROG))
         ELSE
            PFC = BMISS
         ENDIF

C  SPECIFIC HUMIDITY
C  -----------------

         IF(QOB.LT.BMISS.OR.TOB.LT.BMISS.OR.TYP.EQ.111) THEN

C  (QFC NEEDED BY SYNDATA PROGRAM BUT ONLY FOR REPORT TYPE 111)

            QOB = QG01(LB) + (QG01(LA)-QG01(LB))*WT
         ENDIF

C  TEMPERATURE
C  -----------

         IF(TOB.LT.BMISS.OR.SUBSET.EQ.'VADWND  '.OR.TYP.EQ.111) THEN

C  (TFC NEEDED BY CQCVAD AND SYNDATA PROGRAMS, LATTER ONLY FOR REPORT
C   TYPE 111)

            IF(POB.GT.PLEV(1)) THEN
               TOB = TG01(1) + (POB-PLEV(1))*BETAP
            ELSE
               TOB = TG01(LB) + (TG01(LA)-TG01(LB))*WT
            ENDIF
            TOB = TOB - TZERO
         ENDIF

C  HEIGHT
C  ------

         IF(ZOB.LT.BMISS) THEN
            IF(POB.GT.PLEV(1)) THEN
               TM = TG01(1) + (.5*(PINT(1)+POB)-PLEV(1))*BETAP
               ZOB = ZINT(1) - ROG*TM*LOG(POB/PINT(1))
            ELSE
               TM = TG01(LB) + (TG01(LA)-TG01(LB))*WT
               ZOB = ZINT(LI) - ROG*TM*LOG(POB/PINT(LI))
            ENDIF
         ENDIF

C  U AND V COMPONENTS
C  ------------------

         IF(UOB.LT.BMISS .OR. VOB.LT.BMISS) THEN
            UOB = UG01(LB) + (UG01(LA)-UG01(LB))*WT
            VOB = VG01(LB) + (VG01(LA)-VG01(LB))*WT
         ENDIF


C  PRECIPITABLE WATER
C  ------------------

         PWO  = BMISS
         PW1O = BMISS
         PW2O = BMISS
         PW3O = BMISS
         PW4O = BMISS

C  RELATIVE HUMIDITY
C  -----------------

         RHO = BMISS

C  SCATTER THE PROPER FIRST GUESS/ANALYSIS VALUES
C  ----------------------------------------------

         BAK(1,L)  = PFC
         BAK(2,L)  = QOB
         BAK(3,L)  = TOB
         BAK(4,L)  = ZOB
         BAK(5,L)  = UOB
         BAK(6,L)  = VOB
         BAK(7,L)  = PWO
         BAK(8,L)  = PW1O
         BAK(9,L)  = PW2O
         BAK(10,L) = PW3O
         BAK(11,L) = PW4O
         BAK(12,L) = RHO

   10 ENDDO
      ENDIF

      RETURN
      END
