C***********************************************************************
C***********************************************************************
      SUBROUTINE GBLEVN02(IUNITP,IUNITS,NEWTYP)
                                         ! FORMERLY SUBROUTINE FILTAN

      DIMENSION    NFLGRT(100:299,12),OEMIN(2:6)
      CHARACTER*8  STNID
      CHARACTER*40 PEVN,QEVN,TEVN,WEVN,PWVN,PW1VN,PW2VN,PW3VN,PW4VN
      REAL(8)      PEV(4,255),QEV(4,255),TEV(4,255),WEV(5,255),
     $             PWV(4,255),PW1V(4,255),PW2V(4,255),PW3V(4,255),
     $             PW4V(4,255),OBS,QMS,BAK,SID
      LOGICAL      FCST,REJP_PS,REJPS,REJT,REJQ,REJW,REJPW,REJPW1,
     $             REJPW2,REJPW3,REJPW4,SATMQC,SATEMP,SOLN60,SOLS60,
     $             MOERR_P,MOERR_T,ADPUPA_VIRT,DOBERR,DOFCST,SOME_FCST

      COMMON /GBEVAA/ SID,OBS(15,255),QMS(12,255),BAK(12,255),XOB,
     $ YOB,DHR,TYP,NLEV
      COMMON /GBEVBB/ PVCD,VTCD
      COMMON /GBEVCC/ DOVTMP,DOFCST,SOME_FCST,DOBERR,FCST,VIRT,
     $ QTOP_REJ,SATMQC,ADPUPA_VIRT
      COMMON /GBEVEE/PSG01,ZSG01,TG01(500),UG01(500),VG01(500),
     x     QG01(500),zint(500),pint(500),pintlog(500),plev(500),
     x     plevlog(500)

      EQUIVALENCE (SID,STNID)

      DATA PEVN  /'POB  PQM  PPC  PRC      '/
      DATA QEVN  /'QOB  QQM  QPC  QRC      '/
      DATA TEVN  /'TOB  TQM  TPC  TRC      '/
      DATA WEVN  /'UOB  VOB  WQM  WPC  WRC '/
      DATA PWVN  /'PWO  PWQ  PWP  PWR      '/
      DATA PW1VN /'PW1O PW1Q PW1P PW1R     '/
      DATA PW2VN /'PW2O PW2Q PW2P PW2R     '/
      DATA PW3VN /'PW3O PW3Q PW3P PW3R     '/
      DATA PW4VN /'PW4O PW4Q PW4P PW4R     '/

      DATA BMISS /10E10/,NFLGRT/2400*0/

      DATA OEMIN /0.5,0.1,1.0,0.5,1.0/

      NI  = MOD((NINT(TYP)/10),10)

      IF(NEWTYP.EQ.1)  NFLGRT = 0

C  LOGICAL SWITCHES FOR OBSERVATION LOCATION FILTERING
C  ---------------------------------------------------

      SATEMP = ((TYP.GE.160.AND.TYP.LE.179).AND.SATMQC)
      SOLN60 = ((TYP.GE.160.AND.TYP.LE.163).AND.YOB.GE.-60.AND.SATMQC)
      SOLS60 = ((TYP.EQ.160.OR.TYP.EQ.162.OR.TYP.EQ.163).AND.YOB.LT.-60
     $ .AND.SATMQC)

C  CLEAR THE EVENT ARRAYS
C  ----------------------

      PEV  = BMISS
      QEV  = BMISS
      TEV  = BMISS
      WEV  = BMISS
      PWV  = BMISS
      PW1V = BMISS
      PW2V = BMISS
      PW3V = BMISS
      PW4V = BMISS

      MAXPEV  = 0
      MAXQEV  = 0
      MAXTEV  = 0
      MAXWEV  = 0
      MAXPWV  = 0
      MAXPW1V = 0
      MAXPW2V = 0
      MAXPW3V = 0
      MAXPW4V = 0

C  LOOP OVER LEVELS APPLYING UNDERGROUND FILTERING AND SPECIAL RULES
C  -----------------------------------------------------------------

      IF(NLEV.GT.0)  THEN
      DO L=1,NLEV

         POB  = OBS( 1,L)
         QOB  = OBS( 2,L)
         TOB  = OBS( 3,L)
         UOB  = OBS( 5,L)
         VOB  = OBS( 6,L)
         PWO  = OBS( 7,L)
         PW1O = OBS( 8,L)
         PW2O = OBS( 9,L)
         PW3O = OBS(10,L)
         PW4O = OBS(11,L)
         CAT  = OBS(12,L)
         PRSS = OBS(13,L)

         PQM  = QMS( 1,L)
         QQM  = QMS( 2,L)
         TQM  = QMS( 3,L)
         ZQM  = QMS( 4,L)
         WQM  = QMS( 5,L)
         PWQ  = QMS( 6,L)
         PW1Q = QMS( 7,L)
         PW2Q = QMS( 8,L)
         PW3Q = QMS( 9,L)
         PW4Q = QMS(10,L)

         REJP_PS = .FALSE.
         MOERR_P = .FALSE.
         MOERR_T = .FALSE.
         RCD = 99999

C  -------------------------------------------------------------------
C  RULES FOR PRESSURE (ON ANY LEVEL) -- ALL DATA ON LEVEL REJECTED IF:
C    - PRESSURE MORE THAN 100 MB BELOW MODEL (GUESS) SURFACE PRESSURE
C       (AND SWITCH FCST=TRUE) -- "PREVENT" PGM REASON CODE 1
C    - PRESSURE IS ZERO OR IS NEGATIVE -- "PREVENT" PGM REASON CODE 2
C  REJECTION MEANS Q.M. SET TO 8
C  -------------------------------------------------------------------

         IF(POB.LT.BMISS)  THEN
            IF(.NOT.FCST)  PSG01 = POB
            IF(POB-PSG01.GE.100. .OR. POB.LE.0.) THEN
               IF(POB.LE.0.) THEN
                  IF(NI.EQ.8)  THEN
                     WRITE(IUNITS,302) STNID,NINT(TYP),YOB,XOB,POB
  302 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
     $'E, REJECT ALL DATA ON SFC LVL - POB=',F6.1,' MB, FAILS SANITY ',
     $'CHECK')
                  ELSE
                     WRITE(IUNITS,101) STNID,NINT(TYP),YOB,XOB,POB
  101 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
     $'E, REJECT ALL DATA ON LVL - POB=',F6.1,' MB, FAILS SANITY CHECK')
                  ENDIF
                  RCD = 2
               ELSE
                  IF(NI.EQ.8)  THEN
                     WRITE(IUNITS,303) STNID,NINT(TYP),YOB,XOB,POB,PSG01
  303 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
     $ 'E, REJECT ALL DATA ON SFC LVL - POB=',F6.1,' MB, > 100 MB ',
     $ 'BELOW GES PSFC(=',F6.1,'MB)')
                  ELSE
                     WRITE(IUNITS,102) STNID,NINT(TYP),YOB,XOB,POB,PSG01
  102 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
     $ 'E, REJECT ALL DATA ON LVL - POB=',F6.1,' MB, > 100 MB BELOW ',
     $ 'GES PSFC(=',F6.1,' MB)')
                  ENDIF
                  RCD = 1
               ENDIF
               REJ = 8
               REJP_PS = .TRUE.
               PEV(1,L) = POB
               PEV(2,L) = REJ
               PEV(3,L) = PVCD
               PEV(4,L) = RCD
               MAXPEV = L
            ENDIF
         ENDIF

C  -------------------------------------------------------------------
C  RULES FOR SURFACE PRESSURE -- ALL DATA ON SURFACE LEVEL REJECTED IF:
C    - OBSERVATION ERROR IS MISSING (AND SWITCH DOBERR=TRUE) --
C       "PREVENT" PGM REASON CODE 3
C    - PRESSURE IS MORE THAN 100 MB ABOVE OR BELOW MODEL (GUESS)
C       SURFACE PRESSURE (AND SWITCH FCST=TRUE) -- 
C       "PREVENT" PGM REASON CODE 4
C    - PRESSURE IS REPORTED ABOVE 450 MB OR BELOW 1100 MB -- "PREVENT"
C       PGM REASON CODE 2
C    - PRESSURE VIOLATES RULES FOR PRESSURE ON ANY LEVEL (SEE ABOVE)
C  REJECTION FOR FIRST RULE MEANS Q.M. SET TO 9 UNLESS:
C      - ANY OTHER RULE CAUSES REJECTION, THEN Q.M. SET TO 8
C      - Q.M. IS ALREADY > 3 BUT NOT 15, THEN SKIP EVENT AND LEAVE Q.M.
C        AS IS
C  -------------------------------------------------------------------

         IF(POB.LT.BMISS .AND. CAT.EQ.0) THEN
            IF(.NOT.FCST)  PSG01 = POB
            REJPS = OEFG01(POB,TYP,5,OEMIN(5)).GE.BMISS   .OR.
     $              ABS(POB-PSG01).GE.100.                .OR.
     $              POB.LE.450.                           .OR.
     $              POB.GE.1100.
            IF(REJPS.OR.REJP_PS) THEN
               REJ = 8
               IF(.NOT.REJP_PS)  THEN
                  IF(ABS(POB-PSG01).GE.100.)  THEN
                     WRITE(IUNITS,104) STNID,NINT(TYP),YOB,XOB,POB,PSG01
  104 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
     $ 'E, REJECT ALL DATA ON SFC LVL - POB=',F6.1,' MB, > 100 MB ',
     $ 'ABOVE GES PSFC(=',F6.1,'MB)')
                     RCD = 4
                  ELSE IF(POB.LE.450..OR.POB.GE.1100.)  THEN
                     WRITE(IUNITS,105) STNID,NINT(TYP),YOB,XOB,POB
  105 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
     $ 'E, REJECT ALL DATA ON SFC LVL - POB=',F6.1,' MB, FAILS SANITY ',
     $ 'CHECK')
                     RCD = 2
                  ELSE
                     IF(NFLGRT(NINT(TYP),1).EQ.0)  THEN
                        WRITE(IUNITS,201) NINT(TYP)
  201 FORMAT(/' --> FOR ALL REPORTS WITH REPORT TYPE ',I3,', REJECT ',
     $ 'ALL DATA ON SURFACE LEVEL DUE TO MISSING SFC-P OBS ERROR'/)
                        NFLGRT(NINT(TYP),1) = 1
                     ENDIF
CDAK CDAK CDAK CDAK  WRITE(IUNITS,103) STNID,NINT(TYP),YOB,XOB,POB
CD103 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
CDAK $ 'E, REJECT ALL DATA ON SFC LVL - POB=',F6.1,'MB, MISSING OBS.',
CDAK $ ' ERROR')
                     RCD = 3
                     REJ = 9
                  ENDIF
               ENDIF
               REJP_PS = .TRUE.
               IF(RCD.EQ.3)  MOERR_P = .TRUE.
               IF(REJ.EQ.9.AND.(PQM.GT.3.AND.PQM.LT.15))  THEN
CDAKCDAKCDAKCDAK  WRITE(IUNITS,1401) STNID,NINT(TYP),YOB,XOB,PQM
 1401 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
     $ 'E, INPUT PQM =',F4.0,' -- DO NOT APPLY PSFC QM=9 EVENT')
               ELSE
                  PEV(1,L) = POB
                  PEV(2,L) = REJ
                  PEV(3,L) = PVCD
                  PEV(4,L) = RCD
                  MAXPEV = L
               ENDIF
            ENDIF
         ENDIF

C  -------------------------------------------------------------------
C  RULES FOR TEMPERATURE -- TOB AND QOB ON LEVEL REJECTED IF:
C    - OBSERVATION ERROR IS MISSING (AND SWITCH DOBERR=TRUE) --
C       "PREVENT" PGM REASON CODE 3
C    - THIS IS SFC LEVEL AND OBSERVATION ERROR FOR SFC PRESSURE IS
C       MISSING (AND SWITCH DOBERR=TRUE) -- "PREVENT" PGM REASON CODE 3
C    - REPORT IS TYPE 160-163 (LAND TOVS/RTOVS/ATOVS TEMPERATURE
C       SOUNDINGS, ALL PATHS), AND IS AT OR NORTH OF 60 DEGREES SOUTH
C       LATITUDE, AND PRESSURE ON LEVEL IS AT OR BELOW 100 MB (AND
C       SWITCH SATMQC=TRUE)  -- "PREVENT" PGM REASON CODE 6
C    - REPORT IS TYPE 160,162,163 (LAND TOVS/RTOVS/ATOVS TEMPERATURE
C       SOUNDINGS, ALL PATHS BUT CLEAR), AND IS SOUTH OF 60 DEGREES
C       SOUTH LATITUDE, AND PRESSURE ON LEVEL IS BELOW 100 MB (AND
C       SWITCH SATMQC=TRUE) -- "PREVENT" PGM REASON CODE 6
C    - THIS IS SFC LEVEL AND PRESSURE VIOLATES RULES FOR SFC PRESSURE
C       (EXCEPT FOR MISSING OBSERVATION ERROR, ALREADY COVERED IN RULE
C       2 ABOVE) (SEE ABOVE)
C    - PRESSURE ON LEVEL VIOLATES RULES FOR PRESSURE (SEE ABOVE)
C  REJECTION FOR FIRST TWO RULES MEANS Q.M. SET TO 9 UNLESS:
C      - ANY OTHER RULE CAUSES REJECTION, THEN Q.M. SET TO 8
C      - Q.M. IS ALREADY > 3 BUT NOT 15, THEN SKIP EVENT AND LEAVE Q.M.
C        AS IS
C  -------------------------------------------------------------------

         IF(TOB.LT.BMISS) THEN
            REJT = OEFG01(POB,TYP,2,OEMIN(2)).GE.BMISS  .OR. 
     $             (SOLN60.AND.NINT(POB*10.).GE.1000)   .OR.
     $             (SOLS60.AND.NINT(POB*10.).GT.1000)
            IF(REJT.OR.REJP_PS) THEN
               REJ = 8
               IF(.NOT.REJP_PS)  THEN
                  IF(SOLN60.AND.NINT(POB*10.).GE.1000)  THEN
                     IF(NFLGRT(NINT(TYP),6).EQ.0)  THEN
                        WRITE(IUNITS,7304) NINT(TYP)
 7304 FORMAT(/' --> FOR ALL REPORTS WITH REPORT TYPE ',I3,', REJECT ',
     $'TOB/QOB AT AND BELOW 100 MB IF REPORT IS NORTH OF 60S LATITUDE'/)
                        NFLGRT(NINT(TYP),6) = 1
                     ENDIF
                     RCD = 6
                  ELSE  IF(SOLS60.AND.NINT(POB*10.).GT.1000)  THEN
                     IF(NFLGRT(NINT(TYP),7).EQ.0)  THEN
                        WRITE(IUNITS,7305) NINT(TYP)
 7305 FORMAT(/' --> FOR ALL REPORTS WITH REPORT TYPE ',I3,', REJECT ',
     $ 'TOB/QOB BELOW 100 MB IF REPORT IS SOUTH OF 60S LATITUDE'/)
                        NFLGRT(NINT(TYP),7) = 1
                     ENDIF
                     RCD = 6
                  ELSE
                     IF(NFLGRT(NINT(TYP),2).EQ.0)  THEN
                        IF(NI.EQ.8)  THEN
                           WRITE(IUNITS,304) NINT(TYP)
  304 FORMAT(/' --> FOR ALL REPORTS WITH REPORT TYPE ',I3,', REJECT ',
     $ 'TOB/QOB ON SFC LVL DUE TO MISSING OBS ERROR'/)
                        ELSE
                           WRITE(IUNITS,202) NINT(TYP)
  202 FORMAT(/' --> FOR ALL REPORTS WITH REPORT TYPE ',I3,', REJECT ',
     $ 'TOB/QOB ON AT LEAST ONE LVL (IF AVAILABLE ON THAT LVL) DUE TO ',
     $ 'MISSING OBS ERROR'/)
                        ENDIF
                        NFLGRT(NINT(TYP),2) = 1
cdak cdak cdak cdak cdakWRITE(IUNITS,106) STNID,NINT(TYP),YOB,XOB,TOB
cd106 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
cdak $ 'E, REJECT TOB/QOB ON LVL - TOB=',F5.1,'C, MISSING OBS. ERROR')
                     ENDIF
                     RCD = 3
                     REJ = 9
                  ENDIF
               ELSE
                  IF(MOERR_P)  THEN
                     RCD = 3
                     REJ = 9
                  ENDIF
               ENDIF
               IF(RCD.EQ.3)  MOERR_T = .TRUE.
               IF(REJ.EQ.9.AND.(TQM.GT.3.AND.TQM.LT.15))  THEN
CDAKCDAKCDAKCDAK  WRITE(IUNITS,1402) STNID,NINT(TYP),YOB,XOB,TQM
 1402 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
     $ 'E, INPUT TQM =',F4.0,' -- DO NOT APPLY TEMP QM=9 EVENT')
               ELSE
                  TEV(1,L) = TOB
                  TEV(2,L) = REJ
                  TEV(3,L) = PVCD
                  TEV(4,L) = RCD
                  MAXTEV = L
               ENDIF
            ENDIF
         ENDIF

C  -------------------------------------------------------------------
C  RULES FOR SPECIFIC HUMIDITY -- QOB ON LEVEL REJECTED IF:
C    - OBSERVATION ERROR IS MISSING (AND SWITCH DOBERR=TRUE) --
C       "PREVENT" PGM REASON CODE 3
C    - PRESSURE ON LEVEL IS ABOVE "QTOP_REJ" MB {WHERE QTOP_REJ IS
C       READ IN FROM NAMELIST "PREVDATA" (SEE DOCBLOCK)} -- "PREVENT"
C       PGM REASON CODE 5
C    - OBSERVATION ERROR FOR TEMPERATURE ON LEVEL IS MISSING (AND
C       SWITCH DOBERR=TRUE) -- "PREVENT" PGM REASON CODE 3
C    - THIS IS SFC LEVEL AND OBSERVATION ERROR FOR SFC PRESSURE IS
C       MISSING (AND SWITCH DOBERR=TRUE) -- "PREVENT" PGM REASON CODE 3
C    - TEMPERATURE ON LEVEL IS MISSING OR IS LESS THAN -150 DEG. C --
C       "PREVENT" PGM REASON CODE 2
C    - SPECIFIC HUMIDITY IS ZERO OR IS NEGATIVE -- "PREVENT" PGM REASON
C       CODE 2
C    - REPORT IS TYPE 160-179 (SATELLITE TEMPERATURE SOUNDINGS, ALL
C       TYPES, ALL PATHS, LAND AND SEA), ALL PRESSURE LEVELS (AND
C       SWITCH SATMQC=TRUE) -- "PREVENT" PGM REASON CODE 7
C    - TEMPERATURE ON LEVEL VIOLATES RULES FOR TEMPERATURE (EXCEPT FOR
C       MISSING OBSERVATION ERROR, ALREADY COVERED IN RULE 2 ABOVE)
C       (SEE ABOVE)
C    - THIS IS SFC LEVEL AND PRESSURE VIOLATES RULES FOR SFC PRESSURE
C       (EXCEPT FOR MISSING OBSERVATION ERROR, ALREADY COVERED IN RULE
C       4 ABOVE) (SEE ABOVE)
C    - PRESSURE ON LEVEL VIOLATES RULES FOR PRESSURE (SEE ABOVE)
C  REJECTION FOR FIRST FOUR RULES MEANS Q.M. SET TO 9 UNLESS:
C      - ANY OTHER RULE CAUSES REJECTION, THEN Q.M. SET TO 8
C      - Q.M. IS ALREADY > 3 BUT NOT 15, THEN SKIP EVENT AND LEAVE Q.M.
C        AS IS
C  -------------------------------------------------------------------

         IF(QOB.LT.BMISS) THEN
            REJQ = OEFG01(POB,TYP,3,OEMIN(3)).GE.BMISS        .OR.
     $             TOB.GE.BMISS                               .OR.
     $             TOB.LE.-150.                               .OR.
     $             NINT(POB * 10.).LT.NINT(QTOP_REJ * 10.)    .OR.
     $             QOB.LE.0.                                  .OR.
     $             SATEMP                                     .OR.
     $             REJT
            IF(REJQ.OR.REJP_PS) THEN
               REJ = 8
               IF(.NOT.REJP_PS.AND..NOT.REJT)  THEN
                  IF(SATEMP)  THEN
                     IF(NFLGRT(NINT(TYP),8).EQ.0)  THEN
                        WRITE(IUNITS,7306) NINT(TYP)
 7306 FORMAT(/' --> FOR ALL REPORTS WITH REPORT TYPE ',I3,', REJECT ',
     $ 'QOB ON ALL LEVELS'/)
                        NFLGRT(NINT(TYP),8) = 1
                     ENDIF
                     RCD = 7
                  ELSE IF(QOB.LE.0..OR.TOB.GE.BMISS.OR.TOB.LE.-150.)THEN
                     WRITE(IUNITS,111) STNID,NINT(TYP),YOB,XOB,
     $                QOB/1000.,TOB
  111 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
     $ 'E, REJECT QOB ON LVL - QOB=',F6.3,' G/KG, FAILS SANITY CHECK ',
     $ '(TOB=',F5.1,' C)')
                     RCD = 2
                  ELSE IF(OEFG01(POB,TYP,3,OEMIN(3)).GE.BMISS)  THEN
                     IF(NFLGRT(NINT(TYP),3).EQ.0)  THEN
                        IF(NI.EQ.8)  THEN
                           WRITE(IUNITS,305) NINT(TYP)
  305 FORMAT(/' --> FOR ALL REPORTS WITH REPORT TYPE ',I3,', REJECT ',
     $ 'QOB ON SFC LVL DUE TO MISSING OBS ERROR'/)
                        ELSE
                           WRITE(IUNITS,203) NINT(TYP)
  203 FORMAT(/' --> FOR ALL REPORTS WITH REPORT TYPE ',I3,', REJECT ',
     $ 'QOB ON AT LEAST ONE LEVEL (IF AVAILABLE ON THAT LEVEL) DUE TO ',
     $ 'MISSING OBS ERROR'/)
                        ENDIF
                        NFLGRT(NINT(TYP),3) = 1
                     ENDIF
                     RCD = 3
                     REJ = 9
cdak cdak cdak cdak  WRITE(IUNITS,108) STNID,NINT(TYP),YOB,XOB,QOB/1000.
cd108 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
cdak $'E, REJECT QOB ON LVL - QOB=',F6.3,'G/KG, MISSING OBS. ERROR')
                  ELSE
                     WRITE(IUNITS,109) STNID,NINT(TYP),YOB,XOB,
     $                QOB/1000.,QTOP_REJ,POB
  109 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
     $ 'E, REJECT QOB ON LVL - QOB=',F6.3,' G/KG, ABOVE ',F6.1,
     $ 'MB (POB=',F6.1,' MB)')
                     RCD = 5
                     REJ = 9
                  ENDIF
               ELSE
                  IF(MOERR_P.OR.MOERR_T)  THEN
                     RCD = 3
                     REJ = 9
                  ENDIF
               ENDIF
               IF(REJ.EQ.9.AND.(QQM.GT.3.AND.QQM.LT.15))  THEN
CDAKCDAKCDAKCDAK  WRITE(IUNITS,1403) STNID,NINT(TYP),YOB,XOB,QQM
 1403 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
     $ 'E, INPUT QQM =',F4.0,' -- DO NOT APPLY MSTR QM=9 EVENT')
               ELSE
                  QEV(1,L) = QOB
                  QEV(2,L) = REJ
                  QEV(3,L) = PVCD
                  QEV(4,L) = RCD
                  MAXQEV = L
               ENDIF
            ENDIF
         ENDIF

C  -------------------------------------------------------------------
C  RULES FOR WINDS -- UOB AND VOB ON LEVEL REJECTED IF:
C    - OBSERVATION ERROR IS MISSING (AND SWITCH DOBERR=TRUE) --
C       "PREVENT" PGM REASON CODE 3
C    - THIS IS SFC LEVEL AND OBSERVATION ERROR FOR SFC PRESSURE IS
C       MISSING (AND SWITCH DOBERR=TRUE) -- "PREVENT" PGM REASON CODE 3
C    - THIS IS SFC LEVEL AND PRESSURE VIOLATES RULES FOR SFC PRESSURE
C       (EXCEPT FOR MISSING OBSERVATION ERROR, ALREADY COVERED IN RULE
C       2 ABOVE) (SEE ABOVE)
C    - PRESSURE ON LEVEL VIOLATES RULES FOR PRESSURE (SEE ABOVE)
C  REJECTION FOR FIRST TWO RULES MEANS Q.M. SET TO 9 UNLESS:
C      - ANY OTHER RULE CAUSES REJECTION, THEN Q.M. SET TO 8
C      - Q.M. IS ALREADY > 3 BUT NOT 15, THEN SKIP EVENT AND LEAVE Q.M.
C        AS IS
C  -------------------------------------------------------------------

         IF(MIN(UOB,VOB).LT.BMISS) THEN
            REJW = OEFG01(POB,TYP,4,OEMIN(4)).GE.BMISS
            IF(REJW.OR.REJP_PS) THEN
               REJ = 8
               IF(.NOT.REJP_PS)  THEN
                  IF(NFLGRT(NINT(TYP),4).EQ.0)  THEN
                     IF(NI.EQ.8)  THEN
                        WRITE(IUNITS,1304) NINT(TYP)
 1304 FORMAT(/' --> FOR ALL REPORTS WITH REPORT TYPE ',I3,', REJECT ',
     $ 'UOB/VOB ON SFC LVL DUE TO MISSING OBS ERROR'/)
                     ELSE
                        WRITE(IUNITS,204) NINT(TYP)
  204 FORMAT(/' --> FOR ALL REPORTS WITH REPORT TYPE ',I3,', REJECT ',
     $ 'UOB/VOB ON AT LEAST ONE LVL (IF AVAILABLE ON THAT LVL) DUE TO ',
     $ 'MISSING OBS ERROR'/)
                     ENDIF
                     NFLGRT(NINT(TYP),4) = 1
                  ENDIF
cdak cdak cdak    WRITE(IUNITS,112) STNID,NINT(TYP),YOB,XOB
cd112 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
cdak $'E, REJECT UOB/VOB ON LVL - MISSING OBS. ERROR')
                  RCD = 3
                  REJ = 9
               ELSE
                  IF(MOERR_P)  THEN
                     RCD = 3
                     REJ = 9
                  ENDIF
               ENDIF
               IF(REJ.EQ.9.AND.(WQM.GT.3.AND.WQM.LT.15))  THEN
CDAKCDAKCDAKCDAK  WRITE(IUNITS,1404) STNID,NINT(TYP),YOB,XOB,WQM
 1404 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
     $ 'E, INPUT WQM =',F4.0,' -- DO NOT APPLY WIND QM=9 EVENT')
               ELSE
                  WEV(1,L) = UOB
                  WEV(2,L) = VOB
                  WEV(3,L) = REJ
                  WEV(4,L) = PVCD
                  WEV(5,L) = RCD
                  MAXWEV = L
               ENDIF
            ENDIF
         ENDIF

C  -------------------------------------------------------------------
C  RULES FOR TOTAL COLUMN PRECIPITABLE WATER -- PWO REJECTED IF:
C    - OBSERVATION ERROR IS MISSING (AND SWITCH DOBERR=TRUE) --
C       "PREVENT" PGM REASON CODE 3
C  REJECTION MEANS Q.M. SET TO 9 UNLESS:
C      - Q.M. IS ALREADY > 3 BUT NOT 15, THEN SKIP EVENT AND LEAVE Q.M.
C        AS IS
C  -------------------------------------------------------------------

         IF(PWO.LT.BMISS) THEN
            REJPW = OEFG01(PRSS*0.01,TYP,6,OEMIN(6)).GE.BMISS
            IF(REJPW) THEN
               IF(NFLGRT(NINT(TYP),5).EQ.0)  THEN
                  WRITE(IUNITS,205) NINT(TYP)
  205 FORMAT(/' --> FOR ALL REPORTS WITH REPORT TYPE ',I3,', REJECT ',
     $ 'PWO DUE TO MISSING OBS ERROR'/)
                  NFLGRT(NINT(TYP),5) = 1
               ENDIF
cdakcdakcdak   WRITE(IUNITS,113) STNID,NINT(TYP),YOB,XOB,PWO
cd113 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
cdak $'E, REJECT PWO ON LVL - PWO=',F5.1,'MM, MISSING OBS. ERROR')
               IF(PWQ.GT.3.AND.PWQ.LT.15)  THEN
CDAKCDAKCDAKCDAK  WRITE(IUNITS,1405) STNID,NINT(TYP),YOB,XOB,PWQ
 1405 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
     $ 'E, INPUT PWQ =',F4.0,' -- DO NOT APPLY PWtO QM=9 EVENT')
               ELSE
                  PWV(1,L) = PWO
                  PWV(2,L) = 9
                  PWV(3,L) = PVCD
                  PWV(4,L) = 3
                  MAXPWV = L
               ENDIF
            ENDIF
         ENDIF

C  -------------------------------------------------------------------
C  RULES FOR LAYER 1 PRECIPITABLE WATER -- PW1O REJECTED IF:
C    - OBSERVATION ERROR IS MISSING (AND SWITCH DOBERR=TRUE) --
C       "PREVENT" PGM REASON CODE 3
C  REJECTION MEANS Q.M. SET TO 9 UNLESS:
C      - Q.M. IS ALREADY > 3 BUT NOT 15, THEN SKIP EVENT AND LEAVE Q.M.
C        AS IS
C  -------------------------------------------------------------------

         IF(PW1O.LT.BMISS) THEN
            REJPW1 = OEFG01(PRSS*0.01,TYP,6,OEMIN(6)).GE.BMISS
            IF(REJPW1) THEN
               IF(NFLGRT(NINT(TYP),9).EQ.0)  THEN
                  WRITE(IUNITS,206) NINT(TYP)
  206 FORMAT(/' --> FOR ALL REPORTS WITH REPORT TYPE ',I3,', REJECT ',
     $ 'PW1O DUE TO MISSING OBS ERROR'/)
                  NFLGRT(NINT(TYP),9) = 1
               ENDIF
cdakcdakcdak   WRITE(IUNITS,114) STNID,NINT(TYP),YOB,XOB,PW1O
cd114 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
cdak $'E, REJECT PW1O ON LVL - PW1O=',F5.1,'MM, MISSING OBS. ERROR')
               IF(PW1Q.GT.3.AND.PW1Q.LT.15)  THEN
CDAKCDAKCDAKCDAK  WRITE(IUNITS,1406) STNID,NINT(TYP),YOB,XOB,PW1Q
 1406 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
     $ 'E, INPUT PW1Q =',F4.0,' -- DO NOT APPLY PW1O QM=9 EVENT')
               ELSE
                  PW1V(1,L) = PW1O
                  PW1V(2,L) = 9
                  PW1V(3,L) = PVCD
                  PW1V(4,L) = 3
                  MAXPW1V = L
               ENDIF
            ENDIF
         ENDIF

C  -------------------------------------------------------------------
C  RULES FOR LAYER 2 PRECIPITABLE WATER -- PW2O REJECTED IF:
C    - OBSERVATION ERROR IS MISSING (AND SWITCH DOBERR=TRUE) --
C       "PREVENT" PGM REASON CODE 3
C  REJECTION MEANS Q.M. SET TO 9 UNLESS:
C      - Q.M. IS ALREADY > 3 BUT NOT 15, THEN SKIP EVENT AND LEAVE Q.M.
C        AS IS
C  -------------------------------------------------------------------

         IF(PW2O.LT.BMISS) THEN
            REJPW2 = OEFG01(PRSS*0.01,TYP,6,OEMIN(6)).GE.BMISS
            IF(REJPW2) THEN
               IF(NFLGRT(NINT(TYP),10).EQ.0)  THEN
                  WRITE(IUNITS,207) NINT(TYP)
  207 FORMAT(/' --> FOR ALL REPORTS WITH REPORT TYPE ',I3,', REJECT ',
     $ 'PW2O DUE TO MISSING OBS ERROR'/)
                  NFLGRT(NINT(TYP),10) = 1
               ENDIF
cdakcdakcdak   WRITE(IUNITS,115) STNID,NINT(TYP),YOB,XOB,PW2O
cd115 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
cdak $'E, REJECT PW2O ON LVL - PW2O=',F5.1,'MM, MISSING OBS. ERROR')
               IF(PW2Q.GT.3.AND.PW2Q.LT.15)  THEN
CDAKCDAKCDAKCDAK  WRITE(IUNITS,1407) STNID,NINT(TYP),YOB,XOB,PW2Q
 1407 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
     $ 'E, INPUT PW2Q =',F4.0,' -- DO NOT APPLY PW2O QM=9 EVENT')
               ELSE
                  PW2V(1,L) = PW2O
                  PW2V(2,L) = 9
                  PW2V(3,L) = PVCD
                  PW2V(4,L) = 3
                  MAXPW2V = L
               ENDIF
            ENDIF
         ENDIF

C  -------------------------------------------------------------------
C  RULES FOR LAYER 3 PRECIPITABLE WATER -- PW3O REJECTED IF:
C    - OBSERVATION ERROR IS MISSING (AND SWITCH DOBERR=TRUE) --
C       "PREVENT" PGM REASON CODE 3
C  REJECTION MEANS Q.M. SET TO 9 UNLESS:
C      - Q.M. IS ALREADY > 3 BUT NOT 15, THEN SKIP EVENT AND LEAVE Q.M.
C        AS IS
C  -------------------------------------------------------------------

         IF(PW3O.LT.BMISS) THEN
            REJPW3 = OEFG01(PRSS*0.01,TYP,6,OEMIN(6)).GE.BMISS
            IF(REJPW3) THEN
               IF(NFLGRT(NINT(TYP),11).EQ.0)  THEN
                  WRITE(IUNITS,208) NINT(TYP)
  208 FORMAT(/' --> FOR ALL REPORTS WITH REPORT TYPE ',I3,', REJECT ',
     $ 'PW3O DUE TO MISSING OBS ERROR'/)
                  NFLGRT(NINT(TYP),11) = 1
               ENDIF
cdakcdakcdak   WRITE(IUNITS,116) STNID,NINT(TYP),YOB,XOB,PW3O
cd116 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
cdak $'E, REJECT PW3O ON LVL - PW3O=',F5.1,'MM, MISSING OBS. ERROR')
               IF(PW3Q.GT.3.AND.PW3Q.LT.15)  THEN
CDAKCDAKCDAKCDAK  WRITE(IUNITS,1408) STNID,NINT(TYP),YOB,XOB,PW3Q
 1408 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
     $ 'E, INPUT PW3Q =',F4.0,' -- DO NOT APPLY PW3O QM=9 EVENT')
               ELSE
                  PW3V(1,L) = PW3O
                  PW3V(2,L) = 9
                  PW3V(3,L) = PVCD
                  PW3V(4,L) = 3
                  MAXPW3V = L
               ENDIF
            ENDIF
         ENDIF

C  -------------------------------------------------------------------
C  RULES FOR LAYER 4 PRECIPITABLE WATER -- PW4O REJECTED IF:
C    - OBSERVATION ERROR IS MISSING (AND SWITCH DOBERR=TRUE) --
C       "PREVENT" PGM REASON CODE 3
C  REJECTION MEANS Q.M. SET TO 9 UNLESS:
C      - Q.M. IS ALREADY > 3 BUT NOT 15, THEN SKIP EVENT AND LEAVE Q.M.
C        AS IS
C  -------------------------------------------------------------------

         IF(PW4O.LT.BMISS) THEN
            REJPW4 = OEFG01(PRSS*0.01,TYP,6,OEMIN(6)).GE.BMISS
            IF(REJPW4) THEN
               IF(NFLGRT(NINT(TYP),12).EQ.0)  THEN
                  WRITE(IUNITS,209) NINT(TYP)
  209 FORMAT(/' --> FOR ALL REPORTS WITH REPORT TYPE ',I3,', REJECT ',
     $ 'PW4O DUE TO MISSING OBS ERROR'/)
                  NFLGRT(NINT(TYP),12) = 1
               ENDIF
cdakcdakcdak   WRITE(IUNITS,117) STNID,NINT(TYP),YOB,XOB,PW4O
cd117 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
cdak $'E, REJECT PW4O ON LVL - PW4O=',F5.1,'MM, MISSING OBS. ERROR')
               IF(PW4Q.GT.3.AND.PW4Q.LT.15)  THEN
CDAKCDAKCDAKCDAK  WRITE(IUNITS,1409) STNID,NINT(TYP),YOB,XOB,PW4Q
 1409 FORMAT(' ~~> ID ',A8,' (RTP ',I3,'), LAT=',F6.2,'N, LON=',F6.2,
     $ 'E, INPUT PW4Q =',F4.0,' -- DO NOT APPLY PW4O QM=9 EVENT')
               ELSE
                  PW4V(1,L) = PW4O
                  PW4V(2,L) = 9
                  PW4V(3,L) = PVCD
                  PW4V(4,L) = 3
                  MAXPW4V = L
               ENDIF
            ENDIF
         ENDIF

      ENDDO
      ENDIF

C  APPLY THE PROPER EVENTS
C  -----------------------

      IF(MAXPEV .GT.0) CALL UFBINT(IUNITP,PEV, 4,MAXPEV, IRET,PEVN)
      IF(MAXQEV .GT.0) CALL UFBINT(IUNITP,QEV, 4,MAXQEV, IRET,QEVN)
      IF(MAXTEV .GT.0) CALL UFBINT(IUNITP,TEV, 4,MAXTEV, IRET,TEVN)
      IF(MAXWEV .GT.0) CALL UFBINT(IUNITP,WEV, 5,MAXWEV, IRET,WEVN)
      IF(MAXPWV .GT.0) CALL UFBINT(IUNITP,PWV, 4,MAXPWV, IRET,PWVN)
      IF(MAXPW1V.GT.0) CALL UFBINT(IUNITP,PW1V,4,MAXPW1V,IRET,PW1VN)
      IF(MAXPW2V.GT.0) CALL UFBINT(IUNITP,PW2V,4,MAXPW2V,IRET,PW2VN)
      IF(MAXPW3V.GT.0) CALL UFBINT(IUNITP,PW3V,4,MAXPW3V,IRET,PW3VN)
      IF(MAXPW4V.GT.0) CALL UFBINT(IUNITP,PW4V,4,MAXPW4V,IRET,PW4VN)

      RETURN
      END
