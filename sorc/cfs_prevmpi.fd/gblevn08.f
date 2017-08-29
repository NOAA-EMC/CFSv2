
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    GBLEVN08    CALCULATE SPEC. HUMIDITY AND VIRTUAL TEMP
C   PRGMMR: D.A. KEYSER      ORG: NP22       DATE: 2007-09-14
C
C ABSTRACT: CREATE VIRTUAL TEMPERATURE EVENTS WITHIN GBLEVENTS
C   SUBROUTINE.  FOR ALL TYPES EXCEPT RASS, THIS CONSISTS OF FIRST RE-
C   CALCULATING THE SPECIFIC HUMIDITY FROM THE REPORTED DEWPOINT
C   TEMPERATURE AND PRESSURE, FOLLOWED BY THE CALCULATION OF VIRTUAL
C   TEMPERATURE FROM THE JUST-CALCULATED SPECIFIC HUMIDITY AND THE
C   REPORTED (SENSIBLE) TEMPERATURE.  THE RE-CALCULATED SPECIFIC
C   HUMIDITY IS THEN ENCODED AS A STACKED EVENT TO BE LATER WRITTEN
C   INTO THE PREPBUFR FILE (UNDER PROGRAM "VIRTMP", REASON CODE 0).
C   IF THE NAMELIST SWITCH DOVTMP IS TRUE, THEN THE JUST-CALCULATED
C   VIRTUAL TEMPERATURE IS THEN ALSO ENCODED AS A STACKED EVENT TO BE
C   LATER WRITTEN INTO THE PREPBUFR FILE (UNDER PROGRAM "VIRTMP",
C   REASON CODE 0, 2 OR 6).  FOR RASS DATA, SPECIFIC HUMIDITY IS
C   MISSING HOWEVER IF THE NAMELIST SWITCH DOVTMP IS TRUE, A SIMPLE
C   COPY OF THE REPORTED (VIRTUAL) TEMPERATURE IS ENCODED AS A STACKED
C   EVENT TO BE LATER WRITTEN INTO THE PREPBUFR FILE (UNDER PROGRAM
C   "VIRTMP", REASON CODE 3).  THIS SUBROUTINE IS CURRENTLY ONLY
C   CALLED FOR SURFACE LAND ("ADPSFC"), MARINE ("SFCSHP"), MESONET
C   ("MSONET"), RASS ("RASSDA") OR SATELLITE TEMPERATURE RETRIEVAL
C   ("SATEMP") DATA TYPES WHEN SWITCH "ADPUPA_VIRT" IS FALSE AND ONLY
C   FOR SURFACE LAND ("ADPSFC"), MARINE ("SFCSHP"), MESONET ("MSONET"),
C   RASS ("RASSDA"), SATELLITE TEMPERATURE RETRIEVAL ("SATEMP") OR
C   RAOB/DROP/MULTI-LVL RECCO ("ADPUPA") DATA TYPES WHEN SWITCH
C   "ADPUPA_VIRT" IS TRUE.  IT IS ALSO ONLY CALLED IN THE PREVENTS
C   MODE.  THIS ROUTINE IS CALLED ONCE FOR EACH VALID REPORT IN THE
C   PREPBUFR FILE.
C
C PROGRAM HISTORY LOG:
C 1995-05-17  J. WOOLLEN (NP20) - ORIGINAL AUTHOR
C 1997-06-01  D.A. KEYSER - STREAMLINED, ADDED SWITCH DOVTMP
C 1999-12-01 D. A. KEYSER -- SPEC. HUMIDITY AND VIRT. TEMPERATURE ARE
C     NOW CALCULATED WHEN SPEC. HUMIDITY QUAL. MARKER IS BAD (SUBJECT
C     TO A SANITY CHECK), HOWEVER THE VIRT. TEMPERATURE GETS A BAD
C     QUAL. MARKER (8)
C 2004-08-30 D. A. KEYSER -- FOR "RASSDA" TYPES, ENCODES A SIMPLE COPY
C     OF THE REPORTED (VIRTUAL) TEMPERATURE AS A "VIRTMP" EVENT IF
C     DOVTMP IS TRUE, GETS NEW REASON CODE 3
C 2006-07-14 D. A. KEYSER --  PROCESSES REPORTS IN MESSAGE TYPE ADPUPA
C     (I.E., RAOBS, DROPS, MULTI-LEVEL RECCOS) WITH SAME LOGIC AS IN
C     SUBROUTINE VTPEVN OF PROGRAM PREPOBS_CQCBUFR WHEN NEW NAMELIST
C     SWITCH "ADPUPA_VIRT" IS TRUE {NORMALLY "ADPUPA_VIRT" IS FALSE
C     (DEFAULT) BECAUSE SUBSEQUENT PROGRAM PREPOBS_CQCBUFR PERFORMS
C     THIS FUNCTION}
C 2007-09-14 D. A. KEYSER -- FOR NON-"ADPUPA" TYPES, Q.M. 9 IS NOW
C     ASSIGNED TO CALCULATED VIRT. TEMPS IF THE MOISTURE Q.M. IS 9 OR
C     15 AND ORIG. TEMP NOT "BAD", THESE "VIRTMP" EVENTS RECEIVE NEW
C     REASON CODE 4, HAD RECEIVED Q.M. 8 WITH REASON CODE 2 LIKE VIRT.
C     TEMPS CALCULATED FROM "BAD" MOISTURE - THIS MEANS ONLY TRULY
C     "BAD" VIRT. TEMPS WILL NOW GET Q.M. 8 AND VIRT. TEMPS FLAGGED FOR
C     NON-USE BY ASSIMILATION (BUT STILL "GOOD") WILL NOW GET Q.M. 9
C     (GSI MONITORS, BUT DOES NOT USE, OBS WITH Q.M. 9, BUT IT DOES NOT
C     EVEN CONSIDER OBS WITH Q.M. 8); FOR "ADPUPA" TYPES, Q.M. 3 IS NOW
C     ASSIGNED TO CALCULATED VIRT. TEMPS ONLY IF THE MOISTURE Q.M. IS
C     TRULY BAD (I.E. > 3 BUT NOT 9 OR 15) (AND, AS BEFORE, ORIG. TQM
C     IS 1 OR 2 AND POB IS BELOW 700 MB) - BEFORE, TQM SET TO 3 WHEN
C     QQM WAS 9 OR 15 AND ALL OTHER CONDITIONS MET; FOR "SATEMP" TYPES,
C     ENCODES A SIMPLE COPY OF THE REPORTED (VIRTUAL) TEMPERATURE AS A
C     "VIRTMP" EVENT IF DOVTMP IS TRUE, GETS REASON CODE 3 (SIMILAR TO
C     WHAT IS ALREADY DONE FOR "RASSDA" TYPES)
C
C USAGE:    CALL GBLEVN08(IUNITP)
C   INPUT ARGUMENT LIST:
C     IUNITP   - BUFR OUTPUT FILE UNIT
C     SUBSET   - THE BUFR MESSAGE TABLE A ENTRY FOR THE PARTICULAR
C              - REPORT BEING PROCESSED
C
C REMARKS: WILL IMMEDIATELY RETURN TO CALLING PROGRAM IF ANY OF THE
C   FOLLOWING CONDITIONS EXIST: THERE ARE NO LEVELS OF VALID DEWPOINT,
C   OBS, TEMPERATURE Q.M. OR SPEC. HUMIDITY Q.M. IN THE INPUT PREPBUFR
C   FILE FOR THE REPORT.  WILL NOT ATTEMPT EITHER SPEC. HUMIDITY NOR
C   VIRT. TEMP CALC. ON A GIVEN LEVEL IF ANY OF THE FOLLOWING
C   CONDITIONS EXIST: REPORTED PRESSURE OBS IS MISSING, REPORTED
C   (SENSIBLE) TEMPERATURE OBS IS MISSING, OR REPORTED DEWPOINT OBS IS
C   MISSING.
C 
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM-SP
C
C$$$
      SUBROUTINE GBLEVN08(IUNITP,SUBSET) ! FORMERLY SUBROUTINE VTPEVN

      CHARACTER*80 EVNSTQ,EVNSTV
      CHARACTER*8  SUBSET
      REAL(8)      TDP(255),TQM(255),QQM(255),BAKQ(4,255),BAKV(4,255),
     $             OBS,QMS,BAK,SID

      LOGICAL      EVNQ,EVNV,DOVTMP,TROP,ADPUPA_VIRT,DOBERR,DOFCST,
     $             SOME_FCST

      COMMON /GBEVAA/ SID,OBS(15,255),QMS(12,255),BAK(12,255),XOB,
     $ YOB,DHR,TYP,NLEV
      COMMON /GBEVBB/ PVCD,VTCD
      COMMON /GBEVCC/ DOVTMP,DOFCST,SOME_FCST,DOBERR,FCST,VIRT,
     $ QTOP_REJ,SATMQC,ADPUPA_VIRT

      DATA EVNSTQ /'QOB QQM QPC QRC'/
      DATA EVNSTV /'TOB TQM TPC TRC'/
      DATA BMISS /10E10/

C-----------------------------------------------------------------------
C FCNS BELOW CONVERT TEMP/TD (K) & PRESS (MB) INTO SAT./ SPEC. HUM.(G/G)
C-----------------------------------------------------------------------
      ES(T) = 6.1078*EXP((17.269*(T - 273.16))/((T - 273.16)+237.3))
      QS(T,P) = (0.622*ES(T))/(P-(0.378*ES(T)))
C-----------------------------------------------------------------------

C  CLEAR TEMPERATURE AND SPECIFIC HUMIDITY EVENTS
C  ----------------------------------------------

      EVNQ = .FALSE.
      EVNV = .FALSE.
      BAKQ = BMISS
      BAKV = BMISS
      TROP = .FALSE.

C  GET DEWPOINT TEMPERATURE AND CURRENT T,Q QUALITY MARKERS
C  --------------------------------------------------------

      CALL UFBINT(-IUNITP,TDP,1,255,NLTD,'TDO')
      CALL UFBINT(-IUNITP,QQM,1,255,NLQQ,'QQM')
      CALL UFBINT(-IUNITP,TQM,1,255,NLTQ,'TQM')
      IF(SUBSET.NE.'RASSDA  '.AND.SUBSET.NE.'SATEMP  ') THEN
         IF(NLTD.EQ.0) RETURN
         IF(NLQQ.EQ.0) RETURN
      ENDIF
      IF(NLTQ.EQ.0) RETURN
      IF(SUBSET.NE.'RASSDA  '.AND.SUBSET.NE.'SATEMP  ') THEN
         IF(NLTD.NE.NLEV) THEN
            PRINT *, '##GBLEVENTS/GBLEVN08 - NLTD .NE. NLEV - STOP 61'
            CALL ERREXIT(61)
         ENDIF
         IF(NLQQ.NE.NLEV) THEN
            PRINT *, '##GBLEVENTS/GBLEVN08 - NLQQ .NE. NLEV - STOP 63'
            CALL ERREXIT(63)
         ENDIF
      ENDIF
      IF(NLTQ.NE.NLEV) THEN
         PRINT *, '##GBLEVENTS/GBLEVN08 - NLTQ .NE. NLEV - STOP 62'
         CALL ERREXIT(62)
      ENDIF

C  COMPUTE VIRTUAL TEMPERATURE AND SPECIFIC HUMIDITY USING REPORTED DEWP
C  ---------------------------------------------------------------------

      IF(NLEV.GT.0)  THEN
      DO L=1,NLEV
         POB = OBS(1,L)
         TDO = TDP(L)
         TOB = OBS(3,L)
         CAT = OBS(12,L)
         IF(DOVTMP) THEN
            IF(SUBSET.EQ.'RASSDA  '.OR.SUBSET.EQ.'SATEMP  ') THEN
               IF(TOB.LT.BMISS) THEN
                  BAKV(1,L) = TOB
                  BAKV(2,L) = TQM(L)
                  BAKV(3,L) = VTCD
                  BAKV(4,L) = 3
                  EVNV = .TRUE.
                  CYCLE
               ENDIF
            ENDIF
         ENDIF
         IF(POB.LT.BMISS .AND. TOB.LT.BMISS
     $                   .AND. TDO.LT.BMISS) THEN
            IF(QQM(L).GT.3) THEN
C  Don't update q or calculate Tv if bad moisture obs fails sanity check
cdak           IF(TDO.LT.-103.15 .OR. TDO.GT.46.83 .OR. POB.LT.0.1 .OR.
cdak $          POB.GT.1100.)
cdak $ print *, '&&& bad QM fails sanity check'
               IF(TDO.LT.-103.15 .OR. TDO.GT.46.83 .OR. POB.LT.0.1 .OR.
     $          POB.GT.1100.)  CYCLE
            ENDIF
            QOB = QS(TDO+273.16,POB)
            BAKQ(1,L) = QOB*1E6
            BAKQ(2,L) = QQM(L)  ! Moist qm same as before for re-calc. q
            BAKQ(3,L) = VTCD
            BAKQ(4,L) = 0       ! Re-calc. q gets unique reason code 0
            EVNQ = .TRUE.
C  If message type ADPUPA, test this level to see if at or above trop
C   (trop must be above 500 mb to pass test; if no trop level found
C   assume it's at 80 mb)
C  Don't calculate Tv on this level if at or above trop (doesn't affect
C   q calculation)
            TROP = (SUBSET.EQ.'ADPUPA  ' .AND.
     $       ((CAT.EQ.5 .AND. POB.LT.500.) .OR. POB.LT. 80. .OR. TROP))
            IF(DOVTMP .AND. .NOT.TROP) THEN
               BAKV(1,L) = (TOB+273.16)*(1.+.61*QOB)-273.16
               BAKV(3,L) = VTCD
               IF(SUBSET.EQ.'ADPUPA  ') THEN
C  Message type ADPUPA comes here
                  IF((QQM(L).LT.4.OR.QQM(L).EQ.9.OR.QQM(L).EQ.15)
     $             .OR. TQM(L).EQ.0 .OR. TQM(L).GT.3
     $             .OR. POB.LE.700.) THEN
                     BAKV(2,L) = TQM(L) ! Tv qm same as for T when q ok
                                        ! or q flagged by PREPRO (but
                                        ! not bad) 
                     BAKV(4,L) = 0      ! Tv gets unique reason code 0
                  ELSE
                     BAKV(2,L) = 3 !Tv qm susp for bad moist below 700mb
                     BAKV(4,L) = 6 !Tv gets unique reason code 6
                  ENDIF
               ELSE
C  All other message types come here
                  IF(QQM(L).LT.4) THEN
                     BAKV(2,L) = TQM(L) ! Tv qm same as for T when q ok
                     BAKV(4,L) = 0      ! Tv gets unique reason code 0
                  ELSE IF((QQM(L).EQ.9.OR.QQM(L).EQ.15).AND.(TQM(L).LE.
     $             3.OR.TQM(L).GE.15.OR.TQM(L).EQ.9)) THEN
cdak  print *, '%%% process tvirt on lvl ',l,' for missing moist obs ',
cdak $ 'error/high-up moist case when orig. T not "bad" (set TQM=9)'
                     BAKV(2,L) = 9 ! Tv qm 9 for moist w/ missing obs
                                   !  error or moist flagged by PREPRO
                                   !  (but not bad) and T qm orig not
                                   !  "bad"
                     BAKV(4,L) = 4 ! Tv gets unique reason code 4
                  ELSE
cdak  print *, '%%% process tvirt on lvl ',l,' for "bad" QQM case or ',
cdak $ 'missing moist obs error/high-up moist w/ "bad" TQM case (set ',
cdak $ 'TQM=8)
                     BAKV(2,L) = 8 ! Tv qm 8 (bad) for "bad" moist or
                                   !  moist w/ missing obs error or
                                   !  moist flagged by PREPRO (but not
                                   !  bad) and T qm orig "bad"
                     BAKV(4,L) = 2 ! Tv gets unique reason code 2
                  ENDIF
               ENDIF
               EVNV = .TRUE.
            ENDIF
         ENDIF
      ENDDO
      ENDIF

C  ENCODE EVENTS INTO REPORT
C  -------------------------

      IF(NLEV.GT.0)  THEN
         IF(EVNQ) CALL UFBINT(IUNITP,BAKQ,4,NLEV,IRET,EVNSTQ)
         IF(EVNV) CALL UFBINT(IUNITP,BAKV,4,NLEV,IRET,EVNSTV)
      ENDIF

      RETURN
      END
