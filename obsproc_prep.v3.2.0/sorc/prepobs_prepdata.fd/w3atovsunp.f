C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    W3ATOVSUNP
C   PRGMMR: KEYSER           ORG: NP22        DATE: 2013-02-14
C
C ABSTRACT: READS AND UNPACKS ONE REPORT FROM INPUT ATOVS RETRIEVAL
C   BUFR (DUMP) FILE INTO SPECIFIED FORMAT. RETURNS INFORMATION IN THE
C   FORMAT DESCRIBED BELOW.  INCLUDES SOUNDING DATA ON 40 LEVELS AND
C   ATOVS RADIANCES FROM 35 CHANNELS.  ALSO, INFORMATION ABOUT THE
C   INPUT DATA SET ITSELF (NAME, CENTER DATE, DUMP TIME) IS RETURNED.
C
C PROGRAM HISTORY LOG:
C 1999-11-17  D.A. KEYSER/NP22 -- ORIGINAL AUTHOR
C 2000-12-05  D.A. KEYSER/NP22 -- ACCOUNTS FOR NESDIS ERROR IN SCALING
C      BOTTOM PRESSURE VALUE (AND WILL STILL WORK RIGHT IF THEY FIX
C      IT); PERFORMS A SANITY CHECK ON THE BOTTOM PRESSURE VALUE; IF
C      FILTER FLAG NOT FOUND, SET OUTPUT FILTER FLAG TO "-99" WHICH
C      ALERTS CALLING PROGRAM TO PROCESS ONLY EVERY 11'TH RETRIEVAL TO
C      SIMULATE 250KM SAMPLING (NEEDED FOR CDAS RUNS), WAS SET TO 0
C 2001-07-16  D.A. KEYSER/NP22 -- CONSIDERS RETRIEVALS WITH BOTH CLEAR
C      AND CLOUDY BIT "OFF" (=0) IN SATELLITE DATA PROCESSING
C      TECHNIQUE TO BE CLOUDY (BEFORE THEY WERE UNKNOWN)
C 2001-08-07  D.A. KEYSER/NP22 -- CHECKS IERR ON INPUT, IF .ne. 1
C      ASSUMES INPUT FILE IS A DUMP (AS BEFORE), IF = 1 ASSUMES INPUT
C      FILE IS A TANK (/dcom) AND DOES NOT ATTEMPT TO READ A CENTER
C      OR DUMP TIME BUT DOES LIMIT SOME WARNING PRINTOUTS TO 250 LINES;
C      NOW CLOSES INPUT FILE AFTER ALL REPORTS ARE PROCESSED; UPON
C      EACH CALL, CHECKS TO SEE IF INPUT BUFR FILE IS CLOSED, IF SO
C      PROCESSES AS IF FIRST CALL TO SUBROUTINE (ALLOWS > 1 BUFR FILES
C      - WHETHER THE SAME OR DIFFERENT - TO BE READ THROUGH IN THE SAME
C      PROGRAM)
C 2002-03-05  D.A. KEYSER/NP22 --  ACCOUNTS FOR CHANGE FROM MNEMONIC
C      "SZNT" TO "SAZA" (SATELLITE ZENITH ANGLE) IN INPUT ATOVS BUFR
C      DUMP FILES AFTER 3/2002 (WILL STILL WORK PROPERLY FOR DUMP FILES
C      PRIOR TO 3/2002)
C 2008-03-10  D.A. KEYSER/NP22 -- HANDLES METOP-2 SATELLITE (BUFR
C      SATELLITE ID 004)
C 2012-11-30  J. WOOLLEN       -- INITIAL PORT TO WCOSS; RESET BMISS TO
C      A VALUE (10E08) WHICH WILL NOT CAUSE INTEGER OVERFLOW WHICH CAN
C      BE UNPREDICTABLE (PRIOR BMISS VALUE WAS 10E10)
C 2013-02-14  D. A. KEYSER -- FINAL CHANGES TO RUN ON WCOSS; OBTAIN
C      VALUE OF BMISS SET IN CALLING PROGRAM VIA CALL TO BUFRLIB
C      ROUTINE GETBMISS RATHER THAN HARDWIRING IT TO 10E08 (OR 10E10);
C      USE BUFRLIB FUNCTION IBFMS TO DETERMINE IF A VARIABLE READ FROM
C      BUFR FILE IS MISSING (I.E. RETURNED AS BMISS);  USE FORMATTED
C      PRINT STATEMENTS WHERE PREVIOUSLY UNFORMATTED PRINT WAS USED
C      (WCOSS SPLITS UNFORMATTED PRINT AT 80 CHARACTERS)
C
C USAGE:    CALL W3ATOVSUNP(IUNIT,IBDATE,PP,TT,QQ,NLEV,IRTCHN,RTRAD,
C                           NCHN,STNID,ISATOB,RSATOB,DSNAME,IDSDAT,
C                           IDSDMP_8,IERR)
C   INPUT ARGUMENT LIST:
C     IUNIT    - UNIT NUMBER OF INPUT FILE CONTAINING RETRIEVAL DATA
C              - IN BUFR FORMAT.
C     IERR     - DUMP FILE VS. TANK FILE INDICATOR (=0 - DUMP FILE,
C              - MEANING CENTER TIME AND DUMP TIME SHOULD BE
C              - RETURNED IN IDSDAT AND IDMDMP, RESP. WITH OUTPUT
C              - IERR=1 AFTER FIRST CALL TO THIS SUBR. TO OPEN AN
C              - INPUT FILE; =1 - TANK FILE, MEANING CENTER TIME AND
C              - DUMP TIME WILL NEVER BE RETURNED IN IDSDAT AND IDMDMP,
C              - RESP. WITH OUTPUT IERR=1 AFTER FIRST CALL TO THIS
C              - SUBR.  TO OPEN AN INPUT FILE - IDSDAT AND IDSDMP_8 ARE
C              - BOTH RETURNED AS 99) (NOTE: THIS IS READ ONLY UPON
C              - FIRST CALL TO THIS SUBROUTINE TO OPEN AN INPUT FILE)
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     IBDATE   - DATE IN SECTION 1 OF BUFR MESSAGE CONTAINING THIS
C              - RETRIEVAL (IN FORM YYYYMMDDHH)
C     PP       - PRESSURE IN MB AT RETRIEVAL LEVELS (MUST BE
C              - DIMENSIONED TO AT LEAST 40 WORDS BY CALLING PROGRAM
C              - - ONLY 40 WORDS WILL BE FILLED HERE)
C     TT       - VIRTUAL TEMPERATURE IN DEG. K AT RETRIEVAL LEVELS
C              - (MUST BE DIMENSIONED TO AT LEAST 40 WORDS BY CALLING
C              - PROGRAM - ONLY 40 WORDS WILL BE FILLED HERE)
C     QQ       - SPECIFIC HUMIDITY IN G/G AT RETRIEVAL LEVELS (MUST
C              - BE DIMENSIONED TO AT LEAST 40 WORDS BY CALLING
C              - PROGRAM - ONLY 40 WORDS WILL BE FILLED HERE)
C     NLEV     - NUMBER OF RETRIEVAL LEVELS (SHOULD ALWAYS BE 40)
C     IRTCHN   - ATOVS RADIANCE CHANNELS (NUMERIC) (MUST BE
C              - DIMENSIONED TO AT LEAST 35 WORDS BY CALLING PROGRAM -
C              - ONLY 35 WORDS WILL BE FILLED HERE)
C     RTRAD    - ATOVS RADIANCES IN DEG. K (MUST BE DIMENSIONED TO AT
C              - LEAST 35 WORDS BY CALLING PROGRAM - ONLY 35 WORDS
C              - WILL BE FILLED HERE)
C     NCHN     - NUMBER OF RADIANCE CHANNELS (SHOULD ALWAYS BE 35)
C     STNID    - STATION ID (CHAR*8)
C     ISATOB   - 11-WORD INTEGER ARRAY CONTAINING RETURNED DATA
C              - (SEE REMARKS FOR CONTENTS)
C     RSATOB   - 16-WORD REAL ARRAY CONTAINING RETURNED DATA
C              - (SEE REMARKS FOR CONTENTS)
C     DSNAME   - CHARACTER*8 DATA SET NAME (SAME FOR ALL REPORTS IN
C              - A COMMON INPUT DATA SET - SEE REMARKS FOR OUTPUT
C              - IERR=1)
C     IDSDAT   - INTEGER DATA SET CENTER DATE IN FORM YYYYMMDDHH (SAME
C              - FOR ALL REPORTS IN A COMMON INPUT DATA SET - SEE
C              - REMARKS FOR OUTPUT IERR=1) (NOTE: RETURNED AS 99 IF
C              - INPUT IERR IS 1 FOR FIRST CALL TO THIS SUBROUTINE TO
C              - OPEN AN INPUT FILE)
C     IDSDMP_8 - INTEGER*8 DATA SET DUMP TIME IN FORM YYYYMMDDHHMM
C              - (SAME FOR ALL REPORTS IN A COMMON INPUT DATA SET - SEE
C              - REMARKS FOR OUTPUT IERR=1) (NOTE: RETURNED AS 99 IF
C              - INPUT IERR IS 1 FOR FIRST CALL TO THIS SUBROUTINE TO
C              - OPEN AN INPUT FILE)
C     IERR     - ERROR RETURN CODE
C                 =  0 OBSERVATION READ AND UNPACKED INTO OUTPUT
C                        ARGUMENT LOCATIONS (SEE ABOVE).  SEE REMARKS
C                        FOR CONTENTS. NEXT CALL TO W3ATOVSUNP WILL
C                        RETURN NEXT OBSERVATION IN DATA SET.
C                 =  1 INFORMATION ABOUT THE BUFR DATASET IS RETURNED
C                        IN THE OUTPUT ARGUMENTS DSNAME, IDSDAT,
C                        IDSDMP_8 (SEE OUTPUT ARGUMENT LIST ABOVE)
C
C                        THIS SHOULD ALWAYS OCCUR AFTER THE FIRST CALL
C                        TO THIS SUBROUTINE TO OPEN AN INPUT FILE.  NO
C                        REPORT IS UNPACKED AT THIS POINT, AND ONLY
C                        DSNAME, IDSDAT, AND IDSDMP_8 CONTAIN
C                        INFORMATION. ALL SUBSEQUENT CALLS TO
C                        W3ATOVSUNP SHOULD RETURN THE OBSERVATIONS IN
C                        THIS DATA SET, SEQUENTIALLY, (OUTPUT IERR=0)
C                        UNTIL THE END OF FILE IS ENCOUNTERED (OUTPUT
C                        IERR=2).  THE VALUES STORED IN DSNAME, IDSDAT,
C                        AND IDSDMP_8 WILL CONTINUE TO BE RETURNED
C                        ALONG WITH EACH REPORT OUTPUT WHEN OUTPUT IERR
C                        = 0.
C                 =  2 FOR NORMAL END-OF-FILE ENCOUNTERED.
C                 = -1 FOR END-OF-FILE ON FIRST READ TO OPEN AN INPUT
C                        FILE -- EMPTY FILE
C                 = -2 FOR INPUT BUFR FILE NOT Y2K COMPLIANT -- NO
C                        PROCESSING DONE
C                 = -3 CENTER DATE COULD NOT BE OBTAINED FROM INPUT
C                        FILE -- NO PROCESSING DONE (CAN ONLY HAPPEN
C                        WHEN INPUT IERR IS 0 FOR FIRST CALL TO THIS
C                        SUBROUTINE TO OPEN AN INPUT FILE)
C
C   INPUT FILES:
C     UNIT "IUNIT" - FILE CONTAINING BUFR ATOVS DATA
C
C   OUTPUT FILES:
C     UNIT 06 - STANDARD OUTPUT PRINT
C
C REMARKS:
C
C     CONTENTS OF OUTPUT ARGUMENT ISATOB (11 INTEGER WORDS) FOR EACH
C      RETRIEVAL:
C        WORD  1 - SATELLITE IDENTIFIER
C                                      (BUFR C.T. 0-01-007)
C        WORD  2 - PREPBUFR REPORT TYPE
C                     (=161 FOR CLEAR PATH OVER LAND)
C                     (=163 FOR CLOUDY PATH OVER LAND)
C                     (=171 FOR CLEAR PATH OVER OCEAN)
C                     (=173 FOR CLOUDY PATH OVER OCEAN)
C        WORD  3 - SWATH LOCATION :  1 INDICATES LEFT  LIMB OF SWATH
C                                   56 INDICATES RIGHT LIMB OF SWATH
C                           USED FOR DETERMINING DISTANCE FROM NADIR
C        WORD  4 - ORBIT NUMBER
C        WORD  5 - NESDIS LAND-SEA FLAG
C                     (= 0 FOR SEA)
C                     (= 1 FOR LAND)
C        WORD  6 - SATELLITE DATA PROCESSING TECHNIQUE
C                     (= 1 FOR CLEAR PATH)
C                     (= 3 FOR CLOUDY PATH)
C        WORD  7 - ATOVS FILTER FLAG (=  0 FOR GOOD)
C                                    (=  1 FOR REDUNDANT)
C                                    (=-99 FOR MISSING)
C                   IMPORTANT: REDUNDANT RETRIEVALS ARE AT FULL 40 KM
C                              RESOLUTION; GOOD RETRIEVALS ARE AT
C                              250 KM RESOLUTION
C        WORD  8 - SUPERADIABATIC FLAG (=0 FOR NOT SUPERADIABATIC)
C                                      (.NE. 0 FOR SUPERADIABATIC)
C        WORD  9 - DAY/NIGHT QUALIFIER (=0 FOR NIGHT)
C                                      (=1 FOR DAY)
C                                      (BUFR C.T. 0-08-013)
C        WORD 10 - FRAME COUNT (BUFR C.T. 0-25-071)
C        WORD 11 - SNOW/ICE FLAGS (TERRAIN TYPE)
C                                      (BUFR C.T. 0-13-039)
C
C     CONTENTS OF OUTPUT ARGUMENT RSATOB (17 REAL WORDS) FOR EACH
C      RETRIEVAL:
C        WORD  1 - OBSERVATION TIME IN HOURS GOOD TO 0.01 HOUR.
C        WORD  2 - LATITUDE IN DEGREES (N+,S-)
C        WORD  3 - LONGITUDE IN DEGREES EAST (0.0 to 359.99)
C        WORD  4 - SURFACE HEIGHT IN METERS
C        WORD  5 - FIRST ABOVE-GROUND ATOVS PRESSURE LEVEL IN MB
C        WORD  6 - SKIN TEMPERATURE IN DEGREES K
C        WORD  7 - TOTAL PRECIPITABLE WATER IN MM
C        WORD  8 - TOTAL OZONE IN DOBSONS
C        WORD  9 - SEA SURFACE TEMPERATURE IN DEGREES K
C        WORD 10 - CLOUD-TOP PRESSURE IN MB (MISSING IF CLEAR)
C        WORD 11 - SOLAR ZENITH ANGLE IN DEGREES
C        WORD 12 - SATELLITE ZENITH ANGLE IN DEGREES
C        WORD 13 - CLOUD AMOUNT (C.T. 0-20-011)
C        WORD 14 - HIRS CHANNEL UTILIZATION FLAGS
C        WORD 15 - AMSU-A CHANNEL UTILIZATION FLAGS
C        WORD 16 - AMSU-B CHANNEL UTILIZATION FLAGS
C
C FOR ALL DATA: MISSING VALUES ARE RETURNED AS XMISS (99999.) (REAL) OR
C               IMISS (99999) (INTEGER)
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  NCEP WCOSS
C
C$$$
      SUBROUTINE W3ATOVSUNP(IUNIT,IBDATE,PP,TT,QQ,NLEV,IRTCHN,RTRAD,
     $ NCHN,STNID,ISATOB,RSATOB,DSNAME,IDSDAT,IDSDMP_8,IERR)

      REAL  PP(*),TT(*),QQ(*),RTRAD(*)

      INTEGER  IRTCHN(*),JDATE(5),JDUMP(5),NF(0:3),IBIT(2),IPRINT_CNT(2)

      INTEGER(8) IDSDMP_8,JDUMP_8(5)

      CHARACTER*4 CTYPE

      CHARACTER*8 SUBSET,DSNAME

      CHARACTER*50 IDTSTR
      DATA IDTSTR/'SAID CLAT CLON OBQL HOUR MINU SECO FOVN ORBN SADF '/
      CHARACTER*10 IDTST2
      DATA IDTST2/'DNQL TOFF '/

      CHARACTER*55 INFSTR
      DATA INFSTR
     $ /'SOEL SAZA FRCT OZON SDPT HIRC AMAC AMBC TPWT CLAM SZNT '/

      CHARACTER*32 SRFSTR
      DATA SRFSTR/'PRES SELV LSQL TMSKST SST1 TERN '/

      CHARACTER*25 RETSTR
      DATA RETSTR/'PRLC TMDBST MIXR '/

      CHARACTER*12 BRTSTR
      DATA BRTSTR/'CHNM TMBRST '/

      REAL(8) XIDENT_8(12),XINFO_8(11),SRFDAT_8(6),RETDAT_8(3,41),
     $        BRTDAT_8(2,35),AMAXIMUM_8
      REAL(8) BMISS,GETBMISS

      DIMENSION  KOUNT(0:3,0:3),ISATOB(11),RSATOB(16)

      CHARACTER*8   STNID

      CHARACTER*1   NMCHAR(0:35)

      SAVE

      DATA          NMCHAR/'0','1','2','3','4','5','6','7','8','9',
     $                     'A','B','C','D','E','F','G','H','I','J',
     $                     'K','L','M','N','O','P','Q','R','S','T',
     $                     'U','V','W','X','Y','Z'/
 
      DATA IFIRST/0/,KOUNTR/0/,LWI/0/,LWR/0/,MFIRST/0/,nfirst/0/
      DATA  KOUNT/16*0/,XMISS/99999./,IMISS/99999/,NF/31,13,35,17/,
     $ CTYPE/'DUMP'/,IPRINT_CNT/2*0/

C  FCN PR GETS STD. ATMOS. PRESSURE (MB) FROM HEIGHT (M) (HGHT < 11000M)
C  ---------------------------------------------------------------------

      PR(HGHT) = 1013.25 * (((288.15 - (.0065 * HGHT))/288.15)**5.256)

      CALL STATUS(IUNIT,LUN,IL,IM)

      IF(IL.EQ.0) THEN
         print'(" ~~~ input BUFR file is CLOSED - start fresh")'
         IFIRST = 0
      END IF

      IF(IFIRST.EQ.0) THEN

C  FIRST TIME IN TO OPEN AN INPUT FILE, INITIALIZE COUNTERS,
C   SET DATELEN, GET CENTER AND DUMP TIME FOR FILE, AND GET VALUE FOR
C   "BMISS"
C  ------------------------------------------------------------------

         PRINT'("  ==> W3ATOVSUNP -- WCOSS VERSION 02-14-2013")'
         IFIRST     = 1
         KOUNTR     = 0
         MFIRST     = 0
         nfirst     = 0
         KOUNT      = 0
         IPRINT_CNT = 0
         IDSDAT     = 99
         IDSDMP_8   = 99_8
         CALL DATELEN(10)
         IF(IERR.NE.1) THEN
            CTYPE = 'DUMP'
            CALL DUMPBF(IUNIT,JDATE,JDUMP)
cppppp
            print'(" INPUT FILE IS A DUMP FILE ...")'
            print'("  -- CENTER DATE (JDATE) = ",5(I0,1X))', jdate
            print'("  -- DUMP DATE   (JDUMP) = ",5(I0,1X))', jdump
            print'(1X)'
cppppp
            IF(JDATE(1).LE.0)  then
               PRINT'(" ##W3ATOVSUNP - CENTER DATE COULD NOT BE ",
     $         "OBTAINED FROM INPUT FILE ON UNIT ",I0," -- IERR = -3")',
     $         IUNIT
               IERR = -3
               RETURN
            END IF
            IF(JDATE(1).LT.100)  THEN

C IF 2-DIGIT YEAR RETURNED IN JDATE(1), MUST USE "WINDOWING" TECHNIQUE
C  TO CREATE A 4-DIGIT YEAR

C IMPORTANT: IF DATELEN(10) IS CALLED, THE DATE HERE SHOULD ALWAYS
C            CONTAIN A 4-DIGIT YEAR, EVEN IF INPUT FILE IS NOT
C            Y2K COMPLIANT (BUFRLIB DOES THE WINDOWING HERE)

               PRINT'(" ##W3ATOVSUNP - THE FOLLOWING SHOULD NEVER ",
     $          "HAPPEN!!!!!")'
               PRINT'(" ##W3ATOVSUNP - 2-DIGIT YEAR IN JDATE(1) ",
     $          "RETURNED FROM DUMPBF (JDATE IS: ",I5,4I3") - USE ",
     $          "WINDOWING TECHNIQUE TO OBTAIN 4-DIGIT YEAR")', JDATE
               IF(JDATE(1).GT.20)  THEN
                  JDATE(1) = 1900 + JDATE(1)
               ELSE
                  JDATE(1) = 2000 + JDATE(1)
               ENDIF
               PRINT'(" ##W3ATOVSUNP - CORRECTED JDATE(1) WITH 4-DIGIT",
     $          " YEAR, JDATE NOW IS: ",I5,4I3)', JDATE
            ENDIF
            IDSDAT = JDATE(1)*1000000+JDATE(2)*10000+JDATE(3)*100+
     $       JDATE(4)
            IF(JDUMP(1).LE.0)  THEN
               IDSDMP_8 = 999999999999_8
            ELSE
               IF(JDUMP(1).LT.100)  THEN

C IF 2-DIGIT YEAR RETURNED IN JDUMP(1), MUST USE "WINDOWING" TECHNIQUE
C  TO CREATE A 4-DIGIT YEAR

C IMPORTANT: IF DATELEN(10) IS CALLED, THE DATE HERE SHOULD ALWAYS
C            CONTAIN A 4-DIGIT YEAR, EVEN IF INPUT FILE IS NOT
C            Y2K COMPLIANT (BUFRLIB DOES THE WINDOWING HERE)

                  PRINT'(" ##W3ATOVSUNP - THE FOLLOWING SHOULD NEVER ",
     $             "HAPPEN!!!!!")'
                  PRINT'(" ##W3ATOVSUNP - 2-DIGIT YEAR IN JDUMP(1) ",
     $             "RETURNED FROM DUMPBF (JDUMP IS: ",I5,4I3") - USE ",
     $             "WINDOWING TECHNIQUE TO OBTAIN 4-DIGIT YEAR")', JDUMP
                  IF(JDUMP(1).GT.20)  THEN
                     JDUMP(1) = 1900 + JDUMP(1)
                  ELSE
                     JDUMP(1) = 2000 + JDUMP(1)
                  ENDIF
                  PRINT'(" ##W3ATOVSUNP - CORRECTED JDUMP(1) WITH 4-",
     $             "DIGIT YEAR, JDUMP NOW IS: ",I5,4I3)', JDUMP
               END IF
               JDUMP_8 = JDUMP
               IDSDMP_8 = JDUMP_8(1)*100000000+JDUMP_8(2)*1000000+
     $          JDUMP_8(3)*10000+JDUMP_8(4)*100+JDUMP_8(5)
            ENDIF
         ELSE
            CTYPE = 'TANK'
cppppp
            print'(1X)'
            print'(" INPUT FILE IS A TANK FILE ...")'
            print'("  -- CENTER DATE AND DUMP DATE ARE NOT RETURNED")'
            print'(1X)'
cppppp
         ENDIF
c-----------------------------------------------------------------------
c This logic is needed only because SUBSET is not returned in DUMPBF
c  and we need it in order to return dsname - will close data set
c  prior to openbf for ifirst=1 case
         call openbf(iunit,'IN',iunit)
         call readmg(iunit,subset,ibdate,iret)
         if(subset.eq.'NC003104')  then
            dsname = 'ATOVS   '
         else
            dsname = '????????'
         end if
c-----------------------------------------------------------------------

C  GET WORDLENGTH FOR INTEGERS AND REALS
C  -------------------------------------

         CALL WORDLENGTH(LWI,LWR)
         PRINT 678, LWI,LWR
  678 FORMAT('  ===> MACHINE WORD LENGTH: INTEGERS - ',I1,'; REALS - ',
     $ I1/)
c-----------------------------------------------------------------------

C  GET VALUE FOR "BMISS"
C  --------------------

         BMISS = GETBMISS()
         print'(1X)'
         print'(" BUFRLIB value for missing passed into W3ATOVSUNP is:",
     $    1X,G0)', bmiss
         print'(1X)'
c-----------------------------------------------------------------------

         IERR = 1
         RETURN
      ELSE  IF(IFIRST.EQ.1) THEN
c-----------------------------------------------------------------------
c This logic is needed only because SUBSET is not returned in DUMPBF
c  (see a few lines above)
         call closbf(iunit)
c-----------------------------------------------------------------------

C  SECOND TIME IN TO OPEN AN INPUT FILE, OPEN BUFR DATASET FOR INPUT
C   AND DECODE FIRST MESSAGE
C  -----------------------------------------------------------------

         IFIRST = 2
         CALL OPENBF(IUNIT,'IN',IUNIT)
         CALL READMG(IUNIT,SUBSET,IBDATE,IRET)
         IF(IRET.NE.0) THEN
            WRITE(6,1009) IUNIT
 1009       FORMAT('##W3ATOVSUNP ERROR: EMPTY FILE IN UNIT ',I5)
            IERR = -1
            RETURN
         ENDIF
         IF(IBDATE.LT.100000000)  THEN
C IF INPUT BUFR FILE DOES NOT RETURN MESSAGES WITH A 4-DIGIT YEAR,
C  SOMETHING IS WRONG (EVEN NON-COMPLIANT BUFR MESSAGES SHOULD
C  CONSTRUCT A 4-DIGIT YEAR AS LONG AS DATELEN(10) HAS BEEN CALLED
            WRITE(6,1209) IUNIT
 1209       FORMAT('##W3ATOVSUNP ERROR: A 10-DIGIT SECT. 1 BUFR ',
     $       'MESSAGE DATE WAS NOT RETURNED IN UNIT',I5,' - PROBLEM ',
     $       'WITH BUFR FILE')
            IERR = -2
            RETURN
         END IF
      ENDIF

 1000 CONTINUE                                                          

C  EACH CALL TO READSB INCREASES "KOUNTR" BY 1 (REGARDLESS OF RESULT)
C  ------------------------------------------------------------------

      KOUNTR = KOUNTR + 1

      CALL READSB(IUNIT,IRET)
      IF(IRET.NE.0) THEN
        CALL READMG(IUNIT,SUBSET,IBDATE,IRET)
        IF(IRET.NE.0) THEN

C  ALL BUFR MESSAGES HAVE BEEN READ AND DECODED -- ALL DONE
C  --------------------------------------------------------

           WRITE(6,1001) IUNIT
 1001 FORMAT(//' ==> W3ATOVSUNP: END OF FILE ON UNIT',I3,' -- ALL ',
     $ 'DONE'/)
           call closbf(iunit)
           IERR = 2
           RETURN
        ENDIF
         IF(IBDATE.LT.100000000)  THEN
C IF INPUT BUFR FILE DOES NOT RETURN MESSAGES WITH A 4-DIGIT YEAR,
C  SOMETHING IS WRONG (EVEN NON-COMPLIANT BUFR MESSAGES SHOULD
c  CONSTRUCT A 4-DIGIT YEAR AS LONG AS DATELEN(10) HAS BEEN CALLED
            WRITE(6,1209) IUNIT
            IERR = -2
            RETURN
         END IF
        GO TO 1000
      ENDIF

      XIDENT_8 = BMISS
      CALL UFBINT(IUNIT,XIDENT_8,12, 1,IRET,IDTSTR//IDTST2)

      XINFO_8 = BMISS
      CALL UFBINT(IUNIT,XINFO_8 ,11, 1,IRET,INFSTR)

      SRFDAT_8 = BMISS
      CALL UFBINT(IUNIT,SRFDAT_8, 6, 1,IRET,SRFSTR)

      RETDAT_8 = BMISS
      CALL UFBREP(IUNIT,RETDAT_8, 3,41,NLEV,RETSTR)

      BRTDAT_8 = BMISS
      CALL UFBREP(IUNIT,BRTDAT_8, 2,35,NCHN,BRTSTR)

      ISATOB = IMISS
      RSATOB = XMISS
      PP(1:40) = XMISS
      TT(1:40) = XMISS
      QQ(1:40) = XMISS
      IRTCHN(1:35) = IMISS
      RTRAD(1:35) = XMISS

C  SATELLITE IDENTIFIER
C  --------------------

      IF(IBFMS(XIDENT_8(1)).EQ.0)  THEN
         ISATOB(1) = NINT(XIDENT_8(1))
         IF(ISATOB(1).NE.4.AND.
     $     (ISATOB(1).LT.200.OR.ISATOB(1).GT.209)) THEN
            WRITE(6,2071) KOUNTR,ISATOB(1)
 2071 FORMAT(/'##W3ATOVSUNP WARNING: A DECODED RETRIEVAL (#',I7,') HAS',
     $ ' AN UNRECOGNIZED SATELLITE ID (=',I5,') - SKIP IT')
            GO TO 1000
         ENDIF
      ELSE
         WRITE(6,2072) KOUNTR
 2072 FORMAT(/'##W3ATOVSUNP WARNING: A DECODED RETRIEVAL (#',I7,') HAS',
     $ ' A MISSING SATELLITE ID - SKIP IT')
         GO TO 1000
      ENDIF

C  LATITUDE
C  --------

      IF(IBFMS(XIDENT_8(2)).EQ.0)  THEN
         RSATOB(2) = XIDENT_8(2)
      ELSE
         WRITE(6,2073) KOUNTR
 2073 FORMAT(/'##W3ATOVSUNP WARNING: A DECODED RETRIEVAL (#',I7,') HAS',
     $ ' A MISSING LATITUDE - SKIP IT')
         GO TO 1000
      ENDIF

C  LONGITUDE
C  ---------

      IF(IBFMS(XIDENT_8(3)).EQ.0)  THEN

C Important: According to BUFR Manual, CLON (0-06-002) - represented
C  here by "XIDENT_8(3)" - should be in units of Degrees West - and
C  East + (-180.0 to +180.0); however some BUFR data sets (e.g.,
C  PREPBUFR) are known to encode 0-06-002 in units of Degrees East
C  (0.0 to 359.99)
C  -- So we use the following conversion to work in either case ...
         RSATOB(3) = 360._8 - MOD(360._8-XIDENT_8(3),360._8)
         IF(RSATOB(3).EQ.360.0)  RSATOB(3) = 0.0
      ELSE
         WRITE(6,2074) KOUNTR
 2074 FORMAT(/'##W3ATOVSUNP WARNING: A DECODED RETRIEVAL (#',I7,') HAS',
     $ ' A MISSING LONGITUDE - SKIP IT')
         GO TO 1000
      ENDIF

C  FILTER FLAG
C  -----------

      IF(IBFMS(XIDENT_8(12)).EQ.0)  THEN
C  If NESDIS starts encoding filter flag in "TOFF" ...
         ISATOB(7) = NINT(XIDENT_8(12))
      ELSE IF(IBFMS(XIDENT_8(4)).EQ.0)  THEN
C  If NESDIS starts encoding filter flag in "OBQL" ...
         ISATOB(7) = NINT(XIDENT_8(4))
      ELSE
cvvvvvdak
C  Currently filter flag is missing - set to "-99" until NESDIS fixes
C   (alerts calling program to process only every 11'th retrieval to
C   simulate 250km sampling - needed for cdas runs)
cdak     WRITE(6,2075) KOUNTR
c2075 FORMAT(/'##W3ATOVSUNP WARNING: A DECODED RETRIEVAL (#',I7,') HAS',
cdak $ ' A MISSING FILTER FLAG - SKIP IT')
cdak     GO TO 1000
         if(nfirst.eq.0)  then
            write(6,9082)
 9082 format(/'##W3ATOVSUNP WARNING: NESDIS HAS NOT STORED A FILTER ',
     $ 'FLAG SO SET OUTPUT FILTER FLAG TO -99 FOR ALL REPORTS'/)
            nfirst=1
         end if
         isatob(7) = -99
caaaaadak
      ENDIF

C  OBSERVATION TIME
C  ----------------

      AMAXIMUM_8 = MAX(XIDENT_8(5),XIDENT_8(6),XIDENT_8(7))
      IF(IBFMS(AMAXIMUM_8).EQ.0) THEN
         HRFRAC = (60.0 * XIDENT_8(6) + XIDENT_8(7)) / 3600.0
         RSATOB(1) = XIDENT_8(5) + HRFRAC
         RSATOB(1) = 0.01 * AINT(100.0 * RSATOB(1) + 0.5)
      ELSE
         WRITE(6,2076) KOUNTR
 2076 FORMAT(/'##W3ATOVSUNP WARNING: A DECODED RETRIEVAL (#',I7,') HAS',
     $ ' ONE OR MORE MISSING TIME UNITS - SKIP IT')
         GO TO 1000
      ENDIF

C  SWATH LOCATION
C  --------------

      IF(IBFMS(XIDENT_8(8)).EQ.0)  THEN
         ISATOB(3) = NINT(XIDENT_8(8))
      ELSE
         WRITE(6,2077) KOUNTR
 2077 FORMAT(/'##W3ATOVSUNP WARNING: A DECODED RETRIEVAL (#',I7,') HAS',
     $ ' A MISSING SWATH LOCATION - SKIP IT')
         GO TO 1000
      ENDIF

C  ORBIT NUMBER
C  ------------

      IF(IBFMS(XIDENT_8(9)).EQ.0)  ISATOB(4) = NINT(XIDENT_8(9))

C  SUPERADIABATIC FLAG
C  -------------------

      IF(IBFMS(XIDENT_8(10)).EQ.0)  ISATOB(8) = NINT(XIDENT_8(10))
      IF(ISATOB(8).NE.0)  THEN
         WRITE(6,2078) KOUNTR,ISATOB(8)
 2078 FORMAT(/'##W3ATOVSUNP WARNING: A DECODED RETRIEVAL (#',I7,') IS ',
     $ 'SUPERADIABATIC (FLAG=',I5,') - SKIP IT')
         GO TO 1000
      ENDIF

C  DAY-NIGHT INDICATOR
C  -------------------

      IF(IBFMS(XIDENT_8(11)).EQ.0)  ISATOB(9) = NINT(XIDENT_8(11))

C  SOLAR ZENITH ANGLE
C  ------------------

      IF(IBFMS(XINFO_8(1)).EQ.0)  RSATOB(11) = XINFO_8(1)

C  SATELLITE ZENITH ANGLE
C  ----------------------

      IF(IBFMS(XINFO_8(2)).EQ.0)  THEN
         RSATOB(12) = XINFO_8(2)  ! SAZA is mnemonic after 3/2002
      ELSE IF(IBFMS(XINFO_8(11)).EQ.0)  THEN
         RSATOB(12) = XINFO_8(11) ! SZNT is mnemonic before 3/2002
      END IF
      
C  FRAME COUNT
C  -----------

      IF(IBFMS(XINFO_8(3)).EQ.0)  ISATOB(10) = XINFO_8(3)

C  OZONE
C  -----

      IF(IBFMS(XINFO_8(4)).EQ.0)  RSATOB(8) = XINFO_8(4)

C  SATELLITE DATA PROCESSING TECHNIQUE
C  -----------------------------------

      IF(IBFMS(XINFO_8(5)).EQ.0)  THEN
         ISDPT = NINT(XINFO_8(5))
         IOFF = (LWI * 8) - 6
         CALL GBYTES(ISDPT,IBIT,IOFF,1,1,2)
cppppp
cdak     print'(" ISDPT,ICLR,ICDY:",3(I0,1X))', ISDPT,IBIT,ICDY
cppppp
      ELSE
         WRITE(6,2079) KOUNTR
 2079 FORMAT(/'##W3ATOVSUNP WARNING: A DECODED RETRIEVAL (#',I7,') HAS',
     $ ' A MISSING SATELLITE DATA PROCESSING TECHNIQUE - SKIP IT')
         GO TO 1000
      ENDIF

C  HIRS CHANNEL UTILIZATION FLAGS
C  ------------------------------

      IF(IBFMS(XINFO_8(6)).EQ.0)  RSATOB(14) = XINFO_8(6)

C  AMSU-A CHANNEL UTILIZATION FLAGS
C  --------------------------------

      IF(IBFMS(XINFO_8(7)).EQ.0)  RSATOB(15) = XINFO_8(7)

C  AMSU-B CHANNEL UTILIZATION FLAGS
C  --------------------------------

      IF(IBFMS(XINFO_8(8)).EQ.0)  RSATOB(16) = XINFO_8(8)

C  TOTAL PRECIPITABLE WATER (IN MM)
C  --------------------------------

      IF(IBFMS(XINFO_8(9)).EQ.0)  RSATOB(7) = XINFO_8(9)

C  CLOUD AMOUNT
C  ------------

      IF(IBFMS(XINFO_8(10)).EQ.0)  RSATOB(13) = XINFO_8(10)

C  STATION ID
C  ----------

      STNID = '      A '
      MODSAT = MOD(ISATOB(1),4)
      ISATOB(6) = IMISS
      IF(IBIT(1).EQ.1.AND.IBIT(2).EQ.0) THEN

C  CLEAR PATH
C  ----------

         ISATOB(6) = 1
         STNID(8:8) = NMCHAR(NF(MODSAT)-3)
      ELSE IF((IBIT(1).EQ.0.AND.IBIT(2).EQ.1).OR.
     $        (IBIT(1).EQ.0.AND.IBIT(2).EQ.0)) THEN

C  CLOUDY PATH
C  -----------

         ISATOB(6) = 3
         STNID(8:8) = NMCHAR(NF(MODSAT)-1)
      ELSE

C  UNKNOWN PATH
C  ------------

         ISATOB(6) = 0
         STNID(8:8) = NMCHAR(NF(MODSAT))
         WRITE(6,2080) KOUNTR,STNID,NINT(XINFO_8(5)),IBIT
 2080 FORMAT(/'##W3ATOVSUNP WARNING: A DECODED RETRIEVAL (#',I7,',ID=',
     $ A8,') HAS AN UNKNOWN SATELLITE DATA PROCESSING TECH. (=',I5,','/
     $ 24X,'WITH ICLR=',I1,' AND ICDY=',I1,') - SKIP IT')
         GO TO 1000
      ENDIF

      KOUNT(ISATOB(6),MODSAT) = MIN(99999,KOUNT(ISATOB(6),MODSAT) + 1)
      WRITE(STNID(2:6),'(I5.5)') KOUNT(ISATOB(6),MODSAT)

      NADIR = INT(ABS(FLOAT(ISATOB(3)) - 28.5)) / 3
      IF(NADIR.LT.9) NADIR = NADIR + 1
      IF(NADIR.GT.0.AND.NADIR.LT.10)  THEN
         STNID(1:1) = NMCHAR(NADIR)
      ELSE
         WRITE(6,2081) KOUNTR,STNID,NADIR
 2081 FORMAT(/'##W3ATOVSUNP WARNING: A DECODED RETRIEVAL (#',I7,',ID=',
     $ A8,') HAS AN INVALID NADIR PROX. IND. (=',I5,') - SKIP IT')
         GO TO 1000
      ENDIF

      IF(NLEV.EQ.41)  THEN

C  CLOUD-TOP PRESSURE (ON LEVEL 1)
C  -------------------------------

         IF(IBFMS(RETDAT_8(1,1)).EQ.0) RSATOB(10) = RETDAT_8(1,1) * 0.01

C  Note: A value of 1250 mb means clear conditions, set cloud top
C        pressure to missing in this case

         IF(NINT(RSATOB(10)).EQ.1250)  RSATOB(10) = XMISS

C  FILL IN THE LEVEL SOUNDING DATA - MUST HAVE 40 TEMP LEVELS TO ACCEPT
C  --------------------------------------------------------------------

         DO L = 2,NLEV
            LREV = NLEV + 1 - L
            PP(LREV) = RETDAT_8(1,L) * 0.01
            IF(IBFMS(RETDAT_8(2,L)).EQ.0) THEN
               IF(IBFMS(RETDAT_8(3,L)).NE.0) THEN
                  TT(LREV) = RETDAT_8(2,L)
               ELSE
                  QQ(LREV) = RETDAT_8(3,L) / (1.0 + RETDAT_8(3,L))
                  TT(LREV) = RETDAT_8(2,L) * (1.0 + 0.61 * QQ(LREV))
               ENDIF
            ELSE
C  (The amount of printout is limited if input is a TANK since this
C   is very common and can generate thousands of lines of print.)
               IF(CTYPE.EQ.'DUMP'.OR.IPRINT_CNT(2).LT.250)  THEN
                  WRITE(6,3082) KOUNTR
 3082 FORMAT(/'##W3ATOVSUNP WARNING: A DECODED RETRIEVAL (#',I7,') ',
     $ 'CONTAINS 1 OR MORE MISSING TEMPERATURES IN ITS SOUNDING - SKIP',
     $ ' IT')
               ELSE IF(IPRINT_CNT(2).EQ.250)  THEN
                  WRITE(6,3088)
 3088 FORMAT(/'##W3ATOVSUNP WARNING: THE NUMBER OF DECODED RETRIEVALS ',
     $ 'CONTAINING 1 OR MORE MISSING TERMPERATURES IN ITS'/22X,
     $ 'SOUNDING HAS HIT THE PRINTOUT LIMIT - NO MORE PRINTOUT WILL BE',
     $ ' GENERATED'/)
               END IF
               IPRINT_CNT(2) = IPRINT_CNT(2) + 1
               GO TO 1000
            ENDIF
         ENDDO
         NLEV = 40
      ELSE
         WRITE(6,2082) STNID,KOUNTR,NLEV
 2082 FORMAT(/'##W3ATOVSUNP WARNING: ID ',A8,' (#',I7,') DOES NOT ',
     $ 'CONTAIN 40 SOUNDING LVLS PLUS 1 CLOUD-TOP LVL (HAS ',I3,
     $ ' LVLS) -- NO SOUNDING LEVEL DATA PROCESSED')
      ENDIF

      IF(NCHN.EQ.35)  THEN

C  FILL IN THE RADIANCE DATA
C  -------------------------

         DO L = 1,NCHN
            IF(IBFMS(BRTDAT_8(1,L)).EQ.0) IRTCHN(L) =NINT(BRTDAT_8(1,L))
            IF(IBFMS(BRTDAT_8(2,L)).EQ.0) RTRAD(L)  = BRTDAT_8(2,L)
         ENDDO
      ELSE
         WRITE(6,2083) STNID,KOUNTR,NCHN
 2083 FORMAT(/'##W3ATOVSUNP WARNING: ID ',A8,' (#',I7,') DOES NOT ',
     $ 'CONTAIN 35 RADIANCE CHANNELS (HAS ',I3,' CHANNELS) -- NO ',
     $ 'RADIANCE DATA PROCESSED')
      ENDIF

C  SURFACE HEIGHT
C  --------------

      IF(IBFMS(SRFDAT_8(2)).EQ.0)  THEN
         RSATOB(4) = SRFDAT_8(2)
      ELSE
         WRITE(6,2085) KOUNTR,STNID
 2085 FORMAT(/'##W3ATOVSUNP WARNING: A DECODED RETRIEVAL (#',I7,',ID=',
     $ A8,') HAS A MISSING SURFACE HEIGHT - SKIP IT')
         GO TO 1000
      ENDIF

C  BOTTOM LEVEL PRESSURE
C  ---------------------

      IF(SRFDAT_8(1).LT.20000.)  THEN
C --> NESDIS is scaling bottom pressure incorrectly, this accounts for
C      this and for a future fix on their part (fixed on 11/07/2001)
         RSATOB(5) = 0.1 * SRFDAT_8(1)
         IF(MFIRST.EQ.0)  THEN
            WRITE(6,9081)
 9081 FORMAT(/'##W3ATOVSUNP WARNING: NESDIS IS STORING BOTTOM PRESSURE',
     $ ' AS 0.1 PASCALS INSTEAD OF AS PASCALS, THIS SUBROUTINE WILL ',
     $ 'CORRECT'/24X,'FOR THIS UNTIL NESDIS FIXES THEIR ERROR'/)
            MFIRST=1
         END IF
      ELSE IF(IBFMS(SRFDAT_8(1)).EQ.0)  THEN
         RSATOB(5) = 0.01 * SRFDAT_8(1)
      ELSE
         WRITE(6,2084) KOUNTR,STNID
 2084 FORMAT(/'##W3ATOVSUNP WARNING: A DECODED RETRIEVAL (#',I7,',ID=',
     $ A8,') HAS A MISSING BOTTOM PRESSURE LEVEL - SKIP IT')
         GO TO 1000
      ENDIF
C  Do a SANITY check on pressure (NESDIS can have some strange values)
C  (The amount of printout is limited if input is a TANK since this
C   is very common and can generate thousands of lines of print.)
      DVAL = RSATOB(5) - PR(RSATOB(4))
      IF(DVAL.LE.-75..OR.DVAL.GE.50.)  THEN
         IF(CTYPE.EQ.'DUMP'.OR.IPRINT_CNT(1).LT.250)  THEN
            WRITE(6,2087) KOUNTR,STNID,RSATOB(5),NINT(RSATOB(4))
 2087 FORMAT(/'##W3ATOVSUNP WARNING: A DECODED RETRIEVAL (#',I7,',ID=',
     $ A8,') HAS A BAD BOTTOM PRES LVL (PBOT=',F6.1,' MB, ELEV=',I4,
     $ ' M) - SKIP IT')
         ELSE IF(IPRINT_CNT(1).EQ.250)  THEN
            WRITE(6,3087)
 3087 FORMAT(/'##W3ATOVSUNP WARNING: THE NUMBER OF DECODED RETRIEVALS ',
     $ 'WITH A BAD BOTTOM PRES LVL HAS HIT THE PRINTOUT LIMIT - '/22X,
     $ 'NO MORE PRINTOUT WILL BE GENERATED'/)
         END IF
         IPRINT_CNT(1) = IPRINT_CNT(1) + 1
         GO TO 1000
      ENDIF

C  REPORT TYPE AND LAND/SEA TAG
C  ----------------------------

      IF(NINT(SRFDAT_8(3)).EQ.1)  THEN
C  Sea (LSQL=1)
         ISATOB(2) = 170 + ISATOB(6)
         ISATOB(5) = 0
      ELSE  IF(NINT(SRFDAT_8(3)).EQ.0.OR.NINT(SRFDAT_8(3)).EQ.2)  THEN
C  Land (LSQL=0) or Coast (LSQL=2)
         ISATOB(2) = 160 + ISATOB(6)
         ISATOB(5) = 1
      ELSE
         WRITE(6,2086) KOUNTR,STNID
 2086 FORMAT(/'##W3ATOVSUNP WARNING: A DECODED RETRIEVAL (#',I7,',ID=',
     $ A8,') HAS A MISSING LAND/SEA INDICATOR - SKIP IT')
         GO TO 1000
      ENDIF

C  SKIN TEMPERATURE
C  ----------------

      IF(IBFMS(SRFDAT_8(4)).EQ.0)  RSATOB(6) = SRFDAT_8(4)

C  SEA-SURFACE TEMPERATURE
C  -----------------------

      IF(IBFMS(SRFDAT_8(5)).EQ.0)  RSATOB(9) = SRFDAT_8(5)

C  TERRAIN INDICATOR
C  -----------------

      IF(IBFMS(SRFDAT_8(6)).EQ.0)  ISATOB(11) = NINT(SRFDAT_8(6))

C  RETURN WITH DECODED ATOVS RETRIEVAL
C  -----------------------------------

      IERR = 0

      RETURN
      END
