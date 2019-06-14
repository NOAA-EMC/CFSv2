C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: BUFR_DUPRAD
C   PRGMMR: WHITING          ORG: EMC         DATE: 2013-02-01
C
C ABSTRACT: PROCESSES LEVEL 3, 2.5 AND 2 WSR-88D NEXRAD RADAR RADIAL
C   WIND DATABASE REPORTS AND LEVEL 2 WSR-88D NEXRAD REFLECTIVITY
C   DATABASE REPORTS WITH DUPLICATE CHECKING AND TRIMMING TO EXACT TIME
C   WINDOW (DAY DOWN TO SECOND).  THE ALGORITHM SORTS THE REPORTS IN
C   ORDER OF LATITUDE, LONGITUDE, OBSERVATION TIME (TO SECOND), MESSAGE
C   NUMBER (FOR LEVEL 3/2.5 ONLY), ANTENNA TILT (ELEVATION) ANGLE AND
C   (FOR LEVEL 2 REPORTS ONLY) AZIMUTH ANGLE.  THE FILE PATH/NAMES OF
C   THE INPUT AND OUTPUT FILES, (OPTIONALLY) THE TIME WINDOW TO TRIM
C   TO, AND (OPTIONALLY) DEFAULT OVERRIDE DUP CHECKING PARAMETERS ARE
C   READ FROM STANDARD INPUT (UNIT 5) AT THE START OF THIS PROGRAM.  IF
C   THE TIME WINDOW RECORD IS MISSING, THEN NO TIME WINDOWING IS
C   PERFORMED AND THE DEFAULT DUP CHECKING PARAMETERS ARE USED.  ALL
C   OTHER FILE CONNECTIONS ARE MADE THROUGH THE FORTRAN OPEN STATEMENT.
C
C PROGRAM HISTORY LOG:
C 2000-09-17  W. FACEY    ORIGINAL VERSION FOR IMPLEMENTATION
C 2003-05-09  W. FACEY    CHANGED MNEMONIC FOR ANTENNA ELEV ANGLE
C     FROM "AELV" TO CORRECT VALUE OF "ANEL", THIS ERROR LED TO ABOUT 4
C     TIMES TOO MANY DUPLICATES
C 2003-05-14  D. KEYSER   MODIFICATIONS TO PREVENT ARRAY OVERFLOW WHEN
C     THERE ARE > MXTB REPORTS; IMPROVED DIAGNOSTIC PRINT, CORRECTED
C     ERRORS IN ACCUMULATING COUNTS FOR FINAL PRINT SUMMARY; INCREASED
C     LIMIT FOR I/O FILENAME LENGTH FROM 80 CHARACTERS TO 500
C     CHARACTERS;  ADDED CALL TO COMPRESS_CHECK TO INDICATE IF INPUT/
C     OUTPUT FILES ARE COMPRESSED OR UNCOMPRESSED
C 2004-02-02 V. K. KUMAR  MODIFIED TO HANDLE LEVEL 2.5 SUPEROB DATA
C     WHERE THE UPSTREAM INGEST CODE SPLITS A SINGLE RADAR SITE AND
C     TILT ANGLE INTO "PIECES" (SUBSETS) EACH CONTAINING NO MORE THAN
C     500 "LEVELS" IN ORDER TO AVOID A SUBSET BEING LARGER THAN A
C     MESSAGE, W/O ANY CHANGES THIS CODE WOULD TOSS THE EXTRA SUBSETS
C     BECAUSE THEY WOULD BE DUPLICATES (I.E., SAME LAT/LON, HEIGHT,
C     TIME, TILT) - SO, NEW MNEMONIC "MGPT" (MESSAGE NUMBER) WAS ADDED
C     IN INGEST TO DIFFERENTIATE THE PIECES (I.E., IF A COMPLETE
C     STATION/TILT ANGLE CONSISTED OF 1200 "LEVELS", MGPT IS SET TO 1
C     FOR FIRST 500 LEVEL SUBSET, IS SET TO 2 FOR THE SECOND 500 LEVEL
C     SUBSET, AND IS SET TO 3 FOR THE THIRD 200 LEVEL SUBSET), THIS
C     PROGRAM THEN ADDS THE REQUIREMENT THAT "MGPT" MUST ALSO MATCH IN
C     ORDER FOR THE DUPLICATE CHECK TO BE SATISFIED
C 2004-02-02  D. KEYSER   REPLACED CALL TO IN-LINE SUBROUTINE
C     COMPRESS_CHECK WITH CALL TO NEW BUFRLIB ROUTINE MESGBC
C 2006-03-02  D. KEYSER   CHANGES TO ALLOW FOR PROPER PROCESSING OF
C     NEXRAD LEVEL 2 RADIAL WINDS IN MESSAGE TYPES NC006010 TO NC006033
C     OR REFLECTIVITY IN MESSAGE TYPES NC006040 TO NC006063: INCREASED
C     NUMBER OF REPORTS THAT CAN BE PROCESSED FROM 1 MILLION TO 6
C     MILLION, CONSIDERS REPORT TIME OUT TO SECOND IN DUP-CHECKING
C     (WAS ONLY OUT TO MINUTE FOR LEVEL 3 AND LEVEL 2.5 RADIAL WIND
C     REPORTS IN NC006001 AND NC006002, RESP.), CONSIDERS AZIMUTH
C     ANGLE IN DUP-CHECKING (NOT PRESENT IN LEVEL 3/2.5 REPORTS), DOES
C     NOT CONSIDER MESSAGE NUMBER IN DUP-CHECKING (NOT PRESENT FOR
C     LEVEL 2, STILL CONSIDERED FOR LEVEL 3/2.5), DOES NOT CONSIDER
C     SITE OR ANTENNA ELEVATION IN DUP-CHECKING FOR ANY TYPE (HAD DONE
C     SO FOR LEVEL 3/2.5 BUT THIS IS ALWAYS THE SAME FOR A SITE AND
C     LAT/LON, WHICH IS SITE SPECIFIC, IS ALREADY CONSIDERED); CHECKS
C     TO SEE IF INPUT BUFR FILE CONTAINS "DUMMY" MESSAGES CONTAINING
C     DUMP CENTER TIME AND PROCESSING TIME, RESP. IN FIRST TWO MESSAGES
C     OF INPUT FILE (AFTER TABLE MSGS) BY CHECKING THE VALUE OF DUMPJB
C     SCRIPT VARIABLE "DUMMY_MSGS" (READ IN VIA "GETENV") - IF NOT WILL
C     NOT PROCESS INPUT BUFR MESSAGES WITH ZERO SUBSETS AND WILL CALL
C     BUFRLIB ROUTINE CLOSMG WITH A NEGATIVE UNIT NUMBER ARGUMENT PRIOR
C     TO ALL PROCESSING IN ORDER TO SIGNAL IT THAT ANY OUTPUT BUFR
C     MESSAGES WITH ZERO SUBSETS SHOULD BE SKIPPED (NOT WRITTEN OUT) -
C     CODE HAD BEEN HARDWIRED TO ALWAYS ASSUME DUMMY MESSAGES WERE IN
C     THE INPUT FILE; IF TIME WINDOW RECORD (LINE 3) IS EMPTY IN
C     STANDARD INPUT, TIME WINDOW TRIMMING LOOP BYPASSED, BASED ON
C     TEST FOR DEFAULT VALUE OF LATEST REQUESTED DATE WHICH IS NOW
C     CORRECTED TO BE 99999999.00_8 (WAS 99999999.00 BUT IN 4-BYTE REAL
C     MACHINES THIS COULD BE ROUNDED UP TO 100000000.00 MEANING LASTEST
C     DDHH.hh IN TIME TESTS, BEFORE TIME WINDOW TRIMMING LOOP WAS
C     BYPASSED, WOULD BE 0000 AND ALL REPORTS WOULD BE TRIMMED RATHER
C     THAN RETAINED)
C 2007-01-04  D. KEYSER   FIXED A BUG IN CODE WHICH PREVENTED ANY
C     DUPLICATE REPORTS FROM BEING TOSSSED
C 2007-03-23  D. KEYSER   INTRODUCED ALLOCATABLE ARRAYS TO AVOID ARRAY
C     OVERFLOW PROBLEMS, DETERMINES SIZE OF ARRAYS BY CALLING UFBTAB
C     WITH NEGATIVE UNIT NUMBER TO SIMPLY COUNT SUBSETS
C 2007-04-30  D. KEYSER   CHECK TO SEE IF INPUT FILE IS A /dcom OR
C     /dcomdev TANK (I.E., FILE HAS NOT BEEN COPIED FROM TANK TO
C     WORKING DIRECTORY - USUALLY OCCURS ONLY WITH LEVEL 2 DATA) FROM
C     THE SAME HOUR AS THE CURRENT WALL-CLOCK TIME HOUR - SUCH FILES
C     CAN GROW BETWEEN THE TIME THIS PROGRAM CHECKS THE NUMBER OF
C     SUBSETS IN THE FILE (TO DETERMINE ALLOCATABLE ARRAY SIZES) AND
C     THE TIME THIS PROGRAM PROCESSES SUBSETS IN THE FILE, RESULTING IN
C     THE UFBTAB ARRAY LIMIT BEING HIT - TO PREVENT THIS FROM HAPPENING
C     IN THIS CASE, SET THE ALLOCATABLE ARRAY SIZE TO AT LEAST 3
C     MILLION
C 2007-05-25  D. KEYSER   EXPAND CHECK IN 2007-04-30 CHANGE ABOVE TO
C     INCLUDE INPUT FILE FROM /dcom OR /dcomdev TANK FROM HOUR WHICH IS
C     ONE-HOUR PRIOR TO CURRENT WALL-CLOCK TIME HOUR (IN ADDITION TO
C     SAME HOUR AS CURRENT WALL-CLOCK TIME HOUR) - THESE FILES CAN ALSO
C     GROW BETWEEN THE TIME THIS PROGRAM CHECKS THE NUMBER OF SUBSETS
C     IN THE FILE (TO DETERMINE ALLOCATABLE ARRAY SIZES) AND THE TIME
C     THIS PROGRAM PROCESSES SUBSETS IN THE FILE, RESULTING IN THE
C     UFBTAB ARRAY LIMIT BEING HIT - TO PREVENT THIS FROM HAPPENING IN
C     THIS CASE, SET THE ALLOCATABLE ARRAY SIZE TO AT LEAST 3 MILLION
C 2010-05-25  D. KEYSER   CALLS TO IREADMG NOW PASS IN THE NEGATIVE OF
C     THE UNIT NUMBER SO THAT READ ERRORS WILL BE TREATED THE SAME AS
C     END-OF-FILE CONDITIONS AND THIS CODE WILL NOW RUN TO COMPLETION
C     SUCCESSFULLY IN SUCH CASES (NEEDED BECAUSE LATEST BUFRLIB SEEMS
C     TO BE MORE SENSITIVE TO CORRUPTED BUFR MESSAGES - THIS CHANGE
C     WILL PREVENT ABORTS IN SUCH SITUATIONS); IN CASES WHERE THE INPUT
C     FILE (i.e., /dcom OR /dcomdev) MAY BE GROWING BETWEEN THE TIME
C     THIS PROGRAM CHECKS THE NUMBER OF SUBSETS IN THE FILE (TO
C     DETERMINE ALLOCATABLE ARRAY SIZES) AND THE TIME THIS PROGRAM
C     PROCESSES SUBSETS IN THE FILE (RESULTING IN THE UFBTAB ARRAY
C     LIMIT BEING HIT), THE ALLOCATABLE ARRAY SIZE IS NOW SET TO 3
C     MILLION FOR CASES WHERE THE ORIGINAL NUMBER OF SUBSETS IN THE
C     FILE IS 2.5 MILLION OR LESS, AND IS SET TO 20% MORE THAN THE
C     ORIGINAL NUMBER OF SUBSETS IN THE FILE WHEN THE ORIGINAL NUMBER
C     OF SUBSETS IN THE FILE IS 2.5 MILLION OR MORE - ALLOWS FOR
C     EXPANSION EVEN WHEN > 3 MILLION SUBSETS WERE ORIGINALLY IN THE
C     FILE SINCE THESE FILES CAN ALSO GROW; AS A RESULT OF NEW BUFRLIB
C     WHICH CAN HANDLE EMBEDDED DICTIONARY MESSAGES, INCREASES AMOUNT
C     OF BUFRLIB PRINTOUT DURING (ONLY) THE POINT WHEN READING IN
C     MESSAGES IN ORDER TO DETECT ANY EMBEDDED DICTIONARY MESSAGES
C 2012-11-20  J. WOOLLEN  INITIAL PORT TO WCOSS -- ADAPTED IBM/AIX
C       GETENV SUBPROGRAM CALL TO INTEL/LINUX SYNTAX
C 2013-01-13  J. WHITING  PORT TO WCOSS -- UPDATED DOC BLOCKS;
C       REPLACED TESTS VS BMISS W/ IBFMS FUNCTION; REPLACED EXPLICIT
C       ASSIGNMENT OF BMISS W/ GETBMISS() FUNCTION.
C 2013-02-01  J. WHITING  PORT TO WCOSS -- UPDATED DUPES VARIABLE
C       ASSIGNMENT STATEMENT WITH 8-BYTE INTEGER INTRINSIC FUNCTIONS 
C       (KIDNNT) SO AS TO PROPERLY HANDLE LARGE (GLOBAL MISSING) 
C       VALUES;  DECLARED BMISS AND GETBMISS AS 8-BYTE REALS.
C
C USAGE:
C   INPUT FILES:
C     UNIT 05  - STANDARD INPUT - FIRST RECORD CONTAINS INPUT FILE
C                NAME, SECOND RECORD CONTAINS OUTPUT FILE NAME,
C                OPTIONAL THIRD RECORD CONTAINS TIME-WINDOWING
C                SPECIFICATIONS (THE YYYYMMDDHH<.HH> DATE OF THE
C                EARLIEST TIME TO DUMP AND THE YYYYMMDDHH<.HH> DATE OF
C                THE LATEST TIME TO DUMP), OPTIONAL FOURTH RECORD
C                CONTAINS DUP-CHECKING SPECIFICATIONS (IF FOURTH RECORD
C                IS MISSING, DEFAULT DUP-CHECKING SPECIFICATIONS ARE
C                USED, IF THIRD RECORD IS ALSO MISSING, NO TIME
C                WINDOWING IS PERFORMED)
C     UNIT 20  - UNCHECKED AND UNWINDOWED BUFR DUMP FILE
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C     UNIT 50  - DUPLICATE CHECKED AND TIME WINDOWED BUFR DUMP FILE
C
C   SUBPROGRAMS CALLED:
C     SYSTEM     - GETENV
C     LIBRARY:
C       W3NCO    - W3TAGB   W3TAGE   ERREXIT  W3UTCDAT W3MOVDAT
C       W3EMC    - ORDERS
C       BUFRLIB  - DATELEN  OPENBF   COPYMG   UFBTAB   OPENMB
C                  COPYSB   IREADMG  CLOSMG   CLOSBF   MESGBC
C                  IBFMS    GETBMISS
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C            > 0 - ABNORMAL TERMINATION
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  WCOSS
C
C$$$
      PROGRAM BUFR_DUPRAD
 
      PARAMETER (MXTS=8)

      REAL(8),ALLOCATABLE :: TAB_8(:,:)
      INTEGER,ALLOCATABLE :: IWORK(:)
      INTEGER,ALLOCATABLE :: IORD(:)
      INTEGER,ALLOCATABLE :: JDUP(:)

      CHARACTER*500 FILI,FILO
      CHARACTER*80  TSTR,TSTR2
      CHARACTER*45  TEXT7,TEXT8
      CHARACTER*8   SUBSET
      CHARACTER*3   DUMMY_MSGS

      DIMENSION     NDUP(0:3),IDAT(8),JDAT(8)

      REAL(8)       ADATE,BDATE,CDATE,DDATE,RDATE,UFBTAB_8
      REAL(8)       BMISS, GETBMISS

      LOGICAL       DUPES,DYNAMIC_INPUT

      DATA TSTR  /'CLAT CLON DAYS HOUR MINU SECO MGPT ANEL'/
      DATA TSTR2 /'CLAT CLON DAYS HOUR MINU SECO ANEL ANAZ'/

      DATA ADATE /00000000.00_8/
      DATA BDATE /99999999.00_8/
      DATA DEXY  /0/
      DATA DDAY  /0/
      DATA DOUR  /0/
      DATA DMIN  /0/
      DATA DSEC  /0/
      DATA DTB7  /0/
      DATA DTB8  /0/
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL W3TAGB('BUFR_DUPRAD',2013,0029,0062,'NP22')

      print *
      print * ,'---> Welcome to BUFR_DUPRAD - Version 01-29-2013'
      print *

      CALL DATELEN(10)

ccccc CALL OPENBF(0,'QUIET',2) ! Uncomment for extra print from bufrlib

C  ASSIGN DEFAULT VALUE FOR 'MISSING' TO LOCAL BMISS VARIABLE
C  ----------------------------------------------------------

      BMISS = GETBMISS()     ! assign default value for "missing"

C  SET THE COUNTERS TO INITIAL VALUES
C  ----------------------------------

      ISUB  = 0
      NDUP  = 0
      LUBFI = 20
      LUBFJ = 50

C  READ I/O FILENAMES AND ANY OVERRIDE VALUES FOR THINNING PARAMETERS
C  ------------------------------------------------------------------
C     DEFAULT PARAMETERS:
C     ADATE = 00000000.00  LOWER LIMIT FOR DATE/TIME
C     BDATE = 99999999.99  UPPER LIMIT FOR DATE/TIME
C     DEXY  = 0.0  TOLERANCE FOR LAT/LON CHECKS
C     DDAY  = 0.0  TOLERANCE FOR DAY CHECK
C     DOUR  = 0.0  TOLERANCE FOR HOUR CHECK
C     DMIN  = 0.0  TOLERANCE FOR MINUTE CHECK
C     DSEC  = 0.0  TOLERANCE FOR SECOND CHECK
C     DTB7  = 0.0  LEVEL 3/2.5: TOLERANCE FOR MESSAGE NUMBER CHECK
C                  LEVEL 2:     TOLERANCE FOR ANTENNA TILT (ELEVATION)
C                                ANGLE CHECK
C     DTB8  = 0.0  LEVEL 3/2.5: TOLERANCE FOR ANTENNA TILT (ELEVATION)
C                                ANGLE CHECK
C                  LEVEL 2:     TOLERANCE FOR ANTENNA AZIMUTH ANGLE
C                                CHECK
C  ------------------------------------------------------------------
 
      READ(5,'(Q,A)',END=900,ERR=900) NBYTES_FILI,FILI(1:NBYTES_FILI)
      READ(5,'(Q,A)',END=900,ERR=900) NBYTES_FILO,FILO(1:NBYTES_FILO)

cppppp
ccc   print *, 'file fili is ',nbytes_fili,' bytes long'
ccc   print *, 'file filo is ',nbytes_filo,' bytes long'
cppppp

      READ(5,*,END=1) ADATE,BDATE
      READ(5,*,END=1) DEXY,DDAY,DOUR,DMIN,DSEC,DTB7,DTB8
 
    1 CONTINUE

C  OPEN FILE TEMPORARILY TO SEE WHAT THE BUFR MESSAGE TYPE IS (SUBSET)
C  -------------------------------------------------------------------

      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')

      CALL OPENBF(LUBFI,'IN',LUBFI)
      IF(IREADMG(-LUBFI,SUBSET,IDATE).NE.0) THEN
         PRINT *, '#####BUFR_DUPRAD - NO DATA IN INPUT FILE - STOP'
         CALL W3TAGE('BUFR_DUPRAD')
         CALL ERREXIT(00)
      ENDIF
      CALL CLOSBF(LUBFI)
       
      IF(SUBSET(6:8).LE.'002')  THEN  ! Level 3 and 2.5 come here
         TEXT7='TOLERANCE FOR MESSAGE NO CHECK (NUMERIC) ... '
         TEXT8='TOLERANCE FOR TILT ANGLE CHECK (IN DEG) .... '
      ELSE                            ! Level 2 comes here
         TEXT7='TOLERANCE FOR TILT ANGLE CHECK (IN DEG.) ... '
         TEXT8='TOLERANCE FOR AZIMUTH ANGLE CHECK (IN DEG) . '
      ENDIF

      IF(BDATE.NE.99999999.00_8) THEN
         PRINT 200, ADATE,BDATE
      ELSE
         PRINT 201
      ENDIF
  200 FORMAT(/'REQUESTED EARLIEST DATE IS ....... ',F15.2/
     .        'REQUESTED LATEST   DATE IS ....... ',F15.2)
  201 FORMAT(/'@@@@ AS REQUESTED, NO TIME WINDOW TRIMMING IS PERFORMED'/
     .        '@@@@ ALL NON-DUPLICATES ARE RETAINED REGARDLESS OF TIME')
      PRINT 202, FILI(1:NBYTES_FILI),FILO(1:NBYTES_FILO),DEXY,DDAY,DOUR,
     . DMIN,DSEC,TEXT7,DTB7,TEXT8,DTB8
  202 FORMAT(/'UNCHECKED INPUT FILE IS         '/5X,A/
     .        'DUPLICATE CHECKED OUTPUT FILE IS'/5X,A//
     .        'BUFR_DUPRAD PARAMETERS:'/
     .        3X,'TOLERANCE FOR LAT/LON CHECKS (IN DEGREES) .. ',F5.1/
     .        3X,'TOLERANCE FOR DAY CHECK (IN DAYS) .......... ',F5.1/
     .        3X,'TOLERANCE FOR HOUR CHECK (IN HOURS) ........ ',F5.1/
     .        3X,'TOLERANCE FOR MINUTE CHECK (IN MINUTES) .... ',F5.1/
     .        3X,'TOLERANCE FOR SECOND CHECK (IN SECONDS) .... ',F5.1/
     .        3X,A45,F5.1/
     .        3X,A45,F5.1)

C  CHECK TO SEE IF INPUT FILE IS "DYNAMIC" (I.E., IT MAY BE GROWING
C   DURING THE COURSE OF THE RUNNING OF THIS PROGRAM
C  ----------------------------------------------------------------

      CALL W3UTCDAT(IDAT)
      CALL W3MOVDAT((/0.,-1.,0.,0.,0./),IDAT,JDAT)
      IDATE_NOW=IDAT(1)*1000000 + IDAT(2)*10000 + IDAT(3)*100 + IDAT(5)
      IDATE_LAST=JDAT(1)*1000000 + JDAT(2)*10000 + JDAT(3)*100 + JDAT(5)
      DYNAMIC_INPUT = (FILI(1:5).EQ.'/dcom'.AND.(IDATE_NOW.EQ.IDATE.OR.
     . IDATE_LAST.EQ.IDATE))

      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')

      CALL MESGBC(LUBFI,MSGT,ICOMP)
      IF(ICOMP.EQ.1) THEN
         PRINT'(/"INPUT BUFR FILE MESSAGES   C O M P R E S S E D"/'//
     .    '"FIRST MESSAGE TYPE FOUND IS",I5/)', MSGT
      ELSE  IF(ICOMP.EQ.0) THEN
         PRINT'(/"INPUT BUFR FILE MESSAGES   '//
     .    'U N C O M P R E S S E D"/"FIRST MESSAGE TYPE FOUND IS",I5/)',
     .    MSGT
      ELSE IF(ICOMP.EQ.-1)  THEN
         PRINT'(//"ERROR READING INPUT BUFR FILE - MESSAGE '//
     .    'COMPRESSION UNKNOWN"/)'
      ELSE  IF(ICOMP.EQ.-3)  THEN
         PRINT'(/"INPUT BUFR FILE DOES NOT EXIST"/)'
      ELSE  IF(ICOMP.EQ.-2)  THEN
         PRINT'(/"INPUT BUFR FILE HAS NO DATA MESSAGES"/"FIRST '//
     .    'MESSAGE TYPE FOUND IS",I5/)', MSGT
      ENDIF

      CALL CLOSBF(LUBFI)

C  COUNT THE NUMBER OF SUBSETS IN THE FILE TO ALLOCATE SPACE
C  ---------------------------------------------------------

      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
      CALL OPENBF(0,'QUIET',1) ! will generate diagnostic print if an
                               ! embedded BUFR table is read
      CALL UFBTAB(-LUBFI,UFBTAB_8,1,1,MXTB,' ')
      CALL OPENBF(0,'QUIET',0) ! return to default wrt degree of print

C  IF INPUT FILE WAS DETERMINED TO BE "DYNAMIC" (I.E., IT MAY BE
C   GROWING DURING THE COURSE OF THE RUNNING OF THIS PROGRAM), BUMP UP
C   ALLOCATABLE ARRAY SIZE TO 3 MILLION TO ENSURE THAT UFBTAB DOES NOT
C   OVERFLOW
C  -------------------------------------------------------------------

      IF(DYNAMIC_INPUT)  THEN
         IF(MXTB.LE.2500000) THEN
            MXTB_NEW=3000000
         ELSE
            MXTB_NEW=MXTB*1.20
         ENDIF
         PRINT'(/"~~> INPUT FILE MAY BE GROWING AS THIS PROGRAM RUNS,'//
     $    ' BUMP UP MXTB FROM ",I0," TO ",I0/)', MXTB,MXTB_NEW
         MXTB = MAX(MXTB,MXTB_NEW)
      ENDIF

      ALLOCATE(TAB_8(MXTS,MXTB),STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(IWORK(MXTB)     ,STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(IORD(MXTB)      ,STAT=I);IF(I.NE.0) GOTO 901
      ALLOCATE(JDUP(MXTB)      ,STAT=I);IF(I.NE.0) GOTO 901

      TAB_8 = BMISS
      JDUP  = 0
      IORD  = 0

C  MAKE A TABLE OUT OF THE DUP CHECK CRITERIA
C  ------------------------------------------

      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
      IF(SUBSET(6:8).LE.'002')  THEN  ! Level 3 and 2.5 come here
         CALL UFBTAB(LUBFI,TAB_8,MXTS,MXTB,NTAB,TSTR)
      ELSE                            ! Level 2 comes here
         CALL UFBTAB(LUBFI,TAB_8,MXTS,MXTB,NTAB,TSTR2)
      ENDIF


C  GET A SORTED INDEX OF REPORTS BY:
C    TILT, MESSAGE NUMBER, TIME, AND LON/LAT FOR LEVEL 3 AND 2.5
C    AZIMUTH, TILT, TIME, AND LON/LAT FOR LEVEL 2
C  -------------------------------------------------------------
 
      CALL ORDERS( 2,IWORK,TAB_8(8,1),IORD,NTAB,MXTS,8,2) ! tilt (3/2.5)
                                                          ! azimuth (2)
      CALL ORDERS(12,IWORK,TAB_8(7,1),IORD,NTAB,MXTS,8,2) ! msg# (3/2.5)
                                                          ! tilt (2)
      CALL ORDERS(12,IWORK,TAB_8(6,1),IORD,NTAB,MXTS,8,2) ! second
      CALL ORDERS(12,IWORK,TAB_8(5,1),IORD,NTAB,MXTS,8,2) ! minute
      CALL ORDERS(12,IWORK,TAB_8(4,1),IORD,NTAB,MXTS,8,2) ! hour
      CALL ORDERS(12,IWORK,TAB_8(3,1),IORD,NTAB,MXTS,8,2) ! day
      CALL ORDERS(12,IWORK,TAB_8(2,1),IORD,NTAB,MXTS,8,2) ! lon
      CALL ORDERS(12,IWORK,TAB_8(1,1),IORD,NTAB,MXTS,8,2) ! lat
 
C  GO THROUGH THE REPORTS IN ORDER, MARKING DUPLICATES
C  ---------------------------------------------------
 
      DO K=1,NTAB-1
         IREC = IORD(K  )
         JREC = IORD(K+1)
cppppp
ccc      print *, 'xxxxxxxxx'
ccc      print *, 'For REC ',irec,', TAB is: ',(tab_8(ii,irec),ii=1,8)
ccc      print *, 'For REC ',jrec,', TAB is: ',(tab_8(ii,jrec),ii=1,8)
ccc      print *, 'xxxxxxxxx'
cppppp

c Need to use the KIDNNT() intrinsic function here, with 8byte integer 
c output, in order to deal w/ the case when potentially large (ie, 
c greater than 10e7) "missing" values are encountered.

         DUPES = KIDNNT( DABS( TAB_8(1,IREC)-TAB_8(1,JREC) )*100._8 ) 
     .      .LE.   NINT( DEXY*100. ) 
     .     .AND. KIDNNT( DABS( TAB_8(2,IREC)-TAB_8(2,JREC) )*100._8 ) 
     .      .LE.   NINT( DEXY*100. )
     .     .AND. KIDNNT( DABS( TAB_8(3,IREC)-TAB_8(3,JREC) )*100._8 ) 
     .      .LE.   NINT( DDAY*100. )
     .     .AND. KIDNNT( DABS( TAB_8(4,IREC)-TAB_8(4,JREC) )*100._8 ) 
     .      .LE.   NINT( DOUR*100. )
     .     .AND. KIDNNT( DABS( TAB_8(5,IREC)-TAB_8(5,JREC) )*100._8 )
     .      .LE.   NINT( DMIN*100. )
     .     .AND. KIDNNT( DABS( TAB_8(6,IREC)-TAB_8(6,JREC) )*100._8 ) 
     .      .LE.   NINT( DSEC*100. )
     .     .AND. KIDNNT( DABS( TAB_8(7,IREC)-TAB_8(7,JREC) )*100._8 ) 
     .      .LE.   NINT( DTB7*100. )
     .     .AND. KIDNNT( DABS( TAB_8(8,IREC)-TAB_8(8,JREC) )*100._8 ) 
     .      .LE.   NINT( DTB8*100. )
         IF(DUPES) JDUP(IREC) = 1
cppppp
ccc      print *, 'dupes = ',dupes
cppppp
      ENDDO
 
C  TRIM THE EXCESS DATA FROM THE EXACT TIME WINDOW (IF REQUESTED)
C  --------------------------------------------------------------

      IF(BDATE.NE.99999999.00_8) THEN
         CDATE = MOD(ADATE,10000._8)
         DDATE = MOD(BDATE,10000._8)
         DO K=1,NTAB
            IF(IBFMS(TAB_8(6,K)).EQ.1) TAB_8(6,K) = 0     ! data missing
            RDATE =
     .       TAB_8(3,K)*1E2+TAB_8(4,K)+(TAB_8(5,K)*60+TAB_8(6,K))/3600.
            IF(CDATE.LE.DDATE) THEN
               IF(RDATE.LT.CDATE .OR.  RDATE.GT.DDATE) JDUP(K) = 2
            ELSE
               IF(RDATE.LT.CDATE .AND. RDATE.GT.DDATE) JDUP(K) = 2
            ENDIF
         ENDDO
      ENDIF
 
C  WRITE A DUP-CHECKED AND TIME-WINDOWED FILE
C  ------------------------------------------

      OPEN(LUBFI,FILE=FILI(1:NBYTES_FILI),FORM='UNFORMATTED')
      OPEN(LUBFJ,FILE=FILO(1:NBYTES_FILO),FORM='UNFORMATTED')
      CALL OPENBF(LUBFI,'IN ',LUBFI)
      CALL OPENBF(LUBFJ,'OUT',LUBFI)
 
      CALL GETENV('DUMMY_MSGS',DUMMY_MSGS)

C  If input file doesn't contain dummy center and dump time messages 1
C   and 2 (after table messages), before doing anything call closmg
C   with a negative unit number to signal routine that it should not
C   write out ANY messages with zero subsets in them - this holds for
C   all subsequent calls to closmg in this routine, even those done
C   through other bufrlib routines (and even for those calls where the
C   sign of the unit number is positive)
C  --------------------------------------------------------------------

      IF(DUMMY_MSGS.NE.'YES') CALL CLOSMG(-LUBFJ)

      DO WHILE(IREADMG(-LUBFI,SUBSET,IDATE).EQ.0)
         NSUBS = NMSUB(LUBFI)

C  If no subsets in msg & dummy msgs not expected loop to next input msg
C  ---------------------------------------------------------------------

         IF(NSUBS.LE.0.AND.DUMMY_MSGS.NE.'YES')  CYCLE

         DUPES = .FALSE.
         IF(NSUBS.GT.0)  THEN
            DO N=1,NSUBS
               if(isub+n.gt.ntab)  then
                  idup = 3
               else
                  IDUP = JDUP(ISUB+N)
               endif
               IF(IDUP.GT.0) DUPES = .TRUE.
            ENDDO
         ENDIF
         IF(DUPES) THEN
            CALL OPENMB(LUBFJ,SUBSET,IDATE)
            DO WHILE(IFBGET(LUBFI).EQ.0)
               ISUB = ISUB+1
               if(isub.gt.ntab)  then
                  idup = 3
               else
                  IDUP = JDUP(ISUB)
               endif
               IF(IDUP.EQ.0 ) THEN
                  CALL COPYSB(LUBFI,LUBFJ,IRET)
                  NDUP(IDUP) = NDUP(IDUP)+1
               ELSE
                  CALL COPYSB(LUBFI,00000,IRET)
                  NDUP(IDUP) = NDUP(IDUP)+1
               ENDIF
            ENDDO
         ELSE
            IF(NSUBS.GT.0) THEN
               DO N=1,NSUBS
                  IDUP = JDUP(ISUB+N)
                  NDUP(IDUP) = NDUP(IDUP)+1
               ENDDO
            ENDIF

C  In the event that the input file contains dummy center and dump time
C    messages 1 and 2 (after table messages), call closmg with a
C    positive unit number to signal routine that it should write out
C    these messages even though they have zero subsets in them
C  If the input file does not contain dummy messages, a positive unit
C    number here is immaterial because closmg was already called with
C    a negative unit number immediately after the output file was
C    opened (and this holds for all subsequent calls to closmg
C    regardless of the sign of the unit number)
C  -------------------------------------------------------------------

            CALL CLOSMG(LUBFJ)
            CALL COPYMG(LUBFI,LUBFJ)
            ISUB = ISUB+NSUBS
         ENDIF
      ENDDO
 
      CALL CLOSBF(LUBFI)
      CALL CLOSBF(LUBFJ)
      OPEN(LUBFJ,FILE=FILO(1:NBYTES_FILO),FORM='UNFORMATTED')
      CALL MESGBC(LUBFJ,MSGT,ICOMP)
      IF(ICOMP.EQ.1) THEN
         PRINT'(/"OUTPUT BUFR FILE MESSAGES   C O M P R E S S E D"/'//
     .    '"FIRST MESSAGE TYPE FOUND IS",I5/)', MSGT
      ELSE  IF(ICOMP.EQ.0) THEN
         PRINT'(/"OUTPUT BUFR FILE MESSAGES   '//
     .    'U N C O M P R E S S E D"/"FIRST MESSAGE TYPE FOUND IS",I5/)',
     .    MSGT
      ELSE IF(ICOMP.EQ.-1)  THEN
         PRINT'(//"ERROR READING OUTPUT BUFR FILE - MESSAGE '//
     .        'COMPRESSION UNKNOWN"/)'
      ELSE  IF(ICOMP.EQ.-3)  THEN
         PRINT'(/"OUTPUT BUFR FILE DOES NOT EXIST"/)'
      ELSE  IF(ICOMP.EQ.-2)  THEN
         PRINT'(/"OUTPUT BUFR FILE HAS NO DATA MESSAGES"/"FIRST '//
     .    'MESSAGE TYPE FOUND IS",I5/)', MSGT
      ENDIF
      CLOSE(LUBFJ)
 
C  GENERATE REPORT
C  ---------------
 
      PRINT 300, ISUB,NDUP(3),MXTB,NTAB,NDUP(0),NDUP(1),NDUP(2)
  300 FORMAT(/'BUFR_DUPRAD READ IN A TOTAL OF',I12,' REPORTS'/
     .        ' A TOTAL OF ',I12,' REPORTS WERE SKIPPED DUE TO BEING ',
     .        'OVER THE LIMIT OF ',I12/
     .        'BUFR_DUPRAD CHECKED A TOTAL OF',I12,' REPORTS'//
     .        'NUMBER OF REPORTS WRITTEN OUT ..................',I9/
     .        'NUMBER OF REPORTS SKIPPED DUE TO:'/
     .        '   FAILING DUPLICATE CHECK .....................',I9/
     .        '   BEING OUTSIDE TIME WINDOW FOR TRIMMING ......',I9/)

C  END OF PROGRAM
C  --------------

      CALL W3TAGE('BUFR_DUPRAD')
      STOP

C  ERROR EXITS
C  -----------

  900 CONTINUE

      PRINT *, '#####BUFR_DUPRAD - EOF/ERR READING STDIN'
      CALL W3TAGE('BUFR_DUPRAD')
      CALL ERREXIT(99)

  901 CONTINUE

      PRINT *, '#####BUFR_DUPRAD - UNABLE TO ALLOCATE ARRAYS'
      CALL W3TAGE('BUFR_DUPRAD')
      CALL ERREXIT(99)

      END
