C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      PROGRAM DUPREP
 
      PARAMETER (MXTS=6,MXTB=1500000)
 
      COMMON /QUIET/ IPRT
 
      CHARACTER*255 FILI,FILO
      CHARACTER*80  TSTR,RSTR
      CHARACTER*8   CTAB(MXTS,MXTB),SUBSET
      REAL(8)       ADATE,BDATE,BMISS
      REAL(8)       TAB_8(MXTS,MXTB),RAB_8(MXTS,MXTB)
      DIMENSION     JDUP(MXTB)
      DIMENSION     NDUP(0:4),IWORK(MXTB),IORD(MXTB)
      EQUIVALENCE   (TAB_8(1,1),CTAB(1,1))
      LOGICAL       DUPES
 
      DATA TSTR  /'YOB  XOB  DHR  ELV  TYP      '/
      DATA ADATE /00000000.00/
      DATA BDATE /99999999.00/
      DATA BMISS /10E10/
      DATA LUBFI /20/
      DATA LUBFJ /50/
      DATA DEXY  /0/
      DATA DOUR  /0/
      DATA DELV  /0/
      DATA ISUB  /0/
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
      bmiss=10e10; call setbmiss(bmiss) ! this sets bufrlib missing value to 10e10

      call maxout(20000) ! to match previous processing

C  READ I/O FILENAMES AND ANY OVERRIDE VALUES FOR THINNING PARAMETERS
C  ------------------------------------------------------------------
C     DEFAULT PARAMETERS:
C     DEXY = 0.0  TOLERANCE FOR LAT/LON CHECKS
C     DOUR = 0.0  TOLERANCE FOR HOUR CHECK
C     DELV = 0.0  TOLERANCE FOR ELEVATION CHECK
C  ------------------------------------------------------------------
 
      READ(5,'(A)',END=900,ERR=900) FILI
      READ(5,'(A)',END=900,ERR=900) FILO
      READ(5,*,END=1) DEXY,DOUR,DELV
    1 CONTINUE
 
      PRINT *,'REQUESTED EARLIEST DATE IS ... ',ADATE
      PRINT *,'REQUESTED LATEST   DATE IS ... ',BDATE
      PRINT *
      PRINT *,'UNCHECKED AND UNCORRECTED INPUT FILE IS'
      PRINT *,'     ',TRIM(FILI)   
      PRINT *,'DUPLICATE CHECKED AND CORRECTED OUTPUT FILE IS'
      PRINT *,'     ',TRIM(FILO)  
      PRINT *
      PRINT *,'DUPCOR PARAMETERS:'
      PRINT *,'TOLERANCE FOR LAT/LON CHECKS (IN DEGREES) .. ',DEXY
      PRINT *,'TOLERANCE FOR HOUR CHECK (IN HOURS) ........ ',DOUR
      PRINT *,'TOLERANCE FOR ELEVATION  (IN METERS) ....... ',DELV
      PRINT *
 
C  MAKE A TABLE OUT OF THE LATS, LONS, AND TIME COORDINATES
C  --------------------------------------------------------
 
      OPEN(LUBFI,FILE=FILI,FORM='UNFORMATTED')
      CALL UFBMEM(LUBFI,0,IRET,IUNIT)
      CALL MAXOUT(20000)
 
C  READ THE INFORMATION FOR DUP CHECKING AND THINNING
C  --------------------------------------------------
 
      CALL UFBTAM(TAB_8,MXTS,MXTB,NTAB,TSTR)
 
C  INITIAL VALUES FOR MARKERS AND COUNTERS AND CORRECTION INDICATORS
C  -----------------------------------------------------------------
 
      JDUP = 0
      NDUP = 0
 
C  GET A SORTED INDEX OF THE REPORTS BY RECEIPT, OB TIME, AND LON/LAT
C  ------------------------------------------------------------------
 
      CALL ORDERS( 2,IWORK,TAB_8(5,1),IORD,NTAB,MXTS,8,2)
      CALL ORDERS(12,IWORK,TAB_8(4,1),IORD,NTAB,MXTS,8,2)
      CALL ORDERS(12,IWORK,TAB_8(3,1),IORD,NTAB,MXTS,8,2)
      CALL ORDERS(12,IWORK,TAB_8(2,1),IORD,NTAB,MXTS,8,2)
      CALL ORDERS(12,IWORK,TAB_8(1,1),IORD,NTAB,MXTS,8,2)
 
C  GO THROUGH THE REPORTS IN ORDER, MARKING DUPLICATES AND CORRECTIONS
C  -------------------------------------------------------------------
 
      DO 2 K=1,NTAB-1
      I = IORD(K)
      DO KK=K+1,MIN(K+200,NTAB)
      J = IORD(KK)
      IF(NINT(ABS(TAB_8(1,I)-TAB_8(1,J))*100.).LT.NINT(DEXY*100.)) THEN
        DUPES = NINT(ABS(TAB_8(1,I)-TAB_8(1,J))*100.).LE.NINT(DEXY*100.)
     . .AND.    NINT(ABS(TAB_8(2,I)-TAB_8(2,J))*100.).LE.NINT(DEXY*100.)
     . .AND.    NINT(ABS(TAB_8(3,I)-TAB_8(3,J))*100.).LE.NINT(DOUR*100.)
     . .AND.    NINT(ABS(TAB_8(4,I)-TAB_8(4,J))*100.).LE.NINT(DELV*100.)
     . .AND.    NINT(ABS(TAB_8(5,I)-TAB_8(5,J))*100.).LE.0000
        !IF(DUPES) print'(5f10.2)',(tab_8(l,i),l=1,5)
        !IF(DUPES) print'(5f10.2)',(tab_8(l,j),l=1,5)
        !IF(DUPES) print*
        IF(DUPES) JDUP(I) = 2
        IF(DUPES) GOTO 2
      ENDIF
      ENDDO
2     ENDDO
 
C  WRITE A DUP-CHECKED FILE
C  ------------------------
 
      CALL CLOSBF(LUBFJ)
      OPEN(LUBFJ,FILE=FILO,FORM='UNFORMATTED')
      CALL OPENBF(LUBFJ,'OUT',LUBFI)
      call maxout(20000)
      IREC = 1
 
      DO WHILE(IREADMM(IREC,SUBSET,IDATE).EQ.0)
      NSUBS = NMSUB(LUBFI)
      DUPES = .FALSE.
 
      DO N=1,NSUBS
      IDUP = JDUP(ISUB+N)
      IF(IDUP.GT.1) DUPES = .TRUE.
      ENDDO
      IF(.NOT.DUPES) THEN
         DO N=1,NSUBS
         IDUP = JDUP(ISUB+N)
         NDUP(IDUP) = NDUP(IDUP)+1
         ENDDO
      ENDIF
 
      IF(DUPES) THEN
         CALL OPENMB(LUBFJ,SUBSET,IDATE)
         DO WHILE(IFBGET(LUBFI).EQ.0)
         ISUB = ISUB+1
         IDUP = JDUP(ISUB)
         IF(IDUP.LE.1) THEN
            CALL COPYSB(LUBFI,LUBFJ,IRET)
            NDUP(IDUP) = NDUP(IDUP)+1
         ELSE
            !print'(5f10.2)',(tab_8(i,isub),i=1,5)
            CALL COPYSB(LUBFI,00000,IRET)
            NDUP(IDUP) = NDUP(IDUP)+1
         ENDIF
         ENDDO
      ELSE
         CALL CLOSMG(LUBFJ)
         CALL COPYMG(LUBFI,LUBFJ)
         ISUB = ISUB+NSUBS
      ENDIF
 
      ENDDO
 
      CALL CLOSBF(LUBFI)
      CALL CLOSBF(LUBFJ)
 
C  GENERATE REPORT
C  ---------------
 
  100 CONTINUE
      PRINT *,'UNIQUE    REPORTS= ',NDUP(0)
      PRINT *,'CORRECTED REPORTS= ',NDUP(1)
      PRINT *,'DUPLICATE REMOVED= ',NDUP(2)
      PRINT *,'DUPLICATE NOT RMV= ',NDUP(3)
      PRINT *,'UNWINDOWD REPORTS= ',NDUP(4)
      PRINT *
      PRINT *,'DUPCOR PROCESSED ',NTAB,' REPORTS '
      PRINT *
 
      STOP
 
C  ERROR EXITS
C  -----------
 
  900 CONTINUE
      CALL BORT('DUPUPA - EOF ON NAMELIST   ')
      END
