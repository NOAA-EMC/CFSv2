      PROGRAM tocgrib2_cfs
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: tocgrib2_cfs
C   PRGMMR: VUONG              ORG: EMC               DATE: 2015-03-25
C
C ABSTRACT: Program reads selected GRIB2 fields from a file, adds a TOC
C           Flag Field separator block and WMO Header in front of each GRIB2
C           field, and writes them out to a new file.  The output file
C           is in the format required for TOC's FTP Input Service, which
C           can be used to disseminate the GRIB2 bulletins.
C           This service is described at http://weather.gov/tg/ftpingest.html.
C
C PROGRAM HISTORY LOG:
C 2015-03-25  VUONG     This is a special version of tocgrib2_cfs for CFS v2
C                       to add WMO header for 3 fields:PRATE, TMIN and TMAX for 
C                       variable length of forecast hours at every 6-hour interval 
C
C USAGE:
C   INPUT FILES:
C      5       - NAMELIST OF GRIB FIELDS AND ASSOCIATED WMO HEADERS.
C     11       - INPUT GRIB2 FILE.
C     31       - CORRESPONDING INPUT GRIB2 INDEX FILE.
C
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)
C      6       - STANDARD FORTRAN PRINT FILE
C     51       - OUTPUT GRIB BULLETIN FILE IN TOC FORMAT
C
C   SUBPROGRAMS CALLED: (LIST ALL CALLED FROM ANYWHERE IN CODES)
C     UNIQUE:    - MAKWMO
C     LIBRARY:
C       W3LIB    - W3TAGB W3TAGE mkfldsep makwmo
C       G2LIB    - GETGB2P
C       BACIO    - BAREAD BAOPENR BAOPENW BACLOSE WRYTE
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C             10 - Error opening input GRIB2 data file 
C             20 - Error opening output GRIB transmission file 
C             19 - ERROR READING CONTROL CARD FILE - All Bulletins missing
C             30 - Some BULLETINS ARE MISSING
C
C REMARKS:  The "EXTRACT" variable in the namelist allows users to choose
C           whether they want the entire GRIB2 message containing the 
C           requested field (extract=.false.), OR a GRIB2 message containing
C           only the requested field (extract=.true.).  Both options return the
C           same message if the requested field is the only field in the GRIB2
C           message.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C$$$
      use grib_mod
      use pdstemplates
      use gridtemplates
C
      integer,dimension(200) :: IDS,GDT,PDT
      integer   ::    DSCPL,GDTN,PDTN
      integer   ::    nbul,nrec,mbul,dayofmonth,hourofday
      integer,parameter :: lenhead=21,jrew=0

C
      CHARACTER * 6   BULHED
      CHARACTER * 80  DESC,WMOHEAD
      CHARACTER * 160  fileb,filei,fileo
      CHARACTER * 6   envvar
      CHARACTER * 4   KWBX
      CHARACTER * 1   CSEP(80)
      CHARACTER * 1   WMOHDR(lenhead)
      character(len=1),pointer,dimension(:) :: gribm

      logical :: extract=.false.

      interface
        SUBROUTINE GETGB2P(LUGB,LUGI,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,
     &                       EXTRACT,K,GRIBM,LENG,IRET)
           INTEGER,INTENT(IN) :: LUGB,LUGI,J,JDISC,JPDTN,JGDTN
           INTEGER,DIMENSION(:) :: JIDS(*),JPDT(*),JGDT(*)
           LOGICAL,INTENT(IN) :: EXTRACT
           INTEGER,INTENT(OUT) :: K,IRET
           CHARACTER(LEN=1),POINTER,DIMENSION(:) :: GRIBM
        END SUBROUTINE GETGB2P
      end interface
C
      NAMELIST /GRIBIDS/DSCPL,IDS,GDTN,GDT,PDTN,PDT,DESC,WMOHEAD,EXTRACT

      CALL W3TAGB('tocgrib2_cfs',2015,0069,0069,'EMC')                  

      lugb=11      ! Input GRIB2 File
      lugi=31      ! Input GRIB2 INdex File
      lugo=51      ! Output transmission file.
C
C        Read GRIB2 data and index file names from the FORT_nn
C        environment variables, and open the files.
C
      envvar='FORT  '
      write(envvar(5:6),fmt='(I2)') lugb
      call getenv(envvar,fileb)
      write(envvar(5:6),fmt='(I2)') lugi
      call getenv(envvar,filei)

      call baopenr(lugb,fileb,iret1)
      if ( iret1  .ne. 0 ) then
         write(6,fmt='(" Error opening GRIB file: ",A160)') fileb
         write(6,fmt='(" baopenr error = ",I5)') iret1
         stop 10
      endif
C
C         Open GRIB2 index file.  If doesn't open, use just the data
C         file.
C
      call baopenr(lugi,filei,iret2)
      if ( iret2  .ne. 0 ) then
         lugi=0
      endif

C
C        Read output GRIB bulletin file name from  FORTnn
C        environment variable, and open file.
C
      write(envvar(5:6),fmt='(I2)') lugo
      call getenv(envvar,fileo)
      call baopenw(lugo,fileo,iret1)
      if ( iret1  .ne. 0 ) then
         write(6,fmt='(" Error opening output transmission file: ",
     &                 A160)') fileo
         write(6,fmt='(" baopenw error = ",I5)') iret1
         stop 20
      endif
      !
      !  Set Namelist defaults
      !
      DSCPL=-1     ! Grib2 Discipline number
      IDS=-9999    ! GRIB2 Identification Section
      GDTN=-1      ! Grid Definition Template Number
      GDT=-9999    ! Grid Definition Template
      PDTN=-1      ! Product Definition Template Number
      PDT=-9999    ! Product Definition Template
      WMOHEAD='TTAAnn CCCC'
      EXTRACT=.false.
C
C     Read input control records.
C
      READ (*,GRIBIDS,iostat=ios)
      if ( ios .ne. 0 ) then
          write(6,fmt='(" Error reading PDS from input file. iostat = "
     &           ,i5)') ios
      endif
C
C    Set CFS time sreies start at 6-hour
C
      pdt(9) = 6
      iret=0
      nbul=0
      nrec = 0

      DO
C
C        Read and return packed GRIB field
C
         LUGI=0             !  Do not need index file

         CALL GETGB2P(lugb,lugi,jrew,DSCPL,IDS,PDTN,PDT,
     &               GDTN,GDT,extract,KREW,gribm,itot,iret)
         IF (IRET.NE.0) THEN
            IF (IRET.EQ.96)WRITE(6,'(A)')' GETGB2P: ERROR READING INDEX'
     &         //' FILE'
            IF (IRET.EQ.97)WRITE(6,'(A)')' GETGB2P: ERROR READING GRIB'
     &         //' FILE'
c           IF (IRET.EQ.99)WRITE(6,'(A)')' GETGB2P: ERROR REQUEST NOT'
c    &          //' FOUND'
            
            IF (IRET.EQ.99) THEN 
                go to 999
            END IF
            cycle
         END IF
         nrec = nrec + 1
         !  Echo input record
         !
         WRITE(6,FMT='(/,''***********************************'',
     &      ''********************************************'')')
         write(6,'(A,I0)') ' Start new record no. =  ',nrec
         write(6,'(73A)') ' DESC=',DESC(1:73)
         write(6,'(11A)') ' WMOHEAD=',WMOHEAD(1:11)
         write(6,'(A,I0)') ' GRIB2 DISCIPLINE= ',DSCPL
         write(6,'(A,20(1x,I0))')' Section 1=',
     &            (IDS(J),j=1,13)
         if ( GDTN .ne. -1 ) then
            write(6,'(A,I0,A,100(1x,I0))') ' GDT 3. ',GDTN,' =',
     &            (GDT(j),j=1,getgdtlen(GDTN))
         endif
         if ( PDTN .ne. -1 ) then
            write(6,'(A,I0,A,100(1x,I0))') ' PDT 4. ',PDTN,' =',
     &            (PDT(j),j=1,getpdtlen(PDTN))
         endif
C
         WRITE (6,'(A,1x,I0)')' RECORD NO. OF GRIB RECORD IN INPUT '
     &        //'FILE = ', KREW
C
         WRITE (6,'(A,I0)')' Size of GRIB Field = ',itot
C
C        MAKE Flag Field Separator block
C
         iopt=2
         insize=19
         call mkfldsep(csep,iopt,insize,itot+lenhead,lenout)
C         WRITE(6,'(A,80A)')'  csep = ',csep
C
C        MAKE WMO HEADER
C
         dayofmonth=mova2i(gribm(16+16))
         hourofday=mova2i(gribm(16+17))
         CALL MAKWMO (WMOHEAD(1:6),dayofmonth,hourofday,
     &                WMOHEAD(8:11),WMOHDR)
C         WRITE(6,'(21A)') '  WMOHEADER= ',WMOHDR
C
C        write out Separator block, Abbreviated WMO Heading,
C        and GRIB2 field to output file.
C
         call wryte(lugo,lenout,csep)
         call wryte(lugo,lenhead,WMOHDR)
         call wryte(lugo,itot,gribm)
         nbul=nbul+1
         if (associated(gribm)) then
            deallocate(gribm)
            nullify(gribm)
         endif
C    Increment forecast hour at 6-hour interval
         pdt(9) = pdt(9) + 6

      enddo
C
C     CLOSING SECTION
C
 999  if (nbul .EQ. 0 ) then
        WRITE (6,FMT='('' SOMETHING WRONG WITH DATA CARDS...'',
     &         ''NOTHING WAS PROCESSED'')')
        WRITE (6,fmt='(" CHECK GRIB FILE: ",A160)') fileb
c        CALL W3TAGE('tocgrib2_cfs')                                       
        stop 19 
      else
        call baclose (LUGB,iret)
        call baclose (LUGI,iret)
        call baclose (LUGO,iret)
        WRITE (6,FMT='(//,'' ******** RECAP OF THIS EXECUTION '',
     &    ''********'',/,5X,''READ  '',I6,'' INDIVIDUAL IDS'',
     &    /,5X,''WROTE '',I6,'' BULLETINS OUT FOR TRANSMISSION'',
     &    //)') nrec, NBUL
      endif
C
C         TEST TO SEE IF ANY BULLETINS MISSING
C
      mbul = nrec - nbul
      IF (mbul .ne. 0) THEN
        WRITE(6,'(A,1X,I0)')' BULLETINS MISSING = ',mbul
c        CALL W3TAGE('tocgrib2_cfs')                                     
        stop 30
      END IF
C
c      CALL W3TAGE('tocgrib2_cfs')                                       
      STOP
      END
