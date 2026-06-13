C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: GRIB_SNOWGRIB
C   PRGMMR: PAN              ORG: NP23        DATE: 2000-01-27
C
C ABSTRACT: This program creates a global 0.5-degree analysis of
C   snow cover and liquid equivalent snow depth from 16th 
C   mesh (23-km) airforce depth data and NH IMS 16th mesh snow cover
C   data. The procedure is:
C    1) Interpolate the ims and airforce data to the 0.5-degree
C       grid using the ipolates library.
C    2) Convert airforce physical depth to liquid equivalent 
C       depth using a 10:1 ratio.
C    3) In the SH, where there is no ims data, the snow cover
C       is set to 0/100% where the airforce data indicates no snow/snow.
C       The depth is set to the airforce value.
C    4) In the NH, the snow cover is determined by the ims data.
C       Where ims indicates snow cover and the airforce depth 
C       indicates bare ground, the depth is set to 2.5 mm liquid
C       equivalent.  Where ims indicates bare ground, the 
C       depth is set to zero.  Otherwise, the depth is set
C       to the airforce value.  The airforce depth is qc'd by 
C       the ims data because the latter has more accurate
C       coverage.
C
C PROGRAM HISTORY LOG:
C   98-01-30  Hua-Lu Pan
C 2014-11-17  Gayno - Ingest 16th mesh grib1 version of airforce data.
C                     Previously, the 8th mesh binary data was used.
C                     Ingest grib1 or grib2 version of IMS data.
C
C USAGE:
C   INPUT FILES:
C     fort.11  - Northern hemisphere airforce snow depth file (grib1)
C     fort.12  - Southern hemisphere airforce snow depth file (grib1)
C     fort.13  - Northern hemisphere IMS snow cover file (grib1 or grib2)
C
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)
C     fort.51  - 0.5-deg global GRIB snow file before qc (grib1)
C                snow cover and liq equivalent depth in mm.
C     fort.52  - 0.5-deg global GRIB snow file after qc  (grib1)
C                snow cover and liq equivalent depth in mm.
C     fort.6   - print output
C
C   EXIT STATES:  non-0 is FATAL
C     COND =    0 - SUCCESSFUL RUN
C               1 - error opening airforce file
C               2 - error degribbing airforce file header
C               3 - error degribbing airforce file record
C               4 - error in ipolates interpolating airforce data
C               5 - incorrect number of points returned from
C                   ipolates during interpolation of airforce data
C               6 - error opening ims file
C               7 - error degribbing ims cover record
C               8 - incorrect number of points returned from
C                   ipolates during interpolation of ims data
C               9 - error in routine makgds
C              10 - error opening fort.51
C              11 - error writing fort.51 depth record
C              12 - error writing fort.51 cover record
C              13 - error opening fort.52
C              14 - error writing fort.52 depth record
C              15 - error writing fort.52 cover record
C              19 - error in ipolates interpolating ims data
C              26 - ims snow file is not grib1 or grib2
C              40 - error opening file in routine grib_check
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN77
C   MACHINE:  WCOSS
C
C$$$
      program grib_snowgrib
      implicit none
      integer, parameter :: io=720  ! dimensions of 0.5-degree
      integer, parameter :: jo=361  ! output grid.
      integer, parameter :: ijo=io*jo
      integer, parameter :: jn=1024 ! i/j dimension of ims grid
      character :: gdsi(400)
      integer :: kgdsn(200), kgdss(200)
      integer :: kgdso(200), kpdso(200)
      integer :: i, iret, lun
      integer :: jyr, jmo, jdy, jcen, lengds
      logical*1 :: lo(io,jo)
      real :: snow1(io,jo)
      real :: snow2(io,jo)
      real :: snow3(io,jo)
      data kgdsn/5,2*1024,-20826,145000,8,100000,2*23813,0,
     &           9*0,255,180*0/
      data kgdss/5,2*1024, 20826,-125000,8,100000,2*23813,128,
     &           9*0,255,180*0/
      data (kpdso(i),i=1,25)/47,77,4,8,65,1,0,5*0,1,2*0,1,
     &      2*0,2,0,20,1,3*0/
      data (kpdso(i),i=26,200)/175*-1/

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      CALL W3TAGB('GRIB_SNOWGRIB',2000,0027,0070,'NP23')                  
      call makgds(4,kgdso,gdsi,lengds,iret)
      print *, ' iret from makgds =', iret
      if (iret /= 0) then
        print *, ' Fatal error in makgds'
        call errexit(9)
      endif

      print *, ' process gb airforce data'
      lun = 12
      call snowget(lun,kgdss,kgdso,io,jo,lo,snow2)

      lun = 13
      print *, ' process ims data'
      call imssnw(lun,kgdso,jn,ijo,lo,snow3,jcen,jyr,jmo,jdy)
      kpdso(8) = jyr
      kpdso(9) = jmo
      kpdso(10) = jdy
      kpdso(21) = jcen

      lo=.false.; where(snow2 > 0.0)  lo = .true.

! write un-QC'd snow data to unit 51      

      print *, ' open fort.51'
      call baopenw(51,'fort.51',iret)
      if (iret /= 0) then
        print*, ' Fatal error opening fort.51'
        call errexit(10)
      endif

      print *, ' write snow depth'
      call putgb(51,ijo,kpdso,kgdso,lo,snow2,iret)
      if (iret /= 0) then
        print*, ' Fatal error writing fort.51'
        call errexit(11)
      endif
      print *, ' write snow cover'
      kpdso(5) = 238
      call putgb(51,ijo,kpdso,kgdso,lo,snow3,iret)
      if (iret /= 0) then
        print*, ' Fatal error writing fort.51'
        call errexit(12)
      endif

! blend and QC the snow data from AFWA and IMS snow files

      print *, ' qc data'
      call qcsnow(ijo,snow2,snow3)

! write QC'd snow data to unit 52      

      print *, ' open fort.52'
      kpdso(5) = 65
      call baopenw(52,'fort.52',iret)
      if (iret /= 0) then
        print*, ' Fatal error opening fort.52'
        call errexit(13)
      endif

      print *, ' write snow depth'
      call putgb(52,ijo,kpdso,kgdso,lo,snow2,iret)
      if (iret /= 0) then
        print*, ' Fatal error writing fort.52'
        call errexit(14)
      endif
      kpdso(5) = 238
      print *, ' write snow cover'
      call putgb(52,ijo,kpdso,kgdso,lo,snow3,iret)
      if (iret /= 0) then
        print*, ' Fatal error writing fort.52'
        call errexit(15)
      endif

      print*, ' *** NORMAL TERMINATION ***'
      CALL W3TAGE('GRIB_SNOWGRIB')                                           
      end
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    snowget     process airforce daily 23-km snow 
C   PRGMMR: HUA-LU PAN       ORG: W/NMC23    DATE: 98-01-30
C
C ABSTRACT: This routine reads in the nh and sh 23-km air force snow
C   depth analyses (1/16 mesh resolutiion) and interpolates
C   it using an area averaging method to a 0.5-deg global grid.
C   It is called separately for each hemisphere.
C
C PROGRAM HISTORY LOG:
C   98-01-30  Hua-lu Pan
C 2014-11-17  Gayno     Read 16th mesh grib 1 version of data.
C                       Previously, the 8th mesh binary data
C                       was used.
C
C USAGE:    call snowget(lun,jgds,kgdso,io,jo,snowg)
C   INPUT ARGUMENT LIST:
C     lun      - fortran unit number of airforce file
C     jgds     - grib 1 gds array of airforce grid.
C     kgdso    - grib 1 gds array of 0.5-deg grid.
C     i/jo     - i/j dimensions of 0.5-deg grid.
C
C   OUTPUT ARGUMENT LIST:    
C     snowg    - snow depth on the 0.5-deg global grid
C
C REMARKS: None
C
C ATTRIBUTES:
C   LANGUAGE: Fortran 77
C   MACHINE:  WCOSS
C
C$$$
      subroutine snowget(lun,jgdss,kgdso,io,jo,lo,snowg)
      implicit none
c
      integer, intent(in   )      :: lun, jgdss(200), kgdso(200), io, jo
      real,    intent(  out)      :: snowg(io*jo)
c
      character(len=7)            :: fngrib
      integer                     :: iret, lugi
      integer                     :: i,ijafwa
      integer                     :: ibi, ibo, ipopt(20)
      integer                     :: jpds(200), jgds(200)
      integer                     :: kpds(200), kgds(200)
      integer                     :: lskip, numbytes
      integer                     :: numpts, message_num, no
      logical*1, allocatable      :: bitmap(:)
      logical*1                   :: lo(io*jo)
      real, allocatable           :: snowa(:)
      real                        :: rlat(io*jo), rlon(io*jo)

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      if (lun==11) fngrib = "fort.11"
      if (lun==12) fngrib = "fort.12"

      call baopenr(lun,fngrib,iret)
      if (iret /= 0) then
        print*,' Fatal error. bad open, iret is ', iret
        call errexit(1)
      endif

      lugi     = 0
      lskip    = -1
      jpds     = -1
      jpds(5)  = 66     ! snow depth
      jgds     = -1
      call getgbh(lun, lugi, lskip, jpds, jgds, numbytes, 
     &            numpts, message_num, kpds, kgds, iret)
      if (iret /= 0) then
        print*,' Fatal error. Bad degrib of header, iret is ', iret
        call errexit(2)
      endif

      print *, ' iyr, imo, idy =', kpds(8:10)

      ijafwa = kgds(2) * kgds(3)
      allocate(bitmap(ijafwa))
      allocate(snowa(ijafwa))

      call getgb(lun,lugi,ijafwa,lskip,jpds,jgds,numpts,lskip,
     &           kpds,kgdso,bitmap,snowg,iret)
      if (iret /= 0) then
        print*,' Fatal error. bad degrib of data, iret is ', iret
        call errexit(3)
      endif

      print *, ' smax, smin =', maxval(snowg), minval(snowg)

      snowg = snowg * 100.0 ! convert depth to weight @ 100kg/m**3

      !kgds(7) = -80000  ! orientation angle adjustment for iplib routines

      call qc_snow_data(kgds,snowg,kgds(2),kgds(3))

      deallocate(bitmap,snowa)
      call baclose(lun,iret)
      end subroutine snowget
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    imssnw     process nh ims snow cover data
C   PRGMMR: HUA-LU PAN       ORG: W/NMC23    DATE: 98-01-30
C
C ABSTRACT: This routine reads in 23 km nh ims snow cover data (or
C   1/16 mesh resolution) and interpolates it using area averaging
C   to a 0.5-deg global grid.
C
C PROGRAM HISTORY LOG:
C   98-01-30  Hua-lu Pan
C
C USAGE:    call imssnw(lun,kgdso,jn,ijo,lo,snow3,icen,iyr,imo,idy)
C   INPUT ARGUMENT LIST:
C     lun      - fortran unit number of ims file
C     kgdso    - grib 1 gds array of 0.5-deg grid.
C     jn       - i/j dimension of ims grid
C     ijo      - number of grid pnts, 0.5-deg grid
C
C   OUTPUT ARGUMENT LIST:    
C     snow3    - snow cover on the 0.5-deg global grid
C     lo       - bitmap on the 0.5-deg global grid
C     icen     - century of ims data
C     iyr      - year of century of ims data
C     imo      - month of ims data
C     idy      - day of ims data
C
C REMARKS: None
C
C ATTRIBUTES:
C   LANGUAGE: Fortran 77
C   MACHINE:  WCOSS
C
C$$$
      subroutine imssnw(lun,kgdso,jn,ijo,lo,snow3,icen,iyr,imo,idy)
      implicit none
      integer, intent(in   ) :: lun, kgdso(200), jn, ijo
      integer, intent(  out) :: icen, iyr, imo, idy
      real, intent(  out)    :: snow3(ijo)
      character*7 :: fngrib
      integer :: kgds(200), kpds(200), ipopt(20)
      integer :: i, ibi(1), ibo(1), j, ji, jpds(200), jgds(200), lskip
      integer :: lugb, lugi, mdata, ndata, no, iret
      integer :: isgrib, jj, jdisc, jpdtn, jgdtn, k
      integer :: jids(200), jgdt(200), jpdt(200)
      logical*1 :: lo(ijo), li(jn,jn), lmsk(jn,jn)
      logical   :: unpack
      real :: work(jn,jn)
      real :: rlat(ijo), rlon(ijo), smax, smin

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      fngrib='fort.13'
      lugb=lun
      lugi=0
      call grib_check(lugb,fngrib,isgrib)
      if (isgrib == 0) then
        print *,' Fatal error.  ims file not grib1 or grib2'
        call errexit(26)
      endif
      call baopenr(lugb,fngrib,iret)
      if(iret.ne.0) then
        print *, ' Fatal error opening file: ', fngrib
        print *, ' iret =', iret
        call errexit(6)
      endif
      print *, ' file ', fngrib,' opened. unit=',lugb

! process grib1 file

      if (isgrib == 1) then ! grib 1 file
        jgds = -1
        jpds = -1
        jpds(5) = 238
        lskip = -1
        mdata = ijo     
        call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &             kpds,kgds,lmsk,snow3,iret)
        if(ndata.eq.0.or.iret.ne.0) then
          WRITE(6,*) ' Fatal Error in getgb'
          WRITE(6,*) ' KPDS=',KPDS(1:25)
          WRITE(6,*) ' KGDS=',KGDS(1:20)
          write(6,*) ' ndata, iret =', ndata, iret
          CALL errexit(7)
        endif
        WRITE(6,*) ' KPDS=',KPDS(1:25)
        WRITE(6,*) ' KGDS=',KGDS(1:20)
        write(6,*) ' ndata, iret =', ndata, iret
        iyr = kpds(8)
        imo = kpds(9)
        idy = kpds(10)
        icen = kpds(21)
        call baclose(lugb,iret)

! don't process grib2 file

      elseif (isgrib == 2) then
         print*,'grib2 imssnow not allowed'
         stop 99
      endif  ! is file grib1 or grib2?

! report min/max and exit

      print *, ' iyr, imo, idy, icen =', iyr,imo,idy,icen
      smax = -100.
      smin = 5000.
      do i = 1, ijo 
      smax = max(smax,snow3(i))
      smin = min(smin,snow3(i)) 
      enddo
      print *, ' In imssnw, smax, smin =', smax, smin

      end subroutine imssnw
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    qcsnow      qc snow depth based on ims cover
C   PRGMMR: HUA-LU PAN       ORG: W/NMC23    DATE: 98-01-30
C
C ABSTRACT:  Use ims cover to quality control the airforce depth field
C            in the NH.  In the SH, set the snow cover based on the
C            airforce depth.
C
C PROGRAM HISTORY LOG:
C   98-01-30  Hua-lu Pan  Initial version
C
C USAGE:    call qcsnow(ijo,snow2,snow3)
C   INPUT ARGUMENT LIST:
C     ijo    - number of pts of global 0.5-deg grid.
C     snow2  - airforce snow depth on 0.5-deg grid before qc
C     snow3  - ims snow cover on 0.5-deg grid (NH only)
C
C   OUTPUT ARGUMENT LIST:    
C     snow2  - airforce snow depth on the 0.5-deg grid after qc
C     snow3  - global snow cover on the 0.5-deg grid
C
C REMARKS: None
C
C ATTRIBUTES:
C   LANGUAGE: Fortran 77
C   MACHINE:  WCOSS
C
C$$$
      subroutine qcsnow(ijo,snow2,snow3)
      implicit none
      integer, intent(in)    :: ijo
      real,    intent(inout) :: snow2(ijo), snow3(ijo)
      integer :: j, js, jo2
      integer :: nmod1, nmod2, nmod3

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      nmod1 = 0
      nmod2 = 0
      nmod3 = 0
      jo2 = ijo / 2
      do j = 1, jo2
        js = j + jo2
c
c  ims snow cover says there is snow but the airforce depth is near zero, 
c  we add snow.  default snow depth is 2.5 mm water equivalent.
c
        if(snow3(j).ge.50..and.snow2(j).le.0.05) then
          snow2(j) = 2.5
          nmod1 = nmod1 + 1
        endif
c
c  ims snow cover says there is no snow but the airforce depth is 
c  non-zero, we set depth to zero.
c
        if(snow3(j).lt.50.) then
          snow2(j) = 0.
          nmod2 = nmod2 + 1
        endif
        if(snow3(j).gt.0.) then
          nmod3 = nmod3 + 1
        endif
c
c  use Air Force snow depth over southern hemisphere to fill in
c  snow cover field because ims cover field is NH only.
c
        if(snow2(js).ge.0.05) snow3(js) = 100.
      enddo

c  make report and exit routine 

      print *, ' Number of snow points added =', nmod1
      print *, ' Number of snow points removed =', nmod2
      print *, ' Number of snow points =', nmod3
      return
      end subroutine qcsnow
c$$$  subprogram documentation block
c
c subprogram:    grib_check
c   prgmmr: gayno          org: w/np2     date: 2014-nov-18
c
c abstract:  determine whether file is grib1, grib2, or other.
c
c program history log:
c 2014-nov-18  gayno    - initial version
c
c usage: call grib_check(file_name, isgrib)
c
c   input argument list:  file_name - file name
c                         lugb      - file unit number
c
c   output argument list: isgrib - '1' or '2' if grib1/2 file
c                                  '0' if not grib
c
c remarks: none.
c
c attributes:
c   language: fortran 90
c   machine:  IBM WCOSS
c
c$$$
      subroutine grib_check(lugb, file_name, isgrib)
      implicit none

      character*(*), intent(in)         :: file_name
      integer, intent(in)               :: lugb
      integer                           :: istat, iseek, mseek
      integer                           :: lskip, lgrib, version
      integer, intent(out)              :: isgrib

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      print*," check file type of: ", trim(file_name)
      call baopenr (lugb, file_name, istat)

      if (istat /= 0) then
        print*,' Fatal error. Bad open, ISTAT IS ',istat
        call errexit(40)
      end if

      iseek = 0
      mseek = 64
      call skgb2(lugb, iseek, mseek, lskip, lgrib, version)

      call baclose(lugb, istat)

      if (lgrib > 0) then
        isgrib = version
        if (isgrib == 1) print*," file is grib1"
        if (isgrib == 2) print*," file is grib2"
      else
        isgrib = 0
        print*," file is not grib1 or grib2"
      endif

      end subroutine grib_check
c$$$  subprogram documentation block
c
c subprogram:   skgb2
c   prgmmr: gayno          org: w/np2     date: 2014-feb-07
c
c abstract:  determine whether file is grib or not.
c            based on w3nco library routine skgb.
c
c program history log:
c 2014-feb-07  gayno    - initial version
c
c usage: call SKGB2(LUGB,ISEEK,MSEEK,LSKIP,LGRIB,I1)
c
c   input argument list:  lugb  - file unit number
c                         iseek - number of bits to skip
c                                 before search.
c                         mseek - max number of bytes
c                                 to search.
c
c   output argument list:  lskip  - number of bytes to skip
c                                   before message
c                          lgrib  - number of bytes in message.
c                                   '0' if not grib.
c                          i1     - '1' or '2' if grib1/2 file.
c                                   '0' if not grib.
c
c remarks: none.
c
c attributes:
c   language: fortran
c
c$$$
      SUBROUTINE SKGB2(LUGB,ISEEK,MSEEK,LSKIP,LGRIB,I1)
      INTEGER, INTENT( IN)     :: LUGB, ISEEK, MSEEK
      INTEGER, INTENT(OUT)     :: LSKIP, LGRIB, I1
      PARAMETER(LSEEK=128)
      CHARACTER Z(LSEEK)
      CHARACTER Z4(4)
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      I1=0
      LGRIB=0
      KS=ISEEK
      KN=MIN(LSEEK,MSEEK)
      KZ=LSEEK
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c  LOOP UNTIL GRIB MESSAGE IS FOUND
      DO WHILE(LGRIB.EQ.0.AND.KN.GE.8.AND.KZ.EQ.LSEEK)
c  READ PARTIAL SECTION
        CALL BAREAD(LUGB,KS,KN,KZ,Z)
        KM=KZ-8+1
        K=0
c  LOOK FOR 'GRIB...1' IN PARTIAL SECTION
        DO WHILE(LGRIB.EQ.0.AND.K.LT.KM)
          CALL GBYTEC(Z,I4,(K+0)*8,4*8)
          CALL GBYTEC(Z,I1,(K+7)*8,1*8)
          IF(I4.EQ.1196575042.AND.(I1.EQ.1.OR.I1.EQ.2)) THEN
c  LOOK FOR '7777' AT END OF GRIB MESSAGE
            IF (I1.EQ.1) CALL GBYTEC(Z,KG,(K+4)*8,3*8)
            IF (I1.EQ.2) CALL GBYTEC(Z,KG,(K+12)*8,4*8)
            CALL BAREAD(LUGB,KS+K+KG-4,4,K4,Z4)
            IF(K4.EQ.4) THEN
              CALL GBYTEC(Z4,I4,0,4*8)
              IF(I4.EQ.926365495) THEN
c  GRIB MESSAGE FOUND
                LSKIP=KS+K
                LGRIB=KG
              ENDIF
            ENDIF
          ENDIF
          K=K+1
        ENDDO
        KS=KS+KM
        KN=MIN(LSEEK,ISEEK+MSEEK-KS)
      ENDDO
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END subroutine skgb2
!$$$  subprogram documentation block
!
! subprogram:    qc_snow_data
!   prgmmr: gayno          org: w/np2     date: 2014-12-05
!
! abstract:  check for corrupt ims or afwa snow data
!
! program history log:
! 2014-dec-5   gayno    - initial version
!
! usage: call qc_snow_data
!
!   input argument list:  
!
!   output argument list: n/a
!
! remarks: none.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$
      subroutine qc_snow_data(kgds,snow,io,jo)
      use gdswzd_mod

      implicit none

      integer, intent(in) :: kgds(200), io, jo

      real, intent(in)    :: snow(io*jo)

      integer             :: hemi, nret, n, ii, jj
      integer, parameter  :: npts=1

      real                :: fill, xpts(npts), ypts(npts)
      real                :: rlon(npts), rlat(npts)
      real, allocatable   :: snow_2d(:,:)

      fill=9999.

      hemi = kgds(10)  ! 0 for nh, 128 for sh

      allocate(snow_2d(io,jo))
      snow_2d=reshape(snow, (/io,jo/))

      print*,' ensure input data is not corrupt'

      !if (hemi == 0) then

        rlat=75.0
        rlon=-40.
        call gdswzd(kgds,(-1),npts,fill,xpts,ypts,rlon,rlat,nret)
        ii = nint(xpts(1))
        jj = nint(ypts(1))
        if (snow_2d(ii,jj) < .01) then
          print*,' warning: no snow in greenland'
        else
          print*,' snow in greenland'
        endif

        rlat=3.0
        rlon=-60.
        call gdswzd(kgds,(-1),npts,fill,xpts,ypts,rlon,rlat,nret)
        ii = nint(xpts(1))
        jj = nint(ypts(1))
        if (snow_2d(ii,jj) > .0) then
          print*,' warning: snow in south america'
        else
          print*,' no snow in south america'
        endif

        rlat=23.0
        rlon=10.
        call gdswzd(kgds,(-1),npts,fill,xpts,ypts,rlon,rlat,nret)
        ii = nint(xpts(1))
        jj = nint(ypts(1))
        if (snow_2d(ii,jj) > .0) then
          print*,' warning: snow in sahara'
        else
          print*,' no snow in sahara'
        endif

        rlat=20.0
        rlon=80.
        call gdswzd(kgds,(-1),npts,fill,xpts,ypts,rlon,rlat,nret)
        ii = nint(xpts(1))
        jj = nint(ypts(1))
        if (snow_2d(ii,jj) > .0) then
          print*,' warning: snow in s india'
        else
          print*,' no snow in s india'
        endif

      !elseif (hemi == 128) then

        rlat=-88.0
        rlon=0.
        call gdswzd(kgds,(-1),npts,fill,xpts,ypts,rlon,rlat,nret)
        ii = nint(xpts(1))
        jj = nint(ypts(1))
        if (snow_2d(ii,jj) < .01) then
          print*,' warning: no snow in antarctica'
        else
          print*,' snow in antarctica'
        endif

        rlat=-10.0
        rlon=-45.0
        call gdswzd(kgds,(-1),npts,fill,xpts,ypts,rlon,rlat,nret)
        ii = nint(xpts(1))
        jj = nint(ypts(1))
        if (snow_2d(ii,jj) > .0) then
          print*,' warning: snow in south america'
        else
          print*,' no snow in south america'
        endif

        rlat=-20.0
        rlon=-130.0
        call gdswzd(kgds,(-1),npts,fill,xpts,ypts,rlon,rlat,nret)
        ii = nint(xpts(1))
        jj = nint(ypts(1))
        if (snow_2d(ii,jj) > .0) then
          print*,' warning: snow in austrailia'
        else
          print*,' no snow in austrailia'
        endif

        rlat=-9.0
        rlon=25.0
        call gdswzd(kgds,(-1),npts,fill,xpts,ypts,rlon,rlat,nret)
        ii = nint(xpts(1))
        jj = nint(ypts(1))
        if (snow_2d(ii,jj) > .0) then
          print*,' warning: snow in africa'
        else
          print*,' no snow in africa'
        endif

      !endif

      deallocate(snow_2d)

      end subroutine qc_snow_data
