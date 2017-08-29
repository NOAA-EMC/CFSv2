!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM:  GODAS_MKDLYASMPRFS
!   PRGMMR: David W. Behringer  ORG: NP23        DATE: 2003-07-25
!
! ABSTRACT:  Write salinity observations in form used by coupled GODAS
!
! PROGRAM HISTORY LOG:
! 2003-07-25  David W. Behringer
! 2004-09-13  David W. Behringer - Fix of function real_indx:
!              real_indx = float(n) + ... is changed to
!              real_indx = float(n-1) + ...
! 2008-07-11  David W. Behringer - Copied from mkAsmPrfs.f
!              Changed to write profiles to daily files instead of files
!              that span the whole assimilation window.
! 2010-09-27  David W. Behringer - added 2 lines after second reading of unit 11
!              to correct erroneous dropping of some profile files.
!
! USAGE:
!   INPUT FILES:
!     UNIT 11  - SALINITY PROFILE DATA IN IEEE
!     grid_spec.nc - NETCDF GRID_SPEC FILE FOR OCEAN MODEL
!
!   OUTPUT FILES:
!     UNIT 51  - ASSIGNED TO A SUCCESSION OF SALINITY PROFILE FILES
!                WITHIN PROGRAM (not though exporting XLFUNIT_51)
!     UNIT 06  - UNIT 6 (STANDARD PRINTFILE)
!
!   SUBPROGRAMS CALLED FROM PROGRAM: (LIST ALL CALLED FROM ANYWHERE IN CODES)
!     UNIQUE:    - real_indx, inbnds, intrpZ
!                  jlnDayR, jlnDayC, calDayR, calDayC
!     LIBRARY:
!       W3LIB    - w3tagb, w3tage, errexit
!
!   SUBPROGRAMS CALLED FROM MAIN: (LIST ALL CALLED FROM MAIN)
!     UNIQUE:    - real_indx, inbnds, intrpZ
!                  jlnDayR, calDayR
!     LIBRARY:
!       W3LIB    - w3tagb, w3tage, errexit
!
!   EXIT STATES:
!     COND =   0 - SUCCESSFUL RUN
!     COND =  11 - ERROR OPENING UNIT 11
!     COND =  12 - ERROR READING UNIT 11
!     COND =  21 - ERROR OPENING UNIT 12
!     COND =  22 - ERROR READING UNIT 12
!     COND =  51 - ERROR OPENING UNIT 51
!     COND =  52 - ERROR WRITING UNIT 51
!     COND =  61 - ERROR READING FROM COMMAND LINE
!
! REMARKS AND IMPORTANT LOCAL VARIABLES:
!     Reads a date YYYYMMDD from command line
!
! ATTRIBUTES:  (LIST MACHINES FOR WHICH CODE IS USED AND CHECKED OUT)
!
!   MACHINE:  IBM SP
!   LANGUAGE: F90
!
!
!$$$
!
      program mkDlyAsmPrfs
!
!  mkDlyAsmPrfs prepares profiles for use in assimilation
!
      use tmutil_mod
!
      include 'netcdf.inc'
!
      integer imx, jmx, kmx
      character csign*8,sid*2,dtyp*2,qkey*1,csignS*8
      character str*80, cdate*8, dayFile*30
      real, allocatable, dimension(:) :: pt, pz
      real, allocatable, dimension(:) :: xt, yt, zt, zw, tt
      integer, allocatable, dimension(:,:) :: mskt
      integer ayear, amonth, aday, awndw
      integer jday, jday0, jdayS, jdayE
      logical okay
      real xtmn, xtmx, z0g, dZg
!
      character(len=30) :: aname, dname, vname
      integer ncid, ndims, nvars, natts, idunlm, dsiz
      integer nvdm, nvatts, vtype, atype, len
      integer, dimension(4)   :: start, count, vdm
      real(kind=8), allocatable, dimension(:,:)   :: nlev
!
      call w3tagb('GODAS_MKDLYASMPRFS',2003,0164,0164,'NP23')
!
!     kax = maximum number of model levels for assimilation
!     awndw = time window (days) preceding run day
!
      kax = 30
      awndw = 10
!
!  to protect against linear interpolation across large data gaps
!  as might occur with TAO dropouts, set a threshold dZ beyond which
!  a profile is truncated if it is encountered shallower than Z0
!  (applied in intrpZ)
!
      z0g = 300.0
      dZg = 300.0
!
! get date from command line
!
      narg = iargc()
      if (narg .eq. 0) go to 610
      call getarg(1,str)
      read(str,'(i4,2i2)',err=610) ayear, amonth, aday
      iyear0 = ayear - 1
      jday = jlnDayR(ayear, amonth, aday, iyear0)
      jdayS = jday - awndw
      jdayE = jday + 1
!
! open netCDF grid_spec first
! read model coordinates and mask
!
      nfstat = nf_open('grid_spec.nc', 0, ncid)
      if (nfstat .ne. NF_NOERR) then
        write(6,'(a,a)') 'Could not open grid_spec.nc'
        go to 210
      endif
!
! inquire about the file
!
      nfstat = nf_inq(ncid, ndims, nvars, natts, idunlm)
      if (nfstat .ne. NF_NOERR) then
        write(6,'(a)') 'Could not inquire about grid_spec.nc'
        go to 220
      endif
!
! inquire about dimensions
!
      do n=1,ndims
        nfstat = nf_inq_dim(ncid, n, dname, dsiz)
        if (nfstat .ne. NF_NOERR) then
          write(6,'(a)') 'Could not inquire about dimensions in grid_spec.nc'
          go to 220
        endif
        if (dname .eq. 'grid_x_T') then
          imx = dsiz
        else if (dname .eq. 'grid_y_T') then
          jmx = dsiz
        else if (dname .eq. 'zt') then
          kmx = dsiz
        endif
      enddo
!
! allocate some arrays
!
      allocate (xt(imx))
      allocate (yt(jmx))
      allocate (zt(kmx))
      allocate (nlev(imx,jmx))
      allocate (mskt(imx,jmx))
!
! inquire about variables
! read some variables
!
      do n=1,nvars
        nfstat = nf_inq_var(ncid, n, vname, vtype, nvdm, vdm, nvatts)
        if (nfstat .ne. NF_NOERR) then
          write(6,'(a)') 'Could not inquire about variables in grid_spec.nc'
          go to 220
        endif
        if (vname .eq. 'grid_x_T') then
          start(1) = 1
          count(1) = imx
          nfstat = nf_get_vara_real(ncid, n, start, count, xt)
          if (nfstat .ne. NF_NOERR) then
            write(6,'(a)') 'Could not read XT in grid_spec.nc'
            go to 220
          endif
          xtmn = xt(1)
          xtmx = xt(imx)
        else if (vname .eq. 'grid_y_T') then
          start(1) = 1
          count(1) = jmx
          nfstat = nf_get_vara_real(ncid, n, start, count, yt)
          if (nfstat .ne. NF_NOERR) then
            write(6,'(a)') 'Could not read YT in grid_spec.nc'
            go to 220
          endif
        else if (vname .eq. 'zt') then
          start(1) = 1
          count(1) = kmx
          nfstat = nf_get_vara_real(ncid, n, start, count, zt)
          if (nfstat .ne. NF_NOERR) then
            write(6,'(a)') 'Could not read ZT in grid_spec.nc'
            go to 220
          endif
        else if (vname .eq. 'num_levels') then
          start(1) = 1
          count(1) = imx
          start(2) = 1
          count(2) = jmx
          nfstat = nf_get_vara_double(ncid, n, start, count, nlev)
          if (nfstat .ne. NF_NOERR) then
            write(6,'(a)') 'Could not read NLEV in grid_spec.nc'
            go to 220
          endif
        endif
      enddo
!
! close netcdf file
!
      nfstat = nf_close(ncid)
!
      mskt = int(nlev+0.1)
!
      allocate(tt(kmx))
!
! open the input profile file
!
      open (11,status='old',form='unformatted', &
                 & access='sequential', err=110)
!
      nprf = 0
      npmx = 0
      do while (.true.)
        read(11,end=5,err=120) iyear,idate,csign,sid, &
                                       &  dtyp,qkey,yp,xp,np
        nprf = nprf + 1
        if (np .gt. npmx) npmx = np
      end do
    5 continue
      rewind (11)
!
      allocate(pt(npmx))
      allocate(pz(npmx))
!
! get initial day
!
      xi = -1.0
      yj = -1.0
      jday0 = -1
      nout = -1
      okay = .false.
      do while (.not. inbnds(xi,yj) .or. jday0 .lt. jdayS)
        read (11, end=10, err=120) iyear,idate,csign,sid,dtyp, &
                           &  qkey,yp,xp,np,(pz(k),pt(k),k=1,np)
        if (xp .lt. xtmn) xp = xp + 360.0
        if (xp .gt. xtmx) xp = xp - 360.0
        write (cdate,'(i8.8)') idate
        read (cdate,'(4i2)') imon, iday, ihr, imin
        jday0 = jlnDayR(iyear, imon, iday, iyear0)
        xi = real_indx(xp,xt,imx)
        yj = real_indx(yp,yt,jmx)
        nout = nout + 1
      end do
      okay = .true.
   10 continue
      if (.not. okay) then
        write(6,'(a)') 'Reading profile file (unit 11) :'
        write(6,'(a)') ' all profiles appear out of bounds.'
        go to 120
      end if
!
      write (cdate,'(i8.8)') idate
      read (cdate,'(4i2)') imon, iday, ihr, imin
      jday0 = jlnDayR(iyear, imon, iday, iyear0)
!
! create first daily file
!
      write(cdate,'(i4.4,2i2.2)') iyear, imon, iday
      dayFile = cdate // 'sal.prf'
      open (unit=51,file=trim(dayFile),status='new',form='unformatted', &
                 & access='sequential', err=510)
      call intrpZ(kt,pt,pz,np,tt,zt,kmx)
      write (51, err=520) iyear,idate,csign,sid,dtyp,qkey,yp,xp,kt, &
                                   &  (zt(k),tt(k),k=1,kt)
!
! continue loop on input profile file
!
      do while (.true.)
        read (11, end=100, err=120) iyear,idate,csign,sid,dtyp, &
                           &  qkey,yp,xp,np,(pz(k),pt(k),k=1,np)
        if (xp .lt. xtmn) xp = xp + 360.0
        if (xp .gt. xtmx) xp = xp - 360.0
        write (cdate,'(i8.8)') idate
        read (cdate,'(4i2)') imon, iday, ihr, imin
        jday = jlnDayR(iyear, imon, iday, iyear0)
        xi = real_indx(xp,xt,imx)
        yj = real_indx(yp,yt,jmx)
!
        if (inbnds(xi,yj)) then
          if (jday .eq. jday0) then
            call intrpZ(kt,pt,pz,np,tt,zt,kmx)
            write (51, err=520) iyear,idate,csign,sid,dtyp,qkey,yp,xp,kt, &
                                       &  (zt(k),tt(k),k=1,kt)
          else if (jday .gt. jday0 .and. jday .le. jdayE) then
            close (51)
            jday0 = jday
            write (cdate,'(i4.4,2i2.2)') iyear, imon, iday
            dayFile = cdate // 'sal.prf'
            open (unit=51,file=trim(dayFile),status='new',form='unformatted', &
                       & access='sequential', err=510)
!
            call intrpZ(kt,pt,pz,np,tt,zt,kmx)
            write (51, err=520) iyear,idate,csign,sid,dtyp,qkey,yp,xp,kt, &
                                         &  (zt(k),tt(k),k=1,kt)
          else if (jday .gt. jdayE) then
            go to 100
          else if (jday .lt. jday0) then
            write(6,'(a)') 'Reading profile file (unit 11) :'
            write(6,'(a)') ' time sequence error.'
            go to 120
          end if
!
        end if
!
      end do
  100 continue
!
      close (11)
      close (51)
!
      call w3tage('GODAS_MKDLYASMPRFS')
      call errexit(0)
!
  110 write(6,'(a)') 'Error opening profile file on unit 11'
      call w3tage('GODAS_MKDLYASMPRFS')
      call errexit(11)
!
  120 write(6,'(a)') 'Error reading profile file on unit 11'
      call w3tage('GODAS_MKDLYASMPRFS')
      call errexit(12)
!
  210 write(6,'(a)') 'Error opening grid_spec.nc file'
      call w3tage('GODAS_MKDLYASMPRFS')
      call errexit(21)
!
  220 write(6,'(a)') 'Error reading grid_spec.nc file'
      call w3tage('GODAS_MKDLYASMPRFS')
      call errexit(22)
!
  510 write(6,'(a)') 'Error opening profile file on unit 51'
      call w3tage('GODAS_MKDLYASMPRFS')
      call errexit(51)
!
  520 write(6,'(a)') 'Error writing profile file on unit 51'
      call w3tage('GODAS_MKDLYASMPRFS')
      call errexit(52)
!
  610 write(6,'(a)') 'Error reading date from command line'
      call w3tage('GODAS_MKDLYASMPRFS')
      call errexit(61)
!
      contains
!
! -------------------------------------------------------------- 
!
      function real_indx (p, pt, nmx)
!
      real p, pt(*)
      integer nmx
!
      if (p .ge. pt(1) .and. p .le. pt(nmx)) then
        n = 1
        do while (pt(n) .lt. p)
          n = n + 1
        end do
        real_indx = float(n-1) + (p-pt(n-1)) / (pt(n)-pt(n-1))
      else
        real_indx = -1.0
      end if
!
      end function real_indx
!
! --------------------------------------------------------------
!
      function inbnds (x, y)
!
      real x, y
      logical inbnds
!
      if (x .lt. 0.0 .and. y .lt. 0.0) then
        inbnds = .false.
      else
        i = int(x)
        j = int(y)
        if (mskt(i,j) .gt. 0 .or. mskt(i+1,j) .gt. 0 .or. &
            & mskt(i+1,j+1) .gt. 0 .or. mskt(i,j+1) .gt. 0) then
          inbnds = .true.
        else
          inbnds = .false.
        end if
      end if
!
      end function inbnds
!
! -------------------------------------------------------------- 
!
      subroutine intrpZ(kt, ti, zi, ki, to, zo, ko)
!
      real ti(*), zi(*), to(*), zo(*)
      integer kt, ki, ko
!
      integer k, kk
      integer km, kp, kv2, cnt
      real zr
!
      kk = ki
      do k=2,ki
        km = k - 1
        if (zi(km) .le. z0g .and. (zi(k) - zi(km)) .ge. dZg) then
          kk = km
          exit
        end if
      end do
      ki = kk
!
      k = 1
      kt = 1
      if (zi(1) .lt. 30.0) then
        do while (kt .lt. ko)
          if (zo(kt) .le. zi(1)) then
            to(kt) = ti(1)
          else
            do while (zi(k) .lt. zo(kt) .and. k .lt. ki)
              k = k + 1
            end do
            if (zo(kt) .le. zi(k)) then
              zr = (zo(kt) - zi(k-1)) / (zi(k) - zi(k-1))
              to(kt) = ti(k-1) + (ti(k) - ti(k-1)) * zr
            else
              kt = kt - 1
              exit
            end if
          end if
          kt = kt + 1
        end do
      end if
!
      kk = kt
      do k=1,kk
        if (to(k) .le. -5.0) kt = 0
      end do
!
      end subroutine intrpZ
!
      end program mkDlyAsmPrfs
