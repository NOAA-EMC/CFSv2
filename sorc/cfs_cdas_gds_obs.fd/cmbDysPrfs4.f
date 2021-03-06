!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM:  GODAS_CMBDYSPRFS4
!   PRGMMR: David W. Behringer  ORG: NP23        DATE: 2003-07-25
!
! ABSTRACT:  Write temperature observations in form used by coupled GODAS
!
! PROGRAM HISTORY LOG:
! 2003-07-25  David W. Behringer - original C code
! 2010-09-27  David W. Behringer - added 2 lines after second reading of unit 11
!              to correct erroneous dropping of some profile files.
! 2012-01-22  David W. Behringer -Conversion of original C code to Fortran
! 2016-03-10  David W. Behringer - Added code to allow rescale of obs 
!              representation error. Region can be set by namelist.
!
! USAGE:
!   INPUT FILES:
!     UNIT 10  - NAMELIST FILE CONTAINING PRFS_NML
!     UNIT 11  - LIST IN ASCII OF DAILY TEMPERATURE PROFILE FILES IN IEEE
!     UNIT 21  - USED SUCCESSIVELY FOR DAILY PROFILE FILES, NOT SET
!                EXTERNALLY BY A SOFT LINK
!     grid_spec.nc - NETCDF GRID_SPEC FILE FOR OCEAN MODEL
!
!   OUTPUT FILES:
!     UNIT 31  - scratch FILE
!     UNIT 51  - TEMPERATURE FILE, tmpa.mom, AS USED BY GODAS
!     UNIT 06  - UNIT 6 (STANDARD PRINTFILE)
!
!   SUBPROGRAMS CALLED FROM PROGRAM: (LIST ALL CALLED FROM ANYWHERE IN CODES)
!     UNIQUE:    - getM4Grid, real_indx, inbnds
!     LIBRARY:
!       W3LIB    - w3tagb, w3tage, errexit
!
!   SUBPROGRAMS CALLED FROM MAIN: (LIST ALL CALLED FROM MAIN)
!     UNIQUE:    - getM4Grid, real_indx, inbnds
!     LIBRARY:
!       W3LIB    - w3tagb, w3tage, errexit
!
!   EXIT STATES:
!     COND =   0 - SUCCESSFUL RUN
!     COND =  11 - ERROR OPENING UNIT 11
!     COND =  12 - ERROR READING UNIT 11
!     COND =  21 - ERROR OPENING UNIT 12
!     COND =  22 - ERROR READING UNIT 12
!     COND =  31 - ERROR OPENING UNIT 13
!     COND =  32 - ERROR READING UNIT 13
!     COND =  51 - ERROR OPENING UNIT 51
!     COND =  52 - ERROR WRITING UNIT 51
!     COND =  61 - ERROR READING FROM COMMAND LINE
!
! REMARKS AND IMPORTANT LOCAL VARIABLES:
!     None
!
! ATTRIBUTES:  (LIST MACHINES FOR WHICH CODE IS USED AND CHECKED OUT)
!
!   MACHINE:  IBM WCOSS
!   LANGUAGE: F90
!
!
!$$$
!
      program cmbDysPrfs4
!
!  cmbDysPrfs4 combines profiles for use in assimilation
!
      implicit none
!
      include 'netcdf.inc'
!
      integer, parameter :: ndx=1500
      logical, parameter :: ecnst = .true., mkcr = .false.
      real, parameter :: stde=0.1, zeps=2.0
!     kmax = maximum number of model levels for assimilation
      integer, parameter :: kmax=30, kav=5
      real, parameter :: spv=999.999
      real, parameter :: ynlim=65.0
!
      character sid*2,dtyp*2,qkey*1,csign*8
      character dayFile*30
      real, allocatable, dimension(:) :: dz
      real, allocatable, dimension(:) :: xt, yt, zt, st, rev
      real, allocatable, dimension(:) :: a, z, s, tz, zw, sw
      real, allocatable, dimension(:,:,:) :: ac, sc
      integer, allocatable, dimension(:,:) :: smask
      integer :: nprf, nrd, n, nrjt
      integer :: imx, jmx, kmx
      integer :: i, j, k, kk, kw, kd
      integer iyear, idate, mon, day, ihr, min
      real x, y, xi, yj, xtmn, xtmx, se
!
      character(len=30) :: aname, dname, vname
      integer ncid, ndims, nvars, natts, idunlm, dsiz, nfstat
      integer nvdm, nvatts, vtype, atype, len
      integer, dimension(4)   :: start, count, vdm
      real(kind=8), allocatable, dimension(:,:)   :: nlev
!
      real :: lg0, lg1, lt0, lt1, efctr
      logical :: rescl_error, do_rscl
      namelist /prfs_nml/ lg0, lg1, lt0, lt1, efctr, rescl_error
!
      call w3tagb('GODAS_CMBDYSPRFS4',2003,0164,0164,'NP23')
!
! set namelist defaults and read namelist file
!
      rescl_error = .false.
      lg0 = 999.9
      lg1 = -999.9
      lt0 = 99.9
      lt1 = -99.9
      efctr = 1.0
!
      open(10)
      read(10, prfs_nml)
      close(10)
!
      write(6, prfs_nml)
      do_rscl = .false.
!
! open netCDF grid_spec first
! read model coordinates and mask
!
      call getM4Grid
!
      if (mkcr) call getCorrData
!
      allocate(st(kmx))
      allocate(rev(kmx))
      allocate(s(kmx))
      allocate(z(kmx))
      allocate(tz(kmx))
      allocate(zw(kmx))
      allocate(sw(kmx))
!
! open the input list file
!
      open (11,status='old',form='formatted', &
                 & access='sequential', err=110)
!
      open (31,status='replace',form='unformatted', &
                 & access='sequential', err=310)
!
      nprf = 0
      nrjt = 0
      nrd = 0
      do while (.true.)
        read(11,'(a)',end=10,err=120) dayFile
        open(21,file=trim(dayFile),form='unformatted', &
                 & access='sequential', err=210)
        do while (.true.)
          read(21,end=5,err=220) iyear,idate,csign,sid,dtyp, &
             & qkey,y,x,kd,(zt(k),st(k),k=1,kd)
          nrd = nrd + 1
!
          if (x < xtmn) x = x + 360.0
          if (x > xtmx) x = x - 360.0

          xi = real_indx(x, xt, imx)
          yj = real_indx(y, yt, jmx)
          if (xi .gt. 0.0 .and. yj .gt. 0.0 .and. y .le. ynlim) then
            do k=1,kd
              zw(k) = zt(k)
              sw(k) = st(k)
            end do

            kw = 1
            do k=1,kmx
              if (abs(zw(kw) - zt(k)) .le. zeps) then
                s(k) = sw(kw)
                z(k) = zw(kw)
                kw = kw + 1
              else
                s(k) = spv
                z(k) = zt(k)
              end if
              kk = k
              if (kw .gt. kd) exit
            end do
            kd = kk

            if (kd .gt. kmax) kd = kmax

            if (mkcr .and. dtyp .eq. '-1') call corrPrf(kd,xi,yj)

            if (kd > 1 .and. inbnds(xi,yj)) then

              if (rescl_error) then
                do_rscl = .false.
                if (x .ge. lg0 .and. x .le. lg1 .and. &
                              & y .ge. lt0 .and. y .le. lt1) then
                  do_rscl =.true.
                end if
              end if
              do k=1,kd
                st(k) = s(k)
                se = stde
                if (do_rscl) se = se*efctr
                rev(k) = 1.0 / (se*se)
              end do

              mon = idate/1000000;
              day = mod(idate,1000000)/10000;
              ihr  = mod(idate,10000)/100;
              min = mod(idate,100)
              write(31) iyear,mon,day,ihr,min,csign,dtyp,kd,x,y,(st(k),rev(k),k=1,kd)

              nprf = nprf + 1
            else
              nrjt = nrjt + 1
            end if
          end if
        end do
    5   continue
        close (12)
      end do
   10 continue
      close (11)
      rewind (31)
!
      open (51,status='replace',form='unformatted', &
                 & access='sequential', err=510)
      write(51,err=520) nprf
      do n=1,nprf
        read(31,err=320) iyear,mon,day,ihr,min,csign,dtyp,kd,x,y,(st(k),rev(k),k=1,kd)
        !!write(51,err=510) csign,dtyp
        write(51,err=510) iyear,mon,day,ihr,min,kd
        write(51,err=510) x,y
        write(51,err=510) (st(k),rev(k),k=1,kd)
      end do
!
      close(31)
      close(51)
!
      write(6,'(a,i5)') 'Profiles read:     ',nrd
      write(6,'(a,i5)') 'Profiles retained: ',nprf
      write(6,'(a,i5)') 'Profiles rejected: ',nrjt
!
  100 continue
!
      call w3tage('GODAS_CMBDYSPRFS4')
      call errexit(0)
!
  110 write(6,'(a)') 'Error opening profile file on unit 11'
      call w3tage('GODAS_CMBDYSPRFS4')
      call errexit(11)
!
  120 write(6,'(a)') 'Error reading profile file on unit 11'
      call w3tage('GODAS_CMBDYSPRFS4')
      call errexit(12)
!
  210 write(6,'(a)') 'Error opening profile file on unit 21'
      call w3tage('GODAS_CMBDYSPRFS4')
      call errexit(21)
!
  220 write(6,'(a)') 'Error reading profile file on unit 21'
      call w3tage('GODAS_CMBDYSPRFS4')
      call errexit(22)
!
  310 write(6,'(a)') 'Error opening scratch file on unit 31'
      call w3tage('GODAS_CMBDYSPRFS4')
      call errexit(31)
!
  320 write(6,'(a)') 'Error writing scratch file on unit 31'
      call w3tage('GODAS_CMBDYSPRFS4')
      call errexit(32)
!
  330 write(6,'(a)') 'Error reading scratch file on unit 31'
      call w3tage('GODAS_CMBDYSPRFS4')
      call errexit(33)
!
  510 write(6,'(a)') 'Error opening profile file on unit 51'
      call w3tage('GODAS_CMBDYSPRFS4')
      call errexit(51)
!
  520 write(6,'(a)') 'Error writing profile file on unit 51'
      call w3tage('GODAS_CMBDYSPRFS4')
      call errexit(52)
!
  610 write(6,'(a)') 'Error reading date from command line'
      call w3tage('GODAS_CMBDYSPRFS4')
      call errexit(61)
!
      contains
!
! -------------------------------------------------------------- 
!
      subroutine getM4Grid

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
      allocate (smask(imx,jmx))
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
      smask = int(nlev+0.1)
!
      allocate(dz(kmx))
      dz(1) = 2.0*zt(1)
      do k=2,kmx
        dz(k) = 2.0*(zt(k) - zt(k-1)) - dz(k-1)
      end do
!
      return
!
  210 write(6,'(a)') 'Error opening grid_spec.nc file'
      call w3tage('GODAS_CMBDYSPRFS4')
      call errexit(21)
!
  220 write(6,'(a)') 'Error reading grid_spec.nc file'
      call w3tage('GODAS_CMBDYSPRFS4')
      call errexit(22)
!
      end subroutine getM4Grid
!
! -------------------------------------------------------------- 
!
      subroutine getCorrData
!
      integer :: i, j, kmc

      open (12,status='old',form='unformatted', &
                 & access='sequential', err=120)
      read (12, err=121, end=123) i,j,kmc
      if (i .ne. imx .and. j .ne. jmx) go to 122
      if (kmc .lt. kmx) go to 122
      allocate (ac(imx,jmx,kmc))
      allocate (sc(imx,jmx,kmc))
      read (12, err=121, end=123) ac
      read (12, err=121, end=123) sc
      close(12)
!
      return
!
  120 write(6,'(a)') 'Error opening correction file on unit 12'
      call w3tage('GODAS_CMBDYSPRFS4')
      call errexit(12)
!
  121 write(6,'(a)') 'Error reading correction file on unit 12'
      call w3tage('GODAS_CMBDYSPRFS4')
      call errexit(13)
!
  122 write(6,'(a)') 'Dimension mismatch of correction fields'
      write(6,'(2(a,2i4))') 'Correction: ',i,j,kmc,'  Model: ',imx,jmx,kmx
      call w3tage('GODAS_CMBDYSPRFS4')
      call errexit(14)
!
  123 write(6,'(a)') 'Unexpected EOF in correction file on unit 12'
      call w3tage('GODAS_CMBDYSPRFS4')
      call errexit(15)
!
      end subroutine getCorrData
!
! -------------------------------------------------------------- 
!
      subroutine corrPrf(ktn, xi, yj)
!
      integer :: ktn, im, ip, jm, jp, i0, j0, k
      real :: xi, yj, dxm, dxp, dym, dyp, sdk, h, h0
    
      if (inbnds(xi, yj)) then
        ip = int(xi)
        im = ip - 1
        dxm = xi - real(ip)
        dxp = 1.0 - dxm
        jp = int(yj)
        jm = jp - 1
        dym = yj - real(jp)
        dyp = 1.0 - dym
    
        do k=1,ktn
          if (s(k) .ne. spv) then
            if (ac(im,jm,k) .ne. spv .and. ac(im,jp,k) .ne. spv .and. &
               & ac(ip,jp,k) .ne. spv .and. ac(ip,jm,k) .ne. spv) then
              s(k) = s(k) + ac(im,jm,k) * dxp * dyp + &
                 &          ac(im,jp,k) * dxp * dym + &
                 &          ac(ip,jp,k) * dxm * dym + &
                 &          ac(ip,jm,k) * dxm * dyp
              sdk = ac(im,jm,k) * dxp * dyp + &
                 &  ac(im,jp,k) * dxp * dym + &
                 &  ac(ip,jp,k) * dxm * dym + &
                 &  ac(ip,jm,k) * dxm * dyp
              rev(k) = 1.0/(stde*stde + sdk*sdk)
            else
              i0 = -1
              j0 = -1
              h0 = spv
              h = sqrt(dxm*dxm + dym*dym)
              if (ac(im,jm,k) .ne. spv .and. h < h0) then
                h0 = h
                i0 = im
                j0 = jm
              end if
              h = sqrt(dxm*dxm + dyp*dyp)
              if (ac(im,jp,k) .ne. spv .and. h < h0) then
                h0 = h
                i0 = im
                j0 = jp
              end if
              h = sqrt(dxp*dxp + dyp*dyp)
              if (ac(ip,jp,k) .ne. spv .and. h < h0) then
                h0 = h
                i0 = ip
                j0 = jp
              end if
              h = sqrt(dxp*dxp + dym*dym)
              if (ac(ip,jm,k) .ne. spv .and. h < h0) then
                h0 = h
                i0 = ip
                j0 = jm
              end if
              if (i0 .gt. 0 .and. j0 .gt. 0) then
                s(k) = s(k) + ac(i0,j0,k)
                sdk = ac(i0,j0,k)
                rev(k) = 1.0/(stde*stde + sdk*sdk)
              else
                s(k) = spv
                rev(k) = spv
              end if
            end if
          end if
        end do
      end if
!
      end subroutine corrPrf
!
! -------------------------------------------------------------- 
!
      function real_indx (p, pt, nmx)
!
      real p, pt(*)
      integer nmx
      real real_indx
!
      if (p .gt. pt(1) .and. p .le. pt(nmx)) then
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
      if ((x .lt. 0.0 .and. y .lt. 0.0) .or. &
           & int(x) .gt. imx-1 .or. int(y) .gt. jmx-1) then
        inbnds = .false.
      else
        i = int(x)
        j = int(y)
        if (smask(i,j) .gt. 0 .or. smask(i+1,j) .gt. 0 .or. &
            & smask(i+1,j+1) .gt. 0 .or. smask(i,j+1) .gt. 0) then
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
      end program cmbDysPrfs4
