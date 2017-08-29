!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM:  GODAS_CMBDYSPRF4
!   PRGMMR: David W. Behringer  ORG: NP23        DATE: 2003-07-25
!
! ABSTRACT:  Write temperature observations in form used by coupled GODAS
!
! PROGRAM HISTORY LOG:
! 2003-07-25  David W. Behringer - original C code
! 2010-09-27  David W. Behringer - added 2 lines after second reading of unit 11
!              to correct erroneous dropping of some profile files.
! 2013-01-22  David W. Behringer - Conversion of original C code to Fortran
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
      program cmbDysPrf4
!
!  cmbDysPrf4 combines profiles for use in assimilation
!
      implicit none
!
      include 'netcdf.inc'
!
      logical, parameter :: doNrmDz=.true.
      logical, parameter :: doSqRt=.true.
      integer, parameter :: ndx=1500
      real, parameter :: seo=1.0, sef=1.5, zeps=2.0, teps=0.00005
!     kmax = maximum number of model levels for assimilation
      integer, parameter :: kmax=30, kav=5
      real, parameter :: spv=999.999
      real, parameter :: ynlim=65.0
!
      character sid*2,dtyp*2,qkey*1,csign*8
      character dayFile*30
      real, allocatable, dimension(:) :: dz
      real, allocatable, dimension(:) :: xt, yt, zt, tt, rev
      real, allocatable, dimension(:) :: a, z, t, tz, zw, tw
      integer, allocatable, dimension(:,:) :: tmask
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
      call w3tagb('GODAS_CMBDYSPRF4',2003,0164,0164,'NP23')
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

      allocate(tt(kmx))
      allocate(rev(kmx))
      allocate(t(kmx))
      allocate(z(kmx))
      allocate(tz(kmx))
      allocate(zw(kmx))
      allocate(tw(kmx))
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
             & qkey,y,x,kd,(zt(k),tt(k),k=1,kd)
          nrd = nrd + 1
!
          if (x < xtmn) x = x + 360.0
          if (x > xtmx) x = x - 360.0

          xi = real_indx(x, xt, imx)
          yj = real_indx(y, yt, jmx)
          if (xi .gt. 0.0 .and. yj .gt. 0.0 .and. y .le. ynlim) then
            do k=1,kd
              zw(k) = zt(k)
              tw(k) = tt(k)
            end do

            kw = 1
            do k=1,kmx
              if (abs(zw(kw) - zt(k)) .le. zeps) then
                t(k) = tw(kw)
                z(k) = zw(kw)
                kw = kw + 1
              else
                t(k) = spv
                z(k) = zt(k)
              end if
              kk = k
              if (kw .gt. kd) exit
            end do
            kd = kk

            call cmpTz

            if (kd .gt. kmax) kd = kmax

            if (kd > 1 .and. inbnds(xi,yj)) then

              if (rescl_error) then
                do_rscl = .false.
                if (x .ge. lg0 .and. x .le. lg1 .and. &
                              & y .ge. lt0 .and. y .le. lt1) then
                  do_rscl =.true.
                end if
              end if
              do k=1,kd
                tt(k) = t(k)
                se = seo + sef * tz(k)
                if (do_rscl) se = se*efctr
                rev(k) = 1.0 / (se*se)
              end do

              mon = idate/1000000;
              day = mod(idate,1000000)/10000;
              ihr  = mod(idate,10000)/100;
              min = mod(idate,100)
              write(31) iyear,mon,day,ihr,min,csign,dtyp,kd,x,y,(tt(k),rev(k),k=1,kd)

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
        read(31,err=320) iyear,mon,day,ihr,min,csign,dtyp,kd,x,y,(tt(k),rev(k),k=1,kd)
        !!write(51,err=510) csign,dtyp
        write(51,err=510) iyear,mon,day,ihr,min,kd
        write(51,err=510) x,y
        write(51,err=510) (tt(k),rev(k),k=1,kd)
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
      call w3tage('GODAS_CMBDYSPRF4')
      call errexit(0)
!
  110 write(6,'(a)') 'Error opening profile file on unit 11'
      call w3tage('GODAS_CMBDYSPRF4')
      call errexit(11)
!
  120 write(6,'(a)') 'Error reading profile file on unit 11'
      call w3tage('GODAS_CMBDYSPRF4')
      call errexit(12)
!
  210 write(6,'(a)') 'Error opening profile file on unit 21'
      call w3tage('GODAS_CMBDYSPRF4')
      call errexit(21)
!
  220 write(6,'(a)') 'Error reading profile file on unit 21'
      call w3tage('GODAS_CMBDYSPRF4')
      call errexit(22)
!
  310 write(6,'(a)') 'Error opening scratch file on unit 31'
      call w3tage('GODAS_CMBDYSPRF4')
      call errexit(31)
!
  320 write(6,'(a)') 'Error writing scratch file on unit 31'
      call w3tage('GODAS_CMBDYSPRF4')
      call errexit(32)
!
  330 write(6,'(a)') 'Error reading scratch file on unit 31'
      call w3tage('GODAS_CMBDYSPRF4')
      call errexit(33)
!
  510 write(6,'(a)') 'Error opening profile file on unit 51'
      call w3tage('GODAS_CMBDYSPRF4')
      call errexit(51)
!
  520 write(6,'(a)') 'Error writing profile file on unit 51'
      call w3tage('GODAS_CMBDYSPRF4')
      call errexit(52)
!
  610 write(6,'(a)') 'Error reading date from command line'
      call w3tage('GODAS_CMBDYSPRF4')
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
      allocate (tmask(imx,jmx))
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
      tmask = int(nlev+0.1)
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
      call w3tage('GODAS_CMBDYSPRF4')
      call errexit(21)
!
  220 write(6,'(a)') 'Error reading grid_spec.nc file'
      call w3tage('GODAS_CMBDYSPRF4')
      call errexit(22)
!
      end subroutine getM4Grid
!
! -------------------------------------------------------------- 
!
      subroutine cmpTz
!
      integer k, kk, km, kp, kv2, cnt
      real tzmn, tzmx

      do k=2,kd-1
        if (t(k-1) .ne. spv .and. t(k+1) .ne. spv) then
          tz(+k) = (t(k-1) - t(k+1)) / (z(k+1) - z(k-1))
          if (tz(k) .lt. 0.0) tz(k) = 0.0
        else
          tz(k) = spv
        end if
      end do
      tz(1) = spv
      tz(kd) = spv

      do k=1,kd
        if (tz(k) .eq. spv .and. t(k) .ne. spv) then
          km = 0
          do kk=k-1,1,-1
            if (tz(kk) .ne. spv) then
              km = kk
              exit
            end if
          end do
          kp = kd
          do kk=k+1,kd
            if (tz(kk) .ne. spv) then
              kp = kk
              exit
            end if
          end do
          if (km .gt. 0 .and. kp .lt. kd) then
            if ((k-km) .le. (kp-k)) then
              tz(k) = tz(km)
            else
              tz(k) = tz(kp)
            end if
          else if (km .gt. 0) then
            tz(k) = tz(km)
          else if (kp .le. kd) then
            tz(k) = tz(kp)
          else
            tz(k) = spv
          end if
        end if
      end do

      do k=1,kd
        if (tz(k) .eq. spv .and. t(k) .ne. spv) then
          tz(k) = 0.0
        end if
      end do

      if (doNrmDz) then
        do k=1,kd
          if (tz(k) .ne. spv) then
            tz(k) = tz(k) / dz(k)
          end if
        end do
      end if

      if (doSqRt) then
        do k=1,kd
          if (tz(k) .ne. spv) then
            if (tz(k) .gt.  0.0) then
              tz(k) = sqrt(tz(k))
            else
              tz(k) = 0.0
            end if
          end if
        end do
      end if

      if (kav .gt. 1) then
        kv2 = kav / 2
        do k=1,kd
          if (tz(k) .ne. spv) then
            km = k-kv2
            if (km .lt. 1) km = 1
            cnt = 1
            tw(k) = tz(k)
            do kk=k-1,km,-1
              if (tz(kk) .ne. spv) then
                tw(k) = tw(k) + tz(kk)
                cnt = cnt + 1
              else
                exit
              end if
            end do

            kp = k+kv2
            if (kp .ge. kd) kp = kd
            do kk=k+1,kp
              if (tz(kk) .ne. spv) then
                tw(k) = tw(k) + tz(kk)
                cnt = cnt + 1
              else
                exit
              end if
            end do
            if (cnt .gt. 0) tw(k) = tw(k) / cnt
          else
            tw(k) = spv
          end if
        end do
        do k=1,kd
          tz(k) = tw(k)
        end do
      end if

      tzmn = spv
      tzmx = -spv
      do k=1,kd
        if (tz(k) .ne. spv) then
          if (tzmn .gt. tz(k)) tzmn = tz(k)
          if (tzmx .lt. tz(k)) tzmx = tz(k)
        end if
      end do
      tzmx = tzmx - tzmn
      if (tzmx .lt. teps) then
        do k=1,kd
          if (tz(k) .ne. spv) then
            tz(k) = 0.0
          end if
        end do
      else
        do k=1,kd
          if (tz(k) .ne. spv) then
            tz(k) = tz(k) - tzmn
            tz(k) = tz(k) / tzmx
          end if
        end do
      end if
!
      end subroutine cmpTz
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
        if (tmask(i,j) .gt. 0 .or. tmask(i+1,j) .gt. 0 .or. &
            & tmask(i+1,j+1) .gt. 0 .or. tmask(i,j+1) .gt. 0) then
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
      end program cmbDysPrf4
