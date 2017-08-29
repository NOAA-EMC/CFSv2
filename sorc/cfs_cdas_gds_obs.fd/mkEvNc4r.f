!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM:  CDAS_MKEVNC4R
!   PRGMMR: David W. Behringer  ORG: NP23        DATE: 2013-01-27
!
! ABSTRACT: Construct model background error variance for GODAS temperature
!           and salinity base on veritcal gradient of temperature.  The 
!           purpose is to model greater error in the thermocline and 
!           halocline.
!
! PROGRAM HISTORY LOG:
! 2003-07-25  David W. Behringer - original C code
! 2013-01-27  David W. Behringer - Conversion of original C code to Fortran
! 2014-07-21  David W. Behringer - Addition of a constraint on initial estimate
!             background error variance based on difference between forecast
!             temperature and cimatological temperature
!
! USAGE:
!   INPUT FILES:
!     UNIT 11  - MOMv4 restart date file
!     ocean_temp_salt.res.nc - MOMv4 restart netCDF for temperature and salinity
!     grid_spec.nc           - MOMv4 grid specification netCDF
!     TanF_WOA09_M4.nc       - WOA09 temperature interpolated fo MOMv4 grid
!
!   OUTPUT FILES:
!     UNIT 51  - background error variance for temperature as used by GODAS
!     UNIT 52  - background error variance for salinity as used by GODAS
!     UNIT 06  - UNIT 6 (STANDARD PRINTFILE)
!
!   SUBPROGRAMS CALLED FROM PROGRAM: (LIST ALL CALLED FROM ANYWHERE IN CODES)
!     UNIQUE:    - getM4Grid, getTz, getATemp, fltrFile, nrmFile, cielFile,
!                  smthFile, sfcMnMx, hFill, mkSEv, writeFile
!     LIBRARY:
!       W3LIB    - w3tagb, w3tage, errexit
!
!   SUBPROGRAMS CALLED FROM MAIN: (LIST ALL CALLED FROM MAIN)
!     UNIQUE:    - getM4Grid, getTz, getATemp, fltrFile, nrmFile, cielFile,
!                  smthFile, sfcMnMx, hFill, mkSEv, writeFile
!     LIBRARY:
!       W3LIB    - w3tagb, w3tage, errexit
!
!   EXIT STATES:
!     COND =   0 - SUCCESSFUL RUN
!     COND =  11 - ERROR OPENING DATE FILE
!     COND =  12 - ERROR READING DATE FILE
!     COND =  21 - ERROR OPENING GRID_SPEC.NC NETCDF
!     COND =  22 - ERROR READING GRID_SPEC.NC NETCDF
!     COND =  31 - ERROR OPENING OCEAN_TEMP_SALT.RES.NC NETCDF
!     COND =  32 - ERROR READING OCEAN_TEMP_SALT.RES.NC NETCDF
!     COND =  33 - ERROR IN SIZE OF OCEAN_TEMP_SALT.RES.NC NETCDF
!     COND =  51 - ERROR OPENING UNIT 51, TEMP VARIANCE OUTPUT
!     COND =  52 - ERROR WRITING UNIT 51, TEMP VARIANCE OUTPUT
!     COND =  53 - ERROR OPENING UNIT 52, SALT VARIANCE OUTPUT
!     COND =  54 - ERROR WRITING UNIT 52, SALT VARIANCE OUTPUT
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
  program mkEvNc4r
!
  implicit none
  
  include 'netcdf.inc'
  
  real, parameter :: spv = 999.999, epsmx = 0.30
  real, parameter :: rpd = 0.01745329252
  real, parameter :: afctr = 0.25
  
  integer :: imx, jmx, kmx, ioff
  integer, parameter :: kass = 30, iav0 = 11, jav0 = 5, kav0 = 5
  real, allocatable, dimension(:,:,:)  :: a, b, bs, c
  real, allocatable, dimension(:,:)  :: ev
  real, allocatable, dimension(:) :: xt, yt, zt, dz, w
  real :: xtmn, xtmx

  real, allocatable, dimension(:,:,:)  :: tr, ta
  real, allocatable, dimension(:)  :: wr
  real :: mV

  real(kind=8), allocatable, dimension(:,:) :: nlev
  logical, parameter :: SdZ=.true., zTpr=.false., zTpr2=.false.
  logical, parameter :: SqrR=.true., SqrR2=.true.
  integer, parameter :: Sfc=1, Nrml=1
  integer, parameter :: ltS0=-2, ltN0=2, lgW0=130, lgE0=250
  integer, allocatable, dimension(:,:) :: mask
  real, parameter :: VsMn=0.30, VsMx=0.80, gscl=1.0, LsTpr=-90.0, LnTpr=90.0
  real, parameter :: zTpf = 0.25, gciel = 1.0
  integer, dimension(100) :: vbin
  real :: period
  character prog*40, fileOut*90, grdFile*90
  character bsn*2, ulTitle*90, urDate*90, sfileOut*91
  
  integer :: dte, year, month, day
  
  real, allocatable, dimension(:) :: zm, dzm, wtk, wtkp, ab, ap, az
  real(kind=8), allocatable, dimension(:,:,:,:) :: buf
  
! Salinity 
!     if Sm0 = .true., the Sal bkgd error will be zeroed in the mixed layer
!     wherever the surface temperature is greater than isoT
  
  logical :: Sm0 = .false.
  logical, parameter :: SVar = .true.
  
! for salinity in MOM4, is in psu units
!     sfctr = sqrt( 0.001 ) = 3.162e-2
   
  real, parameter :: Tzmn=0.4, sfctr=3.162e-2, isoT=20.0, dTml=0.5
  real, allocatable, dimension(:,:) :: t0
  integer, allocatable, dimension(:,:) :: mlk
  
  integer :: ncid, nfstat, ndims, nvars, natts, idunlm
  integer :: t2Vid, dtVid, xtVid, ytVid, ztVid, tVid, mlVid
  integer, dimension(4) :: start, count
! int ndims, nvars, ngatts, xdimid
! int nspdms
  real :: msV
  character(20) :: dname
  integer :: dsiz
  character(20) :: vname
  integer :: vtype, nvdm, nvatts
  integer, dimension(4) :: vdm
  character(20) :: aname
  integer :: atype, alen
  integer :: n

  call w3tagb('CDAS_MKEVNC4R',2013,0164,0164,'NP23')
  
  if (.not. SVar) Sm0 = .false.
  
  call getM4Grid

  open(11,status='old',form='formatted',access='sequential',err=110)
  read(11,*)
  read(11,*)
  read(11,'(3i6)',err=120) year, month, day
  close(11)
  
  call getTz

  call getATemp

  call fltrFile

  call nrmFile
  
  call cielFile
  
  c = b
  call smthFile
  b = c
  
  call nrmFile
  
 call sfcMnMx
  
 call hFill
  
  if (SVar) then
    call mkSEv
    c = bs
    call smthFile
    bs = c
  end if
  
  call writeFile()

  call w3tage('CDAS_MKEVNC4R')
  call errexit(0)
!
  110 write(6,'(a)') 'Error opening date file'
      call w3tage('GODAS_MKEVNC4R')
      call errexit(11)
!
  120 write(6,'(a)') 'Error reading date file'
      call w3tage('GODAS_MKEVNC4R')
      call errexit(12)
!
  contains
!
! --------------------------------------------------------------
!
      subroutine getM4Grid
!
      integer :: i, ii, j, k

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
      allocate (mask(imx,jmx))
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
      allocate(w(imx))
      w(1) = xt(1)
      ioff = 0
      do i=2,imx
        if (xt(i-1) .lt. 0.0 .and. xt(i) .ge. 0.0) then
          ioff = i - 1
        end if
        w(i) = xt(i)
      end do
      do i=1,imx
        ii = i + ioff
        if (ii .gt. imx) ii = ii - imx
        xt(i) = w(ii)
        if (xt(i) .lt. 0.0) xt(i) = xt(i) + 360.0
      end do

      do j=1,jmx
        do i=1,imx
          ii = i + ioff
          if (ii .gt. imx) ii = ii - imx
          mask(i,j) = int(nlev(ii,j) + 0.01)
        end do
      end do

      allocate(zm(kmx))
      allocate(dzm(kmx))
      allocate(wtk(kmx))
      allocate(wtkp(kmx))
      zm(1) = 0.0
      do k=2,kmx
        zm(k) = 0.5*(zt(k-1) + zt(k))
        dzm(k) = zt(k) - zt(k-1)
      end do
      dzm(1) = dzm(2)
      do k=1,kmx-1
        wtkp(k) = (zm(k+1) - zt(k)) / (zm(k+1) - zm(k))
        wtk(k) = (zt(k) - zm(k)) / (zm(k+1) - zm(k))
      end do
!
      return
!
  210 write(6,'(a)') 'Error opening grid_spec.nc file'
      call w3tage('GODAS_MKEVNC4R')
      call errexit(21)
!
  220 write(6,'(a)') 'Error reading grid_spec.nc file'
      call w3tage('GODAS_MKEVNC4R')
      call errexit(22)
!
      end subroutine getM4Grid
!
! --------------------------------------------------------------
!
  subroutine getTz
!
  integer :: i, ii, j, k, ka
  real :: tzt, bmx
!
  nfstat = nf_open('ocean_temp_salt.res.nc', 0, ncid)
  if (nfstat .ne. NF_NOERR) then
    write(6,'(a)') 'Could not open ocean_temp_salt.res.nc'
    go to 310
  endif
!
  nfstat = nf_inq(ncid, ndims, nvars, natts, idunlm)
  if (nfstat .ne. NF_NOERR) then
    write(6,'(a)') 'Could not inquire about ocean_temp_salt.res.nc'
    go to 320
  endif

  i = -1
  j = -1
  k = -1
  do n=1,ndims
    nfstat = nf_inq_dim(ncid, n, dname, dsiz)
    if (nfstat .ne. NF_NOERR) then
      write(6,'(a)') 'Could not get dimensions from ocean_temp_salt.res.nc'
      go to 320
    endif
    if (dname .eq. 'xaxis_1') then
      i = dsiz
    else if (dname .eq. 'yaxis_1') then
      j = dsiz
    else if (dname .eq. 'zaxis_1') then
      k = dsiz
    endif
  enddo

  if (i .ne. imx .or. j .ne. jmx .or. k .ne. kmx) then
    write(6,'(a,3i4,a,3i4,a)') 'Dimension mismatch: (',  &
            &                       i,j,k,') vs (',imx,jmx,kmx,')'
    go to 330
  endif

  do n=1,nvars
    nfstat = nf_inq_var(ncid, n, vname, vtype, nvdm, vdm, nvatts)
    if (nfstat .ne. NF_NOERR) then
      write(6,'(a)') 'Could not get variables from ocean_temp_salt.res.nc'
      go to 320
    endif
    if (vname .eq. 'temp') tVid = n
  end do

  allocate(a(imx,jmx,kmx))
  allocate(b(imx,jmx,kmx))
  allocate(c(imx,jmx,kmx))
  allocate(ab(kmx))
  allocate(ap(kmx))
  allocate(az(kmx))
  allocate(buf(imx,jmx,kmx,1))

  nfstat = nf_get_var_double(ncid, tVid, buf)
  if (nfstat .ne. NF_NOERR) then
    write(6,'(a)') 'Could not read 1st temp from ocean_temp_salt.res.nc'
    go to 320
  endif
!
  do j=1,jmx
    do i=1,imx
      if (mask(i,j) .gt. 0) then
        ka = mask(i,j)
        ii = i + ioff
        if (ii .gt. imx) ii = ii - imx
        do k=1,ka
          ab(k) = buf(ii,j,k,1)
        end do
 
        do k=2,ka
          ap(k) = (ab(k-1) - ab(k)) / dzm(k)
        end do
        ap(1) = ap(2)
        
        do k=2,ka-1
          az(k) = ap(k) * wtkp(k)  +  ap(k+1) * wtk(k)
        end do
        az(1) = az(2)
        az(ka) = az(ka-1)

        do k=1,kmx
          if (k .le. ka) then
            b(i,j,k) = az(k)
          else
            b(i,j,k) = spv
          end if
        end do
      else
        do k=1,kmx
          b(i,j,k) = spv
        end do
      end if
    end do
  end do
  
  if (Sm0) then
  
    allocate(t0(imx,jmx))
    start(1) = 1
    count(1) = 1
    start(2) = 1
    count(2) = 1
    start(3) = 1
    count(3) = jmx
    start(4) = 1
    count(4) = imx
    nfstat = nf_get_vara_double(ncid, tVid, start, count, t0)
    if (nfstat .ne. NF_NOERR) then
      write(6,'(a,a)') 'Could not read 2nd temp from ocean_temp_salt.res.nc'
      go to 320
    endif
  
    start(1) = 1
    count(1) = 1
    start(2) = 1
    count(2) = kmx
    start(3) = 1
    count(3) = jmx
    start(4) = 1
    count(4) = imx
    nfstat = nf_get_vara_double(ncid, tVid, start, count, a)
    if (nfstat .ne. NF_NOERR) then
      write(6,'(a,a)') 'Could not read 3rd temp from ocean_temp_salt.res.nc'
      go to 320
    endif
  
    allocate(mlk(imx,jmx))
    do j=1,jmx
      do i=1,imx
        if (mask(i,j) .gt. 0 .and. t0(i,j) .gt. isoT) then
          tzt = t0(i,j) - dTml
          mlk(i,j) = 1
          do k=2,mask(i,j)
            if (a(i,j,k) .lt. tzt) then
              mlk(i,j) = k - 1
              exit
            endif
          end do
        else
          mlk(i,j) = 1
        endif
      end do
    end do

  end if

! get a copy of the temperature field to be compared with climatology

  allocate(tr(imx,jmx,kmx))

  nfstat = nf_get_var_double(ncid, tVid, buf)
  if (nfstat .ne. NF_NOERR) then
    write(6,'(a,a)') 'Could not read 4th temp from ocean_temp_salt.res.nc'
    go to 320
  endif

  nfstat = nf_close(ncid)

  do k=1,kmx
    do j=1,jmx
      do i=1,imx
        tr(i,j,k) = buf(i,j,k,1)
      end do
    end do
  end do
  
  bmx = 0.0
  do j=1,jmx
    do i=1,imx
      do k=1,mask(i,j)
        if (b(i,j,k) .gt. bmx) bmx = b(i,j,k)
      end do
    end do
  end do
  if (bmx .gt. 0.0) then
    do j=1,jmx
      do i=1,imx
        do k=1,mask(i,j)
          if (b(i,j,k) .lt. 0.0) then
            b(i,j,k) = 0.0
          else
            b(i,j,k) = b(i,j,k) / bmx
          end if
        end do
      end do
    end do
  end if
  
  allocate(dz(kmx))
  dz(1) = 2.0 * zt(1)
  do k=2,kmx
    dz(k) = 2.0 *(zt(k) - zt(k-1)) - dz(k-1)
  end do

  do j=1,jmx
    do i=1,imx
      if (mask(i,j) .gt. 1) then
        b(i,j,1) = b(i,j,2)
      endif
    end do
  end do

  return
!
  310 write(6,'(a)') 'Error opening file ocean_temp_salt.res.nc'
      call w3tage('GODAS_MKEVNC4R')
      call errexit(31)
!
  320 write(6,'(a)') 'Error reading file ocean_temp_salt.res.nc'
      call w3tage('GODAS_MKEVNC4R')
      call errexit(32)
!
  330 write(6,'(a)') 'ocean_temp_salt.res.nc has wrong dimensions'
      call w3tage('GODAS_MKEVNC4R')
      call errexit(33)
!
  end subroutine getTz
  
!
! --------------------------------------------------------------
!
  subroutine getATemp
!
  integer :: i, ii, j, k
  real :: tmx
!
  nfstat = nf_open('TanF_WOA09_M4.nc', 0, ncid)
  if (nfstat .ne. NF_NOERR) then
    write(6,'(a)') 'Could not open TanF_WOA09_M4.nc'
    go to 410
  endif
!
  nfstat = nf_inq(ncid, ndims, nvars, natts, idunlm)
  if (nfstat .ne. NF_NOERR) then
    write(6,'(a)') 'Could not inquire about TanF_WOA09_M4.nc'
    go to 420
  endif

  i = -1
  j = -1
  k = -1
  do n=1,ndims
    nfstat = nf_inq_dim(ncid, n, dname, dsiz)
    if (nfstat .ne. NF_NOERR) then
      write(6,'(a)') 'Could not get dimensions from TanF_WOA09_M4.nc'
      go to 420
    endif
    if (dname .eq. 'grid_x_T') then
      i = dsiz
    else if (dname .eq. 'grid_y_T') then
      j = dsiz
    else if (dname .eq. 'zt') then
      k = dsiz
    endif
  enddo

  if (i .ne. imx .or. j .ne. jmx .or. k .ne. kmx) then
    write(6,'(a,3i4,a,3i4,a)') 'Dimension mismatch: (',  &
            &                       i,j,k,') vs (',imx,jmx,kmx,')'
    go to 430
  endif

  do n=1,nvars
    nfstat = nf_inq_var(ncid, n, vname, vtype, nvdm, vdm, nvatts)
    if (nfstat .ne. NF_NOERR) then
      write(6,'(a)') 'Could not get variables from TanF_WOA09_M4.nc'
      go to 420
    endif
    if (vname .eq. 'temp') tVid = n
  end do

  allocate(ta(imx,jmx,kmx))

  nfstat = nf_get_var_real(ncid, tVid, ta)
  if (nfstat .ne. NF_NOERR) then
    write(6,'(a)') 'Could not read temp from TanF_WOA09_M4.nc'
    go to 420
  endif

  nfstat = nf_get_att_real(ncid, tVid, "missing_value", mV)
  if (nfstat .ne. NF_NOERR) then
    write(6,'(a)') 'Could not read missing_value from TanF_WOA09_M4.nc'
    go to 420
  endif

  nfstat = nf_close(ncid)

  allocate(wr(imx))
  do k=1,kmx
    do j=1,jmx
      do i=1,imx
        ii = i + ioff
        if (ii .gt. imx) ii = ii - imx
        w(i) = ta(ii,j,k)
        wr(i) = tr(ii,j,k)
      end do
      do i=1,imx
        ta(i,j,k) = w(i);
        tr(i,j,k) = wr(i);
      end do
    end do
  end do

  do j=1,jmx
    do i=1,imx
      do k=mask(i,j)+1,kmx
        ta(i,j,k) = spv
        tr(i,j,k) = spv
      end do
      do k=1,mask(i,j)
        if (ta(i,j,k) .eq. mV) then
          ta(i,j,k) = tr(i,j,k)
        end if
      end do
    end do
  end do

  tmx = 1.0
  do k=1,kmx
    do j=1,jmx
      do i=1,imx
        if (ta(i,j,k) .ne. spv) then
          ta(i,j,k) = ta(i,j,k) - tr(i,j,k)
          ta(i,j,k) = abs(ta(i,j,k))
          if (ta(i,j,k) .gt. tmx) tmx = ta(i,j,k)
        end if
      end do
    end do
  end do

  do k=1,kmx
    do j=1,jmx
      do i=1,imx
        if (ta(i,j,k) .ne. spv) then
          ta(i,j,k) = ta(i,j,k) / tmx
          ta(i,j,k) = sqrt(ta(i,j,k))
        end if
      end do
    end do
  end do

! modify initial Tz estimate of variance

  do k=1,kmx
    do j=1,jmx
      do i=1,imx
        if (ta(i,j,k) .ne. spv) then
          b(i,j,k) = b(i,j,k) + ta(i,j,k)*afctr
        end if
      end do
    end do
  end do

  return
!
  410 write(6,'(a)') 'Error opening file TanF_WOA09_M4.nc'
      call w3tage('GODAS_MKEVNC4R')
      call errexit(41)
!
  420 write(6,'(a)') 'Error reading file TanF_WOA09_M4.nc'
      call w3tage('GODAS_MKEVNC4R')
      call errexit(42)
!
  430 write(6,'(a)') 'TanF_WOA09_M4.nc has wrong dimensions'
      call w3tage('GODAS_MKEVNC4R')
      call errexit(43)

!
  end subroutine getATemp

!
! --------------------------------------------------------------
!
  subroutine nrmFile
!
  integer :: i, j, k
  real :: lts, ltn, lgw, lge, bmx, fctr
  
  bmx = 0.0
  lts = real(ltS0)
  ltn = real(ltN0)
  lgw = real(lgW0)
  lge = real(lgE0)
  do j=1,jmx
    if (yt(j) .gt. ltn) exit
    if (yt(j) .ge. lts) then
      do i=1,imx
        if (xt(i) .gt. lge) exit
        if (xt(i) .ge. lgw) then
          do k=1,mask(i,j)
            if (b(i,j,k) .gt. bmx) bmx = b(i,j,k)
          end do
        end if
      end do
    end if
  end do
 
  fctr = gscl / bmx
  do j=1,jmx
    do i=1,imx
      do k=1,mask(i,j)
        b(i,j,k) = b(i,j,k) * fctr
      end do
    end do
  end do
!
  end subroutine nrmFile
!
! --------------------------------------------------------------
!
  subroutine cielFile
!
  integer :: i, j, k

  if (gciel .gt. 0.0) then
    do j=1,jmx
      do i=1,imx
        do k=1,mask(i,j)
          if (b(i,j,k) .gt. gciel) b(i,j,k) = gciel
        end do
      end do
    end do
  endif
!
  end subroutine cielFile
!
! --------------------------------------------------------------
!
  subroutine smthFile
!
  integer :: i, j, k, ii, i3, im, ip, jj, jm, jp, kk, km, kp, cnt
  integer :: iav, jav, kav

  iav = iav0
  jav = jav0
  kav = kav0
  
  if (iav .ne. 0 .and. jav .ne. 0) then
    iav = iav / 2
    jav = jav / 2
    do k=1,kmx
      do j=1,jmx
        jm = j-jav
        if (jm .lt. 1) jm = 1
        jp = j+jav
        if (jp .gt. jmx) jp = jmx
        do i=1,imx
          if (k .le. mask(i,j)) then
            im = i-iav
            ip = i+iav
            cnt = 0
            a(i,j,k) = 0.0
            do jj=jm,jp
              do i3=im,ip
                ii = i3
                if (ii .lt. 1) ii = ii + imx
                if (ii .gt. imx) ii = ii - imx
                if (k .le. mask(ii,jj)) then
                  a(i,j,k) = a(i,j,k) + c(ii,jj,k)
                  cnt = cnt + 1
                end if
              end do
            end do
            if (cnt > 0) a(i,j,k) = a(i,j,k) / real(cnt)
          else
            a(i,j,k) = spv
          end if
        end do
      end do
    end do
    iav = 2*iav + 1
    jav = 2*jav + 1
  else
    a = c
  end if
  
  if (kav .gt. 1) then
    kav = kav / 2
    do j=1,jmx
      do i=1,imx
        do k=1,kmx
          if (k .le. mask(i,j)) then
            km = k - kav
            if (km .lt. 1) km = 1
            kp = k + kav
            if (kp .gt. kmx) kp = kmx
            cnt = 0
            c(i,j,k) = 0.0
            do kk=km,kp
              if (kk .le. mask(i,j)) then
                c(i,j,k) = c(i,j,k) + a(i,j,kk)
                cnt = cnt + 1
              end if
            end do
            if (cnt .gt. 0) c(i,j,k) = c(i,j,k) / real(cnt)
          else
            c(i,j,k) = spv
          end if
        end do
      end do
    end do
    kav = 2*kav + 1
  else
    c = a
  end if
!
  end subroutine smthFile
!
! --------------------------------------------------------------
!
  subroutine fltrFile
!
  integer :: i, j, k, kt
  real :: tfz, bmx, zmx
  
  if (SdZ) then
    do j=1,jmx
      do i=1,imx
        do k=1,mask(i,j)
          b(i,j,k) = b(i,j,k) / dz(k)
        end do
      end do
    end do
  end if
!
  if (SqrR) then
    do j=1,jmx
      do i=1,imx
        do k=1,mask(i,j)
          if (b(i,j,k) .gt. 0.0) then
            b(i,j,k) = sqrt(b(i,j,k))
            if (SqrR2) then
              b(i,j,k) = sqrt(b(i,j,k))
            end if
          else
            b(i,j,k) = 0.0
          end if
        end do
      end do
    end do
  end if
!
  if (zTpr .or. zTpr2) then
    zmx = zt(kmx)
    do j=1,jmx
      do i=1,imx
        kt = 0
        bmx = 0.0
        do k=1,mask(i,j)
          if (b(i,j,k) .gt. bmx) then
            bmx = b(i,j,k)
            kt = k
          end if
        end do
        do k=1,mask(i,j)
          tfz = (zmx-zt(k) + zTpf*(zt(k)-zt(+kt))) / (zmx-zt(kt))
          if (zTpr2) tfz = tfz * tfz
          b(i,j,k) = b(i,j,k) * tfz
        end do
      end do
    end do
  end if
!
  end subroutine fltrFile
!
! --------------------------------------------------------------
!
  subroutine sfcMnMx
!
  integer :: i, j, k, klv

  do j=1,jmx
    do i=1,imx
      if (b(i,j,1) .lt. VsMn) then
        klv = -1
        do k=1,mask(i,j)
          if (b(i,j,k) .ge. VsMn) then
            klv = k
            exit
          end if
        end do
        do k=1,klv-1
          b(i,j,k) = VsMn
        end do
      else if (b(i,j,1) .gt. VsMx) then
        klv = -1
        do k=1,mask(i,j)
          if (b(i,j,k) .le. VsMx) then
            klv = k
            exit
          end if
        end do
        do k=1,klv-1
          b(i,j,k) = VsMx
        end do
      end if
    end do
  end do
!
  end subroutine sfcMnMx
!
! --------------------------------------------------------------
!
  subroutine hFill
!
  integer :: i, j, k, jm2, ii, jj, n
  integer :: ijd, im, ip, jm, jp, cn
  real :: bf, eps

  jm2 = jmx / 2 + 1

  do k=1,kmx
    do n=1,100
      vbin(n) = 0
    end do

    do j=1,jmx
      do i=1,imx
        if (k .le. mask(i,j)) then
          n = b(i,j,k) * 100.0
          if (n .gt. 99) n = 99
          n = n + 1
          vbin(n) = vbin(n) + 1
        end if
      end do
    end do

    i = 2
    do n=2,100
      if (vbin(n) .gt. vbin(i)) i = n
    end do
    eps = real(i-1) / 100.0
    if (eps .gt. epsmx) eps = epsmx

    do j=jm2,1,-1
      do i=1,imx
        if (k .le. mask(i,j) .and. b(i,j,k) .lt. eps) then
          cn = 0
          bf = 0.0
          ijd = 0
          do while (cn .eq. 0)
            ijd = ijd + 1
            jp = j+ijd
            if (jp .gt. jmx) jp = jmx
            jm = j-ijd
            if (jm .lt. 1) jm = 1
            ip = i+ijd
            im = i-ijd
            if (im .lt. 1) then
              im = im + imx
              do jj=jm,jp
                do ii=im,imx
                  if (k .le. mask(ii,jj) .and. b(ii,jj,k) .ge. eps) then
                    bf = bf + b(ii,jj,k)
                    cn = cn + 1
                  end if
                end do
              end do
              do jj=jm,jp
                do ii=1,ip
                  if (k .le. mask(ii,jj) .and. b(ii,jj,k) .ge. eps) then
                    bf = bf + b(ii,jj,k)
                    cn = cn + 1
                  end if
                end do
              end do
            else if (ip .gt. imx) then
              do jj=jm,jp
                do ii=im,imx
                  if (k .le. mask(ii,jj) .and. b(ii,jj,k) .ge. eps) then
                    bf = bf + b(ii,jj,k)
                    cn = cn + 1
                  end if
                end do
              end do
              ip = ip - imx
              do jj=jm,jp
                do ii=1,ip
                  if (k .le. mask(ii,jj) .and. b(ii,jj,k) .ge. eps) then
                    bf = bf + b(ii,jj,k)
                    cn = cn + 1
                  end if
                end do
              end do
            else
              do jj=jm,jp
                do ii=im,ip
                  if (k .le. mask(ii,jj) .and. b(ii,jj,k) .ge. eps) then
                    bf = bf + b(ii,jj,k)
                    cn = cn + 1
                  end if
                end do
              end do
            end if
          end do
          b(i,j,k) = bf / real(cn)
        end if
      end do
    end do

    do j=jm2,jmx
      do i=1,imx
        if (k .le. mask(i,j) .and. b(i,j,k) .lt. eps) then
          cn = 0
          bf = 0.0
          ijd = 0
          do while (cn .eq. 0)
            ijd = ijd + 1
            jp = j+ijd
            if (jp .gt. jmx) jp = jmx
            jm = j-ijd
            if (jm .lt. 1) jm = 1
            ip = i+ijd
            im = i-ijd
            if (im .lt. 1) then
              im = im + imx
              do jj=jm,jp
                do ii=im,imx
                  if (k .le. mask(ii,jj) .and. b(ii,jj,k) .ge. eps) then
                    bf = bf + b(ii,jj,k)
                    cn = cn + 1
                  end if
                end do
              end do
              do jj=jm,jp
                do ii=1,ip
                  if (k .le. mask(ii,jj) .and. b(ii,jj,k) .ge. eps) then
                    bf = bf + b(ii,jj,k)
                    cn = cn + 1
                  end if
                end do
              end do
            else if (ip .gt. imx) then
              do jj=jm,jp
                do ii=im,imx
                  if (k .le. mask(ii,jj) .and. b(ii,jj,k) .ge. eps) then
                    bf = bf + b(ii,jj,k)
                    cn = cn + 1
                  end if
                end do
              end do
              ip = ip - imx
              do jj=jm,jp
                do ii=1,ip
                  if (k .le. mask(ii,jj) .and. b(ii,jj,k) .ge. eps) then
                    bf = bf + b(ii,jj,k)
                    cn = cn + 1
                  end if
                end do
              end do
            else
              do jj=jm,jp
                do ii=im,ip
                  if (k .le. mask(ii,jj) .and. b(ii,jj,k) .ge. eps) then
                    bf = bf + b(ii,jj,k)
                    cn = cn + 1
                  end if
                end do
              end do
            end if
          end do
          b(i,j,k) = bf / real(cn)
        end if
      end do
    end do
  end do
!
  end subroutine hFill
!
! --------------------------------------------------------------
!
  subroutine mkSEv
!
  integer :: i, j, k, km

  allocate(bs(imx,jmx,kmx))

  bs = b

  do j=1,jmx
    do i=1,imx
      km = 1
      do k=1,mask(i,j)
        if (bs(i,j,k) .gt. Tzmn) km = k
      end do
      do k=1,km
        bs(i,j,k) = 1.0
      end do
      do k=km+1,mask(i,j)
        bs(i,j,k) = bs(i,j,k) / Tzmn
      end do

      if (Sm0) then
        if (mlk(i,j) .gt. 0) then
          do k=1,mlk(i,j)
            bs(i,j,k) = 0.0
          end do
          k = mlk(i,j) + 1
          if (k .lt. mask(i,j)) then
            k = k - 1
            bs(i,j,k) = 0.5 * bs(i,j,k+1)
          end if
        end if
      end if

      do k=1,mask(i,j)
        bs(i,j,k) = bs(i,j,k) * sfctr
      end do
    end do
  end do
!
  end subroutine mkSEv
!
! --------------------------------------------------------------
!
  subroutine writeFile
!
  integer :: i, ii, j, k

  allocate(ev(imx,jmx))

  open(51, status='replace', form='unformatted', access='sequential', err=510)
  write(51, err=520) year, month, day
  write(51, err=520) imx, jmx, kass
  do k=1,kass
    do i=1,imx
      ii = i - ioff
      if (ii .lt. 1) ii = ii + imx
      do j=1,jmx
        ev(i,j) = b(ii,j,k)
      end do
    end do
    write(51, err=520) ev
  end do
  close(51)
!
  if (SVar) then

    open(52, status='replace', form='unformatted', access='sequential', err=530)
    write(52, err=540) year, month, day
    write(52, err=540) imx, jmx, kass
    do k=1,kass
      do i=1,imx
        ii = i - ioff
        if (ii .lt. 1) ii = ii + imx
        do j=1,jmx
          ev(i,j) = bs(ii,j,k)
        end do
      end do
      write(52, err=520) ev
    end do
    close(52)
  end if
!
  return
!
  510 write(6,'(a)') 'Error opening temp bkgrd error file'
      call w3tage('GODAS_MKEVNC4R')
      call errexit(51)
!
  520 write(6,'(a)') 'Error writing temp bkgrd error file'
      call w3tage('GODAS_MKEVNC4R')
      call errexit(52)
!
  530 write(6,'(a)') 'Error opening salt bkgrd error file'
      call w3tage('GODAS_MKEVNC4R')
      call errexit(53)
!
  540 write(6,'(a)') 'Error writing salt bkgrd error file'
      call w3tage('GODAS_MKEVNC4R')
      call errexit(54)
!
  end subroutine writeFile
!
! --------------------------------------------------------------
!
  end program mkEvNc4r
