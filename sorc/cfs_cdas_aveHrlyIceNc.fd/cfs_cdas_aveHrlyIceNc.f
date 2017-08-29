  program aveHrlyNc
!
!  This program is intended for averaging hourly mean
!    netCDF files
!
  include 'netcdf.inc'
!
  character(len=30) :: aname, dname, vname
  integer status, ncid, ncout, ndims, nvars, natts, idunlm
  integer nfiles, nsiz
  integer nvdm, nvatts, vtype, atype, len
  integer, allocatable, dimension(:)            :: dsiz
  real(kind=4), allocatable, dimension(:)       :: var, avar
  integer, dimension(4)                         :: start, count, vdm
  real(kind=8) :: Tdbl, aT1, aT2, dT, Tbnd(2), rT
  real(kind=4) :: mVal
  character(len=255), allocatable, dimension(:) :: inFile
  character(len=255)                            :: outFile
  character(len=80)                             :: str
  integer, parameter :: nfix = 17
  character(len=20), dimension(17) :: fxFld
  data fxFld /'xt', 'xb', 'yt', 'yb', 'time', 'nv', 'ct', 'xv', 'yv', &
   'GEOLON', 'GEOLAT', 'SINROT', 'COSROT', &
   'average_T1', 'average_T2', 'average_DT', 'time_bounds'/
  logical, allocatable, dimension(:)           :: doAve, isTime
!
! get command line arguments
!
  nfiles = iargc()
  if (nfiles .eq. 0) then
    write(6,'(a)') 'The files must be arguments. The format is'
    write(6,'(a)') '  aveHrlyNc aveFile inFile1.nc inFile2.nc inFile3.nc ...'
    call exit(99)
  endif
!
  call getarg(1,outFile)
  nfiles = nfiles - 1
  allocate (inFile(nfiles))
  do n=1,nfiles
    call getarg(n+1,inFile(n))
  enddo
!
! open first netCDF dataset
!
  status = nf_open(inFile(1), 0, ncid)
  if (status .ne. NF_NOERR) then
    write(6,'(a,a)') 'Could not open ', inFile(1)
    call exit(0)
  endif
!
! create new averaged netCDF dataset
!
  status = nf_create(outFile, NF_NOCLOBBER, ncout)
  if (status .ne. NF_NOERR) then
    write(6,'(a,a)') 'Could not create ', outFile
    call exit(0)
  endif
!
! inquire about the file
!
  status = nf_inq(ncid, ndims, nvars, natts, idunlm)
  if (status .ne. NF_NOERR) then
    write(6,'(a,a)') 'Could not inquire about file ', inFile(1)
    call exit(0)
  endif
!
! inquire about the dimensions and create dimensions in new dataset
!
  allocate (dsiz(ndims))
  do n = 1,ndims
    status = nf_inq_dim(ncid, n, dname, dsiz(n))
    if (status .ne. NF_NOERR) then
      write(6,'(a,a)') 'Could not inquire about dimensions in ', inFile(1)
      call exit(0)
    endif
    status = nf_def_dim(ncout, dname, dsiz(n), m)
    if (status .ne. NF_NOERR) then
      write(6,'(a,a)') 'Could not create dimensions in ', outFile
      call exit(0)
    endif
  enddo
!
! inquire about the variables and create variables in new dataset
! inquire about the variable attributes and create them in new dataset
!
  allocate (doAve(nvars))
  allocate (isTime(nvars))
  do n = 1,nvars
    status = nf_inq_var(ncid, n, vname, vtype, nvdm, vdm, nvatts)
    if (status .ne. NF_NOERR) then
      write(6,'(a,a)') 'Could not inquire about variables in ',inFile(1)
      call exit(0)
    endif
    doAve(n) = .true.
    isTime(n) = .false.
    do m=1,nfix
      if (vname .eq. fxFld(m)) doAve(n) = .false.
      if (vtype .eq. NF_DOUBLE) isTime(n) = .true.
    enddo
    status = nf_def_var(ncout, vname, vtype, nvdm, vdm, m)
    if (status .ne. NF_NOERR) then
      write(6,'(a,a)') 'Could not create variables in ',outFile
      call exit(0)
    endif
    if (nvatts .gt. 0) then
      do m = 1,nvatts
        status = nf_inq_attname(ncid, n, m, aname)
        if (status .ne. NF_NOERR) then
          write(6,'(a,a)') 'Error reading attribute of ', vname
          call exit(0)
        endif
        status = nf_copy_att(ncid, n, aname, ncout, n)
      enddo
    endif
  enddo
!
! inquire about global attributes and create them in new dataset
!
  status = nf_inq_varnatts(ncid, NF_GLOBAL, nvatts)
  do m = 1,nvatts
    status = nf_inq_attname(ncid, NF_GLOBAL, m, aname)
    if (aname(1:8) .eq. 'filename') then
      ln1 = lnstr(outFile)
      status = nf_put_att_text(ncout, NF_GLOBAL, aname, ln1, outFile)
    else
      status = nf_copy_att(ncid, NF_GLOBAL, aname, ncout, NF_GLOBAL)
    endif
  enddo
!
  status = nf_enddef(ncout)
!
  allocate (var(2))
  do n = 1,nvars
    if (.not. doAve(n)) then
      status = nf_inq_var(ncid, n, vname, vtype, nvdm, vdm, nvatts)
      nsiz = 1
      do m = 1,nvdm
        start(m) = 1
        count(m) = dsiz(vdm(m))
        nsiz = nsiz * dsiz(vdm(m))
      enddo
      if (.not. isTime(n)) then
        deallocate (var)
        allocate (var(nsiz))
        status = nf_get_vara_real(ncid, n, start, count, var)
        status = nf_put_vara_real(ncout, n, start, count, var)
      else
        if (vname .eq. 'time') then
          status = nf_get_vara_double(ncid, n, start, count, Tdbl)
        else if (vname .eq. 'average_T1') then
          status = nf_get_vara_double(ncid, n, start, count, aT1)
        else if (vname .eq. 'average_T2') then
          status = nf_get_vara_double(ncid, n, start, count, aT2)
        else if (vname .eq. 'average_DT') then
          status = nf_get_vara_double(ncid, n, start, count, dT)
        else if (vname .eq. 'time_bounds') then
          status = nf_get_vara_double(ncid, n, start, count, Tbnd)
        endif
      endif
    endif
  enddo
!
  status = nf_close(ncid)
!
  status = nf_open(inFile(nfiles), 0, ncid)
  if (status .ne. NF_NOERR) then
    write(6,'(a,a)') 'Could not open ', inFile(nfiles)
    call exit(0)
  endif
!
  do n = 1,nvars
    if (.not. doAve(n) .and. isTime(n)) then
      start(1) = 1
      count(1) = 1
      status = nf_inq_var(ncid, n, vname, vtype, nvdm, vdm, nvatts)
      if (vname .eq. 'time') then
        status = nf_get_vara_double(ncid, n, start, count, rT)
        Tdbl = 0.5*(Tdbl + rT)
      else if (vname .eq. 'average_T2') then
        status = nf_get_vara_double(ncid, n, start, count, aT2)
      else if (vname .eq. 'time_bounds') then
        count(1) = 2
        rT = Tbnd(1)
        status = nf_get_vara_double(ncid, n, start, count, Tbnd)
        Tbnd(1) = rT
      endif
    endif
  enddo
!
  dT = aT2 - aT1
!
  do n = 1,nvars
    if (.not. doAve(n) .and. isTime(n)) then
      start(1) = 1
      count(1) = 1
      status = nf_inq_var(ncid, n, vname, vtype, nvdm, vdm, nvatts)
      if (vname .eq. 'time') then
        status = nf_put_vara_double(ncout, n, start, count, Tdbl)
      else if (vname .eq. 'average_T1') then
        status = nf_put_vara_double(ncout, n, start, count, aT1)
      else if (vname .eq. 'average_T2') then
        status = nf_put_vara_double(ncout, n, start, count, aT2)
      else if (vname .eq. 'average_DT') then
        status = nf_put_vara_double(ncout, n, start, count, dT)
      else if (vname .eq. 'time_bounds') then
        count(1) = 2
        status = nf_put_vara_double(ncout, n, start, count, Tbnd)
      endif
    endif
  enddo
!
  status = nf_close(ncid)
!
  allocate (avar(2))
  do n = 1,nvars
    if (doAve(n)) then
!
      status = nf_open(inFile(1), 0, ncid)
      status = nf_inq_var(ncid, n, vname, vtype, nvdm, vdm, nvatts)
      nsiz = 1
      do m = 1,nvdm
        start(m) = 1
        count(m) = dsiz(vdm(m))
        nsiz = nsiz * dsiz(vdm(m))
      enddo
      deallocate (var)
      deallocate (avar)
      allocate (var(nsiz))
      allocate (avar(nsiz))
      status = nf_get_vara_real(ncid, n, start, count, var)
      status = nf_get_att_real(ncid, n, 'missing_value', mVal)
      status = nf_close(ncid)
!
      do m=1,nsiz
        if (var(m) .ne. mVal) then
          avar(m) = var(m)
        else
          avar(m) = var(m)
        endif
      enddo
!
      do nf = 2,nfiles
        status = nf_open(inFile(nf), 0, ncid)
        status = nf_get_vara_real(ncid, n, start, count, var)
        status = nf_close(ncid)
!
        do m=1,nsiz
          if (var(m) .ne. mVal) then
            avar(m) = avar(m) + var(m)
          endif
        enddo
      enddo
!
      do m=1,nsiz
        if (var(m) .ne. mVal) then
          avar(m) = avar(m) / float(nfiles)
        endif
      enddo
!
      status = nf_put_vara_real(ncout, n, start, count, avar)
    endif
  enddo
!
  status = nf_close(ncout)
!
  end program aveHrlyNc
!
! --------------------------------------------------------------------
!
  function lnstr(s)
!
  character*(*) s
  integer n, nmax
!
  nmax = len(s)
  n = 0
  do while (s(n+1:n+1) .ne. ' ' .and. n .lt. nmax)
    n = n+1
  enddo
!
  lnstr = n
!
  end
!
