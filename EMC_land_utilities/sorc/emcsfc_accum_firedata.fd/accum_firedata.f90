 program accum_firedata
!$$$  main program documentation block
!                .      .    .                                       .
! main program: accum_firedata
!   prgmmr: gayno            ORG: NP2                DATE: 2015-JAN-28
!
! $Revision$
!
! abstract: Ingest fire weather files (% burned area) valid over 
! a 6- or 12-hour period.  Accumulate these 6- or 12-hourly values 
! over a user-specified time period (e.g., days or months).  Output
! the result to a grib 2 file.
!
! program history log:
!   2015-jan-28  gayno     initial version
!
! input files:
!   - fort.41 configuration namelisit
!   - burned area data in grib 2.  The file naming convention is:
!
!     "burned_area_$yyyymmdd_$hh1_$hh2_12km.grib2" (12km data)
!     "burned_area_$yyyymmdd_$hh1_$hh2_1km.grib2" (1km data)
!
!     where $hh1 is the hour of the beginning of the period and
!     $hh2 is the hour of the end of the period.  The 12 km
!     data is valid over a 6-hour period.  The 1 km data is
!     valid over a 12-hour period.  All data is on a 
!     lambert conformal grid.
!   
! output files:
!   - burned area data accumulated over a user-specified time
!     range (in grib2) 
!
! remarks: This program assumes all input data is on the same grid.
! It does not horizontally interpolate. 
!   
! condition codes (non-zero is fatal):
!   cond =   0 - successful run
!           30 - bad open of fort.41 config namelist
!           31 - bad read of fort.41 config namelist
!           55 - must process 1km or 12km burned area files
!           60 - bad open of accumulated burned area file
!           61 - bad write of accumulated burned area file
!
!$$$
!
 use grib_mod

 implicit none

 include 'mpif.h'
 integer, external :: iw3jdn

 character*150 :: input_file_dir
 character*100 :: output_file
 character*250 :: fire_file
 character*4   :: year
 character*2   :: month, day
 character*7   :: hour_string_12km_file(0:3)
 character*7   :: hour_string_1km_file(0:1)

 integer :: count_gt_100, count_gt_200, count_gt_300
 integer :: start_date(8), end_date(8), remaining_files, files_per_task
 integer :: start_date_task(8), end_date_task(8), curr_date(8)
 integer :: hour_1km(0:1), hour_12km(0:3)
 integer :: idim, jdim, lat11, lon11, resol
 integer :: start_year, start_month, start_day, start_hour
 integer :: end_year, end_month, end_day, end_hour
 integer :: accum_files, count_rec_task, count_rec_all_task
 integer :: npts, nprocs, num_files, file_interval_hrs
 integer :: ierr, ierr2, myrank, lugb, lugi, n, m, i
 integer :: j, k, jdisc, jgdtn, jpdtn, jids(200), jgdt(200), jpdt(200)
 integer :: yy, mm, dd, hh
 integer :: first_julday_read_task, last_julday_read_task
 integer :: first_julhr_read_task, last_julhr_read_task
 integer :: first_julhr_read, last_julhr_read
 integer :: file_resolution_in_km, files_per_day
 integer, allocatable :: files_each_task(:)

 logical :: save_first, unpack, exists

 real,allocatable :: sum(:), sum_all(:)
 real             :: rinc(5)

 type(gribfield)  :: gfld_firedat

 data hour_string_12km_file /'_00_05_', '_06_11_', '_12_17_', '_18_23_'/
 data hour_string_1km_file  /'_00_11_', '_12_23_'/

 data hour_12km /05, 11, 17, 23/
 data hour_1km  /11, 23/

 namelist /setup/ start_year, start_month, start_day, start_hour, &
                  end_year, end_month, end_day, end_hour, &
                  file_resolution_in_km, &
                  input_file_dir, output_file

 call mpi_init(ierr)
 call mpi_comm_rank(mpi_comm_world, myrank, ierr)
 call mpi_comm_size(mpi_comm_world, nprocs, ierr)

 if(myrank==0) call w3tagb('ACCUM_FIREDATA',2000,365,0,'EMC')

!------------------------------------------------------------------------
! read program configuration namelist.
!------------------------------------------------------------------------

 open (41, iostat=ierr)
 if (ierr /= 0) then
   print*,'- FATAL ERROR: BAD OPEN OF FORT.41 ', ierr
   call mpi_abort(mpi_comm_world, 30)
 endif
 read(41, nml=setup, iostat=ierr)
 if (ierr /= 0) then
   print*,'- FATAL ERROR: BAD READ OF FORT.41 ', ierr
   call mpi_abort(mpi_comm_world, 31)
 endif
 close(41)

!------------------------------------------------------------------------
! set input data grid specs.  currently, nesdis produces 1 and 12-km
! data.  data on a lambert conformal grid.
!------------------------------------------------------------------------

 select case (file_resolution_in_km)
   case (1)
     if(myrank==0) print*,'- PROCESS 1KM FILES'
     idim=6887       ! i-dimension of grid
     jdim=6610       ! j-dimension of grid
     npts=idim*jdim
     lat11=64997800  ! scaled lat of pt 1,1
     lon11=180492400 ! scaled lon of pt 1,1
     resol=1000000   ! resol in mm
     files_per_day=2
   case (12)
     if(myrank==0) print*,'- PROCESS 12KM FILES'
     idim=1033        ! i-dimension of grid
     jdim=723         ! j-dimension of grid
     npts=idim*jdim
     lat11=45745400   ! scaled lat of pt 1,1
     lon11=161722900  ! scaled lon of pt 1,1
     resol=12000000   ! resol in mm
     files_per_day=4
   case default
     if(myrank==0) print*,'- FATAL ERROR: MUST PROCESS 1 OR 12 KM FILES'
     call mpi_abort(mpi_comm_world, 55)
 end select

!------------------------------------------------------------------------
! based on the start/end of the accumulation period and the number
! of mpi tasks, determine the number of files to read per task.
!------------------------------------------------------------------------

 file_interval_hrs = 24 / files_per_day

 allocate (files_each_task(0:nprocs-1))

 start_date(1)=start_year
 start_date(2)=start_month
 start_date(3)=start_day
 start_date(4)=0  ! time zone, use utc
 start_date(5)=start_hour
 start_date(6:8)=0

 if (myrank==0) write(6,105) start_date(1:3),start_date(5)
 105 format(1x,"- START DATE (YYYYMMDDHH): ",i4,1x,i2,1x,i2,1x,i2)

 end_date(1)=end_year
 end_date(2)=end_month
 end_date(3)=end_day
 end_date(4)=0
 end_date(5)=end_hour
 end_date(6:8)=0

 if (myrank==0) write(6,106) end_date(1:3),end_date(5)
 106 format(1x,"- END DATE (YYYYMMDDHH):   ",i4,1x,i2,1x,i2,1x,i2)

 call w3difdat(end_date, start_date, 2, rinc)

 num_files=rinc(2)/file_interval_hrs  ! total # files to read

 files_per_task = num_files / nprocs

 files_each_task=0
 if (files_per_task==0) then
   do n = 0, (num_files-1)
     files_each_task(n)= 1
   enddo
 else
   do n = 0, (nprocs-1)
     files_each_task(n)= files_per_task
   enddo
   remaining_files = mod (num_files, nprocs)
   if (remaining_files > 0) then
     do n = 0, (remaining_files-1)
       files_each_task(n) = files_each_task(n) + 1
     enddo
   endif
 endif

 if(myrank==0) then
   do n = 0, (nprocs-1)
     print*,'- PROCESS ', files_each_task(n), ' FILES FOR TASK: ', n
   enddo
 endif

!------------------------------------------------------------------------
! For each task, determine the start/end of the accumulation period.
!------------------------------------------------------------------------

 allocate(sum(npts))
 sum=0.0
 count_rec_task=0

 first_julhr_read_task=99999999
 first_julday_read_task=99999999
 last_julhr_read_task=0
 last_julday_read_task=0

 if (files_each_task(myrank) == 0) goto 55

 accum_files=0
 if (myrank > 0) then
   do n = 0, (myrank-1)
     accum_files = accum_files + files_each_task(n)*file_interval_hrs
   enddo
 end if

 rinc=0.
 rinc(2)=accum_files

 call w3movdat(rinc, start_date, start_date_task)

 rinc=0.
 rinc(2)=files_each_task(myrank)*file_interval_hrs - file_interval_hrs

 call w3movdat(rinc, start_date_task, end_date_task)

 write(6,108) myrank, start_date_task(1:3),start_date_task(5)
 108 format(1x,"- TASK",1x,i2," START DATE (YYYYMMDDHH): ",i4,1x,i2,1x,i2,1x,i2)

 write(6,109) myrank, end_date_task(1:3), end_date_task(5)
 109 format(1x,"- TASK",1x,i2," END DATE (YYYYMMDDHH):   ",i4,1x,i2,1x,i2,1x,i2)

!------------------------------------------------------------------------
! Now read in burned area grib2 data.
!------------------------------------------------------------------------

 nullify(gfld_firedat%idsect)
 nullify(gfld_firedat%local)
 nullify(gfld_firedat%list_opt)
 nullify(gfld_firedat%igdtmpl)
 nullify(gfld_firedat%ipdtmpl)
 nullify(gfld_firedat%coord_list)
 nullify(gfld_firedat%idrtmpl)
 nullify(gfld_firedat%bmap)
 nullify(gfld_firedat%fld)

 save_first=.true.

 do n = 1, files_each_task(myrank)

   rinc=0.0
   rinc(2)= float(n-1)*file_interval_hrs
   call w3movdat(rinc, start_date_task, curr_date)
   write(year,'(i4)') curr_date(1)
   write(month,'(i2.2)') curr_date(2)
   write(day,'(i2.2)') curr_date(3)

   m = curr_date(5)/file_interval_hrs

   if (file_resolution_in_km == 12) fire_file=trim(input_file_dir) // & 
       "/burned_area_" // year // month // day // hour_string_12km_file(m) // "12km.grib2"

   if (file_resolution_in_km == 1)  fire_file=trim(input_file_dir) // & 
       "/burned_area_" // year // month // day // hour_string_1km_file(m) // "1km.grib2"

!------------------------------------------------------------------------
! Some files may be missing.  If so skip to the next one.
!------------------------------------------------------------------------

   inquire (file=trim(fire_file), exist=exists)
   write(6,100) trim(fire_file), exists

 100 format(1x,"- LOOK FOR FILE: ",a,l)

   if (exists) then

     lugb=11  ! fortran unit number for input data

!------------------------------------------------------------------------
! WARNING!!! When recycling the same fortran unit number for different 
! files, you must force the regeneration of the index file.
! This is done by setting lugi=lugb.
!------------------------------------------------------------------------

     lugi=lugb

!------------------------------------------------------------------------
! If there is a bad open, skip to the next file.
!------------------------------------------------------------------------

     call baopenr(lugb, fire_file, ierr)
     if (ierr /= 0) then
       print*,'- WARNING: BAD OPEN: ',ierr
       print*,'- WILL SKIP OVER FILE.'
       cycle
     endif

!------------------------------------------------------------------------
! We know what the date, grid specs and parameter id of our input burned
! area data should be.  Tell the degribber to check to ensure it is
! what we expect by setting the 'j' arrays below.
!------------------------------------------------------------------------

     j = 0
     jdisc = 2  ! discipline - land surface products (table 0.0)

     jids = -9999            ! section 1 - identification section
     jids(6) = curr_date(1)  ! year
     jids(7) = curr_date(2)  ! month
     jids(8) = curr_date(3)  ! day
     if (file_resolution_in_km == 1) jids(9) = hour_1km(m)  ! hour
     if (file_resolution_in_km == 12) jids(9) = hour_12km(m)  ! hour

     jgdtn = 30           ! section 3 - grid definition template #;
                          ! lambert conformal grid 
     jgdt = -9999         ! grid definition template info
     jgdt(1) = 4          ! shape of earth
     jgdt(8) = idim       ! i-dimension of grid
     jgdt(9) = jdim       ! j-dimension of grid.
     jgdt(10) = lat11     ! corner point latitude
     jgdt(11) = lon11     ! corner point longitude
     jgdt(13)= 40000000   ! true lat (scaled by 10^6)
     jgdt(14)= 264000000  ! orient angle (scaled by 10^6)
     jgdt(15) = resol     ! i-direction resolution
     jgdt(16) = resol     ! j-direction resolution
     jgdt(18)= 0          ! scan mode
     jgdt(19)= 20000000   ! tangent lat 1 (scaled by 10^6)
     jgdt(20)= 60000000   ! tangent lat 2 (scaled by 10^6)

     jpdtn = 31      ! product def template #; satellite data product
     jpdt = -9999    ! product definition template info
     jpdt(1) = 4     ! parameter category, table 4.1 (fire weather)
     jpdt(2) = 3     ! parameter number, table 4.2 (burned area)
     unpack=.true.

     call gf_free2(gfld_firedat)  ! free up memory

     write(6,113) trim(fire_file)
 113 format(1x,"- FILE EXISTS. DEGRIB: ", a)

     call getgb2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
                 unpack, k, gfld_firedat, ierr)

     call baclose(lugb, ierr2)
     if (ierr/=0) then
       print*,'- WARNING: BAD DEGRIB. IERR IS: ', ierr
       print*,'- WILL SKIP OVER FILE.'
       cycle
     endif

     do i = 1, npts
       if (gfld_firedat%bmap(i)) then
         sum(i) = sum(i) + gfld_firedat%fld(i)
       else
         sum(i)=-9.99e6
       endif
     enddo

     if (save_first) then
       yy=gfld_firedat%idsect(6)
       mm=gfld_firedat%idsect(7)
       dd=gfld_firedat%idsect(8)
       hh=gfld_firedat%idsect(9)
       first_julday_read_task=iw3jdn(yy,mm,dd)
       first_julhr_read_task = first_julday_read_task*24 + hh
       save_first=.false.
     endif

     yy=gfld_firedat%idsect(6)
     mm=gfld_firedat%idsect(7)
     dd=gfld_firedat%idsect(8)
     hh=gfld_firedat%idsect(9)
     last_julday_read_task=iw3jdn(yy,mm,dd)
     last_julhr_read_task = last_julday_read_task*24 + hh
     count_rec_task=count_rec_task+1       

   else

     write(6,103)
103  format(1x,"- FILE NOT FOUND")

   endif  ! does file exist?

 enddo

 call gf_free2(gfld_firedat)  ! free up memory

 55 continue

 deallocate (files_each_task)

 write(6,120) myrank, count_rec_task
 120 format(1x, "- TASK", 1x, i2, " NUMBER OF RECORDS READ: ", i4)

 if (count_rec_task > 0) then
   write(6,121) myrank, first_julhr_read_task
   write(6,122) myrank, last_julhr_read_task
 endif

 121 format(1x, "- TASK", 1x, i2, " FIRST JULHR READ: ", i10)
 122 format(1x, "- TASK", 1x, i2, " LAST JULHR READ:  ", i10)

 call mpi_reduce(count_rec_task, count_rec_all_task, 1, mpi_integer, mpi_sum, 0, &
                 mpi_comm_world, ierr)

 call mpi_bcast(count_rec_all_task, 1, mpi_integer, 0, mpi_comm_world, ierr)

 if (myrank==0) write(6,124) count_rec_all_task
 124 format(1x, "- NUMBER OF RECORDS READ ALL TASKS: ",i5)

 if (count_rec_all_task > 0) then

   allocate(sum_all(npts))
   sum_all=0.0

   call mpi_reduce(sum, sum_all, npts, mpi_real, mpi_sum, 0, &
                   mpi_comm_world, ierr)

!------------------------------------------------------------------------
! Sometimes the accumulated burned area exceeds 100% of the grid box.
! Print out information about these points.  And cap the accumulated
! value at 100%.
!------------------------------------------------------------------------

   if (myrank==0)then
     count_gt_100=0
     count_gt_200=0
     count_gt_300=0
     do m = 1, npts
       if (sum_all(m) > 100.0) count_gt_100 = count_gt_100 + 1
       if (sum_all(m) > 200.0) count_gt_200 = count_gt_200 + 1
       if (sum_all(m) > 300.0) count_gt_300 = count_gt_300 + 1
       if (sum_all(m) > 100.0) then
         write(6,135) sum_all(m), mod((m-1),idim)+1, (m-1)/idim +1
         sum_all(m) = 100.0
       endif
     enddo
     print*,"- NUMBER OF POINTS GREATER THAN 100% ",count_gt_100
     print*,"- NUMBER OF POINTS GREATER THAN 200% ",count_gt_200
     print*,"- NUMBER OF POINTS GREATER THAN 300% ",count_gt_300
   endif

 135 format(1x,"- WARNING: ACCUMULATED SUM OF ",f6.1,"% AT POINT ",i5,1x,i5)

   call mpi_reduce(first_julhr_read_task, first_julhr_read, 1, mpi_integer, mpi_min, 0, &
                   mpi_comm_world, ierr)

   call mpi_reduce(last_julhr_read_task, last_julhr_read, 1, mpi_integer, mpi_max, 0, &
                   mpi_comm_world, ierr)

!------------------------------------------------------------------------
! Output the accumulated data to a grib 2 file.
!------------------------------------------------------------------------

   if (myrank==0) then
     write(6,130) first_julhr_read
     write(6,131) last_julhr_read
     call gribit(sum_all,npts,first_julhr_read,last_julhr_read,file_resolution_in_km, output_file)
   end if

 130 format(1x,"- FIRST JULHR READ IN, ALL TASKS: ",1x,i10)
 131 format(1x,"- LAST JULHR READ IN, ALL TASKS:  ",1x,i10)

   deallocate (sum_all)

 else

   if(myrank==0) print*,"- NO FILES READ IN, DON'T CREATE ACCUMLATED FIRE DATA FILE."

 endif

 deallocate (sum)

 if (myrank==0) then
   print*," "
   print*,"*** NORMAL TERMINATION ***"
   print*," " 
   call w3tage('ACCUM_FIREDATA')
 endif

 call mpi_finalize(ierr)

 stop 0

 end program accum_firedata

 subroutine gribit(sum_all, npts, first_julhr, last_julhr, file_resolution_in_km, output_file)
!$$$  subprogram documentation block
!
! subprogram:    gribit
!   prgmmr: gayno          org: w/np2     date: 2015-jan-28
!
! abstract:  write the accumulated burned area data to a grib 2 file.
!
! program history log:
! 2015-jan-28  gayno    - initial version
!
! usage: call gribit(sum_all, npts, first_julhr, last_julhr,
!                    file_resolution_in_km, output_file)
!
!   input argument list:
!     sum_all               - accumulated burned area data in %
!     npts                  - number of grid points
!     first_julhr           - julian hour of beginning of accumulation period
!     last_julhr            - julian hour of end of accumulation period
!     file_resolution_in_km - take a guess
!     output_file           - name of output grib 2 file
!
!   output argument list:
!     none
!
! files:
!   input: none
!
!   output:
!     - accumulated burned area data in grib 2 format
!
! conditon codes:  all fatal
!   60 - bad open of output file
!   61 - bad write of output file
!
! remarks: none
!
!$$$
!
 use grib_mod

 implicit none

 include 'mpif.h'

 character*100, intent(in) :: output_file

 integer, intent(in) :: npts
 integer, intent(in) :: first_julhr
 integer, intent(in) :: last_julhr
 integer, intent(in) :: file_resolution_in_km

 integer             :: first_julhr_grib
 integer             :: last_julhr_grib
 integer             :: first_juldy_grib
 integer             :: last_juldy_grib
 integer             :: lugb, iret, num_hours
 integer             :: idim, jdim, lat11, lon11, resol
 integer             :: iyear,month,iday,idaywk,idayyr

 real, intent(in)    :: sum_all(npts)

 type(gribfield)     :: gfld

!------------------------------------------------------------------------
! Set some grid specs based on whether this is the 1 or 12km data.
!------------------------------------------------------------------------

 select case (file_resolution_in_km)
   case (1)
     idim=6887        ! i-dimension of grid
     jdim=6610        ! j-dimension of grid
     lat11=64997800   ! corner point lat scaled by 10^6
     lon11=180492400  ! corner point lon scaled by 10^6
     resol=1000000    ! resolution in mm
     first_julhr_grib = first_julhr-11  ! want the beginning of the
                                        ! accumulation period.
   case (12)
     idim=1033        ! i-dimension of grid
     jdim=723         ! j-dimension of grid
     lat11=45745400   ! corner point lat scaled by 10^6
     lon11=161722900  ! corner point lon scaled by 10^6
     resol=12000000   ! resolution in mm
     first_julhr_grib = first_julhr-5   ! want the beginning of the
                                        ! accumulation period.
 end select

 last_julhr_grib = last_julhr + 1  ! adjust nesdis time to be the
                                   ! end of a 6-hrly period

 nullify(gfld%idsect)
 nullify(gfld%local)
 nullify(gfld%list_opt)
 nullify(gfld%igdtmpl)
 nullify(gfld%ipdtmpl)
 nullify(gfld%coord_list)
 nullify(gfld%idrtmpl)
 nullify(gfld%bmap)
 nullify(gfld%fld)

! section 0
 gfld%version=2
 gfld%discipline=2

! section 1
 gfld%idsectlen=13
 allocate(gfld%idsect(gfld%idsectlen))

 gfld%idsect(1)=7       ! orig center, octs 6-7
 gfld%idsect(2)=4       ! sub center, octs 8-9
 gfld%idsect(3)=9       ! master table ver #, oct 10
 gfld%idsect(4)=0       ! local table, oct 11
 gfld%idsect(5)=0       ! signif of ref time, oct 12

 first_juldy_grib=first_julhr_grib / 24
 call w3fs26(first_juldy_grib,iyear,month,iday,idaywk,idayyr)

 gfld%idsect(6)=iyear   ! reference year, octs 13-14
 gfld%idsect(7)=month   ! reference month, oct 15
 gfld%idsect(8)=iday    ! reference day, oct 16
 gfld%idsect(9)=mod(first_julhr_grib,24) ! reference hour, oct 17
 gfld%idsect(10:11)=0   ! reference minute and second, octs 18-19
 gfld%idsect(12)=0      ! production status, oct 20
 gfld%idsect(13)=0      ! type of processed data, oct 21

! section 2 - local secion not used
 gfld%locallen=0

! section 3
 gfld%igdtlen=22
 allocate(gfld%igdtmpl(gfld%igdtlen))

 gfld%griddef=0              ! source of grid definition
 gfld%igdtnum=30             ! grid def template number - lambert conf.
 gfld%igdtmpl(1)= 4          ! shape of earth, oct 15
 gfld%igdtmpl(2)= 255        ! scale factor spherical earth, oct 16
 gfld%igdtmpl(3)= -1         ! scale value sphierical earth, octs 17-20
 gfld%igdtmpl(4)= 255        ! scale factor major axis ellip earth, oct 21
 gfld%igdtmpl(5)= -1         ! scale value major axis ellip earth, octs 22-25
 gfld%igdtmpl(6)= 255        ! scale factor minor axis ellip earth, oct 26
 gfld%igdtmpl(7)= -1         ! scale value major axis ellip earth, octs 27-30
 gfld%igdtmpl(8)= idim       ! i-dimension of grid, octs 31-34
 gfld%igdtmpl(9)= jdim       ! j-dimension of grid, octs 35-38
 gfld%igdtmpl(10)= lat11     ! corner point latitude, octs 39-42
 gfld%igdtmpl(11)= lon11     ! corner point longitude, octs 43-46
 gfld%igdtmpl(12)= 48        ! resolution and component flags, oct 47
 gfld%igdtmpl(13)= 40000000  ! true lat in millidegrees, octs 48-51
 gfld%igdtmpl(14)= 264000000 ! orient angle in millidegrees, octs 52-55
 gfld%igdtmpl(15)= resol     ! i-direction grid length, octs 56-59
 gfld%igdtmpl(16)= resol     ! j-direction grid length, octs 60-63
 gfld%igdtmpl(17)= 0         ! projection center flag, oct 64
 gfld%igdtmpl(18)= 0         ! scan mode, oct 65
 gfld%igdtmpl(19)= 20000000  ! secant cone lat 1 in millideg, octs 66-69
 gfld%igdtmpl(20)= 60000000  ! secant cone lat 2 in millideg, octs 70-73
 gfld%igdtmpl(21)= -90000000 ! lat of south pole, octs 74-77
 gfld%igdtmpl(22)= 0         ! lon of south pole, octs 78-81
 gfld%interp_opt=0           ! no appended list of numbers at end of sec 3 - table 3.11

! section 4
 gfld%ipdtlen=29
 gfld%ipdtnum=8            ! template number 8, accumulation

 allocate(gfld%ipdtmpl(gfld%ipdtlen))

 gfld%ipdtmpl(1)=4         ! parm category, oct 10
 gfld%ipdtmpl(2)=3         ! parm number, oct 11
 gfld%ipdtmpl(3)=0         ! type of gen process, oct 12
 gfld%ipdtmpl(4)=255       ! background gen process id, oct 13
 gfld%ipdtmpl(5)=255       ! anaylsis gen process, oct 14
 gfld%ipdtmpl(6)=0         ! hours after ref time data cutoff, oct 15-16
 gfld%ipdtmpl(7)=0         ! minutes after ref time data cutoff, oct 17
 gfld%ipdtmpl(8)=1         ! unit of time range (hours), oct 18
 gfld%ipdtmpl(9)=0         ! fcst time in units defined by oct 18; oct 19-22
 gfld%ipdtmpl(10)=1        ! type of first fixed surface, oct 23
 gfld%ipdtmpl(11)=0        ! scaled factor of first fixed surface, oct 24
 gfld%ipdtmpl(12)=0        ! scaled value of first fixed surface, oct 25-28
 gfld%ipdtmpl(13)=255      ! type of second fixed surface, oct 29
 gfld%ipdtmpl(14)=0        ! scaled factor of second fixed surface, oct 30
 gfld%ipdtmpl(15)=0        ! scaled value of second fixed surface, oct 31-34

 last_juldy_grib=last_julhr_grib/24
 call w3fs26(last_juldy_grib,iyear,month,iday,idaywk,idayyr)

 gfld%ipdtmpl(16)=iyear    ! year of end of overall time interval, oct 35-36
 gfld%ipdtmpl(17)=month    ! mon of end of overall time interval, oct 37
 gfld%ipdtmpl(18)=iday     ! day of end of overall time interval, oct 38
 gfld%ipdtmpl(19)=mod(last_julhr_grib,24)  ! hour of end of overall time interval, oct 39
 gfld%ipdtmpl(20)=0        ! minute of end of overall time interval, oct 40
 gfld%ipdtmpl(21)=0        ! second of end of overall time interval, oct 41
 gfld%ipdtmpl(22)=1        ! # of time ranges, oct 42
 gfld%ipdtmpl(23)=0        ! # data values missing, oct 43-46
 gfld%ipdtmpl(24)=1        ! stat process used (accum), oct 47
 gfld%ipdtmpl(25)=2        ! type of time intervals, oct 48
 gfld%ipdtmpl(26)=1        ! indicator of unit of time range (hours), oct 49

! accumulation period in hours
 num_hours = last_julhr_grib - first_julhr_grib

 gfld%ipdtmpl(27)=num_hours ! accumulation period, oct 50-53
 gfld%ipdtmpl(28)=255
 gfld%ipdtmpl(29)=0

! section 5 - data representation section
 gfld%idrtnum=0
 gfld%idrtlen=5
 allocate(gfld%idrtmpl(gfld%idrtlen))
 gfld%idrtmpl=0
 gfld%idrtmpl(3)=2   ! decimal scaling factor

! section 6 - bitmap section
 gfld%ibmap=0
 allocate(gfld%bmap(npts))
 gfld%bmap=.true.
 where (sum_all < 0.0) gfld%bmap=.false.
 allocate(gfld%fld(npts))
 gfld%fld=sum_all
 gfld%ngrdpts=npts
 gfld%numoct_opt=0
 gfld%num_coord=0   ! used for hybrid coordinates

 print*,'- GRIB ACCUMULATED DATA.'
 print*,'- OPEN FILE: ', trim(output_file)
 lugb=51
 call baopenw(lugb,output_file,iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR: BAD OPEN OF FILE: ', trim(output_file)
   print*,'- IRET IS: ', iret
   call mpi_abort(mpi_comm_world, 60)
 endif

 print*,'- WRITE GRIB DATA'
 call putgb2(lugb,gfld,iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR: BAD WRITE OF FILE: ', trim(output_file)
   print*,'- IRET IS: ', iret
   call mpi_abort(mpi_comm_world, 61)
 endif

 call baclose(lugb, iret)

 call gf_free2(gfld)

 return
 end subroutine gribit
 
 subroutine gf_free2(gfld)
!$$$  subprogram documentation block
!
! subprogram:    gf_free2
!   prgmmr: gayno          org: w/np2     date: 2014-sep-28
!
! abstract:  deallocate the grib2 gribfield pointers.
!
! program history log:
! 2014-sep-28  gayno    - initial version
!
! usage: call gf_free2 with a gribfield data structure
!
!   input argument list:
!     gfld - a gribfield data structure
!
!   output argument list:
!     gfld - a gribfield data structure
!
! files: none
!
! condition codes: none
!
! remarks: the ncep grib2 library has a similar routine but
! it is buggy.
!
!$$$
!
 use grib_mod

 implicit none

 type(gribfield) :: gfld

 if (associated(gfld%idsect)) then
   deallocate(gfld%idsect)
   nullify(gfld%idsect)
 endif
 if (associated(gfld%local)) then
   deallocate(gfld%local)
   nullify(gfld%local)
 endif
 if (associated(gfld%list_opt)) then
   deallocate(gfld%list_opt)
   nullify(gfld%list_opt)
 endif
 if (associated(gfld%igdtmpl)) then
   deallocate(gfld%igdtmpl)
   nullify(gfld%igdtmpl)
 endif
 if (associated(gfld%ipdtmpl)) then
   deallocate(gfld%ipdtmpl)
   nullify(gfld%ipdtmpl)
 endif
 if (associated(gfld%coord_list)) then
   deallocate(gfld%coord_list)
   nullify(gfld%coord_list)
 endif
 if (associated(gfld%idrtmpl)) then
   deallocate(gfld%idrtmpl)
   nullify(gfld%idrtmpl)
 endif
 if (associated(gfld%bmap)) then
   deallocate(gfld%bmap)
   nullify(gfld%bmap)
 endif
 if (associated(gfld%fld)) then
   deallocate(gfld%fld)
   nullify(gfld%fld)
 endif

 return

 end subroutine gf_free2
