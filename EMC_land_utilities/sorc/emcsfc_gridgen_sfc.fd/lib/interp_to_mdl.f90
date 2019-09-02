! 
! $Revision$
!
 subroutine interp_to_mdl(input_file, output_file, iunit_out,  &
                          interp_type, default_value, grib_scale_fac, &
                          interp_mask)
!$$$ subroutine documentation block
!
! subprogram:  interp to model
!   prgmmr: gayno          org: w/np2           date: ????
!
! abstract: driver routine to read source data, interpolate
!   to the model grid, then write result to a grib file
!
! program history log:
! ????        gayno     - initial version
!
! usage:  call routine with the following arguments:
!   inputs:
!      input_file       - path/name of source data
!      output_file      - path/name of output grib file
!      iunit_out        - fortran unit number of output file
!      interp_type      - interpolation method
!      default_value    - default value for undefined model pts
!      grib_scale_fac   - grib scaling factor
!      interp_mask      - points to mask out - water or land
!
!   outputs: none
!
! files: 
!    input:
!      - source data in binary format
!
!    output:
!      - interpolated data in grib 1 or grib 2.
!
! condition codes: 
!   1 - bad open source file
!   2 - bad open output grib file
!   3 - unrecognized grid type
!   4 - error writing output grib file
!   5 - error reading source data
!
! remarks: none.
!
!$$$
 use grib_mod

 use init_grib2

 use program_setup, only        : resol_mdl,  &
                                  imdl,       &
                                  jmdl,       &
                                  thinned,    &
                                  domain_type,   &
                                  dx_mdl,     &
                                  dx_gfs,     &
                                  dy_mdl,     &
                                  grib2 

 use lsmask_orog,  only         : lsmask,        &
                                  wtrmask,       &
                                  lbms_lnd_mdl,  &
                                  lbms_wtr_mdl

 use calc_latlons,  only        : lat_mdl,       &
                                  lon_mdl

 use init_grib1,    only        : kgds_mdl,      &
                                  kpds_mdl

 use mpimod, only               : gather, &
                                  istart_mdl, iend_mdl, iend_mdl_4_loops, &
                                  jstart_mdl, jend_mdl, &
                                  myrank

 use native_endianness, only    : to_native_endianness, &
                                  is_little_endian

 implicit none

 include 'mpif.h'

 type(gribfield) :: gfld

 character(len=mpi_max_error_string) :: errmsg
 character*150, intent(in)           :: input_file
 character*3,   intent(in)           :: interp_mask
 character*2,   intent(in)           :: interp_type
 character*150, intent(in)           :: output_file

 integer*2, allocatable        :: data_src(:,:)
 integer*4                     :: fcst_time_unit
 integer, intent(in)           :: grib_scale_fac
 integer*4                     :: grib_parm_num
 integer*8, parameter          :: header_size = 78
 integer*4                     :: isrc, jsrc, num_records
 integer                       :: istart_src, iend_src
 integer                       :: jstart_src, jend_src
 integer*8                     :: data_size, offset, offset_start
 integer                       :: i, ii, j, iret, n
 integer                       :: iunit_src
 integer, intent(in)           :: iunit_out
 integer                       :: kgds(200)
 integer                       :: kpds(200)
 integer*2, allocatable        :: nearest_i(:,:), nearest_j(:,:)
 integer*4                     :: num_bytes, num_time_unit
 integer*4                     :: scaling_fac
 integer                       :: test
 integer*2                     :: undef_value  ! source data not defined,
                                               ! ex: a water point for land data
 integer*4                     :: year, mon, day, hour

 logical                       :: aavg, bilinear, nn
 logical*1, allocatable        :: lbms_mdl(:,:)

 real, allocatable             :: data_mdl(:,:)
 real, intent(in)              :: default_value
 real, allocatable             :: dummy(:,:)
 real*8                        :: dx_src
 real*8                        :: dy_src
 real*8                        :: lat_11_src  ! lat of point 1,1 on src grid
 real*8                        :: lon_11_src  ! lon of point 1,1 on src grid
 real, allocatable             :: lsmask_mdl(:,:)

!-----------------------------------------------------------------------
! execution starts here.
!-----------------------------------------------------------------------

 if (grib2) then
   call grib2_init(gfld)
 endif

 allocate (data_mdl(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 data_mdl=0.0
 allocate (lbms_mdl(imdl,jmdl))   ! full grid
 allocate (lsmask_mdl(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))

!-----------------------------------------------------------------------
! lsmask_mdl is passed to interpolation routines so it knows to
! ignore land points for a water field, such as sst, or to 
! igmore water points for a land fields, such as greenness.
!-----------------------------------------------------------------------

 if (interp_mask == 'wtr') then
   lbms_mdl   = lbms_wtr_mdl
   lsmask_mdl = wtrmask
 elseif (interp_mask == 'lnd') then
   lbms_mdl   = lbms_lnd_mdl
   lsmask_mdl = lsmask
 end if

!-----------------------------------------------------------------------
! source file contains the raw lat/lon data.  
!-----------------------------------------------------------------------

 print*,'- OPEN SOURCE FILE ', trim(input_file)
 iunit_src = 40
 call mpi_file_open(mpi_comm_world, input_file, mpi_mode_rdonly, &
                    mpi_info_null, iunit_src, iret)
 if (iret /= 0) then
   print*,'- FATAL ERROR: BAD OPEN, IRET IS ', iret
   call mpi_abort(mpi_comm_world, 1, iret)
 endif

!-----------------------------------------------------------------------
! open output grib file that holds the interpolated model data.
!-----------------------------------------------------------------------

 if (myrank == 0) then
   print*,'- OPEN OUTPUT FILE ', trim(output_file)
   call baopenw(iunit_out, output_file, iret)
   if (iret /= 0) then
     print*,'- FATAL ERROR: BAD OPEN, IRET IS ', iret
     call mpi_abort(mpi_comm_world, 2, iret)
   end if
 end if

!-----------------------------------------------------------------------
! read every record in the source data file, interpolate the data to the
! model grid, then grib the interpolated data.
!
! source data must be on a global lat/lon grid.
!
! source data format is as follows:
!
! bytes 1-4   - number of records in file (integer*4)
! bytes 5-8   - i dimension of grid (integer*4)
! bytes 9-12  - j dimension of grid (integer*4)
! bytes 13-20 - n/s resolution in degrees (real*8)
! bytes 21-28 - e/w resolution in degrees (real*8)
! bytes 29-36 - longitude of pixel (1,1) (real*8)
! bytes 37-44 - latitude of pixel (1,1) (real*8)
! bytes 45-46 - water flag (integer*2) - only one water cat allowed
! bytes 47-50 - scaling factor (integer*4)
! bytes 51-54 - year of record (integer*4)
! bytes 55-58 - month of record (integer*4)
! bytes 59-62 - day of record (integer*4)
! bytes 63-66 - hour of record (integer*4)
! bytes 67-70 - forecast time unit (integer*4) (see grib standard)
! bytes 71-74 - num of time units (integer*4) (see grib standard)
! bytes 75-78 - grib parameter number (integer*4) (see grib standard)
! bytes 79-...- the global data (integer*2)
!
! header is repeated for each additional record.
!
!-----------------------------------------------------------------------

 print*,"- READ SOURCE FILE."

 offset = 0_8
 call mpi_file_read_at(iunit_src, offset, num_records, 1, &
                       mpi_integer4, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(num_records)
 print*,'- NUM RECORDS: ',num_records

 offset = 4_8
 call mpi_file_read_at(iunit_src, offset, isrc, 1, &
                       mpi_integer4, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(isrc)
 print*,'- ISRC: ', isrc

 offset = 8_8
 call mpi_file_read_at(iunit_src, offset, jsrc, 1, &
                       mpi_integer4, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(jsrc)
 print*,'- JSRC: ', jsrc

 offset = 12_8
 call mpi_file_read_at(iunit_src, offset, dy_src, 1, &
                       mpi_double_precision, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(dy_src)
 print*,'- DY: ', dy_src

 offset = 20_8
 call mpi_file_read_at(iunit_src, offset, dx_src, 1, &
                       mpi_double_precision, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(dx_src)
 print*,'- DX: ', dx_src

 offset = 28_8
 call mpi_file_read_at(iunit_src, offset, lon_11_src, 1, &
                       mpi_double_precision, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(lon_11_src)
 print*,'- LON11: ',lon_11_src

 offset = 36_8
 call mpi_file_read_at(iunit_src, offset, lat_11_src, 1, &
                       mpi_double_precision, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(lat_11_src)
 print*,'- LAT11: ',lat_11_src

 offset = 44_8
 call mpi_file_read_at(iunit_src, offset, undef_value, 1, &
                       mpi_integer2, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(undef_value)
 print*,'- UNDEF VALUE: ',undef_value

 offset = 46_8
 call mpi_file_read_at(iunit_src, offset, scaling_fac, 1, &
                       mpi_integer4, mpi_status_ignore, iret)
 if (iret /= 0) goto 9000
 call to_native_endianness(scaling_fac)
 print*,'- SCALING FACTOR: ',scaling_fac

 call find_bounds_ll(isrc, jsrc, lat_11_src, lon_11_src, dy_src, dx_src, &
                     istart_src, iend_src, jstart_src, jend_src)

 allocate (data_src(isrc,jstart_src:jend_src))

 data_size= int(isrc,8)*int(jsrc,8)*2_8  ! number of bytes of data record

!-----------------------------------------------------------------------
! for categorical data, the user should specify a nearest neighbor
! interpolation.  otherwise, let the program determine the interpolaton
! type based on the resolutions of the source and model grids.
!-----------------------------------------------------------------------

 nn = .false.
 aavg = .false.
 bilinear = .false.

 if (interp_type == "nn") then
   nn = .true.
   print*,"- WILL USE NEAREST NEIGHBOR INTERPOLATION"
 else
   if ( resol_mdl <= dx_src ) then
     print*,'- WILL USE BI-LINEAR INTERPOLATION '
     bilinear = .true.
   else
     test = int ( (resol_mdl * 0.5) / dx_src )
     if (test == 0) then
       print*,'- WILL USE NEAREST NEIGHBOR INTERPOLATION'
       nn = .true.
     elseif (test > 0) then
       print*,'- WILL TAKE AREA AVERAGE OF SOURCE DATA. '
       aavg = .true.
       if (trim(domain_type) == "egrid" .or. &
           trim(domain_type) == "bgrid" ) then

!-----------------------------------------------------------------------
!        for each source data point, calculate the corresponding 
!        index on the model grid.  this is done once before the loop
!        in order to reduce wall clock time.
!        (lat/lon to i/j on the nam grid is very expensive
!         to calculate)
!-----------------------------------------------------------------------

         print*,'- PREP FOR AREA AVERAGING'
         allocate(nearest_i(istart_src:iend_src,jstart_src:jend_src)) 
         allocate(nearest_j(istart_src:iend_src,jstart_src:jend_src)) 

         if (trim(domain_type) == "egrid") then
           call interp_aavg_egrid_prep(istart_src, iend_src, jstart_src, jend_src, &
                                       isrc, dx_src, dy_src, lat_11_src, &
                                       lon_11_src,  dx_mdl, dy_mdl, &
                                       istart_mdl, iend_mdl, jstart_mdl, jend_mdl, &
                                       imdl, nearest_i, nearest_j)
         else
           call interp_aavg_bgrid_prep(istart_src, iend_src, jstart_src, jend_src, &
                                       isrc, dx_src, dy_src, lat_11_src, lon_11_src, &
                                       istart_mdl, iend_mdl, jstart_mdl, jend_mdl, &
                                       nearest_i, nearest_j)
         endif

       elseif (trim(domain_type) == "gaussian") then

         print*,"- NO PREP REQUIRED FOR GAUSSIAN"

       else

         print*,"- FATAL ERROR: UNRECOGNIZED DOMAIN TYPE: ", trim(domain_type)
         call mpi_abort(mpi_comm_world, 3, iret)

       endif

     endif
   end if
 end if

 MAIN : do n = 1, num_records

!-----------------------------------------------------------------------
! read source data.
!-----------------------------------------------------------------------

   offset_start = (data_size + header_size) * (int(n,8)-1_8)
   offset = offset_start

   print*,'- READING SOURCE RECORD NUMBER ',n

   offset = offset_start+50_8
   call mpi_file_read_at(iunit_src, offset, year, 1, &
                         mpi_integer4, mpi_status_ignore, iret)
   if (iret /= 0) goto 9000
   call to_native_endianness(year)
   print*,'- YEAR: ',year

   offset = offset_start+54_8
   call mpi_file_read_at(iunit_src, offset, mon, 1, &
                         mpi_integer4, mpi_status_ignore, iret)
   if (iret /= 0) goto 9000
   call to_native_endianness(mon)
   print*,'- MON: ',mon

   offset = offset_start+58_8
   call mpi_file_read_at(iunit_src, offset, day, 1, &
                         mpi_integer4, mpi_status_ignore, iret)
   if (iret /= 0) goto 9000
   call to_native_endianness(day)
   print*,'- DAY: ',day

   offset = offset_start+62_8
   call mpi_file_read_at(iunit_src, offset, hour, 1, &
                         mpi_integer4, mpi_status_ignore, iret)
   if (iret /= 0) goto 9000
   call to_native_endianness(hour)
   print*,'- HOUR: ',hour

   offset = offset_start+66_8
   call mpi_file_read_at(iunit_src, offset, fcst_time_unit, 1, &
                         mpi_integer4, mpi_status_ignore, iret)
   if (iret /= 0) goto 9000
   call to_native_endianness(fcst_time_unit)
   print*,'- FCST TIME UNIT: ',fcst_time_unit

   offset = offset_start+70_8
   call mpi_file_read_at(iunit_src, offset, num_time_unit, 1, &
                         mpi_integer4, mpi_status_ignore, iret)
   if (iret /= 0) goto 9000
   call to_native_endianness(num_time_unit)
   print*,'- NUMBER OF TIME UNITS: ',num_time_unit

   offset = offset_start+74_8
   call mpi_file_read_at(iunit_src, offset, grib_parm_num, 1, &
                         mpi_integer4, mpi_status_ignore, iret)
   if (iret /= 0) goto 9000
   call to_native_endianness(grib_parm_num)
   print*,'- GRIB PARMAMETER NUMBER: ',grib_parm_num

   num_bytes = isrc * (jend_src - jstart_src + 1)
   offset    = offset_start+78_8 + 2_8*(int(isrc,8)*(int(jstart_src,8)-1_8))

   call mpi_file_read_at(iunit_src, offset, data_src, num_bytes, &
                         mpi_integer2, mpi_status_ignore, iret)
   if (iret /= 0) goto 9000

   if (is_little_endian) then
     do j = jstart_src, jend_src
     do i = 1, isrc
       call to_native_endianness(data_src(i,j))
     enddo
     enddo
   endif

   print*,'- THE DATA: ',maxval(data_src),minval(data_src)

   if (bilinear) then

     call interp_bilinear (istart_mdl, iend_mdl, iend_mdl_4_loops, &
                           jstart_mdl, jend_mdl, &
                           lat_mdl, lon_mdl, lsmask_mdl, &
                           dx_src, dy_src, lat_11_src, lon_11_src, &
                           default_value, undef_value, scaling_fac, &
                           data_mdl, data_src, isrc, jstart_src, jend_src)
   elseif (nn) then

     call interp_nn(istart_mdl, iend_mdl, iend_mdl_4_loops, &
                    jstart_mdl, jend_mdl,  &
                    lat_mdl, lon_mdl, lsmask_mdl, &
                    dx_src, dy_src, lat_11_src, lon_11_src, &
                    default_value, undef_value, scaling_fac,  &
                    data_mdl, data_src, isrc, jstart_src, jend_src)

   elseif(aavg) then

     if (trim(domain_type) == "egrid" .or. &
         trim(domain_type) == "bgrid") then

       call interp_aavg_nam(istart_mdl, iend_mdl, jstart_mdl, jend_mdl, &
                            lat_mdl, lon_mdl, lsmask_mdl, &
                            dx_src, dy_src, lat_11_src, lon_11_src, &
                            default_value, undef_value, scaling_fac,  &
                            data_mdl, data_src, isrc, istart_src, iend_src, &
                            jstart_src, jend_src, nearest_i, nearest_j)

     else

       call interp_aavg_gaus (istart_mdl, iend_mdl, iend_mdl_4_loops, &
                              jstart_mdl, jend_mdl, &
                              lat_mdl, lon_mdl, lsmask,  &
                              dx_gfs(jstart_mdl:jend_mdl), dy_mdl, &
                              dx_src, dy_src, lat_11_src, lon_11_src,  &
                              default_value, undef_value, scaling_fac, data_mdl, &
                              data_src, isrc, jstart_src, jend_src)

     end if

   end if  ! interp type

!----------------------------------------------------------------------
! output interpolated data to a grib file. 
!----------------------------------------------------------------------

   if (grib2) then
     call set_drt_pdt(gfld,grib_parm_num,year,mon,day,hour)
   else
     kgds     = kgds_mdl
     kpds(1)  = 7               ! center id
     kpds(2)  = 255             ! process id number. ?
     kpds(3)  = kpds_mdl(3)     ! model grid definition
     kpds(4)  = kpds_mdl(4)     ! gds/bms flag
     kpds(5)  = grib_parm_num   ! grib parameter number 
     kpds(6)  = 1               ! level - ground or water surface
     kpds(7)  = 0               ! height pressure of level
     kpds(8)  = mod(year,100)   ! year of century 
     kpds(21) = year/100 + 1    ! century 
     if (kpds(8) == 0) then
       kpds(8) = 100
       kpds(21) = kpds(21) - 1
     end if
     kpds(9)  = mon             ! month
     kpds(10) = day             ! day - greenness valid 15th of month.
     kpds(11) = hour            ! hour
     kpds(12) = 0               ! minute
     kpds(13) = fcst_time_unit  ! fcst time unit - month
     kpds(14) = 0               ! period of time, p1.  set to '0' for analysis
     kpds(15) = num_time_unit   ! number of time units, p2.
     kpds(16) = 51              ! time range indicator
     kpds(17) = 1               ! number in average 
     kpds(18) = 1               ! grib edition 1
     kpds(19) = 130             ! parameter table version number
     kpds(20) = 0               ! number missing from avg/accum
     kpds(22) = 0               ! scaling factor
     if (grib_scale_fac > -1) then
       kpds(22) = grib_scale_fac
     end if
     kpds(23) = 0               ! subcenter
     kpds(24) = 0               ! reserved
     kpds(25) = 0               ! reserved
   endif

!----------------------------------------------------------------------
!  if running a global thinned grid, need to fill in unprocessed
!  points before writing out data.
!----------------------------------------------------------------------

  allocate (dummy(imdl,jmdl))
  call gather(data_mdl, imdl, jmdl, dummy)

  if (myrank == 0) then
    if (thinned) then
      call fill(dummy)
    end if

    if(grib2) then
      gfld%fld=reshape(dummy, (/imdl*jmdl/) )
      gfld%bmap=reshape(lbms_mdl, (/imdl*jmdl/) )
      call putgb2(iunit_out, gfld,iret)
    else
      call putgb (iunit_out, (imdl*jmdl), kpds, kgds, lbms_mdl,  &
                  dummy, iret)
    endif
    if (iret /= 0) then
      print*,"- FATAL ERROR: GRIBBING OF DATA FAILED. IRET IS ", iret
      call mpi_abort(mpi_comm_world, 4, iret)
    end if
  endif

  deallocate (dummy)

 enddo MAIN

 if (allocated(nearest_i)) deallocate (nearest_i)
 if (allocated(nearest_j)) deallocate (nearest_j)

 deallocate (data_src)
 deallocate (data_mdl)

 call mpi_file_close(iunit_src, iret)

 if (myrank == 0) call baclose(iunit_out, iret)

 deallocate (lbms_mdl)
 deallocate (lsmask_mdl)

 if (grib2) then
   call grib2_free(gfld)
 endif

 return

 9000 print*,'- FATAL ERROR READING SOURCE DATA. IRET IS: ', iret
      call mpi_error_string(iret, errmsg, ii, iret)
      print*,"- ERROR IS: ", errmsg(1:ii)
      call mpi_abort(mpi_comm_world, 5, iret)

 end subroutine interp_to_mdl
 
 subroutine set_drt_pdt(gfld,grib_parm_num,year,mon,day,hour)
!$$$ subroutine documentation block
!
! subprogram:  set grib 2 drt and prt
!   
!   prgmmr: gayno          org: w/np2           date: ????
!
! abstract: set the grib 2 data representation template and 
!   product definition template.
!
! program history log:
! ????        gayno     - initial version
!
! usage:  call routine with the following arguments:
!   inputs:
!      year/mon/day/hour    - date of data
!      grib_parm_num        - grib 1 parameter number
!
!   outputs:
!      gfld                 - grib 2 data structure that
!                             holds the header info
!
! files: none
!
! condition codes: none
!
! remarks: none.
!
!$$$
 use grib_mod

 implicit none

 type(gribfield) :: gfld

 integer         :: grib_parm_num,year,mon,day,hour

 if (grib_parm_num==87) then  ! greeness
   gfld%discipline = 2
   gfld%ipdtmpl(1)= 0  ! oct 10; parameter category
   gfld%ipdtmpl(2)= 4  ! oct 11; parameter
   gfld%idrtmpl=0
   gfld%idrtmpl(3)=1 ! decimal scaling factor
 elseif(grib_parm_num==159) then ! maximum snow albedo
   gfld%discipline = 0
   gfld%ipdtmpl(1)= 19  ! oct 10; parameter category
   gfld%ipdtmpl(2)= 192 ! oct 11; parameter
   gfld%idrtmpl=0
   gfld%idrtmpl(3)=1 ! decimal scaling factor
 elseif(grib_parm_num==170) then ! snow-free albedo
   gfld%discipline = 0
   gfld%ipdtmpl(1)= 19  ! oct 10; parameter category
   gfld%ipdtmpl(2)= 193 ! oct 11; parameter
   gfld%idrtmpl=0
   gfld%idrtmpl(3)=1 ! decimal scaling factor
 elseif(grib_parm_num==189) then ! visible black sky albedo
   gfld%discipline = 0
   gfld%ipdtmpl(1)= 19  ! oct 10; parameter category
   gfld%ipdtmpl(2)= 211 ! oct 11; parameter
   gfld%idrtmpl=0
   gfld%idrtmpl(3)=1 ! decimal scaling factor
 elseif(grib_parm_num==190) then ! visible white sky albedo
   gfld%discipline = 0
   gfld%ipdtmpl(1)= 19  ! oct 10; parameter category
   gfld%ipdtmpl(2)= 212 ! oct 11; parameter
   gfld%idrtmpl=0
   gfld%idrtmpl(3)=1 ! decimal scaling factor
 elseif(grib_parm_num==191) then ! near ir black sky albedo
   gfld%discipline = 0
   gfld%ipdtmpl(1)= 19  ! oct 10; parameter category
   gfld%ipdtmpl(2)= 213 ! oct 11; parameter
   gfld%idrtmpl=0
   gfld%idrtmpl(3)=1 ! decimal scaling factor
 elseif(grib_parm_num==192) then ! near ir white sky albedo
   gfld%discipline = 0
   gfld%ipdtmpl(1)= 19  ! oct 10; parameter category
   gfld%ipdtmpl(2)= 214 ! oct 11; parameter
   gfld%idrtmpl=0
   gfld%idrtmpl(3)=1 ! decimal scaling factor
 elseif(grib_parm_num==222) then ! slope type
   gfld%discipline = 2
   gfld%ipdtmpl(1)= 3  ! oct 10; parameter category
   gfld%ipdtmpl(2)= 194 ! oct 11; parameter
   gfld%idrtmpl=0
   gfld%idrtmpl(3)=1 ! decimal scaling factor
 elseif(grib_parm_num==83) then ! roughness
   gfld%discipline = 2
   gfld%ipdtmpl(1)= 0  ! oct 10; parameter category
   gfld%ipdtmpl(2)= 1 ! oct 11; parameter
   gfld%idrtmpl=0
   gfld%idrtmpl(3)=4 ! decimal scaling factor
 elseif(grib_parm_num==182) then ! leaf area index
   gfld%discipline = 2
   gfld%ipdtmpl(1)= 0  ! oct 10; parameter category
   gfld%ipdtmpl(2)= 28 ! oct 11; parameter
   gfld%idrtmpl=0
   gfld%idrtmpl(3)=2 ! decimal scaling factor
 endif

 gfld%idsect(6) = year  ! octs 13-14;  year
 gfld%idsect(7) = mon   ! oct 15; month
 gfld%idsect(8) = day   ! oct 16; day
 gfld%idsect(9) = hour  ! oct 17; hour
 gfld%ibmap = 0 ! bitmap applies

 end subroutine set_drt_pdt
