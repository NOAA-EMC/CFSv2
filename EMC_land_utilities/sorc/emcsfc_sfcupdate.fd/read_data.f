 module read_data
!$$$  module documentation block
!
! module:    read_data
!   prgmmr: gayno         org: w/np2     date: 2005-05-20
!
! $Revision$
!
! abstract: this module contains routines that read 
!           analysis and climatological data on the model grid,
!           and information from the runtime configuration
!           namelist.
!
! module history log:
!   2005-05-20  gayno   - initial version
!   2014-11-10  gayno   - modify to read model analysis and climo
!                         data in grib 1 or grib 2 format.
!
! usage: use read_data
!
! remarks: variable naming convention:
!
!   X_climo_all - climatology of field X for all time periods.
!   X_climo     - climatology of field X for this time step.
!   X_anal      - analysis of field X for this time step.
!
!$$$
 use read_write_utils, only     : inventory,         &
                                  new_time,          &
                                  time_interp,       &
                                  read_grib_data,    &
                                  thin_data,         &
                                  degrib_climo_thin, &
                                  date

 character*10,  save           :: domain_name
 character*150, save           :: snowfree_albedo_climo_file
 character*150, save           :: greenfrc_climo_file
 character*150, save           :: seaice_anal_dir
 character*150, save           :: seaice_climo_file
 character*150, save           :: snow_depth_anal_dir
 character*150, save           :: snow_depth_climo_file
 character*10,  save           :: soil_type_src
 character*150, save           :: soilm_climo_file
 character*150, save           :: sst_anal_dir
 character*150, save           :: sst_climo_file
 character*10,  save           :: veg_type_src
 character*150, save           :: z0_climo_file
 character*150, save           :: fire_anal_dir
 character*200, save           :: fire_mask_file

 logical, save                 :: sst_anlcyc_update

 integer, save                 :: num_soil_types
 integer, save                 :: num_veg_types

 real, allocatable, save       :: alnsf_climo_all(:,:)
 real, allocatable             :: alnsf_climo(:)
 real, allocatable, save       :: alnwf_climo_all(:,:)
 real, allocatable             :: alnwf_climo(:)
 real, allocatable, save       :: alvsf_climo_all(:,:)
 real, allocatable             :: alvsf_climo(:)
 real, allocatable, save       :: alvwf_climo_all(:,:)
 real, allocatable             :: alvwf_climo(:)
 real, save                    :: beta(50)
 real, allocatable, save       :: greenfrc_climo_all(:,:)
 real, allocatable             :: greenfrc_climo(:)
 real, save                    :: merge_coeff_snow_depth
 real, save                    :: merge_coeff_soilm
 real, save                    :: merge_coeff_sst
 real, save                    :: psis(50)
 real, save                    :: salp
 real, save                    :: satdk(50)
 real, allocatable             :: seaice_anal(:)
 real, allocatable, save       :: seaice_climo_all(:,:)
 real, allocatable             :: seaice_climo(:)
 real, save                    :: smchigh
 real, save                    :: smclow
 real, save                    :: smcmax(50)
 real, allocatable             :: snow_cover_anal(:)
 real, allocatable             :: snow_depth_anal(:)
 real, allocatable, save       :: snow_depth_climo_all(:,:)
 real, allocatable             :: snow_depth_climo(:)
 real, allocatable, save       :: snowfree_albedo_climo_all(:,:)
 real, allocatable             :: snowfree_albedo_climo(:)
 real, save                    :: snup(50)
 real, allocatable, save       :: soilm_climo_all(:,:)
 real, allocatable             :: soilm_climo(:)
 real, allocatable             :: sst_anal(:)
 real, allocatable, save       :: sst_climo_all(:,:)
 real, allocatable             :: sst_climo(:)
 real, allocatable             :: sst_climo_taum1(:)
 real, allocatable, save       :: z0_climo_all(:,:)
 real, allocatable             :: z0_climo(:)
 real, allocatable             :: burnt_frac_daily_anal(:)
 real, allocatable             :: burnt_frac_monthly_anal(:)

 type (date), allocatable, save :: alnsf_climo_dates(:)
 type (date), allocatable, save :: alnwf_climo_dates(:)
 type (date), allocatable, save :: alvsf_climo_dates(:)
 type (date), allocatable, save :: alvwf_climo_dates(:)
 type (date), allocatable, save :: greenfrc_climo_dates(:)
 type (date), allocatable, save :: seaice_climo_dates(:)
 type (date), allocatable, save :: snow_depth_climo_dates(:)
 type (date), allocatable, save :: snowfree_albedo_climo_dates(:)
 type (date), allocatable, save :: soilm_climo_dates(:)
 type (date), allocatable, save :: sst_climo_dates(:)
 type (date), allocatable, save :: z0_climo_dates(:)
 
 contains

 subroutine read_climo(jmdl2, ijmdl, imdl, jmdl,  &
                       lonsperlat, ipts, jpts, grid_type, gdtnum_mdl, me)
!$$$  subprogram documentation block
!
! subprogram:    read_climo
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: read climatological fields on the model grid. fields have
!   multiple time periods (i.e., monthly or quarterly). 
!
! program history log:
! 2005-05-20  gayno    - initial version
! 2014-11-10  gayno    - logic to read data in either grib 1 or grib2 
!                        format. 
!
! usage: call read_climo with the following arguments
!
!   input argument list:  
!     grid_type       - flag that identifies grid as regional
!                                    or global
!     gdtnum_mdl      - grib 2 grid description template number
!     imdl/jmdl       - i/j dimensions of full model grid
!     ijmdl           - total number of model grid points.  
!                       will be less than imdl*jmdl for gfs thinned grids
!     ipts/jpts       - holds i/j index of grid points to
!                       be processed.
!     jmdl2           - jmdl / 2
!     lonsperlat      - when running global grids thinned, number
!                       of i points in each row.
!     me              - mpi task number
!
!   output argument list: none
!
! files:
!   input:
!     - soil moisture climatology (grib 1)
!     - sea ice climatology (grib 1)
!     - z0 climatology (grib 1)
!     - sst climatology (grib 1)
!     - snow depth climatology (grib 1)
!     - greenness fraction climatology (grib 1 or grib 2)
!     - gfs snowfree climatology (grib 1 or grib 2)
!     - nam snowfree climatology (grib 1 or grib 2)
!
!   output: none
!
! condition codes: all fatal
!   68 - error reading soil moisture climo file
!   69 - error reading sea ice climo file
!   70 - error reading z0 climo file
!   71 - error reading sst climo file
!   72 - error reading snow depth climo file
!   73 - error reading greenness climo file
!   74 - error reading gfs snow albedo climo file
!   75 - error reading nam snow albedo climo file
!
! remarks: none.
!
!$$$

 implicit none

 include 'mpif.h'

 character*8, intent(in)      :: grid_type

 integer                      :: cat_num, ierr, isgrib, jdisc
 integer, intent(in)          :: gdtnum_mdl, imdl, jmdl
 integer, intent(in)          :: ijmdl
 integer, intent(in)          :: ipts(ijmdl)
 integer, intent(in)          :: jmdl2
 integer, intent(in)          :: jpts(ijmdl)
 integer, intent(in)          :: lonsperlat(jmdl2)
 integer, intent(in)          :: me        ! mpi task number
 integer                      :: param_num
 integer                      :: status
 integer                      :: rec_nums(200), tot_num_recs

 if (merge_coeff_soilm < 99999. .and. len_trim(soilm_climo_file) > 0) then

   if (me == 0) print*,"- READ SOIL MOISTURE CLIMATOLOGY"

   call grib_check(soilm_climo_file, isgrib)

   if (isgrib == 1) then  ! grib 1 file

     param_num = 144

     call inventory(soilm_climo_file, param_num, tot_num_recs, status, me)

     if (status /= 0) then
       if (me == 0) print*,'- FATAL ERROR DURING INVENTORY OF SOIL MOISTURE CLIMO FILE **'
       call w3tage('SFCUPDATE')
       call mpi_abort(mpi_comm_world, 68, ierr)
     end if

     allocate (soilm_climo_all(ijmdl,tot_num_recs))
     allocate (soilm_climo_dates(tot_num_recs))

     call degrib_climo_thin(soilm_climo_all, soilm_climo_dates,  &
                            ijmdl, imdl, jmdl, &
                            jmdl2, ipts, jpts, lonsperlat, param_num, &
                            soilm_climo_file, tot_num_recs, status, me)

     if (status /= 0) then
       if (me == 0) print*,'- FATAL ERROR: BAD DEGRIB OF SOIL MOISTURE CLIMO FILE **'
       call w3tage('SFCUPDATE')
       call mpi_abort(mpi_comm_world, 68, ierr)
     end if

   else

     if (me == 0) print*,'- FATAL ERROR: SOIL MOISTURE CLIMO FILE NOT GRIB1 **'
     call w3tage('SFCUPDATE')
     call mpi_abort(mpi_comm_world, 68, ierr)

   endif

 end if ! soil moisture climo file

 if (len_trim(seaice_climo_file) > 0) then

   if (me == 0) print*,"- READ SEA ICE CLIMATOLOGY"

   call grib_check(seaice_climo_file, isgrib)

   if (isgrib == 1) then  ! grib 1 file

     param_num = 91

     call inventory(seaice_climo_file, param_num, tot_num_recs, status, me)

     if (status /= 0) then
       if (me == 0) print*,'- FATAL ERROR DURING INVENTORY OF SEA ICE CLIMO FILE **'
       call w3tage('SFCUPDATE')
       call mpi_abort(mpi_comm_world, 69, ierr)
     end if

     allocate (seaice_climo_all(ijmdl,tot_num_recs))
     allocate (seaice_climo_dates(tot_num_recs))

     call degrib_climo_thin(seaice_climo_all, seaice_climo_dates, &
                            ijmdl, imdl, jmdl, &
                            jmdl2, ipts, jpts, lonsperlat, param_num, &
                            seaice_climo_file, tot_num_recs, status, me)

     if (status /= 0) then
       if (me == 0) print*,'- FATAL ERROR: BAD DEGRIB OF SEA ICE CLIMO FILE **'
       call w3tage('SFCUPDATE')
       call mpi_abort(mpi_comm_world, 69, ierr)
     end if

   else

     if (me == 0) print*,'- FATAL ERROR: SEA ICE CLIMO FILE NOT GRIB1 **'
     call w3tage('SFCUPDATE')
     call mpi_abort(mpi_comm_world, 69, ierr)

   endif

 end if ! sea ice climo file

 if (len_trim(z0_climo_file) > 0) then
    
   if (me == 0) print*,"- ROUGHNESS LENGTH CLIMATOLOGY"

   call grib_check(z0_climo_file, isgrib)

   if (isgrib == 1) then  ! grib 1 file

     param_num = 83

     call inventory(z0_climo_file, param_num, tot_num_recs, status, me)

     if (status /= 0) then
       if (me == 0) print*,'- FATAL ERROR DURING INVENTORY OF ROUGHNESS LENGTH CLIMO FILE **'
       call w3tage('SFCUPDATE')
       call mpi_abort(mpi_comm_world, 70, ierr)
     end if

     allocate (z0_climo_all(ijmdl,tot_num_recs))
     allocate (z0_climo_dates(tot_num_recs))

     call degrib_climo_thin(z0_climo_all, z0_climo_dates, ijmdl, imdl, jmdl, &
                            jmdl2, ipts, jpts, lonsperlat, param_num, &
                            z0_climo_file, tot_num_recs, status, me)

     if (status /= 0) then
       if (me == 0) print*,'- FATAL ERROR: BAD DEGRIB OF ROUGHNESS LENGTH CLIMO FILE **'
       call w3tage('SFCUPDATE')
       call mpi_abort(mpi_comm_world, 70, ierr)
     end if

   else

     if (me == 0) print*,'- FATAL ERROR: ROUGHNESS LENGTH CLIMO FILE NOT GRIB1 **'
     call w3tage('SFCUPDATE')
     call mpi_abort(mpi_comm_world, 70, ierr)

   endif

 end if  ! roughness length climo

 if ((merge_coeff_sst < 99999. .or. sst_anlcyc_update) .and.  &
      len_trim(sst_climo_file) > 0) then

   if (me == 0) print*,"- READ SST CLIMATOLOGY"

   call grib_check(sst_climo_file, isgrib)

   if (isgrib == 1) then  ! grib 1 file

     param_num = 11

     call inventory(sst_climo_file, param_num, tot_num_recs, status, me)

     if (status /= 0) then
       if (me == 0) print*,'- FATAL ERROR DURING INVENTORY OF SST CLIMO FILE **'
       call w3tage('SFCUPDATE')
       call mpi_abort(mpi_comm_world, 71, ierr)
     end if

     allocate (sst_climo_all(ijmdl,tot_num_recs))
     allocate (sst_climo_dates(tot_num_recs))

     call degrib_climo_thin(sst_climo_all, sst_climo_dates, ijmdl, imdl, jmdl, &
                            jmdl2, ipts, jpts, lonsperlat, param_num, &
                            sst_climo_file, tot_num_recs, status, me)

     if (status /= 0) then
       if (me == 0) print*,'- FATAL ERROR: BAD DEGRIB OF SST CLIMO FILE **'
       call w3tage('SFCUPDATE')
       call mpi_abort(mpi_comm_world, 71, ierr)
     end if

   else

     if (me == 0) print*,'- FATAL ERROR: SST CLIMO FILE NOT GRIB1 **'
     call w3tage('SFCUPDATE')
     call mpi_abort(mpi_comm_world, 71, ierr)

   endif

 end if ! sst climatology

 if (merge_coeff_snow_depth < 99999. .and. len_trim(snow_depth_climo_file) > 0) then

   if (me == 0) print*,"- READ SNOW DEPTH CLIMATOLOGY"

   call grib_check(snow_depth_climo_file, isgrib)

   if (isgrib == 1) then  ! grib 1 file

     param_num = 65

     call inventory(snow_depth_climo_file, param_num, tot_num_recs, status, me)

     if (status /= 0) then
       if (me == 0) print*,'- FATAL ERROR DURING INVENTORY OF SNOW DEPTH CLIMO FILE **'
       call w3tage('SFCUPDATE')
       call mpi_abort(mpi_comm_world, 72, ierr)
     end if

     allocate (snow_depth_climo_all(ijmdl,tot_num_recs))
     allocate (snow_depth_climo_dates(tot_num_recs))

     call degrib_climo_thin(snow_depth_climo_all, snow_depth_climo_dates, &
                            ijmdl, imdl, jmdl, &
                            jmdl2, ipts, jpts, lonsperlat, param_num, &
                            snow_depth_climo_file, tot_num_recs, status, me)

     if (status /= 0) then
       if (me == 0) print*,'- FATAL ERROR: BAD DEGRIB OF SNOW DEPTH CLIMO FILE **'
       call w3tage('SFCUPDATE')
       call mpi_abort(mpi_comm_world, 72, ierr)
     end if

!-----------------------------------------------------------------------
! climo is liquid water equivalent in mm, convert to depth in m
! using a 5:1 ratio.
!-----------------------------------------------------------------------
 
     snow_depth_climo_all = snow_depth_climo_all * 5.0 * 0.001

   else

     if (me == 0) print*,'- FATAL ERROR: SNOW DEPTH CLIMO FILE NOT GRIB1 **'
     call w3tage('SFCUPDATE')
     call mpi_abort(mpi_comm_world, 72, ierr)

   endif

 end if  ! snow depth climo

 if (len_trim(greenfrc_climo_file) > 0) then

   if (me == 0) print*,"- READ GREENNESS FRACTION CLIMATOLOGY"

   call grib_check(greenfrc_climo_file, isgrib)

   if (isgrib == 1) then  ! grib 1 file
     param_num = 87
     call inventory(greenfrc_climo_file, param_num, tot_num_recs, status, me)
   elseif (isgrib == 2) then ! grib 2 file
     jdisc = 2
     cat_num = 0
     param_num = 4
     call inventory2(greenfrc_climo_file, jdisc, cat_num, param_num,  &
                     imdl, jmdl, gdtnum_mdl, rec_nums, tot_num_recs, status)
   else   ! not grib 1 or grib 2
     if (me == 0) print*,"- FATAL ERROR: GREENNESS FILE IS NOT GRIB1 OR GRIB2 **"
     status = 1
   endif

   if (status /= 0) then
     if (me == 0) print*,'- FATAL ERROR DURING INVENTORY OF GREENNESS CLIMO FILE **'
     call w3tage('SFCUPDATE')
     call mpi_abort(mpi_comm_world, 73, ierr)
   end if

   allocate (greenfrc_climo_all(ijmdl,tot_num_recs))
   allocate (greenfrc_climo_dates(tot_num_recs))

   if (isgrib == 1) then  ! grib 1 file
     call degrib_climo_thin(greenfrc_climo_all, greenfrc_climo_dates, &
                            ijmdl, imdl, jmdl, &
                            jmdl2, ipts, jpts, lonsperlat, param_num, &
                            greenfrc_climo_file, tot_num_recs, status, me)
   elseif (isgrib == 2) then ! grib 2 file
     call degrib2_climo_thin(greenfrc_climo_file, imdl, jmdl, ipts, jpts, &
                             ijmdl, lonsperlat, jmdl2, tot_num_recs, rec_nums, &
                             greenfrc_climo_all, greenfrc_climo_dates, status)
   endif

   if (status /= 0) then
     if (me == 0) print*,'- FATAL ERROR: BAD DEGRIB OF GREENNESS CLIMO FILE **'
     call w3tage('SFCUPDATE')
     call mpi_abort(mpi_comm_world, 73, ierr)
   end if

   greenfrc_climo_all = greenfrc_climo_all * 0.01

 end if  ! climo greenness file

 if (trim(grid_type) == "global" .and.    &
     len_trim(snowfree_albedo_climo_file) > 0) then

   if (me == 0) print*,"- READ GFS SNOWFREE ALBEDO CLIMATOLOGY"

   call grib_check(snowfree_albedo_climo_file, isgrib)

   if (isgrib == 1) then  ! grib 1 file

     param_num = 212

     call inventory(snowfree_albedo_climo_file, param_num, &
                    tot_num_recs, status, me)

     if (status /= 0) then
       if (me == 0) print*,'- FATAL ERROR DURING INVENTORY OF GFS SNOWFREE ALBEDO CLIMO FILE **'
       call w3tage('SFCUPDATE')
       call mpi_abort(mpi_comm_world, 74, ierr)
     end if

     allocate (alvsf_climo_all(ijmdl,tot_num_recs))
     allocate (alvsf_climo_dates(tot_num_recs))

     call degrib_climo_thin(alvsf_climo_all, alvsf_climo_dates, ijmdl, imdl, jmdl, &
                            jmdl2, ipts, jpts, lonsperlat, param_num, &
                            snowfree_albedo_climo_file, tot_num_recs, &
                            status, me)

     if (status /= 0) then
       if (me == 0) print*,'- FATAL ERROR: BAD DEGRIB OF GFS SNOWFREE ALBEDO CLIMO FILE **'
       call w3tage('SFCUPDATE')
       call mpi_abort(mpi_comm_world, 74, ierr)
     end if

     alvsf_climo_all = alvsf_climo_all * 0.01   

     param_num = 213

     call inventory(snowfree_albedo_climo_file, param_num, &
                    tot_num_recs, status, me)

     if (status /= 0) then
       if (me == 0) print*,'- FATAL ERROR DURING INVENTORY OF GFS SNOWFREE ALBEDO CLIMO FILE **'
       call w3tage('SFCUPDATE')
       call mpi_abort(mpi_comm_world, 74, ierr)
     end if

     allocate (alnsf_climo_all(ijmdl,tot_num_recs))
     allocate (alnsf_climo_dates(tot_num_recs))

     call degrib_climo_thin(alnsf_climo_all, alnsf_climo_dates, ijmdl, imdl, jmdl, &
                            jmdl2, ipts, jpts, lonsperlat, param_num, &
                            snowfree_albedo_climo_file, tot_num_recs, status, me)

     if (status /= 0) then
       if (me == 0) print*,'- FATAL ERROR: BAD DEGRIB OF GFS SNOWFREE ALBEDO CLIMO FILE **'
       call w3tage('SFCUPDATE')
       call mpi_abort(mpi_comm_world, 74, ierr)
     end if

     alnsf_climo_all = alnsf_climo_all * 0.01

     param_num = 216

     call inventory(snowfree_albedo_climo_file, param_num, &
                    tot_num_recs, status, me)

     if (status /= 0) then
       if (me == 0) print*,'- FATAL ERROR DURING INVENTORY OF GFS SNOWFREE ALBEDO CLIMO FILE **'
       call w3tage('SFCUPDATE')
       call mpi_abort(mpi_comm_world, 74, ierr)
     end if

     allocate (alnwf_climo_all(ijmdl,tot_num_recs))
     allocate (alnwf_climo_dates(tot_num_recs))

     call degrib_climo_thin(alnwf_climo_all, alnwf_climo_dates, ijmdl, imdl, jmdl, &
                            jmdl2, ipts, jpts, lonsperlat, param_num, &
                            snowfree_albedo_climo_file, tot_num_recs, status, me)

     if (status /= 0) then
       if (me == 0) print*,'- FATAL ERROR: BAD DEGRIB OF GFS SNOWFREE ALBEDO CLIMO FILE **'
       call w3tage('SFCUPDATE')
       call mpi_abort(mpi_comm_world, 74, ierr)
     end if

     alnwf_climo_all = alnwf_climo_all * 0.01

     param_num = 215

     call inventory(snowfree_albedo_climo_file, param_num, &
                    tot_num_recs, status, me)

     if (status /= 0) then
       if (me == 0) print*,'- FATAL ERROR DURING INVENTORY OF GFS SNOWFREE ALBEDO CLIMO FILE **'
       call w3tage('SFCUPDATE')
       call mpi_abort(mpi_comm_world, 74, ierr)
     end if

     allocate (alvwf_climo_all(ijmdl,tot_num_recs))
     allocate (alvwf_climo_dates(tot_num_recs))

     call degrib_climo_thin(alvwf_climo_all, alvwf_climo_dates, ijmdl, imdl, jmdl, &
                            jmdl2, ipts, jpts, lonsperlat, param_num, &
                            snowfree_albedo_climo_file, tot_num_recs, status, me)

     if (status /= 0) then
       if (me == 0) print*,'- FATAL ERROR: BAD DEGRIB OF GFS SNOWFREE ALBEDO CLIMO FILE **'
       call w3tage('SFCUPDATE')
       call mpi_abort(mpi_comm_world, 74, ierr)
     end if

     alvwf_climo_all = alvwf_climo_all * 0.01

   else

     if (me == 0) print*,'- FATAL ERROR: GFS SNOWFREE ALBEDO CLIMO FILE NOT GRIB1 **'
     call w3tage('SFCUPDATE')
     call mpi_abort(mpi_comm_world, 74, ierr)

   endif

 elseif (trim(grid_type) == "regional" .and.   &
         len_trim(snowfree_albedo_climo_file) > 0) then

   if (me == 0) print*,"- READ SNOWFREE ALBEDO CLIMATOLOGY"

   call grib_check(snowfree_albedo_climo_file, isgrib)

   if (isgrib == 1) then  ! grib 1 file
     param_num = 170
     call inventory(snowfree_albedo_climo_file, param_num, &
                    tot_num_recs, status, me)
   elseif (isgrib == 2) then ! grib 2 file
     jdisc = 0
     cat_num = 19
     param_num = 193
     call inventory2(snowfree_albedo_climo_file, jdisc, cat_num, param_num,  &
                     imdl, jmdl, gdtnum_mdl, rec_nums, tot_num_recs, status)
   else   ! not grib 1 or grib 2
     if (me == 0) print*,"- FATAL ERROR: SNOWFREE ALBEDO FILE IS NOT GRIB1 OR GRIB2 **"
     status = 1
   endif

   if (status /= 0) then
     if (me == 0) print*,'- FATAL ERROR DURING INVENTORY OF SNOWFREE ALBEDO CLIMO FILE **'
     call w3tage('SFCUPDATE')
     call mpi_abort(mpi_comm_world, 75, ierr)
   end if

   allocate (snowfree_albedo_climo_all(ijmdl,tot_num_recs))
   allocate (snowfree_albedo_climo_dates(tot_num_recs))

   if (isgrib == 1) then
     call degrib_climo_thin(snowfree_albedo_climo_all,    &
                            snowfree_albedo_climo_dates,  &
                            ijmdl, imdl, jmdl,            &
                            jmdl2, ipts, jpts, lonsperlat, param_num, &
                            snowfree_albedo_climo_file,  &
                            tot_num_recs, status, me)
   elseif (isgrib == 2) then
     call degrib2_climo_thin(snowfree_albedo_climo_file, imdl, jmdl, ipts, jpts, &
                             ijmdl, lonsperlat, jmdl2, tot_num_recs, rec_nums, &
                             snowfree_albedo_climo_all, snowfree_albedo_climo_dates, status)
   endif

   if (status /= 0) then
     if (me == 0) print*,'- FATAL ERROR: BAD DEGRIB OF SNOWFREE ALBEDO CLIMO FILE **'
     call w3tage('SFCUPDATE')
     call mpi_abort(mpi_comm_world, 75, ierr)
   end if

   snowfree_albedo_climo_all = snowfree_albedo_climo_all * 0.01

 end if   ! snowfree albedo climo file

 return

 end subroutine read_climo

 subroutine inventory2(filename, jdisc, cat_num, param_num, imdl, jmdl, gdtnum_mdl, &
                       rec_nums, num_recs, ierr)
!$$$  subprogram documentation block
!
! subprogram:   inventory2
!
!   prgmmr: gayno          org: w/np2     date: 2014-11-10
!
! abstract:  inventory a grib 2 climatology file and determine the number
!            of time periods it contains.  store the record number 
!            of each time period.
!
! program history log:
! 2014-11-10  gayno    - initial version
!
! usage: call routine with the following arguments:
!
!   input argument list: none
!     filename           - file to be inventoried.
!     jdisc              - grib 2 discipline to look for
!     cat_num            - grib 2 category number to look for
!     param_num          - grib 2 parameter number to look for
!     i/jmdl             - i/j dimension of model grid
!     gdtnum_mdl         - grib 2 grid definition template number
!                          of model grid
!
!   output argument list: 
!     ierr               - error status; 0 - no errors
!     num_recs           - number of time periods (or records) the
!                          file contains
!     rec_nums           - the record number within the file of each
!                          time period.
!
! files:
!   input:
!     - gribi2 file to be inventoried. unit=iunit
!
!   output: none
!
! condition codes:
!   variable ierr is non-zero if error
!
! remarks: none.
!
!$$$
 use grib_mod

 implicit none

 character*(*), intent(in)  :: filename

 integer, intent(in)        :: jdisc, cat_num, param_num, imdl, jmdl, gdtnum_mdl
 integer, intent(out)       :: num_recs, rec_nums(200), ierr

 integer                    :: iunit, lugi, k
 integer                    :: j, jpdtn, jgdtn
 integer                    :: jids(200), jgdt(200), jpdt(200)

 logical                    :: unpack

 type(gribfield)            :: gfld

 j       = 0          ! search at beginning of file
 jpdtn   = 0          ! search for product definition template number - anal at one level
 jgdtn   = gdtnum_mdl ! search for grid definition template number of
                      ! model grid.
 jids    = -9999      ! array of values in identification section, set to wildcard
 jgdt    = -9999      ! array of values in grid definition template 3.m
 jgdt(8) = imdl       ! does analysis file have correct i/j dimensions?
 jgdt(9) = jmdl
 jpdt    = -9999      ! array of values in product definition template 4.n
 jpdt(1) = cat_num    ! search for parameter category - surface properties
 jpdt(2) = param_num  ! search for parameter number - water temp
 unpack  = .false.    ! unpack data

 call grib2_null(gfld)

 print*,"- OPEN AND INVENTORY FILE ", trim(filename)
 iunit = 13
 lugi  = 0

 ierr = 0

 call baopenr (iunit, filename, ierr)

 if (ierr /= 0) return

 num_recs = 0
 rec_nums = -999

 do while (ierr == 0) 

   call getgb2(iunit, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
               unpack, k, gfld, ierr)

   if (ierr == 0) then
     num_recs = num_recs + 1
     rec_nums(num_recs)=k
   endif

   j=k
   call grib2_free(gfld)

 enddo

 call baclose (iunit, ierr)

 if (num_recs > 0) then
   print*,"- FILE CONTAINS ", num_recs, " RECORDS OF DATA."
   ierr = 0
 else
   print*,"- ** WARNING: FILE CONTAINS ", num_recs, " RECORDS OF DATA."
   ierr = 1
 endif

 return
 end subroutine inventory2

 subroutine degrib2_climo_thin(filename, imdl, jmdl, ipts, jpts, ijmdl, &
                               lonsperlat, jmdl2, num_recs, rec_nums,  &
                               climo_all, climo_dates, ierr)
!$$$  subprogram documentation block
!
! subprogram:   degrib2_climo_thin
!   prgmmr: gayno          org: w/np2     date: 2014-11-10
!
! abstract: degrib a grib 2 climatological data file.  save the data 
!   and the date/time of each record.  then convert to the thinned
!   gfs gaussian grid if required.  data is assumed to be on model grid.
!
! program history log:
! 2014-11-10  gayno    - initial version
!
! usage: call this routine with the following arguments:
!
!   input argument list: 
!     filename             - file name
!     i/jmdl               - i/j dimensions of model grid
!     ijmdl                - total number of model grid points.  
!                            will be less than imdl*jmdl for gfs thinned grids
!     ipts/jpts            - holds i/j index of grid points to
!                            be processed.
!     jmdl2                - jmdl / 2
!     lonsperlat           - when running global grids thinned, number
!                            of i points in each row.
!     num_recs             - number of time periods
!     rec_nums             - record number of each time period
!
!   output argument list: 
!     climo_all            - array containing the climo data for each
!                            time period
!     climo_date           - date/time group of each time period.
!     ierr                 - error status code; 0 if no error
!
! files:
!   input:
!     - grib 2 file to be read
!
!   output: none
!
! condition codes:
!   variable ierr is non-zero if an error occurs.
!
! remarks: none.
!
!$$$

 use grib_mod

 implicit none

 character*(*), intent(in)      :: filename

 integer, intent(in)            :: num_recs, rec_nums(200)
 integer, intent(in)            :: imdl, jmdl, ijmdl, jmdl2
 integer, intent(in)            :: ipts(ijmdl), jpts(ijmdl)
 integer, intent(in)            :: lonsperlat(jmdl2)
 integer, intent(out)           :: ierr

 real, intent(out)              :: climo_all(ijmdl, num_recs)

 integer                        :: iunit, lugi, ierr2
 integer                        :: j, jj, jdisc, jpdtn, jgdtn, k
 integer                        :: jids(200), jgdt(200), jpdt(200)

 logical                        :: unpack

 real, allocatable              :: dummy1d(:), dummy2d(:,:)

 type(date)                     :: climo_dates(num_recs)
 type(gribfield)                :: gfld

 ierr = 0

 print*,"- OPEN AND DEGRIB FILE ", trim(filename)
 iunit = 13
 lugi  = 0
 call baopenr (iunit, filename, ierr)

 if (ierr /= 0) then
   print*,"- ** WARNING: ERROR OPENING FILE: ", ierr
   return
 endif

! set these to wildcards because the call to inventory2 identified the
! records we want (stored in rec_nums)

 jdisc   = -1     ! discipline
 jpdtn   = -1     ! search for product definition template number 
 jgdtn   = -1     ! search for grid definition template number 
 jids    = -9999  ! array of values in identification section, set to wildcard
 jgdt    = -9999  ! array of values in grid definition template 3.m
 jpdt    = -9999  ! array of values in product definition template 4.n
 unpack  = .true. ! unpack data

 allocate(dummy2d(imdl,jmdl))
 allocate(dummy1d(ijmdl))

 call grib2_null(gfld)

 do jj = 1, num_recs

   j = rec_nums(jj) - 1

   print*,'- DEGRIB RECORD ',rec_nums(jj)
   call getgb2(iunit, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
               unpack, k, gfld, ierr)

   if (ierr /= 0) then
     print*,'- WARNING: ERROR DEGRIBBING RECORD'
     call grib2_free(gfld)
     exit
   endif

   dummy2d=reshape (gfld%fld , (/imdl,jmdl/) )

   call thin_data(dummy2d, ipts, jpts, lonsperlat,   &
                  ijmdl, jmdl2, imdl, jmdl, dummy1d)

   climo_dates(jj)%year = gfld%idsect(6)
   climo_dates(jj)%month = gfld%idsect(7)
   climo_dates(jj)%day = gfld%idsect(8)

   climo_all(:,jj)=dummy1d

   call grib2_free(gfld)

 enddo
 
 deallocate(dummy1d, dummy2d)

 call baclose (iunit, ierr2)

 end subroutine degrib2_climo_thin

 subroutine read_sfc_anal_data(lonsperlat, imdl, jmdl, ijmdl,        & 
                               jmdl2, ipts, jpts, grid_type,         &
                               gdtnum_mdl, cycle_year, cycle_month,  &
                               cycle_day, cycle_hour,                &
                               fcst_hour, fhcyc, me)
!$$$  subprogram documentation block
!
! subprogram:    read_sfc_anal_data
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: time interpolate model climatological fields to the
!   current cycle time.  read sst, ice and snow analysis data
!   valid at the current cycle time.
!
! program history log:
! 2005-05-20  gayno    - initial version
! 2014-11-10  gayno    - logic to read analysis data in either 
!                        grib 1 or grib 2 format. 
! 2015-07-08  gayno    - add reads of wild fire analyis data
!                        (daily and monthly accumulated files)
!
! usage: call read_sfc_anal_data with the following arguments
!
!   input argument list:  
!     grid_type       - flag that identifies grid as regional
!                       or global
!     gdtnum_mdl      - grib 2 grid description template number
!     imdl/jmdl       - i/j dimensions of full model grid
!     ijmdl           - total number of model grid points.  
!                       will be less than imdl*jmdl for gfs thinned grids
!     ipts/jpts       - holds i/j index of grid points to
!                       be processed.
!     jmdl2           - jmdl / 2
!     lonsperlat      - when running global grids thinned, number
!                       of i points in each row.
!     me              - mpi task number
!     cycle_year      - update surface fields base on these times.
!     cycle_month       fcst_hour is > 0 when this routine is
!     cycle_day         during the free forecast.
!     cycle_hour
!     fcst_hour
!     fhcyc           - frequency in hours in which the surface
!                       fields are updated.
!
!   output argument list: none
!
! files:
!   input:
!     - sst analysis (grib 1 or grib 2)
!     - sea ice analysis (grib 1 or grib 2)
!     - snow analysis (grib 1 or grib 2)
!     - fire analysis - monthly and daily files (grib 2)
!
!   output: none
!
! condition codes: all fatal
!     11 - sst analysis not grib 1 or grib 2
!     12 - sea ice analysis not grib 1 or grib 2
!     13 - snow analysis not grib 1 or grib 2
!
! remarks: none.
!
!$$$

 use grib_mod

 implicit none

 include 'mpif.h'

 character*10              :: curr_date
 character*8, intent(in)   :: grid_type
 character*150             :: seaice_file
 character*150             :: snow_file
 character*150             :: sst_anal_file
 character*10              :: taum1_date

 integer                   :: curr_day
 integer                   :: curr_hour
 integer                   :: curr_minute
 integer                   :: curr_month
 integer                   :: curr_year
 integer, intent(in)       :: cycle_day
 integer, intent(in)       :: cycle_hour
 integer, intent(in)       :: cycle_month
 integer, intent(in)       :: cycle_year
 integer, intent(in)       :: gdtnum_mdl
 integer                   :: ierr
 integer, intent(in)       :: ijmdl
 integer, intent(in)       :: imdl
 integer, intent(in)       :: ipts(ijmdl)
 integer                   :: isgrib, iunit, lugb, lugi
 integer                   :: j, jdisc, jgdtn, jpdtn, k
 integer                   :: jids(200), jgdt(200), jpdt(200)
 integer, intent(in)       :: jmdl
 integer, intent(in)       :: jmdl2
 integer, intent(in)       :: jpts(ijmdl)
 integer, intent(in)       :: lonsperlat(jmdl2)
 integer, intent(in)       :: me
 integer                   :: num_recs
 integer                   :: parm_num
 integer                   :: status
 integer                   :: taum1_day
 integer                   :: taum1_hour
 integer                   :: taum1_minute
 integer                   :: taum1_month
 integer                   :: taum1_year

 logical                   :: itexists, unpack
 logical, save             :: readcfg
 logical, save             :: readclimo

 real,    allocatable      :: dummy2d(:,:)
 real,    intent(in)       :: fcst_hour
 real,    intent(in)       :: fhcyc

 type(gribfield)           :: gfld

 data readcfg   /.true./
 data readclimo /.true./

!-----------------------------------------------------------------------
! first time here, get path names and file names.
!-----------------------------------------------------------------------

 if (readcfg) then
   call read_config(me)
   readcfg = .false.
 end if

!-----------------------------------------------------------------------
! calculate the date/time based on the cycle time and the forecast
! hour.  note: when run as front end to the analysis or model, the
! forecast hour is zero.
!-----------------------------------------------------------------------

 call new_time(cycle_year, cycle_month, cycle_day, cycle_hour, fcst_hour, &
               curr_year, curr_month, curr_day, curr_hour, curr_minute, curr_date)

!-----------------------------------------------------------------------
! read climo data, first time step only.
!-----------------------------------------------------------------------

 if (readclimo) then
   call read_climo(jmdl2, ijmdl, imdl, jmdl,  &
                   lonsperlat, ipts, jpts, grid_type, gdtnum_mdl, me)
   readclimo = .false.
 end if

!-----------------------------------------------------------------------
! time interpolate climo data every time sfccycle is called.
!-----------------------------------------------------------------------

 if (allocated(greenfrc_climo_all)) then

   num_recs = size(greenfrc_climo_all,dim=2)
   
   allocate (greenfrc_climo(ijmdl))

   if (me == 0) print*,"- TIME INTERPOLATE CLIMO GREENNESS"

   call time_interp(greenfrc_climo_all, greenfrc_climo_dates, num_recs, &
                    ijmdl, curr_year, curr_month, curr_day,  &
                    curr_hour, curr_minute, greenfrc_climo)


 end if

 if (allocated(seaice_climo_all)) then

   num_recs = size(seaice_climo_all,dim=2)
   
   allocate (seaice_climo(ijmdl))

   if (me == 0) print*,"- TIME INTERPOLATE CLIMO SEA ICE"

   call time_interp(seaice_climo_all, seaice_climo_dates, num_recs, &
                    ijmdl, curr_year, curr_month, curr_day,  &
                    curr_hour, curr_minute, seaice_climo)


 end if

 if (allocated(z0_climo_all)) then

   num_recs = size(z0_climo_all,dim=2)

   allocate (z0_climo(ijmdl))

   if (me == 0) print*,"- TIME INTERPOLATE ROUGHNESS"

   call time_interp(z0_climo_all, z0_climo_dates, num_recs, &
                    ijmdl, curr_year, curr_month, curr_day,  &
                    curr_hour, curr_minute, z0_climo)


 end if

 if (allocated(sst_climo_all)) then

   num_recs = size(sst_climo_all,dim=2)
   
   allocate (sst_climo(ijmdl))

   if (me == 0) print*,"- TIME INTERPOLATE CLIMO SST AT CURRENT TIME STEP"

   call time_interp(sst_climo_all, sst_climo_dates, num_recs, &
                    ijmdl, curr_year, curr_month, curr_day,   &
                    curr_hour, curr_minute, sst_climo)

! get climo sst at previous time step

   if (sst_anlcyc_update .and. fcst_hour > 0.0) then

     if (me == 0) print*,"- TIME INTERPOLATE CLIMO SST AT PREVIOUS TIME STEP"

     call new_time(cycle_year, cycle_month, cycle_day, cycle_hour,  &
                  (fcst_hour - fhcyc), taum1_year, taum1_month,    &
                   taum1_day, taum1_hour, taum1_minute, taum1_date)


     allocate (sst_climo_taum1(ijmdl))

     call time_interp(sst_climo_all, sst_climo_dates, num_recs, &
                      ijmdl, taum1_year, taum1_month, taum1_day,   &
                      taum1_hour, taum1_minute, sst_climo_taum1)


   end if

 end if

 if (allocated(snow_depth_climo_all)) then

   num_recs = size(snow_depth_climo_all,dim=2)
   
   allocate (snow_depth_climo(ijmdl))

   if (me == 0) print*,"- TIME INTERPOLATE CLIMO SNOW DEPTH"

   call time_interp(snow_depth_climo_all, snow_depth_climo_dates, num_recs, &
                    ijmdl, curr_year, curr_month, curr_day, &
                    curr_hour, curr_minute, snow_depth_climo)


 end if

 if (allocated(soilm_climo_all)) then

   num_recs = size(soilm_climo_all,dim=2)
   
   allocate (soilm_climo(ijmdl))

   if (me == 0) print*,"- TIME INTERPOLATE CLIMO SOIL MOISTURE"

   call time_interp(soilm_climo_all, soilm_climo_dates, num_recs, &
                    ijmdl, curr_year, curr_month, curr_day, &
                    curr_hour, curr_minute, soilm_climo)


 end if

 if (allocated(alvsf_climo_all)) then

   num_recs = size(alvsf_climo_all,dim=2)
   
   allocate (alvsf_climo(ijmdl))

   if (me == 0) print*,"- TIME INTERPOLATE CLIMO ALVSF"

   call time_interp(alvsf_climo_all, alvsf_climo_dates, num_recs, &
                    ijmdl, curr_year, curr_month, curr_day, &
                    curr_hour, curr_minute, alvsf_climo)


 end if

 if (allocated(alvwf_climo_all)) then

   num_recs = size(alvwf_climo_all,dim=2)
   
   allocate (alvwf_climo(ijmdl))

   if (me == 0) print*,"- TIME INTERPOLATE CLIMO ALVWF"

   call time_interp(alvwf_climo_all, alvwf_climo_dates, num_recs, &
                    ijmdl, curr_year, curr_month, curr_day, &
                    curr_hour, curr_minute, alvwf_climo)


 end if

 if (allocated(alnwf_climo_all)) then

   num_recs = size(alnwf_climo_all,dim=2)
   
   allocate (alnwf_climo(ijmdl))

   if (me == 0) print*,"- TIME INTERPOLATE CLIMO ALNWF"

   call time_interp(alnwf_climo_all, alnwf_climo_dates, num_recs, &
                    ijmdl, curr_year, curr_month, curr_day, &
                    curr_hour, curr_minute, alnwf_climo)


 end if

 if (allocated(alnsf_climo_all)) then

   num_recs = size(alnsf_climo_all,dim=2)
   
   allocate (alnsf_climo(ijmdl))

   if (me == 0) print*,"- TIME INTERPOLATE CLIMO ALNSF"

   call time_interp(alnsf_climo_all, alnsf_climo_dates, num_recs, &
                    ijmdl, curr_year, curr_month, curr_day,  &
                    curr_hour, curr_minute, alnsf_climo)


 end if

!-----------------------------------------------------------------------
! snow free albedo as used by the regiona model.
!-----------------------------------------------------------------------

 if (allocated(snowfree_albedo_climo_all)) then

   num_recs = size(snowfree_albedo_climo_all,dim=2)
   
   allocate (snowfree_albedo_climo(ijmdl))

   if (me == 0) print*,"- TIME INTERPOLATE SNOW FREE ALBEDO"

   call time_interp(snowfree_albedo_climo_all,               &
                    snowfree_albedo_climo_dates, num_recs,   &
                    ijmdl, curr_year, curr_month, curr_day,  &
                    curr_hour, curr_minute, snowfree_albedo_climo)

 end if

!-----------------------------------------------------------------------
! read sst analysis data.
!-----------------------------------------------------------------------

 if ( (merge_coeff_sst < 99999.) .and. (len_trim(sst_anal_dir) > 0) ) then

   sst_anal_file= trim(sst_anal_dir) // "/sst." // curr_date // "." // &
                                        trim(domain_name) // ".grb"

   if (me==0) print *,'- LOOK FOR SST ANALYSIS FILE: ', trim(sst_anal_file)

   inquire (file=trim(sst_anal_file), exist=itexists)
 
   if (itexists) then

     if (me==0) print*,"- WILL READ SST ANALYSIS: ",trim(sst_anal_file)

     call grib_check(sst_anal_file, isgrib)

     if (isgrib == 1) then  ! grib 1 file

       parm_num = 11

       allocate (dummy2d(imdl,jmdl))
       call read_grib_data(sst_anal_file, parm_num, dummy2d, &
                           (imdl*jmdl), status, me)

       if (status /= 0) then
         if (me==0) print*,'*** WARNING: ERROR READING SST ANALYSIS ***'
         if (me==0) print*,'*** WILL CYCLE SST ***'
       else
         allocate(sst_anal(ijmdl))
         call thin_data(dummy2d, ipts, jpts, lonsperlat,   &
                        ijmdl, jmdl2, imdl, jmdl, sst_anal)
       end if
       deallocate(dummy2d)

     elseif (isgrib == 2) then  ! grib 2 file

       j       = 0      ! search at beginning of file
       jdisc   = 10     ! search for discipline; 10 - ocean products
       jpdtn   = 0      ! search for product definition template number - anal at one level
       jgdtn   = gdtnum_mdl  ! search for grid definition template number of
                             ! model grid.
       jids    = -9999  ! array of values in identification section, set to wildcard
       jgdt    = -9999  ! array of values in grid definition template 3.m
       jgdt(8) = imdl   ! does analysis file have correct i/j dimensions?
       jgdt(9) = jmdl
       jpdt    = -9999  ! array of values in product definition template 4.n
       jpdt(1) = 3      ! search for parameter category - surface properties
       jpdt(2) = 0      ! search for parameter number - water temp
       unpack  = .true. ! unpack data

       call grib2_null(gfld)

       if (me==0) print*,"- OPEN FILE ", trim(sst_anal_file)
       iunit = 13
       lugi  = 0
       call baopenr (iunit, sst_anal_file, ierr)

       if (ierr /=0) then
         if (me==0) print*,'*** WARNING: ERROR OPENING SST ANALYSIS ***'
         if (me==0) print*,'*** WILL CYCLE SST ***'
       else
         if (me==0) print*,"- DEGRIB DATA"
         call getgb2(iunit, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
                     unpack, k, gfld, ierr)
         if (ierr /=0) then
           if (me==0) print*,'*** WARNING: ERROR READING SST ANALYSIS ***'
           if (me==0) print*,'*** WILL CYCLE SST ***'
         else
           allocate(dummy2d(imdl,jmdl))
           dummy2d=reshape (gfld%fld , (/imdl,jmdl/) )
           allocate (sst_anal(ijmdl))
           call thin_data(dummy2d, ipts, jpts, lonsperlat,   &
                          ijmdl, jmdl2, imdl, jmdl, sst_anal)
           deallocate(dummy2d)
         end if
         call baclose(iunit, ierr)
       end if

       call grib2_free(gfld)
 
     else

       if (me==0) print*,'- FATAL ERROR: SST ANALYSIS NOT GRIB1 OR GRIB2 ***'
       call w3tage('SFCUPDATE')
       call mpi_abort(mpi_comm_world, 11, ierr)

     end if ! is file grib1 or grib2

   else

     if (me==0) print*,'- SST ANALYSIS FILE NOT FOUND. CYCLE SST.'

   end if

 end if

!-----------------------------------------------------------------------
! sea ice
!-----------------------------------------------------------------------

 if ( len_trim(seaice_anal_dir) > 0 ) then

   seaice_file=trim(seaice_anal_dir) // "/seaice." // curr_date //   &
                                        "." // trim(domain_name) // ".grb"

   if (me==0) print *,'- LOOK FOR SEA ICE FILE: ', trim(seaice_file)

   inquire (file=trim(seaice_file), exist=itexists)

   if (itexists) then

     if (me==0) print*,"- WILL READ SEA ICE ANALYSIS: ", trim(seaice_file)

     call grib_check(seaice_file, isgrib)

     if (isgrib == 1) then  ! grib 1 file

       parm_num = 91

       allocate(dummy2d(imdl,jmdl))
       call read_grib_data(seaice_file, parm_num, dummy2d, &
                           (imdl*jmdl), status, me)

       if (status /= 0) then
         if (me==0) print*,'*** WARNING: ERROR READING SEA ICE ANALYSIS ***'
         if (me==0) print*,'*** WILL CYCLE SEA ICE ***'
       else
         allocate (seaice_anal(ijmdl))
         call thin_data(dummy2d, ipts, jpts, lonsperlat,   &
                        ijmdl, jmdl2, imdl, jmdl, seaice_anal)
       end if
       deallocate(dummy2d)

     elseif (isgrib == 2) then  ! grib 2 file

       j       = 0      ! search at beginning of file
       jdisc   = 10     ! search for discipline; 10 - ocean products
       jpdtn   = 0      ! search for product definition template number 0 - anal at one lvl
       jgdtn   = gdtnum_mdl  ! search for grid definition template number of
                             ! model grid.
       jids    = -9999  ! array of values in identification section, set to wildcard
       jgdt    = -9999  ! array of values in grid definition template 3.m
       jgdt(8) = imdl   ! does analysis file have correct i/j dimensions?
       jgdt(9) = jmdl
       jpdt    = -9999  ! array of values in product definition template 4.n
       jpdt(1) = 2      ! search for parameter category - ice
       jpdt(2) = 0      ! search for parameter number - ice cover
       unpack  = .true. ! unpack data

       call grib2_null(gfld)

       if (me==0) print*,"- OPEN FILE ", trim(seaice_file)
       iunit = 13
       lugi  = 0
       call baopenr (iunit, seaice_file, ierr)

       if (ierr /=0) then
         if (me==0) print*,'*** WARNING: ERROR OPENING SEA ICE ANALYSIS ***'
         if (me==0) print*,'*** WILL CYCLE SEA ICE ***'
       else
         if (me==0) print*,"- DEGRIB DATA"
         call getgb2(iunit, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
                     unpack, k, gfld, ierr)
         if (ierr /=0) then
           if (me==0) print*,'*** WARNING: ERROR READING SEA ICE ANALYSIS ***'
           if (me==0) print*,'*** WILL CYCLE SEA ICE ***'
         else
           allocate(dummy2d(imdl,jmdl))
           dummy2d=reshape (gfld%fld , (/imdl,jmdl/) )
           allocate (seaice_anal(ijmdl))
           call thin_data(dummy2d, ipts, jpts, lonsperlat,   &
                          ijmdl, jmdl2, imdl, jmdl, seaice_anal)
           deallocate(dummy2d)
         end if
         call baclose(iunit, ierr)
       end if

       call grib2_free(gfld)

     else  

       if (me==0) print*,'- FATAL ERROR: SEA ICE ANALYSIS NOT GRIB1 OR GRIB2 ***'
       call w3tage('SFCUPDATE')
       call mpi_abort(mpi_comm_world, 12, ierr)

     end if ! file grib1 or grib2

   else

     if (me==0) print *,'- SEA ICE FILE NOT FOUND. CYCLE SEA ICE.'

   end if  ! sea ice file exists

 end if  ! sea ice file 

!-----------------------------------------------------------------------
! if snow analysis is provided.  then read it and process it over
! land points.  otherwise, cycle snow depth over land.
!-----------------------------------------------------------------------

 if (merge_coeff_snow_depth < 99999. .and. len_trim(snow_depth_anal_dir) > 0) then

   snow_file=trim(snow_depth_anal_dir) // "/snow." // curr_date // &
                                          "." // trim(domain_name) // ".grb"

   if (me==0) print*,"- LOOK FOR SNOW ANALYSIS: ",trim(snow_file)

   inquire (file=trim(snow_file), exist=itexists)

   if (itexists) then

     if (me==0) print*,"- WILL READ SNOW ANALYSIS: ",trim(snow_file)

     call grib_check(snow_file, isgrib)

     if (isgrib == 1) then  ! grib 1 file

       parm_num = 66

       allocate(dummy2d(imdl,jmdl))
       call read_grib_data(snow_file, parm_num, dummy2d, &
                          (imdl*jmdl), status, me)

       if (status /= 0) then
         if (me==0) print*,'*** WARNING: ERROR READING SNOW DEPTH ***'
         if (me==0) print*,'*** WILL CHECK FOR SNOW COVER ***'
         parm_num = 238
         call read_grib_data(snow_file, parm_num, dummy2d, &
                          (imdl*jmdl), status, me)
         if (status /= 0) then
           if (me==0) print*,'*** WARNING: ERROR READING SNOW COVER ***'
           if (me==0) print*,'*** WILL CYCLE SNOW ***'
         else
           allocate (snow_cover_anal(ijmdl)) ! in percent
           call thin_data(dummy2d, ipts, jpts, lonsperlat,   &
                        ijmdl, jmdl2, imdl, jmdl, snow_cover_anal)
         endif
       else
         allocate (snow_depth_anal(ijmdl))   ! in meters
         call thin_data(dummy2d, ipts, jpts, lonsperlat,   &
                        ijmdl, jmdl2, imdl, jmdl, snow_depth_anal)
       end if
       deallocate(dummy2d)
  
     elseif (isgrib == 2) then  ! grib 2 file

       j       = 0      ! search at beginning of file
       jdisc   = 0      ! search for discipline; 0 - meteo products
       jpdtn   = 0      ! search for product definition template number 0 - anal at one lvl
       jgdtn   = gdtnum_mdl  ! search for grid definition template number of
                             ! model grid.
       jids    = -9999  ! array of values in identification section, set to wildcard
       jgdt    = -9999  ! array of values in grid definition template 3.m
       jgdt(8) = imdl   ! does analysis file have correct i/j dimensions?
       jgdt(9) = jmdl
       jpdt    = -9999  ! array of values in product definition template 4.n
       jpdt(1) = 1      ! search for parameter category - moisture
       jpdt(2) = 11     ! search for parameter number - snow depth
       unpack  = .true. ! unpack data

       call grib2_null(gfld)

       if (me==0) print*,"- OPEN FILE ", trim(snow_file)
       iunit = 13
       lugi  = 0
       call baopenr (iunit, snow_file, ierr)

       if (ierr /=0) then
         if (me==0) print*,'*** WARNING: ERROR OPENING SNOW ANALYSIS FILE ***'
         if (me==0) print*,'*** WILL CYCLE SNOW ***'
       else
         if (me==0) print*,"- DEGRIB SNOW DEPTH"
         call getgb2(iunit, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
                     unpack, k, gfld, ierr)
         if (ierr /=0) then
           if (me==0) print*,'*** WARNING: ERROR READING SNOW DEPTH ***'
           if (me==0) print*,'*** WILL CHECK FOR SNOW COVER ***'
           jpdt(1) = 1      ! search for parameter category - moisture
           jpdt(2) = 42     ! search for parameter number - snow cover
           print*,"- DEGRIB SNOW COVER"
           call getgb2(iunit, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
                       unpack, k, gfld, ierr)
           if (ierr /= 0) then
             if (me==0) print*,'*** WARNING: ERROR READING SNOW COVER ***'
             if (me==0) print*,'*** WILL CYCLE SNOW ***'
           else
             allocate(dummy2d(imdl,jmdl))
             dummy2d=reshape (gfld%fld , (/imdl,jmdl/) )
             allocate (snow_cover_anal(ijmdl))
             call thin_data(dummy2d, ipts, jpts, lonsperlat,   &
                            ijmdl, jmdl2, imdl, jmdl, snow_cover_anal)
             deallocate(dummy2d)
           end if
         else
           allocate(dummy2d(imdl,jmdl))
           dummy2d=reshape (gfld%fld , (/imdl,jmdl/) )
           allocate (snow_depth_anal(ijmdl))
           call thin_data(dummy2d, ipts, jpts, lonsperlat,   &
                          ijmdl, jmdl2, imdl, jmdl, snow_depth_anal)
           deallocate(dummy2d)
         end if
         call baclose(iunit, ierr)
       end if

       call grib2_free(gfld)

     else  

       if (me==0) print*,'- FATAL ERROR: SNOW ANALYSIS NOT GRIB1 OR GRIB2 ***'
       call w3tage('SFCUPDATE')
       call mpi_abort(mpi_comm_world, 13, ierr)

     end if ! is file grib1 or grib 2?

   else

     if (me==0) print *,'- SNOW FILE NOT FOUND. CYCLE SNOW.'

   end if  ! does snow file exist?

 end if

!-----------------------------------------------------------------------
! Fire mask (contains percent grid box that has been burned).
! There are two of these files - one for a daily accumulation and
! one for a monthly accumulation.
!-----------------------------------------------------------------------

 if ( len_trim(fire_anal_dir) > 0 ) then

   fire_mask_file=trim(fire_anal_dir) // "/fire." // curr_date //   &
                         ".daily." // trim(domain_name) // ".grib2"

   print *,'- LOOK FOR DAILY ACCUMULATED FIRE FILE: ', trim(fire_mask_file)

   inquire (file=trim(fire_mask_file), exist=itexists)

   if (itexists) then

     print*,"- WILL READ FIRE FILE: ",trim(fire_mask_file)

     lugb=20
     call baopenr(lugb,fire_mask_file,status)

     if (status /= 0) then

       print*,'*** WARNING: ERROR OPENING FILE.  STATUS IS: ', status
       print*,'*** WILL NOT APPLY FIRE DATA ***'

     else  ! file opened, read daily fire data

       j = 0
       lugi = 0        ! no index file
       jdisc = 2       ! discipline; '2' is land sfc products
       jpdtn = 8       ! product def template '8' - accumulation
       jgdtn = -1
       jids = -9999
       jgdt = -9999
       jpdt = -9999    ! section 4
       jpdt(1) = 4     ! oct 10, parameter cat - fire wx products
       jpdt(2) = 3     ! oct 11, parameter number - % burned area
       unpack=.true.

       call grib2_null(gfld)

       call getgb2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
                 unpack, k, gfld, status)

       if (status /= 0) then
         print*,'*** WARNING: ERROR READING FIRE ANALYSIS *** status: ',status
         print*,'*** WILL NOT APPLY FIRE DATA ***'
       else
         allocate(dummy2d(imdl,jmdl))
         dummy2d = reshape( gfld%fld, (/imdl,jmdl/) )
         allocate (burnt_frac_daily_anal(ijmdl))   ! in percent
         call thin_data(dummy2d, ipts, jpts, lonsperlat,   &
                        ijmdl, jmdl2, imdl, jmdl, burnt_frac_daily_anal)
         deallocate(dummy2d)
       end if

       call baclose(lugb,status)

       call grib2_free(gfld)

     endif  ! bad file open

   else
    
     print*,'- DAILY ACCUMULATED FIRE FILE NOT FOUND.'

   end if  ! does daily fire file exist

   fire_mask_file=trim(fire_anal_dir) // "/fire." // curr_date //   &
                         ".monthly." // trim(domain_name) // ".grib2"

   print *,'- LOOK FOR MONTHLY ACCUMULATED FIRE FILE: ', trim(fire_mask_file)

   inquire (file=trim(fire_mask_file), exist=itexists)

   if (itexists) then

     print*,"- WILL READ MONTHLY FIRE FILE: ",trim(fire_mask_file)

     lugb=20
     call baopenr(lugb,fire_mask_file,status)

     if (status /= 0) then

       print*,'*** WARNING: ERROR OPENING FILE.  STATUS IS: ', status
       print*,'*** WILL NOT APPLY FIRE DATA ***'

     else  ! file opened, read monthly fire data

       j = 0
       lugi = 0        ! no index file
       jdisc = 2       ! discipline; '2' is land sfc products
       jpdtn = 8       ! product def template '8' - accumulation
       jgdtn = -1
       jids = -9999
       jgdt = -9999
       jpdt = -9999    ! section 4
       jpdt(1) = 4     ! oct 10, parameter cat - fire wx products
       jpdt(2) = 3     ! oct 11, parameter number - % burned area
       unpack=.true.

       call grib2_null(gfld)

       call getgb2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
                 unpack, k, gfld, status)

       if (status /= 0) then
         print*,'*** WARNING: ERROR READING FIRE ANALYSIS *** status: ',status
         print*,'*** WILL NOT APPLY FIRE DATA ***'
       else
         allocate(dummy2d(imdl,jmdl))
         dummy2d = reshape( gfld%fld, (/imdl,jmdl/) )
         allocate (burnt_frac_monthly_anal(ijmdl))   ! in percent
         call thin_data(dummy2d, ipts, jpts, lonsperlat,   &
                        ijmdl, jmdl2, imdl, jmdl, burnt_frac_monthly_anal)
         deallocate(dummy2d)
       end if

       call baclose(lugb,status)

       call grib2_free(gfld)

     endif  ! bad file open

   else

     print*,'- MONTHLY ACCUMULATED FIRE FILE NOT FOUND.'

   endif  ! monthly file exists

! if monthly data is not read (because of a read error, or user settings) then
! set the monthly analysis to the daily analysis.
  
   if (allocated(burnt_frac_daily_anal) .and. .not.(allocated(burnt_frac_monthly_anal))) then
     allocate (burnt_frac_monthly_anal(ijmdl))
     burnt_frac_monthly_anal = burnt_frac_daily_anal
   endif

 end if  ! look for fire data

 return

 end subroutine read_sfc_anal_data

 subroutine read_config (me)
!$$$  subprogram documentation block
!
! subprogram:    read_config
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: read program run configuration namelist
!
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call read_config with the following arguments
!
!   input argument list:  
!     me              - mpi task number
!
!   output argument list: none
!
! files:
!   input:
!     - configuration namelist, fort.41
!
!   output: none
!    
! condition codes: all fatal
!   51 - invalid vegetation type source
!   52 - invalid soil type source
!   53 - bad read of namelist file
!   54 - bad open of namelist file
!
! remarks: none.
!
!$$$

 implicit none

 include 'mpif.h'

 integer                              :: istat
 integer, intent(in)                  :: me

 namelist /time_varying_analysis_flds/  snowfree_albedo_climo_file,  &
                                        greenfrc_climo_file,         &
                                        z0_climo_file,               &
                                        seaice_anal_dir,             &
                                        seaice_climo_file,           &
                                        sst_anal_dir,                &
                                        sst_climo_file,              &
                                        merge_coeff_sst,             &
                                        sst_anlcyc_update,           &
                                        soilm_climo_file,            &
                                        merge_coeff_soilm,           &
                                        snow_depth_anal_dir,         &
                                        snow_depth_climo_file,       &
                                        merge_coeff_snow_depth,      &
                                        fire_anal_dir

 namelist /soil_parameters/             soil_type_src,          &
                                        smclow,                 &
                                        smchigh,                &
                                        smcmax,                 &
                                        beta,                   &
                                        psis,                   &
                                        satdk
                                    
 namelist /veg_parameters/              veg_type_src,           &
                                        salp,                   &
                                        snup

 namelist /grid_info/                   domain_name

 if (me == 0) then
   print*,''
   print*,"- READ CONFIGURATION NAMELIST"
 end if

 open(41, iostat=istat, err = 9100)

 read(41, nml=grid_info, iostat=istat, err=9000)

 read(41, nml=time_varying_analysis_flds, iostat=istat, err=9000)

 read(41, nml=soil_parameters, iostat=istat, err=9000)

 read(41, nml=veg_parameters, iostat=istat, err=9000)

 close(41)

 if (trim(veg_type_src) == "usgs") then
   num_veg_types = 24
 elseif (trim(veg_type_src) == "igbp") then
   num_veg_types = 20
 elseif (trim(veg_type_src) == "sib") then
   num_veg_types = 13
 else
   if (me == 0) print*,"- FATAL ERROR: INVALID VEGETATION TYPE SOURCE: ", trim(veg_type_src)
   call mpi_abort(mpi_comm_world, 51, istat) 
 end if

 if (trim(soil_type_src) == "statsgo") then
   num_soil_types = 16
 elseif (trim(soil_type_src) == "zobler") then
   num_soil_types = 9
 else
   if (me == 0) print*,"- FATAL ERROR: INVALID SOIL TYPE SOURCE: ", trim(soil_type_src)
   call mpi_abort(mpi_comm_world, 52, istat) 
 end if

 return

!-----------------------------------------------------------------------
! error handling.
!-----------------------------------------------------------------------

 9000 print*,''
      print*,'- FATAL ERROR: BAD READ ON CONFIG NAMELIST.  ISTAT IS ', istat
      close(41)
      call mpi_abort(mpi_comm_world, 53, istat)

 9100 print*,''
      print*,'- FATAL ERROR: BAD OPEN ON CONFIG NAMELIST.  ISTAT IS ', istat
      call mpi_abort(mpi_comm_world, 54, istat)

 end subroutine read_config

 subroutine read_data_cleanup
!$$$  subprogram documentation block
!
! subprogram:    read_data_cleanup
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: clean up memory
!
! program history log:
! 2005-05-20  gayno    - initial version
! 2015-07-08  gayno    - add cleanup of wild fire data arrays
!                        ("burnt_frac")
!
! usage: call read_data_cleanup with no arguments
!
!   input argument list: none
!
!   output argument list: none
!
! files: none
!
! condition codes: none
!
! remarks: none.
!
!$$$

 implicit none

 if (allocated(alnsf_climo_dates))           deallocate (alnsf_climo_dates)
 if (allocated(alnwf_climo_dates))           deallocate (alnwf_climo_dates)
 if (allocated(alvsf_climo_dates))           deallocate (alvsf_climo_dates)
 if (allocated(alvwf_climo_dates))           deallocate (alvwf_climo_dates)
 if (allocated(burnt_frac_daily_anal))       deallocate (burnt_frac_daily_anal)
 if (allocated(burnt_frac_monthly_anal))     deallocate (burnt_frac_monthly_anal)
 if (allocated(greenfrc_climo_dates))        deallocate (greenfrc_climo_dates)
 if (allocated(seaice_climo_dates))          deallocate (seaice_climo_dates)
 if (allocated(snow_depth_climo_dates))      deallocate (snow_depth_climo_dates)
 if (allocated(snowfree_albedo_climo_dates)) deallocate (snowfree_albedo_climo_dates)
 if (allocated(soilm_climo_dates))           deallocate (soilm_climo_dates)
 if (allocated(sst_climo_dates))             deallocate (sst_climo_dates)
 if (allocated(z0_climo_dates))              deallocate (z0_climo_dates)
 if (allocated(alnsf_climo_all))             deallocate (alnsf_climo_all)
 if (allocated(alnwf_climo_all))             deallocate (alnwf_climo_all)
 if (allocated(alvsf_climo_all))             deallocate (alvsf_climo_all)
 if (allocated(alvwf_climo_all))             deallocate (alvwf_climo_all)
 if (allocated(greenfrc_climo_all))          deallocate (greenfrc_climo_all)
 if (allocated(seaice_climo_all))            deallocate (seaice_climo_all)
 if (allocated(snow_depth_climo_all))        deallocate (snow_depth_climo_all)
 if (allocated(snowfree_albedo_climo_all))   deallocate (snowfree_albedo_climo_all)
 if (allocated(soilm_climo_all))             deallocate (soilm_climo_all)
 if (allocated(sst_climo_all))               deallocate (sst_climo_all)
 if (allocated(z0_climo_all))                deallocate (z0_climo_all)

 return
 
 end subroutine read_data_cleanup

 end module read_data

