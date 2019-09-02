 program sfcupdate
!$$$  main program documentation block
!                .      .    .                                       .
! main program: sfcupdate
!   prgmmr: gayno            ORG: NP2                DATE: 2005-05-20
!
! $Revision$
!
! abstract: updates snow, ice, sst and climatological fields
!   for the gfs or nmm models.  performs consistency checks
!   between fields.
!
! program history log:
!   2005-05-20  gayno     initial version
!   2009-05-08  gayno     add ability to read/write nems binary files
!   2014-11-10  gayno     logic to read snow, ice, sst and climatological
!                         fields in either grib 1 or grib 2
!   2015-07-09  gayno     add option to adjust surface fields affected
!                         by wild fires.
!
! usage: 
!
!   input files (required)
!     program configuration namelist; fort.81
!     first guess nmm-nemsio binary file
!     model land/sea mask in grib 1 or grib 2
!
!   input files (optional)
!     snow-free albedo climatology in grib 1 or grib 2
!     greenness fraction climatology in grib 1 or grib 2 
!     roughness length climatology in grib 1 
!     sea ice climatology in grib 1
!     sea ice analysis in grib 1 or grib 2
!     sst climatology in grib 1
!     sst analysis in grib 1 or grib 2
!     soil moisture climatology in grib 1
!     snow depth climatology in grib 1
!     snow analysis in grib 1 or grib 2
!     wild fire (% burned area) analysis in grib 2
!
!   output files: 
!     output nmm-nems binary file
!
!   exit states:  non-zero is fatal
!     cond =   0 - successful run
!          =  20 - model land mask file not grib1 or grib2
!          =  21 - bad degrib of model land mask file (grib2 version)
!          =  22 - bad open of file in routine grib_check
!          =  23 - unknown grid type in routine gdt_to_gds
!          =  24 - unknown grid type in routine grib2_check
!          =  40 - bad open on configuration namelist
!          =  41 - bad read on configuration-grid_info namelist
!          =  42 - bad read on configuration-cycle namelist
!          =  43 - bad read on configuration-model_flds namelist
!          =  44 - bad read on configuration-fixed_flds namelist
!          =  45 - bad read on configuration-output namelist
!          =  46 - bad read on configuration-settings namelist
!          =  47 - bad open on model land mask grib file
!          =  48 - bad read on model land mask grib1 header
!          =  49 - unknown gfs gaussian grid
!          =  51 - invalid vegetation type data source
!          =  52 - invalid soil type data source
!          =  53 - bad read on configuration namelist
!                  (read_data module)
!          =  54 - bad open on configuration namelist
!                  (read_data module)
!          =  57 - bad write on regional output nemsio file
!          =  58 - bad mpi initialization
!          =  60 - bad read on regional input nemsio file
!          =  61 - regional input nemsio file has wrong i/j dimensions
!          =  68 - bad read on soil moisture climo file
!          =  69 - bad read on sea ice climo file
!          =  70 - bad read on roughness climo file
!          =  71 - bad read on sst climo file
!          =  72 - bad read on snow depth climo file
!          =  73 - bad read on greenness climo file
!          =  74 - bad read on gfs snowfree albedo climo file
!          =  75 - bad read on snowfree albedo climo file
!          =  80 - output file type not set correctly
!          =  81 - first guess file type not set correctly
!
! remarks: none
!
!$$$
 use program_setup,   only     : read_config_nml,  &
                                 model_cfg,        &
                                 imdl,             &
                                 jmdl,             &
                                 ijmdl,            &
                                 nsoil,            &
                                 lonsperlat,       &
                                 jmdl2,            &
                                 ipts,             &
                                 jpts,             &
                                 grid_type,        &
                                 gdtnum_mdl,       &
                                 cycle_year,       &
                                 cycle_month,      &
                                 cycle_day,        &
                                 cycle_hour,       &
                                 fcst_hour,        &
                                 setup_cleanup,    &
                                 output_file_type, &
                                 first_guess_file_type

 use read_data, only           : read_data_cleanup

 implicit none

 include 'mpif.h'

 integer                      :: albrecs
 integer                      :: ierr
 integer                      :: me  
 integer, allocatable         :: soil_type(:)
 integer, allocatable         :: veg_type(:)

 real, allocatable            :: albedo(:)    ! includes effects of snow
                                              ! regional model only
 real, allocatable            :: canopy_mc(:) 
 real, parameter              :: fhcyc = 24.0 ! update frequency in hours.
 real, allocatable            :: greenfrc(:)
 real, allocatable            :: lsmask(:)    ! in this routine, 
                                              ! 0 - open water/ice
                                              ! 1 - land
 real, allocatable            :: mxsnow_albedo(:)
 real, allocatable            :: seaice(:)    ! in this routine,
                                              ! 0 - open water/land
                                              ! 1 - ice
 real, allocatable            :: snow_liq_equiv(:)
 real, allocatable            :: skin_temp(:)
 real, allocatable            :: snowfree_albedo(:,:)
 real, allocatable            :: snow_depth(:)
 real, allocatable            :: soil_temp(:,:)
 real, allocatable            :: soilm_liq(:,:)
 real, allocatable            :: soilm_tot(:,:)
 real, allocatable            :: sst(:)
 real, allocatable            :: substrate_temp(:)
 real, allocatable            :: z0(:)

 call w3tagb('SFCUPDATE',2005,0136,0000,'NP2')

 print*,""
 print*,"******************************"
 print*,"*** PROGRAM SURFACE UPDATE ***"
 print*,"******************************"
 print*,""

!-----------------------------------------------------------------------
! mpi i/o is used to read/write the nam nemsio formatted files.
!-----------------------------------------------------------------------

 call mpi_init(ierr)

 if (ierr /= 0) then
   print*,"** FATAL ERROR INITIALIZING MPI"
   call errexit(58)
 end if

 call mpi_comm_rank(mpi_comm_world, me, ierr)

 if (me > 0) goto 900

!-----------------------------------------------------------------------
! get cycle time and directory paths.
!-----------------------------------------------------------------------

 call read_config_nml

!-----------------------------------------------------------------------
! get model grid information.
!-----------------------------------------------------------------------

 call model_cfg

!-----------------------------------------------------------------------
! read in model data.
!-----------------------------------------------------------------------

 if (trim(grid_type) == "global") then
! order as alvsf, alvwf, alnsf, alnwf
   albrecs = 4  
   allocate(snowfree_albedo(ijmdl,albrecs))
 elseif (trim(grid_type) == "regional") then
   albrecs = 1
   allocate(snowfree_albedo(ijmdl,albrecs))
 end if

 allocate(albedo(ijmdl))
 allocate(canopy_mc(ijmdl))
 allocate(greenfrc(ijmdl))
 allocate(mxsnow_albedo(ijmdl))
 allocate(seaice(ijmdl))
 allocate(snow_liq_equiv(ijmdl))
 allocate(skin_temp(ijmdl))
 allocate(snow_depth(ijmdl))
 allocate(soilm_liq(ijmdl,nsoil)) 
 allocate(soilm_tot(ijmdl,nsoil)) 
 allocate(soil_temp(ijmdl,nsoil))
 allocate(soil_type(ijmdl))
 allocate(sst(ijmdl))
 allocate(z0(ijmdl))
 allocate(veg_type(ijmdl))
 allocate(substrate_temp(ijmdl))
 allocate(lsmask(ijmdl))

!if (trim(grid_type) == "global") then
!  program not used for global model yet.  future capability.
!  call read_data_global
!  albedo = -999.9  ! how does sarah update albedo in gfs?

 if (trim(grid_type) == "regional") then

   selectcase(trim(first_guess_file_type))
     case("nems" , "NEMS")  ! nems format
       call read_firstguess_data_regional_nems(snowfree_albedo, albrecs,        &
                                               albedo, mxsnow_albedo,           &  
                                               canopy_mc, greenfrc,             &
                                               seaice, snow_liq_equiv,          &
                                               snow_depth, skin_temp,           &
                                               soilm_liq, soilm_tot, soil_temp, &
                                               z0, veg_type, soil_type,    &
                                               substrate_temp, lsmask)
     case default
       print*,"- FATAL ERROR: MUST SPECIFY FIRST_GUESS_FILE_TYPE, ABORT."
       call mpi_abort(mpi_comm_world, 81, ierr)
     end select 

 end if

!-----------------------------------------------------------------------
! this routine is called by the fcst model as well.
!-----------------------------------------------------------------------

 call sfccycle2(greenfrc, soil_temp,            &
                soilm_tot, soilm_liq,           &
                snowfree_albedo, albrecs,       &
                albedo, mxsnow_albedo,          &
                substrate_temp, skin_temp,      &
                snow_depth, snow_liq_equiv,     &
                seaice, canopy_mc,         &
                z0, soil_type,                  &
                veg_type, lsmask,               &
                imdl, jmdl, ijmdl,              &
                lonsperlat, jmdl2, nsoil,       &
                ipts, jpts, grid_type, gdtnum_mdl, &
                cycle_year, cycle_month,        &
                cycle_day, cycle_hour, &
                fcst_hour, fhcyc, me)

!-----------------------------------------------------------------------
! free up memory.
!-----------------------------------------------------------------------

  call read_data_cleanup

!-----------------------------------------------------------------------
! write data in the format expected by each model.
!-----------------------------------------------------------------------

! program not used for global model yet.  future capability.
!if (trim(grid_type) == "global") then
!  call write_data_global

 if (trim(grid_type) == "regional") then

   selectcase(trim(output_file_type))
     case("nems" , "NEMS")  ! nems format
       call write_output_data_regional_nems(greenfrc,                          &
                                       snow_liq_equiv, snowfree_albedo,   &
                                       albrecs, albedo, canopy_mc,        &
                                       snow_depth, skin_temp,             &
                                       soilm_liq, soilm_tot, soil_temp,   &
                                       z0, veg_type, soil_type,      &
                                       substrate_temp, seaice, lsmask)
     case default
       print*,"- FATAL ERROR: MUST SPECIFY OUTPUT_FILE_TYPE, ABORT."
       call mpi_abort(mpi_comm_world, 80, ierr)
     end select 

 end if

!-----------------------------------------------------------------------
! free up memory.
!-----------------------------------------------------------------------

 call setup_cleanup

 deallocate(albedo)
 deallocate(canopy_mc)
 deallocate(greenfrc)
 deallocate(lsmask)
 deallocate(mxsnow_albedo)
 deallocate(seaice)
 deallocate(snow_liq_equiv)
 deallocate(skin_temp)
 deallocate(snow_depth)
 deallocate(snowfree_albedo)
 deallocate(soil_type)
 deallocate(soilm_liq)
 deallocate(soilm_tot)
 deallocate(soil_temp)
 deallocate(sst)
 deallocate(substrate_temp)
 deallocate(veg_type)
 deallocate(z0)

 900 continue

 call mpi_finalize(ierr)

 if (me == 0) then
   print*,""
   print*,"**************************"
   print*,"*** NORMAL TERMINATION ***"
   print*,"**************************"
   call w3tage('SFCUPDATE')
 endif

 stop 0

 end program sfcupdate
