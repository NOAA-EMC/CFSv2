 subroutine sfccycle2(greenfrc, soil_temp,  &
                      soilm_tot, soilm_liq,     &
                      snowfree_albedo, albrecs,       &
                      albedo, mxsnow_albedo,          &
                      substrate_temp, skin_temp,      &
                      snow_depth, snow_liq_equiv,     &
                      seaice, canopy_mc,         &
                      z0, soil_type,                  &
                      veg_type, lsmask,               &
                      imdl, jmdl, ijmdl,              &
                      lonsperlat, jmdl2, nsoil, &
                      ipts, jpts, grid_type, gdtnum_mdl,           &
                      cycle_year, cycle_month,        &
                      cycle_day, cycle_hour,          &
                      fcst_hour, fhcyc, me)
!$$$  subprogram documentation block
!
! subprogram:   sfccycle2
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! $Revision$
!
! abstract: driver routine that performs the update of landsfc
!   fields common to both the regional and global models.
!
! program history log:
! 2005-05-20  gayno    - initial version
! 2009-05-08  gayno    - modified for new logic in process_data module
! 2014-11-10  gayno    - add gdtnum_mdl to argument list
! 2015-07-09  gayno    - add call to routine qc_fire_data to quality
!                        control wild fire analysis data.
!
! usage: call sfccycle2 with arguments defined below
!
!   input argument list: 
!     albrecs                     - number of snow free albedo records
!                                   (global uses 4, regional uses 1)
!     cycle_year/month/day/hour   - model cycle time
!     fcst_hour                   - forecast hour when running in
!                                   free forecast mode
!     fhcyc                       - frequency in hours that this
!                                   routine is called
!     grid_type                   - flag to tell routines whether a
!                                   global or regional grid is being
!                                   processed
!     gdtnum_mdl                  - grib 2 grid definition template
!                                   number of model grid
!     i/jmdl                      - i/j dimension of full model grid
!     ijmdl                       - number of model grid points to be
!                                   processed.
!     i/jpts                      - i/j indices of model grid that
!                                   will be processed. 
!     jmdl2                       - jmdl / 2
!     lonsperlat                  - for global grids, number of i-points
!                                   in each row when running in thinned
!                                   or reduced mode.
!     me                          - mpi task number
!     mxsnow_albedo               - maximum snow albedo
!     nsoil                       - number of soil layers
!
!   output argument list:
!     albedo                      - albedo including the effects of snow
!     canopy_mc                   - canopy moisture content
!     greenfrc                    - greenness fraction
!     lsmask                      - land mask
!     seaice                      - sea ice
!     snow_liq_equiv              - snow liquid water equivalent in meters
!     skin_temp                   - skin temperature
!     snow_depth                  - snow depth in meters
!     snowfree_albedo             - albedo ignoring effects of snow
!     soil_temp                   - soil temperature
!     soil_type                   - soil type
!     soilm_liq                   - soil moisture - liquid portion
!     soilm_tot                   - soil moisture - liquid + frozen
!     substrate_temp              - soil substrate temperature
!     veg_type                    - vegetation type (or landuse category)
!     z0                          - roughness length
!   
! files: none
!
! condition codes: none
!
! remarks: none.
!
!$$$

 use read_data,  only          : read_sfc_anal_data,   &
                                 salp,                 &
                                 snup,                 &
                                 num_veg_types

 use process_data, only        : process_roughness,    &
                                 process_vegtype,   &
                                 process_soiltype,  &
                                 process_snowfree_albedo_global,    &
                                 process_snowfree_albedo_regional,  &
                                 process_seaice,    &
                                 process_skint,     &
                                 process_soil_temp, &
                                 process_snow,      &
                                 qc_fire_data,      &
                                 process_sst,       &
                                 process_greenfrc,  &
                                 process_soilm,     &
                                 update_snow,       &
                                 update_sfalb

 use soil_utils, only          : calc_albedo

 implicit none

 character*8, intent(in)        :: grid_type

 integer, intent(in)            :: albrecs
 integer, intent(in)            :: cycle_day
 integer, intent(in)            :: cycle_hour
 integer, intent(in)            :: cycle_month
 integer, intent(in)            :: cycle_year
 integer, intent(in)            :: gdtnum_mdl
 integer                        :: ij
 integer, intent(in)            :: ijmdl 
 integer, intent(in)            :: imdl  
 integer, intent(in)            :: ipts(ijmdl)
 integer, intent(in)            :: jmdl
 integer, intent(in)            :: jmdl2
 integer, intent(in)            :: jpts(ijmdl)
 integer, intent(in)            :: lonsperlat(jmdl2)
 integer, intent(in)            :: me
 integer, intent(in)            :: nsoil
 integer, intent(inout)         :: soil_type(ijmdl)
 integer, intent(inout)         :: veg_type(ijmdl)

 real, intent(inout)            :: albedo(ijmdl)
 real, intent(inout)            :: canopy_mc(ijmdl)
 real, intent(in)               :: fcst_hour
 real, intent(in)               :: fhcyc
 real, intent(inout)            :: greenfrc(ijmdl)
 real, intent(inout)            :: lsmask(ijmdl)
 real, allocatable              :: lsmask_temp(:)
 real, intent(in)               :: mxsnow_albedo(ijmdl)
 real, intent(inout)            :: seaice(ijmdl)
 real, intent(inout)            :: snow_liq_equiv(ijmdl)     
 real, intent(inout)            :: skin_temp(ijmdl)
 real, intent(inout)            :: snow_depth(ijmdl) 
 real, intent(inout)            :: snowfree_albedo(ijmdl,albrecs)
 real, intent(inout)            :: soil_temp(ijmdl,nsoil)
 real, intent(inout)            :: soilm_liq(ijmdl,nsoil)
 real, intent(inout)            :: soilm_tot(ijmdl,nsoil)
 real, intent(inout)            :: substrate_temp(ijmdl)
 real, intent(inout)            :: z0(ijmdl)

!-----------------------------------------------------------------------
! read in external analyses of sea ice, snow and sst, if available.
!
! first time here, read climo data selected by user.  first and all
! subsequent time steps, interpolate climo fields to the
! current time step.
!-----------------------------------------------------------------------

 call read_sfc_anal_data(lonsperlat, imdl, jmdl, ijmdl,   &
                         jmdl2, ipts, jpts, grid_type,    &
                         gdtnum_mdl, cycle_year, cycle_month,         &
                         cycle_day, cycle_hour,           &
                         fcst_hour, fhcyc, me)

!-----------------------------------------------------------------------
! adjust fields based on snow/ice info
!-----------------------------------------------------------------------

 call process_seaice(seaice, lsmask, ijmdl, me)

 call process_sst(skin_temp, seaice, lsmask, fhcyc, ijmdl, me)

 call process_vegtype(veg_type, lsmask, seaice, ijmdl, me)

 call process_soiltype(soil_type, lsmask, seaice, ijmdl, me)

 call process_snow(snow_depth, snow_liq_equiv, lsmask, seaice, fhcyc, ijmdl, me)

 call qc_fire_data(snow_depth, lsmask, ijmdl, me)

 call process_skint(skin_temp, snow_liq_equiv, lsmask, &
                    seaice, veg_type, snup, num_veg_types, ijmdl, me)

 call process_soil_temp(soil_temp, substrate_temp,  &
                        skin_temp, lsmask, seaice, ijmdl, nsoil, me)

 if (trim(grid_type) == "global") then

   call process_snowfree_albedo_global(snowfree_albedo, albrecs,  &
                                       lsmask, seaice, ijmdl, me)

 elseif (trim(grid_type) == "regional") then

   call process_snowfree_albedo_regional(snowfree_albedo, albrecs,  &
                                         lsmask, seaice, ijmdl, me)

 end if

 call process_roughness(lsmask, seaice, z0, ijmdl, grid_type, me)

 call process_greenfrc(greenfrc, canopy_mc, lsmask, ijmdl, me)

!-----------------------------------------------------------------------
! call routine calc_albedo to update the albedo for snow effects
! over land. 
!
! at water and ice points, set to the snowfree value that was calculated
! in routine process_snowfree_albedo.
!-----------------------------------------------------------------------

 if (trim(grid_type) == "regional") then

   if (update_snow .or. update_sfalb) then
     print*,"- COMPUTE ALBEDO OVER LAND INCLUDING EFFECTS OF SNOW"
     call calc_albedo(lsmask, veg_type, ijmdl, salp, snup, num_veg_types,  &
                      snowfree_albedo, mxsnow_albedo, snow_liq_equiv, albedo) 
   endif

   do ij = 1, ijmdl
     if (lsmask(ij) == 0.0) albedo(ij) = snowfree_albedo(ij,1)
   enddo

 end if

!-----------------------------------------------------------------------
! nudge to climo total soil moisture and recalculate the liquid portion.
!-----------------------------------------------------------------------

 call process_soilm(soilm_tot, soilm_liq, soil_type, &
                    soil_temp, lsmask, ijmdl, nsoil, fhcyc, me)

!-----------------------------------------------------------------------
! switch back lsmask to nmm convention. 0-seaice/land; 1-open water.
!-----------------------------------------------------------------------

 if (trim(grid_type) == "regional") then

   allocate (lsmask_temp(ijmdl)) 
   lsmask_temp = 1.0   ! open water
   do ij = 1, ijmdl
     if (lsmask(ij) == 1.0) then  ! land
       lsmask_temp(ij) = 0.0
     end if
     if (seaice(ij) == 1.0) then  ! sea ice
       lsmask_temp(ij) = 0.0
     end if
   enddo

   lsmask = lsmask_temp
   deallocate (lsmask_temp)

!-----------------------------------------------------------------------
! switch back lsmask to global convention. 0-open water; 1-land; 2-ice.
!-----------------------------------------------------------------------

 elseif (trim(grid_type) == "global") then

   do ij = 1, ijmdl
     if (lsmask(ij) == 0.0 .and. seaice(ij) == 1.0) then      ! ice
       lsmask(ij) = 2.0
     elseif (lsmask(ij) == 1.0) then                          ! land
       lsmask(ij) = 1.0
     elseif (lsmask(ij) == 0.0 .and. seaice(ij) == 0.0) then  ! open water
       lsmask(ij) = 0.0
     end if
   enddo

 end if

 return

 end subroutine sfccycle2
