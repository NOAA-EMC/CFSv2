 subroutine gridgen_sfc(grid_mdl)
!$$$ subroutine documentation block
!
! subprogram:  grid generation - surface
!   prgmmr: gayno          org: w/np2           date: ????
!
! $Revision$
!
! abstract: driver routine to compute static fields
!   on the model grid and write the result to grib files
!
! program history log:
! ????        gayno     - initial version
!
! usage:  call routine with one argument
!   inputs:
!      grid_mdl - data structure that holds the grid specs
!   outputs:
!      n/a
!
! files: none
!
! condition codes: none
!
! remarks: none.
!
!$$$
 use program_setup, only   : setup, domain_type,  &
                             setup_cleanup, grib2

 use calc_latlons,  only   : calc_latlons_mdl, &
                             calc_latlons_cleanup 
 
 use lsmask_orog,  only    : lsmask_orog_driver, &
                             lsmask_orog_cleanup

 use soil_vegtype_tile, only : soil_vegtype_driver

 use init_grib1, only        : init_pds_gds

 use mpimod, only          : myrank, mpi_cleanup

 use grid_info, only       : grid_specs

 implicit none

 type (grid_specs)         :: grid_mdl

 call w3tagb('GRIDGEN_SFC',2005,0136,0000,'NP2')

!-----------------------------------------------------------------------
! get user-specified options and grid information.
!-----------------------------------------------------------------------

 call setup(grid_mdl)

!-----------------------------------------------------------------------
! calc lat/lons on the model grid
!-----------------------------------------------------------------------

 call calc_latlons_mdl

!-----------------------------------------------------------------------
! if outputing data in grib 1, set up grid header information.
! grib2 initialization occurs in the individual routines.
!-----------------------------------------------------------------------

 if (.not. grib2 .or. trim(domain_type) == 'gaussian') call init_pds_gds

!-----------------------------------------------------------------------
! initialize variables required for lat/lon to i/j transforms.
!-----------------------------------------------------------------------

 call init_grid

!-----------------------------------------------------------------------
! get land sea mask and orography.
!-----------------------------------------------------------------------

 call lsmask_orog_driver

!-----------------------------------------------------------------------
! interpolate leaf area index.
!-----------------------------------------------------------------------

 call leaf_area_index

!-----------------------------------------------------------------------
! interpolate hi-res soil and vegetation types using tiling option.
!-----------------------------------------------------------------------

 call soil_vegtype_driver

!-----------------------------------------------------------------------
! interpolate greenness fraction.
!-----------------------------------------------------------------------

 call green 

!-----------------------------------------------------------------------
! interpolate maximum snow albedo.
!-----------------------------------------------------------------------

 call max_snow_albedo 

!-----------------------------------------------------------------------
! interpolate slope type.
!-----------------------------------------------------------------------
 
 call slope_type

!-----------------------------------------------------------------------
! interpolate roughness data.
!-----------------------------------------------------------------------

 call roughness

!-----------------------------------------------------------------------
! interpolate snow free albedo.
!-----------------------------------------------------------------------

 call snow_free_albedo

!-----------------------------------------------------------------------
! interpolate soil substrate temperature.
! does not take advantage of mpi yet.
!-----------------------------------------------------------------------

 call soil_substrate 

!-----------------------------------------------------------------------
! grib lat/lons
!-----------------------------------------------------------------------

 call grib_latlons

!-----------------------------------------------------------------------
! free up memory.
!-----------------------------------------------------------------------

 call lsmask_orog_cleanup
 call calc_latlons_cleanup
 call setup_cleanup
 call mpi_cleanup

 print*,"************************************"
 print*,"** NORMAL TERMINATION FOR TASK:", myrank,"**"
 print*,"************************************"

 call w3tage('GRIDGEN_SFC')

 return

 end subroutine gridgen_sfc
