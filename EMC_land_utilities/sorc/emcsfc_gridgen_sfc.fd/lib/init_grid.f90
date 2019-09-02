 subroutine init_grid
!$$$ subroutine documentation block
!
! subprogram:  initial grid
!   prgmmr: gayno          org: w/np2           date: ????
!
! $Revision$
!
! abstract: driver that calls routines that initialize
!    some constants for the lat/lon to i/j transform routines.
!
! program history log:
! ????        gayno     - initial version
!
! usage:  call routine with no arguments
!
! files: none
!
! condition codes: none
!
! remarks: none.
!
!$$$
 use calc_latlons, only   : lat_first_mdl, lon_first_mdl

 use program_setup, only  : dx_mdl, dy_mdl, centlat_parent_mdl, centlon_parent_mdl, &
                            domain_type, jmdl, centlat_mdl, centlon_mdl,  &
                            hemi_mdl, orient_lon_mdl

 use ll2xy_util_bgrid, only :  ll2xy_bgrid_init

 use ll2xy_util_egrid, only :  ll2xy_egrid_init

 use ll2xy_util_polar, only :  ll2xy_polar_init

 implicit none

 if (trim(domain_type) == 'bgrid') then

   print*,"- INITIALIZE GRID VARIABLES FOR BGRIDS."
   call ll2xy_bgrid_init(lat_first_mdl, lon_first_mdl, centlat_parent_mdl(1), centlon_parent_mdl(1), &
                         dy_mdl, dx_mdl)
 elseif (trim(domain_type) == 'egrid') then

   print*,"- INITIALIZE GRID VARIABLES FOR EGRIDS."
   call ll2xy_egrid_init(jmdl, dy_mdl, -(dx_mdl), centlat_mdl, centlon_mdl)

 elseif (trim(domain_type) == 'polar') then

   print*,"- INITIALIZE GRID VARIABLES FOR POLAR GRIDS."
   call ll2xy_polar_init(lat_first_mdl, lon_first_mdl, orient_lon_mdl, hemi_mdl, dx_mdl, dy_mdl)

 end if

 return

 end subroutine init_grid
