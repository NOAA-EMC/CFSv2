 module grid_info
!$$$ module documentation block
!
! module:  grid info
!   prgmmr: gayno          org: w/np2           date: ????
!
! $Revision$
!
! abstract: contains a data structure to hold grid specs
!   for the map projections used by this program.
!   also contains a routine to read in the grid specs
!   from a namelist and place them within the data
!   structure.
!
! program history log:
! ????        gayno     - initial version
!
! usage: use grid_info
!
! remarks: none
!
!$$$
 implicit none

 private

 integer, parameter, public :: max_gen = 10

 type, public :: grid_specs
   character(len=20)    :: domain_name="xxx"
   character(len=20)    :: domain_type="xxx"
   character(len=150)   :: gfs_lpl_file=""
   integer              :: imdl=-999
   integer              :: jmdl=-999
   integer              :: imdl_parent(max_gen)=-999
   integer              :: jmdl_parent(max_gen)=-999
   real                 :: centlat_mdl=-999.
   real                 :: centlon_mdl=-999.
   real                 :: centlat_parent_mdl(max_gen)=-999.
   real                 :: centlon_parent_mdl(max_gen)=-999.
   real                 :: dx_mdl=-999.
   real                 :: dy_mdl=-999.
   real                 :: dx_parent_mdl(max_gen)=-999.
   real                 :: dy_parent_mdl(max_gen)=-999.
   real                 :: hemi_mdl=-999.
   real                 :: lat_11_mdl=-999.  ! first grid pt
   real                 :: lon_11_mdl=-999.
   real                 :: lat_22_mdl=-999.  ! last grid pt
   real                 :: lon_22_mdl=-999.
   real                 :: orient_lon_mdl=-999.
   real                 :: tangent_lat_mdl=-999.
 end type grid_specs

 public read_grid_info

 contains

 subroutine read_grid_info(grid_mdl)
!$$$ subroutine documentation block
!
! subroutine:  read grid info
!   prgmmr: gayno          org: w/np2           date: ????
!
! abstract: read the model grid specs from namelist
!   and place in the grid_specs data structure.
!
! program history log:
! ????        gayno     - initial version
!
! usage: call routine with one argument:
!    grid_mdl - data structure to hold model grid specs.
!
! files:
!   input:
!     "grid" namelist, fort.41
!
!   output: none
!
! condition codes:
!   1 - error opening namelist file
!   2 - error reading namelist file
!      
! remarks: none
!
!$$$
 implicit none

 include 'mpif.h'

 type(grid_specs)   :: grid_mdl

 character(len=20)  :: domain_name
 character(len=20)  :: domain_type
 character(len=150) :: gfs_lpl_file
 integer            :: imdl
 integer            :: jmdl
 integer            :: imdl_parent(max_gen)
 integer            :: jmdl_parent(max_gen)
 integer            :: istat
 real               :: centlat_mdl
 real               :: centlon_mdl
 real               :: centlat_parent_mdl(max_gen)
 real               :: centlon_parent_mdl(max_gen)
 real               :: dx_mdl
 real               :: dy_mdl
 real               :: dx_parent_mdl(max_gen)
 real               :: dy_parent_mdl(max_gen)
 real               :: hemi_mdl
 real               :: lat_11_mdl
 real               :: lon_11_mdl
 real               :: lat_22_mdl
 real               :: lon_22_mdl
 real               :: orient_lon_mdl
 real               :: tangent_lat_mdl

 namelist /grid/ domain_name, domain_type, imdl, jmdl, centlat_mdl, centlon_mdl,  &
                 dx_mdl, dy_mdl, hemi_mdl, lat_11_mdl, lon_11_mdl,   &
                 lat_22_mdl, lon_22_mdl, orient_lon_mdl, tangent_lat_mdl, & 
                 imdl_parent, jmdl_parent, dx_parent_mdl, dy_parent_mdl, &
                 centlat_parent_mdl, centlon_parent_mdl, gfs_lpl_file

!-----------------------------------------------------------------------
! initialize map projection info to missing values.  these will
! either be read from the namelist or set in the code.
!-----------------------------------------------------------------------

 domain_name="xxx"
 domain_type="xxx"
 imdl = -999                ! all projections
 jmdl = -999                ! all projections
 centlat_mdl = -999.        ! b and egrid only
 centlon_mdl = -999.        ! b and egrid only
 dx_mdl = -999.             ! all but gaussian
 dy_mdl = -999.             ! all but gaussian
 hemi_mdl = -999.           ! polar stereographic only
 lat_11_mdl = -999.         ! polar, lambert conf, mercator, latlon
 lon_11_mdl = -999.         ! polar, lambert conf, mercator, latlon
 lat_22_mdl = -999.         ! mercator
 lon_22_mdl = -999.         ! mercator
 orient_lon_mdl = -999.     ! polar and lambert conf only
 tangent_lat_mdl = -999.    ! lambert conformal, mercator
 dx_parent_mdl = -999.      ! for b grid nests
 dy_parent_mdl = -999.      ! for b grid nests
 imdl_parent = -999         ! for b grid nests
 jmdl_parent = -999         ! for b grid nests
 centlat_parent_mdl = -999. ! for b grid nests
 centlon_parent_mdl = -999. ! for b grid nests
 gfs_lpl_file=''            ! for gfs grids

 print*,"- READ GRID INFO NAMELIST"

 open(41, iostat=istat, err=900)
 read(41, nml=grid, iostat=istat, err=910)
 close(41)

 grid_mdl%domain_name = domain_name
 grid_mdl%domain_type = domain_type
 grid_mdl%imdl = imdl
 grid_mdl%jmdl = jmdl
 grid_mdl%centlat_mdl = centlat_mdl
 grid_mdl%centlon_mdl = centlon_mdl
 grid_mdl%dx_mdl = dx_mdl
 grid_mdl%dy_mdl = dy_mdl
 grid_mdl%hemi_mdl = hemi_mdl
 grid_mdl%lat_11_mdl = lat_11_mdl
 grid_mdl%lon_11_mdl = lon_11_mdl
 grid_mdl%lat_22_mdl = lat_22_mdl
 grid_mdl%lon_22_mdl = lon_22_mdl
 grid_mdl%orient_lon_mdl = orient_lon_mdl
 grid_mdl%tangent_lat_mdl = tangent_lat_mdl
 grid_mdl%dx_parent_mdl = dx_parent_mdl
 grid_mdl%dy_parent_mdl = dy_parent_mdl
 grid_mdl%imdl_parent = imdl_parent
 grid_mdl%jmdl_parent = jmdl_parent
 grid_mdl%centlat_parent_mdl = centlat_parent_mdl
 grid_mdl%centlon_parent_mdl = centlon_parent_mdl
 grid_mdl%gfs_lpl_file = gfs_lpl_file 

 return

900 print*,"- FATAL ERROR OPENING CONFIG NAMELIST. ISTAT IS ", istat
    call mpi_abort(mpi_comm_world, 1, istat)

910 print*,"- FATAL ERROR READING CONFIG NAMELIST. ISTAT IS ", istat
    call mpi_abort(mpi_comm_world, 2, istat)

 end subroutine read_grid_info

 end module grid_info
