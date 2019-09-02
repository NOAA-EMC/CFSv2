 module init_grib1
!$$$ module documentation block
!
! module:  initialize grib 1
!   prgmmr: gayno          org: w/np2           date: ????
!
! $Revision$
!
! abstract: contains routines to set up the grib 1 
!   grid description section and product definition
!   section.
!
! program history log:
! ????        gayno     - initial version
!
! usage: use init_grib1
!
! remarks: none
!
!$$$
 use program_setup, only           : domain_name, &
                                     domain_type, &
                                     imdl,        &
                                     jmdl,        &
                                     dx_mdl, dy_mdl, &
                                     tangent_lat_mdl

 use calc_latlons, only            : lat_mdl,  &
                                     lon_mdl, &
                                     lat_first_mdl, lon_first_mdl, &
                                     lat_last_mdl, lon_last_mdl


 integer, public                  :: kpds_mdl(200)
 integer, public                  :: kgds_mdl(200)

 contains

 subroutine init_pds_gds
!$$$ subroutine documentation block
!
! subprogram:  initialize grib 1 pds and gds
!
!   prgmmr: gayno          org: w/np2           date: ????
!
! abstract: driver routine to initialize the grib 1 product
!   description section and grid description section.
!
! program history log:
! ????        gayno     - initial version
!
! usage:  call routine with no arguments
!
! files: none
!
! condition codes:
!   1 - unrecognized grid projection, fatal
!
! remarks: none.
!
!$$$
 implicit none

 include 'mpif.h'

 integer                   :: iret

!-----------------------------------------------------------------------
! product definition section.
!-----------------------------------------------------------------------

 kpds_mdl = 0

 kpds_mdl(1) = 7    ! oct 5 - id of center
 kpds_mdl(2) = 96   ! oct 6 - generating process id
 select case (trim(domain_name)) ! oct 7 - grid definition 
   case("t382", "T382", "t878", "T878")
     kpds_mdl(3) = 255     ! ask nco what new number is 
   case("t574", "T574")
     kpds_mdl(3) = 9   
   case("t254", "T254")
     kpds_mdl(3) = 127 
   case("t126", "T126")
     kpds_mdl(3) = 126
   case("t62", "T62")
     kpds_mdl(3) = 98
   case ("t170", "T170")
     kpds_mdl(3) = 170
   case ("t190", "T190")
     kpds_mdl(3) = 255
   case("centnmm")
     kpds_mdl(3) = 91
   case("eta12km")
     kpds_mdl(3) = 96
   case("eta32km")
     kpds_mdl(3) = 192
   case("eastnmm")
     kpds_mdl(3) = 90
   case("westnmm")
     kpds_mdl(3) = 92
   case("aknmm")
     kpds_mdl(3) = 93
   case("ak_dgex")
     kpds_mdl(3) = 186
   case("conus_dgex")
     kpds_mdl(3) = 185
   case("prnmm")
     kpds_mdl(3) = 97
   case("hinmm")
     kpds_mdl(3) = 194
   case default
     kpds_mdl(3) = 255
 end select
 kpds_mdl(4) = 192 ! oct 8 - gds/bms flag
 kpds_mdl(5) = 1   ! oct 9 - parameter indicator
 kpds_mdl(6) = 1   ! oct 10 - type of level
 kpds_mdl(7) = 0   ! oct 11-12 - height/pressure of level
 kpds_mdl(8) = 100   ! oct 13 - year
 kpds_mdl(9) = 1   ! oct 14 - month
 kpds_mdl(10) = 1  ! oct 15 - day
 kpds_mdl(11) = 0  ! oct 16 - hour
 kpds_mdl(12) = 0  ! oct 17 - minute
 kpds_mdl(13) = 1  ! oct 18 - fcst time unit
 kpds_mdl(14) = 0  ! oct 19 - time range 1
 kpds_mdl(15) = 1  ! oct 20 - time range 2
 kpds_mdl(16) = 1  ! oct 21 - time range flag
 kpds_mdl(17) = 0  ! oct 22-23 - number included in average
 kpds_mdl(18) = 1  ! version nr of grib specification
 kpds_mdl(19) = 130 ! version nr of parameter table for land sfc modeling
 kpds_mdl(20) = 0  ! oct 24 - nr missing from average/accumulation
 kpds_mdl(21) = 19 ! oct 25 - century
 kpds_mdl(22) = 0  ! oct 27-28 - decimal scale factor
 kpds_mdl(23) = 4  ! oct 26 - subcenter                  
 kpds_mdl(24) = 0  ! oct 29 - reserved
 kpds_mdl(25) = 0  ! oct 30 - not used

 if (trim(domain_type) == 'gaussian') then

   call init_gds_gaussian

 else if (trim(domain_type) == 'egrid') then

   call init_gds_egrid

 else if (trim(domain_type) == 'bgrid' .or. trim(domain_type) == 'bgrid_global') then

   call init_gds_bgrid

 else if (trim(domain_type) == 'latlon') then

   call init_gds_latlon

 else if (trim(domain_type) == 'lambconf') then

   call init_gds_lambconf

 else if (trim(domain_type) == 'polar') then

   call init_gds_polar

 else if (trim(domain_type) == 'mercator') then

   call init_gds_mercator

 else

   print*,'- FATAL ERROR:'
   print*,'- GRIB1 OPTION DOES NOT SUPPORT THIS MAP PROJECTION ',trim(domain_type)
   call mpi_abort(mpi_comm_world, 1, iret)

 end if

 end subroutine init_pds_gds

 subroutine init_gds_mercator
!$$$ subroutine documentation block
!
! subprogram:  initialize grib 1 gds for mercator
!
!   prgmmr: gayno          org: w/np2           date: ????
!
! abstract: initialize the grib 1 gds for mercator grids
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
 implicit none

 kgds_mdl = 0

 kgds_mdl(1) = 1     ! oct 6 - type of grid (mercator)
 kgds_mdl(2) = imdl  ! oct 7-8 - # pts on latitude circle
 kgds_mdl(3) = jmdl  ! oct 9-10 - # pts on longitude circle
 kgds_mdl(4) = nint(lat_first_mdl*1000.0)  ! oct 11-13 - lat of origin
 kgds_mdl(5)  = nint(lon_first_mdl*1000.0) ! oct 14-16 - lon of origin
 kgds_mdl(6)  = 128  ! oct 17 - resolution flag
 kgds_mdl(7)  = nint(lat_last_mdl*1000.) ! oct 18-20 - lat of extreme point 
 kgds_mdl(8)  = nint(lon_last_mdl*1000.) ! oct 21-23 - lon of extreme point
 kgds_mdl(9)  = nint(tangent_lat_mdl*1000.0)  ! oct 24-26 - tangent latitude
 kgds_mdl(10) = 0    ! oct 27 - reserved
 kgds_mdl(11) = 64   ! oct 28 - scanning mode flag
 kgds_mdl(12) = nint(dx_mdl)  ! oct 29-31 - di
 kgds_mdl(13) = nint(dy_mdl)  ! oct 32-34 - dj
 kgds_mdl(19) = 0    ! oct 4  - # vert coordinate parameters
 kgds_mdl(20) = 255  ! oct 5  - not used, set to 255

 return

 end subroutine init_gds_mercator

 subroutine init_gds_gaussian
!$$$ subroutine documentation block
!
! subprogram:  initialize grib 1 gds for gaussian
!
!   prgmmr: gayno          org: w/np2           date: ????
!
! abstract: initialize the grib 1 gds for gfs gaussian grids
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
 implicit none

 kgds_mdl = 0

 kgds_mdl(1) = 4     ! oct 6 - type of grid (gaussian)
 kgds_mdl(2) = imdl  ! oct 7-8 - # pts on latitude circle
 kgds_mdl(3) = jmdl  ! oct 9-10 - # pts on longitude circle
 kgds_mdl(4) = nint(lat_first_mdl*1000.0)  ! oct 11-13 - lat of origin
 kgds_mdl(5)  = 0    ! oct 14-16 - lon of origin
 kgds_mdl(6)  = 128  ! oct 17 - resolution flag
 kgds_mdl(7)  = nint(lat_last_mdl*1000.) ! oct 18-20 - lat of extreme point 
 kgds_mdl(8)  = nint(lon_last_mdl*1000.) ! oct 21-23 - lon of extreme point
 kgds_mdl(9)  = nint((360.0 / float(imdl))*1000.0)  ! oct 24-25 - longitudinal increment
 kgds_mdl(10) = jmdl / 2    ! oct 26-27 - number of circles pole to equator
 kgds_mdl(11) = 0    ! oct 28 - scanning mode flag
 kgds_mdl(12) = 255  ! oct 29 - reserved
 kgds_mdl(19) = 0    ! oct 4  - # vert coordinate parameters
 kgds_mdl(20) = 255  ! oct 5  - not used, set to 255

 return

 end subroutine init_gds_gaussian

 subroutine init_gds_latlon
!$$$ subroutine documentation block
!
! subprogram:  initialize grib 1 gds for lat/lon grids
!
!   prgmmr: gayno          org: w/np2           date: ????
!
! abstract: initialize the grib 1 gds for regular lat/lon
!   grids.
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
 use program_setup, only       : dx_mdl,   &
                                 dy_mdl

 implicit none
 
 integer :: bit1, bit2

 kgds_mdl = 0

 kgds_mdl(1)  = 0     ! oct 6 - type of grid (lat/lon)
 kgds_mdl(2)  = imdl  ! oct 7-8 - # pts on latitude circle
 kgds_mdl(3)  = jmdl  ! oct 9-10 - # pts on longitude circle
 kgds_mdl(4)  = nint(lat_mdl(1,1)*1000.0)  ! oct 11-13 - lat of origin
 kgds_mdl(5)  = nint(lon_mdl(1,1)*1000.0)  ! oct 14-16 - lon of origin
 kgds_mdl(6)  = 128  ! oct 17 - resolution flag
 kgds_mdl(7)  = nint(lat_mdl(imdl,jmdl)*1000.) ! oct 18-20 - lat of last point 
 kgds_mdl(8)  = nint(lon_mdl(imdl,jmdl)*1000.) ! oct 21-23 - lon of last point
 kgds_mdl(9)  = nint(abs(dx_mdl)*1000.0)  ! oct 24-25 - longitudinal increment
 kgds_mdl(10) = nint(abs(dy_mdl)*1000.0)
! compute scan mode from sign of dx/dy
 bit1=0
 if (dx_mdl < 0.) bit1=1
 bit2=1
 if (dy_mdl < 0.) bit2=0
 kgds_mdl(11) = 128*bit1 + 64*bit2  ! oct 28 - scanning mode flag
 kgds_mdl(12) = 255  ! oct 29 - reserved
 kgds_mdl(19) = 0    ! oct 4  - # vert coordinate parameters
 kgds_mdl(20) = 255  ! oct 5  - not used, set to 255

 return

 end subroutine init_gds_latlon

 subroutine init_gds_egrid
!$$$ subroutine documentation block
!
! subprogram:  initialize grib 1 gds for nam "e" grids
!
!   prgmmr: gayno          org: w/np2           date: ????
!
! abstract: initialize the grib 1 gds for rotated lat/lon
!   grids with "E" stagger.
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
 use program_setup, only      : dx_mdl,      &
                                dy_mdl,      &
                                centlat_mdl, &
                                centlon_mdl

 implicit none

 kgds_mdl = 0

 kgds_mdl( 1) = 203        ! oct 6 - type of grid, arakawa staggered e-grid
 kgds_mdl( 2) = imdl       ! octs 7-8 - i dimension of grid
 kgds_mdl( 3) = jmdl       ! octs 9-10 - j dimension of grid                   
 kgds_mdl( 4) = nint(lat_first_mdl*1000.) ! latitude of first grid point, octs 11-13
 kgds_mdl( 5) = nint(lon_first_mdl*1000.) ! longitude of first grid point, octs 14-16
 kgds_mdl( 6) = 136        ! oct 17 - resolution and component flag
 kgds_mdl( 7) = nint(centlat_mdl*1000.)  
                           ! octs 18-20 - # mass points along southmost row
 kgds_mdl( 8) = nint(centlon_mdl*1000.)  
                           ! octs 21-23 - # rows in each column
 kgds_mdl( 9) = nint(dx_mdl*1000.)  ! octs 24-25 - long direction increment
 kgds_mdl(10) = nint(dy_mdl*1000.)  ! octs 26-27 - lat direction increment
 kgds_mdl(11) = 64         ! oct 28 - scanning mode flag
 kgds_mdl(19) = 0          ! oct 4  - # vert coordinate parameters
 kgds_mdl(20) = 255        ! oct 5  - not used, set to 255
   
 return

 end subroutine init_gds_egrid

 subroutine init_gds_bgrid
!$$$ subroutine documentation block
!
! subprogram:  initialize grib 1 gds for nam "b" grids
!
!   prgmmr: gayno          org: w/np2           date: ????
!
! abstract: initialize the grib 1 gds for rotated lat/lon
!   grids with "B" stagger.
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
 use program_setup, only      : dx_mdl,      &
                                dy_mdl,      &
                                centlat_parent_mdl, &
                                centlon_parent_mdl

 implicit none

 kgds_mdl = 0

 kgds_mdl( 1) = 205        ! oct 6 - type of grid, rotated lat/lon arakawa b-grid
 kgds_mdl( 2) = imdl       ! octs 7-8 - i dimension of grid
 kgds_mdl( 3) = jmdl       ! octs 9-10 - j dimension of grid                   
 kgds_mdl( 4) = nint(lat_first_mdl*1000.) ! latitude of first grid point, octs 11-13
 kgds_mdl( 5) = nint(lon_first_mdl*1000.) ! longitude of first grid point, octs 14-16
 kgds_mdl( 6) = 136        ! oct 17 - resolution and component flag
 kgds_mdl( 7) = nint(centlat_parent_mdl(1)*1000.)  
                           ! octs 18-20 - # mass points along southmost row
 kgds_mdl( 8) = nint(centlon_parent_mdl(1)*1000.)  
                           ! octs 21-23 - # rows in each column
 kgds_mdl( 9) = nint(dx_mdl*1000.)  ! octs 24-25 - long direction increment
 kgds_mdl(10) = nint(dy_mdl*1000.)  ! octs 26-27 - lat direction increment
 kgds_mdl(11) = 64         ! oct 28 - scanning mode flag
 kgds_mdl(12) = nint(lat_last_mdl*1000.) 
 kgds_mdl(13) = nint(lon_last_mdl*1000.)
 kgds_mdl(19) = 0          ! oct 4  - # vert coordinate parameters
 kgds_mdl(20) = 255        ! oct 5  - not used, set to 255
   
 return

 end subroutine init_gds_bgrid

 subroutine init_gds_lambconf
!$$$ subroutine documentation block
!
! subprogram:  initialize grib 1 gds for lambert conformal
!
!   prgmmr: gayno          org: w/np2           date: ????
!
! abstract: initialize the grib 1 gds for lambert conformal
!   grids.
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
 use program_setup, only      : dx_mdl,            &
                                tangent_lat_mdl,   &
                                orient_lon_mdl

 implicit none

 kgds_mdl = 0

 kgds_mdl( 1) = 3          ! oct 6 - type of grid, lambert conf.
 kgds_mdl( 2) = imdl       ! octs 7-8 - i dimension of grid
 kgds_mdl( 3) = jmdl       ! octs 9-10 - j dimension of grid                   
 kgds_mdl( 4) = nint(lat_first_mdl*1000.) ! latitude of first grid point, octs 11-13
 kgds_mdl( 5) = nint(lon_first_mdl*1000.) ! longitude of first grid point, octs 14-16
 kgds_mdl( 6) = 8          ! oct 17 - resolution and component flag
 kgds_mdl( 7) = nint(orient_lon_mdl*1000.)  
                           ! octs 18-20 - # mass points along southmost row
 kgds_mdl( 8) = nint(dx_mdl*1000.)  ! octs 21-23 - x-dir grid length meters
 kgds_mdl( 9) = nint(dx_mdl*1000.)  ! octs 24-26 - y-dir grid length meters
 kgds_mdl(10) = 0          ! oct 27 - projection center flag
 kgds_mdl(11) = 64         ! oct 28 - scanning mode flag
 kgds_mdl(12) = nint(tangent_lat_mdl*1000.) ! oct 29-31 - first lat from pole
                                            ! at which cone cuts earth.
 kgds_mdl(13) = nint(tangent_lat_mdl*1000.) ! oct 32-34 - second lat from pole
                                            ! at which cone cuts earth.
 kgds_mdl(19) = 0          ! oct 4  - # vert coordinate parameters
 kgds_mdl(20) = 255        ! oct 5  - not used, set to 255
   
 return

 end subroutine init_gds_lambconf

 subroutine init_gds_polar
!$$$ subroutine documentation block
!
! subprogram:  initialize grib 1 gds for polar stereographic
!
!   prgmmr: gayno          org: w/np2           date: ????
!
! abstract: initialize the grib 1 gds for polar stereographic
!   grids.
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
 use program_setup, only      : dx_mdl,            &
                                dy_mdl,            &
                                hemi_mdl,          &
                                orient_lon_mdl

 implicit none

 kgds_mdl = 0

 kgds_mdl( 1) = 5          ! oct 6 - type of grid, polar stereo
 kgds_mdl( 2) = imdl       ! octs 7-8 - i dimension of grid
 kgds_mdl( 3) = jmdl       ! octs 9-10 - j dimension of grid                   
 kgds_mdl( 4) = nint(lat_first_mdl*1000.) ! latitude of first grid point, octs 11-13
 kgds_mdl( 5) = nint(lon_first_mdl*1000.) ! longitude of first grid point, octs 14-16
 kgds_mdl( 6) = 8          ! oct 17 - resolution and component flag
 kgds_mdl( 7) = nint(orient_lon_mdl*1000.)  
                           ! octs 18-20 - # orientation longitude
 kgds_mdl( 8) = nint(abs(dx_mdl))  ! octs 21-23 - x-dir grid length meters
 kgds_mdl( 9) = nint(abs(dy_mdl))  ! octs 24-26 - y-dir grid length meters
 if (hemi_mdl > 0.0) then  ! nh
   kgds_mdl(10) = 0          ! oct 27 - projection center flag
 else ! sh
   kgds_mdl(10) = 0          ! oct 27 - projection center flag
 endif
 if (dx_mdl > 0.0) then      ! oct 28 - scanning mode flag
   kgds_mdl(11) = 0
 else
   kgds_mdl(11) = 128
 endif
 if (dy_mdl > 0.0) then
   kgds_mdl(11) = kgds_mdl(11) + 64
 endif
 kgds_mdl(12) = 0          ! octs 29-32 - reserved, set to zero
 kgds_mdl(19) = 0          ! oct 4  - # vert coordinate parameters
 kgds_mdl(20) = 255        ! oct 5  - not used, set to 255
   
 return

 end subroutine init_gds_polar

 end module init_grib1
