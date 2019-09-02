 module calc_latlons
!$$$ module documentation block
!
! $Revision$
!
! module:  calc_latlons
!   prgmmr: gayno          org: w/np2           date: ????
!
! abstract: contains routines to compute lat/lons for
!   several map projections.
!
! program history log:
! ????        gayno     - initial version
!
! usage: use calc_latlons
!
! remarks: none
!
!$$$
 implicit none

 real, allocatable            :: lat_mdl(:,:)        ! mass pts
 real, allocatable            :: lon_mdl(:,:)        ! mass pts
 real, allocatable            :: lat_vpnts_mdl(:,:)  ! velocity pts
 real, allocatable            :: lon_vpnts_mdl(:,:)  ! velocity pts

! the lat/lon of grid point (1,1) and (imdl,jmdl) on full grid.
! used for grib utilities.  

 real                         :: lat_first_mdl, lat_first_vpnt_mdl, lat_last_mdl
 real                         :: lon_first_mdl, lon_first_vpnt_mdl, lon_last_mdl
 real                         :: lon_last_vpnt_mdl, lat_last_vpnt_mdl

 contains

 subroutine calc_latlons_mdl
!$$$ subroutine documentation block
!
! subprogram:  calc_latlons_mdl
!   prgmmr: gayno          org: w/np2           date: ????
!
! abstract: driver routine to compute lat/lon on the model grid
!
! program history log:
! ????        gayno     - initial version
!
! usage:  call routine with no arguments
!
! files: none
!
! condition codes:
!   1 - unsupported map projection
!
! remarks: for staggered e-grids, the static surface fields
!   are only calculated at the mass points.  the velocity points
!   are only output for use in other programs.
!
!$$$
 use program_setup, only       : domain_type

 use mpimod,  only             : istart_mdl, iend_mdl, &
                                 jstart_mdl, jend_mdl

 implicit none

 include 'mpif.h'

 integer                      :: iret

 print*,'- CALCULATE LAT/LONS ON MODEL GRID'

 allocate (lat_mdl(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))
 allocate (lon_mdl(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))

 if (trim(domain_type) == 'gaussian') then
   
   call calc_latlons_gaussian

 elseif (trim(domain_type) == 'egrid') then
   
   allocate(lat_vpnts_mdl(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))  ! velocity points
   allocate(lon_vpnts_mdl(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))  ! velocity points

   call calc_latlons_egrid

 elseif (trim(domain_type) == 'bgrid' .or. trim(domain_type) == 'bgrid_global') then

   allocate(lat_vpnts_mdl(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))  ! velocity points
   allocate(lon_vpnts_mdl(istart_mdl:iend_mdl,jstart_mdl:jend_mdl))  ! velocity points

   call calc_latlons_bgrid

 elseif (trim(domain_type) == 'latlon') then

   call calc_latlons_latlon

 elseif (trim(domain_type) == 'lambconf') then

   call calc_latlons_lambconf

 elseif (trim(domain_type) == 'polar') then

   call calc_latlons_polar

 elseif (trim(domain_type) == 'mercator') then

   call calc_latlons_mercator

 else

   print*,'- FATAL ERROR:'
   print*,'- LAT/LON CALCS NOT SUPPORTED FOR MAP PROJECTION: ',trim(domain_type)
   call mpi_abort(mpi_comm_world, 1, iret)

 endif

 return

 end subroutine calc_latlons_mdl

 subroutine calc_latlons_mercator
!$$$ subroutine documentation block
!
! subprogram:  calc_latlons_mercator
!   prgmmr: gayno          org: w/np2           date: 2015-04-20
!
! abstract: compute lat/lon on a mercator grid
!
! program history log:
! 2015-04-20  gayno     - initial version
!
! usage:  call routine with no arguments
!
! files: none
!
! condition codes: none
!
! remarks: none
!
!$$$
 use program_setup, only       : lon_11_mdl, lon_22_mdl, &
                                 lat_11_mdl, imdl, jmdl, &
                                 dy_mdl, tangent_lat_mdl

 use mpimod, only              : istart_mdl, iend_mdl, &
                                 jstart_mdl, jend_mdl, &
                                 gather

 implicit none

 real, parameter          :: rerth=6.3712E6
 real, parameter          :: pi=3.14159265358979
 real, parameter          :: dpr=180./pi

 integer                  :: i, j

 real, allocatable        :: dummy(:,:)
 real                     :: dlon, dlat, ye, x, y

 dlon = 1.0*(mod(1.0*(lon_22_mdl-lon_11_mdl)-1+3600,360.)+1)/float(imdl-1)
 dlat = dy_mdl/(rerth*cos(tangent_lat_mdl/dpr))
 ye   = 1.-log(tan((lat_11_mdl+90.)/2./dpr))/dlat

 do j = jstart_mdl, jend_mdl
   do i = istart_mdl, iend_mdl
     x=float(i)
     y=float(j)
     lon_mdl(i,j)=mod(lon_11_mdl+dlon*(x-1)+3600,360.)
     lat_mdl(i,j)=2*atan(exp(dlat*(y-ye)))*dpr-90.0
   enddo
 enddo

 allocate(dummy(imdl,jmdl))
 call gather(lat_mdl, imdl, jmdl, dummy)
 lat_first_mdl=dummy(1,1)
 lat_last_mdl=dummy(imdl,jmdl)
 call gather(lon_mdl, imdl, jmdl, dummy)
 lon_first_mdl=dummy(1,1)
 lon_last_mdl=dummy(imdl,jmdl)
 deallocate(dummy)

 return

 end subroutine calc_latlons_mercator

 subroutine calc_latlons_polar
!$$$ subroutine documentation block
!
! subprogram:  calc_latlons_polar
!   prgmmr: gayno          org: w/np2           date: ????
!
! abstract: compute lat/lon on a polar stereographic grid
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
! remarks: taken from ipolates routine gdswiz05.
!
!$$$
 use program_setup, only       : lat_11_mdl,      &
                                 lon_11_mdl,      &
                                 imdl, jmdl,      &
                                 dx_mdl,          &
                                 dy_mdl,          &
                                 orient_lon_mdl,  &
                                 hemi_mdl

 use mpimod, only              : istart_mdl, iend_mdl, &
                                 jstart_mdl, jend_mdl, &
                                 gather, myrank

 implicit none

 include 'mpif.h'

 real, parameter :: slat=60.0  ! standard latitude according
                               ! to grib standard

 real, parameter :: rerth=6.3712E6
 real, parameter :: pi=3.14159265358979
 real, parameter :: dpr=180./PI

 integer            :: i, j, ierr

 real               :: di, dj, dr2, x, y
 real               :: de, de2, dr, xp, yp
 real, allocatable  :: dummy(:,:)

 de=(1.+sin(slat/dpr))*rerth
 dr=de*cos(lat_11_mdl/dpr)/(1+hemi_mdl*sin(lat_11_mdl/dpr))
 xp=1-hemi_mdl*sin((lon_11_mdl-orient_lon_mdl)/dpr)*dr/dx_mdl
 yp=1+cos((lon_11_mdl-orient_lon_mdl)/dpr)*dr/dy_mdl
 de2=de**2

 do j = jstart_mdl, jend_mdl
   y = float(j)
   do i = istart_mdl, iend_mdl
     x = float(i)
     di=(x-xp)*dx_mdl
     dj=(y-yp)*dy_mdl
     dr2=di**2+dj**2
     if(dr2.LT.de2*1.E-6) then
       lon_mdl(i,j)=0.
       lat_mdl(i,j)=hemi_mdl*90.
     else
       lon_mdl(i,j)=mod(orient_lon_mdl+hemi_mdl*dpr*atan2(di,-dj)+3600,360.)
       if (lon_mdl(i,j) > 180.0) lon_mdl(i,j) = lon_mdl(i,j) - 360.0
       lat_mdl(i,j)=hemi_mdl*dpr*asin((de2-dr2)/(de2+dr2))
     endif
   enddo
 enddo

! save lat/lon at grid point (1,1).  it is need for gribbing.

 allocate(dummy(imdl,jmdl))
 call gather(lat_mdl, imdl, jmdl, dummy)
 lat_first_mdl=dummy(1,1)
 call gather(lon_mdl, imdl, jmdl, dummy)
 lon_first_mdl=dummy(1,1)
 deallocate(dummy)

 return

 end subroutine calc_latlons_polar

 subroutine calc_latlons_lambconf
!$$$ subroutine documentation block
!
! subprogram:  calc_latlons_lambconf
!   prgmmr: gayno          org: w/np2           date: ????
!
! abstract: compute lat/lon on a lambert conformal grid
!
! program history log:
! ????        gayno     - initial version
!
! usage:  call routine with no arguments
!
! files: none
!
! condition codes: 
!   1 - error in computation of lat/lon
!
! remarks: none
!
!$$$
 use program_setup, only       : lat_11_mdl,      &
                                 lon_11_mdl,      &
                                 dx_mdl,          &
                                 orient_lon_mdl,  &
                                 tangent_lat_mdl, &
                                 imdl,            &
                                 jmdl

 use mpimod,  only             : gather, istart_mdl, iend_mdl, &
                                 jstart_mdl, jend_mdl, myrank

 implicit none

 include 'mpif.h'

 integer                      :: i, j
 integer                      :: iret

 real                         :: dx_in_meters
 real, allocatable            :: dummy(:,:)

 dx_in_meters = dx_mdl * 1000.0

 do j = jstart_mdl, jend_mdl
   do i = istart_mdl, iend_mdl

     call w3fb12(float(i),float(j),lat_11_mdl,lon_11_mdl,  &
                 dx_in_meters, orient_lon_mdl, tangent_lat_mdl,  &
                 lat_mdl(i,j),lon_mdl(i,j), iret)

     if (iret /= 0) then
       print*,"- FATAL ERROR: BAD POINT IN LAMBERT CONFORMAL GRID CALCULATION: ",i,j
       call mpi_abort(mpi_comm_world, 1, iret)
     end if

     if (lon_mdl(i,j) > 180.0) lon_mdl(i,j) = lon_mdl(i,j) - 360.0

   enddo
 enddo

! save lat/lon at grid point (1,1).  it is need for gribbing.

 allocate(dummy(imdl,jmdl))
 call gather(lat_mdl, imdl, jmdl, dummy)
 lat_first_mdl=dummy(1,1)
 call gather(lon_mdl, imdl, jmdl, dummy)
 lon_first_mdl=dummy(1,1)
 deallocate(dummy)

 end subroutine calc_latlons_lambconf

 subroutine calc_latlons_gaussian
!$$$ subroutine documentation block
!
! subprogram:  calc_latlons_gaussian
!   prgmmr: gayno          org: w/np2           date: ????
!
! abstract: compute lat/lon on a gaussian grid
!
! program history log:
! ????        gayno     - initial version
!
! usage:  call routine with no arguments
!
! files: none
!
! condition codes:  none
!
! remarks: none
!
!$$$
 use program_setup, only       : imdl, jmdl, dx_gfs,  &
                                 thinned, lonsperlat_mdl

 use mpimod, only              : istart_mdl, iend_mdl, &
                                 jstart_mdl, jend_mdl

 implicit none

 include 'mpif.h'

 integer                      :: i
 integer                      :: j, jj

 real                         :: deltalon
 real                         :: lon
 real, allocatable            :: slat(:), wlat(:)

 lon_mdl = -999.
 lat_mdl = -999.

 if (thinned) then
   do j = jstart_mdl, jend_mdl
     deltalon = dx_gfs(j)
     jj = j
     if (j > jmdl/2) jj = jmdl - j + 1
     do i = istart_mdl, iend_mdl
       if (i > lonsperlat_mdl(jj)) cycle
       lon = float(i-1)*deltalon
       if (lon > 180.0) lon = lon - 360.0
       lon_mdl(i,j) = lon
     enddo 
   enddo
 else
   deltalon = 360.0 / float(imdl)
   do i = istart_mdl, iend_mdl
     lon = float(i-1)*deltalon
     if (lon > 180.0) lon = lon - 360.0
     lon_mdl(i,:) = lon
   enddo
 endif

 allocate(slat(jmdl))
 allocate(wlat(jmdl))

 call splat(4, jmdl, slat, wlat)

 do j = jstart_mdl, jend_mdl
   lat_mdl(:,j) = 90.0 - (acos(slat(j))* 180.0 / (4.*atan(1.)))
 enddo

 lat_first_mdl = 90.0 - (acos(slat(1))* 180.0 / (4.*atan(1.)))
 lat_last_mdl  = 90.0 - (acos(slat(jmdl))* 180.0 / (4.*atan(1.)))

 lon_first_mdl = 0.0
 deltalon = 360.0 / float(imdl)
 lon_last_mdl  = (imdl-1)*deltalon
 if (lon_last_mdl > 180.0) lon_last_mdl = lon_last_mdl - 360.0

 deallocate (slat)
 deallocate (wlat)

 return

 end subroutine calc_latlons_gaussian

 subroutine calc_latlons_latlon
!$$$ subroutine documentation block
!
! subprogram:  calc_latlons_latlon
!   prgmmr: gayno          org: w/np2           date: ????
!
! abstract: compute lat/lon on a regular lat/lon grid
!
! program history log:
! ????        gayno     - initial version
!
! usage:  call routine with no arguments
!
! files: none
!
! condition codes:  none
!
! remarks: indices increase from north to south and from
!   greenwich.
!
!$$$
 use program_setup, only     : dx_mdl,     &
                               dy_mdl,     &
                               imdl,       &
                               jmdl,       &
                               lat_11_mdl, &
                               lon_11_mdl

 use mpimod, only            : istart_mdl, iend_mdl, &
                               jstart_mdl, jend_mdl

 implicit none

 integer                    :: i, j

 do j = jstart_mdl, jend_mdl
   do i = istart_mdl, iend_mdl
     lat_mdl(i,j) = lat_11_mdl + (float(j-1) * dy_mdl)
     lon_mdl(i,j) = lon_11_mdl + (float(i-1) * dx_mdl)     
     if (lon_mdl(i,j) > 180.0) lon_mdl(i,j) = lon_mdl(i,j) - 360.0
   enddo
 enddo

 lat_first_mdl = lat_11_mdl 
 lon_first_mdl = lon_11_mdl      
 if (lon_first_mdl > 180.0) lon_first_mdl = lon_first_mdl - 360.0

 lat_last_mdl = lat_11_mdl + (float(jmdl-1) * dy_mdl) 
 lon_last_mdl = lon_11_mdl + (float(imdl-1) * dx_mdl)      
 if (lon_last_mdl > 180.0) lon_last_mdl = lon_last_mdl - 360.0

 return

 end subroutine calc_latlons_latlon

 subroutine calc_latlons_bgrid
!$$$ subroutine documentation block
!
! subprogram:  calc_latlons_bgrid
!   prgmmr: gayno          org: w/np2           date: ????
!
! abstract: compute lat/lon on a rotated lat/lon grid with
!   "B" staggering
!
! program history log:
! ????        gayno     - initial version
!
! usage:  call routine with no arguments
!
! files: none
!
! condition codes:  none
!
! remarks: none
!
!$$$
 use program_setup, only       : imdl, imdl_parent, jmdl, jmdl_parent, &
                                 tph0d => centlat_mdl,  &
                                 tlm0d => centlon_mdl,  &
                                 centlat_parent_mdl, centlon_parent_mdl, &
                                 dlmd => dx_mdl, &
                                 dphd => dy_mdl, &
                                 dx_parent_mdl, dy_parent_mdl, num_gen

 use mpimod, only              : myrank, &
                                 gather, &
                                 istart_mdl, iend_mdl, &
                                 jstart_mdl, jend_mdl

 implicit none

 include 'mpif.h'

 real*8, parameter   :: one = 1.0
 real*8, parameter   :: two = 2.0
 real*8, parameter   :: r180 = 180.0

 integer ::  i, j, ierr
 integer :: nest_i_start, nest_j_start

 integer, allocatable :: im(:), jm(:)

 real*8  :: pi, dtr, tlmh, tph0, dph, dlm
 real*8  :: tphh, ctph, ctph0, stph, stph0, sphh
 real*8  :: rtd, clmh, facth
 
 real,allocatable :: dummy(:,:), dx(:), dy(:), clat(:), clon(:), wbd(:), sbd(:)
 real    :: xcenter,ycenter,wbd_guess,sbd_guess,wbd_parent,sbd_parent

 pi = acos(-one)
 dtr = pi / r180
 rtd = r180 / pi

 allocate(im(num_gen))
 allocate(jm(num_gen))
 allocate(dx(num_gen))
 allocate(dy(num_gen))
 allocate(clat(num_gen))
 allocate(clon(num_gen))
 allocate(wbd(num_gen))
 allocate(sbd(num_gen))

 im(num_gen)=imdl
 jm(num_gen)=jmdl
 dx(num_gen)=dlmd
 dy(num_gen)=dphd
 clat(num_gen)=tph0d
 clon(num_gen)=tlm0d

 do i = 1, (num_gen-1)
   im(i)=imdl_parent(i)
   jm(i)=jmdl_parent(i)
   dx(i)=dx_parent_mdl(i)
   dy(i)=dy_parent_mdl(i)
   clat(i)=centlat_parent_mdl(i)
   clon(i)=centlon_parent_mdl(i)
 enddo

 wbd(1) = - (im(1)-1)*0.5*dx(1)
 sbd(1) = - (jm(1)-1)*0.5*dy(1)

! use dusan's logic to set the west/south boundary.  for nests, need to ensure
! this boundary coincides with a point on its parent grid.  

 do i = 2, num_gen

   call tll(clon(i), clat(i), xcenter, ycenter, clat(1), clon(1))

! x/y center is the lat/lon of the grid center in rotated space.

   wbd_guess = xcenter - (im(i)-1)*0.5*dx(i)
   sbd_guess = ycenter - (jm(i)-1)*0.5*dy(i)

! based on the 'guess', find the value that most closely corresponds to a
! point on its parent grid.

   nest_i_start = nint ( 1. + (wbd_guess - wbd(i-1) ) / dx(i-1) )
   nest_j_start = nint ( 1. + (sbd_guess - sbd(i-1) ) / dy(i-1) )

   wbd(i) = wbd(i-1) + (nest_i_start-1)*dx(i-1)
   sbd(i) = sbd(i-1) + (nest_j_start-1)*dy(i-1)        

 enddo

 wbd = wbd * dtr
 sbd = sbd * dtr

 dph = dy(num_gen)*dtr
 dlm = dx(num_gen)*dtr

 tph0 = clat(1) * dtr
 ctph0 = cos(tph0)
 stph0 = sin(tph0)
 do j = jstart_mdl, jend_mdl
   tphh = sbd(num_gen) + float(j-1) * dph
   ctph = cos(tphh)
   stph = sin(tphh)
 do i = istart_mdl, iend_mdl
   tlmh = wbd(num_gen) + float(i-1) * dlm
   sphh = ctph0 * stph + stph0 * ctph * cos(tlmh)
   if (sphh > one) sphh = one
   lat_mdl(i,j) = asin(sphh)
   clmh = ctph * cos(tlmh) / (cos(lat_mdl(i,j))* ctph0) &
          - tan(lat_mdl(i,j)) * tan(tph0)
   if (clmh > one ) clmh = one
   if (clmh < -one) clmh = -one
   facth = one
   if (tlmh < 0.0) facth = -one
   lon_mdl(i,j) =  clon(1) + (facth * acos(clmh))/dtr
   if (lon_mdl(i,j) < -180.0) lon_mdl(i,j) = lon_mdl(i,j) + 360.0
   lat_mdl(i,j) = lat_mdl(i,j) * rtd
 enddo
 enddo

! save lat/lon at grid corners.  it is needed for grib header.

 allocate(dummy(imdl,jmdl))
 call gather(lat_mdl, imdl, jmdl, dummy)
 lat_first_mdl=dummy(1,1)
 lat_last_mdl=dummy(imdl,jmdl)
 call gather(lon_mdl, imdl, jmdl, dummy)
 lon_first_mdl=dummy(1,1)
 lon_last_mdl=dummy(imdl,jmdl)

! for v points
 wbd = wbd + 0.5*dlm
 sbd = sbd + 0.5*dph

 do j = jstart_mdl, jend_mdl
   tphh = sbd(num_gen) + float(j-1) * dph
   ctph = cos(tphh)
   stph = sin(tphh)
 do i = istart_mdl, iend_mdl
   tlmh = wbd(num_gen) + float(i-1) * dlm
   sphh = ctph0 * stph + stph0 * ctph * cos(tlmh)
   if (sphh > one) sphh = one
   lat_vpnts_mdl(i,j) = asin(sphh)
   clmh = ctph * cos(tlmh) / (cos(lat_vpnts_mdl(i,j))* ctph0) &
          - tan(lat_vpnts_mdl(i,j)) * tan(tph0)
   if (clmh > one ) clmh = one
   if (clmh < -one) clmh = -one
   facth = one
   if (tlmh < 0.0) facth = -one
   lon_vpnts_mdl(i,j) =  clon(1) + (facth * acos(clmh))/dtr
   if (lon_vpnts_mdl(i,j) < -180.0) lon_vpnts_mdl(i,j) = lon_vpnts_mdl(i,j) + 360.0
   lat_vpnts_mdl(i,j) = lat_vpnts_mdl(i,j) * rtd
 enddo
 enddo

 call gather(lat_vpnts_mdl, imdl, jmdl, dummy)
 lat_first_vpnt_mdl=dummy(1,1)
 lat_last_vpnt_mdl=dummy(imdl,jmdl)
 call gather(lon_vpnts_mdl, imdl, jmdl, dummy)
 lon_first_vpnt_mdl=dummy(1,1)
 lon_last_vpnt_mdl=dummy(imdl,jmdl)
 deallocate(dummy)

 deallocate(clat,clon,dx,dy,im,jm,wbd,sbd)

 return

 end subroutine calc_latlons_bgrid

 subroutine calc_latlons_egrid
!$$$ subroutine documentation block
!
! subprogram:  calc_latlons_egrid
!   prgmmr: gayno          org: w/np2           date: ????
!
! abstract: compute lat/lon on a rotated lat/lon grid with
!   "E" staggering
!
! program history log:
! ????        gayno     - initial version
!
! usage:  call routine with no arguments
!
! files: none
!
! condition codes:  none
!
! remarks: based on eta routine etall.
!
!$$$
 use program_setup, only       : im => imdl,   &
                                 jm => jmdl,   &
                                 tph0d_in => centlat_mdl,  &
                                 tlm0d_in => centlon_mdl,  &
                                 dlmd_in => dx_mdl, &
                                 dphd_in => dy_mdl

 use mpimod, only              : gather, myrank, &
                                 istart_mdl, iend_mdl, &
                                 jstart_mdl, jend_mdl

 include 'mpif.h'

 integer :: i,j, ierr

 REAL :: ONE=1.,R180=180.,TWO=2.
 REAL :: CLMH,CLMV,CTPH,CTPH0,CTPV,D2R,DLM,DLMD,DPH,DPHD     &
         ,FACTR,PI,R2D,SB,SBD,SPHH,SPHV,STPH,STPH0,STPV   &
         ,TDLM,TLMH,TLMV,TPH0,TPHH,TPHV,WB,WBD
 REAL :: GLATH, GLATV, GLONH, GLONV

 real    :: dtr, tdph, facth, factv, tlm0d, tph0d
 real    :: tlmh2, tlmv2

 real, allocatable :: dummy(:,:)

!-----------------------------------------------------------------------
!--------------DERIVED GEOMETRICAL CONSTANTS----------------------------
!----------------------------------------------------------------------

      TPH0D=TPH0D_in
      TLM0D=TLM0D_in
      DPHD=DPHD_in
      DLMD=DLMD_in

      WBD=-(IM-ONE)*DLMD
      SBD=-(JM-1)/2*DPHD
      PI=ACOS(-ONE)
      DTR = PI / R180
      TPH0 = TPH0D * DTR
      WB = WBD * DTR
      SB = SBD * DTR
      DLM = DLMD * DTR
      DPH = DPHD * DTR

      write(6,*) 'TPH0,WB,SB,DLM,DPH,DTR: ', TPH0,WB,SB,DLM,DPH,DTR

      TDLM = DLM + DLM
      TDPH = DPH + DPH
!
      STPH0 = SIN(TPH0)
      CTPH0 = COS(TPH0)

!-----------------------------------------------------------------------
!---COMPUTE GEOGRAPHIC LAT AND LONG OF ETA GRID POINTS (H & V POINTS)---
!-----------------------------------------------------------------------
      DO 200 J = jstart_mdl, jend_mdl
         TLMH = WB - TDLM + MOD(J+1,2) * DLM
         TPHH = SB+(J-1)*DPH
         TLMV = WB - TDLM + MOD(J,2) * DLM
         TPHV = TPHH
         STPH = SIN(TPHH)
         CTPH = COS(TPHH)
         STPV = SIN(TPHV)
         CTPV = COS(TPHV)
!----------------------------------------------------------------------
!---------- COMPUTE EARTH LATITUDE/LONGITUDE OF H POINTS --------------
!----------------------------------------------------------------------
         DO 201 I = istart_mdl, iend_mdl
           TLMH2 = TLMH + TDLM*float(i)
           SPHH = CTPH0 * STPH + STPH0 * CTPH * COS(TLMH2)
!cggg got problems near pole.
           if (sphh .gt. 1.) sphh = 1.
           GLATH = ASIN(SPHH)
           CLMH = CTPH * COS(TLMH2) / (COS(GLATH) * CTPH0)    &
                     - TAN(GLATH) * TAN(TPH0)
           IF(CLMH .GT. ONE) CLMH = ONE
           IF(CLMH .LT. -ONE) CLMH = -ONE
           FACTH = ONE
           IF(TLMH2 .GT. 0.) FACTH = -ONE
           GLONH = -TLM0D * DTR + FACTH * ACOS(CLMH)
           lat_mdl(I,J) = GLATH / DTR
           lon_mdl(I,J)= -GLONH / DTR
           IF(lon_mdl(I,J) .GT. 180.) lon_mdl(I,J) = lon_mdl(I,J) - 360.
           IF(lon_mdl(I,J) .LT. -180.) lon_mdl(I,J) = lon_mdl(I,J) + 360.
  201    CONTINUE
!----------------------------------------------------------------------
!---------- COMPUTE EARTH LATITUDE/LONGITUDE OF V POINTS --------------
!----------------------------------------------------------------------
         DO 202 I = istart_mdl, iend_mdl
           TLMV2 = TLMV + TDLM*float(i)
           SPHV = CTPH0 * STPV + STPH0 * CTPV * COS(TLMV2)
!cggg got problems near pole.
           if (sphv .gt. 1.) sphv = 1.
           GLATV = ASIN(SPHV)
           CLMV = CTPV * COS(TLMV2) / (COS(GLATV) * CTPH0)   &
                - TAN(GLATV) * TAN(TPH0)
           IF(CLMV .GT. 1.) CLMV = 1.
           IF(CLMV .LT. -1.) CLMV = -1.
           FACTV = 1.
           IF(TLMV2 .GT. 0.) FACTV = -1.
           GLONV = -TLM0D * DTR + FACTV * ACOS(CLMV)
!
!    CONVERT INTO DEGREES AND EAST LONGITUDE
!
           lat_vpnts_mdl(I,J) = GLATV / DTR
           lon_vpnts_mdl(I,J) = -GLONV / DTR
           IF(lon_vpnts_mdl(I,J) .GT. 180.) lon_vpnts_mdl(I,J) = lon_vpnts_mdl(I,J) - 360.
           IF(lon_vpnts_mdl(I,J) .LT. -180.) lon_vpnts_mdl(I,J) = lon_vpnts_mdl(I,J) + 360.

        if (mod(I,100) .eq. 0 .and. mod(J,100) .eq. 0) then
          write(6,*) 'I,J,HLAT,HLON,VLAT,VLON: ', I,J,lat_mdl(I,J),lon_mdl(I,J) &
                                                 ,lat_vpnts_mdl(I,J),lon_vpnts_mdl(I,J)
        endif
  202    CONTINUE
  200 CONTINUE

! Save lat/lon at corner points, which is needed for grib 2.  the last grid point is:
!
!   lat_last = lat_first + dy * jm 
!   lon_last = lon_first + dx * im 
!
! Note that our convention has even rows offset by dx/2.
! So the last grid point for a grid with an even number of rows
! will be the last 'v' point, not the last 'h' point.

 allocate(dummy(im,jm))
 if (mod(jm,2) == 0) then  
   call gather(lat_mdl, im, jm, dummy)
   lat_first_mdl=dummy(1,1)
   lat_last_mdl=dummy(im,jm)
   call gather(lon_mdl, im, jm, dummy)
   lon_first_mdl=dummy(1,1)
   call gather(lon_vpnts_mdl, im, jm, dummy)
   lon_last_mdl=dummy(im,jm)
 else
   call gather(lat_mdl, im, jm, dummy)
   lat_first_mdl=dummy(1,1)
   lat_last_mdl=dummy(im,jm)
   call gather(lon_mdl, im, jm, dummy)
   lon_first_mdl=dummy(1,1)
   lon_last_mdl=dummy(im,jm)
 end if
 deallocate(dummy)

 RETURN

 END subroutine calc_latlons_egrid

 subroutine tll(almd,aphd,tlmd,tphd,tph0d,tlm0d)
!-------------------------------------------------------------------------------
 real, intent(in) :: almd, aphd
 real, intent(out) :: tlmd, tphd
 real, intent(in) :: tph0d, tlm0d
!-------------------------------------------------------------------------------
 real, parameter :: pi=3.141592654
 real, parameter :: dtr=pi/180.0
!
 real :: tph0, ctph0, stph0, relm, srlm, crlm
 real :: aph, sph, cph, cc, anum, denom
!-------------------------------------------------------------------------------
!
 if (tlm0d==0.0.and.tph0d==0.0) then
      tlmd=almd
      tphd=aphd
 else
      tph0=tph0d*dtr
      ctph0=cos(tph0)
      stph0=sin(tph0)
      relm=(almd-tlm0d)*dtr
      srlm=sin(relm)
      crlm=cos(relm)
      aph=aphd*dtr
      sph=sin(aph)
      cph=cos(aph)
      cc=cph*crlm
      anum=cph*srlm
      denom=ctph0*cc+stph0*sph
      tlmd=atan2(anum,denom)/dtr
      tphd=asin(ctph0*sph-stph0*cc)/dtr
 end if
 return
 end subroutine tll

 subroutine calc_latlons_cleanup
!$$$ subroutine documentation block
!
! subprogram:  calc_latlons_cleanup
!   prgmmr: gayno          org: w/np2           date: ????
!
! abstract: free up memory
!
! program history log:
! ????        gayno     - initial version
!
! usage:  call routine with no arguments
!
! files: none
!
! condition codes:  none
!
! remarks: none
!
!$$$

 if (allocated(lat_mdl))       deallocate (lat_mdl)
 if (allocated(lon_mdl))       deallocate (lon_mdl)
 if (allocated(lat_vpnts_mdl)) deallocate (lat_vpnts_mdl)
 if (allocated(lon_vpnts_mdl)) deallocate (lon_vpnts_mdl)
 
 end subroutine calc_latlons_cleanup

 end module calc_latlons
