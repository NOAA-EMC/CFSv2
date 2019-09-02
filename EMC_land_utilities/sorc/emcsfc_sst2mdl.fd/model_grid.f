 module model_grid
!$$$  module documentation block
!
! $Revision$
!
! module:    model_grid
!   prgmmr: gayno         org: w/np2     date: 2005-05-20
!
! abstract: read in data defining the model grid.
!
! program history log:
!   2005-05-20  gayno   - initial version
!   2007-10-15  gayno   - modified for nam b-grids.
!                         improved thinning for gfs grids.
!   2014-10-25  gayno   - logic to ingest model lat/lon
!                         and landmask fields in grib 2
!                         format.
!
! usage: use model_grid
!
! remarks: some variable definitions
!   i/jpts_mdl    - i/j index of point on full grid
!   imdl          - i-dimension of model grid
!   jmdl          - j-dimension of model grid
!   ijmdl         - total number of model non-land points
!   kgds_mdl      - holds grib gds info of model grid
!   lats_mdl      - latitudes of model grid points
!   lat11/latlast - corner point latitudes
!   lons_mdl      - longitudes of model grid points
!   lon11/lonlast - corner point longitudes
!   lonsperlat    - for global grids, the number of i points
!                   in each row (decrease toward pole)
!   lsmask_mdl    - land mask of model grid (0 - non land, 1-land)
!                   for global grids run thinned, will contain
!                   a modified version of the original mask
!                   that has nonland at all points encompassed by a
!                   thinned point
!   lsmask_mdl_sav - saved copy of land mask of model grid (0 - non land, 1-land)
!                    only used for global thinned grids.
!   resol_mdl     - approximate model resolution in deg
!   thinned       - when true, global grids will run thinned
!                   (# i points decrease toward pole)
!
!$$$
 use program_setup, only         : model_lsmask_file, &
                                   model_lon_file, &
                                   model_lat_file

 use consts, only                : lonsperlat_t62,    &
                                   lonsperlat_t126,   &
                                   lonsperlat_t170,   &
                                   lonsperlat_t190,   &
                                   lonsperlat_t254,   &
                                   lonsperlat_t382
 
 private 

 integer, public                :: imdl
 integer, public                :: ijmdl
 integer, allocatable, public   :: ipts_mdl(:), jpts_mdl(:)
 integer, public                :: jmdl
 integer, public                :: kgds_mdl(200)
 integer, allocatable, public   :: lonsperlat (:) 

 logical, parameter, public     :: thinned = .true.
                                                    
 real, allocatable, public      :: lats_mdl    (:)
 real, allocatable, public      :: lons_mdl    (:)
 real, public                   :: lat11, latlast, lon11, lonlast
 real, allocatable, public      :: lsmask_mdl  (:,:)
 real, allocatable, public      :: lsmask_mdl_sav (:,:)
 real, public                   :: resol_mdl

 public read_mdl_grid_info

 contains

 subroutine read_mdl_grid_info
!$$$  subprogram documentation block
!
! subprogram:    read_mdl_grid_info
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: read latitude, longitude, land/sea mask on the
!   model grid.  data may be in grib1 or grib2 format.
!
! program history log:
! 2005-05-20  gayno    - initial version
! 2014-10-25  gayno    - logic to read these fields from
!                        grib2 files
!
! usage: call read_mdl_grid_info
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files:
!    input:  
!      - model landmask file (grib1 or grib2)
!      - model latitude file (grib1 or grib2)
!      - model longitude file (grib1 or grib2)
!
!    output: none
!
! condition codes:  all fatal
!    76 - unknown gfs grid
!    79 - unrecognized model grid projection
!    80 - bad open of model latitude file
!    81 - bad read of model latitude grib 1 header
!    82 - bad degrib of model latitude file (grib1 version)
!    83 - bad open of model longitude file
!    84 - bad degrib of model longitude file (grib1 version)
!    85 - bad open of model landmask file
!    86 - bad degrib of model landmask file (grib1 version)
!    91 - model latitude file not grib1 or grib2
!    92 - bad degrib of model latitude file (grib2 version)
!    93 - model longitude file not grib1 or grib2
!    94 - bad degrib of model longitude file (grib2 version)
!    95 - model landmask file not grib1 or grib2
!    96 - bad degrib of model landmask file (grib2 version)
!
! remarks: none.
!
!$$$

 use grib_mod   ! ncep grib 2 library

 implicit none

 character*150           :: fngrib

 integer                 :: i,j,ij,k
 integer                 :: ii, jj, iii, istart, iend, imid
 integer                 :: iret, isgrib
 integer, parameter      :: iunit = 14  ! unit of grib file
 integer                 :: jgds(200)
 integer                 :: jpds(200)
 integer                 :: jdisc, jpdtn, jgdtn
 integer                 :: jids(200), jpdt(200), jgdt(200)
 integer                 :: lskip
 integer, parameter      :: lugi = 0    ! unit of grib index file - not used
 integer                 :: kgds(200)
 integer                 :: kpds(200)
 integer                 :: message_num
 integer                 :: numbytes
 integer                 :: numpts

 logical*1, allocatable  :: lbms(:)
 logical                 :: unpack

 real                    :: gridis, gridie, fraction, x1, r
 real, allocatable       :: lats_mdl_temp(:,:)
 real, allocatable       :: lons_mdl_temp(:,:)

 type(gribfield)         :: gfld

 print*,"- READ MODEL GRID INFORMATION"

!-----------------------------------------------------------------------
! read latitudes on the model grid
!-----------------------------------------------------------------------

 print*,''
 print*,"- INGEST MODEL GRID LATITUDES."
 fngrib = model_lat_file

 call grib_check(fngrib, isgrib)

 if (isgrib==0) then
   print*,'- FATAL ERROR: MODEL LAT FILE MUST BE GRIB1 OR GRIB2 FORMAT.'
   call w3tage('SST2MDL')
   call errexit(91)
 end if

 print*,"- OPEN MODEL LATITUDE FILE: ", trim(fngrib)
 call baopenr (iunit, fngrib, iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR: BAD OPEN, IRET IS ', iret
   call w3tage('SST2MDL')
   call errexit(80)
 end if

 if (isgrib==1) then  ! file is grib 1

!-----------------------------------------------------------------------
! tell degribber to search for latitudes
!-----------------------------------------------------------------------

   lskip   = -1  ! read beginning of file
   jgds    = -1
   jpds    = -1
   jpds(5) = 176
   kgds    = -1   
   kpds    = -1  

   print*,"- GET GRIB HEADER"
   call getgbh(iunit, lugi, lskip, jpds, jgds, numbytes,  &
               numpts, message_num, kpds, kgds, iret)

   if (iret /= 0) then
     print*,'- FATAL ERROR: BAD READ OF GRIB HEADER. IRET IS ', iret
     call w3tage('SST2MDL')
     call errexit(81)
   end if

!-----------------------------------------------------------------------
! save gds info for use in gribbing the interpolated
! data later.
!-----------------------------------------------------------------------

   kgds_mdl = kgds

!-----------------------------------------------------------------------
! model resolution is one factor in determining the interpolation
! type.  calculate it in degrees.
!-----------------------------------------------------------------------

   if (kgds_mdl(1) == 4) then  ! gaussian grid
     resol_mdl = float(kgds_mdl(9)) / 1000.0
     imdl = kgds_mdl(2)   ! model grid dimensions
     jmdl = kgds_mdl(3)
   else if (kgds_mdl(1) == 203) then  ! e-grid
     resol_mdl = sqrt( (float(kgds_mdl(9)) / 1000.0)**2   +    &
                       (float(kgds_mdl(10)) / 1000.0)**2  )
     imdl = kgds_mdl(2)   ! model grid dimensions
     jmdl = kgds_mdl(3)
   else if (kgds_mdl(1) == 205) then  ! nam b-grid
     resol_mdl = ((float(kgds_mdl(9)) / 1000.0) + (float(kgds_mdl(10)) / 1000.0)) &
                   * 0.5
     imdl = kgds_mdl(2)   ! model grid dimensions
     jmdl = kgds_mdl(3)
   else
     print*,'- FATAL ERROR: UNRECOGNIZED MODEL GRID PROJECTION.'
     call w3tage('SST2MDL')
     call errexit(79)
   end if

!-----------------------------------------------------------------------
! save the grid id for use in gribbing the interpolated data later.
!-----------------------------------------------------------------------

   allocate(lats_mdl_temp(imdl,jmdl))
   allocate(lbms(imdl*jmdl))

   print*,"- DEGRIB DATA"
   call getgb(iunit, lugi, (imdl*jmdl), lskip, jpds, jgds, &
              numpts, message_num, kpds, kgds, lbms, lats_mdl_temp, iret)

   if (iret /= 0) then
     print*,'- FATAL ERROR: BAD DEGRIB OF FILE. IRET IS ',iret
     call w3tage('SST2MDL')
     call errexit(82)
   end if

   lat11   = lats_mdl_temp(1,1)
   latlast = lats_mdl_temp(imdl,jmdl)

   deallocate(lbms)

 elseif (isgrib==2) then

   j       = 0      ! search at beginning of file
   jdisc   = 0      ! search for discipline; 0 - meteorological products
   jpdtn   = -1     ! search for any product definition template number
   jgdtn   = -1     ! search for any grid definition template number
   jids    = -9999  ! array of values in identification section, set to wildcard
   jgdt    = -9999  ! array of values in grid definition template 3.m
   jpdt    = -9999  ! array of values in product definition template 4.n
   jpdt(1) = 191    ! search for parameter category - misc
   jpdt(2) = 1      ! search for parameter number - latitude
   unpack  = .true. ! unpack data

   call grib2_null(gfld)

   print*,"- DEGRIB DATA"
   call getgb2(iunit, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
               unpack, k, gfld, iret)

   if (iret /=0) then
    print*,'- FATAL ERROR: BAD DEGRIB OF FILE, IRET IS ', iret
    call w3tage('SST2MDL')
    call errexit(92)
   endif

!-----------------------------------------------------------------------
! create the grib 1 gds array from the g2 gdt array.  the grib 1
! gds info is used by ipolates and for gribbing the final snow analysis.
!-----------------------------------------------------------------------

   call gdt_to_gds(gfld%igdtnum, gfld%igdtmpl, gfld%igdtlen, kgds_mdl, &
                   imdl, jmdl, resol_mdl)

   allocate(lats_mdl_temp(imdl,jmdl))
   lats_mdl_temp = reshape (gfld%fld , (/imdl,jmdl/) )

   lat11   = lats_mdl_temp(1,1)
   latlast = lats_mdl_temp(imdl,jmdl)

   call grib2_free(gfld)

 end if  ! is grib1 or grib2

 call baclose(iunit,iret)

 print*,''
 print*,"- MODEL RESOLUTION IN DEGREES IS ", resol_mdl
 print*,"- MODEL RESOLUTION IN KM IS      ", resol_mdl*111.

!-----------------------------------------------------------------------
! read longitudes on the model grid.
!-----------------------------------------------------------------------

 print*,''
 print*,"- INGEST MODEL GRID LONGITUDES."
 fngrib = model_lon_file

 call grib_check(fngrib, isgrib)

 if (isgrib==0) then
   print*,'- FATAL ERROR: MODEL LONGITUDE FILE MUST BE GRIB1 OR GRIB2 FORMAT.'
   call w3tage('SST2MDL')
   call errexit(93)
 end if

 print*,"- OPEN MODEL LONGITUDE FILE: ", trim(fngrib)
 call baopenr (iunit, fngrib, iret)

 if (iret /= 0) then
   print*,"- FATAL ERROR: BAD OPEN. IRET IS ", iret
   call w3tage('SST2MDL')
   call errexit(83)
 end if

 if (isgrib==1) then  ! file is grib 1

   lskip   = -1  
   kgds    = -1   
   kpds    = -1  
   jgds    = -1
   jpds    = -1
   jpds(5) = 177

   allocate(lons_mdl_temp(imdl,jmdl))
   allocate(lbms(imdl*jmdl))

   print*,"- DEGRIB DATA"
   call getgb(iunit, lugi, (imdl*jmdl), lskip, jpds, jgds, &
              numpts, message_num, kpds, kgds, lbms, lons_mdl_temp, iret)

   if (iret /= 0) then
     print*,'- FATAL ERROR: BAD DEGRIB OF DATA. IRET IS ',iret
     call w3tage('SST2MDL')
     call errexit(84)
   end if

   deallocate(lbms)

   lon11   = lons_mdl_temp(1,1)
   lonlast = lons_mdl_temp(imdl,jmdl)

 elseif (isgrib ==2) then  ! grib 2 file

   j       = 0      ! search at beginning of file
   jdisc   = 0      ! search for discipline; 0 - meteorological products
   jpdtn   = -1     ! search for any product definition template number
   jgdtn   = -1     ! search for any grid definition template number
   jids    = -9999  ! array of values in identification section, set to wildcard
   jgdt    = -9999  ! array of values in grid definition template 3.m
   jpdt    = -9999  ! array of values in product definition template 4.n
   jpdt(1) = 191    ! search for parameter category - misc
   jpdt(2) = 2      ! search for parameter number - longitude
   unpack  = .true. ! unpack data

   call grib2_null(gfld)

   print*,"- DEGRIB DATA"
   call getgb2(iunit, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
               unpack, k, gfld, iret)

   if (iret /=0) then
    print*,'- FATAL ERROR: BAD DEGRIB OF FILE, IRET IS ', iret
    call w3tage('SST2MDL')
    call errexit(94)
   endif

   allocate(lons_mdl_temp(imdl,jmdl))
   lons_mdl_temp = reshape (gfld%fld , (/imdl,jmdl/) )

   lon11   = lons_mdl_temp(1,1)
   lonlast = lons_mdl_temp(imdl,jmdl)

   call grib2_free(gfld)

 end if ! is lon file grib1 or grib2?

 call baclose(iunit,iret)

!-----------------------------------------------------------------------
! read model land/sea mask, which is really a decimal %
! of land in a grid box.
!-----------------------------------------------------------------------

 print*,''
 print*,"- INGEST MODEL GRID LANDMASK."

 fngrib = model_lsmask_file

 call grib_check(fngrib, isgrib)

 if (isgrib==0) then
   print*,'- FATAL ERROR: MODEL MASK FILE MUST BE GRIB1 OR GRIB2 FORMAT'
   call w3tage('SST2MDL')
   call errexit(95)
 end if

 print*,"- OPEN MODEL LANDMASK FILE: ", trim(fngrib)
 call baopenr (iunit, fngrib, iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR: BAD OPEN OF FILE. IRET IS ', iret
   call w3tage('SST2MDL')
   call errexit(85)
 end if

 if (isgrib==1) then  ! file is grib 1

   lskip   = -1 
   kgds    = -1  
   kpds    = -1 
   jpds    = -1
   jgds    = -1
   jpds(5) = 81

   allocate(lsmask_mdl(imdl,jmdl))
   allocate(lbms(imdl*jmdl))

   print*,"- DEGRIB DATA"
   call getgb(iunit, lugi, (imdl*jmdl), lskip, jpds, jgds, &
              numpts, message_num, kpds, kgds, lbms, lsmask_mdl, iret)

   if (iret /= 0) then
     print*,'- FATAL ERROR: BAD DEGRIB OF DATA. IRET IS ',iret
     call w3tage('SST2MDL')
     call errexit(86)
   end if

   deallocate (lbms)

 elseif (isgrib==2) then  ! grib 2 file

   j       = 0      ! search at beginning of file
   jdisc   = 2      ! search for discipline; 2 - land-sfc products
   jpdtn   = -1     ! search for any product definition template number
   jgdtn   = -1     ! search for any grid definition template number
   jids    = -9999  ! array of values in identification section, set to wildcard
   jgdt    = -9999  ! array of values in grid definition template 3.m
   jpdt    = -9999  ! array of values in product definition template 4.n
   jpdt(1) = 0      ! search for parameter category - veg/biomass
   jpdt(2) = 0      ! search for parameter number - landcover
   unpack  = .true. ! unpack data

   call grib2_null(gfld)

   print*,"- DEGRIB DATA"
   call getgb2(iunit, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
               unpack, k, gfld, iret)

   if (iret /=0) then
    print*,'- FATAL ERROR: BAD DEGRIB OF FILE, IRET IS ', iret
    call w3tage('SST2MDL')
    call errexit(96)
   endif

   allocate(lsmask_mdl(imdl,jmdl))
   lsmask_mdl = reshape (gfld%fld , (/imdl,jmdl/) )

   call grib2_free(gfld)

 endif ! is mask file grib1 or grib2

 call baclose(iunit,iret)

!-----------------------------------------------------------------------
! global model runs on a thinned grid (# grid points decreases 
! towards the poles).  if thinned logical is set, and this is a 
! gaussian grid, modify the land/sea mask to account for the
! fact that delta x increased toward the poles. 
!-----------------------------------------------------------------------

 if (kgds(1) == 4 .and. thinned) then

   print*,"- RUNNING A THINNED GFS GRID"

   allocate (lonsperlat(jmdl/2))

   if (imdl == 192 .and. jmdl == 94) then      ! t62 grid
     lonsperlat = lonsperlat_t62
   elseif (imdl == 384 .and. jmdl == 190) then ! t126 grid
     lonsperlat = lonsperlat_t126
   elseif (imdl == 512 .and. jmdl == 256) then ! t170 grid
     lonsperlat = lonsperlat_t170
   elseif (imdl == 576 .and. jmdl == 288) then ! t190 grid
     lonsperlat = lonsperlat_t190
   elseif (imdl == 768 .and. jmdl == 384) then ! t254 grid
     lonsperlat = lonsperlat_t254
   elseif (imdl == 1152 .and. jmdl == 576) then ! t382 grid
     lonsperlat = lonsperlat_t382
   else
     print*,'- FATAL ERROR: UNKNOWN GFS GRID.'
     call w3tage('SST2MDL')
     call errexit(76)
   end if

   allocate (lsmask_mdl_sav(imdl,jmdl))
   lsmask_mdl_sav = lsmask_mdl
   lsmask_mdl = 1.0   ! this will identify non-land points to be processed by
                      ! the ipolates routines.

!-----------------------------------------------------------------------
! loop over every point on the thinned grid.  calculate the start/end
! bounds with respect to the full grid in the 'i' direction.  if
! the thinned point contains non-land, set all full grid points within
! the bounds to be non-land.  this modified mask will identify the
! points to be processed by ipolates.  after the call to ipolates,
! the thinned points will be set to a linear weighting of the full points
! located within the thinned point.
!-----------------------------------------------------------------------

   do j = 1, jmdl
     jj = j
     if (j > jmdl/2) jj = jmdl - j + 1
     r = float(imdl)/ float(lonsperlat(jj))
     do i = 1, lonsperlat(jj)
       x1=float(i-1)*r
       imid = nint(x1+1.0)  ! for this thinned grid point, this is
                            ! the nearest 'i' index on the full grid.
       if (lsmask_mdl_sav(imid,j) == 0.0) then
         gridis = x1+1.0-r/2.
         istart = nint(gridis)
         gridie = x1+1.0+r/2.
         iend   = nint(gridie)
         do ii = istart, iend
           if (ii == istart) then
             fraction = 0.5 - (gridis - float(istart))
             if (fraction < 0.0001) cycle
           endif
           if (ii == iend) then
             fraction = 0.5 + (gridie - float(iend))
             if (fraction < 0.0001) cycle
           endif
           iii = ii
           if (iii < 1) iii = imdl + iii
           lsmask_mdl(iii,j) = lsmask_mdl_sav(imid,j)
         enddo
       endif
     enddo
   enddo

 end if

!-----------------------------------------------------------------------
! only process model points that are non-land.
!-----------------------------------------------------------------------

 ij = 0

 do j = 1, jmdl
 do i = 1, imdl
   if(lsmask_mdl(i,j) == 0.0) then
     ij = ij + 1
   endif
 enddo
 enddo

 ijmdl = ij

 if (ijmdl == 0) then  ! grid has only land points, dont run
   print*,'- MODEL GRID ONLY HAS LAND POINTS, DONT CREATE SST FILE.'
   print*,'- NORMAL TERMINATION.'
   call w3tage('SST2MDL')
   call errexit(0)
 endif

 allocate(lats_mdl(ijmdl))
 allocate(lons_mdl(ijmdl))
 allocate(ipts_mdl(ijmdl))
 allocate(jpts_mdl(ijmdl))

 ij = 0
 do j = 1, jmdl
 do i = 1, imdl
   if(lsmask_mdl(i,j) == 0.0) then
     ij = ij + 1
     lats_mdl(ij) = lats_mdl_temp(i,j)
     lons_mdl(ij) = lons_mdl_temp(i,j)
     ipts_mdl(ij) = i
     jpts_mdl(ij) = j
   endif
 enddo
 enddo

 deallocate (lats_mdl_temp)
 deallocate (lons_mdl_temp)

 return

 end subroutine read_mdl_grid_info

 end module model_grid
