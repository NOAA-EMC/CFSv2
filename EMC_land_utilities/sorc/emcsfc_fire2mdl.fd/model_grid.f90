 module model_grid
!$$$  module documentation block
!                .      .    .                                       .
! module:    model_grid
!   prgmmr: gayno         org: w/np2     date: 2015-jan-30
!
! $Revision$
!
! abstract: this module contains routines required to
!           define the model grid.
!
! program history log:
!   2015-jan-30  gayno   - initial version
!
! usage: "use model_grid".  Then call public routine
!        "read_mdl_grid_info" to read the model grid pt latitudes,
!        longitudes, and landmask.  Call public routine
!        "model_grid_cleanup" to deallocate this module's 
!        public allocatable arrays.
!
! remarks: some variable definitions
!    grid_id_mdl          - grib 1 model grid id
!    imdl/jmdl            - i/j dimension of model grid.
!    ijmdl                - number of model land points
!    ipts/jpts_mdl        - the i/j indices of the model
!                           land points.
!    kgds_mdl             - array of grib 1 gds info, used
!                           by ncep ipolates library.
!    lat/lon11            - lat/lon of first model grid pt 
!    lat/lonlast          - lat/lon of last model grid pt 
!    lats/lons_mdl        - model latitudes and longitudes,
!                           land points only
!    lsmask_mdl           - model landmask
!    resol_mdl            - model resolution in km.
!
!$$$
!
 use program_setup, only         : model_lsmask_file, &
                                   model_lon_file, &
                                   model_lat_file

 implicit none

 private 

 integer, public                :: grid_id_mdl
 integer, public                :: imdl
 integer, public                :: jmdl
 integer, public                :: ijmdl ! only land points
 integer, public                :: kgds_mdl(200)
 integer, allocatable, public   :: ipts_mdl(:), jpts_mdl(:) 

 real, allocatable, public      :: lats_mdl    (:)
 real, allocatable, public      :: lons_mdl    (:)
 real, public                   :: lat11, lon11, latlast, lonlast
 real, allocatable, public      :: lsmask_mdl  (:,:)
 real, public                   :: resol_mdl  ! in km

 public                         :: model_grid_cleanup
 public                         :: read_mdl_grid_info

 contains

 subroutine read_mdl_grid_info
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_mdl_grid_info
!   prgmmr: gayno          org: w/np2     date: 2015-jan-30
!
! abstract: this subroutine reads the model grid latitudes, 
!   longitudes and landmask from grib1 or grib2 files.
!   it then filters out the non-land points.
!
! program history log:
! 2015-jan-30  gayno    - initial version
!
! usage: call read_mdl_grid_info
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files:
!   input:
!     - model grid latitudes (grib 1 or grib 2)
!     - model grid longitudes (grib 1 or grib 2)
!     - model grid landmask (grib 1 or grib 2)
!
!   output: none
!
! condition codes: all fatal
!   79 - unrecognized model grid type
!   80 - bad open model latitude file
!   81 - bad read model latitude grib 1 header
!   82 - bad read model latitude file
!   83 - bad open model longitude file
!   84 - bad read model longitude file
!   85 - bad open model landmask file
!   86 - bad read model landmask file
!   91 - model latitude file not grib 1 or grib 2
!   92 - model longitude file not grib 1 or grib 2
!   93 - model landmask file not grib 1 or grib 2
!  
! remarks: none.
!
!$$$
!
 use grib_mod

 implicit none

 character*200           :: fngrib

 integer                 :: i, j, ij, jj, k
 integer                 :: jdisc, jpdtn, jgdtn
 integer                 :: jids(200), jgdt(200), jpdt(200)
 integer                 :: iret, isgrib
 integer, parameter      :: iunit = 14  ! unit of grib file
 integer                 :: jgds(200)
 integer                 :: jpds(200)
 integer                 :: lskip
 integer, parameter      :: lugi = 0    ! unit of grib index file - not used
 integer                 :: kgds(200)
 integer                 :: kpds(200)
 integer                 :: message_num
 integer                 :: numbytes
 integer                 :: numpts

 logical*1, allocatable  :: lbms(:)
 logical                 :: unpack

 real, allocatable       :: lats_mdl_temp  (:,:)
 real, allocatable       :: lons_mdl_temp  (:,:)
 
 type(gribfield)         :: gfld

 print*,"- READ MODEL GRID INFORMATION"

!-----------------------------------------------------------------------
! read latitudes on the model grid
!-----------------------------------------------------------------------

 fngrib = model_lat_file

 print*,"- INGEST MODEL GRID LATITUDES."

 call grib_check(fngrib, isgrib)

 if (isgrib==0) then
   print*,'- FATAL ERROR, MODEL LATITUDE FILE MUST BE GRIB1 OR GRIB2 FORMAT'
   call w3tage('FIRE2MDL')
   call errexit(91)
 end if

 print*,"- OPEN MODEL LATITUDE FILE ", trim(fngrib)
 call baopenr (iunit, fngrib, iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR. BAD FILE OPEN, IRET IS ', iret
   call w3tage('FIRE2MDL')
   call errexit(80)
 end if

 if (isgrib == 1) then

!-----------------------------------------------------------------------
! tell degribber to search for latitudes
!-----------------------------------------------------------------------

   lskip   = -1  ! read beginning of file
   jgds    = -1
   jpds    = -1
   jpds(5) = 176 ! latitude
   kgds    = -1   
   kpds    = -1  

   print*,"- GET GRIB HEADER"
   call getgbh(iunit, lugi, lskip, jpds, jgds, numbytes,  &
               numpts, message_num, kpds, kgds, iret)

   if (iret /= 0) then
     print*,'- FATAL ERROR. BAD READ OF GRIB HEADER. IRET IS ', iret
     call w3tage('FIRE2MDL')
     call errexit(81)
   end if

!-----------------------------------------------------------------------
! model resolution (km) is used to determine the type of interpolation.
!-----------------------------------------------------------------------

   if (kgds(1) == 203) then  ! e-grid 
     resol_mdl = sqrt( (float(kgds(9)) / 1000.0)**2   +    &
                     (float(kgds(10)) / 1000.0)**2  )
     resol_mdl = resol_mdl * 111.0
   else if (kgds(1) == 205) then  ! b-grid 
     resol_mdl = ((float(kgds(9)) / 1000.0) + (float(kgds(10)) / 1000.0)) &
                  * 0.5 * 111.0
   else
     print*,'- FATAL ERROR.  UNRECOGNIZED MODEL GRID'
     call w3tage('FIRE2MDL')
     call errexit(79)
   end if

!-----------------------------------------------------------------------
! get model specs from header.
!-----------------------------------------------------------------------

   imdl = kgds(2)  ! i-dimension of model grid
   jmdl = kgds(3)  ! j-dimension of model grid

   grid_id_mdl = kpds(3) ! grib grid id number. sect 1, oct 7

!-----------------------------------------------------------------------
! save gds for gribbing the interpolated data later.
!-----------------------------------------------------------------------

   kgds_mdl = kgds

!-----------------------------------------------------------------------
! degrib data.
!-----------------------------------------------------------------------

   allocate(lats_mdl_temp(imdl,jmdl))
   allocate(lbms(imdl*jmdl))

   print*,"- DEGRIB DATA"
   call getgb(iunit, lugi, (imdl*jmdl), lskip, jpds, jgds, &
              numpts, message_num, kpds, kgds, lbms, lats_mdl_temp, iret)

   deallocate (lbms)

   if (iret /= 0) then
     print*,'- FATAL ERROR. BAD DEGRIB OF FILE. IRET IS ',iret
     call w3tage('FIRE2MDL')
     call errexit(82)
   end if

 elseif (isgrib == 2) then

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

   if (iret /= 0) then
     print*,'- FATAL ERROR. BAD DEGRIB OF FILE. IRET IS ',iret
     call w3tage('FIRE2MDL')
     call errexit(82)
   end if

!-----------------------------------------------------------------------
! create the grib 1 gds array from the g2 gdt array.  the grib 1
! gds info is used by ipolates and for gribbing the final analysis.
!-----------------------------------------------------------------------

   call gdt_to_gds(gfld%igdtnum, gfld%igdtmpl, gfld%igdtlen, kgds_mdl, &
                   imdl, jmdl, resol_mdl)

   grid_id_mdl = 255 ! grib1 grid id number. n/a for grib2.
                     ! set to 'missing'.

   allocate(lats_mdl_temp(imdl,jmdl))
   lats_mdl_temp = reshape (gfld%fld , (/imdl,jmdl/) )

   call gf_free2(gfld)

 endif  ! is file grib1 or grib2?

 call baclose(iunit,iret)

!-----------------------------------------------------------------------
! read longitudes on the model grid.
!-----------------------------------------------------------------------

 print*,"- INGEST MODEL GRID LONGITUDES"

 fngrib = model_lon_file

 call grib_check(fngrib, isgrib)

 if (isgrib==0) then
   print*,'- FATAL ERROR, MODEL LONGITUDE FILE MUST BE GRIB1 OR GRIB2 FORMAT'
   call w3tage('FIRE2MDL')
   call errexit(92)
 end if

 print*,"- OPEN MODEL LONGITUDE FILE ", trim(fngrib)
 call baopenr (iunit, fngrib, iret)

 if (iret /= 0) then
   print*,"- FATAL ERROR. BAD OPEN. IRET IS ", iret
   call w3tage('FIRE2MDL')
   call errexit(83)
 end if

 if (isgrib == 1) then

   lskip   = -1  
   kgds    = -1   
   kpds    = -1  
   jgds    = -1
   jpds    = -1
   jpds(5) = 177  ! longitude 

   allocate(lons_mdl_temp(imdl,jmdl))
   allocate(lbms(imdl*jmdl))

   print*,"- DEGRIB DATA"
   call getgb(iunit, lugi, (imdl*jmdl), lskip, jpds, jgds, &
              numpts, message_num, kpds, kgds, lbms, lons_mdl_temp, iret)

   deallocate (lbms)

   if (iret /= 0) then
     print*,'- FATAL ERROR. BAD DEGRIB OF DATA. IRET IS ',iret
     call w3tage('FIRE2MDL')
     call errexit(84)
   end if

 elseif (isgrib == 2) then  ! grib 2 file

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

   if (iret /= 0) then
     print*,'- FATAL ERROR. BAD DEGRIB OF FILE. IRET IS ',iret
     call w3tage('FIRE2MDL')
     call errexit(84)
   endif

   allocate(lons_mdl_temp(imdl,jmdl))
   lons_mdl_temp = reshape (gfld%fld , (/imdl,jmdl/) )

   call gf_free2(gfld)

 endif ! is file grib1 or grib2?

 call baclose(iunit, iret)

!-----------------------------------------------------------------------
! read model land/sea mask. 
!-----------------------------------------------------------------------

 print*,"- INGEST MODEL LAND MASK"

 fngrib = model_lsmask_file

 call grib_check(fngrib, isgrib)

 if (isgrib==0) then
   print*,'- FATAL ERROR, MODEL LAND MASK FILE MUST BE GRIB1 OR GRIB2 FORMAT'
   call w3tage('FIRE2MDL')
   call errexit(93)
 end if

 print*,"- OPEN MODEL LANDMASK FILE ", trim(fngrib)
 call baopenr (iunit, fngrib, iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR. BAD OPEN OF FILE. IRET IS ', iret
   call w3tage('FIRE2MDL')
   call errexit(85)
 end if

 if (isgrib == 1) then

   lskip   = -1 
   kgds    = -1  
   kpds    = -1 
   jpds    = -1
   jgds    = -1
   jpds(5) = 81   ! land-sea mask

   allocate(lsmask_mdl(imdl,jmdl))
   allocate(lbms(imdl*jmdl))

   print*,"- DEGRIB DATA"
   call getgb(iunit, lugi, (imdl*jmdl), lskip, jpds, jgds, &
              numpts, message_num, kpds, kgds, lbms, lsmask_mdl, iret)

   deallocate (lbms)

   if (iret /= 0) then
     print*,'- FATAL ERROR. BAD DEGRIB OF DATA. IRET IS ',iret
     call w3tage('FIRE2MDL')
     call errexit(86)
   end if

 elseif (isgrib == 2) then ! grib 2 file

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

   if (iret /= 0) then
     print*,'- FATAL ERROR. BAD DEGRIB OF FILE. IRET IS ',iret
     call w3tage('FIRE2MDL')
     call errexit(86)
   endif

   allocate(lsmask_mdl(imdl,jmdl))
   lsmask_mdl = reshape (gfld%fld , (/imdl,jmdl/) )

   call gf_free2(gfld)

 end if  ! is file grib1 or grib2?

 call baclose(iunit,iret)

!-----------------------------------------------------------------------
! program only worries about land points.   save i/j coordinate
! with respect to 2-d grid.
!-----------------------------------------------------------------------

 ij = 0

 do j = 1, jmdl
 do i = 1, imdl
   if (lsmask_mdl(i,j) > 0.0) then
     ij = ij+1
   end if
 enddo
 enddo

 ijmdl = ij

 if (ijmdl == 0) then  ! grid has only water points, dont run
   print*,' '
   print*,'- MODEL GRID ONLY HAS WATER POINTS, DONT CREATE FIRE FILE.'
   print*,'- NORMAL TERMINATION.'
   call w3tage('FIRE2MDL')
   call errexit(0)
 endif

 allocate (lats_mdl(ijmdl))
 allocate (lons_mdl(ijmdl))
 allocate (ipts_mdl(ijmdl))
 allocate (jpts_mdl(ijmdl))

 ij = 0
 do j = 1, jmdl
 do i = 1, imdl
   if (lsmask_mdl(i,j) > 0.0) then
     ij = ij+1
     lats_mdl(ij) = lats_mdl_temp(i,j)
     lons_mdl(ij) = lons_mdl_temp(i,j)
     ipts_mdl(ij) = i
     jpts_mdl(ij) = j
   end if
 enddo
 enddo

 lat11=lats_mdl_temp(1,1)
 latlast=lats_mdl_temp(imdl,jmdl)
 lon11=lons_mdl_temp(1,1)
 lonlast=lons_mdl_temp(imdl,jmdl)

 deallocate (lats_mdl_temp, lons_mdl_temp)

 return

 end subroutine read_mdl_grid_info

 subroutine model_grid_cleanup
!$$$  subprogram documentation block
!              
! subprogram:    model_grid_cleanup
!   prgmmr: gayno          org: w/np2     date: 2014-nov-11
!
! abstract: deallocate this module's public allocatable arrays.
!
! program history log:
! 2014-nov-11  gayno    - initial version
!
! usage: call model_grid_cleanup
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files: none
!
! condition codes: none
!
! remarks: none.
!
!$$$

 implicit none

 if (allocated(lsmask_mdl))     deallocate(lsmask_mdl)
 if (allocated(lats_mdl))       deallocate(lats_mdl)
 if (allocated(lons_mdl))       deallocate(lons_mdl)
 if (allocated(ipts_mdl))       deallocate(ipts_mdl)
 if (allocated(jpts_mdl))       deallocate(jpts_mdl)
  
 return

 end subroutine model_grid_cleanup

 end module model_grid
