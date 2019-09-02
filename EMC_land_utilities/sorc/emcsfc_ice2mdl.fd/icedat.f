 module icedat
!$$$  module documentation block
!
! module:    icedat
!   prgmmr: gayno         org: w/np2     date: 2005-05-20
!
! $Revision$
!
! abstract: contains routines to read in sea ice data on source grid
!
! program history log:
!   2005-05-20  gayno   - initial version
!   2007-09-26  gayno   - modified for b-grids. improved error handling
!   2014-10-16  gayno   - logic to read ims and global ice data 
!                         in grib 1 or grib 2 format.
!
! usage: use icedat
!
! remarks: some variable definitions
!   avg_ice_src      - row average ice concentration
!   bitmap_src       - bitmap of source grid (land-true)
!   dy_src           - y dim resolution in degrees 
!   ice_src          - ice concentration of source grid (decimal %)
!   snow_src_save    - ims snow cover info (0 - no, 1 - yes)
!   isrc             - i-dimension of source grid
!   jsrc             - j-dimension of source grid
!   lat_11_src       - corner point latitude of source grid
!   mesh_src         - ims data mesh or bediant size
!   resol_src        - resolution of source grid in degrees
!
!$$$

 use program_setup, only  : input_global_src_file,         &
                            input_global_src_lsmask_file,  &
                            input_ims_src_file,         &
                            input_ims_src_lsmask_file,  &
                            data_flag

 integer*1, allocatable  :: snow_src_save(:,:)
 integer                 :: isrc
 integer                 :: jsrc
 integer                 :: kgds_src(200)
 integer                 :: mesh_src

 logical*1, allocatable  :: bitmap_src(:,:)

 real, allocatable       :: avg_ice_src(:)
 real                    :: dy_src
 real, allocatable       :: ice_src(:,:)
 real                    :: lat_11_src
 real                    :: resol_src

 contains

 subroutine read_ice_src_data
!$$$  subprogram documentation block
!
! subprogram:    read_ice_src_data
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  read sea ice data on source grid.
!
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call read_ice_src_data
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

 if (data_flag == "global") then
   call read_ice_global
 else if (data_flag == "ims") then
   call read_ice_ims
 end if

 return

 end subroutine read_ice_src_data

 subroutine read_ice_ims
!$$$  subprogram documentation block
!
! subprogram:    read_ice_ims
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  Degrib ims sea ice data.  Create ims land mask
!            from bitmap.  Degrib ims snow cover data for use
!            as a psuedo ice flag.  Retrieve some ims grid 
!            parameters.
!            
! program history log:
! 2005-05-20  gayno    - initial version
! 2014-10-16  gayno    - ims data may be grib 1 or grib 2 format.
!
! usage: call read_ice_ims
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files:
!    input:
!      - ims NH ice and snow cover data (grib 1 or grib 2), unit=13.
!        There are two versions of this data, 96th mesh (4 km) 
!        and 16th mesh (23 km).  The 16th mesh data does not have
!        a bitmap, so if selected you must also use its
!        corresponding land mask.
!      - ims 16th mesh land mask, ascii
!
!    output: none
!
! condition codes:  all fatal
!    41 - ims data is not grib 1 or grib 2
!    70 - bad degrib of ims grib 1 ice cover record
!    71 - bad degrib of ims grib 1 snow cover record
!    72 - bad degrib of ims grib 1 header
!    73 - bad open of ims file
!    87 - bad open of ims land mask file
!    88 - bad read of ims land mask file
!    89 - ims land mask must be used with 16th mesh
!         data
!    94 - bad degrib of ims grib 2 ice cover record
!    95 - bad degrib of ims grib 2 snow cover record
!
! remarks: none.
!
!$$$

 use grib_mod   ! ncep grib 2 library

 implicit none

 integer, parameter         :: iunit = 13  ! grib file unit number

 integer                    :: i, j, k
 integer                    :: iret, isgrib
 integer                    :: jgds(200)
 integer                    :: jpds(200)
 integer                    :: jdisc, jgdtn, jpdtn
 integer                    :: jids(200), jgdt(200), jpdt(200)
 integer                    :: lskip
 integer, parameter         :: lugi = 0    ! grib index file unit number - not use
 integer                    :: kgds(200)
 integer                    :: kpds(200)
 integer                    :: message_num
 integer                    :: numbytes
 integer                    :: numpts

 logical*1, allocatable     :: lbms(:,:)
 logical                    :: unpack

 real                       :: dum
 real, allocatable          :: dummy(:,:)
 real*4, allocatable        :: dummy4(:,:)

 type(gribfield)            :: gfld

 print*,"- OPEN AND READ ", trim(input_ims_src_file)

 call grib_check(input_ims_src_file, isgrib)

 if (isgrib==0) then
   print*,'- FATAL ERROR: IMS FILE MUST BE GRIB 1 OR GRIB2 FORMAT'
   call w3tage('ICE2MDL')
   call errexit(41)
 end if

 call baopenr (iunit, input_ims_src_file, iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR: BAD OPEN OF FILE, IRET IS ', iret
   call w3tage('ICE2MDL')
   call errexit(73)
 end if

 if (isgrib==1) then  ! grib 1 format

!-----------------------------------------------------------------------
! tell degribber to look for requested data.
!-----------------------------------------------------------------------

   lskip    = -1
   jgds     = -1
   jgds(1)  = 5       ! ims on polar grid.
   jpds     = -1
   jpds(5)  = 238     ! grib param number for snow cover
   kpds     = jpds
   kgds     = jgds

   print*,"- GET GRIB HEADER"

   call getgbh(iunit, lugi, lskip, jpds, jgds, numbytes,  &
               numpts, message_num, kpds, kgds, iret)

   if (iret /= 0) then
     print*,"- FATAL ERROR: BAD DEGRIB OF HEADER. IRET IS ", iret
     call w3tage('ICE2MDL')
     call errexit(72)
   end if

   print*,"- PDS OF DATA ",kpds(1:23)
   print*,"- GDS OF DATA ",kgds(1:15)

   print*,"- DATE OF DATA IS: YEAR ",((kpds(21)-1)*100+kpds(8)), " MON ", &
                                  kpds(9), " DAY ", kpds(10), " HOUR ", kpds(11)

!-----------------------------------------------------------------------
! save grid dimensions, resolution of source grid.
!-----------------------------------------------------------------------

   isrc = kgds(2)
   jsrc = kgds(3)

   mesh_src = isrc / 64

   resol_src = 381. / float(mesh_src) / 111.0  ! ims grid res in deg

!-----------------------------------------------------------------------
! the snow cover will be used as a psuedo ice flag for small, isolated
! model lakes.
!-----------------------------------------------------------------------

   allocate (snow_src_save(isrc,jsrc))
   allocate (dummy(isrc,jsrc))
   allocate (lbms(isrc,jsrc))

   print*,"- DEGRIB SNOW COVER."

   call getgb(iunit, lugi, (isrc*jsrc), lskip, jpds, jgds, &
              numpts, lskip, kpds, kgds, lbms, dummy, iret)

   if (iret /= 0) then
     print*,"- FATAL ERROR: BAD DEGRIB OF DATA. IRET IS ", iret
     call w3tage('ICE2MDL')
     call errexit(71)
   end if

   kgds_src=kgds

   snow_src_save = 0
   where (dummy > 0.0) snow_src_save = 1

   deallocate (dummy, lbms)

   lskip    = -1
   jpds     = -1
   jgds     = -1
   jpds(5)  = 91     ! grib id for ice
   kpds     = jpds
   kgds     = jgds

   allocate (ice_src(isrc,jsrc))
   allocate (bitmap_src(isrc,jsrc))

   print*,"- DEGRIB ICE COVER"

   call getgb(iunit, lugi, (isrc*jsrc), lskip, jpds, jgds, &
              numpts, lskip, kpds, kgds, bitmap_src, ice_src, iret)

   if (iret /= 0) then
     print*,"- FATAL ERROR: BAD DEGRIB OF DATA. IRET IS ", iret
     call w3tage('ICE2MDL')
     call errexit(70)
   end if

 elseif (isgrib==2) then

   print*,"- DEGRIB ICE COVER."

   j       = 0      ! search at beginning of file
   jdisc   = 10     ! search for discipline; 10 - ocean products
   jpdtn   = 0      ! search for product definition template number; 0 - analysis at one level
   jgdtn   = 20     ! search for grid definition template number; 20 - polar stereographic grid
   jids    = -9999  ! array of values in identification section, set to wildcard
   jgdt    = -9999  ! array of values in grid definition template 3.m
   jpdt    = -9999  ! array of values in product definition template 4.n
   jpdt(1) = 2      ! search for parameter category - ice
   jpdt(2) = 0      ! search for parameter number - ice cover in percent.
   unpack  = .true. ! unpack data

   call grib2_null(gfld)

   call getgb2(iunit, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
               unpack, k, gfld, iret)

   if (iret /=0) then
    print*,'- FATAL ERROR: BAD DEGRIB OF FILE, IRET IS ', iret
    call w3tage('ICE2MDL')
    call errexit(94)
   endif

   print*,"- PDS OF DATA ",gfld%ipdtmpl(1:15)
   print*,"- GDS OF DATA ",gfld%igdtmpl(1:18)

   print*,"- DATA VALID AT (YYYYMMDDHH): ", gfld%idsect(6),gfld%idsect(7), &
                                            gfld%idsect(8),gfld%idsect(9)

!-----------------------------------------------------------------------
! set the grib1 kgds array from the g2 grid definition template array.
! the kgds array is used by ipolates.
!-----------------------------------------------------------------------

   call gdt_to_gds(gfld%igdtnum, gfld%igdtmpl, gfld%igdtlen, kgds_src, &
                   isrc, jsrc, dum)

   mesh_src = isrc / 64

   resol_src = 381. / float(mesh_src) / 111.0  ! ims grid res in deg

   if (mesh_src==16) kgds_src(6)=136  ! the ims 16th mesh grib2 data
                                      ! is gribbed with an elliptical
                                      ! earth.  that is wrong. hardwire
                                      ! a fix here.

   allocate (ice_src(isrc,jsrc))
   ice_src  = reshape (gfld%fld , (/isrc,jsrc/) )
   allocate (bitmap_src(isrc,jsrc))
   bitmap_src  = reshape (gfld%bmap , (/isrc,jsrc/) )

   call grib2_free(gfld)

   print*,"- DEGRIB SNOW COVER."

   j       = 0      ! search at beginning of file
   jdisc   = 0      ! search for discipline; 0 - meteorological products
   jpdtn   = 0      ! search for product definition template number; 0 - analysis at one level
   jgdtn   = 20     ! search for grid definition template number; 20 - polar stereographic grid
   jids    = -9999  ! array of values in identification section, set to wildcard
   jgdt    = -9999  ! array of values in grid definition template 3.m
   jpdt    = -9999  ! array of values in product definition template 4.n
   jpdt(1) = 1      ! search for parameter category - moisture
   jpdt(2) = 201    ! search for parameter number - snow cover in percent.
   unpack  = .true. ! unpack data

   call getgb2(iunit, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
               unpack, k, gfld, iret)

   if (iret /=0) then
    print*,'- FATAL ERROR: BAD DEGRIB OF FILE, IRET IS ', iret
    call w3tage('ICE2MDL')
    call errexit(95)
   endif

   allocate (dummy(isrc,jsrc))
   dummy  = reshape (gfld%fld , (/isrc,jsrc/) )
   allocate (snow_src_save(isrc,jsrc))
   snow_src_save = 0
   where (dummy > 0.0) snow_src_save = 1
   deallocate(dummy)

   call grib2_free(gfld)

 endif

 call baclose(iunit,iret)

!-----------------------------------------------------------------------
! the 16th mesh ims grib data does not have a proper
! bitmap section.  therefore, need to read in the mask
! from file.  but the 96th mesh data has a proper bitmap, so use it.
!-----------------------------------------------------------------------

 if (mesh_src == 16) then

   if (len_trim(input_ims_src_lsmask_file) == 0) then
     print*,"- FATAL ERROR: MUST CHOOSE IMS LAND MASK FILE."
     call w3tage('ICE2MDL')
     call errexit(89)
   end if

   print*,"- OPEN IMS 16TH MESH LAND MASK: ", trim(input_ims_src_lsmask_file)

   open(43, file=trim(input_ims_src_lsmask_file), form="formatted", &
        iostat = iret)

   if (iret /= 0) then
     print*,"- FATAL ERROR OPENING IMS LAND MASK FILE. ISTAT IS: ", iret
     call w3tage('ICE2MDL')
     call errexit(87)
   end if

   print*,"- READ IMS 16TH MESH LAND MASK."

   allocate (dummy4(isrc,jsrc))

   do j = 1, 1024
     read(43, 123, iostat=iret) (dummy4(i,j),i=1,1024)
     if (iret /= 0) then
       print*,"- FATAL ERROR READING IMS LAND MASK FILE. ISTAT IS: ", iret
       call w3tage('ICE2MDL')
       call errexit(88)
     end if
   enddo

   close (43)

!-----------------------------------------------------------------------
! the file has 0-sea, 1-land, 9-off hemi.  this code expects
! 0-non-land (or don't use data), 1-land (use data).
!-----------------------------------------------------------------------

   bitmap_src=.false.

   do j = 1, 1024
     do i = 1, 1024
       if (dummy4(i,j) == 0) bitmap_src(i,j) = .true.
     enddo
   enddo

   deallocate (dummy4)

123 FORMAT(80I1)

 end if

 return

 end subroutine read_ice_ims

 subroutine read_ice_global
!$$$  subprogram documentation block
!
! subprogram:    read_ice_global
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  Degrib global sea ice data.  Create global land mask
!            from bitmap.  Calculate row averaged ice concentration.
!            Retrieve some global grid parameters.
!            
! program history log:
! 2005-05-20  gayno    - initial version
! 2014-10-16  gayno    - global sea ice data may be grib 1 or grib 2
!
! usage: call read_ice_global
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files:
!   input:
!     - global lat/lon sea ice file, grib 1 or grib 2
!     - the global ice file's landsea mask (binary), required
!
!   output: none
!
! condition codes: all fatal
!   41 - global ice file not grib 1 or grib 2 format
!   65 - bad open landmask file
!   66 - bad degrib of global ice file's ice record
!        (grib 1 version)
!   68 - bad read global ice file grib 1 header
!   69 - bad open global ice file
!   92 - landsea mask file not selected
!   93 - bad degrib of global ice file's ice record
!        (grib 2 version)
!
! remarks: none.
!
!$$$

 use grib_mod

 implicit none

 character, allocatable  :: dummy(:,:)

 integer                 :: bit2
 integer                 :: count
 integer                 :: i, j, k
 integer                 :: iret, isgrib
 integer                 :: jdisc, jgdtn, jpdtn
 integer                 :: jids(200), jgdt(200), jpdt(200)
 integer, parameter      :: iunit = 13  ! grib file unit number
 integer                 :: jgds(200)
 integer                 :: jpds(200)
 integer                 :: kgds(200) 
 integer                 :: kpds(200)
 integer                 :: lgrib
 integer                 :: lskip
 integer, parameter      :: lugi = 0    ! grib index file unit number - not used
 integer                 :: numbytes
 integer                 :: numpts

 logical*1, allocatable  :: lbms(:)
 logical                 :: unpack

 real                    :: dum, total

 type(gribfield)         :: gfld

!-----------------------------------------------------------------------
! open and read sea ice data file.
!-----------------------------------------------------------------------

 print*,"- OPEN FILE ", trim(input_global_src_file)

 call grib_check(input_global_src_file, isgrib)

 if (isgrib==0) then
   print*,'- FATAL ERROR: GLOBAL ICE FILE MUST BE GRIB 1 OR GRIB2 FORMAT'
   call w3tage('ICE2MDL')
   call errexit(41)
 end if

 call baopenr (iunit, input_global_src_file, iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR: BAD OPEN OF FILE, IRET IS ', iret
   call w3tage('ICE2MDL')
   call errexit(69)
 end if

 if (isgrib == 1) then

!-----------------------------------------------------------------------
! tell degribber to look for sea ice data.
!-----------------------------------------------------------------------

   lskip    = -1 
   jpds     = -1
   jgds     = -1
   jgds(1)  = 0    ! data must be on lat/lon grid
   jpds(5)  = 91
   jpds(6)  = 102
   kpds     = jpds
   kgds     = jgds

   print*,"- READ GRIB HEADER"

   call getgbh(iunit, lugi, lskip, jpds, jgds, lgrib,  &
               numbytes, numpts, kpds, kgds, iret)

   if (iret /= 0) then
     print*,"- FATAL ERROR: BAD READ OF GRIB HEADER, IRET IS ", iret
     call w3tage('ICE2MDL')
     call errexit(68)
   end if

   print*,"- PDS OF DATA ",kpds(1:23)
   print*,"- GDS OF DATA ",kgds(1:15)

   if (kgds(1) /= 0) then
     print*,"- FATAL ERROR: SEA ICE DATA IS NOT ON A LAT/LON GRID"
     call w3tage('ICE2MDL')
     call errexit(67)
   endif

   print*,"- DATE OF DATA IS: YEAR ",((kpds(21)-1)*100+kpds(8)), " MON ", &
                                  kpds(9), " DAY ", kpds(10), " HOUR ", kpds(11)

   kgds_src=kgds

   isrc = kgds(2)
   jsrc = kgds(3)
   
   allocate(ice_src(isrc,jsrc))
   allocate(lbms(isrc*jsrc))

   print*,"- DEGRIB DATA "

   call getgb(iunit, lugi, (isrc*jsrc), lskip, jpds, jgds, &
              numpts, lskip, kpds, kgds, lbms, ice_src, iret)

   if (iret /= 0) then
     print*,"- FATAL ERROR: BAD DEGRIB OF DATA. IRET IS ", iret 
     call w3tage('ICE2MDL')
     call errexit(66)
   end if

   deallocate(lbms)

 elseif (isgrib==2) then

   print*,"- DEGRIB ICE COVER."

   j       = 0      ! search at beginning of file
   jdisc   = 10     ! search for discipline; 10 - ocean products
   jpdtn   = 0      ! search for product definition template number; 0 - analysis at one level
   jgdtn   = 0      ! search for grid definition template number; 0 - lat/lon grid
   jids    = -9999  ! array of values in identification section, set to wildcard
   jgdt    = -9999  ! array of values in grid definition template 3.m
   jpdt    = -9999  ! array of values in product definition template 4.n
   jpdt(1) = 2      ! search for parameter category - ice
   jpdt(2) = 0      ! search for parameter number - ice cover in percent.
   unpack  = .true. ! unpack data

   call grib2_null(gfld)

   call getgb2(iunit, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
               unpack, k, gfld, iret)

   if (iret /=0) then
    print*,'- FATAL ERROR: BAD DEGRIB OF FILE, IRET IS ', iret
    call w3tage('ICE2MDL')
    call errexit(93)
   endif

   print*,"- PDS OF DATA ",gfld%ipdtmpl(1:15)
   print*,"- GDS OF DATA ",gfld%igdtmpl(1:18)

   print*,"- DATA VALID AT (YYYYMMDDHH): ", gfld%idsect(6),gfld%idsect(7), &
                                            gfld%idsect(8),gfld%idsect(9)

!-----------------------------------------------------------------------
! set the grib1 kgds array from the g2 grid definition template array.
! the kgds array is used by ipolates.
!-----------------------------------------------------------------------

   call gdt_to_gds(gfld%igdtnum, gfld%igdtmpl, gfld%igdtlen, kgds_src, &
                   isrc, jsrc, dum)

   allocate (ice_src(isrc,jsrc))
   ice_src  = reshape (gfld%fld , (/isrc,jsrc/) )

   call grib2_free(gfld)

 end if

 call baclose(iunit,iret)

!-----------------------------------------------------------------------
! determine some grid specs. 
!-----------------------------------------------------------------------

 lat_11_src = float(kgds_src(4))  / 1000.0
 dy_src     = 180.0/float(jsrc)
 resol_src  = dy_src
 bit2 = mod(kgds_src(11),128) / 64
 if (bit2 == 0) dy_src = -dy_src

!-----------------------------------------------------------------------
! open and read land/sea mask of sea ice input grid.  will be used
! to properly interpolate along coastlines.  note: file not in
! grib format.  must use routine baread to read it.
!-----------------------------------------------------------------------

 if (len_trim(input_global_src_lsmask_file) == 0) then
   print*,"- FATAL ERROR: MUST CHOOSE GLOBAL SOURCE LANDMASK FILE."
   call w3tage('ICE2MDL')
   call errexit(92)
 endif

 print*,"- OPEN FILE ", trim(input_global_src_lsmask_file)

 call baopenr (iunit, input_global_src_lsmask_file, iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR: BAD OPEN OF FILE, IRET IS ', iret
   call w3tage('ICE2MDL')
   call errexit(65)
 end if

 allocate(dummy(isrc,jsrc))
 allocate(bitmap_src(isrc,jsrc))

 call baread(iunit, 0, (isrc*jsrc), numbytes, dummy)

 call baclose(iunit,iret)

 bitmap_src=.false.
 do j=1,jsrc
 do i=1,isrc
   if (ichar(dummy(i,j))==0) bitmap_src(i,j)=.true.
 enddo
 enddo

!cggg note: when using the 1/12 degree (5 minute data) there are
!cggg several flags for coast.  may want to consider using the
!cggg ice data at these points in addition to points that are
!cggg ocean ("zero")

 deallocate(dummy)

!-----------------------------------------------------------------------
! according to marine branch, there may be some ice concentrations
! that exceed one at non-land points.  cap these at one.
!-----------------------------------------------------------------------

 ice_src = min(ice_src, 1.0)

!-----------------------------------------------------------------------
! calculate the average ice concentration in each latitude band.
! will be used to replace any missing grid points after the
! interpolation has been done.  this can happen for small inland
! lakes as in canada.
!
! if a wide search radius was employed during the interpolation, 
! there should be very few missing points.  
!-----------------------------------------------------------------------

 allocate (avg_ice_src(jsrc))

 do j = 1, jsrc
   count = 0
   total = 0.0
   do i = 1, isrc 
     if (bitmap_src(i,j)) then
       count = count + 1
       total = total + ice_src(i,j)
     endif
   enddo
   if (count > 0) then
     avg_ice_src(j) = total / float(count)
   else
     avg_ice_src(j) = -1.0
   end if
   print*,'- AT LAT ',((j-1)*dy_src+lat_11_src), ' AVG ICE IS ', avg_ice_src(j)
 enddo

 return

 end subroutine read_ice_global

 end module icedat
