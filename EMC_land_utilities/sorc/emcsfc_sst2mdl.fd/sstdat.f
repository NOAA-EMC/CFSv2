 module sstdat
!$$$  module documentation block
!
! $Revision$
!
! module:    sstdat
!   prgmmr: gayno         org: w/np2     date: 2005-05-20
!
! abstract: reads sst source data
!
! program history log:
!   2005-05-20  gayno   - initial version
!   2010-08-11  gayno   - add reads of flake climo data and
!                         1/12 degree global sst landmask file
!   2014-10-28  gayno   - logic to read global mmab sst data
!                         in grib 1 or grib 2 format.  read
!                         grib 2 version of global mmab landmask
!                         file.
!   2015-05-26  gayno   - flake climo file was changed to grib 2
!                         and daily records.
!
! usage: use sstdat
!
! remarks: some variable definitions
!   bitmap_flake     - bitmap (land mask) of the flake climo data
!   bitmap_src       - bitmap (land mask) of global mmab source grid
!   century          - century of global mmab source data
!   day              - day of global mmab source data
!   resol_src        - resolution of global mmab source data in degrees
!   hour             - hour of global mmab source data
!   iflake           - i-dimension of flake climo grid
!   jflake           - j-dimension of flake climo grid
!   isrc             - i-dimension of global mmab source grid
!   jsrc             - j-dimension of global mmab source grid
!   isrc_14km        - i-dimension of GLERL 14km n america source grid
!   jsrc_14km        - j-dimension of GLERL 14km n america source grid
!   kgds_flake       - grib1 grid description section - flake grid
!   kgds_src         - grib1 grid description section - global mmab source grid
!   month            - month of global mmab source data
!   sst_flake        - flake sst data
!   sst_src          - global mmab sst data
!   sst_14km_src     - GLERL 14 km north america sst data
!   year             - year of global mmab data
!
!$$$

 use consts, only         : frz_h20

 use program_setup, only  : input_src_file,   &
                            input_src_bitmap_file, &
                            input_src14km_file, &
                            input_flake_file

 private

 logical*1, allocatable, public  :: bitmap_src(:,:), bitmap_flake(:,:)
 integer, public                 :: century
 integer, public                 :: day
 integer, public                 :: hour
 integer, public                 :: iflake, jflake
 integer, public                 :: isrc
 integer, parameter, public      :: isrc_14km = 153
 integer, public                 :: jsrc
 integer, parameter, public      :: jsrc_14km = 73
 integer, public                 :: kgds_src(200), kgds_flake(200)
 integer, public                 :: month
 integer, public                 :: year 

 real, public                    :: resol_src
 real, allocatable, public       :: sst_flake(:,:)
 real, allocatable, public       :: sst_src(:,:)
 real, allocatable, public       :: sst_14km_src(:,:)

 public readsst

 contains
 
 subroutine readsst
!$$$  subprogram documentation block
!
!subprogram:    readsst
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  Read the following input sst data:
!            - global mmab data (grib1 or grib2) and 
!              its corresponding landmask (grib2)
!            - GLERL 14km north america sst data (binary flat)
!            - FLAKE-based climatological sst data. (grib2)
!            All except the global mmab data are optional.
!   
! program history log:
! 2005-05-20  gayno   - initial version
! 2014-10-28  gayno   - logic to read global mmab sst data
!                       in grib 1 or grib 2 format.  read
!                       grib 2 version of global mmab landmask
!                       file.
! 2015-05-26  gayno   - flake climo file is now grib 2 and
!                       has daily records.
!
! usage: call readsst
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files:
!    input:
!      - global mmab sst data (grib1 or grib2); required
!      - global mmab sst landmask (grib2); optional
!      - GLERL sst data (binary flat); optional
!      - FLAKE climo data (grib2); optional
!
!    output: none
!
! condition codes:  all fatal
!    62 - bad open of global sst data file
!    63 - bad read of global sst grib header (grib 1 version)
!    64 - global sst data not on lat/lon grid
!    65 - bad read of global sst grib data (grib 1 version)
!    70 - global sst data not grib1 or grib2
!    71 - bad read of global sst grib data (grib 2 version)
!    72 - bad open of global sst landmask file
!    73 - bad read of global sst landmask file
!    74 - global sst data and landmask on different grids.
!    97 - bad open of flake file
!    99 - bad read of flake file
!
!$$$

 use grib_mod    ! ncep grib 2 library

 implicit none

 integer                    :: i, j, k
 integer                    :: iret, isgrib
 integer                    :: iunit  ! grib file unit number
 integer                    :: jdisc, jgdtn, jpdtn
 integer                    :: jids(200), jgdt(200), jpdt(200)
 integer                    :: jgds(200), jpds(200), kgds(200), kpds(200)
 integer                    :: lgrib
 integer                    :: lskip
 integer                    :: lugi   ! grib index file unit number - not used
 integer                    :: numbytes
 integer                    :: numpts

 logical                    :: unpack

 real                       :: dummy2
 real(kind=4), allocatable  :: dummy(:,:)
 real, allocatable          :: mask(:,:)

 type(gribfield)            :: gfld

!---------------------------------------------------------------------
! read the global sst file.  this is a required file.
!---------------------------------------------------------------------

 print*,''
 print*,"- INGEST GLOBAL SST DATA"
 call grib_check(input_src_file, isgrib)

 if (isgrib==0) then
   print*,'- FATAL ERROR: GLOBAL SST FILE MUST BE GRIB1 OR GRIB2 FORMAT.'
   call w3tage('SST2MDL')
   call errexit(70)
 end if

 print*,"- OPEN FILE ", trim(input_src_file)

 iunit = 13
 lugi  = 0
 call baopenr (iunit, input_src_file, iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR: BAD OPEN OF FILE, IRET IS ', iret
   call w3tage('SST2MDL')
   call errexit(62)
 end if

 if (isgrib == 1) then  ! a grib 1 file

!---------------------------------------------------------------------
! tell degribber to look for a surface temperature.
!---------------------------------------------------------------------

   lskip    = -1 
   jpds     = -1
   jgds     = -1
   jpds(5)  = 11
   jpds(6)  =  1
   kpds     = jpds
   kgds     = jgds

   print*,"- READ GRIB HEADER"

   call getgbh(iunit, lugi, lskip, jpds, jgds, lgrib,  &
               numbytes, numpts, kpds, kgds, iret)

   if (iret /= 0) then
     print*,"- FATAL ERROR: BAD READ OF GRIB HEADER, IRET IS ", iret
     call w3tage('SST2MDL')
     call errexit(63)
   end if

   print*,"- PDS OF DATA ",kpds(1:23)
   print*,"- GDS OF DATA ",kgds(1:15)

   if (kgds(1) /= 0) then
     print*,"- FATAL ERROR: GLOBAL SST DATA IS NOT ON A LAT/LON GRID."
     call w3tage('SST2MDL')
     call errexit(64)
   endif

   kgds_src = kgds

!--------------------------------------------------------------------
! need to check this date against the last time the
! sst data was updated in the model run.
!--------------------------------------------------------------------

   year    = kpds(8)
   month   = kpds(9)
   day     = kpds(10)
   hour    = kpds(11)
   century = kpds(21)

   print*,"- DATE OF DATA IS: YEAR ",((century-1)*100+year), " MON ", &
                                  month, " DAY ", day, " HOUR ", hour

   isrc = kgds(2)
   jsrc = kgds(3)
   
!--------------------------------------------------------------------
! note, the marine branch creates its global data with no bitmap.
! in other words, there is an sst value everywhere, including
! land areas.  optionally, a seperate bitmap file may be read
! in below.
!--------------------------------------------------------------------

   allocate(sst_src(isrc,jsrc))
   allocate(bitmap_src(isrc,jsrc))

   print*,"- DEGRIB DATA "

   call getgb(iunit, lugi, (isrc*jsrc), lskip, jpds, jgds, &
              numpts, lskip, kpds, kgds, bitmap_src, sst_src, iret)

   if (iret /= 0) then
     print*,"- FATAL ERROR: BAD DEGRIB OF DATA. IRET IS ", iret 
     call w3tage('SST2MDL')
     call errexit(65)
   end if

!--------------------------------------------------------------------
! determine approximate grid resolution in degrees.
!--------------------------------------------------------------------

   resol_src = float(kgds(9))  / 1000.0  ! in degrees

 elseif (isgrib == 2) then  ! file is grib 2

   j       = 0      ! search at beginning of file
   jdisc   = 0      ! search for discipline; 0 - meteorological products
   jpdtn   = -1     ! search for any product definition template number
   jgdtn   = 0      ! search for grid definition template number 0 - lat/lon grid
   jids    = -9999  ! array of values in identification section, set to wildcard
   jgdt    = -9999  ! array of values in grid definition template 3.m
   jpdt    = -9999  ! array of values in product definition template 4.n
   jpdt(1) = 0      ! search for parameter category - temperature
   jpdt(2) = 0      ! search for parameter number - temperature
   unpack  = .true. ! unpack data

   call grib2_null(gfld)

   print*,"- DEGRIB DATA"
   call getgb2(iunit, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
               unpack, k, gfld, iret)

   if (iret /=0) then
    print*,'- FATAL ERROR: BAD DEGRIB OF FILE, IRET IS ', iret
    call w3tage('SST2MDL')
    call errexit(71)
   endif

   print*,"- PDS OF DATA ",gfld%ipdtmpl(1:15)
   print*,"- GDS OF DATA ",gfld%igdtmpl(1:18)

   print*,"- DATA VALID AT (YYYYMMDDHH): ", gfld%idsect(6),gfld%idsect(7), &
                                            gfld%idsect(8),gfld%idsect(9)

   year = mod(gfld%idsect(6),100)
   if (year==0) then
     year = 100
     century = gfld%idsect(6)/100
   else
     century = gfld%idsect(6)/100+1
   endif

   month  = gfld%idsect(7)
   day    = gfld%idsect(8)
   hour   = gfld%idsect(9)

   call gdt_to_gds(gfld%igdtnum, gfld%igdtmpl, gfld%igdtlen, kgds_src, &
                   isrc, jsrc, resol_src)

   allocate (sst_src(isrc,jsrc))
   sst_src  = reshape (gfld%fld , (/isrc,jsrc/) )

   allocate (bitmap_src(isrc,jsrc))
   if (gfld%ibmap == 0) then ! bitmap applies
     bitmap_src  = reshape (gfld%bmap , (/isrc,jsrc/) )
   else
     bitmap_src = .true.
   endif

   call grib2_free(gfld)

 end if  ! is source file grib 1 or grib 2

 call baclose(iunit,iret)

!--------------------------------------------------------------------
! NCEP mmab sst data is not gribbed with a bitmap.  However, the 1/12
! degree data has a separate land mask that can be used if
! selected.  Warning. program only tested with 1/12 degree mask!!
! Must be a grib 2 file.
!--------------------------------------------------------------------

 if (len_trim(input_src_bitmap_file) > 0) then

   print*,''
   print*,"- INGEST GLOBAL SST BITMAP (LANDMASK) FILE"
   print*,"- OPEN BITMAP FILE ", trim(input_src_bitmap_file)

   iunit = 14
   lugi  = 0
   call baopenr (iunit, input_src_bitmap_file, iret)

   if (iret /= 0) then
     print*,'- FATAL ERROR: BAD OPEN OF FILE, IRET IS ', iret
     call w3tage('SST2MDL')
     call errexit(72)
   endif

   j       = 0      ! search at beginning of file
   jdisc   = 2      ! search for discipline 2 - land-sfc products
   jpdtn   = 0      ! search for product definition template number
   jgdtn   = 0      ! search for grid definition template number; 0 - lat/lon grid
   jids    = -9999  ! array of values in identification section, set to wildcard
   jgdt    = -9999  ! array of values in grid definition template 3.m
   jpdt    = -9999  ! array of values in product definition template 4.n
   jpdt(1) = 0      ! search for parameter category 0 - veg/biomass
   jpdt(2) = 0      ! search for parameter number 0 - land cover
   unpack  = .true. ! unpack data

   print*,"- DEGRIB DATA"
   call getgb2(iunit, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
               unpack, k, gfld, iret)

   if (iret /=0) then
    print*,'- FATAL ERROR: BAD DEGRIB OF FILE, IRET IS ', iret
    call w3tage('SST2MDL')
    call errexit(73)
   endif

   print*,"- PDS OF DATA ",gfld%ipdtmpl
   print*,"- GDS OF DATA ",gfld%igdtmpl

!---------------------------------------------------------------------
! A simple check to make sure the bitmap file and the sst file are 
! on the same grid.  A lat/lon grid was specified by setting
! jgdtn = 0 above.  Here, we check the grid dimensions.
!---------------------------------------------------------------------

   if (gfld%igdtmpl(8) /= isrc .or. gfld%igdtmpl(9) /= jsrc) then
     print*,"- FATAL ERROR: SST DATA AND BITMAP DATA ON DIFFERENT GRIDS."
     call w3tage('SST2MDL')
     call errexit(74)
   endif

   allocate(mask(isrc,jsrc))
   mask = reshape (gfld%fld , (/isrc,jsrc/) )

!---------------------------------------------------------------------
! The 1/12 degree land mask is '0' - water; '1.57' - land; 
! '1.95' - coast.  Coast is treated as water in the analysis.
!---------------------------------------------------------------------

   bitmap_src=.true.
   do j=1,jsrc
   do i=1,isrc
     if (mask(i,j) > 1.55 .and. mask(i,j) < 1.59) then  ! land
       bitmap_src(i,j)=.false.
     endif
   enddo
   enddo

   deallocate(mask)

   call grib2_free(gfld)

   call baclose(iunit,iret)

 endif

!---------------------------------------------------------------------
! If desired, flake climo data can be used to fill in lakes
! not resolved by the input sst data.
!---------------------------------------------------------------------

 if (len_trim(input_flake_file) > 0) then

   print*,"- OPEN FLAKE CLIMATOLOGY FILE ", trim(input_flake_file)

   iunit = 15
   call baopenr (iunit, input_flake_file, iret)

   if (iret /= 0) then
     print*,'- FATAL ERROR: BAD OPEN OF FILE, IRET IS ', iret
     call w3tage('SST2MDL')
     call errexit(97)
   endif

   j       = 0      ! search at beginning of file
   lugi    = 0      ! no grib index file
   jdisc   = 10     ! search for discipline; 10 - ocean products
   jpdtn   = 0      ! search for product definition template number; 0 - analysis at one level
   jgdtn   = 0      ! search for grid definition template number; 0 - lat/lon grid
   jids    = -9999  ! array of values in identification section, set to wildcard
   jids(7) = month 
   jids(8) = day
   jgdt    = -9999  ! array of values in grid definiation template 3.m
   jpdt    = -9999  ! array of values in product definition template 4.n
   jpdt(1) = 3      ! search for parameter category - surface properties
   jpdt(2) = 0      ! search for parameter number - water temp
   unpack  = .true. ! unpack data

   call grib2_null(gfld)

   call getgb2(iunit, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
               unpack, k, gfld, iret)

   if (iret /= 0) then
     print*,"- FATAL ERROR: BAD DEGRIB OF DATA. IRET IS ", iret 
     call w3tage('SST2MDL')
     call errexit(99)
   end if

   call baclose(iunit,iret)

   print*,"- TIME OF DATA (YYYYMMDDHH): ", gfld%idsect(6),gfld%idsect(7), &
                                         gfld%idsect(8),gfld%idsect(9)

!--------------------------------------------------------------------
! Convert to grib1 kgds array required by iplib.
!--------------------------------------------------------------------

   call gdt_to_gds(gfld%igdtnum, gfld%igdtmpl, gfld%igdtlen, kgds_flake, &
                   iflake, jflake, dummy2)

   print*,"- PDS OF DATA ",gfld%ipdtmpl
   print*,"- GDS OF DATA ",kgds_flake(1:15)

   allocate(sst_flake(iflake,jflake))
   sst_flake=reshape (gfld%fld, (/iflake,jflake/) )
   allocate(bitmap_flake(iflake,jflake))
   bitmap_flake=reshape (gfld%bmap, (/iflake,jflake/) )

   call grib2_free(gfld)

 endif
 
!--------------------------------------------------------------------
! If desired, read in the GLERL north american 14km sst data
! field.  It spans a domain approx covering (10n-60n, 35w-160w).
! In reality, it is mostly a 50 km sst with 14 km sst overlaid
! only over the great lakes.  Therefore, only use this data
! between 41n-50n and 94w-75w.
!
! This data is optional!  It is included mainly for the
! regional models.  To not use it, set the namelist entry to
! an empty string.
!--------------------------------------------------------------------

 if (len_trim(input_src14km_file) > 0) then

  print*,''
  print*,"- INGEST GLERL REGIONAL SST DATA."
  print*,"- OPEN FILE: ", trim(input_src14km_file)

  iunit=19
  open (iunit, file=trim(input_src14km_file), &
        form="unformatted", iostat=iret, err=200)

  print*,"- READ FILE: ", trim(input_src14km_file)

  allocate (dummy(1041,441))  ! dimensions of entire domain.

  do i = 1, 1041
    read (iunit, iostat=iret, err=200, end=200) (dummy(i,j),j=1,441)
  enddo

  close (iunit)

  allocate (sst_14km_src(isrc_14km,jsrc_14km))

!--------------------------------------------------------------------
! save data between 41n-50n and 94w-75w.
!--------------------------------------------------------------------

  do j = 1, jsrc_14km
  do i = 1, isrc_14km
    sst_14km_src(i,j) = dummy(568+i,248+j) + frz_h20
  enddo
  enddo

  deallocate (dummy)

 end if

 return

 200 print*,"- WARNING: ERROR OPENING OR READING: ",trim(input_src14km_file)
     print*,"- I/O STATUS IS: ", iret
     print*,"- WILL NOT PROCESS THIS DATA."

 if (allocated(dummy)) deallocate(dummy)

 return

 end subroutine readsst

 end module sstdat
