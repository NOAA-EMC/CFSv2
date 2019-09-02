 module firedat
!$$$  module documentation block
!                .      .    .                                       .
! module:    firedat
!   prgmmr: gayno         org: w/np2     date: 2015-jan-30
!
! $Revision$
!
! abstract: this module contains routines to read
!   input nesdis fire (burnt area) data.
!
! program history log:
!   2015-jan-30  gayno   - initial version
!
! usage: "use firedat".  Then call public routine
!   "read_firedat_driver" to read the nesdis fire
!   data.
!
! remarks: user may select the 1- or 12-km nesdis
!   data or both.  Some variable declarations:
!
!   gfld_1km_firedat  - data structure that holds the
!                       data and header info
!   gfld_12km_firedat - data structure that holds the
!                       data and header info
!   use_1km           - when .true., the 1km data 
!                       will be used.
!   use_12km          - when .true., the 12km data 
!                       will be used.
!   valid_time_1km    - data structure that holds
!                       the valid time of the 1km data
!   valid_time_12km   - data structure that holds
!                       the valid time of the 12km data
!
!$$$
!
 use grib_mod

 implicit none

 private

 public                  :: read_firedat_driver

 type(gribfield), public :: gfld_1km_firedat
 type(gribfield), public :: gfld_12km_firedat

 logical, public         :: use_1km, use_12km

 type :: grib_date
   integer century
   integer year
   integer year_4
   integer month
   integer day
   integer hour
   integer accum_period
 end type grib_date

 type (grib_date), public :: valid_time_1km, valid_time_12km

 contains

 subroutine read_firedat_driver
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_firedat_driver
!   prgmmr: gayno          org: w/np2     date: 2015-jan-30
!
! abstract: driver routine for reading nesdis fire (burnt area)
!   data.  there are 1- and 12-km versions.  data are in
!   grib2 format.
!
! program history log:
! 2015-jan-30  gayno    - initial version
!
! usage: call read_firedat_driver
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files: none
!
! condition codes:
!   37 - no input fire/burned area data selected; fatal
!
! remarks: none.
!
!$$$

 use program_setup, only : nesdis_12km_fire_file, &
                           nesdis_1km_fire_file

 implicit none

 call grib2_null(gfld_1km_firedat)
 call grib2_null(gfld_12km_firedat)

 use_12km=.false.
 if (len_trim(nesdis_12km_fire_file) > 0) then
   print*,'- WILL USE 12KM NESDIS DATA'
   call read_firedat(gfld_12km_firedat, nesdis_12km_fire_file, valid_time_12km, use_12km)
 endif

 use_1km=.false.
 if (len_trim(nesdis_1km_fire_file) > 0) then
   print*,'- WILL USE 1KM NESDIS DATA'
   call read_firedat(gfld_1km_firedat, nesdis_1km_fire_file, valid_time_1km, use_1km)
 endif

 if (.not. use_1km .and. .not. use_12km) then
   print*,"- FATAL ERROR. NO INPUT FIRE DATA AVAILABLE, STOP"
   call w3tage('FIRE2MDL')
   call errexit(37)
 endif

 return

 end subroutine read_firedat_driver

 subroutine read_firedat(gfld, nesdis_file, valid_time, status)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_firedat
!   prgmmr: gayno          org: w/np2     date: 2015-jan-30
!
! abstract: read nesdis fire (burnt area) data. there are
!   1- and 12-km versions.  data are in grib2 format.
!
! program history log:
! 2015-jan-30  gayno    - initial version
!
! usage: call read_firedat(gfld, nesdis_file, valid_time, status)
!
!   input argument list: 
!     nesdis_file     - path/name of nesdis burned area data.
!
!   output argument list: 
!     gfld            - grib2 data structure that holds the 
!                       burned area data and header info
!     valid_time      - data structure that holds the valid
!                       time of the burned area data
!     status          - when true, a successful read.
!                       when false, read failed.
!
! files: 
!    input:
!      - nesdis fire/burned area data (grib 2)
!
!    output: none
!
! condition codes: 
!    - if read succesful, 'status' is .true.  otherwise,
!      it is .false.  not fatal.
!
! remarks: none.
!
!$$$

 implicit none

 character*(*), intent(in)    :: nesdis_file

 logical, intent(out)         :: status

 type(gribfield), intent(out) :: gfld
 type(grib_date), intent(out) :: valid_time

 integer                      :: lugb, iret, ij
 integer                      :: j, k, lugi, jdisc, jgdtn, jpdtn
 integer                      :: jids(200), jgdt(200), jpdt(200)

 logical                      :: unpack

 status=.true.

 print*,"- OPEN NESDIS FIRE FILE: ",trim(nesdis_file)
 lugb=13
 call baopenr(lugb,nesdis_file,iret)
 if (iret /= 0) then
   print*,'- WARNING: BAD OPEN, IRET: ',iret
   status=.false.
   return
 endif

 print*,"- DEGRIB NESDIS FIRE DATA"
 j = 0
 lugi = lugb   ! must always force regeneration of index file
               ! when recycling the fortran unit number.
 jdisc = -1
 jpdtn = -1
 jgdtn = -1
 jids = -9999
 jgdt = -9999
 jpdt = -9999
 unpack=.true.

 call getgb2(lugb, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
             unpack, k, gfld, iret)

 if (iret /= 0) then
   print*,'- WARNING: BAD DEGRIB, IRET: ',iret
   status=.false.
   return
 endif

 call baclose(lugb,iret)

 print*,'- GRIB HEADER INFO:'
 print*,'- SECTION 0'
 print*,'- grib version ', gfld%version
 print*,'- discipline   ', gfld%discipline
 print*,'- SECTION 1'
 print*,'- length of id section ',gfld%idsectlen
 print*,'- orig center  ', gfld%idsect(1)
 print*,'- sub center   ', gfld%idsect(2)
 print*,'- master table version number ', gfld%idsect(3)
 print*,'- local table version number ', gfld%idsect(4)
 print*,'- significance of reference time ', gfld%idsect(5)
 print*,'- reference time ',gfld%idsect(6:9)
 print*,'- production status ',gfld%idsect(12)
 print*,'- type of processed data ',gfld%idsect(13)
 if (gfld%locallen > 0) print*,'- length of local section 2 ',gfld%locallen
 print*,'- SECTION 3'
 print*,'- length of section 3 ',gfld%igdtlen
 print*,'- grid definition ',gfld%griddef
 print*,'- grid def template number ',gfld%igdtnum
 print*,'- shape of earth ',gfld%igdtmpl(1)
 print*,'- scale factor of radius of spherical earth ',gfld%igdtmpl(2)
 print*,'- scale value of radius of spherical earth ',gfld%igdtmpl(3)
 print*,'- scale factor of major axis of oblate spheriod earth ',gfld%igdtmpl(4)
 print*,'- scale value of major axis of oblate spheriod earth ',gfld%igdtmpl(5)
 print*,'- scale factor of minor axis of oblate spheriod earth ',gfld%igdtmpl(6)
 print*,'- scale value of minor axis of oblate spheriod earth ',gfld%igdtmpl(7)
 print*,'- number of points along the x axis ',gfld%igdtmpl(8)
 print*,'- number of points along the y axis ',gfld%igdtmpl(9)
 print*,'- latitude of first grid point ',gfld%igdtmpl(10)
 print*,'- longitude of first grid point ',gfld%igdtmpl(11)
 print*,'- resolution and component flags ',gfld%igdtmpl(12)
 print*,'- latitude where dx and dy are specified ',gfld%igdtmpl(13)
 print*,'- longitude of meridian parallel to y-axis ',gfld%igdtmpl(14)
 print*,'- x-direction grid length ',gfld%igdtmpl(15)
 print*,'- y-direction grid length ',gfld%igdtmpl(16)
 print*,'- projection center flag ',gfld%igdtmpl(17)
 print*,'- scanning mode ',gfld%igdtmpl(18)
 print*,'- first lat from pole which secant cone cuts sphere ',gfld%igdtmpl(19)
 print*,'- second lat from pole which secant cone cuts sphere ',gfld%igdtmpl(20)
 print*,'- latitude of south pole of projection ',gfld%igdtmpl(21)
 print*,'- longitude of south pole of projection ',gfld%igdtmpl(22)
 print*,'- SECTION 4'
 print*,'- parameter category ',gfld%ipdtmpl(1)
 print*,'- parameter number ',gfld%ipdtmpl(2)
 print*,'- end of data accumulation period ',gfld%ipdtmpl(16:19)

 valid_time%year_4=gfld%ipdtmpl(16)
 valid_time%year=mod(gfld%ipdtmpl(16),100)
 valid_time%century=gfld%ipdtmpl(16)/100 + 1
 if (valid_time%year==0) then 
   valid_time%year=100
   valid_time%century=valid_time%century-1
 endif
 valid_time%month=gfld%ipdtmpl(17)
 valid_time%day=gfld%ipdtmpl(18)
 valid_time%hour=gfld%ipdtmpl(19)
 valid_time%accum_period=gfld%ipdtmpl(27)  ! accumulation period.

 print*,'- RANGE OF FIRE DATA VALUES: ',maxval(gfld%fld),minval(gfld%fld)

 return
 end subroutine read_firedat

 end module firedat
