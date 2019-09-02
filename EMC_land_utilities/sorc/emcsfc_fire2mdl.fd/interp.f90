!
! $Revision$
!
 subroutine interp
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    interp
!   prgmmr: gayno          org: w/np2     date: 2015-jan-30
!
! abstract: this subroutine interpolates burned area data
!   to the model grid.
!
! program history log:
! 2015-jan-30  gayno    - initial version
!
! usage: call interp
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
!
 use model_grid, only : imdl, jmdl, resol_mdl, &
                        model_grid_cleanup

 use firedat, only : gfld_1km_firedat, gfld_12km_firedat, &
                     use_1km, use_12km

 implicit none

 integer                :: i, j

 real, allocatable      :: one_km_mdl (:,:)
 real, allocatable      :: twelve_km_mdl (:,:)
 real, allocatable      :: fire_mdl(:,:)

!-----------------------------------------------------------------------
! if the model grid resolution is coarser than nesdis burned area data,
! use an area averaging approach.  otherwise, use nearest
! neighbor.
!-----------------------------------------------------------------------

 if (use_1km) then
   allocate(one_km_mdl(imdl,jmdl))
   one_km_mdl=-9.
   if (resol_mdl > 3.0) then
     print*,"- INTERPOLATE 1KM NESDIS DATA USING AREA AVG METHOD"
     call area_average(gfld_1km_firedat, one_km_mdl)
   else
     print*,"- INTERPOLATE 1KM NESDIS DATA USING NEAREST NEIGHBOR METHOD"
     call nearest_neighbor(gfld_1km_firedat, one_km_mdl)
   endif
   call gf_free2(gfld_1km_firedat)
 endif

!-----------------------------------------------------------------------
! always use nearest neighbor interpolation for 12km nesdis data.
! our model grids will have similar or higher resolution.
!-----------------------------------------------------------------------

 if (use_12km) then
   allocate(twelve_km_mdl(imdl,jmdl))
   twelve_km_mdl=-9.
   print*,"- INTERPOLATE 12KM NESDIS DATA USING NEAREST NEIGHBOR METHOD"
   call nearest_neighbor (gfld_12km_firedat, twelve_km_mdl)
   call gf_free2(gfld_12km_firedat)
 endif

!-----------------------------------------------------------------------
! if both the 1 and 12km burned area data are chosen, perform a 
! blend.
!-----------------------------------------------------------------------

 allocate (fire_mdl(imdl,jmdl))
 fire_mdl=0.0

 if (use_12km .and. use_1km) then
   print*,"- BLEND THE 1KM AND 12KM DATA"
   call blend(one_km_mdl, twelve_km_mdl, fire_mdl)
   deallocate (one_km_mdl, twelve_km_mdl)
 else if (use_1km) then
   do j = 1, jmdl
   do i = 1, imdl
     if (one_km_mdl(i,j) > 0.0) then
       fire_mdl(i,j) = one_km_mdl(i,j)
     endif
   enddo
   enddo
   deallocate (one_km_mdl)
 else if (use_12km) then
   do j = 1, jmdl
   do i = 1, imdl
     if (twelve_km_mdl(i,j) > 0.0) then
       fire_mdl(i,j) = twelve_km_mdl(i,j)
     endif
   enddo
   enddo
   deallocate (twelve_km_mdl)
 endif

!-----------------------------------------------------------------------
! output model grid burned area data to a grib2 file.
!-----------------------------------------------------------------------

 call gribit(fire_mdl, use_1km, use_12km)

 deallocate (fire_mdl)

 call model_grid_cleanup
 
 return
 end subroutine interp

 subroutine blend(one_km_mdl, twelve_km_mdl, fire_mdl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    blend
!   prgmmr: gayno          org: w/np2     date: 2015-jan-30
!
! abstract: this subroutine blends 1 and 12-km input
!   nesdis burned area data on the model grid.  the
!   1-km data is given preference.
!
! program history log:
! 2015-jan-30  gayno    - initial version
!
! usage: call blend(one_km_mdl, twelve_km_mdl, fire_mdl)
!
!   input argument list:  
!     one_km_mdl     - 1-km burned area data on model grid
!     twelve_km_mdl  - 12-km burned area data on model grid
!
!   output argument list:
!     fire_mdl       - blended burned area data on the model grid.
!
! files: none
!
! condition codes: none
!
! remarks: none.
!
!$$$
!
 use model_grid, only : imdl, jmdl, lsmask_mdl

 implicit none

 integer              :: i, j
 real, intent(in)     :: one_km_mdl(imdl,jmdl)
 real, intent(in)     :: twelve_km_mdl(imdl,jmdl)
 real, intent(out)    :: fire_mdl(imdl,jmdl)

 do j = 1, jmdl
 do i = 1, imdl
   if (lsmask_mdl(i,j) > 0.0) then  ! only consider land points
     if (one_km_mdl(i,j) < 0.0 .and. twelve_km_mdl(i,j) < 0.0) then
       fire_mdl(i,j)=0.0
     elseif (one_km_mdl(i,j) >= 0.0 .and. twelve_km_mdl(i,j) >= 0.0) then
       fire_mdl(i,j)=one_km_mdl(i,j)
     elseif (one_km_mdl(i,j) >= 0.0) then
       fire_mdl(i,j)=one_km_mdl(i,j)
     elseif (twelve_km_mdl(i,j) >= 0.0) then
       fire_mdl(i,j)=twelve_km_mdl(i,j)
     endif
   endif
 enddo
 enddo

 return
 end subroutine blend

 subroutine area_average (gfld_firedat, fire_mdl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    area_average
!   prgmmr: gayno          org: w/np2     date: 2015-jan-30
!
! abstract: this subroutine interpolates burned area data
!   to the model grid using an area averaging method
!
! program history log:
! 2015-jan-30  gayno    - initial version
!
! usage: call area_average(gfld_firedat, fire_mdl)
!
!   input argument list:  
!     gfld_firedat - input nesdis burned area data (ncep
!                    g2 library gribfield data structure)
!
!   output argument list:
!     fire_mdl     - burned area data on the model grid.
!
! files: none
!
! condition codes: none
!
! remarks: none.
!
!$$$
!
 use grib_mod

 use gdswzd_mod

 use model_grid, only : kgds_mdl, imdl, jmdl

 implicit none

 type(gribfield), intent(in) :: gfld_firedat

 real, intent(out)           :: fire_mdl(imdl,jmdl)

 integer                     :: count
 integer                     :: nearest_i, nearest_j
 integer                     :: i, j, ij, iopt
 integer                     :: ifire, jfire, ijfire, nret
 integer, allocatable        :: count_mdl(:,:)

 real                        :: fill
 real, allocatable           :: xpts(:), ypts(:)
 real, allocatable           :: lons_firedat(:), lats_firedat(:)
 real(kind=4), allocatable   :: firedat_land(:), sum(:,:)

 print*,'- BEGIN AREA AVERAGE INTERPOLATION'

!-----------------------------------------------------------------------
! get dimensions of input fire data grid and number of land points.
!-----------------------------------------------------------------------

 ifire  = gfld_firedat%igdtmpl(8)
 jfire  = gfld_firedat%igdtmpl(9)
 ijfire = ifire*jfire

 count=0
 do ij = 1, ijfire
   if ( gfld_firedat%bmap(ij) ) count=count+1
 enddo

 allocate(xpts(count))
 allocate(ypts(count))
 allocate(firedat_land(count))
 count=0
 do ij = 1, ijfire
   if ( gfld_firedat%bmap(ij) ) then
     count=count+1
     xpts(count) = float(mod((ij-1),ifire) + 1)
     ypts(count) = float(((ij-1)/ifire) + 1)
     firedat_land(count) = gfld_firedat%fld(ij)
   endif
 enddo

!-----------------------------------------------------------------------
! find lat/lon of all land points on the input fire wx grid.
!-----------------------------------------------------------------------

 iopt=1
 fill=-9999.
 allocate(lons_firedat(count))
 lons_firedat=fill
 allocate(lats_firedat(count))
 lats_firedat=fill
 call lambconf(gfld_firedat%igdtmpl,iopt,count,fill, &
               xpts,ypts,lons_firedat,lats_firedat,nret)

!-----------------------------------------------------------------------
! find the corresponding i/j on the model grid
!-----------------------------------------------------------------------

 iopt=-1
 xpts=fill
 ypts=fill
 call gdswzd(kgds_mdl,IOPT,count,FILL,XPTS,YPTS,lons_firedat,lats_firedat,  &
             nret)

 deallocate (lons_firedat, lats_firedat)
 
!-----------------------------------------------------------------------
! sum the valid input fire points within each model grid box.
!-----------------------------------------------------------------------

 allocate (count_mdl(imdl,jmdl))
 allocate (sum(imdl,jmdl))
 count_mdl=0
 sum = 0.0
 do ij = 1, count
   nearest_i = nint(xpts(ij))
   if (nearest_i < 1 .or. nearest_i > imdl) cycle
   nearest_j = nint(ypts(ij))
   if (nearest_j < 1 .or. nearest_j > jmdl) cycle
   count_mdl(nearest_i,nearest_j) = count_mdl(nearest_i,nearest_j) + 1
   sum(nearest_i,nearest_j)       = sum(nearest_i,nearest_j) + firedat_land(ij)
 enddo

 deallocate (xpts, ypts, firedat_land)

 do j = 1, jmdl
 do i = 1, imdl
   if (count_mdl(i,j) > 0) then
     fire_mdl(i,j) = sum(i,j) / count_mdl(i,j)
   endif
 enddo
 enddo

 deallocate (count_mdl, sum)

 return
 end subroutine area_average

 subroutine nearest_neighbor(gfld_firedat, fire_mdl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nearest_neighbor
!   prgmmr: gayno          org: w/np2     date: 2015-jan-30
!
! abstract: this subroutine interpolates burned area data
!   to the model grid using a nearest neighbor method.
!
! program history log:
! 2015-jan-30  gayno    - initial version
!
! usage: call nearest_neighbor(gfld_firedat, fire_mdl)
!
!   input argument list:  
!     gfld_firedat - input nesdis burned area data (ncep
!                    g2 library gribfield data structure)
!
!   output argument list:
!     fire_mdl     - burned area data on the model grid.
!
! files: none
!
! condition codes:
!    45 - grid error in routine lambconf.  model grid
!         likely outside the input burned area grid.
!
! remarks: none.
!
!$$$
!
 use grib_mod

 use model_grid, only : lats_mdl, lons_mdl, ijmdl, &
                        ipts_mdl, jpts_mdl, imdl, jmdl

 implicit none

 type(gribfield), intent(in)   :: gfld_firedat

 real,            intent(out)  :: fire_mdl(imdl,jmdl)

 integer                       :: twod_2_oned
 integer                       :: nearest_i, nearest_j
 integer                       :: ijfire, nret, iopt
 integer                       :: nearest_ij
 integer                       :: ifire, jfire
 integer                       :: count, ij, mspiral
 integer                       :: ixs, jxs, mx, kxs
 integer                       :: kxt, ix, jx

 real                          :: fill
 real, allocatable             :: xpts(:), ypts(:)

 print*,'- BEGIN NEAREST NEIGHBOR INTERPOLATION'

!-----------------------------------------------------------------------
! dimensions of the input nesdis fire data grid.
!-----------------------------------------------------------------------

 ifire = gfld_firedat%igdtmpl(8)
 jfire = gfld_firedat%igdtmpl(9)

!-----------------------------------------------------------------------
! at each model grid point, find the nearest i/j on the nesdis grid.
!-----------------------------------------------------------------------

 allocate (xpts(ijmdl),ypts(ijmdl))
 iopt=-1
 fill=-9999.
 call lambconf(gfld_firedat%igdtmpl,iopt,ijmdl,fill, &
               xpts,ypts,lons_mdl,lats_mdl,nret)

 if (nret /= ijmdl) then
   print*,'- FATAL ERROR IN ROUTINE LAMBCONF.'
   print*,'- NUMBER OF PTS RETURNED WRONG: NRET/IJMDL: ',nret, ijmdl
   call w3tage('FIRE2MDL')
   call errexit(45)
 endif

!-----------------------------------------------------------------------
! perform nearest neighbor interpolation.  if there is no input
! data at the model point, do a spiral search to find valid input
! data.  since fire data is not continuous, only search a short 
! distance from model point.  if search finds no data, then 
! model point set to zero percent burned.
!-----------------------------------------------------------------------

 count=0
 mspiral=2

 do ij = 1, ijmdl
   nearest_i=nint(xpts(ij))
   if (nearest_i < 1 .or. nearest_i > ifire) cycle
   nearest_j=nint(ypts(ij))
   if (nearest_j < 1 .or. nearest_j > jfire) cycle
   ijfire=twod_2_oned(nearest_i,nearest_j,ifire)
   if ( gfld_firedat%bmap(ijfire) ) then
     fire_mdl(ipts_mdl(ij),jpts_mdl(ij)) = gfld_firedat%fld(ijfire)
   else
     count = count + 1
     IXS=SIGN(1.,XPTS(ij)-nearest_i)
     JXS=SIGN(1.,YPTS(ij)-nearest_j)
     SPIRAL : DO MX=2,MSPIRAL**2
       KXS=SQRT(4*MX-2.5)
       KXT=MX-(KXS**2/4+1)
       SELECT CASE(MOD(KXS,4))
       CASE(1)
         IX=nearest_i-IXS*(KXS/4-KXT)
         JX=nearest_j-JXS*KXS/4
       CASE(2)
         IX=nearest_i+IXS*(1+KXS/4)
         JX=nearest_j-JXS*(KXS/4-KXT)
       CASE(3)
         IX=nearest_i+IXS*(1+KXS/4-KXT)
         JX=nearest_j+JXS*(1+KXS/4)
       CASE DEFAULT
         IX=nearest_i-IXS*KXS/4
         JX=nearest_j+JXS*(KXS/4-KXT)
       END SELECT
       if (ix < 1 .or. ix > ifire) cycle SPIRAL
       if (jx < 1 .or. jx > jfire) cycle SPIRAL
       ijfire=twod_2_oned(ix,jx,ifire)
       if ( gfld_firedat%bmap(ijfire) ) then
         fire_mdl(ipts_mdl(ij),jpts_mdl(ij)) = gfld_firedat%fld(ijfire)
         exit SPIRAL
       endif
     enddo SPIRAL
   endif
 enddo

 deallocate(xpts, ypts)

 print*,'- SURROUNDING POINTS SEARCHED FOR VALID DATA AT ', count, ' MODEL POINTS.'

 return

 end subroutine nearest_neighbor

 subroutine gribit(fire_mdl, use_1km, use_12km)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gribit
!   prgmmr: gayno          org: w/np2     date: 2015-jan-30
!
! abstract: this subroutine writes the model burned
!   area data to a grib 2 message.
!
! program history log:
! 2015-jan-30  gayno    - initial version
!
! usage: call gribit(fire_mdl, use_1km, use_12km)
!
!   input argument list:  
!     fire_mdl     - burned area data on the model grid.
!     use_1km      - when true, 1-km input burned area data
!                    was used
!     use_12km     - when true, 12-km input burned area data
!                    was used
!
!   output argument list: n/a
!
! files:
!   input: none
!
!   output:
!     - burned area data on model grid (grib 2)
!
! condition codes:
!    42 - bad open of burned area file
!    43 - bad write of burned area data
!
! remarks: none.
!
!$$$
!
 use grib_mod

 use program_setup, only    : model_fire_file

 use firedat, only          : valid_time_1km, valid_time_12km

 use model_grid, only       : imdl, jmdl, kgds_mdl, lsmask_mdl, &
                              lat11, lon11, latlast, lonlast

 implicit none

 logical, intent(in) :: use_1km, use_12km

 real, intent(in)    :: fire_mdl(imdl,jmdl)

 integer, external   :: iw3jdn, twod_2_oned
 integer             :: i, j, julhr1, julhr12, iret, lugb
 integer             :: accum_period1, accum_period12

 real                :: tlm0d

 type(gribfield)     :: gfld

 call grib2_null(gfld)

! **** section 0 ****

 gfld%version = 2
 gfld%discipline = 2    ! land surface products

! **** section 1 ****

 gfld%idsectlen=13
 allocate(gfld%idsect(gfld%idsectlen))
 gfld%idsect(1)=7  ! orig center
 gfld%idsect(2)=4  ! sub center
 gfld%idsect(3)=9  ! master table ver #
 gfld%idsect(4)=0  ! local table
 gfld%idsect(5)=0  ! signif of ref time

!-----------------------------------------------------------------------
! now determine reference time.  when both 1 and 12-km input data
! were used, pick the latest date and the longest accumulation period.
!-----------------------------------------------------------------------

 julhr1 = 0
 accum_period1=0
 if (use_1km) then
   julhr1 = iw3jdn(valid_time_1km%year_4, valid_time_1km%month, valid_time_1km%day)*24 + &
            valid_time_1km%hour
   accum_period1 = valid_time_1km%accum_period
 endif

 julhr12 = 0
 accum_period12=0
 if (use_12km) then
   julhr12 = iw3jdn(valid_time_12km%year_4,valid_time_12km%month,valid_time_12km%day)*24 + &
             valid_time_12km%hour
   accum_period12 = valid_time_12km%accum_period
 endif

 if (julhr1 > julhr12) then
   gfld%idsect(6)  = valid_time_1km%year_4
   gfld%idsect(7)  = valid_time_1km%month
   gfld%idsect(8)  = valid_time_1km%day
   gfld%idsect(9)  = valid_time_1km%hour
 else
   gfld%idsect(6)  = valid_time_12km%year_4
   gfld%idsect(7)  = valid_time_12km%month
   gfld%idsect(8)  = valid_time_12km%day
   gfld%idsect(9)  = valid_time_12km%hour
 endif

 gfld%idsect(10:11)=0
 gfld%idsect(12)=0  ! production status
 gfld%idsect(13)=0  ! type of processed data

! **** section 2 **** not used
 gfld%locallen=0

! **** section 3 ****

 gfld%griddef = 0      ! oct 6;  source of grid definition, specified in table 3.1
 gfld%ngrdpts = imdl*jmdl ! octs 7-10; number of grid points
 gfld%numoct_opt = 0   ! oct 11; number of octs for optional list of numbers defining # of grid points
                       ! oct 12; list of #s defining # of points
 gfld%igdtnum=1        ! octs 13-14; grid definition template number, table 3.1
 gfld%igdtlen=22
 allocate(gfld%igdtmpl(gfld%igdtlen))
 gfld%igdtmpl(1)=0          ! oct 15; shape of the earth, spherical
 gfld%igdtmpl(2)=255        ! oct 16; scale factor of radius of spherical earth, not used.
 gfld%igdtmpl(3)=-1         ! octs 17-20; scale value of radius of spherical earth, not used.
 gfld%igdtmpl(4)=255        ! oct 21; scale factor of major axis of elliptical earth, not used.
 gfld%igdtmpl(5)=-1         ! octs 22-25; scaled value of major axis of elliptical earth, not used.
 gfld%igdtmpl(6)=255        ! oct 26; scale factor of minor axis of elliptical earth, not used.
 gfld%igdtmpl(7)=-1         ! octs 27-30; scaled value of minor axis of elliptical earth, not used.
 gfld%igdtmpl(8)=imdl       ! octs 31-34; # "i" points
 gfld%igdtmpl(9)=jmdl       ! octs 35-38; # "j" points
 gfld%igdtmpl(10)=0         ! octs 39-42; basic angle of domain, set to 0 when using standard 1e6
 gfld%igdtmpl(11)=-1        ! octs 43-46; basic angle of domain, set to missing when using 1e6
 gfld%igdtmpl(12)=nint(lat11*1.e6)  ! octs 47-50; lat of first grid point
 if (lon11 < 0.) then
   gfld%igdtmpl(13)=nint((lon11+360.)*1.e6)  ! octs 51-54; lon of first grid point
 else
   gfld%igdtmpl(13)=nint(lon11*1.e6)  ! octs 51-54; lon of first grid point
 endif
 gfld%igdtmpl(14)=56        ! oct 55;  resolution and component flags
 gfld%igdtmpl(15)=nint(latlast*1.e6)  ! octs 56-59; lat of last grid point
 if (lonlast < 0.) then
   gfld%igdtmpl(16)=nint((lonlast+360.)*1.e6)  ! octs 60-63; lon of last grid point
 else
   gfld%igdtmpl(16)=nint(lonlast*1.e6)  ! octs 60-63; lon of last grid point
 endif
 gfld%igdtmpl(17)=kgds_mdl(9)*1000    ! octs 64-67; "i" increment
 gfld%igdtmpl(18)=kgds_mdl(10)*1000   ! octs 68-71; "j" increment
 gfld%igdtmpl(19)=64        ! oct 72; scanning mode flag
 gfld%igdtmpl(20)= kgds_mdl(7)*1000 - 90000000 ! octs 73-76; latitude of south pole of projection
 tlm0d = kgds_mdl(8) / 1000.0
 if (tlm0d < 0.) tlm0d = tlm0d + 360.
 gfld%igdtmpl(21)=nint(tlm0d*1e6) ! octs 77-80; longitude of south pole of projection
 gfld%igdtmpl(22)=0         ! octs 81-84; angle of rotation of projection
 gfld%num_opt = 0           ! octs 85-??; number of optional grid points.
 gfld%numoct_opt=0  ! # octets needed for non-regular grids

! **** section 4 ****

 gfld%num_coord=0           ! octs 6-7; number of coordinate values after template.
 gfld%ipdtnum=8             ! octs 8-9; product definition template number - table 4.8
 gfld%ipdtlen=29
 allocate(gfld%ipdtmpl(gfld%ipdtlen))
 gfld%ipdtmpl(1)= 4         ! oct 10; parameter category, fire wx products
 gfld%ipdtmpl(2)= 3         ! oct 11; parameter, fire burned area in percent
 gfld%ipdtmpl(3)= 0         ! oct 12; type of generating process
 gfld%ipdtmpl(4)= 255       ! oct 13; background generating process identifier
 gfld%ipdtmpl(5)= 84        ! oct 14; analysis generating process identifier
 gfld%ipdtmpl(6)= 0         ! octs 15-16; hours after ob cutoff
 gfld%ipdtmpl(7)= 0         ! oct 17; minutes after ob cutoff
 gfld%ipdtmpl(8)= 1         ! oct 18; unit of time range
 gfld%ipdtmpl(9)= 0         ! octs 19-22; forecast time in units defined by oct 18
 gfld%ipdtmpl(10)=1         ! oct 23; type of first fixed surface
 gfld%ipdtmpl(11)=0         ! oct 24; scale factor of first fixed surface
 gfld%ipdtmpl(12)=0         ! octs 25-28; scale value of first fixed surface
 gfld%ipdtmpl(13)=255       ! oct 29; type of second fixed surface, not used
 gfld%ipdtmpl(14)=255       ! oct 30; scale factor of second fixed surface, not used
 gfld%ipdtmpl(15)=-(2**31-1) ! octs 31-34; scaled value of second fixed surface, not used
 gfld%ipdtmpl(16)=gfld%idsect(6)  ! oct 35-36; year of end of overall time interval
 gfld%ipdtmpl(17)=gfld%idsect(7)  ! oct 37; mon of end of overall time interval
 gfld%ipdtmpl(18)=gfld%idsect(8)  ! oct 38; day of end of overall time interval
 gfld%ipdtmpl(19)=gfld%idsect(9)  ! oct 39; hour of end of overall time interval
 gfld%ipdtmpl(20)=0        ! oct 40; minute of end of overall time interval
 gfld%ipdtmpl(21)=0        ! oct 41; second of end of overall time interval
 gfld%ipdtmpl(22)=1        ! oct 42; # of time ranges
 gfld%ipdtmpl(23)=0        ! oct 43-46; # data values missing
 gfld%ipdtmpl(24)=1        ! oct 47; stat process used (accum)
 gfld%ipdtmpl(25)=2        ! oct 48; type of time intervals
 gfld%ipdtmpl(26)=1        ! oct 49; indicator of unit of time range - hours
 gfld%ipdtmpl(27)=max(accum_period1,accum_period12) ! oct 50-53; accumuation period
 gfld%ipdtmpl(28)=255
 gfld%ipdtmpl(29)=0

! **** section 5 ****

 gfld%idrtnum=0    ! data representation template number - table 5.0 simple packing
 gfld%idrtlen=5
 allocate (gfld%idrtmpl(gfld%idrtlen))
 gfld%idrtmpl=0
 gfld%idrtmpl(3)=0  ! decimal scaling factor

! **** section 6 ****

 gfld%ibmap=0   ! use bitmap
 allocate(gfld%bmap(gfld%ngrdpts))

 gfld%bmap=.false.
 do j = 1, jmdl
 do i = 1, imdl
   if (lsmask_mdl(i,j) > 0.0) then
     gfld%bmap(twod_2_oned(i,j,imdl))=.true.
   endif
 enddo
 enddo

! **** section 7 ****

 allocate(gfld%fld(gfld%ngrdpts))
 gfld%fld = reshape ( fire_mdl, (/gfld%ngrdpts/) )

 print*,'- OUTPUT MODEL FIRE DATA IN GRIB2 FORMAT.'
 lugb=60
 print*,'- OPEN FILE: ', trim(model_fire_file)
 call baopenw(lugb,model_fire_file,iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR, BAD OPEN. IRET: ', iret
   call w3tage('FIRE2MDL')
   call errexit(42)
 endif

 print*,'- GRIB DATA.'
 call putgb2(lugb,gfld,iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR, CALL TO PUTGB2 FAILED. IRET: ', iret
   call w3tage('FIRE2MDL')
   call errexit(43)
 endif

 call baclose(lugb,iret)

 call gf_free2(gfld)

 return

 end subroutine gribit

 function twod_2_oned(i,j,idim)

 integer :: twod_2_oned

 twod_2_oned = (j-1)*idim + i

 end function twod_2_oned
