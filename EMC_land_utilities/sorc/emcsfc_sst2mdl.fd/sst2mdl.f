 module sst2mdl
!$$$  module documentation block
!
! $Revision$
!
! module:    sst2mdl
!   prgmmr: gayno         org: w/np2     date: 2005-05-20
!
! abstract: interpolate sst data to model grid and grib the result
!
! program history log:
!   2005-05-20  gayno   - initial version
!   2007-10-15  gayno   - modified for nam b-grids. improved
!                         thinning of gfs grids
!   2014-10-28  gayno   - option to output model sst analysis
!                         in grib2 format.
!   2015-05-27  gayno   - option to use a cressman blend of global
!                         mmab and flake climatological data at
!                         undefined model points.
!
! usage: use sst2mdl
!
! remarks: some variable definitions
!   sst_mdl  - sst on model grid in K
!
!$$$

 use program_setup,        only   : output_file, &
                                    output_grib2, &
                                    climo_4_lakes

 use model_grid,           only   : resol_mdl,   &
                                    imdl,        &
                                    jmdl,        &
                                    ijmdl,       &
                                    lsmask_mdl,  &
                                    lsmask_mdl_sav,  &
                                    lats_mdl,    &
                                    lons_mdl,    &
                                    kgds_mdl,    &
                                    lonsperlat,  &
                                    thinned,     &
                                    ipts_mdl,    &
                                    jpts_mdl

 use sstdat,               only   : bitmap_src, bitmap_flake,  &
                                    resol_src,   &
                                    sst_src, sst_flake,    &
                                    sst_14km_src,&
                                    isrc,        &
                                    isrc_14km,   &
                                    jsrc,        &
                                    jsrc_14km,   &
                                    iflake, jflake, &
                                    kgds_src, kgds_flake,    &
                                    century,     &
                                    day,         &
                                    hour,        &
                                    year,        &
                                    month   

 use read_write_utils,     only   : uninterpred

 private

 public                          :: interp

 real, allocatable, private      :: sst_mdl_1d(:)
 real, allocatable, private      :: sst_mdl(:,:)  ! sst on model grid
 
 contains

 subroutine interp
!$$$  subprogram documentation block
!
! subprogram:   interp
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  interpolate sst data to model grid.
!
! program history log:
! 2005-05-20  gayno   - initial version
! 2007-10-15  gayno   - modified for nam b-grids. improved
!                       thinning of gfs grids.
! 2014-10-28  gayno   - option to output model sst analysis
!                       in grib2.
! 2015-05-27  gayno   - option to use a cressman blend of
!                       global mmab and flake data at undefined
!                       model points.
!
! usage: call interp
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files:
!    inputs: none
!
!    outputs: none
!
! condition codes: all fatal
!    54 - error in ipolates during interpolation of flake data.
!    55 - error in ipolates during interpolation of global mmab data.
!
! remarks:  
!   The model analysis is created as follows:
!    1) global mmab sst data is interpolated to model grid.
!    2) if there is no valid global data available for a
!       model grid point (i.e., a small isolated lake):
!       a) if flake data is chosen, a cressman analysis is
!          is performed at the model point.  the analysis
!          considers surrounding model points resolved by the
!          global mmab data.  The flake value is also used
!          with a weight of one.   At very isolated model 
!          lakes, the result of the cressman is the pure
!          flake value.  
!       b) if flake data is NOT chose, the model point is set 
!          to a nominal value.
!    3) if GLERL north american data is chosen, the
!       model points are set to the GLERL value in the
!       Great Lakes Region.
!    4) if the lake climatology option is chosen, the
!       great salt lake, salton sea, lake champlain and
!       fort peck res model points are set to climo values.
!
!$$$

 use omp_lib

 implicit none

 integer                   :: i, j, ii, jj, ij, int_opt, ipopt(20)
 integer                   :: count, ijmdl2, istart, iend, imid, iii
 integer, allocatable      :: idum(:,:), ipts_flake_mdl(:), jpts_flake_mdl(:)
 integer                   :: kgds_mdl_tmp(200), influence_r_pts
 integer                   :: alpha, i_diff, j_diff, tid
 integer                   :: no, ibo, iret, nret, missing, m
 
 logical*1, allocatable    :: bitmap_mdl(:), bitmap_flake_mdl(:)

 real, allocatable         :: lsmask_1d(:)
 real, allocatable         :: sst_flake_mdl(:), lats_flake_mdl(:), &
                              lons_flake_mdl(:)
 real                      :: sumc, x1, r, fraction, gridis, gridie
 real                      :: dum, gcdist, influence_r, weight
 real, allocatable         :: weight_sum(:), weighted_sst_sum(:)

!----------------------------------------------------------------------
! determine type of interpolation based on resolutions of
! the model and data grids. 
!----------------------------------------------------------------------

 print*,''
 print*,"- INTERPOLATE GLOBAL SST DATA TO MODEL GRID"

 ipopt = 0

 if (resol_src <= (0.5*resol_mdl)) then
   print*,"- INTERPOLATE USING BUDGET METHOD."
   ipopt(1)=-1  ! break model grid cell into 25 points.
   ipopt(2)=-1  ! 25 points are weighted equally.
   ipopt(20) = nint(0.5 / resol_src) + 1   ! search box width of 1/2 deg.
   kgds_mdl_tmp = kgds_mdl
   kgds_mdl_tmp(1) = kgds_mdl_tmp(1) - 255 ! subset of grid
   int_opt = 3
   no = ijmdl
 elseif (resol_src <= (1.5*resol_mdl)) then
   print*,"- INTERPOLATE USING NEIGHBOR METHOD."
   ipopt(1) = nint(0.5 / resol_src) + 1   ! search box width of 0.5 deg.
   kgds_mdl_tmp = kgds_mdl
   kgds_mdl_tmp(1) = -1  ! for subsection of model grid.
   int_opt = 2
   no = ijmdl  ! an input when kgds(1) < 0 (subset of grid)
 else
   print*,"- INTERPOLATE USING BILINEAR METHOD."
   kgds_mdl_tmp = kgds_mdl
   kgds_mdl_tmp(1) = -1  ! for subsection of model grid.
   int_opt = 0
   ipopt(2) = nint(0.5 / resol_src) + 1   ! search box width of 1/2 deg.
   no = ijmdl  ! an input when kgds(1) < 0 (subset of grid)
 end if

!----------------------------------------------------------------------
! call interpolation routine
!----------------------------------------------------------------------

 allocate (bitmap_mdl(ijmdl))
 bitmap_mdl=.false.  ! if interpolation routine can't find data
                     ! at a point, this flag is false.

 allocate (sst_mdl_1d(ijmdl))
 sst_mdl_1d = 0.0

 call ipolates(int_opt, ipopt, kgds_src, kgds_mdl_tmp,   &
              (isrc*jsrc), ijmdl,               &
               1, 1, bitmap_src, sst_src,  &
               no, lats_mdl, lons_mdl, ibo, bitmap_mdl,     &
               sst_mdl_1d, iret)

 if (iret /= 0) then
   print*,"- FATAL ERROR INTERPOLATING GLOBAL SST DATA. IRET IS: ", iret
   call w3tage('SST2MDL')
   call errexit(55)
 endif

!----------------------------------------------------------------------
! The marine branch places a global sst value at all points, even land.
! Therefore, the ipolates routine should always find data for all
! model water points.  However, if you choose to screen out the land point
! sst values by using the associated landmask, there is a possibility that
! there will not be global sst data near every model water point (for
! example small lakes).  These undefined model points are handled
! as follows:
!
! 1) If flake climo data is NOT chosen, then these undefined model
!    points are set to a nominal SST value. 
! 2) If flake climo data IS chosen, then a cressman analysis is 
!    performed at these undefined model points.  The analysis considers
!    surrounding model points with SSTs defined by the global marine
!    branch data.  The flake value at the undefined model point
!    is also considered with a weight of one.  At very isolated
!    and undefined model points, the result of the cressman is
!    the pure flake value.  Elsewhere, the result is a blend of
!    the flake value and surrounding global sst values.
!----------------------------------------------------------------------

 missing=0
 do ij=1,ijmdl
   if(.not.bitmap_mdl(ij))then
     print*,'- GLOBAL SST DATA NOT FOUND AT MODEL POINT: ', ipts_mdl(ij),jpts_mdl(ij)
     missing=missing+1
   endif
 enddo

 if (missing > 0) then

   if (.not.allocated(sst_flake))then  ! flake data not chosen
     print*,'- SET SST AT MISSING POINTS TO NOMINAL VALUE.'
     do ij=1,ijmdl
       if(.not.bitmap_mdl(ij))then
          sst_mdl_1d(ij)=280.0
       endif
     enddo
   else
     print*,'- SET SST AT MISSING POINTS TO CRESSMAN BLEND OF FLAKE CLIMO AND GLOBAL SST.' 
     allocate(ipts_flake_mdl(missing))
     allocate(jpts_flake_mdl(missing))
     allocate(lats_flake_mdl(missing))
     allocate(lons_flake_mdl(missing))
     allocate(bitmap_flake_mdl(missing))
     bitmap_flake_mdl=.false.
     allocate(sst_flake_mdl(missing))
     sst_flake_mdl=0.0
     count=0
     do ij=1,ijmdl
       if (.not.bitmap_mdl(ij))then
         count=count+1
         lats_flake_mdl(count) = lats_mdl(ij)
         lons_flake_mdl(count) = lons_mdl(ij)
         ipts_flake_mdl(count) = ipts_mdl(ij)
         jpts_flake_mdl(count) = jpts_mdl(ij)
       endif
     enddo

!----------------------------------------------------------------------
! Cressman blend at undefined model points.   First, consider 
! surrounding model points set with global SST data.
!----------------------------------------------------------------------

     influence_r=200.  ! radius of influence in km.
     influence_r_pts=ceiling((2.0*influence_r)/(resol_mdl*111.0))
                       ! radius of influence in grid points.
     alpha=3           ! controls how quickly weight decreases with distance.

     allocate(weight_sum(missing))
     weight_sum=0.0
     allocate(weighted_sst_sum(missing))
     weighted_sst_sum = 0.0

!$omp parallel do private(m,tid,ij, i_diff, j_diff, gcdist, weight,dum)
     do m = 1, missing  ! loop over flake pts
       tid=omp_get_thread_num()
       do ij = 1, ijmdl   ! loop over all non-land pts
         if (bitmap_mdl(ij)) then  ! global sst data found at this pt.
           i_diff=abs(ipts_mdl(ij)-ipts_flake_mdl(m))
           j_diff=abs(jpts_mdl(ij)-jpts_flake_mdl(m))
           if (i_diff < influence_r_pts .and. j_diff < influence_r_pts) then
             call w3fb10(lats_mdl(ij),lons_mdl(ij),lats_flake_mdl(m), &
                         lons_flake_mdl(m), dum, gcdist)
             if (gcdist <= influence_r) then
               weight = (influence_r - gcdist) / influence_r
               weight = weight**alpha
               weight_sum(m) = weight_sum(m) + weight
               weighted_sst_sum(m) = (weight*sst_mdl_1d(ij)) + weighted_sst_sum(m)
             endif
           endif
         endif
       enddo
     enddo
!$omp end parallel do

!----------------------------------------------------------------------
! Call ipolates again to interpolate flake data to remaining
! model water points with no sst value.  Since flake will typically
! be used for hires grids, use bilinear interpolation.
!----------------------------------------------------------------------

     kgds_mdl_tmp = kgds_mdl
     kgds_mdl_tmp(1) = -1  ! for subsection of model grid.
     int_opt = 0
     ipopt=0
     ipopt(2) = nint(2.0 / 0.033) + 1   ! search box width of 2 deg.
                                        ! flake data about 0.2 degree.
     no = missing  ! an input when kgds(1) < 0 (subset of grid)

     call ipolates(int_opt, ipopt, kgds_flake, kgds_mdl_tmp,   &
                   (iflake*jflake), missing,               &
                   1, 1, bitmap_flake, sst_flake,  &
                   no, lats_flake_mdl, lons_flake_mdl, ibo, bitmap_flake_mdl,     &
                   sst_flake_mdl, iret)

     if (iret /= 0) then
       print*,"- FATAL ERROR INTERPOLATING FLAKE DATA. IRET IS: ", iret
       call w3tage('SST2MDL')
       call errexit(54)
     endif

     count=0
     do ij = 1, ijmdl
       if (.not.bitmap_mdl(ij))then  ! no global sst data at model grid point
         count=count+1
         if (.not.bitmap_flake_mdl(count)) then  ! no flake data at model grid either.
           print*,'- WARNING: NO GLOBAL SST OR FLAKE DATA AT MODEL POINT: ',ipts_mdl(ij),jpts_mdl(ij)
           print*,'- SET MODEL SST TO NOMINAL VALUE.'
           sst_mdl_1d(ij)=280.0
         else  ! Set to cressman blend. Add flake contribution with weight of one.
           weight_sum(count)=weight_sum(count)+1.0
           weighted_sst_sum(count)=weighted_sst_sum(count)+sst_flake_mdl(count)
           sst_mdl_1d(ij)=weighted_sst_sum(count)/weight_sum(count)
         endif
       endif
     enddo

     deallocate(lats_flake_mdl, lons_flake_mdl, bitmap_flake_mdl, sst_flake_mdl)
     deallocate(bitmap_flake, sst_flake)

   endif  ! was flake data chosen by user?
 endif  ! were there any undefined model grid points?

 deallocate (bitmap_mdl)
 deallocate (sst_src, bitmap_src)

!----------------------------------------------------------------------
! if user selects, overlay 14 km sst data for the Great Lakes region.
!----------------------------------------------------------------------

 if (allocated (sst_14km_src)) then
  print*,"- OVERLAY 14KM SST DATA IN GREAT LAKES REGION."
  call sst14km
  deallocate (sst_14km_src)
 end if

!----------------------------------------------------------------------
! if the user desires, set sst to a climo value for the larger
! conus lakes.
!----------------------------------------------------------------------

 if (climo_4_lakes) then
   print*,"- USE CLIMO FOR THE GREAT SALT LAKE."
   call salt_lake
   print*,"- USE CLIMO FOR THE SALTON SEA."
   call salton_sea
   print*,"- USE CLIMO FOR THE FORT PECK RESERVOIR."
   call fort_peck
   print*,"- USE CLIMO FOR LAKE CHAMPLAIN."
   call champlain
 end if

!----------------------------------------------------------------------
! put back in 2-d array for gribbing.
!----------------------------------------------------------------------

 allocate(sst_mdl(imdl,jmdl))
 sst_mdl=0.0
 do ij = 1, ijmdl
   sst_mdl(ipts_mdl(ij),jpts_mdl(ij)) = sst_mdl_1d(ij)
 enddo

 deallocate(sst_mdl_1d)

!----------------------------------------------------------------------
! if a global model grid, and if running on thinned grid, then
! take a linear weighting of full points located within the thin points.
!----------------------------------------------------------------------

 if (kgds_mdl(1) == 4 .and. thinned) then

   ijmdl2 = sum(lonsperlat) * 2
   allocate (lsmask_1d(ijmdl2))
   allocate (sst_mdl_1d(ijmdl2))

   lsmask_1d  = 0.0
   sst_mdl_1d = 0.0

   ij = 0
   do j = 1, jmdl
     jj = j
     if (jj > jmdl/2) jj = jmdl - j + 1
     r = float(imdl) / float(lonsperlat(jj))
     do i = 1, lonsperlat(jj)
       ij = ij + 1
       x1 = (i-1)*r
       imid = nint(x1+1.0)
       lsmask_1d(ij) = lsmask_mdl_sav(imid,j)
       if (lsmask_mdl_sav(imid,j) > 0.0) cycle
       gridis=x1+1.0-r/2.
       istart=nint(gridis)
       gridie=x1+1.0+r/2.
       iend=nint(gridie)
       sumc = 0.0   
       do ii = istart, iend
         if (ii == istart) then
           fraction = 0.5 - (gridis - float(istart))
         elseif (ii == iend) then
           fraction = 0.5 + (gridie - float(iend))
         else
           fraction = 1.0
         endif
         if (fraction < 0.0001) cycle
         iii = ii
         if (iii < 1) iii = imdl + iii
         sumc = sumc + fraction * sst_mdl(iii,j)
       enddo
       sst_mdl_1d(ij) = sumc / r
    enddo
   enddo
   deallocate (lsmask_mdl_sav)

!----------------------------------------------------------------------
! now place thinned points into 2-d array for output.
!----------------------------------------------------------------------

   allocate (idum(imdl,jmdl))
   idum = 0
   call uninterpred(1, idum, lsmask_1d, lsmask_mdl, imdl, jmdl, ijmdl2, lonsperlat)
   deallocate(lsmask_1d)
   call uninterpred(1, idum, sst_mdl_1d, sst_mdl, imdl, jmdl, ijmdl2, lonsperlat)
   deallocate(sst_mdl_1d)
   deallocate(idum)

 end if

!----------------------------------------------------------------------
! grib the interpolated data.
!----------------------------------------------------------------------
 
 if (output_grib2) then
   print*,''
   print*,'- OUTPUT FINAL SST ANALYSIS IN GRIB 2 FORMAT.'
   call write_grib2
 else
   print*,''
   print*,'- OUTPUT FINAL SST ANALYSIS IN GRIB 1 FORMAT.'
   call write_grib1
 endif

 deallocate (sst_mdl)
 deallocate (lsmask_mdl)
 
 return

 end subroutine interp

 subroutine sst14km
!$$$  subprogram documentation block
!
! subprogram:   sst14km
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  interpolate GLERL 14km (1/8 degree) sst data 
!   to model grid.
!
! program history log:
! 2005-05-20  gayno    - initial version
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
! remarks:  41N,94W is the lat/lon of the SW corner point of the
!   subset of the 14 km SST grid.
!
!$$$

 implicit none

 integer              :: ij
 integer              :: inieb
 integer              :: jnieb

 real                 :: lon

 real, parameter      :: dx_src_14km = 1.0/8.0
 real, parameter      :: dy_src_14km = 1.0/8.0

!----------------------------------------------------------------------
! use nearest neighbor approach to overlay the 14 km data.
!----------------------------------------------------------------------

 do ij = 1, ijmdl

   jnieb = int(((lats_mdl(ij)-41.0)/dy_src_14km)) + 1
    
   if (lons_mdl(ij) < 0.0) then
     lon = lons_mdl(ij) + 360.0
   else
     lon = lons_mdl(ij)
   endif

   inieb = int(((lon-266.0)/dx_src_14km)) + 1

   if ( (inieb >= 1) .and. (jnieb >= 1) ) then
     if( (inieb <= isrc_14km) .and. (jnieb <= jsrc_14km) ) then
       sst_mdl_1d(ij) = sst_14km_src(inieb,jnieb)
     endif
   endif

 enddo

 return

 end subroutine sst14km

 subroutine fort_peck
!$$$  subprogram documentation block
!
! subprogram:   fort_peck
!   prgmmr: gayno          org: w/np2     date: 2006-08-04
!
! abstract:  set sst for fort peck reservior to a climo value.
!
! program history log:
! 2006-08-04  gayno    - initial version
!
! usage: call fort_peck
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files: none
!
! condition codes: none
!
! remarks:  climatology provided by WFO Glasgow.
!
!$$$

 implicit none

 integer                   :: ij
 integer                   :: julday

 real                      :: latbnd(2), lonbnd(2), lon
 real                      :: sst_climo  ! celsius 

 data latbnd/47.0,48.5/,lonbnd/-108.5,-106.0/

 call w3fs13(year,month,day,julday)

 if (julday <=93) then
   sst_climo =0.1
 elseif (julday <= 208) then
   sst_climo = float(julday)*.206994 - 19.3253
 elseif (julday <= 336) then
   sst_climo = float(julday)*(-.13612) + 52.24
 else
   sst_climo = float(julday)*(-.21965) + 80.47
 end if

 write(6,47) sst_climo, julday
 47 format(1x,"- CLIMO SST VALUE FOR FORT PECK: ",f6.2,"C FOR JULDAY: ",i5)

 do ij=1,ijmdl
   if(lats_mdl(ij).gt.latbnd(1).and.lats_mdl(ij).lt.latbnd(2))then
     if (lons_mdl(ij) > 180.0) then
       lon = lons_mdl(ij) - 360.0
     else
       lon = lons_mdl(ij)
     end if
     if(lon.gt.lonbnd(1).and.lon.lt.lonbnd(2))then
       sst_mdl_1d(ij) = sst_climo + 273.16 
     ENDIF
   ENDIF
 ENDDO

 return
 end subroutine fort_peck

 subroutine champlain
!$$$  subprogram documentation block
!
! subprogram:   champlain
!   prgmmr: rogers           org: w/np2     date: 2006-07-01
!
! abstract:  set sst for Lake Champlain to a climo value.
!
! program history log:
! 2006-07-01  rogers    - initial version
!
! usage: call champlain
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files: none
!
! condition codes: none
!
! remarks:  none.
!
!$$$
 implicit none

 integer                   :: days_per_month(12)
 integer                   :: ij
 integer                   :: iarg1, iarg2
 integer                   :: marg0
 integer                   :: mnth0, mnth1

 real                      :: denom
 real                      :: frac
 real                      :: numer
 real                      :: lon
 real                      :: latbnd(2)
 real                      :: climo_temp(12)
 real                      :: lonbnd(2)

 DATA DAYS_PER_MONTH/31,28,31,30,31,30,31,31,30,31,30,31/

 DATA climo_temp/ 1.7, 1.1, 1.7, 2.8, 7.8,15.0,   &
                 20.6,21.7,18.9,12.8, 8.3, 4.4/

 DATA latbnd/43.5,45.1/,lonbnd/-73.5,-73.0/

!-----------------------------------------------------------------------
! interpolate climo to the current date, then insert this temperature
! at grid points within the bounds of the lake.
!-----------------------------------------------------------------------

 MARG0=month-1
 IF(MARG0.LT.1)MARG0=12
 MNTH0=DAYS_PER_MONTH(MARG0)
 MNTH1=DAYS_PER_MONTH(month)
 IF(day.LT.15)THEN
   NUMER=day+MNTH0-15
   DENOM=MNTH0
   IARG1=MARG0
   IARG2=month
 ELSE
   NUMER=day-15
   DENOM=MNTH1
   IARG1=month
   IARG2=month+1
   IF(IARG2.GT.12)IARG2=1
 ENDIF
 FRAC=NUMER/DENOM

 write(6,48) (climo_temp(IARG1)+ (climo_temp(IARG2)-climo_temp(IARG1))*FRAC)
 48 format(1x,"- CLIMO SST VALUE FOR LAKE CHAMPLAIN (DEGREES C): ",f6.2)

 DO IJ=1,ijmdl
   IF(lats_mdl(IJ).GT.latbnd(1).AND.lats_mdl(IJ).LT.latbnd(2))THEN
     if (lons_mdl(ij) > 180.0) then
       lon = lons_mdl(ij) - 360.0
     else
       lon = lons_mdl(ij)
     end if
     IF(LON.GT.lonbnd(1).AND.LON.LT.lonbnd(2))THEN
       sst_mdl_1d(IJ)=climo_temp(IARG1)+     &
                     (climo_temp(IARG2)-climo_temp(IARG1))*FRAC
       sst_mdl_1d(IJ)=sst_mdl_1d(IJ)+273.16
     ENDIF
   ENDIF
 ENDDO

 return
 end subroutine champlain

 subroutine salton_sea
!$$$  subprogram documentation block
!
! subprogram:   salton_sea
!   prgmmr: roger            org: w/np2     date: 2006-07-01
!
! abstract:  set sst for the salton to a climo value.
!
! program history log:
! 2006-07-01  rogers    - initial version
!
! usage: call salton_sea
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files: none
!
! condition codes: none
!
! remarks:  none.
!
!$$$
 implicit none

 integer                   :: days_per_month(12)
 integer                   :: ij
 integer                   :: iarg1, iarg2
 integer                   :: marg0
 integer                   :: mnth0, mnth1

 real                      :: denom
 real                      :: frac
 real                      :: numer 
 real                      :: lon
 real                      :: latbnd(2)
 real                      :: climo_temp(12)
 real                      :: lonbnd(2)

 DATA DAYS_PER_MONTH/31,28,31,30,31,30,31,31,30,31,30,31/

 DATA climo_temp/12.8,12.8,17.2,21.1,24.4,26.7,     &
                 30.0,31.7,29.4,25.0,20.6,15.0/
 
 DATA latbnd/33.0,33.7/,lonbnd/-116.3,-115.3/

!-----------------------------------------------------------------------
! interpolate climo to the current date, then insert this temperature
! at grid points within the bounds of the lake.
!-----------------------------------------------------------------------
 
 MARG0=month-1
 IF(MARG0.LT.1)MARG0=12
 MNTH0=DAYS_PER_MONTH(MARG0)
 MNTH1=DAYS_PER_MONTH(month)
 IF(day.LT.15)THEN
   NUMER=day+MNTH0-15
   DENOM=MNTH0
   IARG1=MARG0
   IARG2=month
 ELSE
   NUMER=day-15
   DENOM=MNTH1
   IARG1=month
   IARG2=month+1
   IF(IARG2.GT.12)IARG2=1
 ENDIF
 FRAC=NUMER/DENOM

 write(6,48) (climo_temp(IARG1)+ (climo_temp(IARG2)-climo_temp(IARG1))*FRAC)
 48 format(1x,"- CLIMO SST VALUE FOR SALTON SEA (DEGREES C): ",f6.2)

 DO IJ=1,ijmdl
   IF(lats_mdl(IJ).GT.latbnd(1).AND.lats_mdl(IJ).LT.latbnd(2))THEN
     if (lons_mdl(ij) > 180.0) then
       lon = lons_mdl(ij) - 360.0
     else
       lon = lons_mdl(ij)
     end if
     IF(LON.GT.lonbnd(1).AND.LON.LT.lonbnd(2))THEN
       sst_mdl_1d(IJ)=climo_temp(IARG1)+     &
                     (climo_temp(IARG2)-climo_temp(IARG1))*FRAC
       sst_mdl_1d(IJ)=sst_mdl_1d(IJ)+273.16
     ENDIF
   ENDIF
 ENDDO

 return
 end subroutine salton_sea

 subroutine salt_lake
!$$$  subprogram documentation block
!
! subprogram:   salt_lake
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  set sst for great salt lake to a climo value.
!
! program history log:
! 2005-05-20  gayno    - initial version
! 2006-07-06  rogers   - changed code to use latest climatology from U. of Utah;
!                        a cosine fit to the bimonthly observational data 
!                        from Saltair Boat harbor (from 1972-1989).
!                        From Steenburgh et al., 2000: Climatology of 
!                        Lake-Effect Snowstorms of the Great Salt Lake. 
!                        Monthly Weather Review, 128, 709-727.
!
! usage: call salt_lake
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files: none
!
! condition codes: none
!
! remarks:  none.
!
!$$$

 implicit none

 integer                   :: ij
 integer                   :: julday

 real                      :: arg1
 real                      :: xjulday
 real                      :: slc_climo
 real                      :: lon
 real                      :: saltla(2)
 real                      :: saltlo(2)

!-----------------------------------------------------------------------
! CORNERS OF SALT LAKE LATITUDE/LONGITUDE BOX
! in degrees---> 40.0     42.0            111.0    114.0
!-----------------------------------------------------------------------
 
 DATA SALTLA/40.0,42.0/,SALTLO/-114.0,-111.0/

!-----------------------------------------------------------------------
! interpolate climo to the current date, then insert this temperature
! at grid points within the bounds of the lake.
!-----------------------------------------------------------------------
 
 call w3fs13(year,month,day,julday)
 xjulday = real(julday)
 arg1 = cos(2.0*3.141592654*xjulday/365.0 + 2.75321)
 slc_climo = 273.16+(12.4945+12.2859*arg1)

 write(6,49) slc_climo,julday
 49 format(1x,"- CLIMO SST VALUE FOR GREAT SALT LAKE: ",f6.2,"K FOR JULDAY: ",i5)

 DO IJ=1,ijmdl

   IF(lats_mdl(IJ).GT.SALTLA(1).AND.lats_mdl(IJ).LT.SALTLA(2))THEN

     if (lons_mdl(ij) > 180.0) then
       lon = lons_mdl(ij) - 360.0
     else
       lon = lons_mdl(ij)
     end if

     IF(LON.GT.SALTLO(1).AND.LON.LT.SALTLO(2))THEN

       sst_mdl_1d(IJ)=slc_climo

     ENDIF

   ENDIF

 ENDDO

 return

 end subroutine salt_lake

 subroutine write_grib1
!$$$  subprogram documentation block
!
! subprogram:   write_grib1
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  output sst data on the model grid in grib 1
!
! program history log:
! 2005-05-20  gayno    - initial version
! 2014-10-25  gayno    - rename as 'write_grib1'.  was
!                        'gribit'.
!
! usage: call write_grib1
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files:
!    input: none
!
!    output: none
!       - model sst data in grib 1 (fort.54)
!
! condition codes:  all fatal
!    59 - bad open of fort.54
!    58 - bad write of fort.54
!     
! remarks: none.
!
!$$$

 implicit none

 integer                    :: iret 
 integer, parameter         :: lugb = 54    ! unit number of output grib file
 integer                    :: kpds(200)

 logical*1                  :: lbms(imdl,jmdl)

!----------------------------------------------------------------------
! set up pds section.  
!
! don't need to set the gds section.  use the kgds array from
! module model_grid.   
!----------------------------------------------------------------------

 kpds = 0

 kpds(1)  = 7          ! center id
 kpds(2)  = 255        ! process id number. use missing
 kpds(3)  = 255        ! grid specified in gds
 kpds(4)  = 192        ! include gds and a bit map section  
 kpds(5)  = 11         ! parameter number for temperature
 kpds(6)  = 1          ! level - ground or water surface
 kpds(7)  = 0          ! height pressure of level
 kpds(8)  = year       ! year of century     the time info is determined   
 kpds(9)  = month      ! month               by the input file.
 kpds(10) = day        ! day
 kpds(11) = hour       ! hour
 kpds(12) = 0          ! minute
 kpds(13) = 1          ! fcst time unit - hour
 kpds(14) = 0          ! period of time, p1.  set to '0' for analysis
 kpds(15) = 0          ! number of time units, p2. 
 kpds(16) = 1          ! initialized analysis product
 kpds(17) = 0          ! number in average
 kpds(18) = 1          ! grib edition 1
 kpds(19) = 3          ! parameter table version number
 kpds(20) = 0          ! number missing from avg/accum
 kpds(21) = century    ! century - set as in the input file
 kpds(22) = 2          ! decimal scale factor
 kpds(23) = 4          ! subcenter - emc
 kpds(24) = 0          ! reserved
 kpds(25) = 0          ! reserved

 lbms=.false.
 where (lsmask_mdl == 0.0)
  lbms = .true.
 endwhere
  
 print*,"- OPEN OUTPUT GRIB FILE ", trim(output_file)
 call baopenw(lugb, output_file, iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR OPENING OUTPUT GRIB FILE. IRET IS ', iret
   call w3tage('SST2MDL')
   call errexit(59)
 end if

 print*,"- WRITE OUTPUT GRIB FILE ", trim(output_file)
 call putgb (lugb, (imdl*jmdl), kpds, kgds_mdl, lbms,  &
             sst_mdl, iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR WRITING OUTPUT GRIB FILE. IRET IS ', iret
   call w3tage('SST2MDL')
   call errexit(58)
 end if
 
 call baclose(lugb, iret)

 return

 end subroutine write_grib1

 subroutine write_grib2
!$$$  subprogram documentation block
!
! subprogram:   write_grib2
!   prgmmr: gayno          org: w/np2     date: 2014-oct-25
!
! abstract:  output sst on the model grid
!            in grib 2 format.
!
! program history log:
! 2014-oct-25  gayno    - initial version
!
! usage: call write_grib2
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files:
!    input: none
!
!    output:
!      - model sst in grib 2 (fort.53)
!
! condition codes:  all fatal
!   48 - bad write of fort.53
!   49 - bad open of fort.53
!   
! remarks: none.
!
!$$$

 use model_grid, only           : lat11, latlast, lon11, lonlast

 use grib_mod

 implicit none

 character(len=1), allocatable :: cgrib(:)

 integer, parameter            :: numcoord = 0
 integer, parameter            :: idrstmplen = 5

 integer                       :: coordlist(numcoord)
 integer                       :: lugb, lcgrib, iret
 integer                       :: igds(5)
 integer                       :: listsec0(2)
 integer                       :: listsec1(13)
 integer                       :: ideflist, idefnum, ipdsnum, idrsnum
 integer                       :: igdstmplen, ipdstmplen
 integer                       :: ipdstmpl(15), idrstmpl(idrstmplen)
 integer, allocatable          :: igdstmpl(:)
 integer                       :: ngrdpts, ibmap, lengrib

 logical*1, allocatable        :: bmap(:), bmap2d(:,:)

 real, allocatable             :: fld(:)

!----------------------------------------------------------------------
! Setup variables and arrays required by grib2 library.
!----------------------------------------------------------------------

 call grib2_check(kgds_mdl, igdstmplen)

 allocate(igdstmpl(igdstmplen))

 call init_grib2(century, year, month, day, hour, &
                 kgds_mdl, lat11, latlast, lon11, lonlast, &
                 listsec0, listsec1, igds, ipdstmpl, ipdsnum, igdstmpl,  &
                 igdstmplen, idrsnum, idrstmpl, idrstmplen, &
                 idefnum, ideflist, ngrdpts)

 lcgrib = imdl*jmdl*4
 allocate(cgrib(lcgrib))   ! this variable holds the grib2 message

 iret=0

!----------------------------------------------------------------------
! Create sections 0 and 1.  There is no section 2, local use section.
!----------------------------------------------------------------------

 print*,"- CREATE SECTIONS 0 AND 1"
 call gribcreate(cgrib,lcgrib,listsec0,listsec1,iret)
 if (iret /= 0) goto 900

!----------------------------------------------------------------------
! Create section 3, the grid description section.
!----------------------------------------------------------------------

 print*,"- CREATE SECTION 3"
 call addgrid(cgrib,lcgrib,igds,igdstmpl,igdstmplen,  &
              ideflist,idefnum,iret)
 if (iret /= 0) goto 900

!----------------------------------------------------------------------
! Create section 4 (product definition section) and 5 (data
! representation section).
!----------------------------------------------------------------------

 allocate(fld(ngrdpts))
 fld = reshape(sst_mdl, (/imdl*jmdl/) )

 ibmap = 0 ! bitmap applies
 allocate(bmap2d(imdl,jmdl))
 bmap2d=.true.
 where (lsmask_mdl > 0.5) bmap2d=.false.
 allocate(bmap(ngrdpts))
 bmap = reshape(bmap2d, (/imdl*jmdl/) )
 deallocate(bmap2d)

 print*,"- CREATE SECTIONS 4 AND 5 FOR SST"
 call addfield(cgrib,lcgrib,ipdsnum,ipdstmpl,ipdstmplen,  &
               coordlist,numcoord,idrsnum,idrstmpl,   &
               idrstmplen,fld,ngrdpts,ibmap,bmap,iret)
 if (iret /= 0) goto 900

!----------------------------------------------------------------------
! Create section 8 - end section.
!----------------------------------------------------------------------

 call gribend(cgrib,lcgrib,lengrib,iret)
 if (iret /= 0) goto 900

!----------------------------------------------------------------------
! Now output grib message to file.
!----------------------------------------------------------------------

 lugb=53
 print*,"- OPEN OUTPUT GRIB FILE ", trim(output_file)
 call baopenw(lugb, output_file, iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR OPENING OUTPUT GRIB FILE. IRET IS ', iret
   call w3tage('SST2MDL')
   call errexit(49)
 end if

 print*,'- WRITE OUTPUT GRIB FILE.'
 call wryte(lugb, lengrib, cgrib)

 call baclose (lugb, iret)

 deallocate(fld, bmap, igdstmpl, cgrib)

 return

 900 continue
 print*,'- FATAL ERROR CREATING GRIB2 MESSAGE. IRET IS ', iret
 call w3tage('SST2MDL')
 call errexit(48)

 end subroutine write_grib2

 end module sst2mdl
