 module ice2mdl
!$$$  module documentation block
!
! module:    ice2mdl
!   prgmmr: gayno         org: w/np2     date: 2005-05-20
!
! $Revision$
!
! abstract: interpolate ice data to model grid and grib the result
!
! program history log:
!   2005-05-20  gayno   - initial version
!   2007-09-29  gayno   - added nam b-grids.  improved
!                         thinning of gfs grids.
!   2014-10-16  gayno   - option to output final ice analysis
!                         in either grib 1 or grib 2.
!   2015-05-28  gayno   - increase search radius when using
!                         ims data.  at points with no ims,
!                         set to zero ice if south of 38N.
!                         previously, 55N was used. 
!
! usage: use ice2mdl
!
! remarks: some variable definitions
!   ice_mdl  - ice concentration on model grid in decimal percent
!
!$$$

 use program_setup,        only   : output_file,  &
                                    data_flag,    &
                                    grib_century, &
                                    grib_day,     &
                                    grib_hour,    &
                                    grib_month,   &
                                    grib_year,    &
                                    output_grib2

 use model_grid,           only   : resol_mdl,     &
                                    ipts_mdl,    &
                                    jpts_mdl,    &
                                    imdl,        &
                                    jmdl,        &
                                    ijmdl,       &
                                    lsmask_mdl,  &
                                    lsmask_mdl_sav,  &
                                    lats_mdl,    &
                                    lons_mdl,    &
                                    kgds_mdl,    &
                                    thinned,     &
                                    lonsperlat,  &
                                    lat11, latlast, &
                                    lon11, lonlast

 use icedat,               only   : resol_src,    &
                                    dy_src,       &
                                    lat_11_src,   &
                                    ice_src,      &
                                    snow_src_save,&
                                    bitmap_src,   &   
                                    kgds_src,     &
                                    isrc,         &
                                    jsrc,         &
                                    avg_ice_src   

 use read_write_utils,     only   : uninterpred

 real, allocatable, private      :: ice_mdl(:,:)  ! ice on model grid
 
 contains

 subroutine interp
!$$$  subprogram documentation block
!
! subprogram:   interp
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  interpolate sea ice to model grid.
!
! program history log:
! 2005-05-20  gayno    - initial version
! 2007-09-29  gayno    - added nam b-grids.  improved
!                        thinning of gfs grids.
! 2014-10-16  gayno    - output final ice analysis in
!                        grib 1 or grib 2 format.
! 2015-05-28  gayno    - increase search radius when using
!                        ims data.  at points with no ims,
!                        set to zero ice if south of 38N.
!                        previously, 55N was used. these
!                        changes improve ice analysis for
!                        hires model grids with small lakes.
!
! usage: call interp
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files: none
!
! condition codes: all fatal
!   55 - error interpolating ice data to model grid
!   76 - global ice data requried for SH grid
!
! remarks:  none.
!
!$$$

 use gdswzd_mod

 implicit none

 integer                   :: i, j, ii, jj, ij
 integer                   :: ijmdl2, istart, iend, imid, iii
 integer, allocatable      :: idum(:,:)
 integer                   :: int_opt, ipopt(20)
 integer                   :: kgds_mdl_tmp(200)
 integer                   :: no, ibo, iret, nret

 logical*1, allocatable    :: bitmap_mdl(:)

 real                      :: dum
 real                      :: gridi(1)
 real                      :: gridj(1)
 real, allocatable         :: ice_mdl_1d(:)
 real, allocatable         :: lsmask_1d(:)
 real                      :: sumc, x1, r, fraction, gridis, gridie
 real, parameter           :: undefined_value = -1.0

!-----------------------------------------------------------------------
! for regional grids located completely within the southern 
! hemisphere, nh only ims  data can't be used.
!-----------------------------------------------------------------------

 if (trim(data_flag) == "ims" .and. maxval(lats_mdl) < 0.0) then
   print*,"- FATAL ERROR: MUST SELECT GLOBAL ICE SOURCE DATA FOR SH REGIONAL GRID."
   call w3tage('ICE2MDL')
   call errexit(76)
 endif

!-----------------------------------------------------------------------
! determine type of interpolation based on resolutions of
! the model and data grids. 
!-----------------------------------------------------------------------

 print*,""
 print*,"- INTERPOLATE SEA ICE DATA TO MODEL GRID"

 ipopt = 0

 if (resol_src < (0.5*resol_mdl)) then
   print*,"- INTERPOLATE SOURCE DATA TO MODEL GRID USING BUDGET METHOD."
   ipopt(1)=2  ! break model grid cell into 25 points.
   ipopt(2:4)=1  ! 25 points are weighted equally.
   ipopt(5)=10  ! 10% coverage of valid data in box
   ipopt(20) = nint(3.5 / resol_src) + 1   ! search box width of 3.5 deg.
   kgds_mdl_tmp = kgds_mdl
   kgds_mdl_tmp(1) = kgds_mdl_tmp(1) - 255 ! subset of grid
   int_opt = 3
   no = ijmdl
 else
   print*,"- INTERPOLATE SOURCE DATA TO MODEL GRID USING NEIGHBOR METHOD."
   ipopt(1) = nint(3.5 / resol_src) + 1   ! search box width of 3.5 deg
   kgds_mdl_tmp = kgds_mdl
   kgds_mdl_tmp(1) = -1  ! for subsection of model grid.
   int_opt = 2
   no = ijmdl  ! an input when kgds(1) < 0 (subset of grid)
 end if

 allocate (bitmap_mdl(ijmdl))
 bitmap_mdl=.false.  ! if interpolation routine can't find data
                     ! at a point, this flag is false.

 allocate (ice_mdl_1d(ijmdl))
 ice_mdl_1d = 0.0  ! initialize to zero concentration because we don't
                ! interpolate in the tropics.

 call ipolates(int_opt, ipopt, kgds_src, kgds_mdl_tmp,   &
              (isrc*jsrc), ijmdl,               &
               1, 1, bitmap_src, ice_src,  &
               no, lats_mdl, lons_mdl, ibo, bitmap_mdl,     &
               ice_mdl_1d, iret)

 if (iret /= 0) then
   print*,"- FATAL ERROR IN INTERPOLATION ROUTINE. IRET IS: ", iret
   call w3tage('ICE2MDL')
   call errexit(55)
 endif

 deallocate (ice_src)

!----------------------------------------------------------------------
! for global grids, ensure permanent ice shelves in antarctica 
! are ice covered.
!----------------------------------------------------------------------

 where(lats_mdl <= -75.0) ice_mdl_1d = 1.0

!----------------------------------------------------------------------
! replace default values.  these are points for which source data
! could not be found.  (ex: isolated small lakes)
!----------------------------------------------------------------------
 
 print*,""
 print*,"- REPLACE DEFAULT VALUES"

 if (data_flag == "global" ) then

!----------------------------------------------------------------------
! replace default values with latitude band average.  may consider
! using climo instead.  however, program strives for very few points
! that need to be filled in this manner.
!----------------------------------------------------------------------

   do ij = 1, ijmdl
       if (.not. bitmap_mdl(ij)) then
         gridj(1) = (lats_mdl(ij) - lat_11_src) / dy_src + 1.0
         jj    = nint(gridj(1))
         if (jj < 1)    jj = 1
         if (jj > jsrc) jj = jsrc
         if (avg_ice_src(jj) >= 0.0) then
           ice_mdl_1d(ij) = avg_ice_src(jj)
         else   ! latitude band average could not be determined.
                ! use a default value based on latitude.
           if (abs(lats_mdl(ij)) > 55.0) then
             ice_mdl_1d(ij) = 1.0
           else
             ice_mdl_1d(ij) = 0.0
           endif
         endif
         print*,"- SET ICE TO ", ice_mdl_1d(ij), " AT POINT: ", &
                   ipts_mdl(ij), jpts_mdl(ij)
       end if
     enddo

   deallocate (avg_ice_src)

 else if (data_flag == "ims") then

!----------------------------------------------------------------------
! Replace default values.  South of 38N, always assume no ice.
! Elsewhere, see if the nearest neighbor source point is snow covered.
! If so, set to ice.
!-----------------------------------------------------------------------

   do ij = 1,ijmdl
     if (.not. bitmap_mdl(ij)) then
       if (lats_mdl(ij) <= 38.0) then
         ice_mdl_1d(ij) = 0.0
       else
         call gdswzd(kgds_src,-1,1,undefined_value,gridi,gridj, &
                     lons_mdl(ij),lats_mdl(ij),nret)
         if (nret /= 1) then
           print*,"- WARNING: MODEL POINT OUTSIDE IMS GRID: ",ipts_mdl(ij), jpts_mdl(ij)
           ice_mdl_1d(ij) = 0.0
         else
           ii = nint(gridi(1))
           jj = nint(gridj(1))
           if (snow_src_save(ii,jj) == 1) then
             ice_mdl_1d(ij) = 1.0
           else
             ice_mdl_1d(ij) = 0.0
           end if
         end if
       end if
       print*,"- SET ICE TO ", ice_mdl_1d(ij), " AT POINT: ",  &
                   ipts_mdl(ij), jpts_mdl(ij)
     end if
   enddo
   deallocate (snow_src_save)

 end if

 deallocate (lats_mdl)
 deallocate (lons_mdl)

!----------------------------------------------------------------------
! put back in 2-d array for gribbing.
!----------------------------------------------------------------------

 allocate(ice_mdl(imdl,jmdl))
 ice_mdl=0.0
 do ij = 1, ijmdl
   ice_mdl(ipts_mdl(ij),jpts_mdl(ij)) = ice_mdl_1d(ij)
 enddo

 deallocate(ice_mdl_1d)

!----------------------------------------------------------------------
! if a global model grid, and if running on thinned grid, then
! take a linear weighting of full points located within the thin points.
!----------------------------------------------------------------------

 if (kgds_mdl(1) == 4 .and. thinned) then

   ijmdl2 = sum(lonsperlat) * 2
   allocate (lsmask_1d(ijmdl2))
   allocate (ice_mdl_1d(ijmdl2))

   lsmask_1d  = 0.0
   ice_mdl_1d = 0.0

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
       sumc = 0.0   ! %
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
         sumc = sumc + fraction * ice_mdl(iii,j)
       enddo
       ice_mdl_1d(ij) = sumc / r
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
   call uninterpred(1, idum, ice_mdl_1d, ice_mdl, imdl, jmdl, ijmdl2, lonsperlat)
   deallocate(ice_mdl_1d)
   deallocate(idum)

 end if

!----------------------------------------------------------------------
! grib the interpolated data.
!----------------------------------------------------------------------

 if (output_grib2) then
   print*,"- OUTPUT ICE ANALYSIS DATA IN GRIB2 FORMAT"
   call write_grib2
 else
   print*,"- OUTPUT ICE ANALYSIS DATA IN GRIB1 FORMAT"
   call write_grib1
 endif

 deallocate (ice_mdl)
 deallocate (lsmask_mdl)

 return

 end subroutine interp

 subroutine write_grib1
!$$$  subprogram documentation block
!
! subprogram:  write_grib1
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  grib ice concentration on the model grid
!
! program history log:
! 2005-05-20  gayno    - initial version
! 2014-10-10  gayno    - rename routine as write_grib1 (was gribit)
!
! usage: call write_grib1
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files:
!   input: none
!
!   output: 
!     - model ice in grib 1, fort.64
!
! condition codes:  all fatal
!   74 - bad open of model ice file
!   75 - bad write of model ice file
!
! remarks: none.
!
!$$$

 implicit none

 integer                    :: iret 
 integer, parameter         :: lugb = 64    ! unit number of output grib file
 integer                    :: kpds(200)

 logical*1                  :: lbms(imdl,jmdl)

!----------------------------------------------------------------------
! set up pds section.  retain several settings from the
! input ice grib file because we are not creating data, but
! simply interpolating someone elses.
!
! don't need to set the gds section.
! since the model grid is not changing, use the kgds array 
! already determined in module model_grid.   
!
!  !!! need to determine the convention for output when running
!      with tiles.  i.e., the grid cell may be a mix of land and
!      non-land.  how do you define the ice concentration?
!      with respect to the grid cell, or the portion of the grid cell
!      that is not land? 
!
!----------------------------------------------------------------------

 kpds = 0

 kpds(1)  = 7           ! center id
 kpds(2)  = 255         ! process id number. set to missing.
 kpds(3)  = 255         ! grid specified in gds
 kpds(4)  = 192         ! include gds and a bit map section  
 kpds(5)  = 91          ! parameter number for ice fraction
 kpds(6)  = 1           ! level - ground or water surface
 kpds(7)  = 0           ! height pressure of level
 kpds(8)  = grib_year   ! year of century     the time info is determined   
 kpds(9)  = grib_month  ! month               according to our operations.
 kpds(10) = grib_day    ! day
 kpds(11) = grib_hour   ! hour
 kpds(12) = 0           ! minute
 kpds(13) = 1           ! fcst time unit - hour
 kpds(14) = 0           ! period of time, p1.  set to '0' for analysis
 kpds(15) = 0           ! number of time units, p2. 
 kpds(16) = 1           ! initialized analysis product
 kpds(17) = 0           ! number in average
 kpds(18) = 1           ! grib edition 1
 kpds(19) = 3           ! parameter table version number
 kpds(20) = 0           ! number missing from avg/accum
 kpds(21) = grib_century ! century - set as in the input file
 kpds(22) = 2           ! decimal scale factor
 kpds(23) = 4           ! subcenter - ncep/emc
 kpds(24) = 0           ! reserved
 kpds(25) = 0           ! reserved

 lbms = .true.         ! set bitmap section
 where(lsmask_mdl > 0.0) lbms = .false.  ! suppress land points 

 print*,""
 print*,"- OPEN OUTPUT GRIB FILE ", trim(output_file)

 call baopenw(lugb, output_file, iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR OPENING OUTPUT GRIB FILE. IRET IS ', iret
   call w3tage('ICE2MDL')
   call errexit(74)
 end if

 print*,"- WRITE OUTPUT GRIB FILE ", trim(output_file)
 call putgb (lugb, (imdl*jmdl), kpds, kgds_mdl, lbms,  &
             ice_mdl, iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR WRITING OUTPUT GRIB FILE. IRET IS ', iret
   call w3tage('ICE2MDL')
   call errexit(75)
 end if
 
 call baclose(lugb, iret)

 return

 end subroutine write_grib1

 subroutine write_grib2
!$$$  subprogram documentation block
!
! subprogram:   write_grib2
!   prgmmr: gayno          org: w/np2     date: 2014-oct-10
!
! abstract:  output ice concentration on the model grid
!            in grib 2 format.
!
! program history log:
! 2014-oct-10  gayno    - initial version
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
!      - model ice in grib2 format; fort.63
!
! condition codes:
!   48 - error creating grib2 message; fatal
!
! remarks: none.
!
!$$$

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

 call init_grib2(grib_century,grib_year, grib_month, grib_day, grib_hour, &
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
! representation section) for ice cover.
!----------------------------------------------------------------------

 allocate(fld(ngrdpts))
 fld = reshape(ice_mdl, (/imdl*jmdl/) )

 ibmap = 0 ! bitmap applies
 allocate(bmap2d(imdl,jmdl))
 bmap2d=.true.
 where (lsmask_mdl > 0.5) bmap2d=.false.
 allocate(bmap(ngrdpts))
 bmap = reshape(bmap2d, (/imdl*jmdl/) )
 deallocate(bmap2d)

 print*,"- CREATE SECTIONS 4 AND 5 FOR ICE COVER"
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

 lugb=63
 print*,"- OPEN OUTPUT GRIB FILE ", trim(output_file)
 call baopenw(lugb, output_file, iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR OPENING OUTPUT GRIB FILE. IRET IS ', iret
   call w3tage('ICE2MDL')
   call errexit(49)
 end if

 print*,'- WRITE OUTPUT GRIB FILE.'
 call wryte(lugb, lengrib, cgrib)

 call baclose (lugb, iret)

 deallocate(fld, bmap, igdstmpl, cgrib)

 return

 900 continue
 print*,'- FATAL ERROR CREATING GRIB2 MESSAGE. IRET IS ', iret
 call w3tage('ICE2MDL')
 call errexit(48)

 end subroutine write_grib2
 end module ice2mdl
