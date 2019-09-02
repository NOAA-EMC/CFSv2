!
! $Revision$
!
 subroutine interp_aavg_bgrid_prep(istart_src, iend_src, jstart_src, jend_src, &
                                   isrc, dx_src, dy_src, lat_11_src, lon_11_src, &
                                   istart_mdl, iend_mdl, jstart_mdl, jend_mdl, &
                                   nearest_i, nearest_j)
!$$$  subprogram documentation block
!
! subprogram:   interp_aavg_bgrid_prep
!   prgmmr: gayno          org: w/np2     date: 2007-08-21
!
! abstract:  for all points on a given source data grid, find the 
!            nearest i/j on the rotated lat/lon bgrid.  this routine assumes the
!            source grid is a global lat/lon projection. 
!
! program history log:
! 2007-08-21  gayno    - initial version
!
! usage: call interp_aavg_bgrid_prep(istart_src, iend_src, jstart_src, jend_src, &
!                                    isrc, dx_src, dy_src, lat_11_src, lon_11_src, &
!                                    istart_mdl, iend_mdl, jstart_mdl, jend_mdl, &
!                                    nearest_i, nearest_j)
!
!   input argument list:
!     dx_src, dy_src  - the n/s and e/w resolution of the source grid in degrees
!     istart/iend_mdl - starting/ending 'i' index on the model grid
!     jstart/jend_mdl - starting/ending 'j' index on the model grid
!     istart/iend_src - starting/ending 'i' index on the source grid
!     jstart/jend_src - starting/ending 'j' index on the source grid
!     isrc            - 'i' dimension of the source grid
!     lat_11_src      - latitude of point (1,1) on source grid
!     lon_11_src      - longitude of point (1,1) on source grid
!
!   output argument list:
!     nearest_i/j     - holds the nearest model i/j index for all
!                       source data points
!
! files: none
!
! condition codes: none
!
! remarks: none.
!
!$$$

 use ll2xy_util_bgrid, only       : ll2xy_bgrid

 implicit none

 integer, parameter              :: missing = -9999

 integer                         :: i, ii, j
 integer, intent(in)             :: istart_mdl, iend_mdl, jstart_mdl, jend_mdl
 integer, intent(in)             :: isrc, istart_src, iend_src, jstart_src, jend_src
 integer                         :: near_i, near_j
 integer*2, intent(out)          :: nearest_i(istart_src:iend_src,jstart_src:jend_src)
 integer*2, intent(out)          :: nearest_j(istart_src:iend_src,jstart_src:jend_src)

 real, intent(in)                :: dx_src, dy_src
 real, intent(in)                :: lat_11_src, lon_11_src
 real                            :: srclat, srclon

 nearest_i = missing
 nearest_j = missing

 do j = jstart_src, jend_src
   do i = istart_src, iend_src

      ii = i
      if (ii < 1) ii = isrc - i
      if (ii > isrc) ii = i - isrc
      srclat = lat_11_src + (j-1)*dy_src
      srclon = lon_11_src + (ii-1)*dx_src

      call ll2xy_bgrid(srclat, srclon, near_i, near_j)

      if (near_i >= istart_mdl .and. near_i <= iend_mdl .and. &
          near_j >= jstart_mdl .and. near_j <= jend_mdl) then
        nearest_i(i,j) = near_i
        nearest_j(i,j) = near_j
      end if

   enddo
 enddo

 return

 end subroutine interp_aavg_bgrid_prep

 subroutine interp_aavg_egrid_prep(istart_src, iend_src, jstart_src, jend_src, &
                                   isrc, dx_src, dy_src, lat_11_src, &
                                   lon_11_src, dx_mdl, dy_mdl, &
                                   istart_mdl, iend_mdl, jstart_mdl, jend_mdl, &
                                   imdl, nearest_i, nearest_j)
!$$$  subprogram documentation block
!
! subprogram:   interp_aavg_egrid_prep
!   prgmmr: gayno          org: w/np2     date: 2007-06-28
!
! abstract:  for all points on a given source data grid, find the 
!            nearest i/j on the rotated lat/lon egrid.  this routine assumes the
!            source grid is a global lat/lon projection. 
!
! program history log:
! 2007-06-28  gayno    - initial version
!
! usage: call interp_aavg_egrid_prep(istart_src, iend_src, jstart_src, jend_src, &
!                                    isrc, dx_src, dy_src, lat_11_src, &
!                                    lon_11_src, dx_mdl, dy_mdl, &
!                                    istart_mdl, iend_mdl, jstart_mdl, jend_mdl, &
!                                    imdl, nearest_i, nearest_j)
!
!   input argument list:
!     dx_mdl, dy_mdl  - the n/s and e/w resolution of the model grid in degrees
!     dx_src, dy_src  - the n/s and e/w resolution of the source grid in degrees
!     imdl            - i dimension of model grid
!     istart/iend_mdl - starting/ending 'i' index on the model grid
!     jstart/jend_mdl - starting/ending 'j' index on the model grid
!     istart/iend_src - starting/ending 'i' index on the source grid
!     jstart/jend_src - starting/ending 'j' index on the source grid
!     isrc            - 'i' dimension of the source grid
!     lat_11_src      - latitude of point (1,1) on source grid
!     lon_11_src      - longitude of point (1,1) on source grid
!
!   output argument list:
!     nearest_i/j     - holds the nearest model i/j index for all
!                       source data points
!
! files: none
!
! condition codes: none
!
! remarks: none.
!
!$$$

 use ll2xy_util_egrid, only       : ll2xy_egrid

 implicit none

 integer, parameter              :: missing = -9999

 integer                         :: i, ii, j
 integer, intent(in)             :: imdl
 integer, intent(in)             :: istart_mdl, iend_mdl, jstart_mdl, jend_mdl
 integer, intent(in)             :: isrc, istart_src, iend_src, jstart_src, jend_src
 integer                         :: near_i, near_j
 integer*2, intent(out)          :: nearest_i(istart_src:iend_src,jstart_src:jend_src)
 integer*2, intent(out)          :: nearest_j(istart_src:iend_src,jstart_src:jend_src)

 real, intent(in)                :: dx_mdl, dy_mdl
 real, intent(in)                :: dx_src, dy_src
 real, intent(in)                :: lat_11_src, lon_11_src
 real                            :: srclat, srclon

 nearest_i = missing
 nearest_j = missing

 do j = jstart_src, jend_src
   do i = istart_src, iend_src

      ii = i
      if (ii < 1) ii = isrc - i
      if (ii > isrc) ii = i - isrc
      srclat = lat_11_src + (j-1)*dy_src
      srclon = lon_11_src + (ii-1)*dx_src

      call ll2xy_egrid(srclat, srclon, imdl, &
                       -(dx_mdl), dy_mdl, near_i, near_j)
                 
      if (near_i >= istart_mdl .and. near_i <= iend_mdl .and. &
          near_j >= jstart_mdl .and. near_j <= jend_mdl) then
        nearest_i(i,j) = near_i
        nearest_j(i,j) = near_j
      end if

   enddo
 enddo

 return

 end subroutine interp_aavg_egrid_prep

 subroutine interp_aavg_nam (istart_mdl, iend_mdl, jstart_mdl, jend_mdl, &
                              lat_mdl, lon_mdl, lsmask, &
                              dx_src, dy_src, lat_11_src, lon_11_src, &
                              default_value, undef_value,  &
                              scaling_fac, data_mdl, data_src,  &
                              isrc, istart_src, iend_src, jstart_src, jend_src, &
                              nearest_i, nearest_j)
!$$$  subprogram documentation block
!
! subprogram:   interp_aavg_nam
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  interpolate data to an nam by taking the
!            area average of the source data.  routine 
!            interp_aavg_egrid_prep or interp_aavg_bgrid_prep 
!            must be called first.
!            assumes source grid is a global lat/lon projection.
!
! program history log:
! 2005-05-20  gayno    - initial version
! 2007-06-28  gayno    - modified to work for mpi
!
! usage: call interp_aavg_nam (istart_mdl, iend_mdl, jstart_mdl, jend_mdl, &
!                               lat_mdl, lon_mdl, lsmask, &
!                               dx_src, dy_src, lat_11_src, lon_11_src, &
!                               default_value, undef_value,  &
!                               scaling_fac, data_mdl, data_src,  &
!                               isrc, istart_src, iend_src, jstart_src, jend_src, &
!                               nearest_i, nearest_j)
!
!   input argument list:
!     data_src        - data on source grid (scaled integer)
!     default_value   - flag value for model points for which source
!                       was not found
!     dx/dy_src       - x/y direction resolution of source data in deg
!     istart/end_mdl  - model grid i-dimension bounds for this task
!     jstart/end_mdl  - model grid j-dimension bounds for this task
!     isrc            - source grid i-dimension
!     istart/end_src  - source grid i-dimension bounds for this task
!     jstart/end_src  - source grid j-dimension bounds for this task
!     lat_mdl         - latitudes on model grid
!     lat_11_src      - latitude of point (1,1) on source grid
!     lon_11_src      - longitude of point (1,1) on source grid
!     lon_mdl         - longitudes on model grid
!     lsmask          - model land/sea mask
!     nearest_i/j     - holds the i/j index of the nearest model
!                       grid point for all source data points
!     scaling_fac     - source data scaling factor
!     undef_value     - flag value indicating source data point is
!                       non-land and to be ingored during interpolation
!
!   output argument list:
!     data_mdl        - data interpolated to the model grid
!
! files: none
!
! condition codes:
!   1 - model point located outside source grid
!
! remarks: none.
!
!$$$

 implicit none

 include 'mpif.h'

 integer, parameter                :: missing = -9999

 integer                           :: count(istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 integer*2, intent(in)             :: data_src (isrc,jstart_src:jend_src)
 integer                           :: i, j, ii, jj, iii, jjj
 integer                           :: iend, istart, jend, jstart
 integer                           :: igrid, iret
 integer*4, intent(in)             :: isrc
 integer                           :: jgrid
 integer, intent(in)               :: istart_mdl, iend_mdl, jstart_mdl, jend_mdl
 integer, intent(in)               :: istart_src, iend_src, jstart_src, jend_src
 integer                           :: krad
 integer*2, intent(in)             :: nearest_i(istart_src:iend_src,jstart_src:jend_src)
 integer*2, intent(in)             :: nearest_j(istart_src:iend_src,jstart_src:jend_src)
 integer*4 , intent(in)            :: scaling_fac
 integer                           :: spiral_rad
 integer*8                         :: sum      (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 integer*2, intent(in)             :: undef_value

 real, intent(out)                 :: data_mdl    (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real, intent(in)                  :: default_value
 real*8, intent(in)                :: dx_src, dy_src 
 real                              :: gridi
 real                              :: gridj
 real*8, intent(in)                :: lat_11_src, lon_11_src
 real, intent(in)                  :: lat_mdl  (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real, intent(in)                  :: lon_mdl  (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real, intent(in)                  :: lsmask   (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)

!----------------------------------------------------------------------
! routine assumes source data is on a global lat/lon grid.
!---------------------------------------------------------------------- 

 count = 0
 sum   = 0.0

 do j = jstart_src, jend_src
   do i = istart_src, iend_src
     iii = i
     if (iii < 1) iii = isrc - iii
     if (iii > isrc) iii = iii - isrc
     if (nearest_i(i,j) /= missing .and. nearest_j(i,j) /= missing .and. &
         data_src(iii,j) /= undef_value) then
       count(nearest_i(i,j),nearest_j(i,j)) = count(nearest_i(i,j),nearest_j(i,j)) + 1
       sum(nearest_i(i,j),nearest_j(i,j)) = sum(nearest_i(i,j),nearest_j(i,j)) + &
                                            data_src(iii,j)
     end if
   enddo
 enddo

 data_mdl = 0.0

 JLOOP : do j = jstart_mdl, jend_mdl
 ILOOP : do i = istart_mdl, iend_mdl

   if (lsmask(i,j) == 0.0) cycle ILOOP

!---------------------------------------------------------------------- 
!  there were valid source data within the model grid box.
!---------------------------------------------------------------------- 

   VALID_COUNT : if (count(i,j) > 0) then
 
     data_mdl(i,j) = float(sum(i,j)) / (float(count(i,j)) * float(scaling_fac))

!---------------------------------------------------------------------- 
!    there were no valid source data within the model grid box,
!    do a spiral search to find a valid value.
!---------------------------------------------------------------------- 

   else

     gridj = (lat_mdl(i,j) - lat_11_src) / dy_src + 1.0
     gridi = (lon_mdl(i,j) - lon_11_src) / dx_src + 1.0

     jgrid = nint (gridj)
     igrid = nint (gridi)

     if (jgrid > jend_src) then
       print*,'- FATAL ERROR IN ROUTINE INTERP_AAVG_NAM.'
       print*,'- MDL PT OUTSIDE SOURCE GRID.'
       call mpi_abort(mpi_comm_world, 1, iret)
     elseif (jgrid < jstart_src) then
       print*,'- FATAL ERROR IN ROUTINE INTERP_AAVG_NAM.'
       print*,'- MDL PT OUTSIDE SOURCE GRID.'
       call mpi_abort(mpi_comm_world, 1, iret)
     end if

     if (igrid > isrc) then
       igrid = igrid - isrc
     else if (igrid < 1) then
       igrid = igrid + isrc
     end if

     spiral_rad = nint(5.0 / dx_src)

     SPIRAL_SEARCH : do krad = 1, spiral_rad

       istart = igrid - krad
       iend   = igrid + krad
       jstart = jgrid - krad
       jend   = jgrid + krad

       do jj = jstart, jend
       do ii = istart, iend

!-----------------------------------------------------------------------
!        search only along outer square.
!-----------------------------------------------------------------------

         if ((jj .eq. jstart) .or. (jj .eq. jend) .or.   &
             (ii .eq. istart) .or. (ii .eq. iend))  then

!-----------------------------------------------------------------------
!          ensure that point being investigated is within
!          the northern and southern bounds of the source grid.
!-----------------------------------------------------------------------

           if ((jj .ge. jstart_src) .and. (jj .le. jend_src)) then

             jjj = jj

!-----------------------------------------------------------------------
!            adjust i-index on source grid when search
!            crosses the date line.
!-----------------------------------------------------------------------

             if (ii .le. 0) then
               iii = isrc + ii
             else if (ii .ge. (isrc+1)) then
               iii = ii - isrc
             else
               iii = ii
             end if

!-----------------------------------------------------------------------
!            a valid value was found.
!-----------------------------------------------------------------------

             if (data_src(iii,jjj) /= undef_value) then
               data_mdl(i,j) = float(data_src(iii,jjj))/float(scaling_fac)
               write (6, 6000) i,j, krad
               cycle ILOOP
             end if

           end if

         end if

       enddo
       enddo

     enddo SPIRAL_SEARCH

!-----------------------------------------------------------------------
!    the circular search failed. therefore assign a default value.
!-----------------------------------------------------------------------

     data_mdl(i,j) = default_value
     write (6, 6100) i, j

   end if VALID_COUNT

 enddo ILOOP
 enddo JLOOP

 return

!-----------------------------------------------------------------------
! format statements.
!-----------------------------------------------------------------------

 6000 FORMAT (1X, '-- CIRCULAR SEARCH AT POINT ', I4, 1X, I4, ' ITERATIONS ',I3)

 6100 FORMAT (1X, '-- DEFAULT VALUE ASSIGNED AT POINT ', 1X, I4, 1X, I4)

 end subroutine interp_aavg_nam

 subroutine interp_aavg_gaus(istart_mdl, iend_mdl, iend_mdl_4_loops, &
                             jstart_mdl, jend_mdl, &
                             lat_mdl, lon_mdl, lsmask,  &
                             dx_mdl, dy_mdl, &
                             dx_src, dy_src, lat_11_src, lon_11_src,  &
                             default_value, undef_value, scaling_fac,  &
                             data_mdl, data_src, isrc, &
                             jsrc_start, jend_src)
!$$$  subprogram documentation block
!
! subprogram:   interp_aavg_gaus
!   prgmmr: gayno          org: w/np2     date: 2007-06-12
!
! abstract:  interpolate data to the GFS gaussian grid by taking
!   an area average of the source data.  
!
! program history log:
! 2007-06-12  gayno    - initial version
!
! usage: call interp_aavg_gaus(istart_mdl, iend_mdl, iend_mdl_4_loops, &
!                              jstart_mdl, jend_mdl, &
!                              lat_mdl, lon_mdl, lsmask,  &
!                              dx_mdl, dy_mdl, &
!                              dx_src, dy_src, lat_11_src, lon_11_src,  &
!                              default_value, undef_value, scaling_fac,  &
!                              data_mdl, data_src, isrc, &
!                              jsrc_start, jend_src)
!
!   input argument list:
!     data_src       - data on source grid (scaled integer)
!     default_value  - flag value for model points for which source
!                      data was not found
!     dx/dy_mdl      - x/y direction resolution of model grid in deg.
!     dx/dy_src      - x/y direction resolution of source data in deg.
!     istart/end_mdl - model grid i-dimension bounds for this task
!     iend_mdl_4_loops - array of upper bound of model grid i-dimension.
!                        used for gfs where i-dim decreases toward poles
!     jstart/end_mdl - model grid j-dimension bounds for this task
!     isrc           - source grid i-dimension
!     jstart/end_src - j dimension bounds of source data
!     lat_mdl        - latitudes on model grid
!     lat_11_src     - latitude of point (1,1) on source grid
!     lon_11_src     - longitude of point (1,1) on source grid
!     lon_mdl        - longitudes on model grid
!     lsmask         - model land/sea mask
!     scaling_fac    - source data scaling factor
!     undef_value    - flag value indicating source data point is
!                      not defined and to be ingored during interpolation
!
!   output argument list:
!     data_mdl       - data interpolated to the model grid
!
! files: none
!
! condition codes:
!  1 - model grid point located outside source grid
!
! remarks: none.
!
!$$$

 implicit none
 
 include 'mpif.h'

 integer*2, intent(in)    :: data_src (isrc,jsrc_start:jend_src)
 integer                  :: iend, iend_save, istart, istart_save
 integer                  :: igrid, jgrid, iret
 integer                  :: i, ii, iii, j, jj, jjj
 integer, intent(in)      :: istart_mdl, iend_mdl, iend_mdl_4_loops(jstart_mdl:jend_mdl)
 integer, intent(in)      :: jstart_mdl, jend_mdl
 integer, intent(in)      :: isrc
 integer, intent(in)      :: jsrc_start, jend_src
 integer                  :: jend, jend_save, jstart, jstart_save
 integer                  :: krad
 integer*4, intent(in)    :: scaling_fac
 integer                  :: spiral_rad
 integer*2, intent(in)    :: undef_value

 real                     :: count
 real, intent(in)         :: dx_mdl(jstart_mdl:jend_mdl), dy_mdl
 real, intent(out)        :: data_mdl (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real, intent(in)         :: default_value
 real*8, intent(in)       :: dx_src, dy_src
 real                     :: gridj, gridi
 real, allocatable        :: i_fraction(:), j_fraction(:)
 real                     :: istart_fraction, iend_fraction
 real                     :: jstart_fraction, jend_fraction
 real, intent(in)         :: lat_mdl (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real*8, intent(in)       :: lat_11_src, lon_11_src
 real, intent(in)         :: lon_mdl (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real, intent(in)         :: lsmask  (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real                     :: sum

!-----------------------------------------------------------------------
! set search radius for spiral search to be 5 degrees.
!-----------------------------------------------------------------------

 spiral_rad = nint(5.0 / dx_src)

 data_mdl = 0.0

 JLOOP : do j = jstart_mdl, jend_mdl
 ILOOP : do i = istart_mdl, iend_mdl_4_loops(j)

   if (lsmask(i,j) == 0.0) cycle ILOOP

!-----------------------------------------------------------------------
!  find the i/j bounds of the model grid box with respect to the source
!  grid.  account for the fact that some source grid points may be
!  only partially within the model grid box.  this becomes less
!  important as the resolution of the source data increases with
!  respect to the model resolution.
!-----------------------------------------------------------------------

   gridi           = ( (lon_mdl(i,j)-0.5*dx_mdl(j)) - lon_11_src) / dx_src + 1.0
   istart          = nint(gridi)
   istart_fraction = 0.5 - (gridi - float(istart))

   gridi         = ( (lon_mdl(i,j)+0.5*dx_mdl(j)) - lon_11_src) / dx_src + 1.0
   iend          = nint(gridi)
   iend_fraction = 0.5 + (gridi - float(iend)) 

   allocate (i_fraction(istart:iend))
   i_fraction         = 1.0
   i_fraction(istart) = istart_fraction
   i_fraction(iend)   = iend_fraction

   if (dy_src > 0) then
     gridj = ( (lat_mdl(i,j)-0.5*dy_mdl) - lat_11_src) / dy_src + 1.0
   else
     gridj = ( (lat_mdl(i,j)+0.5*dy_mdl) - lat_11_src) / dy_src + 1.0
   end if

   jstart          = nint(gridj)
   jstart_fraction = 0.5 - (gridj - float(jstart))

   if (dy_src > 0) then
     gridj = ( (lat_mdl(i,j)+0.5*dy_mdl) - lat_11_src) / dy_src + 1.0
   else
     gridj = ( (lat_mdl(i,j)-0.5*dy_mdl) - lat_11_src) / dy_src + 1.0
   endif

   jend          = nint(gridj)
   jend_fraction = 0.5 + (gridj - float(jend))

   allocate (j_fraction(jstart:jend))
   j_fraction         = 1.0
   j_fraction(jstart) = jstart_fraction
   j_fraction(jend)   = jend_fraction

!-----------------------------------------------------------------------
!  this should not occur if you ensure the model points fall within the
!  latitude band of the source grid data.
!-----------------------------------------------------------------------

   if (jend > jend_src) then
     print*,'- FATAL ERROR IN ROUTINE INTERP_AAVG_GAUS.'
     print*,'- MDL PT OUTSIDE SOURCE GRID.'
     call mpi_abort(mpi_comm_world, 1, iret)
   elseif (jstart < jsrc_start) then
     print*,'- FATAL ERROR IN ROUTINE INTERP_AAVG_GAUS.'
     print*,'- MDL PT OUTSIDE SOURCE GRID.'
     call mpi_abort(mpi_comm_world, 1, iret)
   end if

!-----------------------------------------------------------------------
!  take an area average.
!-----------------------------------------------------------------------

   count = 0.0
   sum   = 0.0

   do ii = istart, iend
       iii = ii
       if (iii .ge. (isrc+1)) iii = iii - isrc
       if (iii .lt. 1)        iii = iii + isrc
     do jj = jstart, jend
       if (data_src(iii,jj) /= undef_value) then
         count = count + (i_fraction(ii) * j_fraction(jj))
         sum = sum + ( float(data_src(iii,jj)) * &
                      i_fraction(ii) * j_fraction(jj) )
       end if
     enddo
   enddo

   deallocate (i_fraction, j_fraction)

   VALID_COUNT : if ( count .gt. 0.0 ) then

     data_mdl(i,j) = sum / (float(scaling_fac)* count)

   else

!-----------------------------------------------------------------------
!    source data is undefined at this point, do a spiral
!    search for valid value.
!-----------------------------------------------------------------------
  
     istart_save = istart
     iend_save   = iend
     jstart_save = jstart
     jend_save   = jend

     SPIRAL_SEARCH : do krad = 1, spiral_rad

       istart = istart_save - krad
       iend   = iend_save + krad
       jstart = jstart_save - krad
       jend   = jend_save + krad

       do jj = jstart, jend
       do ii = istart, iend

!-----------------------------------------------------------------------
!        search only along outer square.
!-----------------------------------------------------------------------

         if ((jj .eq. jstart) .or. (jj .eq. jend) .or.   &
             (ii .eq. istart) .or. (ii .eq. iend))  then

!-----------------------------------------------------------------------
!          ensure that point being investigated is within
!          the northern and southern bounds of the source grid.
!-----------------------------------------------------------------------

           if ((jj .ge. jsrc_start) .and. (jj .le. jend_src)) then

             jjj = jj

!-----------------------------------------------------------------------
!            adjust i-index on source grid when search
!            crosses the date line.
!-----------------------------------------------------------------------

             if (ii .le. 0) then
               iii = isrc + ii
             else if (ii .ge. (isrc+1)) then
               iii = ii - isrc
             else
               iii = ii
             end if

!-----------------------------------------------------------------------
!            a valid value was found.
!-----------------------------------------------------------------------

             if (data_src(iii,jjj) /= undef_value) then
               data_mdl(i,j) = float(data_src(iii,jjj))/float(scaling_fac)
               write (6, 6000) i, j, krad
               cycle ILOOP
             end if

           end if

         end if

       enddo
       enddo

     enddo SPIRAL_SEARCH

!-----------------------------------------------------------------------
!  the circular search failed. therefore assign a default value.
!-----------------------------------------------------------------------

     data_mdl(i,j) = default_value
     write (6, 6100) i, j

   end if VALID_COUNT

 enddo ILOOP
 enddo JLOOP

 return

!-----------------------------------------------------------------------
! format statements.
!-----------------------------------------------------------------------

 6000 FORMAT (1X, '-- CIRCULAR SEARCH AT POINT ', I4, 1X, I4, ' ITERATIONS ',I3)

 6100 FORMAT (1X, '-- DEFAULT VALUE ASSIGNED AT POINT ', 1X, I4, 1X, I4)

 end subroutine interp_aavg_gaus

 subroutine interp_nn(istart_mdl, iend_mdl, iend_mdl_4_loops, &
                      jstart_mdl, jend_mdl, &
                      lat_mdl, lon_mdl, lsmask,  &
                      dx_src, dy_src, lat_11_src, lon_11_src, &
                      default_value, undef_value, scaling_fac,  &
                      data_mdl, data_src, isrc, jstart_src, jend_src) 
!$$$  subprogram documentation block
!
! subprogram:   interp_nn
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  interpolate data to the model grid by taking the
!   nearest neighbor.
!
! program history log:
! 2005-05-20  gayno    - initial version
! 2007-06-28  gayno    - modified to work with mpi
!
! usage: call interp_nn(istart_mdl, iend_mdl, iend_mdl_4_loops,  &
!                       jstart_mdl, jend_mdl, &
!                       lat_mdl, lon_mdl, lsmask,  &
!                       dx_src, dy_src, lat_11_src, lon_11_src, &
!                       default_value, undef_value, scaling_fac,  &
!                       data_mdl, data_src, isrc, jstart_src, jend_src) 
!
!   input argument list:
!     data_src        - data on source grid (scaled integer)
!     default_value   - flag value for model points for which source
!                       was not found
!     dx/dy_src       - x/y direction resolution of source data in deg
!     istart/end_mdl  - model grid i-dimension bounds for this task
!     iend_mdl_4_loops - array of upper bound of model grid i-dimension.
!                        used for gfs where i-dim decreases toward poles
!     jstart/end_mdl  - model grid j-dimension bounds for this task
!     isrc            - source grid i-dimension
!     jstart_src/end  - source grid j-dimension bounds
!     lat_mdl         - latitudes on model grid
!     lat_11_src      - latitude of point (1,1) on source grid
!     lon_11_src      - longitude of point (1,1) on source grid
!     lon_mdl         - longitudes on model grid
!     lsmask          - model land/sea mask 
!     scaling_fac     - source data scaling factor
!                       averaging will occur 
!     undef_value     - flag value indicating source data point is
!                       undefined and to be ingored during interpolation
!
!   output argument list:
!     data_mdl        - data interpolated to the model grid
!
! files: none
!
! condition codes:
!   1 - model grid point located outside source grid
!
! remarks: none.
!
!$$$

 implicit none

 include 'mpif.h'

 integer*2, intent(in)    :: data_src    (isrc,jstart_src:jend_src)
 integer                  :: iend, istart
 integer, intent(in)      :: istart_mdl, iend_mdl, iend_mdl_4_loops(jstart_mdl:jend_mdl)
 integer, intent(in)      :: jstart_mdl, jend_mdl
 integer                  :: igrid, jgrid, iret
 integer                  :: i,j, ii, iii, jj, jjj
 integer*4, intent(in)    :: isrc
 integer, intent(in)      :: jstart_src, jend_src
 integer                  :: jend, jstart
 integer                  :: krad
 integer*4, intent(in)    :: scaling_fac
 integer                  :: spiral_rad
 integer*2, intent(in)    :: undef_value

 real, intent(out)        :: data_mdl    (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real, intent(in)         :: default_value
 real*8, intent(in)       :: dx_src, dy_src
 real                     :: gridj, gridi
 real, intent(in)         :: lat_mdl     (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real*8, intent(in)       :: lat_11_src, lon_11_src
 real, intent(in)         :: lsmask      (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real, intent(in)         :: lon_mdl     (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)

!-----------------------------------------------------------------------
! set search radius for spiral search to be 5 degrees.
!-----------------------------------------------------------------------

 spiral_rad = nint(5.0 / dx_src)

 JLOOP : do j = jstart_mdl, jend_mdl
 ILOOP : do i = istart_mdl, iend_mdl_4_loops(j)

   if (lsmask(i,j) == 0.0) cycle ILOOP

   gridj = (lat_mdl(i,j) - lat_11_src) / dy_src + 1.0
   gridi = (lon_mdl(i,j) - lon_11_src) / dx_src + 1.0

   jgrid = nint (gridj)
   igrid = nint (gridi)

   if (jgrid > jend_src) then
     print*,'- FATAL ERROR IN ROUTINE INTERP_NN.'
     print*,'- MDL PT OUTSIDE SOURCE GRID.'
     call mpi_abort(mpi_comm_world, 1, iret)
   elseif (jgrid < jstart_src) then
     print*,'- FATAL ERROR IN ROUTINE INTERP_NN.'
     print*,'- MDL PT OUTSIDE SOURCE GRID.'
     call mpi_abort(mpi_comm_world, 1, iret)
   end if

   if (igrid > isrc) then
     igrid = igrid - isrc
   else if (igrid < 1) then
     igrid = igrid + isrc
   end if

   if (data_src(igrid,jgrid) /= undef_value) then

     data_mdl(i,j) = float(data_src(igrid,jgrid))/float(scaling_fac)

   else

!-----------------------------------------------------------------------
!    source data is undefined at this point, do a spiral
!    search for valid value.
!-----------------------------------------------------------------------

     SPIRAL_SEARCH : do krad = 1, spiral_rad

       istart = igrid - krad
       iend   = igrid + krad
       jstart = jgrid - krad
       jend   = jgrid + krad

       do jj = jstart, jend
       do ii = istart, iend

!-----------------------------------------------------------------------
!        search only along outer square.
!-----------------------------------------------------------------------

         if ((jj .eq. jstart) .or. (jj .eq. jend) .or.   &
             (ii .eq. istart) .or. (ii .eq. iend))  then

!-----------------------------------------------------------------------
!           ensure that point being investigated is within
!           the northern and southern bounds of the source grid.
!-----------------------------------------------------------------------

           if ((jj .ge. jstart_src) .and. (jj .le. jend_src)) then

             jjj = jj

!-----------------------------------------------------------------------
!            adjust i-index on source grid when search
!            crosses the date line.
!-----------------------------------------------------------------------

             if (ii .le. 0) then
               iii = isrc + ii
             else if (ii .ge. (isrc+1)) then
               iii = ii - isrc
             else
               iii = ii
             end if

!-----------------------------------------------------------------------
!            a valid value was found.
!-----------------------------------------------------------------------

             if (data_src(iii,jjj) /= undef_value) then
               data_mdl(i,j) = float(data_src(iii,jjj))/float(scaling_fac)
               write (6, 6000) i, j, krad
               cycle ILOOP
             end if

           end if

         end if

       enddo
       enddo

     enddo SPIRAL_SEARCH

!-----------------------------------------------------------------------
!    the circular search failed. therefore assign a default value.
!-----------------------------------------------------------------------

     data_mdl(i,j) = default_value
     write (6, 6100) i,j

   endif

 enddo ILOOP
 enddo JLOOP

 return

!-----------------------------------------------------------------------
! format statements.
!-----------------------------------------------------------------------

 6000 FORMAT (1X, '-- CIRCULAR SEARCH AT POINT ', I4, 1X, I4, ' ITERATIONS ',I3)

 6100 FORMAT (1X, '-- DEFAULT VALUE ASSIGNED AT POINT ', 1X, I4, 1X, I4)

 end subroutine interp_nn

 subroutine interp_bilinear (istart_mdl, iend_mdl, iend_mdl_4_loops, &
                             jstart_mdl, jend_mdl,  &
                             lat_mdl, lon_mdl, lsmask, & 
                             dx_src, dy_src, lat_11_src, lon_11_src, &
                             default_value, undef_value, scaling_fac, &
                             data_mdl, data_src, isrc, jstart_src, jend_src)
!$$$  subprogram documentation block
!
! subprogram:   interp_bilinear
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  interpolate data to the model grid by 
!            bilinear interpolation
!
! program history log:
! 2005-05-20  gayno    - initial version
! 2007-06-28  gayno    - modified to work with mpi
!
! usage: call interp_bilinear (istart_mdl, iend_mdl, iend_mdl_4_loops, &
!                              jstart_mdl, jend_mdl,  &
!                              lat_mdl, lon_mdl, lsmask, & 
!                              dx_src, dy_src, lat_11_src, lon_11_src, &
!                              default_value, undef_value, scaling_fac, &
!                              data_mdl, data_src, isrc, jstart_src, jend_src)
!
!   input argument list:
!     data_src        - data on source grid (scaled integer)
!     default_value   - flag value for model points for which source
!                       was not found
!     dx/dy_src       - x/y direction resolution of source data in deg
!     istart/iend_mdl - model grid i-dimension bounds for this task
!     iend_mdl_4_loops - array of upper bound of model grid i-dimension.
!                        used for gfs where i-dim decreases toward poles
!     isrc            - source grid i-dimension
!     jstart/jend_mdl - model grid j-dimension bounds for this task
!     jstart/jend_src - source grid j-dimension
!     lat_mdl         - latitudes on model grid
!     lat_11_src      - latitude of point (1,1) on source grid
!     lon_11_src      - longitude of point (1,1) on source grid
!     lon_mdl         - longitudes on model grid
!     lsmask          - land mask of model grid (0-nonland;>0 land)
!     scaling_fac     - source data scaling factor
!     undef_value     - flag value indicating source data point is
!                      non-land and to be ingored during interpolation
!
!   output argument list:
!     data_mdl        - data interpolated to the model grid
!
! files: none
!
! condition codes:
!   1 - model grid point located outside source grid.
!
! remarks: none.
!
!$$$

 implicit none

 include 'mpif.h'

 integer                  :: iend, istart
 integer, intent(in)      :: istart_mdl, iend_mdl, iend_mdl_4_loops(jstart_mdl:jend_mdl)
 integer                  :: igrid, jgrid, nigrid, njgrid
 integer                  :: igridp1, jgridp1
 integer, intent(in)      :: jstart_mdl, jend_mdl
 integer                  :: i, ii, iii, j, jj, jjj
 integer*4, intent(in)    :: isrc, jstart_src, jend_src
 integer                  :: jend, jstart
 integer                  :: krad, iret
 integer                  :: spiral_rad

 real, intent(out)        :: data_mdl    (istart_mdl:iend_mdl,jstart_mdl:jend_mdl) 
 integer*2, intent(in)    :: data_src    (isrc,jstart_src:jend_src)
 real, intent(in)         :: default_value
 real*8, intent(in)       :: dx_src, dy_src
 real, intent(in)         :: lat_mdl     (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real, intent(in)         :: lon_mdl     (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real                     :: gridj, gridi
 real, intent(in)         :: lat_11_src, lon_11_src
 real, intent(in)         :: lsmask      (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real                     :: term1, term2
 integer*2, intent(in)    :: undef_value
 integer*4, intent(in)    :: scaling_fac
 real                     :: w1, w2

!-----------------------------------------------------------------------
! for spiral searchs, use a generous radius of 5 degrees.
!-----------------------------------------------------------------------

 spiral_rad = nint(5.0 / dx_src)

 JLOOP : do j = jstart_mdl, jend_mdl
   ILOOP : do i = istart_mdl, iend_mdl_4_loops(j)

!-----------------------------------------------------------------------
!    interpolate at land points.
!-----------------------------------------------------------------------

     LAND_TEST : if (lsmask(i,j) > 0.0) then

!-----------------------------------------------------------------------
!      find corresponding "i" and "j" indices on the source grid. 
!      account for cases when you are the poles/dateline.
!-----------------------------------------------------------------------

       gridj = (lat_mdl(i,j) - lat_11_src) / dy_src + 1.0
       jgrid  = int (gridj)

       if (jgrid == jend_src) then
         jgridp1 = jend_src
         njgrid  = jend_src
         w2      = 1
       elseif (jgrid == 0) then
         jgrid   = 1
         jgridp1 = 1
         njgrid  = 1
         w2      = 0
       elseif (jgrid > jend_src) then
         print*,'- FATAL ERROR IN ROUTINE INTERP_BILINEAR.'
         print*,'- MDL PT OUTSIDE SOURCE GRID.'
         call mpi_abort(mpi_comm_world, 1, iret)
       elseif (jgrid < 0) then
         print*,'- FATAL ERROR IN ROUTINE INTERP_BILINEAR.'
         print*,'- MDL PT OUTSIDE SOURCE GRID.'
         call mpi_abort(mpi_comm_world, 1, iret)
       else
         jgridp1 = jgrid + 1
         njgrid  = nint(gridj)
         w2      = mod(gridj,1.0)
       end if

       gridi = (lon_mdl(i,j) - lon_11_src) / dx_src + 1.0
       if (gridi > isrc) gridi = gridi - isrc
       if (gridi < 1.0)  gridi = gridi + isrc 

       w1 = mod(gridi,1.0)

       igrid  = int (gridi)
       if (igrid > isrc) igrid = igrid - isrc
       if (igrid < 1)    igrid = igrid + isrc

       igridp1 = igrid + 1
       if (igridp1 > isrc) igridp1 = igridp1 - isrc

       nigrid = nint(gridi)
       if (nigrid > isrc) nigrid = nigrid - isrc
       if (nigrid < 1)    nigrid = nigrid + isrc

!-----------------------------------------------------------------------
!      the four surrounding points have valid values.  do a bilinear
!      interpolation.
!-----------------------------------------------------------------------

       if ((data_src(igrid,jgrid)     /= undef_value)  .and. &
           (data_src(igridp1,jgrid)   /= undef_value)  .and. &
           (data_src(igrid,jgridp1)   /= undef_value)  .and. &
           (data_src(igridp1,jgridp1) /= undef_value) ) then

         term1 = ( (1.0 - w1) *  float(data_src(igrid,jgrid))/float(scaling_fac) ) +  &
                   w1 * float(data_src(igridp1,jgrid))/float(scaling_fac)
         term2 = ( (1.0 - w1) * float(data_src(igrid,jgridp1))/float(scaling_fac) ) + &
                   w1 * float(data_src(igridp1,jgridp1))/float(scaling_fac) 

         data_mdl(i,j) = ((1.0 - w2) * term1) + &
                           w2 * term2

!-----------------------------------------------------------------------
!      all four surrounding points do not have valid values.  pick
!      the nearest neighbor if valid.
!-----------------------------------------------------------------------

       elseif (data_src(nigrid,njgrid) /= undef_value) then

         data_mdl(i,j) = float(data_src(nigrid,njgrid))/float(scaling_fac)

       else

!-----------------------------------------------------------------------
!        source data is undefined at this point, do a spiral
!        search for valid value.
!-----------------------------------------------------------------------

         SPIRAL_SEARCH : do krad = 1, spiral_rad
                
           istart = nigrid - krad
           iend   = nigrid + krad
           jstart = njgrid - krad
           jend   = njgrid + krad

           do jj = jstart, jend
           do ii = istart, iend

!-----------------------------------------------------------------------
!            search only along outer square.
!-----------------------------------------------------------------------

             if ((jj .eq. jstart) .or. (jj .eq. jend) .or.   &            
                 (ii .eq. istart) .or. (ii .eq. iend))  then

!-----------------------------------------------------------------------
!              ensure that point being investigated is within
!              the northern and southern bounds of the source grid.
!-----------------------------------------------------------------------

               if ((jj .ge. jstart_src) .and. (jj .le. jend_src)) then 

                 jjj = jj

!-----------------------------------------------------------------------
!                adjust i-index on source grid when search
!                crosses the date line.
!-----------------------------------------------------------------------

                 if (ii .le. 0) then
                   iii = isrc + ii
                 else if (ii .ge. (isrc+1)) then
                   iii = ii - isrc                  
                 else
                   iii = ii
                 end if 

!-----------------------------------------------------------------------
!                a valid value was found.
!-----------------------------------------------------------------------

                 if (data_src(iii,jjj) /= undef_value) then
                   data_mdl(i,j) = float(data_src(iii,jjj))/float(scaling_fac)
                   write (6, 6000) i, j, krad
                   cycle ILOOP
                 end if

               end if

             end if

           enddo 
           enddo 

         enddo SPIRAL_SEARCH
 
!-----------------------------------------------------------------------
!        the circular search failed. therefore assign a default value.
!-----------------------------------------------------------------------

         data_mdl(i,j) = default_value
         write (6, 6100) i, j

       end if

     end if LAND_TEST

   enddo ILOOP
 enddo JLOOP

 return

!-----------------------------------------------------------------------
! format statements.
!-----------------------------------------------------------------------

 6000 FORMAT (1X, '-- CIRCULAR SEARCH AT POINT ', I4, 1X, I4, ' ITERATIONS ',I3) 

 6100 FORMAT (1X, '-- DEFAULT VALUE ASSIGNED AT POINT ', 1X, I4, 1X, I4) 
   
 end subroutine interp_bilinear
