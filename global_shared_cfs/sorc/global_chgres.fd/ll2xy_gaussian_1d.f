 subroutine ll2xy_gaussian_1d(kgds_input, ijmdl_output,    &
                              lats_output, lons_output,    &
                              xindx_wrt_input_grid,        &
                              yindx_wrt_input_grid)
!$$$  subprogram documentation block
!
! subprogram:   ll2xy_gaussian_1d
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  given the lat/lons on a gaussian grid, find the
!   corresponding x/y values on another gaussian grid.
!
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage:  ll2xy_gaussian_1d(kgds_input, ijmdl_output,
!                           lats_output, lons_output,
!                           xindx_wrt_input_grid,
!                           yindx_wrt_input_grid)
!
!   input argument list:
!     ijmdl_output   - dimension of output grid
!     kgds_input     - grib gds info of input grid
!     lats_output    - latitudes of output grid
!     lons_output    - longitudes of output grid
!
!   output argument list:
!     xindx_wrt_input_grid - x index with respect to the
!                            input grid
!     yindx_wrt_input_grid - y index with respect to the
!                            input grid
!
! remarks: works on 1-d arrays.  (want to keep this one)
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 use gdswzd_mod

 implicit none

 integer, intent(in)          :: ijmdl_output
 integer, intent(in)          :: kgds_input(200)
 integer                      :: lmap
 integer                      :: lrot
 integer                      :: nret

 real, allocatable            :: area(:)
 real, allocatable            :: crot(:)
 real, intent(inout)          :: lats_output(ijmdl_output)
 real, intent(inout)          :: lons_output(ijmdl_output)
 real, allocatable            :: srot(:)
 real, allocatable            :: xlat(:)
 real, allocatable            :: xlon(:)
 real, allocatable            :: ylat(:)
 real, allocatable            :: ylon(:)
 real, intent(out)            :: xindx_wrt_input_grid(ijmdl_output)
 real, intent(out)            :: yindx_wrt_input_grid(ijmdl_output)

 lmap = 0
 lrot = 0

 xindx_wrt_input_grid = -999.9
 yindx_wrt_input_grid = -999.9

 allocate(crot(ijmdl_output)) ! dummy argument when lrot /=1
 allocate(srot(ijmdl_output)) ! dummy argument when lrot /=1
 allocate(xlon(ijmdl_output)) ! dummy argument when lmap /=1
 allocate(xlat(ijmdl_output)) ! dummy argument when lmap /=1
 allocate(ylon(ijmdl_output)) ! dummy argument when lmap /=1
 allocate(ylat(ijmdl_output)) ! dummy argument when lmap /=1
 allocate(area(ijmdl_output)) ! dummy argument when lmap /=1

!-----------------------------------------------------------------------
! call gdswzd to convert the lat/lon of the input grid points to the
! corresponding x/y coordinates of the output grid.
!
! the "-1" argument tells routine to return x/y coordinates.
! the "-999.9" is a flag value for invalid data.
! the x/y coordinates (xindx_wrt_input_grid,yindx_wrt_input_grid) are
! floating point.
!-----------------------------------------------------------------------

!call gdswzd(kgds_input, -1, ijmdl_output, -999.9, xindx_wrt_input_grid, yindx_wrt_input_grid,        &
!            lons_output, lats_output, nret, lrot, crot, srot, lmap,  &
!            xlon, xlat, ylon, ylat, area )
! SUBROUTINE GDSWZD(KGDS,IOPT,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET, &
!                   CROT,SROT,XLON,XLAT,YLON,YLAT,AREA)


 call gdswzd(kgds_input, -1, ijmdl_output, -999.9, xindx_wrt_input_grid, yindx_wrt_input_grid,        &
             lons_output, lats_output, nret, crot, srot, xlon, xlat, ylon, ylat, area )

 deallocate(crot)
 deallocate(srot)
 deallocate(xlon)
 deallocate(xlat)
 deallocate(ylon)
 deallocate(ylat)
 deallocate(area)

 return

 end subroutine ll2xy_gaussian_1d
