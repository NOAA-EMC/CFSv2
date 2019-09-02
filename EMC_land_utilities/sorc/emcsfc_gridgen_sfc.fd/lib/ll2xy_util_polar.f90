 module ll2xy_util_polar
!$$$ module documentation block
!
! module:  ll2xy_util_polar
!   prgmmr: gayno          org: w/np2           date: 2014-06-04
!
! $Revision$
!
! abstract: routines that transform from lat/lon to
!           i/j for a polar stereographic grid
!
! program history log:
! 2014-06-04  gayno     - initial version
!
! usage: use ll2xy_util_polar
!
! remarks: none
!
!$$$
 real, parameter, private :: pi=3.14159265358979
 real, parameter, private :: dpr=180./pi

 real, private            :: de, xp, yp

 contains

 subroutine ll2xy_polar_init(rlat1, rlon1, orient, h, dxs, dys)

!$$$  subprogram documentation block
!
! subprogram:   ll2xy_polar_init
!   prgmmr: gayno          org: w/np2     date: 2014-06-04
!
! abstract:  initialize variables required for routine ll2xy_polar
!
! program history log:
! 2014-06-04  gayno    - initial version
!
! usage:  ll2xy_polar_init(rlat1, rlon1, orient, h, dxs, dys)
!
!   input argument list:
!     orient         - orientation longitude (degrees)
!     dxs/dys        - x/y grid resolution in meters
!     h              - hemisphere flag (1.0-nh, -1.0-sh)
!     rlat1/rlon1    - lat/lon of corner point (degrees)
!
!   output argument list:
!     n/a
!
! files: none
!
! condition codes: none
!
! remarks:  Taken from gdswiz.  Assumes spherical earth
!           This routine must be called before ll2xy_polar.
!
!$$$

 implicit none

 REAL, PARAMETER     :: RERTH=6.3712E6
 REAL, PARAMETER     :: SLAT=60.0  ! standard latitude according
                                    ! to grib 1 standard

 real, intent(in)    :: rlat1, rlon1, orient, h, dxs, dys

 real                :: dr

 DE=(1.+SIN(SLAT/DPR))*RERTH
 DR=DE*COS(RLAT1/DPR)/(1+H*SIN(RLAT1/DPR))
 XP=1-H*SIN((RLON1-ORIENT)/DPR)*DR/DXS
 YP=1+COS((RLON1-ORIENT)/DPR)*DR/DYS

 return

 end subroutine ll2xy_polar_init

 subroutine ll2xy_polar(orient, dxs, dys, h, &
                        rlat, rlon, xpts, ypts)

!$$$  subprogram documentation block
!
! subprogram:   ll2xy_polar
!   prgmmr: gayno          org: w/np2     date: 2007-05-14
!
! abstract:  given a lat/lon, find the
!   corresponding x/y indices on a polar sterographic grid.
!
! program history log:
! 2007-05-14  gayno    - initial version
!
! usage:  ll2xy_polar(orient, dxs, dys, h, &
!                     rlat, rlon, xpts, ypts)
!
!   input argument list:
!     orient         - orientation longitude (degrees)
!     dxs/dys        - x/y grid resolution in meters
!     h              - hemisphere flag (1.0-nh, -1.0-sh)
!     rlat/rlon      - lat/lon of point of interest (degrees)
!
!   output argument list:
!     xpts/ypts      - x/y indices on the polar grid
!
! files: none
!
! condition codes: none
!
! remarks:  Taken from gdswiz.  Assumes spherical earth
!           Must call ll2xy_polar_init first.
!
!$$$

 implicit none

 real, intent(in)  :: rlon, rlat, dxs, dys, orient, h
 real, intent(out) :: xpts, ypts

 real              :: dr

 DR=DE*TAN((90.0-H*RLAT)/2/DPR)
 XPTS=XP+H*SIN((RLON-ORIENT)/DPR)*DR/DXS
 YPTS=YP-COS((RLON-ORIENT)/DPR)*DR/DYS

 return

 end subroutine ll2xy_polar

 end module ll2xy_util_polar
