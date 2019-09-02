 module ll2xy_util_bgrid
!$$$ module documentation block
!
! module:  ll2xy_util_bgrid
!   prgmmr: gayno          org: w/np2           date: 2014-06-04
!
! $Revision$
!
! abstract: routines that transform from lat/lon to
!           i/j for a rotated lat/lon b-grid.
!
! program history log:
! 2014-06-04  gayno     - initial version
!
! usage: use ll2xy_util_bgrid
!
! remarks: none
!
!$$$
 implicit none

 real, private :: dtr, ctph0, stph0
 real, private :: dph, dlm, tlm0, sbd, wbd

 contains

 subroutine ll2xy_bgrid_init(lat11, lon11, tph0d, tlm0d, dphd, dlmd)
!$$$  subprogram documentation block
!
! subprogram:   ll2xy_bgrid_init
!   prgmmr: gayno          org: w/np2     date: 2014-06-04
!
! abstract: initialize some variables for routine ll2xy_bgrid
!
! program history log:
! 2014-06-04  gayno    - initial version
!
! usage: call ll2xy_bgrid_init(lat11, lon11, tph0d, tlm0d, dphd, dlmd)`
!
!   input argument list:
!     lat11          - corner point latitude (degrees)
!     lon11          - corner point longitude (degrees)
!     dlmd           - delta x of bgrid (degrees)
!     dphd           - delta y of bgrid (degrees)
!     tph0d          - latitude of rotation (degrees)
!     tlm0d          - longtude of rotation (degrees)
!
!   output argument list:
!     n/a
!
! files: none
!
! condition codes: none
!
! remarks:  Must be run before routine ll2xy_bgrid is invoked.
!
!$$$

 implicit none

 real, intent(in)   :: lat11, lon11, tph0d, tlm0d, dphd, dlmd

 real, parameter    :: one = 1.0
 real, parameter    :: r180 = 180.0

 real               :: pi, tph0, lat11r, lon11r
 real               :: relmi, crlmi, srlmi
 real               :: anum, cc, cph, denom, sph

 pi = acos(-one)
 dtr = pi / r180
 dph=dphd*dtr
 dlm=dlmd*dtr
 tph0=tph0d*dtr
 tlm0=tlm0d*dtr
 stph0=sin(tph0)
 ctph0=cos(tph0)
 lat11r = lat11 * dtr
 lon11r = lon11 * dtr
 relmi=tlm0-lon11r
 srlmi=sin(relmi)
 crlmi=cos(relmi)
 SPH=SIN(lat11r)
 CPH=COS(lat11r)
 CC=CPH*CRLMI
 ANUM=CPH*SRLMI
 DENOM=CTPH0*CC+STPH0*SPH
 wbd =-ATAN2(ANUM,DENOM)
 sbd =ASIN(CTPH0*SPH-STPH0*CC)

 return

 end subroutine ll2xy_bgrid_init

 subroutine ll2xy_bgrid(lat, lon, ii, jj)

!$$$  subprogram documentation block
!
! subprogram:   ll2xy_bgrid
!   prgmmr: gayno          org: w/np2     date: 2014-06-04
!
! abstract: for a given lat/lon, find the nearest
!   neighbor point on a rotated lat/lon b-grid.
!
! program history log:
! 2014-06-04  gayno    - initial version
!
! usage: call ll2xy_bgrid(lat, lon, ii, jj)
!
!   input argument list:
!     lat          - latitude of grid point (degrees)
!     lon          - longitude of grid point (degrees)
!
!   output argument list:
!     ii            - nearest i point
!     jj            - nearest j point
!
! files: none
!
! condition codes: none
!
! remarks: For best results, compile with 8 byte floats.
!          Routine ll2xy_bgrid_init must be run first to
!          initialize some constants.
!
!$$$

 implicit none

 integer, intent(out) :: ii, jj

 real, intent(in)     :: lat, lon

 real                 :: latr, lonr
 real                 :: relmi, srlmi, crlmi
 real                 :: sph, cph, cc, anum
 real                 :: denom, tlon, tlat
 real                 :: x, y

 latr = lat * dtr
 lonr = lon * dtr
 relmi=tlm0-lonr
 srlmi=sin(relmi)
 crlmi=cos(relmi)
 sPH=SIN(latr)
 CPH=COS(latr)
 CC=CPH*CRLMI
 ANUM=CPH*SRLMI
 DENOM=CTPH0*CC+STPH0*SPH
 TLON=-ATAN2(ANUM,DENOM)
 TLAT=ASIN(CTPH0*SPH-STPH0*CC)
 y =  ((tlat - sbd) / dph) + 1.0
 x =  ((tlon - wbd) / dlm) + 1.0
 ii = nint(x)
 jj = nint(y)

 return

 end subroutine ll2xy_bgrid

 end module ll2xy_util_bgrid
