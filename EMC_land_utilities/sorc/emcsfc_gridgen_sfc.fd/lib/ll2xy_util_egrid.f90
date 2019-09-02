 module ll2xy_util_egrid
!$$$ module documentation block
!
! module:  ll2xy_util_egrid
!   prgmmr: gayno          org: w/np2           date: 2014-06-04
!
! $Revision$
!
! abstract: routines that transform from lat/lon to
!           i/j for a rotated lat/lon e-grid.
!
! program history log:
! 2014-06-04  gayno     - initial version
!
! usage: use ll2xy_util_egrid
!
! remarks: none
!
!$$$
 real, parameter, private :: D2R=1.74532925E-2
 real, parameter, private :: R2D=1./D2R

 integer, private         :: jmt

 real, private            :: dph, dlm, tlm0, ctph0, stph0

 contains

 subroutine ll2xy_egrid_init(jm, dphd, dlmd, tph0d, tlm0d)

!$$$  subprogram documentation block
!
! subprogram:   ll2xy_egrid_init
!   prgmmr: gayno          org: w/np2     date: 2014-06-04
!
! abstract: initialize some variables for routine ll2xy_egrid
!
! program history log:
! 2014-06-04  gayno    - initial version
!
! usage: call ll2xy_egrid_init(jm, dphd, dlmd, tph0d, tlm0d)
!
!   input argument list:
!     jm             - j dimension of grid
!     dlmd           - delta x of egrid (degrees)
!     dphd           - delta y of egrid (degrees)
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
! remarks:  Must be run before routine ll2xy_egrid.
!
!$$$
 implicit none

 integer, intent(in)   :: jm

 real, intent(in)      :: dphd, dlmd, tph0d, tlm0d

 real                  :: tph0

 JMT=JM/2+1
 DPH=DPHD*D2R
 DLM=DLMD*D2R
 TPH0=TPH0D*D2R
 TLM0=TLM0D*D2R
 STPH0=SIN(TPH0)
 CTPH0=COS(TPH0)

 end subroutine ll2xy_egrid_init

 subroutine ll2xy_egrid(glatd, glond, im, &
                        dlmd, dphd, ii, jj)
!$$$  subprogram documentation block
!
! subprogram:   ll2xy_egrid
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: for a given lat/lon, find the nearest
!   neighbor point on an nmm rotated lat/lon e-grid.
!
! program history log:
! 2005-05-20  gayno    - initial version
! 2006-08-09  gayno    - improved calculation of transformed lat/lon
!
! usage: call ll2xy_egrid(glatd, glond, im,  &
!                         dlmd, dphd, ii, jj)
!
!   input argument list:
!     dlmd           - delta x of egrid (degrees)
!     dphd           - delta y of egrid (degrees)
!     glatd          - latitude of grid point (degrees)
!     glond          - longitude of grid point (degrees)
!     im             - i dimension of egrid
!
!   output argument list:
!     ii             - nearest i point
!     jj             - nearest j point
!
! files: nonoe
!
! condition codes: none
!
! remarks: For better results, compile with 8 byte floats.
!          Routine ll2xy_egrid_init must be run first.
!
!$$$
 implicit none

 integer, intent(in)           :: im
 integer, intent(out)          :: ii, jj

 real, intent(in)              :: dlmd
 real, intent(in)              :: dphd
 real, intent(in)              :: glatd
 real, intent(in)              :: glond

 integer                       :: ncol
 integer                       :: nrow

 real                          :: col, row
 real                          :: d1, d2
 real                          :: dlm1, dlm2
 real                          :: glat, glon
 real                          :: tlat, tlat1, tlat2
 real                          :: tlon, tlon1, tlon2
 real                          :: RELMI, SRLMI, CRLMI
 real                          :: SPH, CPH, CC, ANUM, DENOM

!-----------------------------------------------------------------
!***
!***  CONVERT FROM GEODETIC TO TRANSFORMED COORDINATES (DEGREES)
!***

 GLAT=GLATD*D2R
 GLON=GLOND*D2R
 RELMI=GLON-TLM0
 SRLMI=SIN(RELMI)
 CRLMI=COS(RELMI)
 SPH=SIN(GLAT)
 CPH=COS(GLAT)
 CC=CPH*CRLMI
 ANUM=CPH*SRLMI
 DENOM=CTPH0*CC+STPH0*SPH
 TLON=-ATAN2(ANUM,DENOM)*R2D
 TLAT=ASIN(CTPH0*SPH-STPH0*CC)*R2D
!
!      WRITE(6,50)TLAT,TLON
! 50 FORMAT(' TRANSFORMED LATITUDE IS',F8.3, &
!                  4X,'LONGITUDE IS',F8.3)
!***
!***  FIND THE K VALUE OF THE NEAREST H POINT
!***
 ROW=TLAT/DPHD+JMT
 COL=TLON/DLMD+IM
 NROW=INT(ROW)
 NCOL=INT(COL)
 TLAT=TLAT*D2R
 TLON=TLON*D2R
!***
!***  FIRST CONSIDER THE SITUATION WHERE THE POINT X IS AT
!***
!***              V      H
!***
!***
!***                 X
!***              H      V
!***
 IF(MOD(NROW,2).EQ.1.AND.MOD(NCOL,2).EQ.1.OR.    &
    MOD(NROW,2).EQ.0.AND.MOD(NCOL,2).EQ.0)THEN
   TLAT1=(NROW-JMT)*DPH
   TLAT2=TLAT1+DPH
   TLON1=(NCOL-IM)*DLM
   TLON2=TLON1+DLM
   DLM1=TLON-TLON1
   DLM2=TLON-TLON2
   D1=ACOS(COS(TLAT)*COS(TLAT1)*COS(DLM1)+SIN(TLAT)*SIN(TLAT1))
   D2=ACOS(COS(TLAT)*COS(TLAT2)*COS(DLM2)+SIN(TLAT)*SIN(TLAT2))
   IF(D1.GT.D2)THEN
     NROW=NROW+1
     NCOL=NCOL+1
   ENDIF
!***
!***  NOW CONSIDER THE SITUATION WHERE THE POINT X IS AT
!***
!***              H      V
!***
!***
!***                 X
!***              V      H
!***
 ELSE
   TLAT1=(NROW+1-JMT)*DPH
   TLAT2=TLAT1-DPH
   TLON1=(NCOL-IM)*DLM
   TLON2=TLON1+DLM
   DLM1=TLON-TLON1
   DLM2=TLON-TLON2
   D1=ACOS(COS(TLAT)*COS(TLAT1)*COS(DLM1)+SIN(TLAT)*SIN(TLAT1))
   D2=ACOS(COS(TLAT)*COS(TLAT2)*COS(DLM2)+SIN(TLAT)*SIN(TLAT2))
   IF(D1.LT.D2)THEN
     NROW=NROW+1
   ELSE
     NCOL=NCOL+1
   ENDIF
 ENDIF

 JJ=NROW
 II=NCOL/2

 IF(MOD(JJ,2).EQ.1)II=II+1

 return

 end subroutine ll2xy_egrid

 end module ll2xy_util_egrid
