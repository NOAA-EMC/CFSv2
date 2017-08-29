!-------------------------------------------------------------------------
! NASA Goddard Space Flight Center Land Information System (LIS) V3.0
! Released May 2004
!
! See SOFTWARE DISTRIBUTION POLICY for software distribution policies
!
! The LIS source code and documentation are in the public domain,
! available without fee for educational, research, non-commercial and
! commercial purposes.  Users may distribute the binary or source
! code to third parties provided this statement appears on all copies and
! that no charge is made for such copies.
!
! NASA GSFC MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE
! SOFTWARE FOR ANY PURPOSE.  IT IS PROVIDED AS IS WITHOUT EXPRESS OR
! IMPLIED WARRANTY.  NEITHER NASA GSFC NOR THE US GOVERNMENT SHALL BE
! LIABLE FOR ANY DAMAGES SUFFERED BY THE USER OF THIS SOFTWARE.
!
! See COPYRIGHT.TXT for copyright details.
!
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: define_gds.F90
!  
! !DESCRIPTION: 
!
! Assigns a grid definition section (GDS) array appropriate to the 
! global resolution used.
!
! !REVISION HISTORY: 
!
!  20 Jul 2001: Urszula Jambor; Initial code
!  12 Feb 2002: Urszula Jambor; Added latmax variable assignment
!  06 Mar 2002: Urszula Jambor; Added 1 & 1/2 degree resolution GDS arrays
!  24 Feb 2004: James Geiger; Stripped routine down so it only updates
!                             values needed by the GrADS-DODS server
! 
! !INTERFACE:
subroutine define_gds ( lis )
! !USES:
  use lis_module      ! LDAS non-model-specific 1-D variables
#if ( defined OPENDAP )
  use opendap_module, only : parm_nc, parm_nr,         &
                             output_slat, output_nlat, &
                             output_wlon, output_elon, &
                             ciam
  implicit none
! !ARGUMENTS:
  type (lisdec):: lis              
!EOP
!BOC
!-----------------------------------------------------------------------
!      kgds(1) = 4		!Input grid type (4=Gaussian)
!      kgds(2) = 128		!Number of points on a lat circle
!      kgds(3) = 64		!Number of points on a meridian
!      kgds(4) = -87864		!Latitude of origin x1000
!      kgds(5) = 0		!Longitude of origin x1000
!      kgds(6) = 128		!8 bits (1 byte) related to resolution
!				!(recall that 10000000 = 128), Table 7
!      kgds(7) = 87864		!Latitude of extreme point x1000
!      kgds(8) = -2812		!Longitude of extreme point x1000
!      kgds(9) = 2812		!N/S direction increment x1000
!      kgds(10) = 32		!(Gaussian) # lat circles pole-equator
!      kgds(11) = 64		!8 bit scanning mode flag (Table 8)
!-----------------------------------------------------------------------
     lis%d%gridDesc(2) = parm_nc
     lis%d%gridDesc(3) = parm_nr
     lis%d%gridDesc(4) = output_slat
     lis%d%gridDesc(5) = output_wlon
     lis%d%gridDesc(7) = output_nlat
     lis%d%gridDesc(8) = output_elon

     print*, 'DBG: define_gds -- lis%d%gridDesc(2)', lis%d%gridDesc(2), &
             ' ( ',ciam,' )'
     print*, 'DBG: define_gds -- lis%d%gridDesc(3)', lis%d%gridDesc(3), &
             ' ( ',ciam,' )'
     print*, 'DBG: define_gds -- lis%d%gridDesc(4)', lis%d%gridDesc(4), &
             ' ( ',ciam,' )'
     print*, 'DBG: define_gds -- lis%d%gridDesc(5)', lis%d%gridDesc(5), &
             ' ( ',ciam,' )'
     print*, 'DBG: define_gds -- lis%d%gridDesc(7)', lis%d%gridDesc(7), &
             ' ( ',ciam,' )'
     print*, 'DBG: define_gds -- lis%d%gridDesc(8)', lis%d%gridDesc(8), &
             ' ( ',ciam,' )'
#endif
!EOC
end subroutine define_gds

