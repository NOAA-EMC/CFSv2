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
#include "absoft.h"
!BOP
!
!  !ROUTINE: absoft_release_cache.F90
!  
!  !DESCRIPTION: 
!   This routine forces the explicit clearing of memory caches when using
!   the absoft compiler. 
!
!  !REVISION HISTORY: 
!  10 Sept 03    Jim Geiger  Initial Specification
! 
! !INTERFACE:
subroutine absoft_release_cache()
!EOP
#if ( defined ABSOFT )
   call release_cache()
#endif
end subroutine absoft_release_cache
