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
! !MODULE: domain_pluginMod.F90  
! 
! !DESCRIPTION: 
!   This module contains the definition of the functions used for
!   defining routines that initialize various domains. 
!   
! !REVISION HISTORY: 
!  17Feb2004;   Sujay Kumar  Initial Specification
! 
! !INTERFACE:
module domain_pluginMod
!EOP  
  implicit none
  
contains
!BOP
! !ROUTINE: domain_plugin
!
! !DESCRIPTION:
!
! This is a custom-defined plugin point for introducing a new domain. 
! The specific computations that needs to be performed to initilaize
! a new domain/projection need to be defined in this method.
!
! 
! !INTERFACE:
  subroutine domain_plugin
! !USES:

!EOP
    external readdomain_default
    external createtiles_latlon
    external maketiles_gaussian
#if(defined OPENDAP)
    external maketiles_gds
#endif
   
!BOC
#if ( defined OPENDAP )
   call registerdomain(1,maketiles_gds) 
   call registerinput(1,readdomain_default) 
#else
!   call registerdomain(1,createtiles_latlon)
   call registerdomain(1, maketiles_gaussian) 
   call registerinput(1,readdomain_default)
#endif
!EOC         
  end subroutine domain_plugin
end module domain_pluginMod
