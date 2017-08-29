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
!  !MODULE: domain_module.F90
! 
!  !DESCRIPTION: 
!   This module contains 
!   interfaces and subroutines that controls the incorporation
!   of new domains
!   
!  !REVISION HISTORY: 
!  17Feb04    Sujay Kumar  Initial Specification
! 
!EOP
module domain_module

contains
!BOP
! !ROUTINE: forcing_init
!
! !DESCRIPTION:
! Sets up functions for defining domain initializations
!
! !INTERFACE:
  subroutine define_domains()
! !USES:
  use domain_pluginMod, only : domain_plugin
  use landcover_pluginMod, only : landcover_plugin
  use elevdiff_pluginMod, only : elevdiff_plugin

!EOP
    call domain_plugin
    call landcover_plugin
    call elevdiff_plugin
  end subroutine define_domains
!BOP
! !ROUTINE: 
!
! !DESCRIPTION:
! Makes the domain
! 
! 
! !INTERFACE: 
  subroutine domain_init(domain)
! !USES:
    integer, intent(in) :: domain
!EOP
!BOC
    call makedomain(domain)
  end subroutine domain_init
!EOP


!BOP
! !ROUTINE: read_domain
!
! !DESCRIPTION:
!  calls the appropriate domain
! 
! !INTERFACE: 
  subroutine read_domain(domain)
! !USES:
    integer, intent(in) :: domain
!EOP
!BOC
    call readinput(domain)
  end subroutine read_domain
!EOP
  
end module domain_module
