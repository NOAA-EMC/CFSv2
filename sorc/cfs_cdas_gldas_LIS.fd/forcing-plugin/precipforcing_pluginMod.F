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
! !MODULE: precipforcing_pluginMod.F90  
! 
! !DESCRIPTION: 
!   This module contains the definition of the functions used for
!   incorporating a new observed precipitation forcing scheme.
!   
! !REVISION HISTORY: 
!  12 Dec 03    Sujay Kumar  Initial Specification
! 
! !INTERFACE:
module precipforcing_pluginMod
!EOP  
  implicit none
  
contains
!BOP
! !ROUTINE: precipforcing_plugin
!
! !DESCRIPTION:
!
! This is a custom-defined plugin point for introducing a new observed
!  precipitation forcing scheme.
! The interface mandates that the following routines be implemented
! and registered for each of the forcing scheme. 
!
!  \begin{description}
!  \item[retrieval of forcing data]      
!      Routines to retrieve forcing data and to interpolate them. 
!      (to be registered using registerget)
!  \item[definition of native domain]
!      Routines to define the native domain as a kgds array
!      (to be registered using registerdefnatpcp)
!  \end{description}
! Multiple forcing schemes can be 
! included as well, each distinguished in the function table registry
! by the associated forcing index assigned in the card file. 
! 
! !INTERFACE:
  subroutine precipforcing_plugin
! !USES:
  !  use huffdomain_module
  !  use persdomain_module
    use cmapdomain_module
!EOP
    external gethuff
    external getpers
    external getcmap

    external time_interp_huff
    external time_interp_pers
    external time_interp_cmap

!BOC
  !  call registerpget(2,gethuff)
  !  call registerpget(3,getpers)
    call registerpget(4,getcmap)

  !  call registerdefnatpcp(2,defnathuff)
  !  call registerdefnatpcp(3,defnatpers)
    call registerdefnatpcp(4,defnatcmap)

  !  call registerpti(2,time_interp_huff)
  !  call registerpti(3,time_interp_pers)
    call registerpti(4,time_interp_cmap)

  end subroutine precipforcing_plugin
end module precipforcing_pluginMod
