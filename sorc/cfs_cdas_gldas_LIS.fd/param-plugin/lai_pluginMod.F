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
! !MODULE: lai_pluginMod.F90  
! 
! !DESCRIPTION: 
!   This module contains the definition of the functions used for
!   incorporating a new model forcing scheme. 
!   
! !REVISION HISTORY: 
!  11 Dec 03    Sujay Kumar  Initial Specification
! 
! !INTERFACE:
module lai_pluginMod
!EOP  
  implicit none
  
contains
!BOP
! !ROUTINE: lai_plugin
!
! !DESCRIPTION:
!
! This is a custom-defined plugin point for introducing a new forcing scheme.
! The interface mandates that the following routines be implemented
! and registered for each model forcing scheme. 
!
!  \begin{description}
!  \item[retrieval of forcing data]      
!      Routines to retrieve forcing data and to interpolate them. 
!      (to be registered using registerget)
!  \item[definition of native domain]
!      Routines to define the native domain as a kgds array
!      (to be registered using registerdefnat)
!  \item[temporal interpolation] 
!      Interpolate forcing data temporally. 
!      (to be registered using registertimeinterp)
!  \end{description}
! Multiple forcing schemes can be 
! included as well, each distinguished in the function table registry
! by the associated forcing index assigned in the card file. 
! 
! !INTERFACE:
  subroutine lai_plugin
    external read_avhrrlai, read_avhrrsai
! !USES:
    call registerreadlai(2,read_avhrrlai)    
    call registerreadsai(2,read_avhrrsai)
!EOC
  end subroutine lai_plugin
end module lai_pluginMod
