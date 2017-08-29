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
! !MODULE: baseforcing_pluginMod.F90  
! 
! !DESCRIPTION: 
!   This module contains the definition of the functions used for
!   incorporating a new model forcing scheme. 
!   
! !REVISION HISTORY: 
!  11 Dec 03    Sujay Kumar  Initial Specification
! 
! !INTERFACE:
module baseforcing_pluginMod
!EOP  
  implicit none
  
contains
!BOP
! !ROUTINE: baseforcing_plugin
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
  subroutine baseforcing_plugin
! !USES:
!    use geosdomain_module
    use gdasdomain_module
!    use ecmwfdomain_module
!    use nldasdomain_module
!    use gswpdomain_module
!    use bergdomain_module
!EOP
    external getgeos, getgdas, getecmwf
    external time_interp_geos, time_interp_gdas, time_interp_ecmwf
    external getnldas
    external time_interp_nldas
!    external getgswp
!    external time_interp_gswp
    external getberg
    external time_interp_berg
!BOC
    call registerget(1,getgdas)
  !  call registerget(2,getgeos)
  !  call registerget(3,getecmwf)
  !  call registerget(4,getnldas)
  !  call registerget(5,getgswp)
  !  call registerget(6,getberg)
    
    call registerdefnat(1,defnatgdas)
  !  call registerdefnat(2,defnatgeos)
  !  call registerdefnat(3,defnatecmwf)
  !  call registerdefnat(4,defnatnldas)
  !  call registerdefnat(5,defnatgswp)
  !  call registerdefnat(6,defnatberg)

    call registertimeinterp(1,time_interp_gdas)
  !  call registertimeinterp(2,time_interp_geos)
  !  call registertimeinterp(3,time_interp_ecmwf)
  !  call registertimeinterp(4,time_interp_nldas)
  !  call registertimeinterp(5,time_interp_gswp)
  !  call registertimeinterp(6,time_interp_berg)
!EOC
  end subroutine baseforcing_plugin
end module baseforcing_pluginMod
