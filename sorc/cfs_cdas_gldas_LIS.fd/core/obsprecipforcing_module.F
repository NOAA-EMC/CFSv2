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
#include <misc.h>
!BOP
!
! !MODULE: obsprecipforcing_module.F90
!
! !DESCRIPTION: This module contains 
!   interfaces and subroutines that controls the incorporation
!   of observed precipitation forcing
! 
! !REVISION HISTORY: 
!  14Nov02    Sujay Kumar  Initial Specification
! 
! !INTERFACE:
module obsprecipforcing_module
! !ARGUMENTS:
  real, pointer :: obsprecip(:)
!EOP
!BOP
! !ROUTINE: LIS_obsprecipforcing_init
! 
! !INTERFACE:
  interface LIS_obsprecipforcing_init
     module procedure precip_forcing_init
  end interface
!EOP

!BOP
! !ROUTINE: LIS_get_obsprecip_forcing
! 
! !INTERFACE:
  interface LIS_get_obsprecip_forcing
     module procedure get_obsprecip_forcing
  end interface
!EOP
     
contains
!BOP
! !ROUTINE: precip_forcing_init
! 
! !DESCRIPTION:
! Initializes the variables required for the spatial interpolation 
! of observed precipitation data
!
! !INTERFACE:
  subroutine precip_forcing_init()
! !USES:
    use lisdrv_module, only :lis
!    use def_ipMod, only : allocate_ip, def_ip_input
    use precipforcing_pluginMod, only : precipforcing_plugin
    use spmdMod, only : masterproc
    use grid_spmdMod
!EOP
    implicit none
!BOC
    if(lis%f%gpcpsrc.gt.0) then 
       call precipforcing_plugin
       if(masterproc) then
          call defnatrespcp(lis%f%gpcpsrc)
          allocate(obsprecip(lis%d%ngrid))
!JESSE 20041225
       else
          allocate(obsprecip(gdi(iam)))
       endif
    endif
!EOC
  end subroutine precip_forcing_init
!BOP
! !ROUTINE: get_precip_forcing
! 
! !DESCRIPTION: 
!  Calls the routines to open, read, and interpolate observed
!  precipitation forcing data
! 
! !INTERFACE:
  subroutine get_obsprecip_forcing
! !USES:
    use lisdrv_module, only: lis, gindex
    use grid_spmdMod
!EOP
!BOC
    if(lis%f%gpcpsrc.gt.0) then 
       if(masterproc) then 
          call glbprecip(lis%f%gpcpsrc)
       endif
!JESSE 20041225
!       if(lis%f%findtime1 ==1 .or.  & 
!            lis%f%findtime2==1) then  
          if(npes > 1) then 
             call scatter_precip_data()
          endif
!       endif
       call timeinterppcp(lis%f%gpcpsrc) !JESSE 20041127
    endif
!EOC
  end subroutine get_obsprecip_forcing
!BOP
! !ROUTINE: scatter_precip_data
! 
! !DESCRIPTION:
! Distributes the precipitation forcing data onto the compute nodes
! 
! !INTERFACE:
  subroutine scatter_precip_data()
! !USES:
    use grid_spmdMod
!EOP
#if (defined SPMD)
 print*,"J---call MPI_SCATTERV ", "iam=", iam
 print*," gdi=", gdi
 print*," gdisp=", gdisp
 print*," gdi(iam)=", gdi(iam)
!jesse, for wcoss
    if ( masterproc ) then
    call MPI_SCATTERV(obsprecip,gdi,gdisp,MPI_REAL, & 
         MPI_IN_PLACE,gdi(iam),MPI_REAL,0,MPI_COMM_WORLD,ier)
    else
    call MPI_SCATTERV(obsprecip,gdi,gdisp,MPI_REAL, &
         obsprecip,gdi(iam),MPI_REAL,0,MPI_COMM_WORLD,ier)
    endif
 print*,"J---call MPI_SCATTERV done iam=", iam
#endif
  end subroutine scatter_precip_data

end module obsprecipforcing_module
