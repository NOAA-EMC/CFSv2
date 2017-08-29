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
! !MODULE: obsradforcing_module.F90
! 
! !DESCRIPTION: 
!   This module contains 
!   interfaces and subroutines that controls the incorporation
!   of observed radiation forcing
!   
! !REVISION HISTORY: 
!  14Nov02    Sujay Kumar  Initial Specification
! 
! !INTERFACE:
module obsradforcing_module
  implicit none
! !ARGUMENTS:
  real, pointer :: obswdata1(:)
  real, pointer :: obswdata2(:)
  real, pointer :: oblwdata1(:)
  real, pointer :: oblwdata2(:)      
  integer :: sstat1, sstat2, lstat1,lstat2
    
!EOP

!BOP
! !ROUTINE: LIS_obsradforcing_init
! 
! !INTERFACE:      
  interface LIS_obsradforcing_init
     module procedure rad_forcing_init
  end interface
!EOP

!BOP
! !ROUTINE: LIS_get_obsrad_forcing
! 
! !INTERFACE:
  interface LIS_get_obsrad_forcing
     module procedure get_obsrad_forcing
  end interface
!EOP
     
contains
!BOP
! !ROUTINE: rad_forcing_init
! 
! !DESCRIPTION:
! 
!  Allocates memory for variables required for radiation forcing
!  interpolation
! 
! !INTERFACE:
  subroutine rad_forcing_init()
! !USES:
    use lisdrv_module, only:lis
    use grid_spmdMod
    use radforcing_pluginMod, only :radforcing_plugin
!EOP
    implicit none 
!BOC
    if(lis%f%radsrc.gt.0) then 
       call radforcing_plugin
#if ( defined OPENDAP )
          call defnatresrad(lis%f%radsrc) 
          allocate(obswdata1(gdi(iam)))
          allocate(obswdata2(gdi(iam)))
          allocate(oblwdata1(gdi(iam)))
          allocate(oblwdata2(gdi(iam)))
#else
       if(masterproc) then 
          call defnatresrad(lis%f%radsrc) 
          allocate(obswdata1(lis%d%ngrid))
          allocate(obswdata2(lis%d%ngrid))
          allocate(oblwdata1(lis%d%ngrid))
          allocate(oblwdata2(lis%d%ngrid))
       else
          allocate(obswdata1(gdi(iam)))
          allocate(obswdata2(gdi(iam)))
          allocate(oblwdata1(gdi(iam)))
          allocate(oblwdata2(gdi(iam)))
       endif
#endif
    endif
!EOC
  end subroutine rad_forcing_init
!BOP
! !ROUTINE: get_obsrad_forcing
!
! !DESCRIPTION:
! 
! Calls the routines that read observed radiation forcing methods
! 
! !INTERFACE:
  subroutine get_obsrad_forcing
! !USES:
    use lisdrv_module, only: lis, grid
    use grid_spmdMod
    use driverpardef_module
!EOP
    integer :: i,ier
!BOC
!    sstat1 = 0
!    sstat2 = 0
!    lstat1 = 0
!    lstat2 = 0
    if(lis%f%radsrc.gt.0) then 
#if ( defined OPENDAP )
       call getrad(lis%f%radsrc)
#else
#if (defined SPMD)
       call MPI_GATHERV(grid(1:gdi(iam)),gdi(iam), &
            MPI_GRID_STRUCT,grid,gdi,gdisp,MPI_GRID_STRUCT, & 
            0,MPI_COMM_WORLD, ier)
#endif 
       if(masterproc) then 
          call getrad(lis%f%radsrc)
       endif
#endif
#if ( ! defined OPENDAP )
#if (defined SPMD)
       call MPI_BCAST(sstat1, 1,MPI_INTEGER,0, & 
            MPI_COMM_WORLD, ier)
       call MPI_BCAST(sstat2, 1,MPI_INTEGER,0, & 
            MPI_COMM_WORLD, ier)
       call MPI_BCAST(lstat1, 1,MPI_INTEGER,0, & 
            MPI_COMM_WORLD, ier)
       call MPI_BCAST(lstat2, 1,MPI_INTEGER,0, & 
            MPI_COMM_WORLD, ier)
       call MPI_BCAST(lis%f%findagrtime1,1,MPI_INTEGER,0, & 
            MPI_COMM_WORLD, ier)
       call MPI_BCAST(lis%f%findagrtime2,1,MPI_INTEGER,0, & 
            MPI_COMM_WORLD, ier)
       if ( lis%f%findagrtime1 == 1 .or. lis%f%findagrtime2 == 1 ) then  
          if ( npes > 1 ) then
             call scatter_rad_data()
          endif
       endif
#endif 
#endif
       call timeinterprad(lis%f%radsrc)
    endif
!EOC
  end subroutine get_obsrad_forcing
!BOP
! !ROUTINE: scatter_rad_data
! 
! !DESCRIPTION:
!  
!  Distributes radiation forcing data on to the compute node.
! 
! !INTERFACE:
  subroutine scatter_rad_data()
! !USES:
    use grid_spmdMod
    use lisdrv_module, only : lis
!EOP
    integer :: ier,i
#if (defined SPMD)
    call MPI_SCATTERV(obswdata1,gdi, gdisp, MPI_REAL, & 
         obswdata1,gdi(iam),MPI_REAL,0,MPI_COMM_WORLD,ier)
    call MPI_SCATTERV(obswdata2,gdi, gdisp, MPI_REAL, & 
         obswdata2,gdi(iam),MPI_REAL,0,MPI_COMM_WORLD,ier)
    call MPI_SCATTERV(oblwdata1,gdi, gdisp, MPI_REAL, & 
         oblwdata1,gdi(iam),MPI_REAL,0,MPI_COMM_WORLD,ier)
    call MPI_SCATTERV(oblwdata2,gdi, gdisp, MPI_REAL, & 
         oblwdata2,gdi(iam),MPI_REAL,0,MPI_COMM_WORLD,ier)
#endif 
  end subroutine scatter_rad_data
  
end module obsradforcing_module
      

