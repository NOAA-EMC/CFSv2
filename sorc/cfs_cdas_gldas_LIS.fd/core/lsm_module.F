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
#include "misc.h"
!BOP
!
! !MODULE: lsm_module.F90
! 
! !DESCRIPTION:
!   This module contains interfaces and subroutines that control
!   land surface model initialization, execution, reading and 
!   writing of restart files and other relevant land surface
!   model computations.
! 
! !REVISION HISTORY: 
!  14Nov02    Sujay Kumar  Initial Specification
! 
! !INTERFACE:
module lsm_module
!EOP  
  implicit none

!BOP
! !ROUTINE: LIS_setuplsm
! 
! !INTERFACE:
  interface LIS_setuplsm
!EOP
     module procedure lsm_setup
  end interface

!BOP
! !ROUTINE: LIS_lsm_main
!
! !INTERFACE:
  interface LIS_lsm_main
!EOP
     module procedure run_lsm
  end interface

!BOP
! !ROUTINE: LIS_force2tile
! 
! !INTERFACE:
  interface LIS_force2tile
!EOP
     module procedure lsm_f2t
  end interface
!BOP
! !ROUTINE: LIS_readrestart
! 
! !INTERFACE:
  interface LIS_readrestart
!EOP
     module procedure lsm_readrestart
  end interface
!BOP
! !ROUTINE: LIS_writerestart
! 
! !INTERFACE:
  interface LIS_writerestart
!EOP
     module procedure lsm_writerestart
  end interface

!BOP
! !ROUTINE: LIS_lsm_output
!
! !INTERFACE:
  interface LIS_lsm_output
!EOP
     module procedure write_output
  end interface

!BOP
! !ROUTINE: LIS_setDynlsm
! 
! !INTERFACE:
  interface LIS_setDynlsm
!EOP
     module procedure setDynParams
  end interface
contains
!BOP
! !INTERFACE:
  subroutine LIS_lsm_init()
! !DESCRIPTION:
! Setup functions for each land surface model
!
! !USES:
    use lisdrv_module, only: lis 
    use lsm_pluginMod
#if ( defined OPENDAP )
    use opendap_module
#endif
    use lis_indices_module
!EOP
!BOC        

! These (opendap_init, lis_set_indices) are done here to prevent a 
! circular module dependency with lisdrv_module
#if ( defined OPENDAP )
    call opendap_init()
#endif
    call lis_set_indices()

    call lsm_plugin

    if ( lis%o%wsingle == 1 ) then
       call lsmini(lis%d%lsm,lis%d%nch)
    else
       call lsmini(lis%d%lsm,lis%d%glbnch)
    endif
!EOC
  end subroutine LIS_lsm_init
!BOP
! !ROUTINE: lsm_tile_allocate
! 
! !DESCRIPTION:
! Allocates memory for land surface model variables
!
! !INTERFACE:       
  subroutine lsm_tile_allocate()
! !USES:

!EOP
!BOC

!EOC
  end subroutine lsm_tile_allocate

!BOP
! !ROUTINE: lsm_setup
!
! !DESCRIPTION:
! Completes land surface model initilaization
! 
! !INTERFACE:
  subroutine lsm_setup()
! !USES:
    use lisdrv_module, only : lis
    use tile_spmdMod, only : masterproc
!EOP
!BOC
    call lsmsetup(lis%d%lsm)
!EOC
  end subroutine lsm_setup

!BOP
! !ROUTINE: run_lsm
!
! !DESCRIPTION:
! Executes land surface model runs
!
! !INTERFACE:            
  subroutine run_lsm()
! !USES:
    use lisdrv_module, only: lis
!EOP
!BOC
    call lsmrun(lis%d%lsm)
!EOC
  end subroutine run_lsm

!BOP
! !ROUTINE: lsm_readrestart
!
! !DESCRIPTION:
! Reads restart files
!
! !INTERFACE:
  subroutine lsm_readrestart()
! !USES:
    use lisdrv_module, only : lis

!EOP
!BOC
    call lsmrestart(lis%d%lsm,1)
!EOC
  end subroutine lsm_readrestart
!BOP
! !ROUTINE: write_output
!
! !DESCRIPTION:
! Writes output of land surface model runs
!
! !INTERFACE:
  subroutine write_output()
! !USES:
    use lisdrv_module, only: lis
!EOP
!BOC
    call lsmoutput(lis%d%lsm)
!EOC
  end subroutine write_output

!BOP
! !ROUTINE: setDynParams
! 
! !DESCRIPTION:
! Updates time dependent land surface model parameters
!
! !INTERFACE:
  subroutine setDynParams()
! !USES:
    use lisdrv_module, only : lis
!EOP
!BOC
    call lsmdynsetup(lis%d%lsm)
!EOC
  end subroutine setDynParams
!BOP
! !ROUTINE: lsm_f2t
! 
! !DESCRIPTION:
! Transfers grid forcing to model tiles.
!
! !INTERFACE:
  subroutine lsm_f2t()
! !USES: 
    use lisdrv_module, only: lis, grid,tile
    use tile_spmdMod
    use grid_spmdMod, only : gdisp
!EOP
    integer :: t, index
!BOC
    do t=1, di_array(iam)
       index = tile(t)%index -gdisp(iam)
       call lsmf2t(lis%d%lsm, t, grid(index)%forcing)
    enddo
!EOC
  end subroutine lsm_f2t
!BOP
! !ROUTINE: lsm_writerestart
! 
! !DESCRIPTION:
! Writes restart files
!
! !INTERFACE:
  subroutine lsm_writerestart()
! !USES:
    use lisdrv_module, only : lis
!EOP
!BOC
    call lsmwrst(lis%d%lsm)
!EOC
  end subroutine lsm_writerestart
    
end module lsm_module
