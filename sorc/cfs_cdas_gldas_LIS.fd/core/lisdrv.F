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
!  !ROUTINE: lisdrv.F90 - Main program for LIS
!  
!  !DESCRIPTION: 
!  Main driver program for LIS. It initializes the boundary conditions, 
!  allocates memory for the required variables, sets up appropriated 
!  model parameters, performs the I/O for forcing and land models, 
!  in addition to calling the appropriate land model over different
!  time steps and geographical domains. 
!  \subsection{Function calls for main routines: }
! 
!  The function calls are: 
!  \begin{description}
!  \item[LIS\_domain\_init]  
!      Initializes the domain variables
!  \item[LIS\_allocate\_memory] 
!      Allocates memory for modules, variables 
!  \item[LIS\_lsm\_init] 
!     Initializes land surface model run parameters 
!  \item[LIS\_baseforcing\_init]
!     Initializes model forcing variables
!  \item[LIS\_readrestart]
!     Reads the restart files
!  \item[LIS\_setuplsm]
!     Completes initialization of the land surface model
!  \item[LIS\_ticktime]
!     Manages the advancement of time
!  \item[LIS\_endofrun]
!     Checks if the end of simulation is reached
!  \item[LIS\_get\_base\_forcing]
!     Reads, interpolates model forcing
!  \item[LIS\_force2tile]
!     Transfers grid forcing to model tiles. 
!  \item[LIS\_lsm\_main]
!     Executes land surface model runs.
!  \item[LIS\_lsm\_output]
!     Writes land surface model output
!  \item[LIS\_write\_restart]
!     Writes restart files
!  \end{description}
!
!  !REVISION HISTORY: 
!  14Nov02    Sujay Kumar  Initial Specification
! 
!EOP
program lisdrv
!BOP
! !USES:       
  use precision 
  use lisdrv_module
  use lsm_module
  use baseforcing_module
  use obsprecipforcing_module
  use obsradforcing_module
  use spmdMod
!EOP
  implicit none
  integer :: ierr
!BOC
 call lis_log_msg('MSG: lisdrv -- START')
#if ( defined SPMD )
  print*,"DEFINED SPMD"
#endif
  call LIS_config
  call LIS_domain_init
  call LIS_lsm_init
  call LIS_baseforcing_init
  call LIS_obsprecipforcing_init
  call LIS_obsradforcing_init
  call LIS_setuplsm
  call LIS_readrestart

 call lis_log_msg('MSG: lisdrv -- LOOP START')

  do while (.NOT. LIS_endofrun())
  call lis_log_msg('MSG: lisdrv -- LOOP INCREMENT')
     call LIS_ticktime
     call LIS_setDynlsm 
  call lis_log_msg('MSG: lisdrv -- DONE setDynlsm')
     call LIS_get_base_forcing
  call lis_log_msg('MSG: lisdrv -- DONE base_forcing')
     call LIS_get_obsprecip_forcing
  call lis_log_msg('MSG: lisdrv -- DONE obsprecip')
     call LIS_get_obsrad_forcing
  call lis_log_msg('MSG: lisdrv -- DONE obsrad')
     call LIS_force2tile 
  call lis_log_msg('MSG: lisdrv -- DONE force2tile')
     call LIS_lsm_main
  call lis_log_msg('MSG: lisdrv -- DONE lsm')
     call LIS_lsm_output 
  call lis_log_msg('MSG: lisdrv -- DONE output')
     call LIS_writerestart
  call lis_log_msg('MSG: lisdrv -- DONE restart')
  enddo

!EOC
  call lis_log_msg('MSG: lisdrv -- Done')
#if ( defined SPMD )
  call MPI_FINALIZE(ierr)
#endif 
!  call endrun

end program lisdrv
