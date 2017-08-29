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
!  !ROUTINE: endrun.F90
!  
!  !DESCRIPTION: 
!  Routine to be called to terminate the program. This routines 
!  flushes the output streams and aborts the mpi processes.
!
!  !REVISION HISTORY: 
!  14Nov02    Sujay Kumar  Initial Specification
!
#include <misc.h>
! !INTERFACE:
subroutine endrun
! !USES: 
#if (defined SPMD)
  use mpishorthand, only: MPI_COMM_WORLD
#endif
!EOP
  implicit none
!BOC
  write(6,*)'endrun is being called'
  call flush( 6 )   ! Flush all output to standard output
#if (defined SPMD) 
  call mpi_abort (MPI_COMM_WORLD, 1)  
#else
  call abort
#endif
!EOC   
end subroutine endrun
