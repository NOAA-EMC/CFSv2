!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 3.0                         !
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
! !ROUTINE: lis_log_msg.F90
!
! !DESCRIPTION:
!  This routine formats a given message by prepending a
!  time stamp and appending the process id number.  This newly formatted
!  message is then written to standard out.
!
! !REVISION HISTORY:
!  12 Mar 2004: James Geiger; Initial version
!
! !INTERFACE:
subroutine lis_log_msg(msg)

! !USES:
   use spmdMod, only : iam

   implicit none

!INPUT PARAMETERS:
   character(len=*), intent(in) :: msg

!LOCAL VARIABLES:
   character(len=8)  :: date
   character(len=10) :: time
   character(len=5)  :: zone
   integer, dimension(8) :: values
!EOP

!BOC
   call date_and_time(date,time,zone,values)

   print*,date(1:4),'-',date(5:6),'-',date(7:8),'T',  &
          time(1:2),':',time(3:4),':',time(5:10),' ', &
          trim(msg),' (',iam,')'
!EOC
end subroutine lis_log_msg

!BOP
! !ROUTINE: lis_log_blocked_msg
!
! !DESCRIPTION:
!  This routine call lis_log_msg to print a time-stamped message, and then
!  it waits at an mpi_barrier.
!
! !REVISION HISTORY:
!  16 Aug 2004: James Geiger; Initial version
!
! !INTERFACE:
subroutine lis_log_blocked_msg(msg)

#include "misc.h"

! !USES:
   use spmdMod

   implicit none

!INPUT PARAMETERS:
   character(len=*), intent(in) :: msg

   integer :: ierr

   call lis_log_msg(msg)

#if ( defined SPMD )
   if ( npes > 1 ) then
      call lis_log_msg('DBG: lis_log_blocked_msg -- waiting at barrier')
      call MPI_BARRIER(MPI_COMM_WORLD,ierr)
      call lis_log_msg('DBG: lis_log_blocked_msg -- passed barrier')
   endif
#endif

!EOC
end subroutine lis_log_blocked_msg
