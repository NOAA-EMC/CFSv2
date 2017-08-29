!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
#include <misc.h>
!BOP
! !ROUTINE: noah_gather.F90
!
! !DESCRIPTION:
!  Gathers noah tiles
!
! !REVISION HISTORY:
! 
!  Apr 2003 ; Sujay Kumar, Initial Code
! 
! !INTERFACE:
subroutine noah_gather()
! !USES:
  use tile_spmdMod
  use noah_varder
  use noahpardef_module
!EOP
  IMPLICIT NONE

  integer ierr
!BOC
!#if  (defined SPMD)
  print*,"noah_gather()"
!jesse, for wcoss
  if( masterproc ) then
  call MPI_GATHERV(MPI_IN_PLACE,di_array(iam), &
       MPI_NOAH_STRUCT,noah,di_array,displs,MPI_NOAH_STRUCT, &
       0,MPI_COMM_WORLD, ierr)
  else
  call MPI_GATHERV(noah(1:di_array(iam)),di_array(iam), &
       MPI_NOAH_STRUCT,noah,di_array,displs,MPI_NOAH_STRUCT, &
       0,MPI_COMM_WORLD, ierr)
  endif
!#endif
!EOC  
end subroutine noah_gather


