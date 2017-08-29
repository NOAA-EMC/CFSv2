!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
!BOP
! !ROUTINE: noah_dynsetup.F90
!
! !DESCRIPTION:
!  
!  Updates the time dependent NOAH variables
!
! !REVISION HISTORY:
! 15 Apr 2002: Sujay Kumar   Initial Specification
! 
! !INTERFACE:
subroutine noah_dynsetup()
! !USES:
  use lisdrv_module, only: lis,tile 
  use noah_varder
  use spmdMod, only : masterproc, npes
  use noahpardef_module
!EOP
  IMPLICIT NONE
!BOC
  integer :: t, n,ier
 print*,"noah_dynsetup(",iam,")"
#if ( ! defined OPENDAP )
  if ( npes > 1 ) then
     call noah_gather
  endif
  if(masterproc) then 
#endif
     call noah_gfrac()
     call noah_alb()  
#if ( ! defined OPENDAP )
  endif
  call MPI_BCAST(noahdrv%noah_gflag,1,MPI_INTEGER,0, &
       MPI_COMM_WORLD,ier)
  call MPI_BCAST(noahdrv%noah_aflag,1,MPI_INTEGER,0, &
       MPI_COMM_WORLD,ier)
  if( npes > 1 .and. ( noahdrv%noah_gflag==1 .or. &
       noahdrv%noah_aflag ==1 ) ) then 
     call noah_scatter
  endif
#endif
!EOC  
end subroutine noah_dynsetup

