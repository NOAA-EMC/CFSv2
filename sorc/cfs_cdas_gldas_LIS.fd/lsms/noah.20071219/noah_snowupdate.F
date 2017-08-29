!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 3.1                         !
!-------------------------------------------------------------------------
!BOP
! 
! !ROUTINE: noah_snowupdate.F
!
! !DESCRIPTION:
!
! !REVISION HISTORY:
!  19 Oct 2007 Jesse Meng 
!
! !INTERFACE:
subroutine noah_snowupdate()
! !uses:
  use lisdrv_module, only : lis, grid
  USE noah_varder      ! NOAH tile variables
  use tile_spmdMod
!EOP
  IMPLICIT NONE      

  integer :: t ! loop counters

!=== End Variable Definition =============================================
!BOC

    do t = 1, di_array(iam)

      noah(t)%sneqv = noah(t)%FORCING(15)       !sneqv[m]
      noah(t)%snowh = noah(t)%FORCING(16)       !snowh[m]

    enddo

  return

!EOC
 end subroutine noah_snowupdate

