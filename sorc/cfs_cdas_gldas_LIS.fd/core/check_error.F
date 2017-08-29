!BOP
! !ROUTINE: check_error
! 
! !DESCRIPTION:
! Error check; Program exits in case of error
! 
! !INTERFACE:
subroutine check_error(ierr,msg,iam)
!EOP
  implicit none
  
  integer :: ierr, iam
  character*40 :: msg
!BOC  
  if ( ierr /= 0 ) then
     print*,'ERR: ',msg,' Stopping.',' (',iam,')'
     call endrun
  endif
!EOC
end subroutine check_error

