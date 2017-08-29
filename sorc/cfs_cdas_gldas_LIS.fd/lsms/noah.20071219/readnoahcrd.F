!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: readnoahcrd.F90
!
! !DESCRIPTION:
!  Routine to read Noah specific parameters from the card file. 
!
! !REVISION HISTORY:
! 14 Oct 2003; Sujay Kumar, Initial Code
!
! !INTERFACE:    
subroutine readnoahcrd(noahdrv)
! !USES:
  use noahdrv_module
!EOP
  implicit none
  integer :: lsm
  type(noahdrvdec) :: noahdrv
  namelist /noah/noahdrv
!BOC
  open(11,file='lis.crd',form='formatted',status='old')
  read(unit=11,NML=noah)
  print*,'Running NOAH LSM:'
  print*,'NOAH Active Restart File: ', noahdrv%NOAH_RFILE
  noahdrv%noah_gfractime = 0.0
  noahdrv%noah_albtime = 0
  noahdrv%noah_albdchk = 0
  noahdrv%noah_gfracdchk = 0
  noahdrv%NOAHOPEN=0
  noahdrv%NOAH_ZST     = 9

  close(11)
!EOC
end subroutine readnoahcrd
