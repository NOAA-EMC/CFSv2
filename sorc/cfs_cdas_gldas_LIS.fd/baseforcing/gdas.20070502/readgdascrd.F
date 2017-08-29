!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: readgdascrd.F90
!
! !DESCRIPTION:
!  Routine to read GDAS specific parameters from the card file. 
!
! !REVISION HISTORY:
! 11 Dec 2003; Sujay Kumar, Initial Code
!
! !INTERFACE:    
subroutine readgdascrd(gdasdrv)
! !USES:
  use gdasdrv_module
!EOP
  implicit none
  integer :: lsm
  type(gdasdrvdec) :: gdasdrv
  namelist /gdas/gdasdrv
!BOC
  open(11,file='lis.crd',form='formatted',status='old')
  read(unit=11,NML=gdas)
  print*,'Using GDAS forcing'
  print*, 'GDAS forcing directory :',gdasdrv%GDASDIR
  gdasdrv%GDASTIME1  = 3000.0
  gdasdrv%GDASTIME2  = 0.0

  close(11)
!EOC
end subroutine readgdascrd
