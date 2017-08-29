!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: gdasdrv_module.F90 
!
! !DESCRIPTION:
!  Module containing runtime specific GDAS variables
!
! !REVISION HISTORY:
!
! 11 Dec 2003; Sujay Kumar, Initial Version
! 
! !INTERFACE:
module gdasdrv_module
! !ARGUMENTS:
  type gdasdrvdec
     integer :: ncold, nrold   !AWIPS 212 dimensions
     integer :: nmif
     character*100 :: gdasdir   !GDAS Forcing Directory
     real*8 :: gdastime1,gdastime2
     real*8 :: griduptime1,griduptime2
     logical :: gridchange1, gridchange2
  end type gdasdrvdec
!EOC
end module gdasdrv_module
