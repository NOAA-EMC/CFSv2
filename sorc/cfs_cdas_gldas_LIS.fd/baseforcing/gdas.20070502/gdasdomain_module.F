!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
!BOP
! !MODULE: gdasdomain_module.F90
! 
! !DESCRIPTION: 
!  Contains routines and variables that define the native domain for 
!  GDAS model forcing
! 
! !INTERFACE:
module gdasdomain_module
! !USES:
  use gdasdrv_module
!EOP  
  type(gdasdrvdec) :: gdasdrv
  integer :: mi
contains
  
!BOP
!
! !ROUTINE: defnatgdas.F90
! 
! !DESCRIPTION: 
!  Defines the kgds array describing the native forcing resolution 
!  for GDAS data. 
!
! !REVISION HISTORY: 
! 11Dec2003: Sujay Kumar; Initial Specification
! 
! !INTERFACE:
  subroutine defnatgdas(kgdsi)
! !USES: 
    use lisdrv_module, only: lis
    use time_manager, only: date2time
! NOT USED IN 3.1   use time_module, only : date2time
    implicit none
! !ARGUMENTS:
    integer, intent(inout) :: kgdsi(200)
    integer :: updoy, yr1,mo1,da1,hr1,mn1,ss1
    real :: upgmt
!EOP
!BOC
    call readgdascrd(gdasdrv)
    kgdsi(1) = 4
    kgdsi(2) = 192
    kgdsi(3) = 94
    kgdsi(4) = 88542
    kgdsi(5) = 0
    kgdsi(6) = 128
    kgdsi(7) = -88542
    kgdsi(8) = -1875
    kgdsi(9) = 1875
    kgdsi(10) = 47
    kgdsi(20) = 255
    mi = gdasdrv%ncold*gdasdrv%nrold

    yr1 = 2000
    mo1 = 01
    da1 = 24
    hr1 = 12
    mn1 = 0; ss1 = 0
    call date2time( gdasdrv%griduptime1,updoy,upgmt,yr1,mo1,da1,hr1,mn1,ss1 )
    
    yr1 = 2002     !grid update time
    mo1 = 10
    da1 = 29
    hr1 = 12
    mn1 = 0; ss1 = 0
    call date2time(gdasdrv%griduptime2,updoy,upgmt,yr1,mo1,da1,hr1,mn1,ss1 )
    gdasdrv%gridchange1 = .true.
    gdasdrv%gridchange2 = .true.
!EOC
  end subroutine defnatgdas
end module gdasdomain_module
