!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: mapvegc.F90
!
! !DESCRIPTION: 
!  This subroutine converts the UMD classes to the SIB classes 
!  used by NOAH LSM (v 2.5).
!  (Originally from Dag Lohmann at NCEP)
!
! !REVISION HISTORY:
!
!   28 Apr 2002, K Arsenault:  Added NOAH LSM to LDAS.
! 
! !INTERFACE:
subroutine mapvegc(vegt) 
!EOP
  implicit none

!  Local Variables

  integer :: vegt, sibveg
!BOC
!---------------------------------------------------------------
!  Convert UMD Classes to SIB Classes.
!---------------------------------------------------------------
  if (vegt .eq. 1)  sibveg = 4
  if (vegt .eq. 2)  sibveg = 1
  if (vegt .eq. 3)  sibveg = 5
  if (vegt .eq. 4)  sibveg = 2
  if (vegt .eq. 5)  sibveg = 3
  if (vegt .eq. 6)  sibveg = 3
  if (vegt .eq. 7)  sibveg = 6
  if (vegt .eq. 8)  sibveg = 8
  if (vegt .eq. 9)  sibveg = 9
  if (vegt .eq. 10) sibveg = 7
  if (vegt .eq. 11) sibveg = 12
  if (vegt .eq. 12) sibveg = 11
  if (vegt .eq. 13) sibveg = 11
  if (vegt .gt. 13) then
     sibveg = 7
  end if
!J  vegt=sibveg !J NCEP VTYPE IS ALREADY SIB
  return
!EOC
end subroutine mapvegc

