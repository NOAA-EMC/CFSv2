!-------------------------------------------------------------------------
! NASA Goddard Space Flight Center Land Information System (LIS) V3.0
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
!
! !MODULE: agrmetdrv_module.F90 
!
! !DESCRIPTION:
!  Module containing runtime specific AGRMET variables
!
! !REVISION HISTORY:
!
! 11 Dec 2003; Sujay Kumar, Initial Version
! 
! !INTERFACE:
module agrmetdrv_module
! !ARGUMENTS:
  type agrmetdrvdec
     integer :: ncold, nrold   !AWIPS 212 dimensions
     integer :: nmif
     character*40 :: agrmetdir !AGRMET Forcing Directory
     real*8 :: agrmtime1,agrmtime2
  end type agrmetdrvdec
!EOC
end module agrmetdrv_module
