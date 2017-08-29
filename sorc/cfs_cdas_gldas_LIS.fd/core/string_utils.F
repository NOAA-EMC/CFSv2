!BOP
! 
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
! !MODULE: string_utils
! 
! !DESCRIPTION: 
! This module contains routines that perform string manipulations
! 
! !REVISION HISTORY: 
! 14 Nov 2002: Sujay Kumar: Initial Version, adopted from CLM
!
! !INTERFACE:
module string_utils
!EOP  
  implicit none
  private
  public ::&
       to_upper   ! Convert character string to upper case
contains
!BOP
! 
! !ROUTINE: to_upper
! 
! !DESCRIPTION:
! Convert character string to upper case.
! 
! Method: 
! Use achar and iachar intrinsics to ensure use of ascii collating sequence.
!
! !INTERFACE:
  function to_upper(str)
    implicit none
! !ARGUMENTS:
    character(len=*), intent(in) :: str      ! String to convert to upper case
    character(len=len(str))      :: to_upper
!EOP
    integer :: i                ! Index
    integer :: aseq             ! ascii collating sequence
    character(len=1) :: ctmp    ! Character temporary
!BOC
    do i = 1, len(str)
       ctmp = str(i:i)
       aseq = iachar(ctmp)
       if ( aseq >= 97  .and.  aseq <= 122 ) ctmp = achar(aseq - 32)
       to_upper(i:i) = ctmp
    end do
!EOC
  end function to_upper

end module string_utils
