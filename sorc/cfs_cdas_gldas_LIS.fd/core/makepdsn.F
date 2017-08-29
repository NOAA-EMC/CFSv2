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
!BOP
!
! !ROUTINE: makepdsn
! 
! !DESCRIPTION: 
!  This routine computes the coefficients and terms required for 
!   grib output
! 
!  !REVISION HISTORY:
!       3-19-02  Cosgrove; Altered code for use with CLM writing
!                intervals
!       5-17-02  Cosgrove; Changed code to use ldas%lsm instead of
!                ldas%rmos and ldas%rclm so that forcing generation
!                would still work when rmos set to zero in card 
!       28-5-02  Arsenault; Altered code for use with NOAH writing
!                intervals 
! !INTERFACE:
subroutine makepdsn(yesterday, beforeyester, kpds, hour, writeint)
!EOP
  implicit none
  
  character*8 yesterday, beforeyester
  integer     kpds(25), hour
  real  :: writeint
!     set time-related kpds octets
  if (kpds(16) .ne. 0) then
     kpds(11) = hour - writeint
     if (kpds(11).lt.0) then
        kpds(11)=24-writeint
        read (beforeyester,'(4(i2))') kpds(21), kpds(8), &
             kpds(9), kpds(10)
     else
        read (yesterday,'(4(i2))') kpds(21), kpds(8), &
             kpds(9), kpds(10)
     endif
  else
     read (yesterday,'(4(i2))') kpds(21), kpds(8),&
          kpds(9), kpds(10)
     kpds(11) = hour
  end if
  
  if (kpds(8) .eq. 0) then
     kpds(8) = 100
  else
     kpds(21) = kpds(21) + 1
  end if
  
end subroutine makepdsn
