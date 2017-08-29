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
! !ROUTINE: time_interp_cmap.F90
! 
! !DESCRIPTION:
! 
! Calls the post processing utilities for handling observed 
! precipitation data for CMAP
! 
! !INTERFACE:
  subroutine time_interp_cmap
! !USES:
    use lisdrv_module, only:lis, grid
    use obsprecipforcing_module, only : obsprecip
    use grid_spmdMod
!EOP
    real    :: ratio(gdi(iam))
    integer :: c
!    integer :: C1 = 1
!BOC
 print*,"J---TIME_INTERP_CMAP"
!------------------------------------------------------------------------
! Compute ratio between convective model precip and total model precip
! so that it can be applied to the observed global precip
!------------------------------------------------------------------------
   ! do c = 1,gdi(iam)
      ! if (grid(c)%forcing(8) .ne. 0.0 .and.  & 
      !      grid(c)%forcing(8) .ne. lis%d%udef .and.  & 
      !      grid(c)%forcing(9) .ne. lis%d%udef) then
      !    ratio(c) = grid(c)%forcing(9) / grid(c)%forcing(8) 
      !    if (ratio(c) .gt. 1.0) ratio(c) = 1.0
      !    if (ratio(c) .lt. 0.0) ratio(c) = 0.0
      ! else
      !    ratio(c) = 0.0
      ! endif
   ! enddo
    do c = 1, gdi(iam)
       if (obsprecip(c) .ge. 0.0) then
	  grid(c)%forcing(8) = grid(c)%forcing(8) * obsprecip(c) 
       endif
    enddo
!EOC
  end subroutine time_interp_cmap
