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
!  !MODULE: bilinear_interpMod.F90
!
!  !DESCRIPTION: 
!   This module contains routines that precomputes weights and 
!   other parameters required for spatial interpolation of model
!   forcing
! 
!  !REVISION HISTORY: 
!  14Nov02    Sujay Kumar  Initial Specification
! 
! !INTERFACE:
module bilinear_interpMod 

  implicit none
  ! !ARGUMENTS:
  real, allocatable      :: rlat0(:)
  real, allocatable      :: rlon0(:)
  integer, allocatable   :: n110(:)
  integer, allocatable   :: n120(:)
  integer, allocatable   :: n210(:)
  integer, allocatable   :: n220(:)
  real, allocatable      :: w110(:),w120(:)
  real, allocatable      :: w210(:),w220(:)
  !EOP
contains
  !BOP
  ! !ROUTINE: allocate_bilinear_interp
  !
  ! !DESCRIPTION:
  ! 
  ! Allocates memory for interpolation of model forcing data (GEOS and GDAS)
  ! 
  ! !INTERFACE: 
  subroutine allocate_bilinear_interp(n)
    ! !ARGUMENTS:
    integer, intent(in) :: n
    !EOP
    !BOC
    allocate(rlat0(n))
    allocate(rlon0(n))
    allocate(n110(n))
    allocate(n120(n))
    allocate(n210(n))
    allocate(n220(n))
    allocate(w110(n))
    allocate(w120(n))
    allocate(w210(n))
    allocate(w220(n))
!    mo = n
!    nn = n

    w110 = 0.0
    w120 = 0.0
    w210 = 0.0
    w220 = 0.0
  end subroutine allocate_bilinear_interp

  !BOP
  ! !ROUTINE: bilinear_interp_input
  !
  ! !DESCRIPTION:
  ! 
  ! Calculates spatial variables required for interpolation of GEOS/GDAS
  ! model forcing
  ! 
  ! !INTERFACE:    
  subroutine bilinear_interp_input (gridDesci,gridDesco,npts)
    ! !INPUT ARGUMENTS:
    real, intent(in) :: gridDesci(50)
    integer          :: npts
    !EOP
    integer             :: n
    integer             :: mo, nv 
    real, parameter     :: fill = -9999.0
    real                :: xpts(npts), ypts(npts)
    real                :: gridDesco(50)
    integer             :: i1, i2, j1, j2
    real                :: xi, xf, yi, yf
    integer             :: get_fieldpos
    !BOC
    mo = npts
    !------------------------------------------------------------------------
    !  Calls the routines to decode the grid description and 
    !  calculates the weights and neighbor information to perform
    !  spatial interpolation. This routine eliminates the need to 
    !  compute these weights repeatedly during interpolation. 
    !------------------------------------------------------------------------
    if(gridDesco(1).ge.0) then
       call compute_coord(gridDesco, 0,mo,fill,xpts,ypts,rlon0,rlat0,nv,0)
    endif
    call compute_coord(gridDesci,-1,mo,fill,xpts,ypts,rlon0,rlat0,nv,0)
    do n=1,mo
       xi=xpts(n)
       yi=ypts(n)
       if(xi.ne.fill.and.yi.ne.fill) then
          i1=xi
          i2=i1+1
          j1=yi
          j2=j1+1 
          xf=xi-i1
          yf=yi-j1
          n110(n)=get_fieldpos(i1,j1,gridDesci)
          n210(n)=get_fieldpos(i2,j1,gridDesci)
          n120(n)=get_fieldpos(i1,j2,gridDesci)
          n220(n)=get_fieldpos(i2,j2,gridDesci)
          if(min(n110(n),n210(n),n120(n),n220(n)).gt.0) then
             w110(n)=(1-xf)*(1-yf)
             w210(n)=xf*(1-yf)
             w120(n)=(1-xf)*yf
             w220(n)=xf*yf
          else
             n110(n)=0
             n210(n)=0
             n120(n)=0
             n220(n)=0
          endif
       else
          n110(n)=0
          n210(n)=0
          n120(n)=0
          n220(n)=0
       endif
    enddo

    !EOC
  end subroutine bilinear_interp_input
  
end module bilinear_interpMod

