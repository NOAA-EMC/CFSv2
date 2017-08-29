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
!  !MODULE: conserv_interpMod.F90
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
module conserv_interpMod 

  implicit none
  ! !ARGUMENTS:
  integer                :: nb3,nb4
  real, allocatable      :: rlat3(:)
  real, allocatable      :: rlon3(:)
  integer, allocatable   :: n113(:,:)
  integer, allocatable   :: n123(:,:)
  integer, allocatable   :: n213(:,:)
  integer, allocatable   :: n223(:,:)
  real, allocatable      ::  w113(:,:),w123(:,:)
  real, allocatable      ::  w213(:,:),w223(:,:)
  !EOP
contains
  !BOP
  ! !ROUTINE: allocate_interp
  !
  ! !DESCRIPTION:
  ! 
  ! Allocates memory for interpolation of model forcing data (GEOS and GDAS)
  ! 
  ! !INTERFACE: 

  subroutine allocate_conserv_interp(n)
    ! !ARGUMENTS:
    integer, intent(in) :: n
    !EOC
    allocate(rlat3(n))
    allocate(rlon3(n))
    allocate(n113(n,25))
    allocate(n123(n,25))
    allocate(n213(n,25))
    allocate(n223(n,25))
    allocate(w113(n,25))
    allocate(w123(n,25))
    allocate(w213(n,25))
    allocate(w223(n,25))
    w113 = 0.0
    w123 = 0.0
    w213 = 0.0
    w223 = 0.0
  end subroutine allocate_conserv_interp

  subroutine conserv_interp_input(gridDesci,gridDesco,npts)
    real, intent(in) :: gridDesci(50) 
    real             :: gridDesco(50)
    real, parameter     :: fill = -9999.0
    real                :: xpts(npts), ypts(npts)
    real                :: xptb(npts), yptb(npts)
    real                :: rlob(npts), rlab(npts)
    integer             :: npts
    integer             :: ipopt(20)
    integer             :: nb1, nb2, mo
    integer             :: i1, i2, j1, j2
    real                :: xi, xf, yi, yf
    integer             :: get_fieldpos
    integer             :: iret,ib,nb,jb,n,nv,lb,wb

    !  COMPUTE NUMBER OF OUTPUT POINTS AND THEIR LATITUDES AND LONGITUDES.
    mo = npts
    iret=0
    ipopt = 0
    ipopt(1) = -1
    ipopt(2) = -1
    if(gridDesco(1).ge.0) then
       call compute_coord(gridDesco, 0,mo,fill,xpts,ypts,rlon3,rlat3,nv,0)
       if(mo.eq.0) iret=3
    else
       iret=31
    endif
    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !  SET PARAMETERS
    nb1=ipopt(1)
    if(nb1.eq.-1) nb1=2
    if(iret.eq.0.and.nb1.lt.0.) iret=32
    if(iret.eq.0.and.nb1.ge.20.and.ipopt(2).ne.-1) iret=32
    if(iret.eq.0) then
       nb2=2*nb1+1
       nb3=nb2*nb2
       nb4=nb3
       if(ipopt(2).ne.-1) then
          nb4=ipopt(2)
          do ib=1,nb1
             nb4=nb4+8*ib*ipopt(2+ib)
          enddo
       endif
    else
       nb2=0
       nb3=0
       nb4=0
    endif

    ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !  LOOP OVER SAMPLE POINTS IN OUTPUT GRID BOX
    do nb=1,nb3
       !  locate input points and compute their weights
       jb=(nb-1)/nb2-nb1
       ib=nb-(jb+nb1)*nb2-nb1-1
       lb=max(abs(ib),abs(jb))
       wb=1
       if(ipopt(2).ne.-1) wb=ipopt(2+lb)
       if(wb.ne.0) then
          do n=1,mo
             xptb(n)=xpts(n)+ib/real(nb2)
             yptb(n)=ypts(n)+jb/real(nb2)
          enddo
          call compute_coord(gridDesco, 1,mo,fill,xptb,yptb,rlob,rlab,nv,0)
          call compute_coord(gridDesci,-1,mo,fill,xptb,yptb,rlob,rlab,nv,0)
          if(iret.eq.0.and.nv.eq.0.and.lb.eq.0) iret=2
          do n=1,mo
             xi=xptb(n)
             yi=yptb(n)
             if(xi.ne.fill.and.yi.ne.fill) then
                i1=xi
                i2=i1+1
                j1=yi
                j2=j1+1
                xf=xi-i1
                yf=yi-j1
                n113(n,nb)=get_fieldpos(i1,j1,gridDesci)
                n213(n,nb)=get_fieldpos(i2,j1,gridDesci)
                n123(n,nb)=get_fieldpos(i1,j2,gridDesci)
                n223(n,nb)=get_fieldpos(i2,j2,gridDesci)
                if(min(n113(n,nb),n213(n,nb),n123(n,nb),n223(n,nb)).gt.0) then
                   w113(n,nb)=(1-xf)*(1-yf)
                   w213(n,nb)=xf*(1-yf)
                   w123(n,nb)=(1-xf)*yf
                   w223(n,nb)=xf*yf
                else
                   n113(n,nb)=0
                   n213(n,nb)=0
                   n123(n,nb)=0
                   n223(n,nb)=0
                endif
             else
                n113(n,nb)=0
                n213(n,nb)=0
                n123(n,nb)=0
                n223(n,nb)=0
             endif
!             print*, 'def ',n,nb,n113(n,nb),n213(n,nb)
          enddo
       endif
    enddo
  end subroutine conserv_interp_input

end module conserv_interpMod

