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
! !MODULE: cmapdomain_module.F90
! 
! !DESCRIPTION: 
!  Contains routines and variables that define the native domain
!  for CMAP precipitation product.
! 
! !INTERFACE:
module cmapdomain_module
! !USES:
  use cmapdrv_module
! !ARGUMENTS:
  type(cmapdrvdec) :: cmapdrv
  integer :: mi
  real, allocatable :: rlat(:)
  real, allocatable :: rlon(:)
  integer, allocatable :: n11(:,:)
  integer, allocatable :: n12(:,:)
  integer, allocatable :: n21(:,:)
  integer, allocatable :: n22(:,:)
  real, allocatable ::  w11(:,:),w12(:,:)
  real, allocatable ::  w21(:,:),w22(:,:)
!EOP  

contains
  
!BOP
!
! !ROUTINE: defnatcmap.F90
! 
! !DESCRIPTION: 
!  Defines the gridDesc array describing the native forcing resolution 
!  for CMAP data. 
!
! !REVISION HISTORY: 
! 11Dec2003: Sujay Kumar; Initial Specification
! 
! !INTERFACE:
  subroutine defnatcmap()
! !USES: 
    use lisdrv_module, only: lis
    use time_manager, only : date2time
    implicit none
! !ARGUMENTS:
    real :: gridDesci(50)
    integer :: updoy, yr1,mo1,da1,hr1,mn1,ss1
    real :: upgmt
!EOP
!BOC
    call readcmapcrd(cmapdrv)
    gridDesci = 0
    gridDesci(1) = 4
    gridDesci(2) = 1152
    gridDesci(3) =  576
    gridDesci(4) = 89.761
    gridDesci(5) = 0
    gridDesci(6) = 128
    gridDesci(7) = -89.761
    gridDesci(8) = -0.3125
    gridDesci(9) = 0.3125
    gridDesci(10) = 288
    gridDesci(11) =   0
    gridDesci(20) = 255
    call allocate_cmap_ip(lis%d%lnc*lis%d%lnr)
    call conserv_cmap_interp_input(gridDesci,lis%d%gridDesc,&
         lis%d%lnc*lis%d%lnr)
    yr1 = 2005     !grid update time
    mo1 = 06
    da1 = 01
    hr1 = 00
    mn1 = 0; ss1 = 0
    call date2time(cmapdrv%griduptime1,updoy,upgmt,yr1,mo1,da1,hr1,mn1,ss1 )
    cmapdrv%gridchange1 = .true.
!EOC
  end subroutine defnatcmap
!BOP
! !ROUTINE: allocate_cmap_ip
!
! !DESCRIPTION: 
! 
! Allocates memory for CMAP interpolation variables
! 
! !INTERFACE:
  subroutine allocate_cmap_ip(N)
!EOP
    integer :: N
!BOC
    allocate(rlat(n))
    allocate(rlon(n))
    allocate(n11(n,25))
    allocate(n12(n,25))
    allocate(n21(n,25))
    allocate(n22(n,25))
    allocate(w11(n,25))
    allocate(w12(n,25))
    allocate(w21(n,25))
    allocate(w22(n,25))
    mo = n
    nn = n
    w11 = 0.0
    w12 = 0.0
    w21 = 0.0
    w22 = 0.0
!EOC
  end subroutine allocate_cmap_ip
!BOP
! !ROUTINE: def_cmap_ip_input
! 
! !DESCRIPTION:
! 
! Calculates weights and neighbor information required for 
! CMAP interpolation
! 
! !INTERFACE:
  subroutine conserv_cmap_interp_input(gridDesci,gridDesco,npts)
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
       call compute_coord(gridDesco, 0,mo,fill,xpts,ypts,rlon,rlat,nv,0)
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
                n11(n,nb)=get_fieldpos(i1,j1,gridDesci)
                n21(n,nb)=get_fieldpos(i2,j1,gridDesci)
                n12(n,nb)=get_fieldpos(i1,j2,gridDesci)
                n22(n,nb)=get_fieldpos(i2,j2,gridDesci)
                if(min(n11(n,nb),n21(n,nb),n12(n,nb),n22(n,nb)).gt.0) then
                   w11(n,nb)=(1-xf)*(1-yf)
                   w21(n,nb)=xf*(1-yf)
                   w12(n,nb)=(1-xf)*yf
                   w22(n,nb)=xf*yf
                else
                   n11(n,nb)=0
                   n21(n,nb)=0
                   n12(n,nb)=0
                   n22(n,nb)=0
                endif
             else
                n11(n,nb)=0
                n21(n,nb)=0
                n12(n,nb)=0
                n22(n,nb)=0
             endif
!             print*, 'def ',n,nb,n113(n,nb),n213(n,nb)
          enddo
       endif
    enddo
  end subroutine conserv_cmap_interp_input
#if 0
  subroutine def_cmap_ip_input (gridDesci)
! !USES:
    use spmdMod
    use lisdrv_module, only:lis            
    
!EOP
    use lisdrv_module, only:lis      

    real, intent(in) :: gridDesci(50) 
    real            :: gridDesco(50)
    real, parameter     :: fill = -9999.0
    real                :: xpts(lis%d%lnc*lis%d%lnr), ypts(lis%d%lnc*lis%d%lnr)
    real                :: xptb(lis%d%lnc*lis%d%lnr), yptb(lis%d%lnc*lis%d%lnr)
    real                :: rlob(lis%d%lnc*lis%d%lnr), rlab(lis%d%lnc*lis%d%lnr)
    integer             :: ipopt(20)
    integer             :: nb1, nb2
    integer             :: i1, i2, j1, j2
    real                :: xi, xf, yi, yf
    integer             :: get_fieldpos
    integer             :: iret,ib,nb,jb,n,nv,lb,wb

    gridDesco = lis%d%gridDesc
    mo = lis%d%lnc*lis%d%lnr
    !  COMPUTE NUMBER OF OUTPUT POINTS AND THEIR LATITUDES AND LONGITUDES.
    iret=0
    ipopt = 0
    ipopt(1) = -1
    ipopt(2) = -1
    if(gridDesco(1).ge.0) then
       call compute_coord(gridDesco, 0,mo,fill,xpts,ypts,rlon,rlat,nn,0)
       if(nn.eq.0) iret=3
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
          do n=1,nn
             xptb(n)=xpts(n)+ib/real(nb2)
             yptb(n)=ypts(n)+jb/real(nb2)
          enddo
          call compute_coord(gridDesco, 1,nn,fill,xptb,yptb,rlob,rlab,nv,0)
          call compute_coord(gridDesci,-1,nn,fill,xptb,yptb,rlob,rlab,nv,0)
          if(iret.eq.0.and.nv.eq.0.and.lb.eq.0) iret=2
          do n=1,nn
             xi=xptb(n)
             yi=yptb(n)
             if(xi.ne.fill.and.yi.ne.fill) then
                i1=xi
                i2=i1+1
                j1=yi
                j2=j1+1
                xf=xi-i1
                yf=yi-j1
                n11(n,nb)=get_fieldpos(i1,j1,gridDesci)
                n21(n,nb)=get_fieldpos(i2,j1,gridDesci)
                n12(n,nb)=get_fieldpos(i1,j2,gridDesci)
                n22(n,nb)=get_fieldpos(i2,j2,gridDesci)
                if(min(n11(n,nb),n21(n,nb),n12(n,nb),n22(n,nb)).gt.0) then
                   w11(n,nb)=(1-xf)*(1-yf)
                   w21(n,nb)=xf*(1-yf)
                   w12(n,nb)=(1-xf)*yf
                   w22(n,nb)=xf*yf
                else
                   n11(n,nb)=0
                   n21(n,nb)=0
                   n12(n,nb)=0
                   n22(n,nb)=0
                endif
             else
                n11(n,nb)=0
                n21(n,nb)=0
                n12(n,nb)=0
                n22(n,nb)=0
             endif
!             print*,'cmap ',n,nb,n11(n,nb),n21(n,nb)
          enddo
       endif
    enddo
!EOC
  end subroutine def_cmap_ip_input
#endif
end module cmapdomain_module
