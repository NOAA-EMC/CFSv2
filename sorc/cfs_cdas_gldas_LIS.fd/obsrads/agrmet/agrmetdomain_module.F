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
! !MODULE: agrmetdomain_module.F90
! 
! !DESCRIPTION: 
!  Contains routines and variables that define theb native domain for 
!  AGRMET observed radiation forcing 
! 
! !INTERFACE:
module agrmetdomain_module
! !USES:
  use agrmetdrv_module
!EOP  
  type(agrmetdrvdec) :: agrmetdrv
  integer :: mi, mo
  real, allocatable :: rlat(:)
  real, allocatable :: rlon(:)
  integer, allocatable :: n11(:)
  integer, allocatable :: n12(:)
  integer, allocatable :: n21(:)
  integer, allocatable :: n22(:)
  real, allocatable ::  w11(:),w12(:)
  real, allocatable ::  w21(:),w22(:)
contains
  
!BOP
!
! !ROUTINE: defnatagrmet.F90
! 
! !DESCRIPTION: 
!  Defines the gridDesc array describing the native forcing resolution 
!  for AGRMET data. 
!
! !REVISION HISTORY: 
! 11Dec2003: Sujay Kumar; Initial Specification
! 
! !INTERFACE:
  subroutine defnatagrmet()
! !USES:
    use lisdrv_module, only :lis
    use lis_indices_module
    implicit none
! !ARGUMENTS:
    real :: gridDesci(50)
!EOP
!BOC
    call readagrmetcrd(agrmetdrv)
    gridDesci = 0
    gridDesci(1) = 0
    gridDesci(2) = 1440
    gridDesci(3) = 600
    gridDesci(4) = -59.875
    gridDesci(5) = -179.875
    gridDesci(6) = 128
    gridDesci(7) = 89.875
    gridDesci(8) = 179.875
    gridDesci(9) = 0.250
    gridDesci(10) = 0.250
    gridDesci(11) = 64
    gridDesci(20) = 255
    call allocate_agr_ip(lis_nc_working*lis_nr_working)
    mo =lis_nc_working*lis_nr_working
    call def_agr_ip_input(gridDesci)
!EOC

  end subroutine defnatagrmet
!BOP
! !ROUTINE: allocate_agr_ip
!
! !DESCRIPTION:
! 
! Allocate memory for AGRMET interpolation variables
! 
! !INTERFACE:
  subroutine allocate_agr_ip(N)
!EOP
    integer :: N
!BOC
    allocate(rlat(N))
    allocate(rlon(N))
    allocate(N11(N))
    allocate(N12(N))
    allocate(N21(N))
    allocate(N22(N))
    allocate(w11(N))
    allocate(w12(N))
    allocate(w21(N))
    allocate(w22(N))
    mo = n
    nn = n
    w11 = 0.0
    w12 = 0.0
    w21 = 0.0
    w22 = 0.0
!EOC
  end subroutine allocate_agr_ip
!BOP
! !ROUTINE: def_agr_ip_input
! 
! !DESCRIPTION:
! 
! Calculates weights and neighbor information 
! required for AGRMET interpolation 
!
! !INTERFACE:
  subroutine def_agr_ip_input (gridDesc)
! !USES:
    use spmdMod
    use lisdrv_module, only:lis      
    use lis_indices_module      
!EOP
    real, parameter :: FILL = -9999.0
    integer         :: N
    integer         :: mo, nv 
    real            :: xpts(lis_nc_working*lis_nr_working), ypts(lis_nc_working*lis_nr_working)
    real            :: gridDesc(50), gridDesco(50)
    integer         :: i1, i2, j1, j2
    real            :: xi, xf, yi, yf
    integer         :: get_fieldpos
!BOC
!------------------------------------------------------------------------
!  Calls the routines to decode the grid description and 
!  calculates the weights and neighbor information to perform
!  spatial interpolation. This routine eliminates the need to 
!  compute these weights repeatedly during interpolation. 
!------------------------------------------------------------------------
#if ( ! defined OPENDAP )
    if(masterproc) then
#endif
       gridDesco = lis%d%gridDesc
       mo = lis_nc_working*lis_nr_working
       if(gridDesco(1).ge.0) then
          call compute_coord(gridDesco, 0,mo,fill,xpts,ypts,rlon,rlat,nn,0)
       endif

       call compute_coord(gridDesc,-1,nn,fill,xpts,ypts,rlon,rlat,nv,0)
       do n=1,nn
          xi=xpts(n)
          yi=ypts(n)
          if(xi.ne.fill.and.yi.ne.fill) then
             i1=xi
             i2=i1+1
             j1=yi
             j2=j1+1
             xf=xi-i1
             yf=yi-j1
             n11(n)=get_fieldpos(i1,j1,gridDesc)
             n21(n)=get_fieldpos(i2,j1,gridDesc)
             n12(n)=get_fieldpos(i1,j2,gridDesc)
             n22(n)=get_fieldpos(i2,j2,gridDesc)
             if(min(n11(n),n21(n),n12(n),n22(n)).gt.0) then
                w11(n)=(1-xf)*(1-yf)
                w21(n)=xf*(1-yf)
                w12(n)=(1-xf)*yf
                w22(n)=xf*yf
             else
                n11(n)=0
                n21(n)=0
                n12(n)=0
                n22(n)=0
             endif
          else
             n11(n)=0
             n21(n)=0
             n12(n)=0
             n22(n)=0
          endif
       enddo
       mi = 864000
#if ( ! defined OPENDAP )
    endif
#endif
!EOC
  end subroutine def_agr_ip_input
end module agrmetdomain_module
