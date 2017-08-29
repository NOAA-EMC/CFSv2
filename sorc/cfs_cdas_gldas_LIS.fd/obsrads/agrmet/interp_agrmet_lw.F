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
! !ROUTINE: subroutine interp_agrmet_lw.F90
!
! !DESCRIPTION:
!  Opens, reads, and interpolates AGRMET longwave radiation 
!
!     GRIB IPOLATES UTILITY TO INTERPOLATE DATA FOR NH AND SH
!     IN RTNEPH (512X512) POLAR STEREOGRAPHIC GRIDS TO MERGED
!     GLOBAL DATA IN GLDAS-SPECIFIED LAT/LON GRIDS;
!
! !REVISION HISTORY:
!  26 Jun 2001: Urszula Jambor; Initial code, based on Jesse Meng's 
!               RTNEPH2LATLON.F code.
!  08 Feb 2002: Urszula Jambor; Modified declarations of arrays 
!               dependant on domain & resolution to allocatable.
!               Pass in values for latmax.
!  11 Dec 2002: Urszula Jambor; Added 1/2 & 1 degree resolution GDS arrays
!
! !INTERFACE:
subroutine interp_agrmet_lw( pdata1, outdata, ferror )
! !USES:
  use lisdrv_module,only : lis,gindex   
  use agrmetdomain_module, only : rlat,rlon,w11,w12,w21,w22,n11,n12,n21,& 
       n22, mi,mo
  use lis_indices_module
  implicit none

  integer, parameter :: nagrc = 1440, nagrr=600  
! !ARGUMENTS:
  real :: pdata1(nagrc, nagrr)
  real :: outdata(lis%d%ngrid)  !output array matching grid(c,r)
  integer :: ferror                 !set to zero if error found
!EOP
  character*80 :: nameSH
  integer :: ip, i,j,count
  integer :: ipopt(20)
  integer :: gridDesci(22)
  integer :: gridDesco(50)
  integer, parameter :: km=1
  integer :: ibi(km)
  logical*1 :: li1(mi)   
  integer :: no                  !ipolates returns no=latmax*lonmax
  integer :: iret
  real :: pdata2(nagrc, nagrr)
  real, allocatable :: pdata(:)
  real, allocatable :: ldata1(:)
  integer :: ibo
  logical*1, allocatable :: lo1(:)
!BOC
!---------------------------------------------------------------------
! READ INPUT DATA 
!---------------------------------------------------------------------
  allocate(pdata(mi))
  allocate(lo1(lis_nc_working*lis_nr_working))
  allocate(ldata1(lis_nc_working*lis_nr_working))
  if (ferror == 0) then
     do i=1,lis%d%ngrid
        outdata(i) = lis%d%udef
     end do
  else
     ferror = 1
     ibi = 1
     count = 0
     li1 = .false.
     do j=1,nagrr
        do i=1,nagrc
           pdata(count+i) = pdata1(i,j)
        enddo
        count = count+nagrc
     enddo
     do i=1,mi
        if(pdata(i).eq.-9999) then
           li1(i) = .false.
        else
           li1(i) = .true.
        endif
     enddo
     gridDesco = 0
     gridDesco = lis%d%gridDesc
     call bilinear_interp(gridDesco,ibi,li1,pdata,ibo,lo1,ldata1,mi,mo,&
          rlat,rlon,w11,w12,w21,w22,n11,n12,n21,n22,iret)
     
     if(iret .NE. 0) then
        print*, "IPOLATES ERROR!! PROGRAM STOP!!"
        call exit(iret)
     end if
!---------------------------------------------------------------------
! COMBINE LDATA1 AND LDATA2 INTO LDATA
!---------------------------------------------------------------------
     count = 0
     do j=1,lis_nr_working
        do i=1,lis_nc_working
           if(gindex(i,j).ne. -1) then
              outdata(gindex(i,j)) = ldata1(i+count)
           endif
        enddo
        count = count+lis_nc_working
     enddo
  endif
  deallocate(pdata)
  deallocate(lo1)
  deallocate(ldata1)
!EOC
end subroutine interp_agrmet_lw






