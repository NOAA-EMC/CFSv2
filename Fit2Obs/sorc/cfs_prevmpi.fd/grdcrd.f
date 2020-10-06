!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grdcrd
!   prgmmr: parrish          org: np22                date: 1990-10-11
!
! abstract: get grid coordinates from monotonically increasing or
!           decreasing points
!
! program history log:
!   1990-10-11  parrish
!   1998-04-07  weiyu yang
!   2004-05-17  kleist, documentation
!   2004-05-28  kleist, combine increasing/decreasing routines & fctns
!   2005-05-24  pondeca, add the special case nx=1
!
!   input argument list:
!     d      - input points
!     nd     - number of input points
!     x      - grid values
!     nx     - number of reference grid points
!     flg    - marks order of values in x 
!              (1=increasing, -1=decreasing)
!
!   output argument list:
!     d        - points converted to grid units
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      subroutine grdcrd(d,nd,x,nx,flg)
      implicit none
      integer id,ix,isrchf,nd,nx,flg
      real d(nd),x(nx)

      if(nx>1) then ! "normal" case in which nx>1
         do id=1,nd
         ix=isrchf(nx-1,x,d(id),flg)-1
         if(ix<=0)then
            d(id)=1.0
         elseif(ix>=nx)then
            d(id)=nx
         else
            d(id)=float(ix)+(d(id)-x(ix))/(x(ix+1)-x(ix))
         endif
         enddo
      elseif (nx==1) then ! special case of nx=1
         do id=1,nd
         d(id) = 1
         enddo
      endif

      return
      end ! subroutine grdcrd
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    isrchf
!   prgmmr: parrish          org: np22                date: 1990-10-11
!
! abstract: get grid coordinates from monotonically increasing or
!           decreasing points
!
! program history log:
!   2005-03-07  treadon - add doc block
!
!   input argument list:
!     nx1    - number of input points
!     x      - grid values
!     y      - target value
!     flg    - marks order of values in x
!              (1=increasing, -1=decreasing)
!
!   output argument list:
!     isrchf  - array index of input grid value near target value
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      function isrchf(nx1,x,y,flg)
      implicit none
      integer  isrchf,nx1,flg,k
      real     x(nx1),y

      isrchf=0

      if(flg.eq.1) then
         do k=1,nx1
         if(y<=x(k)) then
            isrchf=k
            go to 100
         end if
         end do
      else
         do k=1,nx1
         if(y>=x(k)) then
            isrchf=k
            go to 100
         end if
         end do
      end if

      isrchf=nx1+1

100   continue
      return
      end
