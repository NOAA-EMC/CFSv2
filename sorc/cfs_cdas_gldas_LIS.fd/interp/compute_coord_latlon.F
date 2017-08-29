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
! !ROUTINE : compute_coord_latlon.F90
!
! !DESCRIPTION:
!  This subroutine computes the grid and earth coordinates of 
!  the specified domain for an equidistant cylindrical projection.
!  This routine is based on the grid
!  decoding routines in the ipolates interoplation package. 
!  
!  The input options include :
!  (iopt= 0) grid and earth coordinates of all grid points
!  (iopt=+1) earth coordinates of selected grid coordinates
!  (iopt=-1) grid coordinates of selected earth coordinates
!  The current code recognizes the following projections:
!             (gridDesc(1)=000) equidistant cylindrical
!             (gridDesc(1)=001) mercator cylindrical
!             (gridDesc(1)=003) lambert conformal conical
!             (gridDesc(1)=004) gaussian cylindrical
!             (gridDesc(1)=005) polar stereographic azimuthal
!             (gridDesc(1)=201) staggered rotated equidistant cylindrical
!             (gridDesc(1)=202) rotated equidistant cylindrical

! !REVISION HISTORY: 
!   04-10-96 Mark Iredell;  Initial Specification
!   05-27-04 Sujay Kumar; Modified verision with floating point arithmetic. 
!
!   input argument list:
!     gridDesc     - integer (200) domain description parameters
!     iopt     - integer option flag
!                ( 0 to compute earth coords of all the grid points)
!                (+1 to compute earth coords of selected grid coords)
!                (-1 to compute grid coords of selected earth coords)
!     npts     - integer maximum number of coordinates
!     fill     - real fill value to set invalid output data
!                (must be impossible value; suggested value: -9999.)
!     xpts     - real (npts) grid x point coordinates if iopt>0
!     ypts     - real (npts) grid y point coordinates if iopt>0
!     rlon     - real (npts) earth longitudes in degrees e if iopt<0
!                (acceptable range: -360. to 360.)
!     rlat     - real (npts) earth latitudes in degrees n if iopt<0
!                (acceptable range: -90. to 90.)
!     lrot     - integer flag to return vector rotations if 1
!
!   output argument list:
!     xpts     - real (npts) grid x point coordinates if iopt<=0
!     ypts     - real (npts) grid y point coordinates if iopt<=0
!     rlon     - real (npts) earth longitudes in degrees e if iopt>=0
!     rlat     - real (npts) earth latitudes in degrees n if iopt>=0
!     nret     - integer number of valid points computed
!                (-1 if projection unrecognized)
! !INTERFACE:
subroutine compute_coord_latlon(gridDesc,iopt,npts,fill,xpts,ypts,& 
     rlon,rlat,nret,lrot)
!EOP
  implicit none
  real :: gridDesc(50)
  integer :: iopt,npts,nret
  real xpts(npts),ypts(npts),rlon(npts),rlat(npts)
  real :: fill,lrot
  real :: rlat1,rlon1,rlat2,rlon2,hi,hj,dlon,dlat
  real :: xmin,xmax,ymin,ymax
  integer :: iscan,jscan,nscan,im,jm,iret,n
  integer :: ii
  if(gridDesc(1).eq.000) then
     im=gridDesc(2)
     jm=gridDesc(3)
     rlat1=gridDesc(4)
     rlon1=gridDesc(5)
     rlat2=gridDesc(7)
     rlon2=gridDesc(8)
     if(rlat1.gt.rlat2) then 
        dlat=-gridDesc(9)
     else
        dlat=gridDesc(9)
     endif
     if(rlon1.gt.rlon2) then 
        dlon=-gridDesc(10)
     else
        dlon = gridDesc(10)
     endif
     xmin=0
     xmax=im+1
     if(im.eq.nint(360/abs(dlon))) xmax=im+2
     ymin=0
     ymax=jm+1
     nret=0

!  translate grid coordinates to earth coordinates
     if(iopt.eq.0.or.iopt.eq.1) then
        do n=1,npts
           if(xpts(n).ge.xmin.and.xpts(n).le.xmax.and. & 
                ypts(n).ge.ymin.and.ypts(n).le.ymax) then
              rlon(n)=rlon1+dlon*(xpts(n)-1)
              if(rlon(n).lt.0) then 
                 rlon(n) = 360+rlon(n)
              endif
              rlat(n)=rlat1+dlat*(ypts(n)-1)
              nret=nret+1
           else
              rlon(n)=fill
              rlat(n)=fill
           endif
        enddo

!  translate earth coordinates to grid coordinates
     elseif(iopt.eq.-1) then
        do n=1,npts
           if(abs(rlon(n)).le.360.and.abs(rlat(n)).le.90) then
              if(rlon(n).gt.180) then 
                 xpts(n)=1+(rlon(n)-360-rlon1)/dlon
              else
                 xpts(n) = 1+(rlon(n)-rlon1)/dlon
              endif
              ypts(n)=1+(rlat(n)-rlat1)/dlat
              if(xpts(n).ge.xmin.and.xpts(n).le.xmax.and. & 
                   ypts(n).ge.ymin.and.ypts(n).le.ymax) then
                 nret=nret+1
              else
                 xpts(n)=fill
                 ypts(n)=fill
              endif
           else
              xpts(n)=fill
              ypts(n)=fill
           endif
        enddo
     endif
!  projection unrecognized
  else
     iret=-1
     if(iopt.ge.0) then
        do n=1,npts
           rlon(n)=fill
           rlat(n)=fill
        enddo
     endif
     if(iopt.le.0) then
        do n=1,npts
           xpts(n)=fill
           ypts(n)=fill
        enddo
     endif
  endif
end subroutine compute_coord_latlon
