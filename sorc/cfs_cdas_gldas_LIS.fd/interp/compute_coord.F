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
! !ROUTINE: compute_coord.F90
!
! !DESCRIPTION: 
!  This subroutine computes the grid and earth coordinates of 
!  the specified domain. This routine is based on the grid
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
subroutine compute_coord(gridDesc,iopt,npts,fill,xpts,ypts,rlon,rlat,nret, & 
     lrot)
!EOP
  implicit none
  real :: gridDesc(50)
  integer :: npts, nret,lrot
  real :: fill
  real :: xpts(npts),ypts(npts),rlon(npts),rlat(npts)
  integer :: iopt, im,jm,kscan,is1,nm,nscan,nn, iopf,n
  integer :: i,j
  if(iopt.eq.0) then
     im=gridDesc(2)
     jm=gridDesc(3)
     nm=im*jm
     nscan=mod(nint(gridDesc(11))/32,2)
     if(nm.le.npts) then
        do n=1,nm
           if(nscan.eq.0) then
              j=(n-1)/im+1
              i=n-im*(j-1)
           else
              i=(n-1)/jm+1
              j=n-jm*(i-1)
           endif
           xpts(n)=i
           ypts(n)=j
        enddo
        do n=nm+1,npts
           xpts(n)=fill
           ypts(n)=fill
        enddo
     else
        do n=1,npts
           xpts(n)=fill
           ypts(n)=fill
        enddo
     endif
     iopf=1
  else
     iopf=iopt
  endif
!  equidistant cylindrical
  if(gridDesc(1).eq.000) then
     call compute_coord_latlon(gridDesc,iopf,npts,fill,xpts,ypts,rlon,rlat,nret, & 
          lrot)
!     gaussian cylindrical
  elseif(gridDesc(1).eq.004) then
     call compute_coord_gauss(gridDesc,iopf,npts,fill,xpts,ypts,rlon,rlat,nret, & 
          lrot)
  endif
end subroutine compute_coord
