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
! !ROUTINE: bilinear_interp.F90
! 
! !DESCRIPTION: 
!  This subprogram performs bilinear interpolation
!  from any grid to any grid for scalar fields. The routine is based
!  on the spatial interpolation package ipolates from NCEP. 
!             
!  The algorithm simply computes (weighted) averages
!  of bilinearly interpolated points arranged in a square box
!  centered around each output grid point and stretching
!  nearly halfway to each of the neighboring grid points.
!  options allow choices of number of points in each radius
!  from the center point (ipopt(1)) which defaults to 2
!  (if ipopt(1)=-1) meaning that 25 points will be averaged;
!  further options are the respective weights for the radius
!  points starting at the center point (ipopt(2:2+ipopt(1))
!  which defaults to all 1 (if ipopt(2)=-1.).
!  only horizontal interpolation is performed.
!  the grids are defined by their grid description sections
!  
!  The grid description arrays are based on the decoding 
!  schemes used by NCEP. However, in order to remove the integer
!  arithmetic employed in the original ipolates, the routines
!  are rewritten using real number manipulations. The general 
!  structure remains the same. 
!    
!  The current code recognizes the following projections:
!             (gridDesc(1)=0) equidistant cylindrical
!             (gridDesc(1)=1) mercator cylindrical
!             (gridDesc(1)=3) lambert conformal conical
!             (gridDesc(1)=4) gaussian cylindrical (spectral native)
!             (gridDesc(1)=5) polar stereographic azimuthal
!             (gridDesc(1)=202) rotated equidistant cylindrical (eta native)
!  where gridDesc could be either input gridDesci or output gridDesco.
!  as an added bonus the number of output grid points
!  and their latitudes and longitudes are also returned.
!  input bitmaps will be interpolated to output bitmaps.
!  output bitmaps will also be created when the output grid
!  extends outside of the domain of the input grid.
!  the output field is set to 0 where the output bitmap is off.
!   INPUT ARGUMENT LIST:
!     ipopt    - integer (20) interpolation options
!                ipopt(1) is number of radius points
!                (defaults to 2 if ipopt(1)=-1);
!                ipopt(2:2+ipopt(1)) are respective weights
!                (defaults to all 1 if ipopt(2)=-1).
!     gridDesci    - real(200) input domain description parameters 
!     gridDesco    - integer (200) output domain description parameters 
!     mi       - integer dimension of input grid fields 
!     mo       - integer dimension of output grid fields
!     ibi      - integer input bitmap flags
!     li       - logical*1 (mi) input bitmaps (if some ibi(k)=1)
!     gi       - real (mi) input fields to interpolate
!
!   OUTPUT ARGUMENT LIST:
!     no       - integer number of output points
!     rlat     - real (mo) output latitudes in degrees
!     rlon     - real (mo) output longitudes in degrees
!     ibo      - integer (km) output bitmap flags
!     lo       - logical*1 (mo) output bitmaps (always output)
!     go       - real (mo) output fields interpolated
!     iret     - integer return code
!                0    successful interpolation
!                2    unrecognized input grid or no grid overlap
!                3    unrecognized output grid
!                31   invalid undefined output grid
!                32   invalid budget method parameters
!        
! !REVISION HISTORY:
!   04-10-96  Mark Iredell; Initial Specification
!   05-27-04  Sujay Kumar : Modified verision with floating point arithmetic, 
!
!
!
! !INTERFACE:
subroutine bilinear_interp(gridDesco,ibi,li,gi,ibo,lo,go,mi,mo, & 
     rlat,rlon,w11,w12,w21,w22,n11,n12,n21,n22,IRET)
!USES:
!EOP
  implicit none
  integer iret,ier
  integer :: mi, mo,nn
  real :: w11(mo),w12(mo)
  real :: w21(mo),w22(mo)
  real :: rlat(mo),rlon(mo)
  integer :: n11(mo),n12(mo)
  integer :: n21(mo),n22(mo)
  real ::  gridDesco(50)
  integer k, n
  integer ibi,ibo

  logical*1 li(mi),lo(mo)
  real gi(mi),go(mo)
  real xpts(mo),ypts(mo)
  real wo(mo)
      
  real, parameter :: fill=-9999.
  nn = mo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  INTERPOLATE WITH OR WITHOUT BITMAPS
  do n=1, nn
     go(n)=0.
     wo(n)=0.
     
     if(li(n11(n))) then
        go(n)=go(n)+w11(n)*gi(n11(n))
        wo(n)=wo(n)+w11(n)
     endif
     if(li(n21(n))) then
        go(n)=go(n)+w21(n)*gi(n21(n))
        wo(n)=wo(n)+w21(n)
     endif
     if(li(n12(n))) then
        go(n)=go(n)+w12(n)*gi(n12(n))
        wo(n)=wo(n)+w12(n)
     endif
     if(li(n22(n))) then
        go(n)=go(n)+w22(n)*gi(n22(n))
        wo(n)=wo(n)+w22(n)
     endif
  enddo
  ibo=1
  do n=1,nn
     lo(n)=wo(n).ge.0.5
     if(lo(n)) then
        go(n)=go(n)/wo(n)
     else
        ibo=1
        go(n)=0.
     endif
  enddo
  if(gridDesco(1).eq.0) call polfixs(nn,mo,1,rlat,rlon,ibo,lo,go)
  
end subroutine bilinear_interp
