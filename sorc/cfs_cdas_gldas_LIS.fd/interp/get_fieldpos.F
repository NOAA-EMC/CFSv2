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
! !ROUTINE: get_field_pos.F90
!
! !DESCRIPTION: 
!  This subprogram returns the field position for a given grid point
!  based on the input grid definition.
!
! !REVISION HISTORY:
!   04-10-96  Mark Iredell; Initial Specification
!   03-11-96  Mark Iredell; Allowed hemispheric grids to wrap over one pole
!   05-27-04  Sujay Kumar; Modified code with floating point arithmetic
!
!   INPUT ARGUMENT LIST:
!     i        - integer x grid point
!     j        - integer y grid point
!     gridDesc     - real (200)  domain description parameters
!
!   OUTPUT ARGUMENT LIST:
!     gridDesc   - integer position in grib field to locate grid point
!                (0 if out of bounds)
!
! !INTERFACE:
function get_fieldpos(i,j,gridDesc) result(field_pos)
!EOP
  integer :: field_pos
  real ::  gridDesc(50)
!  GET GRID DIMENSIONS
  im=gridDesc(2)
  jm=gridDesc(3)
  kscan=0
  is1=0
  nscan=mod(nint(gridDesc(11))/32,2)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ACCOUNT FOR WRAPAROUNDS IN EITHER DIRECTION
  ii=i
  jj=j
  if(gridDesc(1).eq.0.or.gridDesc(1).eq.1.or.gridDesc(1).eq.4) then
     rlon1=gridDesc(5)
     rlon2=gridDesc(8)
     iscan=mod(nint(gridDesc(11))/128,2)
     dlon = gridDesc(9)
     ig=nint(360/abs(dlon))
     if(im.ge.ig) then
        ii=mod(i-1+ig,ig)+1
        if((j.le.0.or.j.ge.jm+1).and.mod(ig,2).eq.0) then
           if(gridDesc(1).eq.0) then
              rlat1=gridDesc(4)
              rlat2=gridDesc(7)
              dlat=abs(rlat2-rlat1)/(jm-1)
              if(j.le.0.and.abs(rlat1).gt.90-0.25*dlat) then
                 jj=2-j
                 ii=mod(ii-1+ig/2,ig)+1
              elseif(j.le.0.and.abs(rlat1).gt.90-0.75*dlat) then
                 jj=1-j
                 ii=mod(ii-1+ig/2,ig)+1
              elseif(j.ge.jm+1.and.abs(rlat2).gt.90-0.25*dlat) then
                 jj=2*jm-j
                 ii=mod(ii-1+ig/2,ig)+1
              elseif(j.ge.jm+1.and.abs(rlat2).gt.90-0.75*dlat) then
                 jj=2*jm+1-j
                 ii=mod(ii-1+ig/2,ig)+1
              endif
           elseif(gridDesc(1).eq.4) then
              jg=gridDesc(10)*2
              if(j.le.0.and.jm.eq.jg) then
                 jj=1-j
                 ii=mod(ii-1+ig/2,ig)+1
              elseif(j.ge.jm+1.and.jm.eq.jg) then
                 jj=2*jm+1-j
                 ii=mod(ii-1+ig/2,ig)+1
              endif
           endif
        endif
     endif
  endif
  if(ii.ge.1.and.ii.le.im.and.jj.ge.1.and.jj.le.jm) then
     if(nscan.eq.0) then
        field_pos=ii+(jj-1)*im
     else
        field_pos=jj+(ii-1)*jm
     endif
  else
     field_pos=0
  endif
end function get_fieldpos
