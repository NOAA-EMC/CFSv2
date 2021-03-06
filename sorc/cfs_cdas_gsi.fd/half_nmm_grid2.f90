subroutine half_nmm_grid2(gin,nx,ny,gout,igtype,iorder)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    half_nmm_grid2    make a-grid from every other row of e-grid
!   prgmmr: parrish          org: np22                date: 2004-06-22
!
! abstract: creates an unstaggered A grid from the staggered E grid used by the wrf nmm.
!           This is done by keeping every other row of the original E grid.  If this 
!           is a mass variable (igtype=1), then no interpolation is required.  If this
!           is a wind variable (igtype=2), then interpolation is necessary.  This procedure
!           is necessary because the gsi is not yet able to work with anything other than
!           unstaggered grids.  This solution introduces greater interpolation error
!           compared to the option fill_nmm_grid2, but has the advantage of 4 times fewer
!           grid points compared to the output of fill_nmm__grid2.  This routine will be
!           eliminated when the gsi has the capability to work directly with staggered grids.
!
! program history log:
!   2004-06-22  parrish, document
!
!   input argument list:
!     gin      - input staggered E grid field over entire horizontal domain
!     nx,ny    - input grid dimensions
!     igtype   - =1, then (1,1) on staggered grid is at corner of grid (mass point for nmm)
!              - =2, then (1,1) is staggered (wind point for nmm, see illustration below)
!
!                   igtype=1:
!
!
!
!       ^   3             x     x     x     x
!       |
!       y   2                x     x     x     x
!
!           1             x     x     x     x
!
!                         1     2     3
!
!                           x -->
!
!                   igtype=2:
!
!
!
!       ^   3             x     x     x     x
!       |
!       y   2          x     x     x     x
!
!           1             x     x     x     x
!
!                         1     2     3
!
!                           x -->
!
!   output argument list
!     gout     - output unstaggered half grid  (reorganized for distibution to local domains)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_single,i_kind
  use constants, only: izero, ione, quarter, zero
  use gridmod, only: iglobal, itotsub, ltosi, ltosj, ltosi_s, ltosj_s

  implicit none

! Declare passed variables
  integer(i_kind)                  ,intent(in   ) :: nx,ny,igtype,iorder
  real(r_single),dimension(nx,ny)  ,intent(in   ) :: gin
  real(r_single),dimension(itotsub),intent(  out) :: gout

! Declare local variables
  integer(i_kind) i,i0,im,j,jj,jm,jp
  real(r_single),dimension(nx,(ny+5_i_kind)/2):: c
  
  if(igtype==ione) then
     jj=izero
     do j=1,ny,2
        jj=jj+ione
        do i=1,nx
           c(i,jj)=gin(i,j)
        end do
     end do
  else
     jj=izero
     do j=1,ny,2
        jj=jj+ione
        jp=j+ione ; if(jp>ny)   jp=j-ione
        jm=j-ione ; if(jm<ione) jm=j+ione
        do i=1,nx
           im=i-ione ; if(im<ione) im=i
           i0=i      ; if(i==nx)   i0=im
           c(i,jj)=quarter*(gin(im,j)+gin(i0,j)+gin(i,jp)+gin(i,jm))
        end do
     end do
  end if

  
! Reorganize for eventual distribution to local domains
  do i=1,itotsub
     gout(i)=zero
  end do
  if(iorder==ione)then
     do i=1,itotsub
        gout(i)=c(ltosj_s(i),ltosi_s(i))
     end do
  else
     do i=1,iglobal
        gout(i)=c(ltosj(i),ltosi(i))
     end do
  endif
  
end subroutine half_nmm_grid2
