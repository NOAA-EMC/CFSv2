subroutine evalqlim(sq,pbc,rq)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    evalqlim
!   prgmmr: tremolet
!
! abstract: Computes Jq component
!
! program history log:
!   2007-03-01  tremolet
!   2008-12-8   todling - updated to GSI-May08
!   2009-01-15  todling - carry summation in quadruple precision
!   2009-08-14  lueken  - update documentation
!   2010-03-23  derber - made consistent with stplimq and intlimq (but not checked)
!   2010-05-05  derber - omp commands removed
!
!   input argument list:
!    sq
!    rq
!    pbc
!
!   output argument list:
!    rq
!    pbc
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind,r_quad
  use constants, only: ione,zero,one,zero_quad
  use gridmod, only: lat1,lon1,lat2,lon2,nsig
  use jfunc, only: factqmin,factqmax,qgues,qsatg
  use mpl_allreducemod, only: mpl_allreduce
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig),intent(in   ) :: sq
  real(r_kind),dimension(lat2,lon2,nsig),intent(inout) :: rq
  real(r_quad)                          ,intent(inout) :: pbc

! Declare local variables
  integer(i_kind) i,j,k
  real(r_quad) :: zbc(2)
  real(r_kind) :: q,term
  
  if (factqmin==zero .and. factqmax==zero) return
  
  zbc=zero_quad
! Loop over interior of subdomain          
  do k = 1,nsig
     do j = 2,lon1+ione
        do i = 2,lat1+ione
!          Value for q
           q = qgues(i,j,k) + sq(i,j,k)
!          Compute penalty for neg q
           if (q<zero) then
              term = factqmin*q/(qsatg(i,j,k)*qsatg(i,j,k))
              zbc(1) = zbc(1) + term*q
!             Adjoint
              rq(i,j,k) = rq(i,j,k) + term
           endif
!          Compute penalty for excess q
           if (q>qsatg(i,j,k)) then
              term=factqmax*(q-qsatg(i,j,k))/(qsatg(i,j,k)*qsatg(i,j,k))
              zbc(2) = zbc(2) + term*(q-qsatg(i,j,k))
!             Adjoint
              rq(i,j,k) = rq(i,j,k) + term
           endif
        end do
     end do
  end do

! Reduce on all procs
  call mpl_allreduce(2,qpvals=zbc)
  pbc=pbc+zbc(1)+zbc(2)

  return
end subroutine evalqlim
