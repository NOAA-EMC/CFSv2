subroutine enorm_state(xst,enorm,yst)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    enorm_state
!   prgmmr: tremolet
!
! abstract: compute energy norm on GSI grid
!
! program history log:
!   2007-10-19  tremolet - initial code
!   2009-01-18  todling  - carry summation in quad precision
!   2009-08-14  lueken   - update documentation
!
!   input argument list:
!    xst
!    yst
!
!   output argument list:
!    enorm
!    yst
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind,r_quad
use constants, only: ione,zero,one,cp,rd,pi,two,r1000
use gridmod, only: nsig,nlat,nlon,lon2,lat2,istart,rlats,ak5,bk5
use mpimod, only: mype
use guess_grids, only: ges_ps,ntguessig
use state_vectors

implicit none

type(state_vector), intent(in   ) :: xst
real(r_quad)      , intent(  out) :: enorm
type(state_vector), intent(inout) :: yst

! Declare local variables
real(r_kind) :: tfact, pfact, pref, tref, gridfac, zps
real(r_kind) :: coslat(lat2), dsig(lat2,lon2,nsig), akk(nsig)
integer(i_kind) :: ii,jj,kk,ijk,ilat
real(r_kind), parameter :: Pa_per_kPa = r1000

! ----------------------------------------------------------------------
tref=280.0_r_kind
pref=1.0e5_r_kind

tfact=cp/tref
pfact=rd*tref/(pref*pref)
gridfac=one/(nlat*nlon)

do kk=1,nsig
   akk(kk) = Pa_per_kPa * (ak5(kk)-ak5(kk+ione))
enddo
dsig=HUGE(dsig)
do jj=2,lon2-ione
   do ii=2,lat2-ione
      zps = Pa_per_kPa * ges_ps(ii,jj,ntguessig)
      do kk=1,nsig
         dsig(ii,jj,kk) = akk(kk) + (bk5(kk)-bk5(kk+ione)) * zps
      enddo
      if (ANY(dsig(ii,jj,:)<=zero)) then
         do kk=1,nsig
            write(6,'(A,I3,4(2X,F18.8))')'enorm ak,bk,pk,dsig=',&
              kk,r1000*ak5(kk),bk5(kk),r1000*ak5(kk)+bk5(kk)*pref,dsig(2,2,kk)
         enddo
         write(6,'(A,I3,4(2X,F18.8))')'enorm ak,bk,pk     =',&
           nsig+ione,r1000*ak5(nsig+ione),bk5(nsig+ione),r1000*ak5(nsig+ione)+bk5(nsig+ione)*pref
         write(6,*)'enorm_state: negative dsig'
         call stop2(123)
      endif
   enddo
enddo

coslat=HUGE(coslat)
do ii=2,lat2-ione
   ilat=istart(mype+ione)+ii-2_i_kind
   if (ilat<ione.or.ilat>nlat) then
      write(6,*)'enorm_state: error ilat',ilat
      call stop2(124)
   end if
   coslat(ii)=cos(rlats(ilat))
   if (coslat(ii)<zero) then
      write(6,*)'enorm_state error coslat:',ii,ilat,rlats(ilat),pi/two,coslat(ii)
      coslat(ii)=-coslat(ii)
   endif
enddo
! ----------------------------------------------------------------------
yst=zero
! ----------------------------------------------------------------------

! U
do kk=1,nsig
   do jj=2,lon2-ione
      do ii=2,lat2-ione
         ijk=(kk-ione)*lon2*lat2 + (jj-ione)*lat2 + ii
         yst%u(ijk)=gridfac*coslat(ii)*dsig(ii,jj,kk)*xst%u(ijk)
      enddo
   enddo
enddo

! V
do kk=1,nsig
   do jj=2,lon2-ione
      do ii=2,lat2-ione
         ijk=(kk-ione)*lon2*lat2 + (jj-ione)*lat2 + ii
         yst%v(ijk)=gridfac*coslat(ii)*dsig(ii,jj,kk)*xst%v(ijk)
      enddo
   enddo
enddo

! T
do kk=1,nsig
   do jj=2,lon2-ione
      do ii=2,lat2-ione
         ijk=(kk-ione)*lon2*lat2 + (jj-ione)*lat2 + ii
         yst%t(ijk)=gridfac*coslat(ii)*dsig(ii,jj,kk)*tfact*xst%t(ijk)
      enddo
   enddo
enddo

! P
do jj=2,lon2-ione
   do ii=2,lat2-ione
      ijk= (jj-ione)*lat2 + ii
      yst%p(ijk)=gridfac*coslat(ii)*pfact*xst%p(ijk)
   enddo
enddo

! ----------------------------------------------------------------------

enorm=DOT_PRODUCT(yst,xst)

! ----------------------------------------------------------------------

return
end subroutine enorm_state
