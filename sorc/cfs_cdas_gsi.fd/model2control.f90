subroutine model2control(rval,bval,grad)
!$$$  subprogram documentation block
!
! abstract:  Converts variables from physical space to control space
!            This is also the adjoint of control2model
!
! program history log:
!   2007-04-16  tremolet - initial code
!   2007-04-27  tremolet - multiply by sqrt(B)^T (from ckerror_ad D. Parrish)
!   2008-12-04  todling  - update interface to ckgcov_ad; add tsen and p3d
!   2008-12-29  todling  - add call to strong balance contraint
!   2009-04-21  derber   - modify call to getstvp to getuv(*,1)
!   2009-11-10  todling  - remove preditors call to mpl_addreduce
!   2010-03-15  zhu      - make changes to ckgcov_ad, use cstate
!
!   input argument list:
!     rval - State variable
!   output argument list:
!     grad - Control variable
!
!$$$
use kinds, only: r_kind,i_kind
use constants, only: zero
use control_vectors
use state_vectors
use bias_predictors
use gsi_4dvar, only: nsubwin, lsqrtb
use gridmod, only: lat2,lon2,nsig,nnnn1o
use berror, only: varprd,fpsproj
use balmod, only: tbalance
use jfunc, only: nsclen,npclen,nrclen,nval_lenz
implicit none

! Declare passed variables
type(state_vector)  , intent(inout) :: rval(nsubwin)
type(predictors)    , intent(in   ) :: bval
type(control_vector), intent(inout) :: grad

! Declare local variables
real(r_kind),dimension(lat2,lon2,nsig) :: workst,workvp,workrh
integer(i_kind) :: ii,jj
real(r_kind) :: gradz(nval_lenz)
type(control_state) :: cstate

!******************************************************************************

if (.not.lsqrtb) then
   write(6,*)'model2control: assumes lsqrtb'
   call stop2(146)
end if

! Loop over control steps
do jj=1,nsubwin

   workst(:,:,:)=zero
   workvp(:,:,:)=zero
   workrh(:,:,:)=zero

! Convert RHS calculations for u,v to st/vp for application of
! background error
   call getuv(rval(jj)%u,rval(jj)%v,workst,workvp,1)

! Calculate sensible temperature
   call tv_to_tsen_ad(rval(jj)%t,rval(jj)%q,rval(jj)%tsen)

! Adjoint of convert input normalized RH to q to add contribution of moisture
! to t, p , and normalized rh
   call normal_rh_to_q_ad(workrh,rval(jj)%t,rval(jj)%p3d,rval(jj)%q)

! Adjoint to convert ps to 3-d pressure
   call getprs_ad(rval(jj)%p,rval(jj)%t,rval(jj)%p3d)

! Multiply by sqrt of background error adjoint (ckerror_ad)
! -----------------------------------------------------------------------------

! Transpose of balance equation
   call tbalance(rval(jj)%t,rval(jj)%p,workst,workvp,fpsproj)

! Apply variances, as well as vertical & horizontal parts of background error
   gradz(:)=zero

   call allocate_cs(cstate)
   call assign_array2cs(cstate,workst,workvp,rval(jj)%t,workrh,&
                        rval(jj)%oz,rval(jj)%cw,rval(jj)%p,rval(jj)%sst)
   call ckgcov_ad(gradz,cstate,nnnn1o)
   call deallocate_cs(cstate)

   do ii=1,nval_lenz
      grad%step(jj)%values(ii)=grad%step(jj)%values(ii)+gradz(ii)
   enddo

! -----------------------------------------------------------------------------

end do

! Bias predictors are duplicated
do ii=1,nsclen
   grad%predr(ii)=grad%predr(ii)+bval%predr(ii)*sqrt(varprd(ii))
enddo
do ii=1,npclen
   grad%predp(ii)=grad%predp(ii)+bval%predp(ii)*sqrt(varprd(nsclen+ii))
enddo

return
end subroutine model2control
