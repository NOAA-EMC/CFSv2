module stpqmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpqmod    module for stpq and its tangent linear stpq_tl
!  pgrmmr:
!
! abstract: module for stpq and its tangent linear stpq_tl
!
! program history log:
!   2005-05-19  Yanqiu zhu - wrap stpq and its tangent linear stpq_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-02  Todling - remove stpq_tl
!   2009-08-12  lueken - update documentation
!
! subroutines included:
!   sub stpq
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
implicit none

PRIVATE
PUBLIC stpq

contains

subroutine stpq(qhead,rq,sq,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpq        calcuate penalty and stepsize from q
!                            with addition of nonlinear qc.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and contribution to stepsize from q
!           using nonlinear qc.
!
! program history log:
!   1991-02-26  derber
!   1993-08-25  wu
!   1998-02-03  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-05  parrish - add non-linear qc option
!   2005-04-11  treadon - merge stpq and stpq_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-10-21  su      - modify for variational qc
!   2007-03-19  tremolet - binning of observations
!   2007-07-28  derber  - modify to use new inner loop obs data structure
!   2007-02-15  rancic  - add foto
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2008-12-03  todling - changed handling of ptr%time
!   2010-01-04  zhang,b - bug fix: accumulate penalty for multiple obs bins
!
!   input argument list:
!     qhead
!     rq       - search direction for q
!     sq       - analysis increment for q
!     sges     - stepsize estimates (nstep)
!     nstep    - number of stepsize estimates (== 0 means use outer iteration values)
!
!   output argument list:
!     out(1:nstep)   - contribution of penalty from q sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: q_ob_type
  use qcmod, only: nlnqc_iter,varqc_iter
  use gridmod, only: latlon1n
  use constants, only: half,one,two,tiny_r_kind,cg_term,zero_quad,r3600
  use jfunc, only: l_foto,dhat_dt,xhat_dt
  implicit none

! Declare passed variables
  type(q_ob_type),pointer             ,intent(in   ) :: qhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  real(r_kind),dimension(latlon1n)    ,intent(in   ) :: rq,sq
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables
  integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8,kk
  real(r_kind) cg_q,val,val2,wgross,wnotgross,q_pg
  real(r_kind),dimension(max(1,nstep))::pen
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8,time_q,qq
  type(q_ob_type), pointer :: qptr

  out=zero_quad

  qptr => qhead
  do while (associated(qptr))
     if(qptr%luse)then
        if(nstep > 0)then
           j1=qptr%ij(1)
           j2=qptr%ij(2)
           j3=qptr%ij(3)
           j4=qptr%ij(4)
           j5=qptr%ij(5)
           j6=qptr%ij(6)
           j7=qptr%ij(7)
           j8=qptr%ij(8)
           w1=qptr%wij(1)
           w2=qptr%wij(2)
           w3=qptr%wij(3)
           w4=qptr%wij(4)
           w5=qptr%wij(5)
           w6=qptr%wij(6)
           w7=qptr%wij(7)
           w8=qptr%wij(8)


           val= w1* rq(j1)+w2* rq(j2)+w3* rq(j3)+w4* rq(j4)+ &
                w5* rq(j5)+w6* rq(j6)+w7* rq(j7)+w8* rq(j8)
           val2=w1* sq(j1)+w2* sq(j2)+w3* sq(j3)+w4* sq(j4)+ &
                w5* sq(j5)+w6* sq(j6)+w7* sq(j7)+w8* sq(j8)-qptr%res
           if(l_foto)then
              time_q=qptr%time*r3600
              val =val +(w1*dhat_dt%q(j1)+w2*dhat_dt%q(j2)+ &
                         w3*dhat_dt%q(j3)+w4*dhat_dt%q(j4)+ &
                         w5*dhat_dt%q(j5)+w6*dhat_dt%q(j6)+ &
                         w7*dhat_dt%q(j7)+w8*dhat_dt%q(j8))*time_q
              val2=val2+(w1*xhat_dt%q(j1)+w2*xhat_dt%q(j2)+ &
                         w3*xhat_dt%q(j3)+w4*xhat_dt%q(j4)+ &
                         w5*xhat_dt%q(j5)+w6*xhat_dt%q(j6)+ &
                         w7*xhat_dt%q(j7)+w8*xhat_dt%q(j8))*time_q
           end if
           do kk=1,nstep
              qq=val2+sges(kk)*val
              pen(kk)=qq*qq*qptr%err2
           end do
        else
           pen(1)=qptr%res*qptr%res*qptr%err2
        end if

!  Modify penalty term if nonlinear QC

        if (nlnqc_iter .and. qptr%pg > tiny_r_kind .and. &
                             qptr%b  > tiny_r_kind) then
           q_pg=qptr%pg*varqc_iter
           cg_q=cg_term/qptr%b
           wnotgross= one-q_pg
           wgross = q_pg*cg_q/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk))+wgross)/(one+wgross))
           end do
        endif
     
        out(1) = out(1)+pen(1)*qptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*qptr%raterr2
        end do
     end if

     qptr => qptr%llpoint

  end do

  return
end subroutine stpq

end module stpqmod
