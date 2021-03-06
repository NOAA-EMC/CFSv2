module intozmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intozmod    module for intoz and its tangent linear intoz_tl
!   prgmmr:
!
! abstract: module for intoz and its tangent linear intoz_tl
!
! program history log:
!   2005-05-13  Yanqiu zhu - wrap intoz and its tangent linear intoz_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-11-26  Todling - remove intoz_tl; add interface back
!   2009-08-13  lueken - update documentation
!
! subroutines included:
!   sub intoz_
!   sub intozlay_
!   sub intozlev_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC intoz

interface intoz; module procedure &
          intoz_
end interface

contains

subroutine intoz_(ozhead,o3lhead,roz,soz)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intoz       call individual ozone obs operators
!   prgmmr: todling       org: np23                date: 2008-11-28
!
! abstract:  This routine calls the individual components of the 
!            ozone observation operator.
!
! program history log:
!   2008-11-28  todling
!   2009-01-08  todling - remove reference to ozohead
!
!   input argument list:
!     ozhead  - layer ozone obs type pointer to obs structure
!     o3lhead - level ozone obs type pointer to obs structure
!     soz     - ozone increment in grid space
!
!   output argument list:
!     roz    - ozone results from observation operator 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!--------
  use kinds, only: r_kind
  use obsmod, only: oz_ob_type,o3l_ob_type
  use gridmod, only: latlon1n
  implicit none

! Declare passed variables
  type( oz_ob_type),pointer       ,intent(in   ) :: ozhead
  type(o3l_ob_type),pointer       ,intent(in   ) :: o3lhead
  real(r_kind),dimension(latlon1n),intent(in   ) :: soz
  real(r_kind),dimension(latlon1n),intent(inout) :: roz

  call intozlay_( ozhead,roz,soz)
  call intozlev_(o3lhead,roz,soz)

end subroutine intoz_

subroutine intozlay_(ozhead,roz,soz)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intoz       apply nonlin qc obs operator for ozone
!   prgmmr: derber           org: np23                date: 1995-07-11
!
! abstract:  This routine applies the observation operator (forward
!            model) and adjoint of this operator for ozone observations
!            with the addition of nonlinear qc.
!
! program history log:
!   1995-07-11  derber
!   1999-03-01  wu - port cray90 code to ibm-sp (mpi version)
!   2004-06-16  treadon - update documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intoz and intoz_qc into single routine
!   2005-06-14  wu      - add OMI total ozone
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-02-15  rancic - add foto
!   2007-02-16  sienkiewicz - add call to routine for level ozone contrib.
!   2007-03-19  tremolet - binning of observations
!   2007-05-30  h.liu   - move interpolation weights w1-w4 inside k loop
!   2007-06-05  tremolet - use observation diagnostics structure
!   2007-07-09  tremolet - observation sensitivity
!   2008-01-04  tremolet - Don't apply H^T if l_do_adjoint is false
!   2008-??-??  ??????   - remove nonlinear qc gradient; folded OMI within layer O3
!   2008-11-28  todling  - turn FOTO optional; changed ptr%time handle
!   2009-01-18  todling  - treat val in quad precision (revisit later)
!
!   input argument list:
!     ozhead  - layer ozone obs type pointer to obs structure
!     soz     - ozone increment in grid space
!     roz
!
!   output argument list:
!     roz    - ozone results from observation operator 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!--------
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: oz_ob_type,lsaveobsens,l_do_adjoint
  use gridmod, only: lat2,lon2,nsig
  use jfunc, only: jiter,l_foto,xhat_dt,dhat_dt
  use constants, only: ione,one,zero,r3600,zero_quad
  implicit none

! Declare passed variables
  type( oz_ob_type),pointer             ,intent(in   ) :: ozhead
  real(r_kind),dimension(lat2*lon2,nsig),intent(in   ) :: soz
  real(r_kind),dimension(lat2*lon2,nsig),intent(inout) :: roz

! Declare local variables
  integer(i_kind) k,j1,j2,j3,j4,kk,iz1,iz2,j1x,j2x,j3x,j4x
  real(r_kind) dz1,pob,delz,time_oz
  real(r_quad) val1,valx
  real(r_kind) w1,w2,w3,w4
  type(oz_ob_type), pointer :: ozptr

!
! SBUV OZONE: LAYER O3 and TOTAL O3
!
! Loop over ozone observations.
  ozptr => ozhead
  do while (associated(ozptr))

!    Set location
     j1=ozptr%ij(1)
     j2=ozptr%ij(2)
     j3=ozptr%ij(3)
     j4=ozptr%ij(4)


!    Accumulate contribution from layer observations
     dz1=nsig+ione
     if ( ozptr%nloz >= ione ) then

        if(l_foto) time_oz = ozptr%time*r3600
        do k=1,ozptr%nloz
           val1= zero_quad
           pob = ozptr%prs(k)
           iz1=dz1
           if (iz1 > nsig) iz1=nsig
           iz2=pob
           do kk=iz1,iz2,-1
              delz=one
              if (kk==iz1) delz=dz1-iz1
              if (kk==iz2) delz=delz-pob+iz2
              w1=ozptr%wij(1,kk)
              w2=ozptr%wij(2,kk)
              w3=ozptr%wij(3,kk)
              w4=ozptr%wij(4,kk)
              val1=val1 + ( &
                   w1* soz(j1,kk)+ &
                   w2* soz(j2,kk)+ &
                   w3* soz(j3,kk)+ &
                   w4* soz(j4,kk))*delz
              if (l_foto) then
                 j1x=w1+(kk-ione)*lat2*lon2
                 j2x=w2+(kk-ione)*lat2*lon2
                 j3x=w3+(kk-ione)*lat2*lon2
                 j4x=w4+(kk-ione)*lat2*lon2
                 val1=val1 + ( &
                     (w1*xhat_dt%oz(j1x)+ &
                      w2*xhat_dt%oz(j2x)+ &
                      w3*xhat_dt%oz(j3x)+ &
                      w4*xhat_dt%oz(j4x))*time_oz)*delz
              endif
           enddo

           if (lsaveobsens) then
              ozptr%diags(k)%ptr%obssen(jiter)=val1*ozptr%err2(k)*ozptr%raterr2(k)
           else
              if (ozptr%luse) ozptr%diags(k)%ptr%tldepart(jiter)=val1
           endif

           if (l_do_adjoint) then
              if (lsaveobsens) then
                 valx = ozptr%diags(k)%ptr%obssen(jiter)

              else
                 val1=val1-ozptr%res(k)

                 valx     = val1*ozptr%err2(k) 
                 valx     = valx*ozptr%raterr2(k)
              endif

              do kk=iz1,iz2,-1
                 delz=one
                 if(kk==iz1)delz=dz1-iz1
                 if(kk==iz2)delz=delz-pob+iz2
                 w1=ozptr%wij(1,kk)
                 w2=ozptr%wij(2,kk)
                 w3=ozptr%wij(3,kk)
                 w4=ozptr%wij(4,kk)
                 roz(j1,kk)  =  roz(j1,kk) + valx*w1*delz
                 roz(j2,kk)  =  roz(j2,kk) + valx*w2*delz
                 roz(j3,kk)  =  roz(j3,kk) + valx*w3*delz
                 roz(j4,kk)  =  roz(j4,kk) + valx*w4*delz
              enddo
              if (l_foto) then
                 do kk=iz1,iz2,-1
                    delz=one
                    if(kk==iz1)delz=dz1-iz1
                    if(kk==iz2)delz=delz-pob+iz2
                    w1=ozptr%wij(1,kk)
                    w2=ozptr%wij(2,kk)
                    w3=ozptr%wij(3,kk)
                    w4=ozptr%wij(4,kk)
                    j1x=w1+(kk-ione)*lat2*lon2
                    j2x=w2+(kk-ione)*lat2*lon2
                    j3x=w3+(kk-ione)*lat2*lon2
                    j4x=w4+(kk-ione)*lat2*lon2
                    dhat_dt%oz(j1x) = dhat_dt%oz(j1x) + valx*w1*delz*time_oz
                    dhat_dt%oz(j2x) = dhat_dt%oz(j2x) + valx*w2*delz*time_oz
                    dhat_dt%oz(j3x) = dhat_dt%oz(j3x) + valx*w3*delz*time_oz
                    dhat_dt%oz(j4x) = dhat_dt%oz(j4x) + valx*w4*delz*time_oz
                 enddo
              endif
              dz1=pob
           endif
        end do

     end if   ! (ozptr%nloz >= 1)

!    Add contribution from total column observation
     k=ozptr%nloz+ione
     val1= zero
     do kk=nsig,1,-1
        w1=ozptr%wij(1,kk)
        w2=ozptr%wij(2,kk)
        w3=ozptr%wij(3,kk)
        w4=ozptr%wij(4,kk)
        val1=val1 + &
             w1* soz(j1,kk)+ &
             w2* soz(j2,kk)+ &
             w3* soz(j3,kk)+ &
             w4* soz(j4,kk)
     enddo
     if (l_foto) then
        do kk=nsig,1,-1
           w1=ozptr%wij(1,kk)
           w2=ozptr%wij(2,kk)
           w3=ozptr%wij(3,kk)
           w4=ozptr%wij(4,kk)
           j1x=w1+(kk-ione)*lat2*lon2
           j2x=w2+(kk-ione)*lat2*lon2
           j3x=w3+(kk-ione)*lat2*lon2
           j4x=w4+(kk-ione)*lat2*lon2
           val1=val1 + &
               (w1*xhat_dt%oz(j1x)+ &
                w2*xhat_dt%oz(j2x)+ &
                w3*xhat_dt%oz(j3x)+ &
                w4*xhat_dt%oz(j4x))*time_oz
        enddo
     endif

     if (lsaveobsens) then
        ozptr%diags(k)%ptr%obssen(jiter)=val1*ozptr%err2(k)*ozptr%raterr2(k)
     else
        if (ozptr%luse) ozptr%diags(k)%ptr%tldepart(jiter)=val1
     endif

     if (l_do_adjoint) then
        if (lsaveobsens) then
           valx = ozptr%diags(k)%ptr%obssen(jiter)

        else
           val1=val1-ozptr%res(k)

           valx     = val1*ozptr%err2(k)
           valx     = valx*ozptr%raterr2(k)
        endif

        do kk=nsig,1,-1
           w1=ozptr%wij(1,kk)
           w2=ozptr%wij(2,kk)
           w3=ozptr%wij(3,kk)
           w4=ozptr%wij(4,kk)
           roz(j1,kk)  = roz(j1,kk) + valx*w1
           roz(j2,kk)  = roz(j2,kk) + valx*w2
           roz(j3,kk)  = roz(j3,kk) + valx*w3
           roz(j4,kk)  = roz(j4,kk) + valx*w4
        enddo
        if (l_foto) then
           do kk=nsig,1,-1
              w1=ozptr%wij(1,kk)
              w2=ozptr%wij(2,kk)
              w3=ozptr%wij(3,kk)
              w4=ozptr%wij(4,kk)
              j1x=w1+(kk-ione)*lat2*lon2
              j2x=w2+(kk-ione)*lat2*lon2
              j3x=w3+(kk-ione)*lat2*lon2
              j4x=w4+(kk-ione)*lat2*lon2
              dhat_dt%oz(j1x) =dhat_dt%oz(j1x) + valx*w1*time_oz
              dhat_dt%oz(j2x) =dhat_dt%oz(j2x) + valx*w2*time_oz
              dhat_dt%oz(j3x) =dhat_dt%oz(j3x) + valx*w3*time_oz
              dhat_dt%oz(j4x) =dhat_dt%oz(j4x) + valx*w4*time_oz
           enddo
        endif
     endif

     ozptr => ozptr%llpoint

! End loop over observations
  enddo

! End of routine
  return
end subroutine intozlay_

subroutine intozlev_(o3lhead,roz1d,soz1d)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    into3l       apply nonlin qc obs operator for o3 level
!   prgmmr: sienkiewicz      org: GMAO                date: 2006-09-14
!
! abstract:  This routine applies the observation operator (forward
!            model) and adjoint of this operator for ozone level 
!            observations with the addition of nonlinear qc.
!
! to do: add time derivatives correctly (Todling) 
!
! program history log:
!   2006-09-14  sienkiewicz - add level ozone
!   2007-01-02  sienkiewicz - separate from intoz (again)
!   2007-02-16  sienkiewicz - changes for new inner loop obs data structure
!   2009-01-08  todling - remove nonlinear qc
!   2009-01-22  sienkiewicz - add time derivative
!
!   input argument list:
!     o3lhead - level ozone obs type pointer to obs structure
!     soz1d   - ozone increment in grid space
!     roz1d
!
!   output argument list:
!     roz1d   - results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  -
!
!$$$
!--------

  use kinds, only: r_kind,i_kind
  use obsmod, only: o3l_ob_type,lsaveobsens, l_do_adjoint
  use gridmod, only: latlon1n
  use constants, only: r3600
  use jfunc, only: jiter,l_foto,xhat_dt,dhat_dt
  implicit none

! Declare passed variables
  type(o3l_ob_type),pointer       ,intent(in   ) :: o3lhead
  real(r_kind),dimension(latlon1n),intent(in   ) :: soz1d
  real(r_kind),dimension(latlon1n),intent(inout) :: roz1d

! Declare local variables
  integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8
  real(r_kind) val,grad
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8,time_o3l
  type(o3l_ob_type), pointer :: o3lptr

! LEVEL-OZONE OBSERVATIONS

! Loop over ozone observations.


  o3lptr => o3lhead

  do while (associated(o3lptr))
     j1=o3lptr%ij(1)
     j2=o3lptr%ij(2)
     j3=o3lptr%ij(3)
     j4=o3lptr%ij(4)
     j5=o3lptr%ij(5)
     j6=o3lptr%ij(6)
     j7=o3lptr%ij(7)
     j8=o3lptr%ij(8)
     w1=o3lptr%wij(1)
     w2=o3lptr%wij(2)
     w3=o3lptr%wij(3)
     w4=o3lptr%wij(4)
     w5=o3lptr%wij(5)
     w6=o3lptr%wij(6)
     w7=o3lptr%wij(7)
     w8=o3lptr%wij(8)


!    Forward model
     val=w1*soz1d(j1)+w2*soz1d(j2)+w3*soz1d(j3)+w4*soz1d(j4)+ &
          w5*soz1d(j5)+w6*soz1d(j6)+w7*soz1d(j7)+w8*soz1d(j8)

     if ( l_foto ) then
        time_o3l=o3lptr%time*r3600
        val=val+&
            (w1*xhat_dt%oz(j1)+w2*xhat_dt%oz(j2)+ &
             w3*xhat_dt%oz(j3)+w4*xhat_dt%oz(j4)+ &
             w5*xhat_dt%oz(j5)+w6*xhat_dt%oz(j6)+ &
             w7*xhat_dt%oz(j7)+w8*xhat_dt%oz(j8))*time_o3l
     endif

     if (lsaveobsens) then
        o3lptr%diags%obssen(jiter) = val*o3lptr%raterr2*o3lptr%err2
     else
        if (o3lptr%luse) o3lptr%diags%tldepart(jiter)=val
     endif

     if (l_do_adjoint) then
        if (lsaveobsens) then
           grad = o3lptr%diags%obssen(jiter)

        else
           val=val-o3lptr%res

           grad = val*o3lptr%raterr2*o3lptr%err2
        endif

!    Adjoint
        roz1d(j1)=roz1d(j1)+w1*grad
        roz1d(j2)=roz1d(j2)+w2*grad
        roz1d(j3)=roz1d(j3)+w3*grad
        roz1d(j4)=roz1d(j4)+w4*grad
        roz1d(j5)=roz1d(j5)+w5*grad
        roz1d(j6)=roz1d(j6)+w6*grad
        roz1d(j7)=roz1d(j7)+w7*grad
        roz1d(j8)=roz1d(j8)+w8*grad

        if ( l_foto ) then
           grad=grad*time_o3l
           dhat_dt%oz(j1)=dhat_dt%oz(j1)+w1*grad
           dhat_dt%oz(j2)=dhat_dt%oz(j2)+w2*grad
           dhat_dt%oz(j3)=dhat_dt%oz(j3)+w3*grad
           dhat_dt%oz(j4)=dhat_dt%oz(j4)+w4*grad
           dhat_dt%oz(j5)=dhat_dt%oz(j5)+w5*grad
           dhat_dt%oz(j6)=dhat_dt%oz(j6)+w6*grad
           dhat_dt%oz(j7)=dhat_dt%oz(j7)+w7*grad
           dhat_dt%oz(j8)=dhat_dt%oz(j8)+w8*grad
        endif

     endif

     o3lptr => o3lptr%llpoint

  end do

! End of routine
  return

end subroutine intozlev_

end module intozmod
