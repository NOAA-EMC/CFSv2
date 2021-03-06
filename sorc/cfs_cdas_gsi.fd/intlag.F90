module intlagmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intlagmod    module for intlag
!   prgmmr: meunier
!
! abstract: module for intlag
!
! program history log:
!   2008-03-23  lmeunier - initial code
!   2009-08-13  lueken - update documentation
!
! subroutines included:
!   sub intlag
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
PUBLIC intlag

contains

subroutine intlag(laghead,ru,rv,su,sv,obsbin)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intlag      apply nonlin qc obs operator for conv. lag
!   prgmmr: meunier          org: np23                date: 2009-03-23
!
! abstract: apply observation operator and adjoint for conventional lag
!           observations with nonlinear qc operator
!
! program history log:
!   2009-03-23  meunier
!
!   input argument list:
!     laghead - obs type pointer to appropriate obs structure
!     su      - zonal wind increment in grid space
!     sv      - meridian wind increment in grid space
!     obsbin
!
!   output argument list:
!     su      - result for zonal wind increment in grid space
!     sv      - result fo meridian wind increment in grid space

! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,zero,izero,ione,tiny_r_kind,cg_term,rad2deg
  use obsmod, only: lag_ob_type, lsaveobsens, l_do_adjoint
  use qcmod, only: nlnqc_iter
  use gridmod, only: latlon1n,iglobal
  use jfunc, only: jiter

  use lag_fields, only: lag_gather_stateuv,lag_ADscatter_stateuv
  use lag_fields, only: lag_u_full,lag_v_full,lag_uv_alloc,lag_uv_fill
  use lag_fields, only: lag_tl_vec,lag_ad_vec
  use lag_fields, only: orig_lag_num,ntotal_orig_lag,lag_kcount
  use lag_traj, only: lag_rk2iter_tl,lag_rk2iter_ad
  ! use lag_traj, only: lag_rk4iter_tl,lag_rk4iter_ad

  use mpimod, only: mype
  implicit none

! Declare passed variables
  type(lag_ob_type),pointer       ,intent(in   ) :: laghead
  real(r_kind),dimension(latlon1n),intent(in   ) :: su,sv
  real(r_kind),dimension(latlon1n),intent(inout) :: ru,rv
  integer(i_kind)                 ,intent(in   ) :: obsbin

! Print level
  integer(i_kind),parameter::iv_debug = izero

! real(r_kind) penalty
  real(r_kind) cg_lag,p0,wnotgross,wgross
  real(r_kind) lon_tl,lat_tl,p_tl
  real(r_kind) grad_lon,grad_lat,grad_p
  type(lag_ob_type), pointer :: lagptr
  integer(i_kind) i,j

  real(r_kind),dimension(:,:),allocatable:: adu_tmp,adv_tmp

  ! If no balloons at all exit (save time)
  if (ntotal_orig_lag==izero) return

  ! Gather the wind fields if not already done
  if (.not.lag_uv_alloc) then
     call lag_gather_stateuv(su,sv,obsbin)
  else
     if (.not.lag_uv_fill(obsbin)) then
        call lag_gather_stateuv(su,sv,obsbin)
     end if
  end if

  ! Allocate AD fields
  if (l_do_adjoint) then
     allocate(adu_tmp(iglobal,lag_kcount))
     allocate(adv_tmp(iglobal,lag_kcount))
     adu_tmp=zero; adv_tmp=zero
  end if

  lagptr => laghead
  do while (associated(lagptr))

    ! Forward model
     lon_tl=lag_tl_vec(orig_lag_num(lagptr%intnum,3),obsbin,1)
     lat_tl=lag_tl_vec(orig_lag_num(lagptr%intnum,3),obsbin,2)
     p_tl  =lag_tl_vec(orig_lag_num(lagptr%intnum,3),obsbin,3)
     if (iv_debug>=2_i_kind) then 
        print *,'TL Orig:',lon_tl*rad2deg,lat_tl*rad2deg
        print *,'SPECI',lagptr%speci
        print *,'SPECR',lagptr%specr
     end if
     if (iv_debug>=ione) then
        print *,'MAX INC U',maxval(abs(lag_u_full(:,:,obsbin)))
        print *,'MAX INC V',maxval(abs(lag_v_full(:,:,obsbin)))
     end if
     call lag_rk2iter_tl(lagptr%speci,lagptr%specr,&
       &lon_tl,lat_tl,p_tl,&
       &lag_u_full(:,:,obsbin),lag_v_full(:,:,obsbin))
     if (iv_debug>=ione) print *,'TL correction:',lon_tl*rad2deg,lat_tl*rad2deg

     if (lsaveobsens) then
        lagptr%diag_lon%obssen(jiter) = lon_tl*lagptr%raterr2*lagptr%err2_lon
        lagptr%diag_lat%obssen(jiter) = lat_tl*lagptr%raterr2*lagptr%err2_lat
     else
        if (lagptr%luse) lagptr%diag_lon%tldepart(jiter)=lon_tl
        if (lagptr%luse) lagptr%diag_lat%tldepart(jiter)=lat_tl
     endif

     if (l_do_adjoint) then
        if (lsaveobsens) then
           grad_lon = lagptr%diag_lon%obssen(jiter)
           grad_lat = lagptr%diag_lat%obssen(jiter)
           grad_p   = zero
 
        else
           lon_tl=lon_tl-lagptr%res_lon
           lat_tl=lat_tl-lagptr%res_lat
 
!          gradient of nonlinear operator
           if (nlnqc_iter .and. lagptr%pg > tiny_r_kind .and. &
                                lagptr%b  > tiny_r_kind) then
              cg_lag=cg_term/lagptr%b
              wnotgross= one-lagptr%pg
              wgross = lagptr%pg*cg_lag/wnotgross
              p0   = wgross/(wgross+&
                    &exp(-half*(lagptr%err2_lon*lon_tl**2+lagptr%err2_lat*lat_tl**2)))
              lon_tl = lon_tl*(one-p0)
              lat_tl = lat_tl*(one-p0)
              if (iv_debug>=ione) print *,'Do nlnqc_iter'
           end if

           grad_lon = lon_tl*lagptr%raterr2*lagptr%err2_lon
           grad_lat = lat_tl*lagptr%raterr2*lagptr%err2_lat
           grad_p   = zero
        endif

        if (iv_debug>=2_i_kind) then
           print *,'Residual a. cor:',lon_tl*rad2deg,lat_tl*rad2deg
           print *,'R application  :',grad_lon*rad2deg,grad_lat*rad2deg
        end if

!       Adjoint
        call lag_rk2iter_ad(lagptr%speci,lagptr%specr,&
          &grad_lon,grad_lat,grad_p,adu_tmp,adv_tmp)
        lag_ad_vec(orig_lag_num(lagptr%intnum,3),obsbin,1)=&
          &lag_ad_vec(orig_lag_num(lagptr%intnum,3),obsbin,1)+grad_lon
        lag_ad_vec(orig_lag_num(lagptr%intnum,3),obsbin,2)=&
          &lag_ad_vec(orig_lag_num(lagptr%intnum,3),obsbin,2)+grad_lat
        lag_ad_vec(orig_lag_num(lagptr%intnum,3),obsbin,3)=&
          &lag_ad_vec(orig_lag_num(lagptr%intnum,3),obsbin,3)+grad_p
        if (iv_debug>=2) then
           do i=1,iglobal
              do j=1,lag_kcount
                 if (adu_tmp(i,j)/=zero) print *,'IntLag Mype',mype,'GradU',adu_tmp(i,j)
                 if (adv_tmp(i,j)/=zero) print *,'IntLag Mype',mype,'GradV',adv_tmp(i,j)
              end do
           end do
        end if

     endif

     lagptr => lagptr%llpoint

  end do

  if (l_do_adjoint) then
     lag_u_full(:,:,obsbin)=adu_tmp
     lag_v_full(:,:,obsbin)=adv_tmp
     ! Scater back the UV adjoint values back to state vector
     call lag_ADscatter_stateuv(ru,rv,obsbin)
     lag_u_full(:,:,obsbin)=zero
     lag_v_full(:,:,obsbin)=zero

     deallocate(adu_tmp)
     deallocate(adv_tmp)
  end if

  ! Force the reload of the new increment for the next minimization step
  lag_uv_fill(obsbin)=.false.

  return
end subroutine intlag

end module intlagmod
