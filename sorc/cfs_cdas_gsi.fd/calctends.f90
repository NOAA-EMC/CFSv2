subroutine calctends(u,v,t,q,oz,cw,teta,z,u_x,u_y,v_x,v_y,t_x,t_y,ps_x,ps_y,&
   q_x,q_y,oz_x,oz_y,cw_x,cw_y,mype,u_t,v_t,t_t,p_t,q_t,oz_t,&
   cw_t,pri)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    calctends       calculate u,v,t,p tendencies
!   prgmmr: kleist           org: np20                date: 2005-09-29
!
! abstract: compute tendencies for pressure, wind, and virtual 
!           temperature
!
! program history log:
!   2005-09-29  kleist
!   2005-10-17  kleist - changes to improve computational efficiency
!   2005-11-21  kleist - add tracer tendencies, use new module
!   2006-04-12  treadon - replace sigi with bk5
!   2006-04-21  kleist - add divergence tendency
!   2006-07-31  kleist - changes to use ps instead of ln(ps)
!   2007-04-16  kleist - remove divergence tendency bits to outside
!   2007-05-08  kleist - add bits for fully generalized coordinate
!   2007-06-21  rancic - add pbl 
!   2007-07-02  derber - move calculation of z_x, z_y into routine
!   2007-07-26 cucurull - add pri in argument list, call getprs_horiz;
!                         move getprs outside calctends;
!                         remove ps from argument list
!   2007-08-08  derber - optimize, remove calculation of t_over* and dp_over* unless needed.
!   2008-06-05  safford - rm unused vars and uses
!   2009-08-20  parrish - replace curvfct with curvx, curvy.  this allows tendency computation to 
!                          work for any general orthogonal coordinate.
!
! usage:
!   input argument list:
!     u        - zonal wind on subdomain
!     v        - meridional wind on subdomain
!     t        - virtual temperature on subdomain
!     z        - sfc terrain height
!     u_x      - zonal derivative of u
!     u_y      - meridional derivative of u
!     v_x      - zonal derivative of v
!     v_y      - meridional derivative of v
!     t_x      - zonal derivative of t
!     t_y      - meridional derivative of t
!     ps_x     - zonal derivative of ps
!     ps_y     - meridional derivative of ps
!     q_x      - zonal derivative of q
!     q_y      - meridional derivative of q
!     oz_x     - zonal derivative of ozone
!     oz_y     - meridional derivative of ozone
!     cw_x     - zonal derivative of cloud water
!     cw_y     - meridional derivative of cloud water
!     mype     - task id
!
!   output argument list:
!     u_t      - time tendency of u
!     v_t      - time tendency of v
!     t_t      - time tendency of t
!     p_t      - time tendency of 3d prs
!     q_t      - time tendency of q
!     oz_t     - time tendency of ozone
!     cw_t     - time tendency of cloud water
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds,only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig,istart,rlats,nlat,idvc5,bk5,&
     jstart,region_lat,region_lon,eta2_ll,wrf_nmm_regional,nems_nmmb_regional,nlon,regional,&
     region_dx,region_dy
  use constants, only: ione,zero,half,one,two,rearth,rd,rcp,omega,grav
  use tendsmod, only: what9,prsth9,r_prsum9,r_prdif9,prdif9,pr_xsum9,pr_xdif9,pr_ysum9,&
     pr_ydif9,curvx,curvy,coriolis
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig)     ,intent(in   ) :: u,v,t,u_x,u_y,v_x,v_y,&
     t_x,t_y,q,oz,cw,q_x,q_y,oz_x,oz_y,cw_x,cw_y
  real(r_kind),dimension(lat2,lon2)          ,intent(in   ) :: ps_x,ps_y,z
  integer(i_kind)                            ,intent(in   ) :: mype
  real(r_kind),dimension(lat2,lon2,nsig)     ,intent(inout) :: teta
  real(r_kind),dimension(lat2,lon2,nsig)     ,intent(  out) :: u_t,v_t,t_t,q_t,oz_t,cw_t
  real(r_kind),dimension(lat2,lon2,nsig+ione),intent(  out) :: p_t
  real(r_kind),dimension(lat2,lon2,nsig+ione),intent(in   ) :: pri

! Declare local variables
  real(r_kind),dimension(lat2,lon2):: z_x,z_y
  real(r_kind),dimension(lat2,lon2,nsig+ione):: pri_x,pri_y
  real(r_kind),dimension(lat2,lon2):: sumkm1,sumvkm1,sum2km1,sum2vkm1
  real(r_kind) tmp,tmp2
  integer(i_kind) i,j,k,ix,ixm,ixp,jx,jxm,jxp,nnn
  real(r_kind) sumk,sumvk,sum2k,sum2vk,uuvv


! NOTES:
!  - equations taken from NCEP Office Note 445 (Juang 2005)
!  - this is the nonlinear routine, which currently in the GSI is only
!    called based on the current guess solution.  As such, basic state
!    variables that are needed for the TLM are loaded here (in the *9
!    arrays)

  what9=zero

! constants
  if(wrf_nmm_regional.or.nems_nmmb_regional) then
     do j=1,lon2
        jx=j+jstart(mype+ione)-2_i_kind
        jx=min(max(ione,jx),nlon)
        jxp=jx+ione
        jxm=jx-ione
        if(jx==ione) then
           jxp=2_i_kind
           jxm=ione
        elseif(jx==nlon) then
           jxp=nlon
           jxm=nlon-ione
        end if
        do i=1,lat2
           ix=istart(mype+ione)+i-2_i_kind
           ix=min(max(ix,ione),nlat)
           ixp=ix+ione
           ixm=ix-ione
           if(ix==ione) then
              ixp=2_i_kind
              ixm=ione
           elseif(ix==nlat) then
              ixp=nlat
              ixm=nlat-ione
           end if
           coriolis(i,j)=two*omega*sin(region_lat(ix,jx))
           curvx(i,j)=(region_dy(ix,jxp)-region_dy(ix,jxm))/((jxp-jxm)*region_dx(ix,jx)*region_dy(ix,jx))
           curvy(i,j)=(region_dx(ixp,jx)-region_dx(ixm,jx))/((ixp-ixm)*region_dx(ix,jx)*region_dy(ix,jx))
        end do
     end do
  else
     do j=1,lon2
        do i=1,lat2
           ix=istart(mype+ione)+i-2_i_kind
           ix=min(max(ix,2_i_kind),nlat-ione)
           coriolis(i,j)=two*omega*sin(rlats(ix))
           curvx(i,j)=zero
           curvy(i,j)=-tan(rlats(ix))/rearth
        end do
     end do
  end if

  call get_zderivs(z,z_x,z_y,mype)
! preliminaries
  call getprs_horiz(ps_x,ps_y,pri,pri_x,pri_y)

  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           r_prsum9(i,j,k)=one/(pri(i,j,k)+pri(i,j,k+ione))
           prdif9(i,j,k)=pri(i,j,k)-pri(i,j,k+ione)
           r_prdif9(i,j,k)=one/prdif9(i,j,k)
           pr_xsum9(i,j,k)=pri_x(i,j,k)+pri_x(i,j,k+ione)
           pr_xdif9(i,j,k)=pri_x(i,j,k)-pri_x(i,j,k+ione)
           pr_ysum9(i,j,k)=pri_y(i,j,k)+pri_y(i,j,k+ione)
           pr_ydif9(i,j,k)=pri_y(i,j,k)-pri_y(i,j,k+ione)
        end do
     end do
  end do



! 1) Compute horizontal part of tendency for 3d pressure (so dps/dt is the same
!    as prsth9(i,j,1) . . . also note that at the top, dp/dt=0
!    or: prsth9(i,j,nsig+ione)=zero
  do j=1,lon2
     do i=1,lat2
        prsth9(i,j,nsig+ione)=zero
     end do
  end do
  do k=nsig,1,-1
     do j=1,lon2
        do i=1,lat2
           prsth9(i,j,k)=prsth9(i,j,k+ione) - ( u(i,j,k)*pr_xdif9(i,j,k) + &
                v(i,j,k)*pr_ydif9(i,j,k)    + (u_x(i,j,k) + v_y(i,j,k))* &
                prdif9(i,j,k) )
        end do
     end do
  end do   

! 1.1) Compute horizontal part of tendency for T (needed for vertical velocity in hybrid theta coordinates)

  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           tmp=-rd*t(i,j,k)*r_prsum9(i,j,k)
           t_t(i,j,k)=-u(i,j,k)*t_x(i,j,k) - v(i,j,k)*t_y(i,j,k)
           t_t(i,j,k)=t_t(i,j,k) -tmp*rcp * ( u(i,j,k)*pr_xsum9(i,j,k) + &
              v(i,j,k)*pr_ysum9(i,j,k) + &
              prsth9(i,j,k) + prsth9(i,j,k+ione) )
        end do  !end do j
     end do    !end do i

  end do  !end do k

! 2) calculate vertical velocity term:  z(dp/dz) (zero at top/bottom interfaces)
! if running global, and there is a c(k) coefficient, we call the vvel subroutine
  if ( (.not.regional) .AND. (idvc5==3_i_kind)) then
     call getvvel(t,t_t,prsth9,prdif9,what9)
  else
     do k=2,nsig
        do j=1,lon2
           do i=1,lat2
              if(wrf_nmm_regional.or.nems_nmmb_regional) then
                 what9(i,j,k)=prsth9(i,j,k)-eta2_ll(k)*prsth9(i,j,1)
              else
                 what9(i,j,k)=prsth9(i,j,k)-bk5(k)*prsth9(i,j,1)
              end if
           end do
        end do
     end do
  end if ! end if on 

! 3) load actual dp/dt here now, as prsth9 is reused in 
!    what9(i,k,1) & what9(i,j,nsig+ione) = zero
!    p_t(i,j,1) is the same as the surface pressure tendency
  do k=1,nsig+ione
     do j=1,lon2
        do i=1,lat2
           p_t(i,j,k)=prsth9(i,j,k)-what9(i,j,k)
        end do
     end do
  end do

! before big k loop, zero out the km1 summation arrays
  do j=1,lon2
     do i=1,lat2
        sumkm1(i,j)=zero
        sum2km1(i,j)=zero
        sumvkm1(i,j)=zero
        sum2vkm1(i,j)=zero
     end do
  end do

! 4) Compute tendencies for wind components & Temperature
  do k=1,nsig
     do j=1,lon2
        do i=1,lat2
           tmp=-rd*t(i,j,k)*r_prsum9(i,j,k)
           uuvv=u(i,j,k)*u(i,j,k)+v(i,j,k)*v(i,j,k)
           u_t(i,j,k)=-u(i,j,k)*u_x(i,j,k) - v(i,j,k)*u_y(i,j,k) + &
              coriolis(i,j)*v(i,j,k) + curvx(i,j)*uuvv
           u_t(i,j,k)=u_t(i,j,k) + pr_xsum9(i,j,k)*tmp - grav*z_x(i,j)

           v_t(i,j,k)=-u(i,j,k)*v_x(i,j,k) - v(i,j,k)*v_y(i,j,k) - &
              coriolis(i,j)*u(i,j,k) + curvy(i,j)*uuvv
           v_t(i,j,k)=v_t(i,j,k) + pr_ysum9(i,j,k)*tmp - grav*z_y(i,j)

! horizontal advection of "tracer" quantities
           q_t (i,j,k) = -u(i,j,k)*q_x (i,j,k) - v(i,j,k)*q_y (i,j,k)
           oz_t(i,j,k) = -u(i,j,k)*oz_x(i,j,k) - v(i,j,k)*oz_y(i,j,k)
           cw_t(i,j,k) = -u(i,j,k)*cw_x(i,j,k) - v(i,j,k)*cw_y(i,j,k)
 
! vertical flux terms
           if (k>ione) then
              tmp = half*what9(i,j,k)*r_prdif9(i,j,k)
              u_t (i,j,k) = u_t (i,j,k) - tmp*(u (i,j,k-1)-u (i,j,k))
              v_t (i,j,k) = v_t (i,j,k) - tmp*(v (i,j,k-1)-v (i,j,k))
              t_t (i,j,k) = t_t (i,j,k) - tmp*(t (i,j,k-1)-t (i,j,k))
              q_t (i,j,k) = q_t (i,j,k) - tmp*(q (i,j,k-1)-q (i,j,k))
              oz_t(i,j,k) = oz_t(i,j,k) - tmp*(oz(i,j,k-1)-oz(i,j,k))
              cw_t(i,j,k) = cw_t(i,j,k) - tmp*(cw(i,j,k-1)-cw(i,j,k))
           end if
           if (k<nsig) then
              tmp = half*what9(i,j,k+ione)*r_prdif9(i,j,k)
              u_t (i,j,k) = u_t (i,j,k) - tmp*(u (i,j,k)-u (i,j,k+ione))
              v_t (i,j,k) = v_t (i,j,k) - tmp*(v (i,j,k)-v (i,j,k+ione))
              t_t (i,j,k) = t_t (i,j,k) - tmp*(t (i,j,k)-t (i,j,k+ione))
              q_t (i,j,k) = q_t (i,j,k) - tmp*(q (i,j,k)-q (i,j,k+ione))
              oz_t(i,j,k) = oz_t(i,j,k) - tmp*(oz(i,j,k)-oz(i,j,k+ione))
              cw_t(i,j,k) = cw_t(i,j,k) - tmp*(cw(i,j,k)-cw(i,j,k+ione))
           end if        
        end do  !end do j
     end do    !end do i

     do j=1,lon2
        do i=1,lat2
           tmp = rd*t(i,j,k)*r_prsum9(i,j,k)
           tmp2 = prdif9(i,j,k)*r_prsum9(i,j,k)
           sumk=sumkm1(i,j) + tmp*( pr_xdif9(i,j,k) - &
               tmp2*pr_xsum9(i,j,k) )
           sumvk=sumvkm1(i,j) + tmp*( pr_ydif9(i,j,k) - &
               tmp2*pr_ysum9(i,j,k) )
           sum2k=sum2km1(i,j) + t_x(i,j,k)*tmp2
           sum2vk=sum2vkm1(i,j) + t_y(i,j,k)*tmp2
 
           u_t(i,j,k) = u_t(i,j,k) - sumkm1(i,j) - rd*sum2km1(i,j) - &
              sumk - rd*sum2k 
           v_t(i,j,k) = v_t(i,j,k) - sumvkm1(i,j) - rd*sum2vkm1(i,j) - &
              sumvk - rd*sum2vk 
 

! load up the km1 arrays for next k loop
           sumkm1(i,j)=sumk
           sumvkm1(i,j)=sumvk
           sum2km1(i,j)=sum2k
           sum2vkm1(i,j)=sum2vk
        end do
     end do
  end do  !end do k

  call turbl(u,v,pri,t,teta,z,u_t,v_t,t_t)

  if(.not.wrf_nmm_regional.and..not.nems_nmmb_regional)then
     do k=1,nsig

        do j=1,lon2
           do i=1,lat2
              ix=istart(mype+ione)+i-2_i_kind
              if (ix == ione .OR. ix == nlat) then
                 u_t(i,j,k)=zero
                 v_t(i,j,k)=zero
              end if
           end do 
        end do

     end do  !end do k
  end if

  return
end subroutine calctends
