!-----------------------------------
      subroutine sfc_ocean                                              &
!...................................
!  ---  inputs:
     &     ( im, ps, u1, v1, t1, q1, tskin, cm, ch, rcl,                &
     &       prsl1, prslki, slimsk, ddvel, flag_iter,                   &
!  ---  outputs:
     &       qsurf, cmm, chh, evap, hflx                                &
     &     )

! ===================================================================== !
!  description:                                                         !
!                                                                       !
!  usage:                                                               !
!                                                                       !
!    call sfc_ocean                                                     !
!       inputs:                                                         !
!          ( im, ps, u1, v1, t1, q1, tskin, cm, ch, rcl,                !
!            prsl1, prslki, slimsk, ddvel, flag_iter,                   !
!       outputs:                                                        !
!            qsurf, cmm, chh, evap, hflx )                              !
!                                                                       !
!                                                                       !
!  subprograms/functions called: fpvs                                   !
!                                                                       !
!                                                                       !
!  program history log:                                                 !
!         xxxx  --             created                                  !
!    oct  2006  -- h. wei      modified (need description)              !
!    apr  2009  -- y.-t. hou   modified to match the modified gbphys_v.f!
!                     rmoved unused variable from argument list.        !
!                     reformatted code and added program documentation. !
!                                                                       !
!                                                                       !
!  ====================  defination of variables  ====================  !
!                                                                       !
!  inputs:                                                       size   !
!     im       - integer, horizontal dimension                     1    !
!     ps       - real, surface pressure                            im   !
!     u1, v1   - real, u/v component of surface layer wind         im   !
!     t1       - real, surface layer mean temperature ( k )        im   !
!     q1       - real, surface layer mean specific humidity        im   !
!     tskin    - real, ground surface skin temperature ( k )       im   !
!     cm       - real, surface exchange coeff for momentum (m/s)   im   !
!     ch       - real, surface exchange coeff heat & moisture(m/s) im   !
!     rcl      - real,                                             im   !
!     prsl1    - real, surface layer mean pressure                 im   !
!     prslki   - real,                                             im   !
!     slimsk   - real, sea/land/ice mask (=0/1/2)                  im   !
!     ddvel    - real,                                             im   !
!     flag_iter- logical,                                          im   !
!                                                                       !
!  outputs:                                                             !
!     qsurf    - real, specific humidity at sfc                    im   !
!     cmm      - real,                                             im   !
!     chh      - real,                                             im   !
!     evap     - real, evaperation from latent heat flux           im   !
!     hflx     - real, sensible heat flux                          im   !
!                                                                       !
! ===================================================================== !
!
      use machine , only : kind_phys
      use funcphys, only : fpvs
      use physcons, only : cp => con_cp, rd => con_rd, eps => con_eps,  &
     &                     epsm1 => con_epsm1, hvap => con_hvap,        &
     &                     rvrdm1 => con_fvirt
!
      implicit none
!
!  ---  constant parameters:
      real (kind=kind_phys), parameter :: cpinv  = 1.0/cp
      real (kind=kind_phys), parameter :: hvapi  = 1.0/hvap
      real (kind=kind_phys), parameter :: elocp  = hvap/cp

!  ---  inputs:
      integer, intent(in) :: im

      real (kind=kind_phys), dimension(im), intent(in) :: ps, u1, v1,   &
     &      t1, q1, tskin, cm, ch, rcl, prsl1, prslki, slimsk, ddvel

      logical, intent(in) :: flag_iter(im)

!  ---  outputs:
      real (kind=kind_phys), dimension(im), intent(out) :: qsurf,       &
     &       cmm, chh, evap, hflx

!  ---  locals:
      real (kind=kind_phys), dimension(im) :: psurf, ps1, q0, qss,      &
     &       rch, rho, theta1, tv1, xrcl, wind

      real (kind=kind_phys) :: tem

      integer :: i

      logical :: flag(im)
!
!===> ...  begin here
!
!  --- ...  flag for open water
      do i = 1, im
         flag(i) = ( slimsk(i)==0.0 .and. flag_iter(i) )
      enddo

!  --- ...  initialize variables. all units are supposedly m.k.s. unless specified
!           psurf is in pascals, wind is wind speed, theta1 is adiabatic surface
!           temp from level 1, rho is density, qss is sat. hum. at surface

      do i = 1, im
        if ( flag(i) ) then
          xrcl(i)  = sqrt( rcl(i) )
          psurf(i) = 1000.0 * ps(i)
          ps1(i)   = 1000.0 * prsl1(i)
          theta1(i) = t1(i) * prslki(i)

          wind(i)  = xrcl(i) * sqrt(u1(i)*u1(i) + v1(i)*v1(i))          &
     &             + max( 0.0, min( ddvel(i), 30.0 ) )
          wind(i)  = max( wind(i), 1.0 )

          q0(i) = max( q1(i), 1.0e-8 )
          tv1(i) = t1(i) * (1.0 + rvrdm1*q0(i))
          rho(i) = ps1(i) / (rd*tv1(i))

          qss(i) = fpvs( tskin(i) )
          qss(i) = eps*qss(i) / (psurf(i) + epsm1*qss(i))
        endif
      enddo

      do i = 1, im
        if ( flag(i) ) then
          evap(i) = 0.0
          hflx(i) = 0.0
        endif
      enddo

!  --- ...  rcp = rho cp ch v

      do i = 1, im
        if ( flag(i) ) then
          rch(i) = rho(i) * cp * ch(i) * wind(i)
          cmm(i) = cm(i) * wind(i)
          chh(i) = rho(i) * ch(i) * wind(i)
        endif
      enddo

!  --- ...  sensible and latent heat flux over open water

      do i = 1, im
        if ( flag(i) ) then
          hflx(i) = rch(i) * (tskin(i) - theta1(i))
          evap(i) = elocp*rch(i) * (qss(i) - q0(i))
          qsurf(i) = qss(i)
        endif
      enddo

      do i = 1, im
        if ( flag(i) ) then
          tem     = 1.0 / rho(i)
          hflx(i) = hflx(i) * tem * cpinv
          evap(i) = evap(i) * tem * hvapi
        endif
      enddo
!
      return
!...................................
      end subroutine sfc_ocean
!-----------------------------------
