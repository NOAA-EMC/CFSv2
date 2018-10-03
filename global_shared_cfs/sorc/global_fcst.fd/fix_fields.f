
      subroutine fix_fields(
     &                  lonsperlar,global_lats_r,xlon,xlat,sfc_fld,
     &                  nst_fld,hprime,jindx1,jindx2,ddy,ozplin,
     &                  cread,cread_nst)
!!     
      use machine , only : kind_rad
      use funcphys                         
      use resol_def
      use namelist_def
      use layout1
      use ozne_def
      use sfc_flx_esmfmod
      use nst_var_esmfmod
      implicit none
!!     
      integer nread, nread_nst
      type(sfc_var_data)  :: sfc_fld
      type(nst_var_data)  :: nst_fld
      character (len=*)   :: cread
      character (len=*)   :: cread_nst
      integer jindx1(lats_node_r),jindx2(lats_node_r)
      real (kind=kind_rad) ddy(lats_node_r)
      real (kind=kind_rad) hprime(lonr,nmtvr,lats_node_r)

      integer iozondp
      real (kind=kind_rad) ozplin(latsozp,levozp,pl_coeff,timeoz)
     &,                    xlon(lonr,lats_node_r)
     &,                    xlat(lonr,lats_node_r)
       
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)
      real, parameter:: rlapse=0.65e-2
      real dt_warm
      integer needoro, i, j
!!     
      call gfuncphys
!!     
      iozondp = 0
      if (ntoz .gt. 0) iozondp = 1
      nread   = 14
!     cread   = 'fort.14'
!     sfc_fld%oro     = 0.
!     needoro = 0
!
      needoro = 1
      call read_mtn_hprim_oz(sfc_fld%slmsk,hprime,needoro,sfc_fld%oro,
     &                       sfc_fld%oro_uf,
     &                       iozondp,ozplin, global_lats_r,lonsperlar)
!
      needoro = 0
      if(.not.adiab)then
        if (fhini == fhrot) then
          if (me == 0) print *,' call read_sfc cread=',cread
          call read_sfc(sfc_fld,needoro,nread,
     &                  cread,global_lats_r,lonsperlar)

          if (nst_fcst > 0) then
            if (me == 0) print *,' call read_nst nst_spinup : ',
     &                             nst_spinup
            nst_fld%slmsk = sfc_fld%slmsk
            if ( nst_spinup ) then
              call set_nst(sfc_fld%tsea,nst_fld)
            else
              nread_nst   = 15
              call read_nst(nst_fld,nread_nst,cread_nst,
     &                      global_lats_r,lonsperlar)
              if (  nst_fcst > 1 ) then
                do j = 1, lats_node_r
                  do i = 1, lonr
                    if ( sfc_fld%slmsk(i,j) == 0.0 ) then
                      dt_warm = (nst_fld%xt(i,j)+nst_fld%xt(i,j))
     &                        /  nst_fld%xz(i,j)
                      sfc_fld%tsea(i,j) = nst_fld%tref(i,j)
     &                 + dt_warm - nst_fld%dt_cool(i,j)
     &                 - (sfc_fld%oro(i,j)-sfc_fld%oro_uf(i,j)) * rlapse
                    endif
                  enddo
                enddo
!
!    when am and nst is not coupled, tsea (in surface file) ==> tref
!
              elseif (nst_fcst == 1) then
                nst_fld%tref = sfc_fld%tsea
              endif
!
!   reset the non-water points, since no mask (sea ice) update for nstanl file
!   done at present
!
              call nst_reset_nonwater(sfc_fld%tsea,nst_fld)
            endif                        ! if ( nst_spinup ) then
          endif                          ! if ( nst_fcst > 0 ) then
        else
          if (me .eq. 0) print *,' call read_sfc_r cread=',cread
          call read_sfc_r(sfc_fld,needoro,nread,
     &                    cread,global_lats_r,lonsperlar)

          if ( nst_fcst > 0 ) then
            nst_fld%slmsk = sfc_fld%slmsk
            nread_nst   = 15
            if (me .eq. 0) print *,' call read_nst_r cread=',cread_nst
            call read_nst_r(nst_fld,nread_nst,cread_nst,
     &                      global_lats_r,lonsperlar)
          endif
        endif
      endif
!     needoro=1
!     call read_mtn_hprim_oz(sfc_fld%slmsk,hprime,needoro,sfc_fld%oro,
!    &     iozondp,ozplin, global_lats_r,lonsperlar)
!      
      call setindxoz(lats_node_r,lats_node_r,global_lats_r,
     &               jindx1,jindx2,ddy)
!      
      call lonlat_para(global_lats_r,xlon,xlat,lonsperlar)
!!     
      return
      end
