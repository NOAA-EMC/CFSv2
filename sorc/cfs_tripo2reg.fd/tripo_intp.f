!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!                                                                              !
!  This program reads ocean and ice data on tripolar grid                      !
!  and interpolate the data onto regular lat/lon grid.                         !
!  It contains 5 subroutines: read_ocn_tp, read_ice_tp, tripo_interp40tc,      !
!                             tripo_interp1tc, tripo                           !
!                                                                              !
!  Xingren Wu   (Xingren.Wu@noaa.gov)                                          !
!     May 18, 2007                                                             !
!  Xingren Wu                                                                  !
!     Nov 16, 2007 - modified                                                  !
!         fix kpds(13) to kpds(15) for handle different forecast time unit     !
!  Xingren Wu                                                                  !
!     Dec 4, 2007 - modified                                                   ! 
!        add extra fields to match production output                           !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

module tripo_intp_mod

      implicit none

contains

      subroutine read_ocn_tp(im,jm,km,geolon_c,geolat_c,geolon_t,geolat_t, &
             t,s,u,v,w,eta,sfcflx,pme,mld,taux,tauy,dckt,dcks,vfc,ocean_file)
      include "netcdf.inc"
      integer im,jm,km,i,j
      integer status, ncid 
      integer :: id_geolon_c,id_geolon_t,id_geolat_c,id_geolat_t, &
                 id_temp,id_salt,id_u,id_v,id_wt,                 &
                 id_eta_t,id_sfc_hflux,id_pme,id_mld,id_tau_x,id_tau_y
      integer :: id_diff_cbt_kpp_t,id_diff_cbt_kpp_s,id_vert_fric_coeff
      real, dimension(im,jm,km) :: t,s,u,v,w
      real, dimension(im,jm,km) :: dckt,dcks,vfc
      real, dimension(im,jm)    :: eta,sfcflx,pme,mld,taux,tauy, &
                                   geolon_c,geolon_t,geolat_c,geolat_t
      character(len=120) :: ocean_file

      status = nf_open(ocean_file, NF_NOWRITE, ncid)
      status = nf_inq_varid(ncid, 'temp ', id_temp)
      status = nf_inq_varid(ncid, 'salt ', id_salt)
      status = nf_inq_varid(ncid, 'u'    , id_u   )
      status = nf_inq_varid(ncid, 'v'    , id_v   )
      status = nf_inq_varid(ncid, 'wt  ' , id_wt)
      status = nf_inq_varid(ncid, 'eta_t', id_eta_t)
      status = nf_inq_varid(ncid, 'sfc_hflux', id_sfc_hflux)
      status = nf_inq_varid(ncid, 'pme  ', id_pme)
      status = nf_inq_varid(ncid, 'mld',   id_mld)
      status = nf_inq_varid(ncid, 'tau_x', id_tau_x)
      status = nf_inq_varid(ncid, 'tau_y', id_tau_y)
      status = nf_inq_varid(ncid, 'diff_cbt_kpp_t',id_diff_cbt_kpp_t)
      status = nf_inq_varid(ncid, 'diff_cbt_kpp_s',id_diff_cbt_kpp_s)
      status = nf_inq_varid(ncid, 'vert_fric_coeff',id_vert_fric_coeff)
      status = nf_inq_varid(ncid, 'geolon_c' , id_geolon_c)
      status = nf_inq_varid(ncid, 'geolon_t' , id_geolon_t)
      status = nf_inq_varid(ncid, 'geolat_c' , id_geolat_c)
      status = nf_inq_varid(ncid, 'geolat_t' , id_geolat_t)

! read variable
      status = nf_get_var_real(ncid, id_geolon_c,   geolon_c)
      status = nf_get_var_real(ncid, id_geolon_t,   geolon_t)
      status = nf_get_var_real(ncid, id_geolat_c,   geolat_c)
      status = nf_get_var_real(ncid, id_geolat_t,   geolat_t)
      status = nf_get_var_real(ncid, id_temp,       t)       !temp
      status = nf_get_var_real(ncid, id_salt,       s)       !salt
      status = nf_get_var_real(ncid, id_u,          u)       !u
      status = nf_get_var_real(ncid, id_v,          v)       !v
      status = nf_get_var_real(ncid, id_wt,         w)       !wt
      status = nf_get_var_real(ncid, id_eta_t,      eta)     !eta_t
      status = nf_get_var_real(ncid, id_sfc_hflux,  sfcflx)  !sfc_hflux
      status = nf_get_var_real(ncid, id_pme,        pme)     !pme
      status = nf_get_var_real(ncid, id_mld,        mld)     !mld
      status = nf_get_var_real(ncid, id_tau_x,      taux)    !tau_x
      status = nf_get_var_real(ncid, id_tau_y,      tauy)    !tau_y
      status = nf_get_var_real(ncid,id_diff_cbt_kpp_t,dckt)  !diff_cbt_kpp_t
      status = nf_get_var_real(ncid,id_diff_cbt_kpp_s,dcks)  !diff_cbt_kpp_s
      status = nf_get_var_real(ncid,id_vert_fric_coeff,vfc)  !vert_fric_coeff

      return

      end subroutine read_ocn_tp

      subroutine read_ice_tp(im,jm,ki,sinrot,cosrot,&
             hi,hs,ts,t1,t2,fi,alb,ui,vi,sst,saltf,icefile)
	     
      include "netcdf.inc" 
          
      character(len=120) :: icefile
      integer :: status,ncid
      integer :: im,jm,ki,i,j
      integer :: id_hi,id_hs,id_ts,id_t1,id_t2,id_cn,id_alb,id_ui,id_vi, &
                 id_sst,id_saltf,id_sinrot,id_cosrot
      real*8, dimension(im,jm) :: hi,hs,ts,t1,t2,fi,alb,ui,vi,sst,saltf,cosrot,sinrot
      real*8, dimension(im,jm,ki) :: cn 
       
      print *, 'icefile:', icefile
      status = nf_open(icefile, NF_NOWRITE, ncid)
      print *, 'status:', status
      if(status.ne.NF_NOERR) stop 'open'
      
      status = nf_inq_varid(ncid, 'HI       ', id_hi)
      status = nf_inq_varid(ncid, 'HS       ', id_hs)
      status = nf_inq_varid(ncid, 'TS       ', id_ts)
      status = nf_inq_varid(ncid, 'T1       ', id_t1)
      status = nf_inq_varid(ncid, 'T2       ', id_t2)
      status = nf_inq_varid(ncid, 'CN       ', id_cn)
      status = nf_inq_varid(ncid, 'ALB      ', id_alb)
      status = nf_inq_varid(ncid, 'UI       ', id_ui)
      status = nf_inq_varid(ncid, 'VI       ', id_vi)
      status = nf_inq_varid(ncid, 'SST      ', id_sst)
      status = nf_inq_varid(ncid, 'SALTF    ', id_saltf)
      status = nf_inq_varid(ncid, 'SINROT   ', id_sinrot)
      status = nf_inq_varid(ncid, 'COSROT   ', id_cosrot)
          
!
! read hi,hs,ts,t1,t2,alb,ui,vi,sst,saltf
!
      status = nf_get_var_double(ncid, id_hi       ,hi)
      status = nf_get_var_double(ncid, id_hs       ,hs)
      status = nf_get_var_double(ncid, id_ts       ,ts)
      status = nf_get_var_double(ncid, id_t1       ,t1)
      status = nf_get_var_double(ncid, id_t2       ,t2)
      status = nf_get_var_double(ncid, id_cn       ,cn)
      status = nf_get_var_double(ncid, id_alb      ,alb)
      status = nf_get_var_double(ncid, id_ui       ,ui)
      status = nf_get_var_double(ncid, id_vi       ,vi)
      status = nf_get_var_double(ncid, id_sst      ,sst)
      status = nf_get_var_double(ncid, id_saltf    ,saltf)
      status = nf_get_var_double(ncid, id_sinrot   ,sinrot)
      status = nf_get_var_double(ncid, id_cosrot   ,cosrot)
      
      fi = sum(cn,3)
      fi(:,:) = fi(:,:)-cn(:,:,1)
      do j=1,jm
      do i=1,im
         if (hi(i,j).lt.1e-5) then
            ts(i,j)=sst(i,j)
            t1(i,j)=sst(i,j)
            t2(i,j)=sst(i,j)
         endif
         if (fi(i,j).gt.1.00001) then
            print *,'Warning: fi>1:',fi(i,j)
            fi(i,j)=1.0
         endif
      enddo
      enddo
      fi = sum(cn,3)

      return

      end subroutine read_ice_tp

      subroutine tripo_interp40tc(im,jm,ko,im1,jm1,im2,jm2,im3,jm3,im4,jm4, &
                           imtp,jmtp,imo,jmo,imos,imoe,imop1,data,dato,tgrid)

      use mod_interp

      integer im,jm,ko
      integer im1,im2,im3,im4,imtp,imo,imos,imoe,imop1
      integer jm1,jm2,jm3,jm4,jmtp,jmo
      real    spv
      parameter(spv=-1.e+34)
!
      integer i,j,k
!
      real, dimension(im,jm,ko) :: data
      real, dimension(imo,jmo,ko) :: dato
!
      real*8, dimension(im1,jm1)  :: lon1,lat1,var1
      real*8, dimension(im2,jm2)  :: lon2,lat2,var2
      real*8, dimension(im3,jm3)  :: lon3,lat3,var3
      real*8, dimension(im4,jm4)  :: lon4,lat4,var4
      real*8, dimension(imo,jmo)    :: lont,latt,vart
      real*8, dimension(imop1,jmo)  :: lonc,latc,varc
      real*8, dimension(imtp,jmtp)  :: vartp
!
      integer, dimension(im1,jm1,ko)  :: mask1,IG1,JG1
      integer, dimension(im2,jm2,ko)  :: mask2
      integer, dimension(im3,jm3,ko)  :: mask3,IG3,JG3
      integer, dimension(im4,jm4,ko)  :: mask4
      integer, dimension(imo,jmo)     :: maskt,num_levels
      integer, dimension(imop1,jmo)   :: maskc
      integer, dimension(imop1,jmo,ko)  :: IG,JG
      integer  :: ierr
      character(len=120) :: ocnintp_grid
      logical  tgrid
!     print *,'tgrid=', tgrid
      if (tgrid) then
         ocnintp_grid='OCNINTPCOEF.T'
      else
         ocnintp_grid='OCNINTPCOEF.C'
      endif
!
      open(21,file=ocnintp_grid,status='old',form='unformatted')
      rewind (21)
      read (21) lon1
      read (21) lat1
      read (21) lon2
      read (21) lat2
      read (21) lon3
      read (21) lat3
      read (21) lon4
      read (21) lat4
      read (21) lont
      read (21) latt
      read (21) lonc
      read (21) latc
      do k=1,ko
         read (21) mask1(:,:,k)
         read (21) mask2(:,:,k)
         read (21) mask3(:,:,k)
         read (21) mask4(:,:,k)
         read (21) IG1(:,:,k)
         read (21) JG1(:,:,k)
         read (21) IG3(:,:,k)
         read (21) JG3(:,:,k)
         read (21) IG(:,:,k)
         read (21) JG(:,:,k)
      enddo
      read (21) num_levels
      close (21)
!     open(50,file='num_levels_1x1.data',access='direct',recl=imo*jmo*4,form='unformatted')
!     read(50,rec=1) num_levels
!     close (50)
      do k=1,ko
         var2=spv
         var4=spv
         do j=1,jm1
         do i=1,im1
            var1(i,j)=data(i,j,k)
         enddo
         enddo
         do j=1,jmtp
         do i=1,imtp
            vartp(i,j)=data(i,jm1+j-1,k)
         enddo
         enddo
         do i=1,jm3
         do j=1,im3/2
            var3(j,i)=vartp(i,j)
            var3(jmtp+j,i)=vartp(im-i+1,jmtp-j+1)
         enddo
         enddo
         call INTERP(im1,jm1,im2,jm2,lon1,lat1,lon2,lat2,mask1(:,:,k),mask2(:,:,k),IG1(:,:,k),JG1(:,:,k),var1,var2,ierr,NOSPVAL=.true.)
         call INTERP(im3,jm3,im4,jm4,lon3,lat3,lon4,lat4,mask3(:,:,k),mask4(:,:,k),IG3(:,:,k),JG3(:,:,k),var3,var4,ierr,NOSPVAL=.true.)
!
         do i=1,imo
            do j=1,jm2
               vart(i,j)=var2(i,j)
            enddo
            do j=jm2+1,jmo
               vart(i,j)=var4(i,j-jm2)
            enddo
         enddo
         if (.NOT. tgrid) then
           do j=1,jmo
              do i=2,imop1
                 maskc(i,j)=0
                 varc(i,j)=vart(i-1,j)
                 if (varc(i,j).LE.spv) then
                    varc(i,j)=spv
                    maskc(i,j)=1
                 endif
              enddo
              varc(1,j)=varc(imop1,j)
              maskc(1,j)=maskc(imop1,j)
           enddo
           vart=spv
           maskt=0
           call INTERP(imop1,jmo,imo,jmo,lonc,latc,lont,latt,maskc,maskt,IG(:,:,k),JG(:,:,k),varc,vart,ierr,NOSPVAL=.true.)
         endif
! QC
        do j=1,jmo
        do i=1,imo
           if (num_levels(i,j) .LT. k) vart(i,j)=spv
        enddo
        enddo
!
! arrange data so it is for: 0 -> 360, 90 N -> 90S and 4478 -> 0
!
        do j=1,jmo
           do i=1,imos
              dato(i,j,ko-k+1)=vart(i+imoe,jmo-j+1)
           enddo
           do i=imos+1,imo
              dato(i,j,ko-k+1)=vart(i-imos,jmo-j+1)
           enddo
        enddo
      enddo
!
      return

      end subroutine tripo_interp40tc

      subroutine tripo_interp1tc(im,jm,ko,im1,jm1,im2,jm2,im3,jm3,im4,jm4, &
                          imtp,jmtp,imo,jmo,imos,imoe,imop1,data,dato,tgrid)

      use mod_interp

      integer im,jm,ko
      integer im1,im2,im3,im4,imtp,imo,imos,imoe,imop1
      integer jm1,jm2,jm3,jm4,jmtp,jmo
      real    spv
      parameter(spv=-1.e+34)
!
      integer i,j,k
!
      real, dimension(im,jm) :: data
      real, dimension(imo,jmo) :: dato
!
      real*8, dimension(im1,jm1)  :: lon1,lat1,var1
      real*8, dimension(im2,jm2)  :: lon2,lat2,var2
      real*8, dimension(im3,jm3)  :: lon3,lat3,var3
      real*8, dimension(im4,jm4)  :: lon4,lat4,var4
      real*8, dimension(imo,jmo)    :: lont,latt,vart
      real*8, dimension(imop1,jmo)  :: lonc,latc,varc
      real*8, dimension(imtp,jmtp)  :: vartp

!
      integer, dimension(im1,jm1,ko)  :: mask1,IG1,JG1
      integer, dimension(im2,jm2,ko)  :: mask2
      integer, dimension(im3,jm3,ko)  :: mask3,IG3,JG3
      integer, dimension(im4,jm4,ko)  :: mask4
      integer, dimension(imo,jmo)     :: maskt,num_levels
      integer, dimension(imop1,jmo)   :: maskc
      integer, dimension(imop1,jmo,ko)  :: IG,JG
      integer  :: ierr
      character(len=120) :: ocnintp_grid
      logical  tgrid
!
!     print *,'tgrid=', tgrid
      if (tgrid) then
         ocnintp_grid='OCNINTPCOEF.T'
      else
         ocnintp_grid='OCNINTPCOEF.C'
      endif
!
      open(21,file=ocnintp_grid,status='old',form='unformatted')
      rewind (21)
      read (21) lon1
      read (21) lat1
      read (21) lon2
      read (21) lat2
      read (21) lon3
      read (21) lat3
      read (21) lon4
      read (21) lat4
      read (21) lont
      read (21) latt
      read (21) lonc
      read (21) latc
      do k=1,ko
         read (21) mask1(:,:,k)
         read (21) mask2(:,:,k)
         read (21) mask3(:,:,k)
         read (21) mask4(:,:,k)
         read (21) IG1(:,:,k)
         read (21) JG1(:,:,k)
         read (21) IG3(:,:,k)
         read (21) JG3(:,:,k)
         read (21) IG(:,:,k)
         read (21) JG(:,:,k)
      enddo
      read (21) num_levels
      close (21)
!     open(50,file='num_levels_1x1.data',access='direct',recl=imo*jmo*4,form='unformatted')
!     read(50,rec=1) num_levels
!     close (50)
      var2=spv
      var4=spv
      do j=1,jm1
      do i=1,im1
         var1(i,j)=data(i,j)
      enddo
      enddo
      do j=1,jmtp
      do i=1,imtp
         vartp(i,j)=data(i,jm1+j-1)
      enddo
      enddo
      do i=1,jm3
      do j=1,im3/2
         var3(j,i)=vartp(i,j)
         var3(jmtp+j,i)=vartp(im-i+1,jmtp-j+1)
      enddo
      enddo
      call INTERP(im1,jm1,im2,jm2,lon1,lat1,lon2,lat2,mask1(:,:,1),mask2(:,:,1),IG1(:,:,1),JG1(:,:,1),var1,var2,ierr,NOSPVAL=.true.)
      call INTERP(im3,jm3,im4,jm4,lon3,lat3,lon4,lat4,mask3(:,:,1),mask4(:,:,1),IG3(:,:,1),JG3(:,:,1),var3,var4,ierr,NOSPVAL=.true.)
!
      do i=1,imo
         do j=1,jm2
            vart(i,j)=var2(i,j)
         enddo
         do j=jm2+1,jmo
            vart(i,j)=var4(i,j-jm2)
         enddo
      enddo
      if (.NOT. tgrid) then
         do j=1,jmo
            do i=2,imop1
               maskc(i,j)=0
               varc(i,j)=vart(i-1,j)
               if (varc(i,j).LE.spv) then
                  varc(i,j)=spv
                  maskc(i,j)=1
               endif
            enddo
            varc(1,j)=varc(imop1,j)
            maskc(1,j)=maskc(imop1,j)
         enddo
         vart=spv
         maskt=0
         call INTERP(imop1,jmo,imo,jmo,lonc,latc,lont,latt,maskc,maskt,IG(:,:,1),JG(:,:,1),varc,vart,ierr,NOSPVAL=.true.)
       endif
! QC
        do j=1,jmo
        do i=1,imo
           if (num_levels(i,j) .LT. 1) vart(i,j)=spv
        enddo
        enddo
!
! arrange data so it is for: 0 -> 360 and 90 N -> 90S
!
        do j=1,jmo
           do i=1,imos
              dato(i,j)=vart(i+imoe,jmo-j+1)
           enddo
           do i=imos+1,imo
              dato(i,j)=vart(i-imos,jmo-j+1)
           enddo
        enddo
!
      return

      end subroutine tripo_interp1tc

      subroutine tripo(idbug,im,jm,km,icefile,ocnfile,iyr,imth,iday,ihr, &
      mfh,mfhout,flonw,flone,dlon,flatn,flats,dlat,jmtp,imos, &
      imo,jmo,jmtpo,mkmoc,nreg,tripolat,outfile,mocfile,mfcstcpl,igenocnp)

      integer nvar,ko,ki,kk,ndtc,mkmoc,nreg,mfcstcpl,igenocnp
      real    hfc

      parameter(nvar=30,ko=40,ki=5)
      parameter(ndtc=7)
!
      character*120 icefile,ocnfile,outfile,mocfile
!
      integer im,jm,km,jmtp
      integer im1c,im3c,imtpc,jm1c,jm3c,jmtpc
      integer im1t,im3t,imtpt,jm1t,jm3t,jmtpt
      integer im2c,im4c,imtpoc,jm2c,jm4c,jmtpoc
      integer im2t,im4t,imtpot,jm2t,jm4t,jmtpot
      integer imo,imop1,jmo,jmtpo,imos,imoe
      integer idbug,iyr,imth,iday,ihr,mfh,mfhout
      real    dlat,dlon,flats,flatn,flonw,flone,tripolat,dtripolat
      real    factor,undef,spv_tau,val
      integer i,j,k,ierr,ilev,ind,iret,nv,ndata,nr,nundef,kreg,jtripolat
!
      real*8, dimension(im,jm)     ::  hi,hs,ts,t1,t2,fi,alb,ui,vi,sst,saltf,crot,srot
!
      real, dimension(im,jm,km)  ::  t,s,u,v,w,vv
      real, dimension(im,jm,km)  ::  dckt,dcks,vfc
      real, dimension(im,jm)     ::  eta,sfcflx,pme,mld,taux,tauy,uice,vice
      real, dimension(im,jm)     ::  clat,clon,tlat,tlon,cosrot,sinrot,varice
!
      real, dimension(imo,jmo)   ::  grid,varsfc
      real, dimension(imo,jmo,km)  :: varocn,tocn,socn

!
      real, dimension(jm,km,nreg)  :: vmoc
      real, dimension(im)  :: xv
      real, dimension(jm)  :: yv

      real zt(ko)
      real zw(ko)
      real dtc(ndtc)
!
      real,    dimension(nvar)  :: fac
      integer, dimension(nvar)  :: kpds5,kpds6,kpds7,kpds22
      integer, dimension(ko)    :: levs

      integer, parameter           :: kpds_dim=200
      integer, dimension(kpds_dim)  :: KPDS,KGDS,JPDS,JGDS
      logical*1 lbms(imo,jmo)
      logical :: climate = .false.
!
      data dtc/2.5,5.,10.,15.,20.,25.,28./
!
! 13=Potential temperature (2)
! 88=Salinity (2)
! 49=u-component of current (2)
! 50=v-component of current (2)
! 40=Geometric Vertical velocity (2)
! 124=Momentum flux, u component (2)
! 125=Momentum flux, v component (2)
! 198=Sea Surface Height Relative to Geoid (129)
! 91=Ice concentration (2)
! 92=Ice thickness (2)
! 66=Snow depth (2)
! 11=Temperature (2)
! 95=u-component of ice drift (2)
! 96=v-component of ice drift (2)
! 188=Evaporation - Precipitation (2)
! 202=Total downward heat flux at surface (downward is positive) (129)
! 195=Geometric Depth Below Sea Surface (129)
! 197=Ocean Heat Content (129)
! 194=Tropical Cyclone Heat Potential (129)
! 195=Ocean Vertical Heat Diffusivity (128)
! 196=Ocean Vertical Salt Diffusivity (128)
! 197=Ocean Vertical Momentum Diffusivity (128)
!

      data kpds5/   13, 88, 49, 50, 40,124,125,198, 91, 92, &
                    66, 11, 95, 96,188,202,195,195,197,194, &
                   195,195,195,195,195,195,195,195,196,197/
      data kpds6/  160,160,160,160,160,  1,  1,  1,  1,  1, &
                     1,  1,  1,  1,  1,  1,237,238,236,239, &
                   235,235,235,235,235,235,235,160,160,160/
      data kpds7/    0,  0,  0,  0,  0,  0,  0,  0,  0,  0, &
                     0,  0,  0,  0,  0,  0,  0,  0, 30,260, &
                    25, 50,100,150,200,250,280,  0,  0,  0/
      data fac/ 273.15,0.001,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0, &
                1.0,273.15,1.0,1.0,-8.64e6,1.0,1.0,1.0,1.0,1.0, &
                1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0/
      data kpds22/   2,  5,  3,  3,  9,  3,  3,  3,  3,  2, &
                     3,  2,  3,  3,  3,  3,  0,  0, -5, -4, &
                     0,  0,  0,  0,  0,  0,  0,  5,  5,  5/
!
      data levs/4478,3972,3483,3016,2579,2174,1807,1479,1193,949, &
                 747, 584, 459, 366, 303, 262, 238, 225, 215,205, &
                 195, 185, 175, 165, 155, 145, 135, 125, 115,105, &
                 95,  85,  75,  65,  55,  45,  35,  25,  15,  5/
!
      data spv_tau/-1.0E+5/
      data undef/-1.0E+34/
!
      data zt/      5.,  15.,  25.,  35.,  45.,  55.,  65.,  75., &
                   85.,  95., 105., 115., 125., 135., 145., 155., &
                  165., 175., 185., 195., 205., 215., 225., 238., &
                  262., 303., 366., 459., 584., 747., 949.,1193., &
                 1479.,1807.,2174.,2579.,3016.,3483.,3972.,4478./
!
      data zw/     10.,  20.,  30.,  40.,  50.,  60.,  70.,  80., &
                   90., 100., 110., 120., 130., 140., 150., 160., &
                  170., 180., 190., 200., 210., 220., 232., 250., &
                  283., 335., 413., 522., 666., 848.,1072.,1337., &
                 1643.,1991.,2377.,2798.,3250.,3728.,4225.,4737./
!
           im1c=im
           jm1c=jm-jmtp+1
           imtpc=im
           jmtpc=jmtp
           im3c=jmtpc*2
           jm3c=imtpc/2
!
           im1t=im
           jm1t=jm-jmtp+1
           imtpt=im
           jmtpt=jmtp
           im3t=jmtpt*2
           jm3t=imtpt/2
!
           imoe=imo-imos
           imop1=imo+1
           imtpoc=imo
           jmtpoc=jmtpo
           imtpot=imo
           jmtpot=jmtpo
!
           im2c=imo
           jm2c=jmo-jmtpoc
           im4c=im2c
           jm4c=jmtpoc
!
           im2t=imo
           jm2t=jmo-jmtpot
           im4t=im2t
           jm4t=jmtpot
!

           if (mfcstcpl.eq.1) climate = .true.
!
           print *,'im = ',im
           print *,'jm = ',jm
           print *,'km = ',km
           print *,'jmtp = ',jmtp

           print *,'im1c = ',im1c
           print *,'jm1c = ',jm1c
           print *,'im3c = ',im3c
           print *,'jm3c = ',jm3c
           print *,'imtpc = ',imtpc
           print *,'jmtpc = ',jmtpc

           print *,'im1t = ',im1t
           print *,'jm1t = ',jm1t
           print *,'im3t = ',im3t
           print *,'jm3t = ',jm3t
           print *,'imtpt = ',imtpt
           print *,'jmtpt = ',jmtpt

           print *,'im2c = ',im2c
           print *,'jm2c = ',jm2c
           print *,'im4c = ',im4c
           print *,'jm4c = ',jm4c
           print *,'jmtpoc = ',jmtpoc

           print *,'im2t = ',im2t
           print *,'jm2t = ',jm2t
           print *,'im4t = ',im4t
           print *,'jm4t = ',jm4t
           print *,'jmtpot = ',jmtpot

           print *,'imo = ',imo
           print *,'jmo = ',jmo
           print *,'imoe = ',imoe

           print *,'IGEN_OCNP = ',igenocnp
           print *,'mfcstcpl = ',mfcstcpl
           print *,'climate = ',climate

        call read_ocn_tp(im,jm,km,clon,clat,tlon,tlat,t,s,u,v,w, &
                eta,sfcflx,pme,mld,taux,tauy,dckt,dcks,vfc,ocnfile)
        print *,'after call read_ocn_tp'
        call read_ice_tp(im,jm,ki,srot,crot,hi,hs,ts,t1,t2,fi,alb,ui,vi,sst,saltf,icefile)
        print *,'after call read_ice_tp'
        cosrot(:,:)=crot(:,:)
        sinrot(:,:)=srot(:,:)
        uice(:,:)=ui(:,:)
        vice(:,:)=vi(:,:)

       do k=1,km
          call rotate_data(im,jm,u(:,:,k),v(:,:,k),cosrot,sinrot,undef)
       enddo
       call rotate_data(im,jm,taux,tauy,cosrot,sinrot,undef)
       call rotate_data(im,jm,uice,vice,cosrot,sinrot,undef)

       if (mkmoc .EQ. 1) then
          xv(:)=clon(:,1)
          yv(:)=clat(1,:)
          jtripolat=jm
          do j=1,jm
             if (yv(j) .GT. tripolat) then
                jtripolat=j
                go to 191
             endif
          enddo
 191      continue
          dtripolat=yv(jtripolat-1)-yv(jtripolat-2)
          print *,'jtripolat,dtripolat: ',jtripolat,dtripolat
          vv(:,:,:)=v(:,:,:)
          do j=jtripolat,jm
             yv(j)=yv(j-1)+dtripolat
             if (yv(j) .GT. 90.) then
                print *,'WARNING: check yv(j) @', j,yv(j)
                stop
             endif
             vv(:,j,:)=-10.
          enddo
          call amoc(vv,xv,yv,zt,im,jm,km,-10.,tripolat,nreg,vmoc)
          open (71,file=mocfile,form='unformatted')

          write (71) iyr,imth,iday,ihr,mfh-mfhout,mfh
          do kreg=1,nreg
          do k=1,km
             write (71) vmoc(:,k,kreg)
          enddo
          enddo
          close (71)
       endif

       call baopenwt(51,outfile,ierr)
       if(ierr.ne.0) then
       print *,'error opening file ',outfile
       print *,' exit 5'
       call errexit(5)
       endif
!
       kpds(1)=7
       if (igenocnp.GT.0) then
          kpds(2)=igenocnp
       else
          kpds(2)=98
       endif
       kpds(3)=10
       kpds(4)=192
       kpds(8)=mod(iyr-1,100)+1
       kpds(9)=imth
       kpds(10)=iday
       kpds(11)=ihr
       kpds(12)=0
       if (mfhout == 1) then
          kpds(13)=1
       else if (mfhout == 3) then
          kpds(13)=10
       else if (mfhout == 6) then
          kpds(13)=11
       else if (mfhout == 12) then
          kpds(13)=12
       else if (mfhout == 24) then
          kpds(13)=2
       else
         print *,'invalid mhout, must be one of (1 3 6 12 24).'
         stop
       endif
       if (climate) then
         kpds(13)=1
         kpds(14)=mfh
         kpds(15)=0
         kpds(16)=10
       else
         if (mfh > 1530) then
           kpds(13)=1
           kpds(14)=mfh
           kpds(15)=0
           kpds(16)=10
         else
           kpds(14)=mfh/mfhout-1
           kpds(15)=kpds(14)+1
           kpds(16)=3
         endif
       endif
       kpds(17)=0
       kpds(18)=1
       kpds(19)=2
       kpds(20)=0
       kpds(21)=((iyr-1)/100)+1
       kpds(23)=4
       kpds(24)=0
       kpds(25)=32

       print*,'kpds:',kpds(1:25)
!
       kgds(1)=0
       kgds(2)=imo
       kgds(3)=jmo
       kgds(4)=nint(flatn*1000.)
       kgds(5)=nint(flonw*1000.)
       kgds(6)=128
       kgds(7)=nint(flats*1000.)
       kgds(8)=nint(flone*1000.)
       kgds(9)=nint(dlon*1000.)
       kgds(10)=nint(dlat*1000.)
       kgds(11)=0
       kgds(12)=0
       kgds(13)=0
       kgds(14)=0
       kgds(15)=0
       kgds(16)=0
       kgds(17)=0
       kgds(18)=0
       kgds(19)=0
       kgds(20)=255
       kgds(21)=0
       kgds(22)=0
!
       ndata=imo*jmo
!
       ind=0
       do nv=1,5
       factor=fac(nv)
       kpds(22)=kpds22(nv)
       print *,nv,' factor ',factor,' kpds22 ',kpds(22)

!t
      if (nv.eq.1) then
       call tripo_interp40tc(im,jm,km,im1t,jm1t,im2t,jm2t,im3t,jm3t,im4t,jm4t, &
                            imtpt,jmtpt,imo,jmo,imos,imoe,imop1,t,varocn,.true.)
       do k=1,km
       do j=1,jmo
       do i=1,imo
          if (varocn(i,j,k).LE.undef) then
             tocn(i,j,km-k+1)=undef
          else
             tocn(i,j,km-k+1)=varocn(i,j,k)+273.15
          endif
       enddo
       enddo
       enddo
!      print *,'tocn(360:365,180:185,1)',tocn(360:365,180:185,1)
      endif

!s
      if (nv.eq.2) then
       call tripo_interp40tc(im,jm,km,im1t,jm1t,im2t,jm2t,im3t,jm3t,im4t,jm4t, &
                            imtpt,jmtpt,imo,jmo,imos,imoe,imop1,s,varocn,.true.)
       do k=1,km
       do j=1,jmo
       do i=1,imo
          socn(i,j,k)=varocn(i,j,km-k+1)
       enddo
       enddo
       enddo
!      print *,'socn(360:365,180:185,1)',socn(360:365,180:185,1)
      endif
!u
      if (nv.eq.3) &
       call tripo_interp40tc(im,jm,km,im1c,jm1c,im2c,jm2c,im3c,jm3c,im4c,jm4c, &
                           imtpc,jmtpc,imo,jmo,imos,imoe,imop1,u,varocn,.false.)
!v
      if (nv.eq.4) &
       call tripo_interp40tc(im,jm,km,im1c,jm1c,im2c,jm2c,im3c,jm3c,im4c,jm4c, &
                           imtpc,jmtpc,imo,jmo,imos,imoe,imop1,v,varocn,.false.)
!w
      if (nv.eq.5) then
       call tripo_interp40tc(im,jm,km,im1t,jm1t,im2t,jm2t,im3t,jm3t,im4t,jm4t, &
                            imtpt,jmtpt,imo,jmo,imos,imoe,imop1,w,varocn,.true.)
       do k=1,km-1
          kk=km-k+1
          do j=1,jmo
          do i=1,imo
            if (varocn(i,j,k).LE.undef) then
               varocn(i,j,k)=varocn(i,j,k+1)
            else
               varocn(i,j,k)=(varocn(i,j,k+1)*(zw(kk)-zt(kk))  &
                      +varocn(i,j,k)*(zt(kk)-zw(kk-1)))/(zw(kk)-zw(kk-1))
            endif
          enddo
          enddo
       enddo
      endif

       do k=1,km
       ind=ind+1
       ilev=levs(k)

!   make bit-map....
       do j=1,jmo
       do i=1,imo
          val=varocn(i,j,k)
          if (val.eq.undef) then
             lbms(i,j) = .false.
          else
             if (nv.eq.1) then
                grid(i,j)=val+factor
             else
                grid(i,j)=val*factor
             endif
             lbms(i,j) = .true.
          endif
       enddo
       enddo
       print *,' record written ',ind,nv,k,grid(92,125),lbms(92,125)
       kpds(5)=kpds5(nv)
       kpds(6)=kpds6(nv)
       kpds(7)=ilev
       CALL PUTGB(51,NDATA,KPDS,KGDS,LBMS,GRID,IRET)
       if (iret.ne.0) then
          print *,' error in PUTGB for iret ',iret,(kpds(nr),nr=5,7)
       print *,' exit 6'
       call errexit(6)
       endif
!
!.. end level-loop
      enddo
!.. end variable-loop
      enddo

!... now read and grib 5 surface records...
!
       do nv=6,27
       factor=fac(nv)
       kpds(22)=kpds22(nv)
       print *,nv,' factor ',factor,' kpds22 ',kpds(22)
       ind=ind+1
       kpds(5)=kpds5(nv)
       kpds(6)=kpds6(nv)
       kpds(7)=kpds7(nv)
       if (nv .EQ. 8 .or. (nv.GE.16 .AND. nv.LE.27)) then
          kpds(19)=129
       else if (nv .EQ. 15) then
          kpds(19)=128
       else
          kpds(19)=2
       endif
! taux
      if (nv.eq.6) then
       call tripo_interp1tc(im,jm,km,im1c,jm1c,im2c,jm2c,im3c,jm3c,im4c,jm4c, &
                       imtpc,jmtpc,imo,jmo,imos,imoe,imop1,taux,varsfc,.false.)
       do j=1,jmo
       do i=1,imo
          if (varsfc(i,j) .LE. spv_tau) varsfc(i,j)=undef
       enddo
       enddo
      endif
! tauy
      if (nv.eq.7) then
       call tripo_interp1tc(im,jm,km,im1c,jm1c,im2c,jm2c,im3c,jm3c,im4c,jm4c, &
                       imtpc,jmtpc,imo,jmo,imos,imoe,imop1,tauy,varsfc,.false.)
       do j=1,jmo
       do i=1,imo
          if (varsfc(i,j) .LE. spv_tau) varsfc(i,j)=undef
       enddo
       enddo
      endif
! eta
      if (nv.eq.8) then
       call tripo_interp1tc(im,jm,km,im1t,jm1t,im2t,jm2t,im3t,jm3t,im4t,jm4t, &
                         imtpt,jmtpt,imo,jmo,imos,imoe,imop1,eta,varsfc,.true.)
       endif
! fi
      if (nv.eq.9) then
       varice=fi
       call tripo_interp1tc(im,jm,km,im1t,jm1t,im2t,jm2t,im3t,jm3t,im4t,jm4t, &
                      imtpt,jmtpt,imo,jmo,imos,imoe,imop1,varice,varsfc,.true.)
       endif
! hi
      if (nv.eq.10) then
       varice=hi
       call tripo_interp1tc(im,jm,km,im1t,jm1t,im2t,jm2t,im3t,jm3t,im4t,jm4t, &
                      imtpt,jmtpt,imo,jmo,imos,imoe,imop1,varice,varsfc,.true.)
       endif
! hs
      if (nv.eq.11) then
       varice=hs
       call tripo_interp1tc(im,jm,km,im1t,jm1t,im2t,jm2t,im3t,jm3t,im4t,jm4t, &
                      imtpt,jmtpt,imo,jmo,imos,imoe,imop1,varice,varsfc,.true.)
       endif
! ts
      if (nv.eq.12) then
       varice=ts
       call tripo_interp1tc(im,jm,km,im1t,jm1t,im2t,jm2t,im3t,jm3t,im4t,jm4t, &
                      imtpt,jmtpt,imo,jmo,imos,imoe,imop1,varice,varsfc,.true.)
       endif
! uice
      if (nv.eq.13) then
       varice=uice
       call tripo_interp1tc(im,jm,km,im1c,jm1c,im2c,jm2c,im3c,jm3c,im4c,jm4c, &
                     imtpc,jmtpc,imo,jmo,imos,imoe,imop1,varice,varsfc,.false.)
       endif
! vice
      if (nv.eq.14) then
       varice=vice
       call tripo_interp1tc(im,jm,km,im1c,jm1c,im2c,jm2c,im3c,jm3c,im4c,jm4c, &
                     imtpc,jmtpc,imo,jmo,imos,imoe,imop1,varice,varsfc,.false.)
       endif
! pme
      if (nv.eq.15) then
       call tripo_interp1tc(im,jm,km,im1t,jm1t,im2t,jm2t,im3t,jm3t,im4t,jm4t, &
                      imtpt,jmtpt,imo,jmo,imos,imoe,imop1,pme,varsfc,.true.)
       endif
! sfc_flx
      if (nv.eq.16) then
       call tripo_interp1tc(im,jm,km,im1t,jm1t,im2t,jm2t,im3t,jm3t,im4t,jm4t, &
                      imtpt,jmtpt,imo,jmo,imos,imoe,imop1,sfcflx,varsfc,.true.)
       endif
! mld
      if (nv.eq.17) then
       call mixed_layer(imo,jmo,km,tocn,socn,zt,varsfc,undef)
      endif
! sfc isothm layer depth
      if (nv.eq.18) then
       call sfc_isothm_layer(imo,jmo,km,tocn,zt,varsfc,undef)
      endif
! ocean heat content
      if (nv.eq.19) then
       call ocean_heat(imo,jmo,km,tocn,socn,zw,zt,varsfc,undef)
      endif
! tropical cyc heat potential
      if (nv.eq.20) then
       call tchp26(imo,jmo,km,tocn,socn,zw,zt,varsfc,undef)
      endif
! depth of 7 different isotherms...
      if (nv.ge.21 .AND. nv.le.27) then
       i=nv-20
       call isothm_layer(imo,jmo,km,dtc(i),tocn,zt,varsfc,undef)
      endif

       nundef=0
       do j=1,jmo
       do i=1,imo
          val=varsfc(i,j)
          if (val.eq.undef) then
             lbms(i,j) = .false.
             nundef=nundef+1
          else
             if (nv.EQ.12) then
                grid(i,j)=val+factor
             else
                grid(i,j)=val*factor
             endif
             lbms(i,j) = .true.
          endif
       enddo
       enddo
       if(nundef.eq.imo*jmo) then
        print *,' record not written because of all undef ', &
        ind,kpds(5),kpds(6),kpds(7)
       else
        print *,' record written ',ind,kpds(5),kpds(6),kpds(7), &
        grid(92,125),lbms(92,125),factor
        CALL PUTGB(51,NDATA,KPDS,KGDS,LBMS,GRID,IRET)
        if(iret.ne.0) then
         print *,' error in PUTGB for iret ',iret,(kpds(k),k=5,7)
         print *,' exit 7'
         call errexit(7)
        endif
       endif

       enddo

      if (mkmoc .EQ. 1) then
       do nv=28,nvar
       factor=fac(nv)
       kpds(22)=kpds22(nv)
       print *,nv,' factor ',factor,' kpds22 ',kpds(22)

!diff_cbt_kpp_t
      if (nv.eq.28) &
       call tripo_interp40tc(im,jm,km,im1t,jm1t,im2t,jm2t,im3t,jm3t,im4t,jm4t, &
                         imtpt,jmtpt,imo,jmo,imos,imoe,imop1,dckt,varocn,.true.)
!diff_cbt_kpp_s
      if (nv.eq.29) &
       call tripo_interp40tc(im,jm,km,im1t,jm1t,im2t,jm2t,im3t,jm3t,im4t,jm4t, &
                         imtpt,jmtpt,imo,jmo,imos,imoe,imop1,dcks,varocn,.true.)
!vert_fric_coeff
      if (nv.eq.30) &
       call tripo_interp40tc(im,jm,km,im1c,jm1c,im2c,jm2c,im3c,jm3c,im4c,jm4c, &
                         imtpc,jmtpc,imo,jmo,imos,imoe,imop1,vfc,varocn,.false.)
       do k=1,km
       ind=ind+1
       ilev=levs(k)

!   make bit-map....
       do j=1,jmo
       do i=1,imo
          val=varocn(i,j,k)
          if (val.eq.undef) then
             lbms(i,j) = .false.
          else
             if (nv.eq.1) then
                grid(i,j)=val+factor
             else
                grid(i,j)=val*factor
             endif
             lbms(i,j) = .true.
          endif
       enddo
       enddo
       print *,' record written ',ind,nv,k,grid(92,125),lbms(92,125)
       kpds(5)=kpds5(nv)
       kpds(6)=kpds6(nv)
       kpds(7)=ilev
       kpds(19)=128
       CALL PUTGB(51,NDATA,KPDS,KGDS,LBMS,GRID,IRET)
       if (iret.ne.0) then
          print *,' error in PUTGB for iret ',iret,(kpds(nr),nr=5,7)
       print *,' exit 6'
       call errexit(6)
       endif
!
!.. end level-loop
      enddo
!.. end variable-loop
      enddo
     endif ! (mkmoc .EQ. 1)

      return
      end subroutine tripo

      subroutine rotate_data(im,jm,udata,vdata,rotcos,rotsin,undef)

      integer im,jm,i,j
      real, dimension(im,jm) :: udata,vdata,rotcos,rotsin
      real  :: u,v,undef

      do j=1,jm
      do i=1,im
         if (udata(i,j) /= undef) then
            u=udata(i,j)*rotcos(i,j)-vdata(i,j)*rotsin(i,j)
            v=udata(i,j)*rotsin(i,j)+vdata(i,j)*rotcos(i,j)
            udata(i,j)=u
            vdata(i,j)=v
         endif
      enddo
      enddo

      return

      end subroutine rotate_data

      subroutine isothm_layer(im,jm,km,dtc,temp,zlev,zisothm,undef)

      real, parameter :: c2k=273.15
      integer inumc,im,jm,km
      integer i,j,k
      real  dtc
      real, dimension(km) :: tz,zlev
      real, dimension(im,jm) :: zisothm
      real, dimension(im,jm,km) :: temp
      real  a,b,tc,undef

      tc=c2k+dtc

      do j=1,jm
      do i=1,im

         zisothm(i,j)=undef
         if (temp(i,j,1) .GE. tc) then
            do k=1,km
               tz(k)=temp(i,j,k)
            enddo
            do k=2,km
               if (tz(k) .LT. -3.0) go to 111
               if (tz(k) .LT. tc) then
                  a = (tz(k)-tc) / (tz(k)-tz(k-1))
                  b = (tc-tz(k-1)) / (tz(k)-tz(k-1))
                  zisothm(i,j)=a*zlev(k-1)+b*zlev(k)
                  go to 111
               endif
            enddo
         endif
 111     continue
      enddo
      enddo

      return
      end subroutine isothm_layer

      subroutine mixed_layer(im,jm,km,temp,salt,zlev,mld,undef)

      real, parameter :: disot=0.8, c2k=273.15
      integer im,jm,km
      integer i,j,k,kmsk,kbm,kbp,krf
      real, dimension(km) :: zlev,plev,sa,ta,th,rho
      real, dimension(im,jm) :: mld
      real, dimension(im,jm,km) :: temp,salt
      real  a,b,deltarho,dr,rb,undef

      do k=1,km
         plev(k) = press(zlev(k),980.0)
      enddo

      do j=1,jm
      do i=1,im
         kmsk = 0
         do k=1,km
            if (temp(i,j,k).GT.0.0 .AND. salt(i,j,k).GT.0.0) then
               ta(k) = temp(i,j,k)-c2k
               sa(k) = salt(i,j,k)
               kmsk = k
            endif
         enddo

         if (kmsk.EQ.0 .OR. ta(1).LT.-3.0) then
            mld(i,j)=undef
         else
            deltarho = (density(0.0,ta(1)-disot,sa(1)) &
                      - density(0.0,ta(1),sa(1)))
            do k=1,kmsk
               th(k) = theta(plev(k),ta(k),sa(k),0.0)
               rho(k) = density(0.0,th(k),sa(k)) - 1000.0
            enddo
            krf = 1
            kbm = 0
            kbp = 0
            do k = krf,kmsk
               if ((rho(k)-rho(krf)) .GE. deltarho) then
                   kbp = k
                    exit
               endif
            enddo
            if (kbp .LE. 1) then
               mld(i,j) = undef
            else
               kbm = kbp - 1
               rb = rho(krf) + deltarho
               dr = rho(kbp) - rho(kbm)
               a = (rho(kbp) - rb) / dr
               b = (rb - rho(kbm)) / dr
               mld(i,j) = zlev(kbm)*a + zlev(kbp)*b
            endif
         endif
      enddo
      enddo

      end subroutine mixed_layer

      subroutine sfc_isothm_layer(im,jm,km,temp,zlev,sitd,undef)

      real, parameter :: disot=0.8
      integer im,jm,km
      integer i,j,k
      real, dimension(im,jm) :: sitd
      real, dimension(im,jm,km) :: temp
      real, dimension(km) :: zlev,tz
      real  a,b,tc,undef

      do j=1,jm
      do i=1,im

         sitd(i,j)=undef

         if (temp(i,j,1).GE.0.0) then
            tc=temp(i,j,1)-disot
            do k=1,km
               tz(k)=temp(i,j,k)
            enddo
            do k=2,km
               if (tz(k).LT.0.0) go to 112
               if (tz(k).LT.tc) then
                  a = (tz(k)-tc) / (tz(k)-tz(k-1))
                  b = (tc-tz(k-1)) / (tz(k)-tz(k-1))
                  sitd(i,j) = a*zlev(k-1) + b*zlev(k)
                  go to 112
               endif
            enddo
         endif

 112     continue

      enddo
      enddo

      return
      end subroutine sfc_isothm_layer

      subroutine ocean_heat(im,jm,km,temp,salt,zblev,zlev,ocnhc,undef)

      integer, parameter :: kmh=26
      real, parameter :: c2k=273.15
      integer im,jm,km
      integer i,j,k
      real, dimension(km) :: zblev,zlev,plev
      real, dimension(im,jm) :: ocnhc
      real, dimension(im,jm,km) :: salt,temp
      real  dptlyr,rk,sk,tk,undef
      real  pk,rhm,rhp,tempk,zk

      k=kmh
      zk=0.5*(300.0+zblev(k-1))
      pk=press(zk,980.0)
      rhm = (zk-zlev(k-1))/(zlev(k)-zlev(k-1))
      rhp = (zlev(k)-zk)/(zlev(k)-zlev(k-1))

      do k=1,km
         plev(k) = press(zlev(k),980.0)
      enddo

      do j=1,jm
      do i=1,im

         ocnhc(i,j)=undef

         if (temp(i,j,kmh).GT.0.0 .AND. salt(i,j,kmh).GT.0.0) then
            ocnhc(i,j)=0.
            do k=1,kmh-1
               tk=temp(i,j,k)-c2k
               sk=salt(i,j,k)
               rk=density(plev(k),tk,sk)
               if (k .eq. 1) then
                  dptlyr=zblev(k)
               else
                  dptlyr=zblev(k)-zblev(k-1)
               endif
               ocnhc(i,j)=ocnhc(i,j) + rk*temp(i,j,k)*dptlyr
            enddo
            k=kmh
            tempk=rhp*temp(i,j,k-1) + rhm*temp(i,j,k)
            tk=tempk - c2k
            sk=rhp*salt(i,j,k-1) + rhm*salt(i,j,k)
            rk=density(pk,tk,sk)
            dptlyr=300.0-zblev(k-1)
            ocnhc(i,j)=ocnhc(i,j)+rk*tempk*dptlyr
            ocnhc(i,j)=ocnhc(i,j)*3996.
         endif
      enddo
      enddo

      return
      end subroutine ocean_heat

      subroutine tchp26(im,jm,km,temp,salt,zblev,zlev,ocnhcp,undef)

      real, parameter :: c2k=273.15, t26=26.0
      integer im,jm,km
      integer i,j,k,k26
      real, dimension(km) :: zblev,zlev,plev,tz
      real, dimension(im,jm) :: ocnhcp,z26isothm
      real, dimension(im,jm,km) :: salt,temp
      real  dptlyr,rk,sk,tk,undef
      real  rhm,rhp,pk,zk
      real  a,b,skk,skm,tc,z26
      logical*1 lbms(im,jm)

      tc=c2k+t26

      do k=1,km
         plev(k) = press(zlev(k),980.0)
      enddo

      do j=1,jm
      do i=1,im

         z26isothm(i,j)=undef
         lbms(i,j)=.false.
         if (temp(i,j,1) .GE. tc) then
            do k=1,km
               tz(k)=temp(i,j,k)
            enddo
            k = 1
            do while (tz(k).GE.tc)
               k26 = k
               if (k.EQ.km) exit
               k = k + 1
            enddo
            k = k26
            if (tz(k) .GT. tc) then
               if (k.LT.km .AND. tz(k+1).GT.0.0) then
                  k = k + 1
                  a = (tz(k)-tc) / (tz(k)-tz(k-1))
                  b = (tc-tz(k-1)) / (tz(k)-tz(k-1))
                  z26isothm(i,j) = a*zlev(k-1) + b*zlev(k)
                  lbms(i,j)=.true.
               else if (k.GE.2 .AND. tz(k).LT.tz(k-1)) then
                  a = (tz(k)-tc) / (tz(k)-tz(k-1))
                  b = (tc-tz(k-1)) / (tz(k)-tz(k-1))
                  z26 = a*zlev(k-1) + b*zlev(k)
                  if (z26.LE.zblev(k)) then
                     z26isothm(i,j) = z26
                     lbms(i,j)=.true.
                  endif
              endif
            else if (tz(k).EQ.tc) then
               z26isothm(i,j) = zlev(k)
               lbms(i,j)=.true.
            endif
         endif

      enddo
      enddo

!
!---------- get ocean heat potential relative to 26C (TCHP) ------------
!
      do j=1,jm
      do i=1,im

         if (temp(i,j,1) .GT. 0.0) then
            ocnhcp(i,j)=0.0
         else
            ocnhcp(i,j)=undef
            cycle
         endif

         if (lbms(i,j)) then   ! we have water above 26c

            z26 = z26isothm(i,j)
!
!  case where Z26 is within the topmost layer
!
            if (z26 .LE. zblev(1)) then
               tk=temp(i,j,1)-c2k
               if (salt(i,j,1) .GT. 0.0) then
                   sk=salt(i,j,1)
               else
                   sk=35.   ! fake salinity
               endif
               rk=density(plev(1),tk,sk)
               dptlyr=z26
               ocnhcp(i,j) = rk*(tk-t26)*dptlyr*3996.
!
!  case where z26 is below the top layer and above the bottom
!
            else
               k26 = 1
               do k=2,km
                  if (z26.GT.zblev(k-1) .AND.  z26.LE.zblev(k)) k26=k
               enddo

               ocnhcp(i,j)=0.0
               do k=1,k26-1
                  tk=temp(i,j,k)-c2k
                  if (salt(i,j,K) .GT. 0.0) then
                     sk=salt(i,j,k)
                  else
                     sk=35.   ! fake salinity
                  endif
                  rk=density(plev(k),tk,sk)
                  if (k .EQ. 1) then
                     dptlyr=zblev(1)
                  else
                     dptlyr=zblev(k)-zblev(k-1)
                  endif
                  ocnhcp(i,j)=ocnhcp(i,j)+rk*(tk-26.0)*dptlyr
               enddo
               k=k26
               zk=0.5*(z26+zblev(k-1))
               pk=press(zk,980.0)
               rhm = (zk-zlev(k-1))/(zlev(k)-zlev(k-1))
               rhp = (zlev(k)-zk)/(zlev(k)-zlev(k-1))
               tk=rhp*temp(i,j,k-1) + rhm*temp(i,j,k) - c2k
               if (salt(i,j,k-1) .GT. 0.0) then
                  skm=salt(i,j,k-1)
               else
                  skm=35.   ! fake salinity
               endif
               if (salt(i,j,k) .GT. 0.0) then
                  skk=salt(i,j,k)
               else
                  skk=35.   ! fake salinity
               endif
               sk=(rhp*skm + rhm*skk)
               rk=density(pk,tk,sk)
               dptlyr=z26-zblev(k-1)
               ocnhcp(i,j)=ocnhcp(i,j)+rk*(tk-26.0)*dptlyr
               ocnhcp(i,j)=ocnhcp(i,j)*3996.
            endif
!
!  case where temperature is above 26C down to the bottom
!
         else if ((temp(i,j,1)-c2k) .GT. t26) then
            ocnhcp(i,j)=0.0
            do k=1,km
               if (temp(i,j,k) .GT. undef) then
                  tk=temp(i,j,k)-c2k
                  if (salt(i,j,k) .GT. 0.0) then
                     sk=salt(i,j,k)
                  else
                     sk=35.   ! fake salinity
                  endif
                  rk=density(plev(k),tk,sk)
                  if (k .EQ. 1) then
                     dptlyr=zblev(1)
                  else
                     dptlyr=zblev(k)-zblev(k-1)
                  endif
                  ocnhcp(i,j)=ocnhcp(i,j)+rk*(tk-26.0)*dptlyr
               endif
            enddo
            ocnhcp(i,j)=ocnhcp(i,j)*3996.
         endif

      enddo
      enddo

      return
      end subroutine tchp26
 
      function press(z, g)

!   copy from cfs_ocean_time.f and modified
!   depth (z) in meters and grav acc'l (g) in cm/sec**2

      integer, parameter :: itr=20
      integer i
      real p, a0, z, g, press
      real(kind=8) :: e, ae, es
!
      p = z*(1.0076+z*(2.3487e-6-z*1.2887e-11))
      e = zeta(p,g)-z
      ae = abs(e)
      es = ae*2.
      do i = 1,itr
        a0 = 0.972643+p*(1.32696e-5-p*(6.228e-12+p*1.885e-16))
        a0 = a0/(1.0+1.83e-5*p)
        p = p-((g+1.113e-4*p)/a0)*e*0.001
        es = ae
        e = zeta(p,g)-z
        ae = abs(e)
        if (ae .le. 0.01) exit
      enddo
!
      press = p
!
      end function press

      function zeta(p, glat)
!
!   copy from cfs_ocean_time.f and modified

      real p, glat, z, zeta

      z = ((-3.434e-12*p+1.113e-7)*p+0.712953)*p+14190.7*log(1.0+1.83e-5*p)
      z = (z/(glat+1.113e-4*p))*1000.

      zeta = z
!
      end function zeta

      function density(prs, tmp, sal)
!
!   copy from cfs_ocean_time.f and modified
!     Density is in units of kg/m**3  (1 g/cm**3 = 1000 kg/m**3)

      real density, prs, tmp, sal
      real p, t, s, kstp, k0, kw, d0, dw
!
      s = sal
      t = tmp
      p = prs/10.00
!
      kw = 19652.21+(148.4206-(2.327105-(1.360477e-2-5.155288e-5*t)*t)*t)*t
!
      k0 = kw+s*(54.6746-(0.603459-(1.09987e-2-6.1670e-5*t)*t)*t)   &
             +sqrt(s*s*s)*(7.944e-2+(1.6483e-2-5.3009e-4*t)*t)
!
      kstp = k0+p*((3.239908+(1.43713e-3+(1.16092e-4-5.77905e-7*t)*t)*t) &
             +s*(2.2838e-3-(1.0981e-5+1.6078e-6*t)*t)                    &
             +sqrt(s*s*s)*1.91075e-4                                     &
             +p*((8.50935e-5-(6.12293e-6-5.2787e-8*t)*t)                 &
             -s*(9.9348e-7-(2.0816e-8+9.1697e-10*t)*t))) 
!
      dw = 999.842594+(6.793952e-2-(9.095290e-3-(1.001685e-4  &
             -(1.120083e-6-6.536332e-9*t)*t)*t)*t)*t
!
      d0 = dw+s*(0.824493-(4.0899e-3-(7.6438e-5-(8.2467e-7        &
             -5.3875e-9*t)*t)*t)*t)                               &
             -sqrt(s*s*s)*(5.72466e-3-(1.0227e-4-1.6546e-6*t)*t)  &
             +s*s*4.8314e-4
!
      density = d0/(1.0-p/kstp)

      end function density

      function theta(p, t, s, pref)

      real(kind=8), parameter :: sqrt2 = 0.7071067811865475
      real theta, p,t, s, pref
      real del_p, del_t1, del_t2, del_t3, del_t4, tp, th

      del_p = pref-p
      del_t1 = del_p*atg(p,t,s)
      tp = t+0.5*del_t1
      del_t2 = del_p*atg((p+0.5*del_p),tp,s)
      tp = t+(-0.5+sqrt2)*del_t1+(1.0-sqrt2)*del_t2
      del_t3 = del_p*atg((p+0.5*del_p),tp,s)
      tp = t-sqrt2*del_t2+(1.0+sqrt2)*del_t3
      del_t4 = del_p*atg(pref,tp,s)
      th = (t+(del_t1+(1.0-sqrt2)*del_t2*2.0 &
         + (1.0+sqrt2)*del_t3*2.0+del_t4)/6.0)
      theta = th

      end function theta

      function atg(p, t, s)

      real atg, p, t, s, ds, a

      ds = s-35.0
      a = (((-2.1687e-16*t+1.8676e-14)*t-4.6206e-13)*p           &
             +((2.7759e-12*t-1.1351e-10)*ds+((-5.4481e-14*t      & 
             +8.733e-12)*t-6.7795e-10)*t+1.8741e-8))*p           &
             +(-4.2393e-8*t+1.8932e-6)*ds                        &
             +((6.6228e-10*t-6.836e-8)*t+8.5258e-6)*t+3.5803e-5

      atg = a

      end function atg


      end module tripo_intp_mod
