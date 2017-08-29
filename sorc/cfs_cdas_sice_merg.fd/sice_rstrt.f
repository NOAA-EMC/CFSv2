      program sice_rstrt

!    2007-10-09  XINGREN WU
!
!    2007-11-01  XINGREN WU
!         Modified: use allocatable variables
!    2007-11-05  XINGREN WU
!         Modified: adjust ocean temp due to the chage of sea-ice
!    2008-08-15  XINGREN WU
!         Modified: add snow to merge gdas snow
!    2008-08-25  XINGREN WU
!         Modified: add sst to cross check sst and sea-ice, remove sea-ice if sst is warm

      use sice_rstrt_mode
      use ocn_rstrt_mode

      implicit none

      integer iyr,imth,iday
      integer i,j,k,k1,kr,n,ios
      real*8  Time,ficenew
      real*8, PARAMETER :: fic=0.15,h1=1.0,h0=0.0,p2=0.2,p5=0.5, &
        hmax=5.0,roughi=0.0005,tf=-1.8
      real (kind=8), allocatable :: part_size(:,:,:),albedo(:,:,:), &
        rough_mom(:,:,:),rough_heat(:,:,:),rough_moist(:,:,:),t_surf(:,:,:)
      real(kind=8), allocatable :: h_snow(:,:,:),h_ice(:,:,:), &
        t_ice1(:,:,:),t_ice2(:,:,:)
      real(kind=8), allocatable :: u_ice(:,:,:),v_ice(:,:,:),sig11(:,:,:), &
        sig22(:,:,:),sig12(:,:,:),flux_u(:,:,:),flux_v(:,:,:),flux_t(:,:,:), &
        flux_q(:,:,:),flux_salt(:,:,:),flux_sw(:,:,:),flux_lw(:,:,:), &
        lprec(:,:,:),fprec(:,:,:),runoff(:,:,:),calving(:,:,:),p_surf(:,:,:)
      real(kind=8), allocatable :: fice(:,:),icecsfc(:,:),icetksfc(:,:), &
        sst(:,:),hsno(:,:),omsk(:,:),ylat(:,:)
      real(kind=8), allocatable :: temp(:,:,:,:),salt(:,:,:,:)
      real, allocatable :: xaxis_1(:), yaxis_1(:),zaxis_1(:), zaxis_2(:), &
        zaxis_3(:)
      real(kind=8), dimension(2) :: albedo_ave,rough_ave,t_surf_ave, &
        t_ice1_ave,t_ice2_ave,u_ice_ave,v_ice_ave,sig11_ave,sig22_ave, &
        sig12_ave,flux_u_ave,flux_v_ave,flux_t_ave,flux_q_ave,flux_salt_ave, &
        flux_sw_ave,flux_lw_ave,p_surf_ave,h_snow_ave
      character(len=120) :: fice_file,hice_file,sst_file,hsno_file,ofile_in,&
        ofile_out,stemp,ssalt,title
      INTEGER, dimension(2) :: nsice
      INTEGER :: im=360,jm=231,ko=40,ki1=6,ki2=5,ki3=1,lm=1
      NAMELIST /GRID_SETTINGS/im,jm,ko,ki1,ki2,ki3,lm

      open(88,file='ice_init_grid_nml',status='old',iostat=ios)
      if (ios.NE.0) then
        print*,'Error opening file ice_init_grid_nml.'
      endif
      read(88,NML=GRID_SETTINGS,iostat=ios)
      write(6,NML=GRID_SETTINGS)

      allocate(part_size(im,jm,ki1),albedo(im,jm,ki1),rough_mom(im,jm,ki1), &
        rough_heat(im,jm,ki1),rough_moist(im,jm,ki1),t_surf(im,jm,ki1))
      allocate(h_snow(im,jm,ki2),h_ice(im,jm,ki2),t_ice1(im,jm,ki2), &
        t_ice2(im,jm,ki2))
      allocate(u_ice(im,jm,ki3),v_ice(im,jm,ki3),sig11(im,jm,ki3), &
        sig22(im,jm,ki3),sig12(im,jm,ki3),flux_u(im,jm,ki3),flux_v(im,jm,ki3), &
        flux_t(im,jm,ki3),flux_q(im,jm,ki3),flux_salt(im,jm,ki3), &
        flux_sw(im,jm,ki3),flux_lw(im,jm,ki3),lprec(im,jm,ki3), &
        fprec(im,jm,ki3),runoff(im,jm,ki3),calving(im,jm,ki3),p_surf(im,jm,ki3))
      allocate(fice(im,jm),icecsfc(im,jm),icetksfc(im,jm),sst(im,jm), &
        hsno(im,jm),omsk(im,jm),ylat(im,jm))
      allocate(temp(im,jm,ko,lm),salt(im,jm,ko,lm))
      allocate(xaxis_1(im),yaxis_1(jm),zaxis_1(ki1),zaxis_2(ki2),zaxis_3(ki3))

      call ice_rstrt_init

      write (6,*) 'year, month, day (e.g. 1999, 9, 19):'
      read (5,*) iyr,imth,iday

      call read_ice_rstrt(im,jm,ki1,ki2,ki3,                      &
        xaxis_1,yaxis_1,zaxis_1,zaxis_2,zaxis_3,Time,             &
        part_size,albedo,rough_mom,rough_heat,rough_moist,t_surf, &
        h_snow,h_ice,t_ice1,t_ice2,                               &
        u_ice,v_ice,sig11,sig22,sig12,flux_u,flux_v,flux_t,flux_q,&
        flux_salt,flux_sw,flux_lw,lprec,fprec,runoff,calving,p_surf)

      call read_gridmask(im,jm,omsk)
      fice_file='regrid_fice.nc'
      call read_fice(im,jm,icecsfc,icetksfc,ylat,fice_file)
      if (hice_clm) then
         hice_file='hice_clm.nc'
         call read_hice(im,jm,icetksfc,hice_file,iyr,imth,iday)
      endif
      if (sst_anl) then
         sst_file='regrid_sst.nc'
         call read_sst(im,jm,icecsfc,sst,sst_file)
      endif
      if (hsno_gdas) then
         hsno_file='regrid_hsno.nc'
         call read_hsno(im,jm,icecsfc,hsno,hsno_file)
      endif

      ofile_in ='ocean_temp_salt.res.nc'
      title   ='RESTART/ocean_temp_salt.res.nc'
      stemp  ='temp'
      ssalt  ='salt'
      call read_ocnvar2(im,jm,ko,ko,lm,temp,salt,stemp,ssalt,ofile_in,2)

      fice(:,:)=1-part_size(:,:,1)
      nsice(:)=0
      albedo_ave(:)=0.
      h_snow_ave(:)=0.
      t_surf_ave(:)=0.
      t_ice1_ave(:)=0.
      t_ice2_ave(:)=0.
      u_ice_ave(:)=0.
      v_ice_ave(:)=0.
      sig11_ave(:)=0.
      sig22_ave(:)=0.
      sig12_ave(:)=0.
      flux_u_ave(:)=0.
      flux_v_ave(:)=0.
      flux_t_ave(:)=0.
      flux_q_ave(:)=0.
      flux_salt_ave(:)=0.
      flux_sw_ave(:)=0.
      flux_lw_ave(:)=0.
      p_surf_ave(:)=0.

      do j=1,jm
      do i=1,im
        if ((omsk(i,j).GT.p5).AND.(fice(i,j).GE.fic)) then
           if (ylat(i,j) .LT. 0.) then
              n=1
           else
              n=2
           endif
           k1=1
           do k=1,ki2
              if (part_size(i,j,k+1).GT.0.01) then
                 kr=k
                 go to 100
              endif
           enddo
           kr=k1
100        continue
           nsice(n)=nsice(n)+1
           albedo_ave(n)=albedo_ave(n)+albedo(i,j,kr+1)
           t_surf_ave(n)=t_surf_ave(n)+t_surf(i,j,kr+1)
           h_snow_ave(n)=h_snow_ave(n)+h_snow(i,j,kr)
           t_ice1_ave(n)=t_ice1_ave(n)+t_ice1(i,j,kr)
           t_ice2_ave(n)=t_ice2_ave(n)+t_ice2(i,j,kr)
           u_ice_ave(n)=u_ice_ave(n)+u_ice(i,j,k1)
           v_ice_ave(n)=v_ice_ave(n)+v_ice(i,j,k1)
           sig11_ave(n)=sig11_ave(n)+sig11(i,j,k1)
           sig22_ave(n)=sig22_ave(n)+sig22(i,j,k1)
           sig12_ave(n)=sig12_ave(n)+sig12(i,j,k1)
           flux_u_ave(n)=flux_u_ave(n)+flux_u(i,j,k1)
           flux_v_ave(n)=flux_v_ave(n)+flux_v(i,j,k1)
           flux_t_ave(n)=flux_t_ave(n)+flux_t(i,j,k1)
           flux_q_ave(n)=flux_q_ave(n)+flux_q(i,j,k1)
           flux_salt_ave(n)=flux_salt_ave(n)+flux_salt(i,j,k1)
           flux_sw_ave(n)=flux_sw_ave(n)+flux_sw(i,j,k1)
           flux_lw_ave(n)=flux_lw_ave(n)+flux_lw(i,j,k1)
           p_surf_ave(n)=p_surf_ave(n)+p_surf(i,j,k1)
        endif
      enddo
      enddo

      do n=1,2
         if (nsice(n) .GT. 0) then
           albedo_ave(n)=albedo_ave(n)/nsice(n)
           h_snow_ave(n)=h_snow_ave(n)/nsice(n)
           t_surf_ave(n)=t_surf_ave(n)/nsice(n)
           t_ice1_ave(n)=t_ice1_ave(n)/nsice(n)
           t_ice2_ave(n)=t_ice2_ave(n)/nsice(n)
           u_ice_ave(n)=u_ice_ave(n)/nsice(n)
           v_ice_ave(n)=v_ice_ave(n)/nsice(n)
           sig11_ave(n)=sig11_ave(n)/nsice(n)
           sig22_ave(n)=sig22_ave(n)/nsice(n)
           sig12_ave(n)=sig12_ave(n)/nsice(n)
           flux_u_ave(n)=flux_u_ave(n)/nsice(n)
           flux_v_ave(n)=flux_v_ave(n)/nsice(n)
           flux_t_ave(n)=flux_t_ave(n)/nsice(n)
           flux_q_ave(n)=flux_q_ave(n)/nsice(n)
           flux_salt_ave(n)=flux_salt_ave(n)/nsice(n)
           flux_sw_ave(n)=flux_sw_ave(n)/nsice(n)
           flux_lw_ave(n)=flux_lw_ave(n)/nsice(n)
           p_surf_ave(n)=p_surf_ave(n)/nsice(n)
         else
           albedo_ave(n)=albedo_fix
           h_snow_ave(n)=h_snow_fix
           t_surf_ave(n)=t_surf_fix
           t_ice1_ave(n)=t_ice1_fix
           t_ice2_ave(n)=t_ice2_fix
           u_ice_ave(n)=u_ice_fix
           v_ice_ave(n)=v_ice_fix
           sig11_ave(n)=sig11_fix
           sig22_ave(n)=sig22_fix
           sig12_ave(n)=sig12_fix
           flux_u_ave(n)=flux_u_fix
           flux_v_ave(n)=flux_v_fix
           flux_t_ave(n)=flux_t_fix
           flux_q_ave(n)=flux_q_fix
           flux_salt_ave(n)=flux_salt_fix
           flux_sw_ave(n)=flux_sw_fix
           flux_lw_ave(n)=flux_lw_fix
           p_surf_ave(n)=p_surf_fix
         endif
         rough_ave(n)=rough_fix
      enddo
!
      print *,'nsice:',      nsice
      print *,'albedo_ave:', albedo_ave
      print *,'h_snow_ave:', h_snow_ave
      print *,'t_surf_ave:', t_surf_ave
      print *,'t_ice1_ave:', t_ice1_ave
      print *,'t_ice2_ave:', t_ice2_ave
      print *,'u_ice_ave:',  u_ice_ave
      print *,'v_ice_ave:',  v_ice_ave
      print *,'sig11_ave:',  sig11_ave
      print *,'sig22_ave:',  sig22_ave
      print *,'sig12_ave:',  sig12_ave
      print *,'flux_u_ave:', flux_u_ave
      print *,'flux_v_ave:', flux_v_ave
      print *,'flux_t_ave:', flux_t_ave
      print *,'flux_q_ave:', flux_q_ave
      print *,'flux_salt_ave:', flux_salt_ave
      print *,'flux_sw_ave:',flux_sw_ave
      print *,'flux_lw_ave:',flux_lw_ave
      print *,'p_surf_ave:', p_surf_ave
      print *,'rough_ave:',  rough_ave
      print *,'fcycle:',  fcycle
      print *,'fhiclm:',  fhiclm
      print *,'wt_hi:',  wt_hi
      print *,'hice_clm:', hice_clm
      print *,'hice_anl:', hice_anl
      print *,'sst_anl:', sst_anl
      print *,'hsno_gdas:', hsno_gdas
      print *,'melt_pond:', melt_pond
!
      do j=1,jm
      do i=1,im
         if (omsk(i,j).GT.p5) then
          if (icecsfc(i,j).GE.fic) then
            if (ylat(i,j) .LT. 0.) then
               n=1
            else
               n=2
            endif
            icecsfc(i,j)=MIN(icecsfc(i,j),h1)
! melt pond in the Arctic for CFSRR (not for GFS)
            if (melt_pond) then
              if ( (imth.GE.6) .AND. (imth.LE.9) .AND. (ylat(i,j).GE.70.0) &
                 .AND. (icecsfc(i,j).LT.fice(i,j)) ) then
                 icecsfc(i,j)=0.5*(icecsfc(i,j)+fice(i,j))
              endif
            endif
            do k=ki1,2,-1
               if (part_size(i,j,k).GT.0.01) then
                  kr=k
                  go to 200
               endif
            enddo
            kr=2
200         continue
            part_size(i,j,1)=1-icecsfc(i,j)
!
! CFSRR
!
            ficenew=icecsfc(i,j)-fice(i,j)
            if (icecsfc(i,j).LT.fice(i,j)) then
               part_size(i,j,2)=part_size(i,j,2)+ficenew
            else
               part_size(i,j,kr)=part_size(i,j,kr)+ficenew
            endif
!
! For GFS
!           if (icecsfc(i,j).LT.fice(i,j)) then
!              part_size(i,j,2)=part_size(i,j,2)+ficenew
!           else
!              if (fice(i,j).gt.fic) then
!                 do k=2,kr
!                    part_size(i,j,k)=part_size(i,j,k)*(1+ficenew/fice(i,j))
!                 enddo
!              else
!                 part_size(i,j,2)=part_size(i,j,2)+ficenew
!              endif
!           endif
            if (fice(i,j).LT.fic) then
              albedo(i,j,2:)=MAX(albedo_ave(n),albedo_fix)
              rough_mom(i,j,2:)=rough_ave(n)
              rough_heat(i,j,2:)=rough_ave(n)
              rough_moist(i,j,2:)=rough_ave(n)
              t_surf(i,j,2:)=MIN(t_surf_ave(n),t_surf_fix)
              h_snow(i,j,1)=MAX(h_snow_ave(n),h_snow_fix)
              h_ice(i,j,1)=p2
              t_ice1(i,j,:)=MIN(t_ice1_ave(n),t_ice1_fix)
              t_ice2(i,j,:)=MIN(t_ice2_ave(n),t_ice2_fix)
              u_ice(i,j,:)=u_ice_ave(n)
              v_ice(i,j,:)=v_ice_ave(n)
              sig11(i,j,:)=sig11_ave(n)
              sig22(i,j,:)=sig22_ave(n)
              sig12(i,j,:)=sig12_ave(n)
              flux_u(i,j,:)=flux_u_ave(n)
              flux_v(i,j,:)=flux_v_ave(n)
              flux_t(i,j,:)=flux_t_ave(n)
              flux_q(i,j,:)=flux_q_ave(n)
              flux_salt(i,j,:)=flux_salt_ave(n)
              flux_sw(i,j,:)=flux_sw_ave(n)
              flux_lw(i,j,:)=flux_lw_ave(n)
              p_surf(i,j,:)=p_surf_ave(n)
            endif
            do k=1,ki2
              if (h_ice(i,j,k) .GE. 0.02) &
              h_ice(i,j,k)=MIN(MAX((wt_hi*h_ice(i,j,k)+(1-wt_hi)*icetksfc(i,j)),P2),hmax)
            enddo
            do k=2,ki2
               if (part_size(i,j,k) < h0) then
                  part_size(i,j,k+1)=part_size(i,j,k+1)+part_size(i,j,k)
                  h_ice(i,j,k)=MAX(h_ice(i,j,k),h_ice(i,j,k-1))
                  h_snow(i,j,k)=MAX(h_snow(i,j,k),h_snow(i,j,k-1))
                  t_ice1(i,j,k)=MIN(t_ice1(i,j,k),t_ice1(i,j,k-1))
                  t_ice2(i,j,k)=MIN(t_ice2(i,j,k),t_ice2(i,j,k-1))
                  part_size(i,j,k) = h0
                  h_ice(i,j,k-1)   = h0
                  h_snow(i,j,k-1)  = h0
                  t_ice1(i,j,k-1)  = h0
                  t_ice2(i,j,k-1)  = h0
               endif
            enddo
            temp(i,j,1,1)=0.5*temp(i,j,1,1)+0.5*tf
            if (hsno_gdas) then
               do k=1,ki2
                  if (h_ice(i,j,k) .GE. 0.02) then
                     h_snow(i,j,k) = hsno(i,j)
                  else
                     h_snow(i,j,k) = h0
                  endif
               enddo
            endif
          else
            if (fice(i,j).GT.h0) then
               temp(i,j,1,1)=0.5*temp(i,j,1,1)+0.5*h0
               part_size(i,j,1)=h1
               if (sst_anl) then
                  t_surf(i,j,1)=sst(i,j)
               endif
               do k=1,ki2
                  part_size(i,j,k+1)=h0
                  h_ice(i,j,k)=h0
                  h_snow(i,j,k)=h0
                  t_ice1(i,j,k)=h0
                  t_ice2(i,j,k)=h0
               enddo
            endif
          endif
         endif
      enddo
      enddo

      call write_ice_rstrt (im,jm,ki1,ki2,ki3,                    &
        xaxis_1,yaxis_1,zaxis_1,zaxis_2,zaxis_3,Time,             &
        part_size,albedo,rough_mom,rough_heat,rough_moist,t_surf, &
        h_snow,h_ice,t_ice1,t_ice2,                               &
        u_ice,v_ice,sig11,sig22,sig12,flux_u,flux_v,flux_t,flux_q,&
        flux_salt,flux_sw,flux_lw,lprec,fprec,runoff,calving,p_surf)

      ofile_out='ocean_temp_salt.rst.nc'
      call write_ocnvar2(im,jm,ko,ko,lm,temp,salt,stemp,ssalt,ofile_out,title,2,1)

      stop
      end
