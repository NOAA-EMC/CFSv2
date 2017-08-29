!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!                                                                              !
!  This program reads ice restart file and real time sst, sea ice and GDAS     !
!  snow data, and merge sea ice and snow data onto ice restart file.           !
!  All data are on tripolar grid.                                              !
!  It contains: ice_rstrt_init, read_ice_rstrt, write_ice_rstrt,read_fice,     !
!               read_sst, read_hsno,read_gridmask                              !
!                                                                              !
!     Xingren Wu   (Xingren.Wu@noaa.gov)                                       !
!     2008-08-15                                                               !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

module sice_rstrt_mode

      implicit none


      real*8, parameter :: albedo_fix    = 0.5              ! albedo
      real*8, parameter :: h_snow_fix    = 0.01             ! snow depth
      real*8, parameter :: rough_fix     = 0.0005           ! roughness
      real*8, parameter :: t_surf_fix    = 271.15           ! surface temp
      real*8, parameter :: t_ice1_fix    = -2.0             ! ice temp
      real*8, parameter :: t_ice2_fix    = -1.0             ! ice temp
      real*8, parameter :: u_ice_fix     = -0.02            ! ice u-velocity
      real*8, parameter :: v_ice_fix     = -0.02            ! ice v-velocity
      real*8, parameter :: sig11_fix     = -5000.           ! ice force
      real*8, parameter :: sig22_fix     = -4500.           ! ice force
      real*8, parameter :: sig12_fix     = -1000.           ! ice force
      real*8, parameter :: flux_u_fix    = 0.0005           ! wind u-stress
      real*8, parameter :: flux_v_fix    = -0.005           ! wind v-stress
      real*8, parameter :: flux_t_fix    = 15.0             ! sensible heat
      real*8, parameter :: flux_q_fix    = 1.0e-6           ! evaporation
      real*8, parameter :: flux_salt_fix = 2.0e-7           ! i2o salt flux
      real*8, parameter :: flux_sw_fix   = 10.0             ! sw
      real*8, parameter :: flux_lw_fix   = -10.0            ! lw
      real*8, parameter :: p_surf_fix    = 1.0e5            ! pressure
      real*8, parameter :: fcycle        = 6.0
      real*8, parameter :: fhiclm        = 50.0
      real*8 :: wt_hi
      logical, parameter :: hice_clm     = .false.
      logical, parameter :: hice_anl     = .false.
      logical, parameter :: sst_anl      = .false.
      logical, parameter :: hsno_gdas    = .false.
      logical, parameter :: melt_pond    = .false.
!
      real, parameter :: wtcl=0.9, wtan=1.0-wtcl


!      NAMELIST /ICE_SETTINGS/ albedo_fix,rough_fix,t_surf_fix,& 
!     &          t_ice1_fix,t_ice2_fix,u_ice_fix,v_ice_fix,   &  
!     &          sig11_fix,sig22_fix,sig12_fix,flux_u_fix,     & 
!     &         flux_v_fix,flux_t_fix,flux_q_fix,flux_salt_fix, &
!     &         flux_sw_fix,flux_lw_fix,p_surf_fix,h_snow_fix,& 
!     &         fcycle,fhiclm,wt_hi,hice_clm,hice_anl,sst_anl,& 
!     &         hsno_gdas,melt_pond

contains

      subroutine ice_rstrt_init

      integer ios

      open(99,file='ice_init_nml',status='old',iostat=ios)
      if (ios.NE.0) then
        print*,'Error opening file ice_init_nml.'
      endif
!      read(99,NML=ICE_SETTINGS,iostat=ios)
      if (fhiclm > 0.) then
         wt_hi=EXP(-fcycle/(24.*fhiclm))
      else
         wt_hi=1.0
      endif
      if (.NOT.hice_anl) wt_hi=1.0
!      write(6,NML=ICE_SETTINGS)

      return
      end subroutine ice_rstrt_init

      subroutine read_ice_rstrt(im,jm,kz1,kz2,kz3, &               
     &   xaxis_1,yaxis_1,zaxis_1,zaxis_2,zaxis_3,Time, &            
     &   part_size,albedo,rough_mom,rough_heat,rough_moist,t_surf,& 
     &   h_snow,h_ice,t_ice1,t_ice2,                         &      
     &   u_ice,v_ice,sig11,sig22,sig12,flux_u,flux_v,flux_t,flux_q, &
     &   flux_salt,flux_sw,flux_lw,lprec,fprec,runoff,calving,p_surf)

      INCLUDE "netcdf.inc"

      integer im,jm,kz1,kz2,kz3
      integer i,j,k,status, ncid
      real*8  Time
      real*8, dimension(im,jm,kz1) :: part_size,albedo,rough_mom,  &
     &                                 rough_heat,rough_moist,t_surf
      real*8, dimension(im,jm,kz2) :: h_snow,h_ice,t_ice1,t_ice2
      real*8, dimension(im,jm,kz3) :: u_ice,v_ice,sig11,sig22,sig12, &
     &                                 flux_u,flux_v,flux_t,flux_q,   &
     &                                 flux_salt,flux_sw,flux_lw,lprec,  &
     &                                 fprec,runoff,calving,p_surf
      real xaxis_1(im), yaxis_1(jm),zaxis_1(kz1),zaxis_2(kz2),zaxis_3(kz3)
      integer :: xaxis_1_dim, yaxis_1_dim,zaxis_1_dim,  &
     &            zaxis_2_dim,zaxis_3_dim,Time_dim
      integer vardim_z1(4),vardim_z2(4),vardim_z3(4),startvar(4),countvar(4)
      integer id_xaxis_1,id_yaxis_1,id_zaxis_1,id_zaxis_2,id_zaxis_3,  &
     &         id_Time,id_part_size,id_albedo,id_rough_mom,id_rough_heat,  &
     &         id_rough_moist,id_t_surf,id_h_snow,id_h_ice,id_t_ice1,  &
     &         id_t_ice2,id_u_ice,id_v_ice,id_sig11,id_sig22,id_sig12,  &
     &         id_flux_u,id_flux_v,id_flux_t,id_flux_q,id_flux_salt,  &
     &         id_flux_sw,id_flux_lw,id_lprec,id_fprec,id_runoff,  &
     &         id_calving,id_p_surf
      character(len=120) :: ice_rstrt_file

      ice_rstrt_file='ice_model.res.nc'

      status = nf_open(ice_rstrt_file, NF_NOWRITE, ncid)  ; ! print *,'ncid=',ncid
      if (status.ne.NF_NOERR) stop 'open'
      status = nf_inq_varid(ncid, 'xaxis_1'     , id_xaxis_1)
      status = nf_inq_varid(ncid, 'yaxis_1'     , id_yaxis_1)
      status = nf_inq_varid(ncid, 'zaxis_1'     , id_zaxis_1)
      status = nf_inq_varid(ncid, 'zaxis_2'     , id_zaxis_2)
      status = nf_inq_varid(ncid, 'zaxis_3'     , id_zaxis_3)
      status = nf_inq_varid(ncid, 'Time'        , id_Time)
      status = nf_inq_varid(ncid, 'part_size'   , id_part_size)
      status = nf_inq_varid(ncid, 'albedo'      , id_albedo)
      status = nf_inq_varid(ncid, 'rough_mom'   , id_rough_mom)
      status = nf_inq_varid(ncid, 'rough_heat'  , id_rough_heat)
      status = nf_inq_varid(ncid, 'rough_moist' , id_rough_moist)
      status = nf_inq_varid(ncid, 't_surf'      , id_t_surf)
      status = nf_inq_varid(ncid, 'h_snow'      , id_h_snow)
      status = nf_inq_varid(ncid, 'h_ice'       , id_h_ice)
      status = nf_inq_varid(ncid, 't_ice1'      , id_t_ice1)
      status = nf_inq_varid(ncid, 't_ice2'      , id_t_ice2)
      status = nf_inq_varid(ncid, 'u_ice'       , id_u_ice)
      status = nf_inq_varid(ncid, 'v_ice'       , id_v_ice)
      status = nf_inq_varid(ncid, 'sig11'       , id_sig11)
      status = nf_inq_varid(ncid, 'sig22'       , id_sig22)
      status = nf_inq_varid(ncid, 'sig12'       , id_sig12)
      status = nf_inq_varid(ncid, 'flux_u'      , id_flux_u)
      status = nf_inq_varid(ncid, 'flux_v'      , id_flux_v)
      status = nf_inq_varid(ncid, 'flux_t'      , id_flux_t)
      status = nf_inq_varid(ncid, 'flux_q'      , id_flux_q)
      status = nf_inq_varid(ncid, 'flux_salt'   , id_flux_salt)
      status = nf_inq_varid(ncid, 'flux_sw'     , id_flux_sw)
      status = nf_inq_varid(ncid, 'flux_lw'     , id_flux_lw)
      status = nf_inq_varid(ncid, 'lprec'       , id_lprec)
      status = nf_inq_varid(ncid, 'fprec'       , id_fprec)
      status = nf_inq_varid(ncid, 'runoff'      , id_runoff)
      status = nf_inq_varid(ncid, 'calving'     , id_calving)
      status = nf_inq_varid(ncid, 'p_surf'     , id_p_surf)

! read variable
      status = nf_get_var_real(ncid, id_xaxis_1,xaxis_1)
      status = nf_get_var_real(ncid, id_yaxis_1,yaxis_1)
      status = nf_get_var_real(ncid, id_zaxis_1,zaxis_1)
      status = nf_get_var_real(ncid, id_zaxis_2,zaxis_2)
      status = nf_get_var_real(ncid, id_zaxis_3,zaxis_3)
      status = nf_get_var_double(ncid, id_Time,Time)
      status = nf_get_var_double(ncid, id_part_size,part_size)
      status = nf_get_var_double(ncid, id_albedo,albedo)
      status = nf_get_var_double(ncid, id_rough_mom,rough_mom)
      status = nf_get_var_double(ncid, id_rough_heat,rough_heat)
      status = nf_get_var_double(ncid, id_rough_moist,rough_moist)
      status = nf_get_var_double(ncid, id_t_surf,t_surf)
      status = nf_get_var_double(ncid, id_h_snow,h_snow)
      status = nf_get_var_double(ncid, id_h_ice,h_ice)
      status = nf_get_var_double(ncid, id_t_ice1,t_ice1)
      status = nf_get_var_double(ncid, id_t_ice2,t_ice2)
      status = nf_get_var_double(ncid, id_u_ice,u_ice)
      status = nf_get_var_double(ncid, id_v_ice,v_ice)
      status = nf_get_var_double(ncid, id_sig11,sig11)
      status = nf_get_var_double(ncid, id_sig22,sig22)
      status = nf_get_var_double(ncid, id_sig12,sig12)
      status = nf_get_var_double(ncid, id_flux_u,flux_u)
      status = nf_get_var_double(ncid, id_flux_v,flux_v)
      status = nf_get_var_double(ncid, id_flux_t,flux_t)
      status = nf_get_var_double(ncid, id_flux_q,flux_q)
      status = nf_get_var_double(ncid, id_flux_salt,flux_salt)
      status = nf_get_var_double(ncid, id_flux_sw,flux_sw)
      status = nf_get_var_double(ncid, id_flux_lw,flux_lw)
      status = nf_get_var_double(ncid, id_lprec,lprec)
      status = nf_get_var_double(ncid, id_fprec,fprec)
      status = nf_get_var_double(ncid, id_runoff,runoff)
      status = nf_get_var_double(ncid, id_calving,calving)
      status = nf_get_var_double(ncid, id_p_surf,p_surf)

      status=nf_close(ncid)
      
      return
      end subroutine read_ice_rstrt

      subroutine write_ice_rstrt (im,jm,kz1,kz2,kz3,   &            
     &   xaxis_1,yaxis_1,zaxis_1,zaxis_2,zaxis_3,Time,   &           
     &   part_size,albedo,rough_mom,rough_heat,rough_moist,t_surf,  &
     &   h_snow,h_ice,t_ice1,t_ice2,                                &
     &   u_ice,v_ice,sig11,sig22,sig12,flux_u,flux_v,flux_t,flux_q, &
     &   flux_salt,flux_sw,flux_lw,lprec,fprec,runoff,calving,p_surf)

      INCLUDE "netcdf.inc"

      integer im,jm,kz1,kz2,kz3
      integer i,j,k,status, ncid
      real*8 Time
      real*8, dimension(im,jm,kz1) :: part_size,albedo,rough_mom, &
     &                                rough_heat,rough_moist,t_surf
      real*8, dimension(im,jm,kz2) :: h_snow,h_ice,t_ice1,t_ice2
      real*8, dimension(im,jm,kz3) :: u_ice,v_ice,sig11,sig22,sig12,&
     &                                 flux_u,flux_v,flux_t,flux_q,  &
     &                                 flux_salt,flux_sw,flux_lw,lprec, &
     &                                 fprec,runoff,calving,p_surf
      real xaxis_1(im), yaxis_1(jm),zaxis_1(kz1),zaxis_2(kz2),zaxis_3(kz3)
      integer :: xaxis_1_dim, yaxis_1_dim,zaxis_1_dim, &
     &            zaxis_2_dim,zaxis_3_dim,Time_dim
      integer vardim_z1(4),vardim_z2(4),vardim_z3(4),startvar(4),countvar(4)
      integer id_xaxis_1,id_yaxis_1,id_zaxis_1,id_zaxis_2,id_zaxis_3, &
     &         id_Time,id_part_size,id_albedo,id_rough_mom,id_rough_heat, &
     &         id_rough_moist,id_t_surf,id_h_snow,id_h_ice,id_t_ice1, &
     &         id_t_ice2,id_u_ice,id_v_ice,id_sig11,id_sig22,id_sig12, &
     &         id_flux_u,id_flux_v,id_flux_t,id_flux_q,id_flux_salt, &
     &         id_flux_sw,id_flux_lw,id_lprec,id_fprec,id_runoff, &
     &         id_calving,id_p_surf
      character(len=120) :: ice_rstrt_file,title

      ice_rstrt_file='ice_model.rst.nc'

      title='RESTART/ice_model.res.nc'

      status = nf_create(ice_rstrt_file,  nf_write, ncid)
      status = nf_def_dim(ncid,'xaxis_1', im , xaxis_1_dim)
      status = nf_def_dim(ncid,'yaxis_1', jm , yaxis_1_dim)
      status = nf_def_dim(ncid,'zaxis_1', kz1, zaxis_1_dim)
      status = nf_def_dim(ncid,'zaxis_2', kz2, zaxis_2_dim)
      status = nf_def_dim(ncid,'zaxis_3', kz3, zaxis_3_dim)
      status = nf_def_dim(ncid,'Time'   , nf_unlimited, Time_dim)

! Define Variables 
      status = nf_def_var(ncid,'xaxis_1', nf_float, 1,xaxis_1_dim,id_xaxis_1)
      status = nf_def_var(ncid,'yaxis_1', nf_float, 1,yaxis_1_dim,id_yaxis_1)
      status = nf_def_var(ncid,'zaxis_1', nf_float, 1,zaxis_1_dim,id_zaxis_1)
      status = nf_def_var(ncid,'zaxis_2', nf_float, 1,zaxis_2_dim,id_zaxis_2)
      status = nf_def_var(ncid,'zaxis_3', nf_float, 1,zaxis_3_dim,id_zaxis_3)
      status = nf_def_var(ncid,'Time',    nf_double,1,Time_dim,      id_Time)
!4D variables 
      vardim_z1= (/xaxis_1_dim,yaxis_1_dim,zaxis_1_dim,Time_dim/)
      vardim_z2= (/xaxis_1_dim,yaxis_1_dim,zaxis_2_dim,Time_dim/)
      vardim_z3= (/xaxis_1_dim,yaxis_1_dim,zaxis_3_dim,Time_dim/)
      status = nf_def_var(ncid,'part_size'  ,nf_double,4,vardim_z1,id_part_size)
      status = nf_def_var(ncid,'albedo'     ,nf_double,4,vardim_z1,id_albedo)
      status = nf_def_var(ncid,'rough_mom'  ,nf_double,4,vardim_z1,id_rough_mom)
      status = nf_def_var(ncid,'rough_heat' ,nf_double,4,vardim_z1,id_rough_heat)
      status = nf_def_var(ncid,'rough_moist',nf_double,4,vardim_z1,id_rough_moist)
      status = nf_def_var(ncid,'t_surf'     ,nf_double,4,vardim_z1,id_t_surf)
      status = nf_def_var(ncid,'h_snow'     ,nf_double,4,vardim_z2,id_h_snow)
      status = nf_def_var(ncid,'h_ice'      ,nf_double,4,vardim_z2,id_h_ice)
      status = nf_def_var(ncid,'t_ice1'     ,nf_double,4,vardim_z2,id_t_ice1)
      status = nf_def_var(ncid,'t_ice2'     ,nf_double,4,vardim_z2,id_t_ice2)
      status = nf_def_var(ncid,'u_ice'      ,nf_double,4,vardim_z3,id_u_ice)
      status = nf_def_var(ncid,'v_ice'      ,nf_double,4,vardim_z3,id_v_ice)
      status = nf_def_var(ncid,'sig11'      ,nf_double,4,vardim_z3,id_sig11)
      status = nf_def_var(ncid,'sig22'      ,nf_double,4,vardim_z3,id_sig22)
      status = nf_def_var(ncid,'sig12'      ,nf_double,4,vardim_z3,id_sig12)
      status = nf_def_var(ncid,'flux_u'     ,nf_double,4,vardim_z3,id_flux_u)
      status = nf_def_var(ncid,'flux_v'     ,nf_double,4,vardim_z3,id_flux_v)
      status = nf_def_var(ncid,'flux_t'     ,nf_double,4,vardim_z3,id_flux_t)
      status = nf_def_var(ncid,'flux_q'     ,nf_double,4,vardim_z3,id_flux_q)
      status = nf_def_var(ncid,'flux_salt'  ,nf_double,4,vardim_z3,id_flux_salt)
      status = nf_def_var(ncid,'flux_sw'    ,nf_double,4,vardim_z3,id_flux_sw)
      status = nf_def_var(ncid,'flux_lw'    ,nf_double,4,vardim_z3,id_flux_lw)
      status = nf_def_var(ncid,'lprec'      ,nf_double,4,vardim_z3,id_lprec)
      status = nf_def_var(ncid,'fprec'      ,nf_double,4,vardim_z3,id_fprec)
      status = nf_def_var(ncid,'runoff'     ,nf_double,4,vardim_z3,id_runoff)
      status = nf_def_var(ncid,'calving'    ,nf_double,4,vardim_z3,id_calving)
      status = nf_def_var(ncid,'p_surf'     ,nf_double,4,vardim_z3,id_p_surf)

! ATTRIBUTES
      status = nf_put_att_text(ncid, nf_global, 'filename',     len_trim(title), title)
!xaxis_1
      status = nf_put_att_text(ncid,id_xaxis_1,'long_name',	  7, 'xaxis_1')
      status = nf_put_att_text(ncid,id_xaxis_1,'units', 	       4, 'none')
      status = nf_put_att_text(ncid,id_xaxis_1,'cartesian_axis',  1, 'X')
!yaxis_1
      status = nf_put_att_text(ncid,id_yaxis_1,'long_name',	  7, 'yaxis_1')
      status = nf_put_att_text(ncid,id_yaxis_1,'units', 	       4, 'none')
      status = nf_put_att_text(ncid,id_yaxis_1,'cartesian_axis',  1, 'Y')
!zaxis_1
      status = nf_put_att_text(ncid,id_zaxis_1,'long_name',	  7, 'zaxis_1')
      status = nf_put_att_text(ncid,id_zaxis_1,'units', 	       4, 'none')
      status = nf_put_att_text(ncid,id_zaxis_1,'cartesian_axis',  1, 'Z')
!zaxis_2
      status = nf_put_att_text(ncid,id_zaxis_2,'long_name',	  7, 'zaxis_2')
      status = nf_put_att_text(ncid,id_zaxis_2,'units', 	       4, 'none')
      status = nf_put_att_text(ncid,id_zaxis_2,'cartesian_axis',  1, 'Z')
!zaxis_3
      status = nf_put_att_text(ncid,id_zaxis_3,'long_name',	  7, 'zaxis_3')
      status = nf_put_att_text(ncid,id_zaxis_3,'units', 	       4, 'none')
      status = nf_put_att_text(ncid,id_zaxis_3,'cartesian_axis',  1, 'Z')
!Time
      status = nf_put_att_text(ncid,id_Time,'long_name',     4, 'Time')
      status = nf_put_att_text(ncid,id_Time,'units',	    10, 'time level')
      status = nf_put_att_text(ncid,id_Time,'cartesian_axis',1, 'T')
!
      status = nf_put_att_text(ncid,id_part_size,'long_name',   9, 'part_size')
      status = nf_put_att_text(ncid,id_part_size,'units',       4, 'none')
      status = nf_put_att_text(ncid,id_albedo,'long_name',      6, 'albedo')
      status = nf_put_att_text(ncid,id_albedo,'units',          4, 'none')
      status = nf_put_att_text(ncid,id_rough_mom,'long_name',   9, 'rough_mom')
      status = nf_put_att_text(ncid,id_rough_mom,'units',       4, 'none')
      status = nf_put_att_text(ncid,id_rough_heat,'long_name', 10, 'rough_heat')
      status = nf_put_att_text(ncid,id_rough_heat,'units',      4, 'none')
      status = nf_put_att_text(ncid,id_rough_moist,'long_name',11, 'rough_moist')
      status = nf_put_att_text(ncid,id_rough_moist,'units',     4, 'none')
      status = nf_put_att_text(ncid,id_t_surf,'long_name',      6, 't_surf')
      status = nf_put_att_text(ncid,id_t_surf,'units',          4, 'none')
      status = nf_put_att_text(ncid,id_h_snow,'long_name',      6, 'h_snow')
      status = nf_put_att_text(ncid,id_h_snow,'units',          4, 'none')
      status = nf_put_att_text(ncid,id_h_ice,'long_name',       5, 'h_ice')
      status = nf_put_att_text(ncid,id_h_ice,'units',           4, 'none')
      status = nf_put_att_text(ncid,id_t_ice1,'long_name',      6, 't_ice1')
      status = nf_put_att_text(ncid,id_t_ice1,'units',          4, 'none')
      status = nf_put_att_text(ncid,id_t_ice2,'long_name',      6, 't_ice2')
      status = nf_put_att_text(ncid,id_t_ice2,'units',          4, 'none')
      status = nf_put_att_text(ncid,id_u_ice,'long_name',       5, 'u_ice')
      status = nf_put_att_text(ncid,id_u_ice,'units',           4, 'none')
      status = nf_put_att_text(ncid,id_v_ice,'long_name',       5, 'v_ice')
      status = nf_put_att_text(ncid,id_v_ice,'units',           4, 'none')
      status = nf_put_att_text(ncid,id_sig11,'long_name',       5, 'sig11')
      status = nf_put_att_text(ncid,id_sig11,'units',           4, 'none')
      status = nf_put_att_text(ncid,id_sig22,'long_name',       5, 'sig22')
      status = nf_put_att_text(ncid,id_sig22,'units',           4, 'none')
      status = nf_put_att_text(ncid,id_sig12,'long_name',       5, 'sig12')
      status = nf_put_att_text(ncid,id_sig12,'units',           4, 'none')
      status = nf_put_att_text(ncid,id_flux_u,'long_name',      6, 'flux_u')
      status = nf_put_att_text(ncid,id_flux_u,'units',          4, 'none')
      status = nf_put_att_text(ncid,id_flux_v,'long_name',      6, 'flux_v')
      status = nf_put_att_text(ncid,id_flux_v,'units',          4, 'none')
      status = nf_put_att_text(ncid,id_flux_t,'long_name',      6, 'flux_t')
      status = nf_put_att_text(ncid,id_flux_t,'units',          4, 'none')
      status = nf_put_att_text(ncid,id_flux_q,'long_name',      6, 'flux_q')
      status = nf_put_att_text(ncid,id_flux_q,'units',          4, 'none')
      status = nf_put_att_text(ncid,id_flux_salt,'long_name',   9, 'flux_salt')
      status = nf_put_att_text(ncid,id_flux_salt,'units',       4, 'none')
      status = nf_put_att_text(ncid,id_flux_sw,'long_name',     7, 'flux_sw')
      status = nf_put_att_text(ncid,id_flux_sw,'units',         4, 'none')
      status = nf_put_att_text(ncid,id_flux_lw,'long_name',     7, 'flux_lw')
      status = nf_put_att_text(ncid,id_flux_lw,'units',         4, 'none')
      status = nf_put_att_text(ncid,id_lprec,'long_name',       5, 'lprec')
      status = nf_put_att_text(ncid,id_lprec,'units',           4, 'none')
      status = nf_put_att_text(ncid,id_fprec,'long_name',       5, 'fprec')
      status = nf_put_att_text(ncid,id_fprec,'units',           4, 'none')
      status = nf_put_att_text(ncid,id_runoff,'long_name',      6, 'runoff')
      status = nf_put_att_text(ncid,id_runoff,'units',          4, 'none')
      status = nf_put_att_text(ncid,id_calving,'long_name',     7, 'calving')
      status = nf_put_att_text(ncid,id_calving,'units',         4, 'none')
      status = nf_put_att_text(ncid,id_p_surf,'long_name',      6, 'p_surf')
      status = nf_put_att_text(ncid,id_p_surf,'units',          4, 'none')

! Leave Define Mode
      status = nf_enddef(ncid)

! Write Variables
      status = nf_put_vara_real(ncid,id_xaxis_1,1,im ,xaxis_1)
      status = nf_put_vara_real(ncid,id_yaxis_1,1,jm ,yaxis_1)
      status = nf_put_vara_real(ncid,id_zaxis_1,1,kz1,zaxis_1)
      status = nf_put_vara_real(ncid,id_zaxis_2,1,kz2,zaxis_2)
      status = nf_put_vara_real(ncid,id_zaxis_3,1,kz3,zaxis_3)
      status = nf_put_vara_double(ncid,id_Time,1,1,Time) ; ! print *,'time=',time

      startvar = (/1,1,1,1/)
      countvar = (/im,jm,kz1,1/)
      status = nf_put_vara_double(ncid,id_part_size  ,startvar,countvar,part_size)
      status = nf_put_vara_double(ncid,id_albedo     ,startvar,countvar,albedo)
      status = nf_put_vara_double(ncid,id_rough_mom  ,startvar,countvar,rough_mom)
      status = nf_put_vara_double(ncid,id_rough_heat ,startvar,countvar,rough_heat)
      status = nf_put_vara_double(ncid,id_rough_moist,startvar,countvar,rough_moist)
      status = nf_put_vara_double(ncid,id_t_surf,startvar,countvar,t_surf)
      startvar = (/1,1,1,1/)
      countvar = (/im,jm,kz2,1/)
      status = nf_put_vara_double(ncid,id_h_snow,startvar,countvar,h_snow)
      status = nf_put_vara_double(ncid,id_h_ice,startvar,countvar,h_ice)
      status = nf_put_vara_double(ncid,id_t_ice1,startvar,countvar,t_ice1)
      status = nf_put_vara_double(ncid,id_t_ice2,startvar,countvar,t_ice2)
      startvar = (/1,1,1,1/)
      countvar = (/im,jm,kz3,1/)
      status = nf_put_vara_double(ncid,id_u_ice,startvar,countvar,u_ice)
      status = nf_put_vara_double(ncid,id_v_ice,startvar,countvar,v_ice)
      status = nf_put_vara_double(ncid,id_sig11,startvar,countvar,sig11)
      status = nf_put_vara_double(ncid,id_sig22,startvar,countvar,sig22)
      status = nf_put_vara_double(ncid,id_sig12,startvar,countvar,sig12)
      status = nf_put_vara_double(ncid,id_flux_u,startvar,countvar,flux_u)
      status = nf_put_vara_double(ncid,id_flux_v,startvar,countvar,flux_v)
      status = nf_put_vara_double(ncid,id_flux_t,startvar,countvar,flux_t)
      status = nf_put_vara_double(ncid,id_flux_q,startvar,countvar,flux_q)
      status = nf_put_vara_double(ncid,id_flux_salt,startvar,countvar,flux_salt)
      status = nf_put_vara_double(ncid,id_flux_sw,startvar,countvar,flux_sw)
      status = nf_put_vara_double(ncid,id_flux_lw,startvar,countvar,flux_lw)
      status = nf_put_vara_double(ncid,id_lprec,startvar,countvar,lprec)
      status = nf_put_vara_double(ncid,id_fprec,startvar,countvar,fprec)
      status = nf_put_vara_double(ncid,id_runoff,startvar,countvar,runoff)
      status = nf_put_vara_double(ncid,id_calving,startvar,countvar,calving)
      status = nf_put_vara_double(ncid,id_p_surf,startvar,countvar,p_surf)

      status=nf_close(ncid)

      return
      end subroutine write_ice_rstrt

      subroutine read_fice(im,jm,icecsfc,icetksfc,y_T,icefile)

      INCLUDE "netcdf.inc"

      character(len=120) :: icefile
      integer :: status,ncid
      integer :: i,j,im,jm
      integer :: id_lat,id_fice
      real*8, dimension(im,jm) :: icecsfc,icetksfc,y_T
      real*8,  PARAMETER :: fimax=1.0,fimin=0.15,h2=2.0,h3=3.0,h0=0.0

      status = nf_open(icefile, NF_NOWRITE, ncid)
      if(status.ne.NF_NOERR) stop 'open'

      status = nf_inq_varid(ncid, 'y_T', id_lat)
      status = nf_inq_varid(ncid, 'icecsfc', id_fice)

      status = nf_get_var_double(ncid, id_lat,y_T)
      status = nf_get_var_double(ncid, id_fice,icecsfc)

      do j=1,jm
      do i=1,im
         icecsfc(i,j)=MIN(fimax,icecsfc(i,j))
         icetksfc(i,j)=h3*icecsfc(i,j)
         if (y_T(i,j).LT.0.) icetksfc(i,j)=h2*icecsfc(i,j)
         if (icecsfc(i,j).LT.fimin) then
            icecsfc(i,j)=h0
            icetksfc(i,j)=h0
         endif
      enddo
      enddo

      status=nf_close(ncid)

      return
      end subroutine read_fice

      subroutine read_hice(im,jm,icetksfc,hicefile,yyyy,mm,dd)

      INCLUDE "netcdf.inc"

      INTEGER, PARAMETER :: km=12
      character(len=120) :: hicefile
      integer :: status,ncid
      integer :: i,j,im,jm,yyyy,mm,dd
      integer :: d1,d2,m1,m2,mm1,mp1,id0(2)
      integer :: id_lat,id_hice
      integer, dimension(km)  :: md(km)
      real*8, dimension(im,jm) :: icetksfc,y_T
      real*8, dimension(im,jm,1,km) :: sit
      real*8  hiceclm,pr
      real*8,  PARAMETER :: himax=5.0,himin=0.2
      data md/31,28,31,30,31,30,31,31,30,31,30,31/

      status = nf_open(hicefile, NF_NOWRITE, ncid)
      if(status.ne.NF_NOERR) stop 'open'

      status = nf_inq_varid(ncid, 'y_T', id_lat)
      status = nf_inq_varid(ncid, 'sit', id_hice)

      status = nf_get_var_double(ncid, id_lat,y_T)
      status = nf_get_var_double(ncid, id_hice,sit)

      id0=0
      if (yyyy .LE. 1980) then
         pr=1.0
      else if (yyyy .GE. 2008) then
         pr=0.8
      else
         pr=(1.0*(2008-yyyy)+0.8*(yyyy-1980))/28
      endif
      mm1=mm-1
      mp1=mm+1
      if (mm1 .EQ. 0) mm1=12
      if (mp1 .EQ. 13) mp1=1
      if (mod(yyyy,4).EQ.0) then
        if (mm1.EQ.2) id0(1)=1
        if (mm.EQ.2) id0(2)=1
      endif
      if (dd.LT.16) then
         d1=md(mm1)+id0(1)-16+dd
         d2=md(mm1)+id0(1)-d1
         m1=mm1
         m2=mm
      else
         d1=dd-16
         d2=md(mm)+id0(2)-d1
         m1=mm
         m2=mp1
      endif
      print *,'yyyy,mm,dd,m1,m2,d1,d2,id0,pr,wtcl,wtan:',yyyy,mm,dd,m1,m2, &
     &         d1,d2,id0,pr,wtcl,wtan

      do j=1,jm
        do i=1,im
         if ((y_T(i,j).GT.30.).AND.(icetksfc(i,j).GT.0.1)) then
            hiceclm=pr*(d1*sit(i,j,1,m2)+d2*sit(i,j,1,m1))/(d1+d2)
            icetksfc(i,j)=MAX(MIN((wtan*icetksfc(i,j)+wtcl*hiceclm),himax),himin)
         endif
        enddo
      enddo

      status=nf_close(ncid)

      return
      end subroutine read_hice

      subroutine read_sst(im,jm,icecsfc,sst,sstfile)

      INCLUDE "netcdf.inc"

      character(len=120) :: sstfile
      integer :: status,ncid
      integer :: i,j,im,jm
      integer :: id_sst
      real*8, dimension(im,jm) :: icecsfc,sst
      real*8,  PARAMETER :: sstice=275.16,h0=0.0

      status = nf_open(sstfile, NF_NOWRITE, ncid)
      if(status.ne.NF_NOERR) stop 'open'

      status = nf_inq_varid(ncid, 'tmpsfc', id_sst)

      status = nf_get_var_double(ncid, id_sst,sst)

      do j=1,jm
      do i=1,im
         if (sst(i,j).GT.sstice) then
            icecsfc(i,j)=h0
         endif
      enddo
      enddo

      status=nf_close(ncid)

      return
      end subroutine read_sst

      subroutine read_hsno(im,jm,icecsfc,hsno,snofile)

      INCLUDE "netcdf.inc"

      character(len=120) :: snofile
      integer :: status,ncid
      integer :: i,j,im,jm
      integer :: id_hsno
      real*8, dimension(im,jm) :: icecsfc,hsno
      real*8,  PARAMETER :: fimin=0.15,h0=0.0

      status = nf_open(snofile, NF_NOWRITE, ncid)
      if(status.ne.NF_NOERR) stop 'open'

      status = nf_inq_varid(ncid, 'hsno', id_hsno)

      status = nf_get_var_double(ncid, id_hsno,hsno)

      do j=1,jm
      do i=1,im
         if (icecsfc(i,j).LT.fimin) then
            hsno(i,j)=h0
         else
            hsno(i,j)=hsno(i,j)*0.001
         endif
      enddo
      enddo

      status=nf_close(ncid)

      return
      end subroutine read_hsno

      subroutine read_gridmask(im,jm,wet)

      INCLUDE "netcdf.inc"

      integer im,jm
      integer i,j,k,status, ncid
      real*8, dimension(im,jm) :: wet
      integer id_wet
      character(len=120) :: grid_file

      grid_file='grid_spec.nc'

      status = nf_open(grid_file, NF_NOWRITE, ncid)
      if (status.ne.NF_NOERR) stop 'open'

      status = nf_inq_varid(ncid, 'wet',id_wet)

! read variable
      status = nf_get_var_double(ncid,id_wet,wet)

      status=nf_close(ncid)

      return
      end subroutine read_gridmask

      end module sice_rstrt_mode

