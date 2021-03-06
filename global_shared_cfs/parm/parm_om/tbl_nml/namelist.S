
 &diag_integral_nml
            file_name = 'diag_integral.out'
	    time_units = 'days'
	    output_interval = -1.0 /

 &fms_io_nml
         threading_read='multi'
         fileset_read='single'
         threading_write='single'
         fileset_write='single' /

 &fms_nml
       clock_grain='COMPONENT' /

 &ice_model_nml
       nsteps_dyn=72
       nsteps_adv=1
       num_part=6
       spec_ice=.false.
       ice_bulk_salin=0.005
       alb_sno=0.80
       t_range_melt=10.0
       heat_rough_ice=5.0e-4
       mom_rough_ice=5.0e-4
       wd_turn=0.0 /

 &monin_obukhov_nml
            neutral = .false. /

 &ocean_adv_vel_diag_nml
      diag_freq=720 /

 &ocean_advection_velocity_nml
      max_advection_velocity = 0.5 /

 &ocean_albedo_nml
      ocean_albedo_option = 2 /

 &ocean_bbc_nml
      u2tides=2.5e-3
      cdbot=1.e-3 /

 &ocean_bih_friction_const_nml
 /

 &ocean_bih_friction_general_nml
 /

 &ocean_convect_nml
      convect_ncon=.false. /

 &ocean_density_nml
      potrho_min=1028.0
      potrho_max=1038.0
      layer_nk=80
      linear_eos=.false. /

 &ocean_domains_nml
 /

 &ocean_freesurf_nml
      barotropic_pred_corr=.true.
      barotropic_leap_frog=.false.            
      pred_corr_gamma=0.20
      smooth_eta_t_fs_laplacian=.false.
      smooth_eta_t_fs_biharmonic=.true.
      smooth_eta_t_laplacian=.false.
      smooth_eta_t_biharmonic=.true.
      vel_micom_bih=0.01
      vel_micom_lap=0.05
      truncate_eta=.false.
      eta_max=8.0
      verbose_truncate=.true.
      frac_crit_cell_height=0.05 
      diag_freq=720
      zero_tendency=.false.
      debug_fs=.false. /

 &ocean_grids_nml
      debug_grid=.false. /

 &ocean_horz_diffuse_bih_nml
      horz_diffuse_on=.false. /

 &ocean_horz_diffuse_lap_nml
      horz_diffuse_on=.false. /

 &ocean_lap_friction_const_nml
 /

 &ocean_lap_friction_general_nml
      lap_friction_on=.true.
      k_smag_iso=2.0
      k_smag_aniso=0.0
      bottom_5point=.true.
      vel_micom_iso=0.10
      eq_vel_micom_iso=0.0
      eq_lat_micom=0.0
      equatorial_zonal=.true.
      equatorial_zonal_lat=20.0
      equatorial_no_smag=.true. 
      viscosity_ncar=.true.
      ncar_only_equatorial=.true.
      vconst_1=8.0e6
      vconst_2=0.0
      vconst_3=0.80
      vconst_4=0.5e-8
      vconst_5=3
      vconst_6=3e8
      vconst_7=100.0
      restrict_polar_visc=.true.
      restrict_polar_visc_lat=60.0
      restrict_polar_visc_ratio=0.35 /

 &ocean_model_nml
      time_tendency='twolevel'
      baroclinic_split = 1
      surface_height_split = 1
      barotropic_split = 80
      energy_diag_freq = 720
      debug=.false.
      acor=.50
      aidif=1.0 /

 &ocean_neutral_physics_nml
      neutral_physics_on=.true.
      neutral_physics_limit=.true.
      neutral_physics_debug=.false.
      aredi=600.0
      agm=600.0
      smax=0.002
      swidth=0.002
      tracer_mix_micom=.false.
      vel_micom=0.0
      gm_velocity_save=.false.
      agm_closure=.true.
      agm_closure_scaling=0.07
      agm_closure_min=100.0
      agm_closure_max=600.0
      agm_closure_length_fixed=.false.
      agm_closure_length_rossby=.false.
      agm_closure_length_bczone=.false.
      agm_closure_baroclinic=.true.
      agm_closure_length=50.e3
      agm_closure_buoy_freq=.004
      agm_closure_upper_depth=100.0
      agm_closure_lower_depth=2000.0
      neutral_physics_simple=.false.
      neutral_linear_gm_taper=.true.
      neutral_sine_taper=.true.
      neutral_blayer_diagnose=.true.
      rossby_radius_max=100e3
      rossby_radius_min=15e3
      tmask_neutral_on=.true. /

 &ocean_obc_nml
 /

 &ocean_overflow_nml
      use_overflow=.true.
      debug_overflow=.false. /

 &ocean_polar_filter_nml
      polar_filter_on=.false. /

 &ocean_pressure_nml
 /

 &ocean_rivermix_nml
      river_insertion_thickness=40.0
      river_diffusion_thickness=0.0      
      river_diffusivity=0.0
      debug_river=.false.
      river_diffuse_salt=.false.
      river_diffuse_temp=.false. /

 &ocean_riverspread_nml
 /

 &ocean_rough_nml
      	 rough_scheme = 'beljaars' /

 &ocean_sbc_nml
      use_waterflux=.true.
      temp_restore_tscale=5.
      salt_restore_tscale=10.0
      salt_restore_under_ice=.true.
      max_ice_thickness=4.0
      zero_net_water_restore=.true.
      zero_net_water_coupler=.true.
      zero_net_salt_restore=.true.
      avg_sfc_velocity=.true.
      avg_sfc_temp_salt_eta=.true.
      frazil_factor=1.0 /

 &ocean_shortwave_nml
      shortwave_pen_on=.true.
      read_chl=.true.
      zmax_pen=100.0
      debug_sw_pen=.false.
      enforce_sw_frac=.false. /

 &ocean_sigma_diffuse_nml
      sigma_diffuse_on=.true.
      tmask_sigma_on=.false.
      sigma_diffusivity_ratio=1.e-6
      sigma_thickness_max=100.0
      tracer_mix_micom=.true.
      vel_micom=0.50 /

 &ocean_thickness_nml
 /

 &ocean_topog_nml
 /

 &ocean_tracer_advect_nml
      debug_tracer_advect=.false.
      advect_sweby_all=.false. /

 &ocean_tracer_diag_nml
      tracer_conserve_days=1
      diag_freq=720 /

 &ocean_tracer_nml
      convective_adjust_on=.true.
      t_min=-5.0
      t_max = 55.0
      s_min = -2.0
      s_max = 55.0
      t_min_limit =-4.5
      t_max_limit =35.0
      s_min_limit =0.0
      s_max_limit =48.0
      use_frazil=.true.
      zero_tendency=.false.
      debug_tracer=.false. /

 &ocean_velocity_advect_nml
 /

 &ocean_velocity_diag_nml
      diag_freq=720 /

 &ocean_velocity_nml
      truncate_velocity=.false.
      truncate_verbose=.true.
      vel_max=2.0
      zero_tendency=.false.
      adams_bashforth_third=.true. /

 &ocean_vert_mix_coeff_kpp_nml
      diffusion_kpp_on=.true.
      diff_cbt_iw=1.34e-7
      visc_cbu_iw=1.0e-4
      double_diffusion=.true.
      Ricr=0.3
      smooth_blmc=.true.
      tidal_mix=.false. /

 &ocean_vert_mix_nml
      bryan_lewis_diffusivity=.true.
      use_diff_cbt_table=.false.
      linear_taper_diff_cbt_table=.false.
      bryan_lewis_lat_depend=.true.
      bryan_lewis_lat_transition=35.0
      afkph_90=0.75
      dfkph_90=0.95
      sfkph_90=4.5e-5
      zfkph_90=2500.0e2
      afkph_00=0.65
      dfkph_00=1.15
      sfkph_00=4.5e-5
      zfkph_00=2500.0e2 /

 &ocean_xlandinsert_nml
      use_xlandinsert=.false.
      verbose_init=.true. /

 &ocean_xlandmix_nml
      use_xlandmix=.false.
      verbose_init=.true. /

 &surface_flux_nml
            ncar_ocean_flux = .true.
	    raoult_sat_vap = .true. /

 &xgrid_nml
            make_exchange_reproduce=.true.
	    interp_method = 'second_order' /

 &godas_nml
      num_cor_tracers=2
      npits=110
      acoef=0.01
      vcvn=1.00
      vsclf=0.5
      debug_godas=.false. /
