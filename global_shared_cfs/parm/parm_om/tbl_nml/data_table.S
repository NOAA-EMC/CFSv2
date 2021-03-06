# format for data_table:
# gridname, fieldname_code, fieldname_file, filename,
#time_spec(-1), time_unit(-1), const_value(-1.e10), ongrid, factor
"OCN", "u_flux", "uflx"  ,  "INPUT/RA2_daily_TAUX.nc",   .false. ,1.0
"OCN", "v_flux", "vflx"  ,  "INPUT/RA2_daily_TAUY.nc",   .false. ,1.0
"OCN", "lprec",  "pme"   ,  "INPUT/RA2_daily_PME.nc",    .false. ,1.0
"OCN", "sw_flux","shrtwv",  "INPUT/RA2_daily_SHRTWV.nc", .false. ,1.0
"OCN", "t_flux", "tflux",   "INPUT/RA2_daily_TFLUX.nc",  .false. ,1.0
"OCN", "runoff", "runoff",  "INPUT/runoff.nc",           .false. ,1.0
"OBS", "temp", "", "INPUT/tmpa.mom",  .false. ,1.0
"OBS", "salt", "", "INPUT/sala.mom",  .false. ,1.0
"OBS", "mdl_tvv", "", "INPUT/tvv.mom", .false. ,1.0
"OBS", "mdl_svv", "", "INPUT/svv.mom", .false. ,1.0
