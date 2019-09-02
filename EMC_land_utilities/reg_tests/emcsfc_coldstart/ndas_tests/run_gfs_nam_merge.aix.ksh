#!/bin/ksh

#@ job_name = test
#@ job_type = parallel
#@ network.MPI=csss,shared,us
#@ total_tasks=1
#@ blocking=unlimited
#@ output = log.out
#@ error = log.out
#@ resources = consumablecpus(1) consumablememory(1000 MB)
#@ wall_clock_limit = 00:02:00
#@ node_usage=shared
#@ account_no=NAM-MTN
#@ class = dev
#@ queue

# run for a 'large' nam grid.  here large means larger
# than the ops ndas domain.  in this case, want to
# use ndas land states in the grid interior and use
# gfs land states elsewhere.  this is accomplished
# by running coldstart twice.  first, interpolate
# gfs to the output grid. then interpolate ndas
# data to the interior of the output grid.

# first interpolate gfs to target grid.
# this is the gfs file.                     
INPUT_FILE="/com/gfs/prod/gdas.20090713/gdas1.t00z.sfcanl"
INPUT_FILE_TYPE="gfs"

# this is the restart file for the 'large' target grid.
# must be nems io format.
WRF_BINARY_FILE="/ptmp/wx20gg/launcher3/test_input_umo_regional.nemsio.d01.gfs"
DYN_CORE="nems"

# the cold start executable
EXEC=/global/save/wx20gg/bgrids/nam_coldstart_wrf.fd/src/nam_coldstart_wrf

# when running 'merged', create fixed climo fields for soil type,
# veg type, z0, snow free albedo, max snow albedo using gridgen_sfc.
# then place grib files in this directory.
FIXDIR="/global/noscrub/wx20gg/coldstart_data/"

set -x

# your working directory
WORK_DIR="/ptmp/wx20gg/launcher3"

cd $WORK_DIR

#----------------------------------------------------------------
# don't touch fort.41 namelist settings.
#----------------------------------------------------------------

cat > ${WORK_DIR}/fort.41 << !
 &input_state_fields
  input_file="${INPUT_FILE}"
  input_file_type="${INPUT_FILE_TYPE}"
 /
 &output_grid_specs
  specs_from_output_file=.false.
  lats_output_file="${FIXDIR}/hiresw_hpnt_latitudes.expb.grb"
  lons_output_file="${FIXDIR}/hiresw_hpnt_longitudes.expb.grb"
  lsmask_output_file="${FIXDIR}/hiresw_slmask.expb.grb"
  orog_output_file="${FIXDIR}/hiresw_elevtiles.expb.grb"
  substrate_temp_output_file="${FIXDIR}/hiresw_tbot.expb.grb"
 /
 &optional_output_fields
  snow_free_albedo_output_file="${FIXDIR}/hiresw_snowfree_albedo.expb.grb"
  greenfrc_output_file="${FIXDIR}/hiresw_vegfrac.expb.grb"
  mxsnow_alb_output_file="${FIXDIR}/hiresw_mxsnoalb.expb.grb"
  slope_type_output_file=""
  soil_type_output_file="${FIXDIR}/hiresw_soiltiles.expb.grb"
  veg_type_output_file="${FIXDIR}/hiresw_vegtiles.expb.grb"
  z0_output_file="${FIXDIR}/hiresw_zorclim.expb.grb"
 /
 &soil_parameters
  soil_src_input = "zobler"
  smclow_input  = 0.5
  smchigh_input = 6.0
  smcmax_input= 0.421, 0.464, 0.468, 0.434, 0.406, 0.465,
                0.404, 0.439, 0.421
  beta_input  =   4.26,  8.72, 11.55,  4.74, 10.73,  8.17,
                  6.77,  5.25,  4.26
  psis_input  =   0.040, 0.620, 0.470, 0.140, 0.100, 0.260,
                  0.140, 0.360, 0.040
  satdk_input = 1.41e-5, 0.20e-5, 0.10e-5, 0.52e-5, 0.72e-5,
                0.25e-5, 0.45e-5, 0.34e-5, 1.41e-5
  soil_src_output = "statsgo"
  smclow_output  = 0.5
  smchigh_output = 3.0
  smcmax_output= 0.395, 0.421, 0.434, 0.476, 0.476, 0.439,
                 0.404, 0.464, 0.465, 0.406, 0.468, 0.457,
                 0.464, -9.99,  0.20, 0.421
  beta_output =  4.05,  4.26,  4.74,  5.33,  5.33,  5.25,
                 6.77,  8.72,  8.17, 10.73, 10.39, 11.55,
                 5.25, -9.99,  4.05,  4.26
  psis_output =  0.0350, 0.0363, 0.1413, 0.7586, 0.7586, 0.3548,
                 0.1349, 0.6166, 0.2630, 0.0977, 0.3236, 0.4677,
                 0.3548, -9.99,  0.0350, 0.0363
  satdk_output = 1.7600e-4, 1.4078e-5, 5.2304e-6, 2.8089e-6, 2.8089e-6,
                3.3770e-6, 4.4518e-6, 2.0348e-6, 2.4464e-6, 7.2199e-6,
                1.3444e-6, 9.7394e-7, 3.3770e-6,     -9.99, 1.4078e-5,
                1.4078e-5
 /

 &veg_parameters
  veg_src_input = "sib"
  veg_src_output = "usgs"
  salp_output = 4.0
  snup_output = 0.020, 0.020, 0.020, 0.020, 0.020, 0.020,
                0.020, 0.020, 0.020, 0.040, 0.040, 0.040,
                0.040, 0.040, 0.040, 0.010, 0.013, 0.020,
                0.013, 0.020, 0.020, 0.020, 0.020, 0.013
 /

 &final_output
  output_file_type="${DYN_CORE}"
  output_file="${WRF_BINARY_FILE}"
 /
 &options
  landice_opt=4
 /
 &nam_options
  merge=.false.
 /
!
poe $EXEC > ./log.gfs.step

rm -f ${WORK_DIR}/fort.41

# now interpolate ndas data to the grid interior.

# ndas file with the land states you want
INPUT_FILE="/com/nam/prod/nam.20090713/nam.t00z.wrfrst_d01.ges"
INPUT_FILE_TYPE="nmmwrf"

cp /ptmp/wx20gg/launcher3/test_input_umo_regional.nemsio.d01.gfs  \
   /ptmp/wx20gg/launcher3/test_input_umo_regional.nemsio.d01.ndas

# 'large' domain nmm file to be updated
WRF_BINARY_FILE="/ptmp/wx20gg/launcher3/test_input_umo_regional.nemsio.d01.ndas"
DYN_CORE="nems"

cat > ${WORK_DIR}/fort.41 << !
 &input_state_fields
  input_file="${INPUT_FILE}"
  input_file_type="${INPUT_FILE_TYPE}"
 /
 &output_grid_specs
  specs_from_output_file=.false.
  lats_output_file="${FIXDIR}/hiresw_hpnt_latitudes.expb.grb"
  lons_output_file="${FIXDIR}/hiresw_hpnt_longitudes.expb.grb"
  lsmask_output_file="${FIXDIR}/hiresw_slmask.expb.grb"
  orog_output_file="${FIXDIR}/hiresw_elevtiles.expb.grb"
  substrate_temp_output_file="${FIXDIR}/hiresw_tbot.expb.grb"
 /
 &optional_output_fields
  snow_free_albedo_output_file="${FIXDIR}/hiresw_snowfree_albedo.expb.grb"
  greenfrc_output_file="${FIXDIR}/hiresw_vegfrac.expb.grb"
  mxsnow_alb_output_file="${FIXDIR}/hiresw_mxsnoalb.expb.grb"
  slope_type_output_file=""
  soil_type_output_file="${FIXDIR}/hiresw_soiltiles.expb.grb"
  veg_type_output_file="${FIXDIR}/hiresw_vegtiles.expb.grb"
  z0_output_file="${FIXDIR}/hiresw_zorclim.expb.grb"
 /
 &soil_parameters
  soil_src_input = "statsgo"
  smclow_input  = 0.5
  smchigh_input = 3.0
  smcmax_input= 0.395, 0.421, 0.434, 0.476, 0.476, 0.439,
                 0.404, 0.464, 0.465, 0.406, 0.468, 0.457,
                 0.464, -9.99,  0.20, 0.421
  beta_input =  4.05,  4.26,  4.74,  5.33,  5.33,  5.25,
                 6.77,  8.72,  8.17, 10.73, 10.39, 11.55,
                 5.25, -9.99,  4.05,  4.26
  psis_input =  0.0350, 0.0363, 0.1413, 0.7586, 0.7586, 0.3548,
                 0.1349, 0.6166, 0.2630, 0.0977, 0.3236, 0.4677,
                 0.3548, -9.99,  0.0350, 0.0363
  satdk_input = 1.7600e-4, 1.4078e-5, 5.2304e-6, 2.8089e-6, 2.8089e-6,
                3.3770e-6, 4.4518e-6, 2.0348e-6, 2.4464e-6, 7.2199e-6,
                1.3444e-6, 9.7394e-7, 3.3770e-6,     -9.99, 1.4078e-5,
                1.4078e-5
  soil_src_output = "statsgo"
  smclow_output  = 0.5
  smchigh_output = 3.0
  smcmax_output= 0.395, 0.421, 0.434, 0.476, 0.476, 0.439,
                 0.404, 0.464, 0.465, 0.406, 0.468, 0.457,
                 0.464, -9.99,  0.20, 0.421
  beta_output =  4.05,  4.26,  4.74,  5.33,  5.33,  5.25,
                 6.77,  8.72,  8.17, 10.73, 10.39, 11.55,
                 5.25, -9.99,  4.05,  4.26
  psis_output =  0.0350, 0.0363, 0.1413, 0.7586, 0.7586, 0.3548,
                 0.1349, 0.6166, 0.2630, 0.0977, 0.3236, 0.4677,
                 0.3548, -9.99,  0.0350, 0.0363
  satdk_output = 1.7600e-4, 1.4078e-5, 5.2304e-6, 2.8089e-6, 2.8089e-6,
                3.3770e-6, 4.4518e-6, 2.0348e-6, 2.4464e-6, 7.2199e-6,
                1.3444e-6, 9.7394e-7, 3.3770e-6,     -9.99, 1.4078e-5,
                1.4078e-5
 /
 &veg_parameters
  veg_src_input = "sib"
  veg_src_output = "usgs"
  salp_output = 4.0
  snup_output = 0.020, 0.020, 0.020, 0.020, 0.020, 0.020,
                0.020, 0.020, 0.020, 0.040, 0.040, 0.040,
                0.040, 0.040, 0.040, 0.010, 0.013, 0.020,
                0.013, 0.020, 0.020, 0.020, 0.020, 0.013
 /
 &final_output
  output_file_type="${DYN_CORE}"
  output_file="${WRF_BINARY_FILE}"
 /
 &options
  landice_opt=3
 /
 &nam_options
  merge=.true.
 /
!
poe $EXEC > ./log.ndas.step

exit 0
