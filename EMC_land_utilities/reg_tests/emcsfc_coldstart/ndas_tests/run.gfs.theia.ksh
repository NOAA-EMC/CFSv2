#!/bin/ksh --login

#-----------------------------------------------------
# Interpolate gfs land states to a nam grid on
# Theia.
#
# To run:  qsub $script
#-----------------------------------------------------

#PBS -l procs=1
#PBS -l walltime=0:05:00
#PBS -l vmem=2000M
#PBS -A fv3-cpu
#PBS -N coldstart
#PBS -o ./log
#PBS -e ./log

set -x

RUN_DIR=$PBS_O_WORKDIR
EXE_DIR=${RUN_DIR}/../../../exec
EXEC=${EXE_DIR}/emcsfc_coldstart

WORK_DIR="/scratch3/NCEPDEV/stmp1/$LOGNAME/coldstart.gfs"
mkdir -p $WORK_DIR
cd $WORK_DIR

FIXDIR=/scratch4/NCEPDEV/da/noscrub/George.Gayno/coldstart

# file with land states you want
INPUT_FILE=$FIXDIR/gdas1.t00z.sfcanl

# file with land states you want updated
cp $FIXDIR/ndas.t12z.input_nemsio.d01.tm12_precoldstart  ./test_input_umo_regional.nemsio.d01 

cat > ./fort.41 << !
 &input_state_fields
  input_file="$INPUT_FILE"
  input_file_type="gfs"
 /

 &output_grid_specs
  specs_from_output_file=.false.
  lats_output_file="${FIXDIR}/nam_hpnt_latitudes.grb"
  lons_output_file="${FIXDIR}/nam_hpnt_longitudes.grb"
  lsmask_output_file="${FIXDIR}/nam_slmask.grb"
  orog_output_file="${FIXDIR}/nam_elevtiles.grb"
  substrate_temp_output_file="${FIXDIR}/nam_tbot.grb"
 /

 &optional_output_fields
  snow_free_albedo_output_file="${FIXDIR}/nam_snowfree_albedo.grb"
  greenfrc_output_file="${FIXDIR}/nam_vegfrac.grb"
  mxsnow_alb_output_file="${FIXDIR}/nam_mxsnoalb.grb"
  slope_type_output_file=""
  soil_type_output_file="${FIXDIR}/nam_soiltiles.grb"
  veg_type_output_file="${FIXDIR}/nam_vegtiles.grb"
  z0_output_file="${FIXDIR}/nam_z0clim.grb"
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
  veg_src_output = "igbp"
  salp_output = 4.0
  snup_output = 0.080, 0.080, 0.080, 0.080, 0.080, 0.020,
                0.020, 0.060, 0.040, 0.020, 0.010, 0.020,
                0.020, 0.020, 0.013, 0.013, 0.010, 0.020,
                0.020, 0.020
 /
 &final_output
  output_file_type="nems"
  output_file="test_input_umo_regional.nemsio.d01"
 /
 &options
  landice_opt=4
 /
 &nam_options
  merge=.false.
 /
!

#------------------------------------------------------------
# Run executable.
#------------------------------------------------------------

np=$PBS_NP

module purge
module load intel/14.0.2
module load impi/4.1.3.048

mpirun -np $np $EXEC

exit 0
