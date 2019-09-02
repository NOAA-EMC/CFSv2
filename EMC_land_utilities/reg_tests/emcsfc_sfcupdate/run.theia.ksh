#!/bin/ksh --login
 
#------------------------------------------------------------
# Run the sfcupdate program on Theia.
#
# To run this script do: "qsub run.theia.ksh"
#------------------------------------------------------------

#PBS -l procs=1
#PBS -l walltime=0:05:00
#PBS -l vmem=2000M
#PBS -A fv3-cpu
#PBS -N sfcupdate
#PBS -o ./log
#PBS -e ./log

#set -x

WORK_DIR="/scratch3/NCEPDEV/stmp1/George.Gayno/sfcupdate"
rm -fr $WORK_DIR
mkdir -p $WORK_DIR
cd $WORK_DIR

MAIN_DIR=$PBS_O_WORKDIR

cp ${MAIN_DIR}/../../exec/emcsfc_sfcupdate     ./

# get nems file from nco's nmmb parallel.
DATA_DIR=/scratch4/NCEPDEV/da/noscrub/George.Gayno/sfcupdate
ln -fs ${DATA_DIR}/ndas.t06z.input_nemsio.d01.tm12_presfcupdate   ./
cp ndas.t06z.input_nemsio.d01.tm12_presfcupdate ndas.t06z.input_nemsio.d01.tm12_afterupdate
chmod 644  ndas.t06z.input_nemsio.d01.tm12_afterupdate
cp ${DATA_DIR}/seaice.2014103018.nam.grb     ./seaice.2014103018.nam.grb
cp ${DATA_DIR}/snow.2014103018.nam.grb       ./snow.2014103018.nam.grb
cp ${DATA_DIR}/sst.2014103018.nam.grb        ./sst.2014103018.nam.grb

# directory where fixed fields for nmmb grid are located.  includes landmask, albedo, greenness.
FIXED_DIR=$DATA_DIR

cat > ./fort.41 << !
 &grid_info
  domain_name="nam"
 /
 &cycle
  cycle_year=2014
  cycle_month=10
  cycle_day=30
  cycle_hour=18
  fcst_hour=0.0
 /
 &model_flds
  first_guess_file="${WORK_DIR}/ndas.t06z.input_nemsio.d01.tm12_presfcupdate"
  first_guess_file_type="nems"
 /
 &fixed_flds
  lsmask_file="${FIXED_DIR}/nam_slmask.grb"
 /
 &time_varying_analysis_flds
  snowfree_albedo_climo_file="${FIXED_DIR}/nam_snowfree_albedo.grb"
  greenfrc_climo_file="${FIXED_DIR}/nam_vegfrac.grb"
  z0_climo_file=""
  seaice_anal_dir="${WORK_DIR}"
  seaice_climo_file=""
  sst_anal_dir="${WORK_DIR}"
  sst_climo_file=""
  merge_coeff_sst=0.
  sst_anlcyc_update=.false.
  soilm_climo_file=""
  merge_coeff_soilm=99999.
  snow_depth_anal_dir="${WORK_DIR}"
  snow_depth_climo_file=""
  merge_coeff_snow_depth=0.
  fire_anal_dir="${WORK_DIR}"
 /
 &output
  output_file="./ndas.t06z.input_nemsio.d01.tm12_afterupdate"
  output_file_type="nems"
 /
 &settings
  thinned=.false.
  nsoil=4
 /
 &soil_parameters
  soil_type_src="statsgo"
  smclow = 0.5
  smchigh = 3.0
  smcmax = 0.395, 0.421, 0.434, 0.476, 0.476, 0.439,
           0.404, 0.464, 0.465, 0.406, 0.468, 0.457,
           0.464, -9.99,  0.20, 0.421
  beta =  4.05,  4.26,  4.74,  5.33,  5.33,  5.25,
          6.77,  8.72,  8.17, 10.73, 10.39, 11.55,
          5.25, -9.99,  4.05,  4.26
  psis =  0.0350, 0.0363, 0.1413, 0.7586, 0.7586, 0.3548,
          0.1349, 0.6166, 0.2630, 0.0977, 0.3236, 0.4677,
          0.3548,  -9.99, 0.0350, 0.0363
  satdk = 1.7600e-4, 1.4078e-5, 5.2304e-6, 2.8089e-6, 2.8089e-6,
          3.3770e-6, 4.4518e-6, 2.0348e-6, 2.4464e-6, 7.2199e-6,
          1.3444e-6, 9.7394e-7, 3.3770e-6,     -9.99, 1.4078e-5,
          1.4078e-5
 /
 &veg_parameters
  veg_type_src="igbp"
  salp = 4.0
  snup = 0.080, 0.080, 0.080, 0.080, 0.080, 0.020,
         0.020, 0.060, 0.040, 0.020, 0.010, 0.020,
         0.020, 0.020, 0.013, 0.013, 0.010, 0.020,
         0.020, 0.020
 /
!

#------------------------------------------------------------
# Run executable.
#------------------------------------------------------------

np=$PBS_NP

module purge
module load intel/15.1.133
module load impi/5.1.1.109

mpirun -np $np ./emcsfc_sfcupdate

exit 0
