#!/bin/ksh --login

#------------------------------------------------------------
# Run the snow2mdl program on Theia for a NAM grid.
#
# To run, type 'qsub $script'
#------------------------------------------------------------

#PBS -l procs=1
#PBS -l vmem=3G
#PBS -l walltime=0:05:00
#PBS -A fv3-cpu
#PBS -N snow2mdl
#PBS -o nam.log
#PBS -e nam.log

set -x

export OUTPUT_GRIB2=.false.

export DATA="/scratch3/NCEPDEV/stmp1/$LOGNAME/snow2mdl"

export HOMEglobal=$PBS_O_WORKDIR/../..

export WGRIB=/apps/wgrib/1.8.1.0b/bin/wgrib
export WGRIB2=/apps/wgrib2/0.1.9.5.1/bin/wgrib2

export IMS_FILE=/scratch4/NCEPDEV/da/noscrub/George.Gayno/snow2mdl/imssnow96.grb
export AFWA_NH_FILE=/scratch4/NCEPDEV/da/noscrub/George.Gayno/snow2mdl/NPR.SNWN.SP.S1200.MESH16
export AFWA_SH_FILE=/scratch4/NCEPDEV/da/noscrub/George.Gayno/snow2mdl/NPR.SNWS.SP.S1200.MESH16

export MODEL_LATITUDE_FILE=/scratch4/NCEPDEV/da/noscrub/George.Gayno/snow2mdl/conus_hpnt_latitudes.grb
export MODEL_LONGITUDE_FILE=/scratch4/NCEPDEV/da/noscrub/George.Gayno/snow2mdl/conus_hpnt_longitudes.grb
export MODEL_SLMASK_FILE=/scratch4/NCEPDEV/da/noscrub/George.Gayno/snow2mdl/conus_slmask.grb
export GFS_LONSPERLAT_FILE=" "

export OMP_NUM_THREADS=1

${HOMEglobal}/ush/emcsfc_snow.sh

exit 0
