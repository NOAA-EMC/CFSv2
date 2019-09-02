#!/bin/sh

#-------------------------------------------------------------------
# Run snow2mdl program for NAM grid on the WCOSS-cray.
#
# Inoke as follows: "cat run.nam.wcoss.cray.sh | bsub"
#-------------------------------------------------------------------

#BSUB -oo nam.log
#BSUB -eo nam.log
#BSUB -q dev_shared
#BSUB -R rusage[mem=2200]
#BSUB -J snow2mdl
#BSUB -P NAM-T2O
#BSUB -cwd .
#BSUB -W 0:03

set -x

module load grib_util
module list

# working directory
export DATA=/gpfs/hps/stmp/$LOGNAME/nam_snow

export HOMEglobal=${LS_SUBCWD}/../..

# The input data directory resides on tide/gyre.  It is ok to read
# from tide/gyre to cray.  Can only see the 'dev' machine's directory.
if [[ -d /gpfs/td1/emc/global/noscrub/George.Gayno/snow2mdl ]]; then
  INPUT_DATA_DIR=/gpfs/td1/emc/global/noscrub/George.Gayno/snow2mdl
elif [[ -d /gpfs/gd1/emc/global/noscrub/George.Gayno/snow2mdl ]]; then
  INPUT_DATA_DIR=/gpfs/gd1/emc/global/noscrub/George.Gayno/snow2mdl
else
  echo "INPUT DATA DIRECTORY DOES NOT EXIST.  EXIT."
fi

# Files that define the model grid.
export MODEL_LATITUDE_FILE=$INPUT_DATA_DIR/nam_hpnt_latitudes.grb
export MODEL_LONGITUDE_FILE=$INPUT_DATA_DIR/nam_hpnt_longitudes.grb
export MODEL_SLMASK_FILE=$INPUT_DATA_DIR/nam_slmask.grb
export GFS_LONSPERLAT_FILE=" "

# The input snow data.
export IMS_FILE=$INPUT_DATA_DIR/imssnow96.grb
export AFWA_NH_FILE=$INPUT_DATA_DIR/NPR.SNWN.SP.S1200.MESH16
export AFWA_SH_FILE=" "

export OUTPUT_GRIB2=".false."

#export FORT_FMT_NO_WRAP_MARGIN=1

${HOMEglobal}/ush/emcsfc_snow.sh

exit 0
