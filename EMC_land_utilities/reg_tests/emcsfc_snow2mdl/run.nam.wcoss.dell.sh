#!/bin/sh

#-------------------------------------------------------------------
# Run snow2mdl program for NAM grid on WCOSS Dell.
#
# Inoke as follows: "cat run.nam.wcoss.dell.sh | bsub"
#-------------------------------------------------------------------

#BSUB -oo nam.log
#BSUB -eo nam.log
#BSUB -q debug
#BSUB -R affinity[core(1)]
#BSUB -J snow2mdl
#BSUB -P NAM-T2O
#BSUB -cwd .
#BSUB -W 0:03
#BSUB -M 2000

set -x

module purge
module load EnvVars/1.0.2
module load ips/18.0.1.163

export DATA=/gpfs/dell1/stmp/$LOGNAME/nam_snow

export HOMEglobal=${LS_SUBCWD}/../..

INPUT_DIR=/gpfs/dell2/emc/modeling/noscrub/George.Gayno/landutil.git/snow2mdl

export MODEL_LATITUDE_FILE=$INPUT_DIR/nam_hpnt_latitudes.grb
export MODEL_LONGITUDE_FILE=$INPUT_DIR/nam_hpnt_longitudes.grb
export MODEL_SLMASK_FILE=$INPUT_DIR/nam_slmask.grb
export GFS_LONSPERLAT_FILE=" "

export IMS_FILE=$INPUT_DIR/imssnow96.grb
export AFWA_NH_FILE=$INPUT_DIR/NPR.SNWN.SP.S1200.MESH16
export AFWA_SH_FILE=" "

export OUTPUT_GRIB2=".false."

module load grib_util/1.1.0

${HOMEglobal}/ush/emcsfc_snow.sh

exit 0
