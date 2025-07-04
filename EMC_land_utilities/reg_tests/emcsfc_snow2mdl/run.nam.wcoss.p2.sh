#!/bin/sh

#-------------------------------------------------------------------
# Run snow2mdl program for NAM grid on WCOSS phase 2 nodes.
#
# Inoke as follows: "cat run.nam.wcoss.p2.sh | bsub"
#-------------------------------------------------------------------

#BSUB -oo nam.log
#BSUB -eo nam.log
#BSUB -q dev2_shared
#BSUB -R rusage[mem=2000]
#BSUB -R affinity[core(1)]
#BSUB -J snow2mdl
#BSUB -P NAM-T2O
#BSUB -cwd .
#BSUB -W 0:03

set -x

export DATA=/stmpp2/$LOGNAME/nam_snow

export HOMEglobal=${LS_SUBCWD}/../..

export MODEL_LATITUDE_FILE=/global/noscrub/George.Gayno/snow2mdl/nam_hpnt_latitudes.grb
export MODEL_LONGITUDE_FILE=/global/noscrub/George.Gayno/snow2mdl/nam_hpnt_longitudes.grb
export MODEL_SLMASK_FILE=/global/noscrub/George.Gayno/snow2mdl/nam_slmask.grb
export GFS_LONSPERLAT_FILE=" "

export IMS_FILE=/global/noscrub/George.Gayno/snow2mdl/imssnow96.grb
export AFWA_NH_FILE=/global/noscrub/George.Gayno/snow2mdl/NPR.SNWN.SP.S1200.MESH16
export AFWA_SH_FILE=" "

export OUTPUT_GRIB2=".false."

module load grib_util

${HOMEglobal}/ush/emcsfc_snow.sh

exit 0
