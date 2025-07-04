#!/bin/sh

#------------------------------------------------------------------
# Run emcsfc_snow2mdl program for the t1534 gfs grid on wcoss
# phase 1.
#
# Invoke this script as follows:  cat run.gfs.wcoss.sh | bsub
#------------------------------------------------------------------

#BSUB -oo gfs.log
#BSUB -eo gfs.log
#BSUB -q dev_shared
#BSUB -R rusage[mem=2000]
#BSUB -R affinity[core]
#BSUB -J snow2mdl
#BSUB -P GFS-T2O
#BSUB -cwd .
#BSUB -W 0:03

set -x

export DATA=/stmpp1/$LOGNAME/gfs_snow

export HOMEglobal=${LS_SUBCWD}/../..

# not needed anymore in ush script anymore
FIXmodel=/global/save/emc.glopara/svn/gfs/trunk/para/fix/fix_am
export MODEL_SLMASK_FILE=$FIXmodel/global_slmask.t1534.3072.1536.grb
export MODEL_LATITUDE_FILE=$FIXmodel/global_latitudes.t1534.3072.1536.grb
export MODEL_LONGITUDE_FILE=$FIXmodel/global_longitudes.t1534.3072.1536.grb
export GFS_LONSPERLAT_FILE=$FIXmodel/global_lonsperlat.t1534.3072.1536.txt

export IMS_FILE=/globaldump/2014071706/gdas/imssnow96.grib2.gdas.2014071706
export AFWA_NH_FILE=/globaldump/2014071706/gdas/NPR.SNWN.SP.S1200.MESH16.grb.gdas.2014071706
export AFWA_SH_FILE=/globaldump/2014071706/gdas/NPR.SNWS.SP.S1200.MESH16.grb.gdas.2014071706

export OUTPUT_GRIB2=.false.

module load grib_util

${HOMEglobal}/ush/emcsfc_snow.sh

exit 0
