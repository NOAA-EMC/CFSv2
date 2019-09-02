#!/bin/sh

#------------------------------------------------------------------
# Run emcsfc_snow2mdl program for the t1534 gfs grid on the
# WCOSS-cray.
#
# Invoke this script as follows:  cat run.gfs.wcoss.cray.sh | bsub
#------------------------------------------------------------------

#BSUB -oo gfs.log
#BSUB -eo gfs.log
#BSUB -q dev_shared
#BSUB -R rusage[mem=2600]
#BSUB -J snow2mdl
#BSUB -P GFS-T2O
#BSUB -cwd .
#BSUB -W 0:03

set -x

module load grib_util
module list

export DATA=/gpfs/hps/stmp/$LOGNAME/gfs_snow

export HOMEglobal=${LS_SUBCWD}/../..

if [[ -d /gpfs/td1/emc/global/save/emc.glopara/svn/gfs/trunk/para/fix/fix_am ]];then
  FIXmodel=/gpfs/td1/emc/global/save/emc.glopara/svn/gfs/trunk/para/fix/fix_am
elif [[ -d /gpfs/gd1/emc/global/save/emc.glopara/svn/gfs/trunk/para/fix/fix_am ]];then
  FIXmodel=/gpfs/gd1/emc/global/save/emc.glopara/svn/gfs/trunk/para/fix/fix_am
else
  echo "INPUT FIXED DATA DIRECTORY DOES NOT EXIST.  EXIT."
fi

export MODEL_SLMASK_FILE=$FIXmodel/global_slmask.t1534.3072.1536.grb
export MODEL_LATITUDE_FILE=$FIXmodel/global_latitudes.t1534.3072.1536.grb
export MODEL_LONGITUDE_FILE=$FIXmodel/global_longitudes.t1534.3072.1536.grb
export GFS_LONSPERLAT_FILE=$FIXmodel/global_lonsperlat.t1534.3072.1536.txt

if [[ -d /gpfs/td3/emc/globaldump/2014071706/gdas ]];then
  INPUT_DATA_DIR=/gpfs/td3/emc/globaldump/2014071706/gdas
elif [[ -d /gpfs/gd3/emc/globaldump/2014071706/gdas ]];then
  INPUT_DATA_DIR=/gpfs/gd3/emc/globaldump/2014071706/gdas
else
  echo "GLOBAL DUMP DATA DIRECTORY DOES NOT EXIST.  EXIT."
fi

export IMS_FILE=$INPUT_DATA_DIR/imssnow96.grib2.gdas.2014071706
export AFWA_NH_FILE=$INPUT_DATA_DIR/NPR.SNWN.SP.S1200.MESH16.grb.gdas.2014071706
export AFWA_SH_FILE=$INPUT_DATA_DIR/NPR.SNWS.SP.S1200.MESH16.grb.gdas.2014071706

export OUTPUT_GRIB2=.false.

${HOMEglobal}/ush/emcsfc_snow.sh

exit 0
