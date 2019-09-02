#!/bin/sh

#--------------------------------------------------------------------------
# Run the emcsfc_ice_blend program on the WCOSS-cray machine.
#
# Invoke as follows: "cat run.ushscript.wcoss.cray.sh | bsub"
#--------------------------------------------------------------------------

#BSUB -oo ice_blend.log
#BSUB -eo ice_blend.log
#BSUB -q dev_shared
#BSUB -R rusage[mem=2000]
#BSUB -J ice_blend
#BSUB -P GFS-T2O
#BSUB -cwd .
#BSUB -W 0:03

set -x

module load grib_util
module list

export DATA=/gpfs/hps/stmp/$LOGNAME/ice_blend

export HOMEglobal=${LS_SUBCWD}/../../

if [[ -d /gpfs/td3/emc/globaldump/ ]];then
  export IMS_FILE=/gpfs/td3/emc/globaldump/2014071706/gdas/imssnow96.grib2.gdas.2014071706
  export FIVE_MIN_ICE_FILE=/gpfs/td3/emc/globaldump/2014071706/gdas/seaice.5min.grib2.gdas.2014071706
elif [[ -d /gpfs/gd3/emc/globaldump/ ]];then
  export IMS_FILE=/gpfs/gd3/emc/globaldump/2014071706/gdas/imssnow96.grib2.gdas.2014071706
  export FIVE_MIN_ICE_FILE=/gpfs/gd3/emc/globaldump/2014071706/gdas/seaice.5min.grib2.gdas.2014071706
else
  echo CANT FIND INPUT DATA DIRECTORY
  exit 8
fi

${HOMEglobal}/ush/emcsfc_ice_blend.sh

exit 0
