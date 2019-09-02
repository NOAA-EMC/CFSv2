#!/bin/sh

#--------------------------------------------------------------------------
# Run the emcsfc_ice_blend program on the WCOSS Dell.
#
# Invoke as follows: "cat $script | bsub"
#--------------------------------------------------------------------------

#BSUB -oo ice_blend.log
#BSUB -eo ice_blend.log
#BSUB -q debug
#BSUB -R affinity[core(1)]
#BSUB -J ice_blend
#BSUB -P GFS-T2O
#BSUB -cwd .
#BSUB -W 0:03
#BSUB -M 2000

set -x

module purge
module load EnvVars/1.0.2
module load ips/18.0.1.163
module load grib_util/1.1.0

export DATA=/gpfs/dell1/stmp/$LOGNAME/ice_blend

export HOMEglobal=${LS_SUBCWD}/../../

date10=2019011206

if [[ -d /gpfs/gp1/emc/globaldump ]]; then
  data_dir=/gpfs/gp1/emc/globaldump/$date10/gdas
else
  data_dir=/gpfs/tp1/emc/globaldump/$date10/gdas
fi

export IMS_FILE=$data_dir/imssnow96.grib2.gdas.$date10
export FIVE_MIN_ICE_FILE=$data_dir/seaice.5min.grib2.gdas.$date10

${HOMEglobal}/ush/emcsfc_ice_blend.sh

exit 0
