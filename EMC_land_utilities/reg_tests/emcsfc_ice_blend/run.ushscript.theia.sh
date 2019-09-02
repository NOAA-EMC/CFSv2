#!/bin/sh

#---------------------------------------------------------------
# Run the ush/emcsfc_ice_blend.sh script on Theia.
#
# Invoke as follows:
# 'qsub $script'
#---------------------------------------------------------------

#PBS -l procs=1
#PBS -l vmem=3G
#PBS -l walltime=0:04:00
#PBS -A fv3-cpu
#PBS -N ice_blend
#PBS -o log
#PBS -e log

set -x

export DATA=/scratch3/NCEPDEV/stmp1/$LOGNAME/ice_blend

export HOMEglobal=${PBS_O_WORKDIR}/../../

export WGRIB=/apps/wgrib/1.8.1.0b/bin/wgrib
export WGRIB2=/apps/wgrib2/0.1.9.5.1/bin/wgrib2
export COPYGB=/scratch4/NCEPDEV/global/save/glopara/svn/gfs/trunk/para/util/exec/copygb
export COPYGB2=/scratch4/NCEPDEV/da/save/George.Gayno/util/copygb2/copygb2
export CNVGRIB=/apps/cnvgrib/1.4.0/bin/cnvgrib

export IMS_FILE=/scratch4/NCEPDEV/global/noscrub/dump/2015032600/gdas/imssnow96.grib2.gdas.2015032600
export FIVE_MIN_ICE_FILE=/scratch4/NCEPDEV/global/noscrub/dump/2015032600/gdas/seaice.5min.grib2.gdas.2015032600

${HOMEglobal}/ush/emcsfc_ice_blend.sh

exit 0
