#!/bin/sh

#------------------------------------------------------------------
# Run emcsfc_snow2mdl program for the t1534 gfs grid on wcoss-dell.
#
# Invoke this script as follows:  cat run.gfs.wcoss.dell.sh | bsub
#------------------------------------------------------------------

#BSUB -oo gfs.log
#BSUB -eo gfs.log
#BSUB -q debug
#BSUB -R affinity[core(1)]
#BSUB -J snow2mdl
#BSUB -P FV3GFS-T2O
#BSUB -cwd .
#BSUB -W 0:03
#BSUB -M 2000

set -x

module purge
module load EnvVars/1.0.2
module load ips/18.0.1.163

export DATA=/gpfs/dell1/stmp/$LOGNAME/gfs_snow

export HOMEglobal=${LS_SUBCWD}/../..

# not needed anymore in ush script anymore
FIXmodel=/gpfs/dell2/emc/modeling/noscrub/emc.glopara/git/fv3gfs/fix/fix_am
export MODEL_SLMASK_FILE=$FIXmodel/global_slmask.t1534.3072.1536.grb
export MODEL_LATITUDE_FILE=$FIXmodel/global_latitudes.t1534.3072.1536.grb
export MODEL_LONGITUDE_FILE=$FIXmodel/global_longitudes.t1534.3072.1536.grb
export GFS_LONSPERLAT_FILE=$FIXmodel/global_lonsperlat.t1534.3072.1536.txt

DATA_DIR=/gpfs/dell2/emc/modeling/noscrub/George.Gayno/landutil.git/snow2mdl
export IMS_FILE=$DATA_DIR/imssnow96.grb.grib2
export AFWA_NH_FILE=$DATA_DIR/NPR.SNWN.SP.S1200.MESH16
export AFWA_SH_FILE=$DATA_DIR/NPR.SNWS.SP.S1200.MESH16

export OUTPUT_GRIB2=.false.

module load grib_util/1.1.0

${HOMEglobal}/ush/emcsfc_snow.sh

exit 0
