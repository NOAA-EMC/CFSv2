#!/bin/sh

#-----------------------------------------------------------
# Run the exemcsfc_global_sfc_prep.sh.ecf script on
# wcoss phase 1.
#
# Invoke this script as follows:
# 'cat $script | bsub'
#-----------------------------------------------------------

#BSUB -oo gl_sfc_prep.log
#BSUB -eo gl_sfc_prep.log
#BSUB -q dev_shared
#BSUB -R rusage[mem=2000]
#BSUB -R affinity[core]
#BSUB -J emcsfc
#BSUB -P GFS-T2O
#BSUB -cwd .
#BSUB -W 0:03

set -x

# work directory
export DATA="/stmpp1/$LOGNAME/emcsfc_sfc_prep"

# gdas or gfs cycle?  The gdas cycle creates an addtional snow file at the
# enkf resolution.
export RUN="gdas"

# usually /nwprod/global_shared.vX.Y.Z
export HOMEglobal=${LS_SUBCWD}/../..

# need gfs 'fixed' files
export FIXglobal_am=/nwprod/gsm.v12.0.1/fix/fix_am

# input data
export IMS_FILE=/globaldump/2014070400/gdas/imssnow96.grib2.gdas.2014070400
export FIVE_MIN_ICE_FILE=/globaldump/2014070400/gdas/seaice.5min.grib2.gdas.2014070400
export AFWA_NH_FILE=/globaldump/2014070400/gdas/NPR.SNWN.SP.S1200.MESH16.grb.gdas.2014070400
export AFWA_SH_FILE=/globaldump/2014070400/gdas/NPR.SNWS.SP.S1200.MESH16.grb.gdas.2014070400

# output snow files for current time (FNSNOA) and previous 6-hr cycle (FNSNOG)
# the old data is only used as a backup.
export FNSNOAJCAP=snow.t1534.grb
export FNSNOGJCAP=snow.t1534.old.grb
export FNSNOAJCAP_ENKF=snow.t574.grb
export FNSNOGJCAP_ENKF=snow.t574.old.grb

# output ice files for the current and previous 6-hr cycle
# the old data is only used as a backup.
export BLENDED_ICE_FILE=blended.ice.grb
export BLENDED_ICE_FILE_m6hrs=blended.ice.grb

# use to define $WGRIB, $CNVGRIB, $WGRIB2, $COPYGB, $COPYGB2
module load grib_util

${HOMEglobal}/scripts/exemcsfc_global_sfc_prep.sh.ecf

exit 0
