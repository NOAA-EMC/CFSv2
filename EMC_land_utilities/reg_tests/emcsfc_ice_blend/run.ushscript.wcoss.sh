#!/bin/sh

#--------------------------------------------------------------------------
# Run the emcsfc_ice_blend program on the WCOSS phase 1 nodes.
#
# Invoke as follows: "cat run.ushscript.wcoss.sh | bsub"
#--------------------------------------------------------------------------

#BSUB -oo ice_blend.log
#BSUB -eo ice_blend.log
#BSUB -q dev_shared
#BSUB -R rusage[mem=2000]
#BSUB -R affinity[core(1)]
#BSUB -J ice_blend
#BSUB -P GFS-T2O
#BSUB -cwd .
#BSUB -W 0:03

set -x

export DATA=/stmpp1/$LOGNAME/ice_blend

export HOMEglobal=${LS_SUBCWD}/../../

export IMS_FILE=/globaldump/2014071706/gdas/imssnow96.grib2.gdas.2014071706
export FIVE_MIN_ICE_FILE=/globaldump/2014071706/gdas/seaice.5min.grib2.gdas.2014071706

module load grib_util

${HOMEglobal}/ush/emcsfc_ice_blend.sh

exit 0
