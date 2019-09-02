#!/bin/sh

#--------------------------------------------------------------
# Run the J*_EMCSFC_SFC_PREP j-jobs on wcoss phase 1.
#
# Invoke as follows:
# 'cat $script | bsub'
#--------------------------------------------------------------

#BSUB -oo jjob.log
#BSUB -eo jjob.log
#BSUB -q dev_shared
#BSUB -R rusage[mem=2000]
#BSUB -R affinity[core]
#BSUB -J emcsfc
#BSUB -P GFS-T2O
#BSUB -cwd .
#BSUB -W 0:03

set -x


export cyc="00"
export job=emcsfc_sfc_prep_${cyc}
export KEEPDATA="YES"
export SENDECF="NO"
export SENDCOM="YES"
export RUN_ENVIR="nco"

export DATA="/stmpp1/$LOGNAME/tmpnwprd/${job}"
export jlogfile="/stmpp1/$LOGNAME/jlogfile"
export COMROOT="/com"
export COMOUT="/stmpp1/$LOGNAME/com"
export COMIN_m6hrs="/stmpp1/$LOGNAME/com_old"

export envir="prod"
export NWROOT="/nw${envir}"

export FIXglobal_am="${NWROOT}/gsm.v12.0.1/fix/fix_am"
export HOMEglobal="${LS_SUBCWD}/../../"

module load grib_util
module load prod_util

export jobid="LLgdas_emcsfc_sfc_prep"
${HOMEglobal}/jobs/JGDAS_EMCSFC_SFC_PREP

#export jobid="LLgfs_emcsfc_sfc_prep"
#${HOMEglobal}/jobs/JGFS_EMCSFC_SFC_PREP

exit 0
