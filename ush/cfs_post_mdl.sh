#!/bin/ksh
################################################################################
# This script runs the post processor.
# Usage: post.sh
# Imported variables:
#   CDATE
#   CDUMP
# Configuration variables:
################################################################################
set -ux

################################################################################
# Go configure

export PERR=${PERR:-""}

################################################################################
# Set other variables

export NCP=${NCP:-cp}
export in_o=${in_o:-0}       # interpolation option, defaults to 0 (bilinear)

export POSTGPSH=${POSTGPSH:-$HOMEcfs/ush/cfs_cdas_nceppost.sh}
export PARM_AM=${PARM_AM:-$HOMEcfs/parm/cfs_parm_am}
export SIGHDR=${SIGHDR:-$HOMEgsm/exec/global_sighdr}
export OUTTYP=${OUTTYP:-3} 
export OROGRAPHY=NULL
#
export GRID_ID62=${GRID_ID62:-2}
export IGEN_ANL=${IGEN_ANL:-98}
export IGEN_FCST=${IGEN_FCST:-98}
export VERBOSE=YES
#
export CTLFILE=${CTL_MDL:-$PARM_AM/am_cntrl.parm_mdl}
#
#      Post the input sigma  file
#
export prefix=${1:-${prefix:-anl}}
export SUFOUT=${2:-${SUFOUT:-.$CDUMP.$CDATE}}
export SIGINP=${SIGINP:-$COM_YMDH/sig${prefix}$SUFOUT}
export FLXINP=/dev/null
export PGBOUT=$COMOUT/spl${prefix}$SUFOUT
export IGEN=$IGEN_ANL
export IDRT=${IDRT_SPL:-4}
export LONB=${LONB_SPL:-$(echo lonb|$SIGHDR ${SIGINP})}
export LATB=${LATB_SPL:-$(echo latb|$SIGHDR ${SIGINP})}
export GENPSICHI=NO
rm $PGBOUT

$POSTGPSH

rc=$?
if [[ $rc -ne 0 ]];then $PERR;err_exit $rc;fi
if [ $GRID_ID62 -gt 0 ] ; then
  prefix2=l$(echo $prefix | cut -c2-3)
  $COPYGB -g$GRID_ID62 -i$in_o -x $PGBOUT $COMOUT/spl${prefix2}${SUFOUT}
fi
#
################################################################################
# Exit gracefully

rc=$?
if [[ $rc -ne 0 ]];then $PERR;err_exit $rc;fi
exit
#
