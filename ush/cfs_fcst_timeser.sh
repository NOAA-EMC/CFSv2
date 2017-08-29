#!/bin/ksh
################################################################################
# This script extracts time series of selected variables from the forecasts.
# Usage: fcst_timeser.sh
################################################################################
#if [ $# -lt 5 ] ; then echo "Usage: $0 cdate sdate edate fhout indir outdir rundir  indxdir";exit 1 ;fi
set -ux

################################################################################
# Set other variables

export SUB=${SUB:-/u/wx20mi/bin/sub}
export BASEDIR=${BASEDIR:-$$DISK_GLOB/wx23sm/cfsrr}
export USHDIR=${USHDIR:-${SHDIR:-$BASEDIR/ush}}
export GLBTSERSH=${GLBTSERSH:-$USHDIR/tser_fcst/glbtseries.sh}
export COUP_FCST=${COUP_FCST:-YES}
export POSTD3D=${POSTD3D:-NO}
#
export CDUMP=${CDUMP:-gdas}
export CKSH=${CKSH:-post}
export nknd=${nknd:-1}
export FHBAK=${FHBAK:-0}
export LDIAG3D=${LDIAG3D:-.true.}
export DYCYC=${DYCYC:-YES}
export TSER_PGB=${TSER_PGB:-YES}
export TSER_FLX=${TSER_FLX:-YES}
export TSER_IPV=${TSER_IPV:-YES}
export TSER_D3D=${TSER_D3D:-NO}
export TSER_OCN=${TSER_OCN:-YES}
#
################################################################################
export PSLOT=${1:-$PSLOT}
export CDATE=${2:-$CDATE}
export sdate=${3:-${sdate:-$CDATE}}
export edate=${4:-${edate:-$CDATE}}
export FHOUT=${5:-${FHOUT:-6}}
export INDIR=${6:-${INDIR:-$COMROT}}
export TIMEDIR=${7:-${TIMEDIR:-$COMROT}}
mkdir -p $TIMEDIR
export FHBAK=${8:-${FHBAK:-0}}
export RUNDIR=${9:-${RUNDIR:-${DATA:-/ptmp/$LOGNAME/pr${PSLOT}${CDATE}_${sdate}_$edate}}}
mkdir -p $RUNDIR
export ENS_NUM=${10:-${ENS_NUM:-1}}
if [ $ENS_NUM -lt 10 ] ; then ENS_NUM=0$ENS_NUM ; fi
#
#
echo "BASEDIR is $BASEDIR"
echo "CDATE   is $CDATE"
echo "PSLOT   is $PSLOT"
echo "CDUMP   is $CDUMP"
echo "CKSH    is $CKSH"
echo "INDIR   is $INDIR"
echo "COMROT  is $COMROT"
echo "TIMEDIR  is $TIMEDIR"
echo "FHBAK   is $FHBAK"
echo "sdate   is $sdate"
echo "edate   is $edate"
echo "nknd    is $nknd"
#
#

if [ $edate -le 0 -o $sdate -ge $edate ] ; then
 echo 'NOT A VALID  PERIOD sdate= '$sdate ' edate= '$edate
 exit
fi
#
fhstrt=$($NHOUR $sdate $CDATE)
#
x0=$(echo $CDATE | cut -c7-10)
x1=$(echo $($NDATE -$FHOUT $sdate) | cut -c7-10)
x2=$(echo $($NDATE  $FHOUT $edate) | cut -c7-10)
if [ $x1 = $x2 -o $x0 = $x1 ] ; then
 yyyy=$(echo $sdate | cut -c1-4)
 mm=$(echo $sdate | cut -c5-6)
 date_str=${yyyy}${mm}
else
 date_str=$sdate.$edate
fi
if [ $nknd -gt 1 -a $FHBAK -eq 0 ] ; then
 export SUFIN=.${CDUMP}$nknd
else
 export SUFIN=.${CDUMP}
fi
export SUFOUT=${SUFOUT_TIME:-.$ENS_NUM.$date_str.$CDATE.grib}

FILES_FOR_TSER=${FILES_FOR_TSER:-'f'}

if [ $CKSH = post ] ; then

  for f in $FILES_FOR_TSER ; do
#              Time series from pgb files
#              -------------------------
    if [ -s $INDIR/pgb${f}${fhstrt}$SUFIN.$CDATE -a $TSER_PGB = YES ] ; then
      $GLBTSERSH $CDATE pgb$f $CDUMP $sdate $FHOUT $edate $INDIR $TIMEDIR
    fi
#               Time series from flx files
#               -------------------------
    if [ -s $INDIR/flx${f}${fhstrt}$SUFIN.$CDATE -a $TSER_FLX = YES ] ; then
      $GLBTSERSH $CDATE flx$f $CDUMP $sdate $FHOUT $edate $INDIR $TIMEDIR
    fi
#               Time series from ipv files
#               -------------------------
    if [ -s $INDIR/ipv${f}${fhstrt}$SUFIN.$CDATE -a $TSER_IPV = YES ] ; then
      $GLBTSERSH $CDATE ipv$f $CDUMP $sdate $FHOUT $edate $INDIR $TIMEDIR
    fi
#               Time series from d3d files
#               -------------------------
    if [ $LDIAG3D = .true. -a $POSTD3D = YES -a $TSER_D3D = YES ] ; then
      if [ -s $INDIR/d3d${f}${fhstrt}$SUFIN.$CDATE ] ; then
        $GLBTSERSH $CDATE d3d$f $CDUMP $sdate $FHOUT $edate $INDIR $TIMEDIR
      fi
    fi
  done
elif [ $CKSH = d3dp ] ; then
  for f in $FILES_FOR_TSER ; do
    if [ -s $INDIR/d3d${f}${fhstrt}$SUFIN.$CDATE -a $TSER_D3D = YES ] ; then
      $GLBTSERSH $CDATE  d3d$f $CDUMP $sdate $FHOUT $edate $INDIR $TIMEDIR
    fi
  done
elif [ $CKSH = ocnp -a $COUP_FCST = YES ] ; then
  for f in $FILES_FOR_TSER ; do
#               Time series of ocean files
#               -------------------------
    if [ -s $INDIR/ocn${f}${fhstrt}$SUFIN.$CDATE -a $TSER_OCN = YES ] ; then
      $GLBTSERSH $CDATE  ocn$f $CDUMP $sdate $FHOUT $edate $INDIR $TIMEDIR
    fi
  done
fi

################################################################################# Exit gracefully

#
rc=0
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
