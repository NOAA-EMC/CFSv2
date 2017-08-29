#!/bin/ksh
################################################################################
# This script runs the time averaging of forecasts.
# Usage: fcst_timemean.sh
################################################################################
#if [ $# -lt 5 ] ; then echo "Usage: $0 cdate sdate edate fhout indir outdir rundir  indxdir";exit 1 ;fi
set -ux

################################################################################
# Set other variables

export SUB=${SUB:-/u/wx20mi/bin/sub}
export BASEDIR=${BASEDIR:-$$DISK_GLOB/wx23sm/cfsrr}
export USHDIR=${USHDIR:-${SHDIR:-$BASEDIR/ush}}
export TIMEMEANSH=${TIMEMEANSH:-$USHDIR/mpi_timavg_fcst.sh}
#export TIMEMEANEXEC=${TIMEMEANEXEC:-$BASEDIR/exec/mpi_timavg_3d_fcst}
export TIMEMEANEXEC=${TIMEMEANEXEC:-$BASEDIR/exec/mpi_timavg_3d}
export COUP_FCST=${COUP_FCST:-YES}
export POSTD3D=${POSTD3D:-NO}
#
export CDUMP=${CDUMP:-gdas}
export CKSH=${CKSH:-post}
export nknd=${nknd:-1}
export FHBAK=${FHBAK:-0}
export LDIAG3D=${LDIAG3D:-.true.}
export DYCYC=${DYCYC:-YES}
export AVG_PGB=${AVG_PGB:-YES}
export AVG_FLX=${AVG_FLX:-YES}
export AVG_IPV=${AVG_IPV:-YES}
export AVG_D3D=${AVG_D3D:-YES}
#
################################################################################
export PSLOT=${1:-$PSLOT}
export CDATE=${2:-$CDATE}
export sdate=${3:-${sdate:-$CDATE}}
export edate=${4:-${edate:-$CDATE}}
export FHOUT=${5:-${FHOUT:-6}}
export INDIR=${6:-${INDIR:-$COMROT}}
export OUTDIR=${7:-${MEANDIR:-$COMROT}}
mkdir -p $OUTDIR
export RUNDIR=${8:-${RUNDIR:-${DATA:-/ptmp/$LOGNAME/pr${PSLOT}${CDATE}_${sdate}_$edate}}}
export INDXDIR=${9:-${INDXDIR:-$RUNDIR/index}}
mkdir -p $RUNDIR
mkdir -p $INDXDIR
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
echo "OUTDIR  is $OUTDIR"
echo "FHBAK   is $FHBAK"
echo "sdate   is $sdate"
echo "edate   is $edate"
echo "nknd    is $nknd"
#
#      Averaging for period from sdate to edate
#     (default : monthly mean of the previous month)
#

if [ $edate -le 0 -o $sdate -ge $edate ] ; then
 echo 'NOT A VALID AVERAGING PERIOD sdate= '$sdate ' edate= '$edate
 exit
fi
#
fhstrt=$($NHOUR $sdate $CDATE)
#if [ $fhstrt -lt 10 ] ; then fhstrt=0$fhstrt ; fi
#
export AVRG_FILE_NAME=${AVRG_FILE_NAME:-yyyymm}
if [ $AVRG_FILE_NAME = yyyymm ] ; then
 yyyy=$(echo $sdate | cut -c1-4)
 mm=$(echo $sdate | cut -c5-6)
 date_str=${yyyy}${mm}
else
 date_str=$sdate.$edate
fi
if [ $nknd -gt 1 -a $FHBAK -eq 0 ] ; then
 export SUFIN=.${CDUMP}$nknd
#export SUFOUT=.${CDUMP}$nknd.$date_str.$CDATE
else
 export SUFIN=.${CDUMP}
#export SUFOUT=.${CDUMP}.$date_str.$CDATE
fi
export SUFOUTB=$CDATE.$ENS_NUM.$date_str.avrg.grib

if [ $DYCYC = YES -a $FHOUT -lt 24 ] ; then
 ntimes=$((1+24/$FHOUT))
else
 ntimes=1
fi

nn=0
until [ $((nn+=1)) -gt $ntimes ] ; do
 if [ $nn -eq 1 ] ; then
  hint=$FHOUT
  sdate_av=$sdate
  edate_av=$edate
  export SUFOUT=$SUFOUTB
 else
  hint=24
  sdate_av=$($NDATE $(((nn-2)*FHOUT)) $sdate)
  hh=$(echo $sdate_av | cut -c9-10)
  edate_av=$edate
  export SUFOUT=$SUFOUTB.${hh}Z
 fi
 if [ $CKSH = post ] ; then

  for f in h f l ; do
#              Time average of pgb files
#              -------------------------
   if [ -s $INDIR/pgb${f}${fhstrt}$SUFIN.$CDATE -a $AVG_PGB = YES ] ; then
    $TIMEMEANSH $CDATE $sdate_av $edate_av $hint pgb  $f
   fi
#               Time average of flx files
#               -------------------------
   if [ -s $INDIR/flx${f}${fhstrt}$SUFIN.$CDATE -a $AVG_FLX = YES ] ; then
    $TIMEMEANSH $CDATE $sdate_av $edate_av $hint flx  $f
   fi
#               Time average of ipv files
#               -------------------------
   if [ -s $INDIR/ipv${f}${fhstrt}$SUFIN.$CDATE -a $AVG_IPV = YES ] ; then
    $TIMEMEANSH $CDATE $sdate_av $edate_av $hint ipv  $f
   fi
#               Time average of d3d files
#               -------------------------
   if [ $LDIAG3D = .true. -a $POSTD3D = YES -a $AVG_D3D = YES ] ; then
    if [ -s $INDIR/d3d${f}${fhstrt}$SUFIN.$CDATE ] ; then
     $TIMEMEANSH $CDATE $sdate_av $edate_av $hint diab $f
    fi
   fi
  done
 elif [ $CKSH = d3dp ] ; then
  for f in h f l ; do
    if [ -s $INDIR/d3d${f}${fhstrt}$SUFIN.$CDATE ] ; then
     $TIMEMEANSH $CDATE $sdate_av $edate_av $hint diab $f
    fi
  done
 elif [ $CKSH = ocnp -a $COUP_FCST = YES ] ; then
  for f in h f l ; do
#               Time average of ocean files
#               -------------------------
    if [ -s $INDIR/ocn${f}${fhstrt}$SUFIN.$CDATE ] ; then
     $TIMEMEANSH $CDATE $sdate_av $edate_av $hint ocn  $f
    fi
  done
 fi
done

################################################################################# Exit gracefully

#
rc=0
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
