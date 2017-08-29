#!/usr/bin/env bash
###############################################################
# 
# script:  cfs_ocn.sh
#
#   This script creates MOM4 ocean grib files from the CFS forecasts
#   using the utility created by Xingren Wu.
#   This script created by shrinivas Moorthi on 02/18/2004
#   Updated for cfsv2 by   shrinivas Moorthi on 03/19/2010
#   based on postmon64
#
# ocn
#
#  $1 ... Coupled run starting date
#  $2 ... Mom4 file processing begining date
#  $3 ... Mom4 file processing ending date
#
#  This script converts the MOM4 output file time-mean.yyyymmdd.nc
#  to daily 2x1 grib files from starting date to ending date.
#  It also outputs eta_daily files on 2.5x2 grid
#

set -eu

echo executing $0 $*

[[ $# -ne 3 ]] && err_exit "`date` $0: argument error" 

start_date=$1
bdate=$2
edate=$3

YMDH=$start_date
momoutdir=$DATA/MOM4_$YMDH/$bdate
mkdir -p $momoutdir
cd $momoutdir

export pgm=$(basename $0) 

##. prep_step ## skip these for mpmd scripts
##startmsg    ## skip these for mpmd scripts

export BASEDIR=${BASEDIR:-$HOMEcfs}
export cfss=${cfss:-"/cfs"}
export cfsp=${cfsp:-cfs_}
export NCP=${NCP:-/bin/cp}
#export OCN2GRIBEXEC=${OCN2GRIBEXEC:-$EXECcfs/${cfsp}tripo2reg}
#export FIX_OM=${FIX_OM:-$BASEDIR/fix/${cfsp}_fix_om/}
export in_o=${in_o:-0}       # interpolation option, defaults to 0 (bilinear)

export OCNDIR=${OCNDIR:-$COM_YMDH}
export COMOUT=${COMOUT:-$COM_YMDH_DG}
export OCN_POST_DIR=${OCN_POST_DIR:-$COMOUT}
export IGEN_FCST=${IGEN_FCST:-98}
export IGEN_OCNP=${IGEN_OCNP:-98}
export VERBOSE=YES
export SUFOUT=${SUFOUT:-""}
export NCP=${NCP:-/bin/cp}

im_ocn=720
jm_ocn=360
flatn=89.75
flats=-89.75
flonw=0.25
flone=359.75

km_mom4=${km_mom4:-40}
im_mom4=720
jm_mom4=410
jmtp_mom4=50
imos=160

export year=$(echo $CDATE | cut -c1-4)
export month=$(echo $CDATE | cut -c5-6)
export day=$(echo $CDATE | cut -c7-8)
export hour=$(echo $CDATE | cut -c9-10)

export idbug=${idbug:-1}
export im=${im_mom4:-360}
export jm=${jm_mom4:-231}
export km=${km_mom4:-40}
export jmtp=${jmtp_mom4:-25}
export imos=${imos:-80}
export imo=${im_ocn:-360}
export jmo=${jm_ocn:-180}
export jmtpo=${jmtpo:-$((jmtp*imo/im))}
export flatn=${flatn:-89.5}
export flats=${flats:--89.5}
export flonw=${flonw:-0.5}
export flone=${flone:-359.5}

if [ $imo -gt 360 ] ; then
 SUFO=h
else
 SUFO=f
fi

ln -sf $FIX_OM/OCNINTP05TO05.C  OCNINTPCOEF.C
ln -sf $FIX_OM/OCNINTP05TO05.T  OCNINTPCOEF.T

export hh_inc_ocn=${hh_inc_ocn:-$FHOUT}
export TM=${TM:-""}
export mkmoc=${mkmoc:-0}
export mfcstcpl=${mfcstcpl:-1}

#date=$($NDATE $FHCYC $bdate)
#date=$bdate
#date=$($NDATE $hh_inc_ocn $bdate)
date=$bdate
until [[ $date -gt $edate ]] ; do
  FH=$($NHOUR $date $CDATE)
  FH=$((FH+0))
  if [ $FH -lt 10 ] ; then FH=0$FH ; fi
  ymd=`echo $date | cut -c1-8`
  yyyy=`echo $date | cut -c1-4`
  mm=`echo $date | cut -c5-6`
  dd=`echo $date | cut -c7-8`
  hh=`echo $date | cut -c9-10`

  export ocnfile=$OCNDIR/ocn_${yyyy}_${mm}_${dd}_${hh}${SUFOUT}$TM.nc
  if [ ! -s $ocnfile ] ; then 
    echo "$ocnfile is not available"
    export err=1; err_chk
    $PERR;exit 1
  fi
  export icefile=$OCNDIR/ice_${yyyy}_${mm}_${dd}_${hh}${SUFOUT}$TM.nc
  if [ ! -s $icefile ] ; then 
    echo "$icefile is not available"
    export err=1; err_chk
    $PERR;exit 1
  fi
  export outfile=$OCN_POST_DIR/ocn${SUFO}${FH}${TM}$SUFOUT
  if [ $mkmoc -eq 1 ] ; then
    export mocfile=$$OCN_POST_DIR/moc${SUFO}${FH}${TM}$SUFOUT
  fi
  if [ ! -s $COMOUT/ocn${SUFO}${FH}$SUFOUT -o ${REDO_OCN:-YES} = YES ] ; then
    output=cfs_mom4_daily_proc.${ymd}_${hh}$TM.out
    error=cfs_mom4_daily_proc.${ymd}_${hh}$TM.err
    export fh=$FH
    $OCN2GRIBEXEC 1>$output 2>$error
    rc=$?
    export err=$rc; err_chk
    if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
#   $NCP $outfile $COMOUT/
    if [ $mkmoc -eq 1 ] ; then
      $NCP $mocfile $COMOUT/
    fi

  fi
  if [ $imo -gt 360 ] ; then
    if [ ! -s $COMOUT/ocnf${FH}$SUFOUT -o ${REDO_OCNCPGB:-YES} = YES ] ; then
      $COPYGB -g3 -i$in_o -x $outfile $COMOUT/ocnf${FH}${TM}$SUFOUT
      rc=$?
    fi
  fi

  rc=$?
  if [[ $rc -ne 0 ]];then
    if [ $RUN_ENVIR = dev ]; then
      $PERR;exit 1
    else
      export err=$rc; err_chk
    fi
  fi

  if [ $SENDCOM = YES ] ; then
    for file in $FILES_TO_SEND_COM ; do
      if [ $file = ocn ] ; then
        current_date=$($NDATE $FH $YMDH)
        if [ ! -s $COM_YMDH_DG/ocnh${current_date}$SUFOUT ] ; then
          ln -fs $COM_YMDH_DG/ocnh${FH}$SUFOUT $COM_YMDH_DG/ocnh${current_date}$SUFOUT
        fi
        if [ ! -s $COM_YMDH_DG/ocnf${current_date}$SUFOUT ] ; then
          ln -fs $COM_YMDH_DG/ocnf${FH}$SUFOUT $COM_YMDH_DG/ocnf${current_date}$SUFOUT
        fi
      fi
    done
  fi
  date=$($NDATE $FHOUT $date)
  FH=$((FH+FHOUT))
# date=$($NDATE $FHCYC $date)
done
#
#
export err=$?; err_chk

