#!/usr/bin/env ksh
set -eux
if [ $# -lt 3 ] ; then echo "Usage:$0  date FH DO_POST [hh_inc_ocn] [TM] [mkmoc]";exit 1;fi

date=$1
FH=$2
DO_POST=$3
export hh_inc_ocn=${4:-$FHOUT}
TM=${5:-""}
export mkmoc=${6:-0}

WRK=$DATA/${FH}${TM}
mkdir -p $WRK
cd $WRK

if [ $DO_POST = YES ] ; then
  #    Post ocean files from netcdf to grib
  #
  export FIX_OCN=${FIX_OCN:-$HOMEcfs/fix/cfs_fix_om}
  $NCP $FIX_OCN/OCNINTP${omres}TO${outres}.C  OCNINTPCOEF.C
  $NCP $FIX_OCN/OCNINTP${omres}TO${outres}.T  OCNINTPCOEF.T
  ymd=$(echo $date | cut -c1-8)
  yyyy=$(echo $date | cut -c1-4)
  mm=$(echo $date | cut -c5-6)
  dd=$(echo $date | cut -c7-8)
  hh=$(echo $date | cut -c9-10)

# define the ocn and ice netcdf files

  export ocnfile=$OCNDIR/ocn_${yyyy}_${mm}_${dd}_${hh}${SUFOUT}$TM.nc
  export icefile=$OCNDIR/ice_${yyyy}_${mm}_${dd}_${hh}${SUFOUT}$TM.nc

# connect the ocnfile or err out

  if [[ ! -s $ocnfile ]]; then
     echo "$ocnfile missing"
     echo "Hourly Ocean File is not available"
     export err=1; err_chk
  fi

# connect the icefile or err out

  if [[ ! -s $icefile ]]; then
     echo "$icefile missing"
     echo "Hourly Ice File is not available"
     export err=1; err_chk
  fi

  export outfile=$DATA/ocn${SUFO}${FH}${TM}$SUFOUT
  if [ $mkmoc -eq 1 ] ; then
    export mocfile=$DATA/moc${SUFO}${FH}${TM}$SUFOUT
  fi
  output=cfs_mom4_daily_proc.${ymd}_${hh}$TM.out
  error=cfs_mom4_daily_proc.${ymd}_${hh}$TM.err
  export fh=$FH
  $OCN2GRIBEXEC 1>$output 2>$error
  rc=$?
  if [ $RUN_ENVIR = nco  -o $RUN_ENVIR = devpara ] ; then
    export err=$rc; err_chk
    $NCP $outfile $COMOUT/${RUN1}.t${cyc}z.ocngrb${SUFO}${FH}${TM}
    if [ $mkmoc -eq 1 ] ; then
      $NCP $mocfile $COMOUT/${RUN1}.t${cyc}z.moc${SUFO}${FH}${TM}
    fi
  else
    if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
    $NCP $outfile $COMOUT/
    if [ $mkmoc -eq 1 ] ; then
      $NCP $mocfile $COMOUT/
    fi
  fi
  $NCP $output $DATA/
  $NCP $error $DATA/

  if [ $imo -gt 360 ] ; then
    if [ $RUN_ENVIR = nco  -o $RUN_ENVIR = devpara ] ; then
      $COPYGB -g3 -i$in_o -x $outfile $COMOUT/${RUN1}.t${cyc}z.ocngrbf${FH}${TM}
    else
      $COPYGB -g3 -i$in_o -x $outfile $COMOUT/ocnf${FH}${TM}$SUFOUT
#     if [ $mkmoc -eq 1 ] ; then
#       $COPYGB -g3 -i$in_o -x $mocfile $COMOUT/mocf${FH}${TM}$SUFOUT
#     fi
    fi
    rc=$?
    if [ $rc -eq 0 ] ; then
       if [ $nknd -gt 1 -a $omres = 1x1 ] ; then rm $outfile ; fi
    else
      echo 'COPYGB error in ocnp rc= ' $rc 
      export err=$rc; err_chk
    fi
  fi
  if [ $GRID_ID25 -gt 0 ] ; then
    if [ $RUN_ENVIR = nco  -o $RUN_ENVIR = devpara ] ; then
      $COPYGB -g$GRID_ID25 -i$in_o -x $outfile $COMOUT/${RUN1}.t${cyc}z.ocngrbl${FH}${TM}
    else
      $COPYGB -g$GRID_ID25 -i$in_o -x $outfile $COMOUT/ocnl${FH}${TM}$SUFOUT
#     if [ $mkmoc -eq 1 ] ; then
#       $COPYGB -g$GRID_ID25 -i$in_o -x $mocfile $COMOUT/mocl${FH}${TM}$SUFOUT
#     fi
    fi
  fi
  if [ ! -z $LINKPOSTFILESH ] ; then
    if [ -s "$LINKPOSTFILESH" ] ; then
      $LINKPOSTFILESH $FH
    fi
  fi
fi          ;  # End of "REDO_POST" if block

#                     For time averaging of forecasts
if [ $AVG_FCST = YES -o $TSER_FCST = YES ] ; then
  FDATE=$($NDATE $FH $CDATE)
  if [ $AVG_INT -eq 999 ] ; then     # Monthly mean
      if [ $PDATE -gt $CDATE ] ; then
       xdate=$(echo $($NDATE 768 $PDATE) | cut -c1-6)0100
      else
       xdate=$(echo $($NDATE 768 $(echo $CDATE | cut -c1-6)0100) | cut -c1-6)0100
      fi
  else
      xdate=$(echo $($NDATE $AVG_INT $PDATE))
  fi
  if [ $FDATE -ge $xdate ] ; then
    export edate_av=$xdate
    export sdate_av=$($NDATE $FHOUT $PDATE)
    PDATE=$xdate
    if [ $EXTERNAL_AVG = NO ] ; then
      if [ $AVG_FCST = YES ] ; then
        if [ $AVG_SUB = YES ] ; then
          $AVGSUBSH $PSLOT $CDATE $sdate_av $edate_av $FHOUT $OCNDIR $OCNMEANDIR $FHBAK
        else
          $FCST_TIMEMEANSH $PSLOT $CDATE $sdate_av $edate_av $FHOUT $OCNDIR $OCNMEANDIR $DATA/$sdate_av $INDXDIR $ENS_NUM 
        fi
        rc=$?
      fi
      if [ $TSER_FCST = YES ] ; then
        if [ $TSER_SUB = YES ] ; then
          $TSERSUBSH $PSLOT $CDATE $sdate_av $edate_av $FHOUT $OCNDIR $TIMEDIR $FHBAK $DATA/tser_$sdate_av $ENS_NUM $CSTEP
        else
          $FCST_TSERSH $PSLOT $CDATE $sdate_av $edate_av $FHOUT $OCNDIR $TIMEDIR $FHBAK $DATA/tser_$sdate_av $ENS_NUM
        fi
        rc=$?
        if [[ $rc -ne 0 ]];then 
          if [ $RUN_ENVIR = nco  -o $RUN_ENVIR = devpara ]; then
            export err=$rc; err_chk
          else
            $PERR;exit 1;fi
          fi
      fi
    fi
  fi
fi
cd $DATA
#rm -rf $WRK
