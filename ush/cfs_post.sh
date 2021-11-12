#!/bin/ksh
#
#  This script template requires following edit variables to be defined
#      %EXECSUBDIR% %YYYY% %MM% %DD% %HH% %FHE% %FHS% %INTSFCX% 
#      %ENDHOUR% %INTFLX% %INTSIG% %PLATFORM% %FTPDIR%
#      %YYYYS% %MMS% %DDS% %HHS% %YYYYE% %MME% %DDE% %HHE%
#      %FTP_SIG% %FTP_SFC% %FTP_ZNL% %FTP_FLX% %FTP_DIA% %FTP_KEN% 
#      %FTP_PGB% %FTP_SGB% %FTP_SFCANL% %FTP_SFCOUT% %FTP_FCSTOUT%
#      %LIST_OF_FILES% %FILES_TO_BE_KEPT% %FILES_TO_BE_AVERAGED%
#      %RMAVRG% %VARIANCE%
#
#     Created by Shrinivas Moorthi - March 2010
#
echo executing $0

set -u

export cfs_endfhrs_sh=${cfs_endfhrs_sh:-$USHcfs/cfs_endfhrs.sh}
export cfs_pgbscript=${cfs_pgbscript:-$USHcfs/cfs_pgb.sh}
export cfs_ocnscript=${cfs_ocnscript:-$USHcfs/cfs_ocn.sh}
export cfs_timeavgsh=${cfs_timeavgsh:-$USHcfs/cfs_timavg_fcst.sh}

FILES_FOR_TSER_AM=${FILES_FOR_TSER_AM:-'f'}
FILES_FOR_TSER_OM=${FILES_FOR_TSER_OM:-'h'}
TSER_PGB=${TSER_PGB:-YES}
TSER_FLX=${TSER_FLX:-YES}
TSER_IPV=${TSER_IPV:-YES}
TSER_OCN=${TSER_OCN:-YES}
POSTD3D=${POSTD3D:-NO}
TSER_D3D=${TSER_D3D:-NO}
LDIAG3D=${LDIAG3D:-.false.}
AVG_GRB=${AVG_GRB:-YES}
COUP_FCST=${COUP_FCST:-YES}
TIMEDIR=${TIMEDIR:-$COM_YMDH_DG}
export SUFIN=${SUFIN:-${SUFOUT:-""}}
mkdir -p $TIMEDIR
#
#  Obtain the member from input:
member=${1:-01}
member=$((member+0))
if [ $member -lt 10 ]; then member=0$member; fi

export YMDH=$start_date
HHS=$(echo $start_date | cut -c9-10)
#
#  obtain forecast hours for various ending dates
#
. $cfs_endfhrs_sh
#
if [ $FHS -lt 1 ] ; then
  fhsp=$FHS
else
  fhsp=$((FHS+INTSIG))
fi
#
#
echo $FILES_TO_BE_KEPT | grep pgb >/dev/null
cck=$?
echo $FILES_TO_BE_AVERAGED | grep pgb >/dev/null
cca=$?

if [ $cck -eq 0 -o $cca -eq 0 ] ; then
   $cfs_pgbscript $fhsp $FHE $INTSIG $start_date 
   rc=$?

#  define bdate and edate
   bdate=$($NDATE $((FHS+INTSIG)) $start_date)
   edate=$($NDATE $FHE $start_date)
                                                      
#  run mpmd ocean post in parallel from bdate to cdate

   set +x

   cdate=$bdate; rm -f cfpfile; nprocs=24
   while [[ $cdate -le $edate ]]; do
   echo "$cfs_ocnscript $start_date $cdate $cdate" >> cfpfile
   cdate=$($NDATE $INTSIG $cdate)
   done

   echo $cfprun
   $cfprun cfpfile |grep 'CFP RANK'
   export err=$?; err_chk
   
#  Collect time series of selected variables

  fhend=$($NHOUR $edate $start_date)
  if [ $fhend -lt 10 ] ; then fhend=0$fhend ; fi
  for f in $FILES_FOR_TSER_AM ; do
#              Time series from pgb files
#              -------------------------
    if [ -s $COM_YMDH_DG/pgb${f}${fhend}$SUFIN -a $TSER_PGB = YES ] ; then
      $cfs_glbtsersh $start_date pgb$f $bdate $INTFLX $edate $COM_YMDH_DG $TIMEDIR $f
#     ((rc+=$?))
    fi
#               Time series from flx files
#               -------------------------
    if [ -s $COM_YMDH/flx${f}${fhend}$SUFIN -a $TSER_FLX = YES ] ; then
      $cfs_glbtsersh $start_date flx$f $bdate $INTFLX $edate $COM_YMDH $TIMEDIR $f
#     ((rc+=$?))
    fi
#               Time series from ipv files
#               -------------------------
    if [ -s $COM_YMDH_DG/ipv${f}${fhend}$SUFIN -a $TSER_IPV = YES ] ; then
      $cfs_glbtsersh $start_date ipv$f $bdate $INTFLX $edate $COM_YMDH_DG $TIMEDIR $f
#     ((rc+=$?))
    fi
#               Time series from d3d files
#               -------------------------
    if [ $LDIAG3D = .true. -a $POSTD3D = YES -a $TSER_D3D = YES ] ; then
      if [ -s $COM_YMDH/d3d${f}${fhend}$SUFIN ] ; then
        $cfs_glbtsersh $start_date d3d$f $bdate $INTFLX $edate $COM_YMDH $TIMEDIR $f
#     ((rc+=$?))
      fi
    fi
  done
  if [ $COUP_FCST = YES ] ; then
    for f in $FILES_FOR_TSER_OM ; do
#               Time series of ocean files
#               -------------------------
      if [ -s $COM_YMDH_DG/ocn${f}${fhend}$SUFIN -a $TSER_OCN = YES ] ; then
       $cfs_glbtsersh $start_date ocn$f $bdate $INTFLX $edate $COM_YMDH_DG $TIMEDIR $f
#      ((rc+=$?))
      fi
    done
  fi

   ((rc+=$?))
  echo 'rc after glbtsersh rc= '$rc
fi
#
#  Compute monthly mean if a month is available
#
if [ $MCOUNT -gt 0 ] ; then
  c=1
  while [ $c -le $MCOUNT ] ; do
          x=\$MFHOUR_$c
          x=`eval echo $x`
          echo MFHOUR_$c=$x
          c=$((c+1))
  done
#
  if [ ! -s $POSTDEFS/monthly.stamp ] ; then
#
# skip first month for averaging if the number of days < maxlen
#
    c=1
#
    flen=$((MFHOUR_1/24))
#   maxlen=15
    maxlen=1
    if [ $flen -lt $maxlen ]; then c=2; fi
#
  else
    read c <$POSTDEFS/monthly.stamp
    c=$((c+1))
  fi
#
  echo 'doing monthly average for c= '$c ' MCOUNT= '$MCOUNT
#
  while [ $c -le $MCOUNT ] ; do
     fte=\$MFHOUR_$c
     fte=`eval echo $fte`
     if [ $fte -gt 0 -a $fte -le $FHE ] ; then
        echo 'Performing averaging'
        cm=$((c-1))
        if [ $cm -eq 0 ] ; then
          fts=0
        else
          fts=\$MFHOUR_$cm
          fts=`eval echo $fts`
        fi
        NEWDATE=$($NDATE $fts $start_date)
        yyyy=$(echo $NEWDATE | cut -c-4)
        mm=$(echo $NEWDATE | cut -c5-6)
        dd=$(echo $NEWDATE | cut -c7-8)
        hh=$(echo $NEWDATE | cut -c9-10)
#
        sdate=$($NDATE $((fts+INTFLX)) $start_date)
        edate=$($NDATE $fte $start_date)

        export AVRG_FILE_NAME=${AVRG_FILE_NAME:-yyyymm}
        if [ $AVRG_FILE_NAME = yyyymm ] ; then
          date_str=${yyyy}${mm}
        else
          date_str=$sdate.$edate
        fi
        export SUFIN=${SUFIN:-.$member.$start_date}
        export SUFOUTB=$SUFIN.$date_str.avrg.grib

        DYCYC=${DYCYC:-YES}
        if [ $DYCYC = YES -a $INTFLX -lt 24 ] ; then
          ntimes=$((1+24/INTFLX))
        else
          ntimes=1
        fi
        nn=0
        until [[ $((nn+=1)) -gt $ntimes ]] ; do
          if [ $nn -eq 1 ] ; then
            hint=$INTFLX
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
#
          for file in $FILES_TO_BE_AVERAGED ; do
             if [ $file = flx -o $file = d3d ] ; then
               export datadir=$COM_YMDH
             else
               export datadir=$COM_YMDH_DG
             fi
             for f in h f l ; do
               if [ -s $datadir/${file}${f}${fte}$SUFIN -a $AVG_GRB = YES ] ; then
                $cfs_timeavgsh $start_date $sdate_av $edate_av $hint $file $f $datadir $COM_YMDH_MG $member
                ((rc+=$?))
               fi
               echo 'rca= '$rc 'file='$file
             done

          done                    # end of file loop
        done                      # end of  nn loop
        if [[ $rc -eq 0 ]] ; then
         echo $c >$POSTDEFS/monthly.stamp
        fi
     fi
     c=$((c+1))
  done
fi

echo $0 finished rc=$rc
export err=$rc; err_chk

