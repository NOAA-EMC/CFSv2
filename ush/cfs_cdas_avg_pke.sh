#!/bin/ksh
set -x

export machine=${machine:-WCOSS}
######################################################################
#   This script creates pke (energetics) monthly means of grib files.
#
#       Originally written by Shrinivas Moorthi
#       Updated by Patrick Tripp Sept 2010
######################################################################
export APRUN=${APRUN:-mpirun.lsf}
export CDUMP=${CDUMP:-"gdas"}

errs=0

if [ $# -lt 5 ] ; then echo "Usage: $0  sdate edate dhour prefix1 prefix2 IO JO KO polist fhini fhmax fhout ";err_exit 1 ;fi

WINDEX=${WINDEX:-$GRBINDEX}
OVERDATE=${OVERDATE:-$HOMEcfs/ush/cfs_overdate_grib.sh}

polist_37="1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,700.,650.,600.,550.,500.,450.,400.,350.,300.,250.,225.,200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,7.,5.,3.,2.,1."

export sdate=${1:-$sdate}
export edate=${2:-$edate}
export dhour=${3:-${dhour:-24}}
export prefix1=${4:-egy}
export prefix2=${5:-f}
export IO=${6:-${IO:-144}}
export JO=${7:-${JO:-73}}
export KO=${8:-${KO:-37}}
export polist=${9:-${polist:-polist_37}}
export fhini=${10:-00}
export fhmax=${11:-120}
export fhout=${12:-6}

export idbug=1
export INDIR=${INDIR:-$COMROT}
export INDXDIR=${INDXDIR:-$RUNDIR}
export OUTDIR=${OUTDIR:-$MONTHDIR}
export SUFOUT=${SUFOUT:-.${CDUMP}.$1:$2.avg}
REMOVE_INDEX=${REMOVE_INDEX:-NO}

mkdir -p $OUTDIR
export RUNDIR=${RUNDIR:-/stmp/$LOGNAME/cfs_cdas_avg_pke_$sdate}
mkdir -p $RUNDIR

cd $RUNDIR

export idim=144
export jdim=73

export syear=`echo $sdate | cut -c1-4`
export smonth=`echo $sdate | cut -c5-6`
export sday=`echo $sdate | cut -c7-8`
export shour=`echo $sdate | cut -c9-10`

export eyear=`echo $edate | cut -c1-4`
export emonth=`echo $edate | cut -c5-6`
export eday=`echo $edate | cut -c7-8`
export ehour=`echo $edate | cut -c9-10`

cmdfile=$RUNDIR/cmdfile
if [ $prefix2 = f -o $prefix2 = h -o $prefix2 = l ] ; then
 ((fhr=fhini-fhout)); while [ $((fhr=10#$fhr+10#$fhout)) -le $fhmax ] ; do

  [ ${#fhr} -lt 2 ] && fhr=0$fhr

  export iname=pgb${prefix2}$fhr
  export index=pgi${prefix2}$fhr
  export oname=${prefix1}${prefix2}$fhr

  # compute index files if they don't exits
  cdate=$sdate
  if [ -s $cmdfile ] ; then rm $cmdfile ; fi
  > $cmdfile
  while [ $cdate -le $edate ] ; do
    pgb=$INDIR/$iname.$CDUMP.$cdate
    [ ! -s $pgb ] && exit 1
    pgi=$INDXDIR/$index.$CDUMP.$cdate
    if [ $REMOVE_INDEX = YES ] ; then /bin/rm $pgi ; fi
    if [ ! -s $pgi ] ; then
	echo $WINDEX $pgb $pgi >>$cmdfile
    fi
    cdate=$($NDATE $dhour $cdate)
  done
  if [ -s $cmdfile ] ; then
   cat $cmdfile
      if [ $machine = IBM ]; then
        ntasks=$(echo $LOADL_PROCESSOR_LIST|wc -w)
        # only valid if count .le. 128
        [ $ntasks -eq 128 ] && ntasks=$(poe hostname|wc -l)
      elif [ $machine = WCOSS ]; then
        if [ -n "$LSB_PJL_TASK_GEOMETRY" ]; then
          ntasks=`echo $LSB_PJL_TASK_GEOMETRY | sed 's/[{}(),]/ /g' | wc -w`
        elif [ -n "$LSB_DJOB_NUMPROC" ]; then
          ntasks=$LSB_DJOB_NUMPROC
        else
          ntasks=1
        fi
      else
        echo "ntasks has not been defined for platform $machine"
      fi

      remainder=$(($ntasks-$(cat $cmdfile|wc -l)%$ntasks))
      n=0;while [ $((n+=1)) -le $remainder ] ;do
          echo "echo do nothing" >>$cmdfile
      done
      l=0
      n=-1
      while read line ;do ((n+=1))
          if [ $((n%ntasks)) -eq 0 ] ; then
              ((l+=1))
              >cmdlist.$l
          fi
          echo "$line" >> cmdlist.$l
      done < $cmdfile
      n=0
      while [ $((n+=1)) -le $l ] ;do
        $APRUN -pgmmodel mpmd -cmdfile cmdlist.$n
        export err=$?; err_chk
      done
  fi

 if [ -s nampke  ] ; then rm nampke ; fi
 echo 'catting'

cat > nampke <<EOF
&nampke
io=$IO,jo=$JO,ko=$KO,po=$polist, idbug=$idbug,
indir="$INDIR", indxdir="$INDXDIR", iname="$iname",
CDUMP="$CDUMP", index="$index",
syear=$syear, smonth=$smonth, sday=$sday, shour=$shour,
eyear=$eyear, emonth=$emonth, eday=$eday, ehour=$ehour,
fhour=$fhr, dhour=$dhour,/
EOF
echo 'after catting'

  $APRUN $AVGPKEEXEC < nampke 1> pkeavg.${sdate}_${edate}.out_$fhr 2> pkeavg.${sdate}_${edate}.err_$fhr
  export err=$?; err_chk
  #if [ $? -ne 0 ] ; then echo "ERROR: $AVGPKEEXEC returned non-zero" ; ((errs+=1)) ; fi
  cat dgout.* > $OUTDIR/${oname}$SUFOUT
 done
else
  export iname=pgb${prefix2}
  export index=pgi${prefix2}
  export oname=${prefix1}${prefix2}
  fhr=${fhr:-0}

# compute index files if they don't exits
  cdate=$sdate
  if [ -s $cmdfile ] ; then rm $cmdfile ; fi
  > $cmdfile
  while [ $cdate -le $edate ] ; do
    pgb=$INDIR/$iname.$CDUMP.$cdate
    [ ! -s $pgb ] && exit 1
    pgi=$INDXDIR/$index.$CDUMP.$cdate
    if [ $REMOVE_INDEX = YES ] ; then /bin/rm $pgi ; fi
    if [ ! -s $pgi ] ; then
       echo $WINDEX $pgb $pgi >>$cmdfile
    fi
    cdate=$($NDATE $dhour $cdate)
  done
  if [ -s $cmdfile ] ; then
   cat $cmdfile
      if [ $machine = IBM ]; then
        ntasks=$(echo $LOADL_PROCESSOR_LIST|wc -w)
        # only valid if count .le. 128
        [ $ntasks -eq 128 ] && ntasks=$(poe hostname|wc -l)
      elif [ $machine = WCOSS ]; then
        if [ -n "$LSB_PJL_TASK_GEOMETRY" ]; then
          ntasks=`echo $LSB_PJL_TASK_GEOMETRY | sed 's/[{}(),]/ /g' | wc -w`
        elif [ -n "$LSB_DJOB_NUMPROC" ]; then
          ntasks=$LSB_DJOB_NUMPROC
        else
          ntasks=1
        fi
      else
        echo "ntasks has not been defined for platform $machine"
      fi

      remainder=$(($ntasks-$(cat $cmdfile|wc -l)%$ntasks))
      n=0;while [ $((n+=1)) -le $remainder ] ;do
          echo "echo do nothing" >>$cmdfile
      done
      l=0
      n=-1
      while read line ;do ((n+=1))
          if [ $((n%ntasks)) -eq 0 ] ; then
              ((l+=1))
              >cmdlist.$l
          fi
          echo "$line" >> cmdlist.$l
      done < $cmdfile
      n=0
      while [ $((n+=1)) -le $l ] ;do
        $APRUN -pgmmodel mpmd -cmdfile cmdlist.$n
        export err=$?; err_chk
      done
  fi

 if [ -s nampke  ] ; then rm nampke ; fi 
   echo 'catting'

cat > nampke <<EOF
&nampke
io=$IO,jo=$JO,ko=$KO,po=$polist, idbug=$idbug,
indir="$INDIR", indxdir="$INDXDIR", iname="$iname",
CDUMP="$CDUMP", index="$index",
syear=$syear, smonth=$smonth, sday=$sday, shour=$shour,
eyear=$eyear, emonth=$emonth, eday=$eday, ehour=$ehour,
fhour=$fhr, dhour=$dhour,/
EOF

  echo 'after catting'
  $APRUN $AVGPKEEXEC <nampke 1>pkeavg.${sdate}_${edate}.out2 2>pkeavg.${sdate}_${edate}.err
  export err=$?; err_chk
  #rc=$?
  #if [ $? -ne 0 ] ; then echo "ERROR: $AVGPKEEXEC returned non-zero" ; ((errs+=1)) ; fi

  cat dgout.* > $OUTDIR/${oname}$SUFOUT
fi

#exit $errs
