#!/bin/ksh
#######################################################################################
# Script cfs_savetimeser.sh concatenates the time series files for each chunk into one 
# daily file and save them in the daily_grib directory
#######################################################################################
echo $0 

sdate=$1
edate=$2
inchour=$3

export APRUN=${APRUN:-mpirun.lsf}

endhour=$($NHOUR $edate $sdate) 
if [ $endhour -lt $inchour ]; then
  inchour=$endhour
fi

shour=6
fout=$FHOUT

PARM_AM=${PARM_AM:-$HOMEcfs/parm/cfs_parm_am}
varlist=$PARM_AM/cfs_parmlist_timeseries

rm -rf $DATA/time_grib
mkdir -p $DATA/time_grib
cd $DATA/time_grib
rm -f poescript

for var in `cat $varlist`
do
  CDATE=$sdate
  echo $var
  varname=`echo $var |awk -F"_" '{print $1}'`
  #ofile=$COM_DG/$varname.${ENS_MEM}.${sdate}.daily
  ofile=$varname.${ENS_MEM}.${sdate}.daily
  rm -f $ofile

  # Concatenate the files 
  idir=$COM_YMDH/daily_grib
  
  if [ $ENS_MEM -le 2 ]
  then
    FSDATE=`$NDATE 6 $CDATE`
    FEDATE=`$NDATE 24 $CDATE`

    ifile=$idir/${var}.$FSDATE.$FEDATE.${ENS_MEM}.$sdate
    if [ -s $ifile ]; then
      cp $ifile $ofile
    else
      echo "$ifile not found"
      export err=1; err_chk
    fi
    shour1=`expr $shour + 24`
    FSDATE=`$NDATE $shour1 $CDATE`
    FEDATE=`$NDATE $inchour $CDATE`
  else
    FSDATE=`$NDATE $shour $CDATE`
    FEDATE=`$NDATE $inchour $CDATE`
  fi

  until [[ $CDATE -ge $edate ]] ; do
    ifile=$idir/${var}.$FSDATE.$FEDATE.${ENS_MEM}.$sdate
    if [ -s $ifile ]; then
      cat $ifile >> $ofile
    else
      echo "$ifile not found"
      export err=1; err_chk
    fi
    CDATE=`$NDATE $inchour $CDATE`
    FSDATE=`$NDATE $shour $CDATE`
    FEDATE=`$NDATE $inchour $CDATE`
    if [ $FEDATE -gt $edate ]; then FEDATE=$edate; fi
  done

  char3=`echo $var |cut -c1-3`
  if [ $char3 = wnd ]; then
    echo "$CNVGRIB -g12 -p40 -nv $ofile ${ofile}.grb2" >>poescript
  else
    echo "$CNVGRIB -g12 -p40 $ofile ${ofile}.grb2" >>poescript
  fi
done      

mpirun cfp poescript
export err=$?; err_chk

if [ -s poescript ] ; then
# if [ $machine = IBM ]; then
#   nprocs=$(echo $LOADL_PROCESSOR_LIST|wc -w)
#   # only valid if count .le. 128
#   [ $nprocs -eq 0 ] && nprocs=$(poe hostname|wc -l)
# elif [ $machine = WCOSS ]; then
#   if [ -n "$LSB_PJL_TASK_GEOMETRY" ]; then
#     nprocs=`echo $LSB_PJL_TASK_GEOMETRY | sed 's/[{}(),]/ /g' | wc -w`
#   elif [ -n "$LSB_DJOB_NUMPROC" ]; then
#     nprocs=$LSB_DJOB_NUMPROC
#   else
#     nprocs=1
#   fi
# elif [ $machine = WCRAY ]; then
#     nprocs=24
# else
#   echo "nprocs has not been defined for platform $machine"
# fi
#
#    remainder=$(($nprocs-$(cat poescript|wc -l)%$nprocs))
#    n=0;while [ $((n+=1)) -le $remainder ] ;do
#      echo "echo do nothing" >> poescript
#    done
#
#    l=0
#    n=-1
#    while read line ;do ((n+=1))
#      if [ $((n%nprocs)) -eq 0 ] ; then
#        ((l+=1))
#        >cmdlist.$l
#      fi
#      echo "$line" >> cmdlist.$l
#    done < poescript
#  n=0
#  while [ $((n+=1)) -le $l ] ;do
#     if [[ $machine = WCOSS ]]; then
#        $APRUN -pgmmodel mpmd -cmdfile cmdlist.$n -stdoutmode ordered
#        export err=$?; err_chk
#     elif [[ $machine = WCRAY ]]; then
#        aprun -q -b -j1 -n$nprocs -d1 -cc depth cfp cmdlist.$n            
#        export err=$?; err_chk
#     fi
#  done

   # Move the grib2 files to time_grib directory
   if [ $SENDCOM = YES ]
   then
     for grb2file in `ls *.grb2`	
     do
       $WGRIB2 $grb2file -s >${grb2file}.idx
     done
     mv *.grb2 $COM_DG/.
     mv *.grb2.idx $COM_DG/.
   fi
  
   if [ $SENDDBN = YES ]; then
     cd $COM_DG
     for name in `ls *.grb2`
     do
       $DBNROOT/bin/dbn_alert MODEL CFS_FCST_TIMESER $job $COM_DG/${name}
       $DBNROOT/bin/dbn_alert MODEL CFS_FCST_TIMESER_WIDX $job $COM_DG/${name}.idx
     done
   fi

fi

exit 0
