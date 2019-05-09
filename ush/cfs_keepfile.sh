#!/bin/ksh
############################################################
#
# script:  cfs_delfile.sh
# purpose: to keep 12-hourly files only up to 45 days 
#  $1 ... file name prefix [pgb/flx/fcstout/...]
#  $2 ... starting forecast hour
#  $3 ... ending forecast hour
#  $4 ... atm forecast hour interval
#  $5 ... file directory
#  $6 ... tot_err
#  $DAY_TO_KEEP is defined in the config script (=45 days)
#  $HOUR_TO_KEEP is defined in the config script(=1080 hr)

set -x
if [ $# -ne 6 ] ; then
	err_exit "`date` $0: argument error"
fi
file=$1
start_forecast_hour=$2
end_forecast_hour=$3
fh_inc=$4
dir=$5
tot_err=$6

export APRUN=${APRUN:-mpirun.lsf}

mkdir -p $DATA/6hrly_grib/$file
cd $DATA/6hrly_grib/$file
rm -f $DATA/6hrly_grib/$file/*

start_date=$YMDH
rm -rf keeplist
tot_err=0
fh=$start_forecast_hour

if [ $file = flxf -o $file = pgbf -o $file = ipvf -o $file = ocnf ]
then
   convert_grb2=yes
else
   convert_grb2=no
fi

rm -f poescript

if [ $end_forecast_hour -ge $HOUR_TO_KEEP ]
then
   end_forecast_hour=$HOUR_TO_KEEP
fi

while [ $fh -le $end_forecast_hour ]
do
  fday=$($NDATE $fh $start_date)
  ofile=${file}$fday.${ENS_MEM}.${start_date}
  ifile=${file}$fh.${ENS_MEM}.${start_date}
  fh=`expr $fh + $fh_inc`
  if [ $fh -lt 10 ]; then fh=0$fh; fi

  if [ $convert_grb2 = yes ]
  then
    echo "$CNVGRIB -g12 -p40 ${dir}/$ifile ${ofile}.grb2" >>poescript
  else
    cp ${dir}/$ifile $COM_HRLY/$ofile
    if [ $file = sigf -o $file = sfcf ]; then
      if [ $SENDDBN = YES ]; then
        $DBNROOT/bin/dbn_alert MODEL CFS_FCST $job $COM_HRLY/$ofile
      fi
    fi
  fi
  err=$?
  tot_err=`expr $tot_err + $err`
done

if [ $convert_grb2 = yes ]
then
  if [ -s poescript ] ; then

#   if [ $machine = IBM ]; then
#     nprocs=$(echo $LOADL_PROCESSOR_LIST|wc -w)
#     # only valid if count .le. 128
#     [ $nprocs -eq 0 ] && nprocs=$(poe hostname|wc -l)
#   elif [ $machine = WCOSS ]; then
#     if [ -n "$LSB_PJL_TASK_GEOMETRY" ]; then
#       nprocs=`echo $LSB_PJL_TASK_GEOMETRY | sed 's/[{}(),]/ /g' | wc -w`
#     elif [ -n "$LSB_DJOB_NUMPROC" ]; then
#       nprocs=$LSB_DJOB_NUMPROC
#     else
#       nprocs=1
#     fi
#   elif [ $machine = WCRAY ]; then
#       nprocs=24
#   else
#     echo "nprocs has not been defined for platform $machine"
#   fi
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
#
#
#   n=0
#   while [ $((n+=1)) -le $l ] ;do
#      if [[ $machine = WCOSS ]]; then
#         $APRUN -pgmmodel mpmd -cmdfile cmdlist.$n -stdoutmode ordered
#         export err=$?; err_chk
#      elif [[ $machine = WCRAY ]]; then
#         aprun -q -b -j1 -n$nprocs -d1 -cc depth cfp cmdlist.$n          
#         export err=$?; err_chk
#      fi
#   done

    mpirun cfp poescript
    export err=$?; err_chk
  fi
  
  # Move the grib2 files to the 6hrly_grib directory
  if [ $SENDCOM = YES ]; then
    for grb2file in `ls *.grb2`
    do
      $WGRIB2 $grb2file >${grb2file}.idx
    done
    mv *.grb2 $COM_HRLY/.
    mv *.grb2.idx $COM_HRLY/.
  fi

  if [ $SENDDBN = YES ]; then
    fh=$start_forecast_hour
    while [ $fh -le $end_forecast_hour ]
    do
      fday=$($NDATE $fh $start_date)
      $DBNROOT/bin/dbn_alert MODEL CFS_FCST $job $COM_HRLY/${file}$fday.${ENS_MEM}.${start_date}.grb2
      $DBNROOT/bin/dbn_alert MODEL CFS_FCST_WIDX $job $COM_HRLY/${file}$fday.${ENS_MEM}.${start_date}.grb2.idx

      fh=`expr $fh + $fh_inc`
      if [ $fh -lt 10 ]; then fh=0$fh; fi
    done
  fi
fi
