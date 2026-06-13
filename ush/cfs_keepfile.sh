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

#set -x
if [ $# -ne 6 ] ; then
	err_exit "`date` $0: argument error"
fi
file=$1
start_forecast_hour=$2
end_forecast_hour=$3
fh_inc=$4
dir=$5
tot_err=$6

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
    $cfprun poescript |grep 'CFP RANK'
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
