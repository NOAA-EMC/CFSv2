#!/bin/ksh
###########################################################
#
#    This script modified by Shrinivas Moorthi on March  2004
# script:  cfs_endfhrs.sh
#
#  Input variables:
#    start_date .... starting date
#    end_date   .... ending date
#
#  This script returns following variables
#    ENDHOUR ... forecast hour from starting date to ending date.
#    YEARENDHOUR ... forecast hour from starting date to the end of the year
#    MFHOUR_n .. forecast hour to the end of coming months (n=1,MCOUNT)
#    MCOUNT  ... number of coming months 
#
#  In order to use thes variables in calling scripts, you need to execute it
#  as a same process, i.e., in the form '.
#  /nfsuser/g03/wx51hh/jif2001/sfm/script/runscr/endfhour' (period
#  and space before the endhour script name).
#
#
echo "\n in unendfhrs: $start_date $end_date"

sdate=$start_date
edate=$end_date
#
YYYYS=$(echo $start_date | cut -c1-4)
MMS=$(echo $start_date | cut -c5-6)
YYYYE=$(echo $end_date | cut -c1-4)
MME=$(echo $end_date | cut -c5-6)

export LDYOFYR=$(($YYYYS+1))010100
export ENDHOUR_OF_THE_YEAR=`$NHOUR $LDYOFYR $sdate `  # length of forecast in hours
export ENDHOUR=`$NHOUR $edate $sdate `       # length of forecast in hours
#
#  forecaset hour to the end of each month
#
MME=`expr $MME + 0`
if [ $MME -lt 10 ] ; then MME=0$MME ; fi
ee=$YYYYE$MME
yy=$YYYYS
mm=`expr $MMS + 1`
if [ $mm -lt 10 ] ; then mm=0$mm ; fi
if [ $mm -gt 12 ] ; then
	mm=01
	yy=`expr $yy + 1`
fi
ss=$yy$mm
MCOUNT=0
while [ $ss -le $ee ] ; do

  MCOUNT=`expr $MCOUNT + 1`
  export MFHOUR_$MCOUNT=`$NHOUR ${ss}0100 $sdate `
  
  mm=`expr $mm + 1`
  if [ $mm -gt 12 ] ; then
    mm=1
    yy=`expr $yy + 1`
  fi
  if [ $mm -lt 10 ] ; then mm=0$mm ; fi
  ss=$yy$mm
done
echo MCOUNT=$MCOUNT
