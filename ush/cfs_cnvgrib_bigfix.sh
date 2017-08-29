#!/bin/ksh
set -ex

######################################################################
# Script to break apart grib files > 2gb and cnvgrib them in chunks
# Then cat them together in order to make one big converted grib
#
# Patrick Tripp - last updated Dec 2010
######################################################################

# Incoming arguments can match those used by a call to cnvgrib
# The last two arguments must be ifile ofile

if [ $# -lt 2 ] ; then
  echo "Usage: $0 [options] infile outfile"
  echo"        See cnvgrib for available options."
  err_exit 1
fi

# In case cnvgrib is defined as THIS script, avoid runaway recursion and use the default cnvgrib here
# This MUST use the cfscgrib2 created especially for CFSv2 by Boi Vuong 
cfscgrib2=${cfscgrib2:-$EXECcfs/cfs_cgrib2}


argc=$#

parms=''

# Parse the arguments
if [ $argc -eq 2 ] ; then
  ifile=$1
  ofile=$2
else

  cnt=1
  maxp=$(($argc - 2))
  while [ $cnt -le $maxp ] 
  do
    arg=$(eval echo \$$cnt)
    parms="$parms $arg"
    ((cnt+=1))
  done
 
  ifile=$(eval echo \$$cnt)
  ((cnt+=1))
  ofile=$(eval echo \$$cnt)
fi

echo "parms: $parms"
echo "ifile: $ifile"
echo "ofile: $ofile"

> $ofile

pid=$$
FIXDIR=$DATA/cnvgrb.$pid
#FIXDIR=${FIXDIR:-${DATA:-/stmp/$LOGNAME/$pid}}
mkdir -p $FIXDIR

TWOGBS=1932735284  # (actual 1.8GB)

# Check filesize
fsize=`/bin/ls -l $ifile | awk '{print $5 }'`

# if size > 2GB break apart
if [ $fsize -gt $TWOGBS ] ; then

  echo "Breaking $ifile into pieces"

  inv=$FIXDIR/gribin.inv
  $WGRIB $ifile > $inv

  # check number of records
  # lrec=`$WGRIB $ifile | tail -1 | awk -F: '{print $1}'`
  lrec=`cat $inv | tail -1 | awk -F: '{print $1}'`

  echo "$lrec"
  echo $fsize
  echo $TWOGBS

  parts=$(($fsize / $TWOGBS + 1))
  echo "parts: $parts"

  segsz=$(($lrec / $parts))
  segmod=$(($lrec % $parts))    # if segmod = 0 then no additional end part
  echo "segsz: $segsz, segmod: $segmod"

  if [ $segmod -ne 0 ] ; then ((parts+=1)) ; fi

  # start and end records
  srec=1
  erec=$lrec

  # partition into pieces smaller than 2gb
  cnt=1
  rec=0
  while [ $cnt -le $parts ]
  do

    # start and end records
    srec=$(($rec + 1))
    erec=$(($srec + $segsz - 1))

    # If it is the last part of uneven segments
    if [ $erec -gt $lrec ] ; then erec=$lrec ; fi

    echo "Segment $cnt: $srec - $erec"

    ipiece=$FIXDIR/gribin.$cnt
    opiece=$FIXDIR/gribout.$cnt
    > $ipiece
    > $opiece

    cat $inv | awk -F: -v begr=$srec -v endr=$erec '{if ($1 >= begr && $1 <= endr) print $0}' | $WGRIB -i -grib $ifile -o $ipiece

    # Add copygb before doing the cfscgrib2 converson to set the BinScale
    $COPYGB -x $ipiece ${ipiece}_new
    $cfscgrib2 $parms ${ipiece}_new $opiece

    export err=$?; err_chk
    # cat chunk to ofile
    cat $opiece >> $ofile

    rec=$erec
    ((cnt+=1))
  done

else
  # else just call cfscgrib2 with whole file
  # Add copygb before doing the cfscgrib2 converson to set the BinScale
  $COPYGB -x $ifile $FIXDIR/gribin
  $cfscgrib2 $parms $FIXDIR/gribin $ofile
  export err=$?; err_chk
fi

/bin/rm -Rf $FIXDIR

