#!/bin/ksh
set -e

###################################################################
# Script to break apart grib files > 2gb and copygb them in chunks
# Then cat them together in order to make one big converted grib
#
# Patrick Tripp - last updated Sep 2010
# Patrick Tripp and Xingren Wu - updated March 5, 2011
###################################################################

# Incoming arguments can match those used by a call to copygb
# This has only been tested using the copygb -x grib1 grib2, not tested using index
# The last two arguments must be ifile ofile

if [ $# -lt 2 ] ; then
  echo "Usage: $0 [options] infile outfile"
  echo"        See copygb for available options."
  exit 1
fi

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
FIXDIR=$DATA/copygb.$pid
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
  if [ $segmod -eq 1 ] ; then segsz=$(($segsz - 1)) ; fi
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

    $COPYGB $parms $ipiece $opiece

    # cat chunk to ofile
    cat $opiece >> $ofile

    rec=$erec
    ((cnt+=1))
  done

else
  # else just call copygb with whole file
  $COPYGB $parms $ifile $ofile
fi

/bin/rm -Rf $FIXDIR

