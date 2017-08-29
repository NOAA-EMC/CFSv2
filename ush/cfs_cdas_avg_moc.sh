#!/bin/ksh
set -x

######################################################
# Script to create the moch mean - ocean diagnostics
#
# Originally written by Xingren Wu
# Updated by Patrick Tripp - Sept 2010
#
# Usage:
# $0 $yyyy $mm $finc
# yyyy = fcst year
# mm   = fcst month
# finc = incremental hours
######################################################

if [ $# -lt 3 ] ; then
    echo "Usage: $0 yyyy mm finc "
    err_exit 8
fi

yyyy=$1
mm=$2
finc=$3

# Expected file size of moch files
ESIZE=329632

errc=0

## FIX
dde=`$USHcfs/cfs_daysinmonth.sh $yyyy $mm`
sdate=${yyyy}${mm}0100
edate=${yyyy}${mm}${dde}18
cdate=$sdate
echo $yyyy $mm

> $TEMPDIR/input.mocfile
cdate=$sdate
until [[ $cdate -gt $edate ]]
do

  file=moch06m.gdas.$cdate
  echo $file >> input.mocfile

  if [[ -e $file ]] ; then

    # Check file size
    fsiz=`/bin/ls -Ll $file | awk '{print $5}'`
    if [[ $fsiz -ne $ESIZE ]] ; then
      echo "ERROR : $file does not have the expected size"
      ((errc+=1))
      err_exit $errc
    fi

  else
    echo "ERROR : $file not found"
    ((errc+=1))
    err_exit $errc
  fi

  cdate=`$NDATE $finc $cdate`
done

export jm=${jm:-410}
export km=${km:-40}
export nreg=${nreg:-5}
export avmocfile=moch06m.$CDUMP.$yyyy$mm.ieee
export nfiles=`cat input.mocfile | wc -l`

$AVMOCEXEC < input.mocfile >avmoc.out
if [[ $? -ne 0 ]] ; then
  echo "ERROR : AVMOCEXEC returned with non-zero exit"
  ((errc+=1))
  err_exit $errc
else
  /bin/mv -f $avmocfile $MONTHDIR
fi

exit $errc
