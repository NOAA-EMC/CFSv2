#!/bin/sh
# This script changes the date of a grib file
if [[ $# -lt 2 ]];then
 echo Usage: $0 yyyymmddhh grib.in [grib.out]
 exit 1
fi
d=$1
[[ $d > 0000000000 && $d < 9999999999 ]]||exit 2
i=$2
[[ -s $i ]]||exit 2
o=${3:-$i}
ln -s $i fort.11
echo $d|$HOMEcfs/exec/cfs_overdate_grib ||exit 3
mv fort.51 $o ||exit 3
rm fort.11
