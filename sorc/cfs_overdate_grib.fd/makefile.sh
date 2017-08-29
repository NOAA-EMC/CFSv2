#!/bin/bash
set -x

export machine=wcoss
export FC=ifort
export EXE=cfs_overdate_grib
export LIBS="$W3NCO_LIB4 $BACIO_LIB4"

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod

