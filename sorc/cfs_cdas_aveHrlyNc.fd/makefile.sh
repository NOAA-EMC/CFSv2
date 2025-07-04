#!/bin/bash
set -x

export machine=wcoss
export FC=ftn   

export FFLAGSM="-O3 -free"
export LDFLAGSM=

export NCDF=$NETCDF_LDFLAGS 
export INC=$NETCDF_INCLUDE  

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod


