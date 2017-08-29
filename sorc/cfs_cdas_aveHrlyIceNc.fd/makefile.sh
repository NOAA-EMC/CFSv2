#!/bin/bash
set -x

export machine=wcoss
export FC=ifort

export FFLAGSM="-O3 -free -xHost"
export LDFLAGSM=

export NCDF=$NETCDF_LDFLAGS                   
export INC=$NETCDF_INCLUDE                      

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod
