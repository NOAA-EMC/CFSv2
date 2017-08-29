#!/bin/bash
set -euax

export machine=wcoss
export CC=icc

export CFLAGSM="-O3"
export LDFLAGSM=
 
module unload NetCDF; module load NetCDF/4.2/serial
export NCDF=$NETCDF_LDFLAGS
export INC=$NETCDF_INCLUDE

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod

