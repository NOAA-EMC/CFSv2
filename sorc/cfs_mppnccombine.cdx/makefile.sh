#!/bin/bash
set -x

export machine=wcoss
export CC=cc

export CFLAGSM="-O3 -xHost"
export LDFLAGSM=
 
##module unload NetCDF; module load NetCDF/4.2/serial
export NCDF="-L$NETCDF/lib -lnetcdf_c++ -lnetcdf"
export INC=$NETCDF_INCLUDE 

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod

