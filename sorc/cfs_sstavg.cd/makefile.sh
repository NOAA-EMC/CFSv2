#!/bin/bash
set -x

export machine=wcoss
export FC=icc

export FFLAGSM="-O3 -xHost -convert big_endian -traceback"
export LDFLAGSM=
 
module unload NetCDF; module load NetCDF/4.5.0

export LIBSM="$W3EMC_LIB4 $W3NCO_LIB4 $BACIO_LIB4 $SIGIO_LIB4"

export NCDF="-L$NETCDF/lib -lnetcdf_c++ -lnetcdf"
export NCDF="-L$NETCDF/lib -lnetcdf"
export INC=$NETCDF_INCLUDE 

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod


