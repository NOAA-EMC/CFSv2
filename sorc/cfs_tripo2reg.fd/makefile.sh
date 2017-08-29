#!/bin/bash
set -x

export machine=wcoss
export FC90=ifort
export FC=ifort

export FFLAGSM="-O3 -free -convert big_endian -traceback"
export FFLAGSM2="-O3 -free -r8"
export LDFLAGSM=
 
export NCDF=$NETCDF_LDFLAGS
export INC=$NETCDF_INCLUDE
export LIBSM="$W3NCO_LIB4 $BACIO_LIB4 $SP_LIB4 $IP_LIB4 $LANDSFCUTIL_LIB4"                          

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod

