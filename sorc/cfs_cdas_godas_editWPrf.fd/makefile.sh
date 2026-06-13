#!/bin/bash
set -eua 
export machine=wcoss
export FC=ftn  

export FFLAGSM="-O3 -free  -convert big_endian -traceback"
export LDFLAGSM=

export W3LIB="$W3EMC_LIB4 $W3NCO_LIB4"
export NCDF=$NETCDF_LDFLAGS
export INC=$NETCDF_INCLUDE

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod

