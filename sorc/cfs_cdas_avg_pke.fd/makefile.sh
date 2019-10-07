#!/bin/bash
set -x
export FC=mpif90  

export FFLAGSM="-O0 -convert big_endian -g -traceback"
export LDFLAGSM="-qopenmp"

export LIBSM="$W3NCO_LIB4 $W3EMC_LIB4 $BACIO_LIB4" 

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile
mv ${make%.*} ..
rm -f *.o *.mod


