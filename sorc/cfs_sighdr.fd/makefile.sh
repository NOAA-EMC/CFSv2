#!/bin/bash
set -x

export machine=wcoss
export FC=ftn  

export FFLAGSM="-O2 -convert big_endian -traceback -FR"
export LDFLAGSM=
 
export LIBSM="$W3EMC_LIB4 $W3NCO_LIB4 $BACIO_LIB4 $SIGIO_LIB4"
export INCMOD=$SIGIO_INC4 

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod

