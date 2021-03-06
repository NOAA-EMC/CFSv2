#!/bin/bash
set -eua
export machine=wcoss
export FC=ifort

export FFLAGSM="-O2 -xHost -FR -convert big_endian -g -traceback"
export LDFLAGSM=

export INCSM=-I$SFCIO_INC4         
export LIBSM="$W3EMC_LIB4 $W3NCO_LIB4 $SP_LIB4 $SFCIO_LIB4"

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod

