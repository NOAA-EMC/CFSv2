#!/bin/bash
set -x

export machine=wcoss
export FC=ifort

export FFLAGSM=
export LDFLAGSM=
 
export LIBSM="$BACIO_LIB4 $W3EMC_LIB4 $W3NCO_LIB4 $IP_LIB4 $SP_LIB4" 

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod

