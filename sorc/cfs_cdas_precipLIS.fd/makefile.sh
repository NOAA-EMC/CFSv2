#!/bin/bash
set -eua

export machine=wcoss
export FC=ftn  

export FFLAGSM=
export LDFLAGSM=
 
export LIBSM="$BACIO_LIB4 $W3EMC_LIB4 $W3NCO_LIB4"

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod

