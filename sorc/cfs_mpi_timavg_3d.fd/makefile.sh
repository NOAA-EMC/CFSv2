#!/bin/bash 
set -x

export machine=wcoss
export FC=ftn        
export FFLAGSM="-O3 -convert big_endian"

export LIBSM="$W3NCO_LIB4 $W3EMC_LIB4 $BACIO_LIB4 $W3NCO_LIB4 "

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile clean
make -f Makefile
mv ${make%.*} ../../exec
make -f Makefile clean

