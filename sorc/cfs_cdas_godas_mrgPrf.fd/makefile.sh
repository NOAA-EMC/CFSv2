#!/bin/bash
set -eua

export machine=wcoss
export FC=ifort

export FFLAGSM="-O3 -free -xHost -convert big_endian -traceback"
export LDFLAGSM=
 
export WLIB="$W3EMC_LIB4 $W3NCO_LIB4" 

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod

