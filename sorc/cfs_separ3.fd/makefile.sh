#!/bin/bash 

set -x
export FCMP=ifort
export FFLAGSM=" -g -O3 -r8 -convert big_endian -traceback -assume byterecl -fpconstant"


echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod

