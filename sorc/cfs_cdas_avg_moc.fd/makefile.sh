#!/bin/bash
set -x
export machine=wcoss
export FC=ftn  

export FFLAGSM="-O2 -convert big_endian -assume byterecl -traceback"
export LDFLAGSM=

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod

