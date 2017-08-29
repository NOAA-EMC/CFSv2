#!/bin/bash
set -x

export machine=wcoss
export FCMP=mpfort
export FCMP95=mpfort
export FFLAGSM="-O3 -r8 -i4 -convert big_endian -FR -assume byterecl -fpconstant"

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod
