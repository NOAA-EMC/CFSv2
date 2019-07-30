#!/bin/bash

set +x
echo
module unload impi/18.0.1
module load smpi/10.1.1.0
module list
echo
set -x


export machine=wcoss
export FCMP=mpiifort
export FCMP95=mpiifort
export FFLAGSM="-O1 -r8 -i4 -convert big_endian -FR -assume byterecl -fpconstant"

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod
