#!/bin/bash
set -eua 

export machine=wcoss
export FC=ifort

export FFLAGSM=
export LDFLAGSM=
 
export LIBSM=

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod

