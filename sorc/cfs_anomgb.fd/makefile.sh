#!/bin/bash
set -eua

export LIBSM="$W3NCO_LIBd $IP_LIBd $SP_LIBd $BACIO_LIB4"

export FC=ftn  

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod

