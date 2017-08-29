#!/bin/bash
set -euax

export LIBJW="$W3NCO_LIB4 $W3EMC_LIB4 $BUFR_LIB4 $SP_LIB4 $SIGIO_LIB4 $SFCIO_LIB4 $BACIO_LIB4"
export INCJW="-I $SIGIO_INC4 -I $SFCIO_INC4" 
export FC=mpiifort

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod


