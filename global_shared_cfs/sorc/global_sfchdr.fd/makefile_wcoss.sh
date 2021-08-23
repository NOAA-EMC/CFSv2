#!/bin/sh
set -x

#-----------------------------------------------------
#-use standard module. called by ../../build_wcoss.sh
#-----------------------------------------------------

export FCMP=ftn  
export FFLAGS=" -O2 -xHOST -convert big_endian -traceback -g -FR"
export LIBSM="${SFCIO_LIB4} \
              ${BACIO_LIB4} \
              ${W3NCO_LIB4} "


make -f Makefile clean
make -f Makefile  
make -f Makefile install
