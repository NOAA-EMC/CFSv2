#!/bin/ksh
set -x

#-----------------------------------------------------
#-use standard module. called by ../../build_wcoss.sh
#-----------------------------------------------------

export FCMP=ftn   

##export DEBUG='-ftrapuv -check all -check nooutput_conversion -fp-stack-check -fstack-protector -traceback -g'
export FFLAGS="-O3 -r8 -convert big_endian -traceback -g"
export OMPFLAG=-qopenmp
export LDFLG=-qopenmp

export LIBSM="${SFCIO_LIB4} \
              ${W3EMC_LIBd} \
              ${W3NCO_LIBd} \
              ${BACIO_LIB4} \
              ${SP_LIBd} "

make -f Makefile clean
make -f Makefile
make -f Makefile install
make -f Makefile clean
