#!/bin/ksh
set -x

#-----------------------------------------------------
#-use standard modules. called by ../../build_wcoss.sh 
#-----------------------------------------------------

export FCMP=ifort
export FCMP95=$FCMP

export FFLAGSM="-i4 -O3 -r8  -convert big_endian -fp-model precise"
export FFLAGS2M="-i4 -O3 -r8 -convert big_endian -fp-model precise -FR"
export RECURS=
export LDFLAGSM="-openmp -auto"
export OMPFLAGM="-openmp -auto"

export LIBSM="${GFSIO_LIB4} \
              ${SIGIO_LIB4} \
              ${SFCIO_LIB4} \
              ${BACIO_LIB4} \
              ${LANDSFCUTIL_LIBd} \
              ${IP_LIBd} \
              ${SP_LIBd} \
              ${W3EMC_LIBd} \
              ${W3NCO_LIBd} \
              ${NEMSIO_LIB} "


make -f Makefile clean
make -f Makefile
make -f Makefile install
make -f Makefile clean
