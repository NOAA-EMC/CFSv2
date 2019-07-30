#!/bin/ksh
set -x
export FCMP=${1:-mpiifort}
export FCMP95=${2:-${FCMP}}

###### LIBS="-L$LIBDIR -lgfsio_v1.1.0_4 -lsigio_4  -lsfcio_4 -lbacio_4 -llandsfcutil_d -lip_d -lw3emc_d -lw3nco_d -lnemsio -lsp_v2.0.1_d "
export LIBS="$GFSIO_LIB4 $SIGIO_LIB4 $SFCIO_LIB4 $BACIO_LIB4 $LANDSFCUTIL_LIBd $IP_LIBd $W3EMC_LIBd $W3NCO_LIBd $NEMSIO_LIB $SP_LIBd" 

export FFLAGSM="-i4 -O3 -r8  -convert big_endian -fp-model precise"
export FFLAGS2M="-i4 -O3 -r8 -convert big_endian -fp-model precise -FR"

export RECURS=

export LIBFLAGSM=" "
export LDFLAGSM="-qopenmp -auto"
export OMPFLAGM="-qopenmp -auto"

make -f Makefile clean
make -f Makefile install
make -f Makefile clean
