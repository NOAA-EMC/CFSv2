#!/bin/bash
set -euax

export machine=wcoss
export FC=ftn   

export FFLAGSM="-O3 -free -convert big_endian -traceback"
export LDFLAGSM=
 
export LIBSM="$BACIO_LIB4 $W3EMC_LIB4 $W3NCO_LIB4 $SP_LIB4 $IP_LIB4 $BUFR_LIB4"
export NCDF=$NETCDF_LDFLAGS
export INC=$NETCDF_INCLUDE

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f make.sice_rstrt  ; mv cfs_cdas_sice_rstrt ../../exec
make -f make.read_sst    ; mv cfs_cdas_read_sst   ../../exec
make -f make.read_fice   ; mv cfs_cdas_read_fice  ../../exec

rm -f *.o *.mod
