#!/bin/bash
set -eua

export machine=wcoss
export CC=icc

export CFLAGSM="-O3"
export LDFLAGSM=

module unload NetCDF; module load NetCDF/4.2/serial
export NCDF="-L/usrx/local/NetCDF/4.2/serial/lib -lnetcdf"
export INC=-I/usrx/local/NetCDF/4.2/serial/include

export NCDF=$NETCDF_LDFLAGS
export INC=$NETCDF_INCLUDE

export execs="cfs_cdas_godas_mk1DySss4i  cfs_cdas_godas_mk1DySst4i cfs_cdas_godas_mkDlyTclm"

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile clean
make -f Makefile; mv $execs ../../exec
make -f Makefile clean

