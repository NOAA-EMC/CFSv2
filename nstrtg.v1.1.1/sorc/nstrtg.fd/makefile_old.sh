#!/bin/sh

module load ips/18.0.1.163
module load impi/18.0.1

module load NetCDF/4.5.0
module load nemsio/2.2.3
module load w3nco/2.0.6
module load bacio/2.0.2

export FCOMP=ifort

export FFLAGS="-O3 -fp-model precise -g -r8 -i4"
export NETCDF_INCLUDE="-I${NETCDF}/include"
export NETCDF_LDFLAGS_F="-L${NETCDF}/lib -lnetcdf -lnetcdff"

make clean
make build
err=$?
if [ $err -ne 0 ]; then
  echo ERROR BUILDING nst_to_rtg      
  exit 2
fi
make install
make clean

exit
