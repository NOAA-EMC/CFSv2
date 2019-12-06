#!/bin/bash

set +x
echo
module purge
module load EnvVars/1.0.2
module load ips/18.0.1.163
module load smpi/10.1.1.0
module load NetCDF/3.6.3
module list
echo
set -ex


export CFLAGSM="-O3 -xHost"
export LDFLAGSM=

export NCDF="-L$NETCDF/lib -lnetcdf"
export INC=-I$NETCDF/include
export CC=mpicc

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

# make the netcdf cfs_mppnccombine executable

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod

#make the mpi wrapper

mpif90 -traceback -g -o cfs_mpinccombine mpinccombine.f
mv cfs_mpinccombine ../../exec
rm -f *.o *.mod
