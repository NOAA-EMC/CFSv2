#!/bin/bash

##module load NetCDF-cray-sandybridge/3.6.3

set -x

export CFLAGSM="-O3"
export LDFLAGSM=

export NCDF="-L$NETCDF/lib -lnetcdf"
export INC=-I$NETCDF/include
export CC=cc

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

# make the netcdf cfs_mppnccombine executable

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod

#make the mpi wrapper

ftn -traceback -g -o cfs_mpinccombine mpinccombine.f
mv cfs_mpinccombine ../../exec
rm -f *.o *.mod
