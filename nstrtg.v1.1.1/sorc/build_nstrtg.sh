#! /usr/bin/env bash
set -eux

mkdir -p ../exec # place for executables

set +x
module purge
source $HOMEcfs/versions/build.ver
module load envvar/${envvar_ver}
module load PrgEnv-intel/${PrgEnv_intel_ver}
module load craype/${craype_ver}
module load intel/${intel_ver}
module load cray-mpich/${cray_mpich_ver}
module load netcdf/${netcdf4_ver}
module load nemsio/${nemsio_ver}
module load w3nco/${w3nco_ver}
module load bacio/${bacio_ver}
module list
set -x

cd ./nstrtg.fd
./makefile.sh
