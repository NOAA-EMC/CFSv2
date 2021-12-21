#!/bin/bash

#------------------------------------------------------------
# Build all "emcsfc" programs.
#
# For more details, see the documentation in each
# program sub-directory.
#------------------------------------------------------------

set -euax

set +x
module purge
source $HOMEcfs/versions/build.ver
module load envvar/${envvar_ver}
module load PrgEnv-intel/${PrgEnv_intel_ver}
module load craype/${craype_ver}
module load intel/${intel_ver}
module load cray-mpich/${cray_mpich_ver}
module load ip/${ip_ver}
module load sp/${sp_ver}
module load w3nco/${w3nco_ver}
module load bacio/${bacio_ver}
module load jasper/${jasper_ver}
module load zlib/${zlib_ver}
module load libpng/${libpng_ver}
module load g2/${g2_ver}
module load landsfcutil/${landsfcutil_ver}
module list
set -x

mkdir -p ../exec # place for executables

for directory in emcsfc_snow2mdl.fd  emcsfc_grib_snowgrib.fd  ## only compile the snow programs
do
echo
cd $directory
makefile.sh           
cd ..
echo
done

echo; echo DONE BUILDING EMCSFC PROGRAMS
