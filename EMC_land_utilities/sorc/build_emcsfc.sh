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
module load envvar/1.0
module load PrgEnv-intel/8.1.0
module load craype/2.7.8
module load intel/19.1.3.304
module load cray-mpich/8.1.7
module load ip/3.3.3
module load sp/2.3.3
module load w3nco/2.4.1
module load bacio/2.4.1
module load jasper/2.0.25
module load zlib/1.2.11
module load libpng/1.6.37
module load g2/3.4.1
module load landsfcutil/2.4.1
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
