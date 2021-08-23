#!/bin/bash 

#---------------------------------------------------------------------------------
#  The driver script for compiling the emcsfc_snow2mdl program.  Loads nceplib
#  module files and exports environment variables required by the makefile
#  Then, invokes the makefile.
#---------------------------------------------------------------------------------
set -euax

#---------------------------------------------------------------------------------
# BUILD PROGRAM ON WCOSS2              
#---------------------------------------------------------------------------------

echo
echo "BUILD EMCSFC_SNOW2MDL PROGRAM ON WCOSS2"
echo

FCOMP=ifort
FFLAGS="-O0 -r8 -i4 -FR -qopenmp -convert big_endian -assume byterecl"

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
module load jasper/2.0.16
module load zlib/1.2.11
module load libpng/1.6.37
module load g2/3.4.1
module load landsfcutil/2.4.1
module list
set -x

make clean
make
make install
make clean 



