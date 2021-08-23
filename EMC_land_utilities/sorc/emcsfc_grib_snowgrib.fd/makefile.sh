#!/bin/bash 

#---------------------------------------------------------------------------------
#  The driver script for compiling the emcsfc_grib_snowgrib program.  Loads
#  module files and exports environment variables required by the makefile
#  Then, invokes the makefile.  
#
#  Only tested on Zeus and the NCEP WCOSS machines.
#
#  To invoke: type 'makefile.sh' from the command line.  If successfully built, 
#  the executable will be installed the ../../exec subdirectory.
#
#  See the README.build file for more details.
#---------------------------------------------------------------------------------

set -euax 

echo
echo "BUILD EMCSFC_GRIB_SNOWGRIB PROGRAM ON WCOSS PHASE 1/2."
echo

export FCOMP=ftn  
export FFLAGS="-O0 -real-size 32 -integer-size 32  -convert big_endian -assume byterecl"
export LDFLAGS="-qopenmp"

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
set -x


make clean
make
make install
make clean

