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

make clean
make
make install
make clean

