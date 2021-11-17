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

make clean
make
make install
make clean 



