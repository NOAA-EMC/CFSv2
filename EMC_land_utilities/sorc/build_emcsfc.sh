#!/bin/bash

#------------------------------------------------------------
# Build all "emcsfc" programs.
#
# For more details, see the documentation in each
# program sub-directory.
#------------------------------------------------------------

set -euax

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
