#!/bin/sh

#------------------------------------------------------------
# Build all "emcsfc" programs.
#
# For more details, see the documentation in each
# program sub-directory.
#------------------------------------------------------------

#set -x

mkdir -p ../exec # place for executables

for directory in emcsfc_snow2mdl.fd  emcsfc_grib_snowgrib.fd
do
  case $directory in
    *gridgen_sfc.fd)
      cd $directory/lib
      make clean
      make.sh
      cd ../driver
      make clean
      make.sh
      cd ../.. ;;
    *)
      cd $directory
      make clean
      make.sh
      cd .. ;;
  esac

done

echo; echo DONE BUILDING EMCSFC PROGRAMS
