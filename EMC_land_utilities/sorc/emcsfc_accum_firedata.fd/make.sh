#!/bin/sh --login

#---------------------------------------------------------------------------------
#  The driver script for compiling the emcsfc_accum_firedata program.  Loads
#  nceplib module files and exports environment variables required by the makefile
#  Then, invokes the makefile.  
#
#  Only tested on Theia, NCEP WCOSS Phase 1/2, Cray and Dell.
#
#  To invoke: type 'make.sh' from the command line.  If successfully built, 
#  the executable will be installed the ../../exec subdirectory.
#
#  See the README.build file for more details.
#---------------------------------------------------------------------------------

#set -x

mac=$(hostname -f)

case $mac in

#---------------------------------------------------------------------------------
# BUILD PROGRAM ON WCOSS DELL.
#---------------------------------------------------------------------------------

v????.ncep.noaa.gov | m????.ncep.noaa.gov)

  echo
  echo "BUILD EMCSFC_ACCUM_FIREDATA PROGRAM ON WCOSS DELL."
  echo

  module purge
  module load EnvVars/1.0.2
  module use ../../modulefiles
  module load module.accum_firedata.wcoss.dell

  module list

  make clean
  make
  rc=$? ;;

#---------------------------------------------------------------------------------
# BUILD PROGRAM ON WCOSS Phase 1/2.
#---------------------------------------------------------------------------------

g????.ncep.noaa.gov | t????.ncep.noaa.gov)

  echo
  echo "BUILD EMCSFC_ACCUM_FIREDATA PROGRAM ON WCOSS PHASE 1/2"
  echo

  module purge
  module load ../../modulefiles/module.accum_firedata.wcoss

  module list

  make clean
  make all
  rc=$?  ;;

#---------------------------------------------------------------------------------
# BUILD PROGRAM ON WCOSS CRAY.
#---------------------------------------------------------------------------------

llogin? | slogin?)

  echo
  echo "BUILD EMCSFC_ACCUM_FIREDATA PROGRAM ON WCOSS-CRAY."
  echo

  module purge
  module load modules/3.2.6.7
  module load ../../modulefiles/module.accum_firedata.wcoss.cray

  module list

  make clean
  make
  rc=$?  ;;

#---------------------------------------------------------------------------------
# BUILD PROGRAM ON THEIA.
#---------------------------------------------------------------------------------

tfe??)

  echo
  echo "BUILD EMCSFC_ACCUM_FIREDATA PROGRAM ON THEIA."
  echo

  module purge

# load ncep library modules

  module use -a /scratch3/NCEPDEV/nwprod/lib/modulefiles
  module load w3nco/v2.0.6
  module load bacio/v2.0.1
  module load jasper
  module load z
  module load png
  module load g2/v2.5.0

# load intel compiler

  module load intel/14.0.2
  module load impi/4.1.3.048
  export FCOMP=mpiifort
  export FFLAGS="-O0 -g"
  export LDFLAGS=" -openmp"

  module list

  make clean
  make
  rc=$?  ;;

#---------------------------------------------------------------------------------
# UNSUPPORTED MACHINE.
#---------------------------------------------------------------------------------

*)

  echo "MACHINE OPTION NOT FOUND. EXIT."
  exit ;;

esac

#---------------------------------------------------------------------------------
# INSTALL EXECUTABLE.
#---------------------------------------------------------------------------------

if ((rc != 0));then
  echo "BUILD FAILED. EXIT."
  exit
else
  make install
fi

exit
