#!/bin/sh --login

#---------------------------------------------------------------------------------
#  The driver script for compiling the emcsfc_fire2mdl program.  Loads
#  nceplib module files and exports environment variables required by the makefile
#  Then, invokes the makefile.  
#
#  Only tested on Theia and NCEP WCOSS Phase 1/2, Cray and Dell machines.
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
  echo "BUILD EMCSFC_FIRE2MDL PROGRAM ON WCOSS DELL."
  echo

  module purge
  module load EnvVars/1.0.2
  module use ../../modulefiles
  module load module.fire2mdl.wcoss.dell

  module list

  make clean
  make
  rc=$? ;;

#---------------------------------------------------------------------------------
# BUILD PROGRAM ON WCOSS Phase 1/2.
#---------------------------------------------------------------------------------

g????.ncep.noaa.gov | t????.ncep.noaa.gov)

  echo
  echo "BUILD EMCSFC_FIRE2MDL PROGRAM ON WCOSS PHASE 1/2."
  echo

  module purge
  module load ../../modulefiles/module.fire2mdl.wcoss

# ensure libraries and include directories exist.

  module list

  make clean
  make all
  rc=$?  ;;

#---------------------------------------------------------------------------------
# BUILD PROGRAM ON WCOSS CRAY.
#---------------------------------------------------------------------------------

llogin? | slogin?)

  echo
  echo "BUILD EMCSFC_FIRE2MDL PROGRAM ON WCOSS-CRAY."
  echo

  module purge
  module load modules/3.2.6.7
  module load ../../modulefiles/module.fire2mdl.wcoss.cray

  module list

  make clean
  make
  rc=$?  ;;

#---------------------------------------------------------------------------------
# BUILD PROGRAM ON THEIA.
#---------------------------------------------------------------------------------

tfe??)

  echo
  echo "BUILD EMCSFC_FIRE2MDL PROGRAM ON THEIA."
  echo

  module purge

# load ncep library modules

  module use -a /scratch3/NCEPDEV/nwprod/lib/modulefiles
  module load ip/v3.0.0
  module load sp/v2.0.2
  module load w3nco/v2.0.6
  module load bacio/v2.0.1
  module load jasper
  module load z
  module load png
  module load g2/v2.5.0

# load intel compiler

  module load intel/14.0.2
  export FCOMP=ifort
  export FFLAGS="-O0 -g -I${G2_INCd} -r8 -i4"

  module list

  make clean
  make
  rc=$?  ;;

#---------------------------------------------------------------------------------
# UNKNOWN MACHINE.
#---------------------------------------------------------------------------------

*)

  echo "MACHINE OPTION NOT FOUND. EXIT."
  exit  ;;

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
