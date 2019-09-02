#!/bin/sh --login

#---------------------------------------------------------------------------------
#  The driver script for compiling the emcsfc_gridgen_sfc driver program.  Loads
#  module files and exports environment variables required by the makefile.
#  Then, invokes the makefile.  
#
#  Only tested on Theia, NCEP WCOSS Phase 1/2, Cray and Dell machines.
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
  echo "BUILD EMCSFC_GRIDGEN_SFC DRIVER PROGRAM ON WCOSS DELL."
  echo

  module purge
  module load EnvVars/1.0.2
  module use ../../../modulefiles
  module load module.gridgen_sfc.wcoss.dell

  module list

  make clean
  make
  rc=$? ;;

#---------------------------------------------------------------------------------
# BUILD PROGRAM ON WCOSS PHASE 1 AND PHASE 2.
#---------------------------------------------------------------------------------

g????.ncep.noaa.gov | t????.ncep.noaa.gov)

  echo
  echo "BUILD EMCSFC_GRIDGEN_SFC DRIVER PROGRAM ON WCOSS PHASE 1 AND PHASE 2."
  echo

  module purge
  module load ../../../modulefiles/module.gridgen_sfc.wcoss

  module list

  make clean
  make all
  rc=$?  ;;

#---------------------------------------------------------------------------------
# BUILD PROGRAM ON WCOSS CRAY.
#---------------------------------------------------------------------------------

llogin? | slogin?)

  echo
  echo "BUILD EMCSFC_GRIDGEN_SFC DRIVER PROGRAM ON WCOSS-CRAY."
  echo

  module purge
  module load modules/3.2.6.7
  module load ../../../modulefiles/module.gridgen_sfc.wcoss.cray

  module list

  make clean
  make
  rc=$?  ;;

#---------------------------------------------------------------------------------
# BUILD PROGRAM ON THEIA.
#---------------------------------------------------------------------------------

tfe??)

  echo
  echo "BUILD EMCSFC_GRIDGEN_SFC DRIVER PROGRAM ON THEIA"
  echo

  module purge

# load intel compiler

  module load intel/15.1.133
  module load impi/5.1.2.150
  export FCOMP=mpiifort
  export FFLAGS="-O3 -fp-model strict -convert big_endian -r8 -i4"
  export LDFLAGS="-openmp"

# load ncep library modules

  module use -a /scratch3/NCEPDEV/nwprod/lib/modulefiles
  module load g2/v2.5.0
  module load ip/v3.0.0
  module load sp/v2.0.2
  module load bacio/v2.0.1
  module load w3nco/v2.0.6
  module load w3emc/v2.0.5
  module load jasper
  module load z
  module load png

  module list

  make clean
  make
  rc=$? ;;

#---------------------------------------------------------------------------------
# UNSUPPORTED MACHINE.
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
