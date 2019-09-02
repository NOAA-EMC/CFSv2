#!/bin/sh --login

#---------------------------------------------------------------------------------
#  The driver script for compiling the emcsfc_gridgen_sfc library.  Loads
#  nceplib module files and exports environment variables required by the makefile
#  Then, invokes the makefile.  
#
#  Only tested on Theia, NCEP WCOSS Phase 1/2, Cray and Dell machines.
#
#  To invoke: type 'make.sh' from the command line.  If successfully built, 
#  the library will be installed in the ../../../lib subdirectory, and the
#  module files will be installed in the ../../../incmod/emcsfc_gridgen_sfc
#  directory.
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
  echo "BUILD EMCSFC_GRIDGEN_SFC LIBRARY ON WCOSS DELL."
  echo

  module purge
  module load EnvVars/1.0.2
  module use ../../../modulefiles
  module load module.gridgen_sfc_lib.wcoss.dell

  module list

  make clean
  make
  rc=$? ;;

#---------------------------------------------------------------------------------
# BUILD LIBRARY ON WCOSS PHASE 1 AND PHASE 2.
#---------------------------------------------------------------------------------

g????.ncep.noaa.gov | t????.ncep.noaa.gov)

  echo
  echo "BUILD EMCSFC_GRIDGEN_SFC LIBRARY ON WCOSS PHASE 1 AND PHASE 2."
  echo

  module purge
  module load ../../../modulefiles/module.gridgen_sfc_lib.wcoss

  module list

  make clean
  make all
  rc=$?  ;;

#---------------------------------------------------------------------------------
# BUILD LIBRARY ON WCOSS CRAY.
#---------------------------------------------------------------------------------

llogin? | slogin?)

  echo
  echo "BUILD EMCSFC_GRIDGEN_SFC LIBRARY ON WCOSS-CRAY."
  echo

  module purge
  module load modules/3.2.6.7
  module load ../../../modulefiles/module.gridgen_sfc_lib.wcoss.cray

  module list

  make clean
  make
  rc=$?  ;;

#---------------------------------------------------------------------------------
# BUILD PROGRAM ON THEIA.
#---------------------------------------------------------------------------------

tfe??)

  echo
  echo "BUILD EMCSFC_GRIDGEN_SFC LIBRARY ON THEIA"
  echo

  module purge

# load intel compiler

  module load intel/15.1.133
  module load impi/5.1.2.150
  export FCOMP=mpiifort
  export FFLAGS="-O3 -fp-model strict -I. -convert big_endian -r8 -i4"

# load ncep library modules

  module use -a /scratch3/NCEPDEV/nwprod/lib/modulefiles
  module load g2/v2.5.0
  module load ip/v3.0.0

  module list

  make clean
  make
  rc=$?  ;;

#---------------------------------------------------------------------------------
# UNSUPPORTED MACHINE.
#---------------------------------------------------------------------------------

*)

  echo "MACHINE OPTION NOT FOUND. EXIT."
  exit   ;;

esac

#---------------------------------------------------------------------------------
# INSTALL LIBRARY.
#---------------------------------------------------------------------------------

if ((rc != 0));then
  echo "BUILD FAILED. EXIT."
  exit
else
  make install
fi

exit
