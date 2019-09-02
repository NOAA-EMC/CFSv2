#!/bin/sh --login

#---------------------------------------------------------------------------------
#  The driver script for compiling the emcsfc_coldstart program.  Loads
#  module files and exports environment variables required by the makefile
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
  echo "BUILD EMCSFC_COLDSTART PROGRAM ON WCOSS DELL."
  echo

  module purge
  module load EnvVars/1.0.2
  module use ../../modulefiles
  module load module.coldstart.wcoss.dell

  module list

  make clean
  make
  rc=$? ;;

#---------------------------------------------------------------------------------
# BUILD PROGRAM ON WCOSS PHASE 1 AND PHASE 2.
#---------------------------------------------------------------------------------

g????.ncep.noaa.gov | t????.ncep.noaa.gov)

  echo
  echo "BUILD EMCSFC_COLDSTART PROGRAM ON WCOSS PHASE 1 AND PHASE 2."
  echo

  module purge
  module load ../../modulefiles/module.coldstart.wcoss

  module list

  make clean
  make all
  rc=$?  ;;

#---------------------------------------------------------------------------------
# BUILD PROGRAM ON WCOSS-CRAY.
#---------------------------------------------------------------------------------

llogin? | slogin?)

  echo
  echo "BUILD EMCSFC_COLDSTART PROGRAM ON WCOSS-CRAY"
  echo

  module purge
  module load modules/3.2.6.7
  module load ../../modulefiles/module.coldstart.wcoss.cray

  module list

  make clean
  make
  rc=$?  ;;

#---------------------------------------------------------------------------------
# BUILD PROGRAM ON THEIA.
#---------------------------------------------------------------------------------

tfe??)

  echo
  echo "BUILD EMCSFC_COLDSTART PROGRAM ON THEIA"
  echo

  module purge

  module load intel/14.0.2
  module load impi/4.1.3.048
  export FCOMP=mpiifort
  export FFLAGS="-O0 -convert big_endian -r8 -i4 -FR"
  export FPPFLAGS="-fpp -DGFS=0 -save-temps"
  export LFLAGS="-openmp"

# load ncep library modules

  module use -a /scratch3/NCEPDEV/nwprod/lib/modulefiles
  module load netcdf/3.6.3
  export NETCDF_INCLUDE=-I${NETCDF}/include
  export NETCDF_LDFLAGS_F="-L${NETCDF}/lib -lnetcdf"
  module load sfcio/v1.0.0
  module load ip/v3.0.0
  module load sp/v2.0.2
  module load w3nco/v2.0.6
  module load bacio/v2.0.1
  module load jasper
  module load z
  module load png
  module load g2/v2.5.0
  module load nemsio/v2.2.1
  module load landsfcutil/v2.1.0

  module list

  make clean
  make
  rc=$?  ;;

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
