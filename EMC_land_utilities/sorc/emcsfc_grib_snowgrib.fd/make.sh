#!/bin/sh --login

#---------------------------------------------------------------------------------
#  The driver script for compiling the emcsfc_grib_snowgrib program.  Loads
#  module files and exports environment variables required by the makefile
#  Then, invokes the makefile.  
#
#  Only tested on Zeus and the NCEP WCOSS machines.
#
#  To invoke: type 'make.sh' from the command line.  If successfully built, 
#  the executable will be installed the ../../exec subdirectory.
#
#  See the README.build file for more details.
#---------------------------------------------------------------------------------

#set -x

mac=$(hostname | cut -c1-1)

if [ $mac = t -o $mac = g ] ; then  #For WCOSS

  echo
  echo "BUILD EMCSFC_GRIB_SNOWGRIB PROGRAM ON WCOSS PHASE 1/2."
  echo

  module purge

# load intel compiler

  module load ics/12.1

# load ncep library modules

  module load ip/v3.0.0
  module load sp/v2.0.2
  module load w3nco/v2.0.6
  module load bacio/v2.0.1
  module load jasper/v1.900.1
  module load z/v1.2.6
  module load png/v1.2.44
  module load g2/v2.5.0

  export FCOMP=ifort
  export FFLAGS="-O0 -real-size 32 -integer-size 32 -I${IP_INC4} -convert big_endian -assume byterecl"
  export LDFLAGS="-openmp"

# ensure libraries and include directories exist.

  make check_prereqs
  rc=$?
  if ((rc != 0));then
    echo "MISSING LIBRARY AND/OR INCLUDE DIRECTORY. EXIT."
    exit
  fi

elif [ $mac = s -o $mac = l ] ; then  #For WCOSS Cray

  echo
  echo "BUILD EMCSFC_GRIB_SNOWGRIB PROGRAM ON WCOSS CRAY."
  echo

  module purge
  module load modules/3.2.6.7

  module load PrgEnv-intel
  module load craype-haswell

  module load ip-intel/3.0.0
  module load sp-intel/2.0.2
  module load w3nco-intel/2.0.6
  module load bacio-intel/2.0.2
  module load jasper-gnu-haswell/1.900.1
  module load zlib-intel-haswell/1.2.7
  module load png-intel-haswell/1.2.49
  module load g2-intel/3.1.0

  export FCOMP=ftn
  export FFLAGS="-O0 -real-size 32 -integer-size 32 -I${IP_INC4} -convert big_endian -assume byterecl"
  export LDFLAGS="-qopenmp"
  
elif [ $mac = v -o $mac = m ] ; then  #For WCOSS Dell

  echo
  echo "BUILD EMCSFC_GRIB_SNOWGRIB PROGRAM ON WCOSS DELL"
  echo

  module purge
  module use /usrx/local/dev/modulefiles

# load intel compiler

  module load ips/18.0.1.163

# load ncep library modules

  module load ip/3.0.1
  module load sp/2.0.2
  module load w3nco/2.0.6
  module load bacio/2.0.2
  module load jasper/1.900.1
  module load zlib/1.2.11
  module load libpng/1.2.44
  module load g2/3.1.0

  export FCOMP=ifort
  export FFLAGS="-O0 -real-size 32 -integer-size 32 -I${IP_INC4} -convert big_endian -assume byterecl"
  export LDFLAGS="-qopenmp"

else

  echo "MACHINE OPTION NOT FOUND. EXIT."
  exit

fi

# invoke the makefile

make clean
make
rc=$?

if ((rc != 0));then
  echo "BUILD FAILED. EXIT."
  exit
else
  make install
  make clean
fi

exit
