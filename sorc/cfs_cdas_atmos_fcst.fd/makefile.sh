#!/bin/bash

set +x
echo
module purge
module load EnvVars/1.0.2
module load ips/18.0.1.163
module load smpi/10.1.1.0
module load ESMF/4_0_0rp2
module load NetCDF/3.6.3
module load bacio/2.0.2
module load nemsio/2.2.3
module load sp/2.0.2
module load w3emc/2.3.0
module load w3nco/2.0.6
module load bufr/11.2.0
module load ip/3.0.1
module load sfcio/1.0.0
module load sigio/2.0.1
module load gfsio/1.1.0
module load landsfcutil/2.1.0
module list
echo
set -x

machine=wcoss

#  WARNING!!! The default endianness is local to the machine.
#   If your initial conditions are bigendian and want to compile on littleendian
#   machine, you must set NATIVE_ENDIAN=NO

NATIVE_ENDIAN=${NATIVE_ENDIAN:-NO}

sorc_dir=$(pwd)
exec_dir=$(pwd)
mkdir -p $exec_dir

ptmp="/gpfs/dell2/ptmp/$USER/Jack/makefcst"
make_dir=$ptmp/branch/sorc/$(basename $sorc_dir)

#####################################################################
if [ $make_dir = $(pwd) ] ; then
  echo "The make_dir is the current directory - compilation aborted"
  echo "To continue compiling, comment out this if loop and rerun"
  exit
fi
#####################################################################

rm -rf $make_dir; mkdir -p $make_dir
cd $make_dir || exit 99
[ $? -ne 0 ] && exit 8

tar -cf- -C$sorc_dir .|tar -xf-

if [ $NATIVE_ENDIAN = YES ] ; then
 cp $sorc_dir/sigio_r_module_native.f sigio_r_module.f
 cp $sorc_dir/bafrio_native.f         bafrio.f
fi

#
 export EXECM=$exec_dir/cfs_cdas_atmos_fcst
#

export CFLAGS="-DLINUX"
export ARCHM=
export PGSZM=
export FRRM=-FR
export FXXM=

export OPTSB="-O3 -convert big_endian -fp-model precise "  

export OPTSBT=$OPTSB
export OPTSIOM="$OPTSBT -r8 "
export OPTSM="$OPTSBT -r8 -qopenmp"
export OPTS_SERM="$OPTSBT -r8 $ARCHM"
export OPTS90M="$OPTSBT   -r8 "
export OPTS90AM="$OPTSBT  -r8 "
export LDFLAGSM=$PGSZM

export F77M=mpif90  
export F90M=mpif90   
export F77B=$F77M
export FCC=mpicc
export LDRM=mpif90   
export LDFLAGSM="$PGSZM -qopenmp -mkl"
export FINC=   #esmf include path found in Makefile
export FINCM="-I$W3EMC_INCd"

export LIBSM="-L$ESMF_LIB  $BACIO_LIB4 $NEMSIO_LIB4 $SP_LIBd $W3EMC_LIBd $W3NCO_LIBd $NETCDF_LDFLAGS_CXX"


echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

echo $F77M
make -f Makefile

cd $sorc_dir
mv ${make%.*} ../../exec
rm -f *.o *.mod

