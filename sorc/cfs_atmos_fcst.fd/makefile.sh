#!/bin/bash
set -x

machine=wcoss
ptmp="/gpfs/dell2/ptmp/$USER/Jack/makefcst"

#  WARNING!!! The default endianness is local to the machine.
#   If your initial conditions are bigendian and want to compile on littleendian
#   machine, you must set NATIVE_ENDIAN=NO

NATIVE_ENDIAN=${NATIVE_ENDIAN:-NO}

sorc_dir=$(pwd)
exec_dir=$(pwd)
mkdir -p $exec_dir

make_dir=$ptmp/branch/sorc/$(basename $sorc_dir)

#####################################################################
if [ $make_dir = $(pwd) ] ; then
  echo "The make_dir is the current directory - compilation aborted"
  echo "To continue compiling, comment out this if loop and rerun"
  exit
fi
#####################################################################

mkdir -p $make_dir
cd $make_dir || exit 99
[ $? -ne 0 ] && exit 8

rm $make_dir/*.o
rm $make_dir/*.mod

tar -cf- -C$sorc_dir .|tar -xf-

if [ $NATIVE_ENDIAN = YES ] ; then
 cp $sorc_dir/sigio_r_module_native.f sigio_r_module.f
 cp $sorc_dir/bafrio_native.f         bafrio.f
fi

#
 export EXECM=$exec_dir/cfs_atmos_fcst
#

export CFLAGS="-DLINUX"
export ARCHM=
export PGSZM=
export FRRM=-FR
export FXXM=

export OPTSB="-O1 -convert big_endian -fp-model precise "  

export OPTSBT=$OPTSB
export OPTSIOM="$OPTSBT -r8 "
export OPTSM="$OPTSBT -r8 -qopenmp"
export OPTS_SERM="$OPTSBT -r8 $ARCHM"
export OPTS90M="$OPTSBT   -r8 "
export OPTS90AM="$OPTSBT  -r8 "
export LDFLAGSM=$PGSZM

export F77M=mpiifort    
export F90M=mpiifort   
export F77B=$F77M
export FCC=mpicc
export LDRM=mpiifort
export LDFLAGSM="$PGSZM -qopenmp -mkl"
export FINC=   #esmf include path found in Makefile
export FINCM="-I$W3EMC_INCd"

#export ESMFLIBM=/gpfs/gp1/usrx/local/esmf-3.1.0rp2/lib/libO/Linux.intel.64.intelmpi.default/libesmf.a
#export ESMF_LIB=/gpfs/gp1/usrx/local/esmf-3.1.0rp2/lib/libO/Linux.intel.64.intelmpi.default/libesmf.a
#export ESMF_MOD=/gpfs/gp1/usrx/local/esmf-3.1.0rp2/mod/modO/Linux.intel.64.intelmpi.default
#export ESMF_INC=/gpfs/gp1/usrx/local/esmf-3.1.0rp2/mod/modO/Linux.intel.64.intelmpi.default
#export ESMFMKFILE=/gpfs/gp1/usrx/local/esmf-3.1.0rp2/lib/libO/Linux.intel.64.intelmpi.default/esmf.mk
###export ESMFMKFILE=/gpfs/dell2/emc/modeling/noscrub/Jack.Woollen/cfsv2_prod_dev_repository/sorc/esmf.mk


 export ESMF_LIB=-L$ESMF_LIB
#export ESMF_MOD=-I$ESMF_MOD
#export ESMF_INC=-I$ESMF_INC

MODULEPATH_ROOT=/usrx/local/prod/modulefiles

echo
echo $ESMF_LIB
echo $ESMF_MOD
echo $ESMFLIB
echo

export LIBSM="$ESMF_LIB  $BACIO_LIB4 $NEMSIO_LIB4 $SP_LIBd $W3EMC_LIBd $W3NCO_LIBd $NETCDF_LDFLAGS_CXX"

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

echo $F77M
make -f Makefile

cd $sorc_dir
mv ${make%.*} ../../exec
rm -f *.o *.mod

