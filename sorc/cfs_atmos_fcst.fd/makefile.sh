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

##rm $make_dir/*.o
##rm $make_dir/*.mod

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

export OPTSB="-O3 -convert big_endian -fp-model precise "  

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

export ESMF_LIB=/gpfs/tp1/usrx/local/esmf-3.1.0rp5/lib/libO/Linux.intel.64.intelmpi.default/libesmf.a
export ESMF_MOD=-I/gpfs/tp1/usrx/local/esmf-3.1.0rp5/mod/modO/Linux.intel.64.intelmpi.default

MODULEPATH_ROOT=/usrx/local/prod/modulefiles

__LMOD_REF_COUNT__LMFILES_=/usrx/local/prod/modulefiles/core_third/EnvVars/1.0.2:1;/usrx/local/prod/modulefiles/core_third/HPSS/5.0.2.5:1;/usrx/local/prod/modulefiles/core_third/lsf/10.1:1;/usrx/local/dev/modulefiles/git/2.14.3:1;/usrx/local/prod/modulefiles/core_third/ips/18.0.1.163:1;/usrx/local/prod/modulefiles/compiler_mpi/ips/18.0.1/impi/18.0.1.lua:1;/gpfs/dell1/nco/ops/nwprod/modulefiles/mpi_prod/ips/18.0.1/impi/18.0.1/nemsio/2.2.3:1;/usrx/local/prod/modulefiles/compiler_third/ips/18.0.1/libpng/1.2.59:1;/gpfs/dell1/nco/ops/nwprod/modulefiles/compiler_prod/ips/18.0.1/crtm/2.2.5:1;/gpfs/dell1/nco/ops/nwprod/modulefiles/compiler_prod/ips/18.0.1/gfsio/1.1.0:1;/gpfs/dell1/nco/ops/nwprod/modulefiles/compiler_prod/ips/18.0.1/g2tmpl/1.5.0:1;/gpfs/dell1/nco/ops/nwprod/modulefiles/mpi_prod/ips/18.0.1/impi/18.0.1/nemsiogfs/2.0.1:1;/gpfs/dell1/nco/ops/nwprod/modulefiles/compiler_prod/ips/18.0.1/dumpjb/5.0.0:1;/usrx/local/prod/modulefiles/compiler_third/ips/18.0.1/xmlparse/2.0.0:1;/gpfs/dell1/nco/ops/nwprod/modulefiles/compiler_prod/ips/18.0.1/landsfcutil/2.1.0:1;/gpfs/dell1/nco/ops/nwprod/modulefiles/compiler_prod/ips/18.0.1/w3nco/2.0.6:1;/usrx/local/prod/modulefiles/compiler_third/ips/18.0.1/NetCDF/4.5.0:1;/gpfs/dell1/nco/ops/nwprod/modulefiles/compiler_prod/ips/18.0.1/bufr/11.2.0:1;/gpfs/tp1/usrx/local/esmf-3.1.0rp5/lib/libO/Linux.intel.64.intelmpi.default

_LMFILES_=/usrx/local/prod/modulefiles/core_third/EnvVars/1.0.2:/usrx/local/prod/modulefiles/core_third/HPSS/5.0.2.5:/usrx/local/prod/modulefiles/core_third/lsf/10.1:/usrx/local/dev/modulefiles/git/2.14.3:/usrx/local/prod/modulefiles/core_third/ips/18.0.1.163:/usrx/local/prod/modulefiles/compiler_mpi/ips/18.0.1/impi/18.0.1.lua:/gpfs/dell1/nco/ops/nwprod/modulefiles/mpi_prod/ips/18.0.1/impi/18.0.1/nemsio/2.2.3:/usrx/local/prod/modulefiles/compiler_third/ips/18.0.1/libpng/1.2.59:/gpfs/dell1/nco/ops/nwprod/modulefiles/compiler_prod/ips/18.0.1/crtm/2.2.5:/gpfs/dell1/nco/ops/nwprod/modulefiles/compiler_prod/ips/18.0.1/gfsio/1.1.0:/gpfs/dell1/nco/ops/nwprod/modulefiles/compiler_prod/ips/18.0.1/g2tmpl/1.5.0:/gpfs/dell1/nco/ops/nwprod/modulefiles/mpi_prod/ips/18.0.1/impi/18.0.1/nemsiogfs/2.0.1:/gpfs/dell1/nco/ops/nwprod/modulefiles/compiler_prod/ips/18.0.1/dumpjb/5.0.0:/usrx/local/prod/modulefiles/compiler_third/ips/18.0.1/xmlparse/2.0.0:/gpfs/dell1/nco/ops/nwprod/modulefiles/compiler_prod/ips/18.0.1/landsfcutil/2.1.0:/gpfs/dell1/nco/ops/nwprod/modulefiles/compiler_prod/ips/18.0.1/w3nco/2.0.6:/usrx/local/prod/modulefiles/compiler_third/ips/18.0.1/NetCDF/4.5.0:/gpfs/dell1/nco/ops/nwprod/modulefiles/compiler_prod/ips/18.0.1/bufr/11.2.0:/gpfs/tp1/usrx/local/esmf-3.1.0rp5/lib/libO/Linux.intel.64.intelmpi.default/libesmf.a

MODULEPATH=/usrx/local/prod/lmod/lmod/modulefiles/Core:/usrx/local/prod/modulefiles/core_third:/usrx/local/prod/modulefiles/defs:/gpfs/dell1/nco/ops/nwprod/modulefiles/core_prod:/usrx/local/prod/modulefiles/compiler_third/ips/18.0.1:/gpfs/dell1/nco/ops/nwprod/modulefiles/compiler_prod/ips/18.0.1:/usrx/local/prod/modulefiles/compiler_mpi/ips/18.0.1:/usrx/local/prod/modulefiles/mpi_third/ips/18.0.1/impi/18.0.1:/gpfs/dell1/nco/ops/nwprod/modulefiles/mpi_prod/ips/18.0.1/impi/18.0.1:/usrx/local/dev/modulefiles


echo
echo $ESMF_LIB
echo $ESMF_MOD
echo $ESMFLIB
echo

export LIBSM="$ESMF_LIB  $BACIO_LIB4 $NEMSIO_LIB4 $SP_LIBd $W3EMC_LIBd $W3NCO_LIBd $ESMF_LIB"

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

echo $F77M
make -f Makefile

cd $sorc_dir
mv ${make%.*} ../../exec
rm -f *.o *.mod

