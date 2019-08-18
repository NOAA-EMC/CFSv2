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


export machine=wcoss
export FCMP=mpif90   
export FCMP95=mpif90  
export FFLAGSM="-O1 -r8 -i4 -convert big_endian -FR -assume byterecl -fpconstant"

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod
