#!/bin/bash
set -x

export machine=acorn 
export FCMP=ftn
export FCMP95=ftn
export FFLAGSM="-O3 -r8 -i4 -convert big_endian -FR -assume byterecl -fpconstant"


export NCDF="-L$NETCDF_LIB" 
export INC="-I$NETCDF_INC" 


echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod
