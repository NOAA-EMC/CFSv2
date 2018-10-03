#!/bin/sh
set -x

mac=$(hostname | cut -c1-1)

#---------------------------------------------
if [ $mac = t -o $mac = g ] ; then  #For WCOSS
#---------------------------------------------

export NWPRODLIB=/nwprod/lib
export FCMP=ifort

export W3NCO_VER=v2.0.6
export W3NCO_DIR=$NWPRODLIB/w3nco/$W3NCO_VER
export W3NCO_LIB4=w3nco_${W3NCO_VER}_4    

export SIGIO_VER=v1.0.1
export SIGIO_DIR=$NWPRODLIB/sigio/$SIGIO_VER
export SIGIO_LIB4=sigio_${SIGIO_VER}_4    
export SIGIO_INC4=${SIGIO_DIR}/incmod/$SIGIO_LIB4

export BACIO_VER=v2.0.1
export BACIO_DIR=$NWPRODLIB/bacio/$BACIO_VER
export BACIO_LIB4=bacio_${BACIO_VER}_4    

#---------------------------------------------
elif [ $mac = f ]; then #For Zeus
#---------------------------------------------
export NWPRODLIB=/contrib/nceplibs/nwprod/lib
export FCMP=ifort

export W3NCO_VER=v2.0.6
export W3NCO_DIR=$NWPRODLIB
export W3NCO_LIB4=w3nco_${W3NCO_VER}_4    

export SIGIO_VER=v2.0.1_beta
export SIGIO_DIR=/contrib/nceplibs/dev/lib
export SIGIO_LIB4=sigio_${SIGIO_VER}   
export SIGIO_INC4=${SIGIO_DIR}/incmod/$SIGIO_LIB4

export BACIO_VER=v2.0.1
export BACIO_DIR=$NWPRODLIB
export BACIO_LIB4=bacio_${BACIO_VER}_4    

#---------------------------------------------
else
 echo "Machine Option Not Found, exit"
 exit
fi
#---------------------------------------------

export FFLAGS=" -O2 -xHOST -convert big_endian -traceback -g -FR"
export LIBSM="-L${SIGIO_DIR} -l${SIGIO_LIB4} \
              -L${BACIO_DIR} -l${BACIO_LIB4} \
              -L${W3NCO_DIR} -l${W3NCO_LIB4} "

make -f Makefile clean
make -f Makefile
make -f Makefile install
