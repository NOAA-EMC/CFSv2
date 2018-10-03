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

export SFCIO_VER=v1.0.0
export SFCIO_DIR=$NWPRODLIB/sfcio/$SFCIO_VER
export SFCIO_LIB4=sfcio_${SFCIO_VER}_4    
export SFCIO_INC4=${SFCIO_DIR}/incmod/$SFCIO_LIB4

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

export SFCIO_VER=
export SFCIO_DIR=$NWPRODLIB
export SFCIO_LIB4=sfcio_4    
export SFCIO_INC4=${SFCIO_DIR}/incmod/$SFCIO_LIB4

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
export LIBSM="-L${SFCIO_DIR} -l${SFCIO_LIB4} \
              -L${BACIO_DIR} -l${BACIO_LIB4} \
              -L${W3NCO_DIR} -l${W3NCO_LIB4} "

make -f Makefile clean
make -f Makefile  
make -f Makefile install
