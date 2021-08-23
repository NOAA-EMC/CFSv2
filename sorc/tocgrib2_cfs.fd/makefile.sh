#!/bin/bash
set -eua

echo
#module purge
#module load EnvVars/1.0.2
#module load ips/18.0.1.163
#module load impi/18.0.1    
#module load bacio/2.0.2
#module load w3nco/2.0.6
#module load jasper/1.900.29
#module load libpng/1.2.59    
#module load zlib/1.2.11
#module load g2/3.1.0
#module list
echo

export FC=ftn  

export LIBS="$BACIO_LIB4 $W3NCO_LIB4 $G2_LIB4 $JASPER_LIB $PNG_LIB $Z_LIB"
export INC=$G2_INC4

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile clean
make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod

