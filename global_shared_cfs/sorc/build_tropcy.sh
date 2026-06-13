#!/bin/bash
# 
#  07052015	E.Mirvis -   made build more universal - environmental module based (see readme)
#               EMC/NCEP/NOAA
#
#  excutables created from build_tropcy.sh: 
#        1) relocate_mv_nvortex.fd/relocate_mv_nvortex
#        2) vint.fd/vint.x
#        3) tave.fd/tave.x
#        4) syndat_qctropcy.fd/syndat_qctropcy
#        5) syndat_maksynrc.fd/syndat_maksynrc
#        6) syndat_getjtbul.fd/syndat_getjtbul
#        7) supvit.fd/supvit
#        8) gettrk.fd/gettrk
#

set -euax

 export INC="${G2_INCd}"
 export LIBS="${W3EMC_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${G2_LIBd} ${PNG_LIB} ${JASPER_LIB} ${Z_LIB}"
 export LIBS_SUP="${W3EMC_LIBd} ${W3NCO_LIBd}"
 export LIBS_REL="${W3NCO_LIB4} ${SIGIO_LIB4} ${BACIO_LIB4} ${SP_LIBd}"
 export LIBS_SIG="${SIGIO_INC4}"
 export LIBS_SYN_GET="${W3NCO_LIB4}"
 export LIBS_SYN_MAK="${W3NCO_LIB4} ${BACIO_LIB4}"
 export LIBS_SYN_QCT="${W3NCO_LIB8}"
 export FFLAGS="-qopenmp -O3 -g -traceback -r8 -I ${SIGIO_INC4}"

#for dir in *.fd; do
# cd $dir
# make clean
# make -f makefile
# cd ..
#done

cd relocate_mv_nvortex.fd
   make clean
   make -f makefile
   make install
   make clean
   cd ../
cd vint.fd
   make clean
   make -f makefile
   make install
   make clean
   cd ../
cd tave.fd
   make clean
   make -f makefile
   make install
   make clean
   cd ../
cd syndat_qctropcy.fd
   make clean
   make -f makefile
   make install
   make clean
   cd ../
cd syndat_maksynrc.fd
   make clean
   make -f makefile
   make install
   make clean
   cd ../
cd syndat_getjtbul.fd
   make clean
   make -f makefile
   make install
   make clean
   cd ../
cd supvit.fd
   make clean
   make -f makefile
   make install
   make clean
   cd ../
cd gettrk.fd
   make clean
   make -f makefile
   make install
   make clean
   cd ../
