#!/bin/bash
set -eua

export LIBS="$W3EMC_LIB4 $W3NCO_LIB4 $BUFR_LIB4"

export FCMP=ifort 
export FFLAGSM="-g -O4 -traceback"
export LDFLAGS=

for NAME in cfs_bufrslupao cfs_bufrslslev cfs_bufrslsfc cfs_sfc cfs_raob cfs_acft cfs_acar cfs_surf; do
 export NAME
 make -f Makefile clean
 make -f Makefile
 make -f Makefile clean
 mv $NAME ../../exec
done

