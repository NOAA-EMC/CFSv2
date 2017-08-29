#!/bin/bash
set -euax

export LIBJW="$W3EMC_LIB4 $W3NCO_LIB4 $BUFR_LIB4"

# make the convdiag executable

make -f makefile_convdiag

# make the duprep.x executable

make -f makefile_duprep

mv cfs_duprep  cfs_post_convdiag ../../exec 

rm -f *.o *.mod
