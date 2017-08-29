#!/bin/bash

set -eua

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

export FC=mpfort
export CC=icc

here=`pwd`; cd make
make -f Makefile
rm -f *.o *.mod
cd $here

mv ${make%.*} ../../exec
rm -f *.o *.mod

