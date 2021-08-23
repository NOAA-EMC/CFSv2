#!/bin/bash
set -x

export FC=ftn

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile clean
make -f Makefile
mv ${make%.*} ../../exec
make -f Makefile clean


