#!/bin/bash
set -eua

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

make -f Makefile clean
make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod

