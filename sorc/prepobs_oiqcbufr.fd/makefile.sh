#! /usr/bin/env bash  

set -eua

module list; set -x

echo; make=`basename $PWD`
echo make-ing ${make%.*}
echo

export FC=mpiifort

make -f Makefile
mv ${make%.*} ../../exec
rm -f *.o *.mod

