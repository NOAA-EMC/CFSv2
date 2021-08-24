#! /usr/bin/env bash
set -eux

mkdir -p ../exec # place for executables

set +x
module purge
source ../modulefiles/nstrtg.wcoss2 
module list
set -x

cd ./nstrtg.fd
./makefile.sh
