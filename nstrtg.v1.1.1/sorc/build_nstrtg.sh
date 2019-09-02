#! /usr/bin/env bash
set -eux

mkdir -p ../exec # place for executables

module purge
source ../modulefiles/nstrtg.wcoss_dell_p3  
module list

cd ./nstrtg.fd
./makefile.sh
