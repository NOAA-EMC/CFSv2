#!/usr/bin/env bash
set -euax

##################################################################
# gsm using module compile standard
# 01/26/2016 Fanglin.Yang@noaa.gov:    Create module load version
##################################################################

curdir=$PWD; mkdir -p ../exec

cd ${curdir}/global_chgres.fd
./makefile.sh

cd ${curdir}/global_cycle.fd
./makefile_wcoss.sh   

cd ${curdir}/global_sfchdr.fd
./makefile_wcoss.sh

cd ${curdir}/global_sighdr.fd
./makefile_wcoss.sh


