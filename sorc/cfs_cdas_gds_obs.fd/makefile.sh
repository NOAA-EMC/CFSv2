#!/bin/bash
set -eua

export FC=ftn   

make -f Makefile

set -x; exec=../../exec

mv cfs_cdas_cmbDysPrf4  $exec
mv cfs_cdas_cmbDysPrfs4 $exec
mv cfs_cdas_mkEvNc4r    $exec

rm -f *.o *.mod

