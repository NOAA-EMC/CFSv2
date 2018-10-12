#!/usr/bin/env bash 
set -x
set -e    # fail if an error is hit so that errors do not go unnoticed

set +x
module purge
module load ics/12.1
module load w3nco/v2.0.6       
module load bufr/v11.1.0   
module load w3emc/v2.2.0     
module load bacio/v2.0.1    
module load sigio/v1.0.1        
module load sp/v2.0.2  
module list
set -x

if [ $# -eq 0 ]; then
  dir_list=*.fd
else
  dir_list=$*
fi

for sdir in $dir_list; do
 cd $sdir
 make clean; make; make install; make clean
 ls -l
 cd ..
done


