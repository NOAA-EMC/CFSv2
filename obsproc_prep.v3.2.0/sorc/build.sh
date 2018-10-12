#!/usr/bin/env bash 
set -x
set -e    # fail if an error is hit so that errors do not go unnoticed

if [ $# -eq 0 ]; then
  dir_list=*.fd
else
  dir_list=$*
fi
echo $dir_list

export W3NCO_LIB4=/nwprod/lib/w3nco/v2.0.6/libw3nco_v2.0.6_4.a
export W3EMC_LIB4=/nwprod/lib/w3emc/v2.2.0/libw3emc_v2.2.0_4.a
export BACIO_LIB4=/nwprod/lib/bacio/v2.0.1/libbacio_v2.0.1_4.a
export SIGIO_LIB4=/nwprod/lib/sigio/v1.0.1/libsigio_v1.0.1_4.a
export SP_LIB4=/nwprod/lib/sp/v2.0.2/libsp_v2.0.2_4.a

for sdir in $dir_list; do
 dir=${sdir%\/}; cd $dir  # chop trailing slash if necessary

 if [[ $dir = prepobs_prepdata.fd ]]; then 
   BUFR_LIB4=/nwprod2/lib/bufr/v11.1.0/libbufr_v11.1.0_4_64.a
 else
   BUFR_LIB4=/nwprod/lib/bufr/v10.2.5/libbufr_v10.2.5_4_64.a
 fi

 make clean; make install; make clean
 ls -l
 cd ..
done


