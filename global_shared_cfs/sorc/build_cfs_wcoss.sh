#!/usr/bin/env bash
set -euax

##############################################################################################
# this script will build the global shared codes used by the portable version of the cfs model
# it should be run ifrom the sorc directory of the global_shared_cfs structure
##############################################################################################

mkdir -p ../exec

echo
./build_ncep_post.sh
echo
./build_tropcy.sh
echo
./build_gsm_wcoss.sh
