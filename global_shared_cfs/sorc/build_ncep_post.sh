#!/usr/bin/env bash

####################################################################################################
#
# post using module compile standard
#
# 10/15 Lin Gan:        Create module load version
# 01/16 Lin Gan:	Update to use GFS Vertical Structure
#
#####################################################################################################
#####################################################################################################


# Lin Gan modifiy to use NCO vertical structure prefix for NCO deployment - 20160131
set +x
source corn-mods.sh
module list
set -x

REPOROOT=$(cd ../..;pwd)
CRTM_LIB=$REPOROOT/global_shared_cfs/CRTMv206/libcrtm_v2.0.6.a
CRTM_INC=$REPOROOT/global_shared_cfs/CRTMv206/incmod/crtm_v2.0.6

cd ncep_post.fd

make -f makefile_wcoss_module clean
make -f makefile_wcoss_module; mv ncep_post ../../exec/
make -f makefile_wcoss_module clean

