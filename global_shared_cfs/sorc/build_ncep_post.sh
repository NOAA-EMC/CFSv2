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
set -euax

# Lin Gan modifiy to use NCO vertical structure prefix for NCO deployment - 20160131

cd ncep_post.fd

make -f makefile_wcoss_module clean
make -f makefile_wcoss_module; mv ncep_post ../../exec/
make -f makefile_wcoss_module clean

