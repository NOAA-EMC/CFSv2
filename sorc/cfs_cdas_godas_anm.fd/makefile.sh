#!/bin/bash
#
# compile script for godas

set -eux

# GODAS
# -----

echo
platform=intel.WCOSS                        # A unique platform identifier
nameG=gdsSOLO_mom5                        # Name for GODAS executable

root=`pwd`
code_dir=$root                        # source code directory
execdirG=$root/exec/$nameG
executableG=$root/exec/cfs_cdas_$nameG  # executable name
mkmfTemplate=$root/bin/mkmf.template.$platform # path to template for your platform
# mppnccombine=$root/bin/mppnccombine.$platform
mkmf=$root/bin/mkmf                       # path to executable mkmf
pathnames=$root/path_names              # path to file containing list of source paths
cppDefs="-DENABLE_GDS -Duse_netCDF -Duse_netCDF3 -Duse_libMPI"

#-------------------------------------------------------------------------------
# setup directory structure
  rm -rf  $execdirG;  mkdir  -p $execdirG
#-------------------------------------------------------------------------------
# compile the model code and create executable

  cd $execdirG; echo `pwd`

  $mkmf -a $code_dir -t $mkmfTemplate -p $executableG -c "$cppDefs" $pathnames $root/include $code_dir/shared/mpp/include 

  make clean  
  make; mv $executableG $root/../../exec
  make clean    

  cd $root; rm -rf exec 
