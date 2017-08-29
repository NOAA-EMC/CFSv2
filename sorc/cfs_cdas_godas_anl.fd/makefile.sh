#!/bin/bash
#
# compile script for godas and regrid_2d
#

# GODAS
# -----

echo
platform=intel.WCOSS                        # A unique platform identifier
nameG=gdsSOLO                             # Name for GODAS executable

root=`pwd`
code_dir=$root/src                        # source code directory
execdirG=$root/exec/$nameG
executableG=$root/exec/cfs_cdas_$nameG  # executable name
mkmfTemplate=$root/bin/mkmf.template.$platform # path to template for your platform
# mppnccombine=$root/bin/mppnccombine.$platform
mkmf=$root/bin/mkmf                       # path to executable mkmf
pathnames=$root/path_names              # path to file containing list of source paths
cppDefs="-DENABLE_GDS -Duse_netCDF -Duse_libMPI"

#-------------------------------------------------------------------------------
# setup directory structure
  if [ ! -d $execdirG ] ; then   mkdir  -p $execdirG ; fi
#-------------------------------------------------------------------------------
# compile the model code and create executable
  cd $execdirG
  echo `pwd`
  $mkmf -a $code_dir -t $mkmfTemplate -p $executableG -c "$cppDefs" $pathnames $root/include $code_dir/shared/mpp/include 
# /usr/local/include

#######################################################################
# make the gdsSOLO executable
#######################################################################
  rm -f *.o *.mod
  make; mv $executableG $root/../../exec 
  rm -f *.o *.mod
#######################################################################

  cd $root
# 
# # regrid_2d
# # ---------
# 
 export  nameR=regrid_2d                          # Name for regrid code
 export  execdirR=$root/exec/$nameR
 #export  executableR=$root/exec/fms_$nameR     # executable name
 export  executableR=$root/exec/cfs_cdas_$nameR # executable name
 export  cppDefs="-Duse_netCDF -Duse_libMPI"
 export  sharedir=$code_dir/shared
 export  regridnames=$root/regrid_names
 export  srclist=$code_dir/preprocessing/$nameR/$nameR.f90
 
# #-------------------------------------------------------------------------------
# # setup directory structure
#   if [ ! -d $execdirR ] ; then   mkdir  -p $execdirR ; fi
# #-------------------------------------------------------------------------------
# # compile the model code and create executable
   cd $execdirR
   echo $(pwd)
 
   $mkmf -a $code_dir -t $mkmfTemplate -p $executableR -c "$cppDefs" $regridnames $root/include $code_dir/shared/mpp/include
# /usr/local/include
 
  rm -f *.o *.mod
  make; mv $executableR $root/../../exec 
  rm -f *.o *.mod

  cd $root; rm -rf exec Makefile .cppdefs

