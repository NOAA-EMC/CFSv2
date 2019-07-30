#!/bin/bash
# compile mom4ice & coupler

set +x
echo
module unload impi/18.0.1
module load smpi/10.1.1.0
module list
echo
set -x

root=`pwd`

export  curdir=$root/compile
export  platform=intel.DELL 

export  code_dir=$root/src
export  name=cfs_ocean_mom4ice 
export  executable=$curdir/exec/$name
export  time_stamp=$root/bin/time_stamp.csh
export  mkmfTemplate=$root/bin/mkmf.template.$platform
export  mkmf=$root/bin/mkmf
export  pathnames=$curdir/path_names    # path to file containing list of source paths
export  cppDefs="-DENABLE_GDS -Duse_netCDF -Duse_libMPI"

#--------------------------------------------------------------------------------------------------------
# compile mppnccombine.c, needed only if $npes > 1
#--------------------------------------------------------------------------------------------------------
# setup directory structure
#--------------------------------------------------------------------------------------------------------
# compile the model code and create executable

if [ ! -s $executable ] ; then
  mkdir -p  $executable:h; cd $executable:h
  $mkmf -f -a $code_dir -t $mkmfTemplate -p $executable -c "$cppDefs" $pathnames $root/include $code_dir/shared/mpp/include /usr/local/include
  make -f Makefile; rm -f *.mod *.o
fi

cd $root; mv $executable ../../exec

rm -rf $executable:h
