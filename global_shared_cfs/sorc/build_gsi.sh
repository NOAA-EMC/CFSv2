#!/bin/sh
target=${1:-dell}
set -x -e
EXECdir=../exec
[ -d $EXECdir ] || mkdir $EXECdir

###. /usrx/local/Modules/3.2.10/init/sh
#module purge
#module load ../modulefiles/modulefile.global_gsi.$target
#module list

# dell modules

set +x
module purge
module load EnvVars/1.0.2
module load ips/18.0.1.163  impi/18.0.1
module list
module load ESMF/4_0_0rp2
module load bacio/2.0.2
module load nemsio/2.2.3
module load sp/2.0.2
module load w3emc/2.3.0
module load w3nco/2.0.6
module load NetCDF/3.6.3
module load bufr/11.3.0
module load ip/3.0.1
module load sfcio/1.0.0
module load sigio/2.0.1
module load landsfcutil/2.1.0
module load gfsio/1.1.0
module load crtm/2.2.5

module list
set -x

#cd gsi.fd
#./configure clean

export COMP_MP=mpiifort
export C_COMP_MP=mpiicc

##export CF=ifort
##export CC=icc   

##CRTM_LIB=/gpfs/dell2/emc/modeling/noscrub/Jack.Woollen/CRTMv206/libcrtm_v2.0.6.a
##CRTM_INC=/gpfs/dell2/emc/modeling/noscrub/Jack.Woollen/CRTMv206/incmod/crtm_v2.0.6

dlist="gsi.fd"
for dir in $dlist; do
 cd $dir
 ./configure clean
 ./configure $target
 make -f Makefile clean
 make -f Makefile -j 8
 cp -p global_gsi ../$EXECdir
 make -f Makefile clean
 ./configure clean
 cd ..
done
