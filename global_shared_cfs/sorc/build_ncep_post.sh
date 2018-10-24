SHELL=/bin/sh

####################################################################################################
#
# post using module compile standard
#
# 10/15 Lin Gan:        Create module load version
# 01/16 Lin Gan:	Update to use GFS Vertical Structure
#
#####################################################################################################
#####################################################################################################


# Lin Gan Module Load
module purge

# Lin Gan modifiy to use NCO vertical structure prefix for NCO deployment - 20160131
#module load ../modulefiles/post/v7.0.0
source dell-mods.sh
module list

CRTM_LIB=/gpfs/dell2/emc/modeling/noscrub/Jack.Woollen/CRTMv206/libcrtm_v2.0.6.a
CRTM_INC=/gpfs/dell2/emc/modeling/noscrub/Jack.Woollen/CRTMv206/incmod/crtm_v2.0.6

cd ncep_post.fd
make -f makefile_wcoss_module clean
make -f makefile_wcoss_module 
mv ncep_post ../../exec/

make -f makefile_wcoss_module clean

