SHELL=/bin/sh
set -euax

##################################################################
# gsm using module compile standard
# 01/26/2016 Fanglin.Yang@noaa.gov:    Create module load version
##################################################################

set +x
module purge
#module load ../modulefiles/gfs/gsm_v13.0.0
source dell-mods.sh
set -x

curdir=$PWD  

##cd ${curdir}/global_fcst.fd 
##makefile_wcoss.sh                    
##

cd ${curdir}/global_chgres.fd
makefile.sh

cd ${curdir}/global_cycle.fd
makefile_wcoss.sh   

cd ${curdir}/global_sfchdr.fd
makefile_wcoss.sh

cd ${curdir}/global_sighdr.fd
makefile_wcoss.sh


