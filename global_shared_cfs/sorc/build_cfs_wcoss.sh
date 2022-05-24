#!/usr/bin/env bash
set -euax

[[ $# -eq 1 ]] && build=$1 || build=all

set +x
module reset
source $HOMEcfs/versions/build.ver
module load PrgEnv-intel/${PrgEnv_intel_ver}
module load craype/${craype_ver}
module load intel/${intel_ver}
module load cray-mpich/${cray_mpich_ver}
module load libjpeg/${libjpeg_ver}
module load bacio/${bacio_ver}
module load sp/${sp_ver}
module load w3emc/${w3emc_ver}
module load w3nco/${w3nco_ver}
module load bufr/${bufr_ver}
module load ip/${ip_ver}
module load g2/${g2_ver}
module load g2tmpl/${g2tmpl_ver}
module load libpng/${libpng_ver}
module load landsfcutil/${landsfcutil_ver} 
module load libxmlparse/${libxmlparse_ver}
module load jasper/${jasper_ver}
module load zlib/${zlib_ver}
module load sigio/${sigio_ver}
module load sfcio/${sfcio_ver}
module load gfsio/${gfsio_ver}
module load netcdf/${netcdf3_ver} 
module use  $HOMEcfs/modules 
module load crtm_${crtm_ver} 
module load wrflib_${wrflib_ver} 
module list
set -x


##############################################################################################
# this script will build the global shared codes used by the portable version of the cfs model
# it should be run ifrom the sorc directory of the global_shared_cfs structure
##############################################################################################

mkdir -p ../exec

[[ $build = trop || $build = all ]] && { echo; ./build_tropcy.sh;    } 

[[ $build = gsm  || $build = all ]] && { echo; ./build_gsm_wcoss.sh; } 

[[ $build = post || $build = all ]] && { echo; ./build_ncep_post.sh; }  

