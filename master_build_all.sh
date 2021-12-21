#!/usr/bin/env bash
set -euax

export HOMEcfs=$PWD; source $HOMEcfs/versions/build.ver

[[ $# -eq 0 || $1 = 206 ]] && { cd $HOMEcfs/global_shared_cfs/CRTMv206; ./build; }

[[ $# -eq 0 || $1 = cfs ]] && { cd $HOMEcfs/sorc; mkdir -p ../exec; ./build_all.wcoss2; }

[[ $# -eq 0 || $1 = shr ]] && { cd $HOMEcfs/global_shared_cfs/sorc; mkdir -p ../exec; ./build_cfs_wcoss.sh; }

###[[ $# -eq 0 || $1 = prp ]] && { cd $HOMEcfs/obsproc_prep.v3.2.0/sorc; mkdir -p ../exec; ./build_dell; }

[[ $# -eq 0 || $1 = nst ]] && { cd $HOMEcfs/nstrtg.v1.1.1/sorc; mkdir -p ../exec; ./build_nstrtg.sh; }

[[ $# -eq 0 || $1 = jis ]] && { cd $HOMEcfs/EMC_land_utilities/sorc; mkdir -p ../exec; ./build_emcsfc.sh; }

[[ $# -eq 0 || $1 = f2o ]] && { cd $HOMEcfs/Fit2Obs/sorc; mkdir -p ../exec; ./makes wcoss2; }

