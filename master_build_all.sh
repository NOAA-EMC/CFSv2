#!/usr/bin/env bash
set -euax

export REPOROOT=$PWD  
export WRFPATH=/gpfs/dell2/emc/modeling/noscrub/Jack.Woollen/wrf_shared.v1.1.0

[[ $# -eq 0 || $1 = 206 ]] && { cd $REPOROOT//global_shared_cfs/CRTMv206; build; }

[[ $# -eq 0 || $1 = cfs ]] && { cd $REPOROOT/sorc; mkdir -p ../exec; build_all.dell; }

[[ $# -eq 0 || $1 = shr ]] && { cd $REPOROOT/global_shared_cfs/sorc; mkdir -p ../exec; build_cfs_wcoss.sh; }

[[ $# -eq 0 || $1 = prp ]] && { cd $REPOROOT/obsproc_prep.v3.2.0/sorc; mkdir -p ../exec; build_dell; }

[[ $# -eq 0 || $1 = nst ]] && { cd $REPOROOT/nstrtg.v1.1.1/sorc; mkdir -p ../exec; build_nstrtg.sh; }

[[ $# -eq 0 || $1 = jis ]] && { cd $REPOROOT/EMC_land_utilities/sorc; mkdir -p ../exec; build_emcsfc.sh; }

[[ $# -eq 0 || $1 = f2o ]] && { cd $REPOROOT/Fit2Obs/sorc; mkdir -p ../exec; makes dell; }

