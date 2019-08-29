#!/usr/bin/env bash
set -euax

export REPOROOT=$PWD  
export WRFPATH=/gpfs/dell2/emc/modeling/noscrub/Jack.Woollen/wrf_shared.v1.1.0

[[ $# -eq 0 || $1 = cfs ]] && { cd $REPOROOT/sorc; mkdir -p ../exec; build_all.dell; }

[[ $# -eq 0 || $1 = shr ]] && { cd $REPOROOT/global_shared_cfs/sorc; mkdir -p ../exec; build_cfs_wcoss.sh; }

[[ $# -eq 0 || $1 = prp ]] && { cd $REPOROOT/obsproc_prep.v3.2.0/sorc; mkdir -p ../exec; build_dell; }