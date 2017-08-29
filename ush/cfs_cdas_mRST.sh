#!/bin/ksh
set -x

[[ $machine = WCRAY ]] && export aprun="aprun -n1" || aprun=""

#if [ $# -lt 5 ] ; then echo "Usage: $0 CDATE SST_DIR SSS_DIR omres GRIDSPEC" ;exit 1 ;fi
#
#  This script creates sst and sss restore datasets for  MOM4 GODAS
#
#  Original GODAS Author : Dave Behringer
#  Adapted and merged for CFS : S. Moorthi
#
export cfss=${cfss:-"/cfs"}
export cfsp=${cfsp:-"cfs_"}
export cfsg=${cfsg:-"cfs_cdas_godas_"}
export FIX_OCN=${FIX_OCN:-$HOMEcfs/fix/${cfsp}fix_om}
export PERR=${PERR:-""}

CDATE=${1:-${CDATE}}
SST_DIR=${2:-${SST_DIR:-$(pwd)}}
SSS_DIR=${3:-${SSS_DIR:-$FIX_OCN}}
omres=${4:-${omres:-05}}
GRIDSPEC=${5:-${GRIDSPEC:-$FIX_OCN/grid_spec_$omres.nc}}
mk1DySst4i=${mk1DySst4i:-$EXECcfs/${cfsg}mk1DySst4i}
mk1DySss4i=${mk1DySss4i:-$EXECcfs/${cfsg}mk1DySss4i}
mkDlyTclm=${mkDlyTclm:-$EXECcfs/${cfsg}mkDlyTclm}

touch temp_sfc_restore.nc
rm temp_sfc_restore.nc
touch salt_sfc_restore.nc
rm salt_sfc_restore.nc

#read dte < run_date
dte=$(echo ${CDATE:-`cat  $expdir/run_date`} | cut -c1-8)
echo $dte

ln -sf $GRIDSPEC                grid_spec.nc
ln -sf $FIX_OCN/TFM4_WOA09.nc   TFM4_WOA09.nc
ln -sf $FIX_OCN/SFM4_WOA09.nc   SFM4_WOA09.nc
ln -sf $SSS_DIR/salt12.nc salt12.nc

echo "Making temp_sfc_restore.nc"
$aprun $mk1DySst4i -p $SST_DIR -g grid_spec.nc -d $dte -o temp_sfc_restore.nc -y 65
export err=$?; err_chk

echo "Making salt_sfc_restore.nc"
$aprun $mk1DySss4i -f salt12.nc -g grid_spec.nc -d $dte -o salt_sfc_restore.nc -y 65
export err=$?; err_chk

# turn these off per DaveB #
#echo "Making temp_z_restore.nc"
#$mkDlyTclm -f TFM4_WOA09.nc -g grid_spec.nc -d $dte -o temp_z_restore.nc
#export err=$?; err_chk
#
#echo "Making salt_z_restore.nc"
#$mkDlyTclm -f SFM4_WOA09.nc -g grid_spec.nc -d $dte -o salt_z_restore.nc
#export err=$?; err_chk
# turn these off per DaveB #

rm -f salt12.nc TFM4_WOA09.nc SFM4_WOA09.nc
