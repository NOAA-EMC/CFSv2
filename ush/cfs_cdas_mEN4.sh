#!/bin/ksh
###########################################################
# This script computes an estimate of the background error
#  variance.  The variance is a function of locality and
#  is updated for each run.
###########################################################
set -x

export cfss=${cfss:-"/cfs"}
export FIX_OCN=${2:-${FIX_OCN:-$HOMEcfs/fix/${cfsp}fix_om}}
export COMROT=${3:-${COMROT:-$(pwd)}}
#
export PERR=${PERR:-""}
#

#rm *.g3

#
# The date of the cureent run is contained in the ascii file
#  run_date.
#
dte=${CDATE:-`cat  run_date`}
#read dte < run_date

export OCNGES=${4:-${OCNGES:-$COMROT/RESTART/$dte.tar}}

#tar -xvf $RESDIR/$dte.tar ocean_temp_salt.res.nc
tar -xvf $OCNGES ${dte}ocean_temp_salt.res.nc
tar -xvf $OCNGES ${dte}coupler.res

if [ ! -f ${dte}ocean_temp_salt.res.nc ]; then
  echo "Missing ocean_temp_salt.res.nc file"
  echo "Cannot compute background VAR"
  export err=99; err_chk
fi

####################################################################
# The executable mkEvNc4r models the local error variance
#  for GODAS temperature on the local vertical gradient of
#  temperature.  It uses the temperature from a recent
#  time mean netCDF output file from GODAS (*.ocean_TS.nc).
# The result is written to an ieee file, tvv.mom.  A 
#  comparable file for the salinity error variance is based
#  on tvv.mom and written to svv.mom.
#
# Here a link is made to the most recent *.ocean_TS.nc time
#  mean archive file.
####################################################################

ln -sf ${dte}ocean_temp_salt.res.nc ocean_temp_salt.res.nc
ln -sf $PARMcfs/cfs_parm_om/bg_input.nml  fort.10
ln -sf ${dte}coupler.res                  fort.11
ln -sf tvv.mom                            fort.51
ln -sf svv.mom                            fort.52

ln -sf $GRIDSPEC                 grid_spec.nc     ## MOM grid spec file            
ln -sf $FIX_OCN/TanF_WOA09_M4.nc TanF_WOA09_M4.nc ## new parm file for mkEvNc4r
ln -sf $FIX_OCN/M4StdErr.nc      M4StdErr.nc      ## new fix  file for mkEvNc4r

${mkEvNc4r:-$EXECcfs/cfs_cdas_mkEvNc4r}; export err=$?; err_chk

####################################################################
#$mkEvNc4r -f TS.nc -o $tvv_mom -gr gs.nc -d $dte -p -Sv -so $svv_mom
#
#  This second use of mkEvNc4r is optional; it makes tvv.g3 and
#  svv.g3, which are versions of tvv.mom and svv.mom that are
#  compatable with a graphical viewer.
#
#tvv.g3=${tvv.g3:-$(pwd)/tvv.g3}
#svv.g3=${svv.g3:-$(pwd)/svv.g3}
#$mkEvNc4r -f TS.nc -o $tvv.g3 -p -Sv -so $svv.g3 -g
####################################################################

rm -f TS.nc
rm -f ${dte}ocean_temp_salt.res.nc
rm -f gs.nc
exit $?

