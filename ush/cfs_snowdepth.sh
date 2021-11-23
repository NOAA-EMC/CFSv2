#!/usr/bin/env bash
set -uax


######################################
# Specify directory paths for this job
######################################

export TANKDIR=${DCOMROOT}

export HOMEgrib=$HOMEcfs/EMC_land_utilities
export EXECgrib=$HOMEgrib/exec
export USHgrib=$HOMEgrib/ush
export FIXgrib=$HOMEgrib/fix

# run the JISNI procedure
# -----------------------

$HOMEgrib/scripts/exsnowgrib.sh

# copy snowdepth files to cfs filenames
# -------------------------------------

cp $COMOUT/snowdepth.global.grb  $COMOUT/${RUN}1.${cycle}.snogrb
cp $COMOUT/snowdepth.t574.grb    $COMOUT/${RUN}1.${cycle}.snogrb_t574
cp $COMOUT/snowdepth.t382.grb    $COMOUT/${RUN}1.${cycle}.snogrb_t382

