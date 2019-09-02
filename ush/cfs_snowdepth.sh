#!/usr/bin/env bash
set -euax


######################################
# Specify directory paths for this job
######################################
export DCOMROOT=${DCOMROOT:-/dcom}   #Luke adds it for test
export USERDIR=${DCOMROOT}
export OUTDIR=${DCOMROOT}
export TANKDIR=${DCOMROOT}
export OUTDIR=${DCOMROOT}

export HOMEgrib=${HOMEgrib:-$HOMEcfs/EMC_land_utilities}
export USHgrib=${USHgrib:-$HOMEgrib/ush}
export EXECgrib=${EXECgrib:-$HOMEgrib/exec}
export FIXgrib=${FIXgrib:-$HOMEgrib/fix}


$HOMEgrib/scripts/exsnowgrib.sh.ecf
