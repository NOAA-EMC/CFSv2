#!/usr/bin/env bash
set -uax


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

# try yesterday if today is missing

if [[ $? -ne 0 ]]; then
  export PDY=$PDYm1
  $HOMEgrib/scripts/exsnowgrib.sh.ecf
fi
