#!/bin/sh

set -x

module load prod_util/1.1.0
module load grib_util/1.1.0

export RUN_ENVIR="emc"
export envir=prod
export NWROOT=/nw${envir}
#export cyc=00
export cyc=20
export job=snowgrib_00${cyc}
export jobid=${job}.$$

export DATAROOT=/gpfs/dell1/stmp/$LOGNAME
export DATA=$DATAROOT/grib_snowgrib
export jlogfile=/gpfs/dell1/stmp/$LOGNAME/jlogfile
export DCOMROOT=/gpfs/tp1/nco/ops/dcom
export COMROOT=/com

export FIXgrib=/gpfs/dell2/emc/modeling/noscrub/George.Gayno/landutil.git/jisni_port/fix
export EXECgrib=/gpfs/dell2/emc/modeling/noscrub/George.Gayno/landutil.git/EMC_land_utilities/exec

export EXSNOWGRIBSH=/gpfs/dell2/emc/modeling/noscrub/George.Gayno/landutil.git/EMC_land_utilities/scripts/exsnowgrib.sh.ecf
export SENDCOM=NO
export SENDDBN_GB2=NO

export KEEPDATA=YES

../../jobs/JISNI

exit 0
