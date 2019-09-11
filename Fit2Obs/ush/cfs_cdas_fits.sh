#!/bin/ksh
if [ $# -ne 6 ] ; then
  echo "Usage: $0 CDATE (yyyymmddhh) PRPO COMOUT DATA fh1 fh2"
  exit 1
fi

set -xeua

export HOMEcfs=${HOMEcfs:-/nwprod}
export cfss=${cfss:-cfs}
export cfsp=${cfsp:-cfs_}
export EXECf2o=$HOMEcfs/Fit2Obs/exec

export CDATE=$1
export PRPO=$2
export COMOUT=$3
export DATA=$4
export fh1=$5
export fh2=$6

prfile=$DATA/fits.$CDATE
> $prfile

rm -f fort.*

list='raob sfc acft acar surf'
for sub in $list ; do
  ln -sf $PRPO             fort.11
  ln -sf f$fh1.$sub.$CDATE fort.51
  ln -sf f$fh2.$sub.$CDATE fort.52
  $EXECf2o/${cfsp}$sub > $prfile.${sub}.f${fh1}_f${fh2}
  export err=$?; $DATA/err_chk
  mv f$fh1.$sub.$CDATE $COMOUT/.
  mv f$fh2.$sub.$CDATE $COMOUT/.

  if [ "$CHGRP_RSTPROD" = 'YES' ]; then
    chgrp rstprod $COMOUT/f$fh1.$sub.$CDATE
    chgrp rstprod $COMOUT/f$fh2.$sub.$CDATE
    chmod 640 $COMOUT/f$fh1.$sub.$CDATE
    chmod 640 $COMOUT/f$fh2.$sub.$CDATE
  fi
done

