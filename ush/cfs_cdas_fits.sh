#!/bin/ksh
if [ $# -ne 6 ] ; then
  echo "Usage: $0 CDATE (yyyymmddhh) PRPO COMOUT DATA fh1 fh2"
  err_exit 1
fi

set -xeua

export cfss=${cfss:-/cfs}
export cfsp=${cfsp:-/cfs}

export CDATE=$1
export PRPO=$2
export COMOUT=$3
export DATAX=$4
export fh1=$5
export fh2=$6

prfile=$DATAX/fits.$CDATE
> $prfile

list='raob sfc acft acar'
for sub in $list ; do
  cp $PRPO fort.11
  ln -sf f$fh1.$sub.$CDATE fort.51
  ln -sf f$fh2.$sub.$CDATE fort.52
  $EXECcfs/${cfsp}$sub > $prfile
  export err=$?; err_chk
  mv f$fh1.$sub.$CDATE $COMOUT/.
  mv f$fh2.$sub.$CDATE $COMOUT/.

  if [ "$CHGRP_RSTPROD" = 'YES' ]; then
    chgrp rstprod $COMOUT/f$fh1.$sub.$CDATE
    chgrp rstprod $COMOUT/f$fh2.$sub.$CDATE
    chmod 640 $COMOUT/f$fh1.$sub.$CDATE
    chmod 640 $COMOUT/f$fh2.$sub.$CDATE
  fi
done

