#!/bin/ksh
if [ $# -ne 9 ] ; then
  echo "Usage: $0 oname nst iall imas iwnd fchr fdhr dtg sfc"
  exit 1
fi

set -xeua

export HOMEcfs=${HOMEcfs:-/nwprod}
export USHcfs=${USHcfs:-$HOMEcfs/ush}
export cfss=${cfss:-/cfs}
export cfsp=${cfsp:-cfs_}
export EXECf20=${EXECf20:-$HOMEcfs/Fit2Obs/exec}

export oname=$1
export nst=$2
export iall=$3
export imas=$4
export iwnd=$5
export fchr=$6
export fdhr=$7
export dtg=$8
export sfc=$9

yyyy=`echo $dtg | cut -c1-4`
yy=`echo $dtg | cut -c3-4`
mm=`echo $dtg | cut -c5-6`
mon=`$USHcfs/cfs_cmon.sh $mm`
dd=`echo $dtg | cut -c7-8`
hh=`echo $dtg | cut -c9-10`

export outfile=$outdir/$oname.$typ.$dtg
export prtfile=$prtdir/$oname.$typ.$dtg.out

export outname=$oname

if [ $sfc -eq 1 ] ; then
  $EXECf20/${cfsp}bufrslsfc > $prtfile
elif [ $sfc -eq 2 ] ; then
  if [ $iprs -eq 1 ] ; then
    outname=$oname.mand.$typ.$dtg
    outfile=$outdir/$outname
  elif [ $iprs -eq 0 ] ; then
    outname=$oname.mdlsig.$typ.$dtg
    outfile=$outdir/$outname
  elif [ $iprs -eq 2 ] ; then
    outname=$oname.all.$typ.$dtg
    outfile=$outdir/$outname
  fi
  $EXECf20/${cfsp}bufrslupao >> $prtfile
elif [ $sfc -eq 3 ] ; then
  $EXECf20/${cfsp}bufrslslev >> $prtfile
elif [ $sfc -eq 4 ] ; then
  $EXECf20/${cfsp}bufrsltovs >> $prtfile
elif [ $sfc -eq 5 ] ; then
  $EXECf20/${cfsp}bufrslssmi >> $prtfile
fi

###cat $prtfile

export err=$?; $DATA/err_chk

if [ "$CHGRP_RSTPROD" = 'YES' ]; then
  if [ -s $outfile ]; then
    chgrp rstprod $outfile
    chmod 640 $outfile
  fi
fi
set +e
grep 'number of stations 0' $prtfile
ese=$?
if [ $ese -eq 0 ] ; then
 echo "number of stations 0 for $dtg"
fi
set -e
