#!/bin/ksh
set -x

YMDH=$1
outdir=$2
nknd=${3:-2}

hpsstar=$USHcfs/cfs_hpsstar  

CENTER=${CENTER:-EMC}       # The hpss archive. EMC, EMC2, or NCDC

cd $outdir

rc=0

sigfile=siganl.gdas$nknd.$YMDH
sfcfile=sfcanl.gdas$nknd.$YMDH
ocnfile=ocnanl.gdas$nknd.$YMDH.tar

now=`date`
echo "Retrieving ICs for $YMDH ... now: $now"

yyyymm=`echo $YMDH | cut -c1-6`
yyyymmdd=`echo $YMDH | cut -c1-8`

hpssdir=/climate/hpss/CFSRR/$CENTER/Reanalysis/CFS_HIC
hpssfile=$hpssdir/$yyyymm/$CENTER.Reanalysis.CFS_HIC.$YMDH.tar
$hpsstar get $hpssfile ocnanl.gdas.$YMDH.tar
((rc+=$?))
/bin/mv ocnanl.gdas.$YMDH.tar $ocnfile

hpssdir=/climate/hpss/CFSRR/$CENTER/Reanalysis/CFS_LIC
hpssfile=$hpssdir/$yyyymm/$CENTER.Reanalysis.CFS_LIC.$yyyymmdd.tar

$hpsstar get $hpssfile $sigfile $sfcfile
((rc+=$?))

now=`date`

if [[ $rc -ne 0 ]] ; then
  echo "ERROR: problem retrieving ICs for $YMDH ... $now"
else
  echo "OK: Completed retrieving ICs for $YMDH ... $now"
fi  
