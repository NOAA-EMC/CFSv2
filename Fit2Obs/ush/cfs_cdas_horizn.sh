#!/bin/ksh
set -xeua
if [ $# -ne 5 ] ; then
  echo "Usage: $0 date (yyyymmddhh) prepqm outdir prtdir typ"
  exit 1
fi

export HOMEcfs=${HOMEcfs:-/nwprod}
export USHcfs=${USHcfs:-$HOMEcfs/ush}
export EXECf2o=$HOMEcfs/Fit2Obs/exec

export date=$1
export prepqm=$2
export outdir=$3
export prtdir=$4
export typ=$5

export infile=$prepqm


##  iall=0 gives only obs value and quality mark
##  iall=1 gives only obs,anl,ges and quality mark
export iall=1

export idbug=0
export idbuga=0

export flonw=0.0
export flone=360.0
export flats=-90.0
export flatn=90.0

yyyy=`echo $date | cut -c1-4`
yy=`echo $date | cut -c3-4`
mm=`echo $date | cut -c5-6`
mon=`$USHcfs/cfs_cmon.sh $mm`
dd=`echo $date | cut -c7-8`
hh=`echo $date | cut -c9-10`
odate=$yy$mm$dd$hh

cd $outdir

for name in ADPSFC SFCSHP ADPUPA AIRCFT AIRCAR
do

export catname=$name
export oname=` echo $catname| tr "[A-Z]" "[a-z]" `

#####################################################################
#                  ADPSFC
#####################################################################
if [ $catname = "ADPSFC" ] ; then
nst=50000

export imas=1
export iwnd=0
export sfc=1
for hr in 0
do
export fchr=$hr
export fdhr=0.5
export dtg=`$EXECf2o/ndate $hr $date`
$USHcfs/cfs_horizn.sh $oname $nst $iall $imas $iwnd $fchr $fdhr $dtg $sfc
done
fi

#####################################################################
#                  SFCSHP
#####################################################################
if [ $catname = "SFCSHP" ] ; then
export nst=50000
export imas=1
export iwnd=1
export sfc=1
for hr in 0
do
export fchr=$hr
export fdhr=0.5
export dtg=`$EXECf2o/ndate $hr $date`
$USHcfs/cfs_horizn.sh $oname $nst $iall $imas $iwnd $fchr $fdhr $dtg $sfc
done
fi

#####################################################################
#                  ADPUPA
#####################################################################
if [ $catname = "ADPUPA" ] ; then
export imas=1
export iwnd=1
export sfc=2
##   mandatory level data...
export nst=50000
export iprs=1
export ilev=21
export itol=0
for hr in 0
do
export fchr=$hr
export fdhr=4.0
export dtg=`$EXECf2o/ndate $hr $date`
$USHcfs/cfs_horizn.sh $oname $nst $iall $imas $iwnd $fchr $fdhr $dtg $sfc
done
fi

#####################################################################
#                  AIRCFT
#####################################################################
if [ $catname = "AIRCFT" ] ; then
export nst=50000
export imas=1
export iwnd=1
export sfc=3
for hr in 0
do
export fchr=$hr
export fdhr=4.0
export dtg=`$EXECf2o/ndate $hr $date`
$USHcfs/cfs_horizn.sh $oname $nst $iall $imas $iwnd $fchr $fdhr $dtg $sfc
done
fi

#####################################################################
#                  AIRCAR
#####################################################################
if [ $catname = "AIRCAR" ] ; then
export nst=50000
export imas=1
export iwnd=1
export sfc=3
for hr in 0
do
export fchr=$hr
export fdhr=4.0
export dtg=`$EXECf2o/ndate $hr $date`
$USHcfs/cfs_horizn.sh $oname $nst $iall $imas $iwnd $fchr $fdhr $dtg $sfc
done
fi

#####################################################################
##  close catname-loop
done
