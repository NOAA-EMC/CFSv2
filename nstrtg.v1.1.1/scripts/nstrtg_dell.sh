#!/bin/bash
#
# Abstract: This utility generates a RTG-like SST analysis file with 12Z NSST analysis in FV3GFS.
#
# Notes:
#       RTG-like SST data file: SST analysis over water grids, derived surface temperature over non-water grids.
#                               1/12 lat/lon grids, 4320 x 2160
# Input files :
#       sfcanl      : 6-hourly GFS NSST foundation temperature analysis (tref) and others like surface mask ( land), T1534 Gaussian grids (3072 x 1536)
#       iceanl      : 6-hourly global sea ice analysis, grib (1/12 degree, 4320 x 2160) 
#       rtgmsk      : Surface land/water mask 
#       sstclm      : SST climatology (monthly, RTG now)
#       salclm      : Salinity climatology (Monthly, Levitus), 1 degree lat/lon grids, 360 x 180
                                   
# Output file :
#       tf_rtg_grb1     : RTG-like Tf analysis with non-water grids filled as RTG, GRIB1
#       tf_rtg_grb2     : RTG-like Tf analysis with non-water grids filled as RTG, GRIB2
#
# Author, 03/21/2019     - Xu Li
#


set -euax

cwd=`pwd`

set +x
##. $MODULESHOME/init/ksh

module purge
module load ips/18.0.1.163
module load impi/18.0.1

module load prod_util/1.1.2
module load NetCDF/4.5.0
module load nemsio/2.2.3
module load w3nco/2.0.6
module load bacio/2.0.2

module list
set -x

# ndate=/u/Xu.Li/bin/ndate

  FixDir=/gpfs/dell2/emc/modeling/noscrub/Xu.Li/nstrtg.v1.1.1/fix
# PTMP=/gpfs/dell2/ptmp/Xu.Li
  PTMP=/gpfs/dell2/ptmp/$LOGNAME 

  cdump=gdas
  exp=prfv3rt3b
#
# logical variable if fill land grids or not
#
  lputsi=.true.
  dsearch=500.0
#
# work directory
#
  WorkDir=$PTMP/nstrtg
#
# sfcanl directory
#
  SfcDir=$PTMP/diag/data/$exp; mkdir -p $SfcDir
#
# monthly SST climatology (1/12 degree RTG is used here)
#
  sstclm=$FixDir/RTGSST.1982.2012.monthly.clim.grb
#
# Salinity climatology, half lat/lon degree
#
  salclm=$FixDir/WOA05_pottemp_salt-GRADS.nc
#
# executable
#
NSTRTG=/gpfs/dell2/emc/modeling/noscrub/Xu.Li/nstrtg.v1.1.1/exec/nstrtg.x

  rm -rf $WorkDir

  mkdir -p $WorkDir

  cd $WorkDir

  ln -s $sstclm    sstclm
  ln -s $salclm    salclm


  btime=2019072200
  etime=2019072200
  incr=24

  atime=$btime

  while [[ atime -le $etime ]]; do

  adate=`echo $atime | cut -c1-8`
  ahh=`echo $atime | cut -c9-10`
#
# input FV3GFS 6-hourlay surface analysis file at Gaussian grids, for T1534, 3072 x 1536
#
  sfcanl=/gpfs/dell1/nco/ops/com/gfs/prod/gdas.$adate/$ahh/gdas.t${ahh}z.sfcanl.nemsio

  ln -fs $sfcanl sfcanl
#
# processing 1/12 degree RTG-like file
#
#    Make namelist for CASE
  cat << EOF > nstrtg_parm.input
  &setup
   catime='$atime',lputsi=$lputsi,dsearch=$dsearch,nx=4320,ny=2160
/
EOF
#
# Surface land/water mask 1/12 lat/lon grids, 4320 x 2160
#
  rtgmsk=$FixDir/rtgssthr_ls_nas.twelfthdeg.dat
#
# input Sea ice daily analysis, 1/12 lat/lon grids, 4320 x 2160
#
  iceanl=/gpfs/tp1/emc/globaldump/$atime/gdas/seaice.5min.grb.gdas.$atime
  ln -fs $iceanl iceanl
  ln -fs $rtgmsk rtgmsk
# iceanl=/scratch4/NCEPDEV/global/noscrub/dump/2018102200/gdas/seaice.5min.blend.grb.gdas.2018102200
#
# output RTG-like Tf analysis file (grib1)
#

 if [ $lputsi = ".true." ]; then
    ln -fs $SfcDir/rtgsstgrb0.083.$cdump.$atime tf_rtg_grb
    ln -fs $SfcDir/rtgsstgrb0.083_awips.$cdump.$atime tf_rtg_grb_awips
    ln -fs /dev/null                                  tf_rtg_grb_awips
 else
    ln -fs $SfcDir/rtgsstgrb0.083_nlf.$cdump.$atime tf_rtg_grb
    ln -fs $SfcDir/rtgsstgrb0.083_nlf_awips.$cdump.$atime tf_rtg_grb_awips
    ln -fs /dev/null                                      tf_rtg_grb_awips
 fi

 time $NSTRTG < nstrtg_parm.input ###1>res.log 2>res.err

exit 

#
# processing 1/2 degree RTG-like file
#
#    Make namelist for CASE
  cat << EOF > nstrtg_parm.input
  &setup
   catime='$atime',lputsi=$lputsi,dsearch=$dsearch,nx=720,ny=360
/
EOF
#
# input Sea ice daily analysis, 1/2 lat/lon grids, 720 x 360
#
# iceanl=/gpfs/gp1/emc/globaldump/$atime/gdas/icegrb.gdas.$atime
# iceanl=/gpfs/tp1/emc/globaldump/$atime/gdas/icegrb.gdas.$atime
#
# Surface land/water mask 1/12 lat/lon grids, 4320 x 2160
#
  rtgmsk=$FixDir/rtgssthr_ls_nas.halfdeg.dat

  ln -fs $iceanl iceanl
  ln -fs $rtgmsk rtgmsk
#
# output RTG-like Tf analysis file (grib1)
#

 if [ $lputsi = ".true." ]; then
    ln -fs $SfcDir/rtgsstgrb0.5.$cdump.$atime tf_rtg_grb
    ln -fs $SfcDir/rtgsstgrb0.5_awips.$cdump.$atime tf_rtg_grb_awips
 else
    ln -fs $SfcDir/rtgsstgrb0.5_nlf.$cdump.$atime tf_rtg_grb
    ln -fs $SfcDir/rtgsstgrb0.5_awips_nlf.$cdump.$atime tf_rtg_grb_awips
 fi

 $NSTRTG < nstrtg_parm.input 1>res_05.log 2>res_05.err


#atime=`$ndate ${incr} $atime`

 atime=`$NDATE ${incr} $atime`

 done

