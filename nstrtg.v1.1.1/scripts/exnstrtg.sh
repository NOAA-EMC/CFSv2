#!/bin/ksh 
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


set -x

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
  SfcDir=$PTMP/diag/data/$exp
#
# monthly SST climatology (1/12 degree RTG is used here)
#
  sstclm=$FIXnstrtg/RTGSST.1982.2012.monthly.clim.grb
#
# Salinity climatology, half lat/lon degree
#
  salclm=$FIXnstrtg/WOA05_pottemp_salt-GRADS.nc
#
# executable
#
 NSTRTG=$HOMEnstrtg/exec/nstrtg.x

  rm -rf $WorkDir

  mkdir -p $WorkDir

  cd $WorkDir

  ln -s $sstclm    sstclm
  ln -s $salclm    salclm


#
# input FV3GFS 6-hourlay surface analysis file at Gaussian grids, for T1534, 3072 x 1536
#
  sfcanl=$COMROOT/gfs/prod/gdas.$cdate/gdas.$cycle.sfcanl.nemsio

  ln -fs $sfcanl sfcanl
#
# input Sea ice daily analysis, 1/12 lat/lon grids, 4320 x 2160
#
  iceanl=/gpfs/tp1/emc/globaldump/$cdate/gdas/gdas.$cycle.seaice.5min.blend.grb
  ln -fs $iceanl iceanl
#
# processing 1/12 degree RTG-like file
#
#
# Surface land/water mask 1/12 lat/lon grids, 4320 x 2160
#
  rtgmsk=$FixDir/rtgssthr_ls_nas.twelfthdeg.dat
  ln -fs $rtgmsk rtgmsk
#
# output RTG-like Tf analysis file (grib1)
#

 ln -fs $COMOUT/rtgsstgrb0.083.$cdump.$atime tf_rtg_grb
 ln -fs $COMOUT/rtgsstgrb0.083_awips.$cdump.$atime tf_rtg_grb_awips

# Make namelist for CASE
  cat << EOF > nstrtg_parm.input
  &setup
   catime='$atime',lputsi=$lputsi,dsearch=$dsearch,nx=4320,ny=2160
/
EOF

 $NSTRTG < nstrtg_parm.input 1>res.log 2>res.err

#exit 

#
# processing 1/2 degree RTG-like file
#
#
# Surface land/water mask 1/12 lat/lon grids, 4320 x 2160
#
  rtgmsk=$FIXnstrtg/rtgssthr_ls_nas.halfdeg.dat
  ln -fs $rtgmsk rtgmsk
#
# output RTG-like Tf analysis file (grib1)
#

 ln -fs $COMOUT/rtgsstgrb0.5.$cdump.$atime tf_rtg_grb
 ln -fs $COMOUT/rtgsstgrb0.5_awips.$cdump.$atime tf_rtg_grb_awips
# Make namelist for CASE
  cat << EOF > nstrtg_parm.input
  &setup
   catime='$atime',lputsi=$lputsi,dsearch=$dsearch,nx=720,ny=360
/
EOF

 $NSTRTG < nstrtg_parm.input 1>res_05.log 2>res_05.err

$GRBINDEX rtgsstgrb0.083_awips rtgsstgrb0.083_awips.index
$GRBINDEX rtgsstgrb0.083 rtgsstgrb0.083.index
$GRBINDEX anomlygrb0.083 anomlygrb0.083.index
$GRBINDEX rtgsstgrb0.5_awips rtgsstgrb0.5_awips.index
$GRBINDEX rtgsstgrb0.5   rtgsstgrb0.5.index


#####################################################################
#
#     PROCESS GLOBAL SEA SURFACE TEMPERATURE (SST) FOR AWIPS
#
#####################################################################


$CNVGRIB -g12 -p40 rtgsstgrb0.083_awips  rtgsst_grb_0.083_awips.grib2
$WGRIB2 rtgsst_grb_0.083_awips.grib2 -s > rtgsst_grb_0.083_awips.grib2.idx

$CNVGRIB -g12 -p2 rtgsstgrb0.083_awips  rtgsst_grb_0.083_nomads.grib2
$WGRIB2 rtgsst_grb_0.083_nomads.grib2 -s > rtgsst_grb_0.083_nomads.grib2.idx

export parmcard=${PARMrtgsst}/grib2_awips_rtgssthr

export FORT11="rtgsst_grb_0.083_awips.grib2"
export FORT31=" "
export FORT51="grib2.awips_rtgssthr_grb_0.083"

$TOCGRIB2 < $parmcard

#####################################################################
#
#     PROCESS GLOBAL SEA SURFACE TEMPERATURE (SST) FOR AWIPS
#
#####################################################################


$CNVGRIB -g12 -p40 rtgsstgrb0.083_awips  rtgsst_grb_0.083_awips.grib2
$WGRIB2 rtgsst_grb_0.083_awips.grib2 -s > rtgsst_grb_0.083_awips.grib2.idx

$CNVGRIB -g12 -p2 rtgsstgrb0.083_awips  rtgsst_grb_0.083_nomads.grib2
$WGRIB2 rtgsst_grb_0.083_nomads.grib2 -s > rtgsst_grb_0.083_nomads.grib2.idx

export parmcard=${PARMrtgsst}/grib2_awips_rtgssthr

export FORT11="rtgsst_grb_0.083_awips.grib2"
export FORT31=" "
export FORT51="grib2.awips_rtgssthr_grb_0.083"

$TOCGRIB2 < $parmcard

##############################
# Convert to grib2 format
##############################
$CNVGRIB -g12 -p40 rtgsstgrb0.5 rtgssthr_grb_0.5.grib2
$CNVGRIB -g12 -p40 rtgsstgrb0.083 rtgssthr_grb_0.083.grib2
$WGRIB2 rtgssthr_grb_0.5.grib2 -s >rtgssthr_grb_0.5.grib2.idx
$WGRIB2 rtgssthr_grb_0.083.grib2 -s > rtgssthr_grb_0.083.grib2.idx

if test "$SENDCOM" = 'YES'
then
  cp rtgsstgrb0.083_awips       $COMOUT/rtgssthr_grb_0.083_awips
  cp rtgsstgrb0.083_awips.index $COMOUT/rtgssthr_grb_0.083_awips.index
  cp rtgsstgrb0.083             $COMOUT/rtgssthr_grb_0.083
  cp rtgsstgrb0.083.index       $COMOUT/rtgssthr_grb_0.083.index
  cp anomlygrb0.083             $COMOUT/anomaly_grb_0.083
  cp anomlygrb0.083.index       $COMOUT/anomaly_grb_0.083.index
  cp rtgsstgrb0.5_awips         $COMOUT/rtgssthr_grb_0.5_awips
  cp rtgsstgrb0.5_awips.index   $COMOUT/rtgssthr_grb_0.5_awips.index
  cp rtgsstgrb0.5               $COMOUT/rtgssthr_grb_0.5
  cp rtgsstgrb0.5.index         $COMOUT/rtgssthr_grb_0.5.index

  cp grib2.awips_rtgssthr_grb_0.083 $COMOUT/grib2.awips_rtgssthr_grb_0.083
  cp rtgssthr_grb_0.5.grib2     $COMOUT/rtgssthr_grb_0.5.grib2
  cp rtgssthr_grb_0.083.grib2   $COMOUT/rtgssthr_grb_0.083.grib2
  cp rtgssthr_grb_0.5.grib2.idx  $COMOUT/rtgssthr_grb_0.5.grib2.idx
  cp rtgssthr_grb_0.083.grib2.idx $COMOUT/rtgssthr_grb_0.083.grib2.idx
  cp rtgsst_grb_0.083_awips.grib2  $COMOUT/rtgssthr_grb_0.083_awips.grib2
  cp rtgsst_grb_0.083_awips.grib2.idx $COMOUT/rtgssthr_grb_0.083_awips.grib2.idx
  cp rtgsst_grb_0.083_nomads.grib2  $COMOUT/rtgssthr_grb_0.083_nomads.grib2
  cp rtgsst_grb_0.083_nomads.grib2.idx $COMOUT/rtgssthr_grb_0.083_nomads.grib2.idx


  if test "$SENDDBN" = 'YES'
  then
     $DBNROOT/bin/dbn_alert MODEL RTG_SST_grib $job $COMOUT/rtgssthr_grb_0.5
     $DBNROOT/bin/dbn_alert MODEL RTG_SST_gribi $job $COMOUT/rtgssthr_grb_0.5.index
     $DBNROOT/bin/dbn_alert MODEL RTG_SST_grib $job $COMOUT/rtgssthr_grb_0.083
     $DBNROOT/bin/dbn_alert MODEL RTG_SST_gribi $job $COMOUT/rtgssthr_grb_0.083.index
     $DBNROOT/bin/dbn_alert MODEL RTG_SST_grib $job $COMOUT/rtgssthr_grb_0.083_awips
     $DBNROOT/bin/dbn_alert MODEL RTG_SST_gribi $job $COMOUT/rtgssthr_grb_0.083_awips.index
     $DBNROOT/bin/dbn_alert NTC_LOW $NET $job $COMOUT/grib2.awips_rtgssthr_grb_0.083

     if [ $SENDDBN_GB2 = YES ]
     then

     $DBNROOT/bin/dbn_alert MODEL RTG_SST_grib_GB2 $job $COMOUT/rtgssthr_grb_0.5.grib2
     $DBNROOT/bin/dbn_alert MODEL RTG_SST_grib_GB2 $job $COMOUT/rtgssthr_grb_0.083.grib2
     $DBNROOT/bin/dbn_alert MODEL RTG_SST_grib_GB2 $job $COMOUT/rtgssthr_grb_0.083_awips.grib2
     $DBNROOT/bin/dbn_alert MODEL RTG_SST_grib_GB2_WIDX $job $COMOUT/rtgssthr_grb_0.5.grib2.idx
     $DBNROOT/bin/dbn_alert MODEL RTG_SST_grib_GB2_WIDX $job $COMOUT/rtgssthr_grb_0.083.grib2.idx
     $DBNROOT/bin/dbn_alert MODEL RTG_SST_grib_GB2_WIDX $job $COMOUT/rtgssthr_grb_0.083_awips.grib2.idx

     fi

  else
     echo "SENDDBN=$SENDDBN, files not posted to db_net."
  fi

fi

set -x
msg='ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"

exit
