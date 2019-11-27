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
# Author, 10/15/2019     - Woollen 
#


set -euax

# loop backwards over one day to find a sfcanl file 
# -------------------------------------------------

date=$1; dend=$($NDATE -24 $date); inc=6  # date is the date for COMGDAS

while [[ $date -ge $dend ]]; do
day=$(echo $date|cut -c 1-8)
our=$(echo $date|cut -c 9-10)
COMGDAS=$COM_GDAS/gdas.${day}/$our

sfcanl=$COMGDAS/gdas.t${our}z.sfcanl.nemsio                             # surface analysis file at Gaussian grids, for T1534, 3072 x 1536
iceanl=$COMGDAS/gdas.t${our}z.seaice.5min.grb                           # input Sea ice daily analysis, 1/12 lat/lon grids, 4320 x 2160  

if [[ ! -s $sfcanl ]] ; then    
   date=$($NDATE -$inc $date)                                           # if no sfcanl try 6 hours earlier 
   continue                  
fi

WorkDir=$DATA/nstrtg.$$; rm -rf $WorkDir; mkdir -p $WorkDir; cd $WorkDir

echo
echo $CDATE nsst from $COMGDAS
echo

rtgmsk=$FIXnst/rtgssthr_ls_nas.twelfthdeg.dat                           # Surface land/water mask 1/12 lat/lon grids, 4320 x 2160
sstclm=$FIXnst/RTGSST.1982.2012.monthly.clim.grb                        # monthly SST climatology (1/12 degree RTG is used here)
salclm=$FIXnst/WOA05_pottemp_salt-GRADS.nc                              # Salinity climatology, half lat/lon degree

ln -sf $sfcanl    sfcanl
ln -sf $iceanl    iceanl
ln -sf $rtgmsk    rtgmsk
ln -sf $sstclm    sstclm
ln -sf $salclm    salclm

ln -sf rtgsstgrb0.083.$date       tf_rtg_grb
ln -sf /dev/null                  tf_rtg_grb_awips

cat << EOF > nstrtg_parm.input # Make namelist for  1/12 degree RTG-like file
&setup
catime='$date',lputsi=.false.,dsearch=500.,nx=4320,ny=2160
/
EOF

# run the NSST file creation process - output file is tf_rtg_grb
# --------------------------------------------------------------

$HOMEnst/exec/nstrtg.x < nstrtg_parm.input  1>res.log 2>err.log

# create the oisst lookalike from the nstsst rtg look alike file and add the land/sea/mask
# ----------------------------------------------------------------------------------------

sstgrb=$FIXnst/sstgrb.gdas.template 
nstout=nstsstqd.gdas.$date
nstsst=tf_rtg_grb      

  NX=1440          # number of points on latitude cirle
  NY=720           # number of points on longitude meridian
LAT0=-89875        # latitude of origin * 1000  (lat = -90 .. 90)
LON0=125           # longitude of origin * 1000  (lon=-360 .. 360 ?)
LAT1=89875         # latitude of extreme (last) point * 1000
LON1=-125          # latitude of extreme (last) point * 1000
  DX=250           # latitudianal increment * 1000  (sign used)
  DY=250           # longitudinal increment * 1000  (sign used)

grid="255 0 $NX $NY $LAT0 $LON0 128 $LAT1 $LON1 $DX $DY 64"
$COPYGB -g "$grid" -x $nstsst         nsttmp
$WGRIB  $sstgrb -d 2 -grib -append -o nsttmp  ### 1>>res.log  2>>err.log

ln -sf nsttmp   fort.11
ln -sf $nstout  fort.51
echo $date|$HOMEcfs/exec/cfs_overdate_grib  1>>res.log  2>>err.log

cp $nstout $COMOUT/cdas1.t${cyc}z.sstgrb.nsst 
cp $nstout $COMOUT/cdas1.t${cyc}z.sstgrb

exit 0

done

echo
echo CANNOT FIND GDAS SFCANL FOR THE LAST 24 HOURS
echo

exit 9999 
