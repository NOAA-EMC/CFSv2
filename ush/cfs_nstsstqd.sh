#!/bin/bash
#
# Abstract: This utility generates a .25deg nsst grib file from the GDAS 1/12th degree rgtnsst file             
#
# Notes: uses COPYGB 
#       RTG-like SST data file: SST analysis over water grids, derived surface temperature over non-water grids.
#                               1/12 lat/lon grids, 4320 x 2160
# Input files :
#       rtgnsst      GDAS 6-hourly NSST foundation temperature analysis  1/12 lat/lon grids, 4320 x 2160 
#                    derived surface temperature over non-water grids                                   
                                   
# Output file :
#       nstsstqd.gdas.$date  - .25 degree NSST grib file for CFS  
#
# Author, 07/18/2020     - Woollen 

set -euax

# loop backwards over one day to find a GDAS nsstgrb file
# -------------------------------------------------------

date=$1; dend=$($NDATE -24 $date); inc=6  # date is the date for COMGDAS

while [[ $date -ge $dend ]]; do
day=$(echo $date|cut -c 1-8)
our=$(echo $date|cut -c 9-10)

rtgfle=$COM_GDAS/gdas.$day/$our/atmos/gdas.t${our}z.rtgssthr.grb        # note COM_GDAS must be defined in dump job script  

if [[ ! -s $rtgfle ]] ; then    
   date=$($NDATE -$inc $date)                                           # if no sfcanl try 6 hours earlier 
   continue                  
fi

echo
echo $CDATE nsst from $COMGDAS
echo


# create the oisst lookalike from the nstsst rtg look alike file and add the land/sea/mask
# ----------------------------------------------------------------------------------------

sstgrb=$FIXnst/sstgrb.gdas.template 
nstout=nstsstqd.gdas.$date
nstsst=$rtgfle  

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
echo CANNOT FIND GDAS NSSTGRB FOR THE LAST 24 HOURS
echo

exit 9999 
