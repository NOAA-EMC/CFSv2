#!/bin/ksh
###################################################################
echo "----------------------------------------------------"
echo "exnawips - convert NCEP GRIB files into GEMPAK Grids"
echo "----------------------------------------------------"
echo "History: Mar 2000 - First implementation of this new script."
echo "S Lilly: May 2008 - add logic to make sure that all of the "
echo "                    data produced from the restricted ECMWF"
echo "                    data on the CCS is properly protected."
echo "C. Magee: 10/2013 - swap X and Y for rtgssthr Atl and Pac."
echo "SPA SH  : 12/2015 - update to be part of rtgssthr verticle structure."
#####################################################################

set -xa

cd $DATA

msg="Begin job for $job"
postmsg "$jlogfile" "$msg"

#
NAGRIB_DIR=$(dirname $NAWIPS)
NAGRIB_TABLE=${NAGRIB_DIR}/fix/nagrib.tbl
###NAGRIB_TABLE=/nwprod/gempak/fix/nagrib.tbl
utilfix_nam=${utilfix_nam:-${UTILROOT}/fix}
NAGRIB=${OS_BIN}/nagrib_nc
#

  cpyfil=gds
  garea=dset
  gbtbls=
  maxgrd=4999
  kxky=
  grdarea=
  proj=
  output=T

# GRIBIN=$COMIN/${model}.${cycle}.${GRIB}${fhr}${EXT}
# GEMGRD=${RUN}_${PDY}${cyc}f${fhr3}
  GRIBIN=$COMIN/rtgssthr_grb_0.083_awips
  GEMGRD=rtgsst_${GRIB}_${PDY}00
  GRIBIN_chk=$GRIBIN

  if [ $GRIB = "atl" ] ; then
    $COPYGB -g "255 0 840 540 50000 -100000 128 5000 -30000 083 083 0" -x $GRIBIN grib$fhr
  else
    # Assume Pacific region
    $COPYGB -g "255 0 960 660 60000 -170000 128 5000 -90000 083 083 0" -x $GRIBIN grib$fhr
  fi
  export pgm="nagrib_nc F$fhr"
  startmsg

   $NAGRIB << EOF
   GBFILE   = grib$fhr
   INDXFL   = 
   GDOUTF   = $GEMGRD
   PROJ     = $proj
   GRDAREA  = $grdarea
   KXKY     = $kxky
   MAXGRD   = $maxgrd
   CPYFIL   = $cpyfil
   GAREA    = $garea
   OUTPUT   = $output
   GBTBLS   = $gbtbls
   GBDIAG   = 
   PDSEXT   = $pdsext
  l
  r
EOF
  export err=$?;err_chk

  #####################################################
  # GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
  # WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
  # FOR THIS CASE HERE.
  #####################################################

  if [ $SENDCOM = "YES" ] ; then
     mv $GEMGRD $COMOUT/$GEMGRD
     if [ $SENDDBN = "YES" ] ; then
       $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job $COMOUT/$GEMGRD
     else
       echo "##### DBN_ALERT_TYPE is: ${DBN_ALERT_TYPE} #####"
     fi
  fi

#####################################################################
# GOOD RUN
set +x
echo "**************JOB $RUN NAWIPS COMPLETED NORMALLY ON THE WCOSS"
echo "**************JOB $RUN NAWIPS COMPLETED NORMALLY ON THE WCOSS"
echo "**************JOB $RUN NAWIPS COMPLETED NORMALLY ON THE WOCSS"
set -x
#####################################################################

msg='Job completed normally.'
echo $msg
postmsg "$jlogfile" "$msg"

############################### END OF SCRIPT #######################
