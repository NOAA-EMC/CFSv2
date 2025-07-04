#!/bin/bash
######################################################################
#  UTILITY SCRIPT NAME :  excfs_grib_awips.sh
#         DATE WRITTEN :  03/30/2015
#
#  Abstract:  This utility script produces the CFS AWIPS GRIB2
#             with selected fields: PRATE, TMIN, and TMAX for
#             forecast hours from F00 up to > 7464 hours (> 311 days)
#             varable of forecast of CFS.
#
######################################################################
echo "---------------------------------------------------------------"
echo "JCFS_AWIPS ( 000Z, 06Z, 12Z and 18Z) products postprocessing      "
echo "---------------------------------------------------------------"
echo "History: March 2015 - First implementation of this new script."
echo " "
######################################################################

########################################
  msg="HAS BEGUN!"
  postmsg "$jlogfile" "$msg"
########################################

cd $DATA

set -x 

echo " ------------------------------------------"
echo "  BEGIN MAKING CFS GRIB2 AWIPS PRODUCTS    "
echo " ------------------------------------------"

set -x
echo " "
echo "#########################################"
echo "#                                       #"
echo "#   Process CFS GRIB2 PRODUCTS          #"
echo "#   FOR FORECAST HOURS F06 - > F7464 or #"
echo "#   (> 311 days) variable length of     #"
echo "#   forecast hours of CFS products      #"
echo "#                                       #"
echo "#########################################"
echo " "
set -x

for type in prate tmax tmin
do 
  ls $COMIN/${cyc}/time_grib_01/${type}.01.${cfs_day}${cyc}.daily.grb2
  if [ $? -ne 0 ]; then
    fileSize=0
  else
    fileSize=`ls -s $COMIN/${cyc}/time_grib_01/${type}.01.${cfs_day}${cyc}.daily.grb2 | awk '{print $1}'`
  fi

  if [ $fileSize -le $MINIMUM_SIZE ]; then
    echo "WARNING: file $COMIN/${cyc}/time_grib_01/${type}.01.${cfs_day}${cyc}.daily.grb2 is not exist or too small. \
          Maybe it is too early to use data in date ${cfs_day} for cyc=$cyc, try the previous date (PDYm1)."
    if [ $cfs_day -eq $PDY ]; then
      echo "Change to PDYm1"
      cfs_day=$PDYm1
      COMIN=$COMINm1
    fi
    # check again for the file size
    fileSize=`ls -s $COMIN/${cyc}/time_grib_01/${type}.01.${cfs_day}${cyc}.daily.grb2 | awk '{print $1}'`
    if [ $fileSize -le $MINIMUM_SIZE ]; then    
      echo "ERROR: file $COMIN/${cyc}/time_grib_01/${type}.01.${cfs_day}${cyc}.daily.grb2 is too small."
      export err=1; err_chk
    fi
  fi

  cp $COMIN/${cyc}/time_grib_01/${type}.01.${cfs_day}${cyc}.daily.grb2 .

# Processing AWIPS GRIB2 grids with WMO headers

  pgm=tocgrib2
  export pgm;. prep_step
  startmsg

  export FORT11=${type}.01.${cfs_day}${cyc}.daily.grb2
  export FORT31=" "
  export FORT51=grib2_cfs_${type}.01.${cfs_day}${cyc}.daily

  $HOMEcfs/exec/tocgrib2_cfs < $HOMEcfs/parm/grib2_cfs_${type}.01.daily

  err=$?;export err ;err_chk
  echo " error from tocgrib2=",$err

  if [ $SENDCOM = "YES" ] ; then

  ##############################
  # Post Files to PCOM
  ##############################
 
  mv grib2_cfs_${type}.01.${cfs_day}${cyc}.daily   $COMOUTwmo/grib2_cfs_${type}.01.${cyc}.daily
 
  fi
 
  ##########################
  # Distribute Data to NCF
  ##########################

  if [ "$SENDDBN" = 'YES' ] ; then
#
#    Distribute Data to TOC (AWIPS)
#
     $DBNROOT/bin/dbn_alert NTC_LOW $NET $job   $COMOUTwmo/grib2_cfs_${type}.01.${cyc}.daily
  else
     msg="File grib2_cfs_${type}.01.${cfs_day}${cyc}.daily  not posted to db_net."
     postmsg "$jlogfile" "$msg"
  fi

done

################################################################################
# GOOD RUN
set -x
echo "**************JOB JCFS_AWIPS COMPLETED NORMALLY ON THE WCOSS"
set -x
################################################################################

msg="HAS COMPLETED NORMALLY!"
postmsg "$jlogfile" "$msg"

############## END OF SCRIPT #######################
