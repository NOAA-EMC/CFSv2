#!/bin/ksh

#############################################################
echo "JSNOWGRB - This jobs runs the Snow Depth Analyses "                     
echo "History: - 01 Mar 2000 - converted to run on the IBM SP"
#############################################################

COMOLD=${COMOLD:-${COMINm1:?}}

# this function is invoked if there is a problem creating
# gaussian snow data.  in this case, the previous days
# gaussian snow file is copied to the current days directory.

function copy_forward
{
   set -x
   OLD_FILE="$COMOLD/${OUTPUT_FILE}"
   OLD_FILE_G2="$COMOLD/${OUTPUT_FILE}.grib2"
   if [[ -s $OLD_FILE && -s $OLD_FILE_G2 ]]
   then
     set +x
     echo " "
     echo "*****************************************************"
     echo "* COPY PREVIOUS DAYS ${DOMAIN} DATA TO CURRENT DIRECTORIES."
     echo "*****************************************************"
     echo " "
     set -x
     cp $OLD_FILE    $COMOUT  
     cp $OLD_FILE_G2 $COMOUT 
     msg="PREVIOUS DAYS ${DOMAIN} DATA COPIED TO CURRENT DIRECTORIES."
     postmsg "$jlogfile" "$msg"
   else
     set +x
     echo " "
     echo "*****************************************************"
     echo "* PREVIOUS DAYS ${DOMAIN} DATA NOT AVAILABLE; ABORT."
     echo "*****************************************************"
     echo " "
     set -x
     msg="NO BACKUP DATA FOR ${DOMAIN}; WILL ABORT"
     postmsg "$jlogfile" "$msg"
     error=1
   fi
}  # end function copy_forward

cd $DATA

###################################################################
# JOB FLOW STEPS
# 1) if cycle is 00z, create global snow analysis on gaussian grid
# 2) create global .5 deg snow analysis
###################################################################

set -x


set +x
echo " "
echo "############################################################"
echo "# Create the gaussian snow depth analysis from the Air "
echo "# Force snow depth analysis and the IMS snow "
echo "# cover analysis.  The IMS is available only in N.H."
echo "############################################################"
echo " "
set -x

IMS_FILE_NAME="imssnow96.grb"
IMS_DIR="${TANKDIR}/${PDY}/wgrbbul"
IMS_FILE=${IMS_DIR}/${IMS_FILE_NAME}

AFWA_DIR=${TANKDIR}/${PDY}/wgrbbul/557thWW_snow
AFWA_GB_FILE_NAME=PS.557WW_SC.U_DI.C_GP.USAFSI_GR.C0P09DEG_AR.GLOBAL_PA.SNOW-ICE_DD.${PDY}_DT.1200_DF.GR2
AFWA_GB_FILE=${AFWA_DIR}/${AFWA_GB_FILE_NAME}

error=0

cp $AFWA_GB_FILE afwa_gb_file; AFWA_GB_FILE=afwa_gb_file

cp $IMS_FILE ims_file; IMS_FILE=ims_file

# run snow2mdl program if data available
# --------------------------------------

if [[ -s $IMS_FILE && -s $AFWA_GB_FILE ]]
then
  TEMP_DATE=`$WGRIB -4yr $IMS_FILE | head -1`
  typeset -L10 IMS_DATE
  IMS_DATE=${TEMP_DATE#*:d=}
  CYCLE_TIME=$IMS_DATE
  CYCLE_YEAR=`echo $CYCLE_TIME | cut -c1-4`
  CYCLE_MON=`echo $CYCLE_TIME | cut -c5-6`
  CYCLE_DAY=`echo $CYCLE_TIME | cut -c7-8`
  CYCLE_HOUR="00"
  for DOMAIN in t382 t574
  do
    msg="RUN SNOW2MDL PROGRAM FOR ${DOMAIN}"
    postmsg "$jlogfile" "$msg"
    OUTPUT_FILE="snowdepth.${DOMAIN}.grb"
    if test "$DOMAIN" = 't574'
    then
      min_d=.05
    fi
    if test "$DOMAIN" = 't382'
    then
      min_d=.025
    fi
    rm -f ./fort.41
    cat > ./fort.41 << !
      &source_data
       autosnow_file=""
       nesdis_snow_file="${IMS_FILE}"
       nesdis_lsmask_file=""
       afwa_snow_global_file="${AFWA_GB_FILE}"
      /
       &qc
        climo_qc_file="${FIXgrib}/emcsfc_snow_cover_climo.grib2"
      /
       &model_specs
        model_lat_file="${FIXgrib}/global_latitudes.${DOMAIN}.grb"
        model_lon_file="${FIXgrib}/global_longitudes.${DOMAIN}.grb"
        model_lsmask_file="${FIXgrib}/global_slmask.${DOMAIN}.grb"
        gfs_lpl_file=""
       /
       &output_data
        model_snow_file="./${OUTPUT_FILE}"
        /
       &output_grib_time
        grib_year=$CYCLE_YEAR
        grib_month=$CYCLE_MON
        grib_day=$CYCLE_DAY
        grib_hour=$CYCLE_HOUR
       /
       &parameters
        lat_threshold=55.0
        min_snow_depth=${min_d}
        snow_cvr_threshold=50.0
        /
!
    ${EXECgrib}/emcsfc_snow2mdl
    export err=$?
    if (($err != 0))
    then
      msg="ABNORMAL TERMINATION IN SNOW2MDL PROGRAM FOR ${DOMAIN}. STATUS IS ${err}"
      postmsg "$jlogfile" "$msg"
      set +x
      echo " "
      echo "*****************************************************"
      echo "* ABNORMAL TERMINATION IN SNOW2MDL PROGRAM FOR ${DOMAIN}"
      echo "*****************************************************"
      echo " "
      set -x
      if test "$SENDCOM" = 'YES'
      then
        copy_forward
      fi
    else
      msg="NORMAL TERMINATION OF SNOW2MDL PROGRAM FOR ${DOMAIN}"
      postmsg "$jlogfile" "$msg"
      set +x
      echo " "
      echo "*****************************************************"
      echo "* NORMAL TERMINATION OF SNOW2MDL PROGRAM FOR ${DOMAIN}"
      echo "*****************************************************"
      echo " "
      set -x
      if test "$SENDCOM" = 'YES'
      then
        set +x
        echo "COPY ${DOMAIN} SNOW DATA TO PRODUCTION DIRECTORIES"
        set -x
        $CNVGRIB -g12 -p31 $OUTPUT_FILE ${OUTPUT_FILE}.grib2
        cp $OUTPUT_FILE       $COMOUT                      
        cp $OUTPUT_FILE.grib2 $COMOUT                         
      fi
      if [ -e ${COMOUT}/$OUTPUT_FILE.grib2 -a $cyc = 18 ] ; then
        if test "$SENDDBN" = 'YES'
        then
          $DBNROOT/bin/dbn_alert MODEL SNOW_GB_GB2 $job ${COMOUT}/${OUTPUT_FILE}.grib2
        fi
      fi
    fi  # did program run error free?
  done  # cycle over domains
else  # all input data not available.
  set +x
  echo " "
  echo "*****************************************************"
  echo "* CURRENT AFWA AND/OR IMS DATA NOT AVAILABLE."
  echo "* CANT RUN SNOW2MDL PROGRAM."
  echo "*****************************************************"
  echo " "
  set -x
  msg="CURRENT AFWA AND/OR IMS DATA NOT AVAILABLE. CANT RUN SNOW2MDL."
  postmsg "$jlogfile" "$msg"

  if test "$SENDCOM" = 'YES'
  then
    for DOMAIN in t382 t574
    do
      OUTPUT_FILE="snowdepth.${DOMAIN}.grb"
      copy_forward
    done # loop over domains.
  fi  # is sendcom yes?

fi # does raw snow data exist?

#-----------------------------------------------------------------
# now run program to create 0.5 degree global snow
# data product.
#-----------------------------------------------------------------

msg="CREATE 0.5 DEGREE GLOBAL SNOW DATA."
postmsg "$jlogfile" "$msg"

rm -f fort.52 fort.11 fort.12 fort.13 

ls -l $TANKDIR/${PDY}/wgrbbul
ls -l $TANKDIR/${PDY}/wgrdbul

cp $COMOUT/snowdepth.global.grb fort.52; errcp=$?
[[ "$errcp" -ne '0' ]]  &&  cp $COMOLD/snowdepth.global.grb fort.52

# abort the job if both current data and backup are empty
# -------------------------------------------------------

IMS_FILE=${IMS_DIR}/imssnow96.grb # use higher resolution for imssnow

if [ ! -s $IMS_FILE -o ! -s $AFWA_GB_FILE ]
then
  if [ ! -s fort.52 ]
  then
    msg=" both current data and backup are empty; abort"
    postmsg "$jlogfile" "$msg"
    export err=99; err_chk
  fi
fi

set +x
echo " "
echo "############################################################"
echo "# Create the global .5 deg snow depth analysis from the Air "
echo "# Force snow depth analysis (47km) and the IMS 23km snow "
echo "# cover analysis.  The IMS 23km is available only in N.H."
echo "############################################################"
echo " "
set -x

# use wgrib2 to convert IMS and AFWA files to 0.5 x 0.5 resolution

cnvgrib -g12 $IMS_FILE imss.grb2
wgrib2 imss.grb2      -new_grid latlon 0.0:720:0.5 90.0:361:-0.5 imss5x5.grb2 
wgrib2 $AFWA_GB_FILE  -new_grid latlon 0.0:720:0.5 90.0:361:-0.5 afwa5x5.grb2
cnvgrib -g21 imss5x5.grb2 imss5x5.grb1
cnvgrib -g21 afwa5x5.grb2 afwa5x5.grb1

echo
wgrib -V afwa5x5.grb1
echo
wgrib -V imss5x5.grb1
echo

###################################
# Make the program unit assignments
###################################

pgm=`basename  $EXECgrib/emcsfc_grib_snowgrib`
. prep_step

ln -sf afwa5x5.grb1         fort.12
ln -sf imss5x5.grb1         fort.13

$EXECgrib/emcsfc_grib_snowgrib >> $pgmout 2> pgmerr
export err=$?

snow_date=`$WGRIB -4yr -d 1 fort.52 | awk -F: '{print $3}' | awk -F= '{print $2}'`

set +x
echo
echo "The date of the snowdepth.global.grb file is $snow_date"
echo
set -x

# abort the job if snow date earlier than -5days
# ----------------------------------------------

diff_date=$PDYm5
if [ "$snow_date" -lt "$diff_date" ];then
    msg=" date of snowdepth.global.grb file is BAD; abort"
    postmsg "$jlogfile" "$msg"
    export err=99; err_chk
fi

if [ "$err" -ne '0' ]; then
    msg="WARNING: $pgm r.c.=$err; copy most recent snowdepth.global.grb file into todays COMOUT"
    postmsg "$jlogfile" "$msg"
else
  msg="0.5 DEGREE GLOBAL SNOW DATA PROGRAM COMPLETED NORMALLY"
  postmsg "$jlogfile" "$msg"
fi

###################################################
# Save the output snow grib files in /dcom and /com
###################################################

if test "$SENDCOM" = 'YES'
then
   cp fort.52 $COMOUT/snowdepth.global.grb
fi

# abort the job if problem creating or copying gaussian grid snow
# ---------------------------------------------------------------

if ((error != 0))
then
  msg="PROBLEM CREATING or COPYING GAUSSIAN GRID SNOW."
  postmsg "$jlogfile" "$msg"
  export err=99; err_chk 
fi

msg="$0 completed normally"
postmsg "$jlogfile" "$msg"
