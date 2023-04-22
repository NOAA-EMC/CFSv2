#!/bin/ksh
##########################################################################
echo "--------------------------------------------------------------------"
echo "excfs_cdas_dump.sh.sms - CFS network"
echo "                       data dump processing"
echo "--------------------------------------------------------------------"
echo "History: Aug 31 2010 - Original script."
###########################################################################
#
#     COMSP       - string indicating the final directory/filename path to
#                   output data destination
#                   (e.g., "/com/cfs/prod/cdas1.20000102/cdas1.t12z.")
#                   {NOTE: If the imported variable "SENDCOM" (see below)
#                          is "NO", then COMSP is hardwired to the string
#                          "$DATA/"}
#     SENDCOM     - string: if = 'NO' will redefine "COMSP" variable to be
#                   "$DATA/", regardless of its imported value - this has
#                   the effect of preventing any files from going to an
#                   operational /com directory path, and instead sending
#                   them to the "DATA" directory
#                   Default is "YES"
###########################################################################

set -x

# Make sure we are in the $DATA directory
cd $DATA

gdas1=gdas

msg="HAS BEGUN on `hostname`"
postmsg "$msg"

cat break > $pgmout

export NET_uc=$(echo $NET | tr [a-z] [A-Z])
export tmmark_uc=$(echo $tmmark | tr [a-z] [A-Z])
export SENDCOM=${SENDCOM:-YES}
export PROCESS_GRIBFLDS=${PROCESS_GRIBFLDS:-YES}
export PROCESS_GDAS_DUMP=${PROCESS_GDAS_DUMP:-YES}
export PROCESS_OCN_DUMP=${PROCESS_OCN_DUMP:-YES}
export COMSP=${COMSP:-$COMOUT/${RUN}1.${cycle}.}         # preferred, but don't know how RUN will be set in J-Script

msg="$NET_uc ANALYSIS TIME IS $PDY$cyc"
postmsg "$msg"

[ "$SENDCOM" = 'NO' ]  &&  COMSP=$DATA/

# get grib fields
# ---------------

if [ "$PROCESS_GRIBFLDS" = 'YES' ]; then

# create snowdepth.global.grb from AFWA snow fields

set -x
echo
echo '------------------------------------------------------------------------------------'
echo 'JISNI processing -------------------------------------------------------------------'
echo '------------------------------------------------------------------------------------'
set -x
#${JISNISH:-$USHcfs/cfs_snowdepth.sh} >$PWD/sdout.$$ 2>&1
${JISNISH:-$USHcfs/cfs_snowdepth.sh} 
export err=$?; pgm=cfs_snowdepth.sh; err_chk
set -x
echo '------------------------------------------------------------------------------------'
echo '------------------------------------------------------------------------------------'
echo
set -x

# create the nsstsstqd files 

set -x
echo
echo '------------------------------------------------------------------------------------'
echo 'NSST SST QD processing -------------------------------------------------------------------'
echo '------------------------------------------------------------------------------------'
set -x
${NSTSSTQD:-$HOMEcfs/ush/cfs_nstsstqd.sh} $CDATE $SFCanl >$PWD/nstout.$$ 2>&1
export err=$?; export pgm=cfs_nstsstqd.sh; err_chk
set -x
echo '------------------------------------------------------------------------------------'
echo '------------------------------------------------------------------------------------'
echo
set -x

#########################################################
##  generate sstgrb index file
#########################################################

   [ -f errfile ] && rm errfile
   $GRBINDEX ${COMSP}sstgrb ${COMSP}sstgrb.index 2> errfile
   errindx=$?
   [ "$errindx" -ne '0' ] && cat errfile
   [ -f errfile ] && rm errfile

   if [ $SENDDBN = YES ]; then
     $DBNROOT/bin/dbn_alert MODEL CDAS1_SSTOIQD_GRB $job ${COMSP}sstgrb
   fi

pgm=$(basename $0)

########################################################
########################################################
#  copy engicegrb        from $COM_ENGICE
########################################################
########################################################


   engicegrb=${COM_ENGICE}.$PDY/engice.t00z.grb
   engiceold=${COM_ENGICE}.$PDYm1/engice.t00z.grb

   if [ -s $engicegrb ]; then
      cp $engicegrb ${COMSP}engicegrb
      msg="todays engice grib file located and copied to /com"
      postmsg "$msg"
   elif [ -s $engiceold ]; then
      cp $engiceold ${COMSP}engicegrb
      msg="**todays engice grib file not located - copy 1-day old file"
      postmsg "$msg"
   elif [ -s $COMGDAS/$gdas1.t${cyc}z.engicegrb ]; then
      cp $COMGDAS/$gdas1.t${cyc}z.engicegrb ${COMSP}engicegrb
   else
      set -x
      echo " "
      echo " ############################################"
      echo " cannot locate engice grib file - fatal error"
      echo " ############################################"
      echo " "
      set -x
      msg="**CANNOT LOCATE ENGICE GRIB FILE --> EXIT DATA DUMP"
      postmsg "$msg"
      err_exit
   fi

fi   #  endif $PROCESS_GRIBFLDS

# get gdas files
if [ "$PROCESS_GDAS_DUMP" = 'YES' ]; then

# copy the production GDAS DUMP files
#              and rename them to cdas1 files to be saved in the CDAS directory
  
##COMGDAS=$COMGDAS/$cyc ## need this for new com structure

  # bufr_d files
  for gdasfile in `ls $COMGDAS/$gdas1.t${cyc}z.*bufr_d |awk ' BEGIN { FS="/"} { print $NF }'`
  do
    cdasfile=`echo $gdasfile |sed -e "s/$gdas1/cdas1/g"`
    cp --preserv=mode,ownership $COMGDAS/$gdasfile $COMOUT/$cdasfile
  done

  # tcvital files
  cp $COMTC/$gdas1.t${cyc}z.syndata.tcvitals.tm00 ${COMSP}syndata.tcvitals.tm00
  cp $COMTC/$gdas1.t${cyc}z.jtwc-fnoc.tcvitals.tm00 ${COMSP}jtwc-fnoc.tcvitals.tm00

  # RFC #1550 Update WCOSS to send more CDAS data to NOMADS -20151211
  if [ $SENDDBN = YES ]; then
     fname=ascatw
     alerttype=CDAS1_BUFR_${fname}
     file=${COMSP}${fname}.tm00.bufr_d
     if [ -s ${file} ]; then
       $DBNROOT/bin/dbn_alert MODEL ${alerttype} $job ${file}
     else
       echo "${file} not exited "
     fi

     for fname in omi ascatt ; do
       alerttype=CDAS1_MSC_${fname}
       file=${COMSP}${fname}.tm00.bufr_d
       if [ -s ${file} ]; then
         $DBNROOT/bin/dbn_alert MODEL ${alerttype} $job ${file}
       else
         echo "${file} not exited "
       fi
     done
  fi

fi  #  endif $PROCESS_GDAS_DUMP

### No need to send out the DUMP files to TOC as they are identical to GDAS dump

# dump ocean bufr data
if [ "$PROCESS_OCN_DUMP" = 'YES' ]; then

### dump ocean data
DUMPMB=${DUMPMB:-$HOMEcfs/ush/bufr_dumpmb}
DUMP_LIST="dbuoy bathy tesac"
LOUD=${LOUD:-off}
GODAS_WNDO=${GODAS_WNDO:-10}

START_DATE=`finddate.sh $PDY d-$GODAS_WNDO`
END_DATE=$PDY

for CTYPE in $DUMP_LIST; do
  $DUMPMB  $START_DATE  $END_DATE  $CTYPE  ###> dumpmb.$CTYPE.output 2>dumpmb.$CTYPE.errfile

#  dumpstat=$?
#  echo status from dump is $dumpstat
#  if [ $dumpstat -ne 0 ]; then
#    msg="Fatal error -- $DUMPMB failed for $CTYPE for dates ${START_DATE} to ${END_DATE}"
#    postmsg "$jlogfile" "$msg"
#    export err=$dumpstat; err_chk
#  fi

  echo
  pwd;ls
  echo

  flist=$CTYPE.list
  [ -f $flist ] && rm $flist

  DATE=$START_DATE
  while [ $DATE -le $END_DATE ]; do
  if [ -s $CTYPE.$DATE ]; then
    echo $CTYPE.$DATE >> $flist
  else
    msg="Warning:  NO $CTYPE DUMP FOR ${DATE}"
      postmsg "$msg"
  fi
  DATE=`finddate.sh $DATE d+1`
  done

  if [ -s $flist ]; then
    sort $flist > $flist.sort
    set +u
    . prep_step
    set -u
    ln -sf $CTYPE.all fort.50

    $HOMEobsproc_dump/exec/bufr_combfr < $flist.sort >> $pgmout
    export err=$?; err_chk

    msg="Done processing $CTYPE for ${START_DATE} to ${END_DATE}"
    postmsg  "$msg"

    if [ "$SENDCOM" = 'YES' ]; then
      [ -s $CTYPE.all ] && cp $CTYPE.all ${COMSP}c${CTYPE}.$tmmark.mbufr_d

      # CTYPE could be dbuoy bathy or tesac
      if [ $SENDDBN = YES ]; then
        $DBNROOT/bin/dbn_alert MODEL CDAS1_BUFR_${CTYPE} $job ${COMSP}c${CTYPE}.$tmmark.mbufr_d
      fi
    fi
  fi

done

### end dump ocean data

export STATUS=YES

fi   #  endif $PROCESS_OCN_DUMP

echo " " >> $pgmout
echo "##################################################################\
####################"  >> $pgmout
echo " " >> $pgmout

#================================================================
#================================================================


# GOOD RUN
set -x
echo " "
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " "
set -x


# save standard output
cat  break $pgmout break > allout
# cat allout
# rm allout

sleep 10
msg='ENDED NORMALLY.'
postmsg "$msg"

################## END OF SCRIPT #######################
