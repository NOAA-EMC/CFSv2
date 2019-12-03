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

set -uax

# Make sure we are in the $DATA directory

cd $DATA||exit 99  

msg="$NET ANALYSIS TIME IS $PDY$cyc"
postmsg "$jlogfile" "$msg"

# copy the cdas dump from history

export COMSP=${COMSP:-$COMOUT/${RUN}1.${cycle}.}  

CDATE=$PDY$cyc
year=$(echo $CDATE|cut -c 1-4)
mnth=$(echo $CDATE|cut -c 5-6)
days=$(echo $CDATE|cut -c 7-8)
hour=$(echo $CDATE|cut -c 9-10)
txxz=t${hour}z

cd $COMOUT ||exit 99
htar -xvf /NCEPPROD/hpss${envir}/runhistory/cfs$year/$year$mnth/$year$mnth$days/Analysis/cfs.dumps.$PDY.tar  \*$txxz\*
rm -f cdas1.$txxz.cmapgrb

[[ ! -s ${COMSP}adpupa.tm00.bufr_d ]] && exit 99

# copy the tcvitals from rtdumps

mach=$(hostname|cut -c 1-1)
[[ $mach = t ]] && mach=tide
[[ $mach = m ]] && mach=tide   
[[ $mach = g ]] && mach=gyre
[[ $mach = v ]] && mach=gyre 

rtdump=/sss/emc/climate/shared/emc.climpara/rtdump/$CDATE/gdas
scp $mach:$rtdump/tcvitl.gdas.$CDATE  $COMOUT/cdas1.$txxz.syndata.tcvitals.tm00

# dump ocean bufr data ###########################################################################

cd $DATA

if [ ${PROCESS_OCN_DUMP:-NO} = YES ]; then

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
  pwd; ls
  echo

  flist=$CTYPE.list
  [ -f $flist ] && rm $flist

  DATE=$START_DATE
  while [ $DATE -le $END_DATE ]; do
  if [ -s $CTYPE.$DATE ]; then
    echo $CTYPE.$DATE >> $flist
  else
    msg="Warning:  NO $CTYPE DUMP FOR ${DATE}"
      postmsg "$jlogfile" "$msg"
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
    postmsg "$jlogfile" "$msg"

    if [ "$SENDCOM" = 'YES' ]; then
      [ -s $CTYPE.all ] && cp $CTYPE.all ${COMSP}c${CTYPE}.$tmmark.mbufr_d

      # CTYPE could be dbuoy bathy or tesac
      if [ $SENDDBN = YES ]; then
        $DBNROOT/bin/dbn_alert MODEL CDAS1_BUFR_${CTYPE} $job ${COMSP}c${CTYPE}.$tmmark.mbufr_d
      fi
    fi
  fi

done

export STATUS=YES

fi   #  endif $PROCESS_OCN_DUMP

#================================================================
#================================================================


# GOOD RUN
set +x
echo " "
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " ****** PROCESSING COMPLETED NORMALLY"
echo " "
set -x



# normal ending

msg='ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################
