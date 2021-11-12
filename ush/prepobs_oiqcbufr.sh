#!/bin/ksh
# Run under ksh (converted to WCOSS)

# This script performs an oi-based quality control on all data
#
# It is normally executed by the script prepobs_makeprepbufr.sh
#  but can also be executed from a checkout parent script
# --------------------------------------------------------------

set -aux

qid=$$

# Positional parameters passed in:
#   1 - path to COPY OF input prepbufr file --> becomes output prepbufr
#       file upon successful completion of this script (note that input
#       prepbufr file is NOT saved by this script)
#   2 - NCEP production date (YYYYMMDDHH)

# Imported variables that must be passed in:
#   DATA - path to working directory
#   OIQCT - path to observation error table file
#   OIQCX - path to PREPOBS_OIQCBUFR program executable

# Imported variables that can be passed in:
#   jlogfile - string indicating path to joblog file (skipped over by this
#              script if not passed in)
#   pgmout   - string indicating path to for standard output file (skipped
#              over by this script if not passed in)

cd $DATA
PRPI=$1
if [ ! -s $PRPI ] ; then exit 1;fi
CDATE10=$2

rm $PRPI.oiqcbufr
rm tosslist

pgm=`basename  $OIQCX`
if [ -s $DATA/prep_step ]; then
   set +u
   . $DATA/prep_step
   set -u
else
   [ -f errfile ] && rm errfile
   unset FORT00 `env | grep "^FORT[0-9]\{1,\}=" | awk -F= '{print $1}'`
fi

#### THE BELOW LIKELY NO LONGER APPLIES ON WCOSS
set +u
[ -n "$LOADL_PROCESSOR_LIST" ] && export XLSMPOPTS=parthds=2:usrthds=2:stack=64000000
set -u

echo "      $CDATE10" > cdate.dat
export FORT11=cdate.dat
export FORT14=$PRPI
export FORT17=$OIQCT
export FORT18=obprt_ipoint.wrk
export FORT20=tolls.wrk
export FORT61=toss.sfc_z
export FORT62=toss.temp_wind
export FORT63=toss.sat_temp
export FORT64=toss.ssmi_wind
export FORT65=tosslist
export FORT70=$PRPI.oiqcbufr
export FORT81=obogram.out
export FORT82=obogram.bin

# find the number of procs

if [ -n "$NCPUS" ]; then
   nprocs=$LSB_DJOB_NUMPROC
   export APRUN="mpiexec -n $nprocs"
else
   echo "nprocs not defined for this platform"
   export err=99; err_chk
fi

# run the oiqc

$APRUN  $OIQCX > outout 2> errfile
export err=$?

cat errfile >> outout
cat outout >> oiqcbufr.out
cp outout obcnt.out

set +u
[ -n "$pgmout" ]  &&  cat outout >> $pgmout
rm outout
set -u

set +x
echo
echo 'The foreground exit status for PREPOBS_OIQCBUFR is ' $err
echo
set -x

if [ "$err" -eq '4' ]; then
msg="WRNG: SOME OBS NOT QC'd BY PGM PREPOBS_OIQCBUFR - # OF OBS > LIMIT \
--> non-fatal"
   set +x
   echo
   echo "$msg"
   echo
   set -x
   set +u
   [ -n "$jlogfile" ] && $DATA/postmsg "$jlogfile" "$msg"
   set -u
   err=0
else
   err_chk
fi

mv $PRPI.oiqcbufr $PRPI

exit 0
