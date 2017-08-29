#!/bin/ksh
###########################################################################
#
# This script encodes the background (first guess) and observational
#  errors into the PREPBUFR reports (interpolated to obs. locations)
#
# It is normally executed by the script prepobs_makeprepbufr.sh
#  but can also be executed from a checkout parent script
# -------------------------------------------------------------
# Positional parameters passed in:
#
#   1 - path to COPY OF input prepbufr file --> becomes output prepbufr
#       file upon successful completion of this script (note that input
#       prepbufr file is NOT saved by this script)
#   2 - expected center date in PREPBUFR file (YYYYMMDDHH)
#
# Imported variables that must be passed in:
#   DATA  - path to working directory
#   NET   - string indicating system network (either "gfs", "gdas", "cdas",
#            "cdc", "nam", "ruc" or "rtma")
#            NOTE1: NET is changed to gdas in the parent Job script for the
#                   RUN=gdas1 (was gfs - NET remains gfs for RUN=gfs).
#            NOTE2: This is read from the program PREPOBS_PREVENTS via a call
#                   to system routine "GETENV".
#
#   sig1   - path to sigma 3-hr forecast valid at t-3                      
#   sig2   - path to sigma 6-hr forecast valid at t-0                      
#   sig3   - path to sigma 9-hr forecast valid at t+3                      
#
#   sfc1   - path to surface 3-hr forecast valid at t-3
#   sfc2   - path to surface 6-hr forecast valid at t-0
#   sfc3   - path to surface 9-hr forecast valid at t+3
# 
#   PRVT  - path to observation error table file
#   PREC  - path to PREPOBS_PREVENTS program parm cards
#   PREX  - path to PREPOBS_PREVENTS program executable
#
# Imported variables that can be passed in:
#   pgmout   - string indicating path to for standard output file (skipped
#              over by this script if not passed in)
#
###########################################################################


set -aux

qid=$$

cd $DATA
PRPI=$1; if [ ! -s $PRPI ] ; then exit 1 ;fi
CDATE10=$2

rm -f $PRPI.prevents
rm -f prevents.filtering

echo "      $CDATE10" > cdate10.dat

set +u
[ -z "$sig1" ] || ln -sf $sig1   fort.20
[ -z "$sig2" ] || ln -sf $sig2   fort.21 
[ -z "$sig3" ] || ln -sf $sig3   fort.22
[ -z "$sig4" ] || ln -sf $sig4   fort.23

[ -z "$sfc1" ] || ln -sf $sfc1   fort.30
[ -z "$sfc2" ] || ln -sf $sfc2   fort.31 
[ -z "$sfc3" ] || ln -sf $sfc3   fort.32
[ -z "$sfc4" ] || ln -sf $sfc4   fort.33
set -u

export FORT11=$PRPI
export FORT14=$PRVT
export FORT15=cdate10.dat
export FORT51=$PRPI.prevents
export FORT52=prevents.filtering

export OMP_NUM_THREADS=${prevthrd:-4}
export MPIRUN=${MPIRUN:-"mpirun -n 4"}

$MPIRUN $PREX $PREC  > outout  2> errfile
export err=$?; err_chk

cat outout errfile

cat errfile >> outout; cat outout > checkoutout
cat outout >> prevents.out
set +u
[ -n "$pgmout" ]  &&  cat outout >> $pgmout
set -u
rm outout
set +x
echo
echo 'The foreground exit status for PREPOBS_PREVENTS is ' $err
echo
set -x
err_chk

if [ "$err" -gt '0' ]; then
   exit 9
else
   mv $PRPI.prevents $PRPI
fi

exit 0
