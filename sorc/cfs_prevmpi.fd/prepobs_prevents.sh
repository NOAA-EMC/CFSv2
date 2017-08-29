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
#   sigf1   - path to sigma 3-hr forecast valid at t-3                      
#   sigf2   - path to sigma 6-hr forecast valid at t-0                      
#   sigf3   - path to sigma 9-hr forecast valid at t+3                      
#
#   sfcf1   - path to surface 3-hr forecast valid at t-3
#   sfcf2   - path to surface 6-hr forecast valid at t-0
#   sfcf3   - path to surface 9-hr forecast valid at t+3
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

pgm=`basename  $PREX`
if [ -s $DATA/prep_step ]; then
   . $DATA/prep_step
else
   [ -f errfile ] && rm errfile
   export XLFUNITS=0
   unset `env | grep XLFUNIT | awk -F= '{print $1}'`

   set +u
   if [ -z "$XLFRTEOPTS" ]; then
     export XLFRTEOPTS="unit_vars=yes"
   else
     export XLFRTEOPTS="${XLFRTEOPTS}:unit_vars=yes"
   fi
   set -u
fi

set +u
if [ -z "$XLFRTEOPTS" ]; then
   export XLFRTEOPTS="nlwidth=132"
else
   export XLFRTEOPTS="${XLFRTEOPTS}:nlwidth=132"
fi
set -u

echo "      $CDATE10" > cdate10.dat

# The PREPOBS_PREVENTS code will soon, or may now, open GFS spectral
# coefficient guess files using sigio routines (via W3LIB rouitne GBLEVENTS)
# via explicit open(unit=number,file=filename) statements.  This conflicts with
# the XLFUNIT statements above.  One can either remove the explicit open
# statements in the code or replace the above XLFUNIT lines with soft links.
# The soft link approach is taken below.

ln -sf $sigf1             fort.20
ln -sf $sigf2             fort.21 
ln -sf $sigf3             fort.22
ln -sf $sigf4             fort.23

ln -sf $sfcf1             fort.30
ln -sf $sfcf2             fort.31 
ln -sf $sfcf3             fort.32
ln -sf $sfcf4             fort.33

export XLFUNIT_11=$PRPI
export XLFUNIT_14=$PRVT
export XLFUNIT_15=cdate10.dat
export XLFUNIT_51=$PRPI.prevents
export XLFUNIT_52=prevents.filtering

#XLSMPOPTS=parthds=12:stack=20000000

TIMEIT=""
[ -s $DATA/timex ] && TIMEIT=$DATA/timex
$TIMEIT $PREX < $PREC ;  > outout  2> errfile
err=$?

cat outout errfile

cat errfile >> outout; cat outout > checkoutout
cat prevents.filtering >> outout
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
if [ -s $DATA/err_chk ]; then
   $DATA/err_chk
else
   if test "$err" -gt '0'
   then
######kill -9 ${qid}
      exit 555
   fi
fi

if [ "$err" -gt '0' ]; then
   exit 9
else
   mv $PRPI.prevents $PRPI
fi

exit 0
