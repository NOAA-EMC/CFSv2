#!/bin/sh
set -eua
#
#   This script originally written by Jack Woollen
#

[ $# -ne 5 ] && echo "$0 <siganl file> <cnvstat file> <prepbufr.in> <prepbufr.out> <date>"
[ $# -ne 5 ] && err_exit

siganl=$1 cnvstat=$2 prepbufr=$3 preppost=$4 date=$5

# fill in the analysis background with prevents

PREP=prepbufr_in_old_post; cp $prepbufr $PREP
$SIGEVENTSH $PREP $date 

prepbufr=$PREP # prepbufr into new post is prepbufr out of old post

# now run the new postevents to layer on the gsi events

export cfss=${cfss:-/cfs}
export cfsp=${cfsp:-cfs_}
export CNVDIAGEXEC=${CNVDIAGEXEC:-$HOMEcfs/exec/${cfsp}post_convdiag}
export DUPREPEXEC=${DUPREPEXEC:-$HOMEcfs/exec/${cfsp}duprep}
export COMBFRSH=${COMBFRSH:-$HOMEcfs/ush/${cfsp}combfr.sh}

# setup the convstat files

tar -xvf $cnvstat; gunzip -v diag_conv*
mv diag_conv_anl.* diag_conv_anl 
mv diag_conv_ges.* diag_conv_ges 

#link the prepbufr input and run postevents

export FORT20=$prepbufr 
$CNVDIAGEXEC ###>stdout 2>&1
export err=$?; err_chk

#combine the postevents outp messages together

$COMBFRSH preppost_* prep_w_dups ; rm -f preppost_*

#run the duplicate elimination
delxy=.005 # this is the lat/lon tolerance in degrees
delhr=.001 # this is the ob time tolerance in hundredths of an hour
delelv=1    # this is the elevation tolerance in meters

cat<<eof|$DUPREPEXEC 
prep_w_dups 
$preppost
$delxy $delhr $delelv
eof

export err=$?; err_chk
rm -f diag_conv_anl diag_conv_ges prep_w_dups fort.*
