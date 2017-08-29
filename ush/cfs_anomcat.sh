#!/bin/bash
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         global_anomcat.sh           
# Script description:  Concatenates height anomalies to a pressure GRIB file
#
# Author:        Mark Iredell       Org: NP23         Date: 1997-03-07
#
# Abstract: This script computes the height anomalies at 1000 and 500 mb
#   and the five-wave height anomaly at 500 mb and concatenates them
#   to a pressure GRIB file.
#
# Script history log:
# 1997-03-07  Mark Iredell
# 1999-05-01  Mark Iredell  SP version
#
# Usage:  global_anomcat.sh PGBOUT [PGIOUT]
#
#   Input script positional parameters:
#     1             Input/output pressure GRIB file
#                   defaults to $PGBOUT
#     2             Input/output pressure GRIB index file
#                   defaults to $PGIOUT, then to none
#
#   Imported Shell Variables:
#     PGBOUT        Output pressure GRIB file
#                   overridden by $1
#     PGIOUT        Output pressure GRIB index file
#                   overridden by $2; defaults to none
#     DATA          working directory
#                   (if nonexistent will be made, used and deleted)
#                   defaults to current working directory
#     ANOMGBEXEC    GRIB anomaly maker
#                   defaults to $$HOMEcfs/exec/cfs_anomgb
#     GRBINDEX      GRIB index maker
#                   defaults to $GRBINDEX 
#     CLMGRB        climate GRIB file
#                   defaults to $HOMEcfs/fix/clmgrb
#     CLMGRI        climate GRIB index file
#                   defaults to $$HOmEcfs/fix/clmgrb.index
#     VERBOSE       Verbose flag (YES or NO)
#                   defaults to NO
#
#   Exported Shell Variables:
#     PGM           Current program name
#     pgm
#     ERR           Last return code
#     err
#
#   Modules and files referenced:
#     programs   : $ANOMGBEXEC
#                  $GRBINDEX
#
#     input data : $1 or $PGBOUT
#                  $2 or $PGIOUT
#                  $CLMGRB
#                  $CLMGRI
#
#     output data: $1 or $PGBOUT
#                  $2 or $PGIOUT
#
#     scratch    : 
#
# Remarks:
#
#   Condition codes
#      0 - no problem encountered
#     >0 - some problem encountered
#
#   Control variable resolution priority
#     1 Command line argument.
#     2 Environment variable.
#     3 Inline default.
#
# Attributes:
#   Language: POSIX shell
#   Machine: IBM SP
#
####
################################################################################
#  Set environment.
set -eua
export VERBOSE=${VERBOSE:-"NO"}
if [[ "$VERBOSE" = "YES" ]]
then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi
#  Command line arguments.
export PGBOUT=${1:-${PGBOUT}}
export PGIOUT=${2:-${PGIOUT}}

#  Filenames.
export ANOMGBEXEC=${ANOMGBEXEC:-$HOMEcfs/exec/cfs_anomgb}
export CLMGRB=${CLMGRB:-$HOMEcfs/fix/clmgrb}
export CLMGRI=${CLMGRI:-$HOMEcfs/fix/clmgrb.index}

################################################################################
#  Preprocessing
pwd=$(pwd)
if [[ -d $DATA ]]
then
   mkdata=NO
else
   mkdir -p $DATA
   mkdata=YES
fi
cd $DATA||exit 99
################################################################################
#  Anomaly concatenation
export PGM=$ANOMGBEXEC
export pgm=$PGM
[[ -z $PGIOUT ]]&&xo="-x"||xo=""
[[ $VERBOSE = YES ]]&&vo="-X"||vo=""
[[ -s $CLMGRI ]]&&co="-c$CLMGRI"||co=""
da=dna$$
db=dnb$$
dc=dnc$$
rm -f $da $db $dc
$ANOMGBEXEC $vo -C$CLMGRB $co -k'4*-1,7,100,1000' $xo $PGBOUT $PGIOUT $da
$ANOMGBEXEC $vo -C$CLMGRB $co -k'4*-1,7,100,500' $xo $PGBOUT $PGIOUT $db
$ANOMGBEXEC $vo -C$CLMGRB $co -k'4*-1,222,100,500' $xo $PGBOUT $PGIOUT $dc
cat $da $db $dc >>$PGBOUT
err=$?
if [[ $err -eq 0 && -n $PGIOUT ]]
then
   $GRBINDEX $PGBOUT $PGIOUT
fi
rm -f $da $db $dc
################################################################################
#  Postprocessing
cd $pwd
[[ $mkdata = YES ]]&&rmdir $DATA
${ENDSCRIPT:-echo}
set +x
if [[ "$VERBOSE" = "YES" ]]
then
   echo $(date) EXITING $0 with return code $err >&2
fi
exit $err
