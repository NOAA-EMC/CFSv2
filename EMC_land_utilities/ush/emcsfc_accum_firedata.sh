#!/bin/sh

####  UNIX Script Documentation Block ###################################
#                      .                                             .
# Script name:  emcsfc_accum_firedata.sh
# RFC Contact:  George Gayno
# Abstract:  This script calls the emcsfc_accum_firedata program to 
#    accumulate sub-daily fire (burned area) data over a
#    user specified period.
#
# Script History Log:
#    02/2015  Gayno   Initial version
#
# Usage:
#    emcsfc_accum_firedata.sh $START_DATE $END_DATE $FILE_RESOLUTION
#        $START_DATE      - start of accumulation period (yyyymmddhh)
#        $END_DATE        - end of accumulation period (yyyymmddhh)
#        $FILE_RESOLUTION - file resolution in km: 1 or 12
#
# Script Parameters:
# Modules and Files references:
#    input files are located in:
#        $INPUT_FILE_DIR - files are grib 2.  The 1/12km data is valid
#                          every twelve/six hours.
#
#    output file:
#        $OUTPUT_FIRE_FILE (accumulate fire/burned area data, grib2)
#
#    executables: /nwprod/emcsfc.vX.Y.Z/exec/emcsfc_accum_firedata - 
#                 $ACCUM_FIRE_EXEC
#
# Condition codes:
#  0    - normal termination
#  1    - incorrect number of script arguments
#  $rc  - non-zero status indicates a problem in emcsfc_accum_firedata
#         execution.  see source code for details -
#         /nwprod/emcsfc.vX.Y.Z/sorc/emcsfc_accum_firedata.fd
#
# If a non-zero status occurs, no accumulated fire data will be created.
# This is not fatal to the model executation.  But any problems should
# be investigated.
#
# Attributes:
#     Language:  RedHat Linux
#     Machine:   NCEP WCOSS
#
#########################################################################


VERBOSE=${VERBOSE:-"YES"}
if [[ "$VERBOSE" == YES ]]; then
  set -x
fi

export jlogfile=${jlogfile:-"jlogfile"}

export pgm=emcsfc_accum_firedata
if [ -f startmsg ]; then
  startmsg
fi

#------------------------------------------------------------------------
# First/second script arguments are the start/end of the 
# accumulation period ($yyyymmddhh).  The third argument is the
# file resolution in km - either 1 or 12.
#------------------------------------------------------------------------

if [ $# -ne 3 ]; then
  msg="WARNING: ${pgm} detects incorrect script arguments. Can not run."
  if [ -f postmsg ]; then
    postmsg "$jlogfile" "$msg"
  fi
  exit 1
fi

START_DATE=$1
START_YEAR=$(echo $START_DATE | cut -c1-4)
START_MONTH=$(echo $START_DATE | cut -c5-6)
START_DAY=$(echo $START_DATE | cut -c7-8)
START_HOUR=$(echo $START_DATE | cut -c9-10)

END_DATE=$2
END_YEAR=$(echo $END_DATE | cut -c1-4)
END_MONTH=$(echo $END_DATE | cut -c5-6)
END_DAY=$(echo $END_DATE | cut -c7-8)
END_HOUR=$(echo $END_DATE | cut -c9-10)

FILE_RESOLUTION=$3

#------------------------------------------------------------------------
# Program executable.
#------------------------------------------------------------------------

envir=${envir:-"prod"}
HOMEemcsfc=${HOMEemcsfc:-/nw${envir}/emcsfc.${emcsfc_ver:?}}
EXECemcsfc=${EXECemcsfc:-$HOMEemcsfc/exec}

ACCUM_FIRE_EXEC=${ACCUM_FIRE_EXEC:-${EXECemcsfc}/emcsfc_accum_firedata}

#------------------------------------------------------------------------
# Working directory.
#------------------------------------------------------------------------

DATA=${DATA:-$(pwd)}
if [ ! -d $DATA ]; then
  mkdir -p $DATA
fi
cd $DATA

#------------------------------------------------------------------------
# The location of the input files.  And the name of the output file.
#------------------------------------------------------------------------

INPUT_FIRE_DIR=${INPUT_FIRE_DIR:-$(pwd)}

OUTPUT_FIRE_FILE=${OUTPUT_FIRE_FILE:-"accum_fire.grib2"}

#------------------------------------------------------------------------
# Create program namelist file and run program.
#------------------------------------------------------------------------

rm -f ./fort.41
cat > ./fort.41 << !
 &setup
   start_year=${START_YEAR}
   start_month=${START_MONTH}
   start_day=${START_DAY}
   start_hour=${START_HOUR}
   end_year=${END_YEAR}
   end_month=${END_MONTH}
   end_day=${END_DAY}
   end_hour=${END_HOUR}
   file_resolution_in_km=${FILE_RESOLUTION}
   input_file_dir="${INPUT_FIRE_DIR}"
   output_file="${OUTPUT_FIRE_FILE}"
 /
!

pgmout=${pgmout:-OUTPUT.ACCUM.FIRE}

mpirun $ACCUM_FIRE_EXEC >> $pgmout 2> errfile
rc=$?

if ((rc != 0));then
  msg="WARNING: ${pgm} completed abnormally."
  if [ -f postmsg ]; then
    postmsg "$jlogfile" "$msg"
  fi
  exit $rc
else
  msg="${pgm} completed normally."
  if [ -f postmsg ]; then
    postmsg "$jlogfile" "$msg"
  fi
fi

rm -f ./fort.41

exit 0
