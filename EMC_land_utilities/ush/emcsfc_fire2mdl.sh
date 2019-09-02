#!/bin/sh

####  UNIX Script Documentation Block ###################################
#                      .                                             .
# Script name:  emcsfc_fire2mdl.sh
# RFC Contact:  George Gayno
# Abstract:  This script calls the emcsfc_fire2mdl program to map
#    fire (burned area) data to a model grid.
#
# Script History Log:
#    02/2015  Gayno   Initial version
#
# Usage:
#    emcsfc_fire2mdl.sh [no arguments]
#
# Script Parameters:
# Modules and Files references:
#    fix:   ${MODEL_LSMASK_FILE} (model landmask, grib 1 or 2)
#           ${MODEL_LATITUDE_FILE} (model latitude, grib 1 or 2)
#           ${MODEL_LONGITUDE_FILE} (model longitude, grib 1 or 2)
#
#    input files (may choose one or both:
#           ${NESDIS_1KM_FIRE_FILE}   (1km burned area data, grib 2)
#           ${NESDIS_12KM_FIRE_FILE} (12km burned area data, grib 2)
#
#    output file:
#           ${MODEL_FIRE_FILE} (burned area data on model grid, grib 2)
#
#    executables: /nwprod/emcsfc.vX.Y.Z/exec/emcsfc_fire2mdl -
#                 ${FIRE2MDLEXEC}
#
# Condition codes:
#  0    - normal termination
#  3    - no input burned area data specified
#  $rc  - non-zero status indicates a problem in emcsfc_fire2mdl execution.
#         see source code for details - /nwprod/emcsfc.vX.Y.Z/sorc/emcsfc_fire2mdl.fd
#
# If a non-zero status occurs, no model fire analysis will be created.
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

export pgm=emcsfc_fire2mdl
if [ -f startmsg ]; then
  startmsg
fi

envir=${envir:-"prod"}
HOMEemcsfc=${HOMEemcsfc:-/nw${envir}/emcsfc.${emcsfc_ver:?}}
EXECemcsfc=${EXECemcsfc:-$HOMEemcsfc/exec}

#------------------------------------------------------------------------
# The program executable.
#------------------------------------------------------------------------

FIRE2MDLEXEC=${FIRE2MDLEXEC:-${EXECemcsfc}/emcsfc_fire2mdl}

#------------------------------------------------------------------------
# Input fire (burned area) data.  May choose 1km, 12km or both.
# Exit with error if no data chosen.
#------------------------------------------------------------------------

NESDIS_1KM_FIRE_FILE=${NESDIS_1KM_FIRE_FILE:-""}
NESDIS_12KM_FIRE_FILE=${NESDIS_12KM_FIRE_FILE:-""}

size1km=${#NESDIS_1KM_FIRE_FILE}
size12km=${#NESDIS_12KM_FIRE_FILE}

if ((size1km == 0 && size12km == 0)); then
  msg="WARNING: ${pgm} detects no input fire data. Can not run."
  if [ -f postmsg ]; then
    postmsg "$jlogfile" "$msg"
  fi
  exit 3
fi

#------------------------------------------------------------------------
# Fixed files that describe the model grid: landmask, 
# latitudes/longitudes.
#------------------------------------------------------------------------

MODEL_LATITUDE_FILE=${MODEL_LATITUDE_FILE:?}
MODEL_LONGITUDE_FILE=${MODEL_LONGITUDE_FILE:?}
MODEL_LSMASK_FILE=${MODEL_LSMASK_FILE:?}

#------------------------------------------------------------------------
# Output fire data on model grid.
#------------------------------------------------------------------------

MODEL_FIRE_FILE=${MODEL_FIRE_FILE:-"fire_model.grib2"}

#------------------------------------------------------------------------
# working directory
#------------------------------------------------------------------------

DATA=${DATA:-$(pwd)}
if [ ! -d $DATA ]; then
  mkdir -p $DATA
fi
cd $DATA

if [ -f prep_step ]; then
  . prep_step
fi

#------------------------------------------------------------------------
# Create program namelist file and run program.
#------------------------------------------------------------------------

rm -f ./fort.41
cat > ./fort.41 << !
 &source_data
  nesdis_1km_fire_file="${NESDIS_1KM_FIRE_FILE}"
  nesdis_12km_fire_file="${NESDIS_12KM_FIRE_FILE}"
 /
 &model_specs
  model_lat_file="${MODEL_LATITUDE_FILE}"
  model_lon_file="${MODEL_LONGITUDE_FILE}"
  model_lsmask_file="${MODEL_LSMASK_FILE}"
 /
 &output_data
  model_fire_file="${MODEL_FIRE_FILE}"
 /
!

pgmout=${pgmout:-OUTPUT.FIRE2MDL}

eval $FIRE2MDLEXEC >> $pgmout 2> errfile
rc=$?

rm -f ./fort.41

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

exit 0
