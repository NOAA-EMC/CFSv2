#!/bin/ksh
set -x
#
#  creates pgb, averaging and ftp archive script (clim_post.sh) and 
#  execute/submit. with this method, clim_post.sh can run independently
#  from any other job, allowing parallel processing with forecast.
#  this script is written for seasonal prediction execution
#
#  $1 ... if 'batch' then clim_post.sh runs as a separate process, else
#         regular serial execution
#  $2 ... directory where execution is taking place.  Since this job
#         execute as a separate process, it needs to know where to
#         get files.
#  $3 ... starting date, starting forecast hour, ending forecast
#         hour, coupling interval (in hours), separated by ':'
#  $4 ... forecast ending date, forecast  hour to the end,
#         forecast hour for this segment,
#         diagnostic file output interval, sigma file output interval,
#         and ending hour for this cycle separated by ':'
#  $5 ... archive directory of the target ftp
#  $6 ... ftp ip address
#
#  note that the following variables must be passed as environmental variables
#  otherwise default values defined in this script will be used
#
#  LIST_OF_FILES
#  FILES_TO_BE_KEPT
#  FILES_TO_BE_AVERAGED
#  FTP_SIG
#  FTP_SFC
#  FTP_ZNL
#  FTP_FLX
#  FTP_DIA
#  FTP_KEN
#  FTP_PGB
#  FTP_SGB
#  FTP_SFCANL
#  FTP_SFCOUT
#  FTP_FCSTOUT
#  FTP_EXTVARS
#  RMAVRG
#  VARIANCE
#
LIST_OF_FILES=${LIST_OF_FILES:='sig sfc znl flx d3d pgb sgb sfcanl sfcout fcstout extvars'}
FILES_TO_BE_KEPT=${FILES_TO_BE_KEPT:='extvars'}
FILES_TO_BE_AVERAGED=${FILES_TO_BE_AVERAGED:='flx pgb'}
FTP_SIG=${FTP_SIG:='NO'}
FTP_SFC=${FTP_SFC:='NO'}
FTP_ZNL=${FTP_ZNL:='NO'}
FTP_FLX=${FTP_FLX:='NO'}
FTP_DIA=${FTP_DIA:='NO'}
FTP_KEN=${FTP_KEN:='NO'}
FTP_PGB=${FTP_PGB:='NO'}
FTP_SGB=${FTP_SGB:='NO'}
FTP_SFCANL=${FTP_SFCANL:='NO'}
FTP_SFCOUT=${FTP_SFCOUT:='NO'}
FTP_FCSTOUT=${FTP_FCSTOUT:='NO'}
FTP_EXTVARS=${FTP_EXTVARS:='NO'}
RMAVRG=${RMAVRG:='YES'}
VARIANCE=${VARIANCE:='YES'}
#
BATCH=$1
INPDIR=$2
start_date=`echo $3 | cut -d':' -f1`
FHS=`echo $3 | cut -d':' -f2`
FHE=`echo $3 | cut -d':' -f3`
FHCYC=`echo $3 | cut -d':' -f4`
end_date=`echo $4 | cut -d':' -f1`
ENDHOUR=`echo $4 | cut -d':' -f2`
INCHOUR=`echo $4 | cut -d':' -f3`
INTFLX=`echo $4 | cut -d':' -f4`
INTSIG=`echo $4 | cut -d':' -f5`
nhours=`echo $4 | cut -d':' -f6`
OUTDIR=$5
#PLATFORM=$6
MEMBER=$6
CLIMDIR=$7
VYYYYMM=$8
#
YMDH=$start_date
cat <<EOF >post_def$cyc.$YMDH-$FHS-$FHE
export start_date=$start_date
export end_date=$end_date
export FHS=$FHS
export FHE=$FHE
export FHCYC=$FHCYC
export ENDHOUR=$ENDHOUR
export INCHOUR=$INCHOUR
export INTFLX=$INTFLX
export INTSIG=$INTSIG
export nhours=$nhours
export LIST_OF_FILES="$LIST_OF_FILES"
export FILES_TO_BE_KEPT="$FILES_TO_BE_KEPT"
export FILES_TO_BE_AVERAGED="$FILES_TO_BE_AVERAGED"
export FILES_TO_SEND_COM="$FILES_TO_SEND_COM"
export FTP_SIG=$FTP_SIG
export FTP_SFC=$FTP_SFC
export FTP_ZNL=$FTP_ZNL
export FTP_FLX=$FTP_FLX
export FTP_DIA=$FTP_DIA
export FTP_KEN=$FTP_KEN
export FTP_PGB=$FTP_PGB
export FTP_SGB=$FTP_SGB
export FTP_SFCANL=$FTP_SFCANL
export FTP_SFCOUT=$FTP_SFCOUT
export FTP_FCSTOUT=$FTP_FCSTOUT
export FTP_EXTVARS=$FTP_EXTVARS
export RMAVRG=$RMAVRG
export VARIANCE=$VARIANCE
export MEMBER=$MEMBER
export VYYYYMM=$VYYYYMM
EOF

mv post_def$cyc.$YMDH-$FHS-$FHE $POSTDEFS/post_def$cyc.$YMDH-$FHS-$FHE
chmod a+x $POSTDEFS/post_def$cyc.$YMDH-$FHS-$FHE
