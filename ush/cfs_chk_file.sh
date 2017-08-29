#!/bin/ksh
set -xau

################################################################################
# Check file function
# Checks appropriate file sizes
#
# Patrick Tripp - Oct 2010
#

if [[ $# -ne 3 ]] ; then
  echo "Usage: chk_file <ftype> <cdump> <fname>"
  err_exit 1
fi

ftype=$1
cdump=$2
fname=$3

if [[ $ftype = "pgb" || $ftype = "ipv" || $ftype = "flx" ]] ; then
  # Filesize not checked for grib types
  exit 0
fi

echo "ftype is $ftype"
echo "cdump is $cdump"
echo "fname is $fname"

rval=0
esize=-1024   # prevent non-set esize and 0 size files
              # from slipping through error checks

if [[ $cdump = "gdas" ]] ; then

  case $ftype in

    sig|siganl|sigf00|sigf06)
      esize=511376936
      ;;

    sfc|sfcanl|sfcf00|sfcf06)
      esize=272591284
      ;;

    satang)
      esize=1084552
      ;;

    biascr)
      esize=139314
      ;;

    ocnanl.tar)
      esize=1242286080
      ;;

    noah.rst)
      esize=37691952
      ;;
    *)
      exit 0
      ;;
  esac

elif [[ $cdump = "gdas2" ]] ; then

  case $ftype in

    sig|siganl|sigf00|sigf06)
      esize=25103400
      ;;

    sfc|sfcanl|sfcf00|sfcf06)
      esize=12842064
      ;;
    *)
      exit 0
      ;;
  esac

else

  # only gdas,gdas2 checked so far
  exit 0
fi

fsize=0

if [[ -e $fname ]] ; then
  fsize=`ls -l $fname | awk '{print $5}'`

  if [[ $fsize -lt $esize ]] ; then
    echo "ERROR: $fname size:$fsize expected > $esize"
    ((rval+=1))
  fi
else
  echo "ERROR: $fname not found"
  ((rval+=1))
fi

export err=$rval; err_chk
#exit $rval

