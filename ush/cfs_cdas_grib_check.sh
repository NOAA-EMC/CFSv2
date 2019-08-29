#!/bin/ksh
set -xeu

######################################################################
# This script checks grib files
#
# grib_check looks that all files between cdate1 and cdate2 at freq 
# 1) files exist
# 2) have the same date/fhour
# 3) internal and external dates/fhour check
# 3) the same number of records
#   Creator of original script - Bob Kistler
#   Updated by Patrick Tripp - Sept 2010
######################################################################

TEMPDIR=${TEMPDIR:-/stmp/$LOGNAME/grib_check.$$}
mkdir -p $TEMPDIR
if [ $# -lt 10 ] ; then 
	echo "$0 dir filetype cdump fhini fhout fhmax cdate1 cdate2 freq anl[0|1] prefix"
	err_exit 1
fi

dir=$1;filetype=$2;cdump=$3;fhini=$4;fhout=$5;fhmax=$6;cdate1=$7;cdate2=$8;freq=$9;anl=${10};prefix=${11}
prefix=${prefix:-f}
cd $dir
((fh=fhini-fhout))
if [ $anl -eq 1 ] ; then ((fh-=fhout)) ; fhmax=$((fh+fhout)) ; fi
while [ $((fh+=fhout)) -le $fhmax ] ; do 
	[ ${#fh} -lt 2 ] && fh=0$fh
	if [ $fh -lt 0 ] ; then fhr=${prefix}nl; else  fhr=${prefix}$fh ;fi
	cdate=$cdate1;while [ $cdate -le $cdate2 ] ; do
		file=$filetype$fhr.$cdump.$cdate
		nrec=$($WGRIB -4yr $file|wc -l) 
		[ $cdate -eq $cdate1 ] && nr1=$nrec
############### echo $cdate $cdate1 $file $nrec $nr1
		if [ $nr1 -ne $nrec ] ; then
			echo "record counts differ $nrec -ne $nr1 $file"; exit 1
		fi
		cdate=$($NDATE $freq $cdate)
	done
done
