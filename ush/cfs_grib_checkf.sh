#!/bin/ksh
set -u
# grib_checkf looks at all forecast files between hours fhini and fhmax
# at interval fhout for a given cdate
# 1) files exist
# 2) have the same date/fhour
# 3) internal and external dates/fhour check
# 3) the same number of records
#   Creator of original script - Bob Kistler
#   Adapted by Shrinivas Moorthi - March 2010
#
#TEMPDIR=${TEMPDIR:-/stmp/$LOGNAME/grib_check.$$}
#mkdir -p $TEMPDIR

if [ $# -lt 6 ] ; then 
	echo "$0 dir filetype fhini fhout fhmax sufin"
	exit 1
fi

dir=$1;filetype=$2;fhini=$3;fhout=$4;fhmax=$5;sufin=$6
cd $dir
((fh=fhini-fhout))
while [ $((fh+=fhout)) -le $fhmax ] ; do 
	[ ${#fh} -lt 2 ] && fh=0$fh
	if [ $fh -lt 0 ] ; then fhr=nl; else  fhr=$fh ;fi
	file=${filetype}${fhr}$sufin
	nrec=$($WGRIB -4yr $file|wc -l) 
	[ $fh -eq $fhini ] && nr1=$nrec
	if [ $nr1 -ne $nrec ] ; then
	echo "record counts differ $nrec -ne $nr1 $file"; exit 1
	fi


#               tfile=$TEMPDIR/tfile
#		$WGRIB -4yr $file|awk -F: '{print $3,$10}'|sort -u > $tfile
#		n=0;while read gdate gp2;do ((n+=1))
#			eval $gdate; eval $gp2
#			if [ $n -gt 1 ] ; then
#				echo "multiple dates in $file"
#				cat $tfile
#				exit 1
#			fi
#			if [ $d -ne $cdate -a $fh -ne $P2 ] ; then
#				echo $file
#				echo "$d -ne $cdate -a $fh -ne $P2"
#				exit 1
#			fi
#		done < $tfile; rm -f tfile  

done

exit 0
