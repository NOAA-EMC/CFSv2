#!/bin/ksh

######################################################################
## Script to determine and output the last day of a month
## Input: YYYY - a year, MM - a month
## Output: the last day of the month
##
## Original script written by: Unknown
## Modified by Patrick Tripp to use ndate - Sept 2010
######################################################################

if [ $# -ne 2 ] ; then
  echo "Usage: $0 yyyy mm"
  err_exit -2
fi


yyyy=$1
mm=$2 

nextmm=`expr $mm + 1`
if [ `expr $mm : '.*'` -lt 2 ] ; then mm=0$mm ;fi
nextyyyy=$yyyy
if [ $nextmm -eq 13 ] ; then
	nextmm=01
	((nextyyyy+=1))
fi
if [ `expr $nextmm : '.*'` -lt 2 ] ; then nextmm=0$nextmm ;fi

nextdate=$nextyyyy${nextmm}0100
lastday=`$NDATE -6 $nextdate | cut -c7-8`
echo "$lastday"

