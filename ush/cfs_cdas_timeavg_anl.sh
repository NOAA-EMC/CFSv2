#!/bin/ksh
set -xeu

#######################################################
#   This script creates monthly means of grib files.
#
#       Originally written by Shrinivas Moorthi
#       Updated by Patrick Tripp Sept 2010
#######################################################

export APRUN="mpiexec -n $NCPUS"
WINDEX=${WINDEX:-$GRBINDEX}
#export MP_STDOUTMODE=ordered
#export MP_LABELIO=yes
#export MP_HOLD_STDIN=yes
#export XLFRTEOPTS="buffering=disable_preconn"
CDUMP=${CDUMP:-"gfs"}
nknd=${nknd:-1}

if [ $# -lt 5 ] ; then echo "Usage: $0 vrfy|fcst cdate1 cdate2 nhours prefix1 prefix2 fhini fhmax fhout ";exit 1 ;fi

TIMEAVGEXEC=${TIMEAVGEXEC:-$HOMEcfs/exec/cfs_mpi_timavg_3d}
GRIBCHECKSH=${GRIBCHECKSH:-$HOMEcfs/ush/cfs_cdas_grib_check.sh}
OVERDATE=${OVERDATE:-$HOMEcfs/ush/cfs_overdate_grib.sh}
INDIR=${INDIR:-$COMROT}

mode=$1        ; cdate1=$2       ; cdate2=$3 ; nhours=$4
prefix1=$5     ; prefix2=${6:-f}
fhini=${7:-00} ; fhmax=${8:-120} ; fhout=${9:-6}

anl=0
nn=$(($(echo $prefix2 | wc -c)-1))
if [ $nn -gt 1 ] ; then
 xx=$(echo $prefix2 | cut -c$((nn-1))-$nn)
 if [ $xx = nl ] ; then anl=1 ; fi
fi
# check the internal content of grib files
if [ ${dogribchk:-YES} = YES ] ; then
 $GRIBCHECKSH $INDIR $prefix1 $CDUMP $fhini $fhout $fhmax $cdate1 $cdate2 $nhours $anl $(echo $prefix2 | cut -c1-1)
fi
RUNDIR=${RUNDIR:-/ptmp/$LOGNAME}
mkdir -p $RUNDIR
cd $RUNDIR
OUTDIR=${OUTDIR:-$(pwd)}
SUFOUT=${SUFOUT:-.${CDUMP}.$2:$3.avg}
REMOVE_INDEX=${REMOVE_INDEX:-NO}

tothr=$($NHOUR $cdate2 $cdate1)
ntimes=$((tothr/nhours+1))

if [ $prefix2 = f -o $prefix2 = h -o $prefix2 = l ] ; then
 ((fhr=fhini-fhout)); while [ $((fhr=10#$fhr+10#$fhout)) -le $fhmax ] ; do

  [ ${#fhr} -lt 2 ] && fhr=0$fhr
  case $mode in 
	fcst)	cdate=$cdate1;
			f=$prefix2 ;;
	vrfy)
			f=v ;
			((bhr=0+fhr)) ;
			cdate1=$($NDATE -$bhr $2)  ;
			cdate2=$($NDATE -$bhr $3) ;;
	*)		echo $mode not recognized;exit 1 ;;
  esac
  echo " &namin ntimes=$ntimes, nhours=$nhours / ">input1
  cdate=$cdate1
  m=0
  while [ $cdate -le $cdate2 ] ; do
   ((m+=1))
   if [ $nknd -gt 1 ] ; then
     file=$INDIR/${prefix1}${prefix2}$fhr.${CDUMP}$nknd.$cdate
   else
     file=$INDIR/${prefix1}${prefix2}$fhr.${CDUMP}.$cdate 
   fi
   [ ! -s $file ] && exit 1
   if [ $REMOVE_INDEX = YES ] ; then /bin/rm $file.index ; fi
   if [ ! -s $file.index ] ; then
    $WINDEX $file $(basename $file).index
   else
    ${NCP:-/bin/cp} -p $file.index $(basename $file).index
   fi
   echo " &files cfile(1)='$file',cfile(2)='$(basename $file).index' /" >> input1
   cdate=$($NDATE $nhours $cdate)
  done

  if [ ! -s $INDIR/${prefix1}${prefix2}$fhr.${CDUMP}.$cdate1 ] ; then exit 1;fi

  $APRUN $TIMEAVGEXEC <input1>mpi_avg.out
  cat gribavg.* >gribavg
# $WGRIB -4yr gribavg|head

  [ -s fort.11 ] && rm fort.11

  $WGRIB gribavg |$WGRIB gribavg -i -grib -o gribavgw
  $OVERDATE $2 gribavgw $OUTDIR/${prefix1}${f}${fhr}${SUFOUT}
# $WGRIB -4yr $OUTDIR/${prefix1}${f}${fhr}${SUFOUT} |head

  set +e; rm gribavg* input1 *.index; set -e

 done
else
  echo " &namin ntimes=$ntimes, nhours=$nhours / ">input1
  cdate=$cdate1
  m=0
  while [ $cdate -le $cdate2 ] ; do
   ((m+=1))
   if [ $nknd -gt 1 ] ; then
     file=$INDIR/${prefix1}${prefix2}.${CDUMP}$nknd.$cdate
   else
     file=$INDIR/${prefix1}${prefix2}.${CDUMP}.$cdate
   fi
   [ ! -s $file ] && exit 1
   if [ $REMOVE_INDEX = YES ] ; then /bin/rm $file.index ; fi
   if [ ! -s $file.index ] ; then
    $WINDEX $file $(basename $file).index
   else
    ${NCP:-/bin/cp} -p $file.index $(basename $file).index
   fi
   echo " &files cfile(1)='$file',cfile(2)='$(basename $file).index' /" >> input1
   cdate=$($NDATE $nhours $cdate)
  done

  if [ ! -s $INDIR/${prefix1}${prefix2}.${CDUMP}.$cdate1 ] ; then exit 1;fi

  $APRUN $TIMEAVGEXEC <input1
  cat gribavg.* >gribavg
# $WGRIB -4yr gribavg|head

  [ -s fort.11 ] && rm fort.11

  $WGRIB gribavg |$WGRIB gribavg -i -grib -o gribavgw
  $OVERDATE $2 gribavgw $OUTDIR/${prefix1}${prefix2}${SUFOUT}
# $WGRIB -4yr $OUTDIR/${prefix1}${prefix2}${SUFOUT} |head

  set +e; rm gribavg* input1 *.index; set -e
fi
