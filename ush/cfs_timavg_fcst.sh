#!/usr/bin/env bash
#
#   This script creates monthly means of grib files.
#                  Shrinivas Moorthi - March 2010
#
#set -xeu
set -eu

if [[ $machine = WCOSS ]]; then
   export APRUN=mpirun.lsf
elif [[ $machine = WCRAY ]]; then
   export APRUN="aprun -n24"
fi

###export MP_COLLECTIVE_OFFLOAD=yes

export WINDEX=${WINDEX:-$GRBINDEX}

if [ $# -lt 5 ] ; then echo "Usage: $0 cdate cdate1 cdate2 fhint prefix1 [prefix2] [indir] [outdir] [ens_mem]  ";exit 1 ;fi

export cfss=${cfss:-"/cfs"}
export cfsp=${cfsp:-"cfs_"}
timeavgexec=${timeavgexec:-$EXECcfs/${cfsp}mpi_timavg_3d}
cfs_gribchksh=${cfs_gribchksh:-$HOMEcfs/ush/${cfsp}grib_checkf.sh}

cdate=$1 ; cdate1=$2  ; cdate2=$3 ; fhint=$4 ; prefix1=$5  ; prefix2=${6:-f}

INDIR=${7:-${INDIR:-${pwd}}}
OUTDIR=${8:-${OUTDIR:-${pwd}}}
ENS_MEM=${9:-${member:-01}}
RUNDIR=${RUNDIR:-$DATA/avrg_${cdate}_$ENS_MEM}
mkdir -p $RUNDIR
cd $RUNDIR

SUFIN=${SUFIN:-.$ENS_MEM.$cdate}
yyyy=$(echo $cdate1 | cut -c-4)
mm=$(echo $cdate1 | cut -c5-6)
SUFOUT=${SUFOUT:-${cdate}.${ENS_MEM}.${yyyy}$mm.avrg.grib}
REMOVE_INDEX=${REMOVE_INDEX:-NO}

fhini=$($NHOUR $cdate1 $cdate)
fhmax=$($NHOUR $cdate2 $cdate)
ntimes=$(((fhmax-fhini)/fhint+1))
iys=$(echo $cdate1 | cut -c1-4)
ims=$(echo $cdate1 | cut -c5-6)
ids=$(echo $cdate1 | cut -c7-8)
ihs=$(echo $cdate1 | cut -c9-10)
iye=$(echo $cdate2 | cut -c1-4)
ime=$(echo $cdate2 | cut -c5-6)
ide=$(echo $cdate2 | cut -c7-8)
ihe=$(echo $cdate2 | cut -c9-10)

# check the internal content of forecast grib files
if [ ${dogribchkfcst:-YES} = YES ] ; then
  $cfs_gribchksh $INDIR ${prefix1}$prefix2 $fhini $fhint $fhmax $SUFIN
fi

if [ $prefix2 = f -o $prefix2 = h -o $prefix2 = l ] ; then

  echo " &namin ntimes=$ntimes, nhours=$fhint,iys=$iys,ims=$ims, ids=$ids,ihs=$ihs,iye=$iye,ime=$ime,ide=$ide,ihe=$ihe, fcst_avrg=.true. /  " >input1

  fhr=$((fhini-fhint)); while [ $((fhr=10#$fhr+10#$fhint)) -le $fhmax ] ; do
    [ ${#fhr} -lt 2 ] && fhr=0$fhr
      file=$INDIR/${prefix1}${prefix2}${fhr}$SUFIN
    [ ! -s $file ] && exit 1
    if [ $REMOVE_INDEX = YES ] ; then /bin/rm $file.index ; fi
    if [ ! -s $file.index ] ; then
     $WINDEX $file $(basename $file).index
    else
     ${NCP:-/bin/cp} $file.index $(basename $file).index
    fi
    echo " &files cfile(1)='$file',cfile(2)='$(basename $file).index' /" >> input1
  done

  #export MP_EUILIBPATH=/u/IBM.Support4/lib13
  $APRUN $timeavgexec <input1 >>output 2>>output
  export err=$?; err_chk
  cat gribavg.* >gribavg
# $WGRIB -4yr gribavg|head

  [ -s fort.11 ] && rm fort.11

  ofile=${prefix1}${prefix2}${SUFOUT}
  ${NCP:-/bin/cp} gribavg $ofile
  $COPYGB -x  $ofile  ${ofile}_new
  $CNVGRIB -g12 -p40 ${ofile}_new ${ofile}.grb2
  $WGRIB2 ${ofile}.grb2 -s >${ofile}.grb2.idx
  mv ${ofile}.grb2 $OUTDIR/${ofile}.grb2
  mv ${ofile}.grb2.idx $OUTDIR/.
  if [ $SENDDBN = YES ]; then
    $DBNROOT/bin/dbn_alert MODEL CFS_FCST_MONTHLY $job $OUTDIR/${ofile}.grb2
    $DBNROOT/bin/dbn_alert MODEL CFS_FCST_MONTHLY_WIDX $job $OUTDIR/${ofile}.grb2.idx
  fi

# $WGRIB -4yr $OUTDIR/${prefix1}${prefix2}${fhr}${SUFOUT} |head

  set +e; rm gribavg* input1 *.index; set -e

else
  echo " Unsupported prefix2 "$prefix2
fi
