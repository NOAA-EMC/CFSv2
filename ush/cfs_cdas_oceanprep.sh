#!/bin/ksh
#
#   This script prepares ocean observations for use in MOM4 global data 
#   assimilation.
#   Original author:           Dave Behringer
#   Updated for use in CFSV2 : Shrinivas Moorthi
#
#
if [ $# -lt 2 ]; then
  echo "Must provide <job id> and <log name> as arguments"
  err_exit 99
fi

export machine=DELL
export aprun="" 

set -x
export cfss=${cfss:-"/cfs"}
export jobb=$1
export username=$2
#
execdir_godasprep=${execdir_godasprep:-$EXECcfs}
cfsg=${cfsg:-"cfs_cdas_godas_"}
fixdir=${fixdir_oprep:-$FIX_OCN}

if [ $# -lt 3 ]; then
  setpdy.sh 
  . ./PDY
  export dte=${PDYm3}$cyc
  export dtem=${PDYm4}$cyc
else
  if [ $3 -ge 1000000000 ]; then
    export dte=$3
    export dtem=$($NDATE -24 $dte)
  else
    echo "If no date is given, today's date with a lag of 3 is assumed."
    echo "If a date is provided, the format is yyyymmddhh"
    err_exit 99
  fi
fi

export CDUMP=${4:-${CDUMP:-gdas}}
export DMP_SUF=${DMP_SUF:-""}

if [ $machine = IBM ]; then
  XLFRTEOPTS="unit_vars=yes"
fi

export logfile=log.dump
#
###############################################################################
#                                                                             #
# The data dump includes temperature profiles and 1/4 degree daily SST.       #
# This script does not dump SBC data.                                         #
#                                                                             #
###############################################################################

echo "#############################################################" > $logfile
msg="Begin GODAS DATA DUMP PROCESSING for $job on `hostname`"
echo $msg >> $logfile
echo "#############################################################" >> $logfile
echo " " >> $logfile
echo "#############################################################" >> $logfile
msg="Begin DUMP PROCESSING for TEMPERATURE PROFILES"
echo $msg >> $logfile
echo "#############################################################" >> $logfile

ndays=${GODAS_WNDO:-10}
model_end_day=$dte


mDy=$((ndays/2))
cDate=$(echo $($NDATE -$((mDy*24)) $dte) | cut -c1-8)
cHour=12
cTime=$cDate$cHour
cRad=$(($mDy * 24 + 11)).999

echo "Temperature profile dump for ${dte}, using previous ${ndays} days"
echo "#############################################################" >> $logfile
echo "Temperature profile dump for ${dte}, using previous ${ndays} days" >> $logfile
echo "#############################################################" >> $logfile

#echo $dte $nDays > model_date_info.dump

echo " " >> $logfile
echo "#############################################################" >> $logfile
msg="Using Climate Dump directory"
echo $msg >> $logfile
echo "#############################################################" >> $logfile
echo " " >> $logfile

if [ $RUN_ENVIR = nco  -o $RUN_ENVIR = devpara ] ; then
  DBUOY=${DBUOY:-$COMIN/cdas1.t${cyc}z.cdbuoy.tm00.mbufr_d}
  BATHY=${BATHY:-$COMIN/cdas1.t${cyc}z.cbathy.tm00.mbufr_d}
  TESAC=${TESAC:-$COMIN/cdas1.t${cyc}z.ctesac.tm00.mbufr_d}
else
  DBUOY=${DBUOY:-$COMIN/$dte/${CDUMP}$DMP_SUF/cbathy.$CDUMP.$dte}
  BATHY=${BATHY:-$COMIN/$dte/${CDUMP}$DMP_SUF/ctesac.$CDUMP.$dte}
  TESAC=${TESAC:-$COMIN/$dte/${CDUMP}$DMP_SUF/cdbuoy.$CDUMP.$dte}
fi
cp $DBUOY   dbuoy.ibm
cp $BATHY   bathy.ibm
cp $TESAC   tesac.ibm 

ln -sf 
set -A cType `echo bathy tesac`
n=0
while [ $n -lt ${#cType[*]} ]; do
  echo ${cType[$n]}
  echo ${cType[$n]} >> $logfile
  if [ $machine = IBM ]; then
    export XLFUNIT_11="${cType[$n]}.ibm"
    export XLFUNIT_51="${cType[$n]}.noqc"
    export XLFUNIT_61="${cType[$n]}.noqc.asc"
  else
    ln -sf ${cType[$n]}.ibm fort.11
    ln -sf ${cType[$n]}.noqc fort.51
    ln -sf ${cType[$n]}.noqc.asc fort.61
  fi

  $aprun $execdir_godasprep/${cfsg}bathybfr >> $logfile
  export err=$?; err_chk

  ((n=$n+1))
done

cType=dbuoy
echo $cType
echo $cType >> $logfile

nDm=$((ndays-1))
export StartDY=$(echo $($NDATE -$((nDm*24)) $dtem) | cut -c1-8)
nDp=$((ndays+1))

echo $StartDY $nDp > args
if [ $machine = IBM ]; then
  export XLFUNIT_11="dbuoy.ibm"
  export XLFUNIT_31="$fixdir/godas_tao.txt"
  export XLFUNIT_32="$fixdir/godas_triton.txt"
  export XLFUNIT_39="$fixdir/godas_rejectbuoy.txt"
  export XLFUNIT_51="dcom_buoy.noqc"
  export XLFUNIT_61="dcom_buoy.noqc.asc"
else
  ln -sf dbuoy.ibm fort.11
  ln -sf $fixdir/godas_tao.txt fort.31
  ln -sf $fixdir/godas_triton.txt fort.32
  ln -sf $fixdir/godas_rejectbuoy.txt fort.39
  ln -sf dcom_buoy.noqc fort.51
  ln -sf dcom_buoy.noqc.asc fort.61
fi

$aprun $execdir_godasprep/${cfsg}buoybtbfr < args >> $logfile
export err=$?; err_chk

cp dcom_buoy.noqc buoy.noqc

#############################disabled###########################################

#echo " " >> $logfile
#echo "############################################################" >> $logfile
#msg="Begin DUMP PROCESSING for SST"
#echo $msg >> $logfile
#echo "############################################################" >> $logfile


export logfile=log.prep

###############################################################################
#                                                                             #
# This part of the script prepares the data gathered by above.                #
# The data includes temperature profiles and 1/4 degree daily SST.            #
#                                                                             #
###############################################################################

omres=${omres:-05}
export  GRIDSPEC=${GRIDSPEC:-$FIX_OCN/grid_spec_$omres.nc}

echo " " >> $logfile
echo "#############################################################" >> $logfile
msg="Begin GODAS DATA PREPARATION for $job on `hostname`"
echo $msg >> $logfile
echo "#############################################################" >> $logfile
echo " " >> $logfile
echo "#############################################################" >> $logfile
msg="Begin QC of TEMPERATURE PROFILES"
echo $msg >> $logfile
echo "#############################################################" >> $logfile
echo " " >> $logfile


# If ndays not set, use default of 10
echo "#############################################################" >> $logfile
echo ${ndays:=10} >> $logfile
echo "#############################################################" >> $logfile

# In a coupled run model_start_day should equal model_end_day and no lag
echo "#############################################################" >> $logfile
echo ${model_end_day:=${PDY}$cyc} >> $logfile
echo "#############################################################" >> $logfile

model_start_day=$model_end_day

#echo $model_start_day $model_end_day $ndays > model_date_info.prep

# Delete the "cyc" in the following two lines
export StartDY=$(echo $($NDATE -$((ndays*24)) ${model_start_day}) | cut -c1-8)
export EndDY=$(echo $($NDATE -24 ${model_start_day}) | cut -c1-8)

echo $StartDY $EndDY >> $logfile

echo "#############################################################" >> $logfile
# set the time stamp of the restart tar file used to qc the temperature files
echo "#############################################################" >> $logfile
yrM=`echo $model_start_day | cut -c -4`
moM=`echo $model_start_day | cut -c 5-6`
dyM=`echo $model_start_day | cut -c 7-8`
export dStamp=$yrM.$moM.$dyM

echo "#############################################################" >> $logfile
echo $dStamp >> $logfile
echo "#############################################################" >> $logfile

echo "#############################################################" >> $logfile
# Check for data availability
echo "#############################################################" >> $logfile

if [ ! -s bathy.noqc ]; then
  echo "#############################################################" >> $logfile
  echo "BATHY data not found" >> $logfile
  echo "#############################################################" >> $logfile
  exit 99
fi
if [ ! -s tesac.noqc ]; then
  echo "#############################################################" >> $logfile
  echo "TESAC data not found" >> $logfile
  echo "#############################################################" >> $logfile
  exit 99
fi
if [ ! -s buoy.noqc ]; then
  echo "#############################################################" >> $logfile
  echo "BUOY data not found" >> $logfile
  echo "#############################################################" >> $logfile
  exit 99
fi

if [ -s $GRIDSPEC ]; then
  ln -sf $GRIDSPEC grid_spec.nc
else
  echo "#############################################################" >> $logfile
  echo "GRID_SPEC FILE not found" >> $logfile
  echo "#############################################################" >> $logfile
  exit 99
fi

if [ -s $fixdir/WOA_T_Climate.nc ]; then
  ln -sf $fixdir/WOA_T_Climate.nc WOA.nc
else
  echo "#############################################################" >> $logfile
  echo "WOA_T_CLIMATE FILE not found" >> $logfile
  echo "#############################################################" >> $logfile
  exit 99
fi

echo " " >> $logfile
echo "############################################################" >> $logfile
echo "     Edit bathy" >> $logfile
echo "############################################################" >> $logfile
echo " " >> $logfile

if [ $machine = IBM ]; then
  export XLFUNIT_11="bathy.noqc"
  export XLFUNIT_51="bathy.srt"
else
  ln -sf bathy.noqc fort.11
  ln -sf bathy.srt fort.51
fi
$aprun $execdir_godasprep/${cfsg}tmSrtPrf >> $logfile
export err=$?; err_chk

if [ $machine = IBM ]; then
  export XLFUNIT_85="bathy.err"
  export XLFUNIT_86="bathy.erw"
  export XLFUNIT_87="bathy.der"

  export XLFUNIT_80="bathy.woa"
  export XLFUNIT_81="bathy.dff"
  export XLFUNIT_11="bathy.srt"
  export XLFUNIT_51="bathy.edt"
else
  ln -sf bathy.err fort.85
  ln -sf bathy.erw fort.86
  ln -sf bathy.der fort.87

  ln -sf bathy.woa fort.80
  ln -sf bathy.dff fort.81
  ln -sf bathy.srt fort.11
  ln -sf bathy.edt fort.51
fi
$aprun $execdir_godasprep/${cfsg}editWPrf >> $logfile
export err=$?; err_chk

if [ $machine = IBM ]; then
  export XLFUNIT_11="bathy.edt"
  export XLFUNIT_51="bathy.dav"
else
  ln -sf bathy.edt fort.11
  ln -sf bathy.dav fort.51
fi
$aprun $execdir_godasprep/${cfsg}avePrfDly >> $logfile
export err=$?; err_chk

echo " " >> $logfile
echo "############################################################" >> $logfile
echo "     Edit tesac" >> $logfile
echo "############################################################" >> $logfile
echo " " >> $logfile

if [ $machine = IBM ]; then
  export XLFUNIT_11="tesac.noqc"
  export XLFUNIT_51="tesac.srt"
else
  ln -sf tesac.noqc fort.11
  ln -sf tesac.srt fort.51
fi
$aprun $execdir_godasprep/${cfsg}tmSrtPrf >> $logfile
export err=$?; err_chk

if [ $machine = IBM ]; then
  export XLFUNIT_85="tesac.err"
  export XLFUNIT_86="tesac.erw"
  export XLFUNIT_87="tesac.der"

  export XLFUNIT_80="tesac.woa"
  export XLFUNIT_81="tesac.dff"
  export XLFUNIT_11="tesac.srt"
  export XLFUNIT_51="tesac.edt"
else
  ln -sf tesac.err fort.85
  ln -sf tesac.erw fort.86
  ln -sf tesac.der fort.87

  ln -sf tesac.woa fort.80
  ln -sf tesac.dff fort.81
  ln -sf tesac.srt fort.11
  ln -sf tesac.edt fort.51
fi
$aprun $execdir_godasprep/${cfsg}editWPrf >> $logfile
export err=$?; err_chk

if [ $machine = IBM ]; then
  export XLFUNIT_11="tesac.edt"
  export XLFUNIT_51="tesac.dav"
else
  ln -sf tesac.edt fort.11
  ln -sf tesac.dav fort.51
fi
$aprun $execdir_godasprep/${cfsg}avePrfDly >> $logfile
export err=$?; err_chk

echo " " >> $logfile
echo "############################################################" >> $logfile
echo "     Edit Buoy" >> $logfile
echo "############################################################" >> $logfile
echo " " >> $logfile

if [ $machine = IBM ]; then
  export XLFUNIT_11="buoy.noqc"
  export XLFUNIT_51="buoy.srt"
else
  ln -sf buoy.noqc fort.11
  ln -sf buoy.srt fort.51
fi
$aprun $execdir_godasprep/${cfsg}tmSrtPrf >> $logfile
export err=$?; err_chk

if [ $machine = IBM ]; then
  export XLFUNIT_85="buoy.err"
  export XLFUNIT_86="buoy.erw"
  export XLFUNIT_87="buoy.der"

  export XLFUNIT_80="buoy.woa"
  export XLFUNIT_81="buoy.dff"
  export XLFUNIT_11="buoy.srt"
  export XLFUNIT_51="buoy.edt"
else
  ln -sf buoy.err fort.85
  ln -sf buoy.erw fort.86
  ln -sf buoy.der fort.87

  ln -sf buoy.woa fort.80
  ln -sf buoy.dff fort.81
  ln -sf buoy.srt fort.11
  ln -sf buoy.edt fort.51
fi
$aprun $execdir_godasprep/${cfsg}editWPrf >> $logfile
export err=$?; err_chk

if [ $machine = IBM ]; then
  export XLFUNIT_11="buoy.edt"
  export XLFUNIT_51="buoy.dav"
else
  ln -sf buoy.edt fort.11
  ln -sf buoy.dav fort.51
fi
$aprun $execdir_godasprep/${cfsg}avePrfDly >> $logfile
export err=$?; err_chk

echo " " >> $logfile
echo "############################################################" >> $logfile
echo "     Merge bathy and tesac" >> $logfile
echo "############################################################" >> $logfile
echo " " >> $logfile

if [ $machine = IBM ]; then
  export XLFUNIT_11="bathy.dav"
  export XLFUNIT_12="tesac.dav"
  export XLFUNIT_51="bat_tes.edt"
else
  ln -sf bathy.dav fort.11
  ln -sf tesac.dav fort.12
  ln -sf bat_tes.edt fort.51
fi
$aprun $execdir_godasprep/${cfsg}mrgPrf >> $logfile
export err=$?; err_chk

echo " " >> $logfile
echo "############################################################" >> $logfile
echo "     Merge bathy/tesac and buoy" >> $logfile
echo "############################################################" >> $logfile
echo " " >> $logfile

if [ $machine = IBM ]; then
  export XLFUNIT_11="bat_tes.edt"
  export XLFUNIT_12="buoy.dav"
  export XLFUNIT_51="tmpa.edt"
else
  ln -sf bat_tes.edt fort.11
  ln -sf buoy.dav fort.12
  ln -sf tmpa.edt fort.51
fi
$aprun $execdir_godasprep/${cfsg}mrgPrf >> $logfile
export err=$?; err_chk

echo " " >> $logfile
echo "############################################################" >> $logfile
echo "     Write temperature profiles to assimilation file" >> $logfile
echo "############################################################" >> $logfile
echo " " >> $logfile

if [ $machine = IBM ]; then
  export XLFUNIT_11="tmpa.edt"
  export XLFUNIT_51="tmpa.mom"
else
  ln -sf tmpa.edt fort.11
  ln -sf tmpa.mom fort.51
fi

$aprun $execdir_godasprep/${cfsg}mkDlyAsmPrf $model_start_day >> $logfile
export err=$?; err_chk

echo " " >> $logfile
echo "############################################################" >> $logfile
echo "     Make synthetic salinity profiles" >> $logfile
echo "############################################################" >> $logfile
echo " " >> $logfile

ln -sf $fixdir/godas_ann.temp ann.temp
ln -sf $fixdir/godas_ann.salt ann.salt

if [ $machine = IBM ]; then
  export XLFUNIT_11="tmpa.edt"
  export XLFUNIT_12="ann.temp"
  export XLFUNIT_13="ann.salt"
  export XLFUNIT_51="sala.edt"
else
  ln -sf tmpa.edt fort.11
  ln -sf ann.temp fort.12
  ln -sf ann.salt fort.13
  ln -sf sala.edt fort.51
fi

$aprun $execdir_godasprep/${cfsg}mkLSAchv >> $logfile
export err=$?; err_chk

echo " " >> $logfile
echo "############################################################" >> $logfile
echo "     Write salinity profiles to assimilation file" >> $logfile
echo "############################################################" >> $logfile
echo " " >> $logfile

if [ $machine = IBM ]; then
  export XLFUNIT_11="sala.edt"
else
  ln -sf sala.edt fort.11
fi

$aprun $execdir_godasprep/${cfsg}mkDlyAsmPrfs $model_start_day >> $logfile
export err=$?; err_chk

logfile=$username.$job.$dte
cat log.* > $logfile

rc=$?
exit $rc
