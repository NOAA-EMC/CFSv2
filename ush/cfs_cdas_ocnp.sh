#!/bin/ksh 
###############################################################
# This script process the ocean post for the CDAS
# This script assumes that the directory $DATA exists
###############################################################
set -x

export cfss=${cfss:-"/cfs"}
export cfsd=${cfsd:-"cfs_cdas_"}
export cfsp=${cfsp:-"cfs_"}
export aveHrlyNc=${aveHrlyNc:-$EXECcfs/${cfsd}aveHrlyNc}
export aveHrlyIceNc=${aveHrlyIceNc:-$EXECcfs/${cfsd}aveHrlyIceNc}
export OCN2GRIBEXEC=${OCN2GRIBEXEC:-$EXECcfs/${cfsp}tripo2reg}
export ocn_postsh=$USHcfs/cfs_cdas_ocnpost.sh 
export NCP=${NCP:-/bin/cp}
#
export mfcstcpl=${mfcstcpl:-0}  #  Default computes meridional overturning circulation
export mkmoc=${mkmoc:-1}  #  Default computes meridional overturning circulation
if [ $mkmoc -eq 1 ] ; then
 export nreg=${nreg:-5}
 export tripolat=${tripolat:-65.0}
fi

export AVG_FCST=${AVG_FCST:-NO}
export AVG_SUB=${AVG_SUB:-YES}
export AVGSUBSH=${AVGSUBSH:-$HOMEcfs/bin/pavg}
export TSER_FCST=${TSER_FCST:-NO}
# AVG_INT is used for both averaging and time-series extraction
if [ $AVG_FCST = YES -o $TSER_FCST = YES ] ; then 
  export AVG_INT=${AVG_INT:-999} 
else
  export AVG_INT="-1"  # default added for variable to be defined in here file ocn_post.sh
fi
export FCST_TIMEMEANSH=${FCST_TIMEMEANSH:-$USHcfs/cfs_fcst_timemean.sh}
export OCNDIR=${OCNDIR:-$COMIN}
export COMROT=${COMROT:-$COMIN}
export OCNMEANDIR=${OCNMEANDIR:-$COMOUT}
export INDXDIR=${INDXDIR:-$DATA/index}
#    For extracting time series of selected variables
export TIMEDIR=${TIMEDIR:-$COMROT}
export TSER_SUB=${TSER_SUB:-YES}
export TSERSUBSH=${TSERSUBSH:-$HOMEcfs/bin/ptsr}
export FCST_TSERSH=${FCST_TSERSH:-$USHcfs/cfs_fcst_timeser.sh}

export EXTERNAL_AVG=${EXTERNAL_AVG:-NO}

export LINKPOSTFILESH=${LINKPOSTFILESH:-""}

export CDFNL=${CDFNL:-gdas}
export GDUMP=${GDUMP:-$CDFNL}
export cdump=$(echo $CDUMP|tr '[a-z]' '[A-Z]')

export nknd=${CKND:-1}
export FHBAK=${FHBAK:-0}

export FHINI=$(eval echo \${FHINIFCST$cycle$cdump:-${FHINI:-0}}|cut -f$nknd -d,)
export FHMAX=$(eval echo \${FHMAXFCST$cycle$cdump:-${FHMAX:-9}}|cut -f$nknd -d,)
export FHOUT=$(eval echo \${FHOUTFCST$cycle$cdump:-${FHOUT:-1}}|cut -f$nknd -d,)
export GRID_ID25=$(eval echo \${GRID25FCST$cycle$cdump:-${GRID_ID25:-0}}|cut -f$nknd -d,)
export FHBAK=$(eval echo \${FHBAKFCST$cycle$cdump:-${FHBAK:-00}},}|cut -f$nknd -d,)

if [ $FHBAK -eq 0 ] ; then export FHINI=$FHBAK ; fi
export FH_STRT_POST=${FH_STRT_POST:-0}
export FH_END_POST=${FH_END_POST:-99999}

export REDO_AVG=${REDO_AVG:-NO}
export JUST_TSER=${JUST_TSER:-NO}
export REDO_TSER=${REDO_TSER:-NO}

export REDO_POST=${REDO_POST:-YES}
export JUST_POST=${JUST_POST:-YES}
export JUST_AVG=${JUST_AVG:-NO}

export in_o=${in_o:-0}       # interpolation option, defaults to 0 (bilinear)
export ENS_NUM=${ENS_NUM:-1}
export omres=$(eval echo \${OMRESFCST$cycle$cdump:-05}|cut -f$nknd -d,)
export omres=${omres:-1x1}
export outres=${omres:-1x1}
export IGEN_ANL=${IGEN_ANL:-197}
export IGEN_FCST=${IGEN_FCST:-197}
export IGEN_OCNP=${IGEN_OCNP:-197}
export VERBOSE=YES
if [ $nknd -gt 1 -a $FHBAK -eq 0 ] ; then
 export SUFOUT=.${CDUMP}$nknd.$CDATE
else
 export SUFOUT=.$CDUMP.$CDATE
fi
#
if [ $outres = '05' ] ; then
 export im_ocn=720
 export jm_ocn=360
 export flatn=89.75
 export flats=-89.75
 export flonw=0.25
 export flone=359.75
fi
export km_mom4=${km_mom4:-40}
if [ $omres = '05' ] ; then
 export im_mom4=720
 export jm_mom4=410
 export jmtp_mom4=50
 export imos=160
elif [ $omres = '1x1' ] ; then
 export im_mom4=360
 export jm_mom4=231
 export jmtp_mom4=25
 export imos=80
fi

export year=$(echo $CDATE | cut -c1-4)
export month=$(echo $CDATE | cut -c5-6)
export day=$(echo $CDATE | cut -c7-8)
export hour=$(echo $CDATE | cut -c9-10)
export idbug=${idbug:-1}
export im=${im_mom4:-360}
export jm=${jm_mom4:-231}
export km=${km_mom4:-40}
export jmtp=${jmtp_mom4:-25}
export imos=${imos:-80}
export imo=${im_ocn:-360}
export jmo=${jm_ocn:-180}
export jmtpo=${jmtpo:-$((jmtp*imo/im))}
export flatn=${flatn:-89.5}
export flats=${flats:--89.5}
export flonw=${flonw:-0.5}
export flone=${flone:-359.5}

# Defaults to 1x1 output

export imo=${im_ocn:-360}
if [ $imo -gt 360 ] ; then
 export SUFO=h
else
 export SUFO=f
fi

# To start post from the middle
if [ $FH_STRT_POST -ne 99999 ] ; then
 FHINI=$FH_STRT_POST
elif [ -s $COMROT/FHREST.$CDUMP.$CDATE.$nknd ] ; then
 read FHINI < $COMROT/FHREST.$CDUMP.$CDATE.$nknd
fi
if [ $FH_END_POST -ne 99999 ] ; then
 FHEND=$FH_END_POST
else
 FHEND=$FHMAX
fi
if [ $FHINI -gt $FHEND ] ; then
 echo ' FHINI > FHEND Post processing stopped, FHINI= '$FHINI', FHEND= '$FHEND
fi

if [ $AVG_FCST = YES -o $TSER_FCST = YES ] ; then
  export xdate=$($NDATE $FHINI $CDATE)
  if [ $AVG_INT -eq 999 ] ; then              # Monthly mean
    export ydate=$(echo $xdate | cut -c1-6)0100
    if [ $ydate -gt $CDATE ] ; then
      export PDATE=$ydate
    else
      export PDATE=$CDATE
    fi
  else
    export ydate=$CDATE
    until  [[ $ydate -gt $xdate ]] ; do
      export PDATE=$ydate
      export ydate=$($NDATE $AVG_INT $PDATE)
    done
  fi
else
   PDATE=$CDATE
fi

export sdate=$($NDATE $FHINI $CDATE)
export edate=$($NDATE $FHEND $CDATE)

export date=$($NDATE $FHOUT $sdate)
FH=$($NHOUR $date $CDATE)
FH=$((FH+0))

if [ $cdump = GDAS -a ${AVRG_HOURLY:-YES} = YES ] ; then
 date_av=$($NDATE 6 $sdate)
 set -A ocnfiles 6
 set -A icefiles 6
fi

# setup and run the ocean post mpmd process

nh=0
cmd=cmd.$$; >$cmd; chmod 755 $cmd
until [[ $date -gt $edate ]] ; do
  if [ $FH -lt 10 ] ; then FH=0$FH ; fi
  ymd=`echo $date | cut -c1-8`
  yyyy=`echo $date | cut -c1-4`
  mm=`echo $date | cut -c5-6`
  dd=`echo $date | cut -c7-8`
  hh=`echo $date | cut -c9-10`

#------------------begin nc file wait from the model-------------------------------------

# define the ocn and ice netcdf files from the model

  export ocnfile=$OCNDIR/ocn_${yyyy}_${mm}_${dd}_${hh}$SUFOUT.nc
  export icefile=$OCNDIR/ice_${yyyy}_${mm}_${dd}_${hh}$SUFOUT.nc

# connect the ocnfile or time out

  nslp=0
  until [[ -s $ocnfile ]] ; do
  sleep 30
  nslp=$((nslp+1))
  if [[ $nslp -gt 20 ]]; then
     msg="Hourly Ocean File is not available"
     postmsg "$jlogfile" "$msg"
     export err=1; err_chk
  fi
  done

# connect the icefile or time out

  nslp=0
  until [[ -s $icefile ]] ; do
  sleep 30
  nslp=$((nslp+1))
  if [[ $nslp -gt 20 ]]; then
     msg="Hourly Ice File is not available"
     postmsg "$jlogfile" "$msg"
     export err=1; err_chk
  fi
  done

  sleep 10 ## make sure the connected files have copied completely

#------------------end of nc file wait---------------------------------------

  if [ $RUN_ENVIR = nco  -o $RUN_ENVIR = devpara ] ; then
    ofile=$COMOUT/${RUN1}.t${cyc}z.ocngrb${SUFO}${FH}
  else
    ofile=$COMOUT/ocn${SUFO}${FH}$SUFOUT
  fi
  if [ ! -s $ofile -o $REDO_POST = YES -a $JUST_AVG = NO ] ; then
    export DO_POST=YES
    echo "$ocn_postsh $date $FH $DO_POST" >>$cmd
    if [ $cdump = GDAS -a ${AVRG_HOURLY:-YES} = YES ] ; then
      if [ $date -le $date_av -a $nknd -eq 1 ] ; then
        nh=$((nh+1))
        ocnfiles[nh]=$ocnfile
        icefiles[nh]=$icefile
        if [ $date -eq $date_av ] ; then
          export TM=${MEAN_STR:-m}
          export ocnfilem=$OCNDIR/ocn_${yyyy}_${mm}_${dd}_${hh}${SUFOUT}$TM.nc
          export icefilem=$OCNDIR/ice_${yyyy}_${mm}_${dd}_${hh}${SUFOUT}$TM.nc
          if [ -s $ocnfilem ] ; then rm $ocnfilem ; fi
          if [ -s $icefilem ] ; then rm $icefilem ; fi
          $aprun $aveHrlyNc    $ocnfilem ${ocnfiles[1]} ${ocnfiles[2]} ${ocnfiles[3]} ${ocnfiles[4]} ${ocnfiles[5]} ${ocnfiles[6]}
          export err=$?;err_chk

          $aprun $aveHrlyIceNc $icefilem ${icefiles[1]} ${icefiles[2]} ${icefiles[3]} ${icefiles[4]} ${icefiles[5]} ${icefiles[6]}
          export err=$?;err_chk

          echo "$ocn_postsh $date_av 06 $DO_POST 6 $TM $mkmoc" >>$cmd
        fi
      fi
    fi
  fi
  date=$($NDATE $FHOUT $date)
  FH=$((FH+FHOUT))
done

if [ -s $cmd ] ; then
  mpirun -l cfp $cmd  |grep 'CFP RANK'
  export err=$?; err_chk
fi

################################################################################
################################################################################
# Exit gracefully

if [ $JUST_POST = YES -o $JUST_AVG = YES ] ; then exit ; fi
$PEND
