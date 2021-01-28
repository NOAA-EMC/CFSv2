#!/bin/ksh
#############################################################################
# This script excfs_cdas_vrfyfits.sh.sms processes fits to obs
# and generates the model verification statistis for CDAS/CFS
# Author:   Suranjana Saha -- Original Script
#           Shrinivas Moorthi -- Second version
#############################################################################
set -euax

export PATH=.:$PATH

##################################################
# Define input variables
##################################################
export CDUMPPREP=${CDUMPPREP:-gdas}
export CDUMPFCST=${CDUMPFCST:-gfs}
export CDUMPANAL=${CDUMPANAL:-$CDUMPPREP}
export cfsp=${cfsp:-cfs_}
export cfsd=${cfsd:-cfs_cdas_}
export NWPROD=${NWPROD:-/nwprod}
export SIGEVENTSH=${SIGEVENTSH:-$USHcfs/${cfsp}prevmpi.sh}
export BUFRPOSTSH=${BUFRPOSTSH:-$USHcfs/${cfsp}bufr_post.sh}
export FITSSH=${FITSSH:-$USHcfs/${cfsd}fits.sh}
export HORZSH=${HORZSH:-$USHcfs/${cfsd}horizn.sh}
export CNVDIAGEXEC=${CNVDIAGEXEC:-$EXECcfs/${cfsp}post_convdiag}
export COMLOC=${COMLOC:-$COMROT}
export COMANL=${COMANL:-$COMROT}
export COMLOX=${COMLOX:-$DATA}   
export COMOUT=${COMOUT:-$DATA}   
export SAVEPREP=${SAVEPREP:-NO}

##[ $NEMS = YES ] && { sig=gfn; sfc=sfn; } || { sig=sig; sfc=sfc; }

export PRVT=${PRVT:-$NWPROD/fix/prepobs_errtable.global}
export PREX=${PREX:-$EXECcfs/cfs_prevmpi}

export CHGRP_RSTPROD=${CHGRP_RSTPROD:-YES}

export HYBLEVS=${HYBLEVS:-/dev/null}
export PREC=$DATA/prec; echo  "&PREVDATA doanls=t,fits=t /" >$PREC

#################################################################

cd $DATA
msg="JOB $job HAS BEGUN"
postmsg "$jlogfile" "$msg"

# make bufr convdiag postevents for analysis only...
fh1=06
fh2=00
pdy=$(echo $CDATE | cut -c1-8)
hh=$(echo $CDATE | cut -c9-10)
cyc=$hh

eval COMLIC=$COM_INA

if [[ $RUN_ENVIR = nemsio || $RUN_ENVIR = netcdf ]] ; then
  [[ $RUN_ENVIR = nemsio ]] && suffix=nemsio || suffix=nc
  export PRPI=$COMLIC/gdas.t${hh}z.prepbufr
  export PRPO=$COMLOX/gdas.t${hh}z.prepqa
  export PRPF=$COMLOX/gdas.t${hh}z.prepqf
  export sig1=$COMLIC/gdas.t${hh}z.atmanl.$suffix
  export sfc1=$COMLIC/gdas.t${hh}z.atmanl.$suffix
  export CNVS=$COMLIC/gdas.t${hh}z.cnvstat
elif [[ $RUN_ENVIR = cfs ]]; then
  tzz=t${hh}z
  export PRPI=$COMLIC/cdas1.$tzz.prepbufr       
  export PRPO=$COMOUT/cdas1.$tzz.prepqa           
  export PRPF=$COMLOX/cdas1.$tzz.prepqf
  export sig1=$COMLIC/cdas1.$tzz.sanl               
  export sfc1=$COMLIC/cdas1.$tzz.sfcanl 
  export CNVS=$COMLIC/cdas1.$tzz.cnvstat 
else 
  echo $RUN_ENVIR = unknown RUN_ENVIR; exit 999 
fi

[ ${ACPROFit:-YES} != NO ] && PRPI=$($USHcfs/ACprof)

$BUFRPOSTSH $sig1 $CNVS $PRPI $PRPO $CDATE

[ "$CHGRP_RSTPROD" = 'YES' ] && chgrp rstprod $PRPO && chmod 640 $PRPO
[ $SAVEPREP = YES          ] && cp $PRPO $ARCDIR/$(basename $PRPO).06.00
[ $SAVEPREP = YES          ] && cp $PRPI $ARCDIR

$FITSSH     $CDATE $PRPO $COMLOX $DATA 06 00           
$HORZSH     $CDATE $PRPO $COMLOX $DATA anl 2> horizout

#################################################################
# make prepqf file containing forecasts
#################################################################

export NDATE=${NDATE:-/nwprod/util/exec/ndate}

cp $PRPO $PRPF || exit 2

if [ $hh = "00" -o $hh = "12" -o $hh = "06" -o $hh = "18" ] ; then
  fp1="fh1=12;fh2=36"
  fp2="fh1=24;fh2=48"
  fp3="fh1=60;fh2=84"
  fp4="fh1=72;fh2=96"
  fp5="fh1=108;fh2=120"
else
  exit
fi

# interpolate background from fh

tspan=6

for fp in $fp1 $fp2 $fp3 $fp4 $fp5 
do
eval $fp
for fh in $fh1 $fh2                 
do

[ $fh = xx ] && continue

FDATE=$($NDATE -$fh $CDATE)
fdy=$(echo $FDATE|cut -c 1-8)
fzz=$(echo $FDATE|cut -c 9-10)
eval COMLICF=$COM_INF

if [[ $RUN_ENVIR = nemsio || $RUN_ENVIR = netcdf ]] ; then
  fhm3=$((fh-$tspan)); [ $fhm3 -lt 10 ] && fhm3=0$fhm3; [ $fhm3 -lt 100 ] && fhm3=0$fhm3
  fhp3=$((fh+$tspan)); [ $fhp3 -lt 10 ] && fhp3=0$fhp3; [ $fhp3 -lt 100 ] && fhp3=0$fhp3
  fh00=$fh;            [ $fh00 -lt 10 ] && fh00=0$fh00; [ $fh00 -lt 100 ] && fh00=0$fh00
  tzz=t$(echo $FDATE|cut -c9-10)z
  [[ $RUN_ENVIR = nemsio ]] && suffix=nemsio || suffix=nc
  export sig1=$COMLICF/gfs.$tzz.atmf$fhm3.$suffix  
  export sig2=$COMLICF/gfs.$tzz.atmf$fh00.$suffix
  export sig3=$COMLICF/gfs.$tzz.atmf$fhp3.$suffix
  export sfc1=$COMLICF/gfs.$tzz.atmf$fhm3.$suffix
  export sfc2=$COMLICF/gfs.$tzz.atmf$fh00.$suffix
  export sfc3=$COMLICF/gfs.$tzz.atmf$fhp3.$suffix
elif [[ $RUN_ENVIR = cfs ]]; then
  CDAM3=$($NDATE -$tspan  $CDATE)
  CDAP3=$($NDATE +$tspan  $CDATE)
  export sig1=$COMLICF/sigf${CDAM3}.01.$FDATE
  export sig2=$COMLICF/sigf${CDATE}.01.$FDATE
  export sig3=$COMLICF/sigf${CDAP3}.01.$FDATE
  export sfc1=$COMLICF/sfcf${CDAM3}.01.$FDATE
  export sfc2=$COMLICF/sfcf${CDATE}.01.$FDATE
  export sfc3=$COMLICF/sfcf${CDAP3}.01.$FDATE
else 
  echo $RUN_ENVIR = unknown RUN_ENVIR; exit 999 
fi

if [ -s $sig1 -a -s $sig2 -a -s $sig3 ] ; then 
 [ -s $sfc1 -a -s $sfc2 -a -s $sfc3 ] && rsfc=t || rsfc=f
 [ $fh = $fh1 ] && echo  "&PREVDATA dofcst=t,nbax=3,span=$tspan,fits=t,rsfc=$rsfc /" >$PREC
 [ $fh = $fh2 ] && echo  "&PREVDATA doanls=t,nbax=3,span=$tspan,fits=t,rsfc=$rsfc /" >$PREC
 cp $PRPF prepqm; $SIGEVENTSH prepqm $CDATE; cp prepqm $PRPF 
else
 [ $fh = $fh1 ] && fh1=xx
 [ $fh = $fh2 ] && fh2=xx
fi

done # endi of inner loop over two forecast times

if [ $fh1 != xx -o $fh2 != xx ] ; then
 [ "$CHGRP_RSTPROD" = 'YES' ] && chgrp rstprod $PRPF && chmod 640 $PRPF
 [ $SAVEPREP = YES          ] && cp $PRPF $ARCDIR/$(basename $PRPO).$fh1.$fh2
 $FITSSH $CDATE $PRPF $COMLOX $DATA $fh1 $fh2 
 [ $hh = 00 -a $fh1 = 24 ] &&  $HORZSH $CDATE $PRPF $COMLOX $DATA fcs 2> horizout
 [ $hh = 12 -a $fh1 = 12 ] &&  $HORZSH $CDATE $PRPF $COMLOX $DATA fcs 2> horizout
fi

done # end of outer loop over multiple forecast times

########################################################
# copy the fit files to the FIT_DIR
########################################################

set +e

rm -f $COMLOX/fxx*

mkdir -p $FIT_DIR
cp $COMLOX/f*.raob.$CDATE  $FIT_DIR 
cp $COMLOX/f*.acft.$CDATE  $FIT_DIR 
cp $COMLOX/f*.acar.$CDATE  $FIT_DIR 
cp $COMLOX/f*.surf.$CDATE  $FIT_DIR 
cp $COMLOX/f*.sfc.$CDATE   $FIT_DIR 

for typ in anl fcs
do
mkdir -p $HORZ_DIR/$typ   
cp -p $COMLOX/adpupa.mand.$typ.$CDATE  $HORZ_DIR/$typ/adpupa.mand.$CDATE 
cp -p $COMLOX/adpsfc.$typ.$CDATE       $HORZ_DIR/$typ/adpsfc.$CDATE      
cp -p $COMLOX/sfcshp.$typ.$CDATE       $HORZ_DIR/$typ/sfcshp.$CDATE      
cp -p $COMLOX/aircar.$typ.$CDATE       $HORZ_DIR/$typ/aircar.$CDATE      
cp -p $COMLOX/aircft.$typ.$CDATE       $HORZ_DIR/$typ/aircft.$CDATE      
done

################## END OF SCRIPT #######################
msg='ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"
cd $(dirname $DATA)
if [ ${KEEPDATA:-"NO"} = "NO" ] ; then rm -rf $DATA ; fi
################## END OF SCRIPT #######################
