#!/bin/ksh
#############################################################################
# This script excfs_cdas_vrfyfits.sh.sms processes fits to obs
# and generates the model verification statistis for CDAS/CFS
# Author:   Suranjana Saha -- Original Script
#           Shrinivas Moorthi -- Second version
#############################################################################
set -uax

##################################################
# Define input variables
##################################################
export CDUMPPREP=${CDUMPPREP:-gdas}
export CDUMPFCST=${CDUMPFCST:-gfs}
export CDUMPANAL=${CDUMPANAL:-$CDUMPPREP}
export cfsp=${cfsp:-cfs_}
export cfsd=${cfsd:-cfs_cdas_}
export SIGEVENTSH=${SIGEVENTSH:-$USHcfs/${cfsp}prevmpi.sh}
export BUFRPOSTSH=${BUFRPOSTSH:-$USHcfs/${cfsp}bufr_post.sh}
export FITSSH=${FITSSH:-$USHcfs/${cfsd}fits.sh}
export HORZSH=${HORZSH:-$USHcfs/${cfsd}horizn.sh}
export CNVDIAGEXEC=${CNVDIAGEXEC:-$EXECcfs/${cfsp}post_convdiag}

export PRVT=${PRVT:-$HOMEprep/fix/prepobs_errtable.global}
export PREX=${PREX:-$HOMEcfs/exec/cfs_prevmpi}

export CHGRP_RSTPROD=${CHGRP_RSTPROD:-YES}

#################################################################

msg="JOB $job HAS BEGUN"
postmsg "$jlogfile" "$msg"
cd $DATA

# make bufr convdiag postevents for analysis only...
fh1=06
fh2=00
hh=$(echo $CDATE | cut -c9-10)
yyyymmdd=$(echo $CDATE | cut -c1-8)

if [ $RUN_ENVIR = nco  -o $RUN_ENVIR = devpara ] ; then
  COMLIC=$COMROT/cdas.$yyyymmdd
  COMLOC=$COMROT/cdas.$yyyymmdd
  export PRPI=$COMLIC/cdas1.t${hh}z.prepbufr
  export PRPO=$COMLOC/cdas1.t${hh}z.prepqa
  export PRPF=$COMLOC/cdas1.t${hh}z.prepqf
  export sig1=$COMLIC/cdas1.t${hh}z.sanl
  export sfc1=$COMLIC/cdas1.t${hh}z.sfcanl
  export CSTAT=${CSTAT:-$COMLIC/cdas1.t${hh}z.cnvstat}
  export PREC=$DATA/prec; echo  "&PREVDATA doanls=t,fits=t /" >$PREC
else
  COMLOC=$COMROT
  export PRPI=$COMLOC/prepqc.$CDUMPPREP.$CDATE
  export PRPO=$COMLOC/prepqa.$CDUMPPREP.$CDATE
  export PRPF=$COMLOC/prepqf.$CDUMPPREP.$CDATE
  export sig1=$COMLOC/siganl.$CDUMPANAL.$CDATE
  export sfc1=$COMLOC/sfcanl.$CDUMPANAL.$CDATE
  export CSTAT=${CSTAT:-$COMLOC/cnvstat.$CDUMPPREP.$CDATE}
  export PREC=$DATA/PREC; echo  "&PREVDATA doanls=t,fits=t /" >$PREC
fi

$BUFRPOSTSH $sig1 $CSTAT $PRPI $PRPO $CDATE

$FITSSH     $CDATE $PRPO $COMLOC $DATA $fh1 $fh2
$HORZSH     $CDATE $PRPO $COMLOC $DATA anl 2> horizout

if [ "$CHGRP_RSTPROD" = 'YES' ]; then
  chgrp rstprod $PRPO
  errch=$?
  if [ $errch -eq 0 ]; then
      chmod 640 $PRPO
  fi
fi

#################################################################
# make prepqf file containing forecasts

if [[ $hh = "00" ]] ; then
  fh1=24
  fh2=48
elif [[ $hh = "12" ]] ; then
  fh1=12
  fh2=36
else
  exit
fi

cp $PRPO $PRPF || err_exit 2

# interpolate background from fh

tspan=6

for fh in $fh1 $fh2
do

FDATE=$($NDATE -$fh $CDATE)
if [ $RUN_ENVIR = nco  -o $RUN_ENVIR = devpara ] ; then
  CDAM3=$($NDATE -$tspan  $CDATE)
  CDAP3=$($NDATE +$tspan  $CDATE)
  fdy=$(echo $FDATE|cut -c1-8); fzz=$(echo $FDATE|cut -c9-10)
  COMLICF=$COMROT/cfs/cfs.$fdy/$fzz/6hrly_grib_01
  export sig1=$COMLICF/sigf${CDAM3}.01.$FDATE        
  export sig2=$COMLICF/sigf${CDATE}.01.$FDATE        
  export sig3=$COMLICF/sigf${CDAP3}.01.$FDATE        
  export sfc1=$COMLICF/sfcf${CDAM3}.01.$FDATE        
  export sfc2=$COMLICF/sfcf${CDATE}.01.$FDATE        
  export sfc3=$COMLICF/sfcf${CDAP3}.01.$FDATE        
else
  fhm3=$((fh-$tspan)); [ $fhm3 -lt 10 ] && fhm3=0$fhm3
  fhp3=$((fh+$tspan)); fh00=$fh          
  export sig1=$COMLOC/sigf$fhm3.$CDUMPFCST.$FDATE 
  export sig2=$COMLOC/sigf$fh00.$CDUMPFCST.$FDATE 
  export sig3=$COMLOC/sigf$fhp3.$CDUMPFCST.$FDATE 
  export sfc1=$COMLOC/sfcf$fhm3.$CDUMPFCST.$FDATE 
  export sfc2=$COMLOC/sfcf$fh00.$CDUMPFCST.$FDATE 
  export sfc3=$COMLOC/sfcf$fhp3.$CDUMPFCST.$FDATE 
fi

[ $fh -eq $fh1 ] && echo  "&PREVDATA dofcst=t,nbax=3,span=$tspan,fits=t /" >$PREC
[ $fh -eq $fh2 ] && echo  "&PREVDATA doanls=t,nbax=3,span=$tspan,fits=t /" >$PREC

cp $PRPF prepqm           ||err_exit 2
$SIGEVENTSH prepqm $CDATE ||err_exit 2
cp prepqm $PRPF           ||err_exit 2

done

[ "$CHGRP_RSTPROD" = 'YES' ] && chgrp rstprod $PRPF && chmod 640 $PRPO

$FITSSH       $CDATE $PRPF $COMLOC $DATA $fh1 $fh2
$HORZSH       $CDATE $PRPF $COMLOC $DATA fcs 2> horizout

########################################################

msg='ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################

