#!/bin/ksh
#
set -x
#
export var=$1
#export dmpdir=${1:-/climate/save/wx24db/ARCS/XBTarc4op/DAILY}
export dmpdir=${2:-/global/shared/dump}
export FIX_OCN=${3:-${FIX_OCN:-$FIXcfs}}
export CDUMP=${4:-gdas}
export RUN_OPREP=${RUN_OPREP:-YES}
#
export cfss=${cfss:-"/cfs"}
export cfsd=${cfsd:-"cfs_cdas_"}
export PERR=${PERR:-""}
#
export ASYM_GODAS=${ASYM_GODAS:-NO}
export GODAS_WNDO=${GODAS_WNDO:-10}
export GODAS_DATA_DELAY=${GODAS_DATA_DELAY:-0}     # Data delay in days
#
export cmbDysPrf4=${cmbDysPrf4:-$EXECcfs/${cfsd}cmbDysPrf4}
export cmbDysPrfs4=${cmbDysPrfs4:-$EXECcfs/${cfsd}cmbDysPrfs4}
#
# This script prepares an ieee file of temperature observations
# to be assimilated during a GODAS run.  A similar script prepares
# the salinity observations.
#
# The start date of the current run can either come from "$CDATE" or from
# an ascii file "run_date"
#
dte=${CDATE:-`cat  run_date`}
dtem=$($NDATE -$((GODAS_WNDO*24)) $dte)

if [ $ASYM_GODAS = YES ] ; then
 MAXDAY=$(((GODAS_WNDO-GODAS_DATA_DELAY)+1))
else
 MAXDAY=33
fi

#arcdir=/climate/save/wx24db/ARCS/XBTarc4op/DAILY
#dtem=`less2wks $dte`

#        use Dave's directory
if [ $dmpdir == ARC ] ; then
 if [ $var = tmp ] ; then
  arcdir=/climate/save/wx24db/ARCS/XBTarc4op/DAILY
 elif [ $var = sal ] ; then
  arcdir=/climate/save/wx24db/ARCS/SALarc4op/DAILY
 fi
fi
dte=$dtem

n=0
while [ $n -lt $MAXDAY ]; do
  dtx=$(echo $dte | cut -c1-8)
  yr=$(echo $dte | cut -c1-4)
  if [ $RUN_OPREP = YES ] ; then
   ${NCP:-/bin/cp} $dmpdir/${dtx}$var.prf  ${dtx}$var.prf
  elif [ $dmpdir == ARC ] ; then
    tar -xvf $arcdir/${yr}$var.tar ${dtx}$var.prf
  else
   ${NCP:-/bin/cp} $dmpdir/$dte/${CDUMP}$DMP_SUF/$var.godas.$dte ${dtx}$var.prf
  fi
  dtem=$dte
# dte=`nxtdy $dtem`
  dte=$($NDATE 24 $dtem)
  ((n=$n+1))
done

ls *.prf > lstFile

cat<<eof>p_input.nml
&tprf_nml
   seo=1.0
   sef=2.0
   rescl_error=.true.
   lg0=305.0
   lg1=359.0
   lt0=-8.0
   lt1=8.0
   efctr=5.0
/

&sprf_nml
   stde=0.2
   rescl_error=.true.
   lg0=305.0
   lg1=359.0
   lt0=-8.0
   lt1=8.0
   efctr=5.0
/
eof

ln -sf $GRIDSPEC grid_spec.nc
ln -sf p_input.nml                       fort.10
ln -sf $PARMcfs/cfs_parm_om/bg_input.nml fort.10
ln -sf lstFile                           fort.11
ln -sf ${var}a.mom                       fort.51

if [ $var = tmp ] ; then
 $cmbDysPrf4
 rc=$?
elif [ $var = sal ] ; then
 $cmbDysPrfs4
 rc=$?
else
 rc=999
fi

if [[ $rc -ne 0 ]];then 
  if [ $RUN_ENVIR = dev ]; then 
    $PERR;exit 1
  else
    export err=$rc; err_chk
  fi
fi

#rm grid_spec.nc
#rm lstFile

# Copy the profile files to COM
tar -cvf $COMOUT/${RUN1}.t${cyc}z.${var}prf.tar *${var}.prf
if [ $SENDDBN = YES ]
then
  $DBNROOT/bin/dbn_alert MODEL CDAS1_OANL $job $COMOUT/${RUN1}.t${cyc}z.${var}prf.tar
fi
