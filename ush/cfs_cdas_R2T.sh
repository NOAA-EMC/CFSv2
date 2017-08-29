#!/bin/ksh

set -x
 if [ $# -eq 0 ]; then
   echo "Must provide atleast restart tarfile date tag on command line:"
   echo "  cfs_cdas_R2T.sh <date_tag>"
   err_exit 99
 fi

 dte=$1
 echo $dte
 export cfss=${cfss:-"/cfs"}
 export cfsp=${cfsp:-"cfs_"}
 FIX_OCN=${FIX_OCN:-$HOMEcfs/fix/${cfsp}fix_om}
 TEMPLATE_DIR=${2:-${TEMPLATE_DIR:-$FIX_OCN}}
 rstrt2tm=${rstrt2tm:-$EXECcfs/cfs_cdas_rstrt2tm}
 prefixo=${3:-""}

#tar -xvf $omrestart.tar coupler.res
#tar -xvf $omrestart.tar ocean_temp_salt.res.nc
#tar -xvf $omrestart.tar ocean_freesurf.res.nc
#tar -xvf $omrestart.tar ocean_velocity.res.nc

 dtFile=coupler.res 
 tsFile=ocean_temp_salt.res.nc 
 etFile=ocean_freesurf.res.nc 
 uvFile=ocean_velocity.res.nc 

 cp $TEMPLATE_DIR/tmpltR.ocn* .

$rstrt2tm -t $tsFile -e $etFile -u $uvFile -d $dtFile -o ${prefixo}OCEAN_ -tp tmpltR.ocn
export err=$?; err_chk

rm tmpltR.ocn*

echo Done.
