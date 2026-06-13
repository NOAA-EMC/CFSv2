#!/bin/ksh
################################################################
#
# script: cfs_mvrecdir4catch
# purpose:  move recovery directory to catch up area, so catch
#           run pick up from catch directory
#
# auther: Xiaoxue Wang 
##################################################################

set -x

export RecvPRODfore=$COMDIR/recovery/cfs.recovery/${cyc}/cfs_m${ENS_MEM}
export RecvPRODpost=$COMDIR/recovery/cfs.defines/${cyc}/cfs_m${ENS_MEM}
export RecvCATCHfore=$COMDIR/catch/cfs.recovery/${cyc}/cfs_m${ENS_MEM}
export RecvCATCHpost=$COMDIR/catch/cfs.defines/${cyc}/cfs_m${ENS_MEM}

#
# decide if the run is a catch up rerun
#
 
if [ -s $RecvCATCHfore/catch.stamp ]; then
  export CatchRestart=`cat $RecvCATCHfore/catch.stamp`
else
  export CatchRestart=NO
fi

if [ $CatchRestart = NO ]; then
  rm -rf $RecvCATCHfore $RecvCATCHpost
  mkdir -p -m 775 $RecvCATCHfore $RecvCATCHpost
  if [ ! -s $RecvPRODfore ]; then
    set -x
    echo " Rerun from Beginning! "
    set -x
    export CatchRestart=Yes
    echo $CatchRestart > $RecvCATCHfore/catch.stamp
    exit
  else 
    mv $RecvPRODfore/* $RecvCATCHfore/.
    mv $RecvPRODpost/* $RecvCATCHpost/.
    cd $RecvCATCHfore/
    start_date=`cat stamp.cfs |awk '{print $1}`
    current_date=`cat stamp.cfs |awk '{print $2}`
    current_date2=`$NDATE -360 ${current_date}`
    if [ $current_date2 -le $start_date ]; then
    current_date2=$start_date
    fi 
    cp stamp.cfs stamp.cfs.hold
    perl -pi -e "s/${current_date}/${current_date2}/g" stamp.cfs

    export CatchRestart=Yes
    echo $CatchRestart > $RecvCATCHfore/catch.stamp

    set -x
    echo " Recovery directories are moved to catchup area"
    set -x
  fi
fi
