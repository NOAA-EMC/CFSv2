#!/usr/bin/env bash
set -eux
#set -nx
###############################################################
# 
# script:  cfs_pgb.sh
#
#   This script runs nceppost for Climate Forecasts in CFS
#   This script modified by shrinivas Moorthi on 07/16/2003
#   Updated for cfsv2    by shrinivas Moorthi on 03/19/2010 and 09/30/2010
#
# pgb
#
#  $1 ... starting forecast hour
#  $2 ... ending forecast hour
#  $3 ... forecast hour increment
#
#  This script assumes that the input sigma file is the form sig.ftxx
#  The output pressure file is the form pgb.ftxx
#
if [ $# -lt 3 ] ; then
	err_exit "`date` $0: argument error" 
fi
#
# tsleep=$((JCAP/FHOUT/6*15+15))
# msleep=240
#
fhs=$1
fhe=$2
fhinc=$3
start_date=${4:-$start_date}

FH=0
prog=pgb

####. prep_step

startmsg
#
export APRUNCY=${APRUNCY:-" "}
export NCP=${NCP:-cp}
export in_o=${in_o:-0}       # interpolation option, defaults to 0 (bilinear)
export SUFOUT=${SUFOUT:-""}
export REDO_PGB=${REDO_PGB:-YES}
export cfss=${cfss:-"/cfs"}
export cfsp=${cfsp:-"cfs_"}
export BASEDIR=${BASEDIR:-$HOMEcfs}
export IGEN_FCST=${IGEN_FCST:-98}
export VERBOSE=YES
export COMOUT=${COMOUT:-$COM_YMDH_DG}
export POSTGPSH=${POSTGPSH:-$HOMEcfs/ush/cfs_cdas_nceppost.sh}
export PARM_AM=${PARM_AM:-$BASEDIR/parm/${cfsp}parm_am}
export CAT_FLX_TO_PGB=${CAT_FLX_TO_PGB:-NO}
# Changed OUTTYP to 3 for WCOSS transition
export OUTTYP=${OUTTYP:-3}
export LINKPOSTFILESH=${LINKPOSTFILESH:-""}
#
#
polist_47d="1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,725.,700.,675.,650.,625.,600.,575.,550.,525.,500.,475.,450.,425.,400.,375.,350.,325.,300.,275.,250.,225.,200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,7.,5.,3.,2.,1."
#
polist_46d="1000.,975.,950.,925.,900.,875.,850.,825.,800.,750.,700., 650.,600.,550.,500.,450.,400.,350.,300.,250.,200.,150.,100.,70.,50.,40.,30.,20.,10.,7.,5.,4.,3.,2.,1.,0.7,0.5,0.4,0.3,0.2,0.1,0.07,0.05,0.04,0.03,0.02"
#
polist_31d="1000.,975.,950.,925.,900.,850.,800.,750.,700.,650.,600.,550.,500.,450.,400.,350.,300.,250.,200.,150.,100.,70.,50.,30.,20.,10.,7.,5.,3.,2.,1."
#
polist_37d="1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,700.,650.,600.,550.,500.,450.,400.,350.,300.,250.,225.,200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,7.,5.,3.,2.,1."
#
polist_26d="1000.,975.,950.,925.,900.,850.,800.,750.,700.,650.,600.,550.,500.,450.,400.,350.,300.,250.,200.,150.,100.,70.,50.,30.,20.,10.,"
#
thlist_16d="270.,280.,290.,300.,310.,320.,330.,350.,400.,450.,550.,650.,850.,1000.,1250.,1500."
#

polist_47=${polist_47:-$polist_47d}
polist_46=${polist_46:-$polist_46d}
polist_31=${polist_31:-$polist_31d}
polist_37=${polist_37:-$polist_37d}
polist_26=${polist_26:-$polist_26d}
thlist_16=${thlist_16:-$thlist_16d}

#
export flag=${pgbres_flag:-f}

export IDRT=${IDRT_NP:-0}         # defaults to lat/lon (for gfsio file)
if [ $IDRT -eq 0 ] ; then
  export LONB=$iop
  export LATB=$jop
  if [ $iop -eq 720 ] ; then
   export GRID_ID=${GRID_ID:-4}      # defaults to lat/lon grid of 0.5 degree
   export flag=h
  elif [ $iop -eq 360 ] ; then
   export GRID_ID=${GRID_ID:-3}      # defaults to lat/lon grid of 1.0 degree
   export flag=f
  elif [ $iop -eq 144 ] ; then
   export GRID_ID=${GRID_ID:-2}      # defaults to lat/lon grid of 2.5 degree
   export flag=l
  fi
else
  LONB_NP=${LONB_NP:-0}
  if [ $LONB_NP -gt 0 ] ; then
   export LONB=$LONB_NP
   export LATB=$LATB_NP
   export flag=m
   export GRID_ID=${GRID_ID:-0}
   if [ $GRID_ID -le 0 ] ; then
    echo 'APPROPRIATE GRID_ID needs to be specified when LONB_NP > 0'
    err_exit
   fi
  else
   export GRID_ID=0
  fi
fi
#export GRID_ID25=${GRID_ID25:-2}  # defaults to lat/lon grid of 2.5 degree
#export GRID_ID62=${GRID_ID62:-98} # defaults to Gaussian grid of T62
#
 if [ $KTO -eq 0 ] ; then
   export  POSTGPVARS="KPO=$kop,PO=$(eval echo \${polist_$kop}),"
 else
   export  POSTGPVARS="KPO=$kop,PO=$(eval echo \${polist_$kop}),KTH=$KTO,TH=$(eval echo \${thlist_$KTO}),"
 fi
#
export POSTD3D=${POSTD3D:-NO}
#if [ $POSTD3D = YES ] ; then
#export CTLFILE=${CTL_FCS:-$PARM_AM/am_cntrl.parm}
#else
# export CTLFILE=${CTL_FCS:-$PARM_AM/am_cntrl.parm_pgb}
#fi
#export CTLFILE=${CTL_FCS:-$PARM_AM/am_cntrl.parm}
#
#
#  If the starting time is "0 hr" then, optionally post analysis
#  -------------------------------------------------------------
#
if [ $fhs -eq 0 -a ${POST_ANL:-YES} = YES ] ; then
 if [ ${POSTSPL:-YES} = YES ] ; then
  CFS_POSTMDLSH=${CFS_POSTMDLSH:-$HOMEcfs/ush/${cfsp}post_mdl.sh}
  if [ -s $COM_YMDH/siganl$SUFOUT ] ; then
    export VDATE=$start_date
    export SIGINP=$COM_YMDH/siganl$SUFOUT
    $CFS_POSTMDLSH anl $SUFOUT
  fi
  if [ -s $COM_YMDH/sigf${FH}$SUFOUT ] ; then
    export VDATE=$($NDATE 06 $start_date)
    export SIGINP=$COM_YMDH/sigf06$SUFOUT
    $CFS_POSTMDLSH f06 $SUFOUT
  fi
 fi
 if [ ! -s $COM_YMDH_DG/pgbanl$SUFOUT  -o $REDO_PGB = YES ] ; then
  export CTLFILE=${CTL_ANL:-$PARM_AM/am_cntrl.parm_anl}
  export IGEN=${IGEN_ANL:-98}
  export VDATE=$start_date
  export SIGINP=$COM_YMDH/siganl$SUFOUT
  export SFCINP=$COM_YMDH/sfcanl$SUFOUT
  export FLXINP=/dev/null
  export PGBOUT1=$COMOUT/pgbanl$SUFOUT
  export PGBOUT=$COMOUT/pgb${flag}nl$SUFOUT
  rm -f $PGBOUT
  export IGEN=$IGEN_FCST
  if [ $KTO -gt 0 ] ; then
    export IPVOUT1=$COMOUT/ipvanl$SUFOUT
    export IPVOUT=$COMOUT/ipv${flag}nl$SUFOUT
    rm -f $IPVOUT
  fi

  $POSTGPSH >>$pgmout 2>errfile
  export err=$?;err_chk

  if [ $GRID_ID -ne 3 -a $iop -gt 360 ] ; then
     $COPYGB -g3 -i$in_o -x $PGBOUT $PGBOUT1
     if [ $KTO -gt 0 ] ; then $COPYGB -g3 -i$in_o -x $IPVOUT $IPVOUT1 ; fi
  else
     [[ $PGBOUT != $PGBOUT1 ]] && mv $PGBOUT $PGBOUT1
     [[ $KTO -gt 0 && $IPVOUT != $IPVOUT1 ]] && mv $IPVOUT $IPVOUT1 
  fi
  rc=$?
  if [[ $rc -ne 0 ]];then
    if [ $RUN_ENVIR = dev ]; then
      $PERR;exit 1
    else
      export err=$rc; err_chk
    fi
  fi

  if [ $SENDCOM = YES ] ; then
    if [ ! -s $COM_YMDH_DG/pgbanl${start_date}$SUFOUT ] ; then
     ln -fs $COM_YMDH_DG/pgbanl$SUFOUT $COM_YMDH_DG/pgbanl${start_date}$SUFOUT
    fi
  fi
 fi
fi
#
export CTLFILE=${CTL_FCS:-$PARM_AM/am_cntrl.parm_pgb}
#
FH=$((fhs-fhinc))
until [[ $((FH=10#$FH+10#$fhinc)) -gt $fhe ]];do [[ $FH -lt 10 ]]&&FH=0$FH
  if [ ! -s $COM_YMDH_DG/pgbf${FH}$SUFOUT  -o $REDO_PGB = YES ] ; then

    export SIGINP=$COM_YMDH/sigf${FH}$SUFOUT

    if [ $IDRT -eq 0  -o $GRID_ID -gt 0 ] ; then
       rm -f flxfile
       $COPYGB -g$GRID_ID -i$in_o -x $COM_YMDH/flxf${FH}${SUFOUT} flxfile
       export FLXINP=flxfile
       if [ $POSTD3D = YES ] ; then
         rm -f d3dfile
         $COPYGB -g$GRID_ID -i$in_o -x $COM_YMDH/d3df${FH}${SUFOUT} d3dfile
         export D3DINP=d3dfile
       fi
    elif [ $IDRT -eq 4 ] ; then
       export FLXINP=$COM_YMDH/flxf${FH}${SUFOUT}
       if [ $POSTD3D = YES ] ; then
         export D3DINP=$COM_YMDH/d3df${FH}${SUFOUT}
       fi
    else
       echo 'INVALID IDRT has value  '$IDRT
       err_exit
    fi

    export PGBOUT1=$COMOUT/pgbf${FH}${SUFOUT}
    export PGBOUT=$COMOUT/pgb${flag}${FH}${SUFOUT}
    rm -f $PGBOUT
    export IGEN=$IGEN_FCST
    if [ $KTO -gt 0 ] ; then
      export IPVOUT1=$COMOUT/ipvf${FH}${SUFOUT}
      export IPVOUT=$COMOUT/ipv${flag}${FH}${SUFOUT}
      rm -f $IPVOUT
    fi
    if [ $POSTD3D = YES ] ; then
      export D3DOUT1=$COMOUT/diabf${FH}${SUFOUT}
      export D3DOUT=$COMOUT/diab${flag}${FH}${SUFOUT}
      rm -f $D3DOUT
    fi

    export VDATE=$($NDATE $FH $start_date)
    $POSTGPSH >>$pgmout 2>errfile
    export err=$?;err_chk

    if [ $PGBOUT != $PGBOUT1 ] ; then
    if [ $CAT_FLX_TO_PGB = YES ] ; then cat flxfile >> $PGBOUT ; fi
    if [ $GRID_ID -ne 3 -a $iop -gt 360 ] ; then
       $COPYGB -g3 -i$in_o -x $PGBOUT $PGBOUT1
       if [ $KTO -gt 0 ] ; then $COPYGB -g3 -i$in_o -x $IPVOUT $IPVOUT1 ; fi
       if [ $POSTD3D = YES ] ; then
         $COPYGB -g3 -i$in_o -x $D3DOUT $D3DOUT1
         if [ $IDRT -eq 0 ] ; then rm -f d3dfile ; fi
       fi
    else
       [ $PGBOUT != $PGBOUT1 ] && mv $PGBOUT $PGBOUT1
       if [ $KTO -gt 0 -a  $IPVOUT != $IPVOUT1 ] ; then mv $IPVOUT $IPVOUT1 ; fi
       if [ $POSTD3D = YES ] ; then
         [ $D3DOUT != $D3DOUT1 ] && mv $D3DOUT $D3DOUT1
         if [ $IDRT -eq 0 ] ; then rm -f d3dfile ; fi
       fi
    fi
    fi

  fi

  # rc=$?
  #if [[ $rc -ne 0 ]];then
  #  if [ $RUN_ENVIR = dev ]; then
  #    $PERR;exit 1
  #  else
  #    export err=$rc; err_chk
  #  fi
  #fi

  if [ $SENDCOM = YES ] ; then
    for file in $FILES_TO_SEND_COM ; do
      if [ $file = pgb ] ; then
        current_date=$($NDATE $FH $YMDH)
        if [ ! -s $COM_YMDH_DG/pgbf${current_date}$SUFOUT ] ; then
          ln -fs $COM_YMDH_DG/pgbf${FH}$SUFOUT $COM_YMDH_DG/pgbf${current_date}$SUFOUT
        fi
      fi
    done
  fi
done
#export err=$?; err_chk

