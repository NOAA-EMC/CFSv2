#!/usr/bin/env bash 
#----------------------------------------------------------------------#
# Additional imported  variables:
# FCSTSHDIR            defaults to $USHcfs
# FORECASTSH           defaults to excfs_fcst.sh.ecf
#
# (see also list of additional imported variables in $FORECASTSH)
#----------------------------------------------------------------------#
######################################################################
#
#   Script:  exrun_cfs.sh.ecf   
#   Author:  Shrinivas Moorthi
#   Current  DATE:    March  2010
#
#   Main script to run (atmosphere/ocean) coupled forecast system (CFS)
#   with MOM4 for seasonal climate prediction
#   Version CFSV2

# ####################
#  START CFSV2 integration 
# ####################

export VERBOSE=${VERBOSE:-NO}
echo " ------------------------------------------------------------"
echo "  "
echo "               IBM-P6 $ENVIR PROCESSING     "
echo "  "
echo "          COUPLED FORECAST SYSTEM ${RUN} FORECAST     "
echo "  "
echo "                `date`     "
echo "  "
echo "                   JOB  $job  "
echo "  "
echo "  "
echo "    $NET $RUN TIME is $YYYYSTART $MMSTART $DDSTART $HHSTART"
echo "  "
echo " ------------------------------------------------------------"
echo "          processing info for this execution"
echo " Home directory is ............................ $HOMEcfs"
echo " Processing directory for files.. ............. $DATA"
echo "  "
echo " Executable file directory is ................. $EXECcfs"
echo " Fixed field directory is ..................... $FIXcfs"
echo " Unix control language file is ................ $USHcfs"
echo "  "
echo " Network id is ................................ $NET"
echo " Run id for $com processing is ................ $RUN"
echo "  "
echo " standard output in file ...................... $pgmout"
echo " unique processing id for run ................. $pid"
echo " SENDCOM=YES means save com files ............. $SENDCOM"
echo " ------------------------------------------------------------"
echo " #####################################################"
echo " ${job} $NET$RUN WILL START AT $start_date  "
echo " ${job} $NET$RUN WILL END   AT $end_date  "
set -x
#
if [[ $RUN_ENVIR = nco  || $RUN_ENVIR = devpara ]] ; then
  echo " ${job} $NET$RUN IN ${cyc}Z CYCLE FORECAST CONTINUES FROM $current_date  "
fi
echo " #####################################################"

export MP_EAGER_LIMIT=32768

export cfss=${cfss:-"/cfs"}
export cfsp=${cfsp:-"cfs_"}                # prefix for cfs specific directories
export PERR=${PERR:-"echo Error Exit"}
export ndays_back_pert=${ndays_back_pert:-3}

export ENS_MEM=$((ENS_MEM+0))
ENS_MEM0=$ENS_MEM
if [ $ENS_MEM -lt 10 ] ; then export ENS_MEM=0$ENS_MEM ; fi

################################
# update or cd to COM directory
################################
export YMDH=$start_date
export COM_YMDH=${COM_YMDH:-$COMOUT/${YMDH}_${ENS_MEM}}
mkdir -m 775 -p $COM_YMDH
export ARCHIVE_DIR=${ARCHIVE_DIR:-$COM_YMDH}
export nknd=${nknd:-""}
export SUFIN=${SUFIN:-.gdas$nknd}
export SUFOUT=${SUFOUT:-.$ENS_MEM.$YMDH}
#
export NTHSTACK=${NTHSTACK:-128000000}
#
export DELTIM=${DELTIM:-600}
export dt_cpl=d${dt_cpld:-1800}
export dt_ocean=${dt_ocean:-1800}
export dt_aocpl=${dt_aocpl:-${dt_ocean:-1800}}
export dt_rstrt=${dt_rstrt:-$dt_cpld}
export CouplingPeriod=${CouplingPeriod:-${DELTIM:-1800}}
export do_irestart=${do_irestart:-.false.}
export PARM_OM=${PARM_OM:-$HOMEcfs/parm/${cfsp}parm_om}
export diagtable=${diagtable:-$PARM_OM/diag_table.hr}
export fieldtable=${fieldtable:-$PARM_OM/field_table}
export datatable=${datatable:-$PARM_OM/data_override}
export namelist=${namelist:-$PARM_OM/namelist}
export FHBEG=0
#

################################
# cd to working directory
################################
cd $DATA

CFSRR_IC=${CFSRR_IC:-YES}
export IC_FROM_DISK=${IC_FROM_DISK:-NO}
export IC_FROM_PROD=${IC_FROM_PROD:-YES}
export INI_DIR=${INI_DIR:-$DATA}
#################################
# create archive directory if need
#################################
if [[ $SENDCOM = YES ]] ; then
  mkdir -p $ARCHIVE_DIR
fi 

export fcsthrs=$($NHOUR $end_date $start_date) ;# total length of forecast in hours
export FH_CYCL=${FH_CYCL:-$fcsthrs}
export YYYY=$(echo $YMDH | cut -c1-4)
export MM=$(echo $YMDH | cut -c5-6)
export DD=$(echo $YMDH | cut -c7-8)
export HH=$(echo $YMDH | cut -c9-10)
export CMDH=$current_date
    
#  To run from an existing restart
#  ------------------------------
if [ $current_date != $start_date -a ${FCST_RESTART:-YES} = YES ] ; then
                   #########################
                   # check recovery files
                   #########################
   if [ $COUP_FCST = YES ] ; then
     if [ -s $RECOVERY/sfcr${SUFOUT}.$CMDH -a             \
          -s $RECOVERY/sigr1${SUFOUT}.$CMDH -a            \
          -s $RECOVERY/sigr2${SUFOUT}.$CMDH -a            \
          -s $RECOVERY/omrestart${SUFOUT}.$CMDH.tar -a    \
          -s $RECOVERY/fluxes_for_OM${SUFOUT}.$CMDH ] ; then
        export SIGI=$RECOVERY/sigr1${SUFOUT}.$CMDH
        export SIGI2=$RECOVERY/sigr2${SUFOUT}.$CMDH
        export SFCI=$RECOVERY/sfcr${SUFOUT}.$CMDH
        export OCNI=$RECOVERY/omrestart${SUFOUT}.$CMDH.tar
        $NCP $RECOVERY/fluxes_for_OM${SUFOUT}.$CMDH fluxes_for_OM
        msg="Coupled Forecast Starting with Restart files in $RECOVERY"
        postmsg "$jlogfile" "$msg"
     else
        echo ' No restart available at' $RECOVERY 'for' $current_date
        echo ' This job cannot be continued - job terminated'
        export err=1; err_chk
        exit
     fi
   else
     if [ -s $RECOVERY/sfcr${SUFOUT}.$CMDH -a             \
          -s $RECOVERY/sigr1${SUFOUT}.$CMDH -a            \
          -s $RECOVERY/sigr2${SUFOUT}.$CMDH -a ] ; then
        export SIGI=$RECOVERY/sigr1${SUFOUT}.$CMDH
        export SIGI2=$RECOVERY/sigr2${SUFOUT}.$CMDH
        export SFCI=$RECOVERY/sfcr${SUFOUT}.$CMDH
        msg="Unoupled Forecast Starting with Restart files in $RECOVERY"
        postmsg "$jlogfile" "$msg"
     else
        echo ' No restart available at' $RECOVERY 'for' $current_date
        export err=1; err_chk
        exit
     fi
   fi

   export nhourb=$($SIGHDR $SIGI2 ifhr)
   rc=$?
   if [[ $rc -ne 0 ]] ; then
     if [ $RUN_ENVIR = nco  -o $RUN_ENVIR = devpara ] ; then
        msg="NO Ocean or Atmospheric Restart files"
        postmsg "$jlogfile" "$msg"
        export err=$rc; err_chk
     else
        $PERR;exit $rc
     fi
   fi
else
   echo 'Either no restart is available or the current_date is start_date'
#
#      To get past initial conditions from HPSS or DISK (CFSR)
#
 if [ $ENS_MEM0 -eq 1 ] ; then
   if [ $RUN_ENVIR = nco  -o $RUN_ENVIR = devpara ] ; then
     IC_FROM_PROD=${IC_FROM_PROD:-YES}
   fi
   if [ $IC_FROM_PROD = YES ] ; then
     export SIGI=${SIGI:-$COMCDAS/cdas1.t${cyc}z.sanl}
     export SFCI=${SFCI:-$COMCDAS/cdas1.t${cyc}z.sfcanl}
     if [ $COUP_FCST = YES ] ; then
       export OCNI=${OCNI:-$COMCDAS/ocnanl$SUFIN.$YMDH.tar}
     fi
   elif [ ${IC_FROM_HPSS:-NO} = YES ] ; then
     .  $USHcfs/cfs_getcfsric.sh $YMDH $DATA $nknd
      export SIGI=$DATA/siganl${SUFIN}.$YMDH
      export SFCI=$DATA/sfcanl${SUFIN}.$YMDH
      export OCNI=$DATA/ocnanl${SUFIN}.$YMDH.tar
      cd $DATA
   elif [ ${IC_FROM_DISK:-YES} = YES ] ; then
      export SIGI=${SIGI:-$INI_DIR/siganl${SUFIN}.$YMDH}
      export SFCI=${SFCI:-$INI_DIR/sfcanl${SUFIN}.$YMDH}
      OCN_ICDIR=${OCN_ICDIR:-$INI_DIR}
      export OCNI=${OCNI:-$OCN_ICDIR/ocnanl${SUFIN}.$YMDH.tar}
   elif [ -s $COM_YMDH/siganl.$ENS_MEM.$YMDH -a \
          -s $COM_YMDH/sfcanl.$ENS_MEM.$YMDH ] ; then
     export SIGI=$COM_YMDH/siganl.$ENS_MEM.$YMDH
     export SFCI=$COM_YMDH/sfcanl.$ENS_MEM.$YMDH
     if [ $COUP_FCST = YES ] ; then
       if [ -s $COM_YMDH/ocnanl.$ENS_MEM.$YMDH.tar ] ; then
         export OCNI=$COM_YMDH/ocnanl.$ENS_MEM.$YMDH.tar
       else
         echo 'NO OCEAN IC SUPPLIED - EXIT'
         export err=1; err_chk
       fi
     fi
   else
     echo 'NO GOOD IC SUPPLIED - EXIT'
   fi
   rc=$?
   if [[ $rc -ne 0 ]] ; then
     if [ $RUN_ENVIR = nco  -o $RUN_ENVIR = devpara ] ; then
        msg="NO Ocean or Atmospheric IC files"
        postmsg "$jlogfile" "$msg"
        export err=$rc; err_chk
     else
        $PERR;exit $rc
     fi
   fi

################################################################################# Run change resolution and surface cycle if necessary

   idvc=$($SIGHDR $SIGI idvc)
   jcap=$($SIGHDR $SIGI jcap)
   levs=$($SIGHDR $SIGI levs)
#
   if [[ $IDVC -ne $idvc || $JCAP -ne $jcap || $LEVS -ne $levs ]] ; then
     export CHGRESSH=${CHGRESSH:-$HOMEgsm/ush/global_chgres.sh}
     export SIGINP=$SIGI
     export SFCINP=$SFCI
     echo $SIGINP
     echo $SFCINP
     export SIGOUT=$COM_YMDH/siganl.$ENS_MEM.$YMDH
     export SFCOUT=$COM_YMDH/sfcanl.$ENS_MEM.$YMDH
#    export SIGOUT=$DATA/siganl.$YMDH
#    export SFCOUT=$DATA/sfcanl.$YMDH
     if [ $gfsio_in = .true. ] ; then
       export GFSOUT=$DATA/gfsanl.$YMDH
     fi
     export OUTTYP=${OUTTYP:-0}
     export LATCH=${LATCH:-8}
     export CHGRESVARS="IDVC=$IDVC,IVSSIG=$ivssig,IVSSFC=$ivssfc,NVCOORD=$nvcoord,IDVM=$IDVM,IDSL=$IDSL,LATCH=$LATCH,$CHGRESVARS"
     if [ $IDVM -gt 3 ] ; then
       if [ $THERMODYN_ID -gt 2 ] ; then
         CPIlist=${CPIlist:-""}
         RIlist=${RIlist:-""}
        export CHGRESVARS=$CHGRESVARS"RI=$RIlist,CPI=$CPIlist"
       fi
     fi
     export SIGLEVEL=${SIGLEVEL:-$HOMEcfs/fix/cfs_fix_am/global_hyblev.l$LEVS.txt}
     export LONSPERLAT=${LONSPERLAT:-$HOMEcfs/fix/cfs_fix_am/global_lonsperlat.t$JCAP.txt}

     $CHGRESSH
     rc=$?
     if [[ $rc -ne 0 ]] ; then
       if [ $RUN_ENVIR = nco  -o $RUN_ENVIR = devpara ] ; then
          msg="NO Ocean or Atmospheric IC files"
          postmsg "$jlogfile" "$msg"
          export err=$rc; err_chk
       else
          $PERR;exit $rc
       fi
     fi
     export SIGI=$SIGOUT
     export SFCI=$SFCOUT
#
     if [ $gfsio_in = .true. ] ; then
       export SIGI=$GFSOUT
     fi
   else
     $NCP $SIGI $COM_YMDH/siganl.$ENS_MEM.$YMDH
     $NCP $SFCI $COM_YMDH/sfcanl.$ENS_MEM.$YMDH
     $FSYNC $COM_YMDH/siganl.$ENS_MEM.$YMDH
     $FSYNC $COM_YMDH/sfcanl.$ENS_MEM.$YMDH
   fi

###If production, copy the IC files to cdas directory
#
#   if [ $RUN_ENVIR = nco  -o $RUN_ENVIR = devpara ] ; then
#      COMCDAS=$COMROT/cdas.$(echo $YMDH | cut -c1-8)
#      $NCP $SIGI $COMCDAS/cdas2.t${cyc}z.sanl
#      $NCP $SFCI $COMCDAS/cdas2.t${cyc}z.sfcanl
#
### fi

   if [ $COUP_FCST = YES ] ; then
     $NCP $OCNI $COM_YMDH/ocnanl.$ENS_MEM.$YMDH.tar
     $FSYNC $COM_YMDH/ocnanl.$ENS_MEM.$YMDH.tar
   fi

#     Added for member 1 to release the m2 m3 and m4 run
   if [ -s $COM_YMDH/siganl.$ENS_MEM.$YMDH -a -s $COM_YMDH/sfcanl.$ENS_MEM.$YMDH -a -s $COM_YMDH/ocnanl.$ENS_MEM.$YMDH.tar ]; then
     ecflow_client --event release_m2m3m4
     echo "done" > $RECOVERY/done.flag_released
   fi
 else

#  If ENS_NUM > 1 and PERTURB_IC=YES, create perturbed IC

   if [ $ENS_MEM0 -gt 1 -a ${PERTURB_IC:-YES} = YES ] ; then
     COMANL_DIR0=${COMANL_DIR0:-$COMOUT/${YMDH}_01}
     if [ ! -d $COMANL_DIR0 ] ; then
       COMANL_DIR0=$COMOUT/${FHOUT}hrly_grib_01
     fi
     YMDH1=$($NDATE -24 $YMDH)
     YYYYMMDD1=$(echo $YMDH1 | cut -c1-8)
     HH1=$(echo $YMDH1 | cut -c9-10)

     #COMANL_DIR1=${COMANL_DIR1:-$COMROT/$RUN.$YMDH1/${YMDH1}_01}     # Comment Julia
     COMANL_DIR1=${COMANL_DIR1:-$COMROT/$RUN.$YYYYMMDD1/$HH1/${FHOUT}hrly_grib_01}

     if [ $ENS_MEM0 -eq 2 ] ; then
       wt1=$(eval echo \${wt1_$ENS_MEM:-0.9})
       wt2=$(eval echo \${wt2_$ENS_MEM:-0.1})
  
       USE_FCST=${USE_FCST:-NO}
#      wt2=$((1.0-wt1))
       export SIGI=$COM_YMDH/siganl.$ENS_MEM.$YMDH

       if [ $RUN_ENVIR = nco  -o $RUN_ENVIR = devpara ] ; then
         dir_str=${FHOUT}hrly_grib
       else
         dir_str=$YMDH1
       fi
       #FCST_DIRm1=${FCST_DIRm1:-$COMROT/$RUN.$YMDH1}
       FCST_DIRm1=${FCST_DIRm1:-$COMROT/$RUN.$YYYYMMDD1/$HH1}
       sigf1=$FCST_DIRm1/${dir_str}_01/sigf${YMDH}.01.$YMDH1
       if [ -s $sigf1 -a $USE_FCST = YES ] ; then
         $sigavg -w $wt1,$wt2 $COMANL_DIR0/siganl.01.$YMDH $sigf1 $SIGI
       else
         $sigavg -w $wt1,$wt2 $COMANL_DIR0/siganl.01.$YMDH $COMANL_DIR1/siganl.01.$YMDH1 $SIGI
       fi
       rc=$?
       if [[ $rc -ne 0 ]] ; then
          echo "Error in obtaining the Atmospheric IC files"
          if [ $RUN_ENVIR = nco  -o $RUN_ENVIR = devpara ] ; then
             export err=$rc; err_chk
          else
            $PERR;exit $rc
          fi
       fi
       if [ $COUP_FCST = YES ] ; then
         export OCNI=$COM_YMDH/ocnanl.$ENS_MEM.$YMDH.tar
         REST_DIRm1=${REST_DIRm1:-$COMROT/$RUN.$YYYYMMDD1/$HH1}
         ocnf1=$REST_DIRm1/${dir_str}_01/omrestart.01.$YMDH1.$YMDH.tar
         if [ -s $ocnf1 -a $USE_FCST = YES ] ; then
           $sstavg -w $wt1 $wt2 $COMANL_DIR0/ocnanl.01.$YMDH.tar $ocnf1 $OCNI
         else
           $sstavg -w $wt1 $wt2 $COMANL_DIR0/ocnanl.01.$YMDH.tar $COMANL_DIR1/ocnanl.01.$YMDH1.tar $OCNI
         fi
         rc=$?
         if [[ $rc -ne 0 ]] ; then
            echo "Error in obtaining the Ocean IC files"
            if [ $RUN_ENVIR = nco  -o $RUN_ENVIR = devpara ] ; then
              export err=$rc; err_chk
            else
              $PERR;exit $rc
            fi
         fi
       fi
     elif [ $ENS_MEM0 -gt 2 ] ; then
# Copy the 24-h forecast sigma files from the previous day:
       if [ $RUN_ENVIR = nco  -o $RUN_ENVIR = devpara ] ; then
         dir_str=${FHOUT}hrly_grib
       else
         dir_str=$YMDH1
       fi
       FCST_DIRm1=${FCST_DIRm1:-$COMROT/$RUN.$YYYYMMDD1/$HH1}
       i_try=1
       while [ $i_try -lt $ndays_back_pert ] ; do
         sigf1=$FCST_DIRm1/${dir_str}_01/sigf${YMDH}.01.$YMDH1
         sigf2=$FCST_DIRm1/${dir_str}_02/sigf${YMDH}.02.$YMDH1
         if [ -s $sigf1 -a -s $sigf2 ] ; then
           break
         else
           YMDH1=$($NDATE -24 $YMDH1)
           YYYYMMDD1=$(echo $YMDH1 |cut -c1-8)
           HH1=$(echo $YMDH1 |cut -c9-10)
           FCST_DIRm1=$COMROT/$RUN.$YYYYMMDD1/$HH1
           if [ $RUN_ENVIR != nco  -a $RUN_ENVIR != devpara ] ; then dir_str=$YMDH1 ; fi
           i_try=$((i_try+1))
         fi
       done
       if [ $COUP_FCST = YES ] ; then
         YMDH1=$($NDATE -24 $YMDH)
         YYYYMMDD1=$(echo $YMDH1 |cut -c1-8)
         HH1=$(echo $YMDH1 |cut -c9-10)
         REST_DIRm1=${REST_DIRm1:-$COMROT/$RUN.$YYYYMMDD1/$HH1}
         i_try=1
         while [ $i_try -lt $ndays_back_pert ] ; do
           ocnf1=$REST_DIRm1/${dir_str}_01/omrestart.01.$YMDH1.$YMDH.tar
           ocnf2=$REST_DIRm1/${dir_str}_02/omrestart.02.$YMDH1.$YMDH.tar
           if [ -s $ocnf1 -a -s $ocnf2 ] ; then
             break
           else
             YMDH1=$($NDATE -24 $YMDH1)
             YYYYMMDD1=$(echo $YMDH1 |cut -c1-8)
             HH1=$(echo $YMDH1 |cut -c9-10)
             REST_DIRm1=$COMROT/$RUN.$YYYYMMDD1/$HH1
             if [ $RUN_ENVIR != nco  -a $RUN_ENVIR != devpara ] ; then dir_str=$YMDH1 ; fi
             i_try=$((i_try+1))
           fi
         done
       fi
       if [ $ENS_MEM0 -eq 3 ] ; then
         wt1=$(eval echo \${wt1_$ENS_MEM:-1.0})
         wt2=$(eval echo \${wt2_$ENS_MEM:-1.0})
         wt3=$(eval echo \${wt3_$ENS_MEM:--1.0})
       elif [ $ENS_MEM0 -eq 4 ] ; then
         wt1=$(eval echo \${wt1_$ENS_MEM:-1.0})
         wt2=$(eval echo \${wt2_$ENS_MEM:--1.0})
         wt3=$(eval echo \${wt3_$ENS_MEM:-1.0})
       fi
#      wt3=$((1-wt1-wt2))
       export SIGI=$COM_YMDH/siganl.$ENS_MEM.$YMDH
       $sigavg -w $wt1,$wt2,$wt3 $COMANL_DIR0/siganl.01.$YMDH $sigf1 $sigf2 $SIGI
       rc=$?
       if [[ $rc -ne 0 ]] ; then
          if [ $RUN_ENVIR = nco  -o $RUN_ENVIR = devpara ] ; then
             msg= "Error in getting the sigma input file for member $ENS_MEM"
             postmsg "$jlogfile" "$msg"
             export err=$rc; err_chk
          else
            $PERR;exit $rc
          fi
       fi
       if [ $COUP_FCST = YES ] ; then
         export OCNI=$COM_YMDH/ocnanl.$ENS_MEM.$YMDH.tar
         $sstavg -w $wt1 $wt2 $wt3 $COMANL_DIR0/ocnanl.01.$YMDH.tar $ocnf1 $ocnf2 $OCNI
         rc=$?
         if [ $rc -ne 0 ] ; then
           if [ $RUN_ENVIR = nco  -o $RUN_ENVIR = devpara ] ; then
             msg="Error in getting the Ocean input file for member $ENS_MEM"
             postmsg "$jlogfile" "$msg"
             export err=$rc; err_chk
           else
             $PERR;exit $rc
           fi
         fi
       fi 
     fi
     $NCP $COMANL_DIR0/sfcanl.01.$YMDH $COM_YMDH/sfcanl.$ENS_MEM.$YMDH
   fi
#  export SIGI=$COM_YMDH/siganl.$ENS_MEM.$YMDH
   export SFCI=$COM_YMDH/sfcanl.$ENS_MEM.$YMDH
 fi                       # if on ENS_MEM
#
 export nhourb=0
fi

#
#  untar the ocean restart file in the $DATA/INPUT directory
#
if [ $COUP_FCST = YES ] ; then
   if [ -s $OCNI ] ; then
     mkdir -p $DATA/INPUT
     cd $DATA/INPUT
     tar -xvf $OCNI
     for newname in $(ls -rt ${CMDH}* | cut -c11-) ; do
       mv ${CMDH}$newname $newname
     done
     cd $DATA
   else
     echo ' Ocean restart file ' $OCNI ' not found'
     export err=555; err_chk
   fi
fi

# Set nhours for the chunk of the forecast in this cycle

export nhours=$((nhourb+FH_CYCL))                                      #### set the end of the next segment
if [ $nhours -gt $fcsthrs ] ; then nhours=$fcsthrs ; fi

export FH_INI=$nhourb
echo "`date` $0: fcst from $nhourb to $nhours is starting" 
export FHCYC=${FHCYC:-24}
startmsg

export NGPTC=${NGPTC:-$((JCAP/10))}

# The time loop for the coupled run in $INCHOUR hr increment

export INC_HOUR=${INC_HOUR:-${INCHOUR:-360}}
if [ $INC_HOUR -gt $nhours ] ; then export INC_HOUR=$nhours ; fi
export INC_HOUR_FST=${INC_HOUR_FST:-$INC_HOUR}

# Loop over forecast segment

until [[ $nhourb -ge $nhours ]] ; do
  export FHINI=$((nhourb+0))
  export INCHOUR=${INC_HOUR:-$INCHOUR}
  if [ $INC_HOUR -gt $INC_HOUR_FST ] ; then
    if [ $FHINI -lt $INC_HOUR_FST ] ; then
      export INCHOUR=$INC_HOUR_FST
    elif [ $FHINI -lt $INC_HOUR ] ; then
      export INCHOUR=$((INC_HOUR-INC_HOUR_FST))
    fi
  fi
  if [[ $FHINI -lt 10 ]] ; then FHINI=0$FHINI ; fi
  export FHMAX=$((nhourb+$INCHOUR))
  if [[ $FHMAX -gt $nhours ]] ; then export FHMAX=$nhours ; fi

  export INCHOUR=$((FHMAX-FHINI))
  if [[ $INCHOUR -lt 24 ]] ; then FHRES=$FHMAX ; fi

  if [[ $FHRES -gt 24 && $(((FHMAX/FHRES)*FHRES)) -ne $FHMAX ]] ; then
    export FHRES=$FHMAX
  fi

  if [[ $(((FHMAX*3600/dt_rstrt)*(dt_rstrt/3600))) -ne $FHMAX ]] ; then
    dt_rstrt=$((FHMAX*3600))
    export INCHOUR=$((FHMAX-FHINI))
  fi

  if [[ $FHMAX -lt 10 ]] ; then export FHMAX=0$FHMAX ; fi

###########################################################################
# export FNTSFA=$sst_filename         ;# Observed or predicted SST grib file
## export FTSFS=99999.0                ;# e-folding time for SST
# export FTSFS=90.0                   ;#  substituted
#                                - this is presumably the GFS falue
## export FNACNA=$ice_filename         ;# Observed sea ice file (left blank)
# export FNACNA=                      ;# Observed sea ice file (left blank)
# export FAISS=0.0                    ;# e-folding time for ice default = 99999
###########################################################################
#
  export CYCLVARS='FALBL=0.0,FALBS=0.0,'
#
  export SUFOUT=${SUFOUT:-""}
  export SIGO='$COM_YMDH/sigf${FH}$SUFOUT'
  export SFCO='$COM_YMDH/sfcf${FH}$SUFOUT'
  export FLXO='$COM_YMDH/flxf${FH}$SUFOUT'
  export LOGO='$COM_YMDH/logf${FH}$SUFOUT'
  export D3DO='$COM_YMDH/d3df${FH}$SUFOUT'
#
  export SIGR1=$DATA/sig1r${FHMAX}${SUFOUT}
  export SIGR2=$DATA/sig2r${FHMAX}${SUFOUT}
  export SFCR=$DATA/sfcr${FHMAX}${SUFOUT}
  export NSSTR=$DATA/nsstr${FHMAX}${SUFOUT}

  export RESDIR=${RESDIR:-$RECOVERY}
  export RESTART_CONTROL_FILE=${RESTART_CONTROL_FILE:-$RESDIR/pr$PSLOT.2restart$SUFOUT}
  if [ $FHINI -eq 0 ] ; then rm -f $RESTART_CONTROL_FILE ; fi
  export AM_SST=$RESDIR/AM_SST_${FHINI}_${FHMAX}

  export VDATE=$($NDATE $FHINI $start_date)

# Execute  Coupled Atmospheric-Ocean model forecast

  $FORECASTSH - $DATA $VDATE $EXEC_OMD $COM_YMDH >$DATA/$(basename $FORECASTSH).out.$FHINI 2>$DATA/$(basename $FORECASTSH).err.$FHINI
  rc=$?

  cat $(basename $FORECASTSH).out.$FHINI > $pgmout
  $NCP $(basename $FORECASTSH).err.$FHINI  errfile

  if [[ $rc -ne 0 ]] ; then
    if [ $RUN_ENVIR = nco  -o $RUN_ENVIR = devpara ] ; then
       msg="Error in executing the forecast script"
       postmsg "$jlogfile" "$msg"
       export err=$rc; err_chk
    else
       $PERR;exit $rc
    fi
  fi

# if [ $rc -ne 0 ] ; then exit $rc; fi
  export err=$rc; err_chk


  LINKFILESH=${LINKFILESH:-""}
  if [ ! -z $LINKFILESH ] ; then
    if [ -s $LINKFILESH ] ; then $LINKFILESH ; fi
  fi
#
# copy the restart files
#
  $NCP $DATA/RESTART/* $DATA/INPUT/

################################################################
#             This section is for multiple restarts
#   cd $DATA/IRESTART
#   nres=$(((FHMAX-FHINI)*3600/dt_rstrt))
#   n=1
#   until [ $n -gt $nres ] ; do
#     FDATE=$($NDATE $((FHINI+n*dt_rstrt/3600)) $CDATE)
#     tar -cvf $RESDIR/omrestart.$CDUMP.$CDATE.$FDATE.tar ${FDATE}*
#     n=$((n+1))
#   done
################################################################
#   cd $DATA/RESTART
#   FDATE=$($NDATE $FHMAX $CDATE)
#   tar -cvf $RESDIR/omrestart.$CDATE.$FDATE.tar *
#   cstr=$(((FHMAX-FHBEG)/DELTIM))
#   $NCP -p $DATA/fluxes_for_OM_$cstr $RESDIR/fluxes_for_OM.$CDATE.$FDATE
  cd $DATA

  echo VDATE = $VDATE after GFS forecast $(date)
#
  nhourb=$FHMAX
  if [ $nhourb -lt $nhours ] ; then
    cd $DATA
    ls -l $SIGR1 ; ls -l $SIGR2 ; ls -l $SFCR ; ls -l $NSSTR
    $NCP $SIGR1 $DATA/sigr1a
    $NCP $SIGR2 $DATA/sigr2a
    $NCP $SFCR  $DATA/sfcra
    if [ ${NSST_ACTIVE:-.false.} = .true. ] ; then
      $NCP $NSSTR  $DATA/nsstra
    fi
    export SIGI=$DATA/sigr1a
    export SIGI2=$DATA/sigr2a
    export SFCI=$DATA/sfcra
    export NSSTI=$DATA/nsstra
    mv $DATA/time_stamp.out $DATA/time_stamp.out_$nhourb
    $NCP $DATA/RESTART/* $DATA/INPUT/
  fi

  if [ $((FH_INI+INCHOUR)) -eq $FHMAX -o $FHMAX -eq $nhours ] ; then
#
#  save necessary files to com
#
    if [ $SENDCOM = YES ] ; then
      for file in $FILES_TO_SEND_COM ; do
        if [ $file = sig -o $file = sfc -o $file = flx -o $file = d3d ] ; then
          FH=$FH_INI
          if [[ $FH_INI -eq 0 ]] ; then
            FH=-$FHOUT
          fi
          until [[ $((FH=10#$FH+10#$FHOUT)) -gt $FHMAX ]] ; do
            [[ $FH -lt 10 ]]&&FH=0$FH
            ln -fs $COM_YMDH/${file}f${FH}$SUFOUT $COM_YMDH/${file}f$($NDATE $FH $YMDH)$SUFOUT
          done
        fi
      done
      if [ -s $DATA/${AM_SST} ] ; then
        ini_date=$($NDATE $FH_INI $YMDH)
        fin_date=$($NDATE $FHMAX $YMDH)
        $NCP $DATA/${AM_SST} $COM_YMDH/AM_SST_${ini_date}_${fin_date}
      fi
    fi

    if [ $SAVEGES = YES ] ; then           #  create recovery/restart files
      current_date=$($NDATE $FHMAX $YMDH)
      CMDH=$current_date
      $NCP $SFCR $RECOVERY/sfcr${SUFOUT}.$CMDH
      $NCP $SIGR1 $RECOVERY/sigr1${SUFOUT}.$CMDH
      $NCP $SIGR2 $RECOVERY/sigr2${SUFOUT}.$CMDH
      if [ ${NSST_ACTIVE:-.false.} = .true. ] ; then
        $NCP  $NSSTR $RECOVERY/nsstr${SUFOUT}.$CMDH
      fi
      cd $DATA/RESTART
      tar -cvf $RECOVERY/omrestart${SUFOUT}.$CMDH.tar *
      cstr=$(((FHMAX-FHBEG)*3600/DELTIM))
      cd $DATA
      $NCP $DATA/fluxes_for_OM_$cstr $RECOVERY/fluxes_for_OM${SUFOUT}.$CMDH
      if [ $FHMAX -eq 24 -a $ENS_MEM -le 2 ] ; then
        if [ $RUN_ENVIR = nco  -o $RUN_ENVIR = devpara ] ; then
          dir_str=${FHOUT}hrly_grib_$ENS_MEM
        else
          dir_str=${YMDH}_$ENS_MEM
        fi
        YYYYMMDD=$(echo $YMDH |cut -c1-8)
        HH=$(echo $YMDH |cut -c9-10)
        mkdir -p $COMROT/$RUN.$YYYYMMDD/$HH/$dir_str
        $NCP $RECOVERY/omrestart${SUFOUT}.$CMDH.tar $COMROT/$RUN.$YYYYMMDD/$HH/$dir_str/
        $NCP $COM_YMDH/sigf${CMDH}.${ENS_MEM}.$YMDH $COMROT/$RUN.$YYYYMMDD/$HH/$dir_str/
      fi

#
# create stamp corresponding g to the above restart
#
      if [[ $current_date -lt $end_date ]] ; then
        echo $start_date $current_date $end_date  $YYYYSTART $YYYYEND $cycle > $RECOVERY/stamp.$RUN
      fi
    fi
#
#  post processing of the forecasts (pgb and average)
#
    archive_dir=$COM_YMDH
    $CFSPREPAREPOSTSH $BATCH_POST $COM_YMDH \
             $start_date:$FH_INI:$FHMAX:$FHCYC \
             $end_date:$fcsthrs:$INCHOUR:$FHOUT:$FHOUT:$nhours \
             $ARCHIVE_DIR  $ENS_MEM $CLIMCOM $VYYYYMM
#    In the above line $ENS_MEM is used to represent an ensemble member
#
    cstr=$(((FHMAX-FHBEG)*3600/DELTIM))
    $NCP $DATA/fluxes_for_OM_$cstr $DATA/fluxes_for_OM
    FH_INI=$((FH_INI+INCHOUR))
  fi
done                #   Coupled forecast for this cycle finished

##echo ' finished' ${cyc}'z cycle run for the ensemble member ' $ENS_MEM ' starting from ' $start_date

##############################################
# end of one cycle for a member of ensemble
##############################################

current_date=$($NDATE $FHMAX $YMDH)

# create continuation stamp or remove stamp if the whole run ic complete

if [[ $current_date -lt $end_date ]] ; then
  echo $start_date $current_date $end_date  $YYYYSTART $YYYYEND $cycle > $RECOVERY/stamp.$RUN
  echo $start_date $current_date $end_date  ###submitting next $FH_CYCL segment                           
  ###cd $homejsw; runcfs $start_date 
else
  echo $start_date $current_date $end_date  > $POSTDEFS/stamp.done
  YMDH_DONE=$YMDH
  ###cd $homejsw; runcfspost $start_date
fi
