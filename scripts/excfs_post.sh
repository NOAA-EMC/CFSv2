#!/usr/bin/env ksh  
######################################################################
#
#   Script:  excfs_postprocessing.sh.sms   
#   Author:  Shrinivas Moorthi
#   DATE:    February, 2004
#   Updated for CFSv2 in April 2010 by Shrinivas Moorthi
#
#   Based on SFM script by Hann-Ming Henry Juang
#
#
#   Main driver to do postprocessing for CFS (Climate Forecast SYSTEM)
#   Coupled Model Integrations
#   History: 2001.11.06 First Implementation  for SFM
######################################################################
# function to check execution wall time
######################################################################
timechk() {
#!/usr/bin/env bash
set -x

tbeg=$1
tnow=`date +"%H%M%S"`

h1=10#$(echo $tbeg|cut -c 1-2)
m1=10#$(echo $tbeg|cut -c 3-4)
s1=10#$(echo $tbeg|cut -c 5-6)

h2=10#$(echo $tnow|cut -c 1-2)
m2=10#$(echo $tnow|cut -c 3-4)
s2=10#$(echo $tnow|cut -c 5-6)

d1=$((h2-h1))
d2=$((m2-m1))
d3=$((s2-s1))

if [[ d3 -lt 0 ]]; then d3=$((d3+60)); d2=$((d2-1)); fi
if [[ d2 -lt 0 ]]; then d2=$((d2+60)); d1=$((d1-1)); fi
if [[ d1 -lt 0 ]]; then d1=$((d1+24)); fi

printf "%2.2d%2.2d%2.2d\n" $d1 $d2 $d3

return 0
}
######################################################################

# ###############################
#  START CFS POST PROCESSING
# ###############################

set -u

set -x
echo " ------------------------------------------------------------"
echo "  "
echo "               IBM-SP $RUN_ENVIR PROCESSING     "
echo "  "
echo "            SEASONAL FORECAST MODEL POSTPROCESSING     "
echo "  "
echo "                `date`     "
echo "  "
echo "                   JOB  $job  "
echo "  "
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
echo " Run id for com processing is ......... ....... $RUN"
echo "  "
echo " standard output in file ...................... $pgmout"
echo " unique processing id for run ................. $jobid"
echo " SENDCOM=YES means save com files ............. $SENDCOM"
echo " ------------------------------------------------------------"
set -x

export PARM_AM=${PARM_AM:-$HOMEcfs/parm/cfs_parm_am}
export POSTGPSH=${POSTGPSH:-$USHcfs/cfs_cdas_nceppost.sh}
export ANOMCATSH=${ANOMCATSH:-$HOMEcfs/ush/cfs_anomcat.sh}
export SIGHDR=${SIGHDR:-$HOMEgsm/exec/global_sighdr}
export GRID_ID25=${GRID_ID25:-2}
export GENPSICHI=${GENPSICHI:-YES}
export NTHSTACK=${NTHSTACK:-4096000000}
export IDRT=${IDRT:-4}
export in_o=${in_o:-0}
export RUN_FLAG=${RUN_FLAG:-''}
export OUTTYP=3

###############################################
# check for post restart or recovery job
###############################################

export ENS_MEM=$((ENS_MEM+0))
export NCP=${NCP:-/bin/cp}
FH_45DAYS=${FH_45DAYS:-1080}
SAVE_TIME_45DAYS=${SAVE_TIME_45DAYS:-YES}
if [ $ENS_MEM -lt 10 ] ; then export ENS_MEM=0$ENS_MEM ; fi

################################################
# looping until no job to do or wait too long
################################################
BREAK_ON_RUN=NO

# setup segment to do parameter

done_max=${done_max:-5}
done_count=0

wait_count=0
wait_max=180

while [ $wait_count -le $wait_max -a $BREAK_ON_RUN = NO ] ; do

  wait_count=$((wait_count+1))  

  while [ -n "$(ls $POSTDEFS/post_def$cyc.*)" ] ; do

      done_count=$((done_count+1))

      #if [[ $done_count -gt $done_max ]]; then
      ### cd $homejsw; ./runcfspost $start_date
      ### exit
      #fi

      [[ $done_count -eq 1 ]] && tbeg=$(date +"%H%M%S")
      tno=$(date +"%H%M%S")
      tck=$(timechk $tbeg) 

      #if [[ $tck -gt 050000 ]]; then
      ### cd $homejsw; ./runcfspost $start_date
      ### exit
      #fi

      ###############################################
      # Run it if there is definition files for post and not time to resubmit
      ###############################################

      defile=$(ls -rt $POSTDEFS/post_def$cyc.* | head -n1)
      bafile=$(basename $defile); postid=${bafile:11}
      . $POSTDEFS/post_def$cyc.$postid

      export CDATE=$start_date
      if [ $((ENS_MEM+0)) -ne $((MEMBER+0)) ] ; then
        echo 'Member number confilct : ENS_MEM= '$ENS_MEM ', MEMBER= '$MEMBER
        echo ' exiting post job'
        exit
      fi
      ###export COM_YMDH=$OUTDIR
      export COM_YMDH_DG=$COM_YMDH/daily_grib
      export COM_YMDH_MG=$CLIMCOM/monthly_grib_${ENS_MEM}         
      export COM_DG=$CLIMCOM/time_grib_${ENS_MEM}
      export COM_HRLY=$CLIMCOM/${FHOUT}hrly_grib_${ENS_MEM}
      mkdir -p $COM_YMDH_DG
      if [ "$AVG_GRB" != NO ]; then
        mkdir -p $COM_YMDH_MG
      fi
      mkdir -p $COM_DG
      mkdir -p $COM_HRLY

      #############################
      # Create date related temp dir
      ###############################
      export YMDH=$start_date
      export SUFOUT=${SUFOUT:-.$ENS_MEM.$YMDH}
      cd $DATA

      ############################################
      # Copy necessary files from COM to temp dir
      ############################################
      if [[ $FHS -eq 0 ]] ; then
        FHINI=-$INTSIG
      else
        FHINI=$FHS
      fi
      export FHS=$FHS
      export FHE=$FHE

      if [ $RUN_ENVIR = nco  -o $RUN_ENVIR = devpara ]
      then
        if [ $FHE -gt $nhours ]
        then
          echo "Forecast hour is greater than end_hour for this chunk"
          echo "Should be processed in the next chunk"
          exit 0
        fi
      fi

      ###########################
      echo jsw  Run postprocessing job
      ###########################

      cfs_postscript=${cfs_postscript:-$USHcfs/cfs_post.sh}
      $cfs_postscript $MEMBER
      rc=$?

      #############################################
      echo jsw Save the analysis files to 6hrly directory
      #############################################
      if [ $FHS -eq 0 ]; then
         $NCP $COM_YMDH/siganl.${ENS_MEM}.$YMDH     $COM_HRLY/.
         $NCP $COM_YMDH/sfcanl.${ENS_MEM}.$YMDH     $COM_HRLY/.
         $NCP $COM_YMDH/ocnanl.${ENS_MEM}.$YMDH.tar $COM_HRLY/.
         for fil in pgbanl ipvanl splanl
         do
           ifile=$COM_YMDH_DG/${fil}.${ENS_MEM}.$YMDH
           ofile=${fil}.${ENS_MEM}.${YMDH}.grb2
           $CNVGRIB -g12 -p40 $ifile $ofile
           $WGRIB2 $ofile -s >${ofile}.idx
           mv $ofile ${ofile}.idx $COM_HRLY/.
 
           if [ $SENDDBN = YES ]; then
              $DBNROOT/bin/dbn_alert MODEL CFS_FCST_ANL $job $COM_HRLY/$ofile
              $DBNROOT/bin/dbn_alert MODEL CFS_FCST_ANL_WIDX $job $COM_HRLY/${ofile}.idx
           fi
         done

         if [ $SENDDBN = YES ]; then
            $DBNROOT/bin/dbn_alert MODEL CFS_FCST_ANL $job $COM_HRLY/siganl.${ENS_MEM}.$YMDH
            $DBNROOT/bin/dbn_alert MODEL CFS_FCST_ANL $job $COM_HRLY/sfcanl.${ENS_MEM}.$YMDH
         fi

      fi

      ##################################################################
      echo  jsw Save the pgbf, ipvf, ocnf and flxf files to the 6hrly directory
      ##################################################################
      tot_err=0
      for file in $HRLY_FILES_TO_BE_KEPT ; do
        if [ $file = flxf -o $file = sigf -o $file = sfcf ] ; then
          fdir=$COM_YMDH
        else
          fdir=$COM_YMDH_DG
        fi

        start_hour=$FHS
        end_hour=$FHE

        if [ $ENS_MEM -eq 01 ]; then
           # Calculate the number of hours to keep 
            YYYYSS=$(echo $start_date | cut -c1-4)
            MMSS=$(echo $start_date | cut -c5-6)
            YYYY_KEEP=$YYYYSS
            MM_KEEP=$((MMSS+MON_TO_KEEP+1))
            while [[ $MM_KEEP -gt 12 ]] ; do
              MM_KEEP=$((MM_KEEP-12))
              YYYY_KEEP=$((YYYY_KEEP+1))
            done
           if [[ $MM_KEEP -lt 10 ]] ; then MM_KEEP=0$MM_KEEP; fi
           export end_date_KEEP=${YYYY_KEEP}${MM_KEEP}0100
           export HOUR_TO_KEEP=$($NHOUR $end_date_KEEP $start_date) 
           if [ $HOUR_TO_KEEP -ge $ENDHOUR ]; then
              export HOUR_TO_KEEP=$ENDHOUR
           fi
         else
           export HOUR_TO_KEEP=$ENDHOUR
         fi   
    
         each_err=0
         if [ $file = sigf ]; then
            export HOUR_TO_KEEP=$HOUR_TO_KEEP_SIG
         elif [ $file = sfcf ]; then
            export HOUR_TO_KEEP=$HOUR_TO_KEEP_SFC
         fi

         if [ $start_hour -le $HOUR_TO_KEEP ]; then
           if [ $file = ocnf -a $FHS -eq 0 ]; then start_hour=6; fi
           if [ $start_hour -lt 10 ]; then start_hour=0$start_hour; fi
           $USHcfs/cfs_keepfile.sh $file $start_hour $end_hour $FHOUT $fdir $each_err
         fi

         ##tot_err=`expr $tot_err + $each_err`
      done

#     clean up temporary files and stop the post job

      if [[ $FHE = $nhours ]] ; then
        if [ $rc =  0 ] ; then
          cd $DATA
          rm -rf $YMDH
        fi
        BREAK_ON_RUN=YES
        if [ ${DOALL_POSTDEFS:-NO} = YES ] ; then
          BREAK_ON_RUN=NO
        fi
      fi
      if [ $SAVE_TIME_45DAYS = YES -a $FHE -eq $FH_45DAYS ]; then

         ##############################################################
         # Run script to concatenate the time-series file and save them
         # in the "time_grib" directory for the first 45 days
         ##############################################################
         edate_45days=$($NDATE $FH_45DAYS $start_date)
         fh_inc=${INC_HOUR:-360}
         $USHcfs/cfs_savetimeser.sh $start_date $edate_45days $fh_inc
      fi

      ######################################################
      echo  jsw IF At the very end of the POST job
      ######################################################

      if [ $FHE -ge $ENDHOUR ] ; then

#       if [ ${CPY_RES_TO_COM:-YES} = YES ] ; then
#         # Copy the restart file to COM for future use
#         $NCP $RECOVERY/*$YMDH* $COM_DG/.
#       fi

         ##############################################################################################
         # Run script to concatenate the time-series file and save them in the "time_grib" directory
         ##############################################################################################
         end_date=$($NDATE $ENDHOUR $start_date)
         fh_inc=${INC_HOUR:-360}
         $USHcfs/cfs_savetimeser.sh $start_date $end_date $fh_inc

         ####################################################
         # Save the necessary files for restart in a tar file
         ####################################################
         cd $DATA
         rm -rf $DATA/LAST_RESTART
         mkdir -p $DATA/LAST_RESTART
         cp $RECOVERY/stamp.cfs $DATA/LAST_RESTART/.
         cp $RECOVERY/cfs_run.2restart $DATA/LAST_RESTART/.
         for ifil in sigr1 sigr2 sfcr fluxes_for_OM
         do
           cp $RECOVERY/${ifil}.${ENS_MEM}.${start_date}.${end_date} $DATA/LAST_RESTART/.
         done
         cp $RECOVERY/omrestart.${ENS_MEM}.${start_date}.${end_date}.tar $DATA/LAST_RESTART/.

         for ifil in sigf sfcf flxf logf
         do
           cp $COM_YMDH/${ifil}${ENDHOUR}.${ENS_MEM}.${start_date} $DATA/LAST_RESTART/.
         done

         for ifil in pgbf ipvf ocnf ocnh
         do
           cp $COM_YMDH_DG/${ifil}${ENDHOUR}.${ENS_MEM}.${start_date} $DATA/LAST_RESTART/.
         done

         # Tar up the files and save it in the 6-hrly directory
         cd $DATA/LAST_RESTART
         tar -cvf $COM_HRLY/restart.${ENS_MEM}.${start_date}.tar *
         rc=$?

         echo jsw End saving the restart files
         cd $DATA 
      fi

      ############################################################
      echo jsw rename the post define and delete them if a member run is completed
      ############################################################
      tot_rc=`expr $tot_err + $rc`
      if [ $tot_rc -eq 0 ] ; then
        mv $POSTDEFS/post_def$cyc.$postid $POSTDEFS/done$cyc.$postid
      else
        err_exit $tot_rc
      fi
#
      if [ $FHE -ge $ENDHOUR ] ; then
        
         #######################################################
         # Remove the hourly files in the COM directory
         #######################################################
         if [ $tot_rc -eq 0 ] ; then
           rm -rf $COM_YMDH
         fi

         ################################################
         # clean up temperary files and stop the post job
         ################################################
         if [ $tot_rc -eq 0 ] ; then
            cd $DATA
            rm -rf $YMDH
            if [ ${RM_POSTDEFS:-YES} = YES ] ; then
              rm -rf $POSTDEFS
#             rm $POSTDEFS/done*.*
#             rm $POSTDEFS/monthly.stamp
            fi
            if [ ${RM_RECOVERY:-YES} = YES ] ; then
              rm -rf $RECOVERY
            fi
         fi

         BREAK_ON_RUN=YES
      fi

    wait_count=0 ## use wait_count for segments
    if [ $BREAK_ON_RUN = YES ] ; then break ; fi
  done
  sleep 30
done

