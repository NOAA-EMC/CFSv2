#!/bin/ksh

set -x

####################################################################################
# Script to create HPSS archives of the CFSv2 forecast runs
# 
# Prod  - Runs once daily to archive all 16 members
#
# Incoming parameters
# --------------------
# CDATE   - yyyymmddhh to run  (hh can be anything for prod)
# ENS_MEM - the ensemble member, 01-04
# 
# Patrick Tripp EMC/NCEP - September 2010
####################################################################################

#######################################################
# Incoming parameters
#######################################################

CDATE=${1:-$CDATE}
ENS_MEM=${2:-${ENS_MEM:-1}}
workdir=${3:-$DATA}
cd $workdir

#######################################################

######################
# Scripts & Utilities
######################

hpsstar=$USHcfs/cfs_hpsstar 

PUTCMD=putv            # putv does a bit comparison - at a speed cost
PUTCMD=${PUTCMD:-put}

COMROT=${COMROT:-$COMROOT/cfs/prod}
POST_ANL=${POST_ANL:-YES}       # YES - Analysis step is done in post


#######################################################
# Determine what cycles and members to run
#######################################################

ENS_MEM=$((ENS_MEM+0))
if [ $ENS_MEM -lt 10 ] ; then ENS_MEM=0$ENS_MEM ; fi

HH=`echo $CDATE | cut -c9-10`
cycleList="$HH"
memberList="$ENS_MEM"

yyyymmddhh=$CDATE
yyyymmdd=`echo $yyyymmddhh | cut -c1-8`
echo "yyyymmdd=$yyyymmdd"

## loop for each cycle and member
for HH in $cycleList
do
  for member in $memberList
  do

    rundate=$yyyymmdd$HH

    YYYYMMDD=`echo $rundate | cut -c1-8`
    YYYY=`echo $rundate | cut -c1-4`
    MM=`echo $rundate | cut -c5-6`
    DD=`echo $rundate | cut -c7-8`

    ######################
    # Setup directories
    ######################

    TEMPDIR=$workdir/$member
    mkdir -p $TEMPDIR

    COMFCST=$COMROT/cfs/cfs.$YYYYMMDD/$HH
    COMANALYSIS=${COMANALYSIS:-$COMROT/cdas.$YYYYMMDD}

    # Monthly means
    MONTHDIR=$COMFCST/monthly_grib_$member

    # Timeseries
    TIMEDIR=$COMFCST/time_grib_$member

    # 6 hourly output
    HRLY6DIR=$COMFCST/6hrly_grib_$member

    HPSSROT=${HPSSROT:-/NCEPPROD/hpssprod/runhistory}

    # Member 1 is in permanent archive
    # Members 2,3,4 are in 2year archive
    if [[ $member == '01' ]] ; then
      #HPSSBASE=$HPSSROT/cfs$YYYY/$YYYY$MM/$rundate
      HPSSBASE=$HPSSROT/cfs$YYYY/$YYYY$MM/$YYYYMMDD
    else
      HPSSBASE=$HPSSROT/2year/cfs$YYYY/$YYYY$MM/$YYYYMMDD
    fi

    ### Determine runtype based on cycle and member

    case $member in
      01)
         runtype="9month"
         ;;
      0[2-4])
        if [[ $HH == "00" ]] ; then 
          runtype="3month"   
        else
          runtype="45day"
        fi
        ;;
      *)
        echo "ERROR: rundate HH must be 00,06,12, or 18 and member must be 01,02,03, or 04"
        exit -8
        ;;
    esac

    echo "runtype=$runtype"   # 45day, 3month, 9month  -- based on rundate and member

    ##############################################
    # Setup the parts to run, depending on runtype
    ##############################################
    # arch6hr=YES		# Archive 6 hourly data - all runtypes
    # archtime=YES		# Cat and archive timeseries - all runtypes
    # archmean=YES		# Cat and archive monthly means - 3month and 9 month
    # archICs=YES		# Archive the Initial Conditions - all runtypes
    # archRES=YES		# Archive the Restart Files - all runtypes
    ##############################################

    ##########################
    # 45 day runs
    ##########################
    if [[ $runtype == "45day" ]] ; then
      arch6hr=${arch6hr:-YES}
      archtime=${archtime:-YES}
      archmean=${archmean:-NO}
      archICs=${archICs:-YES}
      archRES=${archRES:-YES}
    
      # 45 days run to 1080 hours
      fhend45=1080
    fi


    ##########################
    # 3 month (seasonal) runs
    ##########################
    if [[ $runtype == "3month" ]] ; then
      arch6hr=${arch6hr:-YES}
      archtime=${archtime:-YES}
      archmean=${archmean:-YES}
      archICs=${archICs:-YES}
      archRES=${archRES:-YES}

      # These save all 3 months of data
      fmon6hr=3     # 6hourly saved
      fmonts=3     # timeseries and means saved, always out to 4 months

      # fhend is computed later for the 3 month and 9 month forecasts
    fi


    ##########################
    # 9 month runs
    ##########################
    if [[ $runtype == "9month" ]] ; then
      arch6hr=${arch6hr:-YES}
      archtime=${archtime:-YES}
      archmean=${archmean:-YES}
      archICs=${archICs:-YES}
      archRES=${archRES:-YES}

      # Only save the first 6 months of the 9 month 6 hourly
      fmon6hr=6     # 6hourly saved

      # timeseries and means saved out to 9 months
      fmonts=9     # goes out to 10 months

      # Added the following to account for the extra month of forecast 
      # for the last 5 days of the month
      cdatep5=`$NDATE +120 $rundate |cut -c7-8`
      if [ $cdatep5 -le 5 ]
      then
        fmon6hr=6
        fmonts=10
      fi
        
    fi

    # Track accumulated errors
    rcall=0


    ###########################################################
    # 6 HOURLY Archive
    ###########################################################
    if [[ $arch6hr == "YES" ]] ; then

      echo "BEGIN: 6HOURLY Archive, rundate=$rundate, member=$member"

      rc=0

      namelist6hr='flxf pgbf ocnf ipvf'

      sdate=$rundate

      # Calculate edate and fhend for the 3month and 9month runs
      if [[ $runtype == "3month" || $runtype == "9month" ]] ; then

        # Calclulate the end date
        yyyye=`echo $sdate | cut -c1-4`
        mme=`echo $sdate | cut -c5-6`
        mme=$(($mme + $fmon6hr + 1))
        while [[ $mme -gt 12 ]]
        do
          mme=$(($mme - 12))
          yyyye=$(($yyyye + 1))
        done
        if [[ $mme -lt 10 ]] ; then
          mme="0$mme"
        fi
        edate=${yyyye}${mme}0100
    
        # Calculate fhend
        fhend=`$NHOUR $edate $rundate`
      else
        # Must be a 45Day forecast
        fhend=$fhend45
      fi

      # Change to 6 hourly dir
      cd $HRLY6DIR

      for name in $namelist6hr
      do

        hpsslist=$TEMPDIR/$name.6hour.hpsslist
        > $hpsslist
    
        # Include the analysis step files with this archive

        if [ $name = "pgbf" ] ; then
          file=pgbanl.$member.$rundate.grb2
          ecnt=522
        fi
        if [ $name = "ipvf" ] ; then
          file=ipvanl.$member.$rundate.grb2
          ecnt=129
        fi
        if [ $name = "flxf" ] ; then
          file=splanl.$member.$rundate.grb2
          ecnt=8
        fi
        if [ $name = "ocnf" ] ; then
          file=ocnanl.$member.$rundate.tar
          ecnt=0
        fi

        # Error checking
        if [[ -s $file ]] ; then

          ls -l $file
          echo $file >> $hpsslist

          # ocnanl .tar not grib, records not checked
          if [[ $name != "ocnf" ]] ; then
            gcnt=0
            gcnt=`$WGRIB2 -s $file | grep : | wc -l`
 
            if [[ $gcnt -ne $ecnt ]] ; then
              ((rc+=1))
              echo "ERROR: $file - expected $ecnt found $gcnt records"
            fi
          fi  # ocnf

        else
          echo "ERROR: $file does not exist"
          ((rc+=1))
        fi

        # Now for the forecast files
        if [ $name = "pgbf" ] ; then
          ecnt=524
        fi
        if [ $name = "ipvf" ] ; then
          ecnt=130
        fi
        if [ $name = "flxf" ] ; then
          # 101 for 00z, 103 for others
          ecnt=103
        fi
        if [ $name = "ocnf" ] ; then
          ecnt=222
        fi

        # Forecast files
        # list each 6hour cycle, copy file and rename

        ehh=$fhend
        hh=00
        hinc=6
        while [[ $hh -le $ehh ]]
        do

          # Turn hh into a number and add leading 0 if less than 10
          ((hh+=0))
          if [ $hh -lt 10 ] ; then hh=0$hh ; fi

          # No ocnf00 or flxf00
          if [[ ( $name == 'ocnf' || $name == 'flxf' ) && $hh -eq 00 ]] ; then
            echo "skipping $name for $hh"
          else

            # Use the forecast date instead of the forecast hour in the filenames
            vdate=`$NDATE $hh $rundate`

            file=${name}$vdate.$member.$rundate.grb2

            ## Error checking 
            if [[ -s $file ]] ; then

              ls -l $file

              if [[ $name == "flxf" ]] ; then
                # 101 for 00z, 103 for others
                ecnt=103
              fi

              if [[ $name == "flxf" && $hh -eq 00 ]] ; then
                # PREScvb and PREScvt don't get written out on first cycle
                ecnt=101
              fi

              gcnt=0
              gcnt=`$WGRIB2 -s $file | grep : | wc -l`
              if [[ $gcnt -ne $ecnt ]] ; then
                ((rc+=1))
                echo "ERROR: $file - expected $ecnt found $gcnt records"
              fi

              echo $file >> $hpsslist

            else
              echo "ERROR: $file does not exist"
              ((rc+=1))
            fi
          fi # ocnf00 exception

          ((hh+=$hinc))
        done

       
        ######################
        # HPSS Section
        ######################
  
        # Put to HPSS
        HPSSDIR=$HPSSBASE/6hourly
        $hpsstar mkd $HPSSDIR

        # hpssfile=$name.$rundate.m$member.tar
        # cfs.flxf.2010013100.m01.6hrly.tar 
        hpssfile=cfs.$name.$rundate.m$member.6hourly.tar 

        $hpsstar $PUTCMD $HPSSDIR/$hpssfile `cat $hpsslist`
        if [[ $? -ne 0 ]] ; then
          ((rc+=1))
          echo "ERROR: hpsstar returned a non-zero exit status for $hpssfile"
        fi

        # Error Checking
        hpssout=$TEMPDIR/$name.$rundate.out
        hpssinvo=$TEMPDIR/$name.$rundate.inv
        $hpsstar inv $HPSSDIR/$hpssfile > $hpssout
        cat $hpssout | grep HTAR: | grep -v CF_CHK | grep -v "HTAR SUCCESSFUL" |grep -v Listing > $hpssinvo
        rm -f $hpssout

        lcnt=`cat $hpsslist | wc -l`
        hcnt=`cat $hpssinvo | grep -v Listing | wc -l`
#SH         hcnt=`cat $hpssinvo | wc -l`

        # This should ALWAYS be the same
        if [[ $lcnt -ne $hcnt ]] ; then
          echo "ERROR $rundate: local file cnt is $lcnt, hpss tar count is $hcnt"
          echo "Local filelist"
          echo "*************************"
          cat $hpsslist
          echo ""
          echo "HPSS filelist"
          echo "*************************"
          cat $hpssinvo
          ((rc+=1))
        fi

      done # namelist

      # Cleanup
#      /bin/rm -f $TEMPDIR/*

      # Track accumulated errors
      ((rcall+=$rc))

      echo "END: 6HOURLY Archive, rundate=$rundate, member=$member - rc: $rc"

    fi  # if arch6hr
    ###########################################################
    # END 6HOURLY Archive
    ###########################################################






    ###########################################################
    # TIMESERIES Archive
    ###########################################################
    if [[ $archtime == "YES" ]] ; then

      echo "BEGIN: TIMESERIES Archive, rundate=$rundate, member=$member"

      rc=0

      cd $TIMEDIR


      hpsslist=$TEMPDIR/$rundate.time.hpsslist
      > $hpsslist


      # 82 Original variables
      # namelistts='chi200 chi850 dlwsfc dswsfc gflux icecon icethk ipv450 ipv550 ipv650 lhtfl prate pressfc prmsl psi200 psi850 pwat q2m q500 q700 q850 q925 runoff shtfl snohf soilm1 soilm2 soilm3 soilm4 soilt1 weasd t2 t200 t50 t500 t700 t850 t1000 tmax tmin tmp2m tmphy1 tmpsfc ulwsfc ulwtoa uswsfc uswtoa vvel500 wnd1000 wnd10m wnd200 wnd500 wnd700 wnd850 wnd925 wndstrs z1000 z200 z500 z700 z850 ocndt20c ocnheat ocnmld ocnsal15 ocnsal5 ocnslh ocnsst ocnt15 ocnu15 ocnu5 ocnv15 ocnv5 ocnvv55 ocndt2.5c ocndt5c ocndt10c ocndt15c ocndt25c ocndt28c ocnsild ocntchp'

      ## New timeseries added
      # cprat csdlf csdsf csusf nddsf srweq t250 tcdcclm wnd250 vddsf

      # srweq is removed since WGRIB2 doesn't recognize it yet

      # 91 Selected variables
      namelistts='chi200 chi850 cprat csdlf csdsf csusf dlwsfc dswsfc gflux icecon icethk ipv450 ipv550 ipv650 lhtfl nddsf ocndt2.5c ocndt10c ocndt15c ocndt20c ocndt25c ocndt28c ocndt5c ocnheat ocnmld ocnsal15 ocnsal5 ocnsild ocnslh ocnsst ocnt15 ocntchp ocnu15 ocnu5 ocnv15 ocnv5 ocnvv55 prate pressfc prmsl psi200 psi850 pwat q2m q500 q700 q850 q925 runoff shtfl snohf soilm1 soilm2 soilm3 soilm4 soilt1 t1000 t2 t200 t250 t50 t500 t700 t850 tcdcclm tmax tmin tmp2m tmphy1 tmpsfc ulwsfc ulwtoa uswsfc uswtoa vvel500 weasd wnd1000 wnd10m wnd200 wnd250 wnd500 wnd700 wnd850 wnd925 vddsf wndstrs z1000 z200 z500 z700 z850'

      for name in $namelistts
      do

        res=f   # ocn is h

        sdate=$rundate
        if [[ $HH == "00" ]] ; then
          sdate=`$NDATE 6 $rundate`
        fi

        isocn=`echo $name | cut -c1-3`

        if [[ $isocn == "ocn"  ]] ; then
          # Comment out by Julia on 20110207
          #sdate=`$NDATE 6 $rundate`
          res=h
        fi

        if [[ $runtype == "3month" || $runtype == "9month" ]] ; then

          # Calclulate the end date
          yyyye=`echo $sdate | cut -c1-4`
          mme=`echo $sdate | cut -c5-6`
  
          mme=$(($mme + $fmonts + 1))
          while [[ $mme -gt 12 ]]
          do
            mme=$(($mme - 12))
            yyyye=$(($yyyye + 1))
          done
          if [[ $mme -lt 10 ]] ; then
            mme="0$mme"
          fi
          edate=${yyyye}${mme}0100

          # Calculate fhend
          fhend=`$NHOUR $edate $rundate`
        else

          ## Must be a 45Day run
          fhend=$fhend45
          edate=`$NDATE $fhend $rundate`
        fi

        file=$name.$member.$rundate.daily.grb2

        # Build the HPSS List
        echo $file >> $hpsslist

        # Calculate expected number of records
        nrec=`expr $fhend / 6`

        iswnd=`echo $name | cut -c1-3`
        if [[ $iswnd == "wnd" ]] ; then
          nrec=$(($nrec*2))
        fi

        # expected record cnt based on num of hours
        excnt=$nrec

        # Error checking
        if [[ -s $file ]] ; then
          ls $file

          rcnt=0
          rcnt=`$WGRIB2 -s $file | grep : | wc -l`

          if [[ $rcnt -ne $excnt ]] ; then
            echo "ERROR: expected $excnt records, found $rcnt in $file"
            ((rc+=1))
          fi

        else
          echo "ERROR: $file does not exist"
          ((rc+=1))
        fi

      done  # namelistts

      ################
      # HPSS Section
      ################

      cat $hpsslist

      HPSSDIR=$HPSSBASE/time
      $hpsstar mkd $HPSSDIR

      hpssfile=cfs.$rundate.m$member.time.tar

      $hpsstar $PUTCMD $HPSSDIR/$hpssfile `cat $hpsslist`
      if [[ $? -ne 0 ]] ; then
        ((rc+=1))
        echo "ERROR: hpsstar returned a non-zero exit status for $hpssfile"
      fi

      # Error Checking
      hpssout=$TEMPDIR/cfs.$member.$rundate.time.out
      hpssinvo=$TEMPDIR/cfs.$member.$rundate.time.inv
      $hpsstar inv $HPSSDIR/$hpssfile > $hpssout
      #cat $hpssout | grep $rundate | grep $name > $hpssinvo
      cat $hpssout | grep $rundate | grep -v Listing > $hpssinvo
      rm -f $hpssout

      lcnt=`cat $hpsslist | wc -l`
      hcnt=`cat $hpssinvo | grep -v Listing | wc -l`
#SH      hcnt=`cat $hpssinvo | wc -l`

      # This should ALWAYS be the same
      if [[ $lcnt -ne $hcnt ]] ; then
        echo "ERROR $rundate: local file cnt is $lcnt, hpss tar count is $hcnt"
        echo "Local filelist"
        echo "*************************"
        cat $hpsslist
        echo ""
        echo "HPSS filelist"
        echo "*************************"
        cat $hpssinvo
        ((rc+=1))
      fi

      # Track accumulated errors
      ((rcall+=$rc))

      echo "END: TIMESERIES Archive, rundate=$rundate, member=$member - rc: $rc"

    fi  # if archtime == YES
    ###########################################################
    # END TIMESERIES Archive
    ###########################################################




    ###########################################################
    # MONTHLY MEANS Archive
    ###########################################################
    if [[ $archmean == "YES" ]] ; then

      echo "BEGIN: MONTHLY MEANS Archive, rundate=$rundate, member=$member"
      rc=0
    
      hhlist='00 06 12 18'
      export namelist='pgbf ipvf flxf ocnf ocnh'

      cd $MONTHDIR

      mmend=`expr $MM + $fmonts`
      yyyyend=$YYYY

      while [[ $mmend -gt 12 ]]
      do
        mmend=`expr $mmend - 12`
        yyyyend=$(($yyyyend + 1))
      done

      if [[ $mmend -le 9 ]] ; then
        mmend=0$mmend
      fi

      edate_end=${yyyyend}${mmend}0100

      echo "yyyyend: $yyyyend, mmend: $mmend"
      echo "edate_end: $edate_end"

      for name in $namelist
      do

        hpsslist=$TEMPDIR/$name.monthly.hpsslist
        > $hpsslist

        shrtname=`echo $name | cut -c1-3`

        if [[ $shrtname == "flx" ]] ; then
          excnt=103
        fi
        if [[ $shrtname == "pgb" ]] ; then
          excnt=524
        fi
        if [[ $shrtname == "ocn" ]] ; then
          excnt=222
        fi
        if [[ $shrtname == "ipv" ]] ; then
          excnt=130
        fi

        date=$rundate

        # Create the file list and error check
        while [[ $date -le $edate_end ]] ; do

          yyyy=`echo $date | cut -c1-4`
          mm=`echo $date | cut -c5-6`

          # Don't include first month means if DD >= 25
          if [[ $DD -ge 25 && $MM -eq $mm ]] ; then
            echo "Skipping means for $yyyy $mm"
          else

            # The daily average mean
            file=${name}.$member.$rundate.$yyyy$mm.avrg.grib.grb2

            if [[ -s $file ]] ; then

              ls -l $file
              echo $file >> $hpsslist

              # Check record count
              rcnt=0
              rcnt=`$WGRIB2 -s $file | grep : | wc -l`

              if [[ $rcnt -ne $excnt ]] ; then
                echo "ERROR: expected $excnt records, found $rcnt in $file"
                ((rc+=1))
              fi

            else
              echo "ERROR: $file does not exist"
              ((rc+=1))
            fi

            # The diurnal means (00Z, 06Z, 12Z, 18Z)
            for hh in $hhlist
            do
              file=${name}.$member.$rundate.${yyyy}$mm.avrg.grib.${hh}Z.grb2

              if [[ -s $file ]] ; then
                ls -l $file
                echo $file >> $hpsslist

                rcnt=0
                rcnt=`$WGRIB2 -s $file | grep : | wc -l`

                if [[ $rcnt -ne $excnt ]] ; then
                  echo "ERROR: expected $excnt records, found $rcnt in $file"
                  ((rc+=1))
                fi
    
              else
                echo "ERROR: $file does not exist"
                ((rc+=1))
              fi
            done  #HHZ
          fi  # if not first month of a >= 25th dom inidate

          # Calculate the next month
          xmm=$(($mm + 1))
          xyyyy=$yyyy
  
          if [[ $xmm -gt 12 ]] ; then
            xmm=$(($xmm - 12))
            xyyyy=$(($xyyyy + 1))
          fi
          if [[ $xmm -lt 10 ]] ; then
            xmm=0$xmm
          fi

          date=${xyyyy}${xmm}0100

        done  # 

        ################
        # HPSS Section
        ################

        HPSSDIR=/$HPSSBASE/monthly
        $hpsstar mkd $HPSSDIR

        # Change this as needed in operations
        hpssfile=cfs.$name.$rundate.m$member.monthly.tar

        cat "$hpsslist"

        $hpsstar $PUTCMD $HPSSDIR/$hpssfile `cat $hpsslist`
        if [[ $? -ne 0 ]] ; then
          echo "ERROR: hpsstar returned a non-zero exit for $hpssfile"
          ((rc+=1))
        fi

        # Error Checking
        hpssout=$TEMPDIR/cfs.$name.$member.$rundate.monthly.out
        hpssinvo=$TEMPDIR/cfs.$name.$member.$rundate.monthly.inv
        $hpsstar inv $HPSSDIR/$hpssfile > $hpssout
        cat $hpssout | grep $rundate | grep $name | grep -v Listing > $hpssinvo
        rm -f $hpssout

        lcnt=`cat $hpsslist | wc -l`
        hcnt=`cat $hpssinvo | grep -v Listing | wc -l`
#SH       hcnt=`cat $hpssinvo | wc -l`

        # This should ALWAYS be the same
        # This should ALWAYS be the same
        if [[ $lcnt -ne $hcnt ]] ; then
          echo "ERROR $rundate: local file cnt is $lcnt, hpss tar count is $hcnt"
          echo "Local filelist"
          echo "*************************"
          cat $hpsslist
          echo ""
          echo "HPSS filelist"
          echo "*************************"
          cat $hpssinvo
          ((rc+=1))
        fi

      done  # namelist

      # Clean up
      cd $COMROT
#      /bin/rm -f $TEMPDIR/*

      # Track accumulated errors
      ((rcall+=$rc))

      echo "END: MONTHLY MEANS Archive, rundate=$rundate, member=$member - rc: $rc"

    fi  # if archmean == YES
    ###########################################################
    # END MONTHLY MEANS Archive
    ###########################################################



    ###########################################################
    # ICs Archive
    ###########################################################
    if [[ $archICs == "YES" ]] ; then

      echo "BEGIN: ICs Archive, rundate=$rundate, member=$member"
      rc=0

      name=ICs

      rstlist='siganl sfcanl ocnanl'    # The main Initial Condition files
      anlist='ipvanl splanl pgbanl'     # The analysis step files

      hpsslist=$TEMPDIR/$name.$rundate.hpsslist
      > $hpsslist

      cd $HRLY6DIR

      for type in $rstlist
      do

        if [ $type = "ocnanl" ] ; then
          file=$type.$member.$rundate.tar
        else
          file=$type.$member.$rundate
        fi
   
        if [[ -s $file ]] ; then
          /bin/ls $file >> $hpsslist
        else
          echo "ERROR: $file does not exist"
          ((rc+=1))
        fi
      done

      for type in $anlist
      do
        file=$type.$member.$rundate.grb2

        if [[ -s $file ]] ; then
          /bin/ls $file >> $hpsslist
        else
          echo "ERROR: $file does not exist"
          ((rc+=1))
        fi
      done


      ######################
      # HPSS Section
      ######################

      # Put to HPSS
      HPSSDIR=$HPSSBASE/ICs
      $hpsstar mkd $HPSSDIR

      hpssfile=cfs.$rundate.m$member.$name.tar

      $hpsstar $PUTCMD $HPSSDIR/$hpssfile `cat $hpsslist`
      if [[ $? -ne 0 ]] ; then
        ((rc+=1))
        echo "ERROR: hpsstar returned a non-zero exit status for $hpssfile"
      fi

      # Error Checking
      hpssout=$TEMPDIR/$name.$rundate.out
      hpssinvo=$TEMPDIR/$name.$rundate.inv
      $hpsstar inv $HPSSDIR/$hpssfile > $hpssout
      cat $hpssout | grep HTAR: | grep -v CF_CHK | grep -v "HTAR SUCCESSFUL" |grep -v Listing > $hpssinvo
      rm -f $hpssout

      lcnt=`cat $hpsslist | wc -l`
      hcnt=`cat $hpssinvo | grep -v Listing | wc -l`
#SH      hcnt=`cat $hpssinvo | wc -l`

      # This should ALWAYS be the same
      if [[ $lcnt -ne $hcnt ]] ; then
        echo "ERROR $rundate: local file cnt is $lcnt, hpss tar count is $hcnt"
        echo "Local filelist"
        echo "*************************"
        cat $hpsslist
        echo ""
        echo "HPSS filelist"
        echo "*************************"
        cat $hpssinvo
        ((rc+=1))
      fi

      # Clean up
      cd $COMROT
     # rm -Rf $TEMPDIR

      # Track accumulated errors
      ((rcall+=$rc))

      echo "END: ICs Archive, rundate=$rundate, member=$member - rc: $rc"

    fi   # archICs
    ###########################################################
    # END ICs Archive
    ###########################################################

   # /bin/rm -f $TEMPDIR/*

    ###########################################################
    # RESTART Archive
    ###########################################################
    if [[ $archRES == "YES" ]] ; then

      echo "BEGIN: RESTART Archive, rundate=$rundate, member=$member"
      rc=0

      name=RESTART

      hpsslist=$TEMPDIR/$name.$rundate.hpsslist
      > $hpsslist

      cd $HRLY6DIR

      file=restart.$member.$rundate.tar
   
      if [[ -s $file ]] ; then
        /bin/ls $file >> $hpsslist
      else
        echo "ERROR: $file does not exist"
        ((rc+=1))
      fi

      ######################
      # HPSS Section
      ######################

      # Put to HPSS
      HPSSDIR=$HPSSBASE/RESTART
      $hpsstar mkd $HPSSDIR

      hpssfile=cfs.$rundate.m$member.$name.tar

      $hpsstar $PUTCMD $HPSSDIR/$hpssfile `cat $hpsslist`
      if [[ $? -ne 0 ]] ; then
        ((rc+=1))
        echo "ERROR: hpsstar returned a non-zero exit status for $hpssfile"
      fi

      # Error Checking
      hpssout=$TEMPDIR/$name.$rundate.out
      hpssinvo=$TEMPDIR/$name.$rundate.inv
      $hpsstar inv $HPSSDIR/$hpssfile > $hpssout
      #XXW cat $hpssout | grep HTAR | grep -v CF_CHK | grep -v "HTAR SUCCESSFUL" > $hpssinvo
      cat $hpssout | grep "HTAR:" | grep -v CF_CHK | grep -v "HTAR SUCCESSFUL" |grep -v Listing > $hpssinvo
      rm -f $hpssout

      lcnt=`cat $hpsslist | wc -l`
      hcnt=`cat $hpssinvo | grep -v Listing | wc -l`
#SH      hcnt=`cat $hpssinvo | wc -l`

      # This should ALWAYS be the same
      if [[ $lcnt -ne $hcnt ]] ; then
        echo "ERROR $rundate: local file cnt is $lcnt, hpss tar count is $hcnt"
        echo "Local filelist"
        echo "*************************"
        cat $hpsslist
        echo ""
        echo "HPSS filelist"
        echo "*************************"
        cat $hpssinvo
        ((rc+=1))
      fi

      # Clean up
      cd $COMROT
     # rm -Rf $TEMPDIR

      # Track accumulated errors
      ((rcall+=$rc))

      echo "END: RESTART Archive, rundate=$rundate, member=$member - rc: $rc"

    fi   # archRES

  done #  member in memberList
done #  hh in cycleList

## Cleanup

#/bin/rm -Rf $TEMPDIR
export err=$rcall; err_chk

exit $rcall
