########################### EXRTGSST.SH.SMS ###############################
set +xa
echo "---------------------------------------------------------------"
echo "JRTGSST - RTGSSTHR and ETA processing"
echo "---------------------------------------------------------------"
echo "History: Tuesday, May 04, 1993 - Implement SSTOI Analysis"
echo "Revised: Tuesday, August 29, 1995 - convert from CSS to FTA"
echo "Revised: Tue October 3, 1995 - added operational ETA SST script"
echo "Revised: Mon, 22 September, 1996 - Add OI SST processing."
echo "Revised: Wed, 26 October, 1996 - Add Satellite SST to OI processing."
echo "Revised: Tue, 18 March, 1997 - Add rmcomdir step"
echo "Revised: Tue, 17 June, 1997 - Add step to submit mkfossst.sh."
echo "Revised: 02/08/2000 - Initial implementation on the IBM SP "
echo "                      ETA SST script now in separate job"
echo "Revised: 06/12/2000 - Modified to become RTGSSTHR analysis "
echo "         17 Jan 2001 - placed into parallel production."
echo "Revised: Mar 2012 - Chmod 640 ship files."
echo "                    Remove chmod 775 $COMOUT/* to protect restricted files."
echo "---------------------------------------------------------------"
set -xa

###########################################################
# Processing/Flow of script
# 1) Copy this job's input files to the working directory
# 2) Run steps required for data preprocessing and  OI sst analysis
# 3) Save output files to $COMOUT
# 4) Interpolate 0.5-degree grid to 2.5 nh and sh degree grids
# 5) Remove files located in this job's working directory
###########################################################

#############################################################
# Ensure that this job is pointed to the production temporary
# working directory, which is where all of this job's input 
# and output files will be saved during execution
#############################################################
cd $DATA

msg="HAS BEGUN on `hostname`"
postmsg "$jlogfile" "$msg"

set -x

# MPI debugging variables.  Don't always need.
export MP_COREFILE_FORMAT=lite

#############################################################
# cp RTG_SST non-satellite input files from prior 6 days to 
# working directory checking yesterdays directory first, 
# check others if necessary
#############################################################
if [[ $envir = prod || $envir = para || $envir = test || $RUNTYPE = quasi_prod ]] ; then

  for CDATE in $PDYm6 $PDYm5 $PDYm4 $PDYm3 $PDYm2 $PDYm1 
  do
    for CTYPE in ships dbuoy fbuoy seaice.grb satamsre satgoes 
    do
       for SDIR in $COMINm1 $COMINm2 $COMINm3 $COMINm4 $COMINm5 $COMINm6
       do
          if [ -s $SDIR/rtgssthr_$CTYPE.$CDATE ]
          then
             cp $SDIR/rtgssthr_$CTYPE.$CDATE .
             break
          fi
       done
    done
  done

  for satid in mta n19
  do
    for CDATE in $PDYm6 $PDYm5 $PDYm4 $PDYm3 $PDYm2 $PDYm1
    do
      for retr in phys navy merg 
      do
        for CTYPE in dsat$satid nsat$satid 
        do
          for SDIR in $COMINm1 $COMINm2 $COMINm3 $COMINm4 $COMINm5 $COMINm6
          do
             if [ -s $SDIR/rtgssthr_${CTYPE}_$retr.$CDATE ]
             then
                cp $SDIR/rtgssthr_${CTYPE}_$retr.$CDATE .
                break
             fi
          done
        done
      done
    done
  done

else

  proddatalisttrim=${proddatalist/satphyret}
  proddatalisttrim=${proddatalisttrim/satavhrr}
  if [[ -n "$proddatalisttrim" ]] ; then
    for CDATE in $PDYm6 $PDYm5 $PDYm4 $PDYm3 $PDYm2 $PDYm1 $PDY
    do
      for CTYPE in $proddatalisttrim
      do
        for SDIR in $PRODCOPYIN $PRODCOPYINm1 $PRODCOPYINm2 $PRODCOPYINm3 $PRODCOPYINm4 $PRODCOPYINm5 $PRODCOPYINm6
        do
           if [ -s $SDIR/rtgssthr_$CTYPE.$CDATE ]
           then
              cp $SDIR/rtgssthr_$CTYPE.$CDATE .
              break
           fi
        done
      done
    done
  fi

  exprdatalisttrim=${exprdatalist/satphyret}
  exprdatalisttrim=${exprdatalisttrim/satavhrr}
  if [[ -n "$exprdatalisttrim" ]] ; then
    for CDATE in $PDYm6 $PDYm5 $PDYm4 $PDYm3 $PDYm2 $PDYm1 $PDY
    do
      for CTYPE in $exprdatalisttrim
      do
        for SDIR in $COMIN $COMINm1 $COMINm2 $COMINm3 $COMINm4 $COMINm5 $COMINm6
        do
           if [ -s $SDIR/rtgssthr_$CTYPE.$CDATE ]
           then
              cp $SDIR/rtgssthr_$CTYPE.$CDATE .
              break
           fi
        done
      done
    done
  fi

  echo "$proddatalist" | grep satavhrr
  rcprodavhrr=$?

  echo "$exprdatalist" | grep satavhrr
  rcexpravhrr=$?

  if (( $rcprodavhrr == 0 && $rcexpravhrr != 0 )) ; then
    for satid in mta n19
    do
      for CDATE in $PDYm6 $PDYm5 $PDYm4 $PDYm3 $PDYm2 $PDYm1 $PDY
      do
        for CTYPE in dsat$satid nsat$satid 
        do
          for SDIR in $PRODCOPYIN $PRODCOPYINm1 $PRODCOPYINm2 $PRODCOPYINm3 $PRODCOPYINm4 $PRODCOPYINm5 $PRODCOPYINm6
          do
             if [ -s $SDIR/rtgssthr_${CTYPE}_navy.$CDATE ]
             then
                cp $SDIR/rtgssthr_${CTYPE}_navy.$CDATE .
                break
             fi
          done
        done
      done
    done
  fi

  if (( $rcprodavhrr != 0 && $rcexpravhrr == 0 )) ; then
    for satid in mta n19
    do
      for CDATE in $PDYm6 $PDYm5 $PDYm4 $PDYm3 $PDYm2 $PDYm1 $PDY
      do
        for CTYPE in dsat$satid nsat$satid 
        do
          for SDIR in $COMIN $COMINm1 $COMINm2 $COMINm3 $COMINm4 $COMINm5 $COMINm6
          do
             if [ -s $SDIR/rtgssthr_${CTYPE}_navy.$CDATE ]
             then
                cp $SDIR/rtgssthr_${CTYPE}_navy.$CDATE .
                break
             fi
          done
        done
      done
    done
  fi

  echo "$proddatalist" | grep satphyret
  rcprodphyret=$?

  echo "$exprdatalist" | grep satphyret
  rcexprphyret=$?

  if (( $rcprodphyret == 0 && $rcexprphyret != 0 )) ; then
    for satid in mta n19
    do
      for CDATE in $PDYm6 $PDYm5 $PDYm4 $PDYm3 $PDYm2 $PDYm1 $PDY
      do
        for retr in phys merg 
        do
          for CTYPE in dsat$satid nsat$satid 
          do
            for SDIR in $PRODCOPYIN $PRODCOPYINm1 $PRODCOPYINm2 $PRODCOPYINm3 $PRODCOPYINm4 $PRODCOPYINm5 $PRODCOPYINm6
            do
               if [ -s $SDIR/rtgssthr_${CTYPE}_$retr.$CDATE ]
               then
                  cp $SDIR/rtgssthr_${CTYPE}_$retr.$CDATE .
                  break
               fi
            done
          done
        done
      done
    done
  fi

  if (( $rcprodphyret != 0 && $rcexprphyret == 0 )) ; then
    for satid in mta n19
    do
      for CDATE in $PDYm6 $PDYm5 $PDYm4 $PDYm3 $PDYm2 $PDYm1 $PDY
      do
        for retr in phys merg 
        do
          for CTYPE in dsat$satid nsat$satid 
          do
            for SDIR in $COMIN $COMINm1 $COMINm2 $COMINm3 $COMINm4 $COMINm5 $COMINm6
            do
               if [ -s $SDIR/rtgssthr_${CTYPE}_$retr.$CDATE ]
               then
                  cp $SDIR/rtgssthr_${CTYPE}_$retr.$CDATE .
                  break
               fi
            done
          done
        done
      done
    done
  fi

fi
#
# Dump insitu files that were not available in previous runs
#
export DUMP=${USHobsproc}/dumpjb

export CHKPRT=true
export FORM=system
export LOUD=off

#  For each day, check for file for each type (ships, dbuoy, fbuoy)
# dump and process if missing

####DUMPTIME=2221

for CDATE in $PDYm6 $PDYm5 $PDYm4 $PDYm3 $PDYm2 $PDYm1 $PDY
do
  for CTYPE in ships dbuoy fbuoy
  do
    #  if file does not exist or has size of zero, get bufr'd data.
    #  always get bufr'd data for yesterday to pick up late obs.
    if [ $envir = prod -o $envir = para -o $envir = test -o $RUNTYPE = quasi_prod ] ; then
      if [ $CDATE != $PDY ] ; then
        continue
      fi
    else
      if [ -s rtgssthr_$CTYPE.$CDATE ] ; then
        continue
      fi
    fi

    if [ $CTYPE = fbuoy ] 
    then
      $DUMP $CDATE\04.35 17.99999 mbuoy
      dumpstat1=$?
      $DUMP $CDATE\04.35 17.99999 lcman
      dumpstat2=$?
      if [ $dumpstat1 -eq 0 -a $dumpstat2 -eq 0 ] ; then
#  merge moored buoys and CMANs into a "fixed buoy" file
        export pgm=bufr_combfr
        . prep_step 
        export FORT50="fbuoy.system"
        echo -e "mbuoy.system \nlcman.system"|${EXECobsproc}/bufr_combfr
        errcomb=$?
        if [ $errcomb -ne 0 ] ; then
          cp mbuoy.system fbuoy.system
        fi
      elif [ $dumpstat1 -eq 0 ] ; then
        cp mbuoy.system fbuoy.system
      elif [ $dumpstat2 -eq 0 ] ; then
        cp lcman.system fbuoy.system
      fi
      dumpstat=$(($dumpstat1*$dumpstat2))
    elif [ $CTYPE = ships ]
    then
      $DUMP $CDATE\04.35 17.99999 shpall
      dumpstat=$?
      if [ $dumpstat -eq 0 -o $dumpstat -eq 11 ] ; then
        mv shpall.system ships.system
        dumpstat=0
      else
        $DUMP $CDATE\04.35 17.99999 ships
        dumpstat=$?
      fi
    else
      $DUMP $CDATE\04.35 17.99999 $CTYPE
      dumpstat=$?
    fi

    if [ $dumpstat -ne 0 ]
    then
      msg="data dump failed: $dumpstat"
      postmsg "$jlogfile" "$msg"
    else
      export pgm=rtgssthr_shp
      . prep_step
    
#  run rtgssthr_shp on dumped bufr data
      echo $CDATE $CTYPE > rtgsstshp.input
      echo $CHKPRT     >> rtgsstshp.input
    
      export FORT10="$CTYPE.system"
      export FORT31="$FIXrtgsst/rtgssthr_ls_nas.twelfthdeg.dat"
      CDATEm1=$($USHutil/finddate.sh $CDATE d-1)
      export FORT41="rtgssthr_$CTYPE.$CDATEm1"
      export FORT51="rtgssthr_$CTYPE.$CDATE"
    
      startmsg
      $EXECrtgsst/rtgssthr_shp < rtgsstshp.input >> $pgmout 2>errfile
      export err=$?;$USHrtgsst/rtgssthr_errwarn.sh;export err=$?
      err_chk
    fi
  done
done

#  save insitu files for tomorrows run
if test "$SENDCOM" = 'YES'
then
  for CDATE in $PDYm6 $PDYm5 $PDYm4 $PDYm3 $PDYm2 $PDYm1 $PDY
  do
    for CTYPE in ships dbuoy fbuoy
    do
      cp rtgssthr_$CTYPE.$CDATE $COMOUT/rtgssthr_$CTYPE.$CDATE
      if [ $CTYPE = ships ] ; then
        chgrp rstprod $COMOUT/rtgssthr_$CTYPE.$CDATE
        chmod 640 $COMOUT/rtgssthr_$CTYPE.$CDATE
      fi
    done
  done
fi

for CDATE in $PDYm6 $PDYm5 $PDYm4 $PDYm3 $PDYm2 $PDYm1 $PDY
do
# For each day, check for file
#  if file does not exist or has size of zero, get bufr'd data.
#  always get bufr'd data for yesterday to pick up late obs.
  if [ $envir = prod -o $envir = para -o $envir = test -o $RUNTYPE = quasi_prod ] ; then
    if [ $CDATE != $PDY ] ; then
      continue
    fi
  else
    if  [ -s rtgssthr_satamsre.$CDATE ] ; then
      continue
    fi
  fi

  $DUMP $CDATE\04.35 17.99999 amsrem 
  dumpstat=$?

  if [ $dumpstat -ne 0 ]
  then
    msg="data dump failed: $dumpstat"
    postmsg "$jlogfile" "$msg"
  else
    export pgm=rtgssthr_sat_amsre
    . prep_step

    echo $CDATE amsr-e > rtgsstamsre.input
    echo $CHKPRT     >> rtgsstamsre.input

    export FORT21="amsrem.system"
    export FORT23="$FIXrtgsst/rtgssthr_aoi.61.90.clim.halfdeg"
    export FORT24="$FIXrtgsst/rtgssthr_stdev2d"
    export FORT31="$FIXrtgsst/rtgssthr_ls_nas.twelfthdeg.dat"
    CDATEm1=$($USHutil/finddate.sh $CDATE d-1)
    export FORT41="rtgssthr_satamsre.$CDATEm1"
    export FORT51="rtgssthr_satamsre.$CDATE"

    startmsg
    $EXECrtgsst/rtgssthr_sat_amsre < rtgsstamsre.input >>$pgmout 2>&1
    export err=$?;$USHrtgsst/rtgssthr_errwarn.sh;export err=$?
    err_chk
  fi
done

for CDATE in $PDYm6 $PDYm5 $PDYm4 $PDYm3 $PDYm2 $PDYm1 $PDY
do
# For each day, check for file
#  if file does not exist or has size of zero, get bufr'd data.
#  always get bufr'd data for yesterday to pick up late obs.
  if [ $envir = prod -o $envir = para -o $envir = test -o $RUNTYPE = quasi_prod ] ; then
    if [ $CDATE != $PDY ] ; then
      continue 
    fi
  else
    if  [ -s rtgssthr_satgoes.$CDATE ] ; then
      continue 
    fi
  fi

  CDATEGOES=$($USHutil/finddate.sh $CDATE d-1)
  $DUMP $CDATEGOES\00.00 11.99999 sstnsg 
  dumpstat=$?

  if [ $dumpstat -ne 0 ]
  then
    msg="data dump failed: $dumpstat"
    postmsg "$jlogfile" "$msg"
  else
    export pgm=rtgssthr_sat_goes
    . prep_step

    echo $CDATEGOES goes > rtgsstgoes.input
    echo $CHKPRT     >> rtgsstgoes.input

    export FORT21="sstnsg.system"
    export FORT23="$FIXrtgsst/rtgssthr_aoi.61.90.clim.halfdeg"
    export FORT24="$FIXrtgsst/rtgssthr_stdev2d"
    export FORT31="$FIXrtgsst/rtgssthr_ls_nas.twelfthdeg.dat"
    CDATEm1=$($USHutil/finddate.sh $CDATE d-1)
    export FORT41="rtgssthr_satgoes.$CDATEm1"
    export FORT51="rtgssthr_satgoes.$CDATE"

    startmsg
    $EXECrtgsst/rtgssthr_sat_goes < rtgsstgoes.input >>$pgmout 2>&1
    export err=$?;$USHrtgsst/rtgssthr_errwarn.sh;export err=$?
    err_chk
  fi

done

##  For each day, check for file for satellite data
# dump and process if missing
#-----------------------------------------------------------------
export DUMP=${USHobsproc}/dumpjb

export FORM=system
export LOUD=off
export CHKPRT=true
#-----------------------------------------------------------------

for CDATE in $PDYm6 $PDYm5 $PDYm4 $PDYm3 $PDYm2 $PDYm1 $PDY
do
  for CTYPE in sstnvp sstnvh ; do
    if [ $CTYPE = sstnvp ] ; then
      retr=phys
    elif [ $CTYPE = sstnvh ] ; then
      retr=navy
    else
      echo "Illegal retrieval type $CTYPE . "
      continue
    fi
    for satid in mta n19 
    do

# For each day, check for file
#  if file does not exist or has size of zero, get bufr'd data.
#  always get bufr'd data for yesterday to pick up late obs.
      if [ $envir = prod -o $envir = para -o $envir = test -o $RUNTYPE = quasi_prod ] ; then
        if [ $CDATE != $PDY ] ; then
          continue
        fi
      else
        if  [ -s rtgssthr_dsat${satid}_$retr.$CDATE -o -s rtgssthr_nsat${satid}_$retr.$CDATE ] ; then
          continue
        fi
      fi

      $DUMP $CDATE\04.35 17.99999 $CTYPE
      dumpstat=$?

      if [ $dumpstat -ne 0 ]
      then
        msg="data dump failed: $dumpstat"
        postmsg "$jlogfile" "$msg"
      else

        pgm=rtgssthr_satsplit
        export pgm
        . prep_step

        export FORT11=$CTYPE.system
        export FORT51=avhrrn19
        export FORT52=avhrrn17
        export FORT53=avhrrn18
        export FORT54=avhrrmta
        $EXECrtgsst/rtgssthr_satsplit
        errsplit=$?
        export err=$?;$USHrtgsst/rtgssthr_errwarn.sh;export err=$?
        err_chk
        if [ $errsplit -eq 0 ]; then
          echo " --------------------------------------------- "
          echo " ********** COMPLETED PROGRAM rtgssthr_satsplit **********"
          echo " --------------------------------------------- "
          msg="$pgm has split $retr ret dump into N-19, -17, -18 & METOP-A files"
          postmsg "$jlogfile" "$msg"
          for procsatid in mta n19 ; do
            export pgm=rtgssthr_sat
            . prep_step

            echo $CDATE phyret > rtgsstsat.input
            echo $CHKPRT     >> rtgsstsat.input

            export FORT21="avhrr$procsatid"
            export FORT23="$FIXrtgsst/rtgssthr_aoi.61.90.clim.halfdeg"
            export FORT24="$FIXrtgsst/rtgssthr_stdev2d"
            export FORT31="$FIXrtgsst/rtgssthr_ls_nas.twelfthdeg.dat"
            CDATEm1=$($USHutil/finddate.sh $CDATE d-1)
            export FORT41="rtgssthr_dsat${procsatid}_$retr.$CDATEm1"
            export FORT42="rtgssthr_nsat${procsatid}_$retr.$CDATEm1"
            export FORT51="rtgssthr_dsat${procsatid}_$retr.$CDATE"
            export FORT52="rtgssthr_nsat${procsatid}_$retr.$CDATE"

            startmsg
            $EXECrtgsst/rtgssthr_sat < rtgsstsat.input >>$pgmout 2>&1
            export err=$?;$USHrtgsst/rtgssthr_errwarn.sh;export err=$?
            err_chk
          done
        else
          echo "*******************************************************"
          echo "********  ERROR PROGRAM rtgssthr_satsplit RETURN CODE $err ********"
          echo "*******************************************************"
          msg="ERROR PROGRAM rtgssthr_satsplit RETURN CODE $err"
          postmsg "$jlogfile" "$msg"
        fi
        break
      fi
    done
  done

  for satid in mta n19 
  do
    if [ ! -s rtgssthr_dsat${satid}_merg.$CDATE -o ! -s rtgssthr_nsat${satid}_merg.$CDATE ] ; then
      export pgm=rtgssthr_satmerge
      . prep_step

      echo "4320 2160 0.08333333333333333 0.08333333333333333 (format free)" > satmerge.input

      export FORT11="rtgssthr_dsat${satid}_navy.$CDATE"
      export FORT12="rtgssthr_nsat${satid}_navy.$CDATE"
      export FORT21="rtgssthr_dsat${satid}_phys.$CDATE"
      export FORT22="rtgssthr_nsat${satid}_phys.$CDATE"
      export FORT31="$FIXrtgsst/rtgssthr_dist2land.bin"
      export FORT51="rtgssthr_dsat${satid}_merg.$CDATE"
      export FORT52="rtgssthr_nsat${satid}_merg.$CDATE"
      export FORT61="rtgssthr_dsat${satid}_nuse.$CDATE"
      export FORT62="rtgssthr_nsat${satid}_nuse.$CDATE"
  
      $EXECrtgsst/rtgssthr_satmerge < satmerge.input >>$pgmout 2>&1
      export err=$?
      if [[ -e rtgssthr_nsat${satid}_nuse.$CDATE ]] ; then
        mv rtgssthr_nsat${satid}_nuse.$CDATE rtgssthr_nsat${satid}_navy.$CDATE
      fi
      if [[ -e rtgssthr_dsat${satid}_nuse.$CDATE ]] ; then
        mv rtgssthr_dsat${satid}_nuse.$CDATE rtgssthr_dsat${satid}_navy.$CDATE
      fi
      $USHrtgsst/rtgssthr_errwarn.sh;export err=$?
      err_chk

    fi

  done
done

#  save satellite archives for tomorrows run
if test "$SENDCOM" = 'YES'
then
  for satid in mta n19
  do
    for CDATE in $PDYm6 $PDYm5 $PDYm4 $PDYm3 $PDYm2 $PDYm1 $PDY
    do
      for retr in phys navy merg
      do

        cp rtgssthr_dsat${satid}_$retr.$CDATE $COMOUT/rtgssthr_dsat${satid}_$retr.$CDATE
        cp rtgssthr_nsat${satid}_$retr.$CDATE $COMOUT/rtgssthr_nsat${satid}_$retr.$CDATE
      done
    done
  done
  for CDATE in $PDYm6 $PDYm5 $PDYm4 $PDYm3 $PDYm2 $PDYm1 $PDY
  do
    cp rtgssthr_satamsre.$CDATE $COMOUT/rtgssthr_satamsre.$CDATE
    cp rtgssthr_satgoes.$CDATE $COMOUT/rtgssthr_satgoes.$CDATE
  done
fi

##  quality control ship and buoy data
export pgm=rtgssthr_insdt
. prep_step

export FORT11="$FIXrtgsst/rtgssthr_aoi.61.90.clim.halfdeg"
export FORT12="$FIXrtgsst/rtgssthr_stdev2d"
export FORT21="rtgssthr_ships.$PDYm6"
export FORT22="rtgssthr_ships.$PDYm5"
export FORT23="rtgssthr_ships.$PDYm4"
export FORT24="rtgssthr_ships.$PDYm3"
export FORT25="rtgssthr_ships.$PDYm2"
export FORT26="rtgssthr_ships.$PDYm1"
export FORT27="rtgssthr_ships.$PDY"
export FORT31="rtgssthr_dbuoy.$PDYm6"
export FORT32="rtgssthr_dbuoy.$PDYm5"
export FORT33="rtgssthr_dbuoy.$PDYm4"
export FORT34="rtgssthr_dbuoy.$PDYm3"
export FORT35="rtgssthr_dbuoy.$PDYm2"
export FORT36="rtgssthr_dbuoy.$PDYm1"
export FORT37="rtgssthr_dbuoy.$PDY"
export FORT41="rtgssthr_fbuoy.$PDYm6"
export FORT42="rtgssthr_fbuoy.$PDYm5"
export FORT43="rtgssthr_fbuoy.$PDYm4"
export FORT44="rtgssthr_fbuoy.$PDYm3"
export FORT45="rtgssthr_fbuoy.$PDYm2"
export FORT46="rtgssthr_fbuoy.$PDYm1"
export FORT47="rtgssthr_fbuoy.$PDY"
export FORT51="buoydat"
export FORT52="shipdat"

iyrst=`echo "$PDYm6"|cut -c1-4`
imst=`echo "$PDYm6"|cut -c5-6`
idst=`echo "$PDYm6"|cut -c7-8`
cat <<end_ft05 > sstinsdt.input
$iyrst  $imst  $idst  7   iyrst,imst,idst,ndays  (format free)
$CHKPRT
end_ft05

startmsg
$EXECrtgsst/rtgssthr_insdt < sstinsdt.input >> $pgmout 2>errfile
export err=$?;$USHrtgsst/rtgssthr_errwarn.sh;export err=$?;err_chk

#  form 1-deg buoy, ship and ice super obs for oi analysis
# get ice files that were not available in previous runs from /dcom
#  copy all to /com for tomorrows run if SENDCOM
for CDATE in $PDYm6 $PDYm5 $PDYm4 $PDYm3 $PDYm2 $PDYm1 $PDY
do
  if  [ ! -s rtgssthr_seaice.grb.$CDATE ]
  then
    if [ -r ${COM_SEAICE}.${CDATE}/seaice.t00z.5min.grb ]
    then
      cp ${COM_SEAICE}.${CDATE}/seaice.t00z.5min.grb rtgssthr_seaice.grb.$CDATE
      [ $SENDCOM = 'YES' ] && cp rtgssthr_seaice.grb.$CDATE $COMOUT/rtgssthr_seaice.grb.$CDATE
    fi
  else
    [ $SENDCOM = 'YES' ] && cp rtgssthr_seaice.grb.$CDATE $COMOUT/rtgssthr_seaice.grb.$CDATE
  fi
done

export pgm=rtgssthr_superins
. prep_step

export FORT11="buoydat"
export FORT12="shipdat"
export FORT27="$FIXrtgsst/seaice_gland5min"
##export FORT28="$FIXrtgsst/rtgssthr_ls_nas.dat"
export FORT31="rtgssthr_seaice.grb.$PDYm6"
export FORT32="rtgssthr_seaice.grb.$PDYm5"
export FORT33="rtgssthr_seaice.grb.$PDYm4"
export FORT34="rtgssthr_seaice.grb.$PDYm3"
export FORT35="rtgssthr_seaice.grb.$PDYm2"
export FORT36="rtgssthr_seaice.grb.$PDYm1"
export FORT37="rtgssthr_seaice.grb.$PDY"
# SALT.LEVITUS.TOPLAYER must be FORTRAN 77 blocked, IEEE floating-point
export FORT41="$FIXrtgsst/rtgssthr_SALT.LEVITUS.TOPLAYER"
export FORT51="buoy.grid"
export FORT52="ship.grid"
export FORT53="ice.grid"
export FORT61="buoy.realtime"
export FORT62="ship.realtime"
export FORT63="ice.realtime"

startmsg
$EXECrtgsst/rtgssthr_superins >> $pgmout 2>errfile
export err=$?; err_chk
$USHrtgsst/rtgssthr_errwarn.sh;export err=$?;err_chk

##  form 1-deg day and night satellite super obs for oi analysis
export pgm=rtgssthr_supersat
. prep_step

for satid in mta n19 ; do
  export FORT21="buoy.grid"
  export FORT31="rtgssthr_dsat${satid}_merg.$PDYm6"
  export FORT32="rtgssthr_dsat${satid}_merg.$PDYm5"
  export FORT33="rtgssthr_dsat${satid}_merg.$PDYm4"
  export FORT34="rtgssthr_dsat${satid}_merg.$PDYm3"
  export FORT35="rtgssthr_dsat${satid}_merg.$PDYm2"
  export FORT36="rtgssthr_dsat${satid}_merg.$PDYm1"
  export FORT37="rtgssthr_dsat${satid}_merg.$PDY"
  export FORT41="rtgssthr_nsat${satid}_merg.$PDYm6"
  export FORT42="rtgssthr_nsat${satid}_merg.$PDYm5"
  export FORT43="rtgssthr_nsat${satid}_merg.$PDYm4"
  export FORT44="rtgssthr_nsat${satid}_merg.$PDYm3"
  export FORT45="rtgssthr_nsat${satid}_merg.$PDYm2"
  export FORT46="rtgssthr_nsat${satid}_merg.$PDYm1"
  export FORT47="rtgssthr_nsat${satid}_merg.$PDY"
  export FORT51="dsat$satid.grid"
  export FORT52="nsat$satid.grid"

  startmsg
  $EXECrtgsst/rtgssthr_supersat >> $pgmout 2>errfile
  export err=$?;$USHrtgsst/rtgssthr_errwarn.sh;export err=$?;err_chk

done

export pgm=rtgssthr_supersat_amsre
. prep_step

export FORT21="buoy.grid"
export FORT31="rtgssthr_satamsre.$PDYm6"
export FORT32="rtgssthr_satamsre.$PDYm5"
export FORT33="rtgssthr_satamsre.$PDYm4"
export FORT34="rtgssthr_satamsre.$PDYm3"
export FORT35="rtgssthr_satamsre.$PDYm2"
export FORT36="rtgssthr_satamsre.$PDYm1"
export FORT37="rtgssthr_satamsre.$PDY"
export FORT51="satamsre.grid"

startmsg
$EXECrtgsst/rtgssthr_supersat_amsre >> $pgmout 2>&1
export err=$?;$USHrtgsst/rtgssthr_errwarn.sh;export err=$?;err_chk

export pgm=rtgssthr_supersat_goes
. prep_step

export FORT21="buoy.grid"
export FORT31="rtgssthr_satgoes.$PDYm6"
export FORT32="rtgssthr_satgoes.$PDYm5"
export FORT33="rtgssthr_satgoes.$PDYm4"
export FORT34="rtgssthr_satgoes.$PDYm3"
export FORT35="rtgssthr_satgoes.$PDYm2"
export FORT36="rtgssthr_satgoes.$PDYm1"
export FORT37="rtgssthr_satgoes.$PDY"
export FORT51="satgoes.grid"

startmsg
$EXECrtgsst/rtgssthr_supersat_goes >> $pgmout 2>&1
export err=$?;$USHrtgsst/rtgssthr_errwarn.sh;export err=$?;err_chk

##  create guess from previous analysis and climatology
PREVSST=$COMINm1/rtgssthr_grb_0.083
if [ -s $PREVSST ]
then
  cp $PREVSST prevanal.grb
  err=$?
else
  echo "Yesterday's analysis not available for first guess.  Check past days."
  err=1
  for SDIR in $COMINm2 $COMINm3 $COMINm4 $COMINm5 $COMINm6 $COMINm7
  do
    PREVSST=$SDIR/rtgssthr_grb_0.083
    if [ -s $PREVSST ]
    then
      cp $PREVSST prevanal.grb
      err=$?
      warnSUBJ=JRTGSSTHR
      if [[ $envir = para ]] ; then
        warnSUBJ=JRTGSSTHR_PARA
      fi
      warnBODY="Today is ${PDY}.  Guess field in $envir from ${PREVSST}."
      export warnSUBJ warnBODY
      break
    fi
  done
fi

if [ $err -ne 0 ]
then
  echo #####################################################
  echo #####################################################
  echo   !!!!     NO PREVSST FILE   !!!!
  echo #####################################################
  echo #####################################################
  warnSUBJ=JRTGSSTHR
  if [[ $envir = para ]] ; then
    warnSUBJ=JRTGSSTHR_PARA
  fi
  upenvir=$(echo $envir | tr [a-z] [A-Z])
  warnBODY="Todays date: ${PDY}.  No GUESS.  $upenvir RTG_SST FAILED"
  export warnSUBJ warnBODY
  err_exit
  exit
fi

export pgm=rtgssthr_adjguess
. prep_step

export FORT11="$FIXrtgsst/rtgssthr_aoi.61.90.clim.twelfthdeg"
export FORT12="prevanal.grb"
export FORT13="buoy.realtime"
export FORT31="$FIXrtgsst/rtgssthr_ls_nas.twelfthdeg.dat"
export FORT51="guess"
export FORT52="climatology"

startmsg
$EXECrtgsst/rtgssthr_adjguess >> $pgmout 2>errfile
export err=$?;err_chk

$USHrtgsst/rtgssthr_errwarn.sh;export err=$?;err_chk

##  calculate day and night satellite biases
##  remove day and night satellite biases

for satid in mta n19; do

  export pgm=rtgssthr_biascor
  . prep_step

  export FORT11="ship.grid"
  export FORT12="buoy.grid"
  export FORT13="ice.grid"
  export FORT14="dsat$satid.grid"
  export FORT15="nsat$satid.grid"
  export FORT21="rtgssthr_dsat${satid}_merg.$PDY"
  export FORT22="rtgssthr_nsat${satid}_merg.$PDY"
  export FORT31="$FIXrtgsst/rtgssthr_ls_nas.dat"
  export FORT41="guess"
####export FORT42="$FIXrtgsst/rtgssthr_ls_nas.twelfthdeg.dat"
  export FORT51="dsat${satid}c.realtime"
  export FORT52="nsat${satid}c.realtime"
  export FORT71="one.degree.diag"
  export FORT74="four.degree.diag"
  export FORT77="biascor.$satid.4deg"
  export FORT87="weaver"

  startmsg
  $EXECrtgsst/rtgssthr_biascor >> $pgmout 2>&1
  export err=$?;$USHrtgsst/rtgssthr_errwarn.sh;export err=$?;err_chk

done

export pgm=rtgssthr_biascor_amsre
. prep_step

export FORT11="ship.grid"
export FORT12="buoy.grid"
export FORT13="ice.grid"
export FORT14="satamsre.grid"
export FORT21="rtgssthr_satamsre.$PDY"
export FORT31="$FIXrtgsst/rtgssthr_ls_nas.dat"
export FORT41="guess"
export FORT51="satamsrec.realtime"
export FORT71="one.degree.diag"
export FORT74="four.degree.diag"
export FORT77="biascor.amsre.4deg"
export FORT87="weaver"

startmsg
$EXECrtgsst/rtgssthr_biascor_amsre >> $pgmout 2>&1
export err=$?;$USHrtgsst/rtgssthr_errwarn.sh;export err=$?;err_chk

export pgm=rtgssthr_biascor_goes
. prep_step

export FORT11="ship.grid"
export FORT12="buoy.grid"
export FORT13="ice.grid"
export FORT14="satgoes.grid"
export FORT21="rtgssthr_satgoes.$PDY"
export FORT31="$FIXrtgsst/rtgssthr_ls_nas.dat"
export FORT41="guess"
export FORT51="satgoesc.realtime"
export FORT71="one.degree.diag"
export FORT74="four.degree.diag"
export FORT77="biascor.goes.4deg"
export FORT87="weaver"

startmsg
$EXECrtgsst/rtgssthr_biascor_goes >> $pgmout 2>&1
export err=$?;$USHrtgsst/rtgssthr_errwarn.sh;export err=$?;err_chk

## OI SST analysis
# read super observations and previous analysis (first guess)
# compute a new analysis using optimum interpolation (OI)
# and output the new SST OI analysis on a 1 degree grid

export pgm=rtgssthr_rthranl4
. prep_step

export FORT11="buoy.realtime"
export FORT12="ship.realtime"
export FORT13="ice.realtime"
export FORT14="nsatmtac.realtime"
export FORT15="dsatmtac.realtime"
export FORT16="nsatn19c.realtime"
export FORT17="dsatn19c.realtime"
export FORT18="satamsrec.realtime"
export FORT19="satgoesc.realtime"
export FORT28="guess"
export FORT29="climatology"
export FORT31="$FIXrtgsst/rtgssthr_ls_nas.twelfthdeg.dat"
export FORT32="$FIXrtgsst/rtgssthr_ls_nas.halfdeg.dat"
export FORT41="$FIXrtgsst/rtgssthr_stat0.083"
export FORT42="$FIXrtgsst/rtgssthr_IncrAmpGLB0.083"
export FORT43="$FIXrtgsst/rtgssthr_SSTgrdntScl_0.083.gr"
export FORT49="$FIXrtgsst/rtgssthr_pseudo_obs.$PDY"
export FORT62="rtgsstgrb0.083_awips"
export FORT63="rtgsstgrb0.083"
export FORT64="anomlygrb0.083"
export FORT65="rtgsstgrb0.5_awips"
export FORT66="rtgsstgrb0.5"
export FORT70="gridded.datacounts.realtime"
export FORT71="buoy.realtime.used"
export FORT72="ship.realtime.used"
export FORT73="ice.realtime.used"
export FORT74="nsatmtac.realtime.used"
export FORT75="dsatmtac.realtime.used"
export FORT76="nsatn19c.realtime.used"
export FORT77="dsatn19c.realtime.used"
export FORT78="satamsrec.realtime.used"
export FORT79="satgoesc.realtime.used"

cat <<end_ft05 > rthranl.input
 &CONTR2 LBUOY=T,LSHIP=T,LDSATMTA=T,LNSATMTA=T,LDSATN19=T,LNSATN19=T,
         LICE=T,LBUOYCHK=T,LSHIPCHK=T,LDSATMTACHK=T,LNSATMTACHK=T,
         LDSATN19CHK=T,LNSATN19CHK=T,LICECHK=T,LSATAMSRE=T,
         LSATAMSRECHK=T,LSATGOES=T,LSATGOESCHK=T,LPSEUDO=T,
         LPSEUDOCHK=F /
end_ft05

startmsg
mpiexec -n $NCPUS $EXECrtgsst/rtgssthr_rthranl4 < rthranl.input >> $pgmout 2>errfile
export err=$?; err_chk
$USHrtgsst/rtgssthr_errwarn.sh;export err=$?;err_chk

$GRBINDEX rtgsstgrb0.083_awips rtgsstgrb0.083_awips.index 
$GRBINDEX rtgsstgrb0.083 rtgsstgrb0.083.index 
$GRBINDEX anomlygrb0.083 anomlygrb0.083.index 
$GRBINDEX rtgsstgrb0.5_awips rtgsstgrb0.5_awips.index 
$GRBINDEX rtgsstgrb0.5   rtgsstgrb0.5.index 


#####################################################################
#
#     PROCESS GLOBAL SEA SURFACE TEMPERATURE (SST) FOR AWIPS
#
#####################################################################


$CNVGRIB -g12 -p40 rtgsstgrb0.083_awips  rtgsst_grb_0.083_awips.grib2
$WGRIB2 rtgsst_grb_0.083_awips.grib2 -s > rtgsst_grb_0.083_awips.grib2.idx

$CNVGRIB -g12 -p2 rtgsstgrb0.083_awips  rtgsst_grb_0.083_nomads.grib2
$WGRIB2 rtgsst_grb_0.083_nomads.grib2 -s > rtgsst_grb_0.083_nomads.grib2.idx

export parmcard=${PARMrtgsst}/grib2_awips_rtgssthr

export FORT11="rtgsst_grb_0.083_awips.grib2"
export FORT31=" "
export FORT51="grib2.awips_rtgssthr_grb_0.083"

$TOCGRIB2 < $parmcard

##############################
# Convert to grib2 format
##############################
$CNVGRIB -g12 -p40 rtgsstgrb0.5 rtgssthr_grb_0.5.grib2
$CNVGRIB -g12 -p40 rtgsstgrb0.083 rtgssthr_grb_0.083.grib2
$WGRIB2 rtgssthr_grb_0.5.grib2 -s >rtgssthr_grb_0.5.grib2.idx
$WGRIB2 rtgssthr_grb_0.083.grib2 -s > rtgssthr_grb_0.083.grib2.idx

if test "$SENDCOM" = 'YES'
then
  cp rtgsstgrb0.083_awips       $COMOUT/rtgssthr_grb_0.083_awips
  cp rtgsstgrb0.083_awips.index $COMOUT/rtgssthr_grb_0.083_awips.index
  cp rtgsstgrb0.083             $COMOUT/rtgssthr_grb_0.083
  cp rtgsstgrb0.083.index       $COMOUT/rtgssthr_grb_0.083.index
  cp anomlygrb0.083             $COMOUT/anomaly_grb_0.083
  cp anomlygrb0.083.index       $COMOUT/anomaly_grb_0.083.index
  cp rtgsstgrb0.5_awips         $COMOUT/rtgssthr_grb_0.5_awips
  cp rtgsstgrb0.5_awips.index   $COMOUT/rtgssthr_grb_0.5_awips.index
  cp rtgsstgrb0.5               $COMOUT/rtgssthr_grb_0.5
  cp rtgsstgrb0.5.index         $COMOUT/rtgssthr_grb_0.5.index
  for satid in mta n19 ; do
    cp nsat${satid}c.realtime.used     $COMOUT/rtgssthr_nsat${satid}_used.$PDY
    cp dsat${satid}c.realtime.used     $COMOUT/rtgssthr_dsat${satid}_used.$PDY
    cp biascor.$satid.4deg             $COMOUT/rtgssthr_biascor_${satid}_4deg.$PDY
  done
  cp biascor.amsre.4deg                $COMOUT/rtgssthr_biascor_amsre_4deg.$PDY
  cp biascor.goes.4deg                 $COMOUT/rtgssthr_biascor_goes_4deg.$PDY
  cp satamsrec.realtime.used     $COMOUT/rtgssthr_satamsre_used.$PDY
  cp satgoesc.realtime.used      $COMOUT/rtgssthr_satgoes_used.$PDY
  cp gridded.datacounts.realtime $COMOUT/rtgssthr_gridded_datacounts.$PDY
  cp buoy.realtime.used         $COMOUT/rtgssthr_buoys_used.$PDY
  cp ship.realtime.used         $COMOUT/rtgssthr_ships_used.$PDY
  chgrp rstprod $COMOUT/rtgssthr_ships_used.$PDY 
  chmod 640 $COMOUT/rtgssthr_ships_used.$PDY 
  cp ice.realtime.used          $COMOUT/rtgssthr_seaice_used.$PDY
  cp grib2.awips_rtgssthr_grb_0.083 $COMOUT/grib2.awips_rtgssthr_grb_0.083
  cp rtgssthr_grb_0.5.grib2     $COMOUT/rtgssthr_grb_0.5.grib2
  cp rtgssthr_grb_0.083.grib2   $COMOUT/rtgssthr_grb_0.083.grib2
  cp rtgssthr_grb_0.5.grib2.idx  $COMOUT/rtgssthr_grb_0.5.grib2.idx
  cp rtgssthr_grb_0.083.grib2.idx $COMOUT/rtgssthr_grb_0.083.grib2.idx
  cp rtgsst_grb_0.083_awips.grib2  $COMOUT/rtgssthr_grb_0.083_awips.grib2
  cp rtgsst_grb_0.083_awips.grib2.idx $COMOUT/rtgssthr_grb_0.083_awips.grib2.idx
  cp rtgsst_grb_0.083_nomads.grib2  $COMOUT/rtgssthr_grb_0.083_nomads.grib2
  cp rtgsst_grb_0.083_nomads.grib2.idx $COMOUT/rtgssthr_grb_0.083_nomads.grib2.idx
  
  if test "$SENDDBN" = 'YES'
  then
     $DBNROOT/bin/dbn_alert MODEL RTG_SST_grib $job $COMOUT/rtgssthr_grb_0.5
     $DBNROOT/bin/dbn_alert MODEL RTG_SST_gribi $job $COMOUT/rtgssthr_grb_0.5.index
     $DBNROOT/bin/dbn_alert MODEL RTG_SST_grib $job $COMOUT/rtgssthr_grb_0.083
     $DBNROOT/bin/dbn_alert MODEL RTG_SST_gribi $job $COMOUT/rtgssthr_grb_0.083.index
     $DBNROOT/bin/dbn_alert MODEL RTG_SST_grib $job $COMOUT/rtgssthr_grb_0.083_awips
     $DBNROOT/bin/dbn_alert MODEL RTG_SST_gribi $job $COMOUT/rtgssthr_grb_0.083_awips.index
     $DBNROOT/bin/dbn_alert NTC_LOW $NET $job $COMOUT/grib2.awips_rtgssthr_grb_0.083

     if [ $SENDDBN_GB2 = YES ]
     then

     $DBNROOT/bin/dbn_alert MODEL RTG_SST_grib_GB2 $job $COMOUT/rtgssthr_grb_0.5.grib2
     $DBNROOT/bin/dbn_alert MODEL RTG_SST_grib_GB2 $job $COMOUT/rtgssthr_grb_0.083.grib2
     $DBNROOT/bin/dbn_alert MODEL RTG_SST_grib_GB2 $job $COMOUT/rtgssthr_grb_0.083_awips.grib2
     $DBNROOT/bin/dbn_alert MODEL RTG_SST_grib_GB2_WIDX $job $COMOUT/rtgssthr_grb_0.5.grib2.idx
     $DBNROOT/bin/dbn_alert MODEL RTG_SST_grib_GB2_WIDX $job $COMOUT/rtgssthr_grb_0.083.grib2.idx
     $DBNROOT/bin/dbn_alert MODEL RTG_SST_grib_GB2_WIDX $job $COMOUT/rtgssthr_grb_0.083_awips.grib2.idx

     fi

  else
     echo "SENDDBN=$SENDDBN, files not posted to db_net."
  fi

fi

set -x
msg='ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################
