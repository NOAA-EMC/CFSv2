#!/bin/ksh   
#####################################################################
echo "-----------------------------------------------------"
echo " excfs_cdas_nceppost.sh.sms" 
echo " Sep 07 - Chuang - Modified script to run unified post"
echo "-----------------------------------------------------"
#####################################################################

set -euxa

cd $DATA

msg="HAS BEGUN on `hostname`"
postmsg "$jlogfile" "$msg"

export MP_LABELIO=YES

#poe hostname

############################################################
#  Define Variables:
#  -----------------
#  FH           is the current forecast hour.
#  SLEEP_TIME   is the number of seconds to sleep before exiting with error.
#  SLEEP_INT    is the number of seconds to sleep between restrt file checks.
#  restart_file is the name of the file to key off of to kick off post.
############################################################

############################################################
# Post Analysis Files before starting the Forecast Post
############################################################
# Chuang: modify to process analysis when post_times is 00
export stime=`echo $post_times | cut -c1-2`

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
export RUN=${RUN}1

if test -f $COMIN/${RUN}.t${cyc}z.sanl -a ${stime} = "00"
then
   
# add new environmental variables for running new ncep post
# Validation date

   export VDATE=${PDY}${cyc}
   export CTLFILE=$PARM_AM/cfs_cdas_cntrl.parm_anl
   rm -f sigfile
   cp $COMIN/${RUN}.t${cyc}z.sanl sigfile

   export SIGINP=sigfile
   export FLXINP=/dev/null
   export FLXIOUT=flxifile
   export PGBOUT=pgbfile
   export PGIOUT=pgifile
   # Added processing for IPV
   export IPVOUT=ipvfile
   export IGEN=$IGEN_ANL

   if [ $IDRT -eq 0 ] ; then
     export D3DINP=
     if [ $LONB -eq 360 ] ; then
        $COPYGB -xg3 $FLXINP $DATA/fluxgrid
     elif [ $LONB -eq 720 ] ; then
        $COPYGB -xg4 $FLXINP $DATA/fluxgrid
     fi
     export FLXINP=$DATA/fluxgrid
   fi 

# specify fhr even for analysis because postgp uses it    
   export fhr=00
    
   $POSTGPSH

   if test "$SENDCOM" = 'YES'
   then
      #
      # Save Master GRIB/Index files
      #
      cp pgbfile $COMOUT/${RUN}.${cycle}.pgrbhanl      
      $GRBINDEX $COMOUT/${RUN}.${cycle}.pgrbhanl $COMOUT/${RUN}.${cycle}.pgrbihanl 
 
      $COPYGB -xg3 pgbfile $COMOUT/${RUN}.${cycle}.pgrbanl
      $GRBINDEX $COMOUT/${RUN}.${cycle}.pgrbanl $COMOUT/${RUN}.${cycle}.pgrbianl

      rm -f pgb25a pgb25b
      $WGRIB pgbfile|grep -i :kpds6=100:|$COPYGB -xkw -i4,0,35 -g$GRID_ID25 pgbfile pgb25a 
      $WGRIB pgbfile|grep -vi :kpds6=100:|$COPYGB -xkw -i$in_o -g$GRID_ID25 pgbfile pgb25b
      cat pgb25a pgb25b > $COMOUT/${RUN}.t${cyc}z.pgrblanl

      # For IPV files
      cp ipvfile $COMOUT/${RUN}.${cycle}.ipvgrbhanl      
      $COPYGB -xg3 ipvfile $COMOUT/${RUN}.${cycle}.ipvgrbanl
      $COPYGB -xg2 ipvfile $COMOUT/${RUN}.${cycle}.ipvgrblanl

   fi
   rm -f pgbfile pgifile ipvfile pgbfile.grib2 pgbfile.grib2.idx flxfile flxifile flxfile.grib2 flxfile.grib2.idx
fi

SLEEP_LOOP_MAX=`expr $SLEEP_TIME / $SLEEP_INT`

### Add flag "PROCESS_PGB" here to determine when to generate pgb files, default to YES
if [ ${PROCESS_PGB:-YES} = YES ]
then

############################################################
# Chuang: modify to submit one post job at a time
# Loop Through the Post Forecast Files 
############################################################

for fhr in $post_times
do

    ###############################
    # Start Looping for the 
    # existence of the restart files
    ###############################
    set -x
    export pgm="postcheck"
    ic=1
    while [ $ic -le $SLEEP_LOOP_MAX ]
    do
       if test -f $restart_file$fhr
       then
          break
       else
          ic=`expr $ic + 1`
          sleep $SLEEP_INT
       fi
       ###############################
       # If we reach this point assume
       # fcst job never reached restart 
       # period and error exit
       ###############################
       if [ $ic -eq $SLEEP_LOOP_MAX ]
       then
          export err=9
          err_chk
       fi
    done
    set -x

    msg="Starting post for fhr=$fhr"
    postmsg "$jlogfile" "$msg"

    ###############################
    # Put restart files into $GESROOT 
    # for backup to start Model Fcst
    ###############################
    rm -f sigfile
    cp $COMIN/${RUN}.t${cyc}z.sf$fhr sigfile
    cp $COMIN/${RUN}.t${cyc}z.sfluxgrbf$fhr flxfile

    if test $fhr -gt 0
    then
       export IGEN=$IGEN_FCST
    else
       export IGEN=$IGEN_ANL
    fi
    
# add new environmental variables for running new ncep post
# Validation date

    export VDATE=`$NDATE +${fhr} ${PDY}${cyc}`
    export CTLFILE=$PARM_AM/cfs_cdas_cntrl.parm
    export SIGINP=sigfile
    export FLXINP=flxfile
    export FLXIOUT=flxifile
    export PGBOUT=pgbfile
    export IPVOUT=ipvfile
    export PGIOUT=pgifile
    
# specify 1 degree output
    if [ $IDRT -eq 0 ] ; then
       export D3DINP=
       if [ $LONB -eq 360 ] ; then
          $COPYGB -xg3 $FLXINP $DATA/fluxgrid
       elif [ $LONB -eq 720 ] ; then
          $COPYGB -g4 -i${in_o} -x $FLXINP $DATA/fluxgrid
       fi
       export FLXINP=$DATA/fluxgrid
    fi

    $POSTGPSH


    if [ $CAT_FLX_TO_PGB = YES ]
    then
       cat $DATA/fluxgrid >>pgbfile
    fi
    cp pgbfile $COMOUT/${RUN}.${cycle}.pgrbh$fhr      
    $GRBINDEX $COMOUT/${RUN}.${cycle}.pgrbh$fhr $COMOUT/${RUN}.${cycle}.pgrbih$fhr
    mv pgbfile tmpfile
    $COPYGB -xg3 tmpfile pgbfile 
    $GRBINDEX pgbfile pgifile 
  
    cp pgbfile $COMOUT/${RUN}.t${cyc}z.pgrbf$fhr
    cp pgifile $COMOUT/${RUN}.t${cyc}z.pgrbif$fhr

    rm -f pgb25a pgb25b
    $WGRIB tmpfile|grep -i :kpds6=100:|$COPYGB -xkw -i4,0,35 -g$GRID_ID25 tmpfile pgb25a 
    $WGRIB tmpfile|grep -vi :kpds6=100:|$COPYGB -xkw -i$in_o -g$GRID_ID25 tmpfile pgb25b
    cat pgb25a pgb25b > $COMOUT/${RUN}.t${cyc}z.pgrbl$fhr
     
    # For IPV files
    cp ipvfile $COMOUT/${RUN}.${cycle}.ipvgrbh$fhr      
    $COPYGB -xg3 ipvfile $COMOUT/${RUN}.${cycle}.ipvgrbf$fhr
    $COPYGB -g$GRID_ID25 -i$in_o -x ipvfile $COMOUT/${RUN}.${cycle}.ipvgrbl$fhr

    # Create index file for flux
    $GRBINDEX $COMOUT/${RUN}.${cycle}.sfluxgrbf$fhr $COMOUT/${RUN}.${cycle}.sfluxgrbif$fhr
      
    # flux file in T62
    $COPYGB -g98 -i$in_o -x flxfile $COMOUT/${RUN}.${cycle}.sfluxgrbl$fhr

    if test $fhr -lt 100
    then
      pad="0"
    else
      pad=""
    fi
    echo "$PDY$cyc$pad$fhr" > $COMOUT/${RUN}.t${cyc}z.${RUN_FLAG}control

    rm -f pgbfile pgifile pgbfile.grib2 pgbfile.grib2.idx flxfile flxifile flxfile.grib2 flxfile.grib2.idx
    
done
fi  # Done PROCESS PGB

# Add the generation of SPL files here:
if [ ${PROCESS_SPL:-YES} = YES ]; then
 for fhr in anl f06
 do
  export CTLFILE=$PARM_AM/cfs_cdas_cntrl.parm_mdl 
  rm -f sigfile
  cp $COMIN/${RUN}.t${cyc}z.s${fhr} sigfile


  case $fhr in 
   anl) hh=00
        prefix_low=anl ;;
   f06) hh=06
        prefix_low=06 ;;
  esac

  export VDATE=`$NDATE +${hh} ${PDY}${cyc}`
  export in_o=${in_o:-0}       # interpolation option, defaults to 0 (bilinear)
  export GRID_ID62=${GRID_ID62:-98}
  export IDRT=${IDRT_SPL:-4}
  export LONB=${LONB_SPL:-$(echo lonb|$SIGHDR ${SIGINP})}
  export LATB=${LATB_SPL:-$(echo latb|$SIGHDR ${SIGINP})}
  export ko_a=37
  export SIGINP=sigfile
  export FLXINP=/dev/null
  export PGBOUT=splfile
  export FILTER=0
  export IGEN=${IGEN_ANL:-197}
  export GENPSICHI=NO
  export OROGRAPHY=NULL

  $POSTGPSH

  if [ $SENDCOM = YES ]; then
    cp splfile $COMOUT/${RUN}.t${cyc}z.splgrb${fhr}
  fi
  if [ $GRID_ID62 -gt 0 ] ; then
    $COPYGB -g$GRID_ID62 -i$in_o -x splfile $COMOUT/${RUN}.t${cyc}z.splgrbl${prefix_low}
  fi
 
  rm -f splfile

done    # Done adding SPL

fi  # PROCESS_SPL

echo "End Processing PGB and SPL files"

set +x

########################################################
# Process Ocean data
########################################################
if [ ${PROCESS_OCN:-YES} = YES ]
then

OCNPOSTSH=${OCNPOSTSH:-$HOMEcfs/ush/cfs_cdas_ocnp.sh}
export OCNDIR=${OCNDIR:-$COMIN}
export CDATE=${CDATE:-$PDY$cyc}
export CCPOST=${CCPOST:-YES}
export GRID_ID25=${GRID_ID25:-2}

export SUFOUT=${SUFOUT:-".gdas.$CDATE"}
export CDUMP=gdas

$OCNPOSTSH 

echo "End Processing Ocean Data"

fi

#########################################
# Convert to grib2 format and send to TOC
#########################################
rm -f poescript.grb2

if [ ${PROCESS_GB2:-YES} = YES ]
then

# Convert the analysis files:

for name in pgrb ipvgrb; do
  for resl in hanl anl lanl; do
    ifile=$COMOUT/${RUN}.${cycle}.${name}${resl}
    ofile=${RUN}.${cycle}.${name}${resl}.grib2
    echo "$CNVGRIB -g12 -p40 $ifile $ofile" >>poescript.grb2
  done
done

# Convert the forecast files:

for fhr in $post_times; do
  for name in pgrb ipvgrb sfluxgrb; do
    if [ $name = "sfluxgrb" ]; then reslist="f l"; else reslist="h f l"; fi
    for resl in $reslist; do
      ifile=$COMOUT/${RUN}.${cycle}.${name}${resl}${fhr}
      ofile=${RUN}.${cycle}.${name}${resl}${fhr}.grib2
      echo "$CNVGRIB -g12 -p40 $ifile $ofile" >>poescript.grb2
    done
  done
done

# Convert the SPL files:

for name in splgrbanl splgrblanl splgrbf06 splgrbl06
do
  ifile=$COMOUT/${RUN}.${cycle}.${name}
  ofile=${RUN}.${cycle}.${name}.grib2
  echo "$CNVGRIB -g12 -p40 $ifile $ofile" >>poescript.grb2
done

# convert ocean files to grib2

for fhr in 01 02 03 04 05 06 06m 07 08 09; do 
  if [ $fhr = "06m" ]; then reslist="h f"; else reslist="h f l"; fi
  for resl in $reslist; do
    ifile=$COMOUT/${RUN}.${cycle}.ocngrb${resl}${fhr}
    ofile=${RUN}.${cycle}.ocngrb${resl}${fhr}.grib2
    echo "$CNVGRIB -g12 -p40 $ifile $ofile" >>poescript.grb2
   done
done

# run the cfp mpmd process

mpiexec -n 20 cfp poescript.grb2 |grep 'CFP RANK'
export err=$?; err_chk

# Copy all the grib2 files to COMOUT

if [ $SENDCOM = YES ]; then
  for grb2file in `ls ${RUN}.t${cyc}z.*.grib2`
  do
  $WGRIB2 $grb2file -s >${grb2file}.idx
  done
  mv ${RUN}.t${cyc}z.*.grib2 $COMOUT/.
  mv ${RUN}.t${cyc}z.*.grib2.idx $COMOUT/.
fi

fi   # PROCESS_GB2

echo "Done GRIB2 conversion for all the files"

# DBN alert the files to TOC
if [ $SENDDBN = YES ]; then

   # Analysis Files
   run=`echo $RUN | tr '[a-z]' '[A-Z]'`
   $DBNROOT/bin/dbn_alert MODEL ${run}_MSC_sfcanl $job $COMOUT/${RUN}.${cycle}.sfcanl
   $DBNROOT/bin/dbn_alert MODEL ${run}_SA $job $COMOUT/${RUN}.t${cyc}z.sanl
   $DBNROOT/bin/dbn_alert MODEL ${run}_OANL $job $COMOUT/${RUN}.t${cyc}z.ocnanl.tar
   $DBNROOT/bin/dbn_alert MODEL ${run}_PGA_GB2 $job $COMOUT/${RUN}.t${cyc}z.pgrbhanl.grib2
   $DBNROOT/bin/dbn_alert MODEL ${run}_PGA_GB2 $job $COMOUT/${RUN}.t${cyc}z.pgrbanl.grib2
   $DBNROOT/bin/dbn_alert MODEL ${run}_PGA_GB2 $job $COMOUT/${RUN}.t${cyc}z.pgrblanl.grib2
   $DBNROOT/bin/dbn_alert MODEL ${run}_PGA_GB2_WIDX $job $COMOUT/${RUN}.t${cyc}z.pgrbhanl.grib2.idx
   $DBNROOT/bin/dbn_alert MODEL ${run}_PGA_GB2_WIDX $job $COMOUT/${RUN}.t${cyc}z.pgrbanl.grib2.idx
   $DBNROOT/bin/dbn_alert MODEL ${run}_PGA_GB2_WIDX $job $COMOUT/${RUN}.t${cyc}z.pgrblanl.grib2.idx

   $DBNROOT/bin/dbn_alert MODEL ${run}_PGA_GB2 $job $COMOUT/${RUN}.t${cyc}z.ipvgrbhanl.grib2
   $DBNROOT/bin/dbn_alert MODEL ${run}_IPVA_GB2 $job $COMOUT/${RUN}.t${cyc}z.ipvgrbanl.grib2
   $DBNROOT/bin/dbn_alert MODEL ${run}_IPVA_GB2 $job $COMOUT/${RUN}.t${cyc}z.ipvgrblanl.grib2
   $DBNROOT/bin/dbn_alert MODEL ${run}_PGA_GB2_WIDX $job $COMOUT/${RUN}.t${cyc}z.ipvgrbhanl.grib2.idx
   $DBNROOT/bin/dbn_alert MODEL ${run}_IPVA_GB2_WIDX $job $COMOUT/${RUN}.t${cyc}z.ipvgrbanl.grib2.idx
   $DBNROOT/bin/dbn_alert MODEL ${run}_IPVA_GB2_WIDX $job $COMOUT/${RUN}.t${cyc}z.ipvgrblanl.grib2.idx

   # Forecast Files
   for fhr in $post_times
   do
      run=`echo $RUN | tr '[a-z]' '[A-Z]'`
      if [ $fhr -eq 06 ]; then
          $DBNROOT/bin/dbn_alert MODEL ${run}_BF $job $COMOUT/${RUN}.${cycle}.bf${fhr}
          $DBNROOT/bin/dbn_alert MODEL ${run}_SF $job $COMOUT/${RUN}.t${cyc}z.sf${fhr}
      fi
      $DBNROOT/bin/dbn_alert MODEL ${run}_PGBH $job $COMOUT/${RUN}.t${cyc}z.pgrbh${fhr}.grib2
      $DBNROOT/bin/dbn_alert MODEL ${run}_PGB $job $COMOUT/${RUN}.t${cyc}z.pgrbf${fhr}.grib2
      $DBNROOT/bin/dbn_alert MODEL ${run}_PGBL $job $COMOUT/${RUN}.t${cyc}z.pgrbl${fhr}.grib2
      $DBNROOT/bin/dbn_alert MODEL ${run}_PGBH_WIDX $job $COMOUT/${RUN}.t${cyc}z.pgrbh${fhr}.grib2.idx
      $DBNROOT/bin/dbn_alert MODEL ${run}_PGB_WIDX $job $COMOUT/${RUN}.t${cyc}z.pgrbf${fhr}.grib2.idx
      $DBNROOT/bin/dbn_alert MODEL ${run}_PGBL_WIDX $job $COMOUT/${RUN}.t${cyc}z.pgrbl${fhr}.grib2.idx

      $DBNROOT/bin/dbn_alert MODEL ${run}_IPVH $job $COMOUT/${RUN}.t${cyc}z.ipvgrbh${fhr}.grib2
      $DBNROOT/bin/dbn_alert MODEL ${run}_IPV $job $COMOUT/${RUN}.t${cyc}z.ipvgrbf${fhr}.grib2
      $DBNROOT/bin/dbn_alert MODEL ${run}_IPVL $job $COMOUT/${RUN}.t${cyc}z.ipvgrbl${fhr}.grib2
      $DBNROOT/bin/dbn_alert MODEL ${run}_IPVH_WIDX $job $COMOUT/${RUN}.t${cyc}z.ipvgrbh${fhr}.grib2.idx
      $DBNROOT/bin/dbn_alert MODEL ${run}_IPV_WIDX $job $COMOUT/${RUN}.t${cyc}z.ipvgrbf${fhr}.grib2.idx
      $DBNROOT/bin/dbn_alert MODEL ${run}_IPVL_WIDX $job $COMOUT/${RUN}.t${cyc}z.ipvgrbl${fhr}.grib2.idx

      $DBNROOT/bin/dbn_alert MODEL ${run}_SGB $job $COMOUT/${RUN}.t${cyc}z.sfluxgrbf${fhr}.grib2
      $DBNROOT/bin/dbn_alert MODEL ${run}_SGBL $job $COMOUT/${RUN}.t${cyc}z.sfluxgrbl${fhr}.grib2
      $DBNROOT/bin/dbn_alert MODEL ${run}_SGB_WIDX $job $COMOUT/${RUN}.t${cyc}z.sfluxgrbf${fhr}.grib2.idx
      $DBNROOT/bin/dbn_alert MODEL ${run}_SGBL_WIDX $job $COMOUT/${RUN}.t${cyc}z.sfluxgrbl${fhr}.grib2.idx
   done

   # SPL Files
   $DBNROOT/bin/dbn_alert MODEL ${run}_SPL $job $COMOUT/${RUN}.t${cyc}z.splgrbanl.grib2
   $DBNROOT/bin/dbn_alert MODEL ${run}_SPLL $job $COMOUT/${RUN}.t${cyc}z.splgrblanl.grib2
   $DBNROOT/bin/dbn_alert MODEL ${run}_SPL $job $COMOUT/${RUN}.t${cyc}z.splgrbf06.grib2
   $DBNROOT/bin/dbn_alert MODEL ${run}_SPLL $job $COMOUT/${RUN}.t${cyc}z.splgrbl06.grib2
   $DBNROOT/bin/dbn_alert MODEL ${run}_SPL_WIDX $job $COMOUT/${RUN}.t${cyc}z.splgrbanl.grib2.idx
   $DBNROOT/bin/dbn_alert MODEL ${run}_SPLL_WIDX $job $COMOUT/${RUN}.t${cyc}z.splgrblanl.grib2.idx
   $DBNROOT/bin/dbn_alert MODEL ${run}_SPL_WIDX $job $COMOUT/${RUN}.t${cyc}z.splgrbf06.grib2.idx
   $DBNROOT/bin/dbn_alert MODEL ${run}_SPLL_WIDX $job $COMOUT/${RUN}.t${cyc}z.splgrbl06.grib2.idx

   # Ocean Files
   for fhr in 01 02 03 04 05 06 06m 07 08 09; do 
     $DBNROOT/bin/dbn_alert MODEL ${run}_OCNH $job $COMOUT/${RUN}.${cycle}.ocngrbh${fhr}.grib2
     $DBNROOT/bin/dbn_alert MODEL ${run}_OCN $job $COMOUT/${RUN}.${cycle}.ocngrbf${fhr}.grib2
     $DBNROOT/bin/dbn_alert MODEL ${run}_OCNL $job $COMOUT/${RUN}.${cycle}.ocngrbl${fhr}.grib2
     $DBNROOT/bin/dbn_alert MODEL ${run}_OCNH_WIDX $job $COMOUT/${RUN}.${cycle}.ocngrbh${fhr}.grib2.idx
     $DBNROOT/bin/dbn_alert MODEL ${run}_OCN_WIDX $job $COMOUT/${RUN}.${cycle}.ocngrbf${fhr}.grib2.idx
     $DBNROOT/bin/dbn_alert MODEL ${run}_OCNL_WIDX $job $COMOUT/${RUN}.${cycle}.ocngrbl${fhr}.grib2.idx
   done
fi
########################################################

msg='ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"

################## END OF SCRIPT #######################
