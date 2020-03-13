#!/bin/ksh

set -x

####################################################################################
# Script to create HPSS archives of the operational CFSv2 analysis daily runs
# This script runs daily after the 18Z cycle of analysis
#
# Incoming parameters
# --------------------
# YYYYMMDD - The year month and day to run
#
# Patrick Tripp EMC/NCEP/NOAA - September 2010
####################################################################################

#######################################################
# Incoming parameters
#######################################################

YYYYMMDD=${1:-${YYYYMMDD:-20101101}}

echo "YYYYMMDD=$YYYYMMDD"

#######################################################

YYYY=`echo $YYYYMMDD | cut -c1-4`
MM=`echo $YYYYMMDD | cut -c5-6`
DD=`echo $YYYYMMDD | cut -c7-8`

######################
# Base directories
######################

CHKFILEANLSH=${CHKFILEANLSH:-$USHcfs/cfs_chk_file_anl.sh}
CHKFILESH=${CHKFILESH:-$USHcfs/cfs_chk_file.sh}

export TEMPDIR=${TEMPDIR:-$DATA}             # Working Directory
mkdir -p $TEMPDIR

cd $TEMPDIR
COMROT=${COMROT:-$COMROOT/cfs/prod}

# gdas2 files - These will all be from 00Z member 1 forecast
member=01
COMFCST=${COMFCST:-$COMROT/cfs/cfs.$YYYYMMDD/00}
HRLY6DIR=${HRLY6DIR:-$COMFCST/6hrly_grib_$member}

COMANALYSIS=${COMANALYSIS:-$COMROT/cdas.$YYYYMMDD}

HPSSROT=${HPSSROT:-/NCEPPROD/hpss${envir}/runhistory}
HPSSBASE=${HPSSBASE:-$HPSSROT/cfs$YYYY/$YYYY$MM/$YYYYMMDD/Analysis}

# Analysis files staging area
################################
ANALYSISTEMP=${ANALYSISTEMP:-$TEMPDIR/analysis.$YYYYMMDD}
mkdir -p $ANALYSISTEMP

###############################
# Define some external utils
###############################

hpsstar=$USHcfs/cfs_hpsstar 

PUTCMD=putv     # putv does a bit comparison - at a speed cost - put is faster
PUTCMD=${PUTCMD:-putv}

finc=6     # File increment of 6 hours
rcall=0    # ERROR code tracking
rwall=0    # For WARNING messages only

hhlist='00 06 12 18'

##################################################################
# Optionally select parts to run - all depend on docopyanl 1st

dohpssanl=${dohpssanl:-YES}        # Archive analysis/verification files
dohpsshic=${dohpsshic:-YES}	   # Archive High-Res Initial Conditions
dohpsslic=${dohpsslic:-YES}        # Archive Low-Res Initial Conditions
dohpsshigh=${dohpsshigh:-YES}      # Archive High-Res hourly data (pgbh, ocnh, etc.)
dohpsslow=${dohpsslow:-YES}        # Archive Low-Res hourly date (pgbl, ocnf, etc.)
dohpssgdas2=${dohpssgdas2:-YES}    # Archive the gdas2 files from the 00Z m01 forecast
dohpssdumps=${dohpssdumps:-YES}    # Archive dump data
doocndiag=${doocndiag:-YES}        # Ocean diagnostics files



###########################################################
# ANALYSIS HPSS Archive
###########################################################
if [ $dohpssanl = "YES" ] ; then

  echo "BEGIN: ANALYSIS Archive"

  CDAS=cdas1

  hhlist2='00 12'

  fhlist='12 24 36 48'

  name=analysis

  # Each cycle cdas1 types
  typelista="abias abias_pc abias_air cnvstat gsistat radstat oznstat prepbufr prepbufr.acft_profiles prepqa"
  typelista="abias cnvstat gsistat radstat oznstat satang prepbufr prepqa"

  # 00Z only
  typelist0="noah.rst noahbin.lis"

  # these exist hourly for 24 hours
  # previous days 1-23 are in tar file in current directory
  # e.g. cdas1.t00z.LIS.diagnos.20101031.tar

  typelist24="noahgrb.lis"

  # 00,06,12,18 files
  # typelistv="adpsfc.anl sfcshp.anl adpupa.mand.anl aircar.anl aircft.anl f00.acar f06.acar f00.acft f06.acft f00.raob f06.raob f00.sfc f06.sfc"

  # Just 00 and 12
  # typelistv2="prepqf adpsfc.fcs sfcshp.fcs adpupa.mand.fcs aircar.fcs aircft.fcs"

  # 12,24,36,48Z files
  # typelistv48="acar acft raob sfc"

  CDAY=$YYYYMMDD

  rc=0
  rw=0

  cd $COMANALYSIS
  hpsslist=$TEMPDIR/$name.$CDAY.hpsslist
  > $hpsslist

  # cdas1 analysis files types
  for type in $typelista
  do  

    for hh in $hhlist
    do
      file=$CDAS.t${hh}z.${type}
      if [ -s $file ] ; then
        echo $file >> $hpsslist

        # Check file size
        #$CHKFILEANLSH $type $file
        #((rw+=$?))

      else
        echo "ERROR: $file not found for $CDAY$hh"
        ((rc+=1))
      fi
    done
  done   # typelista

  # Just the 00z ones now
  for type in $typelist0
  do

    hh='00'
    file=$CDAS.t${hh}z.${type}
    if [ -s $file ] ; then
      echo $file >> $hpsslist

     # $CHKFILEANLSH $type $file
     # ((rw+=$?))

    else
      echo "ERROR: $file not found for $CDAY$hh"
      ((rc+=1))
    fi
  done   # typelist0

  # Just the 24h noahgrb.lis type
  for type in $typelist24
  do

    hh='00'

    file=$CDAS.t${hh}z.${type}$hh
    if [ -s $file ] ; then
      echo $file >> $hpsslist
      # Check file size
      #$CHKFILEANLSH $type $file
      #((rw+=$?))
    else
       echo "ERROR: $file not found for $CDAY$hh"
      ((rc+=1))
    fi

    # cdas1.t00z.LIS.diagnos.20101031.tar
    PDAY=`$NDATE -24 ${CDAY}00`
    PYYYYMMDD=`echo $PDAY | cut -c1-8`

    file=$CDAS.t${hh}z.LIS.diagnos.$PYYYYMMDD.tar
    if [ -s $file ] ; then
      echo $file >> $hpsslist
      # Check file size
      #$CHKFILEANLSH $type $file
      #((rw+=$?))
    else
       echo "ERROR: $file not found for $CDAY$hh"
      ((rc+=1))
    fi
  done   # typelist24


# # Verification types
# for type in $typelistv
# do
#
#   for hh in $hhlist
#   do
#     file=$type.$CDAY$hh
#     if [ -s $file ] ; then
#       echo $file >> $hpsslist
#
#       # Check file size
#       #$CHKFILEANLSH $type $file
#       #((rw+=$?))
#
#     else
#       echo "ERROR: $file not found for $CDAY$hh"
#       ((rc+=1))
#     fi
#   done
# done # typelistv



# # Verification types 00Z and 12Z only
# for type in $typelistv2
# do
#
#   for hh in $hhlist2
#   do
#
#     if [ $type = "prepqf" ] ; then
#       #cdas1.t00z.prepqf
#       file=$CDAS.t${hh}z.${type}
#     else
#       file=$type.$CDAY$hh
#     fi
#
#     if [ -s $file ] ; then
#       echo $file >> $hpsslist
#
#       # Check file size
#       #$CHKFILEANLSH $type $file
#       #((rw+=$?))
#
#     else
#       echo "ERROR: $file not found for $CDAY$hh"
#       ((rc+=1))
#     fi
#   done
# done # typelistv2



# # Verification types 12,24,36,48
# for type in $typelistv48
# do
#
#   for fhh in $fhlist
#   do
#
#     if [ $fhh -eq 24 -o $fhh -eq 48 ] ; then hh=00 ; else hh=12 ; fi
#
#     file=f$fhh.$type.$CDAY$hh
#
#     if [ -s $file ] ; then
#       echo $file >> $hpsslist
#
#       # Check file size
#       #$CHKFILEANLSH $type $file
#       #((rw+=$?))
#
#     else
#       echo "ERROR: $file not found for $CDAY$fhh"
#       ((rc+=1))
#     fi
#   done
# done # typelistv48


  #################
  # HPSS Section
  #################

  HPSSDIR=$HPSSBASE
  $hpsstar mkd $HPSSDIR

  hpssfile=cfs.$name.$CDAY.tar

  $hpsstar $PUTCMD $HPSSDIR/$hpssfile `cat $hpsslist`
  if [[ $? -ne 0 ]] ; then
    ((rc+=1))
    echo "ERROR: hpsstar $PUTCMD $HPSSDIR/$hpssfile"
#    export err=$rc; err_chk
  fi

  # Local file count 
  lcnt=`cat $hpsslist | wc -l`

  hpssout=$TEMPDIR/$name.$CDAY.chk.out
  hpssinvo=$TEMPDIR/$name.$CDAY.chk.inv
  $hpsstar inv $HPSSDIR/$hpssfile > $hpssout
  #XXW cat $hpssout | grep HTAR | grep -v CF_CHK | grep -v "HTAR SUCCESSFUL" > $hpssinvo
  cat $hpssout | grep HTAR: | grep -v CF_CHK | grep -v "HTAR SUCCESSFUL" |grep -v Listing > $hpssinvo

  # HPSS file count
  #hcnt=`cat $hpssinvo |  wc -l`
  hcnt=`cat $hpssinvo |  wc -l`

  # This should ALWAYS be the same
  if [[ $lcnt -ne $hcnt ]]; then
    echo "ERROR: local file cnt is $lcnt, hpss tar count is $hcnt"
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
#  /bin/rm -f $TEMPDIR/*

  ((rcall+=$rc))    # Accumulator for error code

  if [ $rw -ne 0 ] ; then
    echo "WARNING messages existed for $name - $CDAY"
  fi

  echo "END: ANALYSIS Archive - rc: $rc"

fi  # dohpssanl
###########################################################
# END ANALYSIS Archive
###########################################################






###########################################################
# CFS_HIC Archive
###########################################################
if [ $dohpsshic = "YES" ] ; then

  echo "BEGIN: CFS_HIC Archive"

  rc=0

  CDAY=$YYYYMMDD
  CDUMP=cdas1

  cd $COMANALYSIS

  name=hic

  # sanl sf are sigma, sfc bf are sfc

  rstlist='sanl sf00 sf06 sfcanl bf00 bf06 abias abias_pc abias_air ocnanl.tar'
  rstlist='sanl sf00 sf06 sfcanl bf00 bf06 satang abias ocnanl.tar'
 
  for hh in $hhlist
  do

    IDATE=$CDAY$hh

    hpsslist=$TEMPDIR/$name.$IDATE.hpsslist
    > $hpsslist

    # Get the main files
    for type in $rstlist
    do
      file=$CDUMP.t${hh}z.$type
      /bin/ls $file >> $hpsslist
      if [[ $? -ne 0 ]] ; then
        echo "ERROR: $file does not exist"
        ((rc+=1))
      fi

     # $CHKFILEANLSH $type $file
     # ((rw+=$?))
    done


    ## noah restart files only for 00Z
    # cdas1.t00z.noah.rst
    if [ $hh = "00" ] ; then
      file=$CDUMP.t${hh}z.noah.rst
      /bin/ls $file >> $hpsslist
      if [[ $? -ne 0 ]] ; then
        echo "ERROR: $file does not exist"
        ((rc+=1))
      fi

     # $CHKFILEANLSH noah.rst $file
     # ((rw+=$?))
    fi

    # These only exist for 18Z
    # cdas1.t18z.bf06.LIS only for 18Z
    if [ $hh = "18" ] ; then

      PDATE=`$NDATE -24 $CDAY$hh`
      PYYYYMMDD=`echo $PDATE | cut -c1-8`
      PDIR=$COMROT/cdas.$PYYYYMMDD

      file=$PDIR/$CDUMP.t${hh}z.bf06.LIS
      /bin/ls $file >> $hpsslist
      if [[ $? -ne 0 ]] ; then
        echo "ERROR: $file does not exist"
        ((rc+=1))
      fi

     #  $CHKFILEANLSH bf06.LIS $file
     # ((rw+=$?))
    fi


    ################################
    # HPSS Section
    # CFS_HIC is done per cycle
    ################################

    HPSSDIR=$HPSSBASE
    $hpsstar mkd $HPSSDIR

    hpssfile=cfs.$name.$IDATE.tar

    $hpsstar $PUTCMD $HPSSDIR/$hpssfile `cat $hpsslist`
    if [[ $? -ne 0 ]] ; then
      ((rc+=1))
      echo "ERROR: hpsstar $PUTCMD $HPSSDIR/$hpssfile"
    fi

    # Local file count
    lcnt=`cat $hpsslist | wc -l`

    hpssout=$TEMPDIR/$name.$IDATE.chk.out
    hpssinvo=$TEMPDIR/$name.$IDATE.chk.inv
    $hpsstar inv $HPSSDIR/$hpssfile > $hpssout
    cat $hpssout | grep HTAR: | grep -v CF_CHK | grep -v "HTAR SUCCESSFUL" |grep -v Listing > $hpssinvo

    # HPSS file count
    hcnt=`cat $hpssinvo |  wc -l`

    # This should ALWAYS be the same
    if [[ $lcnt -ne $hcnt ]]; then
      echo "ERROR: local file cnt is $lcnt, hpss tar count is $hcnt"
      echo "Local filelist"
      echo "*************************"
      cat $hpsslist
      echo ""
      echo "HPSS filelist"
      echo "*************************"
      cat $hpssinvo
      ((rc+=1))
    fi

  done   ## hhlist

  # Clean up
#  /bin/rm -f $TEMPDIR/*

  ((rcall+=$rc))    # Accumulator for error code

  if [ $rw -ne 0 ] ; then
    echo "WARNING messages existed for $name - $CDAY"
  fi

  echo "END: CFS_HIC Archive - rc: $rc"

fi  # dohpsshic
###########################################################
# END CFS_HIC Archive
###########################################################








###########################################################
# CFS_LIC
###########################################################
if [ $dohpsslic = "YES" ] ; then

  echo "BEGIN: CFS_LIC Archive"

  rc=0

  CDAY=$YYYYMMDD
  CDUMP=gdas2
  CDAS=cdas1

  name=lic

  cd $ANALYSISTEMP

  hpsslist=$TEMPDIR/$name.$CDAY.hpsslist
  > $hpsslist

  
  # From COMFCST
  rstlista='siganl sfcanl'                          # every cycle gdas2 from forecast
  rstlistf='sigf sfcf noah.rst'   # fXX types, 00Z only

  # From COMANALYSIS
  rstlistg='abias'  

  for hh in $hhlist
  do

    IDATE=${CDAY}$hh

    # Always pull these from the member 01 control run
    member='01'
    COMDIR=$COMROT/cfs/cfs.$CDAY/$hh/6hrly_grib_$member

    ## Grab the gdas2 files from the forecast directory 
    for type in $rstlista
    do
      file=$type.$member.$IDATE
      if [[ -s $COMDIR/$file ]] ; then
        /bin/cp -p $COMDIR/$file $ANALYSISTEMP
        echo $file >> $hpsslist

       # $CHKFILESH $type $CDUMP $file
       # ((rc+=$?))

      else
        echo "ERROR: $file does not exist"
        ((rc+=1))
      fi
    done


    if [ $hh -eq 00 ] ; then

      for type in $rstlistf
      do

        if [ $type = "noah.rst" ] ; then
          file=$CDAS.t${hh}z.$type
          if [[ -s $COMANALYSIS/$file ]] ; then
            /bin/cp -p $COMANALYSIS/$file $ANALYSISTEMP
            echo $file >> $hpsslist

           # $CHKFILEANLSH $type $file
           # ((rc+=$?))

          else
            echo "ERROR: $file does not exist"
            ((rc+=1))
          fi

        # sig or sfc types 
        else

          ## Need 00 and 06
          for hhZ in 00 06
          do
            file=$type${CDAY}$hhZ.$member.$IDATE
            if [[ -s $COMDIR/$file ]] ; then
              /bin/cp -p $COMDIR/$file $ANALYSISTEMP
              echo $file >> $hpsslist

             # $CHKFILESH $type $CDUMP $file
             # ((rc+=$?))

            else
              echo "ERROR: $file does not exist"
              ((rc+=1))
            fi
          done
        fi

      done  # rstlistf

    fi  # hh -eq 00


    ## gdas types
    for type in $rstlistg
    do 

      ## COMANALYSIS 
      file=$CDAS.t${hh}z.$type
      if [[ -s $COMANALYSIS/$file ]] ; then
        /bin/cp -p $COMANALYSIS/$file $ANALYSISTEMP
        echo $file >> $hpsslist

       # $CHKFILEANLSH $type $file
       # ((rc+=$?))

      else
        echo "ERROR: $file does not exist"
        ((rc+=1))
      fi
    done
  done  # hh cycles


  #############################################
  # HPSS Section
  # CFS_LIC has all 4 cycles in one archive
  #############################################

  HPSSDIR=$HPSSBASE
  $hpsstar mkd $HPSSDIR

  hpssfile=cfs.$name.$CDAY.tar

  $hpsstar $PUTCMD $HPSSDIR/$hpssfile `cat $hpsslist`
  if [[ $? -ne 0 ]] ; then
    ((rc+=1))
    echo "ERROR: hpsstar $PUTCMD $HPSSDIR/$hpssfile"
  fi

  # Local file count
  lcnt=`cat $hpsslist | wc -l`

  hpssout=$TEMPDIR/$name.$IDATE.chk.out
  hpssinvo=$TEMPDIR/$name.$IDATE.chk.inv
  $hpsstar inv $HPSSDIR/$hpssfile > $hpssout

  cat $hpssout | grep HTAR: | grep -v CF_CHK | grep -v "HTAR SUCCESSFUL" |grep -v Listing > $hpssinvo

  # HPSS file count
  hcnt=`cat $hpssinvo |  wc -l`

  # This should ALWAYS be the same
  if [[ $lcnt -ne $hcnt ]]; then
    echo "ERROR: local file cnt is $lcnt, hpss tar count is $hcnt"
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
  # /bin/rm -f $TEMPDIR/*
  # /bin/rm -f $ANALYSISTEMP/*

  ((rcall+=$rc))    # Accumulator for error code

  echo "END: CFS_LIC Archive - rc: $rc"

fi   # dohpsslic
###########################################################
# END CFS_LIC Archive
###########################################################







###########################################################
# HIGH RES HOURLY Archive
###########################################################
if [ $dohpsshigh = "YES" ] ; then

  echo "BEGIN: HIGH RES HOURLYS Archive"

  rc=0

  namelist='sfluxgrbf pgrbh ipvgrbh ocngrbh'
 
  CDAY=$YYYYMMDD
  CDUMP=cdas1

  cd $COMANALYSIS

  for name in $namelist
  do

    # Determine expected number of records
    case $name in
      sfluxgrbf)
        fhlist='00 01 02 03 04 05 06 09'
        excntnl=8   # spl
        excnt0=104  # 00
        excnt1=106  # 01-09
      ;;
      pgrbh)
        fhlist='anl 00 01 02 03 04 05 06 09'
        excntnl=554 # nl
        excnt0=664  # 00
        excnt1=666  # 01-09
      ;;
      ipvgrbh)
        fhlist='anl 00 01 02 03 04 05 06 09'
        excntnl=129 # nl
        excnt0=130  # 00
        excnt1=130  # 01-09
      ;;

      ocngrbh)
        fhlist='01 02 03 04 05 06 09'
        excnt1=222
      ;;
      *)
        echo "ERROR: - unrecognized file type - $name"
        exit -8
      ;;
    esac

    hpsslist=$TEMPDIR/$name.$CDAY.hpsslist
    > $hpsslist

    for hh in $hhlist
    do

      # Get the spl analysis files manually for flux
      if [[ $name == "sfluxgrbf" ]] ; then

        excnt=$excntnl

        # splgrbanl
        file=$CDUMP.t${hh}z.splgrbanl.grib2
        ls $file >> $hpsslist
        if [[ $? -ne 0 ]] ; then
          echo "ERROR: $file does not exist"
          ((rc+=1))
        else

          # Check grib record counts
          rcnt=`$WGRIB2 -s $file | wc -l`
          ((rcnt+=0))   # turn into a real number
          if [ $rcnt -ne $excnt ] ; then
            echo "ERROR: expected $excnt records for $file, found $rcnt"
            ((rc+=1))
          fi
        fi

        file=$CDUMP.t${hh}z.splgrbf06.grib2
        ls $file >> $hpsslist
        if [[ $? -ne 0 ]] ; then
          echo "ERROR: $file does not exist"
          ((rc+=1))
        else
          # Check grib record counts
          rcnt=`$WGRIB2 -s $file | wc -l`
          ((rcnt+=0))   # turn into a real number
          if [ $rcnt -ne $excnt ] ; then
            echo "ERROR: expected $excnt records for $file, found $rcnt"
            ((rc+=1))
          fi
        fi
      fi   # name = flx


      # Get the forecast files
      for fh in $fhlist
      do

        if [[ $fh == "anl" ]] ; then
          excnt=$excntnl
        elif [[ $fh == "00" ]] ; then
          excnt=$excnt0
        else
          excnt=$excnt1
        fi
  
        file=$CDUMP.t${hh}z.${name}$fh.grib2
        ls $file >> $hpsslist
        if [[ $? -ne 0 ]] ; then
          echo "ERROR: $file does not exist"
          ((rc+=1))
        else
          # Check grib record counts
          rcnt=`$WGRIB2 -s $file | wc -l`
          ((rcnt+=0))   # turn into a real number
          if [ $rcnt -ne $excnt ] ; then
            echo "ERROR: expected $excnt records for $file, found $rcnt"
            ((rc+=1))
          fi
        fi
      done
    done  # hhlist


    #################
    # HPSS Section
    #################

    HPSSDIR=$HPSSBASE
    $hpsstar mkd $HPSSDIR

    hpssfile=cfs.$name.$CDAY.tar

    $hpsstar $PUTCMD $HPSSDIR/$hpssfile `cat $hpsslist`
    if [[ $? -ne 0 ]] ; then
      ((rc+=1))
      echo "ERROR: hpsstar $PUTCMD $HPSSDIR/$hpssfile"
    fi

    # Local file count
    lcnt=`cat $hpsslist | wc -l`

    hpssout=$TEMPDIR/$name.$CDAY.chk.out
    hpssinvo=$TEMPDIR/$name.$CDAY.chk.inv
    $hpsstar inv $HPSSDIR/$hpssfile > $hpssout
    cat $hpssout | grep HTAR: | grep -v CF_CHK | grep -v "HTAR SUCCESSFUL" |grep -v Listing > $hpssinvo

    # HPSS file count
    hcnt=`cat $hpssinvo |  wc -l`

    # This should ALWAYS be the same
    if [[ $lcnt -ne $hcnt ]]; then
      echo "ERROR: local file cnt is $lcnt, hpss tar count is $hcnt"
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

  ((rcall+=$rc))    # Accumulator for error code

  # Clean up
#  /bin/rm -f $TEMPDIR/*

  echo "END: HIGH RES HOURLYS Archive - rc: $rc"

fi   # dohpsshigh
###########################################################
# END HOURLY HIGH RES
###########################################################









###########################################################
# LOW RES HOURLY Archive
###########################################################
if [ $dohpsslow = "YES" ] ; then

  echo "BEGIN: LOW RES HOURLY Archive"

  rc=0

  namelist='sfluxgrbl pgrbl pgrbf ipvgrbl ocngrbf'

  CDAY=$YYYYMMDD
  CDUMP=cdas1

  cd $COMANALYSIS

  for name in $namelist
  do

    # Determine expected number of records
    case $name in
      sfluxgrbl)
        fhlist='00 01 02 03 04 05 06 09'
        excntnl=8   # spl
        excnt0=104  # 00
        excnt1=106  # 01-09
      ;;
      pgrbl | pgrbf )
        fhlist='anl 00 01 02 03 04 05 06 09'
        excntnl=554 # nl
        excnt0=664  # 00
        excnt1=666  # 01-09
      ;;
      ipvgrbl)
        fhlist='anl 00 01 02 03 04 05 06 09'
        excntnl=129 # nl
        excnt0=130  # 00
        excnt1=130  # 01-09
      ;;

      ocngrbf)
        fhlist='01 02 03 04 05 06 09'
        excnt1=222
      ;;
      *)
        echo "ERROR: - unrecognized file type - $name"
        exit -8
      ;;
    esac

    hpsslist=$TEMPDIR/$name.$CDAY.hpsslist
    > $hpsslist

    for hh in $hhlist
    do

      # Get the spl analysis files manually for flux
      if [[ $name == "sfluxgrbl" ]] ; then

        excnt=$excntnl

        # splgrblanl
        file=$CDUMP.t${hh}z.splgrblanl.grib2
        ls $file >> $hpsslist
        if [[ $? -ne 0 ]] ; then
          echo "ERROR: $file does not exist"
          ((rc+=1))
        else

          # Check grib record counts
          rcnt=`$WGRIB2 -s $file | wc -l`
          ((rcnt+=0))   # turn into a real number
          if [ $rcnt -ne $excnt ] ; then
            echo "ERROR: expected $excnt records for $file, found $rcnt"
            ((rc+=1))
          fi
        fi

        file=$CDUMP.t${hh}z.splgrbl06.grib2
        ls $file >> $hpsslist
        if [[ $? -ne 0 ]] ; then
          echo "ERROR: $file does not exist"
          ((rc+=1))
        else
          # Check grib record counts
          rcnt=`$WGRIB2 -s $file | wc -l`
          ((rcnt+=0))   # turn into a real number
          if [ $rcnt -ne $excnt ] ; then
            echo "ERROR: expected $excnt records for $file, found $rcnt"
            ((rc+=1))
          fi
        fi
      fi   # name = flx

      # Get the forecast files
      for fh in $fhlist
      do

        if [[ $fh == "anl" ]] ; then
          excnt=$excntnl
        elif [[ $fh == "00" ]] ; then
          excnt=$excnt0
        else
          excnt=$excnt1
        fi

        file=$CDUMP.t${hh}z.${name}$fh.grib2
        
        # Special treatment for the pgrbanl file
        if [[ $name == "pgrbf" && $fh == "anl" ]]; then
           name1=pgrb
           file=$CDUMP.t${hh}z.${name1}$fh.grib2
        fi

        ls $file >> $hpsslist
        if [[ $? -ne 0 ]] ; then
          echo "ERROR: $file does not exist"
          ((rc+=1))
        else
          # Check grib record counts
          rcnt=`$WGRIB2 -s $file | wc -l`
          ((rcnt+=0))   # turn into a real number
          if [ $rcnt -ne $excnt ] ; then
            echo "ERROR: expected $excnt records for $file, found $rcnt"
            ((rc+=1))
          fi
        fi
      done
    done  # hhlist


    #################
    # HPSS Section
    #################

    HPSSDIR=$HPSSBASE
    $hpsstar mkd $HPSSDIR

    hpssfile=cfs.$name.$CDAY.tar

    $hpsstar $PUTCMD $HPSSDIR/$hpssfile `cat $hpsslist`
    if [[ $? -ne 0 ]] ; then
      ((rc+=1))
      echo "ERROR: hpsstar $PUTCMD $HPSSDIR/$hpssfile"
    fi

    # Local file count
    lcnt=`cat $hpsslist | wc -l`

    hpssout=$TEMPDIR/$name.$CDAY.chk.out
    hpssinvo=$TEMPDIR/$name.$CDAY.chk.inv
    $hpsstar inv $HPSSDIR/$hpssfile > $hpssout
    cat $hpssout | grep HTAR: | grep -v CF_CHK | grep -v "HTAR SUCCESSFUL" |grep -v Listing > $hpssinvo

    # HPSS file count
    hcnt=`cat $hpssinvo |  wc -l`

    # This should ALWAYS be the same
    if [[ $lcnt -ne $hcnt ]]; then
      echo "ERROR: local file cnt is $lcnt, hpss tar count is $hcnt"
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

  ((rcall+=$rc))    # Accumulator for error code

  # Clean up
#  /bin/rm -f $TEMPDIR/*

  echo "END: LOW RES HOURLY Archive - rc: $rc"

fi   # dohpsslow
###########################################################
# END GRBLOW Archive
###########################################################






###########################################################
# GDAS2 Archive - gdas2 forecast
###########################################################
if [ $dohpssgdas2 = "YES" ] ; then

  echo "BEGIN: GDAS2 Archive"

  rc=0

  name=gdas2

  CDUMP=gdas2

  CDAY=$YYYYMMDD
  CDATE=${YYYYMMDD}00

  fhlist='00 06 12 18 24 30 36 42 48 60 72 84 96 108 120'
  fhlist1='06 12 18 24 30 36 42 48 60 72 84 96 108 120'

  ## FIX - sigf and sfcf are only present for 00 and 06
  typelist='sig sfc pgb ipv flx'

  cd $HRLY6DIR

  hpsslist=$TEMPDIR/$name.$CDAY.hpsslist
  > $hpsslist

  for type in $typelist
  do

    # Get the analysis type files
    ##############################

    if [ $type = "sig" -o $type = "sfc" ] ; then
      file=${type}anl.$member.$CDATE
      if [ -s $file ] ; then
        /bin/ls $file >> $hpsslist
      else
        echo "ERROR: $file does not exist"
        ((rc+=1))
      fi

      #$CHKFILESH $type $CDUMP $file
      #((rc+=$?))
    fi


    if [ $type = "pgb" -o $type = "ipv" ] ; then
      file=${type}anl.$member.$CDATE.grb2
      if [ -s $file ] ; then
        /bin/ls $file >> $hpsslist
      else
        echo "ERROR: $file does not exist"
        ((rc+=1))
      fi

      ## FIX - Add check for pgbanl and ipvanl - grib types
    fi

    # Get the forecast type files
    ##############################

    for fh in $fhlist
    do

      # Use the forecast date instead of the forecast hour in the filenames
      vdate=`$NDATE $fh $CDATE`

      if [ $type = "sig" -o $type = "sfc" ] ; then
        file=${type}f$vdate.$member.$CDATE
      else
        file=${type}f$vdate.$member.$CDATE.grb2
      fi
      if [ -s $file ] ; then
        /bin/ls $file >> $hpsslist
      else
        echo "ERROR: $file does not exist"
        ((rc+=1))
      fi

      ## FIX - check checker
     # $CHKFILESH $type $CDUMP $file
     # ((rc+=$?))
    done

  done  # typelist


  # Get the ocn forecast files
  #############################
  type=ocnf

  for fh in $fhlist1
  do

    # Use the forecast date instead of the forecast hour in the filenames
    vdate=`$NDATE $fh $CDATE`

    file=${type}$vdate.$member.$CDATE.grb2
    if [ -s $file ] ; then
      /bin/ls $file >> $hpsslist
    else
      echo "ERROR: $file does not exist"
      ((rc+=1))
    fi
  done


  ################
  # HPSS Section
  ################

  HPSSDIR=$HPSSBASE
  $hpsstar mkd $HPSSDIR

  hpssfile=cfs.$name.$CDAY.tar

  $hpsstar $PUTCMD $HPSSDIR/$hpssfile `cat $hpsslist`
  if [[ $? -ne 0 ]] ; then
    ((rc+=1))
    echo "ERROR: hpsstar $PUTCMD $HPSSDIR/$hpssfile"
  fi

  # Local file count
  lcnt=`cat $hpsslist | wc -l`

  hpssout=$TEMPDIR/$name.$CDAY.chk.out
  hpssinvo=$TEMPDIR/$name.$CDAY.chk.inv
  $hpsstar inv $HPSSDIR/$hpssfile > $hpssout
  cat $hpssout | grep HTAR: | grep -v CF_CHK | grep -v "HTAR SUCCESSFUL" |grep -v Listing > $hpssinvo

  # HPSS file count
  hcnt=`cat $hpssinvo |  wc -l`

  # This should ALWAYS be the same
  if [[ $lcnt -ne $hcnt ]]; then
    echo "ERROR: local file cnt is $lcnt, hpss tar count is $hcnt"
    echo "Local filelist"
    echo "*************************"
    cat $hpsslist
    echo ""
    echo "HPSS filelist"
    echo "*************************"
    cat $hpssinvo
    ((rc+=1))
  fi


  ((rcall+=$rc))    # Accumulator for error code

  # Clean up
#  /bin/rm -f $TEMPDIR/*

  echo "END: GDAS2 Archive - rc: $rc"

fi   # dohpssgdas2
###########################################################
# END GDAS2 Archive
###########################################################




###########################################################
# DUMPS Archive
###########################################################
if [ $dohpssdumps = "YES" ] ; then

  echo "BEGIN: DUMPS Archive"

  name=dumps

  CDAY=$YYYYMMDD
  CDUMP=cdas1

  dumptypes='sstgrb  snogrb  snogrb_t574  engicegrb  cmapgrb  prepbufr tmpprf.tar salprf.tar'

  rc=0

  cd $COMANALYSIS
  hpsslist=$TEMPDIR/$name.$CDAY.hpsslist
  > $hpsslist

  ######
  # Add all the bufrd_d files
  ######

  type=bufr_d

  # Preserve any symbolic links here
  /bin/ls -1 *$type >> $hpsslist
  if [[ $? -ne 0 ]] ; then
    echo "ERROR: $type dump data not found $COMANALYSIS"
    ((rc+=1))
  fi


  # Copy all four cycles of the specific types
  for hh in $hhlist
  do

    for type in $dumptypes
    do

      file=$CDUMP.t${hh}z.$type

      if [ -s $file ] ; then
        /bin/ls -1 $file >> $hpsslist
      else

        # cmapgrb is only there if updated by CPC - tmpprf and salprf ocean data only at 00z
        if [[ $type == "cmapgrb" || $type == "tmpprf.tar" || $type == "salprf.tar" ]] ; then
          echo "WARNING: $file not found."
        else
          echo "ERROR: $file not found."
          ((rc+=1))
        fi
      fi

    done

  done

  #################
  # HPSS Section
  #################

  # Add all files copied to hpsslist

  HPSSDIR=$HPSSBASE

  hpssfile=cfs.$name.$CDAY.tar

  $hpsstar $PUTCMD $HPSSDIR/$hpssfile `cat $hpsslist`
  if [[ $? -ne 0 ]] ; then
    echo "ERROR: hpsstar $PUTCMD $HPSSDIR/$hpssfile"
    ((rc+=1))
  fi

  ## ERROR Checking

  # Local file count
  lcnt=`cat $hpsslist | wc -l`

  hpssout=$TEMPDIR/$name.$CDAY.chk.out
  hpssinvo=$TEMPDIR/$name.$CDAY.chk.inv
  $hpsstar inv $HPSSDIR/$hpssfile > $hpssout
  cat $hpssout | grep HTAR: | grep -v CF_CHK | grep -v "HTAR SUCCESSFUL" |grep -v Listing > $hpssinvo


  # HPSS file count
  hcnt=`cat $hpssinvo |  wc -l`

  # This should ALWAYS be the same
  if [[ $lcnt -ne $hcnt ]]; then
    echo "ERROR: local file cnt is $lcnt, hpss tar count is $hcnt"
    echo "Local filelist"
    echo "*************************"
    cat $hpsslist
    echo ""
    echo "HPSS filelist"
    echo "*************************"
    cat $hpssinvo
    ((rc+=1))
  fi

  ((rcall+=$rc))    # Accumulator for error code

  # Clean up
  cd $COMANALYSIS
#  /bin/rm -f $TEMPDIR/*

  echo "END: DUMPS Archive - rc: $rc"

fi   # dohpssdumps
###########################################################
# END of DUMPS Archive
###########################################################






###########################################################
# OCNDIAG Archive
###########################################################
if [ $doocndiag = "YES" ] ; then

  name=ocndiag
  fout=24
  CDUMP=gdas
  CDAS=cdas1

  CDAY=$YYYYMMDD

  rc=0

  cd $COMANALYSIS
  hpsslist=$TEMPDIR/$name.$CDAY.hpsslist
  > $hpsslist

  for hh in 00  ##$hhlist ocean analysis only done at 00z
  do

    indate=${CDAY}$hh
    odate=`$NDATE 6 $indate`
    oyyyy=`echo $odate | cut -c1-4`
    omm=`echo $odate | cut -c5-6`
    odd=`echo $odate | cut -c7-8`
    ohh=`echo $odate | cut -c9-10`

    #---------------------------------------------------------
    file=$CDAS.t${hh}z.tsoanl.nc
    expsize=95651852

    if [[ -s $file ]] ; then
      /bin/ls $file >> $hpsslist
      if [ $expsize -ne $(/bin/ls -l $file |awk '{print $5}') ] ; then
        echo "ERROR: $file is wrong size"
        ((rc+=1))
      fi
    else
      echo "ERROR: $file does not exist"
      ((rc+=1))
    fi


    #---------------------------------------------------------
    file=$CDAS.t${hh}z.tsoges.nc
    expsize=95651852

    if [[ -s $file ]] ; then
      /bin/ls $file >> $hpsslist
      if [ $expsize -ne $(/bin/ls -l $file |awk '{print $5}') ] ; then
        echo "ERROR: $file is wrong size"
        ((rc+=1))
      fi
    else
      echo "ERROR: $file does not exist"
      ((rc+=1))
    fi


    #---------------------------------------------------------
    file=$CDAS.t${hh}z.uvoanl.nc
    expsize=94470740

    if [[ -s $file ]] ; then
      /bin/ls $file >> $hpsslist
      if [ $expsize -ne $(/bin/ls -l $file |awk '{print $5}') ] ; then
        echo "ERROR: $file is wrong size"
        ((rc+=1))
      fi
    else
      echo "ERROR: $file does not exist"
      ((rc+=1))
    fi


    #---------------------------------------------------------
    file=$CDAS.t${hh}z.uvoges.nc
    expsize=94470740

    if [[ -s $file ]] ; then
      /bin/ls $file >> $hpsslist
      if [ $expsize -ne $(/bin/ls -l $file |awk '{print $5}') ] ; then
        echo "ERROR: $file is wrong size"
        ((rc+=1))
      fi
    else
      echo "ERROR: $file does not exist"
      ((rc+=1))
    fi


    #---------------------------------------------------------
    file=$CDAS.t${hh}z.moch06m
    expsize=329632

    if [[ -s $file ]] ; then
      /bin/ls $file >> $hpsslist
      if [ $expsize -ne $(/bin/ls -l $file |awk '{print $5}') ] ; then
        echo "ERROR: $file is wrong size"
        ((rc+=1))
      fi
    else
      echo "ERROR: $file does not exist"
      ((rc+=1))
    fi


    #---------------------------------------------------------
    file=$CDAS.t${hh}z.ocngrbh06m
    excnt=342

    if [[ -s $file ]] ; then
      /bin/ls $file >> $hpsslist
      gcnt=`$WGRIB -s $file | wc -l`
      if [[ $gcnt -ne $excnt ]] ; then
        echo "ERROR: $file contains $gcnt records - expected $excnt"
        ((rc+=1))
      fi
    else
      echo "ERROR: $file does not exist"
      ((rc+=1))
    fi


    #---------------------------------------------------------
    file=ocn_${oyyyy}_${omm}_${odd}_${ohh}.$CDUMP.${indate}m.nc
#   expsize=390862868
    expsize=390862884

    if [[ -s $file ]] ; then
      /bin/ls $file >> $hpsslist
      if [ $expsize -ne $(/bin/ls -l $file |awk '{print $5}') ] ; then
        echo "ERROR: $file is wrong size"
        ((rc+=1))
      fi
    else
      echo "ERROR: $file does not exist"
      ((rc+=1))
    fi


    #---------------------------------------------------------
    file=ice_${oyyyy}_${omm}_${odd}_${ohh}.$CDUMP.${indate}m.nc
#   expsize=22454900
    expsize=22454916

    if [[ -s $file ]] ; then
      /bin/ls $file >> $hpsslist
      if [ $expsize -ne $(/bin/ls -l $file |awk '{print $5}') ] ; then
        echo "ERROR: $file is wrong size"
        ((rc+=1))
      fi
    else
      echo "ERROR: $file does not exist"
      ((rc+=1))
    fi

  done  # hhlist


  #################
  # HPSS Section
  #################

  HPSSDIR=$HPSSBASE

  hpssfile=cfs.$name.$CDAY.tar

  $hpsstar $PUTCMD $HPSSDIR/$hpssfile `cat $hpsslist`
  if [[ $? -ne 0 ]] ; then
    echo "ERROR: hpsstar $PUTCMD $HPSSDIR/$hpssfile"
    ((rc+=1))
  fi

  # Local file count
  lcnt=`cat $hpsslist | wc -l`

  hpssout=$TEMPDIR/$name.$CDAY.chk.out
  hpssinvo=$TEMPDIR/$name.$CDAY.chk.inv
  $hpsstar inv $HPSSDIR/$hpssfile > $hpssout
  cat $hpssout | grep HTAR: | grep -v CF_CHK | grep -v "HTAR SUCCESSFUL" |grep -v Listing > $hpssinvo

  # HPSS file count
  hcnt=`cat $hpssinvo |  wc -l`

  # This should ALWAYS be the same
  if [[ $lcnt -ne $hcnt ]]; then
    echo "ERROR: local file cnt is $lcnt, hpss tar count is $hcnt"
    echo "Local filelist"
    echo "*************************"
    cat $hpsslist
    echo ""
    echo "HPSS filelist"
    echo "*************************"
    cat $hpssinvo
    ((rc+=1))
  fi

  ((rcall+=$rc))    # Accumulator for error code

  # Clean up
#  /bin/rm -f $TEMPDIR/*

  echo "END: OCN DIAGNOSTICS Archive - rc: $rc"

fi   # doocndiag
###########################################################
# END of OCNDIAG Archive
###########################################################

export err=$rcall; err_chk
exit $rcall

# Clean up
#cd $COMANALYSIS
#rm -Rf $TEMPDIR
