#!/usr/bin/env bash
##################################################################################
# The purpose of this script is to remove or thin the old CDAS/CFS  directories 
# to maintain a minimum usage of disk space for the CDAS/CFS system
##################################################################################
set -x

#####################################################################
# START FLOW OF CONTROL
# 
# 1) Remove the /com directory after 45 days
# 2) Thin the CDAS directory: 
#   -- Only keep the pgrbh, ipvh, flxf and ocnh files for 45 days 
#   -- All the other files will be deleted after 7 days
# 3) Thin CFS directory:
#   -- Only keep the 6hrly_grib directory for 7 days
# 4) Remove the $GESROOT directories after 3 days
#####################################################################

cd $DATA
msg="JOB $job HAS BEGUN"
postmsg "$jlogfile" "$msg"

export rm_com=${rm_com:-yes}
export clean_com=${clean_com:-yes}
export thin_com_cdas=${thin_com_cdas:-yes}
export thin_com_cfs=${thin_com_cfs:-yes}
#export rm_nwges=${rm_nwges:-yes}

##########################################

#########################################################################
#
# 1.  Clean /com directories
#
#########################################################################
if test "$rm_com" = "yes"
then
  set -u; echo $COMROT; set +u
  DAYS_TO_KEEP=${DAYS_TO_KEEP:-45}
  HOURS_TO_KEEP=`expr $DAYS_TO_KEEP \* 24`      
  date_45=$($NDATE -$HOURS_TO_KEEP $PDY$cyc)
  day_45=`echo $date_45 |cut -c1-8`
  rm -rf $COMROT/cdas.$day_45
  rm -rf $COMROT/cfs.$day_45
  
  ls -1d $COMROT/cdas.* $COMROT/cfs.* >rm_dirlist
  for dir in `cat rm_dirlist`
  do
     pday=$(echo $dir |awk ' BEGIN { FS="."} { print $NF }')
     if [ $pday -lt $day_45 ]; then
     rm -rf $dir
     fi
  done
fi

#########################################################################
#
# 2.  Thin /com directories for CDAS
#
#########################################################################
if test "$thin_com_cdas" = "yes"
then
  days_to_keep_cdas=${days_to_keep_cdas:-15}
  keepfile=$PARMcfs/cfs_parm_am/cfs_cdas_keeplist
  last_day_to_keep_cdas=`finddate.sh $PDY d-$days_to_keep_cdas`
  ls -1d $COMROT/cdas.* >dirlist
  rm -f thin_dirlist
  for dir in `cat dirlist`
  do
     pday=$(echo $dir |awk ' BEGIN { FS="."} { print $NF }')
     if test $pday -lt $last_day_to_keep_cdas
     then
        echo $dir >>thin_dirlist
     fi
  done

  for dir in `cat thin_dirlist`
  do
    echo "Thinning Directory $dir"
    cat /dev/null > keeplist
    for fil in `cat $keepfile`
    do
      ls $dir/$fil >>keeplist
    done

    for fil in `ls $dir`
    do
      if  test "`grep $fil keeplist`" = ""
      then
         rm -rf $dir/$fil
      fi
    done

    rm $dir/*.grib2
  done
fi

#########################################################################
#
# 3.  Thin /com directories for CFS
#
#########################################################################
if test "$thin_com_cfs" = "yes"
then
  days_to_keep_cfs=${days_to_keep_cfs:-7}
  last_day_to_keep_cfs=`finddate.sh $PDY d-$days_to_keep_cfs`
  ls -1d $COMROT/cfs.* >dirlist
  rm -f thin_dirlist
  for dir in `cat dirlist`
  do
     pday=$(echo $dir |awk ' BEGIN { FS="."} { print $NF }')
     if test $pday -lt $last_day_to_keep_cfs
     then
        echo $dir >>thin_dirlist
     fi
  done

  for dir in `cat thin_dirlist`
  do
    echo "Thinning Directory $dir"
    for mem in 01 02 03 04
    do
      for rcyc in 00 06 12 18
      do
        rm -rf $dir/${rcyc}/6hrly_grib_${mem}
      done
    done
  done
fi

#########################################################################
#
# 4. Clean up the $GESROOT directories
#
#########################################################################
#days_to_keep_nwges=${days_to_keep_nwges:-3}
#gespath=${gespath:-$GESROOT/${envir}}
#rm_nwges_day=`finddate.sh $PDY d-$days_to_keep_nwges`

#if [ $rm_nwges = yes ]; then
#  cd $gespath
#  rm -rf cdas.$rm_nwges_day
#fi

#####################################################################

msg="JOB $job HAS COMPLETED NORMALLY."
echo $msg
postmsg "$jlogfile" "$msg"

############## END OF SCRIPT #######################
