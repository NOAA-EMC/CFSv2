#!/bin/ksh
# set -xua
set -x

############################################
# Script to setup and compute monthly means for egy
# Originally written by Shrinivas Moorthi
#
# Updated by Patrick Tripp - Sept 2010
############################################

export OUTDIR=$MONTHDIR
cd $RUNDIR

errs=0

export dogribchk=${dogribchk:-YES}
echo "dogribchk is $dogribchk"

$AVGPKESH $cdate1 $cdate2 $nhours $prefix1 $prefix2 $LONB $LATB $LEVS $polist $fhini $fhmax $fhout
if [[ $? -ne 0 ]] ; then
  echo "ERROR: cfs_cdas_avg_pke.sh returned non-zero exit"
  ((errs+=1))
  err_exit $errs
fi

# Create low-res versions
if [[ $makelow -eq 1 ]] ; then

  if [ $type = "fcst" ] ; then

    for fh in $fhlist
    do
      ifile=$OUTDIR/${prefix1}${prefix2}${fh}$SUFOUT
      if [[ ! -s $ifile ]] ; then
        echo "ERROR: $ifile does not exist"
        ((errs+=1))
        err_exit $errs
      fi

      ofile=$OUTDIR/${prefix1}${prefixo}${fh}$SUFOUT
      /bin/rm -f $ofile
      /bin/rm -f $RUNDIR/pgb25a $RUNDIR/pgb25b

      $WGRIB $ifile|grep -i :kpds6=100:|$COPYGB -xkw -i4,0,35 -g$ogrid $ifile $RUNDIR/pgb25a
      if [[ $? -ne 0 ]] ; then
        echo "ERROR: copygb returned non-zero exit for $ifile $ofile"
        ((errs+=1))
        err_exit $errs
      fi

      $WGRIB $ifile|grep -vi :kpds6=100:|$COPYGB -xkw -i0 -g$ogrid $ifile $RUNDIR/pgb25b
      if [[ $? -ne 0 ]] ; then
        echo "ERROR: copygb returned non-zero exit for $ifile $ofile"
        ((errs+=1))
        err_exit $errs
      fi

      cat $RUNDIR/pgb25a $RUNDIR/pgb25b > $ofile
      if [[ $? -ne 0 ]] ; then
        echo "ERROR: cat returned non-zero exit for $ofile"
        ((errs+=1))
        err_exit $errs
      fi

      if [[ ! -s $ofile ]] ; then
        echo "ERROR: $ofile does not exist"
        ((errs+=1))
        err_exit $errs
      fi

    done
  else  # non fcst, no fhlist

    # analysis types

    ifile=$OUTDIR/${prefix1}${prefix2}$SUFOUT
    if [[ ! -s $ifile ]] ; then
      echo "ERROR: $ifile does not exist"
      ((errs+=1))
      err_exit $errs
    fi

    ofile=$OUTDIR/${prefixo}$SUFOUT
    /bin/rm -f $ofile
    /bin/rm -f $RUNDIR/pgb25a $RUNDIR/pgb25b

    $WGRIB $ifile|grep -i :kpds6=100:|$COPYGB -xkw -i4,0,35 -g$ogrid $ifile $RUNDIR/pgb25a
    if [[ $? -ne 0 ]] ; then
      echo "ERROR: copygb returned non-zero exit for $ifile $ofile"
      ((errs+=1))
      err_exit $errs
    fi

    $WGRIB $ifile|grep -vi :kpds6=100:|$COPYGB -xkw -i0 -g$ogrid $ifile $RUNDIR/pgb25b
    if [[ $? -ne 0 ]] ; then
      echo "ERROR: copygb returned non-zero exit for $ifile $ofile"
      ((errs+=1))
      err_exit $errs
    fi

    cat $RUNDIR/pgb25a $RUNDIR/pgb25b > $ofile
    if [[ $? -ne 0 ]] ; then
      echo "ERROR: cat returned non-zero exit for $ofile"
      ((errs+=1))
      err_exit $errs
    fi

    if [[ ! -s $ofile ]] ; then
      echo "ERROR: $ofile does not exist"
      ((errs+=1))
      err_exit $errs
    fi

  fi
fi

# Exit with error count

exit $errs

