#!/bin/ksh
set -x

######################################################################
#  This script extracts selected grib fields from pgb, flx, ipv or ocn files
#  Originally written by Shrinivas Moorthi
#  Bob Kistler - modified to do CFSRR hourly assimilation files
#  Patrick Tripp - Updated Sept 2010
######################################################################

#export MP_STDOUTMODE=ordered
#export MP_LABELIO=yes
export APRUN=${APRUN:-mpirun}

export yyyymm=${1:-${yyyymm:-$yyyy$mm}}
export inp_file=${2:-${inp_file:-pgbf}}
export CDUMP=${3:-$CDUMP}
export fout=${4:-${fout:-${fhout:-${FHOUT:-6}}}}
export INDIR=${5:-$INDIR}
export TIMEDIR=${6:-${TIMEDIR:-$INDIR}}
export maketim=${7:-${maketim:-1}}
export makelow=${8:-${makelow:-1}}
export makefix=${9:-${makefix:-1}}

dds=01
yyyy=$(echo $yyyymm|cut -c1-4)
mm=$(echo $yyyymm|cut -c5-6)
dde=`$USHcfs/cfs_daysinmonth.sh ${yyyy} $mm`

sdate=${yyyymm}${dds}00
edate=${yyyymm}${dde}18
cdate=$sdate

export RUNDIR=${RUNDIR:-/stmp/$LOGNAME/cfs_analysis_ts.$$}
mkdir -p $RUNDIR

echo "yyyymm is $yyyymm"
echo "CDUMP is $CDUMP"
echo "fout is $fout"
echo "INDIR is $INDIR"
echo "TIMEDIR is $TIMEDIR"

####   37 pgb records

pgbvlst_37='z200 z500 z700 z850 z1000 t2 t50 t200 t500 t700 t850 t1000 u200 v200 u500 v500 u700 v700 u850 v850 u1000 v1000 psi200 psi850 chi200 chi850 vvel500 q500 q700 q850 q925 prmsl u925 v925 u250 v250 t250'

pgbkpd_37=':kpds5=7:kpds6=100:kpds7=200:   :kpds5=7:kpds6=100:kpds7=500: :kpds5=7:kpds6=100:kpds7=700:   :kpds5=7:kpds6=100:kpds7=850: :kpds5=7:kpds6=100:kpds7=1000:  :kpds5=11:kpds6=100:kpds7=2: :kpds5=11:kpds6=100:kpds7=50:   :kpds5=11:kpds6=100:kpds7=200: :kpds5=11:kpds6=100:kpds7=500:  :kpds5=11:kpds6=100:kpds7=700: :kpds5=11:kpds6=100:kpds7=850:  :kpds5=11:kpds6=100:kpds7=1000: :kpds5=33:kpds6=100:kpds7=200:  :kpds5=34:kpds6=100:kpds7=200: :kpds5=33:kpds6=100:kpds7=500:  :kpds5=34:kpds6=100:kpds7=500: :kpds5=33:kpds6=100:kpds7=700:  :kpds5=34:kpds6=100:kpds7=700: :kpds5=33:kpds6=100:kpds7=850:  :kpds5=34:kpds6=100:kpds7=850: :kpds5=33:kpds6=100:kpds7=1000: :kpds5=34:kpds6=100:kpds7=1000: :kpds5=35:kpds6=100:kpds7=200:  :kpds5=35:kpds6=100:kpds7=850: :kpds5=36:kpds6=100:kpds7=200:  :kpds5=36:kpds6=100:kpds7=850: :kpds5=39:kpds6=100:kpds7=500: :kpds5=51:kpds6=100:kpds7=500:  :kpds5=51:kpds6=100:kpds7=700: :kpds5=51:kpds6=100:kpds7=850:  :kpds5=51:kpds6=100:kpds7=925: :kpds5=2:kpds6=102:kpds7=0: :kpds5=33:kpds6=100:kpds7=925:  :kpds5=34:kpds6=100:kpds7=925: :kpds5=33:kpds6=100:kpds7=250:  :kpds5=34:kpds6=100:kpds7=250:  :kpds5=11:kpds6=100:kpds7=250:'

####   37 flx records

# nddsf srweq vddsf not in cdas analysis

flxvlst_37='lhtfl shtfl ustrs vstrs prate pressfc pwat tmp2m  tmpsfc tmphy1 snohf u10m v10m dlwsfc dswsfc ulwsfc ulwtoa uswsfc uswtoa soilm1 soilm2 soilm3 soilm4 soilt1 gflux weasd runoff tmin tmax q2m icecon icethk      csusf csdsf csdlf cprat tcdcclm'

flxkpd_37=':kpds5=121:kpds6=1:kpds7=0: :kpds5=122:kpds6=1:kpds7=0: :kpds5=124:kpds6=1:kpds7=0: :kpds5=125:kpds6=1:kpds7=0: :kpds5=59:kpds6=1:kpds7=0: :kpds5=1:kpds6=1:kpds7=0: :kpds5=54:kpds6=200:kpds7=0: :kpds5=11:kpds6=105:kpds7=2: :kpds5=11:kpds6=1:kpds7=0: :kpds5=11:kpds6=109:kpds7=1: :kpds5=229:kpds6=1:kpds7=0: :kpds5=33:kpds6=105:kpds7=10: :kpds5=34:kpds6=105:kpds7=10: :kpds5=205:kpds6=1:kpds7=0: :kpds5=204:kpds6=1:kpds7=0: :kpds5=212:kpds6=1:kpds7=0: :kpds5=212:kpds6=8:kpds7=0: :kpds5=211:kpds6=1:kpds7=0: :kpds5=211:kpds6=8:kpds7=0: :kpds5=144:kpds6=112:kpds7=10: :kpds5=144:kpds6=112:kpds7=2600: :kpds5=144:kpds6=112:kpds7=10340: :kpds5=144:kpds6=112:kpds7=25800: :kpds5=11:kpds6=112:kpds7=10: :kpds5=155:kpds6=1:kpds7=0: :kpds5=65:kpds6=1:kpds7=0: :kpds5=90:kpds6=1:kpds7=0: :kpds5=16:kpds6=105:kpds7=2: :kpds5=15:kpds6=105:kpds7=2: :kpds5=51:kpds6=105:kpds7=2: :kpds5=91:kpds6=1:kpds7=0: :kpds5=92:kpds6=1:kpds7=0:    kpds5=160:kpds6=1:kpds7=0: kpds5=161:kpds6=1:kpds7=0: kpds5=163:kpds6=1:kpds7=0: kpds5=214:kpds6=1:kpds7=0: kpds5=71:kpds6=200:kpds7=0:'

#flxlists='lhtfl shtfl prate pressfc pwat tmp2m tmpsfc tmphy1 snohf dlwsfc dswsfc ulwsfc ulwtoa uswsfc uswtoa soilm1 soilm2 soilm3 soilm4 soilt1 gflux weasd runoff tmin tmax q2m icecon icethk'

#flxplist='soilm1 soilm2 soilm3 soilm4 weasd'

####   21 ocn records

ocnvlst_21='ocndt20c ocnheat ocnslh ocnsst ocnu5 ocnv5 ocnsal5 ocnu15 ocnv15 ocnt15 ocnsal15 ocnvv55 ocnmld ocndt2.5c ocndt5c ocndt10c ocndt15c ocndt25c ocndt28c ocnsild ocntchp'

ocnkpd_21=':kpds5=195:kpds6=235:kpds7=200: :kpds5=197:kpds6=236:kpds7=30: :kpds5=198:kpds6=1:kpds7=0: :kpds5=13:kpds6=160:kpds7=5: :kpds5=49:kpds6=160:kpds7=5: :kpds5=50:kpds6=160:kpds7=5: :kpds5=88:kpds6=160:kpds7=5: :kpds5=49:kpds6=160:kpds7=15: :kpds5=50:kpds6=160:kpds7=15: :kpds5=13:kpds6=160:kpds7=15: :kpds5=88:kpds6=160:kpds7=15: :kpds5=40:kpds6=160:kpds7=55: :kpds5=195:kpds6=237:kpds7=0: :kpds5=195:kpds6=235:kpds7=25: :kpds5=195:kpds6=235:kpds7=50: :kpds5=195:kpds6=235:kpds7=100: :kpds5=195:kpds6=235:kpds7=150: :kpds5=195:kpds6=235:kpds7=250: :kpds5=195:kpds6=235:kpds7=280: :kpds5=195:kpds6=238:kpds7=0: :kpds5=194:kpds6=239:kpds7=4:'

####   3 ipv records

ipvvlst_3='ipv450 ipv550 ipv650'

ipvkpd_3=':kpds5=4:kpds6=113:kpds7=450: :kpds5=4:kpds6=113:kpds7=550: :kpds5=4:kpds6=113:kpds7=650:'

# flx fields with duplicate kpd values - second grep used to extract averages

kpdave=':kpds5=121:kpds6=1:kpds7=0: :kpds5=122:kpds6=1:kpds7=0: :kpds5=229:kpds6=1:kpds7=0: :kpds5=205:kpds6=1:kpds7=0: :kpds5=204:kpds6=1:kpds7=0: :kpds5=212:kpds6=1:kpds7=0: :kpds5=212:kpds6=8:kpds7=0: :kpds5=211:kpds6=1:kpds7=0: :kpds5=211:kpds6=8:kpds7=0: :kpds5=155:kpds6=1:kpds7=0:' 


prefix=$(echo $inp_file | cut -c1-3)
if [ $prefix = pgb ] ; then
  [ $inp_file = pgbl ] && makelow=0
  varlist=${varlist:-$pgbvlst_37}
  kpdlist=${kpdlist:-$pgbkpd_37}
  set -A fh nl 00 01 02 03 04 05 06
elif [ $prefix = flx ] ; then
  [ $inp_file = flxl ] && makelow=0
  varlist=${varlist:-$flxvlst_37}
  kpdlist=${kpdlist:-$flxkpd_37}
  set -A fh nl 00 01 02 03 04 05 06
elif [ $prefix = ipv ] ; then
  [ $inp_file = ipvl ] && makelow=0
  varlist=${varlist:-$ipvvlst_3}
  kpdlist=${kpdlist:-$ipvkpd_3}
  set -A fh nl 00 01 02 03 04 05 06
elif [ $prefix = ocn ] ; then
  [ $inp_file = ocnl ] && makelow=0
  # varlist=${varlist:-$ocnvlst_13}
  # kpdlist=${kpdlist:-$ocnkpd_13}
  varlist=${varlist:-$ocnvlst_21}
  kpdlist=${kpdlist:-$ocnkpd_21}
  set -A fh 01 02 03 04 05 06
fi

totnum=$(echo $varlist | wc -w)
set -A all_kpdv  $totnum
set -A all_ofile $totnum
set -A ave_kpd   $totnum

n=0
for kpd in $kpdlist
do
  n=$((n+1))
  all_kpdv[n]=$kpd
done

rc=0

cd $RUNDIR


SUFIN=${SUFIN:-.$CDUMP}
SUFOUT=${SUFOUT:-$SUFIN.$yyyymm}
[ $inp_file = pgbl ] && SUFOUT=".l$SUFOUT"
[ $inp_file = flxl ] && SUFOUT=".l$SUFOUT"
[ $inp_file = ocnl ] && SUFOUT=".l$SUFOUT"
[ $inp_file = ipvl ] && SUFOUT=".l$SUFOUT"

echo "SUFIN is $SUFIN"
echo "SUFOUT is $SUFOUT"

if [ $machine = IBM ]; then
  nprocs=`poe hostname | wc -w`
elif [ $machine = WCOSS ]; then
  if [ -n "$LSB_PJL_TASK_GEOMETRY" ]; then
     nprocs=`echo $LSB_PJL_TASK_GEOMETRY | sed 's/[{}(),]/ /g' | wc -w`
  elif [ -n "$LSB_DJOB_NUMPROC" ]; then
     nprocs=$LSB_DJOB_NUMPROC
  else
     nprocs=1
  fi
else
  echo "nprocs has not been defined for platform $machine"
fi


# Create array outfile names
nvar=0
for var in $varlist 
do 
  nc=$(($(echo $var | wc -c)-1))
  fc=$(echo $var | cut -c1-1)
  f3=no
  if [ $nc -gt 3 ] ; then f3=$(echo $var | cut -c1-3) ; fi
  if [ $fc = u -a $f3 !=  ulw -a $f3 != usw ] ; then
    lev=$(echo $var | cut -c2-$nc)
    ofile=$TIMEDIR/wnd${lev}$SUFOUT
  elif [ $fc = v -a $f3 != vve -a $f3 != vdd ] ; then
    lev=$(echo $var | cut -c2-$nc)
    ofile=$TIMEDIR/wnd${lev}$SUFOUT
  else
    ofile=$TIMEDIR/${var}$SUFOUT
  fi

  all_ofile[$((nvar+=1))]=$ofile
done

if [ $maketim = 1 ] ;then
  #fhini=$($NHOUR $sdate $yyyymm)
  #fhmax=$($NHOUR $edate $yyyymm)
  #ntimes=$(((fhmax-fhini)/fout+1))

  tothr=$($NHOUR $edate $sdate)
  ((ntimes=tothr/fout+1))
  ncmd=$(((ntimes-1)/nprocs+1))
  if [ $ncmd -lt 1 ] ; then ncmd=1 ; fi
  > file_0
  n=0
  while [ $((n+=1)) -le $nprocs ]
  do
    echo set -x > invout_$n.sh
    chmod u+x invout_$n.sh
    echo "./invout_$n.sh" >> cmdfile_0
  done
  cat cmdfile_0

  nn=0
  while [ $cdate -le $edate ]
  do
    n=-1
    while [ $((n+=1)) -lt ${#fh[*]} ] 
    do
      FH=${fh[n]}
      ((nn+=1))
      if [ $prefix = flx -a ${fh[n]} = nl ] ; then
        [ $inp_file = flxf ] && ifile=$INDIR/splanl${SUFIN}.$cdate
        [ $inp_file = flxl ] && ifile=$INDIR/spllanl${SUFIN}.$cdate
      else
        ifile=$INDIR/${inp_file}${FH}${SUFIN}.$cdate
      fi
      ls -l $ifile
      ((rc+=$?))
      if [ -s $ifile ] ; then
        #np=$(((nn-1)/ncmd+1))
        ((np=$((nn-1))%nprocs+1))
        invout=$(basename $ifile).invout
        if [ ! -s $invout ] ; then
          echo $WGRIB $ifile \> $invout >> invout_$np.sh
          ((rc+=$?))
        fi
      else
        ((rc+=1))
        echo "$ifile does not exist"
      fi
    done
    cdate=$($NDATE $fout $cdate)
  done  # cdate -le edate

  echo 'Before poe for invout'
  date

  if [ -s cmdfile_0 ] ; then

    if [ $machine = IBM ]; then
      ntasks=$(echo $LOADL_PROCESSOR_LIST|wc -w)
      # only valid if count .le. 128
      [ $ntasks -eq 0 ] && ntasks=$(poe hostname|wc -l)
    elif [ $machine = WCOSS ]; then
      if [ -n "$LSB_PJL_TASK_GEOMETRY" ]; then
        ntasks=`echo $LSB_PJL_TASK_GEOMETRY | sed 's/[{}(),]/ /g' | wc -w`
      elif [ -n "$LSB_DJOB_NUMPROC" ]; then
        ntasks=$LSB_DJOB_NUMPROC
      else
        ntasks=1
      fi
    else
      echo "ntasks has not been defined for platform $machine"
    fi

    remainder=$((($ntasks-$(cat cmdfile_$ncmd|wc -l))%$ntasks))
    n=0
    while [ $((n+=1)) -le $remainder ]
    do
      echo "echo do nothing" >> cmdfile_$ncmd
    done
  fi
  np=0
  while [ $((np+=1)) -le $ncmd ] 
  do
    cat invout_$np.sh
  done

  #$APRUN -pgmmodel mpmd -cmdfile cmdfile_0
  cp cmdfile_0 cmdfile
  $APRUN cfp cmdfile  
  ((rc+=$?))
  if [ $rc -gt 0 ] ; then export err=$rc; err_chk ; else rm invout_*.sh ; rm cmdfile_0 ; fi

  echo ' After invout '
  date

  ncmd=$(((nvar-1)/nprocs+1))
  n=0
  while [ $((n+=1)) -le $ncmd ]
  do 
    [ -s cmdfile_$n ] && rm cmdfile_$n
    > cmdfile_$n
  done

  nn=0
  while [ $((nn+=1)) -le $nvar ]
  do
    echo set -x > script_$nn.sh
    chmod u+x script_$nn.sh
    ofile=${all_ofile[nn]}
    >$ofile

    np=$(((nn-1)/nprocs+1))
    echo "./script_$nn.sh" >> cmdfile_$np
  
    # pair wnd and wndstrs components together
    kpdv=${all_kpdv[nn]}
    # get value of kpds5
    eval $(echo $kpdv|awk -F: '{print $2}')
    if [ $kpds5 = 34 -o $kpds5 = 125 ] ; then 
      echo "echo do nothing" >> script_$nn.sh
      continue
    elif [ $kpds5 = 33 ] ; then 
      kpdv2=$(echo $kpdv|sed 's/=33/=34/')
    elif [ $kpds5 = 124 ] ;then 
      kpdv2=$(echo $kpdv|sed 's/=124/=125/')
    fi

    cdate=$sdate
    while [ $cdate -le $edate ]
    do
      n=-1
      while [ $((n+=1)) -lt ${#fh[*]} ] 
      do
        FH=${fh[n]}
        if [ $prefix = flx -a ${fh[n]} = nl ] ; then
          [ $inp_file = flxf ] && ifile=$INDIR/splanl${SUFIN}.$cdate
          [ $inp_file = flxl ] && ifile=$INDIR/spllanl${SUFIN}.$cdate
          var=$(basename $ofile|awk -F. '{print $1}')
          [ $var != tmphy1 -a $var != pressfc -a $var != pwat ] && continue
        else
          ifile=$INDIR/${inp_file}${FH}${SUFIN}.$cdate
        fi

        invout=$(basename $ifile).invout
        [  ! -s $invout ] && exit 1

        #if [ ${ave_kpd[nn]} = YES ] ; then
        #  start_byt=$(grep "$kpdv" $invout | grep "ave:NAve"| awk -F: '{print $2}')
        #else
        #  start_byt=$(grep "$kpdv" $invout | awk -F: '{print $2}')
        #fi
        start_byt=$(grep "$kpdv" $invout | awk -F: '{print $2}')

        echo $WGRIB $ifile -p $start_byt -grib -append -o $ofile >> script_$nn.sh
        if [ $kpds5 = 33 -o $kpds5 = 124 ] ;then 
          start_byt=$(grep "$kpdv2" $invout | awk -F: '{print $2}')
          echo $WGRIB $ifile -p $start_byt -grib -append -o $ofile >> script_$nn.sh
        fi
        ((rc+=$?))
      done
      cdate=$($NDATE $fout $cdate)
    done  # cdate -e edate
  done  # nvar

  echo Before POE
  date
  if [ -s cmdfile_$ncmd ] ; then
    if [ $machine = IBM ]; then
      ntasks=$(echo $LOADL_PROCESSOR_LIST|wc -w)
      # only valid if count .le. 128
      [ $ntasks -eq 0 ] && ntasks=$(poe hostname|wc -l)
    elif [ $machine = WCOSS ]; then
      if [ -n "$LSB_PJL_TASK_GEOMETRY" ]; then
        ntasks=`echo $LSB_PJL_TASK_GEOMETRY | sed 's/[{}(),]/ /g' | wc -w`
      elif [ -n "$LSB_DJOB_NUMPROC" ]; then
        ntasks=$LSB_DJOB_NUMPROC
      else
        ntasks=1
      fi
    else
      echo "ntasks has not been defined for platform $machine"
    fi

    remainder=$((($ntasks-$(cat cmdfile_$ncmd|wc -l))%$ntasks))
    n=0
    while [ $((n+=1)) -le $remainder ]
    do
      echo "echo do nothing" >> cmdfile_$ncmd
    done
  fi
  n=0
  while [ $((n+=1)) -le $ncmd ]
  do
    #$APRUN -pgmmodel mpmd -cmdfile cmdfile_$n
    cp cmdfile_$n cmdfile
    $APRUN cfp cmdfile 
    ((rc+=$?))
    export err=$rc; err_chk
  done
  rm script_*.sh ; rm cmdfile_* 
fi  # maketim

################################################################################

# fix for junk in tser files
if [ $makefix = 1 ] ; then

cat <<\EOF > tser.fix.sh
#!/bin/ksh
##set -x
if [ $# -ne 1 ] ; then echo "Usage:$0 grib";exit 1;fi
grib=$1
$WGRIB $grib >$grib.wgrib 2>/dev/null
>$grib.fix
>stdout.$$
while read line ;do
start_byt=$(echo $line|awk -F: '{print $2}')
$WGRIB $grib -p $start_byt -grib -append -o $grib.fix >>stdout.$$ 2>&1
done <$grib.wgrib
rm $grib.wgrib stdout.$$  $grib
mv $grib.fix $grib
EOF

  chmod 755 tser.fix.sh

  >ofilelist
  nn=0
  while [ $((nn+=1)) -le $nvar ]
  do
    echo ${all_ofile[nn]} >>ofilelist
  done
  wc -l ofilelist
  cat ofilelist |sort -u >ofilelist.sort
  wc -l ofilelist.sort

  n=0
  npoe=0
  > cmdfile_$npoe
  while read ofile ; do
    [ ! -s $ofile ] && exit 1
    echo "./tser.fix.sh $ofile" >> cmdfile_$npoe
    if [ $((n+=1)) -eq $nprocs ] ; then
      ((npoe+=1))
      > cmdfile_$npoe
      n=0
    fi
  done < ofilelist.sort

  if [ $n -gt 0 ] ; then
    while [ $((n+=1)) -le $nprocs ] ; do
      echo "echo do nothing" >>cmdfile_$npoe
    done
  fi

  n=-1;while [ $((n+=1)) -le $npoe ] ; do
    cat cmdfile_$n
    ###$APRUN -pgmmodel mpmd -cmdfile cmdfile_$n
    cp cmdfile_$n cmdfile
    $APRUN  cfp cmdfile
    export err=$?; err_chk
  done

  while read ofile ; do
    [ ! -s $ofile ] && exit 1
  done < ofilelist.sort

  rm ofilelist ofilelist.sort cmdfile_* tser.fix.sh

fi

################################################################################
if [ $makelow = 1 ] ; then
  $USHcfs/cfs_cdas_glbtime.low.sh $yyyymm $inp_file $CDUMP $fout $INDIR $TIMEDIR
fi
################################################################################

# Exit gracefully

exit $rc


