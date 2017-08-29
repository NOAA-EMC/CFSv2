#!/usr/bin/env ksh  
set -eu

#  This script extracts selected grib fields from pgb, flx, ipv or diab files
#  Shrinivas Moorthi - April 2010

export APRUN=${APRUN:-mpirun.lsf}

if [ $RUN_ENVIR = dev ]; then
  export SHDIR=${SHDIR:-${HOMEDIR:-$BASEDIR}/bin}
  export PBEG=${PBEG:-$SHDIR/pbeg}
  export PERR=${PERR:-$SHDIR/perr}
  export PLOG=${PLOG:-$SHDIR/plog}
  export PEND=${PEND:-$SHDIR/pend}
fi

export CDATE=${1:-$CDATE}
export inp_file=${2:-${inp_file:-pgbf}}
#export CDUMP=${3:-$CDUMP}
export sdate=${3:-${sdate:-$CDATE}}
export fout=${4:-${fout:-${fhout:-${FHOUT:-6}}}}
export edate=${5:-${edate:-$($NDATE -$fout $(echo $($NDATE 768 $(echo $sdate | cut -c1-6)0100) | cut -c1-6)0100)}}
export INDIR=${6:-$INDIR}
export TIMEDIR=${7:-${TIMEDIR:-$INDIR}}
export RUNDIR=${RUNDIR:-${DATA:-/ptmp/$LOGNAME}/glbts_${sdate}_${edate}}
export CLEAN_RUNDIR=${CLEAN_RUNDIR:-NO}
export jn=${jn:-gbltseries}
export resl=${8:-f}
#export RUNLOG=${10:-$RUNLOG}

if [ $sdate -eq $CDATE ] ; then export sdate=$($NDATE $fout $CDATE) ; fi

date

if [ $RUN_ENVIR = dev ]; then
 echo "PSLOT is $PSLOT"
 echo "RUNLOG is $RUNLOG"
 echo "CDUMP is $CDUMP"
 $PLOG "$RUNLOG" OK "$jn begun for $PSLOT"
fi
echo "CDATE is $CDATE"
echo "fout is $fout"
echo "INDIR is $INDIR"
echo "TIMEDIR is $TIMEDIR"
echo "sdate is $sdate"
echo "edate is $edate"
echo "RUNDIR is $RUNDIR"


#$PLOG "$RUNLOG" OK "$jn begun for $PSLOT"
#-------------------------------------------------------------

mkdir -p $RUNDIR
if [ $CLEAN_RUNDIR = YES ] ; then rm -f $RUNDIR/* ; fi

####   35 pgb records

pgbvlst_35='z200 z500 z700 z850 z1000 t2 t50 t200 t500 t700 t850 t1000 u200 v200 u500 v500 u700 v700 u850 v850 u1000 v1000 psi200 psi850 chi200 chi850 vvel500 q500 q700 q850 q925 prmsl u925 v925 u250 v250 t250'

pgbkpd_35=':kpds5=7:kpds6=100:kpds7=200:   :kpds5=7:kpds6=100:kpds7=500: :kpds5=7:kpds6=100:kpds7=700:   :kpds5=7:kpds6=100:kpds7=850: :kpds5=7:kpds6=100:kpds7=1000:  :kpds5=11:kpds6=100:kpds7=2: :kpds5=11:kpds6=100:kpds7=50:   :kpds5=11:kpds6=100:kpds7=200: :kpds5=11:kpds6=100:kpds7=500:  :kpds5=11:kpds6=100:kpds7=700: :kpds5=11:kpds6=100:kpds7=850:  :kpds5=11:kpds6=100:kpds7=1000: :kpds5=33:kpds6=100:kpds7=200:  :kpds5=34:kpds6=100:kpds7=200: :kpds5=33:kpds6=100:kpds7=500:  :kpds5=34:kpds6=100:kpds7=500: :kpds5=33:kpds6=100:kpds7=700:  :kpds5=34:kpds6=100:kpds7=700: :kpds5=33:kpds6=100:kpds7=850:  :kpds5=34:kpds6=100:kpds7=850: :kpds5=33:kpds6=100:kpds7=1000: :kpds5=34:kpds6=100:kpds7=1000: :kpds5=35:kpds6=100:kpds7=200:  :kpds5=35:kpds6=100:kpds7=850: :kpds5=36:kpds6=100:kpds7=200:  :kpds5=36:kpds6=100:kpds7=850: :kpds5=39:kpds6=100:kpds7=500: :kpds5=51:kpds6=100:kpds7=500:  :kpds5=51:kpds6=100:kpds7=700: :kpds5=51:kpds6=100:kpds7=850:  :kpds5=51:kpds6=100:kpds7=925: :kpds5=2:kpds6=102:kpds7=0: :kpds5=33:kpds6=100:kpds7=925 :kpds5=34:kpds6=100:kpds7=925: :kpds5=33:kpds6=100:kpds7=250 :kpds5=34:kpds6=100:kpds7=250: :kpds5=11:kpds6=100:kpds7=250:'

####   40 flx records

flxvlst_40='lhtfl shtfl ustrs vstrs prate pressfc pwat tmp2m  tmpsfc tmphy1 snohf u10m v10m dlwsfc dswsfc ulwsfc ulwtoa uswsfc uswtoa soilm1 soilm2 soilm3 soilm4 soilt1 gflux weasd runoff tmin tmax q2m icecon icethk nddsf srweq vddsf csusf csdsf csdlf cprat tcdcclm'


flxkpd_40=':kpds5=121:kpds6=1:kpds7=0: :kpds5=122:kpds6=1:kpds7=0: :kpds5=124:kpds6=1:kpds7=0: :kpds5=125:kpds6=1:kpds7=0: :kpds5=59:kpds6=1:kpds7=0: :kpds5=1:kpds6=1:kpds7=0: :kpds5=54:kpds6=200:kpds7=0: :kpds5=11:kpds6=105:kpds7=2: :kpds5=11:kpds6=1:kpds7=0: :kpds5=11:kpds6=109:kpds7=1: :kpds5=229:kpds6=1:kpds7=0: :kpds5=33:kpds6=105:kpds7=10: :kpds5=34:kpds6=105:kpds7=10: :kpds5=205:kpds6=1:kpds7=0: :kpds5=204:kpds6=1:kpds7=0: :kpds5=212:kpds6=1:kpds7=0: :kpds5=212:kpds6=8:kpds7=0: :kpds5=211:kpds6=1:kpds7=0: :kpds5=211:kpds6=8:kpds7=0: :kpds5=144:kpds6=112:kpds7=10: :kpds5=144:kpds6=112:kpds7=2600: :kpds5=144:kpds6=112:kpds7=10340: :kpds5=144:kpds6=112:kpds7=25800: :kpds5=11:kpds6=112:kpds7=10: :kpds5=155:kpds6=1:kpds7=0: :kpds5=65:kpds6=1:kpds7=0: :kpds5=90:kpds6=1:kpds7=0: :kpds5=16:kpds6=105:kpds7=2: :kpds5=15:kpds6=105:kpds7=2: :kpds5=51:kpds6=105:kpds7=2: :kpds5=91:kpds6=1:kpds7=0: :kpds5=92:kpds6=1:kpds7=0: :kpds5=169:kpds6=1:kpds7=0: kpds5=64:kpds6=1:kpds7=0: kpds5=167:kpds6=1:kpds7=0: kpds5=160:kpds6=1:kpds7=0: kpds5=161:kpds6=1:kpds7=0: kpds5=163:kpds6=1:kpds7=0: kpds5=214:kpds6=1:kpds7=0: kpds5=71:kpds6=200:kpds7=0:'

#flxlists='lhtfl shtfl prate pressfc pwat tmp2m tmpsfc tmphy1 snohf dlwsfc dswsfc ulwsfc ulwtoa uswsfc uswtoa soilm1 soilm2 soilm3 soilm4 soilt1 gflux weasd runoff tmin tmax q2m icecon icethk'

#flxplist='soilm1 soilm2 soilm3 soilm4 weasd'

####   21 ocn records

#ocnvlst_13='ocndt20c ocnheat ocnslh ocnsst ocnu5 ocnv5 ocnsal5 ocnu15 ocnv15 ocnt15 ocnsal15 ocnvv55 ocnmld'

#ocnkpd_13=':kpds5=195:kpds6=235:kpds7=200: :kpds5=197:kpds6=236:kpds7=30: :kpds5=198:kpds6=1:kpds7=0: :kpds5=13:kpds6=160:kpds7=5: :kpds5=49:kpds6=160:kpds7=5: :kpds5=50:kpds6=160:kpds7=5: :kpds5=88:kpds6=160:kpds7=5: :kpds5=49:kpds6=160:kpds7=15: :kpds5=50:kpds6=160:kpds7=15: :kpds5=13:kpds6=160:kpds7=15: :kpds5=88:kpds6=160:kpds7=15: :kpds5=40:kpds6=160:kpds7=55: :kpds5=195:kpds6=237:kpds7=0:' 

ocnvlst_21='ocndt20c ocnheat ocnslh ocnsst ocnu5 ocnv5 ocnsal5 ocnu15 ocnv15 ocnt15 ocnsal15 ocnvv55 ocnmld ocndt2.5c ocndt5c ocndt10c ocndt15c ocndt25c ocndt28c ocnsild ocntchp'

ocnkpd_21=':kpds5=195:kpds6=235:kpds7=200: :kpds5=197:kpds6=236:kpds7=30: :kpds5=198:kpds6=1:kpds7=0: :kpds5=13:kpds6=160:kpds7=5: :kpds5=49:kpds6=160:kpds7=5: :kpds5=50:kpds6=160:kpds7=5: :kpds5=88:kpds6=160:kpds7=5: :kpds5=49:kpds6=160:kpds7=15: :kpds5=50:kpds6=160:kpds7=15: :kpds5=13:kpds6=160:kpds7=15: :kpds5=88:kpds6=160:kpds7=15: :kpds5=40:kpds6=160:kpds7=55: :kpds5=195:kpds6=237:kpds7=0: :kpds5=195:kpds6=235:kpds7=25: :kpds5=195:kpds6=235:kpds7=50: :kpds5=195:kpds6=235:kpds7=100: :kpds5=195:kpds6=235:kpds7=150: :kpds5=195:kpds6=235:kpds7=250: :kpds5=195:kpds6=235:kpds7=280: :kpds5=195:kpds6=238:kpds7=0: :kpds5=194:kpds6=239:kpds7=4:'

####   3 ipv records

ipvvlst_3='ipv450 ipv550 ipv650'

ipvkpd_3=':kpds5=4:kpds6=113:kpds7=450: :kpds5=4:kpds6=113:kpds7=550: :kpds5=4:kpds6=113:kpds7=650:'

# flx fields with duplicate kpd values - second grep used to extract averages

kpdave=':kpds5=121:kpds6=1:kpds7=0: :kpds5=122:kpds6=1:kpds7=0: :kpds5=229:kpds6=1:kpds7=0: :kpds5=205:kpds6=1:kpds7=0: :kpds5=204:kpds6=1:kpds7=0: :kpds5=212:kpds6=1:kpds7=0: :kpds5=212:kpds6=8:kpds7=0: :kpds5=211:kpds6=1:kpds7=0: :kpds5=211:kpds6=8:kpds7=0: :kpds5=155:kpds6=1:kpds7=0:' 

dup_rec=${dup_rec:-'3 4 12 15 16 33'}

prefix=$(echo $inp_file | cut -c1-3)
if [ $prefix = pgb ] ; then
 varlist=${varlist:-$pgbvlst_35}
 kpdlist=${kpdlist:-$pgbkpd_35}
elif [ $prefix = flx ] ; then
 varlist=${varlist:-$flxvlst_40}
 kpdlist=${kpdlist:-$flxkpd_40}
#elif [ $prefix = diab ] ; then
#varlist=${varlist:-$diablst_32}
#kpdlist=${kpdlist:-$diabkpd_32}
elif [ $prefix = ipv ] ; then
 varlist=${varlist:-$ipvvlst_3}
 kpdlist=${kpdlist:-$ipvkpd_3}
elif [ $prefix = ocn ] ; then
 #varlist=${varlist:-$ocnvlst_13}
 #kpdlist=${kpdlist:-$ocnkpd_13}
 varlist=${varlist:-$ocnvlst_21}
 kpdlist=${kpdlist:-$ocnkpd_21}
fi

totnum=$(echo $varlist | wc -w)
set -A all_kpdv  $totnum
set -A all_ofile $totnum
set -A ave_kpd   $totnum
n=0
for kpd in $kpdlist ; do
 n=$((n+1))
 all_kpdv[n]=$kpd
 ave_kpd[n]=NO
 if [ $prefix = flx ] ; then
  for kpd_av in $kpdave ; do
   if [ $kpd = $kpd_av ] ; then ave_kpd[n]=YES ; fi
  done
 fi
done


rc=0

cd $TIMEDIR


SUFIN=${SUFIN:-""}
#SUFIN=${SUFIN:-.gdas2.$CDATE}
if [ RUN_ENVIR = dev ] ; then
 SUFOUT=${SUFOUT:-.$sdate.$edate$SUFIN}
else
 SUFOUT=.$sdate.$edate$SUFIN
fi

echo "SUFIN is $SUFIN"
echo "SUFOUT is $SUFOUT"

if [ $machine = IBM ]; then
  nprocs=`poe hostname | wc -w`
elif [ $machine = WCOSS ]; then
  set +u
  if [ -n "$LSB_PJL_TASK_GEOMETRY" ]; then
     nprocs=`echo $LSB_PJL_TASK_GEOMETRY | sed 's/[{}(),]/ /g' | wc -w`
  elif [ -n "$LSB_DJOB_NUMPROC" ]; then
     nprocs=$LSB_DJOB_NUMPROC
  else
     nprocs=1
  fi
  set -u
elif [ $machine = WCRAY ]; then
     nprocs=24
else
  echo "nprocs has not been defined for platform $machine"
fi



nvar=0
for var in $varlist ; do
  nc=$(($(echo $var | wc -c)-1))
  fc=$(echo $var | cut -c1-1)
  f3=no
  if [ $nc -gt 3 ] ; then f3=$(echo $var | cut -c1-3) ; fi
  if [ $fc = u -a $f3 !=  ulw -a $f3 != usw ] ; then
    lev=$(echo $var | cut -c2-$nc)
    ofile=wnd${lev}_${resl}$SUFOUT
  elif [ $fc = v -a $f3 != vve -a $f3 != vdd ] ; then
    lev=$(echo $var | cut -c2-$nc)
    ofile=wnd${lev}_${resl}$SUFOUT
  else
    ofile=${var}_${resl}$SUFOUT
  fi
  if [ $fc != v -o $f3 = vve -o $f3 = vdd ] ; then  > $ofile ; fi

  ls -l $ofile
  nvar=$((nvar+1))
  all_ofile[nvar]=$ofile
done

fhini=$($NHOUR $sdate $CDATE)
fhmax=$($NHOUR $edate $CDATE)
ntimes=$(((fhmax-fhini)/fout+1))
ncmd=$(((ntimes-1)/nprocs+1))
if [ $ncmd -lt 1 ] ; then ncmd=1 ; fi
> $RUNDIR/cmdfile_0
n=0
while [ $((n+=1)) -le $nprocs ] ; do
  > $RUNDIR/invout_$n.sh
  chmod u+x $RUNDIR/invout_$n.sh
  echo $RUNDIR/invout_$n.sh >> $RUNDIR/cmdfile_0
done

nn=0
while [ $((nn+=1)) -le $ntimes ] ; do

  FH=$((fhini+(nn-1)*fout))
  if [ $FH -lt 10 ] ; then FH=0$FH ; fi
  ifile=$INDIR/${inp_file}${FH}${SUFIN}
  ls -l $ifile
  ((rc+=$?))
  if [ -s $ifile ] ; then
    np=$(((nn-1)/ncmd+1))
    invout=$RUNDIR/$(basename $ifile).invout_$FH
    if [ ! -s $invout ] ; then
     echo $WGRIB $ifile \> $invout >> $RUNDIR/invout_$np.sh
     ((rc+=$?))
    fi
  else
    ((rc+=1))
    echo "$ifile does not exist"
  fi
done

#echo 'Before poe for invout rc='rc
date

if [ -s $RUNDIR/cmdfile_0 ] ; then
  if [ $machine = IBM ]; then
    ntasks=$(echo $LOADL_PROCESSOR_LIST|wc -w)
    # only valid if count .le. 128
    [ $ntasks -eq 0 ] && ntasks=$(poe hostname|wc -l)
  elif [ $machine = WCOSS ]; then
    set +u
    if [ -n "$LSB_PJL_TASK_GEOMETRY" ]; then
      ntasks=`echo $LSB_PJL_TASK_GEOMETRY | sed 's/[{}(),]/ /g' | wc -w`
    elif [ -n "$LSB_DJOB_NUMPROC" ]; then
      ntasks=$LSB_DJOB_NUMPROC
    else
      ntasks=1
    fi
    set -u
  elif [ $machine = WCRAY ]; then
      ntasks=24
  else
    echo "ntasks has not been defined for platform $machine"
  fi

  remainder=$((($ntasks-$(cat $RUNDIR/cmdfile_0|wc -l))%$ntasks))
  n=0;while [ $((n+=1)) -le $remainder ] ;do
     echo "echo do nothing" >> $RUNDIR/cmdfile_0
  done
  if [[ $machine = WCOSS ]]; then
    $APRUN -pgmmodel mpmd -cmdfile $RUNDIR/cmdfile_0
    ((rc+=$?))
  elif [[ $machine = WCRAY ]]; then
    aprun -q -b -j1 -n$ntasks -d1 -cc depth cfp $RUNDIR/cmdfile_0
    ((rc+=$?))
  fi
  export err=$rc; err_chk
else
  echo "cmdfile_0 does not exist - wgrib for invout did not work"
fi
#echo ' after poe rc='$rc
if [ $rc -gt 0 ] ; then
  if [ $RUN_ENVIR = dev ]; then
    $PERR;exit $rc
  else
    export err=$rc; err_chk
  fi
else
  rm -f $RUNDIR/invout_*.sh ; rm -f cmdfile_0 
fi

echo ' After invout '
date


ncmd=$(((nvar-1)/nprocs+1))
n=0;while [ $((n+=1)) -le $ncmd ] ; do rm -f $RUNDIR/cmdfile_$n ; > $RUNDIR/cmdfile_$n ; done

nn=1
until [ $nn -gt $nvar ] ; do
  > $RUNDIR/script_$nn.sh
  chmod u+x $RUNDIR/script_$nn.sh
  ofile=${all_ofile[nn]}
  kpdv=${all_kpdv[nn]}
  np=$(((nn-1)/nprocs+1))
  echo $RUNDIR/script_$nn.sh >> $RUNDIR/cmdfile_$np

  nt=0
  while [ $((nt+=1)) -le $ntimes ] ; do

  FH=$((fhini+(nt-1)*fout))
  if [ $FH -lt 10 ] ; then FH=0$FH ; fi
    ifile=$INDIR/${inp_file}${FH}${SUFIN}
    invout=$RUNDIR/$(basename $ifile).invout_$FH


    if [ ${ave_kpd[nn]} = YES ] ; then
      start_byt=$(grep "$kpdv" $invout | grep ":NAve"| awk -F: '{print $2}')
      rec_num=$(grep "$kpdv" $invout | grep ":NAve"| awk -F: '{print $1}')
      nrec=$(echo $rec_num |  wc -w)
      nrec=$((nrec+0))
      if [ $nrec -gt 1 ] ; then
        nnr=1
        while [ $nnr -le $nrec ] ; do
       
          nns=$(echo $rec_num | eval awk \'{print \$$nnr}\')
          for rec in $dup_rec ; do
            if [ $rec = $nns ] ; then
              start_byt=$(echo $start_byt | eval awk \'{print \$$nnr}\')
            fi
          done
          nnr=$((nnr+1))
         done
      fi
      start_byt=$(echo $start_byt | awk '{print $1}')


#     if [ $(echo $start_byt | wc -w) -gt 1 ] ; then
#       start_byt=$(echo $start_byt | awk '{print $1}')
#     fi
    else
      start_byt=$(grep "$kpdv" $invout | awk -F: '{print $2}')
      if [ $(echo $start_byt | wc -w) -gt 1 ] ; then
        start_byt=$(echo $start_byt | awk '{print $1}')
      fi
    fi

    echo $WGRIB $ifile -p $start_byt -grib -append -o $ofile >> $RUNDIR/script_$nn.sh
    ((rc+=$?))
#echo ' after wgrib rc='$rc ' nn='$nn
  done
  nn=$((nn+1))
done

echo Before POE
date
if [ -s $RUNDIR/cmdfile_$ncmd ] ; then
  if [ $machine = IBM ]; then
    ntasks=$(echo $LOADL_PROCESSOR_LIST|wc -w)
    # only valid if count .le. 128
    [ $ntasks -eq 0 ] && ntasks=$(poe hostname|wc -l)
  elif [ $machine = WCOSS ]; then
    set +u
    if [ -n "$LSB_PJL_TASK_GEOMETRY" ]; then
      ntasks=`echo $LSB_PJL_TASK_GEOMETRY | sed 's/[{}(),]/ /g' | wc -w`
    elif [ -n "$LSB_DJOB_NUMPROC" ]; then
      ntasks=$LSB_DJOB_NUMPROC
    else
      ntasks=1
    fi
    set -u
  elif [ $machine = WCRAY ]; then
    ntasks=24
  else
    echo "ntasks has not been defined for platform $machine"
  fi

  remainder=$((($ntasks-$(cat $RUNDIR/cmdfile_$ncmd|wc -l))%$ntasks))
  n=0;while [ $((n+=1)) -le $remainder ] ;do
     echo "echo do nothing" >> $RUNDIR/cmdfile_$ncmd
  done
fi
n=0
while [ $((n+=1)) -le $ncmd ] ;do
  if [[ $machine = WCOSS ]]; then
     $APRUN -pgmmodel mpmd -cmdfile $RUNDIR/cmdfile_$n
  elif [[ $machine = WCRAY ]]; then
     time aprun -q -b -j1 -n$ntasks -d1 -cc depth cfp $RUNDIR/cmdfile_$n
  fi
  export err=$?; err_chk
done
((rc+=$?))
if [ $rc -gt 0 ] ; then exit $rc ; else rm -f $RUNDIR/script_*.sh ; rm -f cmdfile_* ; fi

################################################################################
# Exit gracefully

date
if [ $RUN_ENVIR = dev ] ; then
 if [[ $rc -ne 0 ]] ; then
  $PLOG "$RUNLOG" ERROR "$jn failed for $PSLOT"
  exit 1
 else
  $PLOG "$RUNLOG" OK "$jn ended for $PSLOT"
 fi
fi
exit $rc

