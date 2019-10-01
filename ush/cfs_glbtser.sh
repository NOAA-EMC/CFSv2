#!/usr/bin/env ksh  
set -eu

#  This script extracts selected grib fields from pgb, flx, ipv or diab files
#  Shrinivas Moorthi - April 2010

export CDATE=${1:-$CDATE}
export inp_file=${2:-${inp_file:-pgbf}}
#export CDUMP=${3:-$CDUMP}
export sdate=${3:-${sdate:-$CDATE}}
export fout=${4:-${fout:-${fhout:-${FHOUT:-6}}}}
export edate=${5:-${edate:-$($NDATE -$fout $(echo $($NDATE 768 $(echo $sdate | cut -c1-6)0100) | cut -c1-6)0100)}}
export INDIR=${6:-$INDIR}
export TIMEDIR=${7:-${TIMEDIR:-$INDIR}}
export RUNDIR=${RUNDIR:-$DATA} 
export CLEAN_RUNDIR=${CLEAN_RUNDIR:-NO}
export jn=${jn:-gbltseries}
export resl=${8:-f}
export pgm=$0

if [ $sdate -eq $CDATE ] ; then export sdate=$($NDATE $fout $CDATE) ; fi

echo `date` Starting $0
echo "CDATE is $CDATE"
echo "fout is $fout"
echo "INDIR is $INDIR"
echo "TIMEDIR is $TIMEDIR"
echo "sdate is $sdate"
echo "edate is $edate"
echo "RUNDIR is $RUNDIR"

mkdir -p $RUNDIR; if [ $CLEAN_RUNDIR = YES ] ; then rm -f $RUNDIR/* ; fi

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

# setup the variables for this run
# --------------------------------

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
set -A varn      $totnum

# setup arrays for finding start_bytes for inventories
# ----------------------------------------------------

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

# setup arrays for creating cmdfiles for extracting records
# ---------------------------------------------------------

n=0
for var in $varlist; do
 n=$((n+1))
 varn[n]=$var
 echo $n $var ${varn[$n]}
done

# get started with the time series processing
# -------------------------------------------

rc=0

cd $TIMEDIR

SUFIN=${SUFIN:-""}
SUFOUT=.$sdate.$edate$SUFIN

echo "SUFIN is $SUFIN"
echo "SUFOUT is $SUFOUT"

# create output filenames for time series variables
# -------------------------------------------------

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

# Find begining, end, and counts in time period
# ---------------------------------------------

fhini=$($NHOUR $sdate $CDATE)
fhmax=$($NHOUR $edate $CDATE)
ntimes=$(((fhmax-fhini)/fout+1))

# create inventories for each time in this inp_file 
# -------------------------------------------------

nn=0; >$RUNDIR/invout.cmdfile
while [ $((nn+=1)) -le $ntimes ] ; do
  FH=$((fhini+(nn-1)*fout))
  if [ $FH -lt 10 ] ; then FH=0$FH ; fi
  ifile=$INDIR/${inp_file}${FH}${SUFIN}
  ls -l $ifile
  ((rc+=$?))
  if [ -s $ifile ] ; then
    invout=$RUNDIR/$(basename $ifile).invout_$FH
    if [ ! -s $invout ] ; then
     echo $WGRIB $ifile \> $invout >> $RUNDIR/invout.cmdfile
     ((rc+=$?))
    fi
  else
    ((rc+=1))
    echo "$ifile does not exist"
  fi
done; export err=$rc; err_chk  

# run mpmd to get inventories
# ---------------------------

echo `date` run mpmd to get inventories         
mpirun cfp $RUNDIR/invout.cmdfile >/dev/null 
export err=$?; pgm=inventories; err_chk

# loop over times inside vars to make extraction cmdfiles
# -------------------------------------------------------

> $RUNDIR/uwind.cmdfile
> $RUNDIR/vwind.cmdfile
> $RUNDIR/scalr.cmdfile

nn=1
until [ $nn -gt $nvar ] ; do
  ofile=${all_ofile[nn]}
  kpdv=${all_kpdv[nn]}
  var=${varn[nn]}

  >$RUNDIR/uwind.script.$nn; chmod +x $RUNDIR/uwind.script.$nn
  >$RUNDIR/vwind.script.$nn; chmod +x $RUNDIR/vwind.script.$nn
  >$RUNDIR/scalr.script.$nn; chmod +x $RUNDIR/scalr.script.$nn

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
    else
      start_byt=$(grep "$kpdv" $invout | awk -F: '{print $2}')
      if [ $(echo $start_byt | wc -w) -gt 1 ] ; then
        start_byt=$(echo $start_byt | awk '{print $1}')
      fi
    fi

    # separate the extractions into u,v,and scalr lists
    # -------------------------------------------------

    ofn=$(echo $ofile|cut -c 1-3)
    var=$(echo $var  |cut -c 1-1)

    [[ $ofn = wnd && $var = u ]] && echo $WGRIB $ifile -p $start_byt -grib -append -o $ofile >> $RUNDIR/uwind.script.$nn
    [[ $ofn = wnd && $var = v ]] && echo $WGRIB $ifile -p $start_byt -grib -append -o $ofile >> $RUNDIR/vwind.script.$nn
    [[ $ofn = wnd             ]] || echo $WGRIB $ifile -p $start_byt -grib -append -o $ofile >> $RUNDIR/scalr.script.$nn

  done ## time loop
  [[ -s $RUNDIR/uwind.script.$nn ]] && echo $RUNDIR/uwind.script.$nn >> $RUNDIR/uwind.cmdfile
  [[ -s $RUNDIR/vwind.script.$nn ]] && echo $RUNDIR/vwind.script.$nn >> $RUNDIR/vwind.cmdfile
  [[ -s $RUNDIR/scalr.script.$nn ]] && echo $RUNDIR/scalr.script.$nn >> $RUNDIR/scalr.cmdfile
  nn=$((nn+1))
done ## vars loop

# run mpmd to extract recordis for each var and time
# --------------------------------------------------

echo `date` run mpmd to extract record for each var and time
mpirun cfp  $RUNDIR/uwind.cmdfile >/dev/null; export err=$?; pgm=u-extract; err_chk
mpirun cfp  $RUNDIR/vwind.cmdfile >/dev/null; export err=$?; pgm=v-extract; err_chk
mpirun cfp  $RUNDIR/scalr.cmdfile >/dev/null; export err=$?; pgm=s-extract; err_chk

################################################################################
####export err=99; err_chk  # end for test
################################################################################
# Exit gracefully

cd $RUNDIR
rm -f uwind.script.* vwind.script.* scalr.script.* 
rm -f uwind.cmdfile  vwind.cmdfile  scalr.cmdfile  
echo $(date) finished $0 normally 
exit 
