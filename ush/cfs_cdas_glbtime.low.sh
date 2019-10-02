#!/bin/ksh
set -x

######################################################################
#   This script creates the low res timeseries
#
#       Originally written by Shrinivas Moorthi
#       Updated by Patrick Tripp Sept 2010
######################################################################
export APRUN=${APRUN:-mpirun}

# fix for files > 2GB

export COPYGBFIX=${COPYGBFIX:-$USHcfs/cfs_copygb_bigfix.sh}

export yyyymm=${1:-$yyyymm}
export inp_file=${2:-${inp_file:-pgbf}}
export CDUMP=${3:-${CDUMP:-gdas}}
export fout=${4:-${fout:-${fhout:-${FHOUT:-6}}}}
export INDIR=${5:-$INDIR}
export TIMEDIR=${6:-${TIMEDIR:-$INDIR}}


WINDEX=${WINDEX:-${GRBINDEX}

export MP_STDOUTMODE=ordered
export MP_LABELIO=yes

cd $TIMEDIR

yyyy=$(echo $yyyymm|cut -c1-4)
mm=$(echo $yyyymm|cut -c5-6)
dds=01
dde=`$USHcfs/cfs_daysinmonth.sh ${yyyy} $mm`
fout=6

cdate1=${yyyymm}${dds}00
cdate2=${yyyymm}${dde}18
cdate=$cdate1

chunks=${chunks:-3}

nhours=$fout
indexdir=${indexdir:-$INDIR}

cdate=$cdate1

pgblist='z200 z500 z700 z850 z1000 t2 t50 t200 t250 t500 t700 t850 t1000 vvel500 q500 q700 q850 q925 prmsl wnd200 wnd250 wnd500 wnd700 wnd850 wnd925 wnd1000 psi200 psi850 chi200 chi850'

ipvlist='ipv450 ipv550 ipv650'

ocnlist='ocndt20c ocnheat ocnslh ocnsst ocnu5 ocnv5 ocnsal5 ocnu15 ocnv15 ocnt15 ocnsal15 ocnvv55 ocnmld ocndt2.5c ocndt5c ocndt10c ocndt15c ocndt25c ocndt28c ocnsild ocntchp'

flxlist='lhtfl shtfl prate wnd10m wndstrs pressfc pwat tmp2m tmpsfc tmphy1 snohf dlwsfc dswsfc ulwsfc ulwtoa uswsfc uswtoa soilm1 soilm2 soilm3 soilm4 soilt1 gflux weasd runoff tmin tmax q2m icecon icethk csusf csdsf csdlf cprat tcdcclm'

flxplist='soilm1 soilm2 soilm3 soilm4 weasd'
	
# find the number of procs

if [ -n "$LSB_DJOB_NUMPROC" ]; then
   ntasks=$LSB_DJOB_NUMPROC
else
   echo "ntasks not defined for this platform"
   export err=99; err_chk
fi

# setup the cmdfiles

n=0
npoe=0
prefix=$(echo $inp_file|cut -c1-3)
rm cmdfile.*; > cmdfile.$$.$npoe
list=

if [ inp_file = all -o $prefix = flx ] ; then
	list="$list $flxlist"
	for name in $flxlist;do 
		ifile=$name.$CDUMP.$yyyymm 
		[ ! -s $ifile ] && exit 1
		ofile=${name}.l.$CDUMP.$yyyymm

        # This doesnt work for files > 2GB yet
		# echo $COPYGB -xX -i0 -g98 $ifile $ofile>>cmdfile.$$.$npoe
		echo $COPYGBFIX -xX -i0 -g98 $ifile $ofile>>cmdfile.$$.$npoe

		if [ $((n+=1)) -eq $ntasks ] ; then
			((npoe+=1))
			> cmdfile.$$.$npoe
			n=0
		fi
	done 
	list="$list $flxplist"
	for name in $flxplist;do 
		ifile=$name.$CDUMP.$yyyymm 
		[ ! -s $ifile ] && exit 1
		ofile=${name}x0.5.$CDUMP.$yyyymm

		# echo $COPYGB -xX -i0 -g4 $ifile $ofile>>cmdfile.$$.$npoe
		echo $COPYGBFIX -xX -i0 -g4 $ifile $ofile>>cmdfile.$$.$npoe

		if [ $((n+=1)) -eq $ntasks ] ; then
			((npoe+=1))
			> cmdfile.$$.$npoe
			n=0
		fi
	done 
elif [ inp_file = all -o $prefix = pgb  ] ; then
	list="$list $pgblist"
	for name in $pgblist;do 
		ifile=$name.$CDUMP.$yyyymm 
		[ ! -s $ifile ] && exit 1
		ofile=${name}.l.$CDUMP.$yyyymm

		# echo $COPYGB -xX -i0 -g2 $ifile $ofile>>cmdfile.$$.$npoe
		echo $COPYGBFIX -xX -i0 -g2 $ifile $ofile>>cmdfile.$$.$npoe

		if [ $((n+=1)) -eq $ntasks ] ; then
			((npoe+=1))
			> cmdfile.$$.$npoe
			n=0
		fi
	done 
elif [ inp_file = all -o $prefix = ipv  ] ; then
	list="$list $ipvlist"
	for name in $ipvlist;do 
		ifile=$name.$CDUMP.$yyyymm 
		[ ! -s $ifile ] && exit 1
		ofile=${name}.l.$CDUMP.$yyyymm

		echo $COPYGBFIX -xX -i0 -g2 $ifile $ofile>>cmdfile.$$.$npoe

		if [ $((n+=1)) -eq $ntasks ] ; then
			((npoe+=1))
			> cmdfile.$$.$npoe
			n=0
		fi
	done 
elif [ inp_file = all -o $prefix = ocn  ] ; then
	list="$list $ocnlist"
	for name in $ocnlist;do 
		ifile=$name.$CDUMP.$yyyymm 
		[ ! -s $ifile ] && exit 1
		ofile=${name}.l.$CDUMP.$yyyymm

		# echo $COPYGB -xX -i0 -g3 $ifile $ofile>>cmdfile.$$.$npoe
		echo $COPYGBFIX -xX -i0 -g3 $ifile $ofile>>cmdfile.$$.$npoe

		if [ $((n+=1)) -eq $ntasks ] ; then
			((npoe+=1))
			> cmdfile.$$.$npoe
			n=0
		fi
	done 
fi

# concat the separate cmdfiles and run cfp

cat cmdfile.* >cfpfile
mpirun cfp cfpfile  >/dev/null 
export err=$?; err_chk

rm  cfpfile cmdfile.$$* 
exit 
