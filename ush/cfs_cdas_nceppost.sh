#!/bin/ksh   
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         global_nceppost.sh           
# Script description:  Posts the global pressure GRIB file
#
# Author:        Mark Iredell       Org: NP23         Date: 1999-05-01
#
# Abstract: This script reads a single global GFS IO file and (optionally)
#   a global flux file and creates a global pressure GRIB file.
#   The resolution and generating code of the output GRIB file can also
#   be set in the argument list.
#
# Script history log:
# 1999-05-01  Mark Iredell
# 2007-04-04  Huiya Chuang: Modify the script to run unified post
#
# Usage:  global_postgp.sh SIGINP FLXINP FLXIOUT PGBOUT PGIOUT IGEN
#
#   Input script positional parameters:
#     1             Input sigma file
#                   defaults to $SIGINP
#     2             Input flux file
#                   defaults to $FLXINP
#     3             Output flux index file
#                   defaults to $FLXIOUT
#     4             Output pressure GRIB file
#                   defaults to $PGBOUT
#     5             Output pressure GRIB index file
#                   defaults to $PGIOUT, then to none
#     8             Model generating code,
#                   defaults to $IGEN, then to input sigma generating code
#
#   Imported Shell Variables:
#     SIGINP        Input sigma file
#                   overridden by $1
#     FLXINP        Input flux file
#                   overridden by $2
#     FLXIOUT       Output flux index file
#                   overridden by $3
#     PGBOUT        Output pressure GRIB file
#                   overridden by $4. If not defined,
#                   post will use the filename specified in
#                   the control file
#     PGIOUT        Output pressure GRIB index file
#                   overridden by $5; defaults to none
#     IGEN          Model generating code
#                   overridden by $8; defaults to input sigma generating code
##### Moorthi: Add new imported shell variable for running chgres
#     CHGRESSH      optional: the script to run chgres
#		    default to to ${USHGLOBAL}/global_chgres.sh
#     SIGLEVEL      optional: the coordinate text file
#		    default to to /nwprod/fix/global_hyblev.l${LEVS}.txt
##### Chuang: Add new imported Shell Variable for ncep post
#     OUTTYP        Output file type for chgres
#                   1: if user has a sigma file and needs post to run chgres to convert to gfs io file
#                   2: if user already has a gfs io file
#                   3: if user uses post to read sigma file directly
#                   0: if user wishes to generate both gfsio and sigma files
#     VDATE         Verifying date 10 digits yyyymmddhh
#     GFSOUT        Optional, output file name from chgres which is input file name to nceppost 
#                   if model already runs gfs io, make sure GFSOUT is linked to the gfsio file
#     CTLFILE       Optional, Your version of control file if not using operational one
#     OVERPARMEXEC  Optional, the executable for changing Grib KPDS ID
#     FILTER        Optional, set to 1 to filter SLP and 500 mb height using copygb
#     D3DINP        Optional, Inout D3D file, if not defined, post will run
#                   without processing D3D file
#     D3DOUT        Optional, output D3D file, if not defined, post will
#                   use the file name specified in the control file
#     IPVOUT        Optional, output IPV file, if not defined, post will
#                   use the file name specified in the control file
#    GENPSICHI      Optional, set to YES will generate psi and chi and
#                   append it to the end of PGBOUT.  Default to NO
#    GENPSICHIEXE   Optional, specify where executable is for generating
#                   psi and chi.
########################################################################       
#     EXECUTIL      Directory for utility executables
#                   defaults to /nwprod/util/exec
#     USHUTIL       Directory for utility scripts
#                   defaults to /nwprod/util/ush
#     EXECGLOBAL    Directory for global executables
#                   defaults to /nwprod/exec
#     USHGLOBAL     Directory for global scripts
#                   defaults to /nwprod/ush
#     DATA          working directory
#                   (if nonexistent will be made, used and deleted)
#                   defaults to current working directory
#     MP            Multi-processing type ("p" or "s")
#                   defaults to "p", or "s" if LOADL_STEP_TYPE is not PARALLEL
#     XC            Suffix to add to executables
#                   defaults to none
#     POSTGPEXEC    Global post executable
#                   defaults to ${EXECGLOBAL}/ncep_post
#     GRBINDEX      GRIB index maker
#                   defaults to ${EXECUTIL}/grbindex$XC
#     ANOMCATSH     Global anomaly GRIB script
#                   defaults to ${USHGLOBAL}/global_anomcat.sh
#     POSTGPLIST    File containing further namelist inputs
#                   defaults to /dev/null
#     INISCRIPT     Preprocessing script
#                   defaults to none
#     LOGSCRIPT     Log posting script
#                   defaults to none
#     ERRSCRIPT     Error processing script
#                   defaults to 'eval [[ $err = 0 ]]'
#     ENDSCRIPT     Postprocessing script
#                   defaults to none
#     POSTGPVARS    Other namelist inputs to the global post executable
#                   such as IDRT,KO,PO,KTT,KT,PT,KZZ,ZZ,
#                           NCPUS,MXBIT,IDS,POB,POT,MOO,MOOA,MOW,MOWA,
#                           ICEN,ICEN2,IENST,IENSI
#                   defaults to none set
#     VERBOSE       Verbose flag (YES or NO)
#                   defaults to NO
#     PGMOUT        Executable standard output
#                   defaults to $pgmout, then to '&1'
#     PGMERR        Executable standard error
#                   defaults to $pgmerr, then to '&1'
#     pgmout        Executable standard output default
#     pgmerr        Executable standard error default
#     REDOUT        standard output redirect ('1>' or '1>>')
#                   defaults to '1>', or to '1>>' to append if $PGMOUT is a file
#     REDERR        standard error redirect ('2>' or '2>>')
#                   defaults to '2>', or to '2>>' to append if $PGMERR is a file
#
#   Exported Shell Variables:
#     PGM           Current program name
#     pgm
#     ERR           Last return code
#     err
#
#   Modules and files referenced:
#     scripts    : $INISCRIPT
#                  $LOGSCRIPT
#                  $ERRSCRIPT
#                  $ENDSCRIPT
#                  $ANOMCATSH
#
#     programs   : $POSTGPEXEC
#                  $GRBINDEX
#
#     input data : $1 or $SIGINP
#                  $2 or $SFCINP
#                  $POSTGPLIST
#
#     output data: $3 or $FLXIOUT
#                  $4 or $PGBOUT
#                  $5 or $PGIOUT
#                  $PGMOUT
#                  $PGMERR
#
#     scratch    : ${DATA}/postgp.inp.sig
#                  ${DATA}/postgp.inp.flx
#                  ${DATA}/postgp.out.pgb
#
# Remarks:
#
#   Condition codes
#      0 - no problem encountered
#     >0 - some problem encountered
#
#   Control variable resolution priority
#     1 Command line argument.
#     2 Environment variable.
#     3 Inline default.
#
# Attributes:
#   Language: POSIX shell
#   Machine: IBM SP
#
####
################################################################################
#  Set environment.
##set -eua
export VERBOSE=${VERBOSE:-"NO"}
if [[ "$VERBOSE" = "YES" ]]
then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi
#  Command line arguments.
export SIGINP=${1:-${SIGINP}}
export FLXINP=${2:-${FLXINP}}
export FLXIOUT=${3:-${FLXIOUT}}
export PGBOUT=${4:-${PGBOUT}}
#export PGIOUT=${5:-${PGIOUT}}
export PGIOUT=${PGIOUT:-pgb.idx}
export IO=${6:-${IO:-0}}
export JO=${7:-${JO:-0}}
export IGEN=${8:-${IGEN:-0}}
#  Filenames.
export POSTGPLIST=${POSTGPLIST:-/dev/null}
export MODEL_OUT_FORM=${MODEL_OUT_FORM:-grib}
#  Other variables.
export POSTGPVARS=${POSTGPVARS}
export NTHREADS=${NTHREADS:-1}
export NTHSTACK=${NTHSTACK:-64000000}
export PGMOUT=${PGMOUT:-${pgmout:-'&1'}}
export PGMERR=${PGMERR:-${pgmerr:-'&2'}}
export FILTER=${FILTER:-1}
export GENPSICHI=${GENPSICHI:-NO}
export GENPSICHIEXE=${GENPSICHIEXE:-$HOMEcfs/exec/cfs_genpsiandchi}
export OVERPARMEXEC=${OVERPARMEXEC:-$HOMEcfs/exec/cfs_overparm_grib}
export POSTGPEXEC=${POSTGPEXEC:-$HOMEpost/exec/ncep_post}
export CHGRESSH=${CHGRESSH:-$HOMEgsm/ush/global_chgres.sh}
export SIGHDR=${SIGHDR:-$HOMEgsm/exec/global_sighdr}
export D3DINP=${D3DINP:-/dev/null}

typeset -L1 l=$PGMOUT
[[ $l = '&' ]]&&a=''||a='>'
export REDOUT=${REDOUT:-'1>'$a}
typeset -L1 l=$PGMERR
[[ $l = '&' ]]&&a=''||a='>'
export REDERR=${REDERR:-'2>'$a}

################################################################################
#  Preprocessing
${INISCRIPT:-echo}

# Chuang: Run chgres if OUTTYP=1 or 0

if [ ${OUTTYP} -le 1 ] ; then
 # exit if SIGINP does not exist
 if [ ! -s $SIGINP ] ; then
  echo "sigma file not found, exitting"
  exit 111
 fi
 export JCAP=${JCAP:-`echo jcap|$SIGHDR ${SIGINP}`}
 export LEVS=${LEVS:-`echo levs|$SIGHDR ${SIGINP}`}
 export IDVC=${IDVC:-$(echo idvc|$SIGHDR ${SIGINP})}
 export IDVM=${IDVM:-$(echo idvm|$SIGHDR ${SIGINP})}
 export NVCOORD=${NVCOORD:-$(echo nvcoord|$SIGHDR ${SIGINP})}
 export IVSSIG=${IVSSIG:-$(echo ivs|$SIGHDR ${SIGINP})}
 export LATCH=${LATCH:-8}
 if [ ${OUTTYP} -eq 1 ] ; then 
  export CHGRESVARS="IDVC=$IDVC,IDVM=$IDVM,NVCOORD=$NVCOORD,IVSSIG=$IVSSIG,LATCH=$LATCH,"  
 elif [ ${OUTTYP} -eq 0 ] ; then
  export CHGRESVARS="LATCH=$LATCH,$CHGRESVARS"
 fi 
 export SIGLEVEL=${SIGLEVEL:-$HOMEcfs/fix/cfs_fix_am/global_hyblev.l${LEVS}.txt}
 export LONB=${LONB:-`echo lonb|$SIGHDR ${SIGINP}`}
 export LATB=${LATB:-`echo latb|$SIGHDR ${SIGINP}`}
 export IDRT=${IDRT:-4}

 $CHGRESSH

 export ERR=$?
 export err=$ERR
 err_chk 
 
# run post to read sigma file directly if OUTTYP=3
elif [ ${OUTTYP} -eq 3 ] ; then
 export MODEL_OUT_FORM=sigio
 export GFSOUT=${SIGINP}
fi

# reduce thread to 1 since post runs with mpi
export OMP_NUM_THREADS=1

pwd=$(pwd)
if [[ -d $DATA ]]
then
   mkdata=NO
else
   mkdir -p $DATA
   mkdata=YES
fi
cd $DATA||exit 99
################################################################################
#  Post GRIB
export PGM=$POSTGPEXEC
export pgm=$PGM
startmsg   


cat <<EOF >postgp.inp.nml$$
 &NAMPGB
 $POSTGPVARS
 /
EOF

if [[ "$VERBOSE" = "YES" ]]
then
   cat postgp.inp.nml$$
fi

# making the time stamp format for ncep post
export YY=`echo $VDATE | cut -c1-4`
export MM=`echo $VDATE | cut -c5-6`
export DD=`echo $VDATE | cut -c7-8`
export HH=`echo $VDATE | cut -c9-10`

cat > itag <<EOF
$GFSOUT
${MODEL_OUT_FORM}
${YY}-${MM}-${DD}_${HH}:00:00
GFS
$FLXINP
$D3DINP
EOF
cat postgp.inp.nml$$ >> itag
cat itag

rm -f fort.*

#ln -sf $SIGINP     postgp.inp.sig$$
#ln -sf $FLXINP     postgp.inp.flx$$
#ln -sf $PGBOUT     postgp.out.pgb$$

# change model generating Grib number 
if [ ${IGEN} -le 9 ] ; then
 cat ${CTLFILE}|sed s:00082:0000${IGEN}:>./gfs_cntrl.parm
elif [ ${IGEN} -le 99 ] ; then
 cat ${CTLFILE}|sed s:00082:000${IGEN}:>./gfs_cntrl.parm
elif [ ${IGEN} -le 999 ] ; then
 cat ${CTLFILE}|sed s:00082:00${IGEN}:>./gfs_cntrl.parm
else
 ln -sf ${CTLFILE} ./gfs_cntrl.parm 
fi 
ln -sf ./gfs_cntrl.parm fort.14
ln -sf griddef.out fort.110

time mpiexec -n 64 $POSTGPEXEC < itag > outpost_gfs_${VDATE}

export ERR=$?
export err=$ERR
err_chk 

if [ $FILTER = "1" ] ; then

# Filter SLP and 500 mb height using copygb, change GRIB ID, and then
# cat the filtered fields to the pressure GRIB file, from Iredell

copygb=$COPYGB

$copygb -x -i'4,0,80' -k'4*-1,1,102' $PGBOUT tfile
ln -s -f tfile fort.11
ln -s -f prmsl fort.51
echo 0 2|$OVERPARMEXEC

$copygb -x -i'4,1,5' -k'4*-1,7,100,500' $PGBOUT tfile
ln -s -f tfile fort.11
ln -s -f h5wav fort.51
echo 0 222|$OVERPARMEXEC

cat  prmsl h5wav >> $PGBOUT

fi

################################################################################
#  Anomaly concatenation
if [[ -x $ANOMCATSH ]]
then
   if [[ -n $PGIOUT ]]
   then
      $GRBINDEX $PGBOUT $PGIOUT
   fi
   export PGM=$ANOMCATSH
   export pgm=$PGM
   $LOGSCRIPT

   eval $ANOMCATSH $PGBOUT $PGIOUT

   export ERR=$?
   export err=$ERR
   err_chk 
fi
################################################################################
#  Make GRIB index file
if [[ -n $PGIOUT ]]
then
   $GRBINDEX $PGBOUT $PGIOUT
fi
if [[ -s $FLXINP && -n $FLXIOUT ]]
then
   $GRBINDEX $FLXINP $FLXIOUT
fi
################################################################################
# generate psi and chi
echo "GENPSICHI= " $GENPSICHI
if [ $GENPSICHI = YES ] ; then
#echo "PGBOUT PGIOUT=" $PGBOUT $PGIOUT
#echo "YY MM=" $YY $MM
 export psichifile=./psichi.grb
 $GENPSICHIEXE < postgp.inp.nml$$
 rc=$?
 if [[ $rc -ne 0 ]] ; then echo 'Nonzero return code rc= '$rc ; exit 3 ; fi
 cat ./psichi.grb >> $PGBOUT
fi
################################################################################
#  Postprocessing
cd $pwd
[[ $mkdata = YES ]]&&rmdir $DATA
${ENDSCRIPT:-echo}
set -x
if [[ "$VERBOSE" = "YES" ]]
then
   echo $(date) EXITING $0 with return code $err >&2
fi
exit $err
