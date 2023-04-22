#!/usr/bin/env bash 
#------------------------------------------------------------------------------#
#### MLC AM/OM/Coupler script ##################################################
#  
#   Script positional parameters: 
#     [AM positional parameters] - [OM positional parameters]
#  
#   MPMD coupled script script written by Dimitry Sheinin & Shrinivas Moorthi
#   using the GFS script for AM and MOM3 script for OM.
#   Update for running with MOM4 made by Xingren Wu, Jun Wang and S. Moorthi
#  
#   Additional imported shell variables:
#
#     NPROCS_a          number of processors for AM (nonnegative integer)
#     NPROCS_o          number of processors for OM (nonnegative integer)
#     NPROCS_c          number of processors for Coupler (0 or 1; 0 if AM or OM
#                       is run standalone; 1 in the coupled mode)
#     EXEC_CD           directory for Coupler executable
#                       defaults to $EXECcfs
#     EXECcfs           defults to $HOMEcfs/exec
#     C_EXEC            name of Coupler executable
#                       defaults to ${EXEC_CD}/mlc_cfs_coupler
#     CouplingPeriod    period (s) for MPI exchange between Coupler and
#                       Components (time step for Coupler)
#                       defaults to 3600
#     STDOUT_TYPE       determines whether Coupler executable and Component
#                       executables use stdout (common standard output) or
#                       print in separate files (whose names are predefined).
#                       The value is "mess" in the former and any other in the
#                       latter case. Post-execution separation of stdout is
#                       performed in the former case. Defaults to "mess"
#     CLIMcfs4
#                       MOM4ICE forcing data directory. Defaults to
#                       /climate/save/wx20xw/MOM4ICE/CLIM8cfs4
#------------------------------------------------------------------------------#
# Begin pre-execution section of AM script (1)
#------------------------------------------------------------------------------#
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         mlc_gfs.sh
# Script description:  Runs a global spectral model AM coupled to MOM4 OM
#
# AM Author:        Mark Iredell       Org: NP23         Date: 1999-05-01
#
# AM Abstract: AM part of this  script runs a global spectral AM.
#   The initial conditions and run parameters are passed in the argument list.
#
# AM Script history log (for GFS part):
# 1999-05-01  Mark Iredell
# 2004-05-15  Shrinivas Moorthi
# 2005-01-03  Cheng-Hsuan Lu :add namelist SOIL_VEG
#                             set FSMCL(2:4) = FSMCL2
#                             add FNVMNC,FNVMXC,FNSLPC,FNABSC
# 2006-02     Shrinivas Moorthi Modified to run ESMF - Stand Alone
#                             version of GFS - Only filestyle "L"
#                             allowed - Added a ESMF config file
#                             The script can run up to 21 ENS members
#                             concurrently.
# 2006-06     Shrinivas Moorthi : Added default PE$n values to 0
#
# OM Authors: GFDL ; Dave Behringer, Wanqiu Wu - for MOM3 OM
# 2002-2006   Dmitry Shenin and S. Moorthi - MPI coupling
# 2007-06     Xingren Wu : Updated for MOM4 with Sea Ice 
# 2012-12     Shrinivas Moorthi : Updated for WCOSS/ZEUS
#
# Usage:  excfs_fcst.sh.ecf SIGI SFCI SIGO FLXO FHOUT FHMAX IGEN D3DO
#
#   Input script positional parameters:
#     1             Input sigma file 1
#                   defaults to $SIGI; one or the other is required
#     2             Input surface file
#                   defaults to $SFCI; one or the other is required
#     3             Output sigma file with embedded forecast hour '${FH}'
#                   defaults to $SIGO, then to ${COMOUT}/sigf'${FH}'$SUFOUT
#     4             Output flux file with embedded forecast hour '${FH}'
#                   defaults to $FLXO, then to ${COMOUT}/flxf'${FH}'$SUFOUT
#     5             Output frequency in hours
#                   defaults to $FHOUT, then to 3
#     6             Length of forecast in hours
#                   defaults to $FHMAX; otherwise FHSEG is required to be set
#     7             Output generating code
#                   defaults to $IGEN, defaults to 0
#     8             Output flux file with embedded forecast hour '${FH}'
#                   defaults to $D3DO, then to ${COMOUT}/d3df'${FH}'$SUFOUT
#
#   Imported Shell Variables:
#     SIGI          Input sigma file
#                   overridden by $1; one or the other is required
#     SFCI          Input surface file
#                   overridden by $2; one or the other is required
#     SIGO          Output sigma file with embedded forecast hour '${FH}'
#                   overridden by $3; defaults to ${COMOUT}/sigf'${FH}'$SUFOUT
#     FLXO          Output flux file with embedded forecast hour '${FH}'
#                   overridden by $4; defaults to ${COMOUT}/flxf'${FH}'$SUFOUT
#     FHOUT         Output frequency in hours
#                   overridden by $5; defaults to 3
#     FHMAX         Length of forecast in hours
#                   overridden by $6; either FHMAX or FHSEG must be set
#     IGEN          Output generating code
#                   overridden by $7; defaults to 0
#     FIXDIR        Directory for global fixed files
#     EXEC_AMD      Directory for global AM executables
#                   defaults to $NWROOT/exec
#     DATA          working directory
#                   (if nonexistent will be made, used and deleted)
#                   defaults to current working directory
#     COMOUT        output directory
#                   (if nonexistent will be made)
#                   defaults to current working directory
#     XC            Suffix to add to executables
#                   defaults to none
#     SUFOUT        Suffix to add to output filenames
#                   defaults to none
#     NCP           Copy command
#                   defaults to cp
#     SIGHDR        Command to read sigma header
#                   (required if JCAP, LEVS, or FHINI are not specified)
#                   defaults to ${EXEC_AMD}/${cfsp}_sighdr$XC
#     JCAP          Spectral truncation
#                   defaults to the value in the input sigma file header
#     LEVS          Number of levels
#                   defaults to the value in the input sigma file header
#     MTNRSL        A string representing topography resolution
#                   defaults to $JCAP
#     AM_EXEC       Atmospheric Forecast model executable
#
#     SIGI2         Second time level sigma restart file
#                   defaults to NULL
#     model         either "global" or "cfs" - defaults to "cfs"
#     CO2CON        Input CO2 radiation (vertical resolution dependent)
#                   defaults to ${FIXDIR}/${model}_co2con.l${LEVS}.f77
#     MTNVAR        Input mountain variance (horizontal resolution dependent)
#                   defaults to ${FIXDIR}/${model}_mtnvar.t${JCAP}.f77
#     CLTUNE        Input cloud tuning file
#                   defaults to ${FIXDIR}/${model}_cldtune.f77
#     DTBTHE        Input equivalent potential temperature file
#                   defaults to ${FIXDIR}/${model}_tbthe.f77
#     O3FORC        Input ozone production climatology
#                   defaults to ${FIXDIR}/${model}_o3prdlos.txt
#     O3CLIM        Input ozone climatology
#                   defaults to ${FIXDIR}/${model}_o3clim.txt
#     FNGLAC        Input glacier climatology GRIB file
#                   defaults to ${FIXDIR}/${model}_glacier.2x2.grb
#     FNMXIC        Input maximum sea ice climatology GRIB file
#                   defaults to ${FIXDIR}/${model}_maxice.2x2.grb
#     FNTSFC        Input SST climatology GRIB file
#                   defaults to ${FIXDIR}/${model}_oi2sst1x1monclim19822001.grb
#     FNSNOC        Input snow climatology GRIB file
#                   defaults to ${FIXDIR}/${model}_snoclim.1.875.grb
#     FNZORC        Input roughness climatology GRIB file
#                   defaults to ${FIXDIR}/${model}_zorclim.1x1.grb
#     FNALBC        Input albedo climatology GRIB file
#                   defaults to ${FIXDIR}/${model}_albedo4.1x1.grb
#     FNAISC        Input sea ice climatology GRIB file
#                   defaults to ${FIXDIR}/${model}_ice1x1monclim19822001.grb
#     FNTG3C        Input deep soil temperature climatology GRIB file
#                   defaults to ${FIXDIR}/${model}_tg3clim.2.6x1.5.grb
#     FNVEGC        Input vegetation fraction climatology GRIB file
#                   defaults to ${FIXDIR}/${model}_vegfrac.1x1.grb
#     FNVETC        Input vegetation type climatology GRIB file
#                   defaults to ${FIXDIR}/${model}_vegtype.1x1.grb
#     FNSOTC        Input soil type climatology GRIB file
#                   defaults to ${FIXDIR}/${model}_soiltype.1x1.grb
#     FNSMCC        Input soil moisture climatology GRIB file
#                   defaults to ${FIXDIR}/${model}_soilmcpc.1x1.grb
#     FNVMNC        Input min veg frac climatology GRIB file
#                   defaults to ${FIXDIR}/${model}_shdmin.0.144x0.144.grb
#     FNVMXC        Input max veg frac climatology GRIB file
#                   defaults to ${FIXDIR}/${model}_shdmax.0.144x0.144.grb
#     FNSLPC        Input slope type climatology GRIB file
#                   defaults to ${FIXDIR}/${model}_slope.1x1.grb
#     FNABSC        Input max snow albedo climatology GRIB file
#                   defaults to ${FIXDIR}/${model}_snoalb.1x1.grb
#     OROGRAPHY     Input orography GRIB file (horiz resolution dependent)
#                   defaults to ${FIXDIR}/${model}_orography.t$JCAP.grb
#     FNMSKH        Input high resolution land mask GRIB file
#                   defaults to ${FIXDIR}/${model}_seaice_newland.grb
#     FNTSFA        Input SST analysis GRIB file
#                   defaults to none
#     FNACNA        Input sea ice analysis GRIB file
#                   defaults to none
#     FNSNOA        Input snow analysis GRIB file
#                   defaults to none
#     AERODIR       Input aersol climatology directory
#                   defaults to ${FIXDIR}
##########################################################################
#     FIX_RAD       Directory for global fixed files for radiation
#                   Defaults to ${FIXDIR}
#     EMISDIR       Input earth's surface emissivity data directory
#                   defaults to ${FIX_RAD} - export IEMS=1 to activate
#     SOLCDIR       11 year cycle Solar constat data directory
#                   defaults to ${FIX_RAD} - export ISOL=1 to activate
#     VOLCDIR       Volcanic aerosol  data directory
#                   defaults to ${FIX_RAD} - export IAER=11 or 12 to activate
#                   IAER=11 uses opac aero_volc; IAER=12 uses gocart+volc
#     CO2DIR        Historical CO2 data directory
#                   defaults to ${FIX_RAD} - export ICO2=1 or 2 to activate
#                   ICO2=1 gives annual mean and ICO2=2 uses monthly 2D data
##########################################################################
#
#     SIGR1         Output first time level sigma restart file
#                   defaults to ${DATA}/sigr1 which is deleted
#     SIGR2         Output second time level sigma restart file
#                   defaults to ${DATA}/sigr2 which is deleted
#     SFCR          Output surface restart file
#                   defaults to ${DATA}/sfcr which is deleted
#     SFCO          Output surface file with embedded forecast hour '${FH}'
#                   defaults to ${COMOUT}/sfcf'${FH}'$SUFOUT
#     LOGO          Output log file with embedded forecast hour '${FH}'
#                   defaults to ${COMOUT}/logf'${FH}'$SUFOUT
#     INISCRIPT     Preprocessing script
#                   defaults to none
#     LOGSCRIPT     Log posting script
#                   defaults to none
#     ERRSCRIPT     Error processing script
#                   defaults to 'eval [[ $err = 0 ]]'
#     ENDSCRIPT     Postprocessing script
#                   defaults to none
#     FHINI         Starting forecast hour
#                   defaults to the value in the input sigma file header
#     FHSEG         Number of hours to integrate
#                   (only required if FHMAX is not specified)
#                   defaults to 0
#     DELTIM        Timestep in seconds (integer)
#                   defaults to 3600/($JCAP/20)
#     FHRES         Restart frequency in hours
#                   defaults to 24
#     FHZER         Zeroing frequency in hours
#                   defaults to 6
#     FHLWR         Longwave radiation frequency in hours
#                   defaults to 3
#     FHSWR         Shortwave radiation frequency in hours
#                   defaults to 1
#     FHROT         Forecast hour to Read One Time level
#                   defaults to 0
#     FHDFI         Half number of hours of digital filter initialization
#                   defaults to 0
#     FHCYC         Surface cycling frequency in hours
#                   defaults to 0 for no cycling
#     IDVC          Integer ID of the vertical coordinate type
#                   defaults to that in the header for the input upperair
#                   file. IDVC=1 for sigma; IDVC=2 for pressure/sigma hybrid
#     TFILTC        Time filter coefficient
#                   defaults to 0.85
#     FCSTVARS      Other namelist inputs to the forecast executable
#                   defaults to none set
#     TRACERVARS    Other namelist inputs to the forecast executable
#                   defaults to none set
#     FSMCL2        Scale in days to relax to soil moisture climatology
#                   defaults to 99999 for no relaxation
#     FTSFS         Scale in days to relax to SST anomaly to zero
#                   defaults to 90
#     FAISS         Scale in days to relax to sea ice to climatology
#                   defaults to 99999
#     FSNOL         Scale in days to relax to snow to climatology
#                   defaults to 99999
#     CYCLEVARS     Other namelist inputs to the surface cycling
#                   defaults to none set
#     NTHREADS      Number of threads
#                   defaults to 1
#     NTHSTACK      Size of stack per thread
#                   defaults to 64000000
#     FILESTYLE     File management style flag
#                   ('L' for symbolic links in $DATA is the only allowed style)
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
#     VERBOSE       Verbose flag (YES or NO)
#                   defaults to NO
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
#
#     programs   : $AM_EXEC
#                  $OM_EXEC
#                  $C_EXEC
#
#     input data : $1 or $SIGI
#                  $2 or $SFCI
#                  $SIGI2
#                  $FNTSFA
#                  $FNACNA
#                  $FNSNOA
#
#     fixed data : $CO2CON
#                  $MTNVAR
#                  $CLTUNE
#                  $DTBTHE
#                  $O3FORC
#                  $O3CLIM
#                  $FNGLAC
#                  $FNMXIC
#                  $FNTSFC
#                  $FNSNOC
#                  $FNZORC
#                  $FNALBC
#                  $FNAISC
#                  $FNTG3C
#                  $FNVEGC
#                  $FNVETC
#                  $FNSOTC
#                  $FNSMCC
#                  $FNVMNC
#                  $FNVMXC
#                  $FNSLPC
#                  $FNABSC
#                  $FNMSKH
#                  $OROGRAPHY
#
#     output data: $3 or $SIGO
#                  $4 or $FLXO
#                  $SFCO
#                  $LOGO
#                  $SIGR1
#                  $SIGR2
#                  $SFCR
#                  $PGMOUT
#                  $PGMERR
#
#     scratch    : ${DATA}/fort.11
#                  ${DATA}/fort.12
#                  ${DATA}/fort.14
#                  ${DATA}/fort.15
#                  ${DATA}/fort.24
#                  ${DATA}/fort.27
#                  ${DATA}/fort.28
#                  ${DATA}/fort.29
#                  ${DATA}/fort.43
#                  ${DATA}/fort.48
#                  ${DATA}/fort.51
#                  ${DATA}/fort.52
#                  ${DATA}/fort.53
#                  ${DATA}/SIG.F*
#                  ${DATA}/SFC.F*
#                  ${DATA}/FLX.F*
#                  ${DATA}/LOG.F*
#                  ${DATA}/sigr1
#                  ${DATA}/sigr2
#                  ${DATA}/sfcr
#                  ${DATA}/NULL
#
# Remarks:
#
#   Condition codes
#      0 - no problem encountered
#     >0 - some problem encountered
#
#  Control variable resolution priority
#    1 Command line argument.
#    2 Environment variable.
#    3 Inline default.
#
# Attributes:
#   Language: POSIX shell
#   Machine: IBM SP
#
####
################################################################################
#  Set environment.
set -eux

export VERBOSE=${VERBOSE:-"YES"}
if [[ $VERBOSE = YES ]] ; then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi

echo
echo `date` executing forecast script
echo $LD_LIBRARY_PATH
echo

export ENS_NUM=${ENS_NUM:-1}
export FM=${FM:-""}

#  Command line arguments.

separator='-'
n_pp=1
for pp in $*; do
  if [ "$pp" = $separator ] ; then break ; fi
  n_pp=`expr $n_pp + 1`
done
if [ $n_pp -gt $# ] ; then
  echo "WARNING: no separator ( $separator ) in positional parameter list for $0 ."
  echo "         No positional parameters are recognized as OM related"
fi

export SIGI;  if [ 1 -lt $n_pp ] ; then SIGI=$1;  else SIGI=${SIGI:-?};   fi
export SFCI;  if [ 2 -lt $n_pp ] ; then SFCI=$2;  else SFCI=${SFCI:-?};   fi
export SIGO;  if [ 3 -lt $n_pp ] ; then SIGO=$3;  else SIGO=${SIGO};      fi
export FLXO;  if [ 4 -lt $n_pp ] ; then FLXO=$4;  else FLXO=${FLXO};      fi
export FHOUT; if [ 5 -lt $n_pp ] ; then FHOUT=$5; else FHOUT=${FHOUT:-3}; fi
export FHMAX; if [ 6 -lt $n_pp ] ; then FHMAX=$6; else FHMAX=${FHMAX:-0}; fi
export IGEN;  if [ 7 -lt $n_pp ] ; then IGEN=$7;  else IGEN=${IGEN:-0};   fi
export D3DO;  if [ 8 -lt $n_pp ] ; then D3DO=$8;  else D3DO=${D3DO};      fi

#  Directories.

export cfsp=${cfsp:-"cfs_"}
export FIXSUBDA=${FIXSUBDA:-fix/${cfsp}fix_am}
export FIX_RAD=${FIX_RAD:-$FIXDIR}
export DATA=${DATA:-$(pwd)}
export COMOUT=${COMOUT:-$(pwd)}
export RESDIR=${RESDIR:-$DATA}

#  Filenames.

export model=global
export model=${model:-global}
export XC=${XC:-""}
export SUFOUT=${SUFOUT:-""}
export NCP=${NCP:-/bin/cp}

export EXECcfs=${EXECcfs:-$HOMEcfs/exec}
export EXEC_AMD=${EXEC_AMD:-EXECcfs}

export FSYNCEXEC=${FSYNC}
export SIGHDR=${SIGHDR:-$EXECcfs/${cfsp}sighdr$XC}
export JCAP=${JCAP:-$(echo jcap|eval $SIGHDR $SIGI)}
export LEVS=${LEVS:-$(echo levs|eval $SIGHDR $SIGI)}
export LONR=${LONR:-$(echo lonr|eval $SIGHDR ${SIGI}$FM)}
export LATR=${LATR:-$(echo latr|eval $SIGHDR ${SIGI}$FM)}
export LONF=${LONF:-$(echo lonf|eval $SIGHDR ${SIGI}$FM)}
export LATG=${LATG:-$(echo latf|eval $SIGHDR ${SIGI}$FM)}
export NTRAC=${NTRAC:-$(echo ntrac|eval $SIGHDR ${SIGI}$FM)}
export IDVC=${IDVC:-$(echo idvc|eval $SIGHDR ${SIGI}$FM)}
export NMTVR=${NMTVR:-14}
export LSOIL=${LSOIL:-4}
export NTOZ=${NTOZ:-2}
export NTCW=${NTCW:-3}
export NCLD=${NCLD:-1}
export NGPTC=${NGPTC:-$((JCAP/10))}
export ADIAB=${ADIAB:-.false.}
export pre_rad=${pre_rad:-.false.}
export random_xkt2=${random_xkt2:-.true.}
export AM_EXEC=${AM_EXEC:-${EXEC_AMD}/global_fcst$XC}
export MTNRSL=${MTNRSL:-$JCAP}
export SIGI2=${SIGI2:-NULL}
export CO2CON=${CO2CON:-${FIXDIR}/${model}_co2con.l$LEVS.f77}
export MTNVAR=${MTNVAR:-${FIXDIR}/${model}_mtnvar.t$MTNRSL.f77}
export CLTUNE=${CLTUNE:-${FIXDIR}/${model}_cldtune.f77}
export DTBTHE=${DTBTHE:-${FIXDIR}/${model}_tbthe.f77}
export O3FORC=${O3FORC:-${FIXDIR}/global_o3prdlos.f77}
export O3CLIM=${O3CLIM:-${FIXDIR}/${model}_o3clim.txt}
export FNGLAC=${FNGLAC:-${FIXDIR}/${model}_glacier.2x2.grb}
export FNMXIC=${FNMXIC:-${FIXDIR}/${model}_maxice.2x2.grb}
export FNTSFC=${FNTSFC:-${FIXDIR}/cfs_oi2sst1x1monclim19822001.grb}
export FNSNOC=${FNSNOC:-${FIXDIR}/${model}_snoclim.1.875.grb}
export FNZORC=${FNZORC:-${FIXDIR}/${model}_zorclim.1x1.grb}
export FNALBC=${FNALBC:-${FIXDIR}/${model}_albedo4.1x1.grb}
export FNAISC=${FNAISC:-${FIXDIR}/cfs_ice1x1monclim19822001.grb}
export FNTG3C=${FNTG3C:-${FIXDIR}/${model}_tg3clim.2.6x1.5.grb}
export FNVEGC=${FNVEGC:-${FIXDIR}/${model}_vegfrac.0.144.decpercent.grb}
export FNVETC=${FNVETC:-${FIXDIR}/${model}_vegtype.1x1.grb}
export FNSOTC=${FNSOTC:-${FIXDIR}/${model}_soiltype.1x1.grb}
export FNSMCC=${FNSMCC:-${FIXDIR}/${model}_soilmcpc.1x1.grb}
export FNVMNC=${FNVMNC:-${FIXDIR}/${model}_shdmin.0.144x0.144.grb}
export FNVMXC=${FNVMXC:-${FIXDIR}/${model}_shdmax.0.144x0.144.grb}
export FNSLPC=${FNSLPC:-${FIXDIR}/${model}_slope.1x1.grb}
export FNABSC=${FNABSC:-${FIXDIR}/${model}_snoalb.1x1.grb}
export FNMSKH=${FNMSKH:-${FIXDIR}/seaice_newland.grb}
export OROGRAPHY=${OROGRAPHY:-${FIXDIR}/${model}_orography.t$MTNRSL.grb}
export FNTSFA=${FNTSFA:-""}
export FNACNA=${FNACNA:-""}
export FNSNOA=${FNSNOA:-""}

export AERODIR=${AERODIR:-${FIX_RAD}}
export EMISDIR=${EMISDIR:-${FIX_RAD}}
export SOLCDIR=${SOLCDIR:-${FIX_RAD}}
export VOLCDIR=${VOLCDIR:-${FIX_RAD}}
export CO2DIR=${CO2DIR:-${FIX_RAD}}
export IEMS=${IEMS:-0}
export ISOL=${ISOL:-0}
export IAER=${IAER:-0}
export ICO2=${ICO2:-0}

export SIGR1=${SIGR1:-${DATA}/sigr1}
export SIGR2=${SIGR2:-${DATA}/sigr2}
export SFCR=${SFCR:-${DATA}/sfcr}
export SIGO=${SIGO:-${COMOUT}/sigf'${FH}'$SUFOUT}
export SFCO=${SFCO:-${COMOUT}/sfcf'${FH}'$SUFOUT}
export FLXO=${FLXO:-${COMOUT}/flxf'${FH}'$SUFOUT}
export LOGO=${LOGO:-${COMOUT}/logf'${FH}'$SUFOUT}
export D3DO=${D3DO:-${COMOUT}/d3df'${FH}'$SUFOUT}
export INISCRIPT=${INISCRIPT:-""}
export ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}
export LOGSCRIPT=${LOGSCRIPT:-""}
export ENDSCRIPT=${ENDSCRIPT:-""}

#  Other variables.

export FHINI=${FHINI:-$(echo ifhr|eval $SIGHDR $SIGI)}
export CDATE=${CDATE:-$(echo idate|eval $SIGHDR $SIGI)}
export FHSEG=${FHSEG:-0}
export FHMAX=${FHMAX:-$((FHINI+FHSEG))}
export DELTIM=${DELTIM:-$((3600/(JCAP/20)))}
export FHRES=${FHRES:-24}
export FHZER=${FHZER:-6}
export FHLWR=${FHLWR:-1}
export FHSWR=${FHSWR:-1}
export FHROT=${FHROT:-0}
export FHDFI=${FHDFI:-0}

export RUN_NAME=${RUN_NAME:-cfs_fcst}
export RESTART_CONTROL_FILE=${RESTART_CONTROL_FILE:-$DATA/$RUN_NAME.2restart$SUFOUT}
if [ ! -f $RESTART_CONTROL_FILE ] ; then
  FHDFI=${FHDFI_INIT:-$FHDFI}
  echo "FHDFI=$FHDFI"
fi

env>env.out

export FHCYC=${FHCYC:-0}
export gfsio_in=${gfsio_in:-.false.}
export gfsio_out=${gfsio_out:-.false.}
export FCSTVARS="gfsio_in=$gfsio_in,gfsio_out=$gfsio_out,$FCSTVARS"
export GB=${GB:-0}
if [ $gfsio_in = .true. ] ; then
 export GB=1
fi
if [ $IDVC = 1 ] ; then
 export HYBRID=.false.
 export GEN_COORD_HYBRID=.false.
elif [ $IDVC = 2 ] ; then
 export HYBRID=.true.
 export GEN_COORD_HYBRID=.false.
elif [ $IDVC = 3 ] ; then
 export GEN_COORD_HYBRID=.true.
 export HYBRID=.false.
fi

export TFILTC=${TFILTC:-0.85}
export FCSTVARS=${FCSTVARS:-""}
export TRACERVARS=${TRACERVARS:-""}
export FSMCL2=${FSMCL2:-99999}
export FTSFS=${FTSFS:-90}
export FAISS=${FAISS:-99999}
export FSNOL=${FSNOL:-99999}
export FSICL=${FSICL:-99999}
export FSICS=${FSICS:-99999}
export CYCLVARS=${CYCLVARS}
export NTHREADS=${NTHREADS:-1}
export NTHSTACK=${NTHSTACK:-128000000}
export XLSMPOPTS=${XLSMPOPTS:-"parthds=$NTHREADS:stack=$NTHSTACK"}
export OMP_NUM_THREADS=${OMP_NUM_THREADS:-$NTHREADS}
export FILESTYLE=${FILESTYLE:-'L'}
export PGMOUT=${PGMOUT:-${pgmout:-'&1'}}
export PGMERR=${PGMERR:-${pgmerr:-'&2'}}
export MEMBER_NAMES=${MEMBER_NAMES:-''}

export REDOUT=${REDOUT:-'1>'}
export REDERR=${REDERR:-'2>'}

################################################################################
#  Preprocessing

$INISCRIPT
pwd=$(pwd)
if [[ -d $DATA ]] ; then
   mkdata=NO
else
   mkdir -p $DATA
   mkdata=YES
fi
cd $DATA||exit 99
[[ -d $COMOUT ]]||mkdir -p $COMOUT

################################################################################

#  Make forecast
export XLFRTEOPTS="unit_vars=yes:intrinthds=1"
export PGM=$DATA/$(basename $AM_EXEC)
export pgm=$PGM
$LOGSCRIPT
$NCP $AM_EXEC $DATA
rm -f NULL
FH=$((10#$FHINI))
[[ $FH -lt 10 ]]&&FH=0$FH
if [[ $FHINI -gt 0 ]] ; then
   FH=$((10#$FHINI+10#$FHOUT))
   [[ $FH -lt 10 ]]&&FH=0$FH
fi
while [[ $FH -le $FHMAX ]] ; do
   eval rm -f $LOGO
   ((FH=10#$FH+10#$FHOUT))
   [[ $FH -lt 10 ]]&&FH=0$FH
done
if [[ $FILESTYLE = "L" ]] ; then
   ln -fs $CO2CON fort.15
   ln -fs $MTNVAR fort.24
   ln -fs $DTBTHE fort.27
   ln -fs $O3FORC fort.28
   ln -fs $CLTUNE fort.43
   ln -fs $O3CLIM fort.48
else
  echo 'FILESTYLE' $FILESTYLE 'NOT SUPPORTED'
  export err=222
  err_exit $err
fi
AEROSOL_FILE=${AEROSOL_FILE:-global_climaeropac_global.txt}
ln -fs $AERODIR/$AEROSOL_FILE     aerosol.dat
ln -fs $OROGRAPHY orography
if [ $IEMS -gt 0 ] ; then
 EMMISSIVITY_FILE=${EMMISSIVITY_FILE:-global_sfc_emissivity_idx.txt}
 ln -fs $EMISDIR/$EMMISSIVITY_FILE sfc_emissivity_idx.txt
#export FCSTVARS="IEMS=$IEMS,$FCSTVARS"
fi
if [ $ISOL -gt 0 ] ; then
 $NCP $SOLCDIR/*solarconstantdata.txt solarconstantdata.txt
#export FCSTVARS="ISOL=$ISOL,$FCSTVARS"
fi
if [ $IAER -gt 0 ] ; then
 cd $VOLCDIR
#$NCP volcanic_aerosols*.txt $DATA
 for file in `ls | grep volcanic_aerosols` ; do
  $NCP $file $DATA/$(echo $file |sed -e "s/global_//g")
 done
#export FCSTVARS="IAER=$IAER,$FCSTVARS"
fi
if [ $ICO2 -gt 0 ] ; then
 cd $CO2DIR
 for file in `ls | grep co2historicaldata` ; do
  $NCP $file $DATA/$(echo $file |sed -e "s/global_//g")
 done
 CO2_seasonal_cycle=${CO2_seasonal_cycle:-global_co2monthlycyc1976_2006.txt}
 $NCP $CO2_seasonal_cycle $DATA/co2monthlycyc.txt
#export FCSTVARS="ICO2=$ICO2,$FCSTVARS"
fi
cd $DATA
export FCSTVARS="IEMS=$IEMS,ISOL=$ISOL,IAER=$IAER,ICO2=$ICO2,$FCSTVARS"
#
#     For one member case i.e. control
#
if [[ $ENS_NUM -le 1 ]] ; then
  FH=$((10#$FHINI))
  [[ $FH -lt 10 ]]&&FH=0$FH
  if [[ $FHINI -gt 0 ]] ; then
    FH=$((10#$FHINI+10#$FHOUT))
    [[ $FH -lt 10 ]]&&FH=0$FH
  fi
#        For Initial Conditions
  ln -fs $SIGI sig_ini
  ln -fs $SFCI sfc_ini
  ln -fs $SIGI2 sig_ini2
#        For output
  while [[ $FH -le $FHMAX ]] ; do
    eval ln -fs $SIGO SIG.F${FH}
    eval ln -fs $SFCO SFC.F${FH}
    eval ln -fs $FLXO FLX.F${FH}
    eval ln -fs $LOGO LOG.F${FH}
    eval ln -fs $D3DO D3D.F${FH}
    ((FH=10#$FH+10#$FHOUT))
    [[ $FH -lt 10 ]]&&FH=0$FH
  done
  ln -fs $SIGR1 SIGR1
  ln -fs $SIGR2 SIGR2
  ln -fs $SFCR  SFCR
else
#
#   For Ensemble runs (members > 1)
  for MN in $MEMBER_NAMES ; do
#      This is just faking the ensemble ICs.
#   $NCP $SIGI  ${SIGI}${MN}
#   $NCP $SFCI  ${SFCI}${MN}
#   $NCP $SIGI2 ${SIGI2}${MN}
#        For Initial Conditions
    eval ln -fs ${SIGI}${MN}  sig_ini${MN}
    eval ln -fs ${SFCI}${MN}  sfc_ini${MN}
    eval ln -fs ${SIGI2}${MN} sig_ini2${MN}

#        For output

    FH=$((10#$FHINI))
    [[ $FH -lt 10 ]]&&FH=0$FH
    if [[ $FHINI -gt 0 ]] ; then
      FH=$((10#$FHINI+10#$FHOUT))
      [[ $FH -lt 10 ]]&&FH=0$FH
    fi
    while [[ $FH -le $FHMAX ]] ; do
      eval ln -fs $SIGO SIG.F${FH}${MN}
      eval ln -fs $SFCO SFC.F${FH}${MN}
      eval ln -fs $FLXO FLX.F${FH}${MN}
      eval ln -fs $LOGO LOG.F${FH}${MN}
      eval ln -fs $D3DO D3D.F${FH}${MN}
      ((FH=10#$FH+10#$FHOUT))
      [[ $FH -lt 10 ]]&&FH=0$FH
    done
    eval ln -fs ${SIGR1}${MN} SIGR1${MN}
    eval ln -fs ${SIGR2}${MN} SIGR2${MN}
    eval ln -fs ${SFCR}${MN}  SFCR${MN}
  done
fi
#
# Create Configure file (i.e. .rc file) here
# PE$n are to be imported from outside.  If PE$n are not set from outside, the
# model would give equal processors for all ensembel members.

 c=1
 while [ $c -le 21 ] ; do
  eval export PE$c=\${PE$c:-0}
  c=$((c+1))
 done

 export PE1=$NPROCS_a

cat << EOF > gfs_namelist.rc

#nam_gfs +++++++++++++++++++++++++++
NLUNIT:                  35
DELTIM:                  ${DELTIM}.0
NAMELIST:                gfs_namelist
TOTAL_MEMBER:            $ENS_NUM
GRIB_INPUT:              $GB
PE_MEMBER01:             $PE1
PE_MEMBER02:             $PE2
PE_MEMBER03:             $PE3
PE_MEMBER04:             $PE4
PE_MEMBER05:             $PE5
PE_MEMBER06:             $PE6
PE_MEMBER07:             $PE7
PE_MEMBER08:             $PE8
PE_MEMBER09:             $PE9
PE_MEMBER10:             $PE10
PE_MEMBER11:             $PE11
PE_MEMBER12:             $PE12
PE_MEMBER13:             $PE13
PE_MEMBER14:             $PE14
PE_MEMBER14:             $PE15
PE_MEMBER16:             $PE16
PE_MEMBER17:             $PE17
PE_MEMBER18:             $PE18
PE_MEMBER19:             $PE19
PE_MEMBER20:             $PE20
PE_MEMBER21:             $PE21

#ESMF_State_Namelist +++++++++++++++
IDATE1_IMPORT:                    0
Z_IMPORT:                         0
PS_IMPORT:                        0
VOR_IMPORT:                       0
DIV_IMPORT:                       0
TEMP_IMPORT:                      0
Q_IMPORT:                         0
OZ_IMPORT:                        0
SCLD_IMPORT:                      0

IDATE1_EXPORT:                    0
Z_EXPORT:                         0
PS_EXPORT:                        0
VOR_EXPORT:                       0
DIV_EXPORT:                       0
TEMP_EXPORT:                      0
Q_EXPORT:                         0
OZ_EXPORT:                        0
SCLD_EXPORT:                      0

# Surface state.
#---------------
OROGRAPHY_IMPORT:                 0
T_SKIN_IMPORT:                    0
SOIL_MOIS_IMPORT:                 0
SNOW_DEPTH_IMPORT:                0
SOIL_T_IMPORT:                    0
DEEP_SOIL_T_IMPORT:               0
ROUGHNESS_IMPORT:                 0
CONV_CLOUD_COVER_IMPORT:          0
CONV_CLOUD_BASE_IMPORT:           0
CONV_CLOUD_TOP_IMPORT:            0
ALBEDO_VISIBLE_SCATTERED_IMPORT:  0
ALBEDO_VISIBLE_BEAM_IMPORT:       0
ALBEDO_NEARIR_SCATTERED_IMPORT:   0
ALBEDO_NEARIR_BEAM_IMPORT:        0
SEA_LEVEL_ICE_MASK_IMPORT:        0
VEGETATION_COVER_IMPORT:          0
CANOPY_WATER_IMPORT:              0
M10_WIND_FRACTION_IMPORT:         0
VEGETATION_TYPE_IMPORT:           0
SOIL_TYPE_IMPORT:                 0
ZENEITH_ANGLE_FACSF_IMPORT:       0
ZENEITH_ANGLE_FACWF_IMPORT:       0
UUSTAR_IMPORT:                    0
FFMM_IMPORT:                      0
FFHH_IMPORT:                      0
SEA_ICE_THICKNESS_IMPORT:         0
SEA_ICE_CONCENTRATION_IMPORT:     0
TPRCP_IMPORT:                     0
SRFLAG_IMPORT:                    0
ACTUAL_SNOW_DEPTH_IMPORT:         0
LIQUID_SOIL_MOISTURE_IMPORT:      0
VEGETATION_COVER_MIN_IMPORT:      0
VEGETATION_COVER_MAX_IMPORT:      0
SLOPE_TYPE_IMPORT:                0
SNOW_ALBEDO_MAX_IMPORT:           0

OROGRAPHY_EXPORT:                 0
T_SKIN_EXPORT:                    0
SOIL_MOIS_EXPORT:                 0
SNOW_DEPTH_EXPORT:                0
SOIL_T_EXPORT:                    0
DEEP_SOIL_T_EXPORT:               0
ROUGHNESS_EXPORT:                 0
CONV_CLOUD_COVER_EXPORT:          0
CONV_CLOUD_BASE_EXPORT:           0
CONV_CLOUD_TOP_EXPORT:            0
ALBEDO_VISIBLE_SCATTERED_EXPORT:  0
ALBEDO_VISIBLE_BEAM_EXPORT:       0
ALBEDO_NEARIR_SCATTERED_EXPORT:   0
ALBEDO_NEARIR_BEAM_EXPORT:        0
SEA_LEVEL_ICE_MASK_EXPORT:        0
VEGETATION_COVER_EXPORT:          0
CANOPY_WATER_EXPORT:              0
M10_WIND_FRACTION_EXPORT:         0
VEGETATION_TYPE_EXPORT:           0
SOIL_TYPE_EXPORT:                 0
ZENEITH_ANGLE_FACSF_EXPORT:       0
ZENEITH_ANGLE_FACWF_EXPORT:       0
UUSTAR_EXPORT:                    0
FFMM_EXPORT:                      0
FFHH_EXPORT:                      0
SEA_ICE_THICKNESS_EXPORT:         0
SEA_ICE_CONCENTRATION_EXPORT:     0
TPRCP_EXPORT:                     0
SRFLAG_EXPORT:                    0
ACTUAL_SNOW_DEPTH_EXPORT:         0
LIQUID_SOIL_MOISTURE_EXPORT:      0
VEGETATION_COVER_MIN_EXPORT:      0
VEGETATION_COVER_MAX_EXPORT:      0
SLOPE_TYPE_EXPORT:                0
SNOW_ALBEDO_MAX_EXPORT:           0

EOF

#------------------------------------------------------------------------------#
# End pre-execution section of AM script (1)
#------------------------------------------------------------------------------#

if [ $n_pp -gt $# ] ; then
  shift $#
else
  shift $n_pp
fi

AMpre_dir=`pwd`
export PGM_am=$PGM

#------------------------------------------------------------------------------#
# Begin pre-execution section of OM script (2)
#------------------------------------------------------------------------------#

#
########################################################################
#  This script runs the MOM_4 ocean model  for the number of days      #
#  specified in the namelist "namelist.control".  In the SLC           #
#  this is set to be equal to one day.                                 #
########################################################################
#
#omres=${omres:-1x1}
omres=${omres:-05}

export CASE=basic_ibm                   ;# Define experiment paths

export WORKDIR=$1                       ;# forecast job directory
export VDATE=$2                         ;# current forecast (model) time
#export mom3rt=$3                        ;# restoring time scale (days)
export EXEC_OMD=${3:-$EXECcfs}
                                         # MOM4 script/exec directory
export COM_YMDH=${4:-$COM_YMDH}
#
export ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}

echo VDATE = $VDATE beginning MOM4ICE `date`
#
export DATAOUTpath=$WORKDIR/data/mom4ice
#
if [ ! -s $WORKDIR ] ; then
  echo "$WORKDIR does not exist."
  export err=1; err_chk
fi
if [ ! -s $DATAOUTpath ] ; then
 mkdir -p $DATAOUTpath
fi
cd $WORKDIR

if [ $WORKDIR != $DATA ] ; then
  echo "WORKDIR=$WORKDIR differs from DATA=$DATA : must be the same"
  export err=1; err_chk
fi
#
# MOM4ICE setting
#
INCHOUR=${INCHOUR:-$((FHMAX-FHINI))}
hh_inc_ocn=$FHOUT

export EXEC_OMD=${EXEC_OMD:-$EXECcfs}
export OM_EXEC=${OM_EXEC:-${EXEC_OMD}/${cfsp}ocean_mom4ice}
export PGM=$WORKDIR/$(basename $OM_EXEC)
export pgm=$PGM
$NCP $OM_EXEC $WORKDIR
export PARM_OM=${PARM_OM:-$HOMEcfs/parm/${cfsp}parm_om}
export FIX_OM=${FIX_OM:-$HOMEcfs/fix/${cfsp}fix_om}
#
#export days=${days:-1}
export months=${months:-0}
#export yyyy0=${yyyy0:-2002}
# export MOM4ICEDIR=${MOM4ICEDIR:-/climate/save/wx20xw/MOM4ICE}
#
export WORKDIR=$WORKDIR           ; mkdir -p $WORKDIR
export INPUT=$WORKDIR/INPUT       ; mkdir -p $INPUT
export OUTPUT=$WORKDIR/OUTPUT     ; mkdir -p $OUTPUT
export RESTART=$WORKDIR/RESTART   ; mkdir -p $RESTART
export IRESTART=$WORKDIR/IRESTART ; mkdir -p $IRESTART
#
# export executable=$MOM4ICEXEC
export diagtable=${diagtable:-$PARM_OM/diag_table.hrs}
export fieldtable=${fieldtable:-$PARM_OM/field_table}
export datatable=${datatable:-$PARM_OM/data_override}
export namelist=${namelist:-$PARM_OM/namelist}
#
export grid_mom4ice=${grid_mom4ice:-$FIX_OM/grid_spec_$omres.nc.T$JCAP}
export chl=${chl:-$FIX_OM/chl_$omres.nc}
export oisst_clim=${oisst_clim:-$FIX_OM/oisst_clim.nc}
export r2ts_clim=${r2ts_clim:-$FIX_OM/r2ts_clim.nc}
export sst_ice_clim=${sst_ice_clim:-$FIX_OM/sst_ice_clim.nc}
export salt_sfc_restore=${salt_sfc_restore:-$FIX_OM/salt_sfc_restore_$omres.nc}
export temp_sfc_restore=${temp_sfc_restore:-$FIX_OM/temp_sfc_restore_$omres.nc}
export runoff=${runoff:-$FIX_OM/runoff_$omres.nc}
export ohf_sice=${ohf_sice:-$FIX_OM/ohf_sice.nc}
#
# export mppnccombine=${mppnccombine:-$MOM4ICEDIR/bin/mppnccombine.ibm}
# export PGM=$WORKDIR/$(basename $executable)
# export bdate=$VDATE
# export nxtdate=`$NDATE $FHCYC $VDATE`
# export cdate=`echo $bdate | cut -c1-8`
# export edate=`echo $nxtdate | cut -c1-8`
#
# get data sets, input data and executable
#
start_date=$($NDATE $FHINI $CDATE)
end_date=$($NDATE $FHMAX $CDATE)
syyyy=$(echo $start_date | cut -c1-4)
smm=$(echo $start_date | cut -c5-6)
sdd=$(echo $start_date | cut -c7-8)
shh=$(echo $start_date | cut -c9-10)
cat > input.nml <<EOF
        &coupler_nml
            months = $months
            days   = 0
            current_date = $syyyy,$smm,$sdd,$shh,0,0
            hours = $INCHOUR
            minutes = 0
            seconds = 0
            calendar = 'julian'
            dt_cpld  = ${dt_cpld:-3600}
            dt_ocean = ${dt_ocean:-3600}
            dt_atmos = ${DELTIM:-600}
            dt_aocpl = ${dt_aocpl:-${dt_ocean:-3600}}
            dt_rstrt = ${dt_rstrt:-$dt_cpld}
            do_irestart = $do_irestart
            do_atmos = .false.
            do_land = .false.
            do_ice = .true.
            do_ocean = .true.
            concurrent = .false.
            use_lag_fluxes=.true. /
EOF
#
cat $namelist >> input.nml
#
$NCP -p $datatable  data_table
$NCP -p $fieldtable field_table
$NCP -p $diagtable  diag_table
#  Modify diag table to reflect the correct output interval
if [ $FHOUT -lt 10 ] ; then
 sed "s/hh/ $FHOUT/g" diag_table >diag_table_nu
 /bin/mv diag_table_nu diag_table
elif [ $FHOUT -lt 100 ] ; then
 sed "s/hh/$FHOUT/g" diag_table >diag_table_nu
 /bin/mv diag_table_nu diag_table
fi
#
# cd $INPUT
$NCP -p $oisst_clim       $INPUT/oisst_clim.nc
$NCP -p $r2ts_clim        $INPUT/r2ts_clim.nc
$NCP -p $sst_ice_clim     $INPUT/sst_ice_clim.nc
$NCP -p $salt_sfc_restore $INPUT/salt_sfc_restore.nc
$NCP -p $temp_sfc_restore $INPUT/temp_sfc_restore.nc
$NCP -p $grid_mom4ice     $INPUT/grid_spec.nc
$NCP -p $chl              $INPUT/chl.nc
$NCP -p $runoff           $INPUT/runoff.nc
$NCP -p $ohf_sice         $INPUT/ohf_sice.nc
$NCP -p $FIX_OM/MOM4LND${omres}GFSOCNt$JCAP.msk  MOM4LND_GFSOCN
#
echo "Retrieving climatological R2 forcing data..."
#
$NCP -p $FIX_OM/fluxes_init_OM_t$JCAP   fluxes_init_OM

rc=$?
if [ $rc -ne 0 ] ; then
  echo "Failure to copy climatological R2 forcing data, exiting"
  export err=$?; err_chk
else
  echo "Climatological R2 forcing data copied from $FIX_OM/fluxes_init_OM_t$JCAP"
fi
#
cd $WORKDIR

#------------------------------------------------------------------------------#
# End pre-execution section of OM script (2)
#------------------------------------------------------------------------------#

OMpre_dir=`pwd`
   # <- must be $WORKDIR
export PGM_oc=$PGM

#------------------------------------------------------------------------------#
# Begin pre-execution section of Coupler script (3)
#------------------------------------------------------------------------------#

#   Shell Variables to form Coupler namelist (define here if required):
#   variable      value              purpose                                    default
# C_SHT_STR   logical (T/F)   T: climatological rather than AM stress for OM  F
# C_SHT_Q     logical (T/F)   T: climatological rather than AM h. fl. for OM  F
# C_SHT_EmP   logical (T/F)   T: climatological rather than AM E-P    for OM  T
# C_SHT_SWR   logical (T/F)   T: climatological rather than AM SWR    for OM  F
# C_MTL    nonnegative integer      "mask tolerance level"                    2
# C_NODQ      logical (T/F)   T: dQ/dT NOT used                               T
# C_NO_ACT_AM_SI logical (T/F) T: NO action under AM sea ice                  T
# C_OM_UL     real            thickness (m) of OM uppermost layer, does not
#                              matter if C_NODQ=T or C_NO_ACT_AM_SI=T        50.
# C_EFT       real            e-folding time value (s) for OM 
#                             uppermost layer, does not matter
#                             if C_NODQ=T or C_NO_ACT_AM_SI=T            864000.
# C_nprint  positive integer     printout file control                       6
# C_VL     nonnegative integer    "verbosity level",
#                                 relates also to AM, OM                     2
# C_nprper   positive integer       C printout control                      120
# C_npr1st  nonnegative integer     C printout control                      120

export EXEC_CD=${EXEC_CD:-$EXECcfs}
export C_EXEC=${C_EXEC:-${EXEC_CD}/${cfsp}mlc_coupler}
export PGM=$WORKDIR/$(basename $C_EXEC)
export PGM_c=$PGM
$NCP $C_EXEC $WORKDIR

# inserted:
if [ $FHINI -gt 0 ] ; then
 if [ -f $RESTART_CONTROL_FILE -a ! -f fluxes_for_OM ] ; then
   echo "RESTART_CONTROL_FILE=$RESTART_CONTROL_FILE exists but fluxes_for_OM does not: aborting"
   export err=1; err_chk
 fi
fi 
#<-:inserted
# substituted:
#  To get coupler print diagnostics, set C_VL to 2 or 3
#
export C_VL=${C_VL:-1}
if [ -s fluxes_for_OM -a $FHINI -gt 0 ] ; then
  C_restart=T
  echo 'fluxes_for_OM present: C_restart = T assigned'
else
  C_restart=F
  echo 'fluxes_for_OM absent: C_restart = F assigned FHINI='$FHINI
fi

CouplingPeriod=${CouplingPeriod:-${DELTIM:-3600}}
#C_ostepmax=`expr $INCHOUR \* 3600 \/ $CouplingPeriod`
#C_cstepmax=$((INCHOUR*3600/CouplingPeriod))
C_cstepbeg=$((FHINI*3600/CouplingPeriod+1))
C_cstepmax=$((FHMAX*3600/CouplingPeriod))
C_cstepres=$((dt_rstrt/CouplingPeriod))
#
C_write_am_sst=${C_write_am_sst:-.true.}
if [ $C_write_am_sst = .true. ] ; then
  eval ln -fs $AM_SST AM_SST
# eval ln -fs AM_SST_${FHINI}_${FHMAX} AM_SST
fi
#
#if [[ $JCAP = 126 ]] ; then C_IIF=2 ; fi
C_IIF=2

set +u
nl=cpl_nml
echo "Creating C namelist file $nl ..."
echo '&CPL_SETTINGS'>$nl
echo "restart=$C_restart,">>$nl
echo "cstepbeg=$C_cstepbeg,">>$nl
echo "cstepmax=$C_cstepmax,">>$nl
echo "cstepres=$C_cstepres,">>$nl
#echo "ostepmax=$C_ostepmax,">>$nl
if [ "$CouplingPeriod" ] ; then echo "dt_c=$CouplingPeriod,">>$nl; fi
if [ "$C_NO_AM_SST" ]    ; then echo "no_AM_SST=$C_NO_AM_SST,">>$nl; fi
if [ "$C_nprint" ]       ; then echo "nprint=$C_nprint,">>$nl; fi
if [ "$C_VL" ]           ; then echo "VerbLev=$C_VL,">>$nl; fi
if [ "$C_nprper" ]       ; then echo "nprper=$C_nprper,">>$nl; fi
if [ "$C_npr1st" ]       ; then echo "npr1st=$C_npr1st">>$nl; fi
if [ "$C_write_am_sst" ] ; then echo "write_am_sst=$C_write_am_sst">>$nl; fi
if [ "$cice_cover" ]     ; then echo "cice_cover=$cice_cover">>$nl; fi
echo '/'>>$nl
set -u 

cat $nl


#------------------------------------------------------------------------------#
# End pre-execution section of Coupler script (3)
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Begin MPMD execution section (4)
#------------------------------------------------------------------------------#

NPROCS_c=${NPROCS_c:-0}
NPROCS_a=${NPROCS_a:-0}
NPROCS_o=${NPROCS_o:-0}
if [ $NPROCS_c = 0 -a $NPROCS_a != 0 -a $NPROCS_o != 0 -o $NPROCS_c != 0 -a $NPROCS_c != 1 ] ; then
  echo "Illegal combination of numbers of CPUs: NPROCS_c=$NPROCS_c, NPROCS_a=$NPROCS_a, NPROCS_o=$NPROCS_o"
  export err=1; err_chk
fi
NPROCS=$((NPROCS_c+NPROCS_a+NPROCS_o))


cat  > gfs_namelist <<EOF
 &nam_mrf
  FHOUT=$FHOUT, FHMAX=$FHMAX, IGEN=$IGEN, DELTIM=$DELTIM,
  FHRES=$FHRES, FHZER=$FHZER, FHLWR=$FHLWR, FHSWR=$FHSWR,
  FHROT=$FHROT, FHDFI=$FHDFI, FHCYC=$FHCYC,
  ntrac=$NTRAC,nxpt=1,nypt=2,jintmx=2,jcap=$JCAP,levs=$LEVS,lonf=$LONF,
  lonr=$LONR,latg=$LATG,latr=$LATR,ntoz=$NTOZ,ntcw=$NTCW,ncld=$NCLD,lsoil=$LSOIL,
  nmtvr=$NMTVR, ngptc=$NGPTC,hybrid=$HYBRID,tfiltc=$TFILTC,
  gen_coord_hybrid=$GEN_COORD_HYBRID,
  $FCSTVARS /
&TRACER_CONSTANT
  $TRACERVARS /
&SOIL_VEG
  LPARAM = .FALSE./
&NAMSFC
  FNGLAC="$FNGLAC",
  FNMXIC="$FNMXIC",
  FNTSFC="$FNTSFC",
  FNSNOC="$FNSNOC",
  FNZORC="$FNZORC",
  FNALBC="$FNALBC",
  FNAISC="$FNAISC",
  FNTG3C="$FNTG3C",
  FNVEGC="$FNVEGC",
  FNVETC="$FNVETC",
  FNSOTC="$FNSOTC",
  FNSMCC="$FNSMCC",
  FNMSKH="$FNMSKH",
  FNTSFA="$FNTSFA",
  FNACNA="$FNACNA",
  FNSNOA="$FNSNOA",
  FNVMNC="$FNVMNC",
  FNVMXC="$FNVMXC",
  FNSLPC="$FNSLPC",
  FNABSC="$FNABSC",
  LDEBUG=.false.,
  FSMCL(2)=$FSMCL2,
  FSMCL(3)=$FSMCL2,
  FSMCL(4)=$FSMCL2,
  FTSFS=$FTSFS,
  FAISS=$FAISS,
  FSNOL=$FSNOL,
  FSICL=$FSICL,
  FTSFL=99999,
  FAISL=99999,
  FVETL=99999,
  FSOTL=99999,
  FvmnL=99999,
  FvmxL=99999,
  FSLPL=99999,
  FABSL=99999,
  FSNOS=99999,
  FSICS=$FSICS,
  $CYCLVARS /
EOF

#------------------------------------------------------------------------------#
# run the coupled model
#------------------------------------------------------------------------------#

export OMP_NUM_THREADS=2
export OMP_PROC_BIND=true
export OMP_STACKSIZE=2048m 
export thread="--depth $OMP_NUM_THREADS --cpu-bind depth"

time mpiexec --cpu-bind core -n $NPROCS_c $thread $PGM_c : -n $NPROCS_o $thread $PGM_oc : -n $NPROCS_a $thread $PGM_am   ##1>out.poe.$nhourb 2>err.poe.$nhourb
export ERR=$?; export err=$ERR; err_chk

#------------------------------------------------------------------------------#
# End MPMD execution section (4)
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Begin post-execution section of OM script (5)
#------------------------------------------------------------------------------#

echo VDATE = $VDATE after MOM4 run `date`

# ==============================================================
# Do post-processing and save data to the ARCHIVE.
# (appending date suffix to filenames for identification)
# ==============================================================
#
#
# gather the daily or hourly data from each processor into one file
# each file in ocn_list or ice_list is from an individual processor
#
#

export mppnccombine=${mppnccombine:-$EXECcfs/${cfsp}mppnccombine}
export mpinccombine=${mpinccombine:-$EXECcfs/${cfsp}mpinccombine}

> cmdfile_mpp

#  make the mpi_combine  filelist

hh_inc_m=$((hh_inc_ocn/2))
m_date=$($NDATE $hh_inc_m $start_date)
p_date=$($NDATE $hh_inc_ocn $start_date)
set -x
until [ $p_date -gt $end_date ] ; do
  year=`echo $p_date | cut -c1-4`
  month=`echo $p_date | cut -c5-6`
  day=`echo $p_date | cut -c7-8`
  hh=`echo $p_date | cut -c9-10`
  hh=$((hh+0)) ; if [ $hh -lt 10 ]  ; then hh=0$hh ; fi
  yearm=`echo $m_date | cut -c1-4`
  monthm=`echo $m_date | cut -c5-6`
  daym=`echo $m_date | cut -c7-8`
  hhm=`echo $m_date | cut -c9-10`
  export ocnfile=ocn_${yearm}_${monthm}_${daym}_${hhm}.nc
  export icefile=ice_${yearm}_${monthm}_${daym}_${hhm}.nc
  echo $ocnfile >> cmdfile_mpp
  echo $icefile >> cmdfile_mpp
  p_date=$($NDATE $hh_inc_ocn $p_date)
  m_date=$($NDATE $hh_inc_ocn $m_date)
done
set -x

#  mpi_combine the decomposed ocean files into global files

ncmds=$(wc -l cmdfile_mpp|sed 's/cmdfile_mpp//')
[[ $NCPUS -lt $ncmds ]] && ncmds=$NCPUS  

mpiexec -n $ncmds $mpinccombine $mppnccombine cmdfile_mpp  
export err=$?; err_chk

#  adjust the date stamps on the combined files

m_date=$($NDATE $hh_inc_m $start_date)
p_date=$($NDATE $hh_inc_ocn $start_date)
set -x
until [ $p_date -gt $end_date ] ; do
  year=`echo $p_date | cut -c1-4`
  month=`echo $p_date | cut -c5-6`
  day=`echo $p_date | cut -c7-8`
  hh=`echo $p_date | cut -c9-10`
  yearm=`echo $m_date | cut -c1-4`
  monthm=`echo $m_date | cut -c5-6`
  daym=`echo $m_date | cut -c7-8`
  hhm=`echo $m_date | cut -c9-10`

  export ocnfile=ocn_${yearm}_${monthm}_${daym}_${hhm}.nc
  export icefile=ice_${yearm}_${monthm}_${daym}_${hhm}.nc

  echo mv $ocnfile $COM_YMDH/ocn_${year}_${month}_${day}_${hh}${SUFOUT}.nc
  echo mv $icefile $COM_YMDH/ice_${year}_${month}_${day}_${hh}${SUFOUT}.nc

  mv $ocnfile $COM_YMDH/ocn_${year}_${month}_${day}_${hh}${SUFOUT}.nc
  rc=$? ; if [[ $rc -ne 0 ]] ; then exit 1 ; fi
  mv $icefile $COM_YMDH/ice_${year}_${month}_${day}_${hh}${SUFOUT}.nc
  rc=$? ; if [[ $rc -ne 0 ]] ; then exit 1 ; fi
  p_date=$($NDATE $hh_inc_ocn $p_date)
  m_date=$($NDATE $hh_inc_ocn $m_date)
done
set -x

# save printout for analysis

if [ -s printout ] ; then
    echo "=> Saving printout file: prnt.$diag_suffix"
    echo "=>         in directory: $DATAOUTpath"
    ${NCP:-/bin/cp} printout $DATAOUTpath/prnt.$diag_suffix
fi

echo " "
echo "==> done"

echo VDATE = $VDATE after ending MOM4 `date`
#
if [[ $VERBOSE = YES ]] ; then
   echo $(date) EXITING $0 with return code $err >&2
fi

#------------------------------------------------------------------------------#
# End post-execution section of OM script (5)
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
# Begin post-execution section of AM script (6)
#------------------------------------------------------------------------------#

if [ -s $FSYNCEXEC ] ; then
 $FSYNCEXEC $SIGR1 ; $FSYNCEXEC $SIGR2 ; $FSYNCEXEC $SFCR
fi

rm -f NULL
rm -f fort.11 fort.12 fort.14
rm -f fort.15 fort.24 fort.27 fort.28 fort.29 fort.43 fort.48
rm -f orography
rm -f fort.51 fort.52 fort.53
rm -f SIG.F*
rm -f SFC.F*
rm -f FLX.F*
rm -f LOG.F*
rm -f D3D.F*
rm -f ocn*nc.????
rm -f ice*nc.????

################################################################################
#  Postprocessing
################################################################################

cd $pwd
[[ $mkdata = YES ]]&&rmdir $DATA
$ENDSCRIPT

if [[ $VERBOSE = YES ]] ; then
   echo $(date) EXITING $0 with return code $err >&2
fi
err=${err:-0}
if [ $err -eq 0 ] ; then
  date>$RESTART_CONTROL_FILE
fi
exit $err

#------------------------------------------------------------------------------#
# End post-execution section of AM script (6)
#------------------------------------------------------------------------------#
