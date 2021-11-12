#!/bin/ksh
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         excfs_cdas_analysis.sh.ecf
# Script description:  Makes a global spectral model analysis
#
# Author:        Mark Iredell       Org: NP23         Date: 1999-07-15
#
# Abstract: This script makes a global spectral model analysis.
#   Both the surface analysis and the upper-air analysis are performed.
#
# Script history log:
# 1999-05-01  Mark Iredell
# 2002-04-26  Russ Treadon   add global_angupdate processing
# 2003-08-05  Russ Treadon   add processing of hourly sigma and surface guess
# 2004-03-26  Russ Treadon   remove reference to IEEE TOVS-1b files, fix sfcg bug
# 2005-01-03  Cheng-Hsuan Lu :set FSMCL(2:4) = FSMCL2
#                             add FNVMNC,FNVMXC,FNSLPC,FNABSC
# 2005-07-22  Russ Treadon   add processing of NOAA-18 hirs4, amsua, and mhs data
# 2006-11-29  Russ Treadon   update for global_gsi
# 2007-10-18  Russ Treadon   add new options (not activated) to GSI namelist; 
#                            update CRTM related pieces for use with CRTM_GFS
#                            (revision 799); modify SBUVBF variable and 
#                            sbuvbufr OBS_INPUT "dsis" variable to process 
#                            version 8 (sbuv8_*) sbuv/2 ozone data.
# 2007-10-18  Diane Stokes   clean up processing of diagnostic files
# 2008-04-04  Russ Treadon   remove global_chgres
# 2008-09-29  Russ Treadon   add IASI, update to crtm_gfsgsi (rev1855),
#                            turn on varqc and bkgv_flowdep, tsfc_sdv=3
# 2009-02-05  Russ Treadon   add unique thread / stack variables for GSIEXEC
#                            and ANGUPDATEXEC; add OMIBF, TCVITL; add dsfcalc
# 2009-03-19  Russ Treadon   add JCAP_A,NLAT_A,NLON_A,JCAP_B
# 2010-04-28  Russ Treadon   remove global_angupdate
# 2010-05-05  George Gayno   add ANAVINFO file
# 2011-05-10  EMC/SPA        update RTMFIX defaults to ${FIXGLOBAL}/crtm_2.0.2, instead of crtm_gfsgsi
# Usage:  global_analysis.sh SFCGES SIGGES GBIAS GSATANG
#                            SFCANL SIGANL ABIAS IGEN
#
#   Input script positional parameters:
#     1             Input surface guess
#                   defaults to $SFCGES; required
#     2             Input sigma guess
#                   defaults to $SIGGES; required
#     3             Input guess time dependent bias correction coefficients
#                   defaults to $GBIAS; required
#     4             Input guess angle dependent bias correction
#                   defaults to $GSATANG; required
#     5             Output surface analysis
#                   defaults to $SFCANL, then to ${COMOUT}/sfcanl
#     6             Output sigma analysis
#                   defaults to $SIGANL, then to ${COMOUT}/siganl
#     7             Output bias correction
#                   defaults to $ABIAS, then to ${COMOUT}/abias
#     8             Output generating code
#                   defaults to $IGEN, then to 0
#
#   Imported Shell Variables:
#     SFCGES        Input surface guess
#                   overridden by $1; required
#     SIGGES        Input sigma guess
#                   overridden by $2; required
#     GBIAS         Input guess bias correction
#                   overridden by $3; required
#     GSATANG       Input guess angle dependent bias correction
#                   overridden by $4; required
#     SFCANL        Output surface analysis
#                   overridden by $5; defaults to ${COMOUT}/sfcanl
#     SIGANL        Output sigma analysis
#                   overridden by $6; defaults to ${COMOUT}/siganl
#     ABIAS         Output bias correction
#                   overridden by $7; defaults to ${COMOUT}/abias
#     IGEN          Output generating code
#                   overridden by $8; defaults to 0
#     SFCG03        Surface guess valid at -03 hour
#                   defaults to ${COMOUT}/sfcf03; optional input
#     SFCG04        Surface guess valid at -04 hour
#                   defaults to ${COMOUT}/sfcf04; optional input
#     SFCG05        Surface guess valid at -05 hour
#                   defaults to ${COMOUT}/sfcf05; optional input
#     SFCG07        Surface guess valid at -07 hour
#                   defaults to ${COMOUT}/sfcf07; optional input
#     SFCG08        Surface guess valid at -08 hour
#                   defaults to ${COMOUT}/sfcf08; optional input
#     SFCG09        Surface guess valid at -09 hour
#                   defaults to ${COMOUT}/sfcf09; optional input
#     SIGG03        Sigma guess valid at -03 hour
#                   defaults to ${COMOUT}/sigf03; optional input
#     SIGG04        Sigma guess valid at -04 hour
#                   defaults to ${COMOUT}/sigf04; optional input
#     SIGG05        Sigma guess valid at -05 hour
#                   defaults to ${COMOUT}/sigf05; optional input
#     SIGG07        Sigma guess valid at -07 hour
#                   defaults to ${COMOUT}/sigf07; optional input
#     SIGG08        Sigma guess valid at -08 hour
#                   defaults to ${COMOUT}/sigf08; optional input
#     SIGG09        Sigma guess valid at -09 hour
#                   defaults to ${COMOUT}/sigf09; optional input
#     GINCIN        Input increment to guess
#                   defaults to ${COMOUT}/gesfile_in; optional
#     BIASIN        Input bias correction to guess
#                   defaults to ${COMOUT}/biascor_in; optional
#     RADSTAT       Output radiance assimilation statistics
#                   defaults to ${COMIN}/${PREINP}radstat
#     GSISTAT       Output gsi (obs-ges), qc, and iteration statistics
#                   defaults to ${COMIN}/${PREINP}gsistat
#     PCPSTAT       Output precipitation assimilation statistics
#                   defaults to ${COMIN}/${PREINP}pcpstat
#     CNVSTAT       Output conventional observation assimilation statistics
#                   defaults to ${COMIN}/${PREINP}cnvstat
#     OZNSTAT       Output ozone observation assimilation statistics
#                   defaults to ${COMIN}/${PREINP}oznstat
#     GINCOUT       Output increment to guess
#                   defaults to ${COMIN}/${PREINP}gesfile_out
#     BIASOUT       Output bias correction to guess
#                   defaults to ${COMIN}/${PREINP}biascor_out
#     FIXGLOBAL     Directory for global fixed files
#                   defaults to /nwprod/fix
#     FIXcfs        Directory for cfs_cdas fixed files
#                   defaults to /nwprod/fix/cfs_fix_cdas
#     EXECGLOBAL    Directory for global executables
#                   defaults to /nwprod/exec
#     USHGLOBAL     Directory for global scripts
#                   defaults to /nwprod/ush
#     DATA          working directory
#                   (if nonexistent will be made, used and deleted)
#                   defaults to current working directory
#     COMIN         input directory
#                   defaults to current working directory
#     COMOUT        output directory
#                   (if nonexistent will be made)
#                   defaults to current working directory
#     XC            Suffix to add to executables
#                   defaults to none
#     PREINP        Prefix to add to input observation files
#                   defaults to none
#     SUFINP        Suffix to add to input observation files
#                   defaults to none
#     NCP           Copy command
#                   defaults to cp
#     SIGHDR        Command to read sigma header
#                   defaults to ${EXECGLOBAL}/global_sighdr$XC
#     SFCHDR        Command to read surface header
#                   defaults to ${EXECGLOBAL}/global_sfchdr$XC
#     CYCLEXEC      Surface cycle executable
#                   defaults to ${EXECGLOBAL}/global_cycle$XC
#     GSIEXEC       Spectral analysis executable
#                   defaults to ${EXECGLOBAL}/global_gsi$XC
#     CYCLESH       Surface cycle script
#                   defaults to ${USHGLOBAL}/global_cycle.sh
#     BERROR        Input background error file
#                   defaults to ${FIXcfs}/global_berror.l${LEVS}y${NLAT_A}.sig.f77
#     SATANGL       Input satellite angle bias file
#                   defaults to ${FIXcfs}/global_satangbias.txt
#     SATINFO       Input satellite information file
#                   defaults to ${FIXcfs}/global_satinfo.txt
#     RTMFIX        Input directory containing sensor specific coefficients
#                   defaults to ${FIXGLOBAL}/crtm_2.0.2
#     RTMEMIS       Input satellite emissivity coefficients
#                   defaults to ${RTMFIX}/EmisCoeff/Big_Endian/EmisCoeff.bin
#     RTMAERO       Input radiative transfer model aerosol coefficients
#                   defaults to ${RTMFIX}/AerosolCoeff/Big_Endian/AerosolCoeff.bin
#     RTMCLDS       Input radiative transfer model cloud coefficients
#                   defaults to ${RTMFIX}/CloudCoeff/Big_Endian/CloudCoeff.bin
#     ANAVINFO      Input analysis variable file
#                   defaults to ${FIXcfs}/global_anavinfo.l${LEVS}.txt
#     CONVINFO      Input conventional observation information file
#                   defaults to ${FIXcfs}/global_convinfo.txt
#     OZINFO        Input ozone information file
#                   defaults to ${FIXcfs}/global_ozone.txt
#     PCPINFO       Input precipitation information file
#                   defaults to ${FIXcfs}/global_pcpinfo.txt
#     PREPQC        Input QC-ed observation BUFR file
#                   defaults to ${COMIN}/${PREINP}prepbufr${SUFINP}
#     GSNDBF        Input GOES sounder radiance file (bufr format)
#                   defaults to ${COMIN}/${PREINP}goesnd.tm00.bufr_d${SUFINP}
#     GSNDBF1       Input GOES 1x1 sounder radiance file (bufr format)
#                   defaults to ${COMIN}/${PREINP}goesfv.tm00.bufr_d${SUFINP}
#     B1HRS2        Input HIRS/2 radiance file (bufr format)
#                   defaults to ${COMIN}/${PREINP}1bhrs2.tm00.bufr_d${SUFINP}
#     B1MSU         Input MSU radiance file (bufr format)
#                   defaults to ${COMIN}/${PREINP}1bmsu.tm00.bufr_d${SUFINP}
#     B1HRS3        Input HIRS/3 radiance file (bufr format)
#                   defaults to ${COMIN}/${PREINP}1bhrs3.tm00.bufr_d${SUFINP}
#     B1HRS4        Input HIRS/4 radiance file (bufr format)
#                   defaults to ${COMIN}/${PREINP}1bhrs4.tm00.bufr_d${SUFINP}
#     B1AMUA        Input AMSU/A radiance file (bufr format)
#                   defaults to ${COMIN}/${PREINP}1bamua.tm00.bufr_d${SUFINP}
#     B1AMUB        Input AMSU/B radiance file (bufr format)
#                   defaults to ${COMIN}/${PREINP}1bamub.tm00.bufr_d${SUFINP}
#     B1MHS         Input MHS radiance file (bufr format)
#                   defaults to ${COMIN}/${PREINP}1bmhs.tm00.bufr_d${SUFINP}
#     ESHRS3        Input EARS HIRS/3 radiance file (bufr format)
#                   defaults to ${COMIN}/${PREINP}eshrs3.tm00.bufr_d${SUFINP}
#     ESAMUA        Input EARS AMSU/A radiance file (bufr format)
#                   defaults to ${COMIN}/${PREINP}esamua.tm00.bufr_d${SUFINP}
#     ESAMUB        Input EARS AMSU/B radiance file (bufr format)
#                   defaults to ${COMIN}/${PREINP}esamub.tm00.bufr_d${SUFINP}
#     AIRSBF        Input AIRS radiace file (bufr format)
#                   defaults to ${COMIN}/${PREINP}airs.tm00.bufr_d${SUFINP}
#     IASIBF        Input IASI radiace file (bufr format)
#                   defaults to ${COMIN}/${PREINP}mtiasi.tm00.bufr_d${SUFINP}
#     AMSREBF       Input AMSRE radiace file (bufr format)
#                   defaults to ${COMIN}/${PREINP}amsre.tm00.bufr_d${SUFINP}
#     SSMITBF       Input SSMI radiace file (bufr format)
#                   defaults to ${COMIN}/${PREINP}ssmit.tm00.bufr_d${SUFINP}
#     SBUVBF        Input NOAA POES SBUV ozone retrieval file
#                   defaults to ${COMIN}/${PREINP}osbuv8.tm00.bufr_d${SUFINP}
#     GOMEBF        Input GOME ozone retrieval file
#                   defaults to ${COMIN}/${PREINP}gome.tm00.bufr_d${SUFINP}
#     OMIBF         Input OMI ozone retrieval file
#                   defaults to ${COMIN}/${PREINP}omi.tm00.bufr_d${SUFINP}
#     SMIPCP        Input SSM/I precipitation rate file
#                   defaults to ${COMIN}/${PREINP}spssmip.tm00.bufr_d${SUFINP}
#     TMIPCP        Input TMI precipitation rate file
#                   defaults to ${COMIN}/${PREINP}sptrmm.tm00.bufr_d${SUFINP}
#     GPSROBF       Input GPS radio occultation data
#                   defaults to ${COMIN}/${PREINP}gpsro.tm00.bufr_d${SUFINP}
#     TCVITL        Input tcvitals file
#                   defaults to ${COMIN}/${PREINP}syndata.tcvitals.tm00
#     INISCRIPT     Preprocessing script
#                   defaults to none
#     LOGSCRIPT     Log posting script
#                   defaults to none
#     ERRSCRIPT     Error processing script
#                   defaults to 'eval [[ $err = 0 ]]'
#     ENDSCRIPT     Postprocessing script
#                   defaults to none
#     JCAP_A        Spectral truncation for analysis
#                   defaults to the value in the input sigma file header
#     JCAP          Spectral truncation for background
#                   defaults to the value in the input sigma file header
#     LEVS          Number of levels
#     DELTIM        Timestep in seconds
#                   defaults to 3600/($JCAP_A/20)
#     CDATE         Current analysis date in yyyymmddhh format
#                   defaults to the value in the input surface file header
#     LATB          Number of latitudes in surface cycling
#                   defaults to the value in the input surface file header
#     LONB          Number of longitudes in surface cycling
#                   defaults to the value in the input surface file header
#     LSOIL         Number of soil layers
#                   defaults to 2
#     FSMCL2        Scale in days to relax to soil moisture climatology
#                   defaults to 60
#     DELTSFC       Cycling frequency in hours
#                   defaults to forecast hour of $SFCGES
#     LATA          Number of latitudes in spectral analysis
#                   defaults to $LATB
#     LONA          Number of longitudes in spectral analysis
#                   defaults to $LONB
#     NSIG1         Number of levels per MPI task
#                   (Important: number of MPI tasks must be 5*$LEVS/$NSIG1+2)
#                   defaults to 1
#     CYCLVARS      Other namelist inputs to the cycle executable
#                   defaults to none set
#     GSIVARS       Other namelist inputs to the analysis executable
#                   defaults to none set
#     SATVARS       Other namelist input to analysis data usage namelist
#                   defaults to none set
#     NTHREADS      Number of threads
#                   defaults to 1
#     NTHSTACK      Size of stack per thread
#                   defaults to 1024000000
#     NTHREADS_GSI  Number of threads for GSIEXEC
#                   defaults to 1
#     NTHSTACK_GSI  Size of stack per thread for GSIEXEC
#                   defaults to 1024000000
#     FILESTYLE     File management style flag
#                   ('C' to copy to/from $DATA, 'L' for symbolic links in $DATA,
#                    'X' to use XLFUNIT or symbolic links where appropriate)
#                   defaults to 'X'
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
#     programs   : $CYCLEXEC
#                  $GSIEXEC
#
#     fixed data : $BERROR
#                  $SATANGL
#                  $SATINFO
#                  $RTMFIX
#                  $RTMEMIS
#                  $RTMAERO
#                  $RTMCLDS
#                  $ANAVINFO
#                  $CONVINFO
#                  $OZINFO
#                  $PCPINFO
#
#     input data : $SFCGES
#                  $SIGGES
#                  $GBIAS
#                  $GSATANG
#                  $SFCG03
#                  $SFCG04
#                  $SFCG05
#                  $SFCG07
#                  $SFCG08
#                  $SFCG09
#                  $SIGG03
#                  $SIGG04
#                  $SIGG05
#                  $SIGG07
#                  $SIGG08
#                  $SIGG09
#                  $GINCIN
#                  $BIASIN
#                  $FNTSFA
#                  $FNACNA
#                  $FNSNOA
#                  $PREPQC
#                  $GSNDBF
#                  $GSNDBF1
#                  $B1HRS2
#                  $B1MSU
#                  $B1HRS3
#                  $B1HRS4
#                  $B1AMUA
#                  $B1AMUB
#                  $B1MHS
#                  $ESHRS3
#                  $ESAMUA
#                  $ESAMUB
#                  $AIRSBF
#                  $IASIBF
#                  $AMSREBF
#                  $SSMITBF
#                  $SBUVBF
#                  $GOMEBF
#                  $OMIBF
#                  $SMIPCP
#                  $TMIPCP
#                  $GPSROBF
#                  $TCVITL
#
#     output data: $SFCANL
#                  $SIGANL
#                  $ABIAS
#                  $RADSTAT
#                  $GSISTAT
#                  $PCPSTAT
#                  $CNVSTAT
#                  $OZNSTAT
#                  $GINCOUT
#                  $BIASOUT
#                  $PGMOUT
#                  $PGMERR
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
################################################################################
#  Set environment.
echo
echo $(date) EXECUTING $0 $* >&2
echo
export VERBOSE=${VERBOSE:-"NO"}
[[ "$VERBOSE" = "YES" ]] && set -x

#  Command line arguments.
export APRUN=${APRUN:-""}
export SFCGES=${1:-${SFCGES:?}}
export SIGGES=${2:-${SIGGES:?}}
export GBIAS=${3:-${GBIAS:?}}
export GSATANG=${4:-${GSATANG:?}}
export SFCANL=${5:-${SFCANL}}
export SIGANL=${6:-${SIGANL}}
export ABIAS=${7:-${ABIAS}}
export IGEN=${8:-${IGEN:-0}}
#  Directories.
export FIXGLOBAL=${FIXGLOBAL:-/nwprod/fix}
export FIXcfs=${FIXcfs:-/nwprod/fix/cfs_fix_cdas}
export EXECGLOBAL=${EXECGLOBAL:-/nwprod/exec}
export USHGLOBAL=${USHGLOBAL:-/nwprod/ush}
export DATA=${DATA:-$(pwd)}
export COMIN=${COMIN:-$(pwd)}
export COMOUT=${COMOUT:-$(pwd)}
#  Filenames.
export XC=${XC}
export PREINP=${PREINP}
export SUFINP=${SUFINP}
export SIGHDR=${SIGHDR:-${EXECGLOBAL}/global_sighdr$XC}
export SFCHDR=${SFCHDR:-${EXECGLOBAL}/global_sfchdr$XC}
export JCAP=${JCAP:-$($SIGHDR $SIGGES JCAP||echo 0)}
export JCAP_A=${JCAP_A:-$($SIGHDR $SIGGES JCAP||echo 0)}
export LATB=${LATB:-$($SFCHDR $SFCGES LATB||echo 0)}
export LONB=${LONB:-$($SFCHDR $SFCGES LONB||echo 0)}
export LATA=${LATA:-$LATB}
export LONA=${LONA:-$LONB}
export NLAT_A=${NLAT_A:-$(($LATA+2))}
export NLON_A=${NLON_A:-$LONA}
export LEVS=${LEVS:-$($SIGHDR $SIGGES LEVS||echo 0)}
export DELTIM=${DELTIM:-$((3600/($JCAP_A/20)))}
export CYCLEXEC=${CYCLEXEC:-${EXECGLOBAL}/global_cycle$XC}
export GSIEXEC=${GSIEXEC:-${EXECcfs}/cfs_cdas_gsi$XC}
export CYCLESH=${CYCLESH:-${USHGLOBAL}/global_cycle.sh}
export BERROR=${BERROR:-${FIXcfs}/global_berror.l${LEVS}y${NLAT_A}.f77}
export SATANGL=${SATANGL:-${FIXcfs}/global_satangbias.txt}
export SATINFO=${SATINFO:-${FIXcfs}/global_satinfo.txt}
export RTMFIX=${RTMFIX:-${FIXGLOBAL}/crtm_2.0.2}
export RTMEMIS=${RTMEMIS:-${RTMFIX}/EmisCoeff/Big_Endian/EmisCoeff.bin}
export RTMAERO=${RTMAERO:-${RTMFIX}/AerosolCoeff/Big_Endian/AerosolCoeff.bin}
export RTMCLDS=${RTMCLDS:-${RTMFIX}/CloudCoeff/Big_Endian/CloudCoeff.bin}
export ANAVINFO=${ANAVINFO:-${FIXcfs}/global_anavinfo.l${LEVS}.txt}
export CONVINFO=${CONVINFO:-${FIXcfs}/global_convinfo.txt}
export OZINFO=${OZINFO:-${FIXcfs}/global_ozinfo.txt}
export PCPINFO=${PCPINFO:-${FIXcfs}/global_pcpinfo.txt}
export OBERROR=${OBERROR:-${FIXcfs}/prepobs_errtable.global}
export PREPQC=${PREPQC:-${COMIN}/${PREINP}prepbufr${SUFINP}}
export GSNDBF=${GSNDBF:-${COMIN}/${PREINP}goesnd.tm00.bufr_d${SUFINP}}
export GSNDBF1=${GSNDBF1:-${COMIN}/${PREINP}goesfv.tm00.bufr_d${SUFINP}}
export B1HRS2=${B1HRS2:-${COMIN}/${PREINP}1bhrs2.tm00.bufr_d${SUFINP}}
export B1MSU=${B1MSU:-${COMIN}/${PREINP}1bmsu.tm00.bufr_d${SUFINP}}
export B1HRS3=${B1HRS3:-${COMIN}/${PREINP}1bhrs3.tm00.bufr_d${SUFINP}}
export B1HRS4=${B1HRS4:-${COMIN}/${PREINP}1bhrs4.tm00.bufr_d${SUFINP}}
export B1AMUA=${B1AMUA:-${COMIN}/${PREINP}1bamua.tm00.bufr_d${SUFINP}}
export B1AMUB=${B1AMUB:-${COMIN}/${PREINP}1bamub.tm00.bufr_d${SUFINP}}
export B1MHS=${B1MHS:-${COMIN}/${PREINP}1bmhs.tm00.bufr_d${SUFINP}}
export ESHRS3=${ESHRS3:-${COMIN}/${PREINP}eshrs3.tm00.bufr_d${SUFINP}}
export ESAMUA=${ESAMUA:-${COMIN}/${PREINP}esamua.tm00.bufr_d${SUFINP}}
export ESAMUB=${ESAMUB:-${COMIN}/${PREINP}esamub.tm00.bufr_d${SUFINP}}
export AIRSBF=${AIRSBF:-${COMIN}/${PREINP}airsev.tm00.bufr_d${SUFINP}}
export IASIBF=${IASIBF:-${COMIN}/${PREINP}mtiasi.tm00.bufr_d${SUFINP}}
export AMSREBF=${AMSREBF:-${COMIN}/${PREINP}amsre.tm00.bufr_d${SUFINP}}
export SSMITBF=${SSMITBF:-${COMIN}/${PREINP}ssmit.tm00.bufr_d${SUFINP}}
export SBUVBF=${SBUVBF:-${COMIN}/${PREINP}osbuv8.tm00.bufr_d${SUFINP}}
export GOMEBF=${GOMEBF:-${COMIN}/${PREINP}gome.tm00.bufr_d${SUFINP}}
export OMIBF=${OMIBF:-${COMIN}/${PREINP}omi.tm00.bufr_d${SUFINP}}
export SMIPCP=${SMIPCP:-${COMIN}/${PREINP}spssmi.tm00.bufr_d${SUFINP}}
export TMIPCP=${TMIPCP:-${COMIN}/${PREINP}sptrmm.tm00.bufr_d${SUFINP}}
export GPSROBF=${GPSROBF:-${COMIN}/${PREINP}gpsro.tm00.bufr_d${SUFINP}}
export TCVITL=${TCVITL:-${COMIN}/${PREINP}syndata.tcvitals.tm00}
export GINCIN=${GINCIN:-${COMOUT}/gesfile_in}
export BIASIN=${BIASIN:-${COMOUT}/biascor_in}
export SFCG03=${SFCG03:-${COMOUT}/sfcf03}
export SFCG04=${SFCG04:-${COMOUT}/sfcf04}
export SFCG05=${SFCG05:-${COMOUT}/sfcf05}
export SFCG07=${SFCG07:-${COMOUT}/sfcf07}
export SFCG08=${SFCG08:-${COMOUT}/sfcf08}
export SFCG09=${SFCG09:-${COMOUT}/sfcf09}
export SIGG03=${SIGG03:-${COMOUT}/sigf03}
export SIGG04=${SIGG04:-${COMOUT}/sigf04}
export SIGG05=${SIGG05:-${COMOUT}/sigf05}
export SIGG07=${SIGG07:-${COMOUT}/sigf07}
export SIGG08=${SIGG08:-${COMOUT}/sigf08}
export SIGG09=${SIGG09:-${COMOUT}/sigf09}
export SFCANL=${SFCANL:-${COMOUT}/${PREINP}sfcanl}
export SIGANL=${SIGANL:-${COMOUT}/${PREINP}sanl}
export ABIAS=${ABIAS:-${COMOUT}/${PREINP}abias}
export GINCOUT=${GINCOUT:-${COMOUT}/${PREINP}gesfile_out}
export BIASOUT=${BIASOUT:-${COMOUT}/${PREINP}biascor_out}
export RADSTAT=${RADSTAT:-${COMOUT}/${PREINP}radstat}
export GSISTAT=${GSISTAT:-${COMOUT}/${PREINP}gsistat}
export PCPSTAT=${PCPSTAT:-${COMOUT}/${PREINP}pcpstat}
export CNVSTAT=${CNVSTAT:-${COMOUT}/${PREINP}cnvstat}
export OZNSTAT=${OZNSTAT:-${COMOUT}/${PREINP}oznstat}
export INISCRIPT=${INISCRIPT}
export ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}
export LOGSCRIPT=${LOGSCRIPT}
export ENDSCRIPT=${ENDSCRIPT}
#  Other variables.
export CDATE=${CDATE:-$($SFCHDR $SFCGES VDATE||echo 0)}
export LSOIL=${LSOIL:-2}
export FSMCL2=${FSMCL2:-60}
export DELTSFC=${DELTSFC:-$($SFCHDR $SFCGES FHOUR||echo 0)}
export CYCLVARS=${CYCLVARS}
export SETUP=${SETUP:-""}
export GRIDOPTS=${GRIDOPTS:-""}
export BKGVERR=${BKGVERR:-""}
export ANBKGERR=${ANBKGERR:-""}
export JCOPTS=${JCOPTS:-""}
export STRONGOPTS=${STRONGOPTS:-""}
export OBSQC=${OBSQC:-""}
export OBSINPUT=${OBSINPUT:-""}
export SUPERRAD=${SUPERRAD:-""}
export SINGLEOB=${SINGLEOB:-""}
export LAGDATA=${LAGDATA:-""}
export FILESTYLE=${FILESTYLE:-'X'}
export PGMOUT=${PGMOUT:-${pgmout:-'&1'}}
export PGMERR=${PGMERR:-${pgmerr:-'&2'}}
export OBERRFLAG=${OBERRFLAG:-.false.}
export NCP=${NCP:-cp}
export NTHREADS=${NTHREADS:-1}
export NTHREADS_GSI=${NTHREADS_GSI:-1}

typeset -L1 l=$PGMOUT
[[ $l = '&' ]]&&a=''||a='>'
export REDOUT=${REDOUT:-'1>'$a}
typeset -L1 l=$PGMERR
[[ $l = '&' ]]&&a=''||a='>'
export REDERR=${REDERR:-'2>'$a}
COMPRESS=gzip
UNCOMPRESS=gunzip

export wc=${wc:-/usr/bin/wc}

################################################################################
#  Preprocessing
$INISCRIPT
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
#  Update surface guess file to produce surface analysis

$CYCLESH  $SFCGES $SFCANL

rc=$?
export ERR=$rc
export err=$ERR
$ERRSCRIPT||exit 11


################################################################################
#  Make atmospheric analysis

export OMP_NUM_THREADS=$NTHREADS_GSI
export APRUN="mpiexec -n $NCPUS --depth $OMP_NUM_THREADS"
export PGM='$APRUN $DATA/$(basename $GSIEXEC)'
export pgm=$PGM
$LOGSCRIPT

rm -f berror_stats satbias_angle satinfo *SpcCoeff.bin *TauCoeff.bin EmisCoeff.bin
rm -f AerosolCoeff.bin CloudCoeff.bin anavinfo convinfo ozinfo pcpinfo errtable
rm -f prepbufr gsndrbufr gsnd1bufr hirs2bufr msubufr hirs3bufr hirs4bufr
rm -f amsuabufr amsubbufr mhsbufr sbuvbufr gomebufr omibufr ssmirrbufr tmirrbufr
rm -f airsbufr iasibufr amsrebufr ssmitbufr gpsrobufr tcvitl
rm -f hirs3bufrears amsuabufrears amsubbufrears
rm -f satbias_in satbias_ang.in satbias_out gsiparm.anl
rm -f sfcf03 sfcf04 sfcf05 sfcf06 sfcf07 sfcf08 sfcf09
rm -f sigf03 sigf04 sigf05 sigf06 sigf07 sigf08 sigf09 siganl
rm -f gesfile_in gesfiles_out

$NCP $GSIEXEC $DATA
if [[ $FILESTYLE = 'C' ]]
then
   # Fixed fields
   $NCP $BERROR   berror_stats
   $NCP $SATANGL  satbias_angle
   $NCP $SATINFO  satinfo
   $NCP $RTMEMIS  EmisCoeff.bin
   $NCP $RTMAERO  AerosolCoeff.bin
   $NCP $RTMCLDS  CloudCoeff.bin
   $NCP $ANAVINFO anavinfo
   $NCP $CONVINFO convinfo
   $NCP $OZINFO   ozinfo
   $NCP $PCPINFO  pcpinfo
   ${NPC:-cp} $OBERROR  errtable

   set +x
   # CRTM Spectral and Transmittance coefficients
   nsatsen=`cat satinfo | $wc -l`
   isatsen=1
   while [[ $isatsen -le $nsatsen ]]; do
      flag=`head -n $isatsen satinfo | tail -1 | cut -c1-1`
      if [[ "$flag" != "!" ]]; then
         satsen=`head -n $isatsen satinfo | tail -1 | cut -f 2 -d" "`
         spccoeff=${satsen}.SpcCoeff.bin
         if  [[ ! -s $spccoeff ]]; then
            ${NPC:-cp} $RTMFIX/SpcCoeff/Big_Endian/$spccoeff $spccoeff
            ${NPC:-cp} $RTMFIX/TauCoeff/Big_Endian/${satsen}.TauCoeff.bin ${satsen}.TauCoeff.bin
         fi
      fi
      isatsen=` expr $isatsen + 1 `
   done
   [[ "$VERBOSE" = "YES" ]] && set -x


   # Observational data
   set +e
   $NCP $PREPQC   prepbufr
   $NCP $GSNDBF   gsndrbufr
   $NCP $GSNDBF1  gsnd1bufr
   $NCP $B1HRS2   hirs2bufr
   $NCP $B1MSU    msubufr
   $NCP $B1HRS3   hirs3bufr
   $NCP $B1HRS4   hirs4bufr
   $NCP $B1AMUA   amsuabufr
   $NCP $B1AMUB   amsubbufr
   $NCP $B1MHS    mhsbufr
   $NCP $ESHRS3   hirs3bufrears
   $NCP $ESAMUA   amsuabufrears
   $NCP $ESAMUB   amsubbufrears
   $NCP $SBUVBF   sbuvbufr
   $NCP $GOMEBF   gomebufr
   $NCP $OMIBF    omibufr
   $NCP $SMIPCP   ssmirrbufr
   $NCP $TMIPCP   tmirrbufr
   $NCP $AIRSBF   airsbufr
   $NCP $IASIBF   iasibufr
   $NCP $AMSREBF  amsrebufr
   $NCP $SSMITBF  ssmitbufr
   $NCP $GPSROBF  gpsrobufr
   $NCP $TCVITL   tcvitl

   # Required guess fields
   $NCP $GBIAS    satbias_in
   $NCP $GSATANG  satbias_ang.in
   $NCP $SIGG03   sigf03
   $NCP $SIGGES   sigf06
   $NCP $SIGG09   sigf09
   $NCP $SFCG03   sfcf03
   $NCP $SFCGES   sfcf06
   $NCP $SFCG09   sfcf09

## NOTE:  10/01/2003, r.treadon
##    (1) While the global_gsi can handle hourly forecast
##        files, we do not currently utilize hourly input.
##        To prevent misleading error messages in operations, 
##        test for file existence before cp
##
##    (2) File gesfile_in is not currently used in the global_gsi
##        To prevent misleading error messages in operations, 
##        test for file existence before cp
##
##    (3) File biascor_in is not currently used in the global_gsi
##        To prevent misleading error messages in operations,
##        test for file existence before cp


   if [[ -s $SIGG04 ]]; then 
      $NCP $SIGG04   sigf04
   fi
   if [[ -s $SIGG05 ]]; then
      $NCP $SIGG05   sigf05
   fi
   if [[ -s $SIGG07 ]]; then
      $NCP $SIGG07   sigf07
   fi
   if [[ -s $SIGG08 ]]; then
      $NCP $SIGG08   sigf08
   fi

   if [[ -s $SFCG04 ]]; then
      $NCP $SFCG04   sfcf04
   fi
   if [[ -s $SFCG05 ]]; then
      $NCP $SFCG05   sfcf05
   fi
   if [[ -s $SFCG07 ]]; then
      $NCP $SFCG07   sfcf07
   fi
   if [[ -s $SFCG08 ]]; then
      $NCP $SFCG08   sfcf08
   fi
   if [[ -s $GINCIN ]]; then
      $NCP $GINCIN   gesfile_in
   fi
   if [[ -s $BIASIN ]]; then
      $NCP $BIASIN   biascor_in
   fi

else

## NOTE:  10/01/2003, r.treadon
##    Do not add above file existence tests since
##    we do not link to files in operations


   # Fixed fields
   ln -fs $BERROR   berror_stats
   ln -fs $SATANGL  satbias_angle
   ln -fs $SATINFO  satinfo
   ln -fs $RTMEMIS  EmisCoeff.bin
   ln -fs $RTMAERO  AerosolCoeff.bin
   ln -fs $RTMCLDS  CloudCoeff.bin
   ln -fs $ANAVINFO anavinfo
   ln -fs $CONVINFO convinfo
   ln -fs $OZINFO   ozinfo
   ln -fs $PCPINFO  pcpinfo

   # CRTM Spectral and Transmittance coefficients
   nsatsen=`cat satinfo | $wc -l`
   isatsen=1
   while [[ $isatsen -le $nsatsen ]]; do
      flag=`head -n $isatsen satinfo | tail -1 | cut -c1-1`
      if [[ "$flag" != "!" ]]; then
         satsen=`head -n $isatsen satinfo | tail -1 | cut -f 2 -d" "`
         spccoeff=${satsen}.SpcCoeff.bin
         if  [[ ! -s $spccoeff ]]; then
            ln -fs $RTMFIX/SpcCoeff/Big_Endian/$spccoeff $spccoeff
            ln -fs $RTMFIX/TauCoeff/Big_Endian/${satsen}.TauCoeff.bin ${satsen}.TauCoeff.bin
         fi
      fi
      isatsen=` expr $isatsen + 1 `
   done


   # Observational data
   ln -fs $PREPQC   prepbufr
   ln -fs $GSNDBF   gsndrbufr
   ln -fs $GSNDBF1  gsnd1bufr
   ln -fs $B1HRS2   hirs2bufr
   ln -fs $B1MSU    msubufr
   ln -fs $B1HRS3   hirs3bufr
   ln -fs $B1HRS4   hirs4bufr
   ln -fs $B1AMUA   amsuabufr
   ln -fs $B1AMUB   amsubbufr
   ln -fs $B1MHS    mhsbufr
   ln -fs $ESHRS3   hirs3bufrears
   ln -fs $ESAMUA   amsuabufrears
   ln -fs $ESAMUB   amsubbufrears
   ln -fs $AIRSBF   airsbufr
   ln -fs $IASIBF   iasibufr
   ln -fs $AMSREBF  amsrebufr
   ln -fs $SSMITBF  ssmitbufr
   ln -fs $SBUVBF   sbuvbufr
   ln -fs $GOMEBF   gomebufr
   ln -fs $OMIBF    omibufr
   ln -fs $SMIPCP   ssmirrbufr
   ln -fs $TMIPCP   tmirrbufr
   ln -fs $GPSROBF  gpsrobufr
   ln -fs $TCVITL   tcvitl

   # Guess fields
   ln -fs $GBIAS    satbias_in
   ln -fs $GSATANG  satbias_ang.in
   ln -fs $GINCIN   gesfile_in
   ln -fs $BIASIN   biascor_in
   ln -fs $SIGG03   sigf03
   ln -fs $SIGG04   sigf04
   ln -fs $SIGG05   sigf05
   ln -fs $SIGGES   sigf06
   ln -fs $SIGG07   sigf07
   ln -fs $SIGG08   sigf08
   ln -fs $SIGG09   sigf09
   ln -fs $SFCG03   sfcf03
   ln -fs $SFCG04   sfcf04
   ln -fs $SFCG05   sfcf05
   ln -fs $SFCGES   sfcf06
   ln -fs $SFCG07   sfcf07
   ln -fs $SFCG08   sfcf08
   ln -fs $SFCG09   sfcf09
   # Output files
   ln -fs $SIGANL   siganl
   ln -fs $ABIAS    satbias_out
   ln -fs $GINCOUT  gesfile_out
   ln -fs $BIASOUT  biascor_out
fi


# Create global_gsi namelist
cat <<EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=100,niter(2)=150,
   niter_no_qc(1)=50,niter_no_qc(2)=0,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   qoption=2,
   gencode=$IGEN,factqmin=0.005,factqmax=0.005,deltim=$DELTIM,
   ndat=56,npred=5,iguess=-1,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   use_pbl=.false.,
   $SETUP
 /
 &GRIDOPTS
   JCAP_B=$JCAP,JCAP=$JCAP_A,NLAT=$NLAT_A,NLON=$NLON_A,nsig=$LEVS,hybrid=.true.,
   regional=.false.,nlayers(63)=3,nlayers(64)=6,
   $GRIDOPTS
 /
 &BKGERR
   as=0.55,0.55,0.75,0.85,0.75,0.75,1.0,1.0,
   vs=0.7,
   hzscl=1.7,0.8,0.5,
   hswgt=0.45,0.3,0.25,
   bw=0.0,norsp=4,
   bkgv_flowdep=.true.,bkgv_rewgtfct=1.5,
   tsfc_sdv(1)=3.0,tsfc_sdv(2)=3.0,
   $BKGVERR
 /
 &ANBKGERR
   anisotropic=.false.,
   $ANBKGERR
 /
 &JCOPTS
   ljcdfi=.false.,alphajc=0.0,ljcpdry=.true.,bamp_jcpdry=2.5e7,
   $JCOPTS
 /
 &STRONGOPTS
   jcstrong=.true.,nstrong=1,nvmodes_keep=8,period_max=6.,period_width=1.5,
   jcstrong_option=2,baldiag_full=.true.,baldiag_inc=.true.,
   $STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.true.,oberrflg=.false.,c_varqc=0.02,
   use_poq7=.true.,
   $OBSQC
 /
 &OBS_INPUT
   dmesh(1)=145.0,dmesh(2)=150.0,time_window_max=3.0,
   dfile(01)='prepbufr',  dtype(01)='ps',        dplat(01)=' ',       dsis(01)='ps',              dval(01)=0.0,  dthin(01)=0,  dsfcalc(01)=0,
   dfile(02)='prepbufr'   dtype(02)='t',         dplat(02)=' ',       dsis(02)='t',               dval(02)=0.0,  dthin(02)=0,  dsfcalc(02)=0,
   dfile(03)='prepbufr',  dtype(03)='q',         dplat(03)=' ',       dsis(03)='q',               dval(03)=0.0,  dthin(03)=0,  dsfcalc(03)=0,
   dfile(04)='prepbufr',  dtype(04)='pw',        dplat(04)=' ',       dsis(04)='pw',              dval(04)=0.0,  dthin(04)=0,  dsfcalc(04)=0,
   dfile(05)='prepbufr',  dtype(05)='uv',        dplat(05)=' ',       dsis(05)='uv',              dval(05)=0.0,  dthin(05)=0,  dsfcalc(05)=0,
   dfile(06)='prepbufr',  dtype(06)='spd',       dplat(06)=' ',       dsis(06)='spd',             dval(06)=0.0,  dthin(06)=0,  dsfcalc(06)=0,
   dfile(07)='prepbufr',  dtype(07)='dw',        dplat(07)=' ',       dsis(07)='dw',              dval(07)=0.0,  dthin(07)=0,  dsfcalc(07)=0,
   dfile(08)='radarbufr', dtype(08)='rw',        dplat(08)=' ',       dsis(08)='rw',              dval(08)=0.0,  dthin(08)=0,  dsfcalc(08)=0,
   dfile(09)='prepbufr',  dtype(09)='sst',       dplat(09)=' ',       dsis(09)='sst',             dval(09)=0.0,  dthin(09)=0,  dsfcalc(09)=0,
   dfile(10)='gpsrobufr', dtype(10)='gps_ref',   dplat(10)=' ',       dsis(10)='gps_ref',         dval(10)=0.0,  dthin(10)=0,  dsfcalc(10)=0,
   dfile(11)='ssmirrbufr',dtype(11)='pcp_ssmi',  dplat(11)='dmsp',    dsis(11)='pcp_ssmi',        dval(11)=0.0,  dthin(11)=-1, dsfcalc(11)=0,
   dfile(12)='tmirrbufr', dtype(12)='pcp_tmi',   dplat(12)='trmm',    dsis(12)='pcp_tmi',         dval(12)=0.0,  dthin(12)=-1, dsfcalc(12)=0,
   dfile(13)='sbuvbufr',  dtype(13)='sbuv2',     dplat(13)='n16',     dsis(13)='sbuv8_n16',       dval(13)=0.0,  dthin(13)=0,  dsfcalc(13)=0,
   dfile(14)='sbuvbufr',  dtype(14)='sbuv2',     dplat(14)='n17',     dsis(14)='sbuv8_n17',       dval(14)=0.0,  dthin(14)=0,  dsfcalc(14)=0,
   dfile(15)='sbuvbufr',  dtype(15)='sbuv2',     dplat(15)='n18',     dsis(15)='sbuv8_n18',       dval(15)=0.0,  dthin(15)=0,  dsfcalc(15)=0,
   dfile(16)='hirs3bufr', dtype(16)='hirs3',     dplat(16)='n17',     dsis(16)='hirs3_n17',       dval(16)=0.0,  dthin(16)=1,  dsfcalc(16)=1,
   dfile(17)='hirs4bufr', dtype(17)='hirs4',     dplat(17)='metop-a', dsis(17)='hirs4_metop-a',   dval(17)=0.0,  dthin(17)=1,  dsfcalc(17)=1,
   dfile(18)='gimgrbufr', dtype(18)='goes_img',  dplat(18)='g11',     dsis(18)='imgr_g11',        dval(18)=0.0,  dthin(18)=1,  dsfcalc(18)=0,
   dfile(19)='gimgrbufr', dtype(19)='goes_img',  dplat(19)='g12',     dsis(19)='imgr_g12',        dval(19)=0.0,  dthin(19)=1,  dsfcalc(19)=0,
   dfile(20)='airsbufr',  dtype(20)='airs',      dplat(20)='aqua',    dsis(20)='airs281SUBSET_aqua',dval(20)=0.0,dthin(20)=1, dsfcalc(20)=1,
   dfile(21)='amsuabufr', dtype(21)='amsua',     dplat(21)='n15',     dsis(21)='amsua_n15',       dval(21)=0.0,  dthin(21)=1,  dsfcalc(21)=1,
   dfile(22)='amsuabufr', dtype(22)='amsua',     dplat(22)='n18',     dsis(22)='amsua_n18',       dval(22)=0.0,  dthin(22)=1,  dsfcalc(22)=1,
   dfile(23)='amsuabufr', dtype(23)='amsua',     dplat(23)='metop-a', dsis(23)='amsua_metop-a',   dval(23)=0.0,  dthin(23)=1,  dsfcalc(23)=1,
   dfile(24)='airsbufr',  dtype(24)='amsua',     dplat(24)='aqua',    dsis(24)='amsua_aqua',      dval(24)=0.0,  dthin(24)=1,  dsfcalc(24)=1,
   dfile(25)='amsubbufr', dtype(25)='amsub',     dplat(25)='n17',     dsis(25)='amsub_n17',       dval(25)=0.0,  dthin(25)=1,  dsfcalc(25)=1,
   dfile(26)='mhsbufr',   dtype(26)='mhs',       dplat(26)='n18',     dsis(26)='mhs_n18',         dval(26)=0.0,  dthin(26)=1,  dsfcalc(26)=1,
   dfile(27)='mhsbufr',   dtype(27)='mhs',       dplat(27)='metop-a', dsis(27)='mhs_metop-a',     dval(27)=0.0,  dthin(27)=1,  dsfcalc(27)=1,
   dfile(28)='ssmitbufr', dtype(28)='ssmi',      dplat(28)='f14',     dsis(28)='ssmi_f14',        dval(28)=0.0,  dthin(28)=1,  dsfcalc(28)=0,
   dfile(29)='ssmitbufr', dtype(29)='ssmi',      dplat(29)='f15',     dsis(29)='ssmi_f15',        dval(29)=0.0,  dthin(29)=1,  dsfcalc(29)=0,
   dfile(30)='amsrebufr', dtype(30)='amsre_low', dplat(30)='aqua',    dsis(30)='amsre_aqua',      dval(30)=0.0,  dthin(30)=1,  dsfcalc(30)=0,
   dfile(31)='amsrebufr', dtype(31)='amsre_mid', dplat(31)='aqua',    dsis(31)='amsre_aqua',      dval(31)=0.0,  dthin(31)=1,  dsfcalc(31)=0,
   dfile(32)='amsrebufr', dtype(32)='amsre_hig', dplat(32)='aqua',    dsis(32)='amsre_aqua',      dval(32)=0.0,  dthin(32)=1,  dsfcalc(32)=0,
   dfile(33)='ssmisbufr', dtype(33)='ssmis_las', dplat(33)='f16',     dsis(33)='ssmis_f16',       dval(33)=0.0,  dthin(33)=1,  dsfcalc(33)=0,
   dfile(34)='ssmisbufr', dtype(34)='ssmis_uas', dplat(34)='f16',     dsis(34)='ssmis_f16',       dval(34)=0.0,  dthin(34)=1,  dsfcalc(34)=0,
   dfile(35)='ssmisbufr', dtype(35)='ssmis_img', dplat(35)='f16',     dsis(35)='ssmis_f16',       dval(35)=0.0,  dthin(35)=1,  dsfcalc(35)=0,
   dfile(36)='ssmisbufr', dtype(36)='ssmis_env', dplat(36)='f16',     dsis(36)='ssmis_f16',       dval(36)=0.0,  dthin(36)=1,  dsfcalc(36)=0,
   dfile(37)='gsnd1bufr', dtype(37)='sndrd1',    dplat(37)='g12',     dsis(37)='sndrD1_g12',      dval(37)=0.0,  dthin(37)=1,  dsfcalc(37)=0,
   dfile(38)='gsnd1bufr', dtype(38)='sndrd2',    dplat(38)='g12',     dsis(38)='sndrD2_g12',      dval(38)=0.0,  dthin(38)=1,  dsfcalc(38)=0,
   dfile(39)='gsnd1bufr', dtype(39)='sndrd3',    dplat(39)='g12',     dsis(39)='sndrD3_g12',      dval(39)=0.0,  dthin(39)=1,  dsfcalc(39)=0,
   dfile(40)='gsnd1bufr', dtype(40)='sndrd4',    dplat(40)='g12',     dsis(40)='sndrD4_g12',      dval(40)=0.0,  dthin(40)=1,  dsfcalc(40)=0,
   dfile(41)='gsnd1bufr', dtype(41)='sndrd1',    dplat(41)='g11',     dsis(41)='sndrD1_g11',      dval(41)=0.0,  dthin(41)=1,  dsfcalc(41)=0,
   dfile(42)='gsnd1bufr', dtype(42)='sndrd2',    dplat(42)='g11',     dsis(42)='sndrD2_g11',      dval(42)=0.0,  dthin(42)=1,  dsfcalc(42)=0,
   dfile(43)='gsnd1bufr', dtype(43)='sndrd3',    dplat(43)='g11',     dsis(43)='sndrD3_g11',      dval(43)=0.0,  dthin(43)=1,  dsfcalc(43)=0,
   dfile(44)='gsnd1bufr', dtype(44)='sndrd4',    dplat(44)='g11',     dsis(44)='sndrD4_g11',      dval(44)=0.0,  dthin(44)=1,  dsfcalc(44)=0,
   dfile(45)='gsnd1bufr', dtype(45)='sndrd1',    dplat(45)='g13',     dsis(45)='sndrD1_g13',      dval(45)=0.0,  dthin(45)=1,  dsfcalc(45)=0,
   dfile(46)='gsnd1bufr', dtype(46)='sndrd2',    dplat(46)='g13',     dsis(46)='sndrD2_g13',      dval(46)=0.0,  dthin(46)=1,  dsfcalc(46)=0,
   dfile(47)='gsnd1bufr', dtype(47)='sndrd3',    dplat(47)='g13',     dsis(47)='sndrD3_g13',      dval(47)=0.0,  dthin(47)=1,  dsfcalc(47)=0,
   dfile(48)='gsnd1bufr', dtype(48)='sndrd4',    dplat(48)='g13',     dsis(48)='sndrD4_g13',      dval(48)=0.0,  dthin(48)=1,  dsfcalc(48)=0,
   dfile(49)='iasibufr',  dtype(49)='iasi',      dplat(49)='metop-a', dsis(49)='iasi616_metop-a', dval(49)=0.0,  dthin(49)=1,  dsfcalc(49)=1,
   dfile(50)='gomebufr',  dtype(50)='gome',      dplat(50)='metop-a', dsis(50)='gome_metop-a',    dval(50)=0.0,  dthin(50)=2,  dsfcalc(50)=0,
   dfile(51)='omibufr',   dtype(51)='omi',       dplat(51)='aura',    dsis(51)='omi_aura',        dval(51)=0.0,  dthin(51)=2,  dsfcalc(51)=0,
   dfile(52)='sbuvbufr',  dtype(52)='sbuv2',     dplat(52)='n19',     dsis(52)='sbuv8_n19',       dval(52)=0.0,  dthin(52)=0,  dsfcalc(52)=0,
   dfile(53)='hirs4bufr', dtype(53)='hirs4',     dplat(53)='n19',     dsis(53)='hirs4_n19',       dval(53)=0.0,  dthin(53)=1,  dsfcalc(53)=1,
   dfile(54)='amsuabufr', dtype(54)='amsua',     dplat(54)='n19',     dsis(54)='amsua_n19',       dval(54)=0.0,  dthin(54)=1,  dsfcalc(54)=1,
   dfile(55)='mhsbufr',   dtype(55)='mhs',       dplat(55)='n19',     dsis(55)='mhs_n19',         dval(55)=0.0,  dthin(55)=1,  dsfcalc(55)=1,
   dfile(56)='tcvitl'     dtype(56)='tcp',       dplat(56)=' ',       dsis(56)='tcp',             dval(56)=0.0,  dthin(56)=0,  dsfcalc(56)=0,
   $OBSINPUT
 /
  &SUPEROB_RADAR
   $SUPERRAD
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=180.,obpres=1000.,obdattim=${CDATE},
   obhourset=0.,
   $SINGLEOB
 /
 &LAG_DATA
   $LAGDATA
 /
 &HYBRID_ENSEMBLE
 /
 &RAPIDREFRESH_CLDSURF
   l_cloud_analysis=.false.,
   dfi_radar_latent_heat_time_period=30.0,
 /
EOF

# Run global_gsi
eval $PGM < gsiparm.anl $REDOUT$PGMOUT $REDERR$PGMERR
rc=$?

export ERR=$rc
export err=$ERR
$ERRSCRIPT||exit 2


if [[ $FILESTYLE = 'C' ]]
then
   # Output files
   $NCP siganl          $SIGANL
   $NCP satbias_out     $ABIAS
   if [[ -s gesfile_out ]]; then
      $NCP gesfile_out  $GINCOUT
   fi
      if [[ -s biascor_out ]]; then
      $NCP biascor_out  $BIASOUT
   fi
fi

if test "$SAVEGES" = "YES"
then
   cp $SFCANL  $GESdir/${RUN}.${cycle}.sfcanl
   cp $SIGANL  $GESdir/${RUN}.${cycle}.sanl
   cp $ABIAS   $GESdir/${RUN}.${cycle}.abias
fi
if test "$SENDCOM" = "YES"
then
   cp $SFCANL  $COMOUT/${RUN}.${cycle}.sfcanl
   cp $SIGANL  $COMOUT/${RUN}.${cycle}.sanl
   cp $ABIAS   $COMOUT/${RUN}.${cycle}.abias
fi

##############################################################
# Add this statement to release the forecast job once the GSI 
# step is completed
##############################################################
if [ $SENDECF = YES ]
then
   ecflow_client --event release_fcst
fi

# Cat runtime output files.
cat fort.2* > $GSISTAT
cat fort.2*


# Loop over first and last outer loops to generate innovation
# diagnostic files for indicated observation types (groups)
#
# NOTE:  Since we set miter=2 in GSI namelist SETUP, outer
#        loop 03 will contain innovations with respect to 
#        the analysis.  Creation of o-a innovation files
#        is triggered by write_diag(3)=.true.  The setting
#        write_diag(1)=.true. turns on creation of o-g
#        innovation files.
#

cd $DATA    # we should already be in $DATA, but extra cd to be sure.
loops="01 03"
for loop in $loops; do

case $loop in
  01) string=ges;;
  03) string=anl;;
   *) string=$loop;;
esac

#  Collect diagnostic files for obs types (groups) below
   listall="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 sndrd1_g14 sndrd2_g14 sndrd3_g14 sndrd4_g14 sndrd1_g15 sndrd2_g15 sndrd3_g15 sndrd4_g15 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 imgr_g14 imgr_g15 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 sbuv2_n19 gome_metop-a omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 ssmis_las_f17 ssmis_uas_f17 ssmis_img_f17 ssmis_env_f17 ssmis_las_f18 ssmis_uas_f18 ssmis_img_f18 ssmis_env_f18 ssmis_las_f19 ssmis_uas_f19 ssmis_img_f19 ssmis_env_f19 ssmis_las_f20 ssmis_uas_f20 ssmis_img_f20 ssmis_env_f20 iasi_metop-a hirs4_n19 amsua_n19 mhs_n19"

for type in $listall; do
  count=`ls pe*${type}_${loop}* | $wc -l`
  if [[ $count -gt 0 ]]; then
     cat pe*${type}_${loop}* > diag_${type}_${string}.${CDATE}${DIAG_SUFFIX}
  fi
  done
done

cd $DATA    # we should already be in $DATA, but extra cd to be sure.


# Compress diagnostic files
for file in `ls diag_*${CDATE}${DIAG_SUFFIX}`; do
   $COMPRESS $file
done


# Create diagnostic file tarballs
TAROPTS="-uvf"
if [ ! -s $RADSTAT ]; then
   TAROPTS="-cvf"
fi
count=`ls diag_hirs*${CDATE}${SUFFIX}* diag_msu*${CDATE}${SUFFIX}* diag_amsu*${CDATE}${SUFFIX}* diag_sndr*${CDATE}${SUFFIX}* imgr*${CDATE}${SUFFIX}* diag_airs*${CDATE}${SUFFIX}* diag_hsb*${CDATE}${SUFFIX}* diag_ssmi*${CDATE}${SUFFIX}* diag_mhs*${CDATE}${SUFFIX}* diag_amsre*${CDATE}${SUFFIX}* diag_ssmis*${CDATE}${SUFFIX}* diag_iasi*${CDATE}${SUFFIX}* diag_seviri*${CDATE}${SUFFIX}* diag_cris*${CDATE}${SUFFIX}* diag_atms*${CDATE}${SUFFIX}* | $wc -l`
if [[ $count -gt 0 ]]; then
   tar $TAROPTS $RADSTAT diag_hirs*${CDATE}${SUFFIX}* diag_msu*${CDATE}${SUFFIX}* diag_amsu*${CDATE}${SUFFIX}* diag_sndr*${CDATE}${SUFFIX}* imgr*${CDATE}${SUFFIX}* diag_airs*${CDATE}${SUFFIX}* diag_hsb*${CDATE}${SUFFIX}* diag_ssmi*${CDATE}${SUFFIX}* diag_mhs*${CDATE}${SUFFIX}* diag_amsre*${CDATE}${SUFFIX}* diag_ssmis*${CDATE}${SUFFIX}* diag_iasi*${CDATE}${SUFFIX}* diag_seviri*${CDATE}${SUFFIX}* diag_cris*${CDATE}${SUFFIX}* diag_atms*${CDATE}${SUFFIX}*
fi

TAROPTS="-uvf"
if [ ! -s $OZNSTAT ]; then
   TAROPTS="-cvf"
fi
count=`ls diag_sbuv2*${CDATE}${SUFFIX}* diag_gome*${CDATE}${SUFFIX}* diag_omi*${CDATE}${SUFFIX}* diag_mls*${CDATE}${SUFFIX}* | $wc -l`
if [[ $count -gt 0 ]]; then
   tar $TAROPTS $OZNSTAT  diag_sbuv2*${CDATE}${SUFFIX}* diag_gome*${CDATE}${SUFFIX}* diag_omi*${CDATE}${SUFFIX}* diag_mls*${CDATE}${SUFFIX}*
fi

TAROPTS="-uvf"
if [ ! -s $PCPSTAT ]; then
   TAROPTS="-cvf"
fi
count=`ls diag_pcp*${CDATE}${SUFFIX}* | $wc -l`
if [[ $count -gt 0 ]]; then
   tar $TAROPTS $PCPSTAT diag_pcp*${CDATE}${SUFFIX}*
fi

TAROPTS="-uvf"
if [ ! -s $CNVSTAT ]; then
   TAROPTS="-cvf"
fi
count=`ls diag_conv*${CDATE}${SUFFIX}* | $wc -l`
if [[ $count -gt 0 ]]; then
   tar $TAROPTS $CNVSTAT diag_conv*${CDATE}${SUFFIX}*
fi 
chmod 750 $CNVSTAT
chgrp rstprod $CNVSTAT


################################################################################

if test "$RUN" = 'gdas1'
then
    if test "$SENDDBN" = 'YES'
    then
       $DBNROOT/bin/dbn_alert MODEL GDAS1RADSTAT $job $RADSTAT
    fi
fi

################################################################################
#  Postprocessing
cd $pwd
[[ $mkdata = YES ]]&&rmdir $DATA
$ENDSCRIPT
set +x
if [[ "$VERBOSE" = "YES" ]]
then
   echo $(date) EXITING $0 with return code $err >&2
fi
exit $err
