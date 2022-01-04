#!/bin/ksh
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         global_analysis.sh
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
# 2012-01-16  Treadon        add hooks for hybrid ensemble
# 2012-02-14  S. Moorthi     Edited for Zeus and Gaea
# 2013-10-31  R. Todling     Revisit OBS_INPUT (add table) 
# 2014-03-13  X. Li          Add NSST
#
# Usage:  global_analysis.sh SFCGES SIGGES NSTGES GBIAS GBIASPC GRADSTAT GBIASAIR
#                            SFCANL SIGANL NSTANL ABIAS ABIASPC ABIASAIR IGEN
#
#   Input script positional parameters:
#     1             Input surface guess
#                   defaults to $SFCGES; required
#     2             Input sigma guess
#                   defaults to $SIGGES; required
#     3             Input NSST guess
#                   defaults to $NSTGES; required
#     4             Input guess time dependent bias correction coefficients
#                   defaults to $GBIAS; required
#     5             Input guess radiance bias correction pre-conditioning
#                   defaults to $GBIASPC; required
#     6             Input guess radiance diagnostic file
#                   defaults to $GRADSTAT; required
#     7             Input guess aircraft bias correction coefficients
#                   defaults to $GBIASAIR; required
#     8             Output surface analysis
#                   defaults to $SFCANL, then to ${COMOUT}/sfcanl
#     9             Output sigma analysis
#                   defaults to $SIGANL, then to ${COMOUT}/siganl
#    10             Output NSST analysis
#                   defaults to $NSTANL, then to ${COMOUT}/nstanl
#    11             Output bias correction
#                   defaults to $ABIAS, then to ${COMOUT}/abias
#    12             Output bias correction pre-conditioning
#                   defaults to $ABIASPC, then to ${COMOUT}/abias_pc
#    13             Output aircraft bias correction coefficients
#                   defaults to $ABIASAIR, then to ${COMOUT}/abias_air
#    14             Output generating code
#                   defaults to $IGEN, then to 0
#
#   Imported Shell Variables:
#     SFCGES        Input surface guess
#                   overridden by $1; required
#     SIGGES        Input sigma guess
#                   overridden by $2; required
#     GBIAS         Input guess bias correction
#                   overridden by $3; required
#     GBIASPC       Input guess radiance bias correction pre-conditioning
#                   overridden by $4; required
#     GRADSTAT      Input guess angle dependent bias correction
#                   overridden by $5; required
#     GBIASAIR      Input guess aircraft bias correction
#                   overridden by $6; required
#     SFCANL        Output surface analysis
#                   overridden by $7; defaults to ${COMOUT}/sfcanl
#     SIGANL        Output sigma analysis
#                   overridden by $8; defaults to ${COMOUT}/siganl
#     ABIAS         Output bias correction
#                   overridden by $9; defaults to ${COMOUT}/abias
#     ABIASPC       Output bias correction pre-conditioning
#                   overridden by $10; defaults to ${COMOUT}/abias_pc
#     ABIASAIR      Output aircraft bias correction
#                   overridden by $11; defaults to ${COMOUT}/abias_air
#     NSTGES        Input NSST guess
#                   overridden by $10; required
#     NSTANL        Output NSST analysis
#                   overridden by $11; defaults to ${COMOUT}/nstanl
#     IGEN          Output generating code
#                   overridden by $12; defaults to 0
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
#     NSTG03        NSST guess valid at -03 hour
#                   defaults to ${COMOUT}/nstf03; optional input
#     NSTG04        NSST guess valid at -04 hour
#                   defaults to ${COMOUT}/nstf04; optional input
#     NSTG05        NSST guess valid at -05 hour
#                   defaults to ${COMOUT}/nstf05; optional input
#     NSTG07        NSST guess valid at -07 hour
#                   defaults to ${COMOUT}/nstf07; optional input
#     NSTG08        NSST guess valid at -08 hour
#                   defaults to ${COMOUT}/nstf08; optional input
#     NSTG09        NSST guess valid at -09 hour
#                   defaults to ${COMOUT}/nstf09; optional input
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
#     SIGGESENS     template for ensemble member sigma guess
#                   defaults to ${COMOUT}/sigf06_ens; optional input
#     DOHYBVAR      flag (YES or NO) for hybrid ensemble variational option
#                   defaults to NO
#     NMEM_ENS      number of ensemble members included in analysis
#                   defauls to 0
#     GINCIN        Input increment to guess
#                   defaults to ${COMOUT}/gesfile_in; optional
#     BIASIN        Input bias correction to guess
#                   defaults to ${COMOUT}/biascor_in; optional
#     USE_NEWRADBC  Flag to use new radiance bias correction scheme (YES or NO)
#                   defaults to NO
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
#     RUN_SELECT    Flag to only select data (YES or NO)
#                   defaults to NO
#     USE_SELECT    Flag to use selected data (YES or NO)
#                   defaults to NO
#     SELECT_OBS    Tarball containing selected data
#                   defaults to ${COMIN}/${PREINP}obsinput
#     DIAG_SUFFIX   optional suffix for diagnostics files
#                   defaults to empty string
#     DIAG_COMPRESS flag to compress (YES) diagnostics files
#                   defaults to YES
#     DIAG_TARBALL flag to collect (YES) diagnostic files in tarballs
#                   defaults to YES
#     FIXgsm        Directory for global fixed files
#                   defaults to /nwprod/fix
#     EXECgsm       Directory for global executables
#                   defaults to /nwprod/exec
#     USHgsm        Directory for global ush scripts
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
#                   defaults to ${EXECgsm}/global_sighdr$XC
#     SFCHDR        Command to read surface header
#                   defaults to ${EXECgsm}/global_sfchdr$XC
#     CYCLEXEC      Surface cycle executable
#                   defaults to ${EXECgsm}/global_cycle$XC
#     GSIEXEC       Spectral analysis executable
#                   defaults to ${EXECgsm}/global_gsi$XC
#     CYCLESH       Surface cycle script
#                   defaults to ${USHgsm}/global_cycle.sh
#     BERROR        Input background error file
#                   defaults to ${FIXgsm}/global_berror.l${LEVS}y${NLAT_A}.sig.f77
#     SATANGL       Input satellite angle bias file
#                   defaults to ${FIXgsm}/global_satangbias.txt
#     SATINFO       Input satellite information file
#                   defaults to ${FIXgsm}/global_satinfo.txt
#     ATMSFILTER    Path to file describing how ATMS is spatially filtered
#                   defaults to ${FIXgsm}/atms_beamwidth.txt
#     RTMFIX        Input directory containing CRTM coefficients
#                   defaults to ${FIXgsm}/crtm_v2.2.3
#     ANAVINFO      Input analysis variable file
#                   defaults to ${FIXgsm}/global_anavinfo.l${LEVS}.txt
#     CONVINFO      Input conventional observation information file
#                   defaults to ${FIXgsm}/global_convinfo.txt
#     INSITUINFO    Input nsst insitu information file
#                   defaults to ${FIXgsm}/global_insituinfo.txt
#     OZINFO        Input ozone information file
#                   defaults to ${FIXgsm}/global_ozone.txt
#     PCPINFO       Input precipitation information file
#                   defaults to ${FIXgsm}/global_pcpinfo.txt
#     AEROINFO      Input aerosol information file
#                   defaults to ${FIXgsm}/global_aeroinfo.txt
#     SCANINFO      Input satellite scan information file
#                   defaults to ${FIXgsm}/global_scaninfo.txt
#     HYBENSINFO    Input hybrid ensemble localization information file
#                   defaults to ${FIXgsm}/global_hybens_locinfo.l${LEVS}.txt
#     PREPQC        Input QC-ed observation BUFR file
#                   defaults to ${COMIN}/${PREINP}prepbufr${SUFINP}
#     PREPQCPF      Input QC-ed observation profile BUFR file
#                   defaults to ${COMIN}/${PREINP}prepbufr.acft_profiles${SUFINP}
#     SATWND        Input satellite wind file (bufr format)
#                   defaults to ${COMIN}/${PREINP}satwnd.tm00.bufr_d${SUFINP}
#     OSCATBF       Input OSCAT wind file (bufr format)
#                   defaults to ${COMIN}/${PREINP}oscatw.tm00.bufr_d${SUFINP}
#     RAPIDSCATBF   Input RAPIDSCAT wind file (bufr format)
#                   defaults to ${COMIN}/${PREINP}rapidscatw.tm00.bufr_d${SUFINP}
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
#     SEVIRIBF      Input SEVIRI radiace file (bufr format)
#                   defaults to ${COMIN}/${PREINP}sevcsr.tm00.bufr_d${SUFINP}
#     AHIBF         Input AHI radiace file (bufr format)
#                   defaults to ${COMIN}/${PREINP}ahi.tm00.bufr_d${SUFINP}
#     CRISBF        Input CRIS radiance file (bufr format)
#                   defaults to ${COMIN}/${PREINP}cris.tm00.bufr_d${SUFINP}
#     ATMSBF        Input ATMS radiance file (bufr format)
#                   defaults to ${COMIN}/${PREINP}atms.tm00.bufr_d${SUFINP}
#     SAPHIRBF      Input SAPHIR radiance file (bufr format)
#                   defaults to ${COMIN}/${PREINP}saphir.tm00.bufr_d${SUFINP}
#     AMSR2BF       Input AMSR2 L1B brightness temperature files
#                   defaults to ${COMIN}/${PREINP}amsr2.tm00.bufr_d${SUFINP}
#     GMIBF         Input GMI L1CR brightness temperature files
#                   defaults to ${COMIN}/${PREINP}gmi.tm00.bufr_d${SUFINP}
#     SSMITBF       Input SSMI radiace file (bufr format)
#                   defaults to ${COMIN}/${PREINP}ssmit.tm00.bufr_d${SUFINP}
#     SSMISBF       Input SSMIS radiace file (bufr format)
#                   defaults to ${COMIN}/${PREINP}ssmisu.tm00.bufr_d${SUFINP}
#     SBUVBF        Input NOAA POES SBUV ozone retrieval file
#                   defaults to ${COMIN}/${PREINP}osbuv8.tm00.bufr_d${SUFINP}
#     GOMEBF        Input GOME ozone retrieval file
#                   defaults to ${COMIN}/${PREINP}gome.tm00.bufr_d${SUFINP}
#     OMIBF         Input OMI ozone retrieval file
#                   defaults to ${COMIN}/${PREINP}omi.tm00.bufr_d${SUFINP}
#     MLSBF         Input MLS ozone retrieval file
#                   defaults to ${COMIN}/${PREINP}mls.tm00.bufr_d${SUFINP}
#     SMIPCP        Input SSM/I precipitation rate file
#                   defaults to ${COMIN}/${PREINP}spssmip.tm00.bufr_d${SUFINP}
#     TMIPCP        Input TMI precipitation rate file
#                   defaults to ${COMIN}/${PREINP}sptrmm.tm00.bufr_d${SUFINP}
#     GPSROBF       Input GPS radio occultation data
#                   defaults to ${COMIN}/${PREINP}gpsro.tm00.bufr_d${SUFINP}
#     TCVITL        Input tcvitals file
#                   defaults to ${COMIN}/${PREINP}syndata.tcvitals.tm00
#     NSSTBF        Input in situ sea temperature file
#                   defaults to ${COMIN}/${PREINP}nsstbufr.tm00.bufr_d${SUFINP}
#     B1AVHAM       Input AVHRR GAC (AM) file
#                   defaults to ${COMIN}/${PREINP}avcsam.tm00.bufr_d${SUFINP}
#     B1AVHPM       Input AVHRR GAC (PM) file
#                   defaults to ${COMIN}/${PREINP}avcspm.tm00.bufr_d${SUFINP}
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
#     lrun_subdirs  logical to toggle use of subdirectories at runtime for 
#                   pe-specific files
#                   defaults to .true.
#     l4densvar     logical to toggle 4D-EnsVar option
#                   defaults to .false.
#     lwrite4danl   logical to toggle write 4D analysis files
#                   defaults to .false.
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
#                  $ATMSFILTER
#                  $RTMFIX
#                  $ANAVINFO
#                  $CONVINFO
#                  $INSITUINFO
#                  $OZINFO
#                  $PCPINFO
#                  $AEROINFO
#                  $SCANINFO
#                  $HYBENSINFO
#
#     input data : $SFCGES
#                  $SIGGES
#                  $GBIAS
#                  $GBIASPC
#                  $GRADSTAT
#                  $GBIASAIR
#                  $SFCG03
#                  $SFCG04
#                  $SFCG05
#                  $SFCG07
#                  $SFCG08
#                  $SFCG09
#                  $NSTG03
#                  $NSTG04
#                  $NSTG05
#                  $NSTG07
#                  $NSTG08
#                  $NSTG09
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
#                  $PREPQCPF
#                  $SATWND
#                  $OSCATBF
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
#                  $SEVIRIBF
#                  $CRISBF
#                  $ATMSBF
#                  $SSMITBF
#                  $SSMISBF
#                  $SBUVBF
#                  $GOMEBF
#                  $OMIBF
#                  $MLSBF
#                  $SMIPCP
#                  $TMIPCP
#                  $GPSROBF
#                  $TCVITL
#                  $NSSTBF
#                  $B1AVHAM
#                  $B1AVHPM
#
#     output data: $SFCANL
#                  $NSTANL
#                  $SIGANL
#                  $ABIAS
#                  $ABIASPC
#                  $ABIASAIR
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
#   Machine: IBM SP / Zeus / Gaea
#
################################################################################
#  Set environment.
export VERBOSE=${VERBOSE:-"NO"}
if [[ "$VERBOSE" = "YES" ]]
then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi
export machine=${machine:-IBMP6}
export machine=$(echo $machine|tr '[a-z]' '[A-Z]')

#  Command line arguments.
export APRUN=${APRUN:-""}
export SFCGES=${1:-${SFCGES:?}}
export SIGGES=${2:-${SIGGES:?}}
export NSTGES=${3:-${NSTGES:?}}
export GBIAS=${4:-${GBIAS:?}}
export GBIASPC=${5:-${GBIASPC:?}}
export GRADSTAT=${6:-${GRADSTAT:?}}
export GBIASAIR=${7:-${GBIASAIR:?}}
export SFCANL=${8:-${SFCANL}}
export SIGANL=${9:-${SIGANL}}
export NSTANL=${10:-${NSTANL:?}}
export ABIAS=${11:-${ABIAS}}
export ABIASPC=${12:-${ABIASPC}}
export ABIASAIR=${13:-${ABIASAIR}}
export IGEN=${14:-${IGEN:-0}}
export ABIASe=${15:-${ABIASe:-satbias_int.out}}

#  Directories.
export FIXgsm=${FIXgsm:-$HOMEgsm/fix}
export EXECgsm=${EXECgsm:-$HOMEgsm/exec}
export USHgsm=${USHgsm:-$HOMEgsm/ush}
export FIXgsi=${FIXgsi:-$HOMEgsi/fix}
export EXECgsi=${EXECgsi:-$HOMEgsi/exec}

export DATA=${DATA:-/dev/null}
export COMIN=${COMIN:-/dev/null}
export COMOUT=${COMOUT:-/dev/null}
export COMINcfs_cdas=$COMIN

#  Set script / GSI control parameters
NST_GSI=${NST_GSI:-0}                                # NSST:  indicator to control the Tr Analysis mode
NST_TZR=${NST_TZR:-0}                                # NSST:  indicator to control the Tzr_QC mode
NSTINFO=${NSTINFO:-0}                                # NSST:  number of nst variables
FAC_DTL=${FAC_DTL:-0}                                # NSST:  index to apply diurnal thermocline layer  or not
FAC_TSL=${FAC_TSL:-0}                                # NSST:  index to apply thermal skin layer or not
export use_gfs_nemsio=${use_gfs_nemsio:-".false."}   # run GSI with NEMSIO input/output
export l4densvar=${l4densvar:-".false."}             # run GSI in hybrid 4D ensemble-variational mode
export lwrite4danl=${lwrite4danl:-".false."}         # .false. = write single analysis at center time
export DOIAU=${DOIAU:-"NO"}                          # run global_cycle for IAU
export lrun_subdirs=${lrun_subdirs:-".true."}        # run GSI with scratch files in sub-directories
#  Filenames.
export XC=${XC}
export PREINP=${PREINP}
export SUFINP=${SUFINP}
export SIGHDR=${SIGHDR:-${EXECgsm}/global_sighdr$XC}
export SFCHDR=${SFCHDR:-${EXECgsm}/global_sfchdr$XC}
if [ $use_gfs_nemsio = .true. ]; then
  export JCAP=${JCAP:-$($SIGHDR $SIGGES jcap |grep -i "jcap" |awk -F"= " '{print $2}' |awk -F" " '{print $1}')}
  export JCAP_A=${JCAP_A:-$($SIGHDR $SIGGES jcap |grep -i "jcap" |awk -F"= " '{print $2}' |awk -F" " '{print $1}')}
  export LATB=${LATB:-$($SFCHDR $SFCGES latb |grep -i "jcap" |awk -F"= " '{print $2}' |awk -F" " '{print $1}')}
  export LONB=${LONB:-$($SFCHDR $SFCGES lonb |grep -i "jcap" |awk -F"= " '{print $2}' |awk -F" " '{print $1}')}
  export LEVS=${LEVS:-$($SIGHDR $SIGGES levs |grep -i "jcap" |awk -F"= " '{print $2}' |awk -F" " '{print $1}')}
else
  export JCAP=${JCAP:-$($SIGHDR $SIGGES JCAP||echo 0)}
  export JCAP_A=${JCAP_A:-$($SIGHDR $SIGGES JCAP||echo 0)}
  export LATB=${LATB:-$($SFCHDR $SFCGES LATB||echo 0)}
  export LONB=${LONB:-$($SFCHDR $SFCGES LONB||echo 0)}
  export LEVS=${LEVS:-$($SIGHDR $SIGGES LEVS||echo 0)}
fi
export LATA=${LATA:-$LATB}
export LONA=${LONA:-$LONB}
export NLAT_A=${NLAT_A:-$(($LATA+2))}
export NLON_A=${NLON_A:-$LONA}
export DELTIM=${DELTIM:-$((3600/($JCAP_A/20)))}
export CYCLEXEC=${CYCLEXEC:-${EXECgsm}/global_cycle$XC}
export GSIEXEC=${GSIEXEC:-${EXECgsi}/global_gsi$XC}
export CYCLESH=${CYCLESH:-${USHgsm}/global_cycle.sh}
export BERROR=${BERROR:-${FIXgsi}/global_berror.l${LEVS}y${NLAT_A}.f77}
export SATANGL=${SATANGL:-${FIXgsi}/global_satangbias.txt}
export SATINFO=${SATINFO:-${FIXgsi}/global_satinfo.txt}
export ATMSFILTER=${ATMSFILTER:-${FIXgsi}/atms_beamwidth.txt}
export RTMFIX=${RTMFIX:-$NWROOT/lib/crtm/${crtm_ver}/fix}
export ANAVINFO=${ANAVINFO:-${FIXgsi}/global_anavinfo.l${LEVS}.txt}
export CONVINFO=${CONVINFO:-${FIXgsi}/global_convinfo.txt}
export INSITUINFO=${INSITUINFO:-${FIXgsi}/global_insituinfo.txt}
export OZINFO=${OZINFO:-${FIXgsi}/global_ozinfo.txt}
export PCPINFO=${PCPINFO:-${FIXgsi}/global_pcpinfo.txt}
export AEROINFO=${AEROINFO:-${FIXgsi}/global_aeroinfo.txt}
export SCANINFO=${SCANINFO:-${FIXgsi}/global_scaninfo.txt}
export HYBENSINFO=${HYBENSINFO:-${FIXgsi}/global_hybens_locinfo.l${LEVS}.txt}
export OBERROR=${OBERROR:-${FIXgsi}/prepobs_errtable.global}
export PREPQC=${PREPQC:-${COMIN}/${PREINP}prepbufr${SUFINP}}
export PREPQCPF=${PREPQCPF:-${COMIN}/${PREINP}prepbufr.acft_profiles${SUFINP}}
export SATWND=${SATWND:-${COMIN}/${PREINP}satwnd.tm00.bufr_d${SUFINP}}
export OSCATBF=${OSCATBF:-${COMIN}/${PREINP}oscatw.tm00.bufr_d${SUFINP}}
export RAPIDSCATBF=${RAPIDSCATBF:-${COMIN}/${PREINP}rapidscatw.tm00.bufr_d${SUFINP}}
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
export AMSR2BF=${AMSR2BF:-${COMIN}/${PREINP}amsr2.tm00.bufr_d${SUFINP}}
export GMIBF=${GMIBF:-${COMIN}/${PREINP}gmi.tm00.bufr_d${SUFINP}}
export SAPHIRBF=${SAPHIRBF:-${COMIN}/${PREINP}saphir.tm00.bufr_d${SUFINP}}
export SEVIRIBF=${SEVIRIBF:-${COMIN}/${PREINP}sevcsr.tm00.bufr_d${SUFINP}}
export AHIBF=${AHIBF:-${COMIN}/${PREINP}ahi.tm00.bufr_d${SUFINP}}
export CRISBF=${CRISBF:-${COMIN}/${PREINP}cris.tm00.bufr_d${SUFINP}}
export ATMSBF=${ATMSBF:-${COMIN}/${PREINP}atms.tm00.bufr_d${SUFINP}}
export SSMITBF=${SSMITBF:-${COMIN}/${PREINP}ssmit.tm00.bufr_d${SUFINP}}
export SSMISBF=${SSMISBF:-${COMIN}/${PREINP}ssmisu.tm00.bufr_d${SUFINP}}
export SBUVBF=${SBUVBF:-${COMIN}/${PREINP}osbuv8.tm00.bufr_d${SUFINP}}
export GOMEBF=${GOMEBF:-${COMIN}/${PREINP}gome.tm00.bufr_d${SUFINP}}
export OMIBF=${OMIBF:-${COMIN}/${PREINP}omi.tm00.bufr_d${SUFINP}}
export MLSBF=${MLSBF:-${COMIN}/${PREINP}mls.tm00.bufr_d${SUFINP}}
export SMIPCP=${SMIPCP:-${COMIN}/${PREINP}spssmi.tm00.bufr_d${SUFINP}}
export TMIPCP=${TMIPCP:-${COMIN}/${PREINP}sptrmm.tm00.bufr_d${SUFINP}}
export GPSROBF=${GPSROBF:-${COMIN}/${PREINP}gpsro.tm00.bufr_d${SUFINP}}
export TCVITL=${TCVITL:-${COMIN}/${PREINP}syndata.tcvitals.tm00}
export NSSTBF=${NSSTBF:-${COMIN}/${PREINP}nsstbufr.tm00.bufr_d${SUFINP}}
export B1AVHAM=${B1AVHAM:-${COMIN}/${PREINP}avcsam.tm00.bufr_d${SUFINP}}
export B1AVHPM=${B1AVHPM:-${COMIN}/${PREINP}avcspm.tm00.bufr_d${SUFINP}}
export GINCIN=${GINCIN:-${COMOUT}/gesfile_in}
export BIASIN=${BIASIN:-${COMOUT}/biascor_in}
export SFCG03=${SFCG03:-${COMOUT}/sfcf03}
export SFCG04=${SFCG04:-${COMOUT}/sfcf04}
export SFCG05=${SFCG05:-${COMOUT}/sfcf05}
export SFCG07=${SFCG07:-${COMOUT}/sfcf07}
export SFCG08=${SFCG08:-${COMOUT}/sfcf08}
export SFCG09=${SFCG09:-${COMOUT}/sfcf09}
export NSTG03=${NSTG03:-${COMOUT}/nstf03}
export NSTG04=${NSTG04:-${COMOUT}/nstf04}
export NSTG05=${NSTG05:-${COMOUT}/nstf05}
export NSTG07=${NSTG07:-${COMOUT}/nstf07}
export NSTG08=${NSTG08:-${COMOUT}/nstf08}
export NSTG09=${NSTG09:-${COMOUT}/nstf09}
export USE_NEWRADBC=${USE_NEWRADBC:-"NO"}
export SIGG03=${SIGG03:-${COMOUT}/sigf03}
export SIGG04=${SIGG04:-${COMOUT}/sigf04}
export SIGG05=${SIGG05:-${COMOUT}/sigf05}
export SIGG07=${SIGG07:-${COMOUT}/sigf07}
export SIGG08=${SIGG08:-${COMOUT}/sigf08}
export SIGG09=${SIGG09:-${COMOUT}/sigf09}
export SIGGESENS=${SIGGESENS:-${COMOUT}/sigf06_ens}
export NSTGESENS=${NSTGESENS:-${COMOUT}/nstf06_ens}
export SFCGESENS=${SFCGESENS:-${COMOUT}/sfcf06_ens}
export SFCGCYENS=${SFCGCYENS:-${COMOUT}/sfcgcy_ens}
export NSTGESENS_MEAN=${NSTGESENS_MEAN:-${COMOUT}/nstf06_ensmean}
export SFCGESENS_MEAN=${SFCGESENS_MEAN:-${COMOUT}/sfcf06_ensmean}
export SFCGCYENS_MEAN=${SFCGCYENS_MEAN:-${COMOUT}/sfcgcy_ensmean}
export SFCANLENS_MEAN=${SFCANLENS_MEAN:-${COMOUT}/sfcanl_ensmean}
export DOHYBVAR=${DOHYBVAR:-"NO"}
export NMEM_ENS=${NMEM_ENS:-0}
export SFCANL=${SFCANL:-${COMOUT}/${PREINP}sfcanl}
export DTSINC_ENS=${DTSINC_ENS:-${COMOUT}/${PREINP}dtsinc_ens}
export SFCGCY=${SFCGCY:-${COMOUT}/${PREINP}sfcgcy}
export SFCTSK=${SFCTSK:-${COMOUT}/${PREINP}sfctsk}
export NSTANL=${NSTANL:-${COMOUT}/${PREINP}nstanl}
export SIGANL=${SIGANL:-${COMOUT}/${PREINP}sanl}
export ABIAS=${ABIAS:-${COMOUT}/${PREINP}abias}
export ABIASPC=${ABIASPC:-${COMOUT}/${PREINP}abias_pc}
export ABIASAIR=${ABIASAIR:-${COMOUT}/${PREINP}abias_air}
export GINCOUT=${GINCOUT:-${COMOUT}/${PREINP}gesfile_out}
export BIASOUT=${BIASOUT:-${COMOUT}/${PREINP}biascor_out}
export RADSTAT=${RADSTAT:-${COMOUT}/${PREINP}radstat}
export GSISTAT=${GSISTAT:-${COMOUT}/${PREINP}gsistat}
export PCPSTAT=${PCPSTAT:-${COMOUT}/${PREINP}pcpstat}
export CNVSTAT=${CNVSTAT:-${COMOUT}/${PREINP}cnvstat}
export OZNSTAT=${OZNSTAT:-${COMOUT}/${PREINP}oznstat}
export RUN_SELECT=${RUN_SELECT:-"NO"}
export USE_SELECT=${USE_SELECT:-"NO"}
export SELECT_OBS=${SELECT_OBS:-${COMOUT}/${PREINP}obsinput}
export DIAG_SUFFIX=${DIAG_SUFFIX:-""}
export DIAG_COMPRESS=${DIAG_COMPRESS:-"YES"}
export DIAG_TARBALL=${DIAG_TARBALL:-"YES"}
export INISCRIPT=${INISCRIPT}
export ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}
export LOGSCRIPT=${LOGSCRIPT}
export ENDSCRIPT=${ENDSCRIPT}
#  Other variables.
export LSOIL=${LSOIL:-2}
export FSMCL2=${FSMCL2:-60}
if [ $use_gfs_nemsio = .true. ]; then
  export CDATE=${CDATE:-$($SFCHDR $SFCGES idate | grep -i "idate" |awk -F= '{print $2}')}
  export DELTSFC=${DELTSFC:-`$SFCHDR $SFCGES nfhour |awk -F" " '{print $2}'`}
else
  export CDATE=${CDATE:-$($SFCHDR $SFCGES VDATE||echo 0)}
  export DELTSFC=${DELTSFC:-$($SFCHDR $SFCGES FHOUR||echo 0)}
fi
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
export HYBRID_ENSEMBLE=${HYBRID_ENSEMBLE:-""}
export RAPIDREFRESH_CLDSURF=${RAPIDREFRESH_CLDSURF:-""}
export CHEM=${CHEM:-""}
export FILESTYLE=${FILESTYLE:-'X'}
export PGMOUT=${PGMOUT:-${pgmout:-'&1'}}
export PGMERR=${PGMERR:-${pgmerr:-'&2'}}
export OBERRFLAG=${OBERRFLAG:-.false.}
export NCP=${NCP:-cp}
export CHGRP_CMD=${CHGRP_CMD:-"chgrp ${group_name:-rstprod}"}
export NTHREADS=${NTHREADS:-1}
export NTHREADS_GSI=${NTHREADS_GSI:-1}
if [ $machine = IBMP6 ] ; then
  export NTHSTACK=${NTHSTACK:-1024000000}
  export NTHSTACK_GSI=${NTHSTACK_GSI:-1024000000}
  typeset -L1 l=$PGMOUT
  [[ $l = '&' ]]&&a=''||a='>'
  export REDOUT=${REDOUT:-'1>'$a}
  typeset -L1 l=$PGMERR
  [[ $l = '&' ]]&&a=''||a='>'
  export REDERR=${REDERR:-'2>'$a}
  COMPRESS=compress
  UNCOMPRESS=uncompress
else
  typeset -L1 l=$PGMOUT
  [[ $l = '&' ]]&&a=''||a='>'
  export REDOUT=${REDOUT:-'1>'$a}
  typeset -L1 l=$PGMERR
  [[ $l = '&' ]]&&a=''||a='>'
  export REDERR=${REDERR:-'2>'$a}
  COMPRESS=${COMPRESS:-gzip}
  UNCOMPRESS=${UNCOMPRESS:-gunzip}
fi
export wc=${wc:-/usr/bin/wc}

# Set 4D-EnVar specific variables
if [ $l4densvar = .true. ]; then
   export SIGA03=${SIGA03:-${COMOUT}/siga03}
   export SIGA04=${SIGA04:-${COMOUT}/siga04}
   export SIGA05=${SIGA05:-${COMOUT}/siga05}
   export SIGA07=${SIGA07:-${COMOUT}/siga07}
   export SIGA08=${SIGA08:-${COMOUT}/siga08}
   export SIGA09=${SIGA09:-${COMOUT}/siga09}
   export SIGF03ENS=${SIGF03ENS:-${COMROT}/sigf03_ens}
   export SIGF04ENS=${SIGF04ENS:-${COMROT}/sigf04_ens}
   export SIGF05ENS=${SIGF05ENS:-${COMROT}/sigf05_ens}
   export SIGF06ENS=${SIGF06ENS:-${COMROT}/sigf06_ens}
   export SIGF07ENS=${SIGF07ENS:-${COMROT}/sigf07_ens}
   export SIGF08ENS=${SIGF08ENS:-${COMROT}/sigf08_ens}
   export SIGF09ENS=${SIGF09ENS:-${COMROT}/sigf09_ens}
fi

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
rc=0
if [ $NST_GSI -eq 3 ] ; then
   $CYCLESH $SFCGES $SFCGCY
   ra=$?
   ((rc+=ra))
   if [[ "$DOHYBVAR" = "YES" ]]; then
      if [ ! -s ${SFCGCYENS_MEAN} ]; then
         export FNOROG=$FIXgsm/global_orography.t$JCAP_ENKF.$LONB_ENKF.$LATB_ENKF.grb
         export FNOROG_UF=$FIXgsm/global_orography_uf.t$JCAP_ENKF.$LONB_ENKF.$LATB_ENKF.grb
         export FNMASK=$FIXgsm/global_slmask.t$JCAP_ENKF.$LONB_ENKF.$LATB_ENKF.grb
         $CYCLESH $SFCGESENS_MEAN $SFCGCYENS_MEAN
         ra=$?
         ((rc+=ra))
      fi
   fi
else
   $CYCLESH $SFCGES $SFCANL
   ra=$?
   ((rc+=ra))
   if [ $DOIAU = YES ]; then
      export CDATE_SAVE=$CDATE
      export CDATE=$($NDATE -3 $CDATE_SAVE)
      $CYCLESH $SFCG03 $SFCA03
      ra=$?
      ((rc+=ra))
      export CDATE=$CDATE_SAVE
   fi
fi

export ERR=$rc
export err=$ERR
$ERRSCRIPT||exit 11


################################################################################
#  Make atmospheric analysis
export OMP_NUM_THREADS=$NTHREADS_GSI
export PGM='$APRUN $DATA/$(basename $GSIEXEC)'
export pgm=$PGM
$LOGSCRIPT

rm -f berror_stats satbias_angle satinfo *SpcCoeff.bin *TauCoeff.bin EmisCoeff.bin
rm -f AerosolCoeff.bin CloudCoeff.bin anavinfo convinfo ozinfo pcpinfo aeroinfo scaninfo
rm -f errtable prepbufr prepbufr_profl gsndrbufr gsnd1bufr hirs2bufr msubufr hirs3bufr hirs4bufr
rm -f amsuabufr amsubbufr mhsbufr sbuvbufr gomebufr omibufr mlsbufr ssmirrbufr tmirrbufr ahibufr
rm -f airsbufr iasibufr amsrebufr amsr2bufr gmibufr saphirbufr ssmitbufr ssmisbufr gpsrobufr tcvitl hybens_locinfo atms_beamwidth.txt
rm -f hirs3bufrears amsuabufrears amsubbufrears seviribufr crisbufr atmsbufr
rm -f satbias_in satbias_ang.in satbias_out gsiparm.anl satbias_pc satbias_pc.out satbias_out.int
rm -f aircftbias_in aircftbias_out
rm -f sfcf03 sfcf04 sfcf05 sfcf06 sfcf07 sfcf08 sfcf09
rm -f sigf03 sigf04 sigf05 sigf06 sigf07 sigf08 sigf09
rm -f siga03 siga04 siga05 siganl siga07 siga08 siga09
rm -f nstf03 nstf04 nstf05 nstf06 nstf07 nstf08 nstf09
rm -f nstanl
rm -f gesfile_in gesfiles_out
rm -rf dir*

$NCP $GSIEXEC $DATA

if [[ $FILESTYLE = 'C' ]]; then
    export FCPLN=$NCP
else
    export FCPLN="ln -fs"
fi

# Fixed fields
$FCPLN $BERROR   berror_stats
$FCPLN $SATANGL  satbias_angle
$FCPLN $SATINFO  satinfo
$FCPLN $ATMSFILTER atms_beamwidth.txt 
$FCPLN $ANAVINFO anavinfo
$FCPLN $CONVINFO convinfo
$FCPLN $INSITUINFO insituinfo
$FCPLN $OZINFO   ozinfo
$FCPLN $PCPINFO  pcpinfo
$FCPLN $AEROINFO aeroinfo
$FCPLN $SCANINFO scaninfo
$FCPLN $HYBENSINFO hybens_locinfo
$FCPLN $OBERROR  errtable

# CRTM Spectral and Transmittance coefficients
mkdir -p crtm_coeffs
for file in `awk '{if($1!~"!"){print $1}}' satinfo | sort | uniq` ;do
   $FCPLN $RTMFIX/${file}.SpcCoeff.bin ./crtm_coeffs/
   $FCPLN $RTMFIX/${file}.TauCoeff.bin ./crtm_coeffs/
done

$FCPLN $RTMFIX/Nalli.IRwater.EmisCoeff.bin   ./crtm_coeffs/Nalli.IRwater.EmisCoeff.bin
$FCPLN $RTMFIX/NPOESS.IRice.EmisCoeff.bin    ./crtm_coeffs/NPOESS.IRice.EmisCoeff.bin
$FCPLN $RTMFIX/NPOESS.IRland.EmisCoeff.bin   ./crtm_coeffs/NPOESS.IRland.EmisCoeff.bin
$FCPLN $RTMFIX/NPOESS.IRsnow.EmisCoeff.bin   ./crtm_coeffs/NPOESS.IRsnow.EmisCoeff.bin
$FCPLN $RTMFIX/NPOESS.VISice.EmisCoeff.bin   ./crtm_coeffs/NPOESS.VISice.EmisCoeff.bin
$FCPLN $RTMFIX/NPOESS.VISland.EmisCoeff.bin  ./crtm_coeffs/NPOESS.VISland.EmisCoeff.bin
$FCPLN $RTMFIX/NPOESS.VISsnow.EmisCoeff.bin  ./crtm_coeffs/NPOESS.VISsnow.EmisCoeff.bin
$FCPLN $RTMFIX/NPOESS.VISwater.EmisCoeff.bin ./crtm_coeffs/NPOESS.VISwater.EmisCoeff.bin
$FCPLN $RTMFIX/FASTEM6.MWwater.EmisCoeff.bin ./crtm_coeffs/FASTEM6.MWwater.EmisCoeff.bin
$FCPLN $RTMFIX/AerosolCoeff.bin              ./crtm_coeffs/AerosolCoeff.bin
$FCPLN $RTMFIX/CloudCoeff.bin                ./crtm_coeffs/CloudCoeff.bin

# Observational data
$FCPLN $PREPQC   prepbufr
$FCPLN $PREPQCPF prepbufr_profl
$FCPLN $SATWND   satwndbufr
$FCPLN $OSCATBF  oscatbufr
$FCPLN $RAPIDSCATBF  rapidscatbufr
$FCPLN $GSNDBF   gsndrbufr
$FCPLN $GSNDBF1  gsnd1bufr
$FCPLN $B1HRS2   hirs2bufr
$FCPLN $B1MSU    msubufr
$FCPLN $B1HRS3   hirs3bufr
$FCPLN $B1HRS4   hirs4bufr
$FCPLN $B1AMUA   amsuabufr
$FCPLN $B1AMUB   amsubbufr
$FCPLN $B1MHS    mhsbufr
$FCPLN $ESHRS3   hirs3bufrears
$FCPLN $ESAMUA   amsuabufrears
$FCPLN $ESAMUB   amsubbufrears
$FCPLN $SBUVBF   sbuvbufr
$FCPLN $GOMEBF   gomebufr
$FCPLN $OMIBF    omibufr
$FCPLN $MLSBF    mlsbufr
$FCPLN $SMIPCP   ssmirrbufr
$FCPLN $TMIPCP   tmirrbufr
$FCPLN $AIRSBF   airsbufr
$FCPLN $IASIBF   iasibufr
$FCPLN $AMSREBF  amsrebufr
$FCPLN $AMSR2BF  amsr2bufr
$FCPLN $GMIBF    gmibufr
$FCPLN $SAPHIRBF saphirbufr
$FCPLN $SEVIRIBF seviribufr
$FCPLN $CRISBF   crisbufr
$FCPLN $ATMSBF   atmsbufr
$FCPLN $SSMITBF  ssmitbufr
$FCPLN $SSMISBF  ssmisbufr
$FCPLN $GPSROBF  gpsrobufr
$FCPLN $TCVITL   tcvitl
$FCPLN $NSSTBF   nsstbufr
$FCPLN $B1AVHAM  avhambufr
$FCPLN $B1AVHPM  avhpmbufr
$FCPLN $AHIBF    ahibufr

# Required guess fields
$FCPLN $GBIAS    satbias_in
$FCPLN $GBIASPC  satbias_pc
$FCPLN $GRADSTAT radstat.gdas
$FCPLN $GBIASAIR aircftbias_in

$FCPLN $SIGG03   sigf03
$FCPLN $SIGGES   sigf06
$FCPLN $SIGG09   sigf09
$FCPLN $SFCG03   sfcf03
$FCPLN $SFCGES   sfcf06
$FCPLN $SFCG09   sfcf09
$FCPLN $NSTG03   nstf03
$FCPLN $NSTGES   nstf06
$FCPLN $NSTG09   nstf09

if [[ $NST_GSI -gt 0 ]]; then
   if [[ $NST_GSI -eq 3 ]]; then
      $FCPLN $SFCGCY sfcgcy
   else
      $FCPLN $SFCANL sfcgcy
   fi
fi

if [[ "$DOHYBVAR" = "YES" ]]; then
   mkdir -p ensemble_data
   imem=1
   while [[ $imem -le $NMEM_ENS ]]; do
      member="_mem"`printf %03i $imem`
      if [ $l4densvar = .true. ]; then
         sigens3=${SIGF03ENS}${member}
         sigens4=${SIGF04ENS}${member}
         sigens5=${SIGF05ENS}${member}
         sigens6=${SIGF06ENS}${member}
         sigens7=${SIGF07ENS}${member}
         sigens8=${SIGF08ENS}${member}
         sigens9=${SIGF09ENS}${member}
         $FCPLN $sigens3 ./ensemble_data/sigf03_ens${member}
         $FCPLN $sigens4 ./ensemble_data/sigf04_ens${member}
         $FCPLN $sigens5 ./ensemble_data/sigf05_ens${member}
         $FCPLN $sigens6 ./ensemble_data/sigf06_ens${member}
         $FCPLN $sigens7 ./ensemble_data/sigf07_ens${member}
         $FCPLN $sigens8 ./ensemble_data/sigf08_ens${member}
         $FCPLN $sigens9 ./ensemble_data/sigf09_ens${member}
      else
         sigens=${SIGGESENS}${member}
         $FCPLN $sigens ./ensemble_data/sigf06_ens${member}
      fi
      (( imem = $imem + 1 ))
   done
   if [[ $NST_GSI -gt 0 ]]; then
      $FCPLN $NSTGESENS_MEAN nstf06_ensmean
      $FCPLN $SFCGESENS_MEAN sfcf06_ensmean
      if [[ $NST_GSI -eq 3 ]]; then
         $FCPLN $SFCGCYENS_MEAN sfcgcy_ensmean
      else
         $FCPLN $SFCANLENS_MEAN sfcgcy_ensmean
      fi
   fi
fi


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
   $FCPLN $SIGG04   sigf04
fi
if [[ -s $SIGG05 ]]; then
   $FCPLN $SIGG05   sigf05
fi
if [[ -s $SIGG07 ]]; then
   $FCPLN $SIGG07   sigf07
fi
if [[ -s $SIGG08 ]]; then
   $FCPLN $SIGG08   sigf08
fi

if [[ -s $SFCG04 ]]; then
   $FCPLN $SFCG04   sfcf04
fi
if [[ -s $SFCG05 ]]; then
   $FCPLN $SFCG05   sfcf05
fi
if [[ -s $SFCG07 ]]; then
   $FCPLN $SFCG07   sfcf07
fi
if [[ -s $SFCG08 ]]; then
   $FCPLN $SFCG08   sfcf08
fi

if [[ -s $NSTG04 ]]; then
   $FCPLN $NSTG04   nstf04
fi
if [[ -s $NSTG05 ]]; then
   $FCPLN $NSTG05   nstf05
fi
if [[ -s $NSTG07 ]]; then
   $FCPLN $NSTG07   nstf07
fi
if [[ -s $NSTG08 ]]; then
   $FCPLN $NSTG08   nstf08
fi

if [[ -s $GINCIN ]]; then
   $FCPLN $GINCIN   gesfile_in
fi
if [[ -s $BIASIN ]]; then
   $FCPLN $BIASIN   biascor_in
fi

## NOTE:  10/01/2003, r.treadon
##    Do not add above file existence tests since
##    we do not link to files in operations

if [ $FILESTYLE != 'C' ]; then

   # Output files
   ln -fs $SIGANL siganl
   if [ $l4densvar = .true. -a $lwrite4danl = .true. ]; then
      ln -fs $SIGA03 siga03
      ln -fs $SIGA04 siga04
      ln -fs $SIGA05 siga05
      ln -fs $SIGA07 siga07
      ln -fs $SIGA08 siga08
      ln -fs $SIGA09 siga09
   fi
   ln -fs $ABIAS    satbias_out
   ln -fs $ABIASPC  satbias_pc.out
   ln -fs $GINCOUT  gesfile_out
   ln -fs $BIASOUT  biascor_out
   ln -fs $ABIASAIR aircftbias_out

   if [ $NST_GSI -gt 0 ] ; then
      ln -fs $NSTANL nstanl
      ln -fs $SFCTSK sfctsk
      if [[ "$DOHYBVAR" = "YES" ]]; then
         ln -fs $DTSINC_ENS dtsinc_ens
      fi
      if [ $NST_GSI -eq 3 ] ; then
         ln -fs $SFCANL sfcanl
      fi
   fi
#  If requested, create obs_input tarball
   if [[ "$RUN_SELECT" = "YES" ]]; then
      ln -fs $SELECT_OBS obs_input.tar
   fi

fi

# If requested, copy and de-tar obs_input
if [[ "$USE_SELECT" = "YES" ]]; then
  rm obs_input*
  $FCPLN $SELECT_OBS ./obs_input.tar
  tar -xvf obs_input.tar
fi


# If requested, copy and de-tar guess radstat file
if [[ "$USE_NEWRADBC" = "YES" ]]; then
   listdiag=`tar xvf radstat.gdas | cut -d' ' -f2 | grep _ges`
   for type in $listdiag; do
      diag_file=`echo $type | cut -d',' -f1`
      fname=`echo $diag_file | cut -d'.' -f1`
      date=`echo $diag_file | cut -d'.' -f2`
      $UNCOMPRESS $diag_file
      fnameges=$(echo $fname|sed 's/_ges//g')
      mv $fname.$date $fnameges
   done
fi


# Create global_gsi namelist
cat <<EOF > gsiparm.anl
 &SETUP
   miter=2,niter(1)=100,niter(2)=100,
   niter_no_qc(1)=50,niter_no_qc(2)=0,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   qoption=2,
   gencode=$IGEN,factqmin=5.0,factqmax=0.005,deltim=$DELTIM,
   iguess=-1,
   nst_gsi=$NST_GSI,nst_tzr=$NST_TZR,nstinfo=$NSTINFO,fac_dtl=$FAC_DTL,fac_tsl=$FAC_TSL,
   oneobtest=.false.,retrieval=.false.,l_foto=.false.,
   use_pbl=.false.,use_compress=.true.,nsig_ext=12,gpstop=50.,
   use_gfs_nemsio=${use_gfs_nemsio},lrun_subdirs=${lrun_subdirs},
   crtm_coeffs_path='./crtm_coeffs/',
   newpc4pred=.true.,adp_anglebc=.true.,angord=4,passive_bc=.true.,use_edges=.false.,
   diag_precon=.true.,step_start=1.e-3,emiss_bc=.true.,thin4d=.true.,cwoption=3,USE_PREPB_SATWND=.true.,
   $SETUP
 /
 &GRIDOPTS
   JCAP_B=$JCAP,JCAP=$JCAP_A,NLAT=$NLAT_A,NLON=$NLON_A,nsig=$LEVS,
   regional=.false.,nlayers(63)=3,nlayers(64)=6,
   $GRIDOPTS
 /
 &BKGERR
   vs=0.7,
   hzscl=1.7,0.8,0.5,
   hswgt=0.45,0.3,0.25,
   bw=0.0,norsp=4,
   bkgv_flowdep=.true.,bkgv_rewgtfct=1.5,
   bkgv_write=.false.,
   cwcoveqqcov=.false.,
   $BKGVERR
 /
 &ANBKGERR
   anisotropic=.false.,
   $ANBKGERR
 /
 &JCOPTS
   ljcdfi=.false.,alphajc=0.0,ljcpdry=.true.,bamp_jcpdry=5.0e7,
   $JCOPTS
 /
 &STRONGOPTS
   tlnmc_option=2,nstrong=1,nvmodes_keep=8,period_max=6.,period_width=1.5,
   $STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.true.,oberrflg=.false.,c_varqc=0.02,
   use_poq7=.true.,qc_noirjaco3_pole=.true.,
   aircraft_t_bc=.false.,biaspredt=1000.0,upd_aircraft=.true.,
   $OBSQC
 /
 &OBS_INPUT
   dmesh(1)=145.0,dmesh(2)=150.0,dmesh(3)=100.0,time_window_max=3.0,
   $OBSINPUT
 /
OBS_INPUT::
!  dfile          dtype       dplat       dsis                dval    dthin dsfcalc
   prepbufr       ps          null        ps                  0.0     0     0
   prepbufr       t           null        t                   0.0     0     0
!  prepbufr_profl t           null        t                   0.0     0     0
   prepbufr       q           null        q                   0.0     0     0
!  prepbufr_profl q           null        q                   0.0     0     0
   prepbufr       pw          null        pw                  0.0     0     0
   prepbufr       uv          null        uv                  0.0     0     0
!  prepbufr_profl uv          null        uv                  0.0     0     0
   satwndbufr     uv          null        uv                  0.0     0     0
   prepbufr       spd         null        spd                 0.0     0     0
   prepbufr       dw          null        dw                  0.0     0     0
   radarbufr      rw          null        rw                  0.0     0     0
   prepbufr       sst         null        sst                 0.0     0     0
   nsstbufr       sst         nsst        sst                 0.0     0     0
   gpsrobufr      gps_ref     null        gps                 0.0     0     0
   ssmirrbufr     pcp_ssmi    dmsp        pcp_ssmi            0.0    -1     0
   tmirrbufr      pcp_tmi     trmm        pcp_tmi             0.0    -1     0
   sbuvbufr       sbuv2       n16         sbuv8_n16           0.0     0     0
   sbuvbufr       sbuv2       n17         sbuv8_n17           0.0     0     0
   sbuvbufr       sbuv2       n18         sbuv8_n18           0.0     0     0
   hirs3bufr      hirs3       n17         hirs3_n17           0.0     1     0
   hirs4bufr      hirs4       metop-a     hirs4_metop-a       0.0     1     1
   gimgrbufr      goes_img    g11         imgr_g11            0.0     1     0
   gimgrbufr      goes_img    g12         imgr_g12            0.0     1     0
   airsbufr       airs        aqua        airs281SUBSET_aqua  0.0     1     1
   amsuabufr      amsua       n15         amsua_n15           0.0     1     1
   amsuabufr      amsua       n18         amsua_n18           0.0     1     1
   amsuabufr      amsua       metop-a     amsua_metop-a       0.0     1     1
   airsbufr       amsua       aqua        amsua_aqua          0.0     1     1
   amsubbufr      amsub       n17         amsub_n17           0.0     1     1
   mhsbufr        mhs         n18         mhs_n18             0.0     1     1
   mhsbufr        mhs         metop-a     mhs_metop-a         0.0     1     1
   ssmitbufr      ssmi        f14         ssmi_f14            0.0     1     0
   ssmitbufr      ssmi        f15         ssmi_f15            0.0     1     0
   amsrebufr      amsre_low   aqua        amsre_aqua          0.0     1     0
   amsrebufr      amsre_mid   aqua        amsre_aqua          0.0     1     0
   amsrebufr      amsre_hig   aqua        amsre_aqua          0.0     1     0
   ssmisbufr      ssmis       f16         ssmis_f16           0.0     1     0
   ssmisbufr      ssmis       f17         ssmis_f17           0.0     1     0
   ssmisbufr      ssmis       f18         ssmis_f18           0.0     1     0
   ssmisbufr      ssmis       f19         ssmis_f19           0.0     1     0
   gsnd1bufr      sndrd1      g12         sndrD1_g12          0.0     1     0
   gsnd1bufr      sndrd2      g12         sndrD2_g12          0.0     1     0
   gsnd1bufr      sndrd3      g12         sndrD3_g12          0.0     1     0
   gsnd1bufr      sndrd4      g12         sndrD4_g12          0.0     1     0
   gsnd1bufr      sndrd1      g11         sndrD1_g11          0.0     1     0
   gsnd1bufr      sndrd2      g11         sndrD2_g11          0.0     1     0
   gsnd1bufr      sndrd3      g11         sndrD3_g11          0.0     1     0
   gsnd1bufr      sndrd4      g11         sndrD4_g11          0.0     1     0
   gsnd1bufr      sndrd1      g13         sndrD1_g13          0.0     1     0
   gsnd1bufr      sndrd2      g13         sndrD2_g13          0.0     1     0
   gsnd1bufr      sndrd3      g13         sndrD3_g13          0.0     1     0
   gsnd1bufr      sndrd4      g13         sndrD4_g13          0.0     1     0
   iasibufr       iasi        metop-a     iasi616_metop-a     0.0     1     1
   gomebufr       gome        metop-a     gome_metop-a        0.0     2     0
   omibufr        omi         aura        omi_aura            0.0     2     0
   sbuvbufr       sbuv2       n19         sbuv8_n19           0.0     0     0
   hirs4bufr      hirs4       n19         hirs4_n19           0.0     1     1
   amsuabufr      amsua       n19         amsua_n19           0.0     1     1
   mhsbufr        mhs         n19         mhs_n19             0.0     1     1
   tcvitl         tcp         null        tcp                 0.0     0     0
   seviribufr     seviri      m08         seviri_m08          0.0     1     0
   seviribufr     seviri      m09         seviri_m09          0.0     1     0
   seviribufr     seviri      m10         seviri_m10          0.0     1     0
   hirs4bufr      hirs4       metop-b     hirs4_metop-b       0.0     1     1
   amsuabufr      amsua       metop-b     amsua_metop-b       0.0     1     1
   mhsbufr        mhs         metop-b     mhs_metop-b         0.0     1     1
   iasibufr       iasi        metop-b     iasi616_metop-b     0.0     1     1
   gomebufr       gome        metop-b     gome_metop-b        0.0     2     0
   atmsbufr       atms        npp         atms_npp            0.0     1     0
   crisbufr       cris        npp         cris_npp            0.0     1     0
   gsnd1bufr      sndrd1      g14         sndrD1_g14          0.0     1     0
   gsnd1bufr      sndrd2      g14         sndrD2_g14          0.0     1     0
   gsnd1bufr      sndrd3      g14         sndrD3_g14          0.0     1     0
   gsnd1bufr      sndrd4      g14         sndrD4_g14          0.0     1     0
   gsnd1bufr      sndrd1      g15         sndrD1_g15          0.0     1     0
   gsnd1bufr      sndrd2      g15         sndrD2_g15          0.0     1     0
   gsnd1bufr      sndrd3      g15         sndrD3_g15          0.0     1     0
   gsnd1bufr      sndrd4      g15         sndrD4_g15          0.0     1     0
   oscatbufr      uv          null        uv                  0.0     0     0
   mlsbufr        mls30       aura        mls30_aura          0.0     0     0
   avhambufr      avhrr       metop-a     avhrr3_metop-a      0.0     1     0
   avhpmbufr      avhrr       n18         avhrr3_n18          0.0     1     0
   amsr2bufr      amsr2       gcom-w1     amsr2_gcom-w1       0.0     3     0
   gmibufr        gmi         gpm         gmi_gpm             0.0     3     0
   saphirbufr     saphir      meghat      saphir_meghat       0.0     3     0
   ahibufr        ahi         himawari8   ahi_himawari8       0.0     3     0
   rapidscatbufr  uv          null        uv                  0.0     0     0
::
 &SUPEROB_RADAR
   $SUPERRAD
 /
 &LAG_DATA
   $LAGDATA
 /
 &HYBRID_ENSEMBLE
   ensemble_path='./ensemble_data/',
   $HYBRID_ENSEMBLE
 /
 &RAPIDREFRESH_CLDSURF
   dfi_radar_latent_heat_time_period=30.0,
   $RAPIDREFRESH_CLDSURF
 /
 &CHEM
   $CHEM
 /
 &SINGLEOB_TEST
   maginnov=0.1,magoberr=0.1,oneob_type='t',
   oblat=45.,oblon=180.,obpres=1000.,obdattim=${CDATE},
   obhourset=0.,
   $SINGLEOB
 /
EOF


# Run global_gsi
eval $PGM < gsiparm.anl $REDOUT$PGMOUT $REDERR$PGMERR
rc=$?

export ERR=$rc
export err=$ERR
$ERRSCRIPT||exit 2


if [[ $FILESTYLE = 'C' ]]; then
   # Output files
   $NCP siganl $SIGANL
   if [ $l4densvar = .true. -a $lwrite4danl = .true. ]; then
      $NCP siga03 $SIGA03
      $NCP siga04 $SIGA04
      $NCP siga05 $SIGA05
      $NCP siga07 $SIGA07
      $NCP siga08 $SIGA08
      $NCP siga09 $SIGA09
   fi
   $NCP satbias_out     $ABIAS
   $NCP satbias_pc.out  $ABIASPC
   $NCP aircftbias_out  $ABIASAIR
   if [[ -s gesfile_out ]]; then
      $NCP gesfile_out  $GINCOUT
   fi
      if [[ -s biascor_out ]]; then
      $NCP biascor_out  $BIASOUT
   fi
   if [ $NST_GSI -gt 0 ] ; then
      $NCP  nstanl $NSTANL
      if [[ "$DOHYBVAR" = "YES" ]]; then
         $NCP dtsinc_ens $DTSINC_ENS
      fi
      if [ $NST_GSI -eq 3 ] ; then
         $NCP sfcanl $SFCANL
      fi
   fi
fi

# For eobs, eomn and eupd
if [[ -s satbias_out.int ]]; then
   $NCP satbias_out.int $ABIASe
else
   $NCP satbias_in $ABIASe
fi

if test "$SAVEGES" = "YES"
then
   cp $SFCANL   $GESOUT/${PREINP}sfcanl
   if [ $NST_GSI -gt 0 ] ; then
      cp $NSTANL  $GESOUT/${PREINP}nstanl
   fi
   cp $SIGANL   $GESOUT/${PREINP}sanl
   cp $ABIAS    $GESOUT/${PREINP}abias
   cp $ABIASPC  $GESOUT/${PREINP}abias_pc
   cp $ABIASAIR $GESOUT/${PREINP}abias_air
fi
if test "$SENDCOM" = "YES"
then
   cp $SFCANL   $COMOUT/${PREINP}sfcanl
   cp $SIGANL   $COMOUT/${PREINP}sanl
   cp $ABIAS    $COMOUT/${PREINP}abias
   cp $ABIASPC  $COMOUT/${PREINP}abias_pc
   cp $ABIASAIR $COMOUT/${PREINP}abias_air
   if [ $NST_GSI -gt 0 ] ; then
      cp $NSTANL  $GESOUT/${PREINP}nstanl
   fi
fi

##############################################################
# Add this statement to release the forecast job once the GSI 
# step is completed.  Do not release forecast when RUN=enkf
##############################################################
if [ "$SENDECF" = YES -a "$RUN" != enkf ]
then
   ecflow_client --event release_fcst
fi

# Cat runtime output files.
cat fort.2* > $GSISTAT
cat fort.2*

# If requested, create obs_input tarball
if [[ "$RUN_SELECT" = "YES" ]]; then
  echo $(date) START tar obs_input >&2
  rm obs_input.tar
  if [[ $FILESTYLE = 'L' ]]; then
     ln -fs $SELECT_OBS ./obs_input.tar
  fi
  tar -cvf obs_input.tar obs_input*
  if [[ $FILESTYLE = 'C' ]]; then
     $NCP obs_input.tar $SELECT_OBS
  fi
  chmod 750 $SELECT_OBS
  ${CHGRP_CMD} $SELECT_OBS
  echo $(date) END tar obs_input >&2
fi

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

# Set up lists and variables for various types of diagnostic files.
ntype=3

diagtype[0]="conv"
diagtype[1]="pcp_ssmi_dmsp pcp_tmi_trmm"
diagtype[2]="sbuv2_n16 sbuv2_n17 sbuv2_n18 sbuv2_n19 gome_metop-a gome_metop-b omi_aura mls30_aura"
diagtype[3]="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 sndrd1_g14 sndrd2_g14 sndrd3_g14 sndrd4_g14 sndrd1_g15 sndrd2_g15 sndrd3_g15 sndrd4_g15 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 imgr_g14 imgr_g15 ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_f16 ssmis_f17 ssmis_f18 ssmis_f19 ssmis_f20 iasi_metop-a hirs4_n19 amsua_n19 mhs_n19 seviri_m08 seviri_m09 seviri_m10 cris_npp atms_npp hirs4_metop-b amsua_metop-b mhs_metop-b iasi_metop-b avhrr_n18 avhrr_metop-a amsr2_gcom-w1 gmi_gpm saphir_meghat ahi_himawari8"

diaglist[0]=listcnv
diaglist[1]=listpcp
diaglist[2]=listozn
diaglist[3]=listrad

diagfile[0]=$CNVSTAT
diagfile[1]=$PCPSTAT
diagfile[2]=$OZNSTAT
diagfile[3]=$RADSTAT

numfile[0]=0
numfile[1]=0
numfile[2]=0
numfile[3]=0


# Set diagnostic file prefix based on lrun_subdirs variable
if [ $lrun_subdirs = ".true." ]; then
   prefix=" dir.*/"
else
   prefix="pe*"
fi

# Collect diagnostic files as a function of loop and type.
loops="01 03"
for loop in $loops; do
   case $loop in
      01) string=ges;;
      03) string=anl;;
       *) string=$loop;;
   esac
   echo $(date) START loop $string >&2
   n=-1
   while [ $((n+=1)) -le $ntype ] ;do
      for type in `echo ${diagtype[n]}`; do
         count=`ls ${prefix}${type}_${loop}* | $wc -l`
         if [ $count -gt 0 ]; then
            cat ${prefix}${type}_${loop}* > diag_${type}_${string}.${CDATE}${DIAG_SUFFIX}
            echo "diag_${type}_${string}.${CDATE}*" >> ${diaglist[n]}
            numfile[n]=`expr ${numfile[n]} + 1`
         fi
      done
   done
   echo $(date) END loop $string >&2
done

cd $DATA    # we should already be in $DATA, but extra cd to be sure.

# If requested, compress diagnostic files
if [[ $DIAG_COMPRESS = YES ]]; then
   echo $(date) START $COMPRESS diagnostic files >&2
   for file in `ls diag_*${CDATE}${DIAG_SUFFIX}`; do
      $COMPRESS $file
   done
   echo $(date) END $COMPRESS diagnostic files >&2
fi

# If requested, create diagnostic file tarballs
if [[ $DIAG_TARBALL = YES ]]; then
   echo $(date) START tar diagnostic files >&2
   n=-1
   while [ $((n+=1)) -le $ntype ] ;do
      TAROPTS="-uvf"
      if [ ! -s ${diagfile[n]} ]; then
         TAROPTS="-cvf"
      fi
      if [ ${numfile[n]} -gt 0 ]; then
         tar $TAROPTS ${diagfile[n]} `cat ${diaglist[n]}`
      fi
   done

#  Restrict CNVSTAT 
   chmod 750 $CNVSTAT
   ${CHGRP_CMD} $CNVSTAT
   echo $(date) END tar diagnostic files >&2
fi


################################################################################

if test "$RUN" = 'gdas'
then
    if test "$SENDDBN" = 'YES'
    then
       $DBNROOT/bin/dbn_alert MODEL GDAS1RADSTAT $job $RADSTAT
    fi
fi

if test "$RUN" = 'gfs'
then
    if test "$SENDDBN" = 'YES'
    then
       $DBNROOT/bin/dbn_alert MODEL GFS_abias $job $COMOUT/${PREINP}abias
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