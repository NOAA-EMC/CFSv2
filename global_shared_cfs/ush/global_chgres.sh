#!/bin/ksh
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         global_chgres.sh           
# Script description:  Changes resolution of global restart files.
#
# Author:        Mark Iredell       Org: NP23         Date: 1999-03-01
#
# Abstract: This script changes the resolution of the global restart files,
#   namely the sigma file, surface file, nst file or all 3.  When converting
#   an nst file, you must also convert a surface file.  The resolution
#   of the output files is given in the argument list or as imported
#   environment variables.  The resolution of the input files are taken
#   from the header records in the respective files.  Resolution is
#   given as spectral truncation, number of levels, number of longitudes
#   and number of latitudes.  The names of the input and output restart files
#   are also given in the argument list or as imported environment variables.
#   Other control variables may be specified in the imported environment, too.
#   For example, an alternate orography and sigma level structure may be
#   specified for the output sigma file, while an alternate sea-land mask
#   may be specified for the surface file.
#
# Script history log:
# 1999-03-01  Mark Iredell
# 2011-08-05  Added logic for nst restart files.  G. Gayno
# 2011-10-10  Updated for gaea  - S. Moorthi
# 2016-01-26  Updated root directoris to follow NCO implementation standard - F. Yang
#
# Usage:  global_chgres.sh SIGINP SFCINP SIGOUT SFCOUT JCAP LEVS LONB LATB \
#                          GFSOUT OUTTYP IDRT NSTINP NSTOUT SFNOUT
#
#   Input script positional parameters:
#     1             Input sigma file
#                   defaults to $SIGINP; skip sigma conversion if missing
#     2             Input surface file
#                   defaults to $SFCINP; skip surface conversion if missing
#     3             Output sigma file
#                   defaults to $SIGOUT, then to sigout
#     4             Output sigma file
#                   defaults to $SFCOUT, then to sfcout
#     5             New spectral truncation
#                   defaults to $JCAP; one or the other is required
#     6             New number of levels
#                   defaults to $LEVS; one or the other is required
#     7             New number of longitudes
#                   defaults to $LONB; one or the other is required
#     8             New number of latitudes
#                   defaults to $LATB; one or the other is required
#     9             Output gfsio file
#                   defaults to $GFSOUT, then to gfsout
#     10            Output file type 
#                   defaults to $OUTTYP, then to 2
#     11            Grid output file type 
#                   defaults to $IDRT, then to 4
#     12            Input nst file
#                   defaults to $NSTINP; skip nst conversion if missing
#     13            Output nst file
#                   defaults to $NSTOUT, then to nstout
#     14            Output gfsio surface file
#                   defaults to $SFNOUT; then to sfnout
#
#   Imported Shell Variables:
#     SIGINP        Input sigma file
#                   overridden by $1; skip sigma conversion if missing
#     SFCINP        Input surface file
#                   overridden by $2; skip surface conversion if missing
#     NSTINP        Input nst file
#                   overridden by $12; skip surface conversion if missing
#     OUTTYP        Output file type
#                   1: gfsio ; 2: sigio sigma (default); 0: both 
#     IDRT          Grid output file type
#                   4: guassian(default); 0: lat-lon                            
#     SIGOUT        Output sigma file
#                   overridden by $3; defaults to sigout
#     GFSOUT        Output gfsio file
#                   overridden by $9; defaults to gfsout
#     SFCOUT        Output surface file
#                   overridden by $4; defaults to sfcout
#     NSTOUT        Output nst file
#                   overridden by $13; defaults to nstout
#     SFNOUT        Output gfsio surface file
#                   overridden by $14; defaults to sfnout
#     JCAP          New spectral truncation
#                   overridden by $5; one or the other is required
#     LEVS          New number of levels
#                   overridden by $6; one or the other is required
#     LONB          New number of longitudes
#                   overridden by $7; one or the other is required
#     LATB          New number of latitudes
#                   overridden by $8; one or the other is required
#     NTRAC         New number of tracers
#                   defaults to input sigma file value
#     IDVC          New vertical coordinate id (1 for sigma, 2 for hybrid)
#                   defaults to input sigma file value
#     IDSL          New midlayer pressure id (1 for phillips, 2 for mean)
#                   defaults to input sigma file value
#     NWROOT        A string that defaults to /nwprod
#     FIXSUBDA      Subdirectory string for fix fields; defaults to /fix
#                   To use parallel fix, set it to /fix/fix_am
#     FIXgsm        Directory for global fixed files
#                   defaults to ${NWROOT}/$FIXSUBDA
#     EXECgsm       Directory for global executables
#                   defaults to $NWROOT/exec
#     DATA          working directory
#                   (if nonexistent will be made, used and deleted)
#                   defaults to current working directory
#     XC            Suffix to add to executables
#                   defaults to none
#     OROGRAPHY     New orography ("NULL" to use from input sigma file)
#                   defaults to ${FIXgsm}/global_orography.t${JCAP}.grb
#     OROGRAPHY_UF  New unfiltered orography
#                   defaults to ${FIXgsm}/global_orography_uf.t${JCAP}.$LONB.$LATB.grb
#     SIGLEVEL      New sigma levels ("NULL" to use from input sigma file)
#                   defaults to ${FIXgsm}/global_siglevel.l${LEVS}.txt
#     O3CLIM        New ozone climatology (required if adding ozone)
#                   defaults to ${FIXgsm}/global_o3clim.txt
#     SLMASK        New sea-land mask ("NULL" to use from input surface file)
#                   defaults to ${FIXgsm}/global_slmask.t${JCAP}.grb
#     FNGLAC        Input glacier climatology GRIB file
#                   defaults to ${FIXgsm}/global_glacier.2x2.grb
#     FNMXIC        Input maximum sea ice climatology GRIB file
#                   defaults to ${FIXgsm}/global_maxice.2x2.grb
#     FNTSFC        Input SST climatology GRIB file
#                   defaults to ${FIXgsm}/global_sstclim.2x2.grb
#     FNSNOC        Input snow climatology GRIB file
#                   defaults to ${FIXgsm}/global_snoclim.1.875.grb
#     FNZORC        Input roughness climatology 
#                   defaults to sib vegtetation type-based lookup table
#                   FNVETC must be set to ${FIXgsm}/global_vegtype.1x1.grb
#     FNALBC        Input albedo climatology GRIB file
#                   defaults to ${FIXgsm}/global_albedo4.1x1.grb
#     FNAISC        Input sea ice climatology GRIB file
#                   defaults to ${FIXgsm}/global_iceclim.2x2.grb
#     FNTG3C        Input deep soil temperature climatology GRIB file
#                   defaults to ${FIXgsm}/global_tg3clim.2.6x1.5.grb
#     FNVEGC        Input vegetation fraction climatology GRIB file
#                   defaults to ${FIXgsm}/global_vegfrac.1x1.grb
#     FNVETC        Input vegetation type climatology GRIB file
#                   defaults to ${FIXgsm}/global_vegtype.1x1.grb
#     FNSOTC        Input soil type climatology GRIB file
#                   defaults to ${FIXgsm}/global_soiltype.1x1.grb
#     FNSMCC        Input soil moisture climatology GRIB file
#                   defaults to ${FIXgsm}/global_soilmgldas.t$JCAP.$LONB.$LATB.grb
#     FNVMNC        Input min veg frac climatology GRIB file
#                   defaults to ${FIXgsm}/global_shdmin.0.144x0.144.grb
#     FNVMXC        Input max veg frac climatology GRIB file
#                   defaults to ${FIXgsm}/global_shdmax.0.144x0.144.grb
#     FNSLPC        Input slope type climatology GRIB file
#                   defaults to ${FIXgsm}/global_slope.1x1.grb
#     FNABSC        Input max snow albedo climatology GRIB file
#                   defaults to ${FIXgsm}/global_snoalb.1x1.grb
#     FNMSKH        Input high resolution land mask GRIB file
#                   defaults to ${FIXgsm}/seaice_newland.grb
#     CLIMO_
#     FIELDS_OPT    1-Climo/static fields (albedo, soil type, greenness, etc.)
#                     interpolated from input grid.
#                   2-Vegetation, slope and soil type interpolated from input
#                     grid.  All other climo/static fields from sfccycle.
#                   3-All climo/static fields from sfccycle.
#                   defaults to '3'
#     LANDICE_OPT   1-Input no landice => output landice
#                   2-Input landice => output landice.
#                   3-Input no landice => output no landice
#                   4-Input landice => output no landice  
#                   5-Output landice regardless of input  
#     LSOIL         2-Output file with 2 soil layers
#                   4-Output file with 4 soil layers
#                   0-Default, number of soil layers same as input file
#     IVSSFC        Version number of surface restart file
#                   0-Default, same as input file.
#     LONSPERLAT    New lonsperlat ("NULL" to use from input surface file)
#                   defaults to ${FIXgsm}/global_lonsperlat.t${JCAP}.txt
#     CHGRESEXEC    Change resolution executable
#                   defaults to ${EXECgsm}/global_chgres
#     INISCRIPT     Preprocessing script
#                   defaults to none
#     LOGSCRIPT     Log posting script
#                   defaults to none
#     ERRSCRIPT     Error processing script
#                   defaults to 'eval [[ $err = 0 ]]'
#     ENDSCRIPT     Postprocessing script
#                   defaults to none
#     CHGRESVARS    Other namelist inputs to the change resolution executable
#                   such as LONBI,LATBI,IGEN,MGG,JC,MQUICK
#                   defaults to none set
#     NTHREADS      Number of threads
#                   defaults to 1
#     NTHSTACK      Size of stack per thread
#                   defaults to 64000000
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
#     programs   : $CHGRESEXEC
#
#     input data : $1 or $SIGINP
#                  $2 or $SFCINP
#                  $12 or $NSTINP
#                  $OROGRAPHY
#                  $OROGRAPHY_UF
#                  $SIGLEVEL
#                  $O3CLIM
#                  $SLMASK
#                  $LONSPERLAT
#
#     output data: $3 or $SIGOUT
#                  $4 or $SFCOUT
#                  $9 or $GFSOUT
#                  $13 or $NSTOUT
#                  $14 or $SFNOUT
#                  $PGMOUT
#                  $PGMERR
#
#     scratch    : ${DATA}/chgres.inp.sig
#                  ${DATA}/chgres.inp.orogb
#                  ${DATA}/chgres.inp.siglevel
#                  ${DATA}/chgres.inp.sfc
#                  ${DATA}/chgres.inp.nst
#                  ${DATA}/chgres.inp.slmgb
#                  ${DATA}/chgres.out.sig
#                  ${DATA}/chgres.out.sfc
#                  ${DATA}/chgres.out.nst
#                  ${DATA}/fort.35
#                  ${DATA}/fort.81
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

export VERBOSE=${VERBOSE:-"NO"}
if [[ "$VERBOSE" = "YES" ]] ; then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi

#  Command line arguments.
export APRUNC=${APRUNC:-""}
export SIGINP=${1:-${SIGINP:-NULL}}
export SFCINP=${2:-${SFCINP:-NULL}}
export NSTINP=${12:-${NSTINP:-NULL}}
export OUTTYP=2  ###${10:-${OUTTYP:-2}}
export SIGOUT=${3:-${SIGOUT:-sigout}}
export GFSOUT=${9:-${GFSOUT:-gfsout}}
export SFCOUT=${4:-${SFCOUT:-sfcout}}
export NSTOUT=${13:-${NSTOUT:-nstout}}
export SFNOUT=${14:-${SFNOUT:-sfnout}}
export JCAP=${5:-${JCAP:?}}
export LEVS=${6:-${LEVS:?}}
export LONB=${7:-${LONB:?}}
export LATB=${8:-${LATB:?}}
export IDRT=${11:-${IDRT:-4}}

#  Directories.
export BASEDIR=${BASEDIR:-$HOMEcfs}                
export HOMEglobal=${HOMEglobal:-$BASEDIR/global_shared_cfs} 
export EXECgsm=${EXECgsm:-$HOMEglobal/exec}
export FIXgsm=${FIXgsm:-$HOMEcfs/fix/cfs_fix_am}   
export DATA=${DATA:-$(pwd)}
#  Filenames.
export XC=${XC}
export CHGRESEXEC=${CHGRESEXEC:-${EXECgsm}/global_chgres$XC}
export OROGRAPHY=${OROGRAPHY:-${FIXgsm}/global_orography.t${JCAP}.${LONB}.${LATB}.grb}
export OROGRAPHY_UF=${OROGRAPHY_UF:-${FIXgsm}/global_orography_uf.t${JCAP}.$LONB.$LATB.grb}
export SIGLEVEL=${FIXgsm}/global_hyblev.l${LEVS}.txt
export O3CLIM=${O3CLIM:-${FIXgsm}/global_o3clim.txt}
export SLMASK=${SLMASK:-${FIXgsm}/global_slmask.t${JCAP}.${LONB}.${LATB}.grb}
export FNGLAC=${FNGLAC:-${FIXgsm}/global_glacier.2x2.grb}
export FNMXIC=${FNMXIC:-${FIXgsm}/global_maxice.2x2.grb}
export FNTSFC=${FNTSFC:-${FIXgsm}/cfs_oi2sst1x1monclim19822001.grb}
export FNSNOC=${FNSNOC:-${FIXgsm}/global_snoclim.1.875.grb}
###### export FNZORC=${FNZORC:-sib}
export FNZORC=${FNZORC:-${FIXGLOBAL}/global_zorclim.1x1.grb}
export FNALBC=${FNALBC:-${FIXgsm}/global_albedo4.1x1.grb}
export FNAISC=${FNAISC:-${FIXgsm}/cfs_ice1x1monclim19822001.grb}
export FNTG3C=${FNTG3C:-${FIXgsm}/global_tg3clim.2.6x1.5.grb}
export FNVEGC=${FNVEGC:-${FIXgsm}/global_vegfrac.0.144.decpercent.grb}
export FNVETC=${FNVETC:-${FIXgsm}/global_vegtype.1x1.grb}
export FNSOTC=${FNSOTC:-${FIXgsm}/global_soiltype.1x1.grb}
##export FNSMCC=${FNSMCC:-${FIXgsm}/global_soilmgldas.t${JCAP}.${LONB}.${LATB}.grb}
export FNSMCC=${FNSMCC:-${FIXgsm}/global_soilmcpc.1x1.grb} 
export FNVMNC=${FNVMNC:-${FIXgsm}/global_shdmin.0.144x0.144.grb}
export FNVMXC=${FNVMXC:-${FIXgsm}/global_shdmax.0.144x0.144.grb}
export FNSLPC=${FNSLPC:-${FIXgsm}/global_slope.1x1.grb}
export FNABSC=${FNABSC:-${FIXgsm}/global_snoalb.1x1.grb}
export FNMSKH=${FNMSKH:-${FIXgsm}/seaice_newland.grb}
export LANDICE_OPT=${LANDICE_OPT:-2}
export CLIMO_FIELDS_OPT=${CLIMO_FIELDS_OPT:-3}
export LONSPERLAT=${LONSPERLAT:-${FIXgsm}/global_lonsperlat.t${JCAP}.${LONB}.${LATB}.txt}
export INISCRIPT=${INISCRIPT}
export ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}
export LOGSCRIPT=${LOGSCRIPT}
export ENDSCRIPT=${ENDSCRIPT}
#  Other variables.
export NTRAC=${NTRAC:-0}
export IDVC=${IDVC:-0}
export IDSL=${IDSL:-0}
export LSOIL=${LSOIL:-0}
export IVSSFC=${IVSSFC:-0}
export CHGRESVARS=${CHGRESVARS}
export NTHREADS=${NTHREADS:-1}
export NTHSTACK=${NTHSTACK:-1024000000}
export XLSMPOPTS=${XLSMPOPTS:-"parthds=$NTHREADS:stack=$NTHSTACK"}
export KMP_STACKSIZE=${KMP_STACKSIZE:-$NTHSTACK}
export PGMOUT=${PGMOUT:-${pgmout:-'&1'}}
export PGMERR=${PGMERR:-${pgmerr:-'&2'}}

export REDOUT=${REDOUT:-'1>'}
export REDERR=${REDERR:-'2>'}

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
#  Change resolution
#export XLSMPOPTS="parthds=$NTHREADS:stack=$NTHSTACK"
export PGM=$CHGRESEXEC
export pgm=$PGM
$LOGSCRIPT
rm -f NULL
ln -sf $SIGINP        chgres.inp.sig
ln -sf $OROGRAPHY     chgres.inp.orogb
ln -sf $OROGRAPHY_UF  chgres.inp.orogb_uf
ln -sf $SIGLEVEL      chgres.inp.siglevel
ln -sf $O3CLIM        chgres.inp.o3clim
ln -sf $SFCINP        chgres.inp.sfc
ln -sf $SLMASK        chgres.inp.slmgb
ln -sf $LONSPERLAT    chgres.inp.lonsperlat
ln -sf $SIGOUT        chgres.out.sig
ln -sf $GFSOUT        chgres.out.grd
ln -sf $SFCOUT        chgres.out.sfc
ln -sf $NSTOUT        chgres.out.nst
ln -sf $SFNOUT        chgres.out.sfn

if [[ $LANDICE_OPT = 3 || $LANDICE_OPT = 4 ]]
then
 LANDICE=.false.
else
 LANDICE=.true.
fi

cat << EOF > fort.35
 &NAMSFC
  FNGLAC='${FNGLAC}'
  FNMXIC='${FNMXIC}'
  FNTSFC='${FNTSFC}'
  FNSNOC='${FNSNOC}'
  FNZORC='${FNZORC}'
  FNALBC='${FNALBC}'
  FNAISC='${FNAISC}'
  FNTG3C='${FNTG3C}'
  FNVEGC='${FNVEGC}'
  FNVETC='${FNVETC}'
  FNSOTC='${FNSOTC}'
  FNSMCC='${FNSMCC}'
  FNVMNC='${FNVMNC}'
  FNVMXC='${FNVMXC}'
  FNSLPC='${FNSLPC}'
  FNABSC='${FNABSC}'
  FNMSKH='${FNMSKH}'
  FNTSFA=''
  FNACNA=''
  FNSNOA=''
  LDEBUG=.false.
  LANDICE=$LANDICE
/
EOF

cat << EOF > fort.81
 &soil_parameters
  soil_src_input = "zobler"
  smclow_input  = 0.5
  smchigh_input = 6.0
  smcmax_input= 0.421, 0.464, 0.468, 0.434, 0.406, 0.465,
                0.404, 0.439, 0.421
  beta_input  =   4.26,  8.72, 11.55,  4.74, 10.73,  8.17,
                  6.77,  5.25,  4.26
  psis_input  =   0.040, 0.620, 0.470, 0.140, 0.100, 0.260,
                  0.140, 0.360, 0.040
  satdk_input = 1.41e-5, 0.20e-5, 0.10e-5, 0.52e-5, 0.72e-5,
                0.25e-5, 0.45e-5, 0.34e-5, 1.41e-5
  soil_src_output = "zobler"
  smclow_output  = 0.5
  smchigh_output = 6.0
  smcmax_output= 0.421, 0.464, 0.468, 0.434, 0.406, 0.465,
                 0.404, 0.439, 0.421
  beta_output  =  4.26,  8.72, 11.55,  4.74, 10.73,  8.17,
                  6.77,  5.25,  4.26
  psis_output  =  0.040, 0.620, 0.470, 0.140, 0.100, 0.260,
                  0.140, 0.360, 0.040
  satdk_output = 1.41e-5, 0.20e-5, 0.10e-5, 0.52e-5, 0.72e-5,
                 0.25e-5, 0.45e-5, 0.34e-5, 1.41e-5
 /
 &veg_parameters
  veg_src_input = "sib"
  veg_src_output = "sib"
  salp_output= -999.
  snup_output= -999.
 /
 &options
  CLIMO_FIELDS_OPT=${CLIMO_FIELDS_OPT}
  LANDICE_OPT=${LANDICE_OPT}
 /
EOF

export OMP_NUM_THREADS=${OMP_NUM_THREADS_CH:-${CHGRESTHREAD:-1}}

 eval $APRUNC $CHGRESEXEC <<EOF $REDOUT$PGMOUT $REDERR$PGMERR
  &NAMCHG JCAP=$JCAP, LEVS=$LEVS, LONB=$LONB, LATB=$LATB,
           NTRAC=$NTRAC, IDVC=3    , IDSL=$IDSL,
           LSOIL=$LSOIL, IVSSFC=$IVSSFC, OUTTYP=$OUTTYP, IDRT=$IDRT, $CHGRESVARS
 /
EOF

export ERR=$?
export err=$ERR
$ERRSCRIPT||exit 2
rm -f NULL
rm -f chgres.inp.sig chgres.inp.orogb chgres.inp.siglevel chgres.inp.o3clim
rm -f chgres.inp.sfc chgres.inp.nst chgres.inp.slmgb chgres.inp.lonsperlat
rm -f chgres.out.sig chgres.out.sfc chgres.out.nst chgres.out.grd fort.35 fort.81
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
