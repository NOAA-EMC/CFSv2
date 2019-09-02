#!/bin/ksh
 
#-----------------------------------------------------
# Run the fire2mdl program on Theia.
#
# Invoke this script as follows:
# "qsub  runit.theia.ksh"
#-----------------------------------------------------

#PBS -l procs=1
#PBS -l vmem=2G
#PBS -l walltime=0:05:00
#PBS -A fv3-cpu
#PBS -N fire2mdl
#PBS -o fire2mdl.log
#PBS -e fire2mdl.log

set -x

rundir=$PBS_O_WORKDIR
execdir=${rundir}/../../exec

WORK="/scratch3/NCEPDEV/stmp1/$LOGNAME/fire2mdl"
rm -fr $WORK
mkdir -p $WORK
cd $WORK

EXE=${execdir}/emcsfc_fire2mdl

#------------------------------------------------------------
# input/output files are set in the namelist. 
#------------------------------------------------------------

ln -fs ${rundir}/config.theia.nml   ./fort.41

#------------------------------------------------------------
# Run executable.
#------------------------------------------------------------

$EXE

exit 0
