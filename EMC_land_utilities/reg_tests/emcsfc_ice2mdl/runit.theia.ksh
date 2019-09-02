#!/bin/ksh --login

#------------------------------------------------------------
# Run ice2mdl on Theia.
#
# To run, type 'qsub $script'
#------------------------------------------------------------

#PBS -l procs=1
#PBS -l vmem=2G
#PBS -l walltime=0:03:00
#PBS -A fv3-cpu
#PBS -N ice2mdl
#PBS -o log
#PBS -e log

set -x

WORK="/scratch3/NCEPDEV/stmp1/$LOGNAME/ice2mdl"
rm -fr $WORK
mkdir -p $WORK
cd $WORK

rundir=$PBS_O_WORKDIR
execdir=${rundir}/../../exec

cp ${execdir}/emcsfc_ice2mdl     ./

ln -fs ${rundir}/config.theia.nml   ./fort.41

./emcsfc_ice2mdl

exit 0
