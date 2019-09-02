#!/bin/ksh --login

#------------------------------------------------------------
# Run the sst2mdl program on Theia.
#
# To run, type 'qsub $script'
#------------------------------------------------------------

#PBS -l procs=1
#PBS -l vmem=2000M
#PBS -l walltime=0:03:00
#PBS -A fv3-cpu
#PBS -N sst2mdl
#PBS -o sst.log
#PBS -e sst.log

set -x

export OMP_NUM_THREADS=1

WORK="/scratch3/NCEPDEV/stmp1/$LOGNAME/sst2mdl"
rm -fr $WORK
mkdir -p $WORK
cd $WORK

rundir=$PBS_O_WORKDIR
execdir=${rundir}/../../exec

cp ${execdir}/emcsfc_sst2mdl     ./

ln -fs ${rundir}/config.theia.nml   ./fort.41

./emcsfc_sst2mdl

exit 0
