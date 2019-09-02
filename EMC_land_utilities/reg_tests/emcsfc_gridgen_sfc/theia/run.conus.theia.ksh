#!/bin/ksh --login
 
#------------------------------------------------------------
# Run gridgen_sfc on Theia for a nam conus nest.
#
# To run this script do: "qsub run.conus.theia.ksh"
#------------------------------------------------------------

#PBS -l procs=1
#PBS -l walltime=0:05:00
#PBS -l vmem=4000M
#PBS -A fv3-cpu
#PBS -N gridgen_sfc
#PBS -o ./conus.log
#PBS -e ./conus.log

set -x

EXEC_DIR=${PBS_O_WORKDIR}/../../../exec
EXEC=emcsfc_gridgen_sfc

WORK="/scratch3/NCEPDEV/stmp1/$LOGNAME/gridgen_sfc.conus"
rm -fr $WORK
mkdir -p $WORK
cd $WORK

cp ${EXEC_DIR}/${EXEC}     ./

#------------------------------------------------------------
# input/output files are set in the namelist. 
#------------------------------------------------------------

ln -fs ${PBS_O_WORKDIR}/conus.theia.nml   ./fort.41

#------------------------------------------------------------
# Run executable.
#------------------------------------------------------------

np=$PBS_NP

module purge 
module load intel/15.1.133
module load impi/5.1.2.150

mpirun -np $np ./$EXEC

exit 0
