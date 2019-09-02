#!/bin/ksh --login

#---------------------------------------------------------------
# To run the grib_snowgrib program on Theia, type 'qsub $script'
#---------------------------------------------------------------

#PBS -l procs=1
#PBS -l vmem=1G
#PBS -l walltime=0:03:00
#PBS -A fv3-cpu
#PBS -N grib_snowgrib
#PBS -o log
#PBS -e log

set -x

WORK="/scratch3/NCEPDEV/stmp1/$LOGNAME/grib_snowgrib"
rm -fr $WORK
mkdir -p $WORK
cd $WORK

rundir=$PBS_O_WORKDIR
execdir=${rundir}/../../exec

cp ${execdir}/emcsfc_grib_snowgrib  ./

ln -fs /scratch4/NCEPDEV/da/noscrub/George.Gayno/grib_snowgrib/PRD.SPPROD.SNODEPH.NHMAMAP  ./fort.11
ln -fs /scratch4/NCEPDEV/da/noscrub/George.Gayno/grib_snowgrib/PRD.SPPROD.SNODEPH.SHMAMAP  ./fort.12
ln -fs /scratch4/NCEPDEV/da/noscrub/George.Gayno/grib_snowgrib/imssnow.grb  ./fort.13

# output 0.5-degree snow analysis
ln -fs snowdepth.global.grb fort.52

./emcsfc_grib_snowgrib

exit 0
