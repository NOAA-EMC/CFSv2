#!/bin/ksh --login

#-----------------------------------------------------
# invoke this script as follows:
# 'qsub accum.12km.theia.ksh'
#-----------------------------------------------------

#PBS -l procs=1
#PBS -l walltime=0:05:00
#PBS -l vmem=1000M
#PBS -A fv3-cpu
#PBS -N accum_12km_file
#PBS -o ./12km.log
#PBS -e ./12km.log

set -x

WORK=/scratch3/NCEPDEV/stmp1/$LOGNAME/fire_12km
rm -fr $WORK
mkdir -p $WORK
cd $WORK

# first do a 30-day accumulation
cat > ./fort.41 << !
 &setup
   start_year=2014
   start_month=7
   start_day=10
   start_hour=0
   end_year=2014
   end_month=8
   end_day=10
   end_hour=0
   file_resolution_in_km=12
   input_file_dir="/scratch4/NCEPDEV/da/noscrub/George.Gayno/fire_data"
   output_file="./accum.fire.30day.12km.grib2"
 /
!

MAIN_DIR=$PBS_O_WORKDIR/../../exec

np=$PBS_NP

module purge
module load intel/14.0.2
module load impi/4.1.3.048

mpirun -np $np ${MAIN_DIR}/emcsfc_accum_firedata

rm -f ./fort.41

# then do a 48 hour accumulation.
cat > ./fort.41 << !
 &setup
   start_year=2014
   start_month=8
   start_day=8
   start_hour=0
   end_year=2014
   end_month=8
   end_day=10
   end_hour=0
   input_file_dir="/scratch4/NCEPDEV/da/noscrub/George.Gayno/fire_data"
   file_resolution_in_km=12
   output_file="./accum.fire.2day.12km.grib2"
 /
!

mpirun -np $np ${MAIN_DIR}/emcsfc_accum_firedata

rm -f ./fort.41

exit 0
