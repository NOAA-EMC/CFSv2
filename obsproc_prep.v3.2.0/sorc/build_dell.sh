#!/usr/bin/env bash 
set -x
set -e    # fail if an error is hit so that errors do not go unnoticed

set +x
module purge
module load EnvVars/1.0.2
module load ips/18.0.1.163  impi/18.0.1
module load ESMF/4_0_0rp2
module load bacio/2.0.2
module load nemsio/2.2.3
module load sp/2.0.2
module load NetCDF/3.6.3
module load bufr/11.3.0
module load ip/3.0.1
module load sfcio/1.0.0
module load sigio/2.0.1
module load landsfcutil/2.1.0
module load gfsio/1.1.0
module load crtm/2.2.5
module load nemsiogfs/2.0.1
module load grib_util/1.1.0
module load prod_util/1.1.0
module load w3nco/2.0.6
module load w3emc/2.3.0
module list
set -x

if [ $# -eq 0 ]; then
  dir_list=*.fd
else
  dir_list=$*
fi

for sdir in $dir_list; do
 cd $sdir
 make clean; make; make install; make clean
 ls -l
 cd ..
done


