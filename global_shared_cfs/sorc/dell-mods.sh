set -euax

set +x
module purge
module load EnvVars/1.0.2
module load ips/18.0.1.163  impi/18.0.1
module list

module load ESMF/4_0_0rp2
module load bacio/2.0.2
module load nemsio/2.2.3
module load sp/2.0.2
module load w3emc/2.3.0
module load w3nco/2.0.6
module load NetCDF/3.6.3
module load bufr/11.2.0
module load ip/3.0.1
module load sfcio/1.0.0
module load sigio/2.0.1
module load landsfcutil/2.1.0
module load crtm/2.2.5
module load g2/3.1.0
module load libpng/1.2.59
module load jasper/1.900.1
module load zlib/1.2.11
module load gfsio/1.1.0
module load grib_util/1.1.0
module load g2tmpl/1.5.0    
module load xmlparse/2.0.0
module load landsfcutil/2.1.0

module list
