#!/bin/bash
set -eua

module purge
module load envvar/1.0
module load PrgEnv-intel/8.1.0
module load craype/2.7.8
module load intel/19.1.3.304
module load cray-mpich/8.1.7

module use -a /lfs/h2/emc/global/noscrub/Jack.Woollen/modules 
module load esmf_4_0_0rp2

module load libjpeg/9c
module load bacio/2.4.1
module load sp/2.3.3
module load w3emc/2.7.3
module load w3nco/2.4.1
module load bufr/11.4.0
module load ip/3.3.3
module load landsfcutil/2.4.1
module load g2/3.4.1
module load g2tmpl/1.9.1
module load libpng/1.6.37
module load libxmlparse/2.0.0
module load jasper/2.0.25 
module load zlib/1.2.11
module load grib_util/1.2.2
module load netcdf/3.6.3     

module load sigio/2.3.2
module load sfcio/1.4.1
module load gfsio/1.4.1

module list

SIGIO_LIB4=$SIGIO_LIB 
SIGIO_INC4=$SIGIO_INC
SFCIO_LIB4=$SFCIO_LIB  
SFCIO_INC4=$SFCIO_INC 
GFSIO_LIB4=$GFSIO_LIB  
GFSIO_INC4=$GFSIO_INC  

