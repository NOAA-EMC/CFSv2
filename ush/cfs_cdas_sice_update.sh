#!/bin/bash
#=======================================================================
#  regrid_fice : interp 2-D sea-ice data
#  Original Author : Xingren.Wu@noaa.gov
#  Modified to Korn shell for CFS - S. Moorthi
#
set -ux
export cfss=${cfss:-"/cfs"}
export cfsp=${cfsp:-"cfs_"}
export cfsd=${cfsd:-"cfs_cdas_"}
export HICE_CLM=${HICE_CLM:-$HOMEcfs/fix/${cfsp}fix_om/hice_clm.nc}
export GRIDSPEC=${GRIDSPEC:-$HOMEcfs/fix/${cfsp}fix_om/grid_spec.nc}

#setup the compute node command

# find the number of procs

export APRUN="mpiexec -n $LSB_DJOB_NUMPROC"

export name=regrid_fice
export cdate=$1
export icefile=$2
export workdir=${3:-${DATA:-$(pwd)}}            # should be from outside
export sstfile=${4:-$sstfile}

export regrid_exec=${regrid_exec:-$EXECcfs/${cfsd}regrid_2d}
export readfi_exec=${readfi_exec:-$EXECcfs/${cfsd}read_fice}
export readsst_exec=${readsst_exec:-$EXECcfs/${cfsd}read_sst}
export sice_rstrt_exec=${sice_rstrt_exec:-$EXECcfs/${cfsd}sice_rstrt}


ln -sf $GRIDSPEC        grid_spec.nc

yyyy=$(echo $cdate | cut -c1-4)
mm=$(echo $cdate | cut -c5-6)
dd=$(echo $cdate | cut -c7-8)
hh=$(echo $cdate | cut -c9-10)

if [ ! -d $workdir ] ; then exit ; fi
cd $workdir
ln -sf $HICE_CLM        hice_clm.nc


ln -f $icefile icegrb.gdas.$cdate
  $readfi_exec << EOFr
  $yyyy $mm $dd $hh
  $yyyy $mm $dd
EOFr

# --- set up namelist for regrid
cat >input.nml << EOFice
    &regrid_2d_nml
       src_file = 'fice.nc'
       numfields = 1
       src_field_name = 'icecsfc'
       dest_grid = 'grid_spec.nc'
       dest_file = '$name.nc'
       dest_grid_type = 'T'
       vector_field = .false.
       apply_dest_mask=.false.
       /
EOFice
ln -f $sstfile sstgrb.gdas.$cdate
  $readsst_exec << EOFr
  $yyyy $mm $dd $hh
  $yyyy $mm $dd
EOFr

$APRUN $regrid_exec >fms.out
mv fms.out $name.fice.out

# --- set up namelist for regrid
cat >input.nml << EOFice
    &regrid_2d_nml
       src_file = 'sst.nc'
       numfields = 1
       src_field_name = 'tmpsfc'
       dest_grid = 'grid_spec.nc'
       dest_file = 'regrid_sst.nc'
       dest_grid_type = 'T'
       vector_field = .false.
       apply_dest_mask=.false.
       /
EOFice

  $APRUN $regrid_exec >fms.out
  mv fms.out regrid.sst.out

  cat >ice_init_grid_nml << EOFice_init_grid
    &GRID_SETTINGS
       im=$im_mom4
       jm=$jm_mom4
       ko=40
       kz1=6
       kz2=5
       kz3=1
       lm=1
       /
EOFice_init_grid

# --- set up namelist for restart
  cat >ice_init_nml << EOFice_init
    &ICE_SETTINGS
       fcycle=6.0, fhiclm=50.0, hice_clm=.true., hice_anl=.true.,\
       sst_anl=.true., hsno_gdas=.false., melt_pond=.true. /
EOFice_init

  $sice_rstrt_exec << EOFrst
  $yyyy $mm $dd
EOFrst


