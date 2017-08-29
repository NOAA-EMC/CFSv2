#!/bin/tcsh 
#=======================================================================
#      regrid_2d : interp 2-D data
#         Contact : Zhi Liang   email : Zhi.Liang@noaa.gov
#
#  This runscritp can be used to regrid 2-d lat-lon gridded data to logically
#  rectangular grid described by grid descriptor file $dest_grid.  If two
#  fields are specified for regridding, it is assumed that they represent
#  vector components. Rotation to the local grid direction on the target
#  grid will be performed.
#
#  This preprocessing program was tested on the sgi origin3000 system at GFDL.
#  In order to run on other system, some changes may be needed.

  set echo
# set DEBUG                                                 # uncomment this to debug your run with totalview
  set platform     = ibm                                    # A unique identifier for your platform
  set name         = "regrid_2d"                            # name of the data file will be generated
  set npes         = 1                                      # number of processors
  set root         = $cwd:h #:h:h                             # The directory you created when you checkout
  set root         = $root:h
  set root         = $root:h
  set tooldir      = $cwd                                   # directory of the tool
  set sharedir     = $root/src/shared                       # directory of the shared code.
  set includedir   = $root/include                          # fms include directory
  set workdir      = $tooldir/workdir                       # where the tool is run and output is produced
  set executable   = $tooldir/exec/regrid_2d.exe            # executable created after compilation
  set mkmfTemplate = $root/bin/mkmf.template.$platform      # path to template for your platform
  if ( $?DEBUG ) then
    set executable    = $tooldir/debug/regrid_2d.exe 
    set mkmfTemplate  = $root/bin/mkmf.debugtemplate.$platform         
  endif
  set mkmf         = $root/bin/mkmf                         # path to executable mkmf
  set cppDefs      = ( "-Duse_netCDF -Duse_libMPI" )        # list of cpp #defines to be passed to the source files
#  if($npes == 1) set cppDefs  = ( "-Duse_netCDF" )

  set FORCINGdir = /emc4/wx20wn/INPUT_03
# input data file and destination grid
##  set src_file        = /archive/fms/mom4/input_data/hellerman.nc   # source data file
##  set dest_grid       = $tooldir:h/generate_grids/ocean/workdir/ocean_grid.nc  # destination grid
#   set src_file     = $cwd/Reynolds_SST_2000.nc
#   set src_file     = $cwd/DLW.nc
   set src_file     = $cwd/CDAS2_Ps.nc 
   set dest_grid    = $cwd/ocean_grid.nc

# list the source code
  set CORE      = "$tooldir/{regrid_2d.f90}"
  set UTILITIES = "$sharedir/{axis_utils,constants,fms,mpp,horiz_interp,platform,memutils}"
  set UTILITIES    = " $UTILITIES $sharedir/mpp/include $includedir " 
  set srclist   = ( $CORE $UTILITIES )



#--------------------------------------------------------------------------------------------------------
# setup directory structure
  if ( ! -d $workdir )         mkdir $workdir

  cd $workdir

# get executable  
  cp $executable $executable:t

# --- set up namelist

    cat >input.nml <<!
    &regrid_2d_nml
       src_file = '$src_file'
       numfields = 1
       src_field_name = 'pressfc'
       dest_grid = '$dest_grid'
       dest_file = '$name.nc'
       dest_grid_type = 'T'
       vector_field = .false.
       apply_dest_mask=.true.
       /
!

#  run the executable
  if ( $?DEBUG ) then
     if($npes == 1) then
         totalview $executable:t > fms.out
     else
         totalview mpirun -a -np $npes $executable:t > fms.out
     endif
  else
     if($npes == 1) then
        $executable:t >fms.out
     else
        mpirun -np $npes $executable:t >fms.out
     endif
  endif
  
  cat fms.out
       
#---------------------------------------------------------------------
# rename the ascii output file
  mv fms.out $name.fms.out
  mv logfile.out $name.logfile.out

  unset echo
