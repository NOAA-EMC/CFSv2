#!/bin/tcsh 
#=======================================================================
#         Contact : Zhi Liang   email : Zhi.Liang@noaa.gov
#
#  This runscritp can can edit the topography of input grid_spec file "orig_grid" 
#  according to the ascii input file "grid_edits". Then it will output the 
#  new grid_spec file "mod_grid". 
#  This preprocessing program was tested on the sgi origin3000 system at GFDL.
#  In order to run on other system, some changes may be needed.
#=======================================================================
  set echo
# set DEBUG                                            # uncomment this to debug your run with totalview
  set platform     = ibm                               # A unique identifier for your platform
  set name         = "edit_grid"                       # name of the grid file will be generated
  set root         = $cwd:h #:h:h:h                      # The directory you created when you checkout
  set root         = $root:h
  set root         = $root:h
  set root         = $root:h
  set tooldir      = $cwd                              # directory of the tool
  set sharedir     = $root/src/shared                  # directory of the shared code.
  set includedir   = $root/include                     # fms include directory   
  set workdir      = $tooldir/workdir                  # where the tool is run and output is produced
  set executable   = $tooldir/exec/edit_grid.exe       # executable created after compilation
  set mkmfTemplate = $root/bin/mkmf.template.$platform # path to template for your platform
  if ( $?DEBUG ) then
    set executable    = $tooldir/debug/edit_grid.exe 
    set mkmfTemplate  = $root/bin/mkmf.debugtemplate.$platform         
  endif
  set mkmf         = $root/bin/mkmf                    # path to executable mkmf
  set cppDefs      = ( "-Duse_netCDF -Duse_libMPI" )                # list of cpp #defines to be passed to the source files

# grid to be edited and grid_edits test file
  set grid_edits = grid_edits.txt                      # text file to specify the edit region and new depth.
  set orig_grid  = $cwd/ocean_grid.nc         

# list the source code
  
  set CORE      = " $tooldir/{edit_grid.F90,topog.f90,grids_type.f90,grids_util.f90} "
  set UTILITIES    = "$sharedir/{axis_utils,constants,fms,mpp,horiz_interp,platform,memutils}"
  set UTILITIES    = " $UTILITIES $sharedir/mpp/include $includedir " 
  set srclist   = ( $CORE $UTILITIES )

# the following compiler choice are for GFDL user only. If the following command


# compile the model code and create executable

#--------------------------------------------------------------------------------------------------------
# setup directory structure
  if ( ! -d $workdir )         mkdir $workdir

  cd $workdir

# get executable  
  cp $executable $executable:t

# if the grid_edits file does not exist, create one here.
  if( ! -f $grid_edits) then
     cat >$grid_edits <<EOF
258:260,352:353,0
252:253,346:347,0
426:427,356:357,0
429:430,80:81,0
431:431,81:81,0
EOF
  endif

# --- set up namelist  

  cat >input.nml <<!
    &edit_grid_nml
       orig_grid   = '$orig_grid' 
       mod_grid    = '$name.nc'
       grid_edits  = '$grid_edits'
       /
!

  if ( $?DEBUG ) then
     totalview $executable:t > fms.out
  else
     $executable:t >fms.out
  endif
  cat fms.out

# rename ascii output file
  mv fms.out $name.fms.out
  mv logfile.out $name.logfile.out

  unset echo
