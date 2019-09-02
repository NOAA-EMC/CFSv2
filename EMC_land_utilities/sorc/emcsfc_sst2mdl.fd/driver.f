 program driver
!$$$  main program documentation block
! 
! $Revision$
!
! main program: sst2mdl
!   prgmmr: gayno            ORG: NP2                DATE: 2005-05-20
!
! abstract: Creates an sst analysis for a regional e-grid or b-grid;
!   or global gaussian grid.  Then, outputs the result in grib 1 
!   or grib 2 format
!
! program history log:
!   2005-05-20  gayno     initial version
!   2007-10-15  gayno     added nam b-grids. improved thinning
!                         on gfs grid.
!   2014-10-27  gayno     option to read global input sst data
!                         in grib 1 or grib 2 format.  option
!                         to read model grid information in 
!                         grib 1 or grib 2.  option to output
!                         final sst analysis in grib 1 or grib 2
!                         format.
!   2015-05-27  gayno     added cressman blend of global rtg
!                         and flake data for small isolated 
!                         model lakes.
!
! files:
!   input files: to define model grid and for runtime 
!     configuration (all are required).
!     - fort.41 configuration namelist
!     - latitudes on model grid (grib 1 or grib 2)
!     - longitudes on model grid (grib 1 or grib 2)
!     - model land/sea mask (grib 1 or grib 2)
!
!   input sst data; see remarks section on how to select.
!     - global MMAB rtg sst data; required (grib 1 or grib 2)
!     - global MMAB rtg sst land/sea mask (or bitmap); optional (grib 2)
!     - noaa/glerl 14km north american data; optional (binary)
!     - flake climatology; optional (grib 2)
!     
!   output files: 
!     - SST on the model grid (grib1 or grib2).
!
! remarks: 
!   Input data is selected via the fort.81 namelist thru the following
!   namelist character strings:
!
!     - input_src_file - global mmab rtg data
!     - input_src_bitmap_file - global mmab rtg land mask (or bitmap) data
!     - input_src14km_file - NOAA/GLERL regional sst data (n america)
!     - input_flake_file - flake climo sst data
!
!   To select, set variable to the path/name of the file.
!   To not select, set variable to a zero length string.
!
!   User must select a global mmab sst dataset.  Program has only
!   been tested with mmab rtg 1/2 and 1/12-degree data.
!   These data have sst values everywhere, including land.
!
!   The global datasets have a separate land mask that may
!   be optionally used.  When chosen, only sst values at 
!   'water' and 'coast' points will be used.  Land points
!   are bitmapped out.
!
!   A flake climatology dataset may be used to set sst
!   at small model lakes not resolved by the global dataset.  
!   This can happen when the global dataset land mask
!   is used to filter out 'land' points.  The flake dataset
!   is optional. When not chosen, undefined model lakes
!   are assigned a nomimal value.  If flake is chosen,
!   then a cressman analysis is performed at these undefined
!   lakes.  The analysis considers surrounding model points
!   that were resolved by the global dataset and the flake
!   value, which is given a weight of one. At very isolated
!   and undefined model points, the result of the cressman is
!   the pure flake value. 
!
!   A 14km north america sst dataset from NOAA/GLERL may be 
!   optionally used.  When selected, the model points within
!   the Great Lakes will be set to the GLERL value, regardless
!   of the global mmab or cresman/flake value.
!
!   When the 'climo_4_lakes' namelist variable is set to 'true',
!   a climatological value will be used for the following model
!   lakes: great salt lake, salton sea, lake champlain,
!   fort peck resv.  
! 
! condition codes (non-zero is fatal):
!     cond =   0 - successful run
!             40 - bad file open during grib edition check
!             47 - unrecognized grid type in grib2 init step
!             48 - error writing output sst grib2 file
!             49 - error opening output sst grib2 file
!             50 - error converting to grib2 grid desc. templ.
!             54 - error in ipolates interpolation routine during
!                  interpolation of flake data
!             55 - error in ipolates interpolation routine during
!                  interpolation of global mmab data.
!             58 - error writing output sst grib1 file
!             59 - error opening output sst grib1 file
!             60 - bad open on configuration namelist
!             61 - bad read on configuration namelist
!             62 - bad open of global mmab sst grib file
!             63 - bad read of global mmab sst grib header
!             64 - global mmab sst data not on lat/lon grid
!             65 - bad degrib of global mmab sst grib1 data
!             70 - global mmab sst data not grib1 or grib2
!             71 - bad degrib of global mmab sst grib2 data
!             72 - bad open of global mmab land/sea mask (bitmap)
!             73 - bad degrib of global mmab land/sea mask (bitmap)
!             74 - global mmab sst and land/mask (bitmap) data on different grids
!             76 - unknown gfs gaussain model grid
!             79 - unrecognized model grid type
!             80 - bad open on model latitude grib file
!             81 - bad read on model latitude grib1 header
!             82 - bad degrib of model latitude grib1 file
!             83 - bad open on model longitude grib file
!             84 - bad degrib of model longitude grib1 file
!             85 - bad open of model land/sea mask grib file
!             86 - bad degrib of model land/sea mask grib1 file
!             91 - model latitude file not grib1 or grib2
!             92 - bad degrib of model latitude grib2 file
!             93 - model longitude file not grib1 or grib2
!             94 - bad degrib of model longitude grib2 file
!             95 - model land/sea mask file not grib1 or grib2
!             96 - bad degrib of model land/sea mask grib1 file
!             97 - bad open of flake climo file
!             99 - bad degrib of flake climo data
!
!$$$

 use sstdat, only                : readsst

 use model_grid, only            : read_mdl_grid_info

 use sst2mdl, only               : interp

 use program_setup, only         : read_config_nml

 implicit none

 call w3tagb('SST2MDL',2005,0139,0000,'NP2')

 print*,''
 print*,"***********************"
 print*,"*** BEGIN EXECUTION ***"
 print*,"***********************"

!-------------------------------------------------------
! get runtime configuration information.
!-------------------------------------------------------

 call read_config_nml

!-------------------------------------------------------
! read input sst data
!-------------------------------------------------------

 call readsst

!-------------------------------------------------------
! read information about the model grid to which the
! sst data will be interpolated.
!-------------------------------------------------------

 call read_mdl_grid_info

!-------------------------------------------------------
! interpolate the data to the model grid, then write
! it to a grib file.
!-------------------------------------------------------
 
 call interp

 print*,''
 print*,'****************************'
 print*,'**** NORMAL TERMINATION ****'
 print*,'****************************'

 call w3tage('SST2MDL')

 stop

 end program driver
