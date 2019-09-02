 program driver
!$$$  main program documentation block
!                .      .    .                                       .
! main program: ice2mdl
!   prgmmr: gayno            ORG: NP2                DATE: 2005-05-20
!
! $Revision$
!
! abstract: interpolates ims northern hemisphere or global
!   lat/lon ice concentration data to a nmm b-grid or e-grid.
!   interpolates global lat/lon ice concentration data to a 
!   global gaussian grid or a nmm grid.  gribs the result.
!
! program history log:
!   2005-05-20  gayno     initial version
!   2007-09-26  gayno     add processing of nmm b-grids.
!                         improved thinning of gfs grids.
!   2014-10-16  gayno     option to read input ice data in
!                         grib 2 format. option to read
!                         model grid information in grib 2
!                         format.  option to output the
!                         final ice analysis in grib 2.
!   2015-05-28  gayno     increase search radius when using ims
!                         data. at points with no ims,
!                         set to zero ice if south of 38N.
!                         previously, 55N was used.  
!
! files:
!
!   input files, to define model grid, and for runtime configuration:
!     - fort.41 configuration namelist
!     - latitudes on model grid (grib 1 or grib 2)
!     - longitudes on model grid (grib 1 or grib 2)
!     - model land/sea mask (grib 1 or grib 2)
!
!   input ice data files; see remarks section on how to select.
!     - 4km (96th mesh) ims northern hemis ice cover (grib1 or grib2) 
!     - 23km (16th mesh) ims northern hemis ice cover (grib1 or grib2)
!     - 23km (16th mesh) ims northern hemis land mask (ascii)
!     - ncep/mmab global 1/12-degree sea ice concentration (grib1 or grib2)
!     - ncep/mmab global 1/12-degree landmask (binary)
!     - ncep/mmab global 1/2-degree sea ice concentration (grib1 or grib2)
!     - ncep/mmab global 1/2-degree landmask (binary)
!
!   output files:
!     - ice concentration on the model grid (grib1 or grib2).
!
!   remarks:
!     For regional grids in the NH, ims OR mmab global ice data
!     may be selected.  For gfs grids, only mmab global ice data
!     may be selected.
!
!     The 23km ims and the mmab data does not have a bitmap.  So if
!     selected, you must also select its corresponding land mask. 
!
!     Data is selected via the fort.41 namelist thru the following
!     namelist variables.  To select, set variable to the path/name of
!     the file.  To not select, set variable to a zero length string.
!       - input_ims_src_file - the ims ice cover file
!       - input_ims_src_lsmask_file - the ims land mask file
!       - input_global_src_file - the mmab ice concen. file.
!       - input_global_src_lsmask_file - the mmab land mask file.
!
!   exit states:  non-zero states are fatal.
!     cond =   0 - successful run
!             40 - bad file open in routine grib_check
!             41 - ims file must be grib1 or grib 2 format
!             47 - unknown map projection in routine grib_check
!             48 - bad write of output ice analysis grib2 file
!             49 - bad open of output ice analysis grib2 file
!             50 - unknown map projection in routine gdt_to_gds
!             55 - error in ncep ipolates interpolation routine
!             56 - unknown gfs gaussian grid
!             57 - bad degrib of model landmask (grib1 version)
!             58 - bad open of model land mask grib1/2 file
!             59 - bad degrib of model longitude (grib1 version)
!             60 - bad open of model longitude grib1/2 file
!             61 - bad degrib of model latitude data (grib1 version)
!             62 - unrecognized model grid map projection
!             63 - bad degrib of model latitude grib1 header
!             64 - bad open of model latitude grib1/2 file
!             65 - bad open of mmab global landmask file
!             66 - bad degrib of global mmab ice data (grib 1)
!             67 - global mmab grib1 data is not lat/lon grid
!             68 - bad degrib of global mmab grib1 header
!             69 - bad open of global mmab grib1/2 file
!             70 - bad degrib of ims ice record (grib1 version)
!             71 - bad degrib of ims snow record (grib1 version)
!             72 - bad degrib of ims grib1 file header
!             73 - bad open of ims grib1/2 file
!             74 - bad open of output ice analysis grib1 file
!             75 - bad write of output ice analysis grib1 file
!             76 - bad selection of input sea ice data 
!             77 - bad open of configuration namelist
!             78 - bad read of configuration namelist
!             82 - bad degrib of model landmask file (grib2 version)
!             83 - bad degrib of model longitude file (grib2 version)
!             84 - bad degrib of model latitude file (grib2 version)
!             87 - bad open of ims land mask file
!             88 - bad read of ims land mask file
!             89 - ims land mask file not selected.
!             90 - model landmask file not grib1 or grib2 format.
!             91 - model latitude file not grib1 or grib2 format.
!             92 - mmab global landmask file must be selected
!             93 - bad degrib of global mmab ice data (grib 2)
!             94 - bad degrib of ims ice record (grib2 version)
!             95 - bad degrib of ims snow record (grib2 version)
!             96 - model longitude file not grib1 or grib2 format.
!
!$$$

!-------------------------------------------------------
! interpolate sea ice data to the model grid
!-------------------------------------------------------

 use icedat, only                : read_ice_src_data

 use model_grid, only            : read_mdl_grid_info

 use ice2mdl, only               : interp

 use program_setup, only         : read_config_nml

 implicit none

 call w3tagb('ICE2MDL',2005,0139,0000,'NP2')

 print*,''
 print*,"***********************"
 print*,"*** BEGIN EXECUTION ***"
 print*,"***********************"

!-------------------------------------------------------
! get configuration stuff.
!-------------------------------------------------------

 call read_config_nml

!-------------------------------------------------------
! read ice data on the source grid.
!-------------------------------------------------------

 call read_ice_src_data

!-------------------------------------------------------
! read information about the model grid to which the
! sea ice data will be interpolated.
!-------------------------------------------------------

 call read_mdl_grid_info

!-------------------------------------------------------
! interpolate the data to the model grid, then write
! it to a grib 1 or grib 2 file.
!-------------------------------------------------------
 
 call interp

 print*,''
 print*,'****************************'
 print*,'**** NORMAL TERMINATION ****'
 print*,'****************************'

 call w3tage('ICE2MDL')

 stop

 end program driver

