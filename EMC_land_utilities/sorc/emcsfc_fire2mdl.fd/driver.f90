 program driver
!$$$  main program documentation block
!                .      .    .                                       .
! main program: fire2mdl
!   prgmmr: gayno            ORG: NP2              DATE: 2015-jan-30
!
! $Revision$
!
! abstract: interpolate burned area data to a model grid.
!
! program history log:
!   2015-jan-30  gayno     initial version
!
!   input files:
!    - program configuration namelist, fort.41.
!    - 1-km nesdis burned area data in grib2 format.
!    - 12-km nesdis burned area data in grib2 format.
!    - model grid point latutudes in grib1 or grib2.
!    - model grid point longitudes in grib1 or grib2.
!    - model landmask in grib1 or grib2.
!
!   output files:
!     - burned area data on the model grid (grib 2)
!
!   condition codes (non-zero condition is fatal):
!     cond =   0 - successful run
!          =  37 - no input fire data available
!          =  40 - bad file open in routine grib_check
!          =  42 - bad file open - output grib2 data
!          =  43 - bad file write - output grib2 data
!          =  45 - fatal error in routine lambconf
!          =  50 - error converting grib2 grid def
!                  template.  unknown grid type.
!          =  59 - error opening model output file
!          =  60 - error writing model output file
!          =  77 - bad open on configuration namelist
!          =  78 - bad read on configuration namelist
!          =  79 - unrecognized model grid type
!          =  80 - bad open of model latitude file
!          =  81 - bad degrib of model latitude header
!          =  82 - bad degrib of model latitude data
!          =  83 - bad open of model longitude file
!          =  84 - bad degrib of model longitude data
!          =  85 - bad open of model landmask file
!          =  86 - bad degrib of model landmask data
!          =  91 - model latitude file not grib1 or grib2
!          =  92 - model longitude file not grib1 or grib2
!          =  93 - model landmask file not grib1 or grib2
!
! remarks: User may select 1-km or 12-km nesdis data or both.
!   The 12 km data covers north america.  The 1-km data covers
!   conus and parts of canada.  If both are selected, the 
!   1-km takes precendence.  Ex: the NAM grid would have
!   1-km data in the interior and 12-km data toward the edges.
!
!$$$
!
 use program_setup, only : read_config_nml
 use model_grid, only : read_mdl_grid_info
 use firedat, only : read_firedat_driver

 implicit none

 call w3tagb('FIRE2MDL',2012,010,0000,'NP2')

!-------------------------------------------------------------------
! read program configuration namelist.
!-------------------------------------------------------------------

 call read_config_nml
 
!-------------------------------------------------------------------
! read information specfiying the model grid specs.
!-------------------------------------------------------------------

 call read_mdl_grid_info

!-------------------------------------------------------------------
! read nesdis burned area data.
!-------------------------------------------------------------------

 call read_firedat_driver

!-------------------------------------------------------------------
! interpolate nesdis data to model grid and output the result
! to a grib2 file.
!-------------------------------------------------------------------

 call interp

 print*,''
 print*,'*** NORMAL TERMINATION ***'

 call w3tage('FIRE2MDL')

 stop

 end program driver
