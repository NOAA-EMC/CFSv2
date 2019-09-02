 program coldstart_wrf
!$$$  main program documentation block
!                .      .    .                                       .
! main program: coldstart_wrf
!   prgmmr: gayno            ORG: NP2                DATE: 2005-08-03
!
! $Revision$
!
! abstract: interpolates land/sfc states from an edas grid, an nmm
!           grid, or the gfs grid ---> to an nmm or arw grid.
!
! program history log:
!   2005-08-03  gayno     initial version
!   2006-03-13  gayno     added option to get input land states
!                         from a wrf nmm binary file
!   2009-07-13  gayno     added option to process b-grids, 
!                         nemsio formatted files, and to
!                         merge gfs and nam land states
!                         for 'large' regional grids.
!   2012-11-07  gayno     port to WCOSS machine.  remove
!                         read/write of wrf binary files.
!   2014-11-24  gayno     make grib 2 compliant
!   2014-12-04  gayno     although the mpi libraries are used,
!                         this code is not 'parallel'.  in case
!                         the user requests multiple mpi tasks,
!                         only allow the root task to do any
!                         processing.  
!
! usage:
!        
!   input files:
!     runtime configuration namelist - fort.41
!
!   input files containing land/sfc states to be
!   interpolated to output grid:
!     edas land surface binary restart file or
!     nmm nemsio restart file or
!     gfs surface restart file - gfs io format
!
!   optional input files (grib1 or grib2 format):
!     output grid land/sea mask 
!     output grid latitudes 
!     output grid longitudes 
!     output grid orography 
!     output grid substrate temp 
!     output grid snow-free albedo file 
!     output grid greenness 
!     output grid max snow albedo 
!     output grid slope type 
!     output grid soil type 
!     output grid veg type 
!     output grid roughness 
!
!   output grid files:  
!     nmm restart file - nemsio or netcdf format
!     arw restart file - netcdf format
!
!   exit states:  non-0 is fatal
!     cond =     0 - successful run
!          =    22 - bad open of file in routine grib_check
!          =    23 - unrecognized grid type in routine gdt_to_gds
!          =    31 - merge option must be used with nemsio file
!          =    33 - error reading input nemsio file (routine 
!                    read_input_file_nems)
!          =    34 - error writing nmm nems file
!          =    35 - error reading input nemsio file (routine
!                    get_fg)
!          =    36 - error writing arw netcdf file
!          =    37 - error writing nmm netcdf file
!          =    39 - invalid choice of output file type
!                    (routine read_output_grid_specs)
!          =    40 - error initializing mpi environment
!          =    41 - unrecognized model name in nemsio file
!          =    42 - invalid choice of output file type
!                    (routine write_output_data_driver)
!          =    43 - error reading nmm netcdf file
!          =    44 - error processing nemsio file
!          =    50 - error opening configuration namelist
!          =    51 - error reading configuration namelist
!          =    57 - bad open or degrib of output grid grib file
!          =    58 - output grid grib file must be grib1 or grib2
!          =    61 - error opening edas data file
!          =    62 - error reading edas data file
!          =    72 - arw grid must be lambert conformal.
!          =    73 - error reading arw netcdf file
!          =    77 - unrecognized input file type
!          =    83 - error in surface chgres driver
!          =    89 - error opening gfs restart file
!          =    90 - can run with gfs file earlier than ver200501
!          =    91 - error reading gfs restart file
!          =    94 - error initializing nemsio module
!          =    95 - error opening nemsio file
!
! remarks: By default, all land/sfc fields are interpolated from the
!          input gfs, nmm or edas restart file unless specified
!          via an optional output grid grib file.
!
!$$$

 use program_setup, only        : read_config_namelist,   &
                                  read_output_grid_specs

 use read_data, only            : read_input_file

 use interp_data, only          : interp

 use write_data, only           : write_output_data_driver

 implicit none

 include 'mpif.h'

 integer                       :: ierr, myrank

 call mpi_init(ierr)

 if (ierr /= 0) then
   print*,"- FATAL ERROR INITIALIZING MPI.  IERR IS: ", ierr
   call w3tage('COLDSTART')
   call errexit(40)
 end if

 call mpi_comm_rank(mpi_comm_world, myrank, ierr)

 if (myrank > 0) goto 900

 CALL W3TAGB('COLDSTART',2005,0210,0000,'NP2')

 call read_config_namelist

 call read_output_grid_specs

 call read_input_file

! interpolate land fields from input grid to output grid.
 call interp

! update nmm nemsio file with interpolated land fields.
 call write_output_data_driver

 900 continue

 call mpi_finalize(ierr)

 if (myrank == 0) then
   print*,"-------------------------"
   print*,"-- NORMAL TERMINATION ---"
   print*,"-------------------------"
   CALL W3TAGE('COLDSTART')
 endif

 stop 0
 
 end program coldstart_wrf
