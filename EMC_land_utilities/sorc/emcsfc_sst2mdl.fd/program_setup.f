 module program_setup
!$$$  module documentation block
!                .      .    .                                       .
! $Revision$
!
! module:    program_setup
!   prgmmr: gayno         org: w/np2     date: 2005-05-20
!
! abstract: this module reads in data from the program's
!           configuration namelist.
!
! program history log:
!   2005-05-20  gayno   - initial version
!   2007-10-15  gayno   - changed logical name to climo_4_lakes                        
!   2014-10-28  gayno   - add "output_grib2" namelist variable
!
! usage: use program_setup
!
! remarks: some variable definitions
!   climo_4_lakes           - when true, will place a climo value of
!                             sst in the great salt lake, salton sea and 
!                             ft peck reservior, and lake champlain instead 
!                             of using the source data
!   input_flake_file        - path/name of flake climo sst file
!   input_src_file          - path/name of input global mmab sst source file
!   input_src_bitmap_file   - path/name of input global mmab sst source file
!                             land/sea mask
!   input_src14km_file      - path/name of input 14 km regional sst source file
!                             from NOAA/GLERL (covers north america)
!   model_lat_file          - path/name of model latitudes file
!   model_lon_file          - path/name of model longitudes file
!   model_lsmask_file       - path/name of model landmask file
!   output_file             - path/name of sst analysis output from this program
!   output_grib2            - when true, output sst will be in grib 2.
!                             when false, grib 1.
!
!$$$

 implicit none

 private

 character*150, public                :: input_flake_file     
 character*150, public                :: input_src_file     
 character*150, public                :: input_src_bitmap_file     
 character*150, public                :: input_src14km_file  
 character*150, public                :: model_lat_file    
 character*150, public                :: model_lon_file      
 character*150, public                :: model_lsmask_file  
 character*150, public                :: output_file        

 logical, public                      :: climo_4_lakes
 logical, public                      :: output_grib2

 public read_config_nml

 contains

 subroutine read_config_nml
!$$$  subprogram documentation block
!            
! subprogram:    read_config_nml
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: this subroutine reads the program's configuration namelist
!   that contains the the paths/filenames for input and output, and
!   other program control flags.
!
! program history log:
! 2005-05-20  gayno    - initial version
! 2014-10-28  gayno    - new namelist variable "output_grib2"
!
! usage: call read_config_nml
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files:
!   inputs: 
!      - configuration namelist, fort.41
!
!   outputs: none.
!
! condition codes:  all fatal
!   60 - bad open of fort.41
!   61 - bad read of fort.41
!
! remarks: none.
!
!$$$

 implicit none

 integer               :: istat

 namelist /source_data/   input_src_file,      &
                          input_src_bitmap_file, &
                          input_src14km_file,  &
                          input_flake_file

 namelist /model_specs/   model_lat_file,      &
                          model_lon_file,      &
                          model_lsmask_file

 namelist /output_data/  output_file, output_grib2

 namelist /parameters/   climo_4_lakes 

 print*,''
 print*,"- READ CONFIGURATION NAMELIST"

 open(41, iostat=istat, err=900)

 read(41, nml=source_data, iostat=istat, err=910)
 read(41, nml=model_specs, iostat=istat, err=910)
 read(41, nml=output_data, iostat=istat, err=910)
 read(41, nml=parameters,  iostat=istat, err=910)

 close(41)

 return

 900 print*,''
 print*,'- FATAL ERROR: BAD OPEN ON CONFIGURATION NAMELIST.  ISTAT IS ', istat
 close(41)
 call w3tage('SST2MDL')
 call errexit(60)

 910 print*,''
 print*,'- FATAL ERROR: BAD READ ON CONFIGURATION NAMELIST.  ISTAT IS ', istat
 close(41)
 call w3tage('SST2MDL')
 call errexit(61)

 end subroutine read_config_nml

 end module program_setup
