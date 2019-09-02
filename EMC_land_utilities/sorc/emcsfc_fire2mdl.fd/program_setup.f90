 module program_setup
!$$$  module documentation block
!                .      .    .                                       .
! module:    program_setup
!   prgmmr: gayno         org: w/np2     date: 2015-jan-30
!
! $Revision$
!
! abstract: this module contains routines required for
!           the program setup.
!
! program history log:
!   2015-jan-30  gayno   - initial version
!
! usage: "use program_setup".  Then call public routine
!        read_config_nml to read the program configuration
!        namelist.
!
! remarks: some variable definitions
!    model_lat_file        - path/name of model grid pt latitude file
!    model_lon_file        - path/name of model grid pt longitude file
!    model_lsmask_file     - path/name of model landmask file
!    model_fire_file       - path/name of model burned area data 
!                            file.
!    nesdis_1km_fire_file  - path/name of nesdis 1km burned area
!                            data file
!    nesdis_12km_fire_file - path/name of nesdis 12km burned area
!                            data file
!
!$$$
!
 implicit none

 private

 public                    :: read_config_nml

 character*200, public     :: model_lat_file    
 character*200, public     :: model_lon_file      
 character*200, public     :: model_lsmask_file  
 character*200, public     :: model_fire_file    
 character*200, public     :: nesdis_1km_fire_file   
 character*200, public     :: nesdis_12km_fire_file   

 contains

 subroutine read_config_nml
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_config_nml
!   prgmmr: gayno          org: w/np2     date: 2015-jan-30
!
! abstract: this subroutine reads the program's configuration namelist
!   that contains variables that hold program's input and output 
!   filenames.
!
! program history log:
! 2015-jan-30  gayno    - initial version
!
! usage: call read_config_nml
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files:
!   input:
!      - program configuration namelist file, fort.41
!
!   output: none
!
! condition codes:
!   77 - bad open on namelist file
!   78 - bad read on namelist file
!
! remarks: none.
!
!$$$

 implicit none

 integer                         :: istat

 namelist /source_data/ nesdis_1km_fire_file, nesdis_12km_fire_file

 namelist /model_specs/ model_lat_file,       &
                        model_lon_file,       &
                        model_lsmask_file

 namelist /output_data/ model_fire_file 

 print*,''
 print*,"- READ CONFIGURATION NAMELIST"

 open(41, iostat=istat)

 if (istat /= 0) then
   print*,''
   print*,'- FATAL ERROR. BAD OPEN ON CONFIGURATION NAMELIST.  ISTAT IS ', istat
   close(41)
   call w3tage('FIRE2MDL')
   call errexit(77)
 end if

 read(41, nml=source_data, iostat=istat, err=900)
 read(41, nml=model_specs, iostat=istat, err=900)
 read(41, nml=output_data, iostat=istat, err=900)

 close(41)

 return

 900 continue
 print*,''
 print*,'- FATAL ERROR. BAD READ ON CONFIGURATION NAMELIST.  ISTAT IS ', istat
 close(41)
 call w3tage('FIRE2MDL')
 call errexit(78)

 end subroutine read_config_nml

 end module program_setup
