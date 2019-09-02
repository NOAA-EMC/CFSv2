 module program_setup
!$$$  module documentation block
!           
! module:    program_setup
!   prgmmr: gayno         org: w/np2     date: 2005-05-20
!
! $Revision$
!
! abstract: this module reads in data from the program's
!           configuration namelist.
!
! program history log:
!   2005-05-20  gayno   - initial version
!   2014-10-16  gayno   - added 'output_grib2' logical.
!
! usage: use program_setup
!
! remarks: some variable definitions
!   data_flag                      - communicates whether an ims
!                                    or global source data is used
!   grib_century/day/month/year    - date/time to be placed in output
!                                    grib data header
!   input_global_src_file          - path/name of global input source file
!   input_global_src_lsmask_file   - path/name of global input source
!                                    land mask file
!   input_ims_src_file             - path/name of ims source file.
!                                    may be grib1 or grib2 format.
!                                    may be 96th (4km) or 16th mesh (23 km)
!                                    version.
!   input_ims_src_lsmask_file      - path/name of 16th mesh ims 
!                                    land mask file.  NOT used when using
!                                    96th mesh ims data because that 
!                                    data is gribbed with a bitmap.
!   model_lat_file                 - path/name of model latitude grib1 or grib2 file.
!   model_lon_file                 - path/name of model longitude grib1 or grib2 file
!   model_lsmask_file              - path/name of model land mask grib1 or grib2  file
!   output_file                    - path/name of output ice analysis grib1 or grib2 file
!   output_grib2                   - when 'true' the output_file will be grib2 format.
!                                    otherwise, grib 1.
!
!$$$

 implicit none

 private

 character*6, public      :: data_flag

 character*150, public    :: input_global_src_file       
 character*150, public    :: input_global_src_lsmask_file 
 character*150, public    :: input_ims_src_file 
 character*150, public    :: input_ims_src_lsmask_file      
 character*150, public    :: model_lat_file     
 character*150, public    :: model_lon_file      
 character*150, public    :: model_lsmask_file   
 character*150, public    :: output_file          

 integer, public          :: grib_century
 integer, public          :: grib_day
 integer, public          :: grib_hour
 integer, public          :: grib_month
 integer, public          :: grib_year

 logical, public          :: output_grib2

 public                   :: read_config_nml

 contains

 subroutine read_config_nml
!$$$  subprogram documentation block
!            
! subprogram:    read_config_nml
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: this subroutine reads the program's configuration namelist
!   that contains the the paths/filenames for input and output, and
!   other program control info.
!
! program history log:
! 2005-05-20  gayno    - initial version
! 2014-10-16  gayno    - added logical 'output_grib2' to the
!                        'output' namelist.
!
! usage: call read_config_nml
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files:
!    input:
!       - program configuration namelist, fort.41
!
!    output: none
!
! condition codes: all fatal
!    76 - bad selection of input sea ice data
!    77 - bad open of configuration namelist
!    78 - bad read of configuration namelist
!
! remarks: none.
!
!$$$

 implicit none

 integer                    :: istat

 namelist /global_src/ input_global_src_file,        &
                       input_global_src_lsmask_file

 namelist /ims_src/ input_ims_src_file,        &
                    input_ims_src_lsmask_file

 namelist /model_specs/ model_lat_file,     &
                        model_lon_file,     &
                        model_lsmask_file

 namelist /output/      output_file,    &
                        output_grib2

 namelist /output_grib_time/ grib_year,    &
                             grib_month,   &
                             grib_day,     &
                             grib_hour  

 print*,''
 print*,"- READ CONFIGURATION NAMELIST"

 open(41, iostat=istat, err=998)
 read(41, nml=global_src,  iostat=istat, err=999)
 read(41, nml=ims_src,  iostat=istat, err=999)
 read(41, nml=model_specs, iostat=istat, err=999)
 read(41, nml=output, iostat=istat, err=999)
 read(41, nml=output_grib_time, iostat=istat, err=999)
 close(41)

!-----------------------------------------------------------------------
! program only allows one to use either the global or ims data,
! but not both.
!
! set flag to communicate to rest of code what data source to use.
!-----------------------------------------------------------------------

 if (len_trim(input_global_src_file) > 0 .and.    & 
     len_trim(input_ims_src_file) > 0) then
   print*,"- FATAL ERROR: PICK GLOBAL OR IMS DATA, BUT NOT BOTH"
   call w3tage('ICE2MDL')
   call errexit(76)
 else if (len_trim(input_global_src_file) > 0) then
   data_flag = "global"
 else if (len_trim(input_ims_src_file) > 0) then
   data_flag = "ims"
 else
   print*,"- FATAL ERROR: MUST CHOOSE GLOBAL OR IMS ICE DATA."
   print*,"- THEN RERUN PROGRAM."
   call w3tage('ICE2MDL')
   call errexit(76)
 end if

!-----------------------------------------------------------------------
! when gribbing the final interpolated data, use the date/time
! specified in the namelist and not the date/time from the source
! data's header.  most source data arrives several hours after its
! cycle time.  so restamp it for the convenience of operations. 
!-----------------------------------------------------------------------

 grib_century = grib_year / 100

 grib_year = mod(grib_year,100)

 if (grib_year == 0) then
   grib_year = 100
 else
   grib_century = grib_century + 1
 end if

 return

 998 continue
 close (41)
 print*,''
 print*,'- FATAL ERROR: BAD OPEN ON CONFIG NAMELIST.  ISTAT IS ', istat
 call w3tage('ICE2MDL')
 call errexit(77)

 999 continue
 close (41)
 print*,''
 print*,'- FATAL ERROR: BAD READ ON CONFIG NAMELIST.  ISTAT IS ', istat
 call w3tage('ICE2MDL')
 call errexit(78)

 end subroutine read_config_nml

 end module program_setup
