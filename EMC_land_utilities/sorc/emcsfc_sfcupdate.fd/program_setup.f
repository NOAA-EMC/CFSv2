 module program_setup

!$$$  module documentation block
!
! $Revision$
!
! module:    program_setup
!   prgmmr: gayno         org: w/np2     date: 2005-05-20
!
! abstract: this module reads in data from the program's
!           configuration namelist; and reads the model land/sea
!           mask grib file to get info on the model grid.
!
! module history log:
!   2005-05-20  gayno   - initial version
!   2009-05-08  gayno   - modify for nems files
!   2014-11-10  gayno   - modify to read model land/sea mask
!                         in grib 1 or grib 2 format.
!
! usage: use program_setup
!
! remarks: some variable definitions
!   cycle_year/month/day/hour      - program updates land fields based on
!   fcst_hour                        these times.  fcst_hour is > 0
!                                    when this program is called during the
!                                    free forecast.
!   first_guess_file               - program reads first guess land data from
!                                    this file.
!   first_guess_file_type          - set to 'nems' for nems binary file
!   grid_type                      - flag that identifies grid as regional
!                                    or global
!   gdtnum_mdl                     - model land/sea mask grib 2 grid definition
!                                    template number
!   imdl                           - i-dimension of full model grid
!   ijmdl                          - total number of grid pts.  will be less
!                                    than imdl*jmdl for global thinned grids.
!   ijmdl_full                     - imdl*jmdl
!   ipts/jpts                      - holds i/j index of grid points to
!                                    be processed.  used for global thinned
!                                    grids and for running with multiple
!                                    tasks.
!   jmdl                           - j-dimension of full model grid
!   jmdl2                          - jmdl / 2
!   lonsperlat                     - when running global grids thinned, number 
!                                    of i points in each row.
!   lsmask_file                    - path/name of model land/sea mask grib file
!   nsoil                          - number of soil layers
!   output_file                    - program writes updated land data to this file
!   output_file_type               - set to 'nems' for nems binary file 
!   thinned                        - flag that says whether global grid should
!                                    be run in thinned or reduced mode.
!
!$$$

 use consts, only       : lonsperlat_t62,   &
                          lonsperlat_t126,  &
                          lonsperlat_t170,  &
                          lonsperlat_t254,  &
                          lonsperlat_t382

 implicit none

 character*150         :: first_guess_file
 character*8           :: first_guess_file_type
 character*8           :: grid_type
 character*150         :: lsmask_file
 character*150         :: output_file
 character*8           :: output_file_type

 integer               :: cycle_year
 integer               :: cycle_month
 integer               :: cycle_day
 integer               :: cycle_hour 
 integer               :: gdtnum_mdl
 integer               :: imdl     
 integer               :: ijmdl       
 integer               :: ijmdl_full  
 integer, allocatable  :: ipts(:)
 integer               :: jmdl       
 integer               :: jmdl2      
 integer, allocatable  :: jpts(:)
 integer, allocatable  :: lonsperlat(:)

 integer               :: nsoil

 logical               :: thinned

 real                  :: fcst_hour

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
! 2009-05-08  gayno    - add read of variables output_file_type and
!                        first_guess_file_type
!
! usage: call read_config_nml
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files:
!   input:
!     - configuration namelist, fort.41
!
!   output: none
!
! condition codes: all fatal
!   40 - bad open, config namelist file
!   41 - bad read, 'grid_info' namelist
!   42 - bad read, 'cycle' namelist
!   43 - bad read, 'model_fields' namelist
!   44 - bad read, 'fixed_fields' namelist
!   45 - bad read, 'output' namelist
!   46 - bad read, 'settings' namelist
! 
! remarks: none.
!
!$$$
 implicit none

 include 'mpif.h'

 character*20                         :: domain_name

 integer                              :: istat

 namelist /grid_info/  domain_name

 namelist /cycle/      cycle_year,         &
                       cycle_month,        &
                       cycle_day,          &
                       cycle_hour,         &
                       fcst_hour

 namelist /model_flds/  first_guess_file, first_guess_file_type

 namelist /fixed_flds/  lsmask_file
                  
 namelist /output/      output_file, output_file_type

 namelist /settings/    thinned, nsoil

 print*,''
 print*,"- READ CONFIGURATION NAMELIST"

 open(41, iostat=istat)

 if (istat /= 0) then
   print*,''
   print*,'- FATAL ERROR: BAD OPEN ON CONFIG NAMELIST.  ISTAT IS ', istat
   close(41)
   call w3tage('SFCUPDATE')
   call mpi_abort(mpi_comm_world, 40, istat)
 end if

 read(41, nml=grid_info, iostat=istat)

 if (istat /= 0) then
   print*,''
   print*,'- FATAL ERROR: BAD READ ON CONFIG-GRID_INFO NAMELIST.  ISTAT IS ', istat
   close(41)
   call w3tage('SFCUPDATE')
   call mpi_abort(mpi_comm_world, 41, istat)
 end if

 print*,"- PROCESSING DOMAIN: ", trim(domain_name)

 read(41, nml=cycle, iostat=istat)

 if (istat /= 0) then
   print*,''
   print*,'- FATAL ERROR: BAD READ ON CONFIG-CYCLE NAMELIST.  ISTAT IS ', istat
   close(41)
   call w3tage('SFCUPDATE')
   call mpi_abort(mpi_comm_world, 42, istat)
 end if

 print*,"- PROCESSING CYCLE (YYYYMMDDHH): ", cycle_year, cycle_month, &
                                             cycle_day, cycle_hour

 print*,"- PROCESSING FORECAST HOUR: ", fcst_hour

 read(41, nml=model_flds, iostat=istat)

 if (istat /= 0) then
   print*,''
   print*,'- FATAL ERROR: BAD READ ON CONFIG-MODEL_FLDS NAMELIST.  ISTAT IS ', istat
   close(41)
   call w3tage('SFCUPDATE')
   call mpi_abort(mpi_comm_world, 43, istat)
 end if

 read(41, nml=fixed_flds, iostat=istat)

 if (istat /= 0) then
   print*,''
   print*,'- FATAL ERROR: BAD READ ON CONFIG-FIXED_FLDS NAMELIST.  ISTAT IS ', istat
   close(41)
   call w3tage('SFCUPDATE')
   call mpi_abort(mpi_comm_world, 44, istat)
 end if

 read(41, nml=output, iostat=istat)

 if (istat /= 0) then
   print*,''
   print*,'- FATAL ERROR: BAD READ ON CONFIG-OUTPUT NAMELIST.  ISTAT IS ', istat
   close(41)
   call w3tage('SFCUPDATE')
   call mpi_abort(mpi_comm_world, 45, istat)
 end if

 read(41, nml=settings, iostat=istat)

 if (istat /= 0) then
   print*,''
   print*,'- FATAL ERROR: BAD READ ON CONFIG-SETTINGS NAMELIST.  ISTAT IS ', istat
   close(41)
   call w3tage('SFCUPDATE')
   call mpi_abort(mpi_comm_world, 46, istat)
 end if

 close(41)

 return

 end subroutine read_config_nml

 subroutine model_cfg
!$$$  subprogram documentation block
!
! subprogram:    model_cfg
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: get model grid information from the land/sea mask
!   from a grib1 or grib2 file.
!
! program history log:
! 2005-05-20  gayno    - initial version
! 2014-10-30  gayno    - read mask from grib1 or grib2 file
!
! usage: call model_cfg
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files:
!   input:
!     - model land/sea mask in grib 1 or grib 2
! 
!   output: none
! 
! condition codes: all fatal
!   20 - model land/sea mask file not grib 1 or grib 2.
!   21 - bad read land/sea mask grib 2 header
!   47 - bad open model land/sea mask file
!   48 - bad read land/sea mask grib 1 header
!   49 - unrecognized gfs grid
!
! remarks: none.
!
!$$$

 use grib_mod

 implicit none

 include 'mpif.h'

 integer                 :: i
 integer                 :: ij
 integer                 :: iret, isgrib
 integer, parameter      :: iunit = 13  ! grib file unit number
 integer                 :: j, jj, k
 integer                 :: jgds(200)
 integer                 :: jpds(200)
 integer                 :: jdisc, jpdtn, jgdtn
 integer                 :: jids(200), jpdt(200), jgdt(200)
 integer                 :: lgrib
 integer                 :: lskip
 integer, parameter      :: lugi = 0    ! grib index file unit number - not used
 integer                 :: kgds(200)
 integer                 :: kpds(200)
 integer                 :: numbytes
 integer                 :: numpts

 logical                 :: unpack

 real                    :: dum

 type(gribfield)         :: gfld

 print*,"- GET MODEL GRID SPECS FROM: ", trim(lsmask_file)

 call grib_check(lsmask_file, isgrib)

 if (isgrib==0) then
   print*,'- FATAL ERROR: MODEL LAND-SEA MASK FILE MUST BE GRIB1 OR GRIB2 FORMAT'
   call w3tage('SFCUPDATE')
   call mpi_abort(mpi_comm_world, 20, iret)
 end if

 call baopenr (iunit, lsmask_file, iret)

 if (iret /= 0) then
   print*,'- FATAL ERROR: BAD OPEN OF FILE, IRET IS ', iret
   call w3tage('SFCUPDATE')
   call mpi_abort(mpi_comm_world, 47, iret)
 end if

 if (isgrib == 1) then

!-----------------------------------------------------------------------
! tell degribber to look for the land/sea mask.
!-----------------------------------------------------------------------

   lskip    = -1
   jpds     = -1
   jgds     = -1
   jpds(5)  = 81
   kpds     = jpds
   kgds     = jgds

   print*,"- READ GRIB HEADER"

   call getgbh(iunit, lugi, lskip, jpds, jgds, lgrib,  &
               numbytes, numpts, kpds, kgds, iret)

   if (iret /= 0) then
     print*,"- FATAL ERROR: BAD READ OF GRIB HEADER, IRET IS ", iret
     call w3tage('SFCUPDATE')
     call mpi_abort(mpi_comm_world, 48, iret)
   end if

   print*,"- PDS OF MODEL GRID ",kpds(1:23)
   print*,"- GDS OF MODEL GRID ",kgds(1:15)

   imdl  = kgds(2)
   jmdl  = kgds(3)

   print*,"- MODEL GRID DIMENSIONS ARE: ",imdl,jmdl

   gdtnum_mdl = 255
   if (kgds(1) == 203 .or. kgds(1) == 205) gdtnum_mdl = 1
   if (kgds(1) == 4) gdtnum_mdl = 40

 elseif (isgrib == 2) then ! file is grib 2

   j       = 0      ! search at beginning of file
   jdisc   = 2      ! search for discipline; 2 - land-sfc products
   jpdtn   = -1     ! search for any product definition template number
   jgdtn   = -1     ! search for any grid definition template number
   jids    = -9999  ! array of values in identification section, set to wildcard
   jgdt    = -9999  ! array of values in grid definition template 3.m
   jpdt    = -9999  ! array of values in product definition template 4.n
   jpdt(1) = 0      ! search for parameter category - veg/biomass
   jpdt(2) = 0      ! search for parameter number - landcover
   unpack  = .false. ! unpack data

   call grib2_null(gfld)

   print*,"- DEGRIB DATA"
   call getgb2(iunit, lugi, j, jdisc, jids, jpdtn, jpdt, jgdtn, jgdt, &
               unpack, k, gfld, iret)

   if (iret /=0) then
     print*,'- FATAL ERROR: BAD DEGRIB OF FILE, IRET IS ', iret
     call w3tage('SFCUPDATE')
     call mpi_abort(mpi_comm_world, 21, iret)
   endif

   print*,"- PDS OF MODEL GRID ",gfld%ipdtmpl
   print*,"- GDS OF MODEL GRID ",gfld%igdtmpl

   gdtnum_mdl = gfld%igdtnum
 
!-----------------------------------------------------------------------
! Create the grib 1 gds array from the g2 gdt array.  
!-----------------------------------------------------------------------

   call gdt_to_gds(gfld%igdtnum, gfld%igdtmpl, gfld%igdtlen, kgds, &
                   imdl, jmdl, dum)

   call grib2_free(gfld)

 end if

 call baclose(iunit,iret)

!-----------------------------------------------------------------------
! for gaussian grids, determine the number of "i" points for each
! row and store in lonsperlat.  the global forecast model runs on
! a "thinned" grid where the number of i points decreases towards
! the pole.  this code can run on the full or thinned gaussian grid.
!
! for regional grids, the number of "i" points is the same for each
! row.  therefore, this variable is not used.
!
! check gds octet 6 to determine if this grid is gaussian or regional.
!-----------------------------------------------------------------------

 if (kgds(1) == 4) then  ! gaussian

   jmdl2 = jmdl/2
   allocate (lonsperlat(jmdl2))

   grid_type = "global"
 
   if (thinned) then  ! thinned gaussian grid

     print*,"- RUNNING A THINNED GRID"

     if (imdl == 192 .and. jmdl == 94) then      ! t62 grid
       lonsperlat = lonsperlat_t62
     elseif (imdl == 384 .and. jmdl == 190) then ! t126 grid
       lonsperlat = lonsperlat_t126
     elseif (imdl == 512 .and. jmdl == 256) then ! t170 grid
       lonsperlat = lonsperlat_t170
     elseif (imdl == 768 .and. jmdl == 384) then ! t254 grid
       lonsperlat = lonsperlat_t254
     else
       print*,'- FATAL ERROR: UNKNOWN GFS GAUSSIAN GRID.'
       call w3tage('SFCUPDATE')
       call mpi_abort(mpi_comm_world, 49, iret)
     end if

     ijmdl = sum(lonsperlat) * 2

   else ! full gaussian grid

     ijmdl      = imdl*jmdl
     lonsperlat = imdl

   end if

 elseif (kgds(1) == 203 .or. kgds(1) == 205) then    ! rot lat/lon grid

   jmdl2 = 1
   allocate (lonsperlat(jmdl2))

   grid_type = "regional"

   thinned = .false.      ! can't run a regional grid "thinned."
                          ! so account for an invalid choice in the namelist.

   ijmdl      = imdl*jmdl
   lonsperlat = -9999     ! flag value to tell other routines to
                          ! not use this field.

 end if

 ijmdl_full = imdl * jmdl
 
 print*,"- TOTAL NUMBER OF GRID POINTS, FULL GRID      ", ijmdl_full

 if (thinned) then
   print*,"- TOTAL NUMBER OF GRID POINTS TO BE PROCESSED ", ijmdl
 end if

!-----------------------------------------------------------------------
! the ipts and jpts arrays hold the i and j indices of the grid
! points to be processed.  these arrays are mainly used in the
! degribbing of data when running in parallel.  the grib utilities
! return the data for the entire grid, whereas the program only needs
! the data points processed for its particular mpi task.
!-----------------------------------------------------------------------

 allocate(ipts(ijmdl))
 allocate(jpts(ijmdl))

 if (trim(grid_type) == "global") then  ! gaussian

   ij = 0

   do j = 1, jmdl 

     jj = j
     if (j .gt. (jmdl/2)) jj = jmdl - j + 1

     do i = 1, lonsperlat(jj)
       ij = ij + 1
       ipts(ij) = i
       jpts(ij) = j
     enddo

   enddo

 elseif (trim(grid_type) == "regional") then  ! regional e-grid

   ij = 0

   do j = 1, jmdl
     do i = 1, imdl

       ij = ij + 1
       ipts(ij) = i
       jpts(ij) = j
 
     enddo
   enddo

 end if

 return

 end subroutine model_cfg

 subroutine setup_cleanup
!$$$  subprogram documentation block
!
! subprogram:    setup_cleanup
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: free up memory
!
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call setup_cleanup
!
!   input argument list:  n/a
!
!   output argument list: n/a
!
! files: none
!
! condition codes: none
!
! remarks: none.
!
!$$$

 implicit none

 if (allocated(ipts))         deallocate (ipts)
 if (allocated(jpts))         deallocate (jpts)
 if (allocated(lonsperlat))   deallocate (lonsperlat)

 return

 end subroutine setup_cleanup

 end module program_setup
