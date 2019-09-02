 subroutine read_firstguess_data_regional_nems(snowfree_albedo, albrecs,   &
                                          albedo, mxsnow_albedo,           &
                                          canopy_mc, greenfrc,             &
                                          seaice, snow_liq_equiv,          &
                                          snow_depth, skin_temp,           &
                                          soilm_liq, soilm_tot, soil_temp, &
                                          z0, veg_type, soil_type,    &
                                          substrate_temp, lsmask)
!$$$  subprogram documentation block
!
! subprogram:    read_firstguess_data_regional_nems
!   prgmmr: gayno          org: w/np2     date: 2009-05-08
!
! $Revision$
!
! abstract: read surface fields from nems binary file
!
! program history log:
! 2009-05-08  gayno    - initial version
!
! usage: call read_firstguess_data_regional_nems using the following arguments.
!
!   input argument list: 
!     albrecs                      - number of albedo records (1)
!
!   output argument list:
!     albedo                       - albedo including the effects of snow
!     canopy_mc                    - plant canopy moisture content
!     greenfrc                     - greenness fraction
!     lsmask                       - land/sea mask
!     mxsnow_albedo                - maximum snow albedo
!     seaice                       - sea ice
!     snow_liq_equiv               - snow liquid equivalent
!     skin_temp                    - skin temperature
!     snow_depth                   - snow depth
!     snowfree_albedo              - albedo not including effects of snow
!     soil_temp                    - soil temperature
!     soil_type                    - soil type
!     soilm_liq                    - soil moisture - liquid portion
!     soilm_tot                    - soil moisture - liq + frozen
!     sst                          - sea surface temperature
!     substrate_temp               - soil substrate temperature
!     veg_type                     - vegetation type (or landuse)
!     z0                           - roughness length
!
! files:
!   input:
!     - model restart file, nemsio format
!
!   output: none
!
! condition codes: all fatal
!   60 - error reading nemsio file
!   61 - i/j dimensions in nemsio file to not match landmask file   
!
! remarks: none.
!
!$$$

 use nemsio_module

 use program_setup, only        : first_guess_file,   &
                                  ijmdl,              &
                                  nsoil

 implicit none

 include 'mpif.h'

 integer, intent(in)                :: albrecs
 integer, intent(out)               :: soil_type(ijmdl)
 integer, intent(out)               :: veg_type(ijmdl)

 real,   intent(out)                :: albedo(ijmdl)
 real,   intent(out)                :: canopy_mc(ijmdl)
 real,   intent(out)                :: greenfrc(ijmdl)
 real,   intent(out)                :: lsmask(ijmdl)
 real,   intent(out)                :: mxsnow_albedo(ijmdl)
 real,   intent(out)                :: seaice(ijmdl)
 real,   intent(out)                :: snow_liq_equiv(ijmdl)
 real,   intent(out)                :: skin_temp(ijmdl)
 real,   intent(out)                :: snow_depth(ijmdl)
 real,   intent(out)                :: snowfree_albedo(ijmdl)
 real,   intent(out)                :: soilm_liq(ijmdl,nsoil)
 real,   intent(out)                :: soilm_tot(ijmdl,nsoil)
 real,   intent(out)                :: soil_temp(ijmdl,nsoil)
 real,   intent(out)                :: substrate_temp(ijmdl)
 real,   intent(out)                :: z0(ijmdl)

 character*255                      :: gfname
 character(nemsio_charkind8)        :: gaction
 character*20                       :: vlevtyp, vname

 integer(nemsio_intkind)            :: iret, vlev, idum, jdum, kdum, kmdl
 integer                            :: ierr

 real, allocatable                  :: dummy(:)

 type(nemsio_gfile)                 :: gfile

 print*,"- READ FIRST GUESS FILE: ",trim(first_guess_file)
 print*,"- FILE IS NEMS FORMAT."

 print*,'- INITIALIZE NEMSIO MODULE.'
 call nemsio_init(iret=iret)
 if (iret /= 0) goto 9100

 gfname=first_guess_file
 print*,'- OPEN FILE FOR READING: ', trim(gfname)
 gaction="READ"
 call nemsio_open(gfile,gfname,gaction,iret=iret)
 if (iret /= 0) goto 9100

 print*,"- READ FILE HEADER."
 call nemsio_getfilehead(gfile,iret,dimx=idum,dimy=jdum,dimz=kdum)
 if (iret /= 0) goto 9100
 kmdl=kdum

 if ((idum*jdum)/=ijmdl) then  ! dimensions in nems file do not match
                               ! the dimensions in the grib landmask file.
   print*,"- FATAL ERROR: I/J DIMENSIONS IN FILE DO NOT MATCH LANDMASK FILE."
   call mpi_abort(mpi_comm_world, 61, ierr)
 endif

 allocate(dummy(ijmdl))

 print*,"- READ SOIL TYPE."
 vname='sltyp'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9100
 soil_type=nint(dummy)

 print*,"- READ VEGETATION TYPE."
 vname='vgtyp'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9100
 veg_type=nint(dummy)

 print*,"- READ ALBEDO."
 vname='albedo'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,albedo,iret=iret)
 if (iret /= 0) goto 9100

 print*,"- READ CANOPY MOISTURE CONTENT."
 vname='cmc'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,canopy_mc,iret=iret)
 if (iret /= 0) goto 9100

 print*,"- READ GREENNESS."
 vname='vegfrc'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,greenfrc,iret=iret)
 if (iret /= 0) goto 9100

 print*,"- READ SEA ICE."
 vname='sice'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,seaice,iret=iret)
 if (iret /= 0) goto 9100

 print*,"- READ LAND MASK."
 vname='sm'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9100

 lsmask = 1.0     ! land

 where(dummy == 1.0)  lsmask = 0.0    ! open water
 where(seaice == 1.0) lsmask = 0.0    ! sea ice

 print*,'- READ SURFACE PRESSURE'
 vname='pres'
 vlevtyp='layer'
 call nemsio_readrecv(gfile,vname,vlevtyp,kmdl,dummy,iret=iret)
 if (iret /= 0) goto 9100

 print*,"- READ SKIN POTENTIAL TEMPERATURE."
 vname='ths'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,skin_temp,iret=iret)
 if (iret /= 0) goto 9100
! convert to temperature
 skin_temp = skin_temp / ( (100000./dummy) ** 0.286 )
 deallocate(dummy)

 print*,"- READ MAX SNOW ALBEDO."
 vname='mxsnal'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,mxsnow_albedo,iret=iret)
 if (iret /= 0) goto 9100

 print*,"- READ LIQUID EQUIVALENT SNOW."
 vname='sno'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,snow_liq_equiv,iret=iret)
 if (iret /= 0) goto 9100
 snow_liq_equiv=snow_liq_equiv*0.001  ! convert to meters to be 
                                      ! consistent with analysis

 print*,"- READ SNOW DEPTH."
 vname='si'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,snow_depth,iret=iret)
 if (iret /= 0) goto 9100
 snow_depth=snow_depth*0.001   ! convert to meters to be consistent with
                               ! analysis

 print*,"- READ SNOW FREE ALBEDO."
 vname='albase'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,snowfree_albedo,iret=iret)
 if (iret /= 0) goto 9100

 print*,"- READ SUBSTRATE TEMPERATURE."
 vname='tg'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,substrate_temp,iret=iret)
 if (iret /= 0) goto 9100

 print*,"- READ Z0"
 vname='zorl'
 vlevtyp='sfc'
 vlev=1
 call nemsio_readrecv(gfile,vname,vlevtyp,vlev,z0,iret=iret)
 if (iret /= 0) goto 9100

 print*,"- READ SOIL TEMPERATURE."
 vname='stc'
 vlevtyp='soil layer'
 do vlev=1, nsoil
   call nemsio_readrecv(gfile,vname,vlevtyp,vlev,soil_temp(:,vlev),iret=iret)
   if (iret /= 0) goto 9100
 enddo

 print*,"- READ TOTAL SOIL MOISTURE."
 vname='smc'
 vlevtyp='soil layer'
 do vlev=1, nsoil
   call nemsio_readrecv(gfile,vname,vlevtyp,vlev,soilm_tot(:,vlev),iret=iret)
   if (iret /= 0) goto 9100
 enddo

 print*,"- READ LIQUID SOIL MOISTURE."
 vname='sh2o'
 vlevtyp='soil layer'
 do vlev=1, nsoil
   call nemsio_readrecv(gfile,vname,vlevtyp,vlev,soilm_liq(:,vlev),iret=iret)
   if (iret /= 0) goto 9100
 enddo

 print*,'- CLOSE FILE.'
 call nemsio_close(gfile,iret=iret)
 if (iret /= 0) then
   print*,'- CLOSE FAILED, IRET IS: ',iret
 endif

 return

 9100 print*,"- FATAL ERROR: BAD READ. IRET IS: ", iret
 call mpi_abort(mpi_comm_world, 60, ierr)

 end subroutine read_firstguess_data_regional_nems
