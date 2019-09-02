 subroutine write_output_data_regional_nems(greenfrc,                           &
                                       snow_liq_equiv, snowfree_albedo,    &
                                       albrecs, albedo, canopy_mc,         &
                                       snow_depth, skin_temp,              &
                                       soilm_liq, soilm_tot, soil_temp,    &
                                       z0, veg_type, soil_type,       &
                                       substrate_temp, seaice, lsmask)
!$$$  subprogram documentation block
!
! subprogram:    write_output_data_regional_nems
!   prgmmr: gayno          org: w/np2     date: 2009-05-08
!
! $Revision$
!
! abstract: this subroutine writes updated surface fields to the
!   nems binary file
!
! program history log:
! 2009-05-08  gayno    - initial version
!
! usage: call write_output_data_regional with the following arguments:
!
!   input argument list:
!     albedo                        - albedo incl effects of snow
!     albrecs                       - number of albedo records
!                                     (regional uses 1)
!     canopy_mc                     - canopy moisture content
!     greenfrc                      - greenness fraction
!     lsmask                        - land/sea mask - nmm convention
!                                     0 - ice/land, 1 - open water
!     seaice                        - sea ice mask
!     snow_liq_equiv                - snow liquid equivalent
!     skin_temp                     - skin temperature
!     snow_depth                    - snow depth
!     snowfree_albedo               - albedo ignoring the effects of snow
!     soil_temp                     - soil temperature
!     soil_type                     - soil type
!     soilm_liq                     - soil moisture - liquid portion
!     soilm_tot                     - soil moisture - liq + frozen
!     substrate_temp                - soil substrate temperature
!     veg_type                      - vegetation type (or landuse)
!     z0                            - roughness length
!   
!   output argument list: n/a
!
! files:
!    input: none
!
!    output:
!      - model nemsio binary file
!
! condition codes:  all fatal
!    57 - bad open or write of model nemsio binary file
!
! remarks: none.
!
!$$$

 use nemsio_module

 use program_setup, only      : output_file,  &
                                ijmdl,        &
                                nsoil

 implicit none

 include 'mpif.h'

 integer, intent(in)                :: albrecs
 integer, intent(in)                :: soil_type(ijmdl)
 integer, intent(in)                :: veg_type(ijmdl)

 real,   intent(in)                 :: albedo(ijmdl)
 real,   intent(in)                 :: canopy_mc(ijmdl)
 real,   intent(in)                 :: greenfrc(ijmdl)
 real,   intent(in)                 :: lsmask(ijmdl)
 real,   intent(in)                 :: seaice(ijmdl)
 real,   intent(inout)              :: snow_liq_equiv(ijmdl)
 real,   intent(inout)              :: skin_temp(ijmdl)
 real,   intent(inout)              :: snow_depth(ijmdl)
 real,   intent(in)                 :: snowfree_albedo(ijmdl,albrecs)
 real,   intent(in)                 :: soilm_liq(ijmdl,nsoil)
 real,   intent(in)                 :: soilm_tot(ijmdl,nsoil)
 real,   intent(in)                 :: soil_temp(ijmdl,nsoil)
 real,   intent(in)                 :: substrate_temp(ijmdl)
 real,   intent(in)                 :: z0(ijmdl)

 character*255                      :: gfname
 character(nemsio_charkind8)        :: gaction
 character*20                       :: vlevtyp, vname

 integer(nemsio_intkind)            :: iret, idum, kmdl, vlev
 integer                            :: ierr, ij

 real, allocatable                  :: dummy(:)

 type(nemsio_gfile)                 :: gfile

 print*,"- WRITE SURFACE FIELDS TO: ",trim(output_file)
 print*,"- FILE IS NEMS FORMAT."

 print*,'- INITIALIZE NEMSIO MODULE.'
 call nemsio_init(iret=iret)
 if (iret /= 0) goto 9100

 gfname=output_file
 print*,'- OPEN FILE FOR WRITING: ', trim(gfname)
 gaction="RDWR"
 call nemsio_open(gfile,gfname,gaction,iret=iret)
 if (iret /= 0) goto 9100

 print*,"- READ FILE HEADER."
 call nemsio_getfilehead(gfile,iret,dimz=idum)
 if (iret /= 0) goto 9100
 kmdl=idum

 allocate(dummy(ijmdl))

 print*,"- WRITE SOIL TYPE."
 vname='sltyp'
 vlevtyp='sfc'
 vlev=1
 dummy=float(soil_type)
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9100

 print*,"- WRITE VEGETATION TYPE."
 vname='vgtyp'
 vlevtyp='sfc'
 vlev=1
 dummy=float(veg_type)
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9100

 print*,"- WRITE ALBEDO."
 vname='albedo'
 vlevtyp='sfc'
 vlev=1
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,albedo,iret=iret)
 if (iret /= 0) goto 9100

 print*,"- WRITE CANOPY MOISTURE CONTENT."
 vname='cmc'
 vlevtyp='sfc'
 vlev=1
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,canopy_mc,iret=iret)
 if (iret /= 0) goto 9100

 print*,"- WRITE GREENNESS."
 vname='vegfrc'
 vlevtyp='sfc'
 vlev=1
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,greenfrc,iret=iret)
 if (iret /= 0) goto 9100

 print*,"- WRITE SEA ICE."
 vname='sice'
 vlevtyp='sfc'
 vlev=1
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,seaice,iret=iret)
 if (iret /= 0) goto 9100

 print*,"- WRITE LAND MASK."
 vname='sm'
 vlevtyp='sfc'
 vlev=1
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,lsmask,iret=iret)
 if (iret /= 0) goto 9100

 print*,"- WRITE SKIN TEMP."
 vname='tskin'
 vlevtyp='sfc'
 vlev=1
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,skin_temp,iret=iret)
 if (iret /= 0) goto 9100

 dummy=0.0
 do ij=1,ijmdl
   if (lsmask(ij) > 0.99) then
     dummy(ij)=skin_temp(ij)
   endif 
 enddo

 print*,"- WRITE SST."
 vname='tsea'
 vlevtyp='sfc'
 vlev=1
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,dummy,iret=iret)
 if (iret /= 0) goto 9100

 print*,"- WRITE LIQUID EQUIVALENT SNOW."
 vname='sno'
 vlevtyp='sfc'
 vlev=1
 snow_liq_equiv=snow_liq_equiv*1000.  ! model expects mm
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,snow_liq_equiv,iret=iret)
 if (iret /= 0) goto 9100

 print*,"- WRITE SNOW DEPTH."
 vname='si'
 vlevtyp='sfc'
 vlev=1
 snow_depth=snow_depth*1000. ! model expects mm
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,snow_depth,iret=iret)
 if (iret /= 0) goto 9100

 print*,"- WRITE SNOW FREE ALBEDO."
 vname='albase'
 vlevtyp='sfc'
 vlev=1
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,snowfree_albedo(:,1),iret=iret)
 if (iret /= 0) goto 9100

 print*,"- WRITE SUBSTRATE TEMPERATURE."
 vname='tg'
 vlevtyp='sfc'
 vlev=1
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,substrate_temp,iret=iret)
 if (iret /= 0) goto 9100

 print*,"- WRITE Z0"
 vname='zorl'
 vlevtyp='sfc'
 vlev=1
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,z0,iret=iret)
 if (iret /= 0) goto 9100

 print*,"- WRITE SOIL TEMPERATURE."
 vname='stc'
 vlevtyp='soil layer'
 do vlev=1, nsoil
   call nemsio_writerecv(gfile,vname,vlevtyp,vlev,soil_temp(:,vlev),iret=iret)
   if (iret /= 0) goto 9100
 enddo

 print*,"- WRITE TOTAL SOIL MOISTURE."
 vname='smc'
 vlevtyp='soil layer'
 do vlev=1, nsoil
   call nemsio_writerecv(gfile,vname,vlevtyp,vlev,soilm_tot(:,vlev),iret=iret)
   if (iret /= 0) goto 9100
 enddo
 
 print*,"- WRITE LIQUID SOIL MOISTURE."
 vname='sh2o'
 vlevtyp='soil layer'
 do vlev=1, nsoil
   call nemsio_writerecv(gfile,vname,vlevtyp,vlev,soilm_liq(:,vlev),iret=iret)
   if (iret /= 0) goto 9100
 enddo

 print*,'- READ SURFACE PRESSURE.'
 vname='pres'
 vlevtyp='layer'
 call nemsio_readrecv(gfile,vname,vlevtyp,kmdl,dummy,iret=iret)
 if (iret /= 0) goto 9100

 skin_temp = skin_temp * ( (100000./dummy) ** 0.286 )

 print*,"- WRITE SKIN POTENTIAL TEMPERATURE."
 vname='ths'
 vlevtyp='sfc'
 vlev=1
 call nemsio_writerecv(gfile,vname,vlevtyp,vlev,skin_temp,iret=iret)
 if (iret /= 0) goto 9100

 deallocate(dummy)

 print*,'- CLOSE FILE.'
 call nemsio_close(gfile,iret=iret)
 if (iret /= 0) then
   print*,'- CLOSE FAILED, IRET IS: ',iret
 endif

 return

 9100 print*,"- FATAL ERROR: BAD WRITE. IRET IS: ", iret
 call mpi_abort(mpi_comm_world, 57, ierr)

 end subroutine write_output_data_regional_nems
