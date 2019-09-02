 subroutine grib_latlons
!$$$ subroutine documentation block
!
! subprogram:  grib lat/lons
!   prgmmr: gayno          org: w/np2           date: ????
!
! $Revision$
!
! abstract: write the model grid latitude and longitude
!   to grib files.
!
! program history log:
! ????        gayno     - initial version
!
! usage:  call routine with no arguments
!
! files: 
!   input: none
!
!   output: (grib 1 or grib 2)
!     - h-point latitude
!     - h-point longitude
!     - v-point latitude
!     - v-point longitude
!
! condition codes: all fatal
!   1 - error opening h-point latitude file
!   2 - error writing h-point latitude file
!   3 - error opening h-point longitude file
!   4 - error writing h-point longitude file
!   5 - error opening v-point latitude file
!   6 - error writing v-point latitude file
!   7 - error opening v-point longitude file
!   8 - error writing v-point longitude file
!
! remarks: v-point fields only output for staggered
!   nam grids.
!
!$$$
 use init_grib1, only            : kpds_mdl,  &
                                   kgds_mdl
 
 use grib_mod

 use init_grib2

 use calc_latlons, only          : lat_mdl,       &
                                   lon_mdl,       &
                                   lat_vpnts_mdl, &
                                   lon_vpnts_mdl, &
                                   lat_first_vpnt_mdl, &
                                   lon_first_vpnt_mdl, &
                                   lat_last_vpnt_mdl, &
                                   lon_last_vpnt_mdl


 use program_setup, only         : imdl, jmdl, grib2,  &
                                   thinned, domain_type, domain_name

 use mpimod, only                : gather,  &
                                   myrank

 implicit none

 include 'mpif.h'

 type(gribfield)              :: gfld

 character*256                :: fngrib

 integer                      :: iret
 integer                      :: lugb
 integer                      :: kgds(200)
 integer                      :: kpds(200)

 logical*1                    :: lbms(imdl,jmdl)

 real, allocatable            :: dummy(:,:)

 kpds = kpds_mdl
 kgds = kgds_mdl

 lugb = 59
 if(grib2)then
   fngrib  = trim(domain_name)//"_hpnt_latitudes.grb2"
 else
   fngrib  = trim(domain_name)//"_hpnt_latitudes.grb"
 endif

 if (myrank == 0) then
   call baopenw(lugb,fngrib,iret)
   if (iret /= 0) then
     print*,"- FATAL ERROR OPENING: ", trim(fngrib), " IRET IS: ", iret
     call mpi_abort(mpi_comm_world, 1, iret)
   end if
 end if

 allocate (dummy(imdl,jmdl))

 call gather(lat_mdl, imdl, jmdl, dummy)

 if (thinned) then
   call fill(dummy)
 end if

 lbms     = .false.

 print*,"- OUTPUT LATITUDES IN GRIB FORMAT."

 if (grib2) then
   call grib2_init(gfld)
   gfld%fld=reshape(dummy, (/imdl*jmdl/) )
   gfld%discipline = 0
   gfld%ipdtmpl(1)= 191  ! oct 10; parameter category
   gfld%ipdtmpl(2)= 1    ! oct 11; parameter
   gfld%idrtmpl=0
   gfld%idrtmpl(3)=4  ! scaling factor
 else
   kpds(5)  = 176 ! latitude
   kpds(22) = 4   ! scaling factor
   kpds(4)  = 128 ! don't use bitmap for this field
 endif

 if (myrank == 0) then
   if(grib2) then
     call putgb2 (lugb,gfld,iret)
   else
     call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms,    &
                 dummy, iret)
   endif
   if (iret /= 0) then
     print*,"- FATAL ERROR GRIBBING: ", trim(fngrib), " IRET IS: ", iret
     call mpi_abort(mpi_comm_world, 2, iret)
   end if
   call baclose(lugb, iret)
 end if

 lugb = 30

 if (grib2) then
   fngrib  = trim(domain_name)//"_hpnt_longitudes.grb2"
 else
   fngrib  = trim(domain_name)//"_hpnt_longitudes.grb"
 endif

 if (myrank == 0) then
   call baopenw(lugb,fngrib,iret)
   if (iret /= 0) then
     print*,"- FATAL ERROR OPENING: ", trim(fngrib), " IRET IS: ", iret
     call mpi_abort(mpi_comm_world, 3, iret)
   end if
 end if

 call gather(lon_mdl, imdl, jmdl, dummy)

 if (thinned) then
   call fill(dummy)
 end if

 if (grib2) then
   gfld%fld=reshape(dummy, (/imdl*jmdl/) )
   where(gfld%fld < 0.0) gfld%fld=360.0+gfld%fld
   gfld%discipline = 0
   gfld%ipdtmpl(1)= 191  ! oct 10; parameter category
   gfld%ipdtmpl(2)= 2    ! oct 11; parameter
   gfld%idrtmpl=0
   gfld%idrtmpl(3)=4
 else
   kpds(5)  = 177  ! longitude
 endif

 if (myrank == 0) then
   print*,"- OUTPUT LONGITUDES IN GRIB FORMAT."
   if (grib2) then
     call putgb2(lugb,gfld,iret)
   else
     call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms, dummy, iret)
   endif
   if (iret /= 0) then
     print*,"- FATAL ERROR GRIBBING: ", trim(fngrib), " IRET IS: ", iret
     call mpi_abort(mpi_comm_world, 4, iret)
   end if
   call baclose(lugb, iret)
 end if ! myrank

!-----------------------------------------------------------------------
! for staggered grids, output lat/lon on the velocity points.
! for non-staggered grids, these arrays would not have been
! allocated or processed.
!-----------------------------------------------------------------------

 if (allocated(lat_vpnts_mdl) .and. allocated(lon_vpnts_mdl)) then

   lugb = 51
   if (grib2) then
     fngrib  = trim(domain_name)//"_vpnt_latitudes.grb2"
   else
     fngrib  = trim(domain_name)//"_vpnt_latitudes.grb"
   endif

   if (myrank == 0) then
     call baopenw(lugb,fngrib,iret)
     if (iret /= 0) then
       print*,"- FATAL ERROR OPENING: ", trim(fngrib), " IRET IS: ", iret
       call mpi_abort(mpi_comm_world, 5, iret)
     end if
   end if ! myrank

   call gather(lat_vpnts_mdl, imdl, jmdl, dummy)

   if (grib2) then
     gfld%fld=reshape(dummy, (/imdl*jmdl/) )
     gfld%discipline = 0
     gfld%ipdtmpl(1)= 191  ! oct 10; parameter category
     gfld%ipdtmpl(2)= 1    ! oct 11; parameter
     gfld%idrtmpl=0
     gfld%idrtmpl(3)=4
     if (trim(domain_type)=='bgrid') then
       gfld%igdtmpl(19)=78  ! scanning mode flag, the 'v' grid.
                            ! for grib2, point(1,1) is always
                            ! the 'h' point.
     elseif (trim(domain_type)=='egrid') then
       gfld%igdtmpl(19)=72  ! scanning mode flag, the 'v' grid.
                            ! for grib2, point(1,1) is always
                            ! the 'h' point.
     endif
   else
     kpds(5)  = 176  ! latitude
!  note: for b-grids, ipolates assumes point 1,1 is specific to the
!  stagger.  for e-grids, point 1,1 is always the h point.
     if (trim(domain_type)=='bgrid') then
       kgds(4)=nint(lat_first_vpnt_mdl*1000.)
       kgds(5)=nint(lon_first_vpnt_mdl*1000.)
       kgds(12)=nint(lat_last_vpnt_mdl*1000.)
       kgds(13)=nint(lon_last_vpnt_mdl*1000.)
     endif
   endif

   if (myrank == 0) then
     print*,"- OUTPUT VELOCITY POINT LATITUDES IN GRIB FORMAT."
     if(grib2) then
       call putgb2(lugb,gfld,iret)
     else
       call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms, dummy, iret)
     endif
     if (iret /= 0) then
       print*,"- FATAL ERROR GRIBBING: ", trim(fngrib), " IRET IS: ", iret
       call mpi_abort(mpi_comm_world, 6, iret)
     end if
     call baclose(lugb, iret)
   end if ! myrank

   lugb = 52
   if(grib2) then
     fngrib  = trim(domain_name)//"_vpnt_longitudes.grb2"
   else
     fngrib  = trim(domain_name)//"_vpnt_longitudes.grb"
   endif

   if (myrank == 0) then
     call baopenw(lugb,fngrib,iret)
     if (iret /= 0) then
       print*,"- FATAL ERROR OPENING: ", trim(fngrib), " IRET IS: ", iret
       call mpi_abort(mpi_comm_world, 7, iret)
     end if
   end if

   call gather(lon_vpnts_mdl, imdl, jmdl, dummy)

   if (grib2) then
     gfld%fld=reshape(dummy, (/imdl*jmdl/) )
     where(gfld%fld < 0.0) gfld%fld=360.0+gfld%fld
     gfld%discipline = 0
     gfld%ipdtmpl(1)= 191  ! oct 10; parameter category
     gfld%ipdtmpl(2)= 2    ! oct 11; parameter
     gfld%idrtmpl=0
     gfld%idrtmpl(3)=4
     if (trim(domain_type)=='bgrid') then
       gfld%igdtmpl(19)=78  ! scanning mode flag, the 'v' grid.
                            ! for grib2, point(1,1) is always
                            ! the 'h' point.
     elseif (trim(domain_type)=='egrid') then
       gfld%igdtmpl(19)=72  ! scanning mode flag, the 'v' grid.
                            ! for grib2, point(1,1) is always
                            ! the 'h' point.
     endif
   else
     kpds(5)  = 177  ! longitude
   endif

   if (myrank == 0) then
     print*,"- OUTPUT VELOCITY POINT LONGITUDES IN GRIB FORMAT."
     if (grib2) then
       call putgb2(lugb,gfld,iret)
     else
       call putgb (lugb, (imdl*jmdl), kpds, kgds, lbms, dummy, iret)
     endif
     if (iret /= 0) then
       print*,"- FATAL ERROR GRIBBING: ", trim(fngrib), " IRET IS: ", iret
       call mpi_abort(mpi_comm_world, 8, iret)
     end if
     call baclose(lugb, iret)
   end if ! myrank

 end if   ! output v-points?

 if (grib2) then
   call grib2_free(gfld)
 endif

 return

 end subroutine grib_latlons
