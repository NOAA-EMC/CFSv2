 module process_data
! module documentation block
!
! $Revision$
!
! module:    process_data
!   prgmmr: gayno         org: w/np2     date: 2005-05-20
!
! abstract: this module is a collection of subroutines that
!           process various surface (land and water) fields.
!
! program history log:
!   2005-05-20  gayno   - initial version
!   2009-05-08  gayno   - rework logic 
!
! usage: use process_data
!
! remarks: some variable definitions
!   icon_thres                     - sea ice concentration below
!                                    which the model point is
!                                    set to open water 
!   seaice_fg                      - storage array for the first
!                                    guess sea ice
!
!$$$

 use consts, only              : frz_h20  

 implicit none

 private

 logical, public              :: update_sfalb
 logical, public              :: update_snow

 real, parameter, private     :: frz_ice = 271.16
 real, parameter, private     :: icon_thresh = 0.51

 real, allocatable, private   :: seaice_fg(:)

 public                       :: process_roughness
 public                       :: process_greenfrc
 public                       :: process_vegtype
 public                       :: process_soiltype
 public                       :: process_snow
 public                       :: process_sst
 public                       :: process_soilm
 public                       :: process_soil_temp
 public                       :: process_seaice
 public                       :: process_skint
 public                       :: process_snowfree_albedo_global
 public                       :: process_snowfree_albedo_regional
 public                       :: qc_fire_data

 contains

 subroutine process_greenfrc (greenfrc, canopy_mc, lsmask, ijmdl, me)
!$$$  subprogram documentation block
!
! subprogram:    process_greenfrc
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: this subroutine updates greenness fraction
!
! program history log:
! 2005-05-20  gayno    - initial version
! 2015-07-08  gayno    - reduce greenness at points
!                        affected by wild fires
!
! usage: call this routine with the following argument:
!
!   input argument list:
!     ijmdl          - number of model grid points
!     me             - mpi task number
!     greenfrc       - grenness fraction in decimal %
!     lsmask         - model landmask
!     canopy_mc      - canopy moisture content
!
!   output argument list:
!     canopy_mc      - canopy moisture content
!     greenfrc       - grenness fraction in decimal %
!
! files: none
!
! condition codes: none
!
! remarks: remove any canopy water at points where the greenness goes
! below 1%, the flag value for bare ground.
!
!$$$
 use read_data,  only          : greenfrc_climo,  & ! valid curr time step
                                 burnt_frac_monthly_anal
 implicit none

 integer                      :: ij
 integer, intent(in)          :: ijmdl
 integer, intent(in)          :: me

 real, intent(inout)          :: canopy_mc(ijmdl)
 real, intent(inout)          :: greenfrc(ijmdl)
 real, intent(in)             :: lsmask(ijmdl)

 if (allocated(greenfrc_climo)) then

   print*,"- UPDATE GREENNESS BASED ON CLIMATOLOGY"
   if (allocated (burnt_frac_monthly_anal)) print*,"- UPDATE GREENNESS BASED ON WILDFIRES"

   do ij = 1, ijmdl
     if (lsmask(ij) == 1.0) then   ! land points
       greenfrc(ij) = greenfrc_climo(ij)

       if (allocated (burnt_frac_monthly_anal)) then
         if(burnt_frac_monthly_anal(ij) > 0.0) then  ! percent grid box burned by fire
           greenfrc(ij) = (1.0-burnt_frac_monthly_anal(ij)/100.0)*greenfrc(ij)
           greenfrc(ij) = max(greenfrc(ij),0.01)
         endif
       endif

       if (greenfrc(ij) < 0.011) then
         canopy_mc(ij) = 0.0
       endif
     else                        ! non-land points
       greenfrc(ij) = 0.0
     end if
   enddo

   deallocate (greenfrc_climo)

 end if

 return

 end subroutine process_greenfrc

 subroutine process_soilm (soilm_tot, soilm_liq,   &
                           soil_type, soil_temp,   &
                           lsmask, ijmdl,          &
                           nsoil, fhcyc, me)
!$$$  subprogram documentation block
!
! subprogram:    process_soilm
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: this subroutine updates soil moisture
!
! program history log:
! 2005-05-20  gayno    - initial version
! 2015-07-08  gayno    - reduce top layer soil moisture
!                        at points affected by wildfires.
!
! usage: call this routine with the following argument:
!
!   input argument list:
!     fhcyc          - update frequency in hours  
!     ijmdl          - number of model grid points
!     me             - mpi task number
!     lsmask         - model landmask
!     nsoil          - number of soil layers
!     soil_type      - soil type - category
!     soil_temp      - soil temperature
!
!   output argument list:
!     soilm_liq      - liquid portion of soil moisture - volumetric
!     soilm_tot      - total (liq plus frozen) soil moisture - volumetric
!
! files: none
!
! condition codes: none
!
! remarks: none
!
!$$$
 use soil_utils, only          : calc_soil_parms

 use read_data,  only          : soilm_climo,       &
                                 merge_coeff_soilm, &
                                 num_soil_types,    &
                                 smcmax,            &
                                 beta,              &
                                 psis,              &
                                 smcmax,            &
                                 smclow,            &
                                 smchigh,           &
                                 satdk,             &
                                 burnt_frac_daily_anal

 implicit none

 integer                      :: ij
 integer, intent(in)          :: ijmdl
 integer                      :: k
 integer, intent(in)          :: me
 integer, intent(in)          :: nsoil
 integer, intent(in)          :: soil_type(ijmdl)

 real                         :: csoil
 real, intent(in)             :: fhcyc
 real                         :: ratio
 real, intent(in)             :: lsmask(ijmdl)
 real, allocatable            :: smcdry(:)
 real, allocatable            :: smcref(:)
 real, allocatable            :: smcwlt(:)
 real, intent(in)             :: soil_temp(ijmdl,nsoil)
 real, intent(inout)          :: soilm_liq(ijmdl,nsoil)
 real, intent(inout)          :: soilm_tot(ijmdl,nsoil)
 real                         :: soilm_tot_save(nsoil)

 do ij = 1,ijmdl
   if (lsmask(ij) == 0.0) then
     soilm_tot(ij,:) = 1.0    ! flag value for open water and sea ice
     soilm_liq(ij,:) = 1.0
   endif
 enddo

 if (allocated(soilm_climo) .or. allocated(burnt_frac_daily_anal))then

   allocate(smcref(num_soil_types))
   allocate(smcwlt(num_soil_types))
   allocate(smcdry(num_soil_types))

   call calc_soil_parms(smclow, smchigh, smcmax, beta,        &
                        satdk, psis, num_soil_types,          &
                        smcref, smcwlt, smcdry)
 endif

 if (allocated(soilm_climo)) then

   if (merge_coeff_soilm > 0.0) then
     csoil = exp(-fhcyc/24.0/merge_coeff_soilm)
   else
     csoil = 0.0
   end if

   print*,"- UPDATE SOIL MOISTURE BASED ON CLIMATOLOGY."

   do ij = 1, ijmdl

     if (lsmask(ij) == 1.0) then

       soilm_tot_save = soilm_tot(ij,:)

       soilm_tot(ij,:) = (csoil * soilm_tot(ij,:)) +  &
                        ((1.0-csoil) * soilm_climo(ij))

!-----------------------------------------------------------------------
!      ensure merged soil moisture is between the valid bounds.
!      the top layer uses smcdry as its lower threshold.
!-----------------------------------------------------------------------

       soilm_tot(ij,1) = max(soilm_tot(ij,1),smcdry(soil_type(ij)))
       soilm_tot(ij,1) = min(soilm_tot(ij,1),smcmax(soil_type(ij)))

       do k = 2, nsoil
         soilm_tot(ij,k) = max(soilm_tot(ij,k),smcwlt(soil_type(ij)))
         soilm_tot(ij,k) = min(soilm_tot(ij,k),smcmax(soil_type(ij)))
       enddo

!-----------------------------------------------------------------------
!      if the total moisture changes, need to recalculate the 
!      liquid portion.  according to guidance from ken, do NOT use
!      the noah lsm utility to do this.  instead keep the liq / total
!      ratio the same.
!-----------------------------------------------------------------------

       do k = 1, nsoil

         if (soil_temp(ij,k) < (frz_h20-0.001) ) then

           ratio =  soilm_liq(ij,k) / soilm_tot_save(k)
           soilm_liq(ij,k) = ratio * soilm_tot(ij,k)
 
         else

           soilm_liq(ij,k) = soilm_tot(ij,k)

         end if

       enddo

     end if

   enddo

   deallocate (soilm_climo)
 
 end if

 if (allocated (burnt_frac_daily_anal)) then
   print*,"- UPDATE SOIL MOISTURE BASED ON WILDFIRES."
   do ij = 1, ijmdl
     if (lsmask(ij) == 1.0) then
       if(burnt_frac_daily_anal(ij) > 0.0) then        !Fire effect
         soilm_tot(ij,1) = smcdry(soil_type(ij))
         soilm_liq(ij,1) = soilm_tot(ij,1)
       endif
     endif
   enddo
 endif

 if (allocated(smcref)) deallocate(smcref)
 if (allocated(smcwlt)) deallocate(smcwlt)
 if (allocated(smcdry)) deallocate(smcdry)

 return

 end subroutine process_soilm

 subroutine process_sst(skin_temp, seaice, lsmask, fhcyc, ijmdl, me)
!$$$  subprogram documentation block
!
! subprogram:    process_sst
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: this subroutine updates sst
!
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call this routine with the following argument:
!
!   input argument list:
!     fhcyc          - update frequency in hours  
!     ijmdl          - number of model grid points
!     me             - mpi task number
!     lsmask         - model landmask
!     seaice         - sea ice
!
!   output argument list:
!     skin_temp      - skin temperature (sst at water points)
!
! files: none
!
! condition codes: none
!
! remarks: run this routine after the sea ice is updated.
!
!$$$
 use read_data, only         :  sst_climo,        &
                                sst_climo_taum1,  &
                                sst_anal,         &
                                merge_coeff_sst,  &
                                sst_anlcyc_update

 implicit none

 integer                     :: ij
 integer, intent(in)         :: ijmdl
 integer, intent(in)         :: me

 real                        :: csst
 real, intent(in)            :: fhcyc
 real, intent(in)            :: lsmask(ijmdl)  ! 0-open water/ice; 1-land
 real, intent(in)            :: seaice(ijmdl) 
 real, intent(inout)         :: skin_temp(ijmdl)

!-----------------------------------------------------------------------
! set sst at points that were flipped from ice to open water.
! will be overwritten below if user chooses sst analysis.
!-----------------------------------------------------------------------

 if (allocated(seaice_fg)) then
   print*,"- SET SST AT FORMER SEA ICE POINTS."
   do ij = 1, ijmdl
     if (lsmask(ij) == 0.0 .and. seaice(ij) == 0.0 .and. &
         seaice_fg(ij) > 0.0) then
       skin_temp(ij) = frz_ice
     endif
   enddo
 endif

!-----------------------------------------------------------------------
! now blend sst with analysis or climo, if available.
!-----------------------------------------------------------------------

 if (merge_coeff_sst < 99999.) then

   if (merge_coeff_sst > 0.0) then
     csst = exp(-fhcyc/24.0/merge_coeff_sst)
   else
     csst = 0.0
   end if

!-----------------------------------------------------------------------
!  blending to an analysis or climo at open water points.
!-----------------------------------------------------------------------

   if (allocated(sst_anal)) then

     print*,"- UPDATE SST BASED ON NEW ANALYSIS DATA"

     do ij = 1, ijmdl
       if (lsmask(ij) == 0.0 .and. seaice(ij) == 0.0) then  ! open water
         skin_temp(ij) = (csst * skin_temp(ij)) + ((1.0-csst)*sst_anal(ij))
         skin_temp(ij) = max(skin_temp(ij),frz_ice)
       end if
     enddo

   elseif (allocated(sst_climo)) then

     print*,"- UPDATE SST BASED ON CLIMO DATA"

     do ij = 1, ijmdl
       if (lsmask(ij) == 0.0 .and. seaice(ij) == 0.0) then  ! open water
         skin_temp(ij) = (csst * skin_temp(ij)) + ((1.0-csst)*sst_climo(ij))
         skin_temp(ij) = max(skin_temp(ij),frz_ice)
       end if
     enddo

   end if

 else if (sst_anlcyc_update .and. allocated (sst_climo_taum1)) then

   print*,"- UPDATE SST BASED ON ANNUAL CYCLE"

   do ij = 1, ijmdl
     if (lsmask(ij) == 0.0 .and. seaice(ij) == 0.0) then  ! open water
       skin_temp(ij) = skin_temp(ij) + (sst_climo(ij) - sst_climo_taum1(ij))
       skin_temp(ij) = max(skin_temp(ij),frz_ice)
     end if
   enddo

 end if

 if (allocated(sst_anal))         deallocate (sst_anal)
 if (allocated(sst_climo))        deallocate (sst_climo)
 if (allocated(sst_climo_taum1))  deallocate (sst_climo_taum1)

 return

 end subroutine process_sst

 subroutine process_seaice(seaice, lsmask, ijmdl, me)
!$$$  subprogram documentation block
!
! subprogram:    process_seaice
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: this subroutine updates sea ice
!
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call this routine with the following argument:
!
!   input argument list:
!     ijmdl          - number of model grid points
!     me             - mpi task number
!     lsmask         - model landmask
!     seaice         - sea ice
!
!   output argument list:
!     seaice         - sea ice
!
! files: none
!
! condition codes: none
!
! remarks: none
!
!$$$

 use read_data, only            : seaice_anal,  &
                                  seaice_climo

 implicit none

 integer                       :: ij
 integer, intent(in)           :: ijmdl
 integer, intent(in)           :: me

 real, intent(in)              :: lsmask(ijmdl)  ! 0-open water/ice,  1-land
 real, intent(inout)           :: seaice(ijmdl)  ! 0-open water/land, 1-ice 

!-----------------------------------------------------------------------
! save sea ice at prior model time step for use later.
!-----------------------------------------------------------------------

 if (allocated(seaice_anal)) then

   allocate(seaice_fg(ijmdl))
   seaice_fg = seaice

   print*,"- UPDATE SEA ICE ACCORDING TO NEW ANALYSIS."

!-----------------------------------------------------------------------
! external sea ice information is in %.  convert it to the flag
! values used by the models.
!-----------------------------------------------------------------------

   do ij = 1, ijmdl
     if (lsmask(ij) == 0.0) then     ! open water or sea ice
       if ((seaice_anal(ij) >= icon_thresh))  then 
         seaice(ij) = 1.0
       else
         seaice(ij) = 0.0
       end if
     else     ! land
       seaice(ij) = 0.0
     end if
   enddo

 else if (allocated(seaice_climo)) then

   print*,"- UPDATE SEA ICE ACCORDING TO CLIMO."

   allocate(seaice_fg(ijmdl))
   seaice_fg = seaice

   do ij = 1, ijmdl
     if (lsmask(ij) == 0.0) then  ! open water or sea ice
       if ((seaice_climo(ij) >= icon_thresh)) then 
         seaice(ij) = 1.0
       else
         seaice(ij) = 0.0
       end if
     else    ! land
       seaice(ij) = 0.0
     end if
   enddo

 end if

 if (allocated(seaice_anal))   deallocate (seaice_anal)
 if (allocated(seaice_climo))  deallocate (seaice_climo)

 return

 end subroutine process_seaice

 subroutine process_soiltype(soil_type, lsmask, seaice, ijmdl, me)
!$$$  subprogram documentation block
!
! subprogram:    process_soiltype
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: adjust soil type for changes in sea ice.
!
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call this routine with the following argument:
!
!   input argument list:
!     ijmdl          - number of model grid points
!     me             - mpi task number
!     lsmask         - model landmask
!     seaice         - sea ice
!
!   output argument list:
!     soil_type      - soil type, category
!
! files: none
!
! condition codes: none
!
! remarks: none
!
!$$$
 use read_data, only          : soil_type_src

 implicit none

 integer                     :: ij
 integer, intent(in)         :: ijmdl
 integer, intent(in)         :: me
 integer, intent(inout)      :: soil_type(ijmdl)
 integer                     :: soil_type_ice
 integer                     :: soil_type_water

 real, intent(in)            :: lsmask(ijmdl)
 real, intent(in)            :: seaice(ijmdl)

 if (.not. allocated(seaice_fg)) return
 print*,"- ADJUST SOIL TYPE FOR SEA ICE CHANGES."

 if (trim(soil_type_src) == "zobler") then
   soil_type_ice   = 9
   soil_type_water = 0
 else if (trim(soil_type_src) == "statsgo") then
   soil_type_ice   = 0
   soil_type_water = 0
 end if

 do ij = 1, ijmdl
   if (seaice(ij) == 1.0) then      ! ice
     soil_type(ij) = soil_type_ice
   elseif (lsmask(ij) == 0.0 .and. seaice(ij) == 0.0) then  ! open water
     soil_type(ij) = soil_type_water
   end if
 enddo

 end subroutine process_soiltype

 subroutine process_vegtype(veg_type, lsmask, seaice, ijmdl, me)
!$$$  subprogram documentation block
!
! subprogram:    process_vegtype
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: adjust vegetation type for changes in sea ice.
!
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call this routine with the following argument:
!
!   input argument list:
!     ijmdl          - number of model grid points
!     me             - mpi task number
!     lsmask         - model landmask
!     seaice         - sea ice
!
!   output argument list:
!     veg_type       - vegetation type, category
!
! files: none
!
! condition codes: none
!
! remarks: none
!
!$$$

 use read_data, only          : veg_type_src

 implicit none

 integer                     :: ij
 integer, intent(in)         :: ijmdl
 integer, intent(in)         :: me
 integer, intent(inout)      :: veg_type(ijmdl)
 integer                     :: veg_type_ice
 integer                     :: veg_type_water

 real, intent(in)            :: lsmask(ijmdl)
 real, intent(in)            :: seaice(ijmdl)

 if (.not. allocated(seaice_fg)) return

 print*,"- ADJUST VEGETATION TYPE FOR SEA ICE CHANGES."

 if (trim(veg_type_src) == "sib") then  ! global model
   veg_type_ice   = 13
   veg_type_water = 0
 else
   veg_type_ice   = 0
   veg_type_water = 0
 end if

 do ij = 1, ijmdl
   if (seaice(ij) == 1.0) then     ! ice 
     veg_type(ij) = veg_type_ice  
   elseif (lsmask(ij) == 0.0 .and. seaice(ij) == 0.0) then  ! open water
     veg_type(ij) = veg_type_water
   end if
 enddo

 return

 end subroutine process_vegtype

 subroutine process_snow(snow_depth, snow_liq_equiv, lsmask, seaice,  &
                         fhcyc, ijmdl, me)
!$$$  subprogram documentation block
!
! subprogram:    process_snow
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: update snow
!
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call this routine with the following argument:
!
!   input argument list:
!     fhcyc          - frequency of update
!     ijmdl          - number of model grid points
!     me             - mpi task number
!     lsmask         - model landmask
!     seaice         - sea ice
!
!   output argument list:
!     snow_depth     - snow depth
!     snow_liq_equiv - liquid equivalent snow depth
!
! files: none
!
! condition codes: none
!
! remarks: none
!
!$$$
 use read_data, only           : snow_depth_anal,    &
                                 snow_depth_climo,   &
                                 snow_cover_anal,    &
                                 merge_coeff_snow_depth

 implicit none

 integer                      :: ij
 integer, intent(in)          :: ijmdl
 integer, intent(in)          :: me

 real                         :: csnow, hrsnol, trsnol
 real,  intent(in)            :: fhcyc
 real,  intent(in)            :: lsmask(ijmdl)
 real,  intent(in)            :: seaice(ijmdl)
 real,  intent(inout)         :: snow_liq_equiv(ijmdl)  ! water equivalent
 real,  intent(inout)         :: snow_depth(ijmdl)      ! depth

 update_snow = .false.

 if (merge_coeff_snow_depth < 99999.) then

   csnow=0.0
   hrsnol=0.0
   trsnol=0.0
   if (merge_coeff_snow_depth > 0.0) then
     csnow = exp(-fhcyc/24.0/merge_coeff_snow_depth)
   elseif (merge_coeff_snow_depth < -1.0) then
     hrsnol = 1.0 / abs(merge_coeff_snow_depth)
     trsnol = abs(merge_coeff_snow_depth)
   end if

!-----------------------------------------------------------------------
!  blending to an analysis or climo is allowed.  depths are in
!  meters.  convert to liquid equivalent using a 5:1 ratio.
!-----------------------------------------------------------------------

   if (allocated(snow_depth_anal)) then

     update_snow = .true.

     print*,"- UPDATE SNOW USING NEW ANALYSIS."

     if (merge_coeff_snow_depth < -1.0) then
       print*,"- USE ENVELOPE METHOD."
       do ij = 1, ijmdl
         if (lsmask(ij) == 1.0) then    ! land
           if (snow_depth(ij) < (hrsnol*snow_depth_anal(ij))) then
             snow_depth(ij) = hrsnol*snow_depth_anal(ij)
             snow_liq_equiv(ij) = snow_depth(ij) * 0.2 
           elseif (snow_depth(ij) > (trsnol*snow_depth_anal(ij))) then
             snow_depth(ij) = trsnol*snow_depth_anal(ij)
             snow_liq_equiv(ij) = snow_depth(ij) * 0.2 
           endif
         end if
       enddo
     else
       do ij = 1, ijmdl
         if (lsmask(ij) == 1.0) then    ! land
           snow_depth(ij) = (csnow * snow_depth(ij)) + ((1.0-csnow)*snow_depth_anal(ij))
           snow_liq_equiv(ij)     = snow_depth(ij) * 0.2   ! assume 5:1 ratio
         end if
       enddo
     endif

     deallocate (snow_depth_anal)

   elseif (allocated(snow_depth_climo)) then

     update_snow = .true.

     print*,"- UPDATE SNOW USING CLIMATOLOGY."

     do ij = 1, ijmdl
       if (lsmask(ij) == 1.0) then   ! land
         snow_depth(ij) = (csnow * snow_depth(ij)) + ((1.0-csnow)*snow_depth_climo(ij))
         snow_liq_equiv(ij)     = snow_depth(ij) * 0.2   ! assume 5:1 ratio
       end if
     enddo

     deallocate (snow_depth_climo)

   elseif (allocated(snow_cover_anal)) then

     update_snow = .true.

     print*,"- UPDATE SNOW USING SNOW COVER ANALYSIS."

     do ij = 1, ijmdl
       if (lsmask(ij) == 1.0) then   ! land
         if (snow_cover_anal(ij) > 50.0) then  ! 50% coverage
           snow_depth(ij) = max(snow_depth(ij),0.05) 
           snow_liq_equiv(ij) = max(snow_liq_equiv(ij),0.01) 
         else
           snow_depth(ij) = 0.0
           snow_liq_equiv(ij) = 0.0
         end if
       end if
     enddo

   end if

 end if

!-----------------------------------------------------------------------
! even if snow is being cycled (no new snow analysis), its needs
! to be adjusted for any changes in the land sea mask due to sea ice
! changes. i.e., set it to zero over water points. in nam, snow at 
! sea ice is set to a nominal depth in the noah lsm.
!-----------------------------------------------------------------------

 if (allocated(seaice_fg)) then   ! real-time or climo sea ice was applied.
   print*,"- ADJUST SNOW FOR SEA ICE CHANGES."
   do ij = 1, ijmdl   ! open water
     if (lsmask(ij) == 0.0) then  ! not land
       if (seaice(ij) == 0.0) then  ! open water
         snow_depth(ij)     = 0.0
         snow_liq_equiv(ij) = 0.0
       else  ! sea ice
         if (seaice_fg(ij) < 1.0) then  ! was previously open water.
           snow_depth(ij)     = 0.05     ! meters
           snow_liq_equiv(ij) = 0.01     ! meters
         endif
       endif
     endif
   enddo
 endif

 end subroutine process_snow

 subroutine qc_fire_data(snow_depth, lsmask, ijmdl, me)
!$$$  subprogram documentation block
!
! subprogram:    qc_fire_data
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: qc daily accumulated fire data using the
!   model snow depth.  if snow, then assume the fire 
!   data is wrong and set burned percentage to zero.
!           
! program history log:
! 2015-07-08  gayno    - initial version
!
! usage: call this routine with the following argument:
!
!   input argument list:
!     ijmdl          - number of model grid points
!     me             - mpi task number
!     lsmask         - model landmask
!     snow_depth     - model snow depth in meters
!
!   output argument list: none
!
! files: none
!
! condition codes: none
!
! remarks: none
!
!$$$

 use read_data, only          : burnt_frac_daily_anal

 implicit none

 integer,  intent(in)        :: ijmdl, me

 real, intent(in)            :: lsmask(ijmdl)
 real, intent(in)            :: snow_depth(ijmdl)

 integer                     :: ij

 if (.not. allocated(burnt_frac_daily_anal)) return

 print*,'- DONT ALLOW ACTIVE WILDFIRES AT POINTS WITH SNOW.'

 do ij = 1, ijmdl
   if (lsmask(ij) > 0.0 .and. snow_depth(ij) > 0.01) then
     if (burnt_frac_daily_anal(ij) > 0.0) then
       print*,'- RESET BURNED AREA TO ZERO PERCENT AT POINT: ', ij
       burnt_frac_daily_anal(ij) = 0.0
     endif
   endif
 enddo

 return

 end subroutine qc_fire_data

 subroutine process_soil_temp(soil_temp, substrate_temp, &
                              skin_temp, lsmask, seaice, &
                              ijmdl, nsoil, me)
!$$$  subprogram documentation block
!
! subprogram:    process_soil_temp
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: update ice temperature for changes in sea ice
!
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call this routine with the following argument:
!
!   input argument list:
!     ijmdl          - number of model grid points
!     me             - mpi task number
!     lsmask         - model landmask
!     nsoil          - number soil/ice layers
!     seaice         - sea ice
!     skin_temp      - skin/sst temperture
!
!   output argument list:
!     substrate_temp - bottom boundary condition, temperature
!     soil_temp      - soil/ice temperature
!
! files: none
!
! condition codes: none
!
! remarks: none
!
!$$$

 implicit none

 integer                      :: ij
 integer, intent(in)          :: ijmdl
 integer, intent(in)          :: me
 integer, intent(in)          :: nsoil

 real, intent(in)             :: lsmask(ijmdl)
 real, intent(in)             :: seaice(ijmdl)
 real, intent(out)            :: soil_temp(ijmdl,nsoil)
 real, intent(in)             :: skin_temp(ijmdl)
 real, intent(out)            :: substrate_temp(ijmdl)

!-----------------------------------------------------------------------
! soil temperature is always cycled over land and sea ice points.
! if new sea ice point, initalize layer temperature and lower thermal
! boundary temperature to freezing point for sea water.
!-----------------------------------------------------------------------

 if (allocated(seaice_fg)) then   ! real-time or climo sea ice was applied.
   print*,"- SET LAYER TEMPERATURES AT NEW SEA ICE POINTS."
   do ij = 1, ijmdl
     if (lsmask(ij) == 0.0 .and. seaice(ij) == 1.0) then  ! new sea ice
       if (seaice_fg(ij) < 1.0) then
         soil_temp(ij,:)    = frz_ice
         substrate_temp(ij) = frz_ice
       end if
     end if
     if (lsmask(ij) == 0.0 .and. seaice(ij) == 0.0) then      ! open water
       soil_temp(ij,:)    = 273.16  ! not used, use same filler value as noah lsm.
       substrate_temp(ij) = 280.0  ! not use, use filler value
     endif
   enddo
 endif

 return

 end subroutine process_soil_temp

 subroutine process_skint(skin_temp, snow_liq_equiv,   &
                          lsmask, seaice, veg_type, snup, &
                          num_veg_types, ijmdl, me)
!$$$  subprogram documentation block
!
! subprogram:    process_skint
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: update skin temperature for snow and ice changes
!
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call this routine with the following argument:
!
!   input argument list:
!     ijmdl          - number of model grid points
!     me             - mpi task number
!     num_veg_types  - # vegetation type categories
!     veg_type       - vegetation type
!     lsmask         - model landmask
!     seaice         - sea ice
!     snow_liq_equiv - liquild equivalent snow depth
!     snup           - "deep" snow threshold for each vegetation type
!
!   output argument list:
!     skin_temp      - skin temperture
!
! files: none
!
! condition codes: none
!
! remarks: Over deep snow over land, constrain the skin temp to freezing.
! Over patchy snow, skin t is allowed to exceed freezing in the
! noah model.
!
!$$$
 implicit none

 integer                      :: ij
 integer, intent(in)          :: ijmdl
 integer, intent(in)          :: me
 integer, intent(in)          :: num_veg_types
 integer, intent(in)          :: veg_type(ijmdl)

 real, intent(in)             :: lsmask(ijmdl)
 real, intent(in)             :: seaice(ijmdl)
 real, intent(in)             :: snow_liq_equiv(ijmdl) ! snow liq eq in meters
 real, intent(out)            :: skin_temp(ijmdl)
 real, intent(in)             :: snup(num_veg_types)

 if (update_snow) then
   print*,"- UPDATE SKIN TEMPERATURE FOR NEW SNOW."
   do ij = 1, ijmdl
     if (lsmask(ij) == 1.0 .and. snow_liq_equiv(ij) > 0.0) then
       if (snow_liq_equiv(ij) < snup(veg_type(ij))) then   ! patchy snow, noah allows
                                                           ! skin t to exceed 0C.
         skin_temp(ij) = min(skin_temp(ij), 305.0)
       else   ! deep snow
         skin_temp(ij) = min(skin_temp(ij), frz_h20)
       end if
     end if 
   enddo
 endif 

! set skin t at new sea ice points.

 if (allocated(seaice_fg)) then
   print*,"- SET SKIN TEMPERATURE AT NEW SEA ICE POINTS."
   do ij = 1, ijmdl
     if (lsmask(ij) == 0.0 .and. seaice(ij) == 1.0 .and. seaice_fg(ij) == 0.0) then
       skin_temp(ij) = frz_ice
     end if
   enddo
 endif

 return

 end subroutine process_skint

 subroutine process_snowfree_albedo_global (snowfree_albedo, albrecs, &
                                            lsmask, seaice, ijmdl, me)
!$$$  subprogram documentation block
!
! subprogram:    process_snowfree_albedo_global
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: update four-component snow-free albedo for gfs model
!
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call this routine with the following argument:
!
!   input argument list:
!     albrecs        - number of albedo components
!     ijmdl          - number of model grid points
!     me             - mpi task number
!     lsmask         - model landmask
!     seaice         - sea ice
!
!   output argument list:
!     snowfree_albedo - snow free albedo
!
! files: none
!
! condition codes: none
!
! remarks: none
!
!$$$
 use read_data, only            : alvsf_climo,        &
                                  alvwf_climo,        &
                                  alnsf_climo,        &
                                  alnwf_climo

 implicit none

 integer, intent(in)           :: albrecs
 integer                       :: ij
 integer, intent(in)           :: ijmdl
 integer, intent(in)           :: me

 real, intent(in)              :: lsmask(ijmdl)
 real, intent(in)              :: seaice(ijmdl)

 real, intent(inout)           :: snowfree_albedo(ijmdl,albrecs)

 print*,"- PROCESS ALBEDO."

!-----------------------------------------------------------------------
! just check if one array is allocated.  if the user selects climo
! albedo, then all four albedo arrays will be allocated.
!-----------------------------------------------------------------------

 if (allocated(alvsf_climo)) then

   snowfree_albedo(:,1) = alvsf_climo
   snowfree_albedo(:,2) = alvwf_climo
   snowfree_albedo(:,3) = alnsf_climo
   snowfree_albedo(:,4) = alnwf_climo

   deallocate(alvsf_climo)
   deallocate(alvwf_climo)
   deallocate(alnsf_climo)
   deallocate(alnwf_climo)

 end if

!-----------------------------------------------------------------------
! incorporate the range checking as is done in sfccycle.
!
! land type                    max        min
! ---------                   -----      -----
! bare land                    0.80       0.06
! open ocean                   0.06       0.06
! snow covered land            0.80       0.06
! bare sea ice                 0.80       0.06   ?? check this min
! snow covered sea ice         0.80       0.06   ?? check this min
!-----------------------------------------------------------------------

 do ij = 1, ijmdl

   if (lsmask(ij) == 0.0 .and. seaice(ij) == 0.0) then       ! open water
     snowfree_albedo(ij,1) = 0.06
     snowfree_albedo(ij,2) = 0.06
     snowfree_albedo(ij,3) = 0.06
     snowfree_albedo(ij,4) = 0.06
   elseif (lsmask(ij) == 1.0) then   ! land
     snowfree_albedo(ij,1) = max(snowfree_albedo(ij,1), 0.06)
     snowfree_albedo(ij,1) = min(snowfree_albedo(ij,1), 0.80)
     snowfree_albedo(ij,2) = max(snowfree_albedo(ij,2), 0.06)
     snowfree_albedo(ij,2) = min(snowfree_albedo(ij,2), 0.80)
     snowfree_albedo(ij,3) = max(snowfree_albedo(ij,3), 0.06)
     snowfree_albedo(ij,3) = min(snowfree_albedo(ij,3), 0.80)
     snowfree_albedo(ij,4) = max(snowfree_albedo(ij,4), 0.06)
     snowfree_albedo(ij,4) = min(snowfree_albedo(ij,4), 0.80)
   elseif (lsmask(ij) == 0.0 .and. seaice(ij) == 1.0) then   ! sea ice
     snowfree_albedo(ij,1) = 0.80
     snowfree_albedo(ij,2) = 0.80
     snowfree_albedo(ij,3) = 0.80
     snowfree_albedo(ij,4) = 0.80
   end if

 enddo

 return

 end subroutine process_snowfree_albedo_global

 subroutine process_snowfree_albedo_regional (snowfree_albedo, albrecs, &
                                              lsmask, seaice, ijmdl, me)
!$$$  subprogram documentation block
!
! subprogram:    process_snowfree_albedo_regional
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: update snow-free albedo for nmm runs.
!
! program history log:
! 2005-05-20  gayno    - initial version
! 2015-07-08  gayno    - reduce albedo at points affected
!                        by wild fires.
!
! usage: call this routine with the following argument:
!
!   input argument list:
!     albrecs        - number of albedo components
!     ijmdl          - number of model grid points
!     me             - mpi task number
!     lsmask         - model landmask
!     seaice         - sea ice
!
!   output argument list:
!     snowfree_albedo - snow free albedo
!
! files: none
!
! condition codes: none
!
! remarks:
!
! call this routine after sea ice has been updated.
!
! for nmm runs, a single albedo is used (unlike the gfs).
!
! over land, the albedo will be updated according to climo if
! user selects.  this is communicated to the routine by populating
! the snowfree_albedo_climo array.
!
! at non-land points, set the value based on whether it is 
! open water or sea ice.
!
!$$$
 use read_data, only            : snowfree_albedo_climo, &
                                  burnt_frac_monthly_anal

 implicit none

 integer, intent(in)           :: albrecs
 integer                       :: ij
 integer, intent(in)           :: ijmdl
 integer, intent(in)           :: me

 real, intent(in)              :: lsmask(ijmdl)
 real, intent(in)              :: seaice(ijmdl)
 real, intent(inout)           :: snowfree_albedo(ijmdl,albrecs)

!-----------------------------------------------------------------------
! if user selects, update the snow free albedo over land according to
! the date/time.
!-----------------------------------------------------------------------

 update_sfalb=.false.

 if (allocated(snowfree_albedo_climo)) then

   update_sfalb=.true.

   print*,"- UPDATE SNOWFREE ALBEDO OVER LAND"

   do ij = 1, ijmdl
     if (lsmask(ij) == 1.0) then  ! land points
       snowfree_albedo(ij,1) = snowfree_albedo_climo(ij)
     end if
   enddo

   if (allocated (burnt_frac_monthly_anal)) then
     print*,"- UPDATE SNOWFREE ALBEDO FOR WILDFIRES"
     do ij = 1, ijmdl
       if (lsmask(ij) == 1.0) then  ! land points
         if(burnt_frac_monthly_anal(ij) > 0.0) then
           snowfree_albedo(ij,1) = 0.5*snowfree_albedo(ij,1)
         endif
       endif
     enddo
   endif

   deallocate(snowfree_albedo_climo)

 end if

!-----------------------------------------------------------------------
! even if user does not want land values updated, need to account
! for sea ice/open water changes.
!-----------------------------------------------------------------------

 if (allocated(seaice_fg)) then
 print*,"- UPDATE SNOWFREE ALBEDO FOR SEAICE CHANGES"
 do ij = 1, ijmdl
   if (lsmask(ij) == 0.0 .and. seaice(ij) == 1.0) then      ! ice points
     snowfree_albedo(ij,1) = 0.65     ! as in noah
   else if (lsmask(ij) == 0.0 .and. seaice(ij) == 0.0) then ! open water points
     snowfree_albedo(ij,1) = 0.06     ! as in name
   end if
 enddo
 end if

 end subroutine process_snowfree_albedo_regional

 subroutine process_roughness(lsmask, seaice, z0, ijmdl, grid_type, me)
!$$$  subprogram documentation block
!
! subprogram:    process_roughness
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract: update roughness length
!
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call this routine with the following argument:
!
!   input argument list:
!     grid_type      - flag indicating whether this is a gfs or
!                      nam run
!     ijmdl          - number of model grid points
!     me             - mpi task number
!     lsmask         - model landmask
!     seaice         - sea ice
!
!   output argument list:
!     z0             - roughness length
!
! files: none
!
! condition codes: none
!
! remarks: 
!
!   update roughness length over land based on climo.
!
!   use a standard roughness value over sea ice.
!
!   use the first guess roughness over water because the models
!   calculate roughness based on charnock's constant.
!
!   note gfs expects units of cm, nmm expects units of meters.
!
!$$$

 use read_data, only         : z0_climo

 implicit none

 character*8, intent(in)    :: grid_type

 integer                    :: ij
 integer, intent(in)        :: ijmdl
 integer, intent(in)        :: me

 real, intent(in)           :: lsmask(ijmdl)
 real                       :: scalefac  ! convert m to cm
 real, intent(in)           :: seaice(ijmdl)
 real, intent(inout)        :: z0(ijmdl)
 real                       :: z0_ice
 real                       :: z0_min_land
 real                       :: z0_sea

 real, parameter            :: z0_ice_global   = 1.0   ! cm
 real, parameter            :: z0_sea_global   = 1.0   ! cm
 real, parameter            :: z0_ice_regional = 0.001 ! m
 real, parameter            :: z0_sea_regional = 0.001 ! m

 if (trim(grid_type) == "global") then
   scalefac    = 100.0
   z0_ice      = z0_ice_global
   z0_sea      = z0_sea_global
   z0_min_land = 2.0 ! cm
 elseif (trim(grid_type) == "regional") then
   scalefac    = 1.0
   z0_ice      = z0_ice_regional
   z0_sea      = z0_sea_regional
   z0_min_land = 0.02 ! m
 end if

!-----------------------------------------------------------------------
! over land, use time interpolated climo.
!-----------------------------------------------------------------------

 if (allocated(z0_climo)) then

   print*,"- SET ROUGHNESS BASED ON INTERPOLATED CLIMO."

   do ij = 1, ijmdl
     if (lsmask(ij) == 1.0) then   ! land
       z0(ij) = max( (z0_climo(ij)*scalefac), z0_min_land)
     end if
   enddo

   deallocate(z0_climo)

 end if

!-----------------------------------------------------------------------
! over water, the model calculates roughness, so cycle what
! was there previously.  however, if point was previously sea ice,
! set to a "seed" value for water.
!-----------------------------------------------------------------------

 if (allocated(seaice_fg)) then
   print*,"- SET ROUGHNESS SEA ICE CHANGES."
   do ij = 1, ijmdl
     if (lsmask(ij) == 0.0 .and. seaice(ij) == 0.0) then
       if (seaice_fg(ij) > 0.0) then
         z0(ij) = z0_sea  ! former ice point, set to seed value.
       end if
     end if
     if (lsmask(ij) == 0.0 .and. seaice(ij) == 1.0) then
       z0(ij) = z0_ice   ! default value used in noah.
     endif
   enddo
 endif

 return

 end subroutine process_roughness

 end module process_data
