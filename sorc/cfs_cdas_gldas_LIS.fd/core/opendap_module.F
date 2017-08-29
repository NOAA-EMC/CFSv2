!-------------------------------------------------------------------------
! NASA Goddard Space Flight Center Land Information System (LIS) V3.0
! Released May 2004
!
! See SOFTWARE DISTRIBUTION POLICY for software distribution policies
!
! The LIS source code and documentation are in the public domain,
! available without fee for educational, research, non-commercial and
! commercial purposes.  Users may distribute the binary or source
! code to third parties provided this statement appears on all copies and
! that no charge is made for such copies.
!
! NASA GSFC MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE
! SOFTWARE FOR ANY PURPOSE.  IT IS PROVIDED AS IS WITHOUT EXPRESS OR
! IMPLIED WARRANTY.  NEITHER NASA GSFC NOR THE US GOVERNMENT SHALL BE
! LIABLE FOR ANY DAMAGES SUFFERED BY THE USER OF THIS SOFTWARE.
!
! See COPYRIGHT.TXT for copyright details.
!
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: opendap_module.F90
! 
! !DESCRIPTION: 
!
!  This module contains routines needed to initialize and control variables
! required for the execution of GDS-based I/O
!
! !REVISION HISTORY:
! 10 Jul 2003; James Geiger  Initial Specification 
!
! !INTERFACE:
module opendap_module
#if ( defined OPENDAP )
! !USES:
   use ESMF_TimeMgmtMod
   use time_manager
   use lisdrv_module,  only : lis, grid, gindex
   use grid_spmdMod,   only : gdi, gdisp
   use tile_spmdMod,   only : di_array
   use spmdMod

   implicit none
! !ARGUMENTS:
   integer      :: tnroffset    ! Global to local row mapping offset
   integer      :: grid_offset  ! Global to local grid mapping offset
   real         :: output_slat  ! Southern latitude boundary for the 
                                ! interpolated domain
   real         :: output_nlat  ! Northern latitude boundary for the 
                                ! interpolated domain
   real         :: output_wlon  ! Western longitude boundary for the 
                                ! interpolated domain
   real         :: output_elon  ! Eastern longitude boundary for the 
                                ! interpolated domain
   integer      :: parm_slat    ! Southern latitude boundary for each 
                                ! subdomain (for parameter data)
   integer      :: parm_nlat    ! Northern latitude boundary for each 
                                ! subdomain (for parameter data)
   integer      :: parm_wlon    ! Western longitude boundary for each 
                                ! subdomain (for parameter data)
   integer      :: parm_elon    ! Eastern longitude boundary for each 
                                ! subdomain (for parameter data)
   integer      :: parm_nc      ! Number of columns for each subdomain
                                ! (for parameter data)
   integer      :: parm_nr      ! Number of rows for each subdomain 
                                ! (for parameter data)
      integer :: domain

   character*3  :: ciam         ! character representation of processor id
   character*3  :: cdom         ! character representation of domain no
   character*5  :: cparm_slat   ! character representation of cparm$_-$slat
   character*5  :: cparm_nlat   ! character representation of cparm$_-$nlat
   character*5  :: cparm_wlon   ! character representation of cparm$_-$wlon
   character*5  :: cparm_elon   ! character representation of cparm$_-$elon
   character*40 :: opendap_data_prefix ! prefix to prepend to data paths

!EOP
 contains
!BOP
! !ROUTINE: opendap_init
! 
! !DESCRIPTION:
!  Initializes the GDS variables
!
! !INTERFACE:
   subroutine opendap_init()
!EOP
     implicit none
!BOC       
     call init_parm_vars()
     call reset_lis_filepaths()
!EOC       
   end subroutine opendap_init

!BOP
! !ROUTINE: opendap_readcard
! 
! !DESCRIPTION:
! Reads the opendap namelist from the lis.crd card file 
!
! !INTERFACE:
   subroutine opendap_readcard()
!EOP
      implicit none
      namelist /opendap/opendap_data_prefix
!BOC
      open(10,file='lis.crd',form='formatted',status='old')
      read(unit=10,nml=opendap)
      print*,'MSG: opendap_readcard -- Using data prefix ', &
             opendap_data_prefix, ' (', iam, ')'
      close(10)
!EOC
   end subroutine opendap_readcard

!BOP
! !ROUTINE: reset_lis_filepaths
! 
! !DESCRIPTION:
!  Resets various input data filenames for execution through GDS
!
! !INTERFACE:
   subroutine reset_lis_filepaths()
!EOP
      implicit none
!BOC
      lis%p%clfile       = trim(opendap_data_prefix)//'/'// &
                           trim(adjustl(ciam))//'/'//lis%p%clfile
      lis%p%safile       = trim(opendap_data_prefix)//'/'// &
                           trim(adjustl(ciam))//'/'//lis%p%safile
      lis%p%iscfile      = trim(opendap_data_prefix)//'/'// &
                           trim(adjustl(ciam))//'/'//lis%p%iscfile
!EOC
    end subroutine reset_lis_filepaths
!BOP
! !ROUTINE: init_parm_vars
! 
! !DESCRIPTION:
! 
! Computes domain decomposition for native as well as interpolated domains for input data
! 
! !INTERFACE:
    subroutine init_parm_vars()
!EOPI
      use lisdrv_module, only: tile
      implicit none

      integer :: res, center, s_origin, w_origin
      character*5 :: line
!BOC
      if(lis%d%gridDesc(9) .eq. 0.01) then 
         domain = 8 
      elseif(lis%d%gridDesc(9).eq.0.05) then 
         domain = 7
      elseif(lis%d%gridDesc(9) .eq. 0.125) then 
         domain = 6
      elseif(lis%d%gridDesc(9) .eq. 0.25) then 
         domain = 5
      elseif(lis%d%gridDesc(9) .eq. 0.50) then 
         domain = 4
      elseif(lis%d%gridDesc(9) .eq. 1.0) then      
         domain = 3
      elseif((lis%d%gridDesc(9) .eq. 2) .and.  &
           (lis%d%gridDesc(10) .eq. 2.5)) then 
         domain = 2
      endif

      select case (domain)
      case(1)
         print*,'Error!  Cannot handle nldas.'
         stop 999
      case(2)
         print*,'Error!  Cannot handle 2x2.5.'
         stop 999
      case(3)  ! 1 deg
         res = 1000
         center = 500
         s_origin = -59500
         w_origin = -179500
      case(4) ! 1/2 deg
         res = 500
         center = 750
         s_origin = -59750
         w_origin = -179750
      case(5) ! 1/4 deg
         res = 250
         center = 875
         s_origin = -59875
         w_origin = -179875
      case(6) ! 5km
         res = 50
         center = 975
         s_origin = -59975
         w_origin = -179975
      case(7) ! 5km
         res = 50
         center = 975
         s_origin = -59975
         w_origin = -179975
      case(8) ! 1km
         res = 10
         center = 995
         s_origin = -59995
         w_origin = -179995
      case DEFAULT
         print*, "Select domain size (1,2,3,4,5,6,7,8)"
         stop 999
      end select
      
      call set_parm_lat(parm_slat,parm_nlat,output_slat,output_nlat,&
                        parm_wlon,parm_elon,output_wlon,output_elon,&
                        res,s_origin,w_origin)
      !parm_wlon = 1
      !parm_elon = lis%d%gnc
      !parm_wlon = 1 + lis%d%gnc * (lis%d%ic - 1)
      !parm_elon = lis%d%gnc + lis%d%gnc * (lis%d%ic - 1)
      
      lis%d%ngrid = gdi(iam)
      lis%d%nch   = di_array(iam)
      
      !parm_nc    = lis%d%gnc
      parm_nc    = ( parm_elon - parm_wlon + 1 ) 
      parm_nr    = ( parm_nlat - parm_slat + 1 ) 
      
      if ( lis%d%ir > 1 ) then ! running the special 1km regional domain
         tnroffset  = parm_slat - 1 - (lis%d%ir - 1)*lis%d%gnr
      else ! running the ``normal'' way
         tnroffset  = parm_slat - 1
      endif
      grid_offset = tile(1)%index-1

      write(ciam, '(i3)') iam
      write(cdom, '(i3)') domain
      write(cparm_slat, '(i5)') parm_slat
      write(cparm_nlat, '(i5)') parm_nlat
      write(cparm_wlon, '(i5)') parm_wlon
      write(cparm_elon, '(i5)') parm_elon

      print*,'DBG: parm_init -- parm_slat', parm_slat, ' (', iam, ')'
      print*,'DBG: parm_init -- parm_nlat', parm_nlat, ' (', iam, ')'
      print*,'DBG: parm_init -- parm_wlon', parm_wlon, ' (', iam, ')'
      print*,'DBG: parm_init -- parm_elon', parm_elon, ' (', iam, ')'
      print*,'DBG: parm_init -- output_slat', output_slat, ' (', iam, ')'
      print*,'DBG: parm_init -- output_nlat', output_nlat, ' (', iam, ')'
      print*,'DBG: parm_init -- output_wlon', output_wlon, ' (', iam, ')'
      print*,'DBG: parm_init -- output_elon', output_elon, ' (', iam, ')'
      print*,'DBG: parm_init -- ngrid', lis%d%ngrid, ' (', iam, ')'
      print*,'DBG: parm_init -- glbngrid', lis%d%glbngrid, ' (', iam, ')'
      print*,'DBG: parm_init -- lnc', lis%d%lnc, ' (', iam, ')'
      print*,'DBG: parm_init -- lnr', lis%d%lnr, ' (', iam, ')'
      print*,'DBG: parm_init -- gnc', lis%d%gnc, ' (', iam, ')'
      print*,'DBG: parm_init -- gnr', lis%d%gnr, ' (', iam, ')'
      print*,'DBG: parm_init -- tnroffset', tnroffset, ' (', iam, ')'
      print*,'DBG: parm_init -- grid_offset', grid_offset, ' (', iam, ')'
      print*,'DBG: parm_init -- nch', lis%d%nch, ' (', iam, ')'
      print*,'DBG: parm_init -- ngrid', lis%d%ngrid, ' (', iam, ')'
      print*,'DBG: parm_init -- glbnch', lis%d%glbnch, ' (', iam, ')'
      print*,'DBG: parm_init -- glbngrid', lis%d%glbngrid, ' (', iam, ')'
      print*,'DBG: parm_init -- cparm_slat ', cparm_slat, ' (', iam, ')'
      print*,'DBG: parm_init -- cparm_nlat ', cparm_nlat, ' (', iam, ')'
      print*,'DBG: parm_init -- ciam ', ciam, ' (', iam, ')'
      print*,'DBG: parm_init -- parm_nc', parm_nc, ' (', iam, ')'
      print*,'DBG: parm_init -- parm_nr', parm_nr, ' (', iam, ')'
      print*,'DBG: parm_init -- gdi(iam)', gdi(iam), ' (', iam, ')'
      print*,'DBG: parm_init -- tile(1)%row', tile(1)%row, ' (', iam, ')'
      print*,'DBG: parm_init -- tile(1)%col', tile(1)%col, ' (', iam, ')'
      print*,'DBG: parm_init -- tile(gdi(iam))%row', tile(gdi(iam))%row, &
             ' (', iam, ')'
      print*,'DBG: parm_init -- tile(gdi(iam))%col', tile(gdi(iam))%col, &
             ' (', iam, ')'

      call define_gds(lis)
!EOC
    end subroutine init_parm_vars


!BOP
! !ROUTINE: set_parm_lat
! 
! !DESCRIPTION:
! Computes the latitudes of the decomposed domain for the parameter data
! 
! !INTERFACE:
   subroutine set_parm_lat(slat, nlat, oslat, onlat, &
                           wlon, elon, owlon, oelon, &
                           res, s_origin, w_origin)
! !USES:
     use lisdrv_module, only : tile
     implicit none
! !INPUT PARAMETERS:
     integer, intent(in) :: res, s_origin, w_origin

! !OUTPUT PARAMETERS:
     integer, intent(out) :: slat, nlat, wlon, elon
     real, intent(out)    :: oslat, onlat, owlon, oelon
!EOP
!BOC
      ! Set southern latitude index
      !slat = tile( 1 )%row
      slat = tile( 1 )%row  + (lis%d%ir-1)*lis%d%gnr

      ! Set southern latitude boundary
      oslat = s_origin + (slat-1)*res
      oslat = oslat / 1000.

      ! Set northern latitude index
      !nlat = tile( gdi(iam) )%row
      nlat = tile( gdi(iam) )%row  + (lis%d%ir-1)*lis%d%gnr

      ! Set northern latitude boundary
      onlat = s_origin + (nlat-1)*res
      onlat = onlat / 1000.

      ! Set western longitude index
      wlon = 1 + lis%d%gnc * (lis%d%ic - 1)

      ! Set western longitude boundary
      owlon = w_origin + (wlon-1)*res
      owlon = owlon / 1000.

      ! Set eastern longitude index
      elon = lis%d%gnc + lis%d%gnc * (lis%d%ic - 1)

      ! Set eastern longitude boundary
      oelon = w_origin + (elon-1)*res
      oelon = oelon / 1000.

    end subroutine set_parm_lat
#endif
!EOC
  end module opendap_module
