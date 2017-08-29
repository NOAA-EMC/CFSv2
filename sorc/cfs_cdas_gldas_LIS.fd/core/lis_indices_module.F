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
! !MODULE: lis_indices_module.F90
! 
! !DESCRIPTION: 
!   This module contains generic indices for performing loops over
!   rows and columns.  Depending on how LIS is run (e.g., when using
!   a GrADS-DODS server (GDS) or not), the number of rows and columns for 
!   a given domain may change.  This module determines how LIS is run
!   and which set of indices to use.
!   
! !REVISION HISTORY: 
!  08 Apr 2004  James Geiger; Initial Specification
! 
!EOP
module lis_indices_module

   use lisdrv_module, only : lis

   implicit none

   integer :: lis_nc_working ! number of longitude points for the running domain
   integer :: lis_nr_working ! number of latitude points for the running domain

   integer :: lis_nc_data ! number of longitude points for the paramater 
                          ! data domain
   integer :: lis_nr_data ! number of latitude points for the paramater 
                          ! data domain

   integer :: lis_tnroffset ! offset used to map a tile's global row number
                            ! to its local value
                            !
                            ! For a given process, say its patch of tiles
                            ! begins at global row number 18.  All local
                            ! arrays for this process will begin with an index
                            ! of 1.  This offset is used to adjust any
                            ! tile(i)%row references to local indices.
                            !
                            ! For example:
              ! noah(i)%albsf1 = value1(tile(i)%col, tile(i)%row-lis_tnroffset)
                            !
                            ! Currently there is no need for a column offset.

   integer :: lis_grid_offset ! offset used to map a tile's global grid index
                              ! to its local value
                              !
                              ! See lis_tnroffset above.

   integer :: lis_g2l_row_offset ! offset used to map a tile's global row
                                 ! index into its sub-domain row index 
         !  ( running_southern_lat - parameter_data_southern_lat ) / resolution
                                 ! This offset take lis_tnroffset into account.

   integer :: lis_g2l_col_offset ! offset used to map a tile's global column
                                 ! index into its sub-domain column index
         !  ( running_western_lon - parameter_data_western_lon ) / resolution

contains

!BOP
! !ROUTINE: lis_set_indices
!
! !DESCRIPTION: 
!    This routine sets the generic indices -- lis\_nc\_working, 
!    lis\_nr\_working, lis\_tnroffset, lis\_nc\_data, lis\_nr\_data,
!    lis\_g2l\_row\_offset, and lis\_g2l\_col\_offset
!    -- depending on how the LIS executable was compiled.
!
! !INTERFACE: 
subroutine lis_set_indices()
!EOP

#if ( defined OPENDAP )
   use opendap_module, only : parm_nc, parm_nr, tnroffset, grid_offset
#endif

   implicit none

#if ( defined OPENDAP )
   lis_nc_working  = parm_nc
   lis_nr_working  = parm_nr

   lis_nc_data     = parm_nc
   lis_nr_data     = parm_nr

   lis_tnroffset   = tnroffset
   lis_grid_offset = grid_offset
#else
      lis_nc_working  = lis%d%lnc
      lis_nr_working  = lis%d%lnr
      lis_tnroffset   = 0
      lis_grid_offset = 0
!   if(lis%d%domain.eq.8) then 
!    if(lis%d%gridDesc(9) .eq. 0.01) then 
      lis_nc_data     = lis%d%lnc
      lis_nr_data     = lis%d%lnr
!   else
!      lis_nc_data     = lis%d%gnc
!      lis_nr_data     = lis%d%gnr
!   endif
#endif

   lis_g2l_row_offset = lis_global_to_local_row_offset(lis_tnroffset)
   lis_g2l_col_offset = lis_global_to_local_col_offset()

   print*,'DBG: lis_set_indices -- lis_nc_working',lis_nc_working
   print*,'DBG: lis_set_indices -- lis_nr_working',lis_nr_working
   print*,'DBG: lis_set_indices -- lis%d%lnc',lis%d%lnc
   print*,'DBG: lis_set_indices -- lis%d%lnr',lis%d%lnr
   print*,'DBG: lis_set_indices -- lis_nc_data',lis_nc_data
   print*,'DBG: lis_set_indices -- lis_nr_data',lis_nr_data
   print*,'DBG: lis_set_indices -- lis%d%gnc',lis%d%gnc
   print*,'DBG: lis_set_indices -- lis%d%gnr',lis%d%gnr
   print*,'DBG: lis_set_indices -- lis_tnroffset',lis_tnroffset
   print*,'DBG: lis_set_indices -- lis_grid_offset',lis_grid_offset
#if ( defined OPENDAP )
   print*,'DBG: lis_set_indices -- parm_nc',parm_nc
   print*,'DBG: lis_set_indices -- parm_nr',parm_nr
   print*,'DBG: lis_set_indices -- tnroffset',tnroffset
   print*,'DBG: lis_set_indices -- grid_offset',grid_offset
#endif

end subroutine lis_set_indices

!BOP
! !ROUTINE: lis_get_run_slat
!
! !DESCRIPTION: 
!    This routine returns the value of the southern latitude for the
!    running domain.
!
! !INTERFACE: 
function lis_get_run_slat()
!EOP

   implicit none

   real :: lis_get_run_slat

   lis_get_run_slat = lis%d%gridDesc(4)

   return

end function lis_get_run_slat

!BOP
! !ROUTINE: lis_get_run_wlon
!
! !DESCRIPTION: 
!    This routine returns the value of the western longitude for the
!    running domain.
!
! !INTERFACE: 
function lis_get_run_wlon()
!EOP

   implicit none

   real :: lis_get_run_wlon

   lis_get_run_wlon = lis%d%gridDesc(5)

   return

end function lis_get_run_wlon

!BOP
! !ROUTINE: lis_get_run_nlat
!
! !DESCRIPTION: 
!    This routine returns the value of the northern latitude for the
!    running domain.
!
! !INTERFACE: 
function lis_get_run_nlat()
!EOP

   implicit none

   real :: lis_get_run_nlat

   lis_get_run_nlat = lis%d%gridDesc(7)

   return

end function lis_get_run_nlat

!BOP
! !ROUTINE: lis_get_run_elon
!
! !DESCRIPTION: 
!    This routine returns the value of the eastern longitude for the
!    running domain.
!
! !INTERFACE: 
function lis_get_run_elon()
!EOP

   implicit none

   real :: lis_get_run_elon

   lis_get_run_elon = lis%d%gridDesc(8)

   return

end function lis_get_run_elon

!BOP
! !ROUTINE: lis_get_run_lat_res
!
! !DESCRIPTION: 
!    This routine returns the value of the resolution (latitude) for the
!    running domain.
!
! !INTERFACE: 
function lis_get_run_lat_res()
!EOP

   implicit none

   real :: lis_get_run_lat_res

   lis_get_run_lat_res = lis%d%gridDesc(9)

   return

end function lis_get_run_lat_res

!BOP
! !ROUTINE: lis_get_run_lon_res
!
! !DESCRIPTION: 
!    This routine returns the value of the resolution (longitude) for the
!    running domain.
!
! !INTERFACE: 
function lis_get_run_lon_res()
!EOP

   implicit none

   real :: lis_get_run_lon_res

   lis_get_run_lon_res = lis%d%gridDesc(10)

   return

end function lis_get_run_lon_res

!BOP
! !ROUTINE: lis_get_data_slat
!
! !DESCRIPTION: 
!    This routine returns the value of the southern latitude for the
!    parameter data domain.
!
! !INTERFACE: 
function lis_get_data_slat()
!EOP

   implicit none

   real :: lis_get_data_slat

   lis_get_data_slat = lis%d%gridDesc(44)

   return

end function lis_get_data_slat

!BOP
! !ROUTINE: lis_get_data_wlon
!
! !DESCRIPTION: 
!    This routine returns the value of the western longitude for the
!    parameter data domain.
!
! !INTERFACE: 
function lis_get_data_wlon()
!EOP

   implicit none

   real :: lis_get_data_wlon

   lis_get_data_wlon = lis%d%gridDesc(45)

   return

end function lis_get_data_wlon

!BOP
! !ROUTINE: lis_get_data_nlat
!
! !DESCRIPTION: 
!    This routine returns the value of the northern latitude for the
!    parameter data domain.
!
! !INTERFACE: 
function lis_get_data_nlat()
!EOP

   implicit none

   real :: lis_get_data_nlat

   lis_get_data_nlat = lis%d%gridDesc(47)

   return

end function lis_get_data_nlat

!BOP
! !ROUTINE: lis_get_data_elon
!
! !DESCRIPTION: 
!    This routine returns the value of the eastern longitude for the
!    parameter data domain.
!
! !INTERFACE: 
function lis_get_data_elon()
!EOP

   implicit none

   real :: lis_get_data_elon

   lis_get_data_elon = lis%d%gridDesc(48)

   return

end function lis_get_data_elon

!BOP
! !ROUTINE: lis_get_data_lat_res
!
! !DESCRIPTION: 
!    This routine returns the value of the resolution (latitude) for the
!    parameter data domain.
!
! !INTERFACE: 
function lis_get_data_lat_res()
!EOP

   implicit none

   real :: lis_get_data_lat_res

   lis_get_data_lat_res = lis%d%gridDesc(49)

   return

end function lis_get_data_lat_res

!BOP
! !ROUTINE: lis_get_data_lon_res
!
! !DESCRIPTION: 
!    This routine returns the value of the resolution (longitude) for the
!    parameter data domain.
!
! !INTERFACE: 
function lis_get_data_lon_res()
!EOP

   implicit none

   real :: lis_get_data_lon_res

   lis_get_data_lon_res = lis%d%gridDesc(50)

   return

end function lis_get_data_lon_res

!BOP
! !ROUTINE: lis_global_to_local_row_offset
!
! !DESCRIPTION: 
!    This routine returns the offset needed to adjust a global row index
!    value into its corresponding local row index value.
!
! !INTERFACE: 
function lis_global_to_local_row_offset(offset)
!EOP

   implicit none

   real :: lis_global_to_local_row_offset
   integer, intent(in) :: offset

   lis_global_to_local_row_offset =                        &
      nint( ( lis_get_run_slat() - lis_get_data_slat() ) / &
              lis_get_run_lat_res() ) - offset

   return

end function lis_global_to_local_row_offset

!BOP
! !ROUTINE: lis_global_to_local_col_offset
!
! !DESCRIPTION: 
!    This routine returns the offset needed to adjust a global column index
!    value into its corresponding local column index value.
!
! !INTERFACE: 
function lis_global_to_local_col_offset()
!EOP

   implicit none

   real :: lis_global_to_local_col_offset

   lis_global_to_local_col_offset =                        &
      nint( ( lis_get_run_wlon() - lis_get_data_wlon() ) / &
              lis_get_run_lon_res() )

   return

end function lis_global_to_local_col_offset

end module lis_indices_module
