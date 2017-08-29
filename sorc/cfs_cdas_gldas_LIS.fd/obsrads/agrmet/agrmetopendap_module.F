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
! !MODULE: agrmetopendap_module.F90
! 
! !DESCRIPTION: 
!
!  This module contains routines needed to initialize and control variables
! required for the execution of GDS-based I/O specfific to AGRMET forcing
! routines
!
! !REVISION HISTORY:
! 29 Apr 2004: James Geiger;  Initial Specification 
!
! !INTERFACE:
module agrmetopendap_module
#if ( defined OPENDAP )
! !USES:
  use ESMF_TimeMgmtMod
  use lisdrv_module,       only : lis
  use spmdMod,             only : iam
  use agrmetdomain_module, only : agrmetdrv
  use opendap_module,      only : opendap_data_prefix, ciam

  implicit none
   type(esmf_date), save :: agrmet_ref_date !Reference date for AGRMET forcing data
   integer :: agrmet_time_index

   contains

!BOP
! !ROUTINE: opendap_agrmet_init
! 
! !DESCRIPTION:
!  Initializes the AGRMET-GDS variables
!
! !INTERFACE:
   subroutine opendap_agrmet_init()
!EOP
     implicit none
!BOC       
     !call init_agrmet_vars()
     call reset_agrmet_filepaths()
!EOC       
   end subroutine opendap_agrmet_init

!BOP
! !ROUTINE: reset_agrmet_filepaths
! 
! !DESCRIPTION:
!  Resets input data filenames for AGRMET forcing for execution through GDS
!
! !INTERFACE:
     subroutine reset_agrmet_filepaths()
!EOP
       implicit none
!BOC
       agrmetdrv%agrmetdir  = trim(opendap_data_prefix)//'/'// &
                              trim(adjustl(ciam))//'/'//agrmetdrv%agrmetdir
!EOC
     end subroutine reset_agrmet_filepaths

!BOP
! !ROUTINE: init_agrmet_ref_date
! 
! !DESCRIPTION:  
! Initializes the reference date for AGRMET forcing data
! 
! !INTERFACE:
   subroutine init_agrmet_ref_date()
!EOP
      implicit none

      integer :: ref_ymd = 20010301 ! 0z01mar2001
      integer :: ref_tod = 0
      integer :: rc
!BOC
      agrmet_ref_date = esmf_dateinit(esmf_no_leap, ref_ymd, ref_tod, rc)
!EOC
    end subroutine init_agrmet_ref_date

!BOP
! !ROUTINE: set_agrmet_index
!
! !DESCRIPTION: 
! Computes the time-based index for AGRMET forcing data
! 
! !INTERFACE:
   subroutine set_agrmet_index(offset)
     implicit none
! !INPUT PARAMETERS:
     integer, intent(in) :: offset   ! offset from current date in hours
!EOP
     type(esmf_date) :: current_date
     type(esmf_time) :: diff
     logical :: islater
     integer :: rc
     integer :: ymd, tod
     integer :: ndays, nsecs
     logical, save :: ref_data_uninit = .true.
!BOC     
     if ( ref_data_uninit ) then
        print*,'DBG: set_agrmet_index -- initializing ref date',' (', iam, ')'
        call init_agrmet_ref_date()
        ref_data_uninit = .false.
     endif
     ymd = ( lis%t%yr * 10000 ) + ( lis%t%mo * 100 ) + lis%t%da
     tod = ( lis%t%hr *  3600 ) + ( lis%t%mn *  60 ) + lis%t%ss
     current_date = esmf_dateinit(esmf_no_leap, ymd, tod, rc)
     
     diff = esmf_timeinit()
     call esmf_datediff(current_date, agrmet_ref_date, diff, islater, rc)
     call esmf_timeget(diff, ndays, nsecs, rc)
     
     agrmet_time_index = ( (ndays * 24) + (nsecs / 3600) + offset ) + 1
     print*,'DBG: set_agrmet_index -- agrmet_time_index = ', &
            agrmet_time_index, ' (', iam, ')'
!EOC     
   end subroutine set_agrmet_index
#endif
end module agrmetopendap_module
