!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE: noah_varder.F90
!
! !DESCRIPTION:
!  Module for 1-D NOAH land model driver variable initialization
!
! !REVISION HISTORY:
! Apr 2003; Sujay Kumar, Initial Code
!
! !INTERFACE:
module noah_varder
! !USES:        
  use noah_module
  use tile_spmdMod
  use noahpardef_module
  use noahdrv_module
!EOP  
  type(noahdrvdec) :: noahdrv
  type(noahdec), allocatable :: noah(:)
  SAVE
contains
!BOP
! 
! !ROUTINE: noah_varder_ini
! 
! !DESCRIPTION:        
! Reads in runtime noah parameters, allocates memory for variables
! 
! !INTERFACE:
  subroutine noah_varder_ini(nch)
! !USES:
#if ( defined OPENDAP )
    use opendap_module
#endif
!EOP
    integer :: nch
!BOC
    if(masterproc) then
       call readnoahcrd(noahdrv)
    endif
    call def_noahpar_struct
    call MPI_BCAST(noahdrv, 1, MPI_NOAHDRV_STRUCT, 0, & 
         MPI_COMM_WORLD, ierr)
#if ( defined OPENDAP )
    noahdrv%noah_albfile = trim(opendap_home)//trim(adjustl(ciam)) &
         //'/'//noahdrv%noah_albfile
    noahdrv%noah_mgfile  = trim(opendap_home)//trim(adjustl(ciam)) &
         //'/'//noahdrv%noah_mgfile
    noahdrv%noah_mxsnal  = trim(opendap_home)//trim(adjustl(ciam)) &
         //'/'//noahdrv%noah_mxsnal
    noahdrv%noah_tbot    = trim(opendap_home)//trim(adjustl(ciam)) &
         //'/'//noahdrv%noah_tbot
#endif
    if(masterproc) then 
       allocate(noah(nch))
    else
       allocate(noah(di_array(iam)))
    endif
  end subroutine noah_varder_ini
!EOC
end module noah_varder



