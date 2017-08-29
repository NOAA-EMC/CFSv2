!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
#include <misc.h>
!BOP
!
! !MODULE: noahpardef_module.F90
! 
! !DESCRIPTION: 
!
! This module contains routines that defines MPI derived data types
! for Noah LSM
!
! !REVISION HISTORY:
! 
! 06 Oct 2003; Sujay Kumar  Initial Specification 
!
! !INTERFACE:
module noahpardef_module
! !USES:
  use noah_module
  use noahdrv_module
  use spmdMod
  implicit none
! !ARGUMENTS:
  integer:: MPI_NOAH_STRUCT  !MPI derived type for noah$_-$module
  integer :: MPI_NOAHDRV_STRUCT !MPI derived type for noahdrv$_-$module
!EOP
  integer, parameter :: noah_ntypes = 2
  integer, dimension(noah_ntypes) :: noah_blkcnts =(/6,87/)
  integer, dimension(noah_ntypes) :: noah_types = & 
       (/MPI_INTEGER, MPI_REAL/)
  integer, dimension(noah_ntypes) :: noah_displs
  
  integer, parameter :: noahdrv_ntypes = 4
  integer, dimension(noahdrv_ntypes) :: noahdrv_blkcnts =(/10,340,1,3/)
  integer, dimension(noahdrv_ntypes) :: noahdrv_types = & 
       (/MPI_INTEGER, MPI_CHARACTER,MPI_REAL8,MPI_REAL/)
  integer, dimension(noahdrv_ntypes) :: noahdrv_displs
  
contains
!BOP
! !ROUTINE: def_noahpar_struct
!
! !DESCRIPTION:
! 
! Routine that defines MPI derived data types for Noah
!
! !INTERFACE:
  subroutine def_noahpar_struct()
!EOP
    integer:: t,l, ierr
    type(noahdec)::noah
    type(noahdrvdec) :: noahdrv
    call MPI_ADDRESS(noah%ts, noah_displs(1),ierr)
    call MPI_ADDRESS(noah%vegp(1), noah_displs(2),ierr)
    
    do l=noah_ntypes, 1, -1
       noah_displs(l) = noah_displs(l)-noah_displs(1)
    enddo
    call MPI_TYPE_STRUCT(noah_ntypes, noah_blkcnts, noah_displs, & 
         noah_types, MPI_NOAH_STRUCT, ierr)
    call MPI_TYPE_COMMIT(MPI_NOAH_STRUCT, ierr)
    
    call MPI_ADDRESS(noahdrv%noahopen, noahdrv_displs(1),ierr)
    call MPI_ADDRESS(noahdrv%noah_rfile, noahdrv_displs(2),ierr)
    call MPI_ADDRESS(noahdrv%noah_gfractime, noahdrv_displs(3),ierr)
    call MPI_ADDRESS(noahdrv%noah_ism, noahdrv_displs(4),ierr)
    
    do l=noahdrv_ntypes, 1, -1
       noahdrv_displs(l) = noahdrv_displs(l)-noahdrv_displs(1)
    enddo
    call MPI_TYPE_STRUCT(noahdrv_ntypes, noahdrv_blkcnts, noahdrv_displs, & 
         noahdrv_types, MPI_NOAHDRV_STRUCT, ierr)
    call MPI_TYPE_COMMIT(MPI_NOAHDRV_STRUCT, ierr)
  end subroutine def_noahpar_struct
end module noahpardef_module
