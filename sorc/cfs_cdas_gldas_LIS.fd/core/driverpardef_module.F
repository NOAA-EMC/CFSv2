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
#include "misc.h"
!BOP
!
! !MODULE: driverpardef_module.F90
! 
! !DESCRIPTION: 
!
! This module contains routines that defines MPI derived data types
! for LIS driver specific variables
!
! !REVISION HISTORY:
! 
! 06 Oct 2003; Sujay Kumar  Initial Specification 
!
! !INTERFACE:
module driverpardef_module

  ! !USES:
  use tile_module
  use lis_module
  use grid_module
  use spmdMod

  implicit none

  ! !ARGUMENTS:
#if (defined SPMD)
  integer:: MPI_TILE_STRUCT  !MPI derived type for tile$_-$module
  integer:: MPI_GRID_STRUCT  !MPI derived type for grid$_-$module
  integer:: MPI_LD_STRUCT    !MPI derived type for lisdomain 
  integer:: MPI_LF_STRUCT    !MPI derived type for lisforcing
  integer:: MPI_LP_STRUCT    !MPI derived type for lisparameters
  integer:: MPI_LT_STRUCT    !MPI derived type for listime
  integer:: MPI_LO_STRUCT    !MPI derived type for lisoutput
  integer:: MPI_LA_STRUCT    !MPI derived type for lisassimil
  !EOP

  integer, parameter, private :: impi_root = 0   

  integer,parameter :: tile_ntypes = 2
  integer, dimension(tile_ntypes)::tile_blkcnts = & 
       (/4,2/)
  integer,dimension(tile_ntypes)::tile_types = & 
       (/MPI_INTEGER, MPI_REAL/)
  integer, dimension(tile_ntypes):: tile_displs


  integer,parameter :: grid_ntypes = 1
  integer, dimension(grid_ntypes)::grid_blkcnts =  & 
       (/31/)
  integer,dimension(grid_ntypes)::grid_types = & 
       (/MPI_REAL/)
  integer, dimension(grid_ntypes):: grid_displs


  integer,parameter :: ld_ntypes = 2
  integer, dimension(ld_ntypes)::ld_blkcnts =  & 
       (/16,70/)
  integer,dimension(ld_ntypes)::ld_types = & 
       (/MPI_INTEGER, MPI_REAL/)
  integer, dimension(ld_ntypes):: ld_displs


  integer,parameter :: lf_ntypes = 1
  integer, dimension(lf_ntypes)::lf_blkcnts = & 
       (/19/)
  integer,dimension(lf_ntypes)::lf_types = & 
       (/MPI_INTEGER/)
  integer, dimension(lf_ntypes):: lf_displs


  integer,parameter :: lp_ntypes = 3
  integer, dimension(lp_ntypes)::lp_blkcnts = & 
!<kluge sai read>
!       (/5,540,1/)
       (/5,540,2/)
!</kluge sai read>
  integer,dimension(lp_ntypes)::lp_types = & 
       (/MPI_INTEGER,MPI_CHARACTER,MPI_REAL8/)
  integer, dimension(lp_ntypes):: lp_displs


  integer,parameter :: lt_ntypes = 3
  integer, dimension(lt_ntypes)::lt_blkcnts = & 
       (/28,2,3/)
  integer,dimension(lt_ntypes)::lt_types = & 
       (/MPI_INTEGER,MPI_REAL8, MPI_REAL/)
  integer, dimension(lt_ntypes):: lt_displs


  integer,parameter :: lo_ntypes = 2
  integer, dimension(lo_ntypes)::lo_blkcnts = & 
       (/13,80/)
  integer,dimension(lo_ntypes)::lo_types = &
       (/MPI_INTEGER,MPI_CHARACTER/)
  integer, dimension(lo_ntypes):: lo_displs

  integer,parameter :: la_ntypes = 1
  integer, dimension(la_ntypes)::la_blkcnts = & 
       (/5/)
  integer,dimension(la_ntypes)::la_types = & 
       (/MPI_INTEGER/)
  integer, dimension(la_ntypes):: la_displs
#endif 
contains
  !BOPI
  ! !ROUTINE: def_driverpar_structs
  !
  ! !DESCRIPTION:
  ! 
  ! Routine that defines MPI derived data types
  !
  ! !INTERFACE:
  subroutine def_driverpar_structs()
    !EOPI
#if (defined SPMD)
    integer:: t,l, ierr
    type(tiledec)::tile
    type(griddec)::grid
    type(lisdomain) :: ld
    type(lisforcing)::lf
    type(lisparameters)::lp
    type(listime) :: lt
    type(lisoutput)::lo
    type(lisassimil)::la

    call MPI_ADDRESS(tile%col, tile_displs(1), ierr)
    call MPI_ADDRESS(tile%fgrd, tile_displs(2),ierr)
    do t=tile_ntypes, 1, -1
       tile_displs(t) = tile_displs(t)-tile_displs(1)
    enddo

    call MPI_TYPE_STRUCT(tile_ntypes, tile_blkcnts, tile_displs, & 
         tile_types,MPI_TILE_STRUCT, ierr)
    call MPI_TYPE_COMMIT(MPI_TILE_STRUCT,ierr)

    call MPI_ADDRESS(grid%lat, grid_displs(1), ierr)
    do t=grid_ntypes, 1, -1
       grid_displs(t) = grid_displs(t)-grid_displs(1)
    enddo

    call MPI_TYPE_STRUCT(grid_ntypes, grid_blkcnts, grid_displs, & 
         grid_types,MPI_GRID_STRUCT, ierr)
    call MPI_TYPE_COMMIT(MPI_GRID_STRUCT,ierr)



    call MPI_ADDRESS(ld%nch, ld_displs(1), ierr)
    call MPI_ADDRESS(ld%mina, ld_displs(2),ierr)

    do t=ld_ntypes, 1, -1
       ld_displs(t) = ld_displs(t)-ld_displs(1)
    enddo

    call MPI_TYPE_STRUCT(ld_ntypes, ld_blkcnts, ld_displs, & 
         ld_types,MPI_LD_STRUCT, ierr)
    call MPI_TYPE_COMMIT(MPI_LD_STRUCT,ierr)


    call MPI_ADDRESS(lf%force, lf_displs(1), ierr)

    do t=lf_ntypes, 1, -1
       lf_displs(t) = lf_displs(t)-lf_displs(1)
    enddo

    call MPI_TYPE_STRUCT(lf_ntypes, lf_blkcnts, lf_displs, & 
         lf_types,MPI_LF_STRUCT, ierr)
    call MPI_TYPE_COMMIT(MPI_LF_STRUCT,ierr)

    call MPI_ADDRESS(lp%lai, lp_displs(1), ierr)
    call MPI_ADDRESS(lp%mfile, lp_displs(2),ierr)
    call MPI_ADDRESS(lp%laitime, lp_displs(3),ierr)

    do t=lp_ntypes, 1, -1
       lp_displs(t) = lp_displs(t)-lp_displs(1)
    enddo

    call MPI_TYPE_STRUCT(lp_ntypes, lp_blkcnts, lp_displs, & 
         lp_types,MPI_LP_STRUCT, ierr)
    call MPI_TYPE_COMMIT(MPI_LP_STRUCT,ierr)

    call MPI_ADDRESS(lt%sss, lt_displs(1), ierr)
    call MPI_ADDRESS(lt%time, lt_displs(2),ierr)
    call MPI_ADDRESS(lt%gmt, lt_displs(3),ierr)

    do t=lt_ntypes, 1, -1
       lt_displs(t) = lt_displs(t)-lt_displs(1)
    enddo

    call MPI_TYPE_STRUCT(lt_ntypes, lt_blkcnts, lt_displs, & 
         lt_types,MPI_LT_STRUCT, ierr)
    call MPI_TYPE_COMMIT(MPI_LT_STRUCT,ierr)

    call MPI_ADDRESS(lo%wfor, lo_displs(1), ierr)
    call MPI_ADDRESS(lo%odir, lo_displs(2),ierr)

    do t=lo_ntypes, 1, -1
       lo_displs(t) = lo_displs(t)-lo_displs(1)
    enddo

    call MPI_TYPE_STRUCT(lo_ntypes, lo_blkcnts, lo_displs, & 
         lo_types,MPI_LO_STRUCT, ierr)
    call MPI_TYPE_COMMIT(MPI_LO_STRUCT,ierr)

    call MPI_ADDRESS(la%rpsas, la_displs(1), ierr)

    do t=la_ntypes, 1, -1
       la_displs(t) = la_displs(t)-la_displs(1)
    enddo

    call MPI_TYPE_STRUCT(la_ntypes, la_blkcnts, la_displs, & 
         la_types,MPI_LA_STRUCT, ierr)
    call MPI_TYPE_COMMIT(MPI_LA_STRUCT,ierr)
#endif 
  end subroutine def_driverpar_structs
end module driverpardef_module
