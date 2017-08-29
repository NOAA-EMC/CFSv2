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
#include <misc.h>
!BOP
!
! !MODULE: tile_spmdMod.F90
!
! !DESCRIPTION:
!
!  This module contains routines for domain decomposition in tile space
!
! !REVISION HISTORY:
! 
! 14 Nov 2002;  Sujay Kumar  Initial Specification
!
! !INTERFACE:
module tile_spmdMod
! !USES:
  use spmdMod
! !ARGUMENTS:
  integer, allocatable :: di_array(:) !array containing the sizes of the decomposed tile space
  integer, allocatable :: displs(:) !array containing relative displacements fo the tile space buffer

!EOP
  integer :: deltax
  integer, public, allocatable :: nti(:)
  integer, public, allocatable :: ntf(:)
   
contains
!BOP
! !ROUTINE: allocate_tiledd
! 
! !DESCRIPTION:
!
! Allocates memory for arrays containing tile decomposition information
!
! !INTERFACE: 
  subroutine allocate_tiledd()
!EOP
    allocate(nti(0:npes-1))
    allocate(ntf(0:npes-1))
    allocate(di_array(0:npes-1))
    allocate(displs(0:npes-1))
  end subroutine allocate_tiledd
!BOP
! !ROUTINE: tile_spmd_init
! 
! !DESCRIPTION:
! 
! Performs domain decomposition in tile space
! 
! !INTERFACE:
  subroutine tile_spmd_init(tile, nch, nmif)
! !USES:
    use tile_module
! !ARGUMENTS:
    type(tiledec) :: tile(nch)
    integer :: nch, nmif
!EOP
    integer :: ntiles
    integer :: p
    integer :: tindex, gind
!BOC    
    ntiles = 1
    deltax = nch/npes    
    do p=0,npes-2
       nti(p) = ntiles
       tindex = ntiles+deltax-1
       gind  = tile(tindex)%index 
       do while(tile(tindex+1)%index ==gind) 
          tindex = tindex+1
       enddo
       ntf(p) = tindex
       ntiles = tindex+1
    enddo
    nti(npes-1) = ntiles
    ntf(npes-1) = nch
    do i=0,npes-1
       di_array(i) = ntf(i)-nti(i)+1
    enddo
    
    displs(0) = 0
    do i = 1, npes-1
       displs(i) = displs(i-1)+di_array(i-1)
    enddo
    print*, 'MSG: tile_spmd_init -- domain decomp', di_array, displs
!EOC
  end subroutine tile_spmd_init
  subroutine spread_tdds()
    integer ::ierr
#if (defined SPMD)
    call MPI_BCAST(di_array, npes, MPI_INTEGER,0, MPI_COMM_WORLD, ierr)
    call MPI_BCAST(displs, npes, MPI_INTEGER,0,MPI_COMM_WORLD, ierr)
#endif 
  end subroutine spread_tdds
end module tile_spmdMod
