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
! !MODULE: grid_spmdMod.F90
!
! !DESCRIPTION:
!  This module computes domain decomposition on the grid domain
!
! !REVISION HISTORY: 
! 14 Nov 2002 Sujay Kumar Initial Specification
!
! !INTERFACE:
module grid_spmdMod
! !USES:
  use spmdMod
! !ARGUMENTS:
  integer, allocatable :: gdi(:), gdisp(:)
  integer, allocatable :: g2di(:), g2disp(:)
!EOP
contains
!BOP
! !ROUTINE: allocate_gdd
! 
! !DESCRIPTION: 
! Allocates memory for arrays that contain domain decomposition information
! 
! !INTERFACE:
  subroutine allocate_gdd()
! !DESCRIPTION: 
! Allocates memory for arrays that contain domain decomposition information
!EOP
!BOC
    allocate(gdi(0:npes-1))
    allocate(gdisp(0:npes-1))
    allocate(g2di(0:npes-1))
    allocate(g2disp(0:npes-1))
!EOC
  end subroutine allocate_gdd
!BOP
! !ROUTINE: grid_spmd_init
! 
! !DESCRIPTION: 
! Computes domain decomposition based on the number of processors
! 
! !INTERFACE:     
  subroutine grid_spmd_init(tile,nch,nmif,ngrid)
! !USES:
    use tile_module 
    use tile_spmdMod, only : displs
! !INPUT ARGUMENTS:
    integer, intent(in) :: nch, ngrid, nmif
! !OUTPUT ARGUMENTS:
    type(tiledec)::tile(nch)
!EOP       
    integer :: p
!BOC       
    gdisp(0) = 0
    do p=1, npes-1
       gdisp(p) = tile(displs(p))%index
    enddo
    do p = 0 , npes-2
       gdi(p) = gdisp(p+1)-gdisp(p)
    enddo
    gdi(npes-1) = ngrid - gdisp(npes-1)
    do p=0, npes-1
       g2di(p) = gdi(p)*nmif
    enddo
    g2disp(0) = 0
    do p=1, npes-1
       g2disp(p) = g2disp(p-1)+g2di(p-1)
    enddo
!EOC
  end subroutine grid_spmd_init
     
  subroutine spread_gdds()
    integer ::ierr
#if (defined SPMD)
    call MPI_BCAST(gdi, npes, MPI_INTEGER,0, MPI_COMM_WORLD, ierr)
    call MPI_BCAST(gdisp, npes, MPI_INTEGER,0,MPI_COMM_WORLD, ierr)
    call MPI_BCAST(g2di, npes, MPI_INTEGER,0,MPI_COMM_WORLD, ierr)
    call MPI_BCAST(g2disp, npes, MPI_INTEGER,0,MPI_COMM_WORLD, ierr)
#endif
  end subroutine spread_gdds
end module grid_spmdMod
