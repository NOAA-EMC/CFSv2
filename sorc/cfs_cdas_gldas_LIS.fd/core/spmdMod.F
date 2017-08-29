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
! !MODULE: spmdMod.F90
!
! !DESCRIPTION:
!
! MPI routines for initialization and computing arguments for
! different operations.
!
!EOP

#include <misc.h>
!BOP
! !INTERFACE:
module spmdMod
! !ARGUMENTS:
#if (!defined SPMD)
  logical :: masterproc = .true. ! proc 0 logical for printing msgs
  integer :: iam = 0
  integer :: npes = 1
#endif

#if (defined SPMD)

#if (defined OFFLINE)
  use mpishorthand
#endif

#if (defined OFFLINE) 
  integer :: npes        !number of processors
  integer :: iam         !proc number
  logical :: masterproc  !proc 0 logical for printing msgs
#endif
!EOP

!  integer, public, allocatable :: proc_landi(:)
!  integer, public, allocatable :: proc_landf(:)
!  integer, public, allocatable :: proc_patchi(:)
!  integer, public, allocatable :: proc_patchf(:)
!  integer, public, allocatable :: proc_patchpts(:)
!  integer, public, allocatable :: proc_landpts(:)
#endif

  SAVE

!===============================================================================
CONTAINS
!===============================================================================

#if (defined OFFLINE) 
!BOP
! !ROUTINE: spmd_init
!
! !DESCRIPTION:
!  MPI initialization (number of cpus, processes, tids, etc)
! 
! !INTERFACE:
  subroutine spmd_init
!DIR$ NAME (release_cache="_f90a_free_all")
!
!EOP
  implicit none

! ------------------------ local variables -----------------------------
!    integer i,j        ! indices
    integer ier        ! return error status      
!    integer, allocatable :: length(:)
!    integer, allocatable :: displ(:)
!    character*(MPI_MAX_PROCESSOR_NAME), allocatable :: proc_name(:)
!#if (defined OFFLINE)
!    logical mpi_running
!#endif
!-----------------------------------------------------------------------
!BOC
    print*, 'in spmd mod'
#if (defined SPMD)
#if (defined OFFLINE)

    call mpi_init(ier)

#endif
    call mpi_comm_rank(MPI_COMM_WORLD, iam, ier)  

    if (iam==0) then 
       masterproc = .true.
    else
       masterproc = .false.
    end if
    call mpi_comm_size(MPI_COMM_WORLD, npes, ier) 
    print*, '** Number of Procs **',npes
    return
!EOC
#endif
  end subroutine spmd_init

#endif

!===============================================================================

  subroutine spmd_init_patch

!----------------------------------------------------------------------- 
! 
! Purpose: Initialize arrays for number of land/patch points per proc
! 
!----------------------------------------------------------------------- 
#if 0 
    allocate (proc_landi(0:npes-1))
    allocate (proc_landf(0:npes-1))
    allocate (proc_landpts(0:npes-1))
    allocate (proc_patchi(0:npes-1))
    allocate (proc_patchf(0:npes-1))
    allocate (proc_patchpts(0:npes-1))
#endif     
    return
  end subroutine spmd_init_patch

!===============================================================================

  subroutine compute_mpigs_patch (nfact, numtot, numperproc, displs)

!------------------------------------------------------------------
! 
! Purpose: Compute arguments for gatherv, scatterv for patche vectors
! 
!------------------------------------------------------------------

    implicit none

! ------------------- arguments -----------------------------------
    integer, intent(in ) :: nfact                ! multiplicative factor for patches
    integer, intent(out) :: numtot               ! total number of elements (to send or recv)
    integer, intent(out) :: numperproc(0:npes-1) ! per-PE number of items to receive
    integer, intent(out) :: displs(0:npes-1)     ! per-PE displacements
!------------------------------------------------------------------

! ---------------------- local variables --------------------------
    integer :: p                                 ! index
!------------------------------------------------------------------
#if 0    
    numtot = (proc_patchpts(iam))*nfact
    
    do p=0,npes-1
       numperproc(p) = proc_patchpts(p)*nfact
    end do
    
    displs(0) = 0
    do p=1,npes-1
       displs(p) = displs(p-1) + numperproc(p-1)
    end do
#endif    
  end subroutine compute_mpigs_patch

!===============================================================================

  subroutine compute_mpigs_land (nfact, numtot, numperproc, displs)

!------------------------------------------------------------------
! 
! Purpose: Compute arguments for gatherv, scatterv for land vectors
! 
!------------------------------------------------------------------

    implicit none
! ------------------- arguments -----------------------------------
    integer, intent(in ) :: nfact                ! multiplicative factor for patches
    integer, intent(out) :: numtot               ! total number of elements (to send or recv)
    integer, intent(out) :: numperproc(0:npes-1) ! per-PE number of items to receive
    integer, intent(out) :: displs(0:npes-1)     ! per-PE displacements
!------------------------------------------------------------------

! ---------------------- local variables --------------------------
    integer :: p                                 ! index
!------------------------------------------------------------------
#if 0 
   
    numtot = (proc_landpts(iam))*nfact
    
    do p=0,npes-1
       numperproc(p) = proc_landpts(p)*nfact
    end do
    
    displs(0) = 0
    do p=1,npes-1
       displs(p) = displs(p-1) + numperproc(p-1)
    end do
#endif     
  end subroutine compute_mpigs_land

!===============================================================================


end module spmdMod


