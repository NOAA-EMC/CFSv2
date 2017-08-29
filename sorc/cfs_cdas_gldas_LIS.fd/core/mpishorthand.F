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
! !MODULE: mpishorthand.F90
!
! !DESCRIPTION: 
!
! Data and parameters used for MPI. Some shorthand variables 
! with shorter names than the standard MPI parameters. 
! Also some variables used for heap management. Adopted from CLM
!
! !REVISION HISTORY: 
! 02 Jan 2002 : Sujay Kumar: Initial Version
! 
! !INTERFACE:
module mpishorthand
! !USES:
#if (defined SPMD)
#if (defined OSF1)
  include 'mpif.h' 
#else
  use mpi
#endif
#endif
!EOP
  public
#if (defined SPMD)
  
!
! Need to set as variables rather than parameters since some MPI implementations 
! set values for MPI tags at run time
!
  save
  integer, public :: mpiint    ! MPI integers
  integer, public :: mpichar   ! MPI character data
  integer, public :: mpilog    ! MPI logical data
  integer, public :: mpir4     ! MPI real data for r4
  integer, public :: mpir8     ! MPI real data
  integer, public :: mpicom    ! MPI communication
  integer, public :: mpipk     ! MPI packed data
!
! Common info for heap manager
!
  integer, public::  nsend = 0  ! Number of MPI messages sent
  integer, public::  nrecv = 0  ! Number of MPI messages received
  integer, public::  nwsend = 0 ! Number of MPI words sent
  integer, public::  nwrecv = 0 ! Number of MPI words received
#endif
end module mpishorthand
