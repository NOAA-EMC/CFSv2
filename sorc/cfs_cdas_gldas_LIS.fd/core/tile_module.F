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
! !ROUTINE: tile_module.F90
!
! !DESCRIPTION:
!  LIS non-model-specific tile variables only.
!
! !REVISION HISTORY:
!  15 Oct 1999: Paul Houser; Initial code
!  22 Aug 2000: Brian Cosgrove; Modified code for output of
!               standard LDAS output variables--added CC and AC,
!               the canopy and aerodynamic conductance
! !INTERFACE:
module tile_module 
  
  implicit none
  public tiledec
! !ARGUMENTS:
  type tiledec
     integer :: col        !Grid Column of Tile
     integer :: row        !Grid Row of Tile
     integer :: index      !Index of corresponding grid
     integer :: vegt       !Vegetation Type of Tile
     real    :: fgrd       !Fraction of Grid covered by tile 
     real    :: elev   
  end type tiledec
!EOP
end module tile_module
