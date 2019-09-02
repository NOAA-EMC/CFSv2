 program driver
!$$$  main program documentation block
!                .      .    .                                       .
! main program: driver
!   prgmmr: gayno            ORG: NP2                DATE: 2015-JAN-28
!
! $Revision$
!
! abstract: simple driver program to call the gridgen_sfc library.
!
! program history log:
!   2015-jan-28  gayno     initial version
!
!$$$
!
 use grid_info, only : grid_specs, read_grid_info

 implicit none

 include 'mpif.h'

 type(grid_specs)    :: grid_mdl

 integer             :: ierr

 call w3tagb('GRIDGEN_SFC_DRIVER',2005,0136,0000,'NP2')

 call mpi_init(ierr)

 call read_grid_info(grid_mdl)

 call gridgen_sfc(grid_mdl)

 call w3tage('GRIDGEN_SFC_DRIVER')

 call mpi_finalize(ierr)

 stop 0
 
 end program driver
