#include <misc.h>
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
!  !MODULE: baseforcing_module.F90
! 
!  !DESCRIPTION: 
!   This module contains 
!   interfaces and subroutines that controls the incorporation
!   of model forcing
!   
!  !REVISION HISTORY: 
!  14Nov02    Sujay Kumar  Initial Specification
! 
!EOP
module baseforcing_module
  use grid_spmdMod
  implicit none 
  real, pointer :: glbdata1(:,:)
  real, pointer :: glbdata2(:,:)
!BOP
! !ROUTINE: LIS_get_base_forcing
! 
! !INTERFACE:             
  interface LIS_get_base_forcing
     module procedure ld_getbaseforcing
  end interface
!EOP

!BOP
! !ROUTINE: LIS_baseforcing_init
!
! !INTERFACE:
  interface LIS_baseforcing_init
     module procedure init_baseforcing
  end interface
!EOP
contains
!BOP
! !ROUTINE: forcing_init
!
! !DESCRIPTION:
! Sets up functions for reading model forcing
!
! !INTERFACE:
  subroutine forcing_init()
! !USES:
    use baseforcing_pluginMod
!EOP
    call baseforcing_plugin

  end subroutine forcing_init
!BOP
! !ROUTINE: init_baseforcing
!
! !DESCRIPTION:
! Initializes model forcing variables and allocates memory
! 
! 
! !INTERFACE: 
  subroutine init_baseforcing()
! !USES:
    use bilinear_interpMod, only: bilinear_interp_input, &
         allocate_bilinear_interp
    use conserv_interpMod, only : conserv_interp_input, & 
         allocate_conserv_interp
    use lisdrv_module, only: lis, getforcing
    use lis_indices_module, only: lis_nc_working, lis_nr_working
    real :: gridDesci(50)
!EOP
!BOC
    gridDesci = 0
    call forcing_init()
    call allocate_forcing_mem()

#if ( ! defined OPENDAP )
    if ( masterproc ) then
#endif
       call defnatres(getforcing(),gridDesci)   

       if(lis%f%interp.eq.1) then 
          call allocate_bilinear_interp(lis_nc_working*lis_nr_working)
          call bilinear_interp_input(gridDesci,lis%d%gridDesc,&
               lis_nc_working*lis_nr_working)
       elseif(lis%f%interp.eq.2) then
          call allocate_bilinear_interp(lis_nc_working*lis_nr_working)
          call bilinear_interp_input(gridDesci,lis%d%gridDesc,&
               lis_nc_working*lis_nr_working) 
          call allocate_conserv_interp(lis_nc_working*lis_nr_working)
          call conserv_interp_input(gridDesci,lis%d%gridDesc,&
               lis_nc_working*lis_nr_working)
       endif
#if ( ! defined OPENDAP )
    endif
#endif

!EOC
  end subroutine init_baseforcing
!BOP
! !ROUTINE: get
!
! !DESCRIPTION:
! Retrieves and interpolates model forcing
! 
! !INTERFACE:      
  subroutine get()
! !USES:
    use lisdrv_module, only: getforcing
!EOP
!BOC
    call getf(getforcing())
!EOC
  end subroutine get
!BOP
! !ROUTINE: time_interp
!
! !DESCRIPTION:
! Computes temporal interpolation
!
! !INTERFACE:
  subroutine time_interp()
! !USES:
    use lisdrv_module,only :getforcing
!EOP
!BOC
    call timeinterp(getforcing())
!EOC
  end subroutine time_interp

  subroutine allocate_forcing_mem()
    use lisdrv_module, only: lis,getnmif
    integer:: nmif
!BOC
    nmif = getnmif()
    if( masterproc ) then 
       allocate(glbdata1(nmif,lis%d%glbngrid))
       allocate(glbdata2(nmif,lis%d%glbngrid))
    else
       allocate(glbdata1(nmif,gdi(iam)))
       allocate(glbdata2(nmif,gdi(iam)))
    endif
!EOC
  end subroutine allocate_forcing_mem

!BOP
! !ROUTINE: ld_getbaseforcing
!
! !DESCRIPTION:
! Retrieves model forcing and invokes spatial and interpolation routines
! 
! !INTERFACE:
  subroutine ld_getbaseforcing()
! !USES:
    use lisdrv_module, only: lis, grid
    use grid_spmdMod, only : gdisp,gdi
!EOP
    integer :: ier, index, t, f
    real :: fforce,force_tmp,force_hum,force_lwd
    real :: force_prs
!BOC
#if ( defined OPENDAP )
    call get()
#else
    if ( masterproc ) then
       call get()
    endif
#if (defined SPMD)
    call MPI_BCAST(lis%f%findtime1,1,MPI_INTEGER,0, & 
         MPI_COMM_WORLD, ier)
    call MPI_BCAST(lis%f%findtime2,1,MPI_INTEGER,0, & 
         MPI_COMM_WORLD, ier)
    call MPI_BCAST(lis%f%gridchange,1,MPI_INTEGER,0, & 
         MPI_COMM_WORLD, ier)
#endif 
    
    if(lis%f%findtime1 ==1 .or.  & 
         lis%f%findtime2==1) then  
       if(npes > 1) then 
          call scatter_data()
       endif
    endif
    if ( lis%f%gridchange == 1 ) then ! grid HAS changed
       if(npes>1) then
          call scatter_elev()
       endif
       lis%f%gridchange = 0
    endif
#endif
    call time_interp()
!EOC
    if(lis%f%ecor .eq. 1) then 
       do t=1,gdi(iam)
          index = t
          do f=1,lis%f%nforce
             force_tmp = grid(index)%forcing(1)
             force_hum = grid(index)%forcing(2)
             force_lwd = grid(index)%forcing(4)
             force_prs = grid(index)%forcing(7)
             if (f .eq. 1 .or. f .eq. 2 .or. f .eq. 4 .or. f .eq. 7) then
                call elevadjust(t,f,fforce,force_tmp,force_hum,force_lwd, &
                     force_prs)
                grid(index)%forcing(f)=fforce
             endif
          enddo
       enddo
    end if
  end subroutine ld_getbaseforcing
!BOP
! !ROUTINE: scatter_data
!
! !DESCRIPTION:
! Distributes the forcing data on compute nodes
! 
! !INTERFACE:
  subroutine scatter_data()
! !USES:
    use lisdrv_module, only : lis
!EOP
    integer :: ier
#if (defined SPMD)
!jesse, for wcoss
  if ( masterproc ) then
    call MPI_SCATTERV(glbdata1,g2di,g2disp,MPI_REAL, & 
         MPI_IN_PLACE,g2di(iam),MPI_REAL,0,MPI_COMM_WORLD,ier)
    call MPI_SCATTERV(glbdata2,g2di,g2disp,MPI_REAL, & 
         MPI_IN_PLACE,g2di(iam),MPI_REAL,0,MPI_COMM_WORLD,ier)
  else
    call MPI_SCATTERV(glbdata1,g2di,g2disp,MPI_REAL, &
         glbdata1,g2di(iam),MPI_REAL,0,MPI_COMM_WORLD,ier)
    call MPI_SCATTERV(glbdata2,g2di,g2disp,MPI_REAL, &
         glbdata2,g2di(iam),MPI_REAL,0,MPI_COMM_WORLD,ier)
  endif
#endif 
  end subroutine scatter_data

!BOP
! !ROUTINE: scatter_elev
!
! !DESCRIPTION:
! Distributes the elevation difference correction on compute nodes
! 
! !INTERFACE:
  subroutine scatter_elev()
! !USES:
    use lisdrv_module, only : tile
    use driverpardef_module
    use tile_spmdMod
!EOP
    integer :: ierr
#if (defined SPMD)    
!jesse, for wcoss
    if( masterproc ) then
    call MPI_SCATTERV(tile,di_array,displs, & 
         MPI_TILE_STRUCT,MPI_IN_PLACE,di_array(iam),MPI_TILE_STRUCT, & 
         0,MPI_COMM_WORLD,ierr)
    else
    call MPI_SCATTERV(tile,di_array,displs, &
         MPI_TILE_STRUCT,tile,di_array(iam),MPI_TILE_STRUCT, &
         0,MPI_COMM_WORLD,ierr)
    endif
#endif 
  end subroutine scatter_elev

end module baseforcing_module
