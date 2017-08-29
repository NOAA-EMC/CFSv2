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
! !MODULE: lisdrv_module.F90
! 
! !DESCRIPTION: 
!   Main program for LIS
!   This module contains interfaces and subroutines that control
!   program execution.
!   
! !REVISION HISTORY: 
!  14Nov02    Sujay Kumar  Initial Specification
! 
!EOP
module lisdrv_module
!BOP
! !USES:       
  use lis_module
  use grid_module
  use time_manager
  use tile_spmdMod
  use tile_module
!EOP
  implicit none
  
  type(lisdec)::lis
  type(griddec), pointer::grid(:)
  integer,pointer :: glbgindex(:,:)
  integer,allocatable :: gindex(:,:)
  type(tiledec), pointer:: tile(:)

contains
!BOP
! !ROUTINE: ld_domain_init 
!
! !DESCRIPTION: 
! Calls routines to read the card file, and initialize the time manager
!
! !INTERFACE: 
  subroutine LIS_config()
!EOP
!BOC
    call spmd_init()
    if ( masterproc ) then 
       call readcard()
    endif
!-------------------------------------------------------------------
! we have read in the time parameters. Use them to initialize ESMF
! time manager 
!-------------------------------------------------------------------
    if ( masterproc ) then 
       call setup_timeMgr()
       lis%t%endtime = 0
    endif
!EOC
  end subroutine LIS_config
  
!BOP
! !ROUTINE: setup_timeMgr
!
! !DESCRIPTION: 
! Initializes the ESMF time manager
!
! !INTERFACE:       
  subroutine setup_timeMgr()
!EOP
!BOC
    call timemgr_init(lis%t)
    print*, 'time manager initialized..'
!EOC
  end subroutine setup_timeMgr

!BOP
! !ROUTINE: LIS_ticktime
!
! !DESCRIPTION: 
! Uses the ESMF time manager to handle model timestepping. 
! 
! !INTERFACE: 
  subroutine LIS_ticktime()
! !USES:
#if (defined SPMD)
    use driverpardef_module, only: MPI_LT_STRUCT
#endif
!EOP
    integer :: curSec
    integer :: ierr
!BOC
    if(masterproc) then
       call advance_timestep(lis%t)
    endif
#if (defined SPMD)
    call MPI_BCAST(lis%t, 1, MPI_LT_STRUCT, 0, & 
         MPI_COMM_WORLD, ierr)
#endif 
!EOC
  end subroutine LIS_ticktime

!BOP
! !ROUTINE: LIS_domain_init
!
! !DESCRIPTION: 
! Allocates memory for the domain variables, initializes MPI
! data structures, and computes domain decomposition
!
! !INTERFACE: 
  subroutine LIS_domain_init
! 
!EOP
    use driverpardef_module
    use grid_spmdMod
    use domain_module, only : define_domains, read_domain,&
                              domain_init

    integer :: nc,nr,maxt
    integer :: ierr
    integer :: curSec

!BOC

#if (defined SPMD)
    call def_driverpar_structs()
#endif

#if ( defined OPENDAP )
    call MPI_BCAST(lis%d, 1, MPI_LD_STRUCT, 0, MPI_COMM_WORLD, ierr)
    call MPI_BCAST(lis%f, 1, MPI_LF_STRUCT, 0, MPI_COMM_WORLD, ierr)
    call MPI_BCAST(lis%p, 1, MPI_LP_STRUCT, 0, MPI_COMM_WORLD, ierr)

    call define_domains()
    call read_domain(lis%d%domain)
#else
    if ( masterproc ) then
       call define_domains()
       call read_domain(lis%d%domain)
    endif
#endif
    if ( masterproc ) then
       nc = getnc()
       nr = getnr()
       maxt = getmaxt()
       call setnch(nc, nr, maxt)
    endif

#if 0
#if ( defined OPENDAP )
#if (defined SPMD)
    call MPI_BCAST(lis%d%ic, 1, MPI_INTEGER, 0, & 
         MPI_COMM_WORLD, ierr)
    call MPI_BCAST(lis%d%ir, 1, MPI_INTEGER, 0, & 
         MPI_COMM_WORLD, ierr)
    call MPI_BCAST(lis%d%lnc, 1, MPI_INTEGER, 0, & 
         MPI_COMM_WORLD, ierr)
    call MPI_BCAST(lis%d%lnr, 1, MPI_INTEGER, 0, & 
         MPI_COMM_WORLD, ierr)
    call MPI_BCAST(lis%p%nt, 1, MPI_INTEGER, 0, &
         MPI_COMM_WORLD, ierr)
    call MPI_BCAST(lis%d%mina, 1, MPI_REAL, 0, &
         MPI_COMM_WORLD, ierr)
    call MPI_BCAST(lis%d%maxt, 1, MPI_INTEGER, 0, & 
         MPI_COMM_WORLD, ierr)
!    call MPI_BCAST(lis%p%koster, 1, MPI_INTEGER, 0, & 
!         MPI_COMM_WORLD, ierr)
    call MPI_BCAST(lis%p%mfile, 50, MPI_CHARACTER, 0, & 
         MPI_COMM_WORLD, ierr)
    call MPI_BCAST(lis%p%vfile, 50, MPI_CHARACTER, 0, &
         MPI_COMM_WORLD, ierr)
#endif
#endif 
#endif

#if ( ! defined OPENDAP )
    if(masterproc)  then 
#endif
       call domain_init(lis%d%domain)
#if ( ! defined OPENDAP )
    endif
#endif
#if (defined SPMD)
    !call def_driverpar_structs()
    call MPI_BCAST(lis%d, 1, MPI_LD_STRUCT, 0, & 
          MPI_COMM_WORLD, ierr)
    call MPI_BCAST(lis%f, 1, MPI_LF_STRUCT, 0, &
         MPI_COMM_WORLD, ierr)
    call MPI_BCAST(lis%p, 1, MPI_LP_STRUCT, 0, &
         MPI_COMM_WORLD, ierr)
    call MPI_BCAST(lis%o, 1, MPI_LO_STRUCT,0, &
         MPI_COMM_WORLD, ierr)
    call MPI_BCAST(lis%a, 1, MPI_LA_STRUCT,0, &
         MPI_COMM_WORLD, ierr)
    call MPI_BCAST(lis%t, 1, MPI_LT_STRUCT,0, &
         MPI_COMM_WORLD, ierr)
#endif 
#if ( ! defined OPENDAP )
    lis%d%ngrid = lis%d%glbngrid
    lis%d%nch   = lis%d%glbnch
#endif
    call allocate_tiledd
    if(masterproc) then 
       call tile_spmd_init(tile, lis%d%glbnch,lis%f%nmif) 
    endif
    call spread_tdds()
    if(.NOT.masterproc) then 
       allocate(tile(di_array(iam)))
    endif
#if (defined SPMD)
    if(npes>1) then
!jesse, for wcoss
    if ( masterproc ) then
       call MPI_SCATTERV(tile,di_array,displs, & 
            MPI_TILE_STRUCT,MPI_IN_PLACE,di_array(iam),MPI_TILE_STRUCT, & 
            0,MPI_COMM_WORLD,ierr)
    else
       call MPI_SCATTERV(tile,di_array,displs, &
            MPI_TILE_STRUCT,tile,di_array(iam),MPI_TILE_STRUCT, &
            0,MPI_COMM_WORLD,ierr)
    endif
    endif
#endif 
    call allocate_gdd
    if(masterproc) then 
       call grid_spmd_init(tile,lis%d%glbnch, & 
            lis%f%nmif, lis%d%glbngrid)
    endif
    call spread_gdds()
    if(.NOT.masterproc) then
       allocate(grid(gdi(iam)))
    endif
#if (defined SPMD)
    if(npes >1) then         
!jesse, for wcoss
    if ( masterproc ) then
       call MPI_SCATTERV(grid,gdi,gdisp, & 
            MPI_GRID_STRUCT,MPI_IN_PLACE,gdi(iam),MPI_GRID_STRUCT, & 
            0,MPI_COMM_WORLD,ierr)
    else
       call MPI_SCATTERV(grid,gdi,gdisp, &
            MPI_GRID_STRUCT,grid,gdi(iam),MPI_GRID_STRUCT, &
            0,MPI_COMM_WORLD,ierr)
    endif
    endif
#endif 
    call dist_gindex()
#if 0
!<kluge>
    if(masterproc) then
       call get_curr_date(lis%t%yr,lis%t%mo,lis%t%da,curSec)
       call sec2time(curSec,lis%t%hr,lis%t%mn,lis%t%ss)
       call updatetime(lis%t)
    endif
!</kluge>
#endif
#if(defined SPMD)
    call MPI_BCAST(lis%t, 1, MPI_LT_STRUCT, 0,  & 
         MPI_COMM_WORLD, ierr)
#endif

!EOC
  end subroutine LIS_domain_init

!BOP
! !ROUTINE: setnch
!
! !DESCRIPTION: 
! Computes an estimate of the total number of tiles 
!  
! !INTERFACE:       
  subroutine setnch(nc, nr, maxt)
! !ARGUMENTS: 
    integer::nc
    integer::nr
    integer::maxt
!EOP
!BOC
    lis%d%glbnch = nc*nr*maxt !!may be too big      
!EOC
  end subroutine setnch

!BOP
! !ROUTINE: getdomain
!
! !DESCRIPTION: 
! Returns the domain resolution
!
! !INTERFACE:       
  function getdomain() result(d)
! !ARGUMENTS: 
    integer :: d      

!EOP
!BOC
    d = lis%d%domain
!EOC
  end function getdomain
!BOP
! !ROUTINE: getlsm
!
! !DESCRIPTION: 
! Returns which land surface model is executed
!
! !INTERFACE:             
  function getlsm() result(lsmno)
! !ARGUMENTS: 
    integer :: lsmno       
!EOP
!BOC
    lsmno = lis%d%lsm
!EOC
  end function getlsm
!BOP
! !ROUTINE: getnch
!
! !DESCRIPTION: 
! Returns the number of model tiles
!
! !INTERFACE:                   
  function getnch() result(n)
! !ARGUMENTS: 
    integer :: n       

!EOP
!BOC
    n = lis%d%glbnch
!EOC
  end function getnch
!BOP
! !ROUTINE: getnc
!
! !DESCRIPTION:
! Returns the number of columns
!
! !INTERFACE:                   
  function getnc() result(ncol)
! !ARGUMENTS: 
    integer :: ncol
!EOP
!BOC
    ncol = lis%d%lnc
!EOC
  end function getnc
!BOP
! !ROUTINE: getnr
!
! !DESCRIPTION:
! Returns the number of columns
!
! !INTERFACE:                         
  function getnr() result(nrow)
! !ARGUMENTS: 
    integer :: nrow
!EOP
!BOC
    nrow = lis%d%lnr
!EOC
  end function getnr
!BOP
! !ROUTINE: gettileindex
!
! !DESCRIPTION:
! Returns the index of a tile given lat lon
!      
! !INTERFACE:
  function gettileindex(lat,lon) result(k)
! !ARGUMENTS:
    real :: lat, lon
    integer :: k, t
!EOP
!BOC
    k = -1
    do t=1,lis%d%nch
       if((grid(tile(t)%index)%lat .eq. lat) .and. &
            (grid(tile(t)%index)%lon .eq. lon)) then 
          k = t
       endif
    enddo
!EOC
  end function gettileindex

!BOP
! !ROUTINE: getmaxt
!
! !DESCRIPTION:
! Returns the maximum number of tiles
!      
! !INTERFACE:
  function getmaxt() result(maxt)
! !ARGUMENTS:
    integer :: maxt
!EOP
!BOC
    maxt = lis%d%maxt
!EOC
  end function getmaxt
!BOP
! !ROUTINE: getforcing
!      
! !DESCRIPTION: 
! Returns the type of forcing used

! !INTERFACE:
  function getforcing() result(f)
! !ARGUMENTS:
    integer:: f
!EOP
!BOC 
    f = lis%f%force
!EOC
  end function getforcing
!BOP
! !ROUTINE: getnmif
!      
! !DESCRIPTION: 
! Returns the number of forcing variables for model initialization
!
! !INTERFACE:      
  function getnmif() result(f)
! !ARGUMENTS:
    integer:: f
!EOP
!BOC
    f = lis%f%nmif
!EOC
  end function getnmif
!BOP
! !ROUTINE: LIS_endofrun
!
! !DESCRIPTION:
! Returns if the end of simulation has reached
!      
! !INTERFACE:            
  function LIS_endofrun() result(finish)
! !ARGUMENTS:
    logical :: finish
    integer :: ierr
!EOP
!BOC
    if(masterproc) then
       finish = is_last_step(lis%t)
    endif
#if (defined SPMD)      
    call MPI_BCAST(finish, 1, MPI_LOGICAL, 0, & 
         MPI_COMM_WORLD, ierr)
#endif
!EOC
  end function LIS_endofrun

!BOP
! !ROUTINE: dist_gindex
!      
! !DESCRIPTION: 
! Distributes the mask indices on compute nodes for a GDS-based execution
!
! !INTERFACE:            
  subroutine dist_gindex
! !USES:
    use grid_spmdMod

    implicit none
! !ARGUMENTS:
    integer :: findex, lindex, nr_count, t
    integer :: ierr
    integer :: grid_offset, grid_lb, grid_ub
    integer :: i, j
#if (defined SPMD)
    integer :: status(MPI_STATUS_SIZE)
#endif
!EOP
!BOC
#if ( defined OPENDAP )
#if (defined SPMD)
    if ( npes > 1 ) then
       if ( masterproc ) then
          do t = 1, npes-1
             findex = tile( gdisp(t) + 1 )%row
             lindex = tile( gdisp(t) + gdi(t) )%row
             nr_count = (lindex-findex+1)
             print*,'DBG: lisdrv_module -- ', & 
                  't, findex,lindex,lis%d%gnc,nr_count', & 
                  t, findex,lindex,lis%d%gnc,nr_count,' (', iam,')'
             call mpi_send(nr_count,1,MPI_INTEGER,t,t, & 
                  MPI_COMM_WORLD,ierr)
             call mpi_send(glbgindex(:,findex:lindex), & 
                  lis%d%gnc*nr_count, & 
                  MPI_INTEGER,t,t,MPI_COMM_WORLD,ierr)
          enddo
       else 
          call mpi_recv(nr_count,1,MPI_INTEGER,0,MPI_ANY_TAG, & 
               MPI_COMM_WORLD,status,ierr)
          allocate(gindex(lis%d%gnc,nr_count),stat=ierr)
          call check_error(ierr,'lisdrv_module', &
               'Error allocating gindex.',iam)
          call mpi_recv(gindex,lis%d%gnc*nr_count,MPI_INTEGER,0, & 
               MPI_ANY_TAG, & 
               MPI_COMM_WORLD,status,ierr)
       endif
    endif
    
    if ( masterproc ) then
       findex = tile( gdisp(0) + 1 )%row
       lindex = tile( gdisp(0) + gdi(0) )%row
       nr_count = (lindex-findex+1)
       print*,'DBG: lisdrv_module -- ', & 
            'findex,lindex,lis%d%gnc,nr_count', & 
            findex,lindex,lis%d%gnc,nr_count,' (', iam,')'
       allocate(gindex(lis%d%gnc,nr_count),stat=ierr)
       call check_error(ierr,'lisdrv_module', & 
            'Error allocating gindex.',iam)
       print*,'DBG: lisdrv_module -- size(gindex),size(glbgindex)', & 
            size(gindex(1:lis%d%gnc,1:nr_count)), & 
            size(glbgindex(1:lis%d%gnc,findex:lindex))
       gindex = glbgindex(:,findex:lindex)
    endif
    
    grid_lb = gdisp(iam) + 1
    grid_ub = gdisp(iam) + gdi(iam)
    grid_offset = gdisp(iam)
    do j = 1, nr_count 
       do i = 1, lis%d%gnc
          if ( gindex(i,j) /= -1 ) then
             if ( ( gindex(i,j) < grid_lb ) .or. & 
                  ( gindex(i,j) > grid_ub ) ) then
                gindex(i,j) = - 1
             else
                gindex(i,j) = gindex(i,j) - grid_offset
             endif
          endif
       enddo
    enddo
#endif 
#else
    if ( masterproc ) then
       allocate(gindex(lis%d%lnc,lis%d%lnr),stat=ierr)
       call check_error(ierr,'lisdrv_module', & 
            'Error allocating gindex.',iam)
       gindex=glbgindex
    endif
#endif
!EOC
  end subroutine dist_gindex
  
end module lisdrv_module
