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
! !MODULE: drv_output_mod
! 
! Module containing output methods for different file formats
! 
! !REVISION HISTORY:
!  10Feb2004; Sujay Kumar; Initial Specification
! 
! !INTERFACE:
module drv_output_mod
  !EOP
#if ( defined USE_NETCDF )
  use netcdf
#endif
contains

  !BOP
  ! !ROUTINE: drv_writevar_bin
  ! 
  ! !DESCRIPTION:
  !  Write the variable to an output file in different formats
  !
  ! !REVISION HISTORY:
  !  10Feb2004; Sujay Kumar, Initial Code
  !
  ! !INTERFACE:
  subroutine drv_writevar_bin(ftn, var)

    use lisdrv_module, only : lis,tile,gindex

    integer, intent(in) :: ftn
    real, intent(in)    :: var(lis%d%glbnch)
    real, allocatable :: ttmp(:)
    real, allocatable :: gtmp(:,:)

    if(lis%o%wtil.eq.1) then 
       allocate(ttmp(lis%d%glbnch))
       call t2gr(var,ttmp,lis%d%glbngrid,lis%d%glbnch,tile)
       write(ftn) ttmp
       deallocate(ttmp)
    else 
       allocate(gtmp(lis%d%lnc,lis%d%lnr))
       call tile2grid(var,gtmp,lis%d%glbnch,lis%d%lnc,& 
            lis%d%lnr,lis%d%udef,tile)
       write(ftn) gtmp
       deallocate(gtmp)
    endif
  end subroutine drv_writevar_bin

  subroutine drv_writevar_netcdf(ftn,var,dim1,varid)

    use lisdrv_module, only : lis,tile,gindex
    integer,intent(in) :: ftn,dim1
    real, intent(in)   :: var(lis%d%glbnch)
#if ( defined USE_NETCDF )
    integer :: varid, iret
    iret =nf90_put_var(ftn, varid, var, (/1,dim1/),(/lis%d%glbnch,1/))
#endif
  end subroutine drv_writevar_netcdf

  subroutine drv_writevar_netcdf3d(ftn,var,dim1,soill,varid)

    use lisdrv_module, only : lis,tile,gindex

    integer,intent(in) :: ftn,dim1,soill
!    real, intent(in)   :: var(dim1,lis%d%glbnch)
    real, intent(in)   :: var(lis%d%glbnch)
#if ( defined USE_NETCDF )
!    real, allocatable :: ttmp(:)
    integer :: varid, iret
!    allocate(ttmp(lis%d%glbnch))
!    call t2gr(var,ttmp,lis%d%glbngrid,lis%d%glbnch,tile)
    iret =nf90_put_var(ftn, varid, var, (/1,dim1,soill/),(/lis%d%glbnch,1,1/))
!    call drv_writenf3(ftn,varid,1,var,dim1,lis%d%glbnch)
!    deallocate(ttmp)
#endif
  end subroutine drv_writevar_netcdf3d


  subroutine drv_writevar_grib(ftn, var, kpds, lismask,writeint,& 
       today, yesterday)
    use lisdrv_module, only : lis,tile,gindex

    integer, intent(in)      :: ftn
    integer, intent(in)      :: kpds(25)
    character*8,intent(in)   :: today, yesterday
    real, intent(in)         :: writeint
    logical*1, intent(in) :: lismask(lis%d%lnc,lis%d%lnr)
    real, intent(in)         :: var(lis%d%glbnch)
    integer                  :: kpds_t(200),gridDesc(50)

    integer                  :: ngrid
    logical*1                :: lismask1(lis%d%lnc*lis%d%lnr)
    integer                  :: c,r,count
    real, allocatable        :: ttmp(:)

    gridDesc = 0
    gridDesc(1) = nint(lis%d%gridDesc(1))
    gridDesc(2) = nint(lis%d%gridDesc(2))
    gridDesc(3) = nint(lis%d%gridDesc(3))
    gridDesc(4) = nint(lis%d%gridDesc(4)*1000)
    gridDesc(5) = nint(lis%d%gridDesc(5)*1000)
    gridDesc(6) = nint(lis%d%gridDesc(6))
    gridDesc(7) = nint(lis%d%gridDesc(7)*1000)
    gridDesc(8) = nint(lis%d%gridDesc(8)*1000)
    gridDesc(9) = nint(lis%d%gridDesc(9)*1000)
    gridDesc(10)= nint(lis%d%gridDesc(10)*1000)
    gridDesc(11)= 0
    kpds_t= 0
    do i=1,25
       kpds_t(i) = kpds(i)
    enddo
    ngrid = lis%d%lnc*lis%d%lnr
    allocate(ttmp(ngrid))
    count = 1
    lismask1 = .false.
    ttmp = 0
!    do r=1,lis%d%lnr
    do r=lis%d%lnr,1,-1
       do c=1,lis%d%lnc
          if(gindex(c,r).ne.-1) then 
             ttmp(count) = var(gindex(c,r))
             lismask1(count) = .true.
          else
!             ttmp(count) = 9.999E+20
          endif
          
          count = count+1
       enddo
    enddo

    call makepdsn(today, yesterday, kpds_t, lis%t%hr,writeint)
    call putgb(ftn,ngrid,kpds_t,gridDesc,lismask1,ttmp,iret)

    if (iret .ne. 0) then
       print*, 'putgb failed for hour = ',lis%t%hr,',','field =',ttmp
       call endrun
    end if
    deallocate(ttmp)

  end subroutine drv_writevar_grib


  !BOP
  ! !ROUTINE: tile2grid
  ! 
  ! !DESCRIPTION:
  !  Transfer variables from tile space to grid space
  !
  ! !REVISION HISTORY:
  !  20 Oct 2003; Sujay Kumar, Initial Code
  !  23 Jun 2004; Sujar Kumar, Corrected handling of udef
  !  13 Aug 2004; James Geiger, Corrected indices for GDS-based running mode
  !
  ! !INTERFACE:
  subroutine tile2grid(t,g,nch,nc,nr,udef,tile)
    ! !USES:
    use tile_module
    use lisdrv_module,      only : glbgindex, lis
    use lis_indices_module, only : lis_g2l_row_offset, &
                                   lis_g2l_col_offset, &
                                   lis_tnroffset
    !EOP 
    implicit none

    integer, intent(in)      :: nch,nc,nr
    integer :: cindex, rindex
    real, intent(in)         :: t(nch)
    real, intent(out)        :: g(nc,nr)
    real, intent(in)         :: udef
    type(tiledec),intent(in) :: tile(nch)
    !BOC  
    integer             :: c,r,i
    g=0
    do r=1,nr
       do c=1,nc
          if(glbgindex(c,r).eq.-1) then 
             g(c,r) = udef
!             print*,  glbgindex(c,r),c,r,udef
          endif
       enddo
    enddo
!for non-1km resolutions.
!    if(lis%d%domain.ne.8) then 
!    if(lis%d%gridDesc(9) .ne.0.01) then 
!       do i=1, nch
!          c=tile(i)%col
!          r=tile(i)%row! + lis_tnroffset
          !rindex = r - nint((lis%d%gridDesc(4)-lis%d%gridDesc(44)) &
          !     /lis%d%gridDesc(9))
          !cindex = c - nint((lis%d%gridDesc(5)-lis%d%gridDesc(45)) &
          !     /lis%d%gridDesc(10))
!          rindex = r - lis_g2l_row_offset
!          cindex = c - lis_g2l_col_offset
!          g(cindex,rindex) = g(cindex,rindex) + t(i)*tile(i)%fgrd 
!       end do
!    else
       do i=1, nch
          c=tile(i)%col
          r=tile(i)%row
          g(c,r) = g(c,r) + t(i)*tile(i)%fgrd 
       end do
!    endif
!    do r=1,nr
!       do c=1,nc
!          g(c,r) = g(c,r) + t(glbgindex(c,r))*tile(glbgindex(c,r))%fgrd
!          print*, c,r,t(glbgindex(c,r)), tile(glbgindex(c,r))%fgrd, g(c,r)
!       enddo
!    enddo
    return
    !EOC
  end subroutine tile2grid

  !BOP
  !
  ! !ROUTINE: t2gr.F90
  ! 
  ! !DESCRIPTION:
  !  Aggregate variables for all tiles at each grid point
  !
  ! !REVISION HISTORY:
  !  15 Oct 1999: Paul Houser; Initial Code
  !
  ! !INTERFACE:
  subroutine t2gr(t,g,ngrid, nch,tile)
    ! !USES:
    use tile_module
    !EOP
    implicit none

    integer, intent(in)      :: nch,ngrid
    real, intent(in)         :: t(nch)
    real, intent(out)        :: g(ngrid)
    type(tiledec),intent(in) :: tile(nch)
    !BOC  
    integer       :: c,r,i
    g=0.0
    do i=1,nch
       g(tile(i)%index)=g(tile(i)%index)+t(i)*tile(i)%fgrd
    enddo
    return
    !EOC
  end subroutine t2gr

end module drv_output_mod
