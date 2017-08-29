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
! !ROUTINE: createtiles_latlon
!
! !DESCRIPTION:
!  This primary goal of this routine is to determine tile space for 
!  a lat/lon domain
!
! !REVISION HISTORY:
!  1  Oct 1999: Jared Entin; Initial code
!  15 Oct 1999: Paul Houser; Major F90 and major structure revision
!  3  Jan 2000: Minor T=0 bug fix, should have no effect on output
!  8  Mar 2000: Brian Cosgrove; Initialized FGRD to 0 For Dec Alpha Runs
! 22  Aug 2000: Brian Cosgrove; Altered code for US/Mexico/Canada Mask
! 04  Feb 2001: Jon Gottschalck; Added option to read and use Koster tile space
! 17  Oct 2003: Sujay Kumar ; Initial version of subsetting code
!
! !INTERFACE:
subroutine createtiles_latlon()
! !USES:
  use lisdrv_module, only: lis
  use grid_module
  use spmdMod
!EOP
  IMPLICIT NONE
  real, allocatable :: elevdiff(:, :)
!=== Local Variables =====================================================
  integer ::c,r,t,i,j,count  ! Loop counters
  real, allocatable :: VEG(:,:,:) !Temporary vegetation processing variable
  real :: isum
  real, allocatable :: tmpelev(:)
  integer :: landnveg
  real,allocatable :: tsum(:,:)  !Temporary processing variable
  real,allocatable :: fgrd(:,:,:)

  integer :: ios1,mlat,mlon,line,glnc,glnr
  integer :: line1,line2
  integer :: nc_dom

  integer :: ierr
  integer :: gnc, gnr
  integer :: cindex, rindex
  
  real, allocatable :: localmask(:,:)
  real :: locallat
  real :: locallon
  
!=== End Variable Definition =============================================
!BOC

  if ( masterproc ) then
     if(lis%d%gridDesc(42) > lis%d%lnc .or. &
          lis%d%gridDesc(43) > lis%d%lnr)  then !using a subdomain
        lis%d%gnc = lis%d%gridDesc(42)
        lis%d%gnr = lis%d%gridDesc(43)
     else
        lis%d%gnc = lis%d%lnc
        lis%d%gnr = lis%d%lnr
     endif

     allocate(localmask(lis%d%lnc,lis%d%lnr))
     call readlandmask(lis%d%landcover, localmask)
     

     allocate(elevdiff(lis%d%lnc, lis%d%lnr), stat=ierr)
     call check_error(ierr,'Error allocating elev diff.',iam)
     
     call readelevdiff(lis%d%elev, elevdiff)
!<debug print>
print*,'DBG: creattiles -- elevdiff'
!write(*,*) elevdiff
!</debug print>

     allocate(fgrd(lis%d%lnc,lis%d%lnr,lis%p%nt), stat=ierr)
     call check_error(ierr,'Error allocating fgrd.',iam)

     call readlandcover(lis%d%landcover, fgrd)

     allocate(tsum(lis%d%lnc, lis%d%lnr), stat=ierr)
     call check_error(ierr,'Error allocating tsum.',iam)
     tsum = 0.0
     
     call calculate_domveg(fgrd, tsum)
      
     call create_vegtilespace(fgrd, tsum, localmask, elevdiff)

     deallocate(elevdiff)

     deallocate(localmask, stat=ierr)
     call check_error(ierr,'Error allocating glbmask',iam)      
     deallocate(fgrd, stat=ierr)
     call check_error(ierr,'Error allocating glbfgrd',iam)
     deallocate(tsum, stat=ierr)
     call check_error(ierr,'Error allocating glbtsum.',iam)
     
     write(*,*) 'msg: maketiles -- actual number of tiles:', & 
          lis%d%glbnch,' (',iam,')'
     write(*,*)
     
     write(*,*) 'msg: maketiles -- size of grid dimension:', & 
          lis%d%glbngrid,' (',iam,')'
      
  endif
  print*,'MSG: maketiles -- done',' (',iam,')'   
  return
!EOC
end subroutine createtiles_latlon
