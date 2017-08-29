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
#include "absoft.h"
!BOP
!
! !ROUTINE: maketiles_nongds.F90
!
! !DESCRIPTION:
!  This primary goal of this routine is to determine tile space for 
!  MPI-based I/O
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
subroutine maketiles_nongds()
! !USES:
  use lisdrv_module, only: lis, grid, glbgindex, tile
  use grid_module
  use spmdMod
!EOP
  IMPLICIT NONE
  real, allocatable :: elevdiff(:, :)
  integer, allocatable :: pveg(:,:,:)
! for writing dominant veg types..
  real, allocatable :: domveg(:,:)
!=== Local Variables =====================================================
  INTEGER :: ppp,cc,C,R,T,I,J,count  ! Loop counters
  real, allocatable :: VEG(:,:,:) !Temporary vegetation processing variable
  real :: isum
  real, allocatable :: tmpelev(:)
  INTEGER :: KVEG, J2, LANDNVEG
  REAL :: TPGRID
  REAL    :: RSUM          !Temporary vegetation processing variable
  REAL    :: FVT(LIS%P%NT)  !Temporary vegetation processing variable
  REAL    :: MAX           !Temporary vegetation processing variable
  real,allocatable :: tsum(:,:)  !Temporary processing variable
  real,allocatable :: lat(:,:)
  real,allocatable :: lon(:,:)
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
        gnc = lis%d%gridDesc(42)
        gnr = lis%d%gridDesc(43)
     else
        gnc = lis%d%lnc
        gnr = lis%d%lnr
     endif
     lis%d%gnc = gnc
     lis%d%gnr = gnr

     allocate(lat(gnc,gnr), stat=ierr)
     call check_error(ierr,'Error allocating lat.',iam)
     
     allocate(lon(gnc,gnr), stat=ierr)
     call check_error(ierr,'Error allocating lon.',iam)

     do r=1,gnr
        do c=1,gnc
           lat(c,r) = lis%d%gridDesc(44)+(r-1)*lis%d%gridDesc(49)
           lon(c,r) = lis%d%gridDesc(45)+(c-1)*lis%d%gridDesc(50)
        enddo
     enddo

     allocate(localmask(lis%d%lnc,lis%d%lnr))
     call readlandmask(lis%d%domain, localmask)
     
     allocate(elevdiff(lis%d%lnc, lis%d%lnr), stat=ierr)
     call check_error(ierr,'Error allocating elev diff.',iam)
     
     call readelevdiff(lis%d%elev, elevdiff)

     allocate(fgrd(lis%d%lnc,lis%d%lnr,lis%p%nt), stat=ierr)
     call check_error(ierr,'Error allocating fgrd.',iam)

     call readlandcover(lis%d%landcover, fgrd)

     allocate(tsum(lis%d%lnc, lis%d%lnr), stat=ierr)
     call check_error(ierr,'Error allocating tsum.',iam)
     tsum = 0.0
!----------------------------------------------------------------------      
! Exclude tiles with MINA (minimum tile grid area),  
! normalize remaining tiles to 100%
!----------------------------------------------------------------------      
     do r=1,lis%d%lnr 
         do c=1,lis%d%lnc            
            rsum=0.0
            do t=1,lis%p%nt
               if(fgrd(c,r,t).lt.lis%d%mina)then
                  fgrd(c,r,t)=0.0 
               endif
               rsum=rsum+fgrd(c,r,t)
            enddo
!----------------------------------------------------------------------      
! renormalize veg fractions within a grid to 1
!----------------------------------------------------------------------      
            if(rsum.gt.0.0) then  
               do t=1,lis%p%nt  
                  if(rsum.gt.0.0)fgrd(c,r,t)=fgrd(c,r,t)/rsum
               enddo
               
               rsum=0.0
               do t=1,lis%p%nt
                  rsum=rsum+fgrd(c,r,t)
               enddo
               
               if(rsum.lt.0.9999.or.rsum.gt.1.0001)then 
                  write(*,*) 'Error1 in vegetation tiles',rsum,c,r
               endif
            endif
         enddo
      enddo
      
      allocate(pveg(lis%d%lnc,lis%d%lnr,lis%p%nt), stat=ierr)
      call check_error(ierr,'Error allocating pveg.',iam)
!----------------------------------------------------------------------      
! Exclude tiles with MAXT (Maximum Tiles per grid), 
!   normalize remaining tiles to 100%
! Determine the grid predominance order of the tiles
!  PVEG(NT) will contain the predominance order of tiles
!----------------------------------------------------------------------      
      do r=1,lis%d%lnr 
         do c=1,lis%d%lnc 
            do t=1,lis%p%nt
               fvt(t)=fgrd(c,r,t)
               pveg(c,r,t)=0
            enddo
            do i=1,lis%p%nt  
               max=0.0
               t=0
               do j=1,lis%p%nt
                  if(fvt(j).gt.max)then
                     if(fgrd(c,r,j).gt.0) then
                        max=fvt(j)
                        t=j
                     endif
                  endif
               enddo
               if(t.gt.0) then
                  pveg(c,r,t)=i
                  fvt(t)=-999.0       
               endif
            enddo
         enddo
      enddo
!----------------------------------------------------------------------      
! Impose MAXT Cutoff
!----------------------------------------------------------------------
      do r=1,lis%d%lnr 
         do c=1,lis%d%lnc 
            rsum=0.0
            do t=1,lis%p%nt
               if(pveg(c,r,t).lt.1) then
                  fgrd(c,r,t)=0.0    
                  pveg(c,r,t)=0  
               endif
               if(pveg(c,r,t).gt.lis%d%maxt) then
                  fgrd(c,r,t)=0.0            
                  pveg(c,r,t)=0  
               endif
               rsum=rsum+fgrd(c,r,t)
            enddo
!----------------------------------------------------------------------
! renormalize veg fractions within a grid to 1
!----------------------------------------------------------------------
            if(rsum.gt.0.0) then  
               do t=1,lis%p%nt  
                  if(rsum.gt.0.0)fgrd(c,r,t)= fgrd(c,r,t)/rsum
               enddo
               
               rsum=0.0
               do t=1,lis%p%nt
                  rsum=rsum+ fgrd(c,r,t)  !recalculate rsum to check 
               enddo
               tsum(c,r)=rsum
               
               if(rsum.lt.0.9999.or.rsum.gt.1.0001)then  !check renormalization
                  write(*,*) 'Error2 in vegetation tiles',rsum,c,r
               endif
            endif
         enddo
      enddo
      deallocate(pveg)
      call absoft_release_cache()
      
      landnveg = 5 
!----------------------------------------------------------------------
!  Make Tile Space
!----------------------------------------------------------------------
      lis%d%glbnch=0
      do t=1,lis%p%nt 
         do r=1,lis%d%lnr      
            do c=1,lis%d%lnc   
               if(localmask(c,r).gt.0.99.and. & 
                    localmask(c,r).lt.3.01)then !we have land
                  if(fgrd(c,r,t).gt.0.0)then
                     lis%d%glbnch=lis%d%glbnch+1 
                  endif
                  if(tsum(c,r).eq.0.0.and.t.eq.landnveg)then 
                     lis%d%glbnch=lis%d%glbnch+1 
                  endif
               endif
            enddo
         enddo
      enddo
      
      print*, 'DBG: maketiles -- glbnch',lis%d%glbnch,' (',iam,')'
      allocate(tile(lis%d%glbnch))

      lis%d%glbngrid=0
      do r=1,lis%d%lnr
         do c=1,lis%d%lnc
            if(localmask(c,r).gt.0.99 .and. & 
                 localmask(c,r).lt.3.01) then
               lis%d%glbngrid=lis%d%glbngrid+1
            endif
         enddo
      enddo
      count = 1
      print*, 'DBG: maketiles1 -- glbnch',lis%d%glbnch,' (',iam,')'
      allocate(grid(lis%d%glbngrid))
      allocate(glbgindex(lis%d%lnc, lis%d%lnr))
      print*, 'DBG: maketiles2 -- glbnch',lis%d%glbnch,' (',iam,')'
      do r=1,lis%d%lnr
         do c=1,lis%d%lnc
            glbgindex(c,r) = -1
            if(localmask(c,r).gt.0.99 .and. & 
                 localmask(c,r).lt.3.01) then
               locallat = lis%d%gridDesc(4)+(r-1)*lis%d%gridDesc(9)
               locallon = lis%d%gridDesc(5)+(c-1)*lis%d%gridDesc(10)
               grid(count)%lat = locallat
               grid(count)%lon = locallon
               grid(count)%fgrd = fgrd(c,r,:)
               glbgindex(c,r) = count
               count = count+1
            endif
         enddo
      enddo
      print*, 'DBG: maketiles3 -- glbnch',lis%d%glbnch,' (',iam,')'
!--------------------------------------------------------------------
!   For writing dominant Vegetation types
!--------------------------------------------------------------------
      if(lis%o%wparam .eq.1) then 
         allocate(domveg(lis%d%lnc,lis%d%lnr))
         domveg = 0
      endif
      count = 0
      do r=1,gnr  
         do c=1,gnc
            if(lat(c,r).ge.lis%d%gridDesc(4).and. & 
                 lat(c,r).le.lis%d%gridDesc(7).and. & 
                 lon(c,r).ge.lis%d%gridDesc(5).and. & 
                 lon(c,r).le.lis%d%gridDesc(8)) then
               rindex = r - nint((lis%d%gridDesc(4)-lis%d%lc_gridDesc(1)) &
                    /lis%d%gridDesc(9))
               cindex = c - nint((lis%d%gridDesc(5)-lis%d%lc_gridDesc(2)) &
                    /lis%d%gridDesc(10))
               do t=1,lis%p%nt
                  if(localmask(cindex,rindex).gt.0.99.and. & 
                       localmask(cindex,rindex).lt.3.01)then 
                     if(fgrd(cindex,rindex,t).gt.0.0)then
                        count = count+1
                        tile(count)%row=r    
                        tile(count)%col=c    
                        tile(count)%index = glbgindex(cindex,rindex)
                        tile(count)%vegt=t
                        if(lis%o%wparam.eq.1) then 
                           domveg(cindex,rindex) = t*1.0
                        endif
                        tile(count)%fgrd=fgrd(cindex,rindex,t)
                        if(lis%f%ecor.eq.1) then 
                           if(elevdiff(cindex,rindex).eq.-9999.0) then
                                elevdiff(cindex,rindex) = 0.0
                           endif
                           tile(count)%elev=elevdiff(cindex,rindex)
                        endif
                     endif
!----------------------------------------------------------------------
! What if we we have land without vegetation assigned
!----------------------------------------------------------------------
                     if(tsum(cindex,rindex).eq.0.0.and.t.eq.landnveg)then  
                        count=count+1  
                        tile(count)%row=r  
                        tile(count)%col=c  
                        tile(count)%index = glbgindex(cindex,rindex)
                        tile(count)%vegt=t
                        if(lis%o%wparam.eq.1) then 
                           domveg(cindex,rindex) = t*1.0
                        endif
                        tile(count)%fgrd=1.0
                        if(lis%f%ecor.eq.1) then 
                           if(elevdiff(c,r).eq.-9999.0) & 
                                elevdiff(c,r) = 0.0
                           tile(count)%elev=elevdiff(cindex,rindex)
                        endif
                     endif
                  endif
               enddo
            endif
         enddo
      enddo
      if(lis%o%wparam.eq.1) then 
         open(32,file="domvegtype.bin",form='unformatted')
         write(32) domveg
         close(32)
         deallocate(domveg)
      endif
      if(lis%f%ecor.eq.1)  deallocate(elevdiff)
      deallocate(lat)
      deallocate(lon)
      print*, 'DBG: maketiles4 -- glbnch',lis%d%glbnch,' (',iam,')'
      deallocate(localmask, stat=ierr)
      call check_error(ierr,'Error allocating glbmask',iam)      
      deallocate(fgrd, stat=ierr)
      call check_error(ierr,'Error allocating glbfgrd',iam)
      deallocate(tsum, stat=ierr)
      call check_error(ierr,'Error allocating glbtsum.',iam)
      call absoft_release_cache()
      
      WRITE(*,*) 'MSG: maketiles -- Actual Number of Tiles:', & 
           LIS%D%GLBNCH,' (',iam,')'
      WRITE(*,*)
      
      WRITE(*,*) 'MSG: maketiles -- Size of Grid Dimension:', & 
           lis%d%glbngrid,' (',iam,')'
      
   endif
   print*,'MSG: maketiles -- done',' (',iam,')'   
   return
!EOC
end subroutine maketiles_nongds
