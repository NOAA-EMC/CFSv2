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
! !ROUTINE: create_vegtilespace
!
! !DESCRIPTION:
!  This primary goal of this routine is to determine the 
!  tiles based on dominant vegetaiton
!
! !REVISION HISTORY:
!  09 Sept 2004: Sujay Kumar ; Initial version
!
! !INTERFACE:
subroutine create_vegtilespace(fgrd, tsum, localmask, elevdiff)
  use lisdrv_module, only : lis, grid, tile, glbgindex
  use spmdMod, only : iam
  implicit none

  real :: fgrd(lis%d%lnc, lis%d%lnr, lis%p%nt)
  real :: tsum(lis%d%lnc, lis%d%lnr)
  real :: localmask(lis%d%lnc, lis%d%lnr)
  real :: elevdiff(lis%d%lnc, lis%d%lnr)
  real,allocatable :: lat(:,:)
  real,allocatable :: lon(:,:)
  real, allocatable :: domveg(:,:)
  real :: locallat, locallon
  integer :: landnveg
  integer :: c, r, t, cindex, rindex
  integer :: gnc, gnr
  integer :: count, ierr

  if(lis%d%gridDesc(9) .ne.0.01) then 
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
  else
     gnc = lis%d%lnc
     gnr = lis%d%lnr
  endif
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

  landnveg = 5 
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
  if(lis%d%gridDesc(9) .ne. 0.01 ) then 
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
                       tile(count)%row=rindex
                       tile(count)%col=cindex
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
                       tile(count)%row=rindex  
                       tile(count)%col=cindex
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
  else
     do r=1,lis%d%lnr  
         do c=1,lis%d%lnc
            do t=1,lis%p%nt
               if(localmask(c,r).gt.0.0)then 
                  if(fgrd(c,r,t).gt.0.0)then
                     count = count+1
                     tile(count)%row=r    
                     tile(count)%col=c    
                     tile(count)%index = glbgindex(c,r)
                     tile(count)%vegt=t
                     if(lis%o%wparam.eq.1) then 
                        domveg(c,r) = t*1.0
                     endif
                     tile(count)%fgrd=fgrd(c,r,t)
                     if(lis%f%ecor.eq.1) then 
                        if(elevdiff(c,r).eq.-9999.0) & 
                             elevdiff(c,r) = 0.0
                        tile(count)%elev = elevdiff(c,r)
                       endif
                  endif
!----------------------------------------------------------------------
! What if we we have land without vegetation assigned
!----------------------------------------------------------------------
                  if(tsum(c,r).eq.0.0.and.t.eq.landnveg)then  
                     count=count+1  
                     tile(count)%row=r  
                     tile(count)%col=c  
                     tile(count)%index = glbgindex(c,r)
                     tile(count)%vegt=t
                     if(lis%o%wparam.eq.1) then 
                        domveg(c,r) = t*1.0
                     endif
                     tile(count)%fgrd=1.0
                     if(lis%f%ecor.eq.1) then
                        if(elevdiff(c,r).eq.-9999.0) & 
                             elevdiff(c,r) = 0.0
                          tile(count)%elev = elevdiff(c,r)
                       endif
                  endif
               endif
            enddo
         enddo
      enddo
   endif
  if(lis%o%wparam.eq.1) then 
     open(32,file="domvegtype.bin",form='unformatted')
     write(32) domveg
     close(32)
     deallocate(domveg)
  endif
  deallocate(lat)
  deallocate(lon)
end subroutine create_vegtilespace




