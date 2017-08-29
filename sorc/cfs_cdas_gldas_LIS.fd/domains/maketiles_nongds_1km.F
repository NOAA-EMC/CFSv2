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
! !ROUTINE: maketiles_nongds_1km.F90
!
! !DESCRIPTION:
!  This primary goal of this routine is to determine tile space for 
!  MPI-based I/O for the 1km  resolution
!
! !REVISION HISTORY:
! 15Feb2004: Sujay Kumar ; Initial version 
!
! !INTERFACE:
subroutine maketiles_nongds_1km()
! !USES:
  use lisdrv_module, only: lis, grid, glbgindex, tile
  use grid_module
  use spmdMod
!EOP
  IMPLICIT NONE
  real, allocatable :: mask(:,:)
  real, allocatable :: elevdiff(:, :)
  integer, allocatable :: pveg(:,:,:)
! for writing dominant veg types..
  real, allocatable :: domveg(:,:)
!=== Local Variables =====================================================
  integer :: ios1,mlat,mlon,line,glnc,glnr
  integer :: line1,line2
  integer :: ppp,cc,C,R,T,I,J,count  ! Loop counters
  real, allocatable :: VEG(:,:,:) !Temporary vegetation processing variable
  real :: isum
  integer, allocatable :: tmpelev(:)
  INTEGER :: KVEG, J2, LANDNVEG
  REAL :: TPGRID
  REAL    :: RSUM          !Temporary vegetation processing variable
  REAL    :: FVT(LIS%P%NT)  !Temporary vegetation processing variable
  REAL    :: MAX           !Temporary vegetation processing variable
  INTEGER :: NCHP          !Number of tiles use for array size
  real,allocatable :: tsum(:,:)  !Temporary processing variable
  real,allocatable :: lat(:,:)
  real,allocatable :: lon(:,:)
  real,allocatable :: fgrd(:,:,:)
  
  integer :: ierr
  integer :: gnc, gnr
  integer :: cindex, rindex
  
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
     allocate(lat(lis%d%lnc,lis%d%lnr), stat=ierr)
     call check_error(ierr,'Error allocating lat.',iam)
     
     allocate(lon(lis%d%lnc,lis%d%lnr), stat=ierr)
     call check_error(ierr,'Error allocating lon.',iam)
     
     allocate(mask(lis%d%lnc,lis%d%lnr), stat=ierr)
     call check_error(ierr,'Error allocating mask.',iam)
     
     nchp = lis%d%glbnch           
     print*,'MSG: maketiles -- Reading ',trim(lis%p%mfile), & 
          ' (',iam,')'
     open(30,file=lis%p%mfile,form='unformatted', & 
          access='direct',recl=4,iostat=ios1)
     line1 = nint((lis%d%gridDesc(4)-lis%d%gridDesc(44))/lis%d%gridDesc(9))+ 1
     line2 = nint((lis%d%gridDesc(5)-lis%d%gridDesc(45))/lis%d%gridDesc(10)) + 1
     do r=1,lis%d%lnr
        do c=1,lis%d%lnc
          glnc = line2+c-1
          glnr = line1+r-1
          lat(c,r) = -59.995+(glnr-1)*0.01
          lon(c,r) = -179.995 + (glnc-1)*0.01
!          print*, c,r,lat(c,r),lon(c,r)
!          mlat = (lat(c,r)-(-59.995+0.01/2))/0.01+1
!          mlon = (lon(c,r)-(-179.995+0.01/2))/0.01+1
!          line = (glnr-1)*36000+glnc
          line = (glnr-1)*nint(lis%d%gridDesc(42))+glnc
          read(30,rec=line) mask(c,r)
        enddo
     enddo
     close(30)
     if(lis%f%ecor .eq. 1) then 
        allocate(elevdiff(lis%d%lnc,lis%d%lnr), stat=ierr)
        call check_error(ierr,'Error allocating elev diff.',iam)
        print*,'MSG: maketiles -- Reading ',trim(lis%p%elevfile), & 
             ' (',iam,')'
        elevdiff = 0.0
        open(21,file=lis%p%elevfile,form='unformatted', & 
             access='direct',recl=4,iostat=ios1)
        if (ios1 /= 0) then
           print*, "stop: problem opening elevation difference file"
           print*, "try running without elevation correction option."
           call endrun
        else
           do r=1,lis%d%lnr
              do c=1,lis%d%lnc
                 glnc = line2+c-1
                 glnr = line1+r-1
!                 lat(c,r) = -59.995+(glnr-1)*0.01
!                 lon(c,r) = -179.995 + (glnc-1)*0.01
!                 mlat = (lat(c,r)-(-59.995+0.01/2))/0.01+1
!                 mlon = (lon(c,r)-(-179.995+0.01/2))/0.01+1
                 line = (glnr-1)*36000+glnc
                 read(21,rec=line) elevdiff(c,r)
              enddo
           enddo
        endif
        close(21)
        print*, 'done reading elevation difference file..'
        call absoft_release_cache()
     endif

     print*,'MSG: maketiles -- Reading ',trim(lis%p%vfile), & 
          ' (',iam,')'     
     
     allocate(tsum(lis%d%lnc,lis%d%lnr), stat=ierr)
     call check_error(ierr,'Error allocating tsum.',iam)
     tsum = 0.0

     allocate(fgrd(lis%d%lnc,lis%d%lnr,lis%p%nt), stat=ierr)
     call check_error(ierr,'Error allocating fgrd.',iam)
     
     allocate(veg(lis%d%lnc,lis%d%lnr,lis%p%nt), stat=ierr)
     call check_error(ierr,'Error allocating veg.',iam)
     
     veg = 0
!----------------------------------------------------------------------      
! Select which tile-space veg. info to use (UMD or Koster)
!----------------------------------------------------------------------      
     mask = -999.99
     open(98,file=lis%p%vfile,status='old',form='unformatted',&
          access ='direct',recl=4,iostat=ios1)
     do r=1,lis%d%lnr
        do c=1,lis%d%lnc
           glnc = line2+c-1
           glnr = line1+r-1
!           lat(c,r) = -59.995+(glnr-1)*0.01
!           lon(c,r) = -179.995+(glnc-1)*0.01
!           mlat = (lat(c,r)-(-59.995+0.01/2))/0.01+1
!           mlon = (lon(c,r)-(-179.995+0.01/2))/0.01+1
!           line = (mlat-1)*36000+mlon
           line = (glnr-1)*36000+glnc
           read(98,rec=line) tsum(c,r)
           if(tsum(c,r).gt.0) mask(c,r) = 1.0 
           if(tsum(c,r).gt.0.2) veg(c,r,NINT(tsum(c,r))) = 1.0
        enddo
     enddo
     print*,'MSG: maketiles -- Done reading ',trim(lis%p%vfile), & 
          ' (',iam,')'
     do r=1,lis%d%lnr
        do c=1,lis%d%lnc 
           isum=0.0
           do t=1,lis%p%nt 
              isum=isum+veg(c,r,t)  !recompute ISUM without water points
           enddo
           do t=1,lis%p%nt 
              fgrd(c,r,t)=0.0
              if(isum.gt.0) fgrd(c,r,t)=veg(c,r,t)/isum
           enddo
        end do
     enddo
     close(98)
     deallocate(veg)

!     open (92,file='umdveg.bin',form='unformatted')
!     write(92) lat
!     write(92) lon
!     do t=1,lis%p%nt
!        write(92) fgrd(:,:,t)
!     enddo
!     close(92)

     print*,'MSG: maketiles -- Done calculations with ',&
          trim(lis%p%vfile), & 
          ' (',iam,')'
     call absoft_release_cache()
     
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
               if(mask(c,r).gt.0.0)then !we have land
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
            if(mask(c,r).gt.0.0) then
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
            if(mask(c,r).gt.0.0) then
               grid(count)%lat = lat(c,r)
               grid(count)%lon = lon(c,r)
               grid(count)%fgrd = fgrd(c,r,:)
               glbgindex(c,r) = count
!               print*, c,r, count, lat(c,r),lon(c,r)
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
         domveg = -9999.0
      endif
      count = 0
      do r=1,lis%d%lnr  
         do c=1,lis%d%lnc
            do t=1,lis%p%nt
               if(mask(c,r).gt.0.0)then 
                  if(fgrd(c,r,t).gt.0.0)then
                     count = count+1
                     tile(count)%row=r    
                     tile(count)%col=c    
                     tile(count)%index = glbgindex(c,r)
!                     print*, count,c,r,t
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
      deallocate(mask, stat=ierr)
      call check_error(ierr,'Error allocating glbmask',iam)      
      deallocate(fgrd, stat=ierr)
      call check_error(ierr,'Error allocating glbfgrd',iam)
      deallocate(tsum, stat=ierr)
      call check_error(ierr,'Error allocating glbtsum.',iam)
      call absoft_release_cache()
      
      WRITE(*,*) 'MSG: maketiles -- Size of Tile Dimension:',NCHP, &
           ' (',iam,')'
      WRITE(*,*) 'MSG: maketiles -- Actual Number of Tiles:', & 
           LIS%D%GLBNCH,' (',iam,')'
      WRITE(*,*)
      
      WRITE(*,*) 'MSG: maketiles -- Size of Grid Dimension:', & 
           lis%d%glbngrid,' (',iam,')'
   endif
   print*,'MSG: maketiles -- done',' (',iam,')'   
   return
!EOC
 end subroutine maketiles_nongds_1km
