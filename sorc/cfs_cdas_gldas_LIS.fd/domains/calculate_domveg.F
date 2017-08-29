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
! !ROUTINE: calculate_domveg
!
! !DESCRIPTION:
!  This primary goal of this routine is to determine the 
!  percentages of dominant vegetation to create tiles
!
! !REVISION HISTORY:
!  09 Sept 2004: Sujay Kumar ; Initial version
!
! !INTERFACE:
subroutine calculate_domveg(fgrd, tsum)
  use lisdrv_module, only : lis
  use spmdMod, only : iam
  implicit none

  real :: fgrd(lis%d%lnc, lis%d%lnr, lis%p%nt)
  real :: tsum(lis%d%lnc, lis%d%lnr)

  integer, allocatable :: pveg(:,:,:)
  integer :: c, r, t, ierr, i, j
  real    :: rsum         
  real    :: fvt(lis%p%nt)  
  real    :: max
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
end subroutine calculate_domveg
