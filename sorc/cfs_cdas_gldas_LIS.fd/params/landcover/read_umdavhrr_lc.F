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
! !ROUTINE: read_umdavhrr_lc
!
! !DESCRIPTION:
!  This subroutine retrieves UMD-AVHRR landcover data

! !REVISION HISTORY:
!  03 Sept 2004: Sujay Kumar; Initial Specification
!
! !INTERFACE:
subroutine read_umdavhrr_lc(fgrd)
! !USES:
  use lisdrv_module, only : lis
  use spmdMod
!EOP      
  implicit none
  real,allocatable :: lat(:,:)
  real,allocatable :: lon(:,:)
  real, allocatable :: veg(:,:,:) 
  real, allocatable :: tsum(:,:)
  real  :: fgrd(lis%d%lnc,lis%d%lnr,lis%p%nt)
  real :: isum
  integer :: cindex, rindex, c,r
  integer :: t, ierr, ios1
  integer :: line1, line2, glnc, glnr, line
  
  fgrd = 0.0

  if(lis%d%gridDesc(9) .ne. 0.01) then 
     allocate(lat(lis%d%gnc,lis%d%gnr), stat=ierr)
     call check_error(ierr,'Error allocating lat.',iam)
     
     allocate(lon(lis%d%gnc,lis%d%gnr), stat=ierr)
     call check_error(ierr,'Error allocating lon.',iam)
     
     allocate(veg(lis%d%gnc,lis%d%gnr,lis%p%nt), stat=ierr)
     call check_error(ierr,'Error allocating veg',iam)
     
     print*,'MSG: maketiles -- Reading ',trim(lis%p%vfile), & 
          ' (',iam,')'
     open(98,file=lis%p%vfile,form='unformatted')
     read(98) lat
     read(98) lon
     do t = 1, lis%p%nt
        read(98) veg(:,:,t)
     enddo
     
     print*,'MSG: maketiles -- Done reading ',trim(lis%p%vfile), & 
          ' (',iam,')'
     do r=1,lis%d%gnr 
        do c=1,lis%d%gnc 
           isum=0.0
           if(lat(c,r).ge.lis%d%gridDesc(4).and. & 
                lat(c,r).le.lis%d%gridDesc(7).and. & 
                lon(c,r).ge.lis%d%gridDesc(5).and. & 
                lon(c,r).le.lis%d%gridDesc(8)) then
              rindex = r - nint((lis%d%gridDesc(4)-lis%d%lc_gridDesc(1)) &
                   /lis%d%gridDesc(9))
              cindex = c - nint((lis%d%gridDesc(5)-lis%d%lc_gridDesc(2)) &
                   /lis%d%gridDesc(10))
              do t=1,lis%p%nt 
                 fgrd(cindex,rindex,t) = veg(c,r,t)
              enddo
           end if
        enddo
     enddo
     close(98)
     deallocate(veg)
     deallocate(lat)
     deallocate(lon)
  else
     allocate(tsum(lis%d%lnc,lis%d%lnr), stat=ierr)
     call check_error(ierr,'Error allocating tsum.',iam)

     allocate(veg(lis%d%lnc,lis%d%lnr,lis%p%nt), stat=ierr)
     call check_error(ierr,'Error allocating veg.',iam)

     tsum = 0.0
     line1 = nint((lis%d%gridDesc(4)-lis%d%gridDesc(44))/lis%d%gridDesc(9))+ 1
     line2 = nint((lis%d%gridDesc(5)-lis%d%gridDesc(45))/lis%d%gridDesc(10)) + 1

     open(98,file=lis%p%vfile,status='old',form='unformatted',&
          access ='direct',recl=4,iostat=ios1)
     do r=1,lis%d%lnr
        do c=1,lis%d%lnc
           glnc = line2+c-1
           glnr = line1+r-1
           line = (glnr-1)*36000+glnc
           read(98,rec=line) tsum(c,r)
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
     deallocate(tsum)
  endif
  
!EOC
end subroutine read_umdavhrr_lc
