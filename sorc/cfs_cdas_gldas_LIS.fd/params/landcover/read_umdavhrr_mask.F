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
! !ROUTINE: read_umdavhrr_mask
!
! !DESCRIPTION:
!  This subroutine retrieves UMD-AVHRR landcover data

! !REVISION HISTORY:
!  03 Sept 2004: Sujay Kumar; Initial Specification
!
! !INTERFACE:
subroutine read_umdavhrr_mask(localmask)
! !USES:
  use lisdrv_module, only : lis
  use spmdMod

!EOP      
  implicit none
  real,allocatable :: lat(:,:)
  real,allocatable :: lon(:,:)
  real, allocatable :: mask(:,:)

  real :: localmask(lis%d%lnc,lis%d%lnr)

  integer :: cindex, rindex, c,r
  integer :: t, ierr, ios1
  integer :: glnc, glnr, line, line1, line2

  if(lis%d%gridDesc(9) .ne. 0.01) then 
     allocate(lat(lis%d%gnc,lis%d%gnr), stat=ierr)
     call check_error(ierr,'Error allocating lat.',iam)
     
     allocate(lon(lis%d%gnc,lis%d%gnr), stat=ierr)
     call check_error(ierr,'Error allocating lon.',iam)
     
     allocate(mask(lis%d%gnc, lis%d%gnr), stat=ierr)
     call check_error(ierr,'Error allocating mask.',iam)

     print*,'MSG: maketiles -- Reading ',trim(lis%p%mfile), & 
          ' (',iam,')'
     open(30,file=lis%p%mfile,form='unformatted',status='old')
     read(30) lat
     read(30) lon
     read(30) mask
     close(30)
     print*,'MSG: maketiles -- Done reading ',trim(lis%p%mfile), & 
          ' (',iam,')'
     localmask = 0

     do r=1,lis%d%gnr
        do c=1,lis%d%gnc
           if(lat(c,r).ge.lis%d%gridDesc(4).and. & 
                lat(c,r).le.lis%d%gridDesc(7).and. & 
                lon(c,r).ge.lis%d%gridDesc(5).and. & 
                lon(c,r).le.lis%d%gridDesc(8)) then
              rindex = r - nint((lis%d%gridDesc(4)-lis%d%lc_gridDesc(1)) &
                   /lis%d%gridDesc(9))
              cindex = c - nint((lis%d%gridDesc(5)-lis%d%lc_gridDesc(2)) &
                   /lis%d%gridDesc(10))
              localmask(cindex,rindex) = mask(c,r)
           endif
        end do
     end do
     deallocate(lat)
     deallocate(lon)
     deallocate(mask)
  else
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
          line = (glnr-1)*nint(lis%d%gridDesc(42))+glnc
          read(30,rec=line) localmask(c,r)
        enddo
     enddo
  endif

  call absoft_release_cache()

  
!EOC
end subroutine read_umdavhrr_mask
