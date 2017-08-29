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
! !ROUTINE: read_elevdiff_gtopo30
!
! !DESCRIPTION:
!  This subroutine retrieves UMD-AVHRR landcover data

! !REVISION HISTORY:
!  03 Sept 2004: Sujay Kumar; Initial Specification
!
! !INTERFACE:
subroutine read_elevdiff_gtopo30(elevdiff)
! !USES:
  use lisdrv_module, only : lis
  use lis_openfileMod
  use spmdMod

  implicit none

  integer :: line1, line2
  integer :: nc_dom, ierr
  real    :: elevdiff(lis%d%lnc, lis%d%lnr)
  integer :: c, r, glnc, glnr, line
!EOP      

  if ( lis%f%ecor .eq. 1 ) then 
     line1 = nint((lis%d%gridDesc(4)-lis%d%elev_gridDesc(1))/lis%d%gridDesc(9)) + 1
     line2 = nint((lis%d%gridDesc(5)-lis%d%elev_gridDesc(2))/lis%d%gridDesc(10)) + 1
     
     nc_dom = nint((lis%d%elev_gridDesc(4)-lis%d%elev_gridDesc(2))/&
          lis%d%elev_gridDesc(5))+1        
     
     elevdiff = 0.0
     print*, 'Reading elevation file ...',lis%p%elevfile
     call lis_open_file(21,file=lis%p%elevfile,form='unformatted', &
                        access='direct',recl=4,status='old',script='getelev.pl')
!     open(21,file=lis%p%elevfile,form='unformatted', & 
!          access='direct',recl=4,iostat=ierr)
!     if (ierr /= 0) then
!        print*, "stop: problem opening elevation difference file"
!        print*, "try running without elevation correction option."
!        call endrun
!     else
        do r=1,lis%d%lnr
           do c=1,lis%d%lnc
              glnc = line2+c-1
              glnr = line1+r-1
              line = (glnr-1)*nc_dom+glnc
              read(21,rec=line) elevdiff(c,r)
!<debug print>
print*,'DBG: gtopo30 -- c,r,elevdiff ', c,r,elevdiff(c,r)
!</debug print>
           enddo
        enddo
!     endif
     close(21)
     call lis_log_msg('MSG: read_elevdiff_gtopo30 -- Done reading elevation difference file')
  endif
  
!EOC
end subroutine read_elevdiff_gtopo30

