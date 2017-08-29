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
! !ROUTINE: read_statsgosoils
!
! !DESCRIPTION:
!  This subroutine retrieves STATSGO soils data

! !REVISION HISTORY:
!  03 Sept 2004: Sujay Kumar; Initial Specification
!
! !INTERFACE:
subroutine read_statsgosilt(array)
! !USES:
  use lisdrv_module, only : lis, tile
  use lis_openfileMod
  use lis_indices_module
!EOP      
  implicit none
  real,          intent(inout) :: array(lis_nc_data, lis_nr_data)

  integer :: line1, line2, line
  integer :: c,r, glnc, glnr
  integer :: nc_dom
  print*, 'MSG: reading STATSGO silt file ',lis%p%sifile
  call lis_set_filename(lis%p%sifile) 
  call lis_open_file(15,file=lis%p%sifile,form='unformatted',status='old',&
                     access='direct',recl=4, script='getsilt.pl')
  line1 = nint((lis%d%gridDesc(4)-lis%d%soil_gridDesc(1))/lis%d%gridDesc(9))+1
  line2 = nint((lis%d%gridDesc(5)-lis%d%soil_gridDesc(2))/lis%d%gridDesc(10))+1

  nc_dom = nint((lis%d%soil_gridDesc(4)-lis%d%soil_gridDesc(2))/&
       lis%d%soil_gridDesc(6))+1
  do r=1,lis%d%lnr
     do c=1,lis%d%lnc
        glnc = line2+c-1
        glnr = line1+r-1
        line = (glnr-1)*nc_dom+glnc
        read(15,rec=line) array(c,r)
     enddo
  enddo

  print*, 'MSG: setnoahp -- read silt file'

!EOC
end subroutine read_statsgosilt

