!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
#include <misc.h>
!BOP
!
! !MODULE: gswp_module.F90
! 
! !DESCRIPTION:
!   This module contains useful routines that generates indices 
!   for reading the GSWP data
! 
! !REVISION HISTORY: 
!  24Feb04    Sujay Kumar  Initial Specification
! 
! !INTERFACE:
module gswp_module
    
contains
  subroutine getgswp_monindex(yr,mo,index)
    integer, intent(out) :: index
    integer, intent(in) :: yr, mo
    integer :: k 
    logical :: leap 
    index = 0
    index = index + (yr-1982)*12 + mo
  end subroutine getgswp_monindex
  
  subroutine getgswp_timeindex(yr,mo,da,hr,index)
    
    implicit none
    integer, intent(in)  :: yr, mo, da, hr
    integer, intent(out) :: index
    integer :: ryr, rmo, rda, rhr, days1(12),days2(12),yrs(13)
    integer :: k
    data yrs /184,365,366,365,365,365,366,365,365,365,366,365,365/ !from 1983-94
    data days1 /31,28,31,30,31,30,31,31,30,31,30,31/
    data days2 /31,29,31,30,31,30,31,31,30,31,30,31/
    logical :: leap 
    ryr = 1982
    rmo = 7
    rda = 1
    rhr = 0
    
    index = 0
    if(yr.gt.1982) then 
       if((mod(yr,4).eq.0.and.mod(yr,100).ne.0) &     !correct for leap year
            .or.(mod(yr,400).eq.0))then             !correct for y2k
          leap = .true.                   
       else
          leap = .false.
       endif
       k = mo
       index = index + hr+ (da-1)*24 
       do while(k.gt.1)
          if(leap) then 
             index = index + days2(k-1) *24  !current year's
          else 
             index = index + days1(k-1) *24  !current year's
          endif
          k = k-1
       enddo
       ! now add the missing years..
       k = yr-1982
       do while(k.ge.1) 
          index = index + yrs(k)*24
          k = k-1
       enddo
       index = index/3
    else
       if(mo.gt.7) then 
          k=mo
          index = index+hr+(da-1)*24
          do while(k .gt.7) 
             index = index + days1(k-1)*24
             k = k-1
          end do
       else
          index = hr+(da-1)*24
       endif
       index = index/3
    endif
    index = index+1
    !convert times to 3 hour index
    
  end subroutine getgswp_timeindex
  
end module gswp_module
