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
! !MODULE: time_manager.F90
!
! !DESCRIPTION:
! 
! This module contains wrapper functions that uses the ESMF time manager
! for time managment
!
! !INTERFACE:
module time_manager

   use spmdMod, only: masterproc
   use lis_module
   use precision

!EOP
   implicit none

! Public methods
!BOP
! !ARGUMENTS:
   public :: &
      timemgr_init,             &! time manager initialization
      advance_timestep,         &! increment timestep number
      get_step_size,            &! return step size in seconds
      get_nstep,                &! return timestep number
      get_curr_date,            &! return date components at end of current timestep
      get_prev_date,            &! return date components at beginning of current timestep
      get_start_date,           &! return date components of the start date
      get_ref_date,             &! return date components of the reference date
      get_curr_time,            &! return components of elapsed time since reference date
      get_curr_calday,          &! return calendar day at end of current timestep
      is_last_step,             &! return true on last timestep
      timemgr_write_restart,    &! write info to file needed to restart the time manager
      timemgr_read_restart,     &! read info from file needed to restart the time manager
      timemgr_restart,          &! restart the time manager
      tick,                     &
      date2time


!=========================================================================================
contains
!=========================================================================================
!BOP
! !ROUTINE: timemgr_init
! 
! !DESCRIPTION:
! 
! Initialize the ESMF time manager.
!
! NOTE - This assumes that the namelist variables 
! have been set before this routine is called.  
!
! !INTERFACE:
subroutine timemgr_init(lt)
!EOP
   implicit none
   type(listime) :: lt

! Local variables
   call date2time(lt%etime,lt%edoy,lt%egmt, & 
        lt%eyr,lt%emo,lt%eda,lt%ehr,lt%emn,lt%ess)
   lt%tscount = 0 
!-----------------------------------------------------------------------
! Print configuration summary to log file (stdout).
!-----------------------------------------------------------------------
   if (masterproc) then
      call timemgr_print(lt)
   end if
!EOC
end subroutine timemgr_init


!BOP
! !ROUTINE: timemgr_print
!
! !DESCRIPTION:
!
! Restart the ESMF time manager.
!
! NOTE - Assumptions:
! 1) The namelist variables have been set before this routine is called.\\
!    The stop date is the only thing that can be changed by the user on a restart.\\
! 2) Restart data have been read on the master process before this routine is called.\\
!    (timemgr$_-$read$_-$restart called from control/restart.F90::read$_-$restart)\\
!
! !INTERFACE:
subroutine timemgr_restart()
!EOP
   implicit none

! Local variables

!-----------------------------------------------------------------------
! Print configuration summary to log file (stdout).
!-----------------------------------------------------------------------
!   if (masterproc) then
!      call timemgr_print()
!   end if
!EOC
end subroutine timemgr_restart
!=========================================================================================
!BOP
! !ROUTINE: timemgr_print
!
! !DESCRIPTION:
! 
! Prints the time manager information
! 
! !INTERFACE:
subroutine timemgr_print(lt)
!EOP
   implicit none
   type(listime) :: lt
! Local variables

   write(6,*)' ************************************************'

   write(6,*)' Timestep size (seconds):  ', lt%ts
   write(6,*)' Start date (ymd tod):     ', lt%syr, lt%smo, lt%sda
   write(6,*)' Stop date (ymd tod):      ', lt%eyr, lt%emo, lt%eda
   write(6,*)' Current step number:      ', lt%tscount
   write(6,*)' Current date (ymd tod):   ', lt%yr, lt%mo, lt%da

   write(6,*)' ************************************************'
!EOC
end subroutine timemgr_print
!=========================================================================================
!BOP
! !ROUTINE: advance_timestep
!
! !DESCRIPTION:
! 
! Increment the timestep number.
!
! !INTERFACE:
subroutine advance_timestep(lt)
!EOP

   implicit none
   
   type(listime) :: lt
! Local variables
  integer :: days(12)
  data days /31,28,31,30,31,30,31,31,30,31,30,31/
!BOC
  lt%ss = lt%ss + lt%ts
  
  do while(lt%ss .gt. 59) 
     lt%ss = lt%ss - 60 
     lt%mn = lt%mn + 1
  enddo
  
  do while(lt%mn .gt.59)
     lt%mn = lt%mn -60
     lt%hr = lt%hr+1
  enddo
  
  do while(lt%hr .ge.24) 
     lt%hr = lt%hr -24
     lt%da = lt%da +1
  enddo
  
  if((mod(lt%yr,4) .eq. 0 .and. mod(lt%yr, 100).ne.0) &!leap year
       .or.(mod(lt%yr,400) .eq.0)) then 
     days(2) = 29
  else 
     days(2) = 28
  endif
  
  do while(lt%da.gt.days(lt%mo))
     lt%da = lt%da - days(lt%mo)
     lt%mo = lt%mo + 1
     if(lt%mo.gt.12) exit
  enddo
  
  do while(lt%mo .gt. 12) 
     lt%mo = lt%mo-12
     lt%yr = lt%yr +1
  enddo
  
  call date2time(lt%time,lt%doy,lt%gmt,& 
       lt%yr, lt%mo, lt%da, lt%hr, lt%mn, lt%ss)

  lt%tscount = lt%tscount + 1

  write(*,24)'GSFC-LIS time: ',lt%mo,'/',lt%da,'/', & 
       lt%yr,lt%hr,':',lt%mn,':',lt%ss
24 format(a16,i2,a1,i2,a1,i4,1x,i2,a1,i2,a1,i2)

  if(lt%endcode.eq.0)then  !end at real-time date (tbd)
     write(*,*)'warning: do not know how to stop in real-time' 
  !   write(79,*)'warning: do not know how to stop in real-time'
  endif
  if(lt%endcode.eq.1)then  !end on date specified in lis.crd file
     call date2time(lt%etime,lt%edoy,lt%egmt, & 
          lt%eyr,lt%emo,lt%eda,lt%ehr,lt%emn,lt%ess)
     if(lt%time.ge.lt%etime)then
        lt%endtime=1
        write(*,*) 'GSFC-LDAS run completed'
  !      write(79,*) 'GSFC-LDAS run completed'
     endif
  endif

!EOC
end subroutine advance_timestep
!=========================================================================================
!BOP
! !ROUTINE: get_step_size
!
! !DESCRIPTION:
!
! Return the step size in seconds.
!
! !INTERFACE:
function get_step_size(lt)
!EOP

   implicit none
   type(listime) :: lt
! Return value
   integer :: get_step_size

! Local variables
   get_step_size = lt%ts
!EOC
end function get_step_size
!=========================================================================================
!BOP
! !ROUTINE: get_nstep
!
! !DESCRIPTION: 
!
! Return the timestep number.
! 
! !INTERFACE:
function get_nstep(lt)
!EOP
   implicit none
   type(listime) :: lt
! Return value
   integer :: get_nstep

! Local variables
   get_nstep = lt%tscount
!EOC
end function get_nstep
!=========================================================================================
!BOP
! !ROUTINE: get_curr_day
! 
! !DESCRIPTION: 
!
! Return date components valid at end of current timestep with an optional
! offset (positive or negative) in seconds.
!
! !INTERFACE:
subroutine get_curr_date(lt, yr, mon, day, tod, offset)
!EOP
   implicit none
   
! Arguments
   type(listime) :: lt
   integer, intent(out) ::&
      yr,    &! year
      mon,   &! month
      day,   &! day of month
      tod     ! time of day (seconds past 0Z)

   integer, optional, intent(in) :: offset  ! Offset from current time in seconds.
                                            ! Positive for future times, negative 
                                            ! for previous times.
   yr = lt%yr
   mon = lt%mo
   day = lt%da
   tod = lt%ss+lt%mn*60+lt%hr*3600
!EOC
end subroutine get_curr_date
!=========================================================================================
!BOP
! !ROUTINE: get_prev_date
! 
! !DESCRIPTION:
!
! Return date components valid at beginning of current timestep.
!
! !INTERFACE:
subroutine get_prev_date(yr, mon, day, tod)
!EOP
   implicit none
   
! Arguments
   integer, intent(out) ::&
      yr,    &! year
      mon,   &! month
      day,   &! day of month
      tod     ! time of day (seconds past 0Z)
   
   print*, 'get_prev_date not implemented, stopping..'
   call endrun
!EOC
end subroutine get_prev_date
!=========================================================================================
!BOP
! !ROUTINE: get_start_date
!
! !DESCRIPTION:
!
! Return date components valid at beginning of initial run.
!
! !INTERFACE:
!
subroutine get_start_date(yr, mon, day, tod)
!EOP
   implicit none
   
! Arguments
   integer, intent(out) ::&
      yr,    &! year
      mon,   &! month
      day,   &! day of month
      tod     ! time of day (seconds past 0Z)

! Local variables
   print*, 'get_start_date not implemented, stopping..'
   call endrun
!EOC
end subroutine get_start_date
!=========================================================================================
!BOP
! !ROUTINE: get_ref_date
! 
! !DESCRIPTION:
!
! Return date components of the reference date.
! 
! !INTERFACE:
subroutine get_ref_date(yr, mon, day, tod)
!EOP
   implicit none
   
! Arguments
   integer, intent(out) ::&
      yr,    &! year
      mon,   &! month
      day,   &! day of month
      tod     ! time of day (seconds past 0Z)

! Local variables
   print*, 'get_ref_date not implemented, stopping..'
   call endrun
!EOC
end subroutine get_ref_date
!=========================================================================================
!BOP
! !ROUTINE: get_curr_time
! 
! !DESCRIPTION:
! 
! Return time components valid at end of current timestep.
! Current time is the time interval between the current date and the reference date.
!
! !INTERFACE:
subroutine get_curr_time(days, seconds)
!EOP

   implicit none
   
! Arguments
   integer, intent(out) ::&
      days,   &! number of whole days in time interval
      seconds  ! remaining seconds in time interval

! Local variables

!EOC
end subroutine get_curr_time
!=========================================================================================
!BOP
! !ROUTINE: get_curr_calday
! 
! !DESCRIPTION:
!
! Return calendar day at end of current timestep with optional offset.
! Calendar day 1.0 = 0Z on Jan 1.
! 
! !INTERFACE:
function get_curr_calday(lt,offset)
!EOP

   implicit none
   
! Arguments
   type(listime) :: lt
   integer, optional, intent(in) :: offset  ! Offset from current time in seconds.
                                            ! Positive for future times, negative 
                                            ! for previous times.
! Return value
   real(r8) :: get_curr_calday
   integer :: days(12)
   integer :: i
   data days /31,28,31,30,31,30,31,31,30,31,30,31/

   get_curr_calday = 0
   if((mod(lt%yr,4) .eq. 0 .and. mod(lt%yr, 100).ne.0) &!leap year
        .or.(mod(lt%yr,400) .eq.0)) then 
      days(2) = 29
   else 
      days(2) = 28
   endif
   if(lt%mo .ne. 1) then 
      do i=1,lt%mo-1
         get_curr_calday = get_curr_calday+days(i)
      enddo
   endif
   get_curr_calday = get_curr_calday + real(lt%da) + real(lt%hr)/24 + &
        real(lt%mn)/(24*60) + real(lt%ss)/(24*60*60)
   
   if (present(offset)) then
      if (offset > 0) then
         get_curr_calday = get_curr_calday + real(offset)/(24*60*60)
      else if (offset < 0) then
         get_curr_calday = get_curr_calday - real(offset)/(24*60*60)
      endif
   endif

!EOC
end function get_curr_calday


!=========================================================================================
!BOP
! !ROUTINE: is_last_step
! 
! !DESCRIPTION:
!
! Return true on last timestep.
!
! !INTERFACE:
function is_last_step(lt)
!EOP
   implicit none
   type(listime) :: lt
! Return value
   logical :: is_last_step

! Local variables
   is_last_step = .false.
   if(lt%time .ge. lt%etime) then
      is_last_step = .true.
   endif
end function is_last_step
!=========================================================================================
!BOP
! !ROUTINE: timemgr_write_restart
! 
! !DESCRIPTION:
! 
! Write information needed on restart to a binary Fortran file.
! It is assumed that this routine is called only from the master proc if in SPMD mode.
! 
! !INTERFACE:
subroutine timemgr_write_restart(ftn_unit)
!EOP

   implicit none

! Arguments
   integer, intent(in) :: ftn_unit  ! Fortran unit number


end subroutine timemgr_write_restart
!=========================================================================================
!BOP
! !ROUTINE: timemgr_read_restart
! 
! !DESCRIPTION:
! 
! Read information needed on restart from a binary Fortran file.
! It is assumed that this routine is called only from the master proc if in SPMD mode.
! 
! !INTERFACE:
subroutine timemgr_read_restart(ftn_unit)
!EOP
   implicit none

! Arguments
   integer, intent(in) :: ftn_unit  ! Fortran unit number

end subroutine timemgr_read_restart
!BOP
!
! !DESCRIPTION:
!  determines time in years, based on year, month, day hour etc..
!   or reverse (date2time).  
!
! !REVISION HISTORY:
!  15 oct 1999: paul houser; initial code
!  21 feb 2002: brian cosgrove; corrected leap year code line.  days(2)
!               was not being reset to 28 after leaving a leap year,
!               it was staying 29
! !INTERFACE:
  subroutine date2time(time,doy,gmt,yr,mo,da,hr,mn,ss)

    implicit none
! !ARGUMENTS:
    integer yr,mo,da,hr,mn,ss,yrdays,doy,days(13),k
    real*8 time
    real gmt
!EOP
    data days /31,28,31,30,31,30,31,31,30,31,30,31,30/
!BOC    
    if((mod(yr,4).eq.0.and.mod(yr,100).ne.0) &     !correct for leap year
         .or.(mod(yr,400).eq.0))then             !correct for y2k
       yrdays=366                  
    else
       yrdays=365
    endif
    
    doy=0
    do k=1,(mo-1)
       doy=doy+days(k)
    enddo
    doy=doy+da
    
    if(yrdays.eq.366.and.mo.gt.2)doy=doy+1
    
    time=(dfloat(yr)+((((((dfloat(ss)/60.d0)+dfloat(mn))/60.d0)+ & 
         dfloat(hr))/24.d0)+dfloat(doy-1))/dfloat(yrdays))
    
    gmt=( ( (float(ss)/60.0) +float(mn)) /60.0)+float(hr)
    return
!EOC
  end subroutine date2time

!BOP
! 
! !DESCRIPTION:
!  advance (or retract) time variables a specified amount 
!  (a nonmodular version of ticktime.f.)
!
! !REVISION HISTORY:
!  1  oct 1999: jared entin; initial code
!  15 oct 1999: paul houser; significant f90 revision
! 
! !INTERFACE:
  subroutine tick(time,doy,gmt,yr,mo,da,hr,mn,ss,ts)
    implicit none
! !ARGUMENTS:
    real*8 time
    integer days(13)
    integer yr,mo,da,hr,mn,ss,ts,doy
    real gmt
!EOP
    
    integer prvmo   !previous month
    

    data days/31,28,31,30,31,30,31,31,30,31,30,31,31/

!=== end variable list ===================================================
!BOC
143 format(a1,' yr',i6,' mo',i5,' dy',i5,' hr',i5, & 
         ' mn',i6,' ss',i8,' ts',i8)
    ss=ss+ts
    do while(ss.gt.59)
       ss=ss-60
       mn=mn+1
    enddo
    do while(ss.lt.0)
       ss=ss+60
       mn=mn-1
    enddo
    do while(mn.gt.59)
       mn=mn-60
       hr=hr+1
    enddo
    
    do while(mn.lt.0)
       mn=mn+60
       hr=hr-1
    enddo
    do while(hr.gt.23)
       hr=hr-24
       da=da+1
    enddo
    
    do while(hr.lt.0)
       hr=hr+24
       da=da-1
    enddo
    
    if((mod(yr,4).eq.0.and.mod(yr,100).ne.0) &    !correct for leap year
         .or.(mod(yr,400).eq.0))then               !correct for y2k
       days(2)=29                  
    else
       days(2)=28
    endif
    
    do while(da.gt.days(mo))
       da=da-days(mo)
       mo=mo+1
    enddo
    
    do while(da.lt.1)
       
       prvmo=mo-1
       if(mo.eq.1) prvmo=12
       
       da=da+days(prvmo)
       
       if(prvmo.eq.12) then
          mo=prvmo
          yr=yr-1
       else
          mo=prvmo
       endif
    enddo
    do while(mo.gt.12)
       mo=mo-12
       yr=yr+1
    enddo

    do while(mo.lt.1)
       mo=mo+12
       yr=yr-1
    enddo
    call date2time(time,doy,gmt,yr,mo,da,hr,mn,ss)
    return
!EOC
  end subroutine tick

  subroutine time2date(time,doy,gmt,yr,mo,da,hr,mn)

    implicit none
    integer yr,mo,da,hr,mn,ss,yrdays,doy,days(13)
    real*8 time,tmp
    real gmt
    data days /31,28,31,30,31,30,31,31,30,31,30,31,30/
    
    yr  = dint(time)
    tmp =     (time) 
    
    if((mod(yr,4).eq.0.and.mod(yr,100).ne.0) &     !correct for leap year
         .or.(mod(yr,400).eq.0))then             !correct for y2k
       yrdays=366                  
    else
       yrdays=365
    endif
    if (yrdays.eq.366) then
       days(2)=29
    else
       days(2)=28
    endif
    
    doy  = dint((tmp-yr)*dfloat(yrdays))+1 
    tmp =      ((tmp-yr)*dfloat(yrdays))+1 
    hr  = nint((tmp-doy)*24.d0) 
    tmp =     ((tmp-doy)*24.d0) 
    
    mn  = dint((tmp-hr)*60.d0) 
    tmp =     ((tmp-hr)*60.d0) 
    
    ss  = dint((tmp-mn)*60.d0) 
    mo=1
    do while (doy.gt.0)
       doy=doy-days(mo)
       mo=mo+1
    enddo
    mo=mo-1
    da=doy+days(mo)
    
    gmt=(((float(ss)/60.0)+float(mn))/60.0)+float(hr)
    
    if(gmt.eq.24) then
       gmt=0
       da=da+1
       if (da.gt.days(mo)) then
          da=1
          mo=mo+1
          if (mo.gt.12) then
             mo=1
             yr=yr+1
          endif
       endif
    endif
    return
  end subroutine time2date
#if 0 
!=========================================================================================
!BOP
! 
! !ROUTINE: chkrc
! 
! !DESCRIPTION: 
! Checks the return code for errors
!
! !INTERFACE:
subroutine chkrc(rc, mes)
!EOP
   implicit none
   integer, intent(in)          :: rc   ! return code from time management library
   character(len=*), intent(in) :: mes  ! error message
!BOC
   if ( rc == esmf_success ) return
   write(6,*) mes
   call endrun
!EOC
end subroutine chkrc
#endif
end module time_manager
