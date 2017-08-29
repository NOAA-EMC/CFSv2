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
! !ROUTINE: stats.F90
!
! !DESCRIPTION:
!  Calculates statistics for a given variable
!
! !REVISION HISTORY:
! Nov 11 1999:  Jon Radakovich; Initial code
! 
! !INTERFACE:
subroutine stats(var,udef,nch,mean,stdev,min,max)
! !ARGUMENTS: 
  integer, intent(in) :: nch
  real, intent(in)    :: var(nch), udef
  real, intent(out)   :: mean,stdev,min,max
!EOP
  integer :: t, count
  real :: dev, vsum
!=== End Variable List ===================================================
!BOC
  vsum=0.
  mean=0.
  dev=0.
  stdev=0.
  min=100000.
  max=-100000.
  count = 0 
  do t=1,nch
     if(var(t).ne.udef)then
        count = count +1
        vsum=vsum+var(t)
        if(var(t).gt.max)max=var(t)
        if(var(t).lt.min)min=var(t)
     endif
  enddo
  if(vsum.eq.0.)then
     max=0.
     min=0.
  endif
  if(count .ge.1) then 
     mean=vsum/float(count)
  else
     mean = 0 
  endif
  count = 0 
  do t=1,nch
     if(var(t).ne.udef)then
        count = count + 1
        dev=dev+(var(t)-mean)**2
     endif
  enddo
  if(count .gt.1) then 
     stdev=(dev*(float(count)-1)**(-1))**(0.5)
  else
     stdev = 0
  endif
  return
!EOC
end subroutine stats

