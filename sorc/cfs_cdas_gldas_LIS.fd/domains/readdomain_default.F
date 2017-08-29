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
! !ROUTINE: readdomain_default
! 
! !DESCRIPTION: 
!
!  Reads in LIS run specifics from lis.crd
!
! !REVISION HISTORY:
! 
! REVISION HISTORY:
!  15 Oct 1999: Paul Houser; Initial code
!  4  Apr 2000: Jeffrey Walker; Added catchment model output interval
!  11 Apr 2000: Brian Cosgrove; Added Elevation correction and Forcing
!               Mask read statements
!  6  Jun 2000: Jon Radakovich; Updated for new version of CLM
!  23 Feb 2001: Urszula Jambor; Added GEOS or GDAS forcing option
!  27 Mar 2001: Jon Gottschalck; Revision of subroutine by implementing namelists
!  05 Sep 2001: Brian Cosgrove; Altered forcing logfile output to include
!               more precip types
!  04 Feb 2002: Jon Gottschalck; Added section to set to Koster tilespace files if necessary
!  15 Apr 2002: Urszula Jambor; Added ECMWF forcing options, also
!               adding 1 & 1/2 degree GLDAS domain options.
!  28 Apr 2002: Kristi Arsenault; Added NOAH LSM code
!  14 Nov 2003: Sujay Kumar; Modified card file that includes regional 
!               modeling options
! !INTERFACE:
subroutine readdomain_default
! !USES:
  use lisdrv_module, only : lis
  implicit none
!EOP

!=== Local Variables =====================================================
  INTEGER :: I                       ! Loop Counter
  integer :: tl,k
  real :: run_dd(7)
  real :: param_dd(6)
  NAMELIST /run_domain/run_dd
  NAMELIST /param_domain/param_dd
  character :: ch=' '
!=== End Variable Definition =============================================
!BOC
  open(10,file='lis.crd',form='formatted',status='old')

  read(unit=10,NML=run_domain)
  read(unit=10,NML=param_domain)
  print*,'DOMAIN details:'
!------------------------------------------------------------------------
! Read namelist of parameters depending on the domain
!------------------------------------------------------------------------
  lis%d%gridDesc(1) = run_dd(1)
  lis%d%gridDesc(4) = run_dd(2)
  lis%d%gridDesc(5) = run_dd(3)
  lis%d%gridDesc(7) = run_dd(4)
  lis%d%gridDesc(8) = run_dd(5)
  lis%d%gridDesc(9) = run_dd(6)
  lis%d%gridDesc(10) = run_dd(7)
 
  lis%d%gridDesc(41) = lis%d%gridDesc(1)
  lis%d%gridDesc(44) = param_dd(1)
  lis%d%gridDesc(45) = param_dd(2)
  lis%d%gridDesc(47) = param_dd(3)
  lis%d%gridDesc(48) = param_dd(4)
  lis%d%gridDesc(49) = param_dd(5)
  lis%d%gridDesc(50) = param_dd(6)
!  if(lis%d%gridDesc(1).eq.0) then 
!     lis%d%gridDesc(10) = run_dd(7)
!     lis%d%gridDesc(50) = param_dd(5)
!  elseif(lis%d%gridDesc(1) .eq. 4 ) then
!     lis%d%gridDesc(10) = run_dd(7)
!     lis%d%gridDesc(50) = param_dd(6)
!  endif
!  lis%d%gridDesc(48) = param_dd(4)
!  lis%d%gridDesc(49) = param_dd(6)
  if(lis%d%gridDesc(1).eq.0) then 
     lis%d%gridDesc(43) = nint((lis%d%gridDesc(47)-lis%d%gridDesc(44))/lis%d%gridDesc(49)) + 1
     lis%d%gridDesc(42) = nint((lis%d%gridDesc(48)-lis%d%gridDesc(45))/lis%d%gridDesc(50)) + 1
  elseif(lis%d%gridDesc(1).eq.4) then 
     lis%d%gridDesc(43) = 2*lis%d%gridDesc(50)
     lis%d%gridDesc(42) = nint(360/param_dd(5))
  endif


  if(lis%d%gridDesc(1).eq.0) then 
     lis%d%gridDesc(6) = 128
     lis%d%gridDesc(11) = 64
     lis%d%gridDesc(20) = 255
     
  if(lis%d%gridDesc(7).le.lis%d%gridDesc(4)) then
     print*, 'lat2 must be greater than lat1'
     print*, 'Stopping run...'
     call endrun
  endif
  if(lis%d%gridDesc(8).le.lis%d%gridDesc(5)) then
     print*, 'lon2 must be greater than lon1'
     print*, 'Stopping run...'
     call endrun
  endif
#if 0 
  if(mod(lis%d%gridDesc(4)-lis%d%gridDesc(44),lis%d%gridDesc(9)).ne.0) then
     tl = (nint((lis%d%gridDesc(4)-lis%d%gridDesc(44))/lis%d%gridDesc(9)))
     lis%d%gridDesc(4) = lis%d%gridDesc(44)+tl*lis%d%gridDesc(9) 
     print*, 'modified gridDesc(4) ',lis%d%gridDesc(4)
  endif
  if(mod(lis%d%gridDesc(5)-lis%d%gridDesc(45),lis%d%gridDesc(10)).ne.0) then
     tl = (nint((lis%d%gridDesc(5)-lis%d%gridDesc(45))/lis%d%gridDesc(10)))
     lis%d%gridDesc(5) = lis%d%gridDesc(45)+tl*lis%d%gridDesc(10) 
     print*, 'modified gridDesc(5) ',lis%d%gridDesc(5)
  endif
  
  if(mod(lis%d%gridDesc(47)-lis%d%gridDesc(7),lis%d%gridDesc(9)).ne.0) then
     tl = (int((lis%d%gridDesc(47)-lis%d%gridDesc(7))/lis%d%gridDesc(9)))
     lis%d%gridDesc(7) = lis%d%gridDesc(47)-tl*lis%d%gridDesc(9) 
     print*, 'modified gridDesc(7) ',lis%d%gridDesc(7)
  endif
  if(mod(lis%d%gridDesc(48)-lis%d%gridDesc(8),lis%d%gridDesc(10)).ne.0) then
     tl = (int((lis%d%gridDesc(48)-lis%d%gridDesc(8))/lis%d%gridDesc(10)))
     lis%d%gridDesc(8) = lis%d%gridDesc(48)-tl*lis%d%gridDesc(10) 
     print*, 'modified gridDesc(8) ',lis%d%gridDesc(8)
  endif
#endif
  lis%d%gridDesc(2) = nint((lis%d%gridDesc(8)-lis%d%gridDesc(5))/lis%d%gridDesc(10))+ 1
  lis%d%gridDesc(3) = nint((lis%d%gridDesc(7)-lis%d%gridDesc(4))/lis%d%gridDesc(9)) + 1
  elseif(lis%d%gridDesc(1).eq.4) then 
      lis%d%gridDesc(2) = nint(360/run_dd(6))
      lis%d%gridDesc(3) = 2*lis%d%gridDesc(10)
      lis%d%gridDesc(6) = 128
      lis%d%gridDesc(11) = 64
      lis%d%gridDesc(20) = 255     
      lis%d%gridDesc(46) = 128
  endif
  do k=1,13
     print*, '(',k,',',lis%d%gridDesc(k),')'
  enddo
  do k=41,50
     print*, '(',k,',',lis%d%gridDesc(k),')'
  enddo

  lis%d%lnc = lis%d%gridDesc(2)
  lis%d%lnr = lis%d%gridDesc(3)
  print*,'running domain','(',lis%d%lnc,lis%d%lnr,')'
  print*,'parameter domain','(',lis%d%gridDesc(42),lis%d%gridDesc(43),')'
  close(10)
  return
!EOC  
end subroutine readdomain_default
      
