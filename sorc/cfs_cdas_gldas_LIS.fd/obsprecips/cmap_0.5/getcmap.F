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
! !ROUTINE: getcmap.F90
!
! !DESCRIPTION:
!  Opens and reads global precipitation forcing
!
!    CTIME  = Current time\\
!    FTIMENRL  = Nearest future data for NRL data\\
!    FTIMEHUFF = Nearest future data for HUFFMAN data\\
!    FTIMEPERS = Nearest future data for PERSIANN data\\
!
! !REVISION HISTORY:
! 17 Jul 2001: Jon Gottschalck; Initial code
! 10 Oct 2001: Jon Gottschalck; Modified to adjust convective precip
!               using a ratio of the model convective / total ratio
! 30 Jul 2002: Jon Gottschalck; Added PERSIANN and HUFFMAN global observed precip data sources
!
! !INTERFACE:
subroutine getcmap()
! !USES:
  use lisdrv_module, only : lis, gindex
  use time_manager
  use cmapdomain_module, only : cmapdrv, conserv_cmap_interp_input
  use obsprecipforcing_module, only: obsprecip !JESSE 20041127
  use gdasdomain_module, only : gdasdrv

  implicit none
!EOP
   
!==== Local Variables=======================
  integer :: c, r, flag1, flag2                                     ! Program flow flags used in HUFFMAN precip section
  integer :: ferror_cmap,ferror_huff, ferror_pers      ! Error flags for precip data sources
  integer :: doy1, yr1, mo1, da1, hr1, mn1, ss1, ts1                ! Time parameters for current LDAS time
  integer :: doy5, yr5, mo5, da5, hr5, mn5, ss5, ts5                ! Time parameters for CMAP boundary end time
  integer :: endtime_cmap   ! 1=get a new file 
  real*8  :: ctime,ftime_cmap  ! Current LDAS time and end boundary times for precip data sources 
  real*8  :: datatime, gap, breaktime, fnametime                    ! Times used in HUFFMAN to determine data and filename boundaries (see below)
  real    :: gmt1, gmt2, gmt3, gmt4, kgmt3, mgmt3, gmt5             ! GMT times for current LDAS time and end boundary times for precip data sources
  character(len=80) :: name(2)                                      ! Filename variables for precip data sources, 1=cmap, 2=gdas
  integer :: index
  real :: gridDesci(50)
!=== End Variable Definition =======================
!BOC
 PRINT*,"J---GETCMAP"
  endtime_cmap = 0
!------------------------------------------------------------------------
! Set parameter to measure 1.5 hour time offset when using HUFFMAN
!------------------------------------------------------------------------
  gap = 0.0001712328767098370
!------------------------------------------------------------------------
! Determine required observed precip data times 
! (current, accumulation end time)
! Model current time
!------------------------------------------------------------------------
  yr1 = lis%t%yr  !current time
  mo1 = lis%t%mo
  da1 = lis%t%da
  hr1 = lis%t%hr
  mn1 = lis%t%mn
  ss1 = 0
  ts1 = 0
  call tick( ctime, doy1, gmt1, yr1, mo1, da1, hr1, mn1, ss1, ts1 )   
  print*, "current time     =", ctime
!------------------------------------------------------------------------ 
! CMAP product end time
!------------------------------------------------------------------------
  yr5 = lis%t%yr  !cmap accumulation time data
  mo5 = lis%t%mo
  da5 = lis%t%da
  hr5 = 6*(lis%t%hr/6)
  mn5 = 0
  ss5 = 0
  ts5 = 0
  call tick( ftime_cmap, doy5, gmt5, yr5, mo5, da5, hr5, mn5, ss5, ts5 )
!------------------------------------------------------------------------
! Ensure that data is found during first time step
!------------------------------------------------------------------------
  if ( lis%f%gpcpsrc.eq.4.and. get_nstep(lis%t).eq. 1 &
  .or.lis%f%rstflag .eq. 1) then
     endtime_cmap = 1
     cmapdrv%cmaptime = ftime_cmap
     lis%f%rstflag = 0
  endif

  print*, "cmapdrv%cmaptime =", cmapdrv%cmaptime

     cmapdrv%ncold = 720
     cmapdrv%nrold = 360 
     cmapdrv%gridchange1 = .false.
!------------------------------------------------------------------------
! Ensure that data is found during first time step
!------------------------------------------------------------------------
!  if ( lis%f%gpcpsrc.eq.4.and. get_nstep(lis%t).eq. 1 & 
!  .or.lis%f%rstflag .eq. 1) then 
!     endtime_cmap = 1
!     lis%f%rstflag = 0
!  endif
!------------------------------------------------------------------------
! Check for and get CMAP CPC Precipitation data
!------------------------------------------------------------------------

  if (lis%f%gpcpsrc==4) then
     if ( ctime > cmapdrv%cmaptime ) endtime_cmap = 1
!     if ( ctime >= cmapdrv%cmaptime ) endtime_cmap = 1

  print*, "endtime_cmap =", endtime_cmap
  print*, yr5, mo5, da5, hr5

     if ( endtime_cmap == 1 ) then  !get new time2 data
        ferror_cmap = 0
        write(name(1),"('cmap.gdas.',i4,i2.2,i2.2,i2.2)") yr5, mo5, da5, hr5
       !call cmapfile( name(1), cmapdrv%cmapdir, yr5, mo5, da5, hr5 )
        print*, 'getcmap() Getting new CMAP CPC data ',name(1)
        call gdasfile6hrly( name(2),gdasdrv%gdasdir, yr5, mo5, da5, hr5 )
        print*, 'getcmap() Getting new GDAS 6hr data ',name(2)
        call glbprecip_cmap( name, ferror_cmap, hr5 )
        hr5 = hr5 + 6
        call tick( ftime_cmap, doy5, gmt5, yr5, mo5, da5, hr5, mn5, ss5, ts5 )
        cmapdrv%cmaptime = ftime_cmap
     endif  !need new cmap
  endif
  print*,"DONE getcmap()"
  return
!EOC
end subroutine getcmap

!BOP
! !ROUTINE: cmapfile
!
! !DESCRIPTION: This subroutine puts together CMAP file name for
!               6 hour file intervals
!
! !INTERFACE:
subroutine cmapfile( name, cmapdir, yr, mo, da, hr)
!EOP
  implicit none

!==== Local Variables=======================

  character(len=80) :: name, cmapdir
  integer :: yr, mo, da, hr
  integer :: i, c
  integer :: uyr, umo, uda, uhr, umn, uss, ts1
  integer :: doy
  real    :: gmt
  character*1 :: fbase(80), fdir(8), ftime(10), fsubs(10), fsubs2(4)

!=== End Variable Definition ===============

!=== formats for filename segments
!BOC
91 format (a4)
92 format (80a1)
93 format (a80)
94 format (i4, i2, i2, i2)
95 format (10a1)
96 format (a40)
97 format (a11)
98 format (a1, i4, a1)
99 format (8a1)
!------------------------------------------------------------------------
! Make variables for the time used to create the file
! We don't want these variables being passed out
!------------------------------------------------------------------------
  uyr = yr
  umo = mo
  uda = da
  uhr = 6*(hr/6)  !hour needs to be a multiple of 6 hours
  umn = 0
  uss = 0
  ts1 = -6*60*60 !roll back time.

  print*,yr, mo, da, hr 
  open(unit=90, file='temp', form='formatted', access='direct', recl=80)
  write(90, 96, rec=1) cmapdir
  read(90, 92, rec=1) (fbase(i), i=1,80)

  write(90, 98, rec=1) '/', uyr, '/'
  read(90, 99, rec=1) fdir
  do i = 1, 6
     if ( fdir(i) == ' ' ) fdir(i) = '0'
  end do

  write(90, 97, rec=1) '_drean_smth'
  read (90, 92, rec=1) (fsubs(i), i=1,11)

  write(90, 94, rec=1) uyr, umo, uda, uhr
  read(90, 95, rec=1) ftime
  do i = 1, 10
     if ( ftime(i) == ' ' ) ftime(i) = '0'
  end do

  write(90, 91, rec=1) '.grb'
  read (90, 92, rec=1) (fsubs2(i), i=1,4)
  c = 0
  do i = 1, 80
     if ( (fbase(i) == ' ') .and. (c == 0) ) c = i-1
  end do

!  write(90, 92, rec=1) (fbase(i), i=1,c), (fdir(i), i=1,6),  &
!                       (ftime(i), i=1,10),(fsubs(i), i=1,11)
   write(90, 92, rec=1) (fbase(i), i=1,c), '/', &
                        (ftime(i), i=1,10),(fsubs(i), i=1,11)
 
  read(90, 93, rec=1) name

  close(90)
  return
!EOC
end subroutine cmapfile


subroutine gdasfile6hrly( name, gdasdir, yr, mo, da, hr )

  use time_manager
  implicit none

! !INPUT PARAMETERS:
  character(len=80) :: gdasdir
  integer :: yr, mo, da, hr
! !OUTPUT PARAMETERS:
  character(len=80) :: name
!EOP
  integer :: i, c
  integer :: uyr, umo, uda, uhr, umn, uss, ts1
  integer :: ghh, gff
  integer :: remainder
  integer :: doy
  real    :: gmt
  real*8  :: dumbtime
  character(len=2) :: initcode, fcstcode
  character*1 :: fbase(80), fdir(15), ftime(10), fsubs(22)
  character(LEN=100) :: temp
!=== End Variable Definition ===============

!=== formats for filename segments
!BOC
92 format (80a1)
93 format (a80)
94 format (i4, i2, i2, a2)
95 format (10a1)
96 format (a40)
97 format (a16, a2, a3)
98 format (a1, i4, i2, a1)
99 format (8a1)
!-----------------------------------------------------------------
!  Make variables for the time used to create the file
!  We don't want these variables being passed out
!-----------------------------------------------------------------
  dumbtime = 0.
  doy = 1
  gmt = 0.
  uyr = yr
  umo = mo
  uda = da
  uhr = hr
  umn = 0
  uss = 0
  ts1 = -24. * 60. * 60.

  ghh = uhr
  gff = 6

  write(initcode,'(i2.2)') ghh
  write(fcstcode,'(i2.2)') gff

  write(UNIT=temp, fmt='(a40)') gdasdir
  read(UNIT=temp, fmt='(80a1)') (fbase(i), i=1,80)
  c = 0
  do i = 1, 80
     if ( fbase(i) .NE. ' ' ) c = c + 1
  end do

  write(UNIT=temp, fmt='(a6, i4, i2, i2, a1)') '/gdas.',uyr,umo,uda,'/'
  read(UNIT=temp, fmt='(15a1)') fdir
  do i = 1, 15
     if ( fdir(i) == ' ' ) fdir(i) = '0'
  end do

  write(UNIT=temp, fmt='(a7,a2,a11,a2)') &
        'gdas1.t',initcode,'z.sfluxgrbf',fcstcode

  read(UNIT=temp, fmt='(22a1)') fsubs

  write(UNIT=temp, fmt='(80a1)') &
        (fbase(i),i=1,c), (fdir(i),i=1,15), (fsubs(i),i=1,22)

  read(UNIT=temp, fmt='(a80)') name

  return
!EOC
end subroutine gdasfile6hrly


