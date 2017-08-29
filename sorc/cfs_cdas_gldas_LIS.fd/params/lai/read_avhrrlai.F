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
! !ROUTINE: read_avhrrlai
!
! !DESCRIPTION:
!  This program reads in AVHRR LAI data
!
! !REVISION HISTORY:
!  27 Nov 2001: Jon Gottschalck; Initial code
!  20 Feb 2002: Jon Gottschalck; Modified to use for 1/4 and 2x2.5 using 1/8 degree monthly data
!  10 Sept 2004: Sujay Kumar, Initial Specification
! 
! !INTERFACE: 
subroutine read_avhrrlai(lai1, lai2, wt1, wt2)
! !USES:
  use time_manager
  use lisdrv_module, only : grid,lis
  use filename_mod
  use precision
!EOP
  implicit none

!=== Arguments ===========================================================
  integer :: domain
  integer :: cindex, rindex
  logical :: laifile1,laifile2
  real*8             :: time1,time2                              ! Temporary Time variables
  integer            :: yr1,mo1,yr2,mo2                          ! Temporary Time variables
  integer            :: doy1,doy2                                ! Temporary Time variables
  real               :: gmt1,gmt2                        ! Interpolation weights
  integer            :: zeroi,numi                               ! Integer Number Holders
  character (len=4)  :: cyr1,cyr2                                ! Filename variables
  character (len=2)  :: cmo1,cmo2                                ! Filename variables
  character (len=80) :: name, avhrrdir
  character(len=80)  :: name9,  name10, name11, name12
  character(len=80)  :: name13, name14, name15, name16
  character(len=80)  :: ntop1, ntop2, nbot1, nbot2
  character(len=100) :: temp
  real(r8) :: lai1(lis%d%nch), lai2(lis%d%nch)
  real :: wt1, wt2
!=== End Local variable list
!BOC
!------------------------------------------------------------------------
! Determine current time to find correct LAI files
!------------------------------------------------------------------------
  if (lis%t%tscount .eq. 0) then
   lis%t%yr = lis%t%syr
   lis%t%mo = lis%t%smo
   lis%t%da = lis%t%sda
   lis%t%mn = lis%t%smn
   lis%t%ss = lis%t%sss
  else
   lis%t%yr = lis%t%yr
   lis%t%mo = lis%t%mo
   lis%t%da = lis%t%da
   lis%t%mn = lis%t%mn
   lis%t%ss = lis%t%ss
  endif
  
  call date2time(lis%t%time,lis%t%doy,lis%t%gmt,lis%t%yr, &
       lis%t%mo,lis%t%da,lis%t%hr,lis%t%mn,lis%t%ss)
!------------------------------------------------------------------------
! Initialize LAI flag varaible
!------------------------------------------------------------------------
   lis%p%laiflag = 0
   
   zeroi=0
   numi=16
!------------------------------------------------------------------------   
! Determine Monthly data Times (Assume Monthly 
! value valid at DA=16 HR=00Z)
!------------------------------------------------------------------------
   if (lis%t%da .lt. 16) then
      mo1 = lis%t%mo-1
      yr1 = lis%t%yr
      if (mo1 .eq. 0) then
         mo1 = 12
         yr1 = lis%t%yr - 1
      endif
      mo2 = lis%t%mo
      yr2 = lis%t%yr
   else
      mo1 = lis%t%mo
      yr1 = lis%t%yr
      mo2 = lis%t%mo+1
      yr2 = lis%t%yr
      if (mo2 .eq. 13) then
         mo2 = 1
         yr2 = lis%t%yr + 1
      endif
   endif
   
   call date2time(time1,doy1,gmt1,yr1,mo1,numi,zeroi,zeroi,zeroi)
   call date2time(time2,doy2,gmt2,yr2,mo2,numi,zeroi,zeroi,zeroi) 
!------------------------------------------------------------------------   
! Check to see if need new LAI data
!------------------------------------------------------------------------
   if (time2 .gt. lis%p%laitime) then 
      lis%p%laiflag = 1
   else
      lis%p%laiflag = 0
   endif
   
   avhrrdir = lis%p%avhrrdir
!------------------------------------------------------------------------
! Determine weights between months
!------------------------------------------------------------------------
   wt1 = (time2-lis%t%time)/(time2-time1)
   wt2 = (lis%t%time-time1)/(time2-time1)

!------------------------------------------------------------------------   
! Get new LAI data if required
!------------------------------------------------------------------------
  if (lis%p%laiflag .eq. 1) then
     print*, 'in avhrr lai read.'
     write(unit=temp,fmt='(i4,i2.2)') yr1, mo1
     read (unit=temp,fmt='(a4,a2)') cyr1, cmo1
     write(unit=temp,fmt='(i4,i2.2)') yr2,  mo2  
     read (unit=temp,fmt='(a4,a2)') cyr2, cmo2
 
   lis%p%laitime = time2
   if(lis%d%gridDesc(9).eq.0.01) then
      domain = 8 
   else if(lis%d%gridDesc(9).eq.0.05) then
      domain = 7
   endif
   if(domain ==8)  then
      call avhrr_laifile_1km(name9,name10,name11,name12,& 
           lis%p%avhrrdir,cyr1,cyr2,cmo1,cmo2)
   else if(domain ==7)  then
      call avhrr_laifile_5km( & 
           name9,name10,name11,name12,& 
           lis%p%avhrrdir,cyr1,cyr2,cmo1,cmo2)
   else 
      call avhrr_laifilename(name9,name10,name11,name12, &
           lis%p%avhrrdir,cyr1,cyr2,cmo1,cmo2)
   endif
!------------------------------------------------------------------------   
! Open AVHRR LAI files (assumes realtime monthly files are present first
! then uses climatology files)
! Assume realtime monthly files are present as default
!------------------------------------------------------------------------  
   print*, 'name9  ',name9
   print*, 'name10 ',name10
   inquire(file=name9,exist=laifile1)
   inquire(file=name10,exist=laifile2)
   call climatologylairead(name11, name12, lai1, lai2)
end if
end subroutine read_avhrrlai
