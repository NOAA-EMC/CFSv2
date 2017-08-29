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
! !ROUTINE: climatologysairead.F90
!
! !DESCRIPTION:
!  This program reads in AVHRR LAI data for CLM
!
! !REVISION HISTORY:
!  27 Nov 2001: Jon Gottschalck; Initial code
!  20 Feb 2002: Jon Gottschalck; Modified to use for 1/4 and 2x2.5 using 1/8 degree monthly data
!  01 Oct 2002: Jon Gottschalck; Modified to add MODIS LAI data
! 
! !INTERFACE: 
subroutine climatologysairead(name15,name16,sai_t1_f,sai_t2_f)
! !USES:
  use time_manager
  use lisdrv_module, only : grid,lis, tile
  use spmdMod,only :iam
  use precision
!EOP
  implicit none

!=== Arguments ===========================================================
  real :: tsai(lis%d%nch)
  real,allocatable :: domlai(:,:)
  integer :: cindex, rindex
  integer :: index
  real(r8) :: sai_t1_f(lis%d%nch)  
  real(r8) :: sai_t2_f(lis%d%nch)  
!=== Local variables
  integer            :: line,t,v,mlat,mlon,l
  real               :: lat1,lon1,lat2,lon2                      ! Lat/Lon to determine specific data for each tile in direct access files
  character*1        :: sai_1(14),sai_2(14)  ! LAI/SAI for all vegetation types
  integer            :: flag1, flag2
  integer            :: cnt1,cnt2,cnt3,cnt4,cnt5,cnt6,cnt7,cnt8,ii8,j8
  real               :: sum1,sum2,sum3,sum4,sum5,sum6,sum7,sum8
  integer            :: d_start_nr,d_start_nc,start_8th_nr,start_8th_nc
  integer            :: end_8th_nr,end_8th_nc,k,mm
  character(len=80) :: name15, name16
  character(len=80) :: ntop1, ntop2, nbot1, nbot2
  character(len=100) :: temp
  real    :: pthtop(17),pthbot(17)  ! Canopy top and bottom height for the 13 UMD vegetation types
  integer :: p, domain
  real*8  :: latdeg, londeg
 
!=== End Local variable list
  if(lis%d%gridDesc(9) .eq. 0.01) then 
     domain = 8 
  elseif(lis%d%gridDesc(9).eq.0.05) then 
     domain = 7
  elseif(lis%d%gridDesc(9) .eq. 0.125) then 
     domain = 6
  elseif(lis%d%gridDesc(9) .eq. 0.25) then 
     domain = 5
  elseif(lis%d%gridDesc(9) .eq. 0.50) then 
     domain = 4
  elseif(lis%d%gridDesc(9) .eq. 1.0) then      
     domain = 3
  elseif((lis%d%gridDesc(9) .eq. 2) .and.  &
       (lis%d%gridDesc(10) .eq. 2.5)) then 
     domain = 2
  endif
   flag1 = 0
   flag2 = 0
   print*, 'reading sai file ',name15
   if(domain==8) then
      open(12,file=name15,status='old',form='unformatted',&
           access='direct',recl=1)
      print*, 'msg: lairead -- using 1/8 avhrr sai data for month 1 ', &
           name15,' (',iam,')'
      flag1 = 1
   else if(domain==7) then 
      open(12,file=name15,status='old',form='unformatted',&
           access='direct',recl=22)
      print*, 'msg: lairead -- using 1/8 avhrr sai data for month 1 ', &
           name15,' (',iam,')'
      flag1 = 1
   else 
      open(12,file=name15,status='old',form='unformatted',&
           access='direct',recl=24)
      print*, 'msg: lairead -- using 1/8 avhrr sai data for month 1 ', &
           name15,' (',iam,')'
      flag1 = 1
   endif
   
   if( domain ==8) then 
      open(13,file=name16,status='old',form='unformatted',&
           access='direct',recl=1)
      flag2 = 1
      print*, 'msg: lairead -- using 1/8 avhrr lai/dsai data for month 2 ', &
           name16,' (',iam,')'
   else if( domain ==7) then 
      open(13,file=name16,status='old',form='unformatted',&
           access='direct',recl=22)
      flag2 = 1
      print*, 'msg: lairead -- using 1/8 avhrr lai/dsai data for month 2 ', &
           name16,' (',iam,')'
   else 
      open(13,file=name16,status='old',form='unformatted',&
           access='direct',recl=24)
      flag2 = 1
      print*, 'msg: lairead -- no realtime monthly data for month 2', &
           ' (',iam,')'
      print*, 'msg: lairead -- using 1/8 avhrr sai data for month 2 ', &
           name16,' (',iam,')'
   endif

   do t=1,lis%d%nch
      index = tile(t)%index
      
      latdeg = grid(index)%lat
      londeg = grid(index)%lon
      if (domain .ne. 1 .and. domain .le. 5) then
  
         select case (domain)
         case (2)
            d_start_nr  = ((latdeg - (-60)) / 2.0) + 1
            start_8th_nr = ((d_start_nr - 1) * 16) + 1
            end_8th_nr   =   start_8th_nr + 15
            d_start_nc  = ((londeg - (-180)) / 2.5) + 1
            start_8th_nc = ((d_start_nc - 1) * 20) + 1
            end_8th_nc   =   start_8th_nc + 19
         case (3)
            d_start_nr  = ((latdeg - (-59.500)) / 1.00) + 1
            start_8th_nr = ((d_start_nr - 1) * 8) + 1
            end_8th_nr   =   start_8th_nr + 7
            d_start_nc  = ((londeg - (-179.500)) / 1.00) + 1
            start_8th_nc = ((d_start_nc - 1) * 8) + 1
            end_8th_nc   =   start_8th_nc + 7
         case (4)
            d_start_nr  = ((latdeg - (-59.750)) / 0.50) + 1
            start_8th_nr = ((d_start_nr - 1) * 4) + 1
            end_8th_nr   =   start_8th_nr + 3
            d_start_nc  = ((londeg - (-179.750)) / 0.50) + 1
            start_8th_nc = ((d_start_nc - 1) * 4) + 1
            end_8th_nc   =   start_8th_nc + 3
         case (5)
            d_start_nr  = ((latdeg - (-59.875)) / 0.25) + 1
            start_8th_nr = ((d_start_nr - 1) * 2) + 1
            end_8th_nr   =   start_8th_nr + 1
            d_start_nc  = ((londeg - (-179.875)) / 0.25) + 1
            start_8th_nc = ((d_start_nc - 1) * 2) + 1
            end_8th_nc   =   start_8th_nc + 1
         case default
            print*, 'err: lairead -- improper domain selection',' (',iam,')'
            call endrun
         end select
!------------------------------------------------------------------------   
! Initilaize sums for LAI month 1, LAI month 2, DSAI month 1, DSAI month 2
!------------------------------------------------------------------------   
         sum1 = 0.0
         sum2 = 0.0
         sum3 = 0.0
         sum4 = 0.0
         cnt1 = 0
         cnt2 = 0
         cnt3 = 0
         cnt4 = 0
!------------------------------------------------------------------------   
! Looping over 1/8 grid space that relates to 1/4 or 2x2.5 domains
!------------------------------------------------------------------------      
         do ii8 = start_8th_nr,end_8th_nr
            do j8 = start_8th_nc,end_8th_nc
               line = (ii8 - 1)*2880 + j8
               read(12,rec=line) lat1, lon1, sai_1
               read(13,rec=line) lat2, lon2, sai_2

               select case (lis%p%lai)
                  
               case(2)     ! avhrr lai
                  if (ichar(sai_1(tile(t)%vegt+1)) .ne. 251 &
                       .and. ichar(sai_1(tile(t)%vegt+1)) .ne. 0) then
                     sum3 = sum3 + (ichar(sai_1(tile(t)%vegt+1))) * 0.04
                     cnt3 = cnt3 + 1
                  endif
                  if (ichar(sai_2(tile(t)%vegt+1)) .ne. 251 &
                       .and. ichar(sai_2(tile(t)%vegt+1)) .ne. 0) then
                     sum4 = sum4 + (ichar(sai_2(tile(t)%vegt+1))) * 0.04
                     cnt4 = cnt4 + 1
                  endif
                  
               case(3)     ! modis lai
                  if (ichar(sai_1(tile(t)%vegt+1)) .lt. 200) then
                     sum3 = sum3 + (ichar(sai_1(tile(t)%vegt+1))) * 0.10
                     cnt3 = cnt3 + 1
                  endif
                  if (ichar(sai_2(tile(t)%vegt+1)) .lt. 200) then
                     sum4 = sum4 + (ichar(sai_2(tile(t)%vegt+1))) * 0.10
                     cnt4 = cnt4 + 1
                  endif
               case default
                  print*, 'err:  lairead -- not a valid sai domain',' (',iam,')'
                  call endrun

               end select
            enddo
         enddo
!------------------------------------------------------------------------     
! Compute averages for the vegetation type represented by tile
!------------------------------------------------------------------------      
         if (cnt3 .ne. 0) then
            sai_t1_f(t) = sum3 / cnt3
         else
            sai_t1_f(t) = 0
         endif
         if (cnt4 .ne. 0) then
            sai_t2_f(t) = sum4 / cnt4
         else
            sai_t2_f(t) = 0
         endif
      elseif(domain.eq.7) then 
         mlat = (latdeg + 59.975) / 0.05 + 1
         mlon = (londeg + 179.975) / 0.05 + 1
         line = (mlat - 1)*7200 + mlon
         read(12,rec=line) lat1, lon1, sai_1
         read(13,rec=line) lat2, lon2, sai_2
         select case(lis%p%lai)
         case(2)   ! avhrr lai
            sai_t1_f(t) = ichar(sai_1(tile(t)%vegt+1)) * 0.04
            sai_t2_f(t) = ichar(sai_2(tile(t)%vegt+1)) * 0.04
         case(3)
            sai_t1_f(t) = ichar(sai_1(tile(t)%vegt+1)) * 0.10
            sai_t2_f(t) = ichar(sai_2(tile(t)%vegt+1)) * 0.10
         case default
            print*, 'err: lairead -- invalid domain for lai data',' (',iam,')'
            call endrun
         end select
      elseif(domain.eq.8) then 
         mlat = (latdeg + 59.995) / 0.01 + 1
         mlon = (londeg + 179.995) / 0.01 + 1
         line = (mlat - 1)*36000 + mlon
!=== read in data for both lai and dsai
!=== only 1 veg for each tile @ 1km
         read(12,rec=line) sai_1(1)
         read(13,rec=line) sai_2(1)
     !=== scale to real physical values
         select case(lis%p%lai)
         case(2)   ! avhrr lai
            sai_t1_f(t) = ichar(sai_1(1)) * 0.04
            sai_t2_f(t) = ichar(sai_2(1)) * 0.04
         case(3)
            sai_t1_f(t) = ichar(sai_1(1)) * 0.10
            sai_t2_f(t) = ichar(sai_2(1)) * 0.10
         case default
            print*, 'err: lairead -- invalid domain for lai data',' (',iam,')'
            stop
         end select
      endif
   enddo
   close(10)
   close(11)
   close(12)
   close(13)

 end subroutine climatologysairead

