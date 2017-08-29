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
! !ROUTINE: climatologylairead.F90
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
subroutine climatologylairead(name11, name12, lai_t1_f, lai_t2_f)
! !USES:
  use time_manager
  use lisdrv_module, only : grid,lis,tile
  use lis_indices_module, only : lis_g2l_row_offset, lis_g2l_col_offset
  use spmdMod, only : iam
  use precision
!EOP
  implicit none

!=== Arguments ===========================================================
  real,allocatable :: domlai(:,:)
  integer :: cindex, rindex
  integer :: index
  integer :: mlat,mlon,line
  real(r8) :: lai_t1_f(lis%d%nch)  
  real(r8) :: lai_t2_f(lis%d%nch)  
!=== Local variables
  integer            :: t
  character*1        :: lai_1(14),lai_2(14)          ! LAI/SAI for all vegetation types
  real               :: top_1(14),top_2(14),bot_1(14),bot_2(14)  ! TOP/BOT for all vegetation types
  real               :: lat1,lon1,lat2,lon2                      ! Lat/Lon to determine specific data for each tile in direct access files
  INTEGER            :: FLAG1, FLAG2
  INTEGER            :: CNT1,CNT2,CNT3,CNT4,CNT5,CNT6,CNT7,CNT8,II8,J8
  REAL               :: SUM1,SUM2,SUM3,SUM4,SUM5,SUM6,SUM7,SUM8
  INTEGER            :: D_START_NR,D_START_NC,START_8TH_NR,START_8TH_NC
  INTEGER            :: END_8TH_NR,END_8TH_NC,K,MM
  character(len=80) :: name11, name12
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
   if(domain==8) then 
      open(10,file=name11,status='old',form='unformatted',&
           access='direct',recl=1)
      print*, 'msg: lairead -- no realtime monthly data for month 1',&
           ' (',iam,')'
      print*, 'msg: lairead -- using 1/8 avhrr lai/dsai data for month 1 ', &
           name11,' (',iam,')'
      flag1 = 1
   else if(domain==7) then 
      print*, 'open 10 .',name11
      open(10,file=name11,status='old',form='unformatted',&
           access='direct',recl=22)
      print*, 'msg: lairead -- no realtime monthly data for month 1',&
           ' (',iam,')'
      print*, 'msg: lairead -- using 1/8 avhrr lai/dsai data for month 1 ', &
           name11,' (',iam,')'
      flag1 = 1
   else 
      print*, 'open 10 2 ',name11
      open(10,file=name11,status='old',form='unformatted',&
           access='direct',recl=24)
      print*, 'msg: lairead -- no realtime monthly data for month 1',&
           ' (',iam,')'
      print*, 'msg: lairead -- using 1/8 avhrr lai/dsai data for month 1 ', &
           name11,' (',iam,')'
      flag1 = 1
   endif
   
   if( domain ==8) then 
      open(11,file=name12,status='old',form='unformatted',&
           access='direct',recl=1)
      close(13)
      flag2 = 1
      print*, 'msg: lairead -- no realtime monthly data for month 2',&
           ' (',iam,')'
      print*, 'msg: lairead -- using 1/8 avhrr lai/dsai data for month 2 ', &
           name12,' (',iam,')'
   else if( domain ==7) then 
      open(11,file=name12,status='old',form='unformatted',&
           access='direct',recl=22)
      close(13)
      flag2 = 1
      print*, 'msg: lairead -- no realtime monthly data for month 2',&
           ' (',iam,')'
      print*, 'msg: lairead -- using 1/8 avhrr lai/dsai data for month 2 ', &
           name12,' (',iam,')'
   else 
      open(11,file=name12,status='old',form='unformatted',&
           access='direct',recl=24)
      flag2 = 1
      print*, 'msg: lairead -- no realtime monthly data for month 2', &
           ' (',iam,')'
      print*, 'msg: lairead -- using 1/8 avhrr lai/dsai data for month 2 ', &
           name12,' (',iam,')'
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
               read(10,rec=line) lat1, lon1, lai_1
               read(11,rec=line) lat2, lon2, lai_2

               select case (lis%p%lai)
                  
               case(2)     ! avhrr lai
                  
                  if (ichar(lai_1(tile(t)%vegt+1)) .ne. 251 &
                       .and. ichar(lai_1(tile(t)%vegt+1)) .ne. 0) then
                     sum1 = sum1 + (ichar(lai_1(tile(t)%vegt+1))) * 0.04
                     cnt1 = cnt1 + 1
                  endif
                  if (ichar(lai_2(tile(t)%vegt+1)) .ne. 251 &
                       .and. ichar(lai_2(tile(t)%vegt+1)) .ne. 0) then
                     sum2 = sum2 + (ichar(lai_2(tile(t)%vegt+1))) * 0.04
                     cnt2 = cnt2 + 1
                  endif                  
               case(3)     ! modis lai
                  if (ichar(lai_1(tile(t)%vegt+1)) .lt. 200) then
                     sum1 = sum1 + (ichar(lai_1(tile(t)%vegt+1))) * 0.10
                     cnt1 = cnt1 + 1
                  endif
                  if (ichar(lai_2(tile(t)%vegt+1)) .lt. 200) then
                     sum2 = sum2 + (ichar(lai_2(tile(t)%vegt+1))) * 0.10
                     cnt2 = cnt2 + 1
                  endif
               case default
                  print*, 'err:  lairead -- not a valid lai domain',' (',iam,')'
                  call endrun

               end select
            enddo
         enddo
!------------------------------------------------------------------------     
! Compute averages for the vegetation type represented by tile
!------------------------------------------------------------------------      
         if (cnt1 .ne. 0) then
            lai_t1_f(t) = sum1 / cnt1
         else
            lai_t1_f(t) = 0
         endif
         if (cnt2 .ne. 0) then
            lai_t2_f(t) = sum2 / cnt2
         else
            lai_t2_f(t) = 0       
         endif
      else if(domain.eq.7) then 
!     MLAT = (LATDEG - (-59.9375)) / 0.125 + 1
!     MLON = (LONDEG - (-179.9375)) / 0.125 + 1
!     LINE = (MLAT - 1)*2880 + MLON
         mlat = (latdeg +59.975) / 0.05 + 1
         mlon = (londeg +179.975) / 0.05 + 1
         line = (mlat - 1)*7200 + mlon
         read(10,rec=line) lat1, lon1, lai_1
         read(11,rec=line) lat2, lon2, lai_2
         select case(lis%p%lai)
         case(2)   ! avhrr lai
            lai_t1_f(t) = ichar(lai_1(tile(t)%vegt+1)) * 0.04
            lai_t2_f(t) = ichar(lai_2(tile(t)%vegt+1)) * 0.04
         case(3)
            lai_t1_f(t) = ichar(lai_1(tile(t)%vegt+1)) * 0.10
            lai_t2_f(t) = ichar(lai_2(tile(t)%vegt+1)) * 0.10
         case default
            print*, 'err: lairead -- invalid domain for lai data',' (',iam,')'
            call endrun
         end select
      else if(domain.eq.8) then 

         mlat = (latdeg +59.995) / 0.01 + 1
         mlon = (londeg +179.995) / 0.01 + 1
         line = (mlat - 1)*36000 + mlon
         read(10,rec=line) lai_1(1)
         read(11,rec=line) lai_2(1)
!         print*, 'line ..',lai_1(1),lai_2(1)
!         print*, 'line ..',latdeg, londeg
     !=== scale to real physical values
         select case(lis%p%lai)
         case(2)   ! avhrr lai
            lai_t1_f(t) = ichar(lai_1(1)) * 0.04
            lai_t2_f(t) = ichar(lai_2(1)) * 0.04
!            if(lai_t1_f(1).lt.10) & 
!            print*, t, lai_t1_f(1),lai_t2_f(t)
         case(3)
            lai_t1_f(t) = ichar(lai_1(1)) * 0.10
            lai_t2_f(t) = ichar(lai_2(1)) * 0.10
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

 end subroutine climatologylairead

