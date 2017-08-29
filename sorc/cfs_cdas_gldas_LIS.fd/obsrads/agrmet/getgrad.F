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
! !ROUTINE: getgrad.F90
!
! !DESCRIPTION:
!  Opens, reads, interpolates and overlays radiation forcing.  
!
!    TIME1 = most recent past data
!    TIME2 = most recent future data 
!
!
! !REVISION HISTORY:
!  28  Oct 1999: Brian Cosgrove; Initial code, see getrad.f
!  27  Apr 2000: Brian Cosgrove' Disabled zenith angle correction cutoff for 
!                cos(zen) less than .2
!  11  May 2000: Brian Cosgrove; Enabled correction cutoffs for cos(zen) less
!                than .1, stop model if computed value is greater than 1367 w/m2
!  08  Jan 2001: Brian Cosgrove; Added check to see if czb or czm is equal
!                to zero before trying to divide by czm or czb.  If it is
!                zero, set radiation value to zero
!  06  Mar 2001: Brian Cosgrove; Changed computation of WT1 and WT2 in cases
!                where the previous hour or the next hour of observed radiation
!                is not available.  Substituted TIME1 for LDAS%PINKTIME1 and
!                LDAS%NESTIME1 and TIME2 for LDAS%PINKTIME2 and LDAS%NESTIME2
!  15  Jun 2001: Urszula Jambor; Reworked algorithm for AGRMET data & GLDAS.
!  15  Oct 2001: Jesse Meng; Replace lis%agrmet flag by lis%agrmetsw and 
!                lis%agrmetlw;  added call retagrlw() to calculate 
!                AGRMET LW;
!  25  Feb 2002: Urszula Jambor; check on both SW & LW file status before 
!                using 
!  04  Jun 2002: Urszula Jambor; allowed fall back to model SW.
!  10  Dec 2002: Urszula Jambor; replaced lis%astat1,2 with local sstat1,2
!                Reorganized routine to mirror other get-routines,and 
!                corrected bug in file status check for initial time1. 
! !INTERFACE:
subroutine getgrad 
! !USES:
  use lisdrv_module, only : lis, grid
  use obsradforcing_module, only : obswdata1,obswdata2,oblwdata1,oblwdata2,&
       sstat1,sstat2,lstat1,lstat2
  use time_manager
  use agrmetdomain_module, only : agrmetdrv
#if ( defined OPENDAP )
  use agrmetopendap_module, only : set_agrmet_index
  use spmdMod
#endif
  implicit none
!EOP

!=== Local Variables =====================================================
  integer :: c, r, f, zdoy
  integer :: yr1, mo1, da1, hr1, mn1, ss1, doy1, ts1
  integer :: yr2, mo2, da2, hr2, mn2, ss2, doy2, ts2

  integer :: movetime      ! if 1=move time 2 data into time 1
  integer :: nstep, ierr
  real*8 :: time1, time2
  real   :: wt1, wt2, zw1, zw2, czb, cze, czm, gmt1, gmt2
  real   :: obsw(lis%d%lnc,lis%d%lnr)

  character*80 :: nameNH, nameSH

!=== End Variable Definition =============================================
!BOC
!----------------------------------------------------------------------
! Determine Required Observed Radiation Data Times 
!----------------------------------------------------------------------
  yr1 = lis%t%yr    !Previous Hour
  mo1 = lis%t%mo
  da1 = lis%t%da
  hr1 = lis%t%hr
  mn1 = 0
  ss1 = 0
  ts1 = 0
  call tick ( time1, doy1, gmt1, yr1, mo1, da1, hr1, mn1, ss1, ts1 )

  yr2 = lis%t%yr    !Next Hour
  mo2 = lis%t%mo
  da2 = lis%t%da
  hr2 = lis%t%hr
  mn2 = 0
  ss2 = 0
  ts2 = 60*60
  call tick ( time2, doy2, gmt2, yr2, mo2, da2, hr2, mn2, ss2, ts2 )

  lis%f%findagrtime1=0
  lis%f%findagrtime2=0
  movetime=0

  if(lis%t%time.ge.agrmetdrv%agrmtime2) then 
     movetime=1
     lis%f%findagrtime2=1
  endif

#if ( defined OPENDAP )
  if ( masterproc ) then
     nstep = get_nstep(lis%t)
  endif
  call MPI_BCAST(nstep, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
#else
  nstep = get_nstep(lis%t)
#endif
  if ( nstep == 0 .or. nstep == 1 ) then
     lis%f%findagrtime1=1
     lis%f%findagrtime2=1
     movetime=0
  endif

  if (lis%f%findagrtime1==1) then  
#if ( defined OPENDAP )
     call set_agrmet_index(0)
#endif
     sstat1 = 0
     sstat2 = 0
     lstat1 = 0
     lstat2 = 0
     call agrSWfile ( nameSH, lis, yr1, mo1, da1, hr1 )
     print*, 'using AGRMET ',nameSH
     sstat1 = 0
     call retglbSW ( 1, nameSH, sstat1, 1 )

!JESSE 20041227 USE SW ONLY
!     lstat1 = 0
!     call retagrlw ( 1, yr1, mo1, da1, hr1, lstat1, 1 )
!
!     if ((sstat1 + lstat1) < 2) then
!        sstat1 = 0
!        lstat1 = 0
!     end if
     if (sstat1 /= 0) agrmetdrv%agrmtime1 = time1
  endif 
  if(movetime.eq.1) then 
     agrmetdrv%agrmtime1 = agrmetdrv%agrmtime2
     sstat1 = sstat2
     lstat1 = lstat2
     do c=1, lis%d%ngrid
           obswdata1(c) = obswdata2(c)
!           oblwdata1(c) = oblwdata2(c)
     end do 
  endif 
  
  if(lis%f%findagrtime2.eq.1) then 
#if ( defined OPENDAP )
     call set_agrmet_index(1)
#endif
     sstat2 = 0
     lstat2 = 0
     call agrSWfile ( nameSH, lis, yr2, mo2, da2, hr2 )
     sstat2 = 0
     print*, 'using AGRMET ',nameSH
     call retglbSW ( 2, nameSH, sstat2, 1 )
!     lstat2 = 0
!     call retagrlw ( 2, yr2, mo2, da2, hr2, lstat2, 1 )
! 
!     if ((sstat2 + lstat2) < 2) then
!        sstat2 = 0
!        lstat2 = 0
!     end if
     if (sstat2 /= 0) agrmetdrv%agrmtime2 = time2
  endif
!----------------------------------------------------------------------
! Print out Status of data holdings
!----------------------------------------------------------------------
  if (lis%t%time == time1) then
     if (sstat1==0) write(*,*) 'NO AGR SW USED',mo1,da1,yr1,hr1
     if (sstat1/=0) write(*,*) 'USED AGRMET SW',mo1,da1,yr1,hr1
     if (sstat2==0) write(*,*) 'NO AGR SW USED',mo2,da2,yr2,hr2
     if (sstat2/=0) write(*,*) 'USED AGRMET SW',mo2,da2,yr2,hr2
     
     if (lstat1==0) write(*,*) 'NO AGR LW USED',mo1,da1,yr1,hr1
     if (lstat1/=0) write(*,*) 'USED AGRMET LW',mo1,da1,yr1,hr1
     if (lstat2==0) write(*,*) 'NO AGR LW USED',mo2,da2,yr2,hr2
     if (lstat2/=0) write(*,*) 'USED AGRMET LW',mo2,da2,yr2,hr2
  endif
  return
!EOC
end subroutine getgrad

!BOP
! !ROUTINE: agrSWfile:
!
! !DESCRIPTION:
!  This subroutine puts together the radiation data filenames.
!
! !REVISION HISTORY:
!  28  Oct 1999: Brian Cosgrove; Initial code
!  18  Jun 2001: Urszula Jambor; Modified for AGRMET data use in GLDAS
!  24  Oct 2001: Jesse Meng; Modified for AGRMET directory structure
!  15  Aug 2003: Sujay Kumar: Modified to create a global filename 
!                instead of two filenames for each hemisphere.
!
! !INTERFACE:
subroutine agrSWfile ( nameSH, lis, yr, mo, da, hr )
! !USES:
  use lis_module         
  use agrmetdomain_module, only : agrmetdrv
!EOP
  implicit none
  type (lisdec) lis

!=== Local Variables =====================================================

  character*80 ::  nameSH
  integer :: yr, mo, da, hr

  integer :: i, c

  character*1 :: fname(80), fbase(80), fsub(80)
  character*1 :: ftime(15), fdir(13), fhemi(1)

!=== End Variable Definition =============================================

!BOC
92 format (80a1)
93 format (a80)
94 format (a5, i4, i2, i2, i2)
95 format (15a1)
96 format (a40)
97 format (a1)
98 format (a6, i4, i2, a1)
99 format (13a1)

  open(unit=81, file='temp', form='formatted', access='direct', recl=80)
  write(81,96,rec=1) agrmetdrv%agrmetdir  
  read(81,92,rec=1) (fbase(i), i=1,80)

  write(81,98,REC=1)'/SWDN/',yr,mo,'/'
  read(81,99,rec=1) fdir
  do i = 1, 13
     if ( fdir(i) == ' ' ) fdir(i) = '0'
  end do
  write(81,94,rec=1) 'swdn_', yr, mo, da, hr
  read(81,95,rec=1) ftime
  do i = 1, 15
     if ( ftime(i) == ' ' ) ftime(i) = '0'
  end do
  c = 0
  do i = 1, 80
     if ( (fbase(i) == ' ') .and. (c == 0) ) c = i-1
  end do

  write(81, 92, rec=1) (fbase(i),i=1,c), (fdir(i),i=1,13), &
       (ftime(i),i=1,15)
  read(81, 93, rec=1) nameSH
  close(81)
  
  return
!EOC
end subroutine agrSWfile

