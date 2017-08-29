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
! !ROUTINE: retagrlw.F90
! 
! !DESCRIPTION:
!  Opens, reads, interpolates and overlays LW radiation forcing.
!
!    TIME1 = most recent past data\\
!    TIME2 = most recent future data\\
!
!	1. Load AGRMET 3 level cloud amount (convertted from RTNEPH)\\
!	2. Convert 2 m forcing specific humidity to vapor pressure\\
!	3. Use AGRMET subroutine longwv() to calculate LW DOWN. Arguments are:\\
!	   cloud amount [$\%$], 2 m temperature [K], and 2 m vapor pressure [Pa]\\
!
! !REVISION HISTORY:
!  28 Oct 1999: Brian Cosgrove; Initial code
!  11 Apr 2000: Brian Cosgrove; changed code to use Forcing Mask (With inland
!               water filled in).  Deleted unused variables.
!  20 Jun 2000: Brian Cosgrove; changed code so that it uses LDAS%UDEF and
!                not a hard-wired undefined value of -999.9 and -999.0
!  25 Jun 2001: Urszula Jambor; Modified for AGRMET data use in GLDAS.
!  25 Oct 2001: Jesse Meng; Modified for AGRMET LW scheme implementation
! 
! !INTERFACE:
subroutine retagrlw ( order,yr, mo, da, hr, ferror, flag )
! !USES:
  use lisdrv_module, only : lis, grid
  use obsradforcing_module, only : oblwdata1,oblwdata2
  use agrmetdomain_module, only : agrmetdrv
  use lis_openfilemod
  implicit none
! !ARGUMENTS:
  integer :: yr, mo, da, hr
  integer :: order              !retrieve data for time1 or time2
  integer :: ferror             !0=no radiation data found
                                !1=found observational data
!EOP  
  integer, parameter :: nagrc=1440, nagrr=600  
!=== Local Variables =====================================================
  integer :: i,j
  integer :: openerrN=0, openerrS=0 !set to non-zero if error found
  integer :: readerrN=0, readerrS=0 !set to non-zero if error found
  character*4  :: cyr
  character*2  :: cmo, cda, chr
  character*80 :: nameSH

  integer :: flag               !data source, 1=AGRMET, all we have for now
  integer :: fvalid             !0=cloud data undef

  integer :: c, r
  
  real :: pdata1H(nagrc,nagrr)
  real :: pdata1M(nagrc,nagrr)
  real :: pdata1L(nagrc,nagrr)

  real :: cldamtH(lis%d%ngrid) !high layer cloud amount [%]
  real :: cldamtM(lis%d%ngrid) !mid  layer cloud amount [%]
  real :: cldamtL(lis%d%ngrid) !low  layer cloud amount [%]
  
  real :: cldamt1d( 3 )        !single column 3 layet cldamt [%]
  
  real :: tair                 !2 m temperature [K]
  real :: vaporP               !2 m vapor pressure [Pa]

  real :: rldown               !LW down [W/m2]
  
!=== End Variable Definition =============================================
!BOC
!-------------------------------------------------------------------------
! If using AGRMET data, open, read in and interpolate AGRMET files
!   to appropriate GLDAS resolution;
! If error reading file, or data completely missing, ferror=0;
!------------------------------------------------------------------------
  ferror = 0
  do c = 1, lis%d%ngrid
     if (order == 1) then
        oblwdata1(c) = lis%d%udef
     else if (order == 2) then
        oblwdata2(c) = lis%d%udef
     end if
  end do

  if (flag == 1) then
!------------------------------------------------------------------------
! 1. Generate AGRMET cloud filenames
! Load 3 layers cldamt data 
!------------------------------------------------------------------------

     write(cyr, '(I4.4)') yr
     write(cmo, '(I2.2)') mo
     write(cda, '(I2.2)') da
     write(chr, '(I2.2)') hr
     
     nameSH = trim(agrmetdrv%agrmetdir)//'/CloudAGR/'//cyr//cmo// &
          '/cldamt_'//cyr//cmo//cda//chr
!     print*, 'Reading AGRMET file : ',nameSH
!     open(11, file=nameSH, status='old',access='direct',&
!          recl=3456000, iostat=openerrN)
     call lis_open_file(11, file=nameSH, status='old',access='direct',&
          recl=3456000, script='getagrmet_lw.pl')
#if 0
! Jesse's way
     read(11, rec=1,iostat=readerrN) pdata1H
     read(11, rec=2,iostat=readerrN) pdata1M
     read(11, rec=3,iostat=readerrN) pdata1L
#else
! Cluster's way
     read(11, rec=1,iostat=readerrN) pdata1L
     read(11, rec=2,iostat=readerrN) pdata1M
     read(11, rec=3,iostat=readerrN) pdata1H
#endif
     close(11)
     if ((openerrN+readerrN) > 0) then
        ferror = 0 
        print*, 'AGRMET file problem: ',nameSH
     else
        ferror = 1
     endif
     call interp_agrmet_lw( pdata1H,cldamtH, ferror )
     call interp_agrmet_lw( pdata1M,cldamtM, ferror )
     call interp_agrmet_lw(pdata1L,cldamtL, ferror )
     if ( ferror .EQ. 0 ) then
        call lis_log_msg('ERR: retagrlw -- ferror == 0')
        return
     endif
!----------------------------------------------------------------------
! If AGRMET cloud data is not undefined, set ferror=1
!----------------------------------------------------------------------
     fvalid = 0
     do c=1, lis%d%ngrid
        if (cldamtH(c) >= 0.0) fvalid = 1
        if (cldamtM(c) >= 0.0) fvalid = 1
        if (cldamtL(c) >= 0.0) fvalid = 1
     end do
     if ( fvalid .EQ. 0 ) then 
        call lis_log_msg('ERR: retagrlw -- fvalid == 0')
        return
     endif
     do c=1,lis%d%ngrid
        rldown = 0.
           tair = grid(c)%forcing(1)
!----------------------------------------------------------------------
! 2. CONVERT 2 METER SPECIFIC HUMIDITY TO VAPOR PRESSURE 
!----------------------------------------------------------------------
           vaporP = grid(c)%forcing(2) * grid(c)%forcing(7) / &
                    ( 0.622 + grid(c)%forcing(2) * (1-0.622) )	      
!----------------------------------------------------------------------
! If tair, vaporP, and ALL 3 AGRMET LAYERS cldamt are defined, 
! calculate rldown; transfer to proper output array
!----------------------------------------------------------------------
           fvalid = 1
	   if (tair.LT. 0.0) fvalid = 0
	   if (vaporP.LT. 0.0) fvalid = 0 
           if (cldamtH(c) .LT. 0.0) fvalid = 0
           if (cldamtM(c) .LT. 0.0) fvalid = 0
           if (cldamtL(c) .LT. 0.0) fvalid = 0
	      
	   if (fvalid == 1) then
              cldamt1d(1) = cldamtL(c)
	      cldamt1d(2) = cldamtM(c)
	      cldamt1d(3) = cldamtH(c)
!----------------------------------------------------------------------
! 3. Calculate lw down 
!----------------------------------------------------------------------
              call agrlwdn( tair, vaporp, cldamt1d, rldown )
              if (order==1) then
                 oblwdata1(c) = rldown
              else if (order==2) then
                 oblwdata2(c) = rldown
              end if
           else	
              if (order==1) then
                 oblwdata1(c) = lis%d%udef
              else if (order==2) then
                 oblwdata2(c) = lis%d%udef
              end if              
           end if 
        end do
     end if
  return
!EOC
end subroutine retagrlw








