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
! !ROUTINE: retglbSW.F90
!
! !DESCRIPTION:
!  Opens, reads, and interpolates radiation forcing.
!
!    TIME1 = most recent past data\\
!    TIME2 = most recent future data\\
!
!
! !REVISION HISTORY:
!  28 Oct 1999: Brian Cosgrove; Initial code, retrad.f
!  25 Jun 2001: Urszula Jambor; Renamed & modified for AGRMET data 
!               use in GLDAS.
!  21 Nov 2001: Urszula Jambor; Added iostat check in open & read stmts;
!               renamed to distinguish SW from LW routines.
!  27 Feb 2002: Urszula Jambor; Added call to fill_land to compensate
!               deficiencies in interpolation to coarse 2x2.5 grid.
!  16 Oct 2002: Urszula Jambor; Corrected array ranges used in 2x2.5 case.
!               Previously, array mismatch occurred.
!  11 Dec 2002: Urszula Jambor; Added domain 4 and 5 in attached routine
!
! !INTERFACE:
subroutine retglbSW ( order, nameSH, ferror, flag )
! !USES:
  use lisdrv_module,only  : lis, grid      	! LDAS non-model-specIFic 1-D variables
  use obsradforcing_module, only : obswdata1, obswdata2
  implicit none
! !ARGUMENTS:
  character*80 :: nameSH
  integer :: flag               !data source, 1=AGRMET
  integer :: order              !retrieve data for time1 or time2
  integer :: ferror             !0=no radiation data found
                                !1=found observational data (may be udef)
!EOP
  integer :: c, r

  real :: outdata(lis%d%ngrid)

!=== End Variable Definition =============================================
!BOC
!-------------------------------------------------------------------------
! If using AGRMET data, open, read in and interpolate AGRMET files
! to appropriate GLDAS resolution
! If error reading file, ferror=0 else ferror=1
!-------------------------------------------------------------------------
  if (flag == 1) then
     call interp_agrmet_sw( nameSH, outdata, ferror )
     do c=1, lis%d%ngrid
        if (order == 1) then
           obswdata1(c) = outdata(c)
        else if (order == 2) then
           obswdata2(c) = outdata(c)
        end if
     end do
  end if !flag=1
!EOC  
end subroutine retglbSW



