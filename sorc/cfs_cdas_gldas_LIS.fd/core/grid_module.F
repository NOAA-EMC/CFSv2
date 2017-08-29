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
! !MODULE: grid_module.F90
!
! !DESCRIPTION:
!  LIS non-model-specific grid variables only.
!
!  FORCING() ARRAY: \\
!  1. T 2m    Temperature interpolated to 2 metres [$K$] \\
!  2. q 2m    Instantaneous specific humidity interpolated to 2 metres[$kg/kg$] \\
!  3. radswg  Downward shortwave flux at the ground [$W/m^2$] \\
!  4. lwgdwn  Downward longwave radiation at the ground [$W/m^2$] \\
!  5. u 10m   Instantaneous zonal wind interpolated to 10 metres [$m/s$] \\
!  6. v 10m   Instantaneous meridional wind interpolated to 10 metres[$m/s$] \\
!  7. ps      Instantaneous Surface Pressure [$Pa$] \\
!  8. preacc  Total precipitation [$mm/s$] \\
!!  9. precon  Convective precipatation [$mm/s$] \\
!  9. gfrac   Greenness fraction (0-1) 
! 10. albedo  Surface albedo (0-1)
! 11. zlvl    Height of atmospheric forcing [m]
! 12. z0      Surface roughness [m]
! 13. ch      Surface exchange coefficient [m/s]   
! 14. t1      Skin Temperature [K]
! 15. sneqv   SWE [m]
! 16. snowh   Snow Depth [m]
!
! !REVISION HISTORY:
!  15 Oct 1999: Paul Houser; Initial code
!  11 Apr 2000: Brian Cosgrove; Added Forcing Mask variables
!  23 Feb 2001: Urszula Jambor; Added GEOS & GDAS forcing variables
!  27 Feb 2001: Brian Cosgrove; Added Catchment forcing data variables
!  23 Mar 2001: Jon Radakovich; Added variables for PSAS assimilation
!  04 Sep 2001: Brian Cosgrove; Added variabes for humidity, precip,par
!               brightness temp,precip mask, removed awips2lis and 
!               pinker2lis variables, GRIB interp. package used now
!  15 Oct 2001: Jesse Meng; Revised doc block with forcing array definition
!  15 Oct 2001: Jesse Meng; Added oblwdata1 and oblwdata2
!  14 Nov 2002: Sujay Kumar; Optimized version of grid_module
!  01 May 2007: Jesse Meng; Added zlvl, z0, ch, t1
!  19 Nov 2007: Jesse Meng; Added gfrac, sneqv, snowh
!              When changing number of variables, MUST
!              modify grid_blkcnts = (/J/) in driverpardef_module.F
!              J = number of real
!
! !INTERFACE:
module grid_module 
  implicit none
  public griddec
! !ARGUMENTS:  
  type griddec
     real    :: lat            !latitude of grid point
     real    :: lon            !longitude of grid point 
     real    :: forcing(16)    !interpolated LIS forcing array
     real    :: fgrd(13)  !fraction of vegetation class in grid     
  end type griddec
!EOP
end module grid_module


