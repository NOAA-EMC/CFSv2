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
! !ROUTINE: elevadjust.F90
!
! !DESCRIPTION:
!  Corrects Temperature, Pressure, Humidity and Longwave Radiation
!  values for differences in elevation between EDAS and LDAS grids.
!
! !REVISION HISTORY:
!  11 Apr 2000: Brian Cosgrove; Initial Code
!  12 May 2000: Brian Cosgrove; Corrected for zero humidities
!  09 Aug 2000: Brian Cosgrove; Corrected program so that
!               it only performs calculations if both
!               the elevation difference file and the forcing
!               data file (use temperature data as check for all
!               fields) contain defined values
!  25 Jan 2001: Matt Rodell; Compute number of input and output
!		grid points, use to allocate local arrays
!  27 Feb 2001: Brian Cosgrove; Added statement to check for use of
!               catchment data so that correct elevation correction
!               files is used
!  15 Mar 2001: Jon Gottschalck; if-then to handle negative vapor
!		pressures in long wave correction
!  15 Mar 2001: Matt Rodell; merge NLDAS and GLDAS versions
!  14 Nov 2003: Sujay Kumar; Adopted in LIS
!
! !INTERFACE:
subroutine elevadjust(t,f,fforce,force_tmp,force_hum,force_lwd, &
     force_prs)
  ! !USES:
  use lisdrv_module, only: tile
  implicit none
  !INPUT PARAMETERS:
  integer, intent(in) :: f, t
  !OUTPUT PARAMETERS:
  real, intent(inout) :: fforce,force_tmp,force_hum,&
       force_lwd,force_prs
  !EOP

  integer, parameter :: bb=2016
  integer err !iostat error code

  real :: mee,mfe,ee,fe
  real :: lapse, grav, rdry, ratio
  real :: esat,qsat,rh,fesat,fqsat,femiss,emiss
  real :: tcforce,pcforce,hcforce,lcforce,tbar
  !BOC  
  grav = 9.81
  rdry = 287.
  lapse = -0.0065
  tcforce=force_tmp+(lapse*tile(t)%elev)
  tbar=(force_tmp+tcforce)/2.
  pcforce=force_prs/(exp((grav*tile(t)%elev)/(rdry*tbar)))
  if (force_hum .eq. 0) force_hum=1e-08
  ee=(force_hum*force_prs)/0.622               
  esat=611.2*(exp((17.67*(force_tmp-273.15))/&
       ((force_tmp-273.15)+243.5)))
  qsat=(0.622*esat)/(force_prs-(0.378*esat))
  rh=(force_hum/qsat)*100.
  fesat=611.2*(exp((17.67*(tcforce-273.15))/ &
       ((tcforce-273.15)+243.5)))
  fqsat=(0.622*fesat)/(pcforce-(0.378*fesat))
  hcforce=(rh*fqsat)/100.
  fe=(hcforce*pcforce)/0.622
  mee=ee/100.
  mfe=fe/100.
  !----------------------------------------------------------------------
  ! correct for negative vapor pressure at very low temperatures at
  ! high latitudes
  !----------------------------------------------------------------------
  if (mee .le. 0) mee = 1e-08
  if (mfe .le. 0) mfe = 1e-08
  emiss  =1.08*(1-exp(-mee**(force_tmp/bb)))
  femiss =1.08*(1-exp(-mfe**(tcforce/bb)))
  ratio=(femiss*(tcforce**4))/(emiss*(force_tmp**4))
  lcforce=force_lwd*ratio

  select case (f)
  case(1)
     fforce=tcforce
  case(2)
     fforce=hcforce
  case(4)
     fforce=lcforce
  case(7)
     fforce=pcforce
  case default
     print*, "not a valid forcing type for elevation adjustment"
     call endrun
  end select
  return
  !EOC
end subroutine elevadjust

