!-------------------------------------------------------------------------
!     NASA GSFC Land Information Systems LIS 2.3                         !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE: noahdrv_module.F90 
!
! !DESCRIPTION:
!  Module for runtime specific Noah variables
!
! !REVISION HISTORY:
!
! 14 Oct 2003; Sujay Kumar, Initial Version
! 
! !INTERFACE:
module noahdrv_module
! !ARGUMENTS:
  type noahdrvdec
     integer :: noahopen       !Keeps track of opening files
     integer :: numoutnh       !Counts number of output times for Noah
     integer :: noah_nvegp     !Number of static vegetation parameter
     integer :: noah_nsoilp    !Number of static soil parameters
     integer :: noah_zst       !Number of Zobler soil classes
     integer :: noah_gflag     !Time flag to update gfrac files
     integer :: noah_albtime   !Time flag to update albedo files
     integer :: noah_aflag     !Time flag to update albedo files 
     integer :: noah_albdchk   !Day check to interpolate alb values
     integer :: noah_gfracdchk !Day check to interpolate gfrac value
     character*40 :: NOAH_RFILE !NOAH Active Restart File
!     character*40 :: NOAH_MFILE !NOAH model init. restart file
     CHARACTER*40 :: NOAH_VFILE !NOAH Static Vegetation Parameter File
     CHARACTER*40 :: NOAH_SFILE !NOAH Soil Parameter File
     CHARACTER*40 :: NOAH_SLPFILE!NOAH Slope Type File
     CHARACTER*40 :: NOAH_MGFILE !NOAH Monthly Veg. Green Frac.
     CHARACTER*40 :: NOAH_ALBFILE !NOAH Quart. Snow-free albedo
     CHARACTER*50 :: NOAH_MXSNAL !NOAH GLDAS max snow albedo
     CHARACTER*50 :: NOAH_TBOT   !NOAH GLDAS Bottom Temp
     REAL*8 :: NOAH_GFRACTIME  !Time flag to update gfrac files
     REAL :: NOAH_ISM          !NOAH Initial Soil Moisture (m3/m3)
     REAL :: NOAH_IT           !NOAH Initial Soil Temperature (K)
     REAL :: WRITEINTN         !NOAH Output Interval (hours)
  end type noahdrvdec
!EOC
end module noahdrv_module
